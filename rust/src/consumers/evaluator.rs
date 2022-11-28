use crate::plugins::evaluate_plugin::evaluate_plugin_for_plaintext_backend;
use crate::structs::count::Count;
use crate::structs::directives::Directive;
use crate::structs::function::FunctionBody;
use crate::structs::plugin::PluginBody;
use crate::structs::types::Type;
use crate::structs::value::value_to_biguint;
use crate::structs::wirerange::{
    add_types_to_wire_ranges, check_wire_ranges_with_counts, WireRangeWithType,
};
use crate::{Gate, Message, PrivateInputs, PublicInputs, Relation, Result, TypeId, WireId};
use num_bigint::BigUint;
use num_traits::identities::{One, Zero};
use num_traits::Pow;
use std::collections::{HashMap, VecDeque};
use std::convert::TryFrom;

/// The `ZKBackend` trait should be implemented by any backend that wants to evaluate SIEVE IR circuits.
/// It has to define 2 types:
///  - `Wire`: represents a variable in the circuit. It should implement the `Clone` trait.
///  - `TypeElement`: represents elements of the underlying type. Mainly used when importing
///                    public/private inputs from the corresponding pools.
/// see `PlaintextBackend` for an working example of an implementation.
pub trait ZKBackend {
    type Wire: Clone;
    /// Usually a big Integer type.
    type TypeElement: 'static + Clone;

    /// Imports a `Self::TypeElement` from a byte buffer, in little endian.
    /// If the buffer does not represent an element of the underlying type, then
    /// it returns an Err.
    fn from_bytes_le(val: &[u8]) -> Result<Self::TypeElement>;
    /// Set the underlying type of the running backend.
    /// If the type is not compatible with this ZKBackend, then it should return Err
    fn set_types(&mut self, types: &[Type]) -> Result<()>;

    /// Returns a `TypeElement` representing '1' in the underlying type.
    fn one(&self) -> Result<Self::TypeElement>;
    /// Returns a `TypeElement` representing '-1' in the underlying type.
    fn minus_one(&self, type_id: &TypeId) -> Result<Self::TypeElement>;
    /// Returns a `TypeElement` representing '0' in the underlying type.
    fn zero(&self) -> Result<Self::TypeElement>;

    // Returns a new instance of a given Wire id
    fn copy(&mut self, type_id: &TypeId, wire: &Self::Wire) -> Result<Self::Wire>;

    /// Imports a constant value into a new `Self::Wire`.
    fn constant(&mut self, type_id: &TypeId, val: Self::TypeElement) -> Result<Self::Wire>;
    /// This functions checks whether the given `Self::Wire` is indeed zero or not.
    /// Depending upon whether `self` is in prover, or verifier mode, this function should act
    /// accordingly. E.g. in prover mode, if the wire is not zero, then this function should
    /// return an Err.
    fn assert_zero(&mut self, type_id: &TypeId, wire: &Self::Wire) -> Result<()>;

    /// Adds two wires into a new wire.
    fn add(&mut self, type_id: &TypeId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire>;
    /// Multiplies two wires into a new wire.
    fn multiply(&mut self, type_id: &TypeId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire>;
    /// Adds a given wire by a constant `Self::TypeElement` into a new wire.
    fn add_constant(
        &mut self,
        type_id: &TypeId,
        a: &Self::Wire,
        b: Self::TypeElement,
    ) -> Result<Self::Wire>;
    /// Multiplies a given wire by a constant `Self::TypeElement` into a new wire.
    fn mul_constant(
        &mut self,
        type_id: &TypeId,
        a: &Self::Wire,
        b: Self::TypeElement,
    ) -> Result<Self::Wire>;

    /// This functions declares a new public input variable owning the value given as parameter,
    /// which should be stored in a new wire.
    fn public_input(&mut self, type_id: &TypeId, val: Self::TypeElement) -> Result<Self::Wire>;
    /// This functions declares a new private input variable owning the value given as parameter,
    /// which should be stored in a new wire.
    /// The value is given as a `Option`, because depending upon the type of this ZKBackend
    /// (prover / verifier), it should act differently.
    ///  - In prover mode, the private input should be provided, so the value should be `Some`.
    ///  - In verifier mode, the private input should normally not be provided (except maybe in test mode)
    /// Both cases should return a `Self::Wire` so the ZKBackend should have a specific wire value
    /// to handle it when in verifier mode.
    fn private_input(
        &mut self,
        type_id: &TypeId,
        val: Option<Self::TypeElement>,
    ) -> Result<Self::Wire>;

    /// New is used to allocate in a contiguous memory space wires between first and last inclusive.
    /// If the backend is not interested in contiguous allocations, this function should do nothing.
    fn gate_new(&mut self, type_id: &TypeId, first: WireId, last: WireId) -> Result<()>;

    /// Convert `inputs` from type `input_type_id` into type `output_type_id`.
    /// The result is stored in new wires.
    /// The result must contain `output_wire_count` wires.
    fn convert(
        &mut self,
        output_type_id: &TypeId,
        output_wire_count: u64,
        input_type_id: &TypeId,
        inputs: &[&Self::Wire],
    ) -> Result<Vec<Self::Wire>>;

    /// Evaluate plugin defined in `plugin_body` on `inputs`, `public_inputs` and `private_inputs`
    /// The result is stored in new wires.
    /// The result must be compliant with `output_wire_count`.
    /// If the plugin is unknown or if `input_count`, `output_count`, `inputs`, `public_inputs`,
    /// `private_inputs` or `plugin_body` are not compliant with the plugin specifications,
    /// then `evaluate_plugin` returns an error.
    fn evaluate_plugin(
        &mut self,
        output_count: &[Count],
        input_count: &[Count],
        inputs: &[&Self::Wire],
        public_inputs: &HashMap<TypeId, Vec<Self::TypeElement>>,
        private_inputs: &HashMap<TypeId, Vec<Self::TypeElement>>,
        plugin_body: &PluginBody,
    ) -> Result<Vec<Self::Wire>>;
}

/// This structure defines a function as defined in the circuit, but without the name.
/// It's mainly used to retrieve information from the name.
struct FunctionDeclaration {
    body: FunctionBody,
    output_count: Vec<Count>,
    input_count: Vec<Count>,
}

/// This structure is the core of IR evaluation. It is instantiated using a ZKBackend,
/// and will read the IR circuit, parses it, and calls the corresponding function from the
/// ZKBackend to evaluate each single operation.
/// It will inline functions.
///
/// # Example
/// ```
/// use zki_sieve::consumers::evaluator::{PlaintextBackend, Evaluator};
/// use zki_sieve::producers::examples::*;
///
/// let relation = example_relation();
/// let public_inputs = example_public_inputs();
/// let private_inputs = example_private_inputs();
///
/// let mut zkbackend = PlaintextBackend::default();
/// let mut simulator = Evaluator::default();
/// let _ = simulator.ingest_public_inputs(&public_inputs);
/// let _ = simulator.ingest_private_inputs(&private_inputs);
/// let _ = simulator.ingest_relation(&relation, &mut zkbackend);
/// ```
pub struct Evaluator<B: ZKBackend> {
    values: HashMap<(TypeId, WireId), B::Wire>,
    inputs: EvaluatorInputs<B>,
    // name => (body, output_count, input_count)
    known_functions: HashMap<String, FunctionDeclaration>,

    found_error: Option<String>,
}

impl<B: ZKBackend> Default for Evaluator<B> {
    fn default() -> Self {
        Evaluator {
            values: Default::default(),
            known_functions: Default::default(),
            inputs: Default::default(),
            found_error: None,
        }
    }
}

pub struct EvaluatorInputs<B: ZKBackend> {
    types: Vec<Type>,
    public_inputs_queue: HashMap<Type, VecDeque<B::TypeElement>>,
    private_inputs_queue: HashMap<Type, VecDeque<B::TypeElement>>,
}

impl<B: ZKBackend> Default for EvaluatorInputs<B> {
    fn default() -> Self {
        EvaluatorInputs {
            types: vec![],
            public_inputs_queue: Default::default(),
            private_inputs_queue: Default::default(),
        }
    }
}

impl<B: ZKBackend> Evaluator<B> {
    /// Creates an Evaluator for an iterator over `Messages`
    /// The returned Evaluator can then be reused to ingest more messages using the one of the
    /// `ingest_***` functions.
    pub fn from_messages(messages: impl Iterator<Item = Result<Message>>, backend: &mut B) -> Self {
        let mut evaluator = Evaluator::default();
        messages.for_each(|msg| evaluator.ingest_message(&msg.unwrap(), backend));
        evaluator
    }

    /// Returns the list of violations detected when evaluating the IR circuit.
    /// It consumes `self`.
    pub fn get_violations(self) -> Vec<String> {
        let mut violations = vec![];
        if let Some(err) = self.found_error {
            violations.push(err);
        }
        violations
    }

    /// Ingests a `Message` using the ZKBackend given in parameter.
    /// If a error was found in previous Messages, then it does nothing but returns,
    /// otherwise it ingests the message.
    pub fn ingest_message(&mut self, msg: &Message, backend: &mut B) {
        if self.found_error.is_some() {
            return;
        }

        match self.ingest_message_(msg, backend) {
            Err(err) => self.found_error = Some(err.to_string()),
            Ok(()) => {}
        }
    }

    fn ingest_message_(&mut self, msg: &Message, backend: &mut B) -> Result<()> {
        match msg {
            Message::PublicInputs(i) => self.ingest_public_inputs(i),
            Message::PrivateInputs(w) => self.ingest_private_inputs(w),
            Message::Relation(r) => self.ingest_relation(r, backend),
        }
    }

    /// Ingest a `PublicInputs` message, and returns a `Result` whether ot nor an error
    /// was encountered. It stores the public input values in a pool.
    pub fn ingest_public_inputs(&mut self, public_inputs: &PublicInputs) -> Result<()> {
        let cleaned_type = public_inputs.type_value.cleaned_type();
        let inputs = self
            .inputs
            .public_inputs_queue
            .entry(cleaned_type)
            .or_insert_with(VecDeque::new);
        public_inputs
            .inputs
            .iter()
            .try_for_each::<_, Result<()>>(|value| {
                inputs.push_back(B::from_bytes_le(value)?);
                Ok(())
            })?;
        Ok(())
    }

    /// Ingest a `PrivateInputs` message, and returns a `Result` whether ot nor an error
    /// was encountered. It stores the private input values in a pool.
    pub fn ingest_private_inputs(&mut self, private_inputs: &PrivateInputs) -> Result<()> {
        let cleaned_type = private_inputs.type_value.cleaned_type();
        let inputs = self
            .inputs
            .private_inputs_queue
            .entry(cleaned_type)
            .or_insert_with(VecDeque::new);
        private_inputs
            .inputs
            .iter()
            .try_for_each::<_, Result<()>>(|value| {
                inputs.push_back(B::from_bytes_le(value)?);
                Ok(())
            })?;
        Ok(())
    }

    /// Ingest a `Relation` message
    pub fn ingest_relation(&mut self, relation: &Relation, backend: &mut B) -> Result<()> {
        if self.inputs.types.is_empty() {
            relation.types.iter().for_each(|type_value| {
                self.inputs.types.push(type_value.cleaned_type());
            });
        }
        backend.set_types(&relation.types)?;

        for directive in relation.directives.iter() {
            match directive {
                Directive::Function(function) => {
                    self.known_functions.insert(
                        function.name.clone(),
                        FunctionDeclaration {
                            body: function.body.clone(),
                            output_count: function.output_count.clone(),
                            input_count: function.input_count.clone(),
                        },
                    );
                }
                Directive::Gate(gate) => {
                    Self::ingest_gate(
                        gate,
                        backend,
                        &mut self.values,
                        &self.known_functions,
                        &mut self.inputs,
                    )?;
                }
            };
        }

        Ok(())
    }

    /// This function consumes and returns some public/private inputs
    /// - `inputs` contains the available public/private inputs
    /// - `type_id` contains the TypeId of the public/private inputs we would like to consume
    /// - `count` contains the number of public/private inputs we would like to consume
    /// - `is_public` is true if we would like to consume public inputs and false if we would like
    /// to consume private inputs
    fn get_input_values(
        inputs: &mut EvaluatorInputs<B>,
        type_id: &TypeId,
        count: u64,
        is_public: bool,
    ) -> Result<Vec<B::TypeElement>> {
        let type_value = inputs
            .types
            .get(usize::try_from(*type_id)?)
            .ok_or(format!("Unknown type id ({})", type_id))?;
        let (inputs_queue, err_message) = if is_public {
            (
                &mut inputs.public_inputs_queue,
                "Not enough public inputs to consume",
            )
        } else {
            (
                &mut inputs.private_inputs_queue,
                "Not enough private inputs to consume",
            )
        };
        let inputs_queue_for_type = inputs_queue.get_mut(type_value).ok_or(err_message)?;
        let mut values = vec![];
        for _ in 0..count {
            let val = inputs_queue_for_type.pop_front().ok_or(err_message)?;
            values.push(val);
        }
        Ok(values)
    }

    /// This function ingests one gate at a time (but can call itself recursively)
    /// - `scope` contains the list of existing wires with their respective value. It will be
    ///    augmented if this gate produces outputs, or reduced if this is a `GateDelete`
    /// - `known_functions` is the map of functions defined in previous or current `Relation` message
    ///    current gate is a `GateFor`
    /// - `moduli` is used mainly in convert gates.
    /// - `public_inputs` and `private_inputs` are the public_inputs and private_inputs pools, implemented as Queues.
    ///    They will be consumed whenever necessary.
    fn ingest_gate(
        gate: &Gate,
        backend: &mut B,
        scope: &mut HashMap<(TypeId, WireId), B::Wire>,
        known_functions: &HashMap<String, FunctionDeclaration>,
        inputs: &mut EvaluatorInputs<B>,
    ) -> Result<()> {
        use Gate::*;

        macro_rules! get {
            ($type_id:expr, $wire_id:expr) => {{
                get::<B>(scope, $type_id, $wire_id)
            }};
        }

        macro_rules! set {
            ($type_id: expr, $wire_id:expr, $wire_name:expr) => {{
                set::<B>(scope, $type_id, $wire_id, $wire_name)
            }};
        }

        match gate {
            Constant(type_id, out, value) => {
                let wire = backend.constant(type_id, B::from_bytes_le(value)?)?;
                set!(*type_id, *out, wire)?;
            }

            AssertZero(type_id, inp) => {
                let inp_wire = get!(*type_id, *inp)?;
                if backend.assert_zero(type_id, inp_wire).is_err() {
                    return Err(format!(
                        "Wire ({}: {}) should be 0, while it is not",
                        *type_id, *inp
                    )
                    .into());
                }
            }

            Copy(type_id, out, inp) => {
                let in_wire = get!(*type_id, *inp)?;
                let out_wire = backend.copy(type_id, in_wire)?;
                set!(*type_id, *out, out_wire)?;
            }

            Add(type_id, out, left, right) => {
                let l = get!(*type_id, *left)?;
                let r = get!(*type_id, *right)?;
                let sum = backend.add(type_id, l, r)?;
                set!(*type_id, *out, sum)?;
            }

            Mul(type_id, out, left, right) => {
                let l = get!(*type_id, *left)?;
                let r = get!(*type_id, *right)?;
                let prod = backend.multiply(type_id, l, r)?;
                set!(*type_id, *out, prod)?;
            }

            AddConstant(type_id, out, inp, constant) => {
                let l = get!(*type_id, *inp)?;
                let r = B::from_bytes_le(constant)?;
                let sum = backend.add_constant(type_id, l, r)?;
                set!(*type_id, *out, sum)?;
            }

            MulConstant(type_id, out, inp, constant) => {
                let l = get!(*type_id, *inp)?;
                let r = B::from_bytes_le(constant)?;
                let prod = backend.mul_constant(type_id, l, r)?;
                set!(*type_id, *out, prod)?;
            }

            Public(type_id, out) => {
                let mut val = Self::get_input_values(inputs, type_id, 1, true)?;
                set_public_input(backend, scope, *type_id, *out, val.pop().unwrap())?;
            }

            Private(type_id, out) => {
                let val_result = Self::get_input_values(inputs, type_id, 1, false);
                match val_result {
                    Ok(mut values) => {
                        set_private_input(
                            backend,
                            scope,
                            *type_id,
                            *out,
                            Some(values.pop().unwrap()),
                        )?;
                    }
                    // It is not always possible to retrieve private values (e.g. for verifier).
                    // It is managed in `set_private_input` by a value input equal to `None`.
                    Err(_) => set_private_input(backend, scope, *type_id, *out, None)?,
                };
            }

            New(type_id, first, last) => {
                backend.gate_new(type_id, *first, *last)?;
            }

            Delete(type_id, first, last) => {
                for current in *first..=*last {
                    remove::<B>(scope, *type_id, current)?;
                }
            }

            Convert(
                out_type_id,
                out_first_id,
                out_last_id,
                in_type_id,
                in_first_id,
                in_last_id,
            ) => {
                // Check ranges are correct (first <= last)
                if out_first_id > out_last_id {
                    return Err(format!(
                        "Evaluation of a Convert gate:: out_last_id ({}) must be greater than out_first_id ({}).", out_last_id, out_first_id
                    )
                        .into());
                }
                if in_first_id > in_last_id {
                    return Err(format!(
                        "Evaluation of a Convert gate:: in_last_id ({}) must be greater than in_first_id ({}).", in_last_id, in_first_id
                    )
                        .into());
                }

                // Retrieve input values
                let input_values = (*in_first_id..=*in_last_id)
                    .map(|wire_id| get::<B>(scope, *in_type_id, wire_id))
                    .collect::<Result<Vec<_>>>()?;

                // Evaluate the Convert gate
                let expected_output_len = *out_last_id - *out_first_id + 1;
                let out =
                    backend.convert(out_type_id, expected_output_len, in_type_id, &input_values)?;

                // Check out length
                if expected_output_len != u64::try_from(out.len())? {
                    // It is not possible
                    panic!("Evaluation of a Convert gate: number of output wires mismatch.");
                }

                // Store out values
                out.iter().zip(*out_first_id..=*out_last_id).try_for_each(
                    |(output_value, out_wire_id)| {
                        set::<B>(scope, *out_type_id, out_wire_id, output_value.clone())
                    },
                )?;
            }

            Call(name, out_ids, in_ids) => {
                let function = known_functions.get(name).ok_or("Unknown function")?;

                // simple checks.
                if !check_wire_ranges_with_counts(out_ids, &function.output_count) {
                    return Err(format!(
                        "Evaluation of a Call gate ({}): number of output wires mismatch.",
                        name
                    )
                    .into());
                }
                if !check_wire_ranges_with_counts(in_ids, &function.input_count) {
                    return Err(format!(
                        "Evaluation of a Call gate ({}): number of input wires mismatch.",
                        name
                    )
                    .into());
                }

                let out_ids_with_types = add_types_to_wire_ranges(out_ids, &function.output_count)?;
                let in_ids_with_types = add_types_to_wire_ranges(in_ids, &function.input_count)?;

                match &function.body {
                    FunctionBody::Gates(gates) => {
                        Self::ingest_subcircuit(
                            gates,
                            backend,
                            &out_ids_with_types,
                            &in_ids_with_types,
                            scope,
                            known_functions,
                            inputs,
                        )?;
                    }
                    FunctionBody::PluginBody(plugin_body) => {
                        // Retrieve input values
                        let mut input_values = vec![];
                        in_ids_with_types
                            .iter()
                            .try_for_each(|wirerange_with_type| {
                                (wirerange_with_type.first_id..=wirerange_with_type.last_id)
                                    .try_for_each::<_, Result<()>>(|wire_id| {
                                        let val =
                                            get::<B>(scope, wirerange_with_type.type_id, wire_id)?;
                                        input_values.push(val);
                                        Ok(())
                                    })
                            })?;

                        // Retrieve public inputs
                        let mut public_inputs = HashMap::new();
                        plugin_body
                            .public_count
                            .iter()
                            .try_for_each::<_, Result<()>>(|(type_id, count)| {
                                let values = Self::get_input_values(inputs, type_id, *count, true)?;
                                public_inputs.insert(*type_id, values);
                                Ok(())
                            })?;
                        // Retrieve private inputs
                        let mut private_inputs = HashMap::new();
                        plugin_body
                            .private_count
                            .iter()
                            .try_for_each::<_, Result<()>>(|(type_id, count)| {
                                let values =
                                    Self::get_input_values(inputs, type_id, *count, false)?;
                                private_inputs.insert(*type_id, values);
                                Ok(())
                            })?;

                        // Evaluate plugin
                        let out: Vec<B::Wire> = backend.evaluate_plugin(
                            &function.output_count,
                            &function.input_count,
                            &input_values,
                            &public_inputs,
                            &private_inputs,
                            plugin_body,
                        )?;

                        // Store output values
                        // Check out length
                        let expected_output_len = out_ids
                            .iter()
                            .map(|wirerange| wirerange.last_id - wirerange.first_id + 1)
                            .sum::<u64>();
                        if expected_output_len != u64::try_from(out.len())? {
                            // It is not possible
                            panic!(
                                "Evaluation of a Call gate ({}): number of output wires mismatch.",
                                name
                            );
                        }

                        // Store output values
                        let mut flatten_out_ids_with_types = vec![];
                        out_ids_with_types.iter().for_each(|wirerange| {
                            (wirerange.first_id..=wirerange.last_id).for_each(|wire_id| {
                                flatten_out_ids_with_types.push((wirerange.type_id, wire_id))
                            })
                        });
                        out.iter()
                            .zip(flatten_out_ids_with_types.iter())
                            .try_for_each(|(output_value, (type_id, output_wire))| {
                                set::<B>(scope, *type_id, *output_wire, output_value.clone())
                            })?;
                    }
                };
            }
        }
        Ok(())
    }

    /// This function is similar to `ingest_gate` except that it operates linearly on a subcircuit
    /// (i.e. a list of gates in an inner body of another gate).
    /// It will operate on an internal `scope`, and will write outputs produced by the subcircuit
    /// into the caller scope.
    /// Internally, it will call `ingest_gate` with the internal scope for each sub-gate.
    fn ingest_subcircuit(
        subcircuit: &[Gate],
        backend: &mut B,
        out_ids_with_types: &[WireRangeWithType],
        in_ids_with_types: &[WireRangeWithType],
        scope: &mut HashMap<(TypeId, WireId), B::Wire>,
        known_functions: &HashMap<String, FunctionDeclaration>,
        inputs: &mut EvaluatorInputs<B>,
    ) -> Result<()> {
        let mut new_scope: HashMap<(TypeId, WireId), B::Wire> = HashMap::new();

        // copy the inputs required by this function into the new scope, at the proper index
        let mut input_indexes = HashMap::new();
        out_ids_with_types.iter().for_each(|wirerange_with_type| {
            let idx = input_indexes
                .entry(wirerange_with_type.type_id)
                .or_insert(0);
            *idx += wirerange_with_type.last_id - wirerange_with_type.first_id + 1;
        });
        for wirerange_with_type in in_ids_with_types.iter() {
            for wire_id in wirerange_with_type.first_id..=wirerange_with_type.last_id {
                let idx = input_indexes
                    .entry(wirerange_with_type.type_id)
                    .or_insert(0);
                let i = get::<B>(scope, wirerange_with_type.type_id, wire_id)?;
                set::<B>(
                    &mut new_scope,
                    wirerange_with_type.type_id,
                    *idx,
                    backend.copy(&wirerange_with_type.type_id, i)?,
                )?;
                *idx += 1;
            }
        }

        // evaluate the subcircuit in the new scope.
        for gate in subcircuit {
            Self::ingest_gate(gate, backend, &mut new_scope, known_functions, inputs)?;
        }

        // copy the outputs produced from 'new_scope', into 'scope'
        let mut output_indexes = HashMap::new();
        for wirerange_with_type in out_ids_with_types.iter() {
            for wire_id in wirerange_with_type.first_id..=wirerange_with_type.last_id {
                let idx = output_indexes
                    .entry(wirerange_with_type.type_id)
                    .or_insert(0);
                let w = get::<B>(&new_scope, wirerange_with_type.type_id, *idx)?;
                set::<B>(
                    scope,
                    wirerange_with_type.type_id,
                    wire_id,
                    backend.copy(&wirerange_with_type.type_id, w)?,
                )?;
                *idx += 1;
            }
        }

        Ok(())
    }

    /// This helper function can be used to retrieve value of a given wire at some point
    /// if it has *NOT* been deleted yet, otherwise it will return an Err.
    pub fn get(&self, type_id: TypeId, wire_id: WireId) -> Result<&B::Wire> {
        get::<B>(&self.values, type_id, wire_id)
    }
}

fn set_public_input<I: ZKBackend>(
    backend: &mut I,
    scope: &mut HashMap<(TypeId, WireId), I::Wire>,
    type_id: TypeId,
    wire_id: WireId,
    value: I::TypeElement,
) -> Result<()> {
    let wire = backend.public_input(&type_id, value)?;
    set::<I>(scope, type_id, wire_id, wire)
}

fn set_private_input<I: ZKBackend>(
    backend: &mut I,
    scope: &mut HashMap<(TypeId, WireId), I::Wire>,
    type_id: TypeId,
    wire_id: WireId,
    value: Option<I::TypeElement>,
) -> Result<()> {
    let wire = backend.private_input(&type_id, value)?;
    set::<I>(scope, type_id, wire_id, wire)
}

fn set<I: ZKBackend>(
    scope: &mut HashMap<(TypeId, WireId), I::Wire>,
    type_id: TypeId,
    wire_id: WireId,
    wire: I::Wire,
) -> Result<()> {
    if scope.insert((type_id, wire_id), wire).is_some() {
        Err(format!(
            "Wire ({}: {}) already has a value in this scope.",
            type_id, wire_id
        )
        .into())
    } else {
        Ok(())
    }
}

pub fn get<I: ZKBackend>(
    scope: &HashMap<(TypeId, WireId), I::Wire>,
    type_id: TypeId,
    wire_id: WireId,
) -> Result<&I::Wire> {
    scope
        .get(&(type_id, wire_id))
        .ok_or_else(|| format!("No value given for wire ({}: {})", type_id, wire_id).into())
}

fn remove<I: ZKBackend>(
    scope: &mut HashMap<(TypeId, WireId), I::Wire>,
    type_id: TypeId,
    wire_id: WireId,
) -> Result<I::Wire> {
    scope
        .remove(&(type_id, wire_id))
        .ok_or_else(|| format!("No value given for wire ({}: {})", type_id, wire_id).into())
}

/// This is the default backend, evaluating a IR circuit in plaintext, meaning that it is not meant
/// for security purposes, will never ensure ZK properties, ...
/// It's used only for demo or tests.
/// Moreover, it's not optimized at all for modular operations (e.g. modular multiplications) and
/// can even be slower than a secure backend if the evaluated circuit contains a lot of such
/// operations.
/// Currently, this backend does not support 'verifier' mode, and requires private inputs to be provided.
#[derive(Default)]
pub struct PlaintextBackend {
    pub types: Vec<PlaintextType>,
}

/// A `PlaintextType` is similar to a `Type` except that the value in `Type::Field` is a `BigUint` instead of a `Value`
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum PlaintextType {
    Field(BigUint),
    // PluginType(name, operation, params)
    PluginType(String, String, Vec<String>),
}

impl ZKBackend for PlaintextBackend {
    type Wire = BigUint;
    type TypeElement = BigUint;

    fn from_bytes_le(val: &[u8]) -> Result<Self::TypeElement> {
        Ok(value_to_biguint(val))
    }

    fn set_types(&mut self, types: &[Type]) -> Result<()> {
        if !self.types.is_empty() {
            self.types = vec![];
        }
        types
            .iter()
            .try_for_each::<_, Result<()>>(|type_value| match type_value {
                Type::Field(modulo) => {
                    let biguint_val = &value_to_biguint(modulo);
                    if biguint_val.is_zero() {
                        return Err("Modulus cannot be zero.".into());
                    }
                    self.types.push(PlaintextType::Field(biguint_val.clone()));
                    Ok(())
                }
                Type::PluginType(name, operation, params) => {
                    self.types.push(PlaintextType::PluginType(
                        name.clone(),
                        operation.clone(),
                        params.clone(),
                    ));
                    Ok(())
                }
            })?;
        Ok(())
    }

    fn one(&self) -> Result<Self::TypeElement> {
        Ok(BigUint::one())
    }

    fn minus_one(&self, type_id: &TypeId) -> Result<Self::TypeElement> {
        let modulo = self.get_modulo(type_id)?;
        if modulo.is_zero() {
            return Err("Modulus is equal to zero.".into());
        }
        Ok(modulo - self.one()?)
    }

    fn zero(&self) -> Result<Self::TypeElement> {
        Ok(BigUint::zero())
    }

    fn copy(&mut self, _type_id: &TypeId, wire: &Self::Wire) -> Result<Self::Wire> {
        Ok(wire.clone())
    }

    fn constant(&mut self, _type_id: &TypeId, val: Self::TypeElement) -> Result<Self::Wire> {
        Ok(val)
    }

    fn assert_zero(&mut self, _type_id: &TypeId, wire: &Self::Wire) -> Result<()> {
        if wire.is_zero() {
            Ok(())
        } else {
            Err("AssertZero failed".into())
        }
    }

    fn add(&mut self, type_id: &TypeId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        let modulo = self.get_modulo(type_id)?;
        Ok((a + b) % modulo)
    }

    fn multiply(&mut self, type_id: &TypeId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        let modulo = self.get_modulo(type_id)?;
        Ok((a * b) % modulo)
    }

    fn add_constant(
        &mut self,
        type_id: &TypeId,
        a: &Self::Wire,
        b: Self::TypeElement,
    ) -> Result<Self::Wire> {
        let modulo = self.get_modulo(type_id)?;
        Ok((a + b) % modulo)
    }

    fn mul_constant(
        &mut self,
        type_id: &TypeId,
        a: &Self::Wire,
        b: Self::TypeElement,
    ) -> Result<Self::Wire> {
        let modulo = self.get_modulo(type_id)?;
        Ok((a * b) % modulo)
    }

    fn public_input(&mut self, type_id: &TypeId, val: Self::TypeElement) -> Result<Self::Wire> {
        self.constant(type_id, val)
    }

    fn private_input(
        &mut self,
        type_id: &TypeId,
        val: Option<Self::TypeElement>,
    ) -> Result<Self::Wire> {
        self.constant(
            type_id,
            val.unwrap_or_else(|| panic!("Missing private input value for PlaintextBackend")),
        )
    }

    fn gate_new(&mut self, _: &TypeId, _: WireId, _: WireId) -> Result<()> {
        Ok(())
    }

    /// See specs Section 3.5
    /// Inputs and outputs are expressed in big-endian representation.
    /// To convert `p` wires `x1...xp` in field `A` into `q` wires `y1...yq` in field `B`,
    /// we first convert the `p` wires in field `A` into a natural number
    /// `N =\sum_{i=1}^p x_i A^{p-1} mod B^q`.
    /// Then we represent `N` into `q` wires in field `B` `y1...yq`:
    /// `N = \sum{i=1}^q y_i B^{q-1}`.
    fn convert(
        &mut self,
        output_type: &TypeId,
        output_wire_count: u64,
        input_type: &TypeId,
        inputs: &[&Self::Wire],
    ) -> Result<Vec<Self::Wire>> {
        // Retrieve input/output moduli
        let input_modulo = self.get_modulo(input_type)?;
        let output_modulo = self.get_modulo(output_type)?;

        // Convert input to BigUint
        let mut number: BigUint = BigUint::zero();
        for input in inputs.iter() {
            number *= input_modulo;
            number += *input;
        }

        // Take the modulo (to avoid overflow)
        // number = number mod  (output_modulo ^ output_wire_count)
        let modulo = Pow::pow(output_modulo, output_wire_count);
        if number >= modulo {
            println!("WARNING: overflow during a conversion")
        }
        number %= modulo;

        // Convert BigUint value into output elements
        let mut result = vec![];
        for _ in 0..output_wire_count {
            result.insert(0, &number % output_modulo);
            number = &number / output_modulo;
        }

        Ok(result)
    }

    fn evaluate_plugin(
        &mut self,
        output_count: &[Count],
        input_count: &[Count],
        inputs: &[&Self::Wire],
        public_inputs: &HashMap<TypeId, Vec<Self::TypeElement>>,
        private_inputs: &HashMap<TypeId, Vec<Self::TypeElement>>,
        plugin_body: &PluginBody,
    ) -> Result<Vec<Self::Wire>> {
        evaluate_plugin_for_plaintext_backend(
            output_count,
            input_count,
            inputs,
            public_inputs,
            private_inputs,
            plugin_body,
            &self.types,
        )
    }
}

impl PlaintextBackend {
    fn get_modulo<'a>(&'a self, type_id: &'a TypeId) -> Result<&'a BigUint> {
        let plaintext_type = self
            .types
            .get(*type_id as usize)
            .ok_or(format!("Type id {} is not defined.", *type_id))?;
        match plaintext_type {
            PlaintextType::Field(modulo) => Ok(modulo),
            PlaintextType::PluginType(_, _, _) => {
                Err("Cannot retrieve a modulo from a PluginType".into())
            }
        }
    }
}

#[test]
fn test_evaluator() -> Result<()> {
    use crate::consumers::evaluator::Evaluator;
    use crate::producers::examples::*;

    let relation = example_relation();
    let public_inputs = example_public_inputs();
    let private_inputs = example_private_inputs();

    let mut zkbackend = PlaintextBackend::default();
    let mut simulator: Evaluator<PlaintextBackend> = Evaluator::default();

    simulator.ingest_public_inputs(&public_inputs)?;
    simulator.ingest_private_inputs(&private_inputs)?;
    simulator.ingest_relation(&relation, &mut zkbackend)?;

    assert_eq!(simulator.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_evaluator_as_verifier() -> Result<()> {
    /// This test simply checks that the Evaluator code could run with any ZKInterpreter without issue
    use crate::consumers::evaluator::Evaluator;
    use crate::producers::examples::*;
    use std::convert::TryFrom;

    let relation = example_relation();
    let public_inputs = example_public_inputs();

    struct VerifierInterpreter {}
    impl ZKBackend for VerifierInterpreter {
        type Wire = i64;
        type TypeElement = BigUint;
        fn from_bytes_le(_val: &[u8]) -> Result<Self::TypeElement> {
            Ok(BigUint::zero())
        }
        fn set_types(&mut self, _moduli: &[Type]) -> Result<()> {
            Ok(())
        }
        fn one(&self) -> Result<Self::TypeElement> {
            Ok(BigUint::one())
        }
        fn zero(&self) -> Result<Self::TypeElement> {
            Ok(BigUint::zero())
        }
        fn minus_one(&self, _type_id: &TypeId) -> Result<Self::TypeElement> {
            Ok(BigUint::one())
        }
        fn copy(&mut self, _type_id: &TypeId, wire: &Self::Wire) -> Result<Self::Wire> {
            Ok(*wire)
        }
        fn constant(&mut self, _type_id: &TypeId, _val: Self::TypeElement) -> Result<Self::Wire> {
            Ok(0)
        }
        fn assert_zero(&mut self, _type_id: &TypeId, _wire: &Self::Wire) -> Result<()> {
            Ok(())
        }
        fn add(
            &mut self,
            _type_id: &TypeId,
            _a: &Self::Wire,
            _b: &Self::Wire,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn multiply(
            &mut self,
            _type_id: &TypeId,
            _a: &Self::Wire,
            _b: &Self::Wire,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn add_constant(
            &mut self,
            _type_id: &TypeId,
            _a: &Self::Wire,
            _b: Self::TypeElement,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn mul_constant(
            &mut self,
            _type_id: &TypeId,
            _a: &Self::Wire,
            _b: Self::TypeElement,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn public_input(
            &mut self,
            _type_id: &TypeId,
            _val: Self::TypeElement,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn private_input(
            &mut self,
            _type_id: &TypeId,
            _val: Option<Self::TypeElement>,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn gate_new(&mut self, _: &TypeId, _: WireId, _: WireId) -> Result<()> {
            Ok(())
        }
        fn convert(
            &mut self,
            _output_type: &TypeId,
            output_wire_count: u64,
            _input_type: &TypeId,
            _inputs: &[&Self::Wire],
        ) -> Result<Vec<Self::Wire>> {
            Ok(vec![0; usize::try_from(output_wire_count)?])
        }
        fn evaluate_plugin(
            &mut self,
            output_count: &[Count],
            _input_count: &[Count],
            _inputs: &[&Self::Wire],
            _public_inputs: &HashMap<TypeId, Vec<Self::TypeElement>>,
            _private_inputs: &HashMap<TypeId, Vec<Self::TypeElement>>,
            _plugin_body: &PluginBody,
        ) -> Result<Vec<Self::Wire>> {
            let count = output_count.iter().map(|count| count.count).sum::<u64>();
            Ok(vec![0; usize::try_from(count)?])
        }
    }

    let mut zkbackend = VerifierInterpreter {};
    let mut simulator = Evaluator::default();
    simulator.ingest_public_inputs(&public_inputs)?;
    simulator.ingest_relation(&relation, &mut zkbackend)?;

    assert_eq!(simulator.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_evaluator_wrong_result() -> Result<()> {
    use crate::consumers::evaluator::Evaluator;
    use crate::producers::examples::*;

    let relation = example_relation();
    let public_inputs = example_public_inputs();
    let private_inputs = example_private_inputs_incorrect();

    let mut zkbackend = PlaintextBackend::default();
    let mut simulator = Evaluator::default();
    let _ = simulator.ingest_public_inputs(&public_inputs);
    let _ = simulator.ingest_private_inputs(&private_inputs);
    let should_be_err = simulator.ingest_relation(&relation, &mut zkbackend);

    assert!(should_be_err.is_err());
    assert_eq!(
        "Wire (0: 8) should be 0, while it is not",
        should_be_err.err().unwrap().to_string()
    );

    Ok(())
}

#[test]
fn test_evaluator_conversion() {
    use crate::consumers::evaluator::Evaluator;
    use crate::producers::examples::literal32;
    use crate::structs::conversion::Conversion;
    use crate::structs::IR_VERSION;

    let public_inputs = PublicInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(literal32(7)),
        inputs: vec![vec![2], vec![4]],
    };
    let private_inputs = PrivateInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(literal32(101)),
        inputs: vec![vec![2], vec![81]],
    };
    let relation = Relation {
        version: IR_VERSION.to_string(),
        plugins: vec![],
        types: vec![Type::Field(literal32(7)), Type::Field(literal32(101))],
        conversions: vec![Conversion::new(Count::new(0, 2), Count::new(1, 2))],
        directives: vec![
            Directive::Gate(Gate::Private(1, 0)), // 2
            Directive::Gate(Gate::Private(1, 1)), // 81
            // (2*101 + 81) mod 7^2 = 38
            // 38 = 5*7 + 3
            Directive::Gate(Gate::Convert(0, 0, 1, 1, 0, 1)),
            Directive::Gate(Gate::Public(0, 2)),    // 6
            Directive::Gate(Gate::Add(0, 3, 0, 2)), // 5 + 2 = 0 mod 7
            Directive::Gate(Gate::AssertZero(0, 3)),
            Directive::Gate(Gate::Public(0, 4)),    // 2
            Directive::Gate(Gate::Add(0, 5, 1, 4)), // 3 + 4 = 0 mod 7
            Directive::Gate(Gate::AssertZero(0, 5)),
            Directive::Gate(Gate::Delete(0, 0, 5)),
            Directive::Gate(Gate::Delete(1, 0, 0)),
        ],
    };

    let mut zkbackend = PlaintextBackend::default();
    let mut simulator = Evaluator::default();
    simulator.ingest_public_inputs(&public_inputs).unwrap();
    simulator.ingest_private_inputs(&private_inputs).unwrap();
    simulator
        .ingest_relation(&relation, &mut zkbackend)
        .unwrap();

    let public_inputs = PublicInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(literal32(7)),
        inputs: vec![vec![2], vec![4]],
    };
    let private_inputs = PrivateInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(literal32(101)),
        inputs: vec![vec![2], vec![81]],
    };
    let relation = Relation {
        version: IR_VERSION.to_string(),
        plugins: vec![],
        types: vec![Type::Field(literal32(7)), Type::Field(literal32(101))],
        conversions: vec![Conversion::new(Count::new(0, 2), Count::new(1, 2))],
        directives: vec![
            Directive::Gate(Gate::Private(1, 0)), // 2
            Directive::Gate(Gate::Private(1, 1)), // 81
            // (2*101 + 81) mod 7^2 = 38
            // 38 = 5*7 + 3
            // Violation: in_ids is empty (in_first_id > in_last_id)
            Directive::Gate(Gate::Convert(0, 0, 1, 1, 1, 0)),
            Directive::Gate(Gate::Public(0, 2)),    // 6
            Directive::Gate(Gate::Add(0, 3, 0, 2)), // 5 + 2 = 0 mod 7
            Directive::Gate(Gate::AssertZero(0, 3)),
            Directive::Gate(Gate::Public(0, 4)),    // 2
            Directive::Gate(Gate::Add(0, 5, 1, 4)), // 3 + 4 = 0 mod 7
            Directive::Gate(Gate::AssertZero(0, 5)),
            Directive::Gate(Gate::Delete(0, 0, 5)),
            Directive::Gate(Gate::Delete(1, 0, 0)),
        ],
    };

    let mut zkbackend = PlaintextBackend::default();
    let mut simulator = Evaluator::default();
    simulator.ingest_public_inputs(&public_inputs).unwrap();
    simulator.ingest_private_inputs(&private_inputs).unwrap();
    let should_be_err = simulator.ingest_relation(&relation, &mut zkbackend);
    assert!(should_be_err.is_err());
    assert_eq!(
        "Evaluation of a Convert gate:: in_last_id (0) must be greater than in_first_id (1).",
        should_be_err.err().unwrap().to_string()
    );
}
