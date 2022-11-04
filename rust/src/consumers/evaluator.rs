use crate::plugins::evaluate_plugin::evaluate_plugin_for_plaintext_backend;
use crate::structs::count::{wirelist_to_count_list, CountList};
use crate::structs::function::FunctionBody;
use crate::structs::plugin::PluginBody;
use crate::structs::wire::{expand_wirelist, is_one_type_wirelist, wirelist_len, WireList};
use crate::{
    Gate, Header, Message, PrivateInputs, PublicInputs, Relation, Result, TypeId, Value, WireId,
};
use num_bigint::BigUint;
use num_traits::identities::{One, Zero};
use std::collections::{HashMap, VecDeque};

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
    fn set_types(&mut self, moduli: &[Value]) -> Result<()>;

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

    /// Evaluate plugin defined in `plugin_body` on `inputs`
    /// The result is stored in new wires.
    /// The result must be compliant with `output_wire_count`.
    /// If the plugin is unknown or if `input_count`, `output_count`, `inputs` or `plugin_body`
    /// are not compliant with the plugin specifications,
    /// then `evaluate_plugin` returns an error.
    fn evaluate_plugin(
        &mut self,
        output_count: &CountList,
        input_count: &CountList,
        inputs: &[&Self::Wire],
        plugin_body: &PluginBody,
    ) -> Result<Vec<Self::Wire>>;
}

/// This structure defines a function as defined in the circuit, but without the name.
/// It's mainly used to retrieve information from the name.
struct FunctionDeclaration {
    body: FunctionBody,
    output_count: CountList,
    input_count: CountList,
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
    params: EvaluatorParams,

    verified_at_least_one_gate: bool,
    found_error: Option<String>,
}

impl<B: ZKBackend> Default for Evaluator<B> {
    fn default() -> Self {
        Evaluator {
            values: Default::default(),
            params: Default::default(),
            inputs: Default::default(),
            verified_at_least_one_gate: false,
            found_error: None,
        }
    }
}

pub struct EvaluatorInputs<B: ZKBackend> {
    public_inputs_queue: Vec<VecDeque<B::TypeElement>>,
    private_inputs_queue: Vec<VecDeque<B::TypeElement>>,
}

impl<B: ZKBackend> Default for EvaluatorInputs<B> {
    fn default() -> Self {
        EvaluatorInputs {
            public_inputs_queue: Default::default(),
            private_inputs_queue: Default::default(),
        }
    }
}

#[derive(Default)]
pub struct EvaluatorParams {
    moduli: Vec<BigUint>,
    // name => (body, output_count, input_count)
    known_functions: HashMap<String, FunctionDeclaration>,
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
        if !self.verified_at_least_one_gate {
            violations.push("Did not receive any gate to verify.".to_string());
        }
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

    fn ingest_header(&mut self, header: &Header) -> Result<()> {
        if !self.params.moduli.is_empty() {
            // Header has already been ingested !
            return Ok(());
        }
        for modulo in &header.types {
            self.params.moduli.push(BigUint::from_bytes_le(modulo));
            self.inputs.public_inputs_queue.push(VecDeque::new());
            self.inputs.private_inputs_queue.push(VecDeque::new());
        }
        Ok(())
    }

    /// Ingest a `PublicInputs` message, and returns a `Result` whether ot nor an error
    /// was encountered. It stores the public input values in a pool.
    pub fn ingest_public_inputs(&mut self, public_inputs: &PublicInputs) -> Result<()> {
        self.ingest_header(&public_inputs.header)?;

        for (i, inputs) in public_inputs.inputs.iter().enumerate() {
            let values = self
                .inputs
                .public_inputs_queue
                .get_mut(i)
                .ok_or("No enough elements in public_inputs_queue")?;
            for value in &inputs.values {
                values.push_back(B::from_bytes_le(value)?);
            }
        }
        Ok(())
    }

    /// Ingest a `PrivateInputs` message, and returns a `Result` whether ot nor an error
    /// was encountered. It stores the private input values in a pool.
    pub fn ingest_private_inputs(&mut self, private_inputs: &PrivateInputs) -> Result<()> {
        self.ingest_header(&private_inputs.header)?;

        for (i, inputs) in private_inputs.inputs.iter().enumerate() {
            let values = self
                .inputs
                .private_inputs_queue
                .get_mut(i)
                .ok_or("No enough elements in private_inputs_queue")?;
            for value in &inputs.values {
                values.push_back(B::from_bytes_le(value)?);
            }
        }
        Ok(())
    }

    /// Ingest a `Relation` message
    pub fn ingest_relation(&mut self, relation: &Relation, backend: &mut B) -> Result<()> {
        self.ingest_header(&relation.header)?;
        backend.set_types(&relation.header.types)?;

        if !relation.gates.is_empty() {
            self.verified_at_least_one_gate = true;
        }

        for f in relation.functions.iter() {
            self.params.known_functions.insert(
                f.name.clone(),
                FunctionDeclaration {
                    body: f.body.clone(),
                    output_count: f.output_count.clone(),
                    input_count: f.input_count.clone(),
                },
            );
        }

        for gate in &relation.gates {
            Self::ingest_gate(
                gate,
                backend,
                &mut self.values,
                &self.params,
                &mut self.inputs,
            )?;
        }
        Ok(())
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
        params: &EvaluatorParams,
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

            PublicInput(type_id, out) => {
                let public_inputs_for_type = inputs
                    .public_inputs_queue
                    .get_mut(*type_id as usize)
                    .ok_or(format!(
                        "Unknown type id {} when evaluating an PublicInput gate",
                        *type_id
                    ))?;
                let val = if let Some(inner) = public_inputs_for_type.pop_front() {
                    inner
                } else {
                    return Err("Not enough public inputs to consume".into());
                };
                set_public_input(backend, scope, *type_id, *out, val)?;
            }

            PrivateInput(type_id, out) => {
                let private_inputs_for_type = inputs
                    .private_inputs_queue
                    .get_mut(*type_id as usize)
                    .ok_or(format!(
                        "Unknown type id {} when evaluating a PrivateInput gate",
                        *type_id
                    ))?;
                let val = private_inputs_for_type.pop_front();
                set_private_input(backend, scope, *type_id, *out, val)?;
            }

            New(type_id, first, last) => {
                backend.gate_new(type_id, *first, *last)?;
            }

            Delete(type_id, first, last) => {
                let last_value = last.unwrap_or(*first);
                for current in *first..=last_value {
                    remove::<B>(scope, *type_id, current)?;
                }
            }

            Convert(output_wires, input_wires) => {
                let output_type = match is_one_type_wirelist(output_wires) {
                    Err(err) => {
                        return Err(
                            format!("Error with output wires of a Convert gate: {}", err).into(),
                        )
                    }
                    Ok(val) => val,
                };

                let input_type = match is_one_type_wirelist(input_wires) {
                    Err(err) => {
                        return Err(
                            format!("Error with input wires of a Convert gate: {}", err).into()
                        )
                    }
                    Ok(val) => val,
                };

                let expanded_input_wires = expand_wirelist(input_wires)?;
                let mut input_values = vec![];
                for (_, input_wire) in expanded_input_wires.iter() {
                    let val = get!(input_type, *input_wire)?;
                    input_values.push(val);
                }
                let out: Vec<B::Wire> = backend.convert(
                    &output_type,
                    wirelist_len(output_wires) as u64,
                    &input_type,
                    &input_values,
                )?;

                let expanded_output_wires = expand_wirelist(output_wires)?;
                if expanded_output_wires.len() != out.len() {
                    // It is not possible
                    panic!("In a convert gate, out and expanded_output_wires have not the same length !");
                }
                out.iter().zip(expanded_output_wires.iter()).try_for_each(
                    |(output_value, (_, output_wire))| {
                        set::<B>(scope, output_type, *output_wire, output_value.clone())
                    },
                )?;
            }

            Call(name, output_wires, input_wires) => {
                let function = params.known_functions.get(name).ok_or("Unknown function")?;

                // simple checks.
                let output_count: CountList = wirelist_to_count_list(output_wires);
                if output_count != function.output_count {
                    return Err(format!("Wrong number of output variables in call to function {} (Expected {:?} / Got {:?}).", name, function.output_count, output_count).into());
                }
                let input_count: CountList = wirelist_to_count_list(input_wires);
                if input_count != function.input_count {
                    return Err(format!("Wrong number of input variables in call to function {} (Expected {:?} / Got {:?}).", name, function.input_count, input_count).into());
                }

                match &function.body {
                    FunctionBody::Gates(gates) => {
                        Self::ingest_subcircuit(
                            gates,
                            backend,
                            output_wires,
                            input_wires,
                            scope,
                            params,
                            inputs,
                        )?;
                    }
                    FunctionBody::PluginBody(plugin_body) => {
                        // Retrieve input values
                        let expanded_input_wires = expand_wirelist(input_wires)?;
                        let mut input_values = vec![];
                        for (type_id, input_wire) in expanded_input_wires.iter() {
                            let val = get!(*type_id, *input_wire)?;
                            input_values.push(val);
                        }
                        // Evaluate plugin
                        let out: Vec<B::Wire> = backend.evaluate_plugin(
                            &output_count,
                            &input_count,
                            &input_values,
                            plugin_body,
                        )?;
                        // Store output values
                        let expanded_output_wires = expand_wirelist(output_wires)?;
                        if expanded_output_wires.len() != out.len() {
                            // It is not possible
                            panic!("During a plugin evaluation, out and expanded_output_wires have not the same length !");
                        }
                        out.iter().zip(expanded_output_wires.iter()).try_for_each(
                            |(output_value, (type_id, output_wire))| {
                                set::<B>(scope, *type_id, *output_wire, output_value.clone())
                            },
                        )?;
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
        output_list: &WireList,
        input_list: &WireList,
        scope: &mut HashMap<(TypeId, WireId), B::Wire>,
        params: &EvaluatorParams,
        inputs: &mut EvaluatorInputs<B>,
    ) -> Result<()> {
        let mut new_scope: HashMap<(TypeId, WireId), B::Wire> = HashMap::new();

        // copy the inputs required by this function into the new scope, at the proper index
        let expanded_inputs = expand_wirelist(input_list)?;
        let mut input_indexes = wirelist_to_count_list(output_list);
        for (input_type, input_wire) in expanded_inputs.iter() {
            let idx = input_indexes.entry(*input_type).or_insert(0);
            let i = get::<B>(scope, *input_type, *input_wire)?;
            set::<B>(
                &mut new_scope,
                *input_type,
                *idx,
                backend.copy(input_type, i)?,
            )?;
            *idx += 1;
        }
        // evaluate the subcircuit in the new scope.
        for gate in subcircuit {
            Self::ingest_gate(gate, backend, &mut new_scope, params, inputs)?;
        }
        // copy the outputs produced from 'new_scope', into 'scope'
        let expanded_outputs = expand_wirelist(output_list)?;
        let mut output_indexes: CountList = HashMap::new();
        for (output_type, output_wire) in expanded_outputs.iter() {
            let idx = output_indexes.entry(*output_type).or_insert(0);
            let w = get::<B>(&new_scope, *output_type, *idx)?;
            set::<B>(
                scope,
                *output_type,
                *output_wire,
                backend.copy(output_type, w)?,
            )?;
            *idx += 1;
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

pub fn get_modulo<'a>(type_id: &'a TypeId, moduli: &'a [BigUint]) -> Result<&'a BigUint> {
    let modulo = moduli.get(*type_id as usize);
    if let Some(value) = modulo {
        Ok(value)
    } else {
        Err(format!("Type id {} is not defined.", *type_id).into())
    }
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
    pub m: Vec<BigUint>,
}

impl ZKBackend for PlaintextBackend {
    type Wire = BigUint;
    type TypeElement = BigUint;

    fn from_bytes_le(val: &[u8]) -> Result<Self::TypeElement> {
        Ok(BigUint::from_bytes_le(val))
    }

    fn set_types(&mut self, moduli: &[Value]) -> Result<()> {
        if !self.m.is_empty() {
            self.m = vec![];
        }
        for val in moduli {
            let biguint_val = &BigUint::from_bytes_le(val);
            if biguint_val.is_zero() {
                return Err("Modulus cannot be zero.".into());
            }
            self.m.push(biguint_val.clone());
        }
        Ok(())
    }

    fn one(&self) -> Result<Self::TypeElement> {
        Ok(BigUint::one())
    }

    fn minus_one(&self, type_id: &TypeId) -> Result<Self::TypeElement> {
        let modulo = get_modulo(type_id, &self.m)?;
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
        let modulo = get_modulo(type_id, &self.m)?;
        Ok((a + b) % modulo)
    }

    fn multiply(&mut self, type_id: &TypeId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        let modulo = get_modulo(type_id, &self.m)?;
        Ok((a * b) % modulo)
    }

    fn add_constant(
        &mut self,
        type_id: &TypeId,
        a: &Self::Wire,
        b: Self::TypeElement,
    ) -> Result<Self::Wire> {
        let modulo = get_modulo(type_id, &self.m)?;
        Ok((a + b) % modulo)
    }

    fn mul_constant(
        &mut self,
        type_id: &TypeId,
        a: &Self::Wire,
        b: Self::TypeElement,
    ) -> Result<Self::Wire> {
        let modulo = get_modulo(type_id, &self.m)?;
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

    fn convert(
        &mut self,
        output_type: &TypeId,
        output_wire_count: u64,
        input_type: &TypeId,
        inputs: &[&Self::Wire],
    ) -> Result<Vec<Self::Wire>> {
        // Retrieve input/output moduli
        let input_modulo = get_modulo(input_type, &self.m)?;
        let output_modulo = get_modulo(output_type, &self.m)?;

        // Convert input to BigUint
        let mut number: BigUint = BigUint::zero();
        for input in inputs.iter() {
            number *= input_modulo;
            number += *input;
        }

        // Convert BigUint value into output element
        let mut result = vec![];
        for _ in 0..output_wire_count {
            result.insert(0, &number % output_modulo);
            number = &number / output_modulo;
        }

        if !number.is_zero() {
            return Err("Impossible conversion".into());
        }
        Ok(result)
    }

    fn evaluate_plugin(
        &mut self,
        output_count: &CountList,
        input_count: &CountList,
        inputs: &[&Self::Wire],
        plugin_body: &PluginBody,
    ) -> Result<Vec<Self::Wire>> {
        evaluate_plugin_for_plaintext_backend(
            output_count,
            input_count,
            inputs,
            plugin_body,
            &self.m,
        )
    }
}

#[test]
fn test_evaluator() -> crate::Result<()> {
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
fn test_evaluator_as_verifier() -> crate::Result<()> {
    /// This test simply checks that the Evaluator code could run with any ZKInterpreter without issue
    use crate::consumers::evaluator::Evaluator;
    use crate::producers::examples::*;
    use crate::structs::count::count_len;
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
        fn set_types(&mut self, _moduli: &[Value]) -> Result<()> {
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
            output_count: &CountList,
            _input_count: &CountList,
            _inputs: &[&Self::Wire],
            _plugin_body: &PluginBody,
        ) -> Result<Vec<Self::Wire>> {
            let count = count_len(output_count);
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
fn test_evaluator_wrong_result() -> crate::Result<()> {
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
