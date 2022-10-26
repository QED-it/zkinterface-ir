use crate::structs::count::{wirelist_to_count_list, CountList};
use crate::structs::wire::{expand_wirelist, is_one_field_wirelist, wirelist_len, WireList};
use crate::{
    FieldId, Gate, Header, Message, PrivateInputs, PublicInputs, Relation, Result, Value, WireId,
};
use num_bigint::BigUint;
use num_traits::identities::{One, Zero};
use std::collections::{HashMap, VecDeque};

/// The `ZKBackend` trait should be implemented by any backend that wants to evaluate SIEVE IR circuits.
/// It has to define 2 types:
///  - `Wire`: represents a variable in the circuit. It should implement the `Clone` trait.
///  - `FieldElement`: represents elements of the underlying field. Mainly used when importing
///                    public/private inputs from the corresponding pools.
/// see `PlaintextBackend` for an working example of an implementation.
pub trait ZKBackend {
    type Wire: Clone;
    /// Usually a big Integer type.
    type FieldElement: 'static + Clone;

    /// Imports a `Self::FieldElement` from a byte buffer, in little endian.
    /// If the buffer does not represent an element of the underlying field, then
    /// it returns an Err.
    fn from_bytes_le(val: &[u8]) -> Result<Self::FieldElement>;
    /// Set the underlying field of the running backend.
    /// If the field is not compatible with this ZKBackend, then it should return Err
    fn set_fields(&mut self, moduli: &[Value]) -> Result<()>;

    /// Returns a `FieldElement` representing '1' in the underlying field.
    fn one(&self) -> Result<Self::FieldElement>;
    /// Returns a `FieldElement` representing '-1' in the underlying field.
    fn minus_one(&self, field_id: &FieldId) -> Result<Self::FieldElement>;
    /// Returns a `FieldElement` representing '0' in the underlying field.
    fn zero(&self) -> Result<Self::FieldElement>;

    // Returns a new instance of a given Wire id
    fn copy(&mut self, field_id: &FieldId, wire: &Self::Wire) -> Result<Self::Wire>;

    /// Imports a constant value into a new `Self::Wire`.
    fn constant(&mut self, field_id: &FieldId, val: Self::FieldElement) -> Result<Self::Wire>;
    /// This functions checks whether the given `Self::Wire` is indeed zero or not.
    /// Depending upon whether `self` is in prover, or verifier mode, this function should act
    /// accordingly. E.g. in prover mode, if the wire is not zero, then this function should
    /// return an Err.
    fn assert_zero(&mut self, field_id: &FieldId, wire: &Self::Wire) -> Result<()>;

    /// Adds two wires into a new wire.
    fn add(&mut self, field_id: &FieldId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire>;
    /// Multiplies two wires into a new wire.
    fn multiply(
        &mut self,
        field_id: &FieldId,
        a: &Self::Wire,
        b: &Self::Wire,
    ) -> Result<Self::Wire>;
    /// Adds a given wire by a constant `Self::FieldElement` into a new wire.
    fn add_constant(
        &mut self,
        field_id: &FieldId,
        a: &Self::Wire,
        b: Self::FieldElement,
    ) -> Result<Self::Wire>;
    /// Multiplies a given wire by a constant `Self::FieldElement` into a new wire.
    fn mul_constant(
        &mut self,
        field_id: &FieldId,
        a: &Self::Wire,
        b: Self::FieldElement,
    ) -> Result<Self::Wire>;

    /// This functions declares a new public input variable owning the value given as parameter,
    /// which should be stored in a new wire.
    fn public_input(&mut self, field_id: &FieldId, val: Self::FieldElement) -> Result<Self::Wire>;
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
        field_id: &FieldId,
        val: Option<Self::FieldElement>,
    ) -> Result<Self::Wire>;

    /// Convert `inputs` from field `input_field_id` into field `output_field_id`.
    /// The result is stored in new wires.
    /// The result must contain at most `output_wire_count` wires.
    fn convert(
        &mut self,
        output_field_id: &FieldId,
        output_wire_count: u64,
        input_field_id: &FieldId,
        inputs: &[&Self::Wire],
    ) -> Result<Vec<Self::Wire>>;
}

/// This structure defines a function as defined in the circuit, but without the name.
/// It's mainly used to retrieve information from the name.
struct FunctionDeclaration {
    subcircuit: Vec<Gate>,
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
    values: HashMap<(FieldId, WireId), B::Wire>,
    moduli: Vec<BigUint>,
    public_inputs_queue: Vec<VecDeque<B::FieldElement>>,
    private_inputs_queue: Vec<VecDeque<B::FieldElement>>,

    // name => (subcircuit, output_count, input_count)
    known_functions: HashMap<String, FunctionDeclaration>,

    verified_at_least_one_gate: bool,
    found_error: Option<String>,
}

impl<B: ZKBackend> Default for Evaluator<B> {
    fn default() -> Self {
        Evaluator {
            values: Default::default(),
            moduli: vec![],
            public_inputs_queue: vec![],
            private_inputs_queue: vec![],
            known_functions: Default::default(),
            verified_at_least_one_gate: false,
            found_error: None,
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
        if !self.moduli.is_empty() {
            // Header has already been ingested !
            return Ok(());
        }
        for field in &header.fields {
            self.moduli.push(BigUint::from_bytes_le(field));
            self.public_inputs_queue.push(VecDeque::new());
            self.private_inputs_queue.push(VecDeque::new());
        }
        Ok(())
    }

    /// Ingest a `PublicInputs` message, and returns a `Result` whether ot nor an error
    /// was encountered. It stores the public input values in a pool.
    pub fn ingest_public_inputs(&mut self, public_inputs: &PublicInputs) -> Result<()> {
        self.ingest_header(&public_inputs.header)?;

        for (i, inputs) in public_inputs.inputs.iter().enumerate() {
            let values = self
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
        backend.set_fields(&relation.header.fields)?;

        if !relation.gates.is_empty() {
            self.verified_at_least_one_gate = true;
        }

        for f in relation.functions.iter() {
            self.known_functions.insert(
                f.name.clone(),
                FunctionDeclaration {
                    subcircuit: f.body.clone(),
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
                &self.known_functions,
                &self.moduli,
                &mut self.public_inputs_queue,
                &mut self.private_inputs_queue,
            )?;
        }
        Ok(())
    }

    /// This function ingests one gate at a time (but can call itself recursively)
    /// - `scope` contains the list of existing wires with their respective value. It will be
    ///    augmented if this gate produces outputs, or reduced if this is a `GateFree`
    /// - `known_functions` is the map of functions defined in previous or current `Relation` message
    ///    current gate is a `GateFor`
    /// - `moduli` is used mainly in convert gates.
    /// - `public_inputs` and `private_inputs` are the public_inputs and private_inputs pools, implemented as Queues.
    ///    They will be consumed whenever necessary.
    fn ingest_gate(
        gate: &Gate,
        backend: &mut B,
        scope: &mut HashMap<(FieldId, WireId), B::Wire>,
        known_functions: &HashMap<String, FunctionDeclaration>,
        moduli: &[BigUint],
        public_inputs: &mut Vec<VecDeque<B::FieldElement>>,
        private_inputs: &mut Vec<VecDeque<B::FieldElement>>,
    ) -> Result<()> {
        use Gate::*;

        macro_rules! get {
            ($field_id:expr, $wire_id:expr) => {{
                get::<B>(scope, $field_id, $wire_id)
            }};
        }

        macro_rules! set {
            ($field_id: expr, $wire_id:expr, $wire_name:expr) => {{
                set::<B>(scope, $field_id, $wire_id, $wire_name)
            }};
        }

        match gate {
            Constant(field_id, out, value) => {
                let wire = backend.constant(field_id, B::from_bytes_le(value)?)?;
                set!(*field_id, *out, wire)?;
            }

            AssertZero(field_id, inp) => {
                let inp_wire = get!(*field_id, *inp)?;
                if backend.assert_zero(field_id, inp_wire).is_err() {
                    return Err(format!(
                        "Wire ({}: {}) should be 0, while it is not",
                        *field_id, *inp
                    )
                    .into());
                }
            }

            Copy(field_id, out, inp) => {
                let in_wire = get!(*field_id, *inp)?;
                let out_wire = backend.copy(field_id, in_wire)?;
                set!(*field_id, *out, out_wire)?;
            }

            Add(field_id, out, left, right) => {
                let l = get!(*field_id, *left)?;
                let r = get!(*field_id, *right)?;
                let sum = backend.add(field_id, l, r)?;
                set!(*field_id, *out, sum)?;
            }

            Mul(field_id, out, left, right) => {
                let l = get!(*field_id, *left)?;
                let r = get!(*field_id, *right)?;
                let prod = backend.multiply(field_id, l, r)?;
                set!(*field_id, *out, prod)?;
            }

            AddConstant(field_id, out, inp, constant) => {
                let l = get!(*field_id, *inp)?;
                let r = B::from_bytes_le(constant)?;
                let sum = backend.add_constant(field_id, l, r)?;
                set!(*field_id, *out, sum)?;
            }

            MulConstant(field_id, out, inp, constant) => {
                let l = get!(*field_id, *inp)?;
                let r = B::from_bytes_le(constant)?;
                let prod = backend.mul_constant(field_id, l, r)?;
                set!(*field_id, *out, prod)?;
            }

            PublicInput(field_id, out) => {
                let public_inputs_for_field =
                    public_inputs.get_mut(*field_id as usize).ok_or(format!(
                        "Unknown field {} when evaluating an PublicInput gate",
                        *field_id
                    ))?;
                let val = if let Some(inner) = public_inputs_for_field.pop_front() {
                    inner
                } else {
                    return Err("Not enough public inputs to consume".into());
                };
                set_public_input(backend, scope, *field_id, *out, val)?;
            }

            PrivateInput(field_id, out) => {
                let private_inputs_for_field =
                    private_inputs.get_mut(*field_id as usize).ok_or(format!(
                        "Unknown field {} when evaluating a PrivateInput gate",
                        *field_id
                    ))?;
                let val = private_inputs_for_field.pop_front();
                set_private_input(backend, scope, *field_id, *out, val)?;
            }

            Free(field_id, first, last) => {
                let last_value = last.unwrap_or(*first);
                for current in *first..=last_value {
                    remove::<B>(scope, *field_id, current)?;
                }
            }

            Convert(output_wires, input_wires) => {
                let output_field = match is_one_field_wirelist(output_wires) {
                    Err(err) => {
                        return Err(
                            format!("Error with output wires of a Convert gate: {}", err).into(),
                        )
                    }
                    Ok(val) => val,
                };

                let input_field = match is_one_field_wirelist(input_wires) {
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
                    let val = get!(input_field, *input_wire)?;
                    input_values.push(val);
                }
                let out: Vec<<B as ZKBackend>::Wire> = backend.convert(
                    &output_field,
                    wirelist_len(output_wires) as u64,
                    &input_field,
                    &input_values,
                )?;

                let expanded_output_wires = expand_wirelist(output_wires)?;
                if expanded_output_wires.len() != out.len() {
                    // It is not possible
                    panic!("In a convert gate, out and expanded_output_wires have not the same length !");
                }
                out.iter().zip(expanded_output_wires.iter()).try_for_each(
                    |(output_value, (_, output_wire))| {
                        // let value = *(output_value.clone());
                        set::<B>(scope, output_field, *output_wire, output_value.clone())
                    },
                )?;
            }

            Call(name, output_wires, input_wires) => {
                let function = known_functions.get(name).ok_or("Unknown function")?;

                // simple checks.
                let output_count: CountList = wirelist_to_count_list(output_wires);
                if output_count != function.output_count {
                    return Err(format!("Wrong number of output variables in call to function {} (Expected {:?} / Got {:?}).", name, function.output_count, output_count).into());
                }
                let input_count: CountList = wirelist_to_count_list(input_wires);
                if input_count != function.input_count {
                    return Err(format!("Wrong number of input variables in call to function {} (Expected {:?} / Got {:?}).", name, function.input_count, input_count).into());
                }

                Self::ingest_subcircuit(
                    &function.subcircuit,
                    backend,
                    output_wires,
                    input_wires,
                    scope,
                    known_functions,
                    moduli,
                    public_inputs,
                    private_inputs,
                )?;
            }

            AnonCall(output_wires, input_wires, _, _, subcircuit) => {
                Self::ingest_subcircuit(
                    subcircuit,
                    backend,
                    output_wires,
                    input_wires,
                    scope,
                    known_functions,
                    moduli,
                    public_inputs,
                    private_inputs,
                )?;
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
        scope: &mut HashMap<(FieldId, WireId), B::Wire>,
        known_functions: &HashMap<String, FunctionDeclaration>,
        moduli: &[BigUint],
        public_inputs: &mut Vec<VecDeque<B::FieldElement>>,
        private_inputs: &mut Vec<VecDeque<B::FieldElement>>,
    ) -> Result<()> {
        let mut new_scope: HashMap<(FieldId, WireId), B::Wire> = HashMap::new();

        // copy the inputs required by this function into the new scope, at the proper index
        let expanded_inputs = expand_wirelist(input_list)?;
        let mut input_indexes = wirelist_to_count_list(output_list);
        for (input_field, input_wire) in expanded_inputs.iter() {
            let idx = input_indexes.entry(*input_field).or_insert(0);
            let i = get::<B>(scope, *input_field, *input_wire)?;
            set::<B>(
                &mut new_scope,
                *input_field,
                *idx,
                backend.copy(input_field, i)?,
            )?;
            *idx += 1;
        }
        // evaluate the subcircuit in the new scope.
        for gate in subcircuit {
            Self::ingest_gate(
                gate,
                backend,
                &mut new_scope,
                known_functions,
                moduli,
                public_inputs,
                private_inputs,
            )?;
        }
        // copy the outputs produced from 'new_scope', into 'scope'
        let expanded_outputs = expand_wirelist(output_list)?;
        let mut output_indexes: CountList = HashMap::new();
        for (output_field, output_wire) in expanded_outputs.iter() {
            let idx = output_indexes.entry(*output_field).or_insert(0);
            let w = get::<B>(&new_scope, *output_field, *idx)?;
            set::<B>(
                scope,
                *output_field,
                *output_wire,
                backend.copy(output_field, w)?,
            )?;
            *idx += 1;
        }

        Ok(())
    }

    /// This helper function can be used to retrieve value of a given wire at some point
    /// if it has *NOT* been freed yet, otherwise it will return an Err.
    pub fn get(&self, field_id: FieldId, wire_id: WireId) -> Result<&B::Wire> {
        get::<B>(&self.values, field_id, wire_id)
    }
}

fn set_public_input<I: ZKBackend>(
    backend: &mut I,
    scope: &mut HashMap<(FieldId, WireId), I::Wire>,
    field_id: FieldId,
    wire_id: WireId,
    value: I::FieldElement,
) -> Result<()> {
    let wire = backend.public_input(&field_id, value)?;
    set::<I>(scope, field_id, wire_id, wire)
}

fn set_private_input<I: ZKBackend>(
    backend: &mut I,
    scope: &mut HashMap<(FieldId, WireId), I::Wire>,
    field_id: FieldId,
    wire_id: WireId,
    value: Option<I::FieldElement>,
) -> Result<()> {
    let wire = backend.private_input(&field_id, value)?;
    set::<I>(scope, field_id, wire_id, wire)
}

fn set<I: ZKBackend>(
    scope: &mut HashMap<(FieldId, WireId), I::Wire>,
    field_id: FieldId,
    wire_id: WireId,
    wire: I::Wire,
) -> Result<()> {
    if scope.insert((field_id, wire_id), wire).is_some() {
        Err(format!(
            "Wire ({}: {}) already has a value in this scope.",
            field_id, wire_id
        )
        .into())
    } else {
        Ok(())
    }
}

pub fn get<I: ZKBackend>(
    scope: &HashMap<(FieldId, WireId), I::Wire>,
    field_id: FieldId,
    wire_id: WireId,
) -> Result<&I::Wire> {
    scope
        .get(&(field_id, wire_id))
        .ok_or_else(|| format!("No value given for wire ({}: {})", field_id, wire_id).into())
}

fn remove<I: ZKBackend>(
    scope: &mut HashMap<(FieldId, WireId), I::Wire>,
    field_id: FieldId,
    wire_id: WireId,
) -> Result<I::Wire> {
    scope
        .remove(&(field_id, wire_id))
        .ok_or_else(|| format!("No value given for wire ({}: {})", field_id, wire_id).into())
}

pub fn get_field<'a>(field_id: &'a FieldId, moduli: &'a [BigUint]) -> Result<&'a BigUint> {
    let field = moduli.get(*field_id as usize);
    if let Some(value) = field {
        Ok(value)
    } else {
        Err(format!("Field {} is not defined.", *field_id).into())
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
    type FieldElement = BigUint;

    fn from_bytes_le(val: &[u8]) -> Result<Self::FieldElement> {
        Ok(BigUint::from_bytes_le(val))
    }

    fn set_fields(&mut self, moduli: &[Value]) -> Result<()> {
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

    fn one(&self) -> Result<Self::FieldElement> {
        Ok(BigUint::one())
    }

    fn minus_one(&self, field_id: &FieldId) -> Result<Self::FieldElement> {
        let field = get_field(field_id, &self.m)?;
        if field.is_zero() {
            return Err("Modulus is equal to zero.".into());
        }
        Ok(field - self.one()?)
    }

    fn zero(&self) -> Result<Self::FieldElement> {
        Ok(BigUint::zero())
    }

    fn copy(&mut self, _field_id: &FieldId, wire: &Self::Wire) -> Result<Self::Wire> {
        Ok(wire.clone())
    }

    fn constant(&mut self, _field_id: &FieldId, val: Self::FieldElement) -> Result<Self::Wire> {
        Ok(val)
    }

    fn assert_zero(&mut self, _field_id: &FieldId, wire: &Self::Wire) -> Result<()> {
        if wire.is_zero() {
            Ok(())
        } else {
            Err("AssertZero failed".into())
        }
    }

    fn add(&mut self, field_id: &FieldId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        let field = get_field(field_id, &self.m)?;
        Ok((a + b) % field)
    }

    fn multiply(
        &mut self,
        field_id: &FieldId,
        a: &Self::Wire,
        b: &Self::Wire,
    ) -> Result<Self::Wire> {
        let field = get_field(field_id, &self.m)?;
        Ok((a * b) % field)
    }

    fn add_constant(
        &mut self,
        field_id: &FieldId,
        a: &Self::Wire,
        b: Self::FieldElement,
    ) -> Result<Self::Wire> {
        let field = get_field(field_id, &self.m)?;
        Ok((a + b) % field)
    }

    fn mul_constant(
        &mut self,
        field_id: &FieldId,
        a: &Self::Wire,
        b: Self::FieldElement,
    ) -> Result<Self::Wire> {
        let field = get_field(field_id, &self.m)?;
        Ok((a * b) % field)
    }

    fn public_input(&mut self, field_id: &FieldId, val: Self::FieldElement) -> Result<Self::Wire> {
        self.constant(field_id, val)
    }

    fn private_input(
        &mut self,
        field_id: &FieldId,
        val: Option<Self::FieldElement>,
    ) -> Result<Self::Wire> {
        self.constant(
            field_id,
            val.unwrap_or_else(|| panic!("Missing private input value for PlaintextBackend")),
        )
    }

    fn convert(
        &mut self,
        output_field: &FieldId,
        output_wire_count: u64,
        input_field: &FieldId,
        inputs: &[&Self::Wire],
    ) -> Result<Vec<Self::Wire>> {
        // Retrieve input/output field moduli
        let input_field_value = get_field(input_field, &self.m)?;
        let output_field_value = get_field(output_field, &self.m)?;

        // Convert input to BigUint
        let mut number: BigUint = BigUint::zero();
        for input in inputs.iter() {
            number *= input_field_value;
            number += *input;
        }

        // Convert BigUint value into output_field
        let mut result = vec![];
        for _ in 0..output_wire_count {
            result.insert(0, &number % output_field_value);
            number = &number / output_field_value;
        }

        if !number.is_zero() {
            return Err("Impossible conversion".into());
        }
        Ok(result)
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
    use crate::consumers::evaluator::Evaluator;
    /// This test simply checks that the Evaluator code could run with any ZKInterpreter without issue
    use crate::producers::examples::*;

    let relation = example_relation();
    let public_inputs = example_public_inputs();

    struct VerifierInterpreter {}
    impl ZKBackend for VerifierInterpreter {
        type Wire = i64;
        type FieldElement = BigUint;
        fn from_bytes_le(_val: &[u8]) -> Result<Self::FieldElement> {
            Ok(BigUint::zero())
        }
        fn set_fields(&mut self, _moduli: &[Value]) -> Result<()> {
            Ok(())
        }
        fn one(&self) -> Result<Self::FieldElement> {
            Ok(BigUint::one())
        }
        fn zero(&self) -> Result<Self::FieldElement> {
            Ok(BigUint::zero())
        }
        fn minus_one(&self, _field_id: &FieldId) -> Result<Self::FieldElement> {
            Ok(BigUint::one())
        }
        fn copy(&mut self, _field_id: &FieldId, wire: &Self::Wire) -> Result<Self::Wire> {
            Ok(*wire)
        }
        fn constant(
            &mut self,
            _field_id: &FieldId,
            _val: Self::FieldElement,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn assert_zero(&mut self, _field_id: &FieldId, _wire: &Self::Wire) -> Result<()> {
            Ok(())
        }
        fn add(
            &mut self,
            _field_id: &FieldId,
            _a: &Self::Wire,
            _b: &Self::Wire,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn multiply(
            &mut self,
            _field_id: &FieldId,
            _a: &Self::Wire,
            _b: &Self::Wire,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn add_constant(
            &mut self,
            _field_id: &FieldId,
            _a: &Self::Wire,
            _b: Self::FieldElement,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn mul_constant(
            &mut self,
            _field_id: &FieldId,
            _a: &Self::Wire,
            _b: Self::FieldElement,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn public_input(
            &mut self,
            _field_id: &FieldId,
            _val: Self::FieldElement,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn private_input(
            &mut self,
            _field_id: &FieldId,
            _val: Option<Self::FieldElement>,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn convert(
            &mut self,
            _output_field: &FieldId,
            output_wire_count: u64,
            _input_field: &FieldId,
            _inputs: &[&Self::Wire],
        ) -> Result<Vec<Self::Wire>> {
            Ok(vec![0; output_wire_count as usize])
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
