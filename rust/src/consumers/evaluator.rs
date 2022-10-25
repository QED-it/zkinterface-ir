use crate::structs::count::{count_list_max, wirelist_to_count_list, CountList};
use crate::structs::function::{CaseInvoke, ForLoopBody};
use crate::structs::iterators::evaluate_iterexpr_list;
use crate::structs::relation::{contains_feature, BOOL};
use crate::structs::wire::{
    expand_wirelist, is_one_field_wirelist, wire_ids_to_wirelist, wirelist_len, WireList,
};
use crate::{FieldId, Gate, Header, Instance, Message, Relation, Result, Value, WireId, Witness};
use num_bigint::BigUint;
use num_traits::identities::{One, Zero};
use std::collections::{HashMap, VecDeque};
use std::ops::{BitAnd, BitXor, Shr};

/// The `ZKBackend` trait should be implemented by any backend that wants to evaluate SIEVE IR circuits.
/// It has to define 2 types:
///  - `Wire`: represents a variable in the circuit. It should implement the `Clone` trait.
///  - `FieldElement`: represents elements of the underlying field. Mainly used when importing
///                    instances/witnesses from the corresponding pools.
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
    fn set_fields(&mut self, moduli: &[Value], is_boolean: bool) -> Result<()>;

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

    /// Performs a boolean `and` between two wires. The result is stored in a new wire.
    fn and(&mut self, field_id: &FieldId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire>;
    /// Performs a boolean `xor` between two wires. The result is stored in a new wire.
    fn xor(&mut self, field_id: &FieldId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire>;
    /// Performs a boolean `not` on a given wire. The result is stored in a new wire.
    fn not(&mut self, field_id: &FieldId, a: &Self::Wire) -> Result<Self::Wire>;

    /// This functions declares a new instance variable owning the value given as parameter,
    /// which should be stored in a new wire.
    fn instance(&mut self, field_id: &FieldId, val: Self::FieldElement) -> Result<Self::Wire>;
    /// This functions declares a new witness variable owning the value given as parameter,
    /// which should be stored in a new wire.
    /// The value is given as a `Option`, because depending upon the type of this ZKBackend
    /// (prover / verifier), it should act differently.
    ///  - In prover mode, the witness should be provided, so the value should be `Some`.
    ///  - In verifier mode, the witness should normally not be provided (except maybe in test mode)
    /// Both cases should return a `Self::Wire` so the ZKBackend should have a specific wire value
    /// to handle it when in verifier mode.
    fn witness(
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

/// Used to evaluate a 'multiplication' in either the arithmetic case or the boolean,
/// where it's replaced by an AND operation.
fn as_mul<B: ZKBackend>(
    backend: &mut B,
    field_id: &FieldId,
    a: &B::Wire,
    b: &B::Wire,
    is_bool: bool,
) -> Result<B::Wire> {
    if is_bool {
        backend.and(field_id, a, b)
    } else {
        backend.multiply(field_id, a, b)
    }
}

/// Used to evaluate an 'addition' in either the arithmetic case or the boolean,
/// where it's replaced by an XOR operation.
fn as_add<B: ZKBackend>(
    backend: &mut B,
    field_id: &FieldId,
    a: &B::Wire,
    b: &B::Wire,
    is_bool: bool,
) -> Result<B::Wire> {
    if is_bool {
        backend.xor(field_id, a, b)
    } else {
        backend.add(field_id, a, b)
    }
}
// Computes the 'negative' value. `minus_one_buf` must be provided and include a buffer with modulus minus one
fn as_negate<B: ZKBackend>(
    backend: &mut B,
    field_id: &FieldId,
    wire: &B::Wire,
    is_bool: bool,
) -> Result<B::Wire> {
    if is_bool {
        // negation in boolean field is identity
        backend.copy(field_id, wire)
    } else {
        backend.mul_constant(field_id, wire, backend.minus_one(field_id)?)
    }
}

// Computes an 'addition' with constant. Boolean fields are not supported.
fn as_add_one<B: ZKBackend>(
    backend: &mut B,
    field_id: &FieldId,
    wire: &B::Wire,
    is_bool: bool,
) -> Result<B::Wire> {
    if is_bool {
        // adding one in boolean field is not
        backend.not(field_id, wire)
    } else {
        backend.add_constant(field_id, wire, backend.one()?)
    }
}

/// This structure defines a function as defined in the circuit, but without the name.
/// It's mainly used to retrieve information from the name.
struct FunctionDeclaration {
    subcircuit: Vec<Gate>,
    instance_count: CountList,
    witness_count: CountList,
    output_count: CountList,
    input_count: CountList,
}

/// This structure is the core of IR evaluation. It is instantiated using a ZKBackend,
/// and will read the IR circuit, parses it, and calls the corresponding function from the
/// ZKBackend to evaluate each single operation.
/// It will inline functions, unroll loops, and multiplex switches.
///
/// # Example
/// ```
/// use zki_sieve::consumers::evaluator::{PlaintextBackend, Evaluator};
/// use zki_sieve::producers::examples::*;
///
/// let relation = example_relation();
/// let instance = example_instance();
/// let witness = example_witness();
///
/// let mut zkbackend = PlaintextBackend::default();
/// let mut simulator = Evaluator::default();
/// let _ = simulator.ingest_instance(&instance);
/// let _ = simulator.ingest_witness(&witness);
/// let _ = simulator.ingest_relation(&relation, &mut zkbackend);
/// ```
pub struct Evaluator<B: ZKBackend> {
    values: HashMap<(FieldId, WireId), B::Wire>,
    moduli: Vec<BigUint>,
    instance_queue: Vec<VecDeque<B::FieldElement>>,
    witness_queue: Vec<VecDeque<B::FieldElement>>,
    is_boolean: bool,

    // name => (subcircuit, instance_count, witness_count, output_count, input_count)
    known_functions: HashMap<String, FunctionDeclaration>,

    verified_at_least_one_gate: bool,
    found_error: Option<String>,
}

impl<B: ZKBackend> Default for Evaluator<B> {
    fn default() -> Self {
        Evaluator {
            values: Default::default(),
            moduli: vec![],
            instance_queue: vec![],
            witness_queue: vec![],
            is_boolean: false,
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
            Message::Instance(i) => self.ingest_instance(i),
            Message::Witness(w) => self.ingest_witness(w),
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
            self.instance_queue.push(VecDeque::new());
            self.witness_queue.push(VecDeque::new());
        }
        Ok(())
    }

    /// Ingest an `Instance` message, and returns a `Result` whether ot nor an error
    /// was encountered. It stores the instance values in a pool.
    pub fn ingest_instance(&mut self, instance: &Instance) -> Result<()> {
        self.ingest_header(&instance.header)?;

        for (i, inputs) in instance.common_inputs.iter().enumerate() {
            let instances = self
                .instance_queue
                .get_mut(i)
                .ok_or("No enough elements in instance_queue")?;
            for value in &inputs.inputs {
                instances.push_back(B::from_bytes_le(value)?);
            }
        }
        Ok(())
    }

    /// Ingest an `Witness` message, and returns a `Result` whether ot nor an error
    /// was encountered. It stores the witness values in a pool.
    pub fn ingest_witness(&mut self, witness: &Witness) -> Result<()> {
        self.ingest_header(&witness.header)?;

        for (i, inputs) in witness.short_witness.iter().enumerate() {
            let witnesses = self
                .witness_queue
                .get_mut(i)
                .ok_or("No enough elements in witness_queue")?;
            for value in &inputs.inputs {
                witnesses.push_back(B::from_bytes_le(value)?);
            }
        }
        Ok(())
    }

    /// Ingest a `Relation` message
    pub fn ingest_relation(&mut self, relation: &Relation, backend: &mut B) -> Result<()> {
        self.ingest_header(&relation.header)?;
        self.is_boolean = contains_feature(relation.gate_mask, BOOL);
        backend.set_fields(&relation.header.fields, self.is_boolean)?;

        if !relation.gates.is_empty() {
            self.verified_at_least_one_gate = true;
        }

        for f in relation.functions.iter() {
            self.known_functions.insert(
                f.name.clone(),
                FunctionDeclaration {
                    subcircuit: f.body.clone(),
                    instance_count: f.instance_count.clone(),
                    witness_count: f.witness_count.clone(),
                    output_count: f.output_count.clone(),
                    input_count: f.input_count.clone(),
                },
            );
        }

        let mut known_iterators = HashMap::new();

        for gate in &relation.gates {
            Self::ingest_gate(
                gate,
                backend,
                &mut self.values,
                &self.known_functions,
                &mut known_iterators,
                &self.moduli,
                self.is_boolean,
                &mut self.instance_queue,
                &mut self.witness_queue,
                None,
            )?;
        }
        Ok(())
    }

    /// This function ingests one gate at a time (but can call itself recursively)
    /// If the current gate is in a branch of a switch, then it has to be weighted.
    /// The weight is used in `AssertZero` gates by multiplying the tested wire by the weight. It
    /// allows to 'disable' assertions that are in a not-taken branch. The weight should propagate
    /// to inner gates.
    /// - `scope` contains the list of existing wires with their respective value. It will be
    ///    augmented if this gate produces outputs, or reduced if this is a `GateFree`
    /// - `known_functions` is the map of functions defined in previous or current `Relation` message
    /// - `known_iterators` is the map of defined iterators. It will be temporarily updated if the
    ///    current gate is a `GateFor`
    /// - `moduli` and `is_boolean` are used mainly in switches to compute the weight of each branch.
    /// - `instances` and `witnesses` are the instances and witnesses pools, implemented as Queues.
    ///    They will be consumed whenever necessary.
    fn ingest_gate(
        gate: &Gate,
        backend: &mut B,
        scope: &mut HashMap<(FieldId, WireId), B::Wire>,
        known_functions: &HashMap<String, FunctionDeclaration>,
        known_iterators: &mut HashMap<String, u64>,
        moduli: &[BigUint],
        is_boolean: bool,
        instances: &mut Vec<VecDeque<B::FieldElement>>,
        witnesses: &mut Vec<VecDeque<B::FieldElement>>,
        weight: Option<&B::Wire>,
    ) -> Result<()> {
        use Gate::*;

        let nb_fields = instances.len();

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
                let should_be_zero = if let Some(w) = weight {
                    as_mul(backend, field_id, w, inp_wire, is_boolean)?
                } else {
                    backend.copy(field_id, inp_wire)?
                };
                if backend.assert_zero(field_id, &should_be_zero).is_err() {
                    return Err(format!(
                        "Wire ({}: {}) (may be weighted) should be 0, while it is not",
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

            And(field_id, out, left, right) => {
                let l = get!(*field_id, *left)?;
                let r = get!(*field_id, *right)?;
                let and = backend.and(field_id, l, r)?;
                set!(*field_id, *out, and)?;
            }

            Xor(field_id, out, left, right) => {
                let l = get!(*field_id, *left)?;
                let r = get!(*field_id, *right)?;
                let xor = backend.xor(field_id, l, r)?;
                set!(*field_id, *out, xor)?;
            }

            Not(field_id, out, inp) => {
                let val = get!(*field_id, *inp)?;
                let not = backend.not(field_id, val)?;
                set!(*field_id, *out, not)?;
            }

            Instance(field_id, out) => {
                let instances_for_field = instances.get_mut(*field_id as usize).ok_or(format!(
                    "Unknown field {} when evaluating an Instance gate",
                    *field_id
                ))?;
                let val = if let Some(inner) = instances_for_field.pop_front() {
                    inner
                } else {
                    return Err("Not enough instance to consume".into());
                };
                set_instance(backend, scope, *field_id, *out, val)?;
            }

            Witness(field_id, out) => {
                let witnesses_for_field = witnesses.get_mut(*field_id as usize).ok_or(format!(
                    "Unknown field {} when evaluating a Witness gate",
                    *field_id
                ))?;
                let val = witnesses_for_field.pop_front();
                set_witness(backend, scope, *field_id, *out, val)?;
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

                // in the case of an named call, iterators *ARE NOT* forwarded into inner bodies.
                Self::ingest_subcircuit(
                    &function.subcircuit,
                    backend,
                    output_wires,
                    input_wires,
                    scope,
                    known_functions,
                    &mut HashMap::new(),
                    moduli,
                    is_boolean,
                    instances,
                    witnesses,
                    weight,
                )?;
            }

            AnonCall(output_wires, input_wires, _, _, subcircuit) => {
                // in the case of an anoncall, iterators *ARE* forwarded into inner bodies.
                Self::ingest_subcircuit(
                    subcircuit,
                    backend,
                    output_wires,
                    input_wires,
                    scope,
                    known_functions,
                    known_iterators,
                    moduli,
                    is_boolean,
                    instances,
                    witnesses,
                    weight,
                )?;
            }

            // For loops are unrolled. The body is called as many times (NB: the loop bounds are
            // inclusive), and iterator expressions are evaluated for each.
            For(iterator_name, start_val, end_val, _, body) => {
                for i in *start_val..=*end_val {
                    known_iterators.insert(iterator_name.clone(), i);

                    match body {
                        ForLoopBody::IterExprCall(name, field_id, outputs, inputs) => {
                            let function = known_functions.get(name).ok_or("Unknown function")?;
                            let expanded_output = evaluate_iterexpr_list(outputs, known_iterators);
                            let expanded_input = evaluate_iterexpr_list(inputs, known_iterators);

                            // simple checks.
                            let call_output_count =
                                HashMap::from([(*field_id, expanded_output.len() as u64)]);
                            if call_output_count != function.output_count {
                                return Err(format!("Wrong number of output variables in call to function {} (Expected {:?} / Got {:?}).", name, function.output_count, call_output_count).into());
                            }
                            let call_input_count =
                                HashMap::from([(*field_id, expanded_input.len() as u64)]);
                            if call_input_count != function.input_count {
                                return Err(format!("Wrong number of input variables in call to function {} (Expected {:?} / Got {:?}).", name, function.input_count, call_input_count).into());
                            }

                            Self::ingest_subcircuit(
                                &function.subcircuit,
                                backend,
                                &wire_ids_to_wirelist(field_id, &expanded_output),
                                &wire_ids_to_wirelist(field_id, &expanded_input),
                                scope,
                                known_functions,
                                &mut HashMap::new(),
                                moduli,
                                is_boolean,
                                instances,
                                witnesses,
                                weight,
                            )?;
                        }
                        ForLoopBody::IterExprAnonCall(
                            field_id,
                            output_wires,
                            input_wires,
                            _,
                            _,
                            subcircuit,
                        ) => {
                            let expanded_output =
                                evaluate_iterexpr_list(output_wires, known_iterators);
                            let expanded_input =
                                evaluate_iterexpr_list(input_wires, known_iterators);

                            Self::ingest_subcircuit(
                                subcircuit,
                                backend,
                                &wire_ids_to_wirelist(field_id, &expanded_output),
                                &wire_ids_to_wirelist(field_id, &expanded_input),
                                scope,
                                known_functions,
                                known_iterators,
                                moduli,
                                is_boolean,
                                instances,
                                witnesses,
                                weight,
                            )?;
                        }
                    }
                }
                known_iterators.remove(iterator_name);
            }

            // Switches are multiplexed. Each branch has a weight, which is (in plaintext) either 1
            // or 0 (0 if the branch is not taken, 1 otherwise)
            Switch(condition_field_id, condition_wire_id, output_wires, cases, branches) => {
                // determine the maximum instance/witness consumption per field
                let mut max_instance_count: CountList = HashMap::new();
                let mut max_witness_count: CountList = HashMap::new();
                for branch in branches.iter() {
                    let (instance_cnt, witness_cnt) = match branch {
                        CaseInvoke::AbstractGateCall(name, _) => {
                            let function = known_functions.get(name).ok_or("Unknown function")?;
                            (
                                function.instance_count.clone(),
                                function.witness_count.clone(),
                            )
                        }
                        CaseInvoke::AbstractAnonCall(_, instance_count, witness_count, _) => {
                            (instance_count.clone(), witness_count.clone())
                        }
                    };
                    count_list_max(&mut max_instance_count, &instance_cnt);
                    count_list_max(&mut max_witness_count, &witness_cnt);
                }

                // 'consumes' max_instances and max_witnesses values from the corresponding pools
                // by removing them from the instances/witnesses variables, and storing them into
                // new queues. The new queues (a clone of them) will be used in each branch.
                let mut new_instances = vec![VecDeque::new(); nb_fields];
                for (field_id, count) in max_instance_count {
                    let instances_for_field = instances.get_mut(field_id as usize).ok_or(
                        format!("Unknown field {} when evaluating a switch.", field_id),
                    )?;
                    let mut new_instances_for_field: VecDeque<B::FieldElement> =
                        instances_for_field
                            .split_off(std::cmp::min(instances_for_field.len(), count as usize));
                    std::mem::swap(instances_for_field, &mut new_instances_for_field);
                    new_instances[field_id as usize] = new_instances_for_field;
                }

                let mut new_witnesses = vec![VecDeque::new(); nb_fields];
                for (field_id, count) in max_witness_count {
                    let witnesses_for_field = witnesses.get_mut(field_id as usize).ok_or(
                        format!("Unknown field {} when evaluating a switch.", field_id),
                    )?;
                    let mut new_witnesses_for_field: VecDeque<B::FieldElement> =
                        witnesses_for_field
                            .split_off(std::cmp::min(witnesses_for_field.len(), count as usize));
                    std::mem::swap(witnesses_for_field, &mut new_witnesses_for_field);
                    new_witnesses[field_id as usize] = new_witnesses_for_field;
                }

                // This will handle the input/output wires for each branches. Output wires will then
                // be combined using their respective weight.
                let mut branches_scope = Vec::new();

                let expanded_output = expand_wirelist(output_wires)?;
                for (output_field, _) in expanded_output.iter() {
                    if *output_field != *condition_field_id {
                        return Err("In a Switch, the field of all output wires must be equal to the condition field.".into());
                    }
                }
                let mut weights = Vec::new();

                for (case, branch) in cases.iter().zip(branches.iter()) {
                    // Compute (1 - ('case' - 'condition') ^ (self.modulus - 1))
                    let branch_weight = compute_weight(
                        backend,
                        case,
                        condition_field_id,
                        get!(*condition_field_id, *condition_wire_id)?,
                        moduli,
                        is_boolean,
                    )?;
                    let weighted_branch_weight = if let Some(w) = weight {
                        as_mul(backend, condition_field_id, w, &branch_weight, is_boolean)?
                    } else {
                        branch_weight
                    };

                    let mut branch_scope: HashMap<(FieldId, WireId), B::Wire> = HashMap::new();
                    match branch {
                        CaseInvoke::AbstractGateCall(name, input_wires) => {
                            let function = known_functions
                                .get(name)
                                .ok_or(format!("Unknown function: {}", name))?;

                            // simple checks.
                            let output_wires_countlist = wirelist_to_count_list(output_wires);
                            if output_wires_countlist != function.output_count {
                                return Err(format!("Wrong number of output variables in call to function {} (Expected {:?} / Got {:?}).", name, function.output_count, output_wires_countlist).into());
                            }
                            let input_wires_countlist = wirelist_to_count_list(input_wires);
                            if input_wires_countlist != function.input_count {
                                return Err(format!("Wrong number of input variables in call to function {} (Expected {:?} / Got {:?}).", name, function.input_count, input_wires_countlist).into());
                            }

                            let expanded_input = expand_wirelist(input_wires)?;
                            for (input_field, input_wire) in expanded_input.iter() {
                                let w = get!(*input_field, *input_wire)?;
                                branch_scope.insert(
                                    (*input_field, *input_wire),
                                    backend.copy(input_field, w)?,
                                );
                            }
                            Self::ingest_subcircuit(
                                &function.subcircuit,
                                backend,
                                output_wires,
                                input_wires,
                                &mut branch_scope,
                                known_functions,
                                &mut HashMap::new(),
                                moduli,
                                is_boolean,
                                &mut new_instances.clone(),
                                &mut new_witnesses.clone(),
                                Some(&weighted_branch_weight),
                            )?;
                        }
                        CaseInvoke::AbstractAnonCall(input_wires, _, _, subcircuit) => {
                            let expanded_input = expand_wirelist(input_wires)?;
                            for (input_field, input_wire) in expanded_input.iter() {
                                let w = get!(*input_field, *input_wire)?;
                                branch_scope.insert(
                                    (*input_field, *input_wire),
                                    backend.copy(input_field, w)?,
                                );
                            }
                            Self::ingest_subcircuit(
                                subcircuit,
                                backend,
                                output_wires,
                                input_wires,
                                &mut branch_scope,
                                known_functions,
                                known_iterators,
                                moduli,
                                is_boolean,
                                &mut new_instances.clone(),
                                &mut new_witnesses.clone(),
                                Some(&weighted_branch_weight),
                            )?;
                        }
                    }
                    weights.push(weighted_branch_weight);
                    // TODO we don't need all the scope here, only the output wires.
                    branches_scope.push(branch_scope);
                }

                // Compute the weighted sum for all output wire.
                for (output_field, output_wire) in expanded_output.iter() {
                    let weighted_output = branches_scope.iter().zip(weights.iter()).fold(
                        backend.constant(output_field, backend.zero()?),
                        |accu, (branch_scope, branch_weight)| {
                            let weighted_wire = as_mul(
                                backend,
                                output_field,
                                get::<B>(branch_scope, *output_field, *output_wire)?,
                                branch_weight,
                                is_boolean,
                            )?;
                            as_add(backend, output_field, &accu?, &weighted_wire, is_boolean)
                        },
                    )?;
                    set!(*output_field, *output_wire, weighted_output)?;
                }
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
        known_iterators: &mut HashMap<String, u64>,
        moduli: &[BigUint],
        is_boolean: bool,
        instances: &mut Vec<VecDeque<B::FieldElement>>,
        witnesses: &mut Vec<VecDeque<B::FieldElement>>,
        weight: Option<&B::Wire>,
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
                known_iterators,
                moduli,
                is_boolean,
                instances,
                witnesses,
                weight,
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

fn set_instance<I: ZKBackend>(
    backend: &mut I,
    scope: &mut HashMap<(FieldId, WireId), I::Wire>,
    field_id: FieldId,
    wire_id: WireId,
    value: I::FieldElement,
) -> Result<()> {
    let wire = backend.instance(&field_id, value)?;
    set::<I>(scope, field_id, wire_id, wire)
}

fn set_witness<I: ZKBackend>(
    backend: &mut I,
    scope: &mut HashMap<(FieldId, WireId), I::Wire>,
    field_id: FieldId,
    wire_id: WireId,
    value: Option<I::FieldElement>,
) -> Result<()> {
    let wire = backend.witness(&field_id, value)?;
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

/// This function will compute the modular exponentiation of a given base to a given exponent
/// recursively. It returns the wire holding the result.
fn exp<I: ZKBackend>(
    backend: &mut I,
    field_id: &FieldId,
    base: &I::Wire,
    exponent: &BigUint,
    field: &BigUint,
    is_boolean: bool,
) -> Result<I::Wire> {
    if exponent.is_one() {
        return backend.copy(field_id, base);
    }

    let previous = exp(backend, field_id, base, &exponent.shr(1), field, is_boolean)?;

    let ret = as_mul(backend, field_id, &previous, &previous, is_boolean);
    if exponent.bitand(BigUint::one()).is_one() {
        as_mul(backend, field_id, &ret?, base, is_boolean)
    } else {
        ret
    }
}

/// This function will compute '1 - (case - condition)^(p-1)' using a bunch of mul/and add/xor addc/xorc gates
fn compute_weight<B: ZKBackend>(
    backend: &mut B,
    case: &[u8],
    field_id: &FieldId,
    condition: &B::Wire,
    moduli: &[BigUint],
    is_boolean: bool,
) -> Result<B::Wire> {
    let field = get_field(field_id, moduli)?;
    let case_wire = &backend.constant(field_id, B::from_bytes_le(case)?)?;
    // scalar value
    let exponent = field - &BigUint::one();

    let minus_cond = &as_negate(backend, field_id, condition, is_boolean)?;
    let base = &as_add(backend, field_id, case_wire, minus_cond, is_boolean)?;
    let base_to_exp = &exp(backend, field_id, base, &exponent, field, is_boolean)?;
    let right = &as_negate(backend, field_id, base_to_exp, is_boolean)?;
    as_add_one(backend, field_id, right, is_boolean)
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
/// Currently, this backend does not support 'verifier' mode, and requires witnesses to be provided.
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

    fn set_fields(&mut self, moduli: &[Value], _is_boolean: bool) -> Result<()> {
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

    fn and(&mut self, field_id: &FieldId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        let field = get_field(field_id, &self.m)?;
        Ok((a.bitand(b)) % field)
    }

    fn xor(&mut self, field_id: &FieldId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        let field = get_field(field_id, &self.m)?;
        Ok((a.bitxor(b)) % field)
    }

    fn not(&mut self, _field_id: &FieldId, a: &Self::Wire) -> Result<Self::Wire> {
        Ok(if a.is_zero() {
            BigUint::one()
        } else {
            BigUint::zero()
        })
    }

    fn instance(&mut self, field_id: &FieldId, val: Self::FieldElement) -> Result<Self::Wire> {
        self.constant(field_id, val)
    }

    fn witness(
        &mut self,
        field_id: &FieldId,
        val: Option<Self::FieldElement>,
    ) -> Result<Self::Wire> {
        self.constant(
            field_id,
            val.unwrap_or_else(|| panic!("Missing witness value for PlaintextBackend")),
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
fn test_exponentiation() -> Result<()> {
    use itertools::izip;

    let moduli = vec![
        BigUint::from(16249742125730185677094195492597105093u128),
        BigUint::from(101u64),
    ];

    let bases = vec![BigUint::from(2u128), BigUint::from(42u64)];

    let exponents = vec![
        BigUint::from(2206000150907221872269901214599500635u128),
        BigUint::from(100u64),
    ];

    let expecteds = vec![
        BigUint::from(5834907326474057072663503101785122138u128),
        BigUint::one(),
    ];

    for (modulus, base, exponent, expected) in izip!(
        moduli.iter(),
        bases.iter(),
        exponents.iter(),
        expecteds.iter()
    ) {
        let mut backend = PlaintextBackend::default();
        backend.set_fields(&[modulus.to_bytes_le()], false)?;
        let result = exp(&mut backend, &0, base, exponent, modulus, false)?;
        assert_eq!(result, *expected);
    }

    Ok(())
}

#[test]
fn test_evaluator() -> crate::Result<()> {
    use crate::consumers::evaluator::Evaluator;
    use crate::producers::examples::*;

    let relation = example_relation();
    let instance = example_instance();
    let witness = example_witness();

    let mut zkbackend = PlaintextBackend::default();
    let mut simulator: Evaluator<PlaintextBackend> = Evaluator::default();

    simulator.ingest_instance(&instance)?;
    simulator.ingest_witness(&witness)?;
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
    let instance = example_instance();

    struct VerifierInterpreter {}
    impl ZKBackend for VerifierInterpreter {
        type Wire = i64;
        type FieldElement = BigUint;
        fn from_bytes_le(_val: &[u8]) -> Result<Self::FieldElement> {
            Ok(BigUint::zero())
        }
        fn set_fields(&mut self, _moduli: &[Value], _is_boolean: bool) -> Result<()> {
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
        fn and(
            &mut self,
            _field_id: &FieldId,
            _a: &Self::Wire,
            _b: &Self::Wire,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn xor(
            &mut self,
            _field_id: &FieldId,
            _a: &Self::Wire,
            _b: &Self::Wire,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn not(&mut self, _field_id: &FieldId, _a: &Self::Wire) -> Result<Self::Wire> {
            Ok(0)
        }
        fn instance(
            &mut self,
            _field_id: &FieldId,
            _val: Self::FieldElement,
        ) -> Result<Self::Wire> {
            Ok(0)
        }
        fn witness(
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
    simulator.ingest_instance(&instance)?;
    simulator.ingest_relation(&relation, &mut zkbackend)?;

    assert_eq!(simulator.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_evaluator_wrong_result() -> crate::Result<()> {
    use crate::consumers::evaluator::Evaluator;
    use crate::producers::examples::*;

    let relation = example_relation();
    let instance = example_instance();
    let witness = example_witness_incorrect();

    let mut zkbackend = PlaintextBackend::default();
    let mut simulator = Evaluator::default();
    let _ = simulator.ingest_instance(&instance);
    let _ = simulator.ingest_witness(&witness);
    let should_be_err = simulator.ingest_relation(&relation, &mut zkbackend);

    assert!(should_be_err.is_err());
    assert_eq!(
        "Wire (0: 9) (may be weighted) should be 0, while it is not",
        should_be_err.err().unwrap().to_string()
    );

    Ok(())
}
