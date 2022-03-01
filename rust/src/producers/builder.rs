use std::mem::take;
use std::collections::HashMap;

pub use super::build_gates::{BuildGate, BuildComplexGate};
use super::build_gates::NO_OUTPUT;
use crate::producers::sink::MemorySink;
use crate::structs::{value::Value, function::Function, wire::WireList, wire::WireListElement};
use crate::{Gate, Header, Instance, Relation, Sink, WireId, Witness};
use std::cell::{RefCell, Cell};
use crate::structs::relation::{ARITH, FUNCTION, FOR, SWITCH};
use crate::Result;

pub trait GateBuilderT {
    /// Allocates a new wire id for the output and creates a new gate,
    /// Returns the newly allocated WireId.
    fn create_gate(&self, gate: BuildGate) -> WireId;

    /// Allocates some new wire ids for the output and creates a new gate,
    /// Returns the newly allocated WireIds.
    fn create_complex_gate(&self, gate: BuildComplexGate) -> Result<WireList>;
}

/// MessageBuilder builds messages by buffering sequences of gates and witness/instance values.
/// Flush completed messages to a Sink.
/// finish() must be called.
struct MessageBuilder<S: Sink> {
    sink: S,

    instance: Instance,
    witness: Witness,
    relation: Relation,

    /// Maximum number of gates or witness or instance values to hold at once.
    /// Default 100,000 or ~12MB of memory.
    /// Size estimation: 40 per witness + 40 per instance + 48 per gate = 128 bytes.
    pub max_len: usize,
}

impl<S: Sink> MessageBuilder<S> {

    fn new(sink: S, header: Header, gateset: u16, features: u16) -> Self {
        Self {
            sink,
            instance: Instance {
                header: header.clone(),
                common_inputs: vec![],
            },
            witness: Witness {
                header: header.clone(),
                short_witness: vec![],
            },
            relation: Relation {
                header: header.clone(),
                gate_mask: gateset,
                feat_mask: features,
                functions: vec![],
                gates: vec![],
            },
            max_len: 100 * 1000,
        }
    }

    fn push_instance_value(&mut self, value: Value) {
        self.instance.common_inputs.push(value);
        if self.instance.common_inputs.len() == self.max_len {
            self.flush_instance();
        }
    }

    fn push_witness_value(&mut self, value: Value) {
        self.witness.short_witness.push(value);
        if self.witness.short_witness.len() == self.max_len {
            self.flush_witness();
        }
    }

    fn push_gate(&mut self, gate: Gate) {
        self.relation.gates.push(gate);
        if self.relation.gates.len() + self.relation.functions.len() == self.max_len {
            self.flush_relation();
        }
    }

    fn push_function(&mut self, function: Function) {
        self.relation.functions.push(function);
        if self.relation.gates.len() + self.relation.functions.len() == self.max_len {
            self.flush_relation();
        }
    }

    fn flush_instance(&mut self) {
        self.sink.push_instance_message(&self.instance).unwrap();
        self.instance.common_inputs.clear();
    }

    fn flush_witness(&mut self) {
        self.sink.push_witness_message(&self.witness).unwrap();
        self.witness.short_witness.clear();
    }

    fn flush_relation(&mut self) {
        self.sink.push_relation_message(&self.relation).unwrap();
        self.relation.gates.clear();
        self.relation.functions.clear();
    }

    fn finish(mut self) -> S {
        if !self.instance.common_inputs.is_empty() {
            self.flush_instance();
        }
        if !self.witness.short_witness.is_empty() {
            self.flush_witness();
        }
        if !self.relation.gates.is_empty() || !self.relation.functions.is_empty() {
            self.flush_relation();
        }
        self.sink
    }
}

/// GateBuilder allocates wire IDs, builds gates, and tracks instance and witness values.
///
/// # Example
/// ```
/// use zki_sieve::producers::builder::{GateBuilderT, GateBuilder, BuildGate::*};
/// use zki_sieve::producers::sink::MemorySink;
/// use zki_sieve::Header;
///
/// let b = GateBuilder::new(MemorySink::default(), Header::default());
///
/// let my_id = b.create_gate(Constant(vec![0]));
/// b.create_gate(AssertZero(my_id));
/// ```
pub struct GateBuilder<S: Sink> {
    msg_build: RefCell<MessageBuilder<S>>,

    known_functions: RefCell<HashMap<String, usize>>,
    free_id: Cell<WireId>,
}

impl<S: Sink> GateBuilderT for GateBuilder<S> {
    fn create_gate(&self, mut gate: BuildGate) -> WireId {
        let out_id = if gate.has_output() {
            self.alloc()
        } else {
            NO_OUTPUT
        };

        match gate {
            BuildGate::Instance(ref mut value) => {
                self.push_instance_value(take(value));
            }
            BuildGate::Witness(Some(ref mut value)) => {
                self.push_witness_value(take(value));
            }
            _ => {}
        }

        self.msg_build.borrow_mut().push_gate(gate.with_output(out_id));

        out_id
    }

    fn create_complex_gate(&self, gate: BuildComplexGate) -> Result<WireList> {
        let output_count = match gate {
            BuildComplexGate::Call(ref name, _) => self.known_function_output_count(name)?,
        };
        let output_wires = self.multiple_alloc(output_count);

        self.msg_build.borrow_mut().push_gate(gate.with_output(output_wires.clone()));

        Ok(output_wires)
    }
}

impl<S: Sink> GateBuilder<S> {
    /// new creates a new builder.
    pub fn new(sink: S, header: Header) -> Self {
        Self::new_with_functionalities(sink, header, ARITH, FUNCTION|FOR|SWITCH)
    }

    pub fn new_with_functionalities(sink: S, header: Header, gateset: u16, features: u16) -> Self {
        GateBuilder {
            msg_build: RefCell::new(MessageBuilder::new(sink, header, gateset, features)),
            known_functions: RefCell::new(HashMap::new()),
            free_id: Cell::new(0),
        }
    }

    /// alloc allocates a new wire ID.
    fn alloc(&self) -> WireId {
        let id = self.free_id.take();
        self.free_id.set(id + 1);
        id
    }

    /// alloc allocates n wire IDs.
    fn multiple_alloc(&self, n: usize) -> WireList {
        let id = self.free_id.take();
        let next: u64 = id + n as u64;
        self.free_id.set(next);
        vec![WireListElement::WireRange(id, next-1)]
    }

    /// known_function_output_count returns the output_count of the function with name `name`.
    fn known_function_output_count(&self, name: &String) -> Result<usize> {
        match self.known_functions.borrow().get(name) {
            None => Err(format!("Function {} does not exist !", name).into()),
            Some(v) => Ok(*v),
        }
    }

    pub fn push_witness_value(&self, val: Value) {
        self.msg_build.borrow_mut().push_witness_value(val);
    }

    pub fn push_instance_value(&self, val: Value) {
        self.msg_build.borrow_mut().push_instance_value(val);
    }

    pub fn push_function(&self, function: Function) -> Result<()> {
        if self.known_functions.borrow().contains_key(&function.name) {
            return Err(format!("Function {} already exists !", function.name).into());
        } else {
            self.known_functions.borrow_mut().insert(function.name.clone(), function.output_count);
        }
        self.msg_build.borrow_mut().push_function(function);
        Ok(())
    }

    pub fn finish(self) -> S {
        self.msg_build.into_inner().finish()
    }
}

pub fn new_example_builder() -> GateBuilder<MemorySink> {
    GateBuilder::new(MemorySink::default(), Header::default())
}

#[test]
fn test_builder_with_function() {
    use crate::producers::builder::{GateBuilderT, GateBuilder, BuildGate::*, BuildComplexGate::*};
    use crate::producers::{sink::MemorySink, examples};
    use crate::structs::{gates::Gate, function::Function};
    use crate::structs::wire::{WireListElement::*, expand_wirelist};
    use crate::consumers::source::Source;
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};

    let b = GateBuilder::new(MemorySink::default(), examples::example_header());

    let custom_sub = Function::new(
        "custom_sub".to_string(),
        2,
        4,
        0,
        0,
        vec![
            Gate::MulConstant(6, 4, vec![100]), // *(-1)
            Gate::MulConstant(7, 5, vec![100]), // *(-1)
            Gate::Add(0, 2, 6),
            Gate::Add(1, 3, 7)]
     );
    b.push_function(custom_sub).unwrap();

    // Try to push two functions with the same name
    // It should return an error
    let custom_function= Function::new(
        "custom_sub".to_string(),
        0,
        0,
        0,
        0,
        vec![]
    );
    assert!(b.push_function(custom_function).is_err());

    let id_0 = b.create_gate(Constant(vec![40]));
    let id_1 = b.create_gate(Constant(vec![30]));
    let id_2 = b.create_gate(Constant(vec![10]));
    let id_3 = b.create_gate(Constant(vec![5]));

    let out = b.create_complex_gate(Call("custom_sub".to_string(), vec![Wire(id_0), Wire(id_1), Wire(id_2), Wire(id_3)])).unwrap();
    let out = expand_wirelist(&out);
    assert_eq!(out.len(), 2);

    let witness_0 = b.create_gate(Witness(Some(vec![30])));
    let witness_1 = b.create_gate(Witness(Some(vec![25])));

    let neg_witness_0 = b.create_gate(MulConstant(witness_0, vec![100])); // *(-1)
    let neg_witness_1 = b.create_gate(MulConstant(witness_1, vec![100])); // *(-1)

    let res_0 = b.create_gate(Add(out[0], neg_witness_0));
    let res_1 = b.create_gate(Add(out[1], neg_witness_1));

    b.create_gate(AssertZero(res_0));
    b.create_gate(AssertZero(res_1));

    // Try to call an unknown function
    // It should return an error
    assert!(b.create_complex_gate(Call("unknown_function".to_string(), vec![Wire(id_0)])).is_err());

    let sink = b.finish();

    let mut zkbackend = PlaintextBackend::default();
    let source: Source = sink.into();
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut zkbackend);
    assert_eq!(evaluator.get_violations().len(), 0);
}

#[test]
fn test_builder_with_witness_instance_function() {
    use crate::producers::builder::{GateBuilderT, GateBuilder, BuildGate::*, BuildComplexGate::*};
    use crate::producers::{sink::MemorySink, examples};
    use crate::structs::{gates::Gate, function::Function};
    use crate::structs::wire::{WireListElement::*, expand_wirelist};
    use crate::consumers::source::Source;
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};

    let b = GateBuilder::new(MemorySink::default(), examples::example_header());

    let sub_witness_instance = Function::new(
        "sub_witness_instance".to_string(),
        2,
        2,
        1,
        1,
        vec![
            Gate::Instance(4),
            Gate::Witness(5),
            Gate::MulConstant(6, 4, vec![100]), // -instance
            Gate::MulConstant(7, 5, vec![100]), // -witness
            Gate::Add(0, 2, 6), // input[0] - instance
            Gate::Add(1, 3, 7)] // input[1] - witness
    );
    b.push_function(sub_witness_instance).unwrap();

    b.push_instance_value(vec![10]);
    b.push_witness_value(vec![10]);

    let id_0 = b.create_gate(Constant(vec![10]));
    let id_1 = b.create_gate(Constant(vec![10]));

    let out = b.create_complex_gate(Call("sub_witness_instance".to_string(), vec![Wire(id_0), Wire(id_1)])).unwrap();
    let out = expand_wirelist(&out);
    assert_eq!(out.len(), 2);

    b.create_gate(AssertZero(out[0]));
    b.create_gate(AssertZero(out[1]));
    let sink = b.finish();

    let mut zkbackend = PlaintextBackend::default();
    let source: Source = sink.into();
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut zkbackend);
    assert_eq!(evaluator.get_violations().len(), 0);
}