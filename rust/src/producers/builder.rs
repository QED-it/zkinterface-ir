use std::collections::HashMap;
use std::mem::take;

use super::build_gates::NO_OUTPUT;
pub use super::build_gates::{BuildComplexGate, BuildGate};
use crate::producers::sink::MemorySink;
use crate::structs::relation::{ARITH, FOR, FUNCTION, SWITCH};
use crate::structs::wire::{expand_wirelist, WireList, WireListElement};
use crate::structs::{function::Function, value::Value};
use crate::Result;
use crate::{Gate, Header, Instance, Relation, Sink, WireId, Witness};

pub trait GateBuilderT {
    /// Allocates a new wire id for the output and creates a new gate,
    /// Returns the newly allocated WireId.
    fn create_gate(&mut self, gate: BuildGate) -> WireId;

    /// Pushes instances and witnesses,
    /// Allocates some new wire ids for the output,
    /// Creates a new gate,
    /// Returns the newly allocated WireIds.
    fn create_complex_gate(
        &mut self,
        gate: BuildComplexGate,
        instances: Vec<Value>,
        witnesses: Vec<Value>,
    ) -> Result<WireList>;
}

/// MessageBuilder builds messages by buffering sequences of gates and witness/instance values.
/// Flush completed messages to a Sink.
/// finish() must be called.
struct MessageBuilder<S: Sink> {
    sink: S,

    instance: Instance,
    witness: Witness,
    relation: Relation,

    /// Current size (sum of the number of gates) of the relation's functions vector
    functions_size: usize,

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
            functions_size: 0,
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
        if self.relation.gates.len() + self.functions_size >= self.max_len {
            self.flush_relation();
        }
    }

    fn push_function(&mut self, function: Function) {
        self.functions_size += function.body.len();
        self.relation.functions.push(function);
        if self.relation.gates.len() + self.functions_size >= self.max_len {
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
        self.functions_size = 0;
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
/// let mut b = GateBuilder::new(MemorySink::default(), Header::default());
///
/// let my_id = b.create_gate(Constant(vec![0]));
/// b.create_gate(AssertZero(my_id));
/// ```
pub struct GateBuilder<S: Sink> {
    msg_build: MessageBuilder<S>,

    // name => FunctionParams
    known_functions: HashMap<String, FunctionParams>,
    free_id: WireId,
}

/// FunctionParams contains the number of inputs, outputs, instances and witnesses of a function.
#[derive(Clone, Copy)]
struct FunctionParams {
    input_count: usize,
    output_count: usize,
    instance_count: usize,
    witness_count: usize,
}

/// known_function_params returns the FunctionParams of the function with name `name`.
/// If no function with name `name` belongs to the HashMap `known_functions`, then it returns an error.
fn known_function_params(
    known_functions: &HashMap<String, FunctionParams>,
    name: &String,
) -> Result<FunctionParams> {
    match known_functions.get(name) {
        None => Err(format!("Function {} does not exist !", name).into()),
        Some(v) => Ok(*v),
    }
}

/// alloc allocates a new wire ID.
fn alloc(free_id: &mut WireId) -> WireId {
    let id = free_id.clone();
    *free_id = id + 1;
    id
}

/// alloc allocates n wire IDs.
fn multiple_alloc(free_id: &mut WireId, n: usize) -> WireList {
    let id = free_id.clone();
    let next: u64 = id + n as u64;
    *free_id = next;
    vec![WireListElement::WireRange(id, next - 1)]
}

impl<S: Sink> GateBuilderT for GateBuilder<S> {
    fn create_gate(&mut self, mut gate: BuildGate) -> WireId {
        let out_id = if gate.has_output() {
            alloc(&mut self.free_id)
        } else {
            NO_OUTPUT
        };

        match gate {
            BuildGate::Instance(Some(ref mut value)) => {
                self.push_instance_value(take(value));
            }
            BuildGate::Witness(Some(ref mut value)) => {
                self.push_witness_value(take(value));
            }
            _ => {}
        }

        self.msg_build.push_gate(gate.with_output(out_id));

        out_id
    }

    fn create_complex_gate(
        &mut self,
        gate: BuildComplexGate,
        instances: Vec<Value>,
        witnesses: Vec<Value>,
    ) -> Result<WireList> {
        // Check inputs size and return output_count
        let output_count = match gate {
            BuildComplexGate::Call(ref name, ref input_wires) => {
                let function_params = known_function_params(&self.known_functions, name)?;
                let input_count = expand_wirelist(input_wires).len();
                if function_params.input_count != input_count {
                    return Err(format!(
                        "Function {} has {} inputs and is called with {} inputs.",
                        name, function_params.input_count, input_count
                    )
                    .into());
                }
                if function_params.instance_count != instances.len() {
                    return Err(format!(
                        "Function {} has {} instances and is called with {} instances.",
                        name,
                        function_params.instance_count,
                        instances.len()
                    )
                    .into());
                }
                if function_params.witness_count != witnesses.len() {
                    return Err(format!(
                        "Function {} has {} witnesses and is called with {} witnesses.",
                        name,
                        function_params.witness_count,
                        witnesses.len()
                    )
                    .into());
                }
                function_params.output_count
            }
        };

        // Push instances
        for instance in instances {
            self.msg_build.push_instance_value(instance);
        }
        // Push witnesses
        for witness in witnesses {
            self.msg_build.push_witness_value(witness);
        }

        let output_wires = multiple_alloc(&mut self.free_id, output_count);
        self.msg_build
            .push_gate(gate.with_output(output_wires.clone()));

        Ok(output_wires)
    }
}

impl<S: Sink> GateBuilder<S> {
    /// new creates a new builder.
    pub fn new(sink: S, header: Header) -> Self {
        Self::new_with_functionalities(sink, header, ARITH, FUNCTION | FOR | SWITCH)
    }

    pub fn new_with_functionalities(sink: S, header: Header, gateset: u16, features: u16) -> Self {
        GateBuilder {
            msg_build: MessageBuilder::new(sink, header, gateset, features),
            known_functions: HashMap::new(),
            free_id: 0,
        }
    }

    pub fn new_function_builder(
        &self,
        name: String,
        output_count: usize,
        input_count: usize,
    ) -> FunctionBuilder {
        FunctionBuilder {
            name: name,
            output_count: output_count,
            input_count: input_count,
            gates: vec![],
            instance_count: 0,
            witness_count: 0,
            known_functions: &self.known_functions,
            free_id: (output_count + input_count) as u64,
        }
    }

    pub(crate) fn push_witness_value(&mut self, val: Value) {
        self.msg_build.push_witness_value(val);
    }

    pub(crate) fn push_instance_value(&mut self, val: Value) {
        self.msg_build.push_instance_value(val);
    }

    pub fn push_function(&mut self, function: Function) -> Result<()> {
        if self.known_functions.contains_key(&function.name) {
            return Err(format!("Function {} already exists !", function.name).into());
        } else {
            self.known_functions.insert(
                function.name.clone(),
                FunctionParams {
                    input_count: function.input_count,
                    output_count: function.output_count,
                    instance_count: function.instance_count,
                    witness_count: function.witness_count,
                },
            );
        }
        self.msg_build.push_function(function);
        Ok(())
    }

    pub fn finish(self) -> S {
        self.msg_build.finish()
    }
}

pub fn new_example_builder() -> GateBuilder<MemorySink> {
    GateBuilder::new(MemorySink::default(), Header::default())
}

/// FunctionBuilder builds a Function by allocating wire IDs and building gates.
/// finish() must be called to obtain the function.
/// The number of instances and witnesses consumed by the function are evaluated on the fly.
///
/// # Example
/// ```
/// use zki_sieve::producers::builder::{FunctionBuilder, GateBuilder,  BuildGate::*};
/// use zki_sieve::producers::sink::MemorySink;
/// use zki_sieve::Header;
///
/// let mut b = GateBuilder::new(MemorySink::default(), Header::default());
///
///  let witness_square = {
///     let mut fb = b.new_function_builder("witness_square".to_string(), 1, 0);
///     let witness_wire = fb.create_gate(Witness(None));
///     let output_wire = fb.create_gate(Mul(witness_wire, witness_wire));
///
///     fb.finish(vec![output_wire]).unwrap()
///  };
/// ```
pub struct FunctionBuilder<'a> {
    name: String,
    output_count: usize,
    input_count: usize,
    gates: Vec<Gate>,

    instance_count: usize, // evaluated on the fly
    witness_count: usize,  // evaluated on the fly
    known_functions: &'a HashMap<String, FunctionParams>,
    free_id: WireId,
}

impl FunctionBuilder<'_> {
    /// Returns a vector containing the inputs wire IDs.
    pub fn input_wire_ids(&self) -> Vec<WireId> {
        (self.output_count..(self.output_count + self.input_count))
            .map(|value| value as u64)
            .collect()
    }

    /// Updates instance_count and witness_count,
    /// Allocates a new wire id for the output and creates a new gate,
    /// Returns the newly allocated WireId.
    pub fn create_gate(&mut self, gate: BuildGate) -> WireId {
        let out_id = if gate.has_output() {
            alloc(&mut self.free_id)
        } else {
            NO_OUTPUT
        };

        match gate {
            BuildGate::Instance(_) => {
                self.instance_count += 1;
            }
            BuildGate::Witness(_) => {
                self.witness_count += 1;
            }
            _ => {}
        }

        self.gates.push(gate.with_output(out_id));

        out_id
    }

    /// Allocates some new wire ids for the output,
    /// Updates instance_count and witness_count,
    /// Creates a new gate,
    /// Returns the newly allocated WireIds.
    pub fn create_complex_gate(&mut self, gate: BuildComplexGate) -> Result<WireList> {
        // Check inputs size and return function_params
        let function_params = match gate {
            BuildComplexGate::Call(ref name, ref input_wires) => {
                let function_params = known_function_params(&self.known_functions, name)?;
                // Check inputs size
                let input_count = expand_wirelist(input_wires).len();
                if function_params.input_count != input_count {
                    return Err(format!(
                        "Function {} has {} inputs and is called with {} inputs.",
                        name, function_params.input_count, input_count
                    )
                    .into());
                }
                function_params
            }
        };
        let output_wires = multiple_alloc(&mut self.free_id, function_params.output_count);

        self.witness_count += function_params.witness_count;
        self.instance_count += function_params.instance_count;

        self.gates.push(gate.with_output(output_wires.clone()));

        Ok(output_wires)
    }

    // Creates and returns the Function
    pub fn finish(&mut self, output_wires: Vec<WireId>) -> Result<Function> {
        if output_wires.len() != self.output_count {
            return Err(format!(
                "Function {} should return {} outputs (and not {})",
                self.name,
                self.output_count,
                output_wires.len()
            )
            .into());
        }
        // Could we avoid to copy output wires ?
        for i in 0..output_wires.len() {
            self.gates.push(Gate::Copy(i as u64, output_wires[i]));
        }

        Ok(Function::new(
            self.name.clone(),
            self.output_count,
            self.input_count,
            self.instance_count,
            self.witness_count,
            self.gates.to_vec(),
        ))
    }
}

#[test]
fn test_builder_with_function() {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};
    use crate::consumers::source::Source;
    use crate::producers::builder::{BuildComplexGate::*, BuildGate::*, GateBuilder, GateBuilderT};
    use crate::producers::{examples, sink::MemorySink};
    use crate::structs::wire::{expand_wirelist, WireListElement::*};

    let mut b = GateBuilder::new(MemorySink::default(), examples::example_header());

    let custom_sub = {
        let mut fb = b.new_function_builder("custom_sub".to_string(), 2, 4);

        let input_wires = fb.input_wire_ids();
        let neg_input2_wire = fb.create_gate(MulConstant(input_wires[2], vec![100]));
        let neg_input3_wire = fb.create_gate(MulConstant(input_wires[3], vec![100]));
        let output0_wire = fb.create_gate(Add(input_wires[0], neg_input2_wire));
        let output1_wire = fb.create_gate(Add(input_wires[1], neg_input3_wire));
        let custom_sub = fb.finish(vec![output0_wire, output1_wire]).unwrap();
        custom_sub
    };

    b.push_function(custom_sub).unwrap();

    // Try to push two functions with the same name
    // It should return an error
    let custom_function = Function::new("custom_sub".to_string(), 0, 0, 0, 0, vec![]);
    assert!(b.push_function(custom_function).is_err());

    let id_0 = b.create_gate(Constant(vec![40]));
    let id_1 = b.create_gate(Constant(vec![30]));
    let id_2 = b.create_gate(Constant(vec![10]));
    let id_3 = b.create_gate(Constant(vec![5]));

    let out = b
        .create_complex_gate(
            Call(
                "custom_sub".to_string(),
                vec![Wire(id_0), Wire(id_1), Wire(id_2), Wire(id_3)],
            ),
            vec![],
            vec![],
        )
        .unwrap();
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
    assert!(b
        .create_complex_gate(
            Call("unknown_function".to_string(), vec![Wire(id_0)]),
            vec![],
            vec![]
        )
        .is_err());

    let sink = b.finish();

    let mut zkbackend = PlaintextBackend::default();
    let source: Source = sink.into();
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut zkbackend);
    assert_eq!(evaluator.get_violations().len(), 0);
}

#[test]
fn test_builder_with_several_functions() {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};
    use crate::consumers::source::Source;
    use crate::producers::builder::{BuildComplexGate::*, BuildGate::*, GateBuilder, GateBuilderT};
    use crate::producers::{examples, sink::MemorySink};
    use crate::structs::wire::{expand_wirelist, WireListElement::*};

    let mut b = GateBuilder::new(MemorySink::default(), examples::example_header());

    let witness_square = {
        let mut fb = b.new_function_builder("witness_square".to_string(), 1, 0);
        let witness_wire = fb.create_gate(Witness(None));
        let output_wire = fb.create_gate(Mul(witness_wire, witness_wire));

        fb.finish(vec![output_wire]).unwrap()
    };

    b.push_function(witness_square).unwrap();

    let sub_instance_witness_square = {
        let mut fb = b.new_function_builder("sub_instance_witness_square".to_string(), 1, 0);
        let instance_wire = fb.create_gate(Instance(None));

        // Try to call a function with a wrong number of inputs
        // Should return an error
        let test = fb.create_complex_gate(Call(
            "witness_square".to_string(),
            vec![Wire(instance_wire)],
        ));
        assert!(test.is_err());

        // Try to Call a not defined function
        // Should return an error
        let test = fb.create_complex_gate(Call("test".to_string(), vec![Wire(instance_wire)]));
        assert!(test.is_err());

        let witness_square_wires = fb
            .create_complex_gate(Call("witness_square".to_string(), vec![]))
            .unwrap();
        let witness_square_wires = expand_wirelist(&witness_square_wires);
        let neg_witness_square_wire =
            fb.create_gate(MulConstant(witness_square_wires[0], vec![100]));
        let output_wire = fb.create_gate(Add(instance_wire, neg_witness_square_wire));

        fb.finish(vec![output_wire]).unwrap()
    };

    b.push_function(sub_instance_witness_square).unwrap();

    // Try to call a function with a wrong number of instances
    // Should return an error
    let test = b.create_complex_gate(
        Call("sub_instance_witness_square".to_string(), vec![]),
        vec![],
        vec![vec![5]],
    );
    assert!(test.is_err());

    // Try to call a function with a wrong number of witnesses
    // Should return an error
    let test = b.create_complex_gate(
        Call("sub_instance_witness_square".to_string(), vec![]),
        vec![vec![25]],
        vec![],
    );
    assert!(test.is_err());

    let out = b
        .create_complex_gate(
            Call("sub_instance_witness_square".to_string(), vec![]),
            vec![vec![25]],
            vec![vec![5]],
        )
        .unwrap();
    let out = expand_wirelist(&out);
    assert_eq!(out.len(), 1);

    b.create_gate(AssertZero(out[0]));

    let sink = b.finish();

    let mut zkbackend = PlaintextBackend::default();
    let source: Source = sink.into();
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut zkbackend);
    assert_eq!(evaluator.get_violations().len(), 0);
}
