use serde::{Deserialize, Serialize};

use std::collections::HashMap;
use std::mem::take;

use super::build_gates::NO_OUTPUT;
pub use super::build_gates::{BuildComplexGate, BuildGate};
use crate::producers::sink::MemorySink;
use crate::structs::count::{
    count_list_max, vector_of_values_to_count_list, wirelist_to_count_list, CountList,
};
use crate::structs::gates::replace_output_wires;
use crate::structs::inputs::Inputs;
use crate::structs::relation::{ARITH, SIMPLE};
use crate::structs::wire::{WireList, WireListElement};
use crate::structs::{function::CaseInvoke, function::Function, value::Value};
use crate::Result;
use crate::{FieldId, Gate, Header, Instance, Relation, Sink, WireId, Witness};

pub trait GateBuilderT {
    /// Allocates a new wire id for the output and creates a new gate,
    /// Returns the newly allocated WireId.
    fn create_gate(&mut self, gate: BuildGate) -> Result<WireId>;

    /// Pushes instances and witnesses,
    /// Allocates some new wire ids for the output,
    /// Creates a new gate,
    /// Returns the newly allocated WireIds.
    fn create_complex_gate(
        &mut self,
        gate: BuildComplexGate,
        instances: Vec<Vec<Value>>,
        witnesses: Vec<Vec<Value>>,
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
        let common_inputs = vec![Inputs { inputs: vec![] }; header.fields.len()];
        let short_witness = vec![Inputs { inputs: vec![] }; header.fields.len()];
        Self {
            sink,
            instance: Instance {
                header: header.clone(),
                common_inputs,
            },
            witness: Witness {
                header: header.clone(),
                short_witness,
            },
            relation: Relation {
                header,
                gate_mask: gateset,
                feat_mask: features,
                functions: vec![],
                gates: vec![],
            },
            functions_size: 0,
            max_len: 100 * 1000,
        }
    }

    fn push_instance_value(&mut self, field_id: FieldId, value: Value) -> Result<()> {
        if let Some(inputs) = self.instance.common_inputs.get_mut(field_id as usize) {
            inputs.inputs.push(value);
        } else {
            return Err(format!(
                "Field {} is not defined, cannot push instance value.",
                field_id
            )
            .into());
        }
        if self.instance.get_instance_len() == self.max_len {
            self.flush_instance();
        }
        Ok(())
    }

    fn push_witness_value(&mut self, field_id: FieldId, value: Value) -> Result<()> {
        if let Some(inputs) = self.witness.short_witness.get_mut(field_id as usize) {
            inputs.inputs.push(value);
        } else {
            return Err(format!(
                "Field {} is not defined, cannot push witness value.",
                field_id
            )
            .into());
        }
        if self.witness.get_witness_len() == self.max_len {
            self.flush_witness();
        }
        Ok(())
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
        for inputs in &mut self.instance.common_inputs {
            inputs.inputs.clear();
        }
    }

    fn flush_witness(&mut self) {
        self.sink.push_witness_message(&self.witness).unwrap();
        for inputs in &mut self.witness.short_witness {
            inputs.inputs.clear();
        }
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

    fn nb_fields(&self) -> u8 {
        let result = self.relation.header.fields.len();
        assert!(result <= u8::MAX as usize);
        result as u8
    }
}

/// GateBuilder allocates wire IDs, builds gates, and tracks instance and witness values.
///
/// # Example
/// ```
/// use zki_sieve::producers::builder::{GateBuilderT, GateBuilder, BuildGate::*};
/// use zki_sieve::producers::sink::MemorySink;
/// use zki_sieve::structs::relation::{BOOL, SIMPLE};
/// use zki_sieve::Header;
///
/// let mut b = GateBuilder::new(MemorySink::default(), Header::default(), BOOL, SIMPLE);
///
/// let field_id = 0;
/// let my_id = b.create_gate(Constant(field_id, vec![0])).unwrap();
/// b.create_gate(AssertZero(field_id, my_id)).unwrap();
/// ```
pub struct GateBuilder<S: Sink> {
    msg_build: MessageBuilder<S>,

    // name => FunctionParams
    known_functions: HashMap<String, FunctionParams>,
    free_id: HashMap<FieldId, WireId>,
}

/// FunctionParams contains the number of inputs, outputs, instances and witnesses of a function.
//#[derive(Clone, Copy)]
struct FunctionParams {
    input_count: CountList,
    output_count: CountList,
    instance_count: CountList,
    witness_count: CountList,
}

impl FunctionParams {
    fn check(
        &self,
        name: &str,
        input_count: Option<CountList>,
        output_count: Option<CountList>,
        instance_count: Option<CountList>,
        witness_count: Option<CountList>,
    ) -> Result<()> {
        if let Some(count) = input_count {
            if count != self.input_count {
                return Err(format!(
                    "Function {} has {:?} inputs and is called with {:?} inputs.",
                    name, self.input_count, count
                )
                .into());
            }
        }
        if let Some(count) = output_count {
            if count != self.output_count {
                return Err(format!(
                    "Function {} has {:?} outputs and is called with {:?} outputs.",
                    name, self.output_count, count
                )
                .into());
            }
        }
        if let Some(count) = instance_count {
            if count != self.instance_count {
                return Err(format!(
                    "Function {} has {:?} instances and is called with {:?} instances.",
                    name, self.instance_count, count
                )
                .into());
            }
        }
        if let Some(count) = witness_count {
            if count != self.witness_count {
                return Err(format!(
                    "Function {} has {:?} witnesses and is called with {:?} witnesses.",
                    name, self.witness_count, count
                )
                .into());
            }
        }
        Ok(())
    }
}

/// known_function_params returns the FunctionParams of the function with name `name`.
/// If no function with name `name` belongs to the HashMap `known_functions`, then it returns an error.
fn known_function_params<'a>(
    known_functions: &'a HashMap<String, FunctionParams>,
    name: &str,
) -> Result<&'a FunctionParams> {
    match known_functions.get(name) {
        None => Err(format!("Function {} does not exist !", name).into()),
        Some(v) => Ok(v),
    }
}

/// alloc allocates a new wire ID.
fn alloc(field_id: FieldId, free_id: &mut HashMap<FieldId, WireId>) -> WireId {
    let id = free_id.entry(field_id).or_insert(0);
    let out_id = *id;
    *id = out_id + 1;
    out_id
}

/// alloc allocates n wire IDs.
fn multiple_alloc(field_id: FieldId, free_id: &mut HashMap<FieldId, WireId>, n: usize) -> WireList {
    match n {
        0 => vec![],
        1 => vec![WireListElement::Wire(field_id, alloc(field_id, free_id))],
        _ => {
            let id = free_id.entry(field_id).or_insert(0);
            let first_id = *id;
            let next: u64 = first_id + n as u64;
            *id = next;
            vec![WireListElement::WireRange(field_id, first_id, next - 1)]
        }
    }
}

impl<S: Sink> GateBuilderT for GateBuilder<S> {
    fn create_gate(&mut self, mut gate: BuildGate) -> Result<WireId> {
        let field_id = gate.get_field();
        if field_id >= self.msg_build.nb_fields() {
            return Err(format!(
                "Field {} is not defined, we cannot create the gate",
                field_id
            )
            .into());
        }
        let out_id = if gate.has_output() {
            alloc(field_id, &mut self.free_id)
        } else {
            NO_OUTPUT
        };

        match gate {
            BuildGate::Instance(_, Some(ref mut value)) => {
                self.push_instance_value(field_id, take(value))?;
            }
            BuildGate::Witness(_, Some(ref mut value)) => {
                self.push_witness_value(field_id, take(value))?;
            }
            _ => {}
        }

        self.msg_build.push_gate(gate.with_output(out_id));

        Ok(out_id)
    }

    fn create_complex_gate(
        &mut self,
        gate: BuildComplexGate,
        instances: Vec<Vec<Value>>,
        witnesses: Vec<Vec<Value>>,
    ) -> Result<WireList> {
        // Check inputs, instances, witnesses size and return output_count
        let output_count: CountList = match gate {
            BuildComplexGate::Call(ref name, ref input_wires) => {
                let function_params = known_function_params(&self.known_functions, name)?;
                let input_count = wirelist_to_count_list(input_wires);
                let instances_count = vector_of_values_to_count_list(&instances);
                let witnesses_count = vector_of_values_to_count_list(&witnesses);
                function_params.check(
                    name,
                    Some(input_count),
                    None,
                    Some(instances_count),
                    Some(witnesses_count),
                )?;
                function_params.output_count.clone()
            }
            BuildComplexGate::Switch(_, _, _, _, ref params) => {
                let instances_count = vector_of_values_to_count_list(&instances);
                let witnesses_count = vector_of_values_to_count_list(&witnesses);
                params.check(None, Some(instances_count), Some(witnesses_count))?;
                params.output_count.clone()
            }
            BuildComplexGate::Convert(field_id, output_wire_count, _) => {
                // TODO convert_gate: check that the convert gate with this signature has already been declared
                // Check that we have no instance and no witness
                if !instances.is_empty() {
                    return Err("A Convert gate does not contain an instance".into());
                }
                if !witnesses.is_empty() {
                    return Err("A Convert gate does not contain a witness".into());
                }
                HashMap::from([(field_id, output_wire_count)])
            }
        };

        // Push instances
        for (i, values) in instances.iter().enumerate() {
            for value in values {
                assert!(i <= u8::MAX as usize);
                self.msg_build.push_instance_value(i as u8, value.clone())?;
            }
        }
        // Push witnesses
        for (i, values) in witnesses.iter().enumerate() {
            for value in values {
                assert!(i <= u8::MAX as usize);
                self.msg_build.push_witness_value(i as u8, value.clone())?;
            }
        }

        let mut output_wires: WireList = vec![];
        for (field_id, count) in output_count {
            let wires = multiple_alloc(field_id, &mut self.free_id, count as usize);
            output_wires.extend(wires);
        }

        self.msg_build
            .push_gate(gate.with_output(output_wires.clone()));
        Ok(output_wires)
    }
}

impl<S: Sink> GateBuilder<S> {
    /// new creates a new builder.
    pub fn new(sink: S, header: Header, gateset: u16, features: u16) -> Self {
        GateBuilder {
            msg_build: MessageBuilder::new(sink, header, gateset, features),
            known_functions: HashMap::new(),
            free_id: HashMap::new(),
        }
    }

    pub fn new_function_builder(
        &self,
        name: String,
        output_count: CountList,
        input_count: CountList,
    ) -> FunctionBuilder {
        let mut free_id = HashMap::new();
        output_count.iter().for_each(|(field_id, count)| {
            free_id.insert(*field_id, *count);
        });
        input_count.iter().for_each(|(field_id, count)| {
            let field_id_count = free_id.entry(*field_id).or_insert(0);
            *field_id_count += count;
        });
        FunctionBuilder {
            name,
            output_count,
            input_count,
            gates: vec![],
            instance_count: HashMap::new(),
            witness_count: HashMap::new(),
            known_functions: &self.known_functions,
            free_id,
        }
    }

    pub fn new_switch_builder(&self, output_count: CountList) -> SwitchBuilder {
        SwitchBuilder {
            output_count,
            cases: vec![],
            branches: vec![],
            instance_count: HashMap::new(), // evaluated on the fly
            witness_count: HashMap::new(),  // evaluated on the fly
            known_functions: &self.known_functions,
        }
    }

    pub(crate) fn push_witness_value(&mut self, field_id: FieldId, val: Value) -> Result<()> {
        self.msg_build.push_witness_value(field_id, val)
    }

    pub(crate) fn push_instance_value(&mut self, field_id: FieldId, val: Value) -> Result<()> {
        self.msg_build.push_instance_value(field_id, val)
    }

    pub fn push_function(&mut self, function: Function) -> Result<()> {
        if self.known_functions.contains_key(&function.name) {
            return Err(format!("Function {} already exists !", function.name).into());
        } else {
            self.known_functions.insert(
                function.name.clone(),
                FunctionParams {
                    input_count: function.input_count.clone(),
                    output_count: function.output_count.clone(),
                    instance_count: function.instance_count.clone(),
                    witness_count: function.witness_count.clone(),
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
    GateBuilder::new(MemorySink::default(), Header::default(), ARITH, SIMPLE)
}

/// FunctionBuilder builds a Function by allocating wire IDs and building gates.
/// finish() must be called to obtain the function.
/// The number of instances and witnesses consumed by the function are evaluated on the fly.
///
/// # Example
/// ```
/// use std::collections::HashMap;
/// use zki_sieve::producers::builder::{FunctionBuilder, GateBuilder,  BuildGate::*};
/// use zki_sieve::producers::sink::MemorySink;
/// use zki_sieve::structs::relation::{ARITH, FUNCTION};
/// use zki_sieve::structs::wire::WireListElement;
/// use zki_sieve::wirelist;
/// use zki_sieve::Header;
///
/// let mut b = GateBuilder::new(MemorySink::default(), Header::default(), ARITH, FUNCTION);
///
///  let witness_square = {
///     let mut fb = b.new_function_builder("witness_square".to_string(), HashMap::from([(0,1)]), HashMap::new());
///     let witness_wire = fb.create_gate(Witness(0, None));
///     let output_wire = fb.create_gate(Mul(0, witness_wire, witness_wire));
///
///     fb.finish(wirelist![0; output_wire]).unwrap()
///  };
/// ```
pub struct FunctionBuilder<'a> {
    name: String,
    output_count: CountList,
    input_count: CountList,
    gates: Vec<Gate>,

    instance_count: CountList, // evaluated on the fly
    witness_count: CountList,  // evaluated on the fly
    known_functions: &'a HashMap<String, FunctionParams>,
    free_id: HashMap<FieldId, WireId>,
}

impl FunctionBuilder<'_> {
    /// Returns a Vec<(FieldId, WireId)> containing the inputs wires (without WireRange).
    pub fn input_wires(&self) -> Vec<(FieldId, WireId)> {
        let mut map = HashMap::new();
        for (field_id, count) in self.output_count.iter() {
            map.insert(*field_id, *count);
        }
        let mut result: Vec<(FieldId, WireId)> = vec![];
        for (field_id, count) in self.input_count.iter() {
            let field_id_count = map.entry(*field_id).or_insert(0);
            for id in *field_id_count..(*field_id_count + *count) {
                result.push((*field_id, id));
            }
        }
        result
    }

    /// Updates instance_count and witness_count,
    /// Allocates a new wire id for the output and creates a new gate,
    /// Returns the newly allocated WireId.
    pub fn create_gate(&mut self, gate: BuildGate) -> WireId {
        let field_id = gate.get_field();
        let out_id = if gate.has_output() {
            alloc(field_id, &mut self.free_id)
        } else {
            NO_OUTPUT
        };

        match gate {
            BuildGate::Instance(instance_field_id, _) => {
                let count = self.instance_count.entry(instance_field_id).or_insert(0);
                *count += 1;
            }
            BuildGate::Witness(witness_field_id, _) => {
                let count = self.witness_count.entry(witness_field_id).or_insert(0);
                *count += 1;
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
        let (output_count, instance_count, witness_count) = match gate {
            BuildComplexGate::Call(ref name, ref input_wires) => {
                let function_params = known_function_params(self.known_functions, name)?;
                // Check inputs size
                let input_count = wirelist_to_count_list(input_wires);
                if function_params.input_count != input_count {
                    return Err(format!(
                        "Function {} has {:?} inputs and is called with {:?} inputs.",
                        name, function_params.input_count, input_count
                    )
                    .into());
                }
                (
                    function_params.output_count.clone(),
                    function_params.instance_count.clone(),
                    function_params.witness_count.clone(),
                )
            }
            BuildComplexGate::Switch(_, _, _, _, ref params) => (
                params.output_count.clone(),
                params.instance_count.clone(),
                params.witness_count.clone(),
            ),
            BuildComplexGate::Convert(field_id, output_wire_count, _) => (
                HashMap::from([(field_id, output_wire_count)]),
                HashMap::new(),
                HashMap::new(),
            ),
        };

        let mut output_wires: WireList = vec![];
        for (field_id, count) in output_count {
            output_wires.extend(multiple_alloc(field_id, &mut self.free_id, count as usize));
        }

        for (field_id, count) in witness_count {
            let field_witness_count = self.witness_count.entry(field_id).or_insert(0);
            *field_witness_count += count;
        }
        for (field_id, count) in instance_count {
            let field_instance_count = self.instance_count.entry(field_id).or_insert(0);
            *field_instance_count += count;
        }

        self.gates.push(gate.with_output(output_wires.clone()));

        Ok(output_wires)
    }

    // Creates and returns the Function
    pub fn finish(&mut self, output_wires: WireList) -> Result<Function> {
        let given_output_count: CountList = wirelist_to_count_list(&output_wires);
        if given_output_count != self.output_count {
            return Err(format!(
                "Function {} should return {:?} outputs (and not {:?})",
                self.name, self.output_count, given_output_count
            )
            .into());
        }

        replace_output_wires(&mut self.gates, &output_wires)?;

        Ok(Function::new(
            self.name.clone(),
            self.output_count.clone(),
            self.input_count.clone(),
            self.instance_count.clone(),
            self.witness_count.clone(),
            self.gates.to_vec(),
        ))
    }
}

/// SwitchBuilder builds a Switch by creating branches from defined functions.
/// finish() must be called to obtain the switch.
/// The number of instances and witnesses consumed by the switch are evaluated on the fly.
///
/// # Example
/// ```
/// use std::collections::HashMap;
/// use zki_sieve::producers::builder::{FunctionBuilder, GateBuilder, GateBuilderT, BuildGate::*};
/// use zki_sieve::producers::sink::MemorySink;
/// use zki_sieve::Header;
/// use zki_sieve::structs::relation::{ARITH, FOR_FUNCTION_SWITCH};
/// use zki_sieve::structs::wire::WireListElement;
/// use zki_sieve::wirelist;
///
/// let mut b = GateBuilder::new(MemorySink::default(), Header::default(), ARITH, FOR_FUNCTION_SWITCH);
///
/// let custom_sub = {
///     let mut fb = b.new_function_builder("custom_sub".to_string(), HashMap::from([(0,2)]), HashMap::from([(0,4)]));
///
///     let input_wires = fb.input_wires();
///     let neg_input2_wire = fb.create_gate(MulConstant(0, input_wires[2].1, vec![100]));
///     let neg_input3_wire = fb.create_gate(MulConstant(0, input_wires[3].1, vec![100]));
///     let output0_wire = fb.create_gate(Add(0, input_wires[0].1, neg_input2_wire));
///     let output1_wire = fb.create_gate(Add(0, input_wires[1].1, neg_input3_wire));
///     fb.finish(vec![WireListElement::Wire(0,output0_wire), WireListElement::Wire(0,output1_wire)]).unwrap()
/// };
/// b.push_function(custom_sub).unwrap();
///
/// let custom_add = {
///     let mut fb = b.new_function_builder("custom_add".to_string(), HashMap::from([(0,2)]), HashMap::from([(0,2)]));
///
///     let input_wires = fb.input_wires();
///     let instance = fb.create_gate(Instance(0, None));
///     let witness = fb.create_gate(Witness(0, None));
///     let output0_wire = fb.create_gate(Add(0, input_wires[0].1, instance));
///     let output1_wire = fb.create_gate(Add(0, input_wires[1].1, witness));
///     fb.finish(vec![WireListElement::Wire(0,output0_wire), WireListElement::Wire(0,output1_wire)]).unwrap()
/// };
/// b.push_function(custom_add).unwrap();
///
/// let branch_input_0 = b.create_gate(Constant(0, vec![10])).unwrap();
/// let branch_input_1 = b.create_gate(Constant(0, vec![15])).unwrap();
/// let branch_input_2 = b.create_gate(Instance(0, Some(vec![20]))).unwrap();
/// let branch_input_3 = b.create_gate(Witness(0, Some(vec![25]))).unwrap();
/// let branch_condition = b.create_gate(Constant(0, vec![3])).unwrap();
///
///  let custom_switch = {
///     let mut sb = b.new_switch_builder(HashMap::from([(0, 2)]));
///
///     let branch0 = sb
///         .create_branch_from(
///             "custom_sub".to_string(),
///             wirelist![0; branch_input_0, branch_input_1, branch_input_2, branch_input_3],
///         )
///         .unwrap();
///     sb.push_branch(branch0, vec![10]).unwrap();
///
///     let branch1 = sb
///         .create_branch_from(
///             "custom_add".to_string(),
///             wirelist![0; branch_input_1, branch_input_0],
///         )
///         .unwrap();
///     sb.push_branch(branch1, vec![3]).unwrap();
///     sb.finish(0, branch_condition).unwrap()
///  };
///  let branch_out = b
///     .create_complex_gate(custom_switch, vec![vec![vec![5]]], vec![vec![vec![15]]])
///     .unwrap();
/// ```
pub struct SwitchBuilder<'a> {
    output_count: CountList,

    cases: Vec<Value>,
    branches: Vec<CaseInvoke>,

    instance_count: CountList, // evaluated on the fly
    witness_count: CountList,  // evaluated on the fly
    known_functions: &'a HashMap<String, FunctionParams>,
}

impl SwitchBuilder<'_> {
    /// Creates a branch by calling the function `name` on inputs `inputs`
    pub fn create_branch_from(&self, name: String, inputs: WireList) -> Result<BranchBuilder> {
        // Check that the function exists
        let function_params = known_function_params(self.known_functions, &name)?;
        // Check input_count
        let input_count = wirelist_to_count_list(&inputs);
        function_params.check(&name, Some(input_count), None, None, None)?;
        Ok(BranchBuilder {
            branch: CaseInvoke::AbstractGateCall(name.clone(), inputs),
            params: FunctionParams {
                input_count: function_params.input_count.clone(),
                output_count: function_params.output_count.clone(),
                instance_count: function_params.instance_count.clone(),
                witness_count: function_params.witness_count.clone(),
            },
        })
    }

    pub fn push_branch(&mut self, branch: BranchBuilder, case: Value) -> Result<()> {
        // Check output_count
        if self.output_count != branch.params.output_count {
            return Err(format!(
                "The switch has {:?} outputs and the branch has {:?} outputs.",
                self.output_count, branch.params.output_count
            )
            .into());
        }

        // Check that the case value does not already exist
        for value in &self.cases {
            if *value == case {
                return Err(
                    "You cannot create a switch with two cases with the same value.".into(),
                );
            }
        }

        // Update instance_count and witness_count
        count_list_max(&mut self.instance_count, &branch.params.instance_count);
        count_list_max(&mut self.witness_count, &branch.params.witness_count);

        // Add the branch and the case in the SwitchBuilder
        self.cases.push(case);
        self.branches.push(branch.branch);

        Ok(())
    }

    pub fn finish(
        self,
        condition_field: FieldId,
        condition_wire: WireId,
    ) -> Result<BuildComplexGate> {
        if self.branches.len() != self.cases.len() {
            return Err(format!(
                "The switch has {} branches and {} cases.",
                self.branches.len(),
                self.cases.len()
            )
            .into());
        }
        if self.branches.is_empty() {
            return Err("Cannot create an empty switch !".into());
        }
        let params = SwitchParams {
            output_count: self.output_count,
            instance_count: self.instance_count,
            witness_count: self.witness_count,
        };
        Ok(BuildComplexGate::Switch(
            condition_field,
            condition_wire,
            self.cases,
            self.branches,
            params,
        ))
    }
}

/// SwitchParams contains the number of inputs, instances and witnesses of a switch.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct SwitchParams {
    output_count: CountList,
    instance_count: CountList,
    witness_count: CountList,
}

impl SwitchParams {
    fn check(
        &self,
        output_count: Option<CountList>,
        instance_count: Option<CountList>,
        witness_count: Option<CountList>,
    ) -> Result<()> {
        if let Some(count) = output_count {
            if count != self.output_count {
                return Err(format!(
                    "Switch has {:?} outputs and is called with {:?} outputs.",
                    self.output_count, count
                )
                .into());
            }
        }
        if let Some(count) = instance_count {
            if count != self.instance_count {
                return Err(format!(
                    "Switch has {:?} instances and is called with {:?} instances.",
                    self.instance_count, count
                )
                .into());
            }
        }
        if let Some(count) = witness_count {
            if count != self.witness_count {
                return Err(format!(
                    "Switch has {:?} witnesses and is called with {:?} witnesses.",
                    self.instance_count, count
                )
                .into());
            }
        }
        Ok(())
    }
}

pub struct BranchBuilder {
    branch: CaseInvoke,
    params: FunctionParams,
}

#[test]
fn test_builder_with_function() {
    use std::collections::HashMap;

    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};
    use crate::consumers::source::Source;
    use crate::producers::builder::{BuildComplexGate::*, BuildGate::*, GateBuilder, GateBuilderT};
    use crate::producers::{examples, sink::MemorySink};
    use crate::structs::relation::FOR_FUNCTION_SWITCH;
    use crate::structs::wire::expand_wirelist;
    use crate::wirelist;

    let mut b = GateBuilder::new(
        MemorySink::default(),
        examples::example_header(),
        ARITH,
        FOR_FUNCTION_SWITCH,
    );

    let custom_sub = {
        let mut fb = b.new_function_builder(
            "custom_sub".to_string(),
            HashMap::from([(0, 2)]),
            HashMap::from([(0, 4)]),
        );

        let input_wires = fb.input_wires();
        let neg_input2_wire = fb.create_gate(MulConstant(0, input_wires[2].1, vec![100]));
        let neg_input3_wire = fb.create_gate(MulConstant(0, input_wires[3].1, vec![100]));
        let output0_wire = fb.create_gate(Add(0, input_wires[0].1, neg_input2_wire));
        let output1_wire = fb.create_gate(Add(0, input_wires[1].1, neg_input3_wire));
        let custom_sub = fb.finish(wirelist![0; output0_wire, output1_wire]).unwrap();
        custom_sub
    };

    b.push_function(custom_sub).unwrap();

    // Try to push two functions with the same name
    // It should return an error
    let custom_function = Function::new(
        "custom_sub".to_string(),
        HashMap::new(),
        HashMap::new(),
        HashMap::new(),
        HashMap::new(),
        vec![],
    );
    assert!(b.push_function(custom_function).is_err());

    let id_0 = b.create_gate(Constant(0, vec![40])).unwrap();
    let id_1 = b.create_gate(Constant(0, vec![30])).unwrap();
    let id_2 = b.create_gate(Constant(0, vec![10])).unwrap();
    let id_3 = b.create_gate(Constant(0, vec![5])).unwrap();

    let out = b
        .create_complex_gate(
            Call(
                "custom_sub".to_string(),
                wirelist![0;id_0, id_1, id_2, id_3],
            ),
            vec![],
            vec![],
        )
        .unwrap();
    let out = expand_wirelist(&out).unwrap();
    assert_eq!(out.len(), 2);

    let witness_0 = b.create_gate(Witness(0, Some(vec![30]))).unwrap();
    let witness_1 = b.create_gate(Witness(0, Some(vec![25]))).unwrap();

    let neg_witness_0 = b.create_gate(MulConstant(0, witness_0, vec![100])).unwrap(); // *(-1)
    let neg_witness_1 = b.create_gate(MulConstant(0, witness_1, vec![100])).unwrap(); // *(-1)

    let res_0 = b.create_gate(Add(0, out[0].1, neg_witness_0)).unwrap();
    let res_1 = b.create_gate(Add(0, out[1].1, neg_witness_1)).unwrap();

    b.create_gate(AssertZero(0, res_0)).unwrap();
    b.create_gate(AssertZero(0, res_1)).unwrap();

    // Try to call an unknown function
    // It should return an error
    assert!(b
        .create_complex_gate(
            Call("unknown_function".to_string(), wirelist![0;id_0]),
            vec![],
            vec![]
        )
        .is_err());

    let sink = b.finish();

    let mut zkbackend = PlaintextBackend::default();
    let source: Source = sink.into();
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut zkbackend);
    assert_eq!(evaluator.get_violations(), Vec::<String>::new());
}

#[test]
fn test_builder_with_several_functions() {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};
    use crate::consumers::source::Source;
    use crate::producers::builder::{BuildComplexGate::*, BuildGate::*, GateBuilder, GateBuilderT};
    use crate::producers::{examples, sink::MemorySink};
    use crate::structs::relation::FOR_FUNCTION_SWITCH;
    use crate::structs::wire::expand_wirelist;
    use crate::wirelist;

    let field_id: FieldId = 0;

    let mut b = GateBuilder::new(
        MemorySink::default(),
        examples::example_header(),
        ARITH,
        FOR_FUNCTION_SWITCH,
    );

    let witness_square = {
        let mut fb = b.new_function_builder(
            "witness_square".to_string(),
            HashMap::from([(0, 1)]),
            HashMap::new(),
        );
        let witness_wire = fb.create_gate(Witness(field_id, None));
        let output_wire = fb.create_gate(Mul(field_id, witness_wire, witness_wire));

        fb.finish(wirelist![0; output_wire]).unwrap()
    };

    b.push_function(witness_square).unwrap();

    let sub_instance_witness_square = {
        let mut fb = b.new_function_builder(
            "sub_instance_witness_square".to_string(),
            HashMap::from([(0, 1)]),
            HashMap::new(),
        );
        let instance_wire = fb.create_gate(Instance(field_id, None));

        // Try to call a function with a wrong number of inputs
        // Should return an error
        let test = fb.create_complex_gate(Call(
            "witness_square".to_string(),
            wirelist![0; instance_wire],
        ));
        assert!(test.is_err());

        // Try to Call a not defined function
        // Should return an error
        let test = fb.create_complex_gate(Call("test".to_string(), wirelist![0;instance_wire]));
        assert!(test.is_err());

        let witness_square_wires = fb
            .create_complex_gate(Call("witness_square".to_string(), vec![]))
            .unwrap();
        let witness_square_wires = expand_wirelist(&witness_square_wires).unwrap();
        let neg_witness_square_wire =
            fb.create_gate(MulConstant(field_id, witness_square_wires[0].1, vec![100]));
        let output_wire = fb.create_gate(Add(field_id, instance_wire, neg_witness_square_wire));

        fb.finish(wirelist![0;output_wire]).unwrap()
    };

    b.push_function(sub_instance_witness_square).unwrap();

    // Try to call a function with a wrong number of instances
    // Should return an error
    let test = b.create_complex_gate(
        Call("sub_instance_witness_square".to_string(), vec![]),
        vec![],
        vec![vec![vec![5]]],
    );
    assert!(test.is_err());

    // Try to call a function with a wrong number of witnesses
    // Should return an error
    let test = b.create_complex_gate(
        Call("sub_instance_witness_square".to_string(), vec![]),
        vec![vec![vec![25]]],
        vec![],
    );
    assert!(test.is_err());

    let out = b
        .create_complex_gate(
            Call("sub_instance_witness_square".to_string(), vec![]),
            vec![vec![vec![25]]],
            vec![vec![vec![5]]],
        )
        .unwrap();
    let out = expand_wirelist(&out).unwrap();
    assert_eq!(out.len(), 1);

    b.create_gate(AssertZero(field_id, out[0].1)).unwrap();

    let sink = b.finish();

    let mut zkbackend = PlaintextBackend::default();
    let source: Source = sink.into();
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut zkbackend);
    assert_eq!(evaluator.get_violations(), Vec::<String>::new());
}

#[test]
fn test_switch_builder() {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};
    use crate::consumers::source::Source;
    use crate::producers::builder::{BuildComplexGate::*, BuildGate::*, GateBuilder, GateBuilderT};
    use crate::producers::{examples, sink::MemorySink};
    use crate::structs::relation::FOR_FUNCTION_SWITCH;
    use crate::structs::wire::expand_wirelist;
    use crate::wirelist;

    let mut b = GateBuilder::new(
        MemorySink::default(),
        examples::example_header(),
        ARITH,
        FOR_FUNCTION_SWITCH,
    );

    let custom_sub = {
        let mut fb = b.new_function_builder(
            "custom_sub".to_string(),
            HashMap::from([(0, 2)]),
            HashMap::from([(0, 2)]),
        );

        let input_wires = fb.input_wires();
        let instance = fb.create_gate(Instance(0, None));
        let witness = fb.create_gate(Witness(0, None));
        let neg_instance = fb.create_gate(MulConstant(0, instance, vec![100]));
        let neg_witness = fb.create_gate(MulConstant(0, witness, vec![100]));
        let output0_wire = fb.create_gate(Add(0, input_wires[0].1, neg_instance));
        let output1_wire = fb.create_gate(Add(0, input_wires[1].1, neg_witness));
        fb.finish(wirelist![0;output0_wire, output1_wire]).unwrap()
    };
    b.push_function(custom_sub).unwrap();

    let custum_add = {
        let mut fb = b.new_function_builder(
            "custom_add".to_string(),
            HashMap::from([(0, 2)]),
            HashMap::from([(0, 2)]),
        );
        let input_wires = fb.input_wires();
        let instance = fb.create_gate(Instance(0, None));
        let witness = fb.create_gate(Witness(0, None));
        let out0 = fb.create_gate(Add(0, input_wires[0].1, instance));
        let out1 = fb.create_gate(Add(0, input_wires[1].1, witness));
        let witness2 = fb.create_gate(Witness(0, None));
        fb.create_gate(AssertZero(0, witness2));
        fb.finish(wirelist![0;out0, out1]).unwrap()
    };
    b.push_function(custum_add).unwrap();

    let assert_equal_witness = {
        let mut fb = b.new_function_builder(
            "assert_equal_witness".to_string(),
            HashMap::new(),
            HashMap::from([(0, 1)]),
        );
        let input_wires = fb.input_wires();
        let witness = fb.create_gate(Witness(0, None));
        let neg_witness = fb.create_gate(MulConstant(0, witness, vec![100]));
        let add_result = fb.create_gate(Add(0, input_wires[0].1, neg_witness));
        fb.create_gate(AssertZero(0, add_result));
        fb.finish(vec![]).unwrap()
    };
    b.push_function(assert_equal_witness).unwrap();

    let branch_input_0 = b.create_gate(Constant(0, vec![10])).unwrap();
    let branch_input_1 = b.create_gate(Constant(0, vec![15])).unwrap();
    let branch_condition = b.create_gate(Constant(0, vec![1])).unwrap();

    let switch = {
        let mut sb = b.new_switch_builder(HashMap::from([(0, 2)]));
        // Try to create a call branch with an unknown function
        // Should return an error
        let branch_test = sb.create_branch_from(
            "unknown_function".to_string(),
            wirelist![0;branch_input_0, branch_input_1],
        );
        assert!(branch_test.is_err());

        let branch0 = sb
            .create_branch_from(
                "custom_sub".to_string(),
                wirelist![0;branch_input_0, branch_input_1],
            )
            .unwrap();
        sb.push_branch(branch0, vec![0]).unwrap();

        // Create a branch with the same case value as the previous branch
        // Should return an error
        let branch1 = sb
            .create_branch_from(
                "custom_add".to_string(),
                wirelist![0;branch_input_0, branch_input_1],
            )
            .unwrap();
        let test = sb.push_branch(branch1, vec![0]);
        assert!(test.is_err());

        let branch1 = sb
            .create_branch_from(
                "custom_add".to_string(),
                wirelist![0;branch_input_0, branch_input_1],
            )
            .unwrap();
        sb.push_branch(branch1, vec![1]).unwrap();
        sb.finish(0, branch_condition).unwrap()
    };
    let branch_out = b
        .create_complex_gate(switch, vec![vec![vec![5]]], vec![vec![vec![15], vec![0]]])
        .unwrap();
    let branch_out = expand_wirelist(&branch_out).unwrap();
    b.create_complex_gate(
        Call(
            "assert_equal_witness".to_string(),
            wirelist![branch_out[0].0;branch_out[0].1],
        ),
        vec![],
        vec![vec![vec![15]]],
    )
    .unwrap();
    b.create_complex_gate(
        Call(
            "assert_equal_witness".to_string(),
            wirelist![branch_out[0].0;branch_out[1].1],
        ),
        vec![],
        vec![vec![vec![30]]],
    )
    .unwrap();

    // Try to create a switch without branch
    // Should return an error
    let sb = b.new_switch_builder(HashMap::new());
    let switch = sb.finish(0, branch_condition);
    assert!(switch.is_err());

    let value_55 = b.create_gate(Constant(0, vec![55])).unwrap();
    let condition = b.create_gate(Constant(0, vec![60])).unwrap();

    // Try to create a switch with only one branch (should succeed)
    let switch = {
        let mut sb = b.new_switch_builder(HashMap::new());
        let branch = sb
            .create_branch_from("assert_equal_witness".to_string(), wirelist![0;value_55])
            .unwrap();
        sb.push_branch(branch, vec![60]).unwrap();
        sb.finish(0, condition).unwrap()
    };
    b.create_complex_gate(switch, vec![], vec![vec![vec![55]]])
        .unwrap();

    // Try to call a switch with a wrong number of instances or witnesses
    // Should return an error
    let switch = {
        let mut sb = b.new_switch_builder(HashMap::new());
        let branch = sb
            .create_branch_from("assert_equal_witness".to_string(), wirelist![0;value_55])
            .unwrap();
        sb.push_branch(branch, vec![60]).unwrap();
        sb.finish(0, condition).unwrap()
    };
    let result = b.create_complex_gate(switch, vec![], vec![]);
    assert!(result.is_err());

    let sink = b.finish();

    let mut zkbackend = PlaintextBackend::default();
    let source: Source = sink.into();
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut zkbackend);
    assert_eq!(evaluator.get_violations(), Vec::<String>::new());
}

#[test]
fn test_switch_nested_in_function() {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};
    use crate::consumers::source::Source;
    use crate::producers::builder::{BuildComplexGate::*, BuildGate::*, GateBuilder, GateBuilderT};
    use crate::producers::{examples, sink::MemorySink};
    use crate::structs::relation::FOR_FUNCTION_SWITCH;
    use crate::structs::wire::expand_wirelist;
    use crate::wirelist;

    let mut b = GateBuilder::new(
        MemorySink::default(),
        examples::example_header(),
        ARITH,
        FOR_FUNCTION_SWITCH,
    );

    let custom_sub = {
        let mut fb = b.new_function_builder(
            "custom_sub".to_string(),
            HashMap::from([(0, 2)]),
            HashMap::from([(0, 2)]),
        );

        let input_wires = fb.input_wires();
        let instance = fb.create_gate(Instance(0, None));
        let witness = fb.create_gate(Witness(0, None));
        let neg_instance = fb.create_gate(MulConstant(0, instance, vec![100]));
        let neg_witness = fb.create_gate(MulConstant(0, witness, vec![100]));
        let output0_wire = fb.create_gate(Add(0, input_wires[0].1, neg_instance));
        let output1_wire = fb.create_gate(Add(0, input_wires[1].1, neg_witness));
        fb.finish(wirelist![0;output0_wire, output1_wire]).unwrap()
    };
    b.push_function(custom_sub).unwrap();

    let custum_add = {
        let mut fb = b.new_function_builder(
            "custom_add".to_string(),
            HashMap::from([(0, 2)]),
            HashMap::from([(0, 2)]),
        );
        let input_wires = fb.input_wires();
        let instance = fb.create_gate(Instance(0, None));
        let witness = fb.create_gate(Witness(0, None));
        let out0 = fb.create_gate(Add(0, input_wires[0].1, instance));
        let out1 = fb.create_gate(Add(0, input_wires[1].1, witness));
        fb.finish(wirelist![0;out0, out1]).unwrap()
    };
    b.push_function(custum_add).unwrap();

    let id_0 = b.create_gate(Constant(0, vec![40])).unwrap();
    let id_1 = b.create_gate(Constant(0, vec![30])).unwrap();
    let condition = b.create_gate(Constant(0, vec![1])).unwrap();

    let function_with_switch = {
        let mut fb = b.new_function_builder(
            "function_with_switch".to_string(),
            HashMap::from([(0, 2)]),
            HashMap::from([(0, 3)]),
        );
        let input_wires = fb.input_wires();

        let switch = {
            let mut sb = b.new_switch_builder(HashMap::from([(0, 2)]));
            let branch0 = sb
                .create_branch_from(
                    "custom_sub".to_string(),
                    wirelist![0;input_wires[0].1, input_wires[1].1],
                )
                .unwrap();
            sb.push_branch(branch0, vec![0]).unwrap();

            let branch1 = sb
                .create_branch_from(
                    "custom_add".to_string(),
                    wirelist![0;input_wires[0].1, input_wires[1].1],
                )
                .unwrap();
            sb.push_branch(branch1, vec![1]).unwrap();

            sb.finish(0, input_wires[2].1).unwrap()
        };
        let out = fb.create_complex_gate(switch).unwrap();
        fb.finish(out).unwrap()
    };

    b.push_function(function_with_switch).unwrap();

    let out = b
        .create_complex_gate(
            Call(
                "function_with_switch".to_string(),
                wirelist![0;id_0, id_1, condition],
            ),
            vec![vec![vec![10]]],
            vec![vec![vec![5]]],
        )
        .unwrap();
    let out = expand_wirelist(&out).unwrap();

    let assert_equal_witness = {
        let mut fb = b.new_function_builder(
            "assert_equal_witness".to_string(),
            HashMap::new(),
            HashMap::from([(0, 1)]),
        );
        let input_wires = fb.input_wires();
        let witness = fb.create_gate(Witness(0, None));
        let neg_witness = fb.create_gate(MulConstant(0, witness, vec![100]));
        let add_result = fb.create_gate(Add(0, input_wires[0].1, neg_witness));
        fb.create_gate(AssertZero(0, add_result));
        fb.finish(vec![]).unwrap()
    };
    b.push_function(assert_equal_witness).unwrap();

    b.create_complex_gate(
        Call(
            "assert_equal_witness".to_string(),
            wirelist![out[0].0; out[0].1],
        ),
        vec![],
        vec![vec![vec![50]]],
    )
    .unwrap();
    b.create_complex_gate(
        Call(
            "assert_equal_witness".to_string(),
            wirelist![out[1].0;out[1].1],
        ),
        vec![],
        vec![vec![vec![35]]],
    )
    .unwrap();

    let sink = b.finish();

    let mut zkbackend = PlaintextBackend::default();
    let source: Source = sink.into();
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut zkbackend);
    assert_eq!(evaluator.get_violations(), Vec::<String>::new());
}
