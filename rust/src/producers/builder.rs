use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::mem::take;

use super::build_gates::NO_OUTPUT;
pub use super::build_gates::{BuildComplexGate, BuildGate};
use crate::producers::sink::MemorySink;
use crate::structs::count::{count_list_to_hashmap, wirelist_to_count_list, Count};
use crate::structs::function::{Function, FunctionBody};
use crate::structs::gates::replace_output_wires;
use crate::structs::inputs::Inputs;
use crate::structs::plugin::PluginBody;
use crate::structs::value::Value;
use crate::structs::wire::{wirelist_to_hashmap, WireList, WireListElement};
use crate::structs::IR_VERSION;
use crate::Result;
use crate::{Gate, PrivateInputs, PublicInputs, Relation, Sink, TypeId, WireId};

pub trait GateBuilderT {
    /// Allocates a new wire id for the output and creates a new gate,
    /// Returns the newly allocated WireId.
    fn create_gate(&mut self, gate: BuildGate) -> Result<WireId>;

    /// Pushes public and private inputs,
    /// Allocates some new wire ids for the output,
    /// Creates a new gate,
    /// Returns the newly allocated WireIds.
    fn create_complex_gate(
        &mut self,
        gate: BuildComplexGate,
        public_inputs: Vec<Vec<Value>>,
        private_inputs: Vec<Vec<Value>>,
    ) -> Result<WireList>;
}

/// MessageBuilder builds messages by buffering sequences of gates and public/private values.
/// Flush completed messages to a Sink.
/// finish() must be called.
struct MessageBuilder<S: Sink> {
    sink: S,

    public_inputs: PublicInputs,
    private_inputs: PrivateInputs,
    relation: Relation,

    /// Current size (sum of the number of gates) of the relation's functions vector
    functions_size: usize,

    /// Maximum number of gates or public or private values to hold at once.
    /// Default 100,000 or ~12MB of memory.
    /// Size estimation: 40 per public_input + 40 per private_input + 48 per gate = 128 bytes.
    pub max_len: usize,
}

impl<S: Sink> MessageBuilder<S> {
    fn new(sink: S, types: &[Value]) -> Self {
        let public_inputs = vec![Inputs { values: vec![] }; types.len()];
        let private_inputs = vec![Inputs { values: vec![] }; types.len()];
        Self {
            sink,
            public_inputs: PublicInputs {
                version: IR_VERSION.to_string(),
                types: types.to_owned(),
                inputs: public_inputs,
            },
            private_inputs: PrivateInputs {
                version: IR_VERSION.to_string(),
                types: types.to_owned(),
                inputs: private_inputs,
            },
            relation: Relation {
                version: IR_VERSION.to_string(),
                types: types.to_owned(),
                plugins: vec![],
                functions: vec![],
                gates: vec![],
            },
            functions_size: 0,
            max_len: 100 * 1000,
        }
    }

    fn push_public_input_value(&mut self, type_id: TypeId, value: Value) -> Result<()> {
        if let Some(inputs) = self.public_inputs.inputs.get_mut(usize::try_from(type_id)?) {
            inputs.values.push(value);
        } else {
            return Err(format!(
                "Type id {} is not defined, cannot push public input value.",
                type_id
            )
            .into());
        }
        if self.public_inputs.get_public_inputs_len() == self.max_len {
            self.flush_public_inputs();
        }
        Ok(())
    }

    fn push_private_input_value(&mut self, type_id: TypeId, value: Value) -> Result<()> {
        if let Some(inputs) = self
            .private_inputs
            .inputs
            .get_mut(usize::try_from(type_id)?)
        {
            inputs.values.push(value);
        } else {
            return Err(format!(
                "Type id {} is not defined, cannot push private input value.",
                type_id
            )
            .into());
        }
        if self.private_inputs.get_private_inputs_len() == self.max_len {
            self.flush_private_inputs();
        }
        Ok(())
    }

    fn push_gate(&mut self, gate: Gate) {
        self.relation.gates.push(gate);
        if self.relation.gates.len() + self.relation.plugins.len() + self.functions_size
            >= self.max_len
        {
            self.flush_relation();
        }
    }

    fn push_plugin(&mut self, plugin_name: String) {
        self.relation.plugins.push(plugin_name);
        if self.relation.gates.len() + self.relation.plugins.len() + self.functions_size
            >= self.max_len
        {
            self.flush_relation();
        }
    }

    fn push_function(&mut self, function: Function) {
        let func_size = match &function.body {
            FunctionBody::Gates(gates) => gates.len(),
            FunctionBody::PluginBody(_) => 1,
        };
        self.functions_size += func_size;
        self.relation.functions.push(function);
        if self.relation.gates.len() + self.relation.plugins.len() + self.functions_size
            >= self.max_len
        {
            self.flush_relation();
        }
    }

    fn flush_public_inputs(&mut self) {
        self.sink
            .push_public_inputs_message(&self.public_inputs)
            .unwrap();
        for inputs in &mut self.public_inputs.inputs {
            inputs.values.clear();
        }
    }

    fn flush_private_inputs(&mut self) {
        self.sink
            .push_private_inputs_message(&self.private_inputs)
            .unwrap();
        for inputs in &mut self.private_inputs.inputs {
            inputs.values.clear();
        }
    }

    fn flush_relation(&mut self) {
        self.sink.push_relation_message(&self.relation).unwrap();
        self.relation.gates.clear();
        self.relation.functions.clear();
        self.functions_size = 0;
    }

    fn finish(mut self) -> S {
        if !self.public_inputs.inputs.is_empty() {
            self.flush_public_inputs();
        }
        if !self.private_inputs.inputs.is_empty() {
            self.flush_private_inputs();
        }
        if !self.relation.gates.is_empty() || !self.relation.functions.is_empty() {
            self.flush_relation();
        }
        self.sink
    }
}

/// GateBuilder allocates wire IDs, builds gates, and tracks public and private inputs.
///
/// # Example
/// ```
/// use zki_sieve::producers::builder::{GateBuilderT, GateBuilder, BuildGate::*};
/// use zki_sieve::producers::sink::MemorySink;
///
/// let mut b = GateBuilder::new(MemorySink::default(), &vec![vec![2]]);
///
/// let type_id = 0;
/// let my_id = b.create_gate(Constant(type_id, vec![0])).unwrap();
/// b.create_gate(AssertZero(type_id, my_id)).unwrap();
/// ```
pub struct GateBuilder<S: Sink> {
    msg_build: MessageBuilder<S>,

    // name => FunctionParams
    known_functions: HashMap<String, FunctionParams>,
    known_plugins: HashSet<String>,
    next_available_id: HashMap<TypeId, WireId>,
}

/// FunctionParams contains the number of inputs, outputs, public/private inputs of a function.
//#[derive(Clone, Copy)]
struct FunctionParams {
    input_count: Vec<Count>,
    output_count: Vec<Count>,
    public_count: HashMap<TypeId, u64>,
    private_count: HashMap<TypeId, u64>,
}

impl FunctionParams {
    fn check(
        &self,
        name: &str,
        input_count: Option<Vec<Count>>,
        output_count: Option<Vec<Count>>,
        public_count: Option<HashMap<TypeId, u64>>,
        private_count: Option<HashMap<TypeId, u64>>,
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
        if let Some(count) = public_count {
            if count != self.public_count {
                return Err(format!(
                    "Function {} has {:?} public inputs and is called with {:?} public inputs.",
                    name, self.public_count, count
                )
                .into());
            }
        }
        if let Some(count) = private_count {
            if count != self.private_count {
                return Err(format!(
                    "Function {} has {:?} private inputs and is called with {:?} private inputs.",
                    name, self.private_count, count
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

pub fn create_plugin_function(
    function_name: String,
    output_count: Vec<Count>,
    input_count: Vec<Count>,
    plugin_body: PluginBody,
) -> Result<Function> {
    if function_name.is_empty() {
        return Err("Cannot create a function with an empty name".into());
    }
    if plugin_body.name.is_empty() {
        return Err("Cannot create a plugin function with an empty plugin name".into());
    }
    if plugin_body.operation.is_empty() {
        return Err("Cannot create a plugin function with an empty plugin operation".into());
    }
    Ok(Function::new(
        function_name,
        output_count,
        input_count,
        FunctionBody::PluginBody(plugin_body),
    ))
}

/// alloc allocates a new wire ID.
fn alloc(type_id: TypeId, next_available_id: &mut HashMap<TypeId, WireId>) -> WireId {
    let id = next_available_id.entry(type_id).or_insert(0);
    let out_id = *id;
    *id = out_id + 1;
    out_id
}

/// alloc allocates n wire IDs.
fn multiple_alloc(
    type_id: TypeId,
    next_available_id: &mut HashMap<TypeId, WireId>,
    n: u64,
) -> WireList {
    match n {
        0 => vec![],
        1 => vec![WireListElement::Wire(
            type_id,
            alloc(type_id, next_available_id),
        )],
        _ => {
            let id = next_available_id.entry(type_id).or_insert(0);
            let first_id = *id;
            let next = first_id + n;
            *id = next;
            vec![WireListElement::WireRange(type_id, first_id, next - 1)]
        }
    }
}

impl<S: Sink> GateBuilderT for GateBuilder<S> {
    fn create_gate(&mut self, mut gate: BuildGate) -> Result<WireId> {
        let type_id = gate.get_type_id();
        if usize::try_from(type_id)? >= self.msg_build.relation.types.len() {
            return Err(format!(
                "Type id {} is not defined, we cannot create the gate",
                type_id
            )
            .into());
        }
        let out_id = if gate.has_output() {
            alloc(type_id, &mut self.next_available_id)
        } else {
            NO_OUTPUT
        };

        match gate {
            BuildGate::PublicInput(_, Some(ref mut value)) => {
                self.push_public_input_value(type_id, take(value))?;
            }
            BuildGate::PrivateInput(_, Some(ref mut value)) => {
                self.push_private_input_value(type_id, take(value))?;
            }
            _ => {}
        }

        self.msg_build.push_gate(gate.with_output(out_id));

        Ok(out_id)
    }

    fn create_complex_gate(
        &mut self,
        gate: BuildComplexGate,
        public_inputs: Vec<Vec<Value>>,
        private_inputs: Vec<Vec<Value>>,
    ) -> Result<WireList> {
        // Check inputs, public_inputs, private_inputs size and return output_count
        let output_count = match gate {
            BuildComplexGate::Call(ref name, ref input_wires) => {
                let function_params = known_function_params(&self.known_functions, name)?;
                let input_count = wirelist_to_count_list(input_wires);
                let mut public_count_map = HashMap::new();
                for (i, inputs) in public_inputs.iter().enumerate() {
                    if !inputs.is_empty() {
                        public_count_map.insert(u8::try_from(i)?, u64::try_from(inputs.len())?);
                    }
                }
                let mut private_count_map = HashMap::new();
                for (i, inputs) in private_inputs.iter().enumerate() {
                    if !inputs.is_empty() {
                        private_count_map.insert(u8::try_from(i)?, u64::try_from(inputs.len())?);
                    }
                }
                function_params.check(
                    name,
                    Some(input_count),
                    None,
                    Some(public_count_map),
                    Some(private_count_map),
                )?;
                function_params.output_count.clone()
            }

            BuildComplexGate::Convert(type_id, output_wire_count, _) => {
                // TODO convert_gate: check that the convert gate with this signature has already been declared
                // Check that we have no public/private inputs
                if !public_inputs.is_empty() {
                    return Err("A Convert gate does not contain a public input".into());
                }
                if !private_inputs.is_empty() {
                    return Err("A Convert gate does not contain a private_inputs".into());
                }
                vec![Count::new(type_id, output_wire_count)]
            }
        };

        // Push public inputs
        for (i, values) in public_inputs.iter().enumerate() {
            for value in values {
                self.msg_build
                    .push_public_input_value(u8::try_from(i)?, value.clone())?;
            }
        }
        // Push private inputs
        for (i, values) in private_inputs.iter().enumerate() {
            for value in values {
                self.msg_build
                    .push_private_input_value(u8::try_from(i)?, value.clone())?;
            }
        }

        let mut output_wires: WireList = vec![];
        for count in output_count {
            let wires = multiple_alloc(count.type_id, &mut self.next_available_id, count.count);
            output_wires.extend(wires);
        }

        self.msg_build
            .push_gate(gate.with_output(output_wires.clone()));
        Ok(output_wires)
    }
}

impl<S: Sink> GateBuilder<S> {
    /// new creates a new builder.
    pub fn new(sink: S, types: &[Value]) -> Self {
        GateBuilder {
            msg_build: MessageBuilder::new(sink, types),
            known_plugins: HashSet::new(),
            known_functions: HashMap::new(),
            next_available_id: HashMap::new(),
        }
    }

    pub fn new_function_builder(
        &self,
        name: String,
        output_count: Vec<Count>,
        input_count: Vec<Count>,
    ) -> FunctionBuilder {
        let mut next_available_id = HashMap::new();
        output_count.iter().for_each(|count| {
            next_available_id.insert(count.type_id, count.count);
        });
        input_count.iter().for_each(|count| {
            let type_id_count = next_available_id.entry(count.type_id).or_insert(0);
            *type_id_count += count.count;
        });
        FunctionBuilder {
            name,
            output_count,
            input_count,
            gates: vec![],
            public_count: HashMap::new(),
            private_count: HashMap::new(),
            known_functions: &self.known_functions,
            next_available_id,
        }
    }

    pub(crate) fn push_private_input_value(&mut self, type_id: TypeId, val: Value) -> Result<()> {
        self.msg_build.push_private_input_value(type_id, val)
    }

    pub(crate) fn push_public_input_value(&mut self, type_id: TypeId, val: Value) -> Result<()> {
        self.msg_build.push_public_input_value(type_id, val)
    }

    pub fn push_function(&mut self, function_with_counts: FunctionWithInputsCounts) -> Result<()> {
        // Check that there are no other functions with the same name
        if self
            .known_functions
            .contains_key(&function_with_counts.function.name)
        {
            return Err(format!(
                "Function {} already exists !",
                function_with_counts.function.name
            )
            .into());
        }

        // Add the function into known_functions
        self.known_functions.insert(
            function_with_counts.function.name.clone(),
            FunctionParams {
                input_count: function_with_counts.function.input_count.clone(),
                output_count: function_with_counts.function.output_count.clone(),
                public_count: function_with_counts.public_count.clone(),
                private_count: function_with_counts.private_count.clone(),
            },
        );

        // If the function is a plugin function, add the plugin names into the list of used plugins
        if let FunctionBody::PluginBody(plugin_body) = &function_with_counts.function.body {
            if self.known_plugins.insert(plugin_body.name.clone()) {
                self.msg_build.push_plugin(plugin_body.name.clone());
            }
        }
        // Add the function into the list of functions in the Relation
        self.msg_build.push_function(function_with_counts.function);
        Ok(())
    }

    pub fn push_plugin(&mut self, function: Function) -> Result<()> {
        if let FunctionBody::PluginBody(ref plugin_body) = function.body {
            let public_count = plugin_body.public_count.clone();
            let private_count = plugin_body.private_count.clone();
            self.push_function(FunctionWithInputsCounts {
                function,
                public_count,
                private_count,
            })
        } else {
            Err("push_plugin must be called with a plugin function".into())
        }
    }

    pub fn finish(self) -> S {
        self.msg_build.finish()
    }
}

pub fn new_example_builder() -> GateBuilder<MemorySink> {
    GateBuilder::new(MemorySink::default(), &[vec![2]])
}

pub struct FunctionWithInputsCounts {
    function: Function,
    public_count: HashMap<TypeId, u64>,
    private_count: HashMap<TypeId, u64>,
}

/// FunctionBuilder builds a Function by allocating wire IDs and building gates.
/// finish() must be called to obtain the function.
/// The number of public and private inputs consumed by the function are evaluated on the fly.
///
/// # Example
/// ```
/// use std::collections::HashMap;
/// use zki_sieve::producers::builder::{FunctionBuilder, GateBuilder,  BuildGate::*};
/// use zki_sieve::producers::sink::MemorySink;
/// use zki_sieve::structs::count::Count;
/// use zki_sieve::structs::wire::WireListElement;
/// use zki_sieve::wirelist;
///
/// let mut b = GateBuilder::new(MemorySink::default(), &vec![vec![7]]);
///
///  let private_square = {
///     let mut fb = b.new_function_builder("private_square".to_string(), vec![Count::new(0, 1)], vec![]);
///     let private_input_wire = fb.create_gate(PrivateInput(0, None));
///     let output_wire = fb.create_gate(Mul(0, private_input_wire, private_input_wire));
///
///     fb.finish(wirelist![0; output_wire]).unwrap()
///  };
/// ```
pub struct FunctionBuilder<'a> {
    name: String,
    output_count: Vec<Count>,
    input_count: Vec<Count>,
    gates: Vec<Gate>,

    public_count: HashMap<TypeId, u64>,  // evaluated on the fly
    private_count: HashMap<TypeId, u64>, // evaluated on the fly
    known_functions: &'a HashMap<String, FunctionParams>,
    next_available_id: HashMap<TypeId, WireId>,
}

impl FunctionBuilder<'_> {
    /// Returns a Vec<(TypeId, WireId)> containing the inputs wires (without WireRange).
    pub fn input_wires(&self) -> Vec<(TypeId, WireId)> {
        let mut map = HashMap::new();
        for count in self.output_count.iter() {
            map.insert(count.type_id, count.count);
        }
        let mut result: Vec<(TypeId, WireId)> = vec![];
        for count in self.input_count.iter() {
            let type_id_count = map.entry(count.type_id).or_insert(0);
            for id in *type_id_count..(*type_id_count + count.count) {
                result.push((count.type_id, id));
            }
        }
        result
    }

    /// Updates public_count and private_count,
    /// Allocates a new wire id for the output and creates a new gate,
    /// Returns the newly allocated WireId.
    pub fn create_gate(&mut self, gate: BuildGate) -> WireId {
        let type_id = gate.get_type_id();
        let out_id = if gate.has_output() {
            alloc(type_id, &mut self.next_available_id)
        } else {
            NO_OUTPUT
        };

        match gate {
            BuildGate::PublicInput(type_id, _) => {
                let count = self.public_count.entry(type_id).or_insert(0);
                *count += 1;
            }
            BuildGate::PrivateInput(type_id, _) => {
                let count = self.private_count.entry(type_id).or_insert(0);
                *count += 1;
            }
            _ => {}
        }

        self.gates.push(gate.with_output(out_id));

        out_id
    }

    /// Allocates some new wire ids for the output,
    /// Updates public_count and private_count,
    /// Creates a new gate,
    /// Returns the newly allocated WireIds.
    pub fn create_complex_gate(&mut self, gate: BuildComplexGate) -> Result<WireList> {
        // Check inputs size and return function_params
        let (output_count, public_count, private_count) = match gate {
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
                    function_params.public_count.clone(),
                    function_params.private_count.clone(),
                )
            }
            BuildComplexGate::Convert(type_id, output_wire_count, _) => (
                vec![Count::new(type_id, output_wire_count)],
                HashMap::new(),
                HashMap::new(),
            ),
        };

        let mut output_wires: WireList = vec![];
        for count in output_count {
            output_wires.extend(multiple_alloc(
                count.type_id,
                &mut self.next_available_id,
                count.count,
            ));
        }

        for (type_id, count) in private_count.iter() {
            let type_private_count = self.private_count.entry(*type_id).or_insert(0);
            *type_private_count += *count;
        }
        for (type_id, count) in public_count.iter() {
            let type_public_count = self.public_count.entry(*type_id).or_insert(0);
            *type_public_count += *count;
        }

        self.gates.push(gate.with_output(output_wires.clone()));

        Ok(output_wires)
    }

    // Creates and returns the Function as well as the number of public/private inputs consumed by this Function
    pub fn finish(&mut self, output_wires: WireList) -> Result<FunctionWithInputsCounts> {
        let output_wires_map = wirelist_to_hashmap(&output_wires);
        let output_count_map = count_list_to_hashmap(&self.output_count);
        if output_wires_map != output_count_map {
            return Err(format!(
                "Function {} should return {:?} outputs (and not {:?})",
                self.name, output_count_map, output_wires_map
            )
            .into());
        }

        replace_output_wires(&mut self.gates, &output_wires)?;

        Ok(FunctionWithInputsCounts {
            function: Function::new(
                self.name.clone(),
                self.output_count.clone(),
                self.input_count.clone(),
                FunctionBody::Gates(self.gates.to_vec()),
            ),
            public_count: self.public_count.clone(),
            private_count: self.private_count.clone(),
        })
    }
}

#[test]
fn test_builder_with_function() {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};
    use crate::consumers::source::Source;
    use crate::producers::builder::{BuildComplexGate::*, BuildGate::*, GateBuilder, GateBuilderT};
    use crate::producers::sink::MemorySink;
    use crate::structs::wire::expand_wirelist;
    use crate::wirelist;

    let mut b = GateBuilder::new(MemorySink::default(), &[vec![101]]);

    let custom_sub = {
        let mut fb = b.new_function_builder(
            "custom_sub".to_string(),
            vec![Count::new(0, 2)],
            vec![Count::new(0, 4)],
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
    let custom_function = FunctionWithInputsCounts {
        function: Function::new(
            "custom_sub".to_string(),
            vec![],
            vec![],
            FunctionBody::Gates(vec![]),
        ),
        public_count: HashMap::new(),
        private_count: HashMap::new(),
    };
    assert!(b.push_function(custom_function).is_err());

    b.create_gate(New(0, 0, 3)).unwrap();
    let id_0 = b.create_gate(Constant(0, vec![40])).unwrap();
    let _id_1 = b.create_gate(Constant(0, vec![30])).unwrap();
    let _id_2 = b.create_gate(Constant(0, vec![10])).unwrap();
    let id_3 = b.create_gate(Constant(0, vec![5])).unwrap();

    let out = b
        .create_complex_gate(
            Call(
                "custom_sub".to_string(),
                vec![WireListElement::WireRange(0, id_0, id_3)],
            ),
            vec![],
            vec![],
        )
        .unwrap();
    let out = expand_wirelist(&out).unwrap();
    assert_eq!(out.len(), 2);

    let private_0 = b.create_gate(PrivateInput(0, Some(vec![30]))).unwrap();
    let private_1 = b.create_gate(PrivateInput(0, Some(vec![25]))).unwrap();

    let neg_private_0 = b.create_gate(MulConstant(0, private_0, vec![100])).unwrap(); // *(-1)
    let neg_private_1 = b.create_gate(MulConstant(0, private_1, vec![100])).unwrap(); // *(-1)

    let res_0 = b.create_gate(Add(0, out[0].1, neg_private_0)).unwrap();
    let res_1 = b.create_gate(Add(0, out[1].1, neg_private_1)).unwrap();

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
    use crate::producers::sink::MemorySink;
    use crate::structs::wire::expand_wirelist;
    use crate::wirelist;

    let type_id: TypeId = 0;

    let mut b = GateBuilder::new(MemorySink::default(), &[vec![101]]);

    let private_square = {
        let mut fb =
            b.new_function_builder("private_square".to_string(), vec![Count::new(0, 1)], vec![]);
        let private_wire = fb.create_gate(PrivateInput(type_id, None));
        let output_wire = fb.create_gate(Mul(type_id, private_wire, private_wire));

        fb.finish(wirelist![0; output_wire]).unwrap()
    };

    b.push_function(private_square).unwrap();

    let sub_public_private_square = {
        let mut fb = b.new_function_builder(
            "sub_public_private_square".to_string(),
            vec![Count::new(0, 1)],
            vec![],
        );
        let public_wire = fb.create_gate(PublicInput(type_id, None));

        // Try to call a function with a wrong number of inputs
        // Should return an error
        let test = fb.create_complex_gate(Call(
            "private_square".to_string(),
            wirelist![0; public_wire],
        ));
        assert!(test.is_err());

        // Try to Call a not defined function
        // Should return an error
        let test = fb.create_complex_gate(Call("test".to_string(), wirelist![0;public_wire]));
        assert!(test.is_err());

        let private_square_wires = fb
            .create_complex_gate(Call("private_square".to_string(), vec![]))
            .unwrap();
        let private_square_wires = expand_wirelist(&private_square_wires).unwrap();
        let neg_private_square_wire =
            fb.create_gate(MulConstant(type_id, private_square_wires[0].1, vec![100]));
        let output_wire = fb.create_gate(Add(type_id, public_wire, neg_private_square_wire));

        fb.finish(wirelist![0;output_wire]).unwrap()
    };

    b.push_function(sub_public_private_square).unwrap();

    // Try to call a function with a wrong number of public inputs
    // Should return an error
    let test = b.create_complex_gate(
        Call("sub_public_private_square".to_string(), vec![]),
        vec![],
        vec![vec![vec![5]]],
    );
    assert!(test.is_err());

    // Try to call a function with a wrong number of private inputs
    // Should return an error
    let test = b.create_complex_gate(
        Call("sub_public_private_square".to_string(), vec![]),
        vec![vec![vec![25]]],
        vec![],
    );
    assert!(test.is_err());

    let out = b
        .create_complex_gate(
            Call("sub_public_private_square".to_string(), vec![]),
            vec![vec![vec![25]]],
            vec![vec![vec![5]]],
        )
        .unwrap();
    let out = expand_wirelist(&out).unwrap();
    assert_eq!(out.len(), 1);

    b.create_gate(AssertZero(type_id, out[0].1)).unwrap();

    let sink = b.finish();

    let mut zkbackend = PlaintextBackend::default();
    let source: Source = sink.into();
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut zkbackend);
    assert_eq!(evaluator.get_violations(), Vec::<String>::new());
}

#[test]
fn test_builder_with_plugin() {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};
    use crate::consumers::source::Source;
    use crate::producers::builder::{BuildComplexGate::*, BuildGate::*, GateBuilder, GateBuilderT};
    use crate::producers::sink::MemorySink;
    use crate::structs::wire::expand_wirelist;

    let type_id: TypeId = 0;

    let mut b = GateBuilder::new(MemorySink::default(), &[vec![101]]);

    let vector_len: u64 = 2;
    let vector_add_plugin = create_plugin_function(
        "vector_add_2".to_string(),
        vec![Count::new(type_id, vector_len)],
        vec![
            Count::new(type_id, vector_len),
            Count::new(type_id, vector_len),
        ],
        PluginBody {
            name: "vector".to_string(),
            operation: "add".to_string(),
            params: vec![type_id.to_string(), vector_len.to_string()],
            public_count: HashMap::new(),
            private_count: HashMap::new(),
        },
    )
    .unwrap();

    b.push_plugin(vector_add_plugin).unwrap();

    let private_0 = b.create_gate(PrivateInput(type_id, Some(vec![1]))).unwrap();
    let private_1 = b.create_gate(PrivateInput(type_id, Some(vec![2]))).unwrap();
    let public_0 = b.create_gate(PrivateInput(type_id, Some(vec![3]))).unwrap();
    let public_1 = b.create_gate(PrivateInput(type_id, Some(vec![4]))).unwrap();

    let out = b
        .create_complex_gate(
            Call(
                "vector_add_2".to_string(),
                vec![
                    WireListElement::WireRange(type_id, private_0, private_1),
                    WireListElement::WireRange(type_id, public_0, public_1),
                ],
            ),
            vec![],
            vec![],
        )
        .unwrap();
    let out = expand_wirelist(&out).unwrap();
    assert_eq!(out.len() as u64, vector_len);

    let out_0 = b
        .create_gate(AddConstant(type_id, out[0].1, vec![97]))
        .unwrap();
    let out_1 = b
        .create_gate(AddConstant(type_id, out[1].1, vec![95]))
        .unwrap();

    b.create_gate(AssertZero(type_id, out_0)).unwrap();
    b.create_gate(AssertZero(type_id, out_1)).unwrap();

    let sink = b.finish();

    let mut zkbackend = PlaintextBackend::default();
    let source: Source = sink.into();
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut zkbackend);
    assert_eq!(evaluator.get_violations(), Vec::<String>::new());
}
