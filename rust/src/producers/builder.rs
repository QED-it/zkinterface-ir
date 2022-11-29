use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::mem::take;

use super::build_gates::NO_OUTPUT;
pub use super::build_gates::{BuildComplexGate, BuildGate};
use crate::producers::sink::MemorySink;
use crate::structs::conversion::Conversion;
use crate::structs::count::Count;
use crate::structs::directives::Directive;
use crate::structs::function::{Function, FunctionBody, FunctionCounts};
use crate::structs::gates::replace_output_wires;
use crate::structs::plugin::PluginBody;
use crate::structs::types::Type;
use crate::structs::value::Value;
use crate::structs::wirerange::{
    add_types_to_wire_ranges, check_wire_ranges_with_counts, WireRange,
};
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
    ) -> Result<Vec<WireRange>>;
}

/// MessageBuilder builds messages by buffering sequences of gates and public/private values.
/// Flush completed messages to a Sink.
/// finish() must be called.
struct MessageBuilder<S: Sink> {
    sink: S,

    public_inputs: HashMap<TypeId, PublicInputs>,
    private_inputs: HashMap<TypeId, PrivateInputs>,
    relation: Relation,

    /// Current size (sum of the number of gates) of the relation's functions vector
    functions_size: usize,

    /// Maximum number of gates or public or private values to hold at once.
    /// Default 100,000 or ~12MB of memory.
    /// Size estimation: 40 per public_input + 40 per private_input + 48 per gate = 128 bytes.
    pub max_len: usize,
}

impl<S: Sink> MessageBuilder<S> {
    fn new(sink: S, plugins: &[String], types: &[Type], conversions: &[Conversion]) -> Self {
        Self {
            sink,
            public_inputs: HashMap::new(),
            private_inputs: HashMap::new(),
            relation: Relation {
                version: IR_VERSION.to_string(),
                plugins: plugins.to_owned(),
                types: types.to_owned(),
                conversions: conversions.to_owned(),
                directives: vec![],
            },
            functions_size: 0,
            max_len: 100 * 1000,
        }
    }

    fn push_public_input_value(&mut self, type_id: TypeId, value: Value) -> Result<()> {
        let type_value = self
            .relation
            .types
            .get(usize::try_from(type_id)?)
            .ok_or(format!(
                "When pushing a public input value, the type id ({}) is unknown.",
                type_id
            ))?;

        // Push value into the PublicInputs corresponding to type_id
        let public_input = self.public_inputs.entry(type_id).or_insert(PublicInputs {
            version: self.relation.version.clone(),
            type_value: type_value.clone(),
            inputs: vec![],
        });
        public_input.inputs.push(value);

        if public_input.inputs.len() == self.max_len {
            self.flush_public_inputs(type_id);
        }
        Ok(())
    }

    fn push_private_input_value(&mut self, type_id: TypeId, value: Value) -> Result<()> {
        let type_value = self
            .relation
            .types
            .get(usize::try_from(type_id)?)
            .ok_or(format!(
                "When pushing a public input value, the type id ({}) is unknown.",
                type_id
            ))?;

        // Push value into the PrivateInputs corresponding to type_id
        let private_input = self.private_inputs.entry(type_id).or_insert(PrivateInputs {
            version: self.relation.version.clone(),
            type_value: type_value.clone(),
            inputs: vec![],
        });
        private_input.inputs.push(value);

        if private_input.inputs.len() == self.max_len {
            self.flush_private_inputs(type_id);
        }
        Ok(())
    }

    fn push_gate(&mut self, gate: Gate) {
        self.relation.directives.push(Directive::Gate(gate));
        if self.relation.directives.len()
            + self.relation.plugins.len()
            + self.relation.conversions.len()
            + self.functions_size
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
        self.relation.directives.push(Directive::Function(function));
        if self.relation.directives.len()
            + self.relation.plugins.len()
            + self.relation.conversions.len()
            + self.functions_size
            >= self.max_len
        {
            self.flush_relation();
        }
    }

    fn flush_public_inputs(&mut self, type_id: TypeId) {
        let public_input = self.public_inputs.get(&type_id).unwrap();
        self.sink.push_public_inputs_message(public_input).unwrap();
        self.public_inputs.remove(&type_id);
    }

    fn flush_all_public_inputs(&mut self) {
        let type_ids = self
            .public_inputs
            .iter()
            .map(|(type_id, _)| *type_id)
            .collect::<Vec<_>>();
        for type_id in type_ids.iter() {
            self.flush_public_inputs(*type_id);
        }
    }

    fn flush_private_inputs(&mut self, type_id: TypeId) {
        let private_input = self.private_inputs.get(&type_id).unwrap();
        self.sink
            .push_private_inputs_message(private_input)
            .unwrap();
        self.private_inputs.remove(&type_id);
    }

    fn flush_all_private_inputs(&mut self) {
        let type_ids = self
            .private_inputs
            .iter()
            .map(|(type_id, _)| *type_id)
            .collect::<Vec<_>>();
        for type_id in type_ids.iter() {
            self.flush_private_inputs(*type_id);
        }
    }

    fn flush_relation(&mut self) {
        self.sink.push_relation_message(&self.relation).unwrap();
        self.relation.plugins.clear();
        self.relation.types.clear();
        self.relation.conversions.clear();
        self.relation.directives.clear();
        self.functions_size = 0;
    }

    fn finish(mut self) -> S {
        self.flush_all_public_inputs();
        self.flush_all_private_inputs();
        if !self.relation.directives.is_empty() {
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
/// use zki_sieve::structs::types::Type;
///
/// let mut b = GateBuilder::new(MemorySink::default(), &[], &[Type::new_field_type(vec![2])], &[]);
///
/// let type_id = 0;
/// let my_id = b.create_gate(Constant(type_id, vec![0])).unwrap();
/// b.create_gate(AssertZero(type_id, my_id)).unwrap();
/// ```
pub struct GateBuilder<S: Sink> {
    msg_build: MessageBuilder<S>,

    // name => FunctionCounts
    known_functions: HashMap<String, FunctionCounts>,
    known_plugins: HashSet<String>,
    known_conversions: HashSet<Conversion>,
    next_available_id: HashMap<TypeId, WireId>,
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
) -> WireRange {
    let id = next_available_id.entry(type_id).or_insert(0);
    let first_id = *id;
    let next = first_id + n;
    *id = next;
    WireRange::new(first_id, next - 1)
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
            BuildGate::Public(_, Some(ref mut value)) => {
                self.push_public_input_value(type_id, take(value))?;
            }
            BuildGate::Private(_, Some(ref mut value)) => {
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
    ) -> Result<Vec<WireRange>> {
        // Check inputs, public_inputs, private_inputs size and return output_count
        let output_count = match gate {
            BuildComplexGate::Call(ref name, ref in_ids) => {
                let function_counts =
                    FunctionCounts::get_function_counts(&self.known_functions, name)?;
                // Check inputs
                if !check_wire_ranges_with_counts(in_ids, &function_counts.input_count) {
                    return Err(format!(
                        "Call to function {}: number of input wires mismatch.",
                        name
                    )
                    .into());
                }
                // Check public inputs
                let mut public_count_map = HashMap::new();
                for (i, inputs) in public_inputs.iter().enumerate() {
                    if !inputs.is_empty() {
                        public_count_map.insert(u8::try_from(i)?, u64::try_from(inputs.len())?);
                    }
                }
                if public_count_map != function_counts.public_count {
                    return Err(format!(
                        "Call to function {}: number of public inputs mismatch.",
                        name
                    )
                    .into());
                }
                // Check private inputs
                let mut private_count_map = HashMap::new();
                for (i, inputs) in private_inputs.iter().enumerate() {
                    if !inputs.is_empty() {
                        private_count_map.insert(u8::try_from(i)?, u64::try_from(inputs.len())?);
                    }
                }
                if private_count_map != function_counts.private_count {
                    return Err(format!(
                        "Call to function {}: number of private inputs mismatch.",
                        name
                    )
                    .into());
                }
                function_counts.output_count
            }

            BuildComplexGate::Convert(
                out_type_id,
                output_wire_count,
                in_type_id,
                in_first_id,
                in_last_id,
            ) => {
                // Check that the convert gate has been declared
                let conversion = Conversion::new(
                    Count::new(out_type_id, output_wire_count),
                    Count::new(in_type_id, in_last_id - in_first_id + 1),
                );
                if !self.known_conversions.contains(&conversion) {
                    return Err("Impossible to call an undeclared conversion".into());
                }

                // Check that we have no public/private inputs
                if !public_inputs.is_empty() {
                    return Err("A Convert gate does not contain a public input".into());
                }
                if !private_inputs.is_empty() {
                    return Err("A Convert gate does not contain a private_inputs".into());
                }
                vec![Count::new(out_type_id, output_wire_count)]
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

        let out_ids = output_count
            .iter()
            .map(|count| multiple_alloc(count.type_id, &mut self.next_available_id, count.count))
            .collect::<Vec<_>>();

        self.msg_build.push_gate(gate.with_output(out_ids.clone()));
        Ok(out_ids)
    }
}

impl<S: Sink> GateBuilder<S> {
    /// new creates a new builder.
    pub fn new(sink: S, plugins: &[String], types: &[Type], conversions: &[Conversion]) -> Self {
        // Plugins and conversion cannot be filled on the fly
        // because they must be flushed in the first relation message
        let mut known_plugins = HashSet::new();
        plugins.iter().for_each(|plugin_name| {
            known_plugins.insert(plugin_name.clone());
        });
        let mut known_conversions = HashSet::new();
        conversions.iter().for_each(|conversion| {
            known_conversions.insert(conversion.clone());
        });

        GateBuilder {
            msg_build: MessageBuilder::new(sink, plugins, types, conversions),
            known_plugins,
            known_conversions,
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
            known_conversions: &self.known_conversions,
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

    pub fn push_function(&mut self, function_with_infos: FunctionWithInfos) -> Result<()> {
        // Check that there are no other functions with the same name
        if self
            .known_functions
            .contains_key(&function_with_infos.function.name)
        {
            return Err(format!(
                "Function {} already exists !",
                function_with_infos.function.name
            )
            .into());
        }

        // Add the function into known_functions
        self.known_functions.insert(
            function_with_infos.function.name.clone(),
            FunctionCounts {
                input_count: function_with_infos.function.input_count.clone(),
                output_count: function_with_infos.function.output_count.clone(),
                public_count: function_with_infos.public_count.clone(),
                private_count: function_with_infos.private_count.clone(),
            },
        );

        // If the function is a plugin function, check that the plugin name have been declared
        if let FunctionBody::PluginBody(plugin_body) = &function_with_infos.function.body {
            if !self.known_plugins.contains(&plugin_body.name) {
                return Err("The plugin name of a Plugin function should be declared".into());
            }
        }

        // Add the function into the list of functions in the Relation
        self.msg_build.push_function(function_with_infos.function);
        Ok(())
    }

    pub fn push_plugin_function(&mut self, function: Function) -> Result<()> {
        if let FunctionBody::PluginBody(ref plugin_body) = function.body {
            let public_count = plugin_body.public_count.clone();
            let private_count = plugin_body.private_count.clone();
            self.push_function(FunctionWithInfos {
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
    GateBuilder::new(
        MemorySink::default(),
        &[],
        &[Type::new_field_type(vec![2])],
        &[],
    )
}

pub struct FunctionWithInfos {
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
/// use zki_sieve::structs::types::Type;
/// use zki_sieve::structs::wirerange::WireRange;
///
/// let mut b = GateBuilder::new(MemorySink::default(), &[], &[Type::new_field_type(vec![7])], &[]);
///
///  let private_square = {
///     let mut fb = b.new_function_builder("private_square".to_string(), vec![Count::new(0, 1)], vec![]);
///     let private_input_wire = fb.create_gate(Private(0, None));
///     let output_wire = fb.create_gate(Mul(0, private_input_wire, private_input_wire));
///
///     fb.finish(vec![WireRange::new(output_wire, output_wire)]).unwrap()
///  };
/// ```
pub struct FunctionBuilder<'a> {
    name: String,
    output_count: Vec<Count>,
    input_count: Vec<Count>,
    gates: Vec<Gate>,

    public_count: HashMap<TypeId, u64>,  // evaluated on the fly
    private_count: HashMap<TypeId, u64>, // evaluated on the fly
    known_conversions: &'a HashSet<Conversion>,
    known_functions: &'a HashMap<String, FunctionCounts>,
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
            BuildGate::Public(type_id, _) => {
                let count = self.public_count.entry(type_id).or_insert(0);
                *count += 1;
            }
            BuildGate::Private(type_id, _) => {
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
    pub fn create_complex_gate(&mut self, gate: BuildComplexGate) -> Result<Vec<WireRange>> {
        // Check inputs size, consume public/private inputs and return output_count
        let output_count = match gate {
            BuildComplexGate::Call(ref name, ref in_ids) => {
                // Retrieve function counts (and check that the function has already been declared)
                let function_counts =
                    FunctionCounts::get_function_counts(self.known_functions, name)?;

                // Check inputs size
                if !check_wire_ranges_with_counts(in_ids, &function_counts.input_count) {
                    return Err(format!(
                        "Call to function {}: number of input wires mismatch.",
                        name
                    )
                    .into());
                }

                // Consume public/private inputs
                function_counts
                    .private_count
                    .iter()
                    .for_each(|(type_id, count)| {
                        let type_private_count = self.private_count.entry(*type_id).or_insert(0);
                        *type_private_count += *count;
                    });
                function_counts
                    .public_count
                    .iter()
                    .for_each(|(type_id, count)| {
                        let type_public_count = self.public_count.entry(*type_id).or_insert(0);
                        *type_public_count += *count;
                    });
                function_counts.output_count
            }
            BuildComplexGate::Convert(
                out_type_id,
                out_wire_count,
                in_type_id,
                in_first_id,
                in_last_id,
            ) => {
                // Check that the convert gate has been declared
                let conversion = Conversion::new(
                    Count::new(out_type_id, out_wire_count),
                    Count::new(in_type_id, in_last_id - in_first_id + 1),
                );
                if !self.known_conversions.contains(&conversion) {
                    return Err("Impossible to call an undeclared conversion".into());
                }

                vec![Count::new(out_type_id, out_wire_count)]
            }
        };

        let out_ids = output_count
            .iter()
            .map(|count| multiple_alloc(count.type_id, &mut self.next_available_id, count.count))
            .collect::<Vec<_>>();

        self.gates.push(gate.with_output(out_ids.clone()));

        Ok(out_ids)
    }

    // Creates and returns the Function as well as the number of public/private inputs consumed by this Function
    pub fn finish(&mut self, out_ids: Vec<WireRange>) -> Result<FunctionWithInfos> {
        if !check_wire_ranges_with_counts(&out_ids, &self.output_count) {
            return Err(format!(
                "Function {} cannot be created (wrong number of output wires)",
                self.name
            )
            .into());
        }

        replace_output_wires(
            &mut self.gates,
            &add_types_to_wire_ranges(&out_ids, &self.output_count)?,
            self.known_functions,
        )?;

        Ok(FunctionWithInfos {
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

    let mut b = GateBuilder::new(
        MemorySink::default(),
        &[],
        &[Type::new_field_type(vec![101])],
        &[],
    );

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
        let custom_sub = fb
            .finish(vec![WireRange::new(output0_wire, output1_wire)])
            .unwrap();
        custom_sub
    };

    b.push_function(custom_sub).unwrap();

    // Try to push two functions with the same name
    // It should return an error
    let custom_function = FunctionWithInfos {
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
            Call("custom_sub".to_string(), vec![WireRange::new(id_0, id_3)]),
            vec![],
            vec![],
        )
        .unwrap();
    assert_eq!(out.len(), 1);
    let out = (out[0].first_id..=out[0].last_id).collect::<Vec<_>>();
    assert_eq!(out.len(), 2);

    let private_0 = b.create_gate(Private(0, Some(vec![30]))).unwrap();
    let private_1 = b.create_gate(Private(0, Some(vec![25]))).unwrap();

    let neg_private_0 = b.create_gate(MulConstant(0, private_0, vec![100])).unwrap(); // *(-1)
    let neg_private_1 = b.create_gate(MulConstant(0, private_1, vec![100])).unwrap(); // *(-1)

    let res_0 = b.create_gate(Add(0, out[0], neg_private_0)).unwrap();
    let res_1 = b.create_gate(Add(0, out[1], neg_private_1)).unwrap();

    b.create_gate(AssertZero(0, res_0)).unwrap();
    b.create_gate(AssertZero(0, res_1)).unwrap();

    // Try to call an unknown function
    // It should return an error
    assert!(b
        .create_complex_gate(
            Call(
                "unknown_function".to_string(),
                vec![WireRange::new(id_0, id_0)]
            ),
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

    let type_id: TypeId = 0;

    let mut b = GateBuilder::new(
        MemorySink::default(),
        &[],
        &[Type::new_field_type(vec![101])],
        &[],
    );

    let private_square = {
        let mut fb =
            b.new_function_builder("private_square".to_string(), vec![Count::new(0, 1)], vec![]);
        let private_wire = fb.create_gate(Private(type_id, None));
        let output_wire = fb.create_gate(Mul(type_id, private_wire, private_wire));

        fb.finish(vec![WireRange::new(output_wire, output_wire)])
            .unwrap()
    };

    b.push_function(private_square).unwrap();

    let sub_public_private_square = {
        let mut fb = b.new_function_builder(
            "sub_public_private_square".to_string(),
            vec![Count::new(0, 1)],
            vec![],
        );
        let public_wire = fb.create_gate(Public(type_id, None));

        // Try to call a function with a wrong number of inputs
        // Should return an error
        let test = fb.create_complex_gate(Call(
            "private_square".to_string(),
            vec![WireRange::new(public_wire, public_wire)],
        ));
        assert!(test.is_err());

        // Try to Call a not defined function
        // Should return an error
        let test = fb.create_complex_gate(Call(
            "test".to_string(),
            vec![WireRange::new(public_wire, public_wire)],
        ));
        assert!(test.is_err());

        let private_square_wires = fb
            .create_complex_gate(Call("private_square".to_string(), vec![]))
            .unwrap();
        assert_eq!(private_square_wires.len(), 1);
        let private_square_wires = (private_square_wires[0].first_id
            ..=private_square_wires[0].last_id)
            .collect::<Vec<_>>();
        assert_eq!(private_square_wires.len(), 1);
        let neg_private_square_wire =
            fb.create_gate(MulConstant(type_id, private_square_wires[0], vec![100]));
        let output_wire = fb.create_gate(Add(type_id, public_wire, neg_private_square_wire));

        fb.finish(vec![WireRange::new(output_wire, output_wire)])
            .unwrap()
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
    assert_eq!(out.len(), 1);
    let out = (out[0].first_id..=out[0].last_id).collect::<Vec<_>>();
    assert_eq!(out.len(), 1);

    b.create_gate(AssertZero(type_id, out[0])).unwrap();

    let sink = b.finish();

    let mut zkbackend = PlaintextBackend::default();
    let source: Source = sink.into();
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut zkbackend);
    assert_eq!(evaluator.get_violations(), Vec::<String>::new());
}

#[test]
fn test_builder_with_conversion() {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};
    use crate::consumers::source::Source;
    use crate::producers::builder::{BuildComplexGate::*, BuildGate::*, GateBuilder, GateBuilderT};
    use crate::producers::sink::MemorySink;

    let type_id_7: TypeId = 0;
    let type_id_101: TypeId = 1;

    let mut b = GateBuilder::new(
        MemorySink::default(),
        &[],
        &[
            Type::new_field_type(vec![7]),
            Type::new_field_type(vec![101]),
        ],
        &[Conversion::new(
            Count::new(type_id_101, 3),
            Count::new(type_id_7, 2),
        )],
    );

    let id_0 = b.create_gate(Private(type_id_7, Some(vec![1]))).unwrap();
    let id_1 = b.create_gate(Private(type_id_7, Some(vec![3]))).unwrap();
    let out = b
        .create_complex_gate(
            Convert(type_id_101, 3, type_id_7, id_0, id_1),
            vec![],
            vec![],
        )
        .unwrap();
    assert_eq!(out.len(), 1);
    let out = (out[0].first_id..=out[0].last_id).collect::<Vec<_>>();
    assert_eq!(out.len(), 3);
    b.create_gate(AssertZero(type_id_101, out[0])).unwrap();
    b.create_gate(AssertZero(type_id_101, out[1])).unwrap();
    let id_2 = b
        .create_gate(AddConstant(type_id_101, out[2], vec![91]))
        .unwrap();
    b.create_gate(AssertZero(type_id_101, id_2)).unwrap();

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

    let type_id: TypeId = 0;

    let mut b = GateBuilder::new(
        MemorySink::default(),
        &["vector".to_string()],
        &[Type::new_field_type(vec![101])],
        &[],
    );

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

    b.push_plugin_function(vector_add_plugin).unwrap();

    let private_0 = b.create_gate(Private(type_id, Some(vec![1]))).unwrap();
    let private_1 = b.create_gate(Private(type_id, Some(vec![2]))).unwrap();
    let public_0 = b.create_gate(Private(type_id, Some(vec![3]))).unwrap();
    let public_1 = b.create_gate(Private(type_id, Some(vec![4]))).unwrap();

    let out = b
        .create_complex_gate(
            Call(
                "vector_add_2".to_string(),
                vec![
                    WireRange::new(private_0, private_1),
                    WireRange::new(public_0, public_1),
                ],
            ),
            vec![],
            vec![],
        )
        .unwrap();
    assert_eq!(out.len(), 1);
    let out = (out[0].first_id..=out[0].last_id).collect::<Vec<_>>();
    assert_eq!(out.len() as u64, vector_len);

    let out_0 = b
        .create_gate(AddConstant(type_id, out[0], vec![97]))
        .unwrap();
    let out_1 = b
        .create_gate(AddConstant(type_id, out[1], vec![95]))
        .unwrap();

    b.create_gate(AssertZero(type_id, out_0)).unwrap();
    b.create_gate(AssertZero(type_id, out_1)).unwrap();

    let sink = b.finish();

    let mut zkbackend = PlaintextBackend::default();
    let source: Source = sink.into();
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut zkbackend);
    assert_eq!(evaluator.get_violations(), Vec::<String>::new());
}

#[test]
fn test_builder_with_plugin_type() {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};
    use crate::consumers::source::Source;
    use crate::producers::builder::{BuildComplexGate::*, BuildGate::*, GateBuilder, GateBuilderT};
    use crate::producers::sink::MemorySink;

    let type_id: TypeId = 0;

    let mut b = GateBuilder::new(
        MemorySink::default(),
        &["ring".to_string()],
        &[Type::new_plugin_type(
            "ring".to_string(),
            "type".to_string(),
            vec!["2".to_string(), "4".to_string()],
        )],
        &[],
    );

    let ring_add = create_plugin_function(
        "ring_add".to_string(),
        vec![Count::new(type_id, 1)],
        vec![Count::new(type_id, 1), Count::new(type_id, 1)],
        PluginBody {
            name: "ring".to_string(),
            operation: "add".to_string(),
            params: vec![type_id.to_string()],
            public_count: HashMap::new(),
            private_count: HashMap::new(),
        },
    )
    .unwrap();
    b.push_plugin_function(ring_add).unwrap();

    let id_0 = b.create_gate(Private(type_id, Some(vec![10]))).unwrap();
    let id_1 = b.create_gate(Private(type_id, Some(vec![8]))).unwrap();
    let out = b
        .create_complex_gate(
            Call(
                "ring_add".to_string(),
                vec![WireRange::new(id_0, id_0), WireRange::new(id_1, id_1)],
            ),
            vec![],
            vec![],
        )
        .unwrap();
    assert_eq!(out.len(), 1);
    let out = (out[0].first_id..=out[0].last_id).collect::<Vec<_>>();
    assert_eq!(out.len(), 1);
    let out = out[0];

    let ring_equal = create_plugin_function(
        "ring_equal".to_string(),
        vec![],
        vec![Count::new(type_id, 1), Count::new(type_id, 1)],
        PluginBody {
            name: "ring".to_string(),
            operation: "equal".to_string(),
            params: vec![type_id.to_string()],
            public_count: HashMap::new(),
            private_count: HashMap::new(),
        },
    )
    .unwrap();
    b.push_plugin_function(ring_equal).unwrap();

    let pub_0 = b.create_gate(Public(type_id, Some(vec![2]))).unwrap();
    let res = b
        .create_complex_gate(
            Call(
                "ring_equal".to_string(),
                vec![WireRange::new(out, out), WireRange::new(pub_0, pub_0)],
            ),
            vec![],
            vec![],
        )
        .unwrap();
    assert_eq!(res.len(), 0);

    let sink = b.finish();

    let mut zkbackend = PlaintextBackend::default();
    let source: Source = sink.into();
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut zkbackend);
    assert_eq!(evaluator.get_violations(), Vec::<String>::new());
}

#[test]
fn test_builder_with_functions_with_several_input_output_types() {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};
    use crate::consumers::source::Source;
    use crate::producers::builder::{BuildComplexGate::*, BuildGate::*, GateBuilder, GateBuilderT};
    use crate::producers::sink::MemorySink;

    let mut b = GateBuilder::new(
        MemorySink::default(),
        &[],
        &[Type::Field(vec![7]), Type::Field(vec![101])],
        &[
            Conversion::new(Count::new(0, 1), Count::new(1, 1)),
            Conversion::new(Count::new(1, 1), Count::new(0, 1)),
        ],
    );

    b.create_gate(New(0, 0, 1)).unwrap();
    b.create_gate(New(1, 0, 1)).unwrap();
    let pub_0 = b.create_gate(Public(0, Some(vec![3]))).unwrap();
    let pub_1 = b.create_gate(Public(0, Some(vec![5]))).unwrap();
    let priv_0 = b.create_gate(Private(1, Some(vec![10]))).unwrap();
    let priv_1 = b.create_gate(Private(1, Some(vec![20]))).unwrap();

    let custom_function = {
        let mut fb = b.new_function_builder(
            "custom".to_string(),
            vec![Count::new(0, 1), Count::new(1, 1)],
            vec![Count::new(0, 2), Count::new(1, 2)],
        );
        let input_wires = fb.input_wires();
        let add_0 = fb.create_gate(Add(0, input_wires[0].1, input_wires[1].1));
        let out_0 = fb
            .create_complex_gate(Convert(1, 1, 0, add_0, add_0))
            .unwrap();
        assert_eq!(out_0.len(), 1);
        assert_eq!(out_0[0].first_id, out_0[0].last_id);
        let add_1 = fb.create_gate(Add(1, input_wires[2].1, input_wires[3].1));
        let out_1 = fb
            .create_complex_gate(Convert(0, 1, 1, add_1, add_1))
            .unwrap();
        assert_eq!(out_1.len(), 1);
        assert_eq!(out_1[0].first_id, out_1[0].last_id);
        fb.finish(vec![out_1[0].clone(), out_0[0].clone()]).unwrap()
    };

    b.push_function(custom_function).unwrap();

    let out = b
        .create_complex_gate(
            Call(
                "custom".to_string(),
                vec![WireRange::new(pub_0, pub_1), WireRange::new(priv_0, priv_1)],
            ),
            vec![],
            vec![],
        )
        .unwrap();
    assert_eq!(out.len(), 2);
    assert_eq!(out[0].first_id, out[0].last_id);
    assert_eq!(out[1].first_id, out[0].last_id);

    let res_0 = b
        .create_gate(AddConstant(0, out[0].first_id, vec![5]))
        .unwrap();
    b.create_gate(AssertZero(0, res_0)).unwrap();
    let res_1 = b
        .create_gate(AddConstant(1, out[1].first_id, vec![100]))
        .unwrap();
    b.create_gate(AssertZero(1, res_1)).unwrap();

    b.create_gate(Delete(0, 0, res_0)).unwrap();
    b.create_gate(Delete(1, 0, res_1)).unwrap();

    let sink = b.finish();

    let mut zkbackend = PlaintextBackend::default();
    let source: Source = sink.into();
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut zkbackend);
    assert_eq!(evaluator.get_violations(), Vec::<String>::new());
}
