extern crate serde;
extern crate serde_json;

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::structs::directives::Directive;
use crate::structs::function::FunctionBody;
use crate::structs::types::Type;
use crate::{Gate, Message, PrivateInputs, PublicInputs, Relation, Result};

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct GateStats {
    // Inputs.
    pub public_inputs_consumed: u64,
    pub private_inputs_consumed: u64,
    pub public_inputs_provided: usize,
    pub private_inputs_provided: usize,

    // Gates.
    pub constants_gates: usize,
    pub assert_zero_gates: usize,
    pub copy_gates: usize,
    pub add_gates: usize,
    pub mul_gates: usize,
    pub add_constant_gates: usize,
    pub mul_constant_gates: usize,
    pub variables_allocated_with_new: u64,
    pub variables_deleted: u64,
    pub convert_gates: usize,

    pub functions_defined: usize,
    pub functions_called: usize,

    pub plugins_defined: usize,
    pub plugins_called: usize,

    pub conversions_defined: usize,

    // The number of messages into which the statement was split.
    pub public_inputs_messages: usize,
    pub private_inputs_messages: usize,
    pub relation_messages: usize,
}

#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum FunctionContent {
    Plugin(String, String, Vec<String>, u64, u64), // (name, operation, params, public_count, private_count)
    Function(GateStats),                           // (stats)
}

#[derive(Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Stats {
    // Types.
    pub types: Vec<Type>,

    pub gate_stats: GateStats,

    pub functions: BTreeMap<String, FunctionContent>,
}

impl Stats {
    pub fn from_messages(messages: impl Iterator<Item = Result<Message>>) -> Self {
        let mut stats = Stats::default();
        messages.for_each(|msg| stats.ingest_message(&msg.unwrap()));
        stats
    }

    pub fn ingest_message(&mut self, msg: &Message) {
        match msg {
            Message::PublicInputs(i) => self.ingest_public_inputs(i),
            Message::PrivateInputs(w) => self.ingest_private_inputs(w),
            Message::Relation(r) => self.ingest_relation(r),
        }
    }

    pub fn ingest_public_inputs(&mut self, public_inputs: &PublicInputs) {
        self.gate_stats.public_inputs_provided += public_inputs.inputs.len();
        self.gate_stats.public_inputs_messages += 1;
    }

    pub fn ingest_private_inputs(&mut self, private_inputs: &PrivateInputs) {
        self.gate_stats.private_inputs_provided += private_inputs.inputs.len();
        self.gate_stats.private_inputs_messages += 1;
    }

    pub fn ingest_relation(&mut self, relation: &Relation) {
        self.ingest_types(&relation.types);
        self.gate_stats.relation_messages += 1;

        self.gate_stats.conversions_defined += relation.conversions.len();

        for directive in relation.directives.iter() {
            match directive {
                Directive::Function(function) => {
                    let name = function.name.clone();
                    match &function.body {
                        FunctionBody::Gates(gates) => {
                            self.gate_stats.functions_defined += 1;
                            let func_stats = ingest_subcircuit(gates, &self.functions);
                            self.functions
                                .insert(name.clone(), FunctionContent::Function(func_stats));
                        }
                        FunctionBody::PluginBody(plugin_body) => {
                            let public_count = plugin_body
                                .public_count
                                .iter()
                                .map(|(_, count)| *count)
                                .sum();
                            let private_count = plugin_body
                                .private_count
                                .iter()
                                .map(|(_, count)| *count)
                                .sum();
                            self.gate_stats.plugins_defined += 1;
                            self.functions.insert(
                                name.clone(),
                                FunctionContent::Plugin(
                                    plugin_body.name.clone(),
                                    plugin_body.operation.clone(),
                                    plugin_body.params.clone(),
                                    public_count,
                                    private_count,
                                ),
                            );
                        }
                    }
                }
                Directive::Gate(gate) => {
                    self.gate_stats.ingest_gate(gate, &self.functions);
                }
            }
        }
    }

    fn ingest_types(&mut self, types: &[Type]) {
        if self.types.is_empty() {
            // Types have not yet been ingested
            types
                .iter()
                .for_each(|type_value| self.types.push(type_value.clone()));
        }
    }
}

fn ingest_subcircuit(
    subcircuit: &[Gate],
    known_functions: &BTreeMap<String, FunctionContent>,
) -> GateStats {
    let mut local_stats = GateStats::default();
    for gate in subcircuit {
        local_stats.ingest_gate(gate, known_functions);
    }
    local_stats
}

impl GateStats {
    fn ingest_gate(&mut self, gate: &Gate, known_functions: &BTreeMap<String, FunctionContent>) {
        use Gate::*;

        match gate {
            Constant(_type_id, _out, _value) => {
                self.constants_gates += 1;
            }

            AssertZero(_type_id, _inp) => {
                self.assert_zero_gates += 1;
            }

            Copy(_type_id, _out, _inp) => {
                self.copy_gates += 1;
            }

            Add(_type_id, _out, _left, _right) => {
                self.add_gates += 1;
            }

            Mul(_type_id, _out, _left, _right) => {
                self.mul_gates += 1;
            }

            AddConstant(_type_id, _out, _inp, _constant) => {
                self.add_constant_gates += 1;
            }

            MulConstant(_type_id, _out, _inp, _constant) => {
                self.mul_constant_gates += 1;
            }

            Public(_type_id, _out) => {
                self.public_inputs_consumed += 1;
            }

            Private(_type_id, _out) => {
                self.private_inputs_consumed += 1;
            }

            New(_type_id, first, last) => {
                self.variables_allocated_with_new += last - first + 1;
            }

            Delete(_type_id, first, last) => {
                self.variables_deleted += *last - *first + 1;
            }

            Convert(_, _, _, _, _, _) => {
                self.convert_gates += 1;
            }

            Call(name, _, _) => {
                if let Some(func_content) = known_functions.get(name) {
                    match func_content {
                        FunctionContent::Function(stats) => {
                            self.functions_called += 1;
                            self.ingest_call_stats(stats);
                        }
                        FunctionContent::Plugin(_, _, _, pub_count, priv_count) => {
                            self.plugins_called += 1;
                            self.public_inputs_consumed += pub_count;
                            self.private_inputs_consumed += priv_count;
                        }
                    };
                } else {
                    eprintln!("WARNING Stats: function not defined \"{}\"", name);
                }
            }
        }
    }

    fn ingest_call_stats(&mut self, other: &GateStats) {
        // Inputs
        self.public_inputs_consumed += other.public_inputs_consumed;
        self.private_inputs_consumed += other.private_inputs_consumed;

        // Gates.
        self.constants_gates += other.constants_gates;
        self.assert_zero_gates += other.assert_zero_gates;
        self.copy_gates += other.copy_gates;
        self.add_gates += other.add_gates;
        self.mul_gates += other.mul_gates;
        self.add_constant_gates += other.add_constant_gates;
        self.mul_constant_gates += other.mul_constant_gates;
        self.variables_allocated_with_new += other.variables_allocated_with_new;
        self.variables_deleted += other.variables_deleted;
        self.convert_gates += other.convert_gates;

        self.functions_called += other.functions_called;
        self.plugins_called += other.plugins_called;
    }
}

#[test]
fn test_stats() {
    use crate::producers::examples::*;

    let public_inputs = example_public_inputs();
    let private_inputs = example_private_inputs();
    let relation = example_relation();

    let mut stats = Stats::default();
    public_inputs
        .iter()
        .for_each(|inputs| stats.ingest_public_inputs(inputs));
    private_inputs
        .iter()
        .for_each(|inputs| stats.ingest_private_inputs(inputs));
    stats.ingest_relation(&relation);

    let mut expected_stats = Stats {
        types: vec![
            Type::Field(vec![7]),
            Type::Field(vec![101]),
            Type::PluginType(
                "zkif_ring".to_string(),
                "type".to_string(),
                vec!["2".to_string(), "8".to_string()],
            ),
        ],
        gate_stats: GateStats {
            public_inputs_consumed: 2,
            private_inputs_consumed: 6,
            public_inputs_provided: 2,
            private_inputs_provided: 6,
            constants_gates: 0,
            assert_zero_gates: 1,
            copy_gates: 0,
            add_gates: 2,
            mul_gates: 1,
            add_constant_gates: 0,
            mul_constant_gates: 1,
            variables_allocated_with_new: 9,
            variables_deleted: 18,
            functions_defined: 1,
            functions_called: 1,
            plugins_defined: 5,
            plugins_called: 5,
            conversions_defined: 1,
            convert_gates: 3,
            public_inputs_messages: 2,
            private_inputs_messages: 3,
            relation_messages: 1,
        },
        functions: BTreeMap::new(),
    };
    expected_stats.functions.insert(
        "assert_equal_private".to_string(),
        FunctionContent::Plugin(
            "zkif_assert_equal".to_string(),
            "private".to_string(),
            vec!["1".to_string(), "1".to_string()],
            0,
            1,
        ),
    );
    expected_stats.functions.insert(
        "square".to_string(),
        FunctionContent::Function(GateStats {
            mul_gates: 1,
            ..GateStats::default()
        }),
    );
    expected_stats.functions.insert(
        "vector_mul_7_2".to_string(),
        FunctionContent::Plugin(
            "zkif_vector".to_string(),
            "mul".to_string(),
            vec!["1".to_string(), "2".to_string()],
            0,
            0,
        ),
    );
    expected_stats.functions.insert(
        "ring_add".to_string(),
        FunctionContent::Plugin(
            "zkif_ring".to_string(),
            "add".to_string(),
            vec!["2".to_string()],
            0,
            0,
        ),
    );
    expected_stats.functions.insert(
        "ring_mul".to_string(),
        FunctionContent::Plugin(
            "zkif_ring".to_string(),
            "mul".to_string(),
            vec!["2".to_string()],
            0,
            0,
        ),
    );
    expected_stats.functions.insert(
        "ring_equal".to_string(),
        FunctionContent::Plugin(
            "zkif_ring".to_string(),
            "equal".to_string(),
            vec!["2".to_string()],
            0,
            0,
        ),
    );

    assert_eq!(expected_stats, stats);
}
