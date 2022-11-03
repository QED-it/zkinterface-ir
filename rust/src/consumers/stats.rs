extern crate serde;
extern crate serde_json;

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::structs::function::FunctionBody;
use crate::{Gate, Header, Message, PrivateInputs, PublicInputs, Relation, Result, Value};

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct GateStats {
    // Inputs.
    pub public_variables: u64,
    pub private_variables: u64,
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

    pub functions_defined: usize,
    pub functions_called: usize,

    pub plugins_defined: usize,
    pub plugins_called: usize,

    pub convert_gates: usize,

    // The number of messages into which the statement was split.
    pub public_inputs_messages: usize,
    pub private_inputs_messages: usize,
    pub relation_messages: usize,
}

#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum FunctionContent {
    Plugin(u64, u64),              // (public_count, private_count)
    Function(GateStats, u64, u64), // (stats, public_count, private_count)
}

#[derive(Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Stats {
    // Header.
    pub moduli: Vec<Value>,

    pub gate_stats: GateStats,

    pub functions: HashMap<String, FunctionContent>,
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
        self.ingest_header(&public_inputs.header);
        self.gate_stats.public_inputs_messages += 1;
    }

    pub fn ingest_private_inputs(&mut self, private_inputs: &PrivateInputs) {
        self.ingest_header(&private_inputs.header);
        self.gate_stats.private_inputs_messages += 1;
    }

    pub fn ingest_relation(&mut self, relation: &Relation) {
        self.ingest_header(&relation.header);
        self.gate_stats.relation_messages += 1;

        for f in relation.functions.iter() {
            let name = f.name.clone();
            let public_count = f.public_count.values().sum::<u64>();
            let private_count = f.private_count.values().sum::<u64>();
            match &f.body {
                FunctionBody::Gates(gates) => {
                    self.gate_stats.functions_defined += 1;
                    let func_stats = ingest_subcircuit(gates, &self.functions);
                    self.functions.insert(
                        name.clone(),
                        FunctionContent::Function(func_stats, public_count, private_count),
                    );
                }
                FunctionBody::PluginBody(_) => {
                    self.gate_stats.plugins_defined += 1;
                    self.functions.insert(
                        name.clone(),
                        FunctionContent::Plugin(public_count, private_count),
                    );
                }
            }
        }

        for gate in &relation.gates {
            self.gate_stats.ingest_gate(gate, &self.functions);
        }
    }

    fn ingest_header(&mut self, header: &Header) {
        if self.moduli.is_empty() {
            // Header has not yet been ingested
            for field in &header.fields {
                self.moduli.push(field.clone());
            }
        }
    }
}

fn ingest_subcircuit(
    subcircuit: &[Gate],
    known_functions: &HashMap<String, FunctionContent>,
) -> GateStats {
    let mut local_stats = GateStats::default();
    for gate in subcircuit {
        local_stats.ingest_gate(gate, known_functions);
    }
    local_stats
}

impl GateStats {
    fn ingest_gate(&mut self, gate: &Gate, known_functions: &HashMap<String, FunctionContent>) {
        use Gate::*;

        match gate {
            Constant(_field_id, _out, _value) => {
                self.constants_gates += 1;
            }

            AssertZero(_field_id, _inp) => {
                self.assert_zero_gates += 1;
            }

            Copy(_field_id, _out, _inp) => {
                self.copy_gates += 1;
            }

            Add(_field_id, _out, _left, _right) => {
                self.add_gates += 1;
            }

            Mul(_field_id, _out, _left, _right) => {
                self.mul_gates += 1;
            }

            AddConstant(_field_id, _out, _inp, _constant) => {
                self.add_constant_gates += 1;
            }

            MulConstant(_field_id, _out, _inp, _constant) => {
                self.mul_constant_gates += 1;
            }

            PublicInput(_field_id, _out) => {
                self.public_variables += 1;
            }

            PrivateInput(_field_id, _out) => {
                self.private_variables += 1;
            }

            New(_field_id, first, last) => {
                self.variables_allocated_with_new += last - first + 1;
            }

            Delete(_field_id, first, last) => {
                let last_one = last.unwrap_or(*first);
                self.variables_deleted += last_one - *first + 1;
            }

            Convert(_, _) => {
                self.convert_gates += 1;
            }

            Call(name, _, _) => {
                if let Some(func_content) = known_functions.get(name) {
                    match func_content {
                        FunctionContent::Function(stats, pub_count, priv_count) => {
                            self.functions_called += 1;
                            self.ingest_call_stats(stats);
                            self.public_variables += pub_count;
                            self.private_variables += priv_count;
                        }
                        FunctionContent::Plugin(pub_count, priv_count) => {
                            self.plugins_called += 1;
                            self.public_variables += pub_count;
                            self.private_variables += priv_count;
                        }
                    };
                } else {
                    eprintln!("WARNING Stats: function not defined \"{}\"", name);
                }
            }

            AnonCall(_, _, public_count, private_count, subcircuit) => {
                self.ingest_call_stats(&ingest_subcircuit(subcircuit, known_functions));
                self.public_variables += public_count.values().sum::<u64>();
                self.private_variables += private_count.values().sum::<u64>();
            }
        }
    }

    fn ingest_call_stats(&mut self, other: &GateStats) {
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
    }
}

#[test]
fn test_stats() -> Result<()> {
    use crate::producers::examples::*;

    let public_inputs = example_public_inputs();
    let private_inputs = example_private_inputs();
    let relation = example_relation();

    let mut stats = Stats::default();
    stats.ingest_public_inputs(&public_inputs);
    stats.ingest_private_inputs(&private_inputs);
    stats.ingest_relation(&relation);

    let mut expected_stats = Stats {
        moduli: vec![literal(EXAMPLE_MODULUS)],
        gate_stats: GateStats {
            public_variables: 3,
            private_variables: 3,
            constants_gates: 1,
            assert_zero_gates: 3,
            copy_gates: 0,
            add_gates: 2,
            mul_gates: 4,
            add_constant_gates: 0,
            mul_constant_gates: 0,
            variables_allocated_with_new: 8,
            variables_deleted: 12,
            functions_defined: 1,
            functions_called: 3,
            plugins_defined: 0,
            plugins_called: 0,
            convert_gates: 0,
            public_inputs_messages: 1,
            private_inputs_messages: 1,
            relation_messages: 1,
        },
        functions: HashMap::new(),
    };
    expected_stats.functions.insert(
        "com.example::mul".to_string(),
        FunctionContent::Function(
            GateStats {
                mul_gates: 1,
                ..GateStats::default()
            },
            0,
            0,
        ),
    );

    assert_eq!(expected_stats, stats);

    Ok(())
}
