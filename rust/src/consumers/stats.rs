extern crate serde;
extern crate serde_json;

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::structs::function::ForLoopBody;
use crate::{Gate, Header, Instance, Message, Relation, Result, Value, Witness};

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct GateStats {
    // Inputs.
    pub instance_variables: u64,
    pub witness_variables: u64,
    // Gates.
    pub constants_gates: usize,
    pub assert_zero_gates: usize,
    pub copy_gates: usize,
    pub add_gates: usize,
    pub mul_gates: usize,
    pub add_constant_gates: usize,
    pub mul_constant_gates: usize,
    pub variables_freed: u64,

    pub functions_defined: usize,
    pub functions_called: usize,

    pub convert_gates: usize,

    pub for_loops: usize,

    // The number of messages into which the statement was split.
    pub instance_messages: usize,
    pub witness_messages: usize,
    pub relation_messages: usize,
}

#[derive(Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Stats {
    // Header.
    pub moduli: Vec<Value>,

    pub gate_stats: GateStats,

    // Function definitions => stats / instance_count / witness_count
    pub functions: HashMap<String, (GateStats, u64, u64)>,
}

impl Stats {
    pub fn from_messages(messages: impl Iterator<Item = Result<Message>>) -> Self {
        let mut stats = Stats::default();
        messages.for_each(|msg| stats.ingest_message(&msg.unwrap()));
        stats
    }

    pub fn ingest_message(&mut self, msg: &Message) {
        match msg {
            Message::Instance(i) => self.ingest_instance(i),
            Message::Witness(w) => self.ingest_witness(w),
            Message::Relation(r) => self.ingest_relation(r),
        }
    }

    pub fn ingest_instance(&mut self, instance: &Instance) {
        self.ingest_header(&instance.header);
        self.gate_stats.instance_messages += 1;
    }

    pub fn ingest_witness(&mut self, witness: &Witness) {
        self.ingest_header(&witness.header);
        self.gate_stats.witness_messages += 1;
    }

    pub fn ingest_relation(&mut self, relation: &Relation) {
        self.ingest_header(&relation.header);
        self.gate_stats.relation_messages += 1;

        for f in relation.functions.iter() {
            let name = f.name.clone();
            let instance_count = f.instance_count.values().sum::<u64>();
            let witness_count = f.witness_count.values().sum::<u64>();
            let subcircuit = f.body.clone();

            // Just record the signature.
            self.gate_stats.functions_defined += 1;
            let func_stats = ingest_subcircuit(&subcircuit, &self.functions);
            self.functions
                .insert(name.clone(), (func_stats, instance_count, witness_count));
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
    known_functions: &HashMap<String, (GateStats, u64, u64)>,
) -> GateStats {
    let mut local_stats = GateStats::default();
    for gate in subcircuit {
        local_stats.ingest_gate(gate, known_functions);
    }
    local_stats
}

impl GateStats {
    fn ingest_gate(
        &mut self,
        gate: &Gate,
        known_functions: &HashMap<String, (GateStats, u64, u64)>,
    ) {
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

            Instance(_field_id, _out) => {
                self.instance_variables += 1;
            }

            Witness(_field_id, _out) => {
                self.witness_variables += 1;
            }

            Free(_field_id, first, last) => {
                let last_one = last.unwrap_or(*first);
                self.variables_freed += last_one - *first + 1;
            }

            Convert(_, _) => {
                self.convert_gates += 1;
            }

            Call(name, _, _) => {
                self.functions_called += 1;
                if let Some(stats_ins_wit) = known_functions.get(name).cloned() {
                    self.ingest_call_stats(&stats_ins_wit.0);
                    self.instance_variables += stats_ins_wit.1;
                    self.witness_variables += stats_ins_wit.2;
                } else {
                    eprintln!("WARNING Stats: function not defined \"{}\"", name);
                }
            }

            AnonCall(_, _, instance_count, witness_count, subcircuit) => {
                self.ingest_call_stats(&ingest_subcircuit(subcircuit, known_functions));
                self.instance_variables += instance_count.values().sum::<u64>();
                self.witness_variables += witness_count.values().sum::<u64>();
            }

            For(_, start_val, end_val, _, body) => {
                self.for_loops += 1;
                for _ in *start_val..=*end_val {
                    match body {
                        ForLoopBody::IterExprCall(name, _, _, _) => {
                            self.functions_called += 1;
                            if let Some(stats_ins_wit) = known_functions.get(name).cloned() {
                                self.ingest_call_stats(&stats_ins_wit.0);
                                self.instance_variables += stats_ins_wit.1;
                                self.witness_variables += stats_ins_wit.2;
                            } else {
                                eprintln!("WARNING Stats: function not defined \"{}\"", name);
                            }
                        }
                        ForLoopBody::IterExprAnonCall(
                            _,
                            _,
                            _,
                            instance_count,
                            witness_count,
                            subcircuit,
                        ) => {
                            self.ingest_call_stats(&ingest_subcircuit(subcircuit, known_functions));
                            self.instance_variables += instance_count.values().sum::<u64>();
                            self.witness_variables += witness_count.values().sum::<u64>();
                        }
                    }
                }
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
        self.variables_freed += other.variables_freed;

        self.convert_gates += other.convert_gates;
        self.for_loops += other.for_loops;
        self.functions_called += other.functions_called;
    }
}

#[test]
fn test_stats() -> Result<()> {
    use crate::producers::examples::*;

    let instance = example_instance();
    let witness = example_witness();
    let relation = example_relation();

    let mut stats = Stats::default();
    stats.ingest_instance(&instance);
    stats.ingest_witness(&witness);
    stats.ingest_relation(&relation);

    let mut expected_stats = Stats {
        moduli: vec![literal(EXAMPLE_MODULUS)],
        gate_stats: GateStats {
            instance_variables: 3,
            witness_variables: 4,
            constants_gates: 1,
            assert_zero_gates: 4,
            copy_gates: 0,
            add_gates: 24,
            mul_gates: 19,
            add_constant_gates: 0,
            mul_constant_gates: 1,
            variables_freed: 51,
            functions_defined: 1,
            functions_called: 19,
            convert_gates: 0,
            for_loops: 2,
            instance_messages: 1,
            witness_messages: 1,
            relation_messages: 1,
        },
        functions: HashMap::new(),
    };
    expected_stats.functions.insert(
        "com.example::mul".to_string(),
        (
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
