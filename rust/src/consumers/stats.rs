extern crate serde;
extern crate serde_json;

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{Gate, Header, Instance, Message, Relation, Witness, Result};
use crate::structs::function::{CaseInvoke, ForLoopBody};

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Stats {
    // Header.
    pub field_characteristic: Vec<u8>,
    pub field_degree: u32,
    // Inputs.
    pub instance_variables: usize,
    pub witness_variables: usize,
    // Gates.
    pub constants_gates: usize,
    pub assert_zero_gates: usize,
    pub copy_gates: usize,
    pub add_gates: usize,
    pub mul_gates: usize,
    pub add_constant_gates: usize,
    pub mul_constant_gates: usize,
    pub and_gates: usize,
    pub xor_gates: usize,
    pub not_gates: usize,
    pub variables_freed: usize,

    pub functions_defined: usize,
    pub functions_called: usize,

    pub switches: usize,
    pub branches: usize,

    // The number of messages into which the statement was split.
    pub instance_messages: usize,
    pub witness_messages: usize,
    pub relation_messages: usize,
    // Function definitions => stats / instance_count / witness_count
    pub functions: HashMap<String, (Stats, usize, usize)>,
}

impl Stats {
    pub fn from_messages(messages: impl Iterator<Item = Result<Message>>) -> Self {
        let mut stats = Stats::default();
        messages.for_each(|msg| stats.ingest_message(&msg.unwrap()));
        stats
    }

    pub fn ingest_message(&mut self, msg: &Message) {
        match msg {
            Message::Instance(i) => self.ingest_instance(&i),
            Message::Witness(w) => self.ingest_witness(&w),
            Message::Relation(r) => self.ingest_relation(&r),
        }
    }

    pub fn ingest_instance(&mut self, instance: &Instance) {
        self.ingest_header(&instance.header);
        self.instance_messages += 1;
    }

    pub fn ingest_witness(&mut self, witness: &Witness) {
        self.ingest_header(&witness.header);
        self.witness_messages += 1;
    }

    pub fn ingest_relation(&mut self, relation: &Relation) {
        self.ingest_header(&relation.header);
        self.relation_messages += 1;

        for f in relation.functions.iter() {
            let (name, _, _, instance_count, witness_count, subcircuit) =
                (f.name.clone(), f.output_count, f.input_count, f.instance_count, f.witness_count, f.body.clone());
            // Just record the signature.
            self.functions_defined += 1;
            let func_stats = self.ingest_subcircuit(&subcircuit);
            self.functions.insert(name.clone(), (func_stats, instance_count, witness_count));

        }

        for gate in &relation.gates {
            self.ingest_gate(gate);
        }
    }

    fn ingest_gate(&mut self, gate: &Gate) {
        use Gate::*;

        match gate {
            Constant(_out, _value) => {
                self.constants_gates += 1;
            }

            AssertZero(_inp) => {
                self.assert_zero_gates += 1;
            }

            Copy(_out, _inp) => {
                self.copy_gates += 1;
            }

            Add(_out, _left, _right) => {
                self.add_gates += 1;
            }

            Mul(_out, _left, _right) => {
                self.mul_gates += 1;
            }

            AddConstant(_out, _inp, _constant) => {
                self.add_constant_gates += 1;
            }

            MulConstant(_out, _inp, _constant) => {
                self.mul_constant_gates += 1;
            }

            And(_out, _left, _right) => {
                self.and_gates += 1;
            }

            Xor(_out, _left, _right) => {
                self.xor_gates += 1;
            }

            Not(_out, _inp) => {
                self.not_gates += 1;
            }

            Instance(_out) => {
                self.instance_variables += 1;
            }

            Witness(_out) => {
                self.witness_variables += 1;
            }

            Free(first, last) => {
                let last_one = last.unwrap_or(*first);
                self.variables_freed += (last_one - *first + 1) as usize;
            }

            Call(name, _, _) => {
                self.functions_called += 1;
                if let Some(stats_ins_wit) = self.functions.get(name).cloned() {
                    self.ingest_call_stats(&stats_ins_wit.0);
                    self.instance_variables += stats_ins_wit.1;
                    self.witness_variables  += stats_ins_wit.2;
                } else {
                    eprintln!("WARNING Stats: function not defined \"{}\"", name);
                }
            }

            AnonCall(_, _, instance_count, witness_count, subcircuit) => {
                self.ingest_call_stats(&self.ingest_subcircuit(subcircuit));
                self.instance_variables += instance_count;
                self.witness_variables  += witness_count;
            }

            Switch(_, _,  _, branches) => {
                self.switches += 1;
                self.branches += branches.len();
                let (mut max_instance_count, mut max_witness_count) = (0usize, 0usize);

                for branch in branches {
                    let (instance_count, witness_count) = match branch {
                        CaseInvoke::AbstractGateCall(name, _) => {
                            self.functions_called += 1;
                            if let Some(stats_ins_wit) = self.functions.get(name).cloned() {
                                self.ingest_call_stats(&stats_ins_wit.0);
                                (stats_ins_wit.1, stats_ins_wit.2)
                            } else {
                                eprintln!("WARNING Stats: function not defined \"{}\"", name);
                                (0usize, 0usize)
                            }
                        }
                        CaseInvoke::AbstractAnonCall(_, instance_count, witness_count, subcircuit) => {
                            self.ingest_call_stats(&self.ingest_subcircuit(subcircuit));
                            (*instance_count, *witness_count)
                        }
                    };

                    max_instance_count = std::cmp::max(max_instance_count, instance_count);
                    max_witness_count  = std::cmp::max(max_witness_count,  witness_count);
                }

                self.instance_variables += max_instance_count;
                self.witness_variables  += max_witness_count;
            }

            For(
                _,
                start_val,
                end_val,
                _,
                body
            ) => {
                for _ in *start_val..=*end_val {
                    match body {
                        ForLoopBody::IterExprCall(name, _, _) => {
                            self.functions_called += 1;
                            if let Some(stats_ins_wit) = self.functions.get(name).cloned() {
                                self.ingest_call_stats(&stats_ins_wit.0);
                                self.instance_variables += stats_ins_wit.1;
                                self.witness_variables += stats_ins_wit.2;
                            } else {
                                eprintln!("WARNING Stats: function not defined \"{}\"", name);
                            }
                        }
                        ForLoopBody::IterExprAnonCall(_, _,
                                                      instance_count,
                                                      witness_count,
                                                      subcircuit
                        ) => {
                            self.ingest_call_stats(&self.ingest_subcircuit(subcircuit));
                            self.instance_variables += *instance_count;
                            self.witness_variables += *witness_count;
                        }
                    }
                }
            },
        }
    }

    fn ingest_call_stats(&mut self, other: &Stats) {
        // Gates.
        self.constants_gates += other.constants_gates;
        self.assert_zero_gates += other.assert_zero_gates;
        self.copy_gates += other.copy_gates;
        self.add_gates += other.add_gates;
        self.mul_gates += other.mul_gates;
        self.add_constant_gates += other.add_constant_gates;
        self.mul_constant_gates += other.mul_constant_gates;
        self.and_gates += other.and_gates;
        self.xor_gates += other.xor_gates;
        self.not_gates += other.not_gates;
        self.variables_freed += other.variables_freed;

        self.switches += other.switches;
        self.branches += other.branches;
        self.functions_called += other.functions_called;
    }

    fn ingest_header(&mut self, header: &Header) {
        self.field_characteristic = header.field_characteristic.clone();
        self.field_degree = header.field_degree;
    }

    fn ingest_subcircuit(&self, subcircuit: &[Gate]) -> Stats {
        let mut local_stats = Stats::default();
        local_stats.functions = self.functions.clone();
        for gate in subcircuit {
            local_stats.ingest_gate(gate);
        }
        local_stats
    }
}

#[test]
fn test_stats() -> crate::Result<()> {
    use crate::producers::examples::*;

    let instance = example_instance();
    let witness = example_witness();
    let relation = example_relation();

    let mut stats = Stats::default();
    stats.ingest_instance(&instance);
    stats.ingest_witness(&witness);
    stats.ingest_relation(&relation);

    let mut expected_stats = Stats {
        field_characteristic: literal(EXAMPLE_MODULUS),
        field_degree: 1,
        instance_variables: 3,
        witness_variables: 3,
        constants_gates: 1,
        assert_zero_gates: 2,
        copy_gates: 0,
        add_gates: 25,
        mul_gates: 5,
        add_constant_gates: 0,
        mul_constant_gates: 1,
        and_gates: 0,
        xor_gates: 0,
        not_gates: 0,
        variables_freed: 35,
        functions_defined: 1,
        functions_called: 4,
        switches: 1,
        branches: 2,
        instance_messages: 1,
        witness_messages: 1,
        relation_messages: 1,
        functions: HashMap::new(),
    };
    expected_stats.functions.insert(
        "com.example::mul".to_string(),
        (Stats {
            mul_gates: 1,
            ..Stats::default()
        }, 0, 0),
    );

    assert_eq!(expected_stats, stats);

    Ok(())
}
