extern crate serde;
extern crate serde_json;

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{Gate, Header, Instance, Message, Relation, Witness, Result};
use crate::structs::functions::Directive;
use std::cmp::max;

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
    // Function definitions
    pub functions: HashMap<String, Stats>,
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

            Function(name, _, _, _, _, implementation) => {
                self.functions_defined += 1;
                let mut func_stats = Stats::default();
                for gate in implementation {
                    func_stats.ingest_gate(gate);
                }
                self.functions.insert(name.clone(), func_stats);
            }

            Call(_, dir) => {
                self.functions_called += 1;
                self.ingest_directive(dir);
            }

            Switch(_, _, _, branches) => {
                self.switches += 1;
                self.branches += branches.len();
                let (mut max_inst, mut max_wit) = (0usize, 0usize);
                for dir in branches {
                    let (inst, wit) = self.get_inst_wit_nbr(dir).unwrap();
                    max_inst = max(max_inst, inst);
                    max_wit = max(max_wit, wit);
                }

                for dir in branches {
                    let (inst, wit) = self.get_inst_wit_nbr(dir).unwrap();
                    self.ingest_directive(dir);
                    self.instance_variables -= inst;
                    self.witness_variables -= wit;
                }

                self.instance_variables += max_inst;
                self.witness_variables += max_wit;

            }
        }
    }

    fn ingest_call_stats(&mut self, other: &Stats) {
        // Inputs.
        self.instance_variables += other.instance_variables;
        self.witness_variables += other.witness_variables;
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
    }

    fn ingest_header(&mut self, header: &Header) {
        self.field_characteristic = header.field_characteristic.clone();
        self.field_degree = header.field_degree;
    }

    fn get_inst_wit_nbr(&self, directive: &Directive) -> Result<(usize, usize)> {
        match directive {
            Directive::AbstractCall(name, _) => {
                let stats = self.functions.get(name).ok_or("unknown function")?;
                Ok((stats.instance_variables, stats.witness_variables))
            }
            Directive::AbstractAnonCall(_, instance_count, witness_count, _) => Ok((*instance_count, *witness_count))
        }
    }

    fn ingest_directive(&mut self, dir: &Directive) {
        use Directive::*;
        match dir {
            AbstractCall(name, _) => {
                if let Some(func_stats) = self.functions.get(name).cloned() {
                    self.ingest_call_stats(&func_stats);
                } else {
                    eprintln!("WARNING Stats: function not defined \"{}\"", name);
                }
            }
            AbstractAnonCall(_, _, _, subcircuit) => {
                for gate in subcircuit {
                    self.ingest_gate(gate);
                }
            },
        }
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
        instance_variables: 1,
        witness_variables: 2,
        constants_gates: 1,
        assert_zero_gates: 1,
        copy_gates: 0,
        add_gates: 3,
        mul_gates: 6,
        add_constant_gates: 0,
        mul_constant_gates: 0,
        and_gates: 0,
        xor_gates: 0,
        not_gates: 0,
        variables_freed: 8,
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
        "example/mul".to_string(),
        Stats {
            mul_gates: 1,
            ..Stats::default()
        },
    );

    assert_eq!(expected_stats, stats);

    Ok(())
}
