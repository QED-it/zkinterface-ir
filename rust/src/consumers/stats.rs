extern crate serde;
extern crate serde_json;

use serde::{Deserialize, Serialize};

use crate::{Gate, Header, Instance, Message, Relation, Witness};

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
    // The number of messages into which the statement was split.
    pub instance_messages: usize,
    pub witness_messages: usize,
    pub relation_messages: usize,
}

impl Stats {
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
        use Gate::*;

        self.ingest_header(&relation.header);
        self.relation_messages += 1;

        for gate in &relation.gates {
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

                Function(_, _, _, _, _) => unimplemented!(),
                Call(_, _, _, _) => unimplemented!(),
            }
        }
    }

    fn ingest_header(&mut self, header: &Header) {
        self.field_characteristic = header.field_characteristic.clone();
        self.field_degree = header.field_degree;
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

    let expected_stats = Stats {
        field_characteristic: literal(EXAMPLE_MODULUS),
        field_degree: 1,
        instance_variables: 1,
        witness_variables: 2,
        constants_gates: 1,
        assert_zero_gates: 1,
        copy_gates: 0,
        add_gates: 2,
        mul_gates: 3,
        add_constant_gates: 0,
        mul_constant_gates: 0,
        and_gates: 0,
        xor_gates: 0,
        not_gates: 0,
        variables_freed: 8,
        instance_messages: 1,
        witness_messages: 1,
        relation_messages: 1,
    };
    assert_eq!(expected_stats, stats);

    Ok(())
}
