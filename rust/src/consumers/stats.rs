extern crate serde;
extern crate serde_json;

use serde::{Deserialize, Serialize};

use crate::{Gate, Header, Instance, Message, Relation, Witness};

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Stats {
    pub field_characteristic: Vec<u8>,
    pub field_degree: u32,
    pub instance_variables: usize,
    pub witness_variables: usize,
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
}

impl Stats {
    pub fn ingest_message(&mut self, msg: &Message) {
        match msg {
            Message::Instance(i) => self.ingest_instance(&i),
            Message::Witness(w) => self.ingest_witness(&w),
            Message::Relation(r) => self.ingest_relation(&r),
        }
    }

    pub fn ingest_instance(&mut self, _instance: &Instance) {}

    pub fn ingest_witness(&mut self, _witness: &Witness) {}

    pub fn ingest_relation(&mut self, relation: &Relation) {
        use Gate::*;

        self.ingest_header(&relation.header);

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
    };
    assert_eq!(expected_stats, stats);

    Ok(())
}
