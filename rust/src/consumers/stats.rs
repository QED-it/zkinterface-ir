extern crate serde;
extern crate serde_json;

use serde::{Deserialize, Serialize};

use crate::{Header, Relation, Instance, Witness, Messages, Gate};

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
    pub fn new() -> Stats {
        Stats {
            field_characteristic: vec![],
            field_degree: 0,
            instance_variables: 0,
            witness_variables: 0,
            constants_gates: 0,
            assert_zero_gates: 0,
            copy_gates: 0,
            add_gates: 0,
            mul_gates: 0,
            add_constant_gates: 0,
            mul_constant_gates: 0,
            and_gates: 0,
            xor_gates: 0,
            not_gates: 0,
        }
    }

    pub fn ingest_messages(&mut self, messages: &Messages) {
        for instance in &messages.instances {
            self.ingest_instance(instance);
        }
        for witness in &messages.witnesses {
            self.ingest_witness(witness);
        }
        for relation in &messages.relations {
            self.ingest_relation(relation);
        }
    }


    fn ingest_header(&mut self, header: &Header) {
        self.field_characteristic = header.field_characteristic.clone();
        self.field_degree = header.field_degree;
    }

    fn ingest_instance(&mut self, instance: &Instance) {
        self.ingest_header(&instance.header);
        self.instance_variables += instance.common_inputs.len();
    }

    fn ingest_witness(&mut self, witness: &Witness) {
        self.ingest_header(&witness.header);
        self.witness_variables += witness.short_witness.len();
    }

    fn ingest_relation(&mut self, relation: &Relation) {
        self.ingest_header(&relation.header);

        for gate in &relation.gates {
            match gate {
                Gate::Constant(_out, _value) => {
                    self.constants_gates += 1;
                }

                Gate::AssertZero(_inp) => {
                    self.assert_zero_gates += 1;
                }

                Gate::Copy(_out, _inp) => {
                    self.copy_gates += 1;
                }

                Gate::Add(_out, _left, _right) => {
                    self.add_gates += 1;
                }

                Gate::Mul(_out, _left, _right) => {
                    self.mul_gates += 1;
                }

                Gate::AddConstant(_out, _inp, _constant) => {
                    self.add_constant_gates += 1;
                }

                Gate::MulConstant(_out, _inp, _constant) => {
                    self.mul_constant_gates += 1;
                }

                Gate::And(_out, _left, _right) => {
                    self.and_gates += 1;
                }

                Gate::Xor(_out, _left, _right) => {
                    self.xor_gates += 1;
                }

                Gate::Not(_out, _inp) => {
                    self.not_gates += 1;
                }
            }
        }
    }



}


#[test]
fn test_stats() -> crate::Result<()> {
    use crate::producers::examples::*;

    let instance = example_instance();
    let witness = example_witness();
    let relation = example_relation();

    let mut stats = Stats::new();
    stats.ingest_instance(&instance);
    stats.ingest_witness(&witness);
    stats.ingest_relation(&relation);


    let expected_stats = Stats {
            field_characteristic: literal(MODULUS),
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