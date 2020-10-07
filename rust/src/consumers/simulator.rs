use crate::{Result, Header, Relation, Instance, Witness, Messages, Gate};
use std::collections::HashMap;
use num_bigint::BigUint;
use num_traits::identities::{Zero, One};
use std::ops::{BitAnd, BitXor};

type Wire = u64;
type Value = BigUint;

#[derive(Clone, Default)]
pub struct Simulator {
    values: HashMap<Wire, Value>,
    modulus: Value,
}

impl Simulator {
    pub fn simulate(&mut self, messages: &Messages) -> Result<()> {
        for header in &messages.headers {
            self.ingest_header(header)?;
        }
        for instance in &messages.instances {
            self.ingest_instance(instance)?;
        }
        for witness in &messages.witnesses {
            self.ingest_witness(witness)?;
        }
        for gates in &messages.relations {
            self.ingest_relation(gates)?;
        }
        Ok(())
    }

    pub fn ingest_header(&mut self, header: &Header) -> Result<()> {
        self.modulus = BigUint::from_bytes_le(&header.field_characteristic);
        if self.modulus.is_zero() {
            Err("Header.field_characteristic cannot be zero".into())
        } else {
            Ok(())
        }
    }

    pub fn ingest_instance(&mut self, instance: &Instance) -> Result<()> {
        self.ensure_header()?;

        for var in &instance.common_inputs {
            self.set_encoded(var.id, &var.value);
        }
        Ok(())
    }

    pub fn ingest_witness(&mut self, witness: &Witness) -> Result<()> {
        self.ensure_header()?;

        for var in &witness.short_witness {
            self.set_encoded(var.id, &var.value);
        }
        Ok(())
    }

    pub fn ingest_relation(&mut self, relation: &Relation) -> Result<()> {
        self.ensure_header()?;

        for gate in &relation.gates {
            match gate {
                Gate::Constant(out, value) =>
                    self.set_encoded(*out, value),

                Gate::AssertZero(inp) => {
                    let val = self.get(*inp)?;
                    if !val.is_zero() {
                        return Err(format!("wire_{} should equal 0 but has value {}", *inp, val).into());
                    }
                }

                Gate::Copy(out, inp) => {
                    let value = self.get(*inp)?.clone();
                    self.set(*out, value);
                }

                Gate::Add(out, left, right) => {
                    let l = self.get(*left)?;
                    let r = self.get(*right)?;
                    let sum = l + r;
                    self.set(*out, sum);
                }

                Gate::Mul(out, left, right) => {
                    let l = self.get(*left)?;
                    let r = self.get(*right)?;
                    let prod = l * r;
                    self.set(*out, prod);
                }

                Gate::AddConstant(out, inp, constant) => {
                    let l = self.get(*inp)?;
                    let r = BigUint::from_bytes_le(constant);
                    let sum = l + r;
                    self.set(*out, sum);
                }

                Gate::MulConstant(out, inp, constant) => {
                    let l = self.get(*inp)?;
                    let r = BigUint::from_bytes_le(constant);
                    let prod = l * r;
                    self.set(*out, prod);
                }

                Gate::And(out, left, right) => {
                    let l = self.get(*left)?;
                    let r = self.get(*right)?;
                    let and = l.bitand(r);
                    self.set(*out, and);
                }

                Gate::Xor(out, left, right) => {
                    let l = self.get(*left)?;
                    let r = self.get(*right)?;
                    let xor = l.bitxor(r);
                    self.set(*out, xor);
                }

                Gate::Not(out, inp) => {
                    let val = self.get(*inp)?;
                    let not = if val.is_zero() { BigUint::one() } else { BigUint::zero() };
                    self.set(*out, not);
                }
            }
        }
        Ok(())
    }


    fn set_encoded(&mut self, id: Wire, encoded: &[u8]) {
        self.set(id, BigUint::from_bytes_le(encoded));
    }

    fn set(&mut self, id: Wire, mut value: Value) {
        value %= &self.modulus;
        self.values.insert(id, value);
    }

    fn get(&self, id: Wire) -> Result<&Value> {
        self.values.get(&id)
            .ok_or(format!("No value given for wire_{}", id).into())
    }

    fn ensure_header(&self) -> Result<()> {
        match self.modulus.is_zero() {
            true => Err("A header must be provided before other messages.".into()),
            _ => Ok(()),
        }
    }
}

#[test]
fn test_simulator() -> Result<()> {
    use crate::producers::examples::*;

    let header = example_header();
    let relation = example_relation();
    let instance = example_instance();
    let witness = example_witness();

    let mut simulator = Simulator::default();
    simulator.ingest_header(&header)?;
    simulator.ingest_instance(&instance)?;
    simulator.ingest_witness(&witness)?;
    simulator.ingest_relation(&relation)?;

    Ok(())
}
