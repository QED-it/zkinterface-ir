use crate::{Result, Header, Relation, Instance, Witness, Message, Gate};
use std::collections::{HashMap, VecDeque};
use num_bigint::BigUint;
use num_traits::identities::{Zero, One};
use std::ops::{BitAnd, BitXor};

type Wire = u64;
type Repr = BigUint;

#[derive(Clone, Default)]
pub struct Evaluator {
    values: HashMap<Wire, Repr>,
    modulus: Repr,
    instance_queue: VecDeque<Repr>,
    witness_queue: VecDeque<Repr>,

    verified_at_least_one_gate: bool,
    found_error: Option<String>,
}

impl Evaluator {
    pub fn from_messages(messages: impl Iterator<Item=Result<Message>>) -> Self {
        let mut evaluator = Evaluator::default();
        messages.for_each(|msg|
            evaluator.ingest_message(&msg.unwrap()));
        evaluator
    }

    pub fn get_violations(self) -> Vec<String> {
        let mut violations = vec![];
        if !self.verified_at_least_one_gate {
            violations.push("Did not receive any gate to verify.".to_string());
        }
        if let Some(err) = self.found_error {
            violations.push(err);
        }
        violations
    }

    pub fn ingest_message(&mut self, msg: &Message) {
        if self.found_error.is_some() { return; }

        match self.ingest_message_(msg) {
            Err(err) => self.found_error = Some(err.to_string()),
            Ok(()) => {}
        }
    }

    fn ingest_message_(&mut self, msg: &Message) -> Result<()> {
        match msg {
            Message::Instance(i) => self.ingest_instance(&i),
            Message::Witness(w) => self.ingest_witness(&w),
            Message::Relation(r) => self.ingest_relation(&r),
        }
    }

    fn ingest_header(&mut self, header: &Header) -> Result<()> {
        self.modulus = BigUint::from_bytes_le(&header.field_characteristic);
        if self.modulus.is_zero() {
            Err("Header.field_characteristic cannot be zero".into())
        } else {
            Ok(())
        }
    }

    pub fn ingest_instance(&mut self, instance: &Instance) -> Result<()> {
        self.ingest_header(&instance.header)?;

        for value in &instance.common_inputs {
            self.instance_queue.push_back(BigUint::from_bytes_le(value));
        }
        Ok(())
    }

    pub fn ingest_witness(&mut self, witness: &Witness) -> Result<()> {
        self.ingest_header(&witness.header)?;

        for value in &witness.short_witness {
            self.witness_queue.push_back(BigUint::from_bytes_le(value));
        }
        Ok(())
    }

    pub fn ingest_relation(&mut self, relation: &Relation) -> Result<()> {
        use Gate::*;

        self.ingest_header(&relation.header)?;

        if relation.gates.len() > 0 {
            self.verified_at_least_one_gate = true;
        }

        for gate in &relation.gates {
            match gate {
                Constant(out, value) =>
                    self.set_encoded(*out, value),

                AssertZero(inp) => {
                    let val = self.get(*inp)?;
                    if !val.is_zero() {
                        return Err(format!("wire_{} should equal 0 but has value {}", *inp, val).into());
                    }
                }

                Copy(out, inp) => {
                    let value = self.get(*inp)?.clone();
                    self.set(*out, value);
                }

                Add(out, left, right) => {
                    let l = self.get(*left)?;
                    let r = self.get(*right)?;
                    let sum = l + r;
                    self.set(*out, sum);
                }

                Mul(out, left, right) => {
                    let l = self.get(*left)?;
                    let r = self.get(*right)?;
                    let prod = l * r;
                    self.set(*out, prod);
                }

                AddConstant(out, inp, constant) => {
                    let l = self.get(*inp)?;
                    let r = BigUint::from_bytes_le(constant);
                    let sum = l + r;
                    self.set(*out, sum);
                }

                MulConstant(out, inp, constant) => {
                    let l = self.get(*inp)?;
                    let r = BigUint::from_bytes_le(constant);
                    let prod = l * r;
                    self.set(*out, prod);
                }

                And(out, left, right) => {
                    let l = self.get(*left)?;
                    let r = self.get(*right)?;
                    let and = l.bitand(r);
                    self.set(*out, and);
                }

                Xor(out, left, right) => {
                    let l = self.get(*left)?;
                    let r = self.get(*right)?;
                    let xor = l.bitxor(r);
                    self.set(*out, xor);
                }

                Not(out, inp) => {
                    let val = self.get(*inp)?;
                    let not = if val.is_zero() { BigUint::one() } else { BigUint::zero() };
                    self.set(*out, not);
                }

                Instance(out) => {
                    let val = self.instance_queue.pop_front().unwrap();
                    self.set(*out, val);
                }

                Witness(out) => {
                    let val = self.witness_queue.pop_front().unwrap();
                    self.set(*out, val);
                }
            }
        }
        Ok(())
    }


    fn set_encoded(&mut self, id: Wire, encoded: &[u8]) {
        self.set(id, BigUint::from_bytes_le(encoded));
    }

    fn set(&mut self, id: Wire, mut value: Repr) {
        value %= &self.modulus;
        self.values.insert(id, value);
    }

    pub fn get(&self, id: Wire) -> Result<&Repr> {
        self.values.get(&id)
            .ok_or(format!("No value given for wire_{}", id).into())
    }
}

#[test]
fn test_simulator() -> Result<()> {
    use crate::producers::examples::*;

    let relation = example_relation();
    let instance = example_instance();
    let witness = example_witness();

    let mut simulator = Evaluator::default();
    simulator.ingest_instance(&instance)?;
    simulator.ingest_witness(&witness)?;
    simulator.ingest_relation(&relation)?;

    Ok(())
}
