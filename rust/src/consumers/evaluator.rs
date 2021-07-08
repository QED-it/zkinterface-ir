use crate::{Gate, Header, Instance, Message, Relation, Result, Witness, WireId};
use num_bigint::BigUint;
use num_traits::identities::{One, Zero};
use std::collections::{HashMap, VecDeque};
use std::ops::{BitAnd, BitXor};
use crate::structs::subcircuit::translate_gates;
use crate::structs::wire::expand_wirelist;
use crate::structs::function::CaseInvoke;

type Repr = BigUint;

#[derive(Clone)]
pub struct Evaluator {
    values: HashMap<WireId, Repr>,
    modulus: Repr,
    instance_queue: VecDeque<Repr>,
    witness_queue: VecDeque<Repr>,

    // name => (output_count, input_count, instance_count, witness_count, subcircuit)
    known_functions: HashMap<String, Vec<Gate>>,

    // use to allocate temporary wires if required.
    free_local_wire :WireId,

    verified_at_least_one_gate: bool,
    found_error: Option<String>,
}

impl Default for Evaluator {
    fn default() -> Self {
        Evaluator {
            values : Default::default(),
            modulus: Default::default(),
            instance_queue: Default::default(),
            witness_queue: Default::default(),

            // name => (output_count, input_count, instance_count, witness_count, subcircuit)
            known_functions: Default::default(),

            // use to allocate temporary wires if required.
            free_local_wire : 1u64<<32,

            verified_at_least_one_gate: Default::default(),
            found_error: Default::default(),
        }
    }
}

impl Evaluator {
    pub fn from_messages(messages: impl Iterator<Item = Result<Message>>) -> Self {
        let mut evaluator = Evaluator::default();
        messages.for_each(|msg| evaluator.ingest_message(&msg.unwrap()));
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
        if self.found_error.is_some() {
            return;
        }

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
        self.ingest_header(&relation.header)?;

        if relation.gates.len() > 0 {
            self.verified_at_least_one_gate = true;
        }

        for f in relation.functions.iter() {
            self.known_functions.insert(f.name.clone(), f.body.clone());
        }


        for gate in &relation.gates {
            self.ingest_gate(gate)?;
        }
        Ok(())
    }

    fn ingest_gate(&mut self, gate: &Gate) -> Result<()> {
        use Gate::*;

        match gate {
            Constant(out, value) => self.set_encoded(*out, value),

            AssertZero(inp) => {
                let val = self.get(*inp)?;
                if !val.is_zero() {
                    return Err(
                        format!("wire_{} should equal 0 but has value {}", *inp, val).into(),
                    );
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
                let not = if val.is_zero() {
                    BigUint::one()
                } else {
                    BigUint::zero()
                };
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

            Free(first, last) => {
                let last_value = last.unwrap_or(*first);
                for current in *first..=last_value {
                    self.remove(current)?;
                }
            }

            Call(name, output_wires, input_wires) => {
                let subcircuit= self.known_functions.get(name).cloned().ok_or("Unknown function")?;
                let expanded_output = expand_wirelist(output_wires);
                let expanded_input = expand_wirelist(input_wires);
                self.ingest_subcircuit(&subcircuit, &expanded_output, &expanded_input)?;
            }

            AnonCall(output_wires, input_wires, _, _, subcircuit) => {
                let expanded_output = expand_wirelist(output_wires);
                let expanded_input = expand_wirelist(input_wires);
                self.ingest_subcircuit(subcircuit, &expanded_output, &expanded_input)?;
            }

            Switch(condition, output_wires, cases, branches) => {

                let mut selected  :bool = false;

                for (case, branch) in cases.iter().zip(branches.iter()) {
                    if self.get(*condition).ok() == Some(&Repr::from_bytes_le(case)) {
                        selected = true;
                        match branch {
                            CaseInvoke::AbstractGateCall(name, inputs) => self.ingest_gate(&Call(name.clone(), output_wires.clone(), inputs.clone()))?,
                            CaseInvoke::AbstractAnonCall(input_wires, instance_count, witness_count, subcircuit) => {
                                self.ingest_gate(&AnonCall(output_wires.clone(), input_wires.clone(), *instance_count, *witness_count, subcircuit.clone()))?
                            }
                        }
                    }
                }

                if !selected {
                    return Err(
                        format!("wire_{} value does not match any of the cases", *condition).into(),
                    );
                }
            }
/*
            For(
                start_val,
                end_val,
                _, _,
                output_mapping,
                input_mapping,
                body
            ) => {
                for i in *start_val..=*end_val {
                    let output_wires= expand_wire_mappings(output_mapping, i);
                    let input_wires= expand_wire_mappings(input_mapping, i);
                    self.ingest_subcircuit(body, &output_wires, &input_wires)?;
                }
            },
 */
        }
        Ok(())
    }

    fn set_encoded(&mut self, id: WireId, encoded: &[u8]) {
        self.set(id, BigUint::from_bytes_le(encoded));
    }

    fn set(&mut self, id: WireId, mut value: Repr) {
        value %= &self.modulus;
        self.values.insert(id, value);
    }

    pub fn get(&self, id: WireId) -> Result<&Repr> {
        self.values
            .get(&id)
            .ok_or(format!("No value given for wire_{}", id).into())
    }

    fn remove(&mut self, id: WireId) -> Result<Repr> {
        self.values
            .remove(&id)
            .ok_or(format!("No value given for wire_{}", id).into())
    }

    /// This function will evaluate all the gates in the subcircuit, applying a translation to each
    /// relative to the current workspace.
    /// It will also consume instance and witnesses whenever required.
    fn ingest_subcircuit(&mut self, subcircuit: &[Gate], output_wires: &[WireId], input_wires: &[WireId]) -> Result<()> {
        let output_input_wires = [output_wires, input_wires].concat();

        let mut free_local_wire = self.free_local_wire;
        for gate in translate_gates(subcircuit, &output_input_wires, &mut free_local_wire) {
            self.ingest_gate(&gate)?;
        }
        self.free_local_wire = free_local_wire;

        Ok(())
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

    assert_eq!(simulator.get_violations().len(), 0);

    Ok(())
}

#[test]
fn test_switch() -> Result<()> {
    use Gate::*;
    use crate::producers::examples::*;
    use crate::structs::relation::{ARITH, SWITCH};

    let instance = example_instance();
    let witness = example_witness();
    let relation =
        Relation {
            header: example_header(),
            gate_mask: ARITH,
            feat_mask: SWITCH,
            gates: vec![
                Witness(1),
                Switch(1,                     // condition
                    vec![0, 2, 4, 5, 6],      // output wires
                    vec![1],                  // input wires
                    1, 1,                     // instance_count, witness_count
                    vec![vec![3], vec![5]],   // cases
                    vec![                     // branches
                        vec![                 // case 3
                            Instance(0),
                            Witness(1),
                            Mul(2, 5, 5),
                            Mul(3, 1, 1),
                            Add(4, 2, 3),
                        ],
                        vec![                 // case 5
                            Instance(0),
                            Witness(2),
                            Mul(1, 5, 0),
                            Mul(3, 1, 2),
                            Add(4, 2, 3),
                        ],
                    ],
                ),
                Constant(3, encode_negative_one(&example_header())), // -1
                Mul(7, 3, 0),                                              // - instance_0
                Add(8, 6, 7),                                              // sum - instance_0
                Free(0, Some(7)),                                          // Free all previous wires
                AssertZero(8),                                             // difference == 0
            ],
        };


    let mut simulator = Evaluator::default();
    simulator.ingest_instance(&instance)?;
    simulator.ingest_witness(&witness)?;
    simulator.ingest_relation(&relation)?;

    assert_eq!(simulator.get_violations().len(), 0);

    Ok(())
}
