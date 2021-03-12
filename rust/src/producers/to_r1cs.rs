use num_bigint::BigUint;
use std::ops::Sub;

use crate::structs::assignment::Assignment;
use crate::structs::{Value, WireId};
use crate::Gate::*;
use crate::{Gate, Header, Instance, Relation, Witness};

use zkinterface::BilinearConstraint;
use zkinterface::CircuitHeader as zkiCircuitHeader;
use zkinterface::ConstraintSystem as zkiConstraintSystem;
use zkinterface::Variables as zkiVariables;
use zkinterface::Witness as zkiWitness;

use std::collections::HashMap;

// swapped from from_r1cs.rs
// field maximum is one less than field characteristic
pub fn extract_field_maximum(header: &Header) -> BigUint {
    let mut fm = BigUint::from_bytes_le(&header.field_characteristic);
    let one: u8 = 1;
    fm = fm.sub(one);
    return fm;
}

// pad a little-endian vector of value out to len
// takes vec by value intentionally so ownership can be returned if same length
pub fn pad_le_u8_vec(v: Value, len: usize) -> Value {
    let vlen = v.len();
    assert!(vlen <= len);
    match vlen.cmp(&len) {
        std::cmp::Ordering::Equal => v.to_vec(),
        // pad as little-endian: add zeroes after set values
        std::cmp::Ordering::Less => [v, vec![0; len - vlen]].concat(),
        // impossible to pad to a shorter length
        std::cmp::Ordering::Greater => panic!(),
    }
}

//instance variables: values explicitly set for a wire, not as R1CS constraints
// return the zkInterface Variables struct, plus the free_variable_id
pub fn combine_variables(assignments: &[Assignment]) -> zkiVariables {
    let mut ids: Vec<u64> = vec![];
    let mut values: Vec<u8> = vec![];

    let maxlen = assignments
        .iter()
        .fold(0, |acc, a| std::cmp::max(acc, a.value.len()));

    for a in assignments {
        ids.push(a.id);
        values.extend(pad_le_u8_vec(a.value.to_vec(), maxlen));
    }

    zkiVariables {
        variable_ids: ids,
        values: match values.len() {
            0 => None,
            _ => Some(values),
        },
    }
}

pub fn make_combination(ids: Vec<u64>, coefficients: Vec<u8>) -> zkiVariables {
    zkiVariables {
        variable_ids: ids,
        values: Some(coefficients),
    }
}

pub fn padded_concat(a: &Value, b: &Value) -> Vec<u8> {
    let alen = a.len();
    let blen = b.len();
    // shadow with owned explicit copy - just referencing would try to move vec out
    let a = a.to_vec();
    let b = b.to_vec();
    match alen.cmp(&blen) {
        std::cmp::Ordering::Equal => [a, b].concat(),
        // pad as little-endian: add zeroes after set values
        std::cmp::Ordering::Less => [a, vec![0; blen - alen], b].concat(),
        std::cmp::Ordering::Greater => [a, b, vec![0; alen - blen]].concat(),
    }
}

pub fn gate_to_simple_constraint(gate: &Gate) -> BilinearConstraint {
    let (a, b, c) = match gate {
        // Note: Constant gate seems to be eclipsed by AddConstant and MulConstant - what are they for again?
        Constant(_, _) => panic!("Cannot create simple constraint for constant!"),
        Copy(out, input) => (
            (vec![*input], vec![1]),
            (vec![0], vec![1]),
            (vec![*out], vec![1]),
        ),
        Add(out, x, y) => (
            (vec![*x, *y], vec![1, 1]),
            (vec![0], vec![1]),
            (vec![*out], vec![1]),
        ),
        Mul(out, x, y) => (
            (vec![*x], vec![1]),
            (vec![*y], vec![1]),
            (vec![*out], vec![1]),
        ),
        AddConstant(out, x, value) => (
            (vec![*x, 0], padded_concat(&vec![1], value)),
            (vec![0], vec![1]),
            (vec![*out], vec![1]),
        ),
        MulConstant(out, x, value) => (
            (vec![*x], vec![1]),
            (vec![0], value.to_vec()),
            (vec![*out], vec![1]),
        ),

        // And, Xor, Not all for boolean only (prime field {0, 1})
        And(out, x, y) => (
            // gate copied from Mul, because 0/1 multiplication is equivalent to And
            (vec![*x], vec![1]),
            (vec![*y], vec![1]),
            (vec![*out], vec![1]),
        ),
        Xor(out, x, y) => (
            // unsigned int field on {0, 1}; add together for 1 if different, or 0+0 = 1+1 mod 2
            // so, gate copied from Add
            (vec![*x, *y], vec![1, 1]),
            (vec![0], vec![1]),
            (vec![*out], vec![1]),
        ),
        Not(out, x) => (
            // unsigned int field on {0, 1}; adding 1 flips value
            (vec![*x, 0], vec![1, 1]),
            (vec![0], vec![1]),
            (vec![*out], vec![1]),
        ),

        AssertZero(x) => ((vec![*x], vec![1]), (vec![0], vec![1]), (vec![0], vec![0])),
    };
    BilinearConstraint {
        linear_combination_a: make_combination(a.0, a.1),
        linear_combination_b: make_combination(b.0, b.1),
        linear_combination_c: make_combination(c.0, c.1),
    }
}

// must convert Constant gates into coefficients for r1cs
#[derive(Default)]
pub struct GateConverter {
    pub constraints: Vec<BilinearConstraint>,
    pub constant_values: HashMap<WireId, Value>,
    pub all_gates: Vec<Gate>,
}

impl GateConverter {
    pub fn add_gate(&mut self, gate: &Gate) {
        match gate {
            // If the gate is a Constant, record the constant value; we must substitute, because R1CS only has
            // a single wire id (0) for constants.
            Constant(w, v) => {
                self.constant_values.insert(*w, v.to_vec());
            }

            // When converting to r1cs, we assume no gate has only constants as inputs.
            // However, there may be a constant wire from the Constant gate type.
            // In those cases, instead create an AddConstant or MulConstant gate.
            // Also, we record constant add/mul gates to populate the witness.
            Add(out, x, y) => {
                if let Some(val) = self.constant_values.get(x) {
                    let cg = AddConstant(*out, *y, val.to_vec());
                    self.constraints.push(gate_to_simple_constraint(&cg));
                    self.all_gates.push(cg);
                } else if let Some(val) = self.constant_values.get(y) {
                    let cg = AddConstant(*out, *x, val.to_vec());
                    self.constraints.push(gate_to_simple_constraint(&cg));
                    self.all_gates.push(cg);
                } else {
                    self.constraints.push(gate_to_simple_constraint(gate));
                    self.all_gates.push(gate.clone());
                }
            }

            Mul(out, x, y) => {
                if let Some(val) = self.constant_values.get(x) {
                    let cg = MulConstant(*out, *y, val.to_vec());
                    self.constraints.push(gate_to_simple_constraint(&cg));
                    self.all_gates.push(cg);
                } else if let Some(val) = self.constant_values.get(y) {
                    let cg = MulConstant(*out, *x, val.to_vec());
                    self.constraints.push(gate_to_simple_constraint(&cg));
                    self.all_gates.push(cg);
                } else {
                    self.constraints.push(gate_to_simple_constraint(gate));
                    self.all_gates.push(gate.clone());
                }
            }

            _ => {
                self.constraints.push(gate_to_simple_constraint(gate));
                self.all_gates.push(gate.clone());
            },
        }
    }
}

pub fn to_r1cs(
    instance: &Instance,
    relation: &Relation,
) -> (zkiCircuitHeader, zkiConstraintSystem, Vec<Gate>) {
    assert_eq!(instance.header, relation.header);
    let header = &instance.header;
    let assignments = &instance.common_inputs;

    let fm = extract_field_maximum(&header);
    let instance_variables = combine_variables(&assignments);

    // Remember that max returns a reference! Deref so no double borrow
    let var_max_id = *instance_variables.variable_ids.iter().max().unwrap();

    // Output wire is always greatest id of a gate; find the max
    let gate_max_id = relation
        .gates
        .iter()
        .fold(0, |acc, gate| match gate.get_output_wire_id() {
            Some(w) => std::cmp::max(acc, w),
            None => acc,
        });
    // Free variable is one more than all assigned wire ids
    let free_variable_id = std::cmp::max(var_max_id, gate_max_id) + 1;

    // must set in header: vec of instance variables, free variable id, field maximum
    // leaving configuration as None is fine
    let zki_header = zkiCircuitHeader {
        instance_variables,
        free_variable_id,
        field_maximum: Some(fm.to_bytes_le()),
        ..zkiCircuitHeader::default()
    };

    let mut gc = GateConverter::default();

    for g in &relation.gates {
        gc.add_gate(g);
    }

    let cs = zkiConstraintSystem {
        constraints: gc.constraints,
    };

    println!("Header: {:?}", zki_header);
    println!("Constants: {:?}", gc.constant_values);
    println!("All gates: {:?}", relation.gates);

    (zki_header, cs, gc.all_gates)
}

// Conversion works the same for the witness as for the I/O variables,
// but gates that can be inferred in the IR must have their values computed explicitly for the witness here.
// To do this inference, we need all of the gates (with constants removed by to_r1cs) and the field size.
pub fn to_r1cs_completed_witness(witness: &Witness, common_variables: &Vec<Assignment>, all_gates: &Vec<Gate>, field_characteristic: &BigUint) -> zkiWitness {
    let to_map = |x : &Vec<Assignment>| x.iter()
        .map(|a| (a.id, BigUint::from_bytes_le(&a.value)))
        .collect();

    let mut witness_assignments: HashMap<WireId, BigUint> = to_map(&witness.short_witness);
    let mut all_assignments: HashMap<WireId, BigUint> = to_map(&common_variables);
    all_assignments.extend(witness_assignments.clone());

    // AssertZero adds no wires, so don't check it - this allows unwrap of output wire ID
    // Similarly, Constant gates have been removed
    let mut all_gates : Vec<Gate> = all_gates.iter().filter(|g| match g {
        AssertZero(_) => false,
        Constant(_,_) => false,
        _ => true,
    }).map(|g| g.clone()).collect();
    all_gates.sort_by(|a, b| a.get_output_wire_id().unwrap().cmp(&b.get_output_wire_id().unwrap()));

    println!("All assignments: {:?}", all_assignments);
    println!("All gates: {:?}", all_gates);

    for gate in all_gates {
        println!("Gate: {:?}", gate);
        if all_assignments.contains_key(&gate.get_output_wire_id().unwrap()) {
            // don't compute if a value already exists in the map
            continue;
        }
        match gate {
            Copy(out, x) => {
                let xval = all_assignments.get(&x).unwrap().clone();
                all_assignments.insert(out, xval.clone());
                witness_assignments.insert(out, xval);
            },
    
            Add(out, x, y) => {
                let xval = all_assignments.get(&x).unwrap();
                let yval = all_assignments.get(&y).unwrap();
                let oval = (xval + yval) % field_characteristic;
                all_assignments.insert(out, oval.clone());
                witness_assignments.insert(out, oval);
            },
            Mul(out, x, y) => {
                let xval = all_assignments.get(&x).unwrap();
                let yval = all_assignments.get(&y).unwrap();
                let oval = (xval * yval) % field_characteristic;
                all_assignments.insert(out, oval.clone());
                witness_assignments.insert(out, oval);
            },

            AddConstant(out, x, value) => {
                let xval = all_assignments.get(&x).unwrap();
                let cval = BigUint::from_bytes_le(&value);
                let oval = (xval + cval) % field_characteristic;
                all_assignments.insert(out, oval.clone());
                witness_assignments.insert(out, oval);
            },
            MulConstant(out, x, value) => {
                let xval = all_assignments.get(&x).unwrap();
                let cval = BigUint::from_bytes_le(&value);
                let oval = (xval * cval) % field_characteristic;
                all_assignments.insert(out, oval.clone());
                witness_assignments.insert(out, oval);
            }, 

            And(out, x, y) => {
                let xval = all_assignments.get(&x).unwrap();
                let yval = all_assignments.get(&y).unwrap();
                let oval = xval & yval;
                all_assignments.insert(out, oval.clone());
                witness_assignments.insert(out, oval);
            },    
            Xor(out, x, y) => {
                let xval = all_assignments.get(&x).unwrap();
                let yval = all_assignments.get(&y).unwrap();
                let oval = xval ^ yval;
                all_assignments.insert(out, oval.clone());
                witness_assignments.insert(out, oval);
            },
            Not(out, x) => {
                let xval = all_assignments.get(&x).unwrap();
                let oval : BigUint = xval ^ BigUint::from_bytes_le(b"1");
                all_assignments.insert(out, oval.clone());
                witness_assignments.insert(out, oval);
            },

            AssertZero(_) => panic!("all_gates must have filtered out AssertZero gates!"),
            Constant(_,_) => panic!("all_gates must have filtered out Constant gates!"),
        }
    }

    let assignment_vec: Vec<Assignment> = witness_assignments
        .iter()
        .map(|(id, value)| Assignment {
            id: *id,
            value: value.to_bytes_le(),
        })
        .collect();
    let assigned_variables = combine_variables(&assignment_vec);
    zkiWitness { assigned_variables }
}

#[cfg(test)]
fn assert_same_io_values(instance: &Instance, zki_header: &zkiCircuitHeader) -> crate::Result<()> {
    let (converted_vars, _) = crate::producers::from_r1cs::zki_variables_to_vec_assignment(
        &zki_header.instance_variables,
    );
    let r1cs_vals: HashMap<WireId, BigUint> = converted_vars
        .iter()
        .map(|x| (x.id, BigUint::from_bytes_le(&x.value)))
        .collect();

    for a in instance.common_inputs.iter() {
        let aval = BigUint::from_bytes_le(&a.value);
        let bval = r1cs_vals.get(&a.id).clone().unwrap();
        assert_eq!(aval, *bval);
    }

    Ok(())
}

#[cfg(test)]
fn assert_same_witness_values(witness: &Witness, zki_witness: &zkiWitness) -> crate::Result<()> {
    let (converted_vars, _) = crate::producers::from_r1cs::zki_variables_to_vec_assignment(
        &zki_witness.assigned_variables,
    );
    let r1cs_vals: std::collections::HashMap<crate::structs::WireId, BigUint> = converted_vars
        .iter()
        .map(|x| (x.id, BigUint::from_bytes_le(&x.value)))
        .collect();

    for a in witness.short_witness.iter() {
        let aval = BigUint::from_bytes_le(&a.value);
        let bval = r1cs_vals.get(&a.id).clone().unwrap();
        assert_eq!(aval, *bval);
    }

    Ok(())
}

#[test]
fn test_same_values_after_conversion() -> crate::Result<()> {
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;
    use zkinterface::producers::examples::example_constraints as zki_example_constraints;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;

    // begin tests as with from_r1cs

    let zki_header = zki_example_header_inputs(3, 4, 25);
    let zki_witness = zki_example_witness_inputs(3, 4);
    let zki_r1cs = zki_example_constraints();

    let (instance, relation) = crate::producers::from_r1cs::to_ir(&zki_header, &zki_r1cs);
    let witness = crate::producers::from_r1cs::to_witness(&zki_header, &zki_witness);

    // now convert back into r1cs (there)
    let (zki_header2, _, all_gates) = to_r1cs(&instance, &relation);
    let zki_witness2 = to_r1cs_completed_witness(&witness, &instance.common_inputs, &all_gates, &BigUint::from_bytes_le(&relation.header.field_characteristic));

    assert_same_io_values(&instance, &zki_header2)?;

    assert_same_witness_values(&witness, &zki_witness2)?;

    Ok(())
}

#[test]
fn test_with_validate() -> crate::Result<()> {
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;
    use zkinterface::producers::examples::example_constraints as zki_example_constraints;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;

    // begin tests as with from_r1cs

    let zki_header = zki_example_header_inputs(3, 4, 25);
    let zki_witness = zki_example_witness_inputs(3, 4);
    let zki_r1cs = zki_example_constraints();

    let (instance, relation) = crate::producers::from_r1cs::to_ir(&zki_header, &zki_r1cs);
    let witness = crate::producers::from_r1cs::to_witness(&zki_header, &zki_witness);

    // now convert back into r1cs (there)
    let (zki_header2, zki_r1cs2, all_gates) = to_r1cs(&instance, &relation);
    let zki_witness2 = to_r1cs_completed_witness(&witness, &instance.common_inputs, &all_gates, &BigUint::from_bytes_le(&relation.header.field_characteristic));

    let mut validator = zkinterface::consumers::validator::Validator::new_as_prover();
    validator.ingest_constraint_system(&zki_r1cs2);
    validator.ingest_witness(&zki_witness2);
    validator.ingest_header(&zki_header2);

    let violations = validator.get_violations();
    if violations.len() > 0 {
        eprintln!("Violations:\n- {}\n", violations.join("\n- "));
    }

    Ok(())
}

#[test]
fn test_with_simulator() -> crate::Result<()> {
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;
    use zkinterface::producers::examples::example_constraints as zki_example_constraints;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;
    // begin tests as with from_r1cs

    let zki_header = zki_example_header_inputs(3, 4, 25);
    let zki_witness = zki_example_witness_inputs(3, 4);
    let zki_r1cs = zki_example_constraints();

    let (instance, relation) = crate::producers::from_r1cs::to_ir(&zki_header, &zki_r1cs);
    let witness = crate::producers::from_r1cs::to_witness(&zki_header, &zki_witness);

    // now convert back into r1cs (there)
    let (zki_header2, zki_r1cs2, all_gates) = to_r1cs(&instance, &relation);
    let zki_witness2 = to_r1cs_completed_witness(&witness, &instance.common_inputs, &all_gates, &BigUint::from_bytes_le(&relation.header.field_characteristic));

    let mut simulator = zkinterface::consumers::simulator::Simulator::default();
    simulator.ingest_header(&zki_header2)?;
    simulator.ingest_witness(&zki_witness2)?;
    simulator.ingest_constraint_system(&zki_r1cs2)?;

    Ok(())
}
