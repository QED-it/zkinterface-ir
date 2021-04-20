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

use std::collections::btree_map::Entry;
use std::collections::BTreeMap;

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

pub fn pad_to_max(vals: &[&Value]) -> Vec<u8> {
    let max_len = vals.iter().map(|v| v.len()).max().unwrap();
    // shadow with owned explicit copy - just referencing would try to move vec out
    
    let mut all_padded : Vec<Vec<u8>> = vec![];
    for v in vals {
        let vlen = v.len();
        let padded = match v.len().cmp(&max_len) {
            std::cmp::Ordering::Equal => v.to_vec(),
            // pad as little-endian: add zeroes after set values
            std::cmp::Ordering::Less => [v.to_vec(), vec![0; max_len - vlen]].concat(),
            std::cmp::Ordering::Greater => panic!("Value {:?} has max length greater than itself?", v),
        };
        all_padded.push(padded);
    }
    return all_padded.concat();
}

// must convert Constant gates into coefficients for r1cs
// also must perform modular reduction (libsnark for R1CS doesn't use field_maximum to set modulus);
// addition is folded into same gate, but multiplication needs a second layer to add/subtract a multiple
#[derive(Default)]
pub struct GateConverter {
    pub constraints: Vec<BilinearConstraint>,
    pub field_characteristic: Vec<u8>,
    pub new_modulus: Option<Vec<u8>>,

    // map of original wire ID of Constant() gate to the value
    pub constant_values: BTreeMap<WireId, Value>,

    // map of original wire ID to new wire ID
    pub shifted_wire_ids: BTreeMap<WireId, WireId>,

    // map of new output wire ID to modular-unreduced output wire ID, if different
    pub mod_unreduced_outputs: BTreeMap<WireId, WireId>,

    // map of new output wire ID to modular correction factor variable ID, such that:
    // (output % field size) + (correction * field size) = output
    // to perform division with remainder and bind a variable to (output % field size)
    pub mod_correction_wire: BTreeMap<WireId, WireId>,

    pub all_gates: Vec<Gate>,
    pub free_variable_id: WireId,
}

impl GateConverter {

    // swapped from from_r1cs.rs
    // field maximum is one less than field characteristic
    pub fn extract_field_maximum(&mut self, header: &Header) -> BigUint {
        self.field_characteristic = header.field_characteristic.to_vec();
        let mut fm = BigUint::from_bytes_le(&header.field_characteristic);
        let one: u8 = 1;
        fm = fm.sub(one);
        return fm;
    }

    pub fn append_wire(&mut self) -> WireId {
        let new_id = self.free_variable_id;
        self.free_variable_id += 1;
        return new_id
    }

    pub fn to_new_id(&mut self, id: &WireId) -> WireId {
        match self.shifted_wire_ids.entry(*id) {
            Entry::Occupied(o) => *o.get(),

            Entry::Vacant(v) => {
                // can't use append_wire() because double mutable borrow
                let new_id = self.free_variable_id;
                self.free_variable_id += 1;
                v.insert(new_id.clone());
                new_id
            }
        }
    }

    // Adds a modular reduction constraint after the new output wire.
    // Intended for after a multiplication gate, but works in other cases.
    // The gate passed should be rewritten with new wire IDs.
    pub fn add_mod_correction_constraint(&mut self, out: &WireId, unreduced: &WireId, correction: &WireId) {
        let a = (vec![*out, *correction], pad_to_max(&[&vec![1], &self.field_characteristic]));
        let b = (vec![0], vec![1]);
        let c = (vec![*unreduced], vec![1]);

        self.constraints.push(BilinearConstraint {
            linear_combination_a: make_combination(a.0, a.1),
            linear_combination_b: make_combination(b.0, b.1),
            linear_combination_c: make_combination(c.0, c.1),
        });

    }


    pub fn gate_to_simple_constraint(&mut self, gate: &Gate) {
        let (a, b, c) = match gate {
            // Note: Constant gate seems to be eclipsed by AddConstant and MulConstant
            // If a gate is added that needs constant 1 at position 0, mark as such
            Constant(_, _) => panic!("Cannot create simple constraint for constant!"),
            Copy(out, input) => (
                (vec![*input], vec![1]),
                (vec![0], vec![1]),
                (vec![*out], vec![1]),
            ),

            Add(out, x, y) => {
                let correction = self.mod_correction_wire.get(out).unwrap();
                (
                    (vec![*out, *correction], pad_to_max(&[&vec![1], &self.field_characteristic])),
                    (vec![0], vec![1]),
                    (vec![*x, *y], vec![1, 1]),
                )
            }

            Mul(out, x, y) => {
                let unreduced = self.mod_unreduced_outputs.get(out).unwrap();
                (
                    (vec![*x], vec![1]),
                    (vec![*y], vec![1]),
                    (vec![*unreduced], vec![1]),
                )
            },

            AddConstant(out, x, value) => {
                let correction = self.mod_correction_wire.get(out).unwrap();
                (
                    (vec![*out, *correction], pad_to_max(&[&vec![1], &self.field_characteristic])),
                    (vec![0], vec![1]),
                    (vec![*x, 0], pad_to_max(&[&vec![1], value])),
                )
            }

            MulConstant(out, x, value) => {
                let unreduced = self.mod_unreduced_outputs.get(out).unwrap();
                (
                    (vec![*x], vec![1]),
                    (vec![0], value.to_vec()),
                    (vec![*unreduced], vec![1]),
                )
            }

            And(_,_,_) => panic!("And should have been rewritten!"),
            Xor(_,_,_) => panic!("Xor should have been rewritten!"),
            Not(_,_) => panic!("Not should have been rewritten!"),

            AssertZero(x) => {
                (
                    (vec![*x], vec![1]), 
                    (vec![0], vec![1]), 
                    (vec![0], vec![0]),
                )
            }
        };
        self.constraints.push(BilinearConstraint {
            linear_combination_a: make_combination(a.0, a.1),
            linear_combination_b: make_combination(b.0, b.1),
            linear_combination_c: make_combination(c.0, c.1),
        });
    }


    pub fn add_gate(&mut self, gate: &Gate) {
        // First, rewrite And/Xor/Not as Mul/Add/AddConstant, because they're the same and require field of 0/1
        let rewritten_gate = match gate {
            And(out, x, y) => Mul(*out, *x, *y),
            Xor(out, x, y) => Add(*out, *x, *y),
            Not(out, x) => AddConstant(*out, *x, vec![1]),

            // If not one of the gates to rewrite, keep the same
            _ => gate.clone(),

        };

        match &rewritten_gate {
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
                if let Some(val) = self.constant_values.get(&x) {
                    let val = val.to_vec();
                    let y = self.to_new_id(y);
                    let correction_wire = self.append_wire();
                    let out = self.to_new_id(out);
                    self.mod_correction_wire.insert(out, correction_wire);
                    let cg = AddConstant(out, y, val);
                    self.gate_to_simple_constraint(&cg);
                    self.all_gates.push(cg);
                } else if let Some(val) = self.constant_values.get(&y) {
                    let val = val.to_vec();
                    let x = self.to_new_id(x);
                    let correction_wire = self.append_wire();
                    let out = self.to_new_id(out);
                    self.mod_correction_wire.insert(out, correction_wire);
                    let cg = AddConstant(out, x, val);
                    self.gate_to_simple_constraint(&cg);
                    self.all_gates.push(cg);
                } else {
                    let x = self.to_new_id(x);
                    let y = self.to_new_id(y);
                    let correction_wire = self.append_wire();
                    let out = self.to_new_id(out);
                    self.mod_correction_wire.insert(out, correction_wire);
                    let gate = Add(out, x, y);
                    self.gate_to_simple_constraint(&gate);
                    self.all_gates.push(gate);
                }
            }

            Mul(out, x, y) => {
                if let Some(val) = self.constant_values.get(&x) {
                    let val = val.to_vec();
                    let y = self.to_new_id(y);
                    let correction_wire = self.append_wire();
                    let unreduced_wire = self.append_wire();
                    let out = self.to_new_id(out);
                    self.mod_correction_wire.insert(out, correction_wire);
                    self.mod_unreduced_outputs.insert(out, unreduced_wire);
                    let cg = MulConstant(out, y, val);
                    self.gate_to_simple_constraint(&cg);
                    self.add_mod_correction_constraint(&out, &unreduced_wire, &correction_wire);
                    self.all_gates.push(cg);
                } else if let Some(val) = self.constant_values.get(&y) {
                    let val = val.to_vec();
                    let x = self.to_new_id(x);
                    let correction_wire = self.append_wire();
                    let unreduced_wire = self.append_wire();
                    let out = self.to_new_id(out);
                    self.mod_correction_wire.insert(out, correction_wire);
                    self.mod_unreduced_outputs.insert(out, unreduced_wire);
                    let cg = MulConstant(out, x, val);
                    self.gate_to_simple_constraint(&cg);
                    self.add_mod_correction_constraint(&out, &unreduced_wire, &correction_wire);
                    self.all_gates.push(cg);
                } else {
                    let x = self.to_new_id(x);
                    let y = self.to_new_id(y);
                    let correction_wire = self.append_wire();
                    let unreduced_wire = self.append_wire();
                    let out = self.to_new_id(out);
                    self.mod_correction_wire.insert(out, correction_wire);
                    self.mod_unreduced_outputs.insert(out, unreduced_wire);
                    let gate = Mul(out, x, y);
                    self.gate_to_simple_constraint(&gate);
                    self.add_mod_correction_constraint(&out, &unreduced_wire, &correction_wire);
                    self.gate_to_simple_constraint(&gate);
                    self.all_gates.push(gate);
                }
            }

            Copy(out, x) => {
                let x = self.to_new_id(x);
                let out = self.to_new_id(out);
                let gate = Copy(out, x);
                self.gate_to_simple_constraint(&gate);
                self.all_gates.push(gate);
            }

            AddConstant(out, x, value) => {
                let x = self.to_new_id(x);
                let correction_wire = self.append_wire();
                let out = self.to_new_id(out);
                self.mod_correction_wire.insert(out, correction_wire);
                let gate = AddConstant(out, x, value.to_vec());
                self.gate_to_simple_constraint(&gate);
                self.all_gates.push(gate);
            }

            MulConstant(out, x, value) => {
                let x = self.to_new_id(x);
                let correction_wire = self.append_wire();
                let unreduced_wire = self.append_wire();
                let out = self.to_new_id(out);
                self.mod_correction_wire.insert(out, correction_wire);
                self.mod_unreduced_outputs.insert(out, unreduced_wire);
                let gate = MulConstant(out, x, value.to_vec());
                self.gate_to_simple_constraint(&gate);
                self.add_mod_correction_constraint(&out, &unreduced_wire, &correction_wire);
                self.all_gates.push(gate);
            }

            And(_,_,_) => panic!("And should have been rewritten!"),
            Xor(_,_,_) => panic!("Xor should have been rewritten!"),
            Not(_,_) => panic!("Not should have been rewritten!"),

            AssertZero(x) => {
                let x = self.to_new_id(x);
                let gate = AssertZero(x);
                self.gate_to_simple_constraint(&gate);
                self.all_gates.push(gate);
            }
        }
    }

    pub fn ingest_header_and_relation(
        &mut self,
        instance: &Instance,
        relation: &Relation,
    ) -> (zkiCircuitHeader, zkiConstraintSystem) {
        assert_eq!(instance.header, relation.header);
        let header = &instance.header;

        let fm = self.extract_field_maximum(&header);

        let assignments: Vec<Assignment> = instance
            .common_inputs
            .iter()
            .map(|a| Assignment {
                id: self.to_new_id(&a.id),
                value: a.value.clone(),
            })
            .collect();

        for g in &relation.gates {
            self.add_gate(g);
        }

        let instance_variables = combine_variables(&assignments);

        // must set in header: vec of instance variables, free variable id, field maximum
        // leaving configuration as None is fine
        let zki_header = zkiCircuitHeader {
            instance_variables,
            free_variable_id: self.free_variable_id,
            field_maximum: Some(pad_le_u8_vec(fm.to_bytes_le(), 8)),
            ..zkiCircuitHeader::default()
        };

        let cs = zkiConstraintSystem {
            constraints: self.constraints.clone(),
        };

        (zki_header, cs)
    }

    // Conversion works the same for the witness as for the I/O variables,
    // but gates that can be inferred in the IR must have their values computed explicitly for the witness here.
    // To do this inference, we need all of the gates (with constants removed by to_r1cs) and the field size.
    pub fn update_witness(
        &self,
        witness: &Witness,
        common_variables: &Vec<Assignment>,
        field_characteristic: &BigUint,
    ) -> zkiWitness {
        let to_map = |x: &Vec<Assignment>| {
            x.iter()
                .map(|a| (a.id, BigUint::from_bytes_le(&a.value)))
                .collect()
        };

        let mut witness_assignments: BTreeMap<WireId, BigUint> = to_map(&witness.short_witness);
        let mut all_assignments_original: BTreeMap<WireId, BigUint> = to_map(&common_variables);
        all_assignments_original.extend(witness_assignments.clone());

        // Rewrite to new wire IDs
        let mut all_assignments: BTreeMap<WireId, BigUint> = all_assignments_original
            .into_iter()
            .map(|(k, v)| (*self.shifted_wire_ids.get(&k).unwrap(), v))
            .collect();
        witness_assignments = witness_assignments
            .into_iter()
            .map(|(k, v)| (*self.shifted_wire_ids.get(&k).unwrap(), v))
            .collect();

        // AssertZero adds no wires, so don't check it - this allows unwrap of output wire ID
        let mut all_gates: Vec<Gate> = self
            .all_gates
            .iter()
            .filter(|g| match g {
                AssertZero(_) => false,
                _ => true,
            })
            .map(|g| g.clone())
            .collect();
        all_gates.sort_by(|a, b| {
            a.get_output_wire_id()
                .unwrap()
                .cmp(&b.get_output_wire_id().unwrap())
        });

        for gate in all_gates {
            if all_assignments.contains_key(&gate.get_output_wire_id().unwrap()) {
                // don't compute if a value already exists in the map
                continue;
            }

            match gate {
                Copy(out, x) => {
                    let xval = all_assignments.get(&x).unwrap().clone();
                    all_assignments.insert(out, xval.clone());
                    witness_assignments.insert(out, xval);
                }

                Add(out, x, y) => {
                    let xval = all_assignments.get(&x).unwrap();
                    let yval = all_assignments.get(&y).unwrap();
                    let sum = xval + yval;
                    let correction = sum.clone() / field_characteristic;
                    let correction_wire = self.mod_correction_wire.get(&out).unwrap();
                    witness_assignments.insert(*correction_wire, correction);
                    let oval = sum % field_characteristic;
                    all_assignments.insert(out, oval.clone());
                    witness_assignments.insert(out, oval);
                }
                Mul(out, x, y) => {
                    let xval = all_assignments.get(&x).unwrap();
                    let yval = all_assignments.get(&y).unwrap();
                    let prod = xval * yval;
                    let correction = prod.clone() / field_characteristic;
                    let correction_wire = self.mod_correction_wire.get(&out).unwrap();
                    let unreduced_wire = self.mod_unreduced_outputs.get(&out).unwrap();
                    witness_assignments.insert(*correction_wire, correction);
                    witness_assignments.insert(*unreduced_wire, prod.clone());
                    let oval = prod % field_characteristic;
                    all_assignments.insert(out, oval.clone());
                    witness_assignments.insert(out, oval);
                }

                AddConstant(out, x, value) => {
                    let xval = all_assignments.get(&x).unwrap();
                    let cval = BigUint::from_bytes_le(&value);
                    let sum = xval + cval;
                    let correction = sum.clone() / field_characteristic;
                    let correction_wire = self.mod_correction_wire.get(&out).unwrap();
                    witness_assignments.insert(*correction_wire, correction);
                    let oval = sum % field_characteristic;
                    all_assignments.insert(out, oval.clone());
                    witness_assignments.insert(out, oval);
                }
                MulConstant(out, x, value) => {
                    let xval = all_assignments.get(&x).unwrap();
                    let cval = BigUint::from_bytes_le(&value);
                    let prod = xval * cval;
                    let correction = prod.clone() / field_characteristic;
                    let correction_wire = self.mod_correction_wire.get(&out).unwrap();
                    let unreduced_wire = self.mod_unreduced_outputs.get(&out).unwrap();
                    witness_assignments.insert(*correction_wire, correction);
                    witness_assignments.insert(*unreduced_wire, prod.clone());
                    let oval = prod % field_characteristic;
                    all_assignments.insert(out, oval.clone());
                    witness_assignments.insert(out, oval);
                }

                And(_,_,_) => panic!("And should have been rewritten!"),
                Xor(_,_,_) => panic!("Xor should have been rewritten!"),
                Not(_,_) => panic!("Not should have been rewritten!"),
                Constant(_, _) => panic!("all_gates must have filtered out Constant gates!"),
                AssertZero(_) => panic!("all_gates must have filtered out AssertZero gates!"),
            }
        }

        let mut assignment_vec: Vec<Assignment> = witness_assignments
            .iter()
            .map(|(id, value)| Assignment {
                id: *id,
                value: value.to_bytes_le(),
            })
            .collect();
        assignment_vec.sort_by(|a, b| a.id.partial_cmp(&b.id).unwrap());
        let assigned_variables = combine_variables(&assignment_vec);
        zkiWitness { assigned_variables }
    }
}

pub fn to_r1cs(
    instance: &Instance,
    relation: &Relation,
    witness: &Witness,
) -> (zkiCircuitHeader, zkiConstraintSystem, zkiWitness) {
    let mut gc = GateConverter {
        free_variable_id: 1, //skip 0 for constant
        ..GateConverter::default()
    };

    // reserve wire ID maps for common inputs, then witness inputs
    for a in instance
        .common_inputs
        .iter()
        .chain(witness.short_witness.iter())
    {
        gc.to_new_id(&a.id);
    }

    let (zki_header, zki_r1cs) = gc.ingest_header_and_relation(&instance, &relation);

    let zki_witness = gc.update_witness(
        &witness,
        &instance.common_inputs,
        &BigUint::from_bytes_le(&relation.header.field_characteristic),
    );

    (zki_header, zki_r1cs, zki_witness)
}

#[cfg(test)]
fn assert_same_io_values(instance: &Instance, zki_header: &zkiCircuitHeader) -> crate::Result<()> {
    let converted_vars = crate::producers::from_r1cs::zki_variables_to_vec_assignment(
        &zki_header.instance_variables,
    );
    let r1cs_vals: BTreeMap<WireId, BigUint> = converted_vars
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
    let converted_vars = crate::producers::from_r1cs::zki_variables_to_vec_assignment(
        &zki_witness.assigned_variables,
    );
    let r1cs_vals: std::collections::BTreeMap<crate::structs::WireId, BigUint> = converted_vars
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

    // now convert back into r1cs
    let (zki_header2, _, zki_witness2) = to_r1cs(&instance, &relation, &witness);

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

    // now convert back into r1cs
    let (zki_header2, zki_r1cs2, zki_witness2) = to_r1cs(&instance, &relation, &witness);

    let mut validator = zkinterface::consumers::validator::Validator::new_as_prover();
    validator.ingest_header(&zki_header2);
    validator.ingest_witness(&zki_witness2);
    validator.ingest_constraint_system(&zki_r1cs2);

    let violations = validator.get_violations();
    if violations.len() > 0 {
        let msg = format!("Violations:\n- {}\n", violations.join("\n- "));
        panic!(msg);
    }

    Ok(())
}

#[test]
fn test_with_validate_2() -> crate::Result<()> {
    // This time use an example in straight IR
    use crate::producers::examples::*;

    let instance = example_instance();
    let witness = example_witness();
    let relation = example_relation();

    // now convert back into r1cs
    let (zki_header, zki_r1cs, zki_witness) = to_r1cs(&instance, &relation, &witness);

    let mut validator = zkinterface::consumers::validator::Validator::new_as_prover();
    validator.ingest_header(&zki_header);
    validator.ingest_witness(&zki_witness);
    validator.ingest_constraint_system(&zki_r1cs);

    let violations = validator.get_violations();
    if violations.len() > 0 {
        let msg = format!("Violations:\n- {}\n", violations.join("\n- "));
        panic!(msg);
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

    // now convert back into r1cs
    let (zki_header2, zki_r1cs2, zki_witness2) = to_r1cs(&instance, &relation, &witness);

    let mut simulator = zkinterface::consumers::simulator::Simulator::default();
    simulator.ingest_header(&zki_header2)?;
    simulator.ingest_witness(&zki_witness2)?;
    simulator.ingest_constraint_system(&zki_r1cs2)?;

    Ok(())
}

#[test]
fn test_with_simulator_2() -> crate::Result<()> {
    // This time use an example in straight IR
    use crate::producers::examples::*;

    let instance = example_instance();
    let witness = example_witness();
    let relation = example_relation();

    // now convert back into r1cs
    let (zki_header, zki_r1cs, zki_witness) = to_r1cs(&instance, &relation, &witness);

    println!("Header: {:?}", zki_header);
    println!("Witness: {:?}", zki_witness);
    println!("Constraints: {:?}", zki_r1cs);

    let mut simulator = zkinterface::consumers::simulator::Simulator::default();
    simulator.ingest_header(&zki_header)?;
    simulator.ingest_witness(&zki_witness)?;
    simulator.ingest_constraint_system(&zki_r1cs)?;

    Ok(())
}
