use num_bigint::BigUint;
use std::ops::Sub;

use crate::structs::value::Value;
use crate::structs::WireId;
use crate::Gate::*;
use crate::{Gate, Header, Instance, Relation, Witness};
use crate::consumers::flattening::flatten_relation;

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

// variables: values explicitly set for a wire, not as R1CS constraints
// return the zkInterface Variables struct
pub fn combine_variables(assignments: &BTreeMap<WireId, Value>) -> zkiVariables {
    let mut ids: Vec<u64> = vec![];
    let mut values: Vec<u8> = vec![];

    let maxlen = assignments
        .values()
        .fold(0, |acc, a| std::cmp::max(acc, a.len()));

    for (k, v) in assignments {
        ids.push(*k);
        values.extend(pad_le_u8_vec(v.to_vec(), maxlen));
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
    pub field_characteristic: Value,

    // map of Instance() wire ID to its value
    pub instance_values: BTreeMap<WireId, Value>,

    // map of Witness() wire ID to its value
    pub witness_values: BTreeMap<WireId, Value>,

    // map of Constant() wire ID to its value
    pub constant_values: BTreeMap<WireId, Value>,

    // true if we need to perform modular reduction manually because of ignored field size
    perform_modular_reduction: bool,

    // map of output wire ID to modular correction factor variable ID, such that:
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
                match self.perform_modular_reduction {
                    true => {
                    let correction = self.mod_correction_wire.get(out).unwrap();
                        (
                            (vec![*out, *correction], pad_to_max(&[&vec![1], &self.field_characteristic])),
                            (vec![0], vec![1]),
                            (vec![*x, *y], vec![1, 1]),
                    )},
                    
                    false => {(
                        (vec![*out], vec![1]),
                        (vec![0], vec![1]),
                        (vec![*x, *y], vec![1, 1]),
                    )},
                }
            }

            Mul(out, x, y) => {
                match self.perform_modular_reduction {
                    true => {
                        let correction = self.mod_correction_wire.get(out).unwrap();
                        (
                            (vec![*x], vec![1]),
                            (vec![*y], vec![1]),
                            (vec![*out, *correction], pad_to_max(&[&vec![1], &self.field_characteristic])),
                    )},
                    
                    false => {(
                        (vec![*x], vec![1]),
                        (vec![*y], vec![1]),
                        (vec![*out], vec![1]),
                    )},
                }
            },

            AddConstant(out, x, value) => {
                match self.perform_modular_reduction {
                    true => {
                    let correction = self.mod_correction_wire.get(out).unwrap();
                    (
                        (vec![*out, *correction], pad_to_max(&[&vec![1], &self.field_characteristic])),
                        (vec![0], vec![1]),
                        (vec![*x, 0], pad_to_max(&[&vec![1], value])),
                    )},
                    
                    false => {(
                        (vec![*out], vec![1]),
                        (vec![0], vec![1]),
                        (vec![*x, 0], pad_to_max(&[&vec![1], value])),
                    )},
                }
            }

            MulConstant(out, x, value) => {
                match self.perform_modular_reduction {
                    true => {
                        let correction = self.mod_correction_wire.get(out).unwrap();
                        (
                            (vec![*x], vec![1]),
                            (vec![0], value.to_vec()),
                            (vec![*out, *correction], pad_to_max(&[&vec![1], &self.field_characteristic])),
                    )},
                    
                    false => {(
                        (vec![*x], vec![1]),
                        (vec![0], value.to_vec()),
                        (vec![*out], vec![1]),
                    )},
                }
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
            Gate::Instance(_) => panic!("Instance gate should have been removed!"),
            Gate::Witness(_) => panic!("Witness gate should have been removed!"),
            Free(_, _) => panic!("Free should have been removed!"),

            _ => panic!("Not yet supported: {:?}", gate),
        };
        self.constraints.push(BilinearConstraint {
            linear_combination_a: make_combination(a.0, a.1),
            linear_combination_b: make_combination(b.0, b.1),
            linear_combination_c: make_combination(c.0, c.1),
        });
    }


    pub fn add_gate(&mut self, gate: &Gate) {
        // First, rewrite And/Xor/Not as Mul/Add/AddConstant, because they're the same when requiring field of 0/1
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
                if self.perform_modular_reduction {
                    let correction_wire = self.append_wire();
                    self.mod_correction_wire.insert(*out, correction_wire);
                }
                if let Some(val) = self.constant_values.get(x) {
                    let val = val.to_vec();
                    let cg = AddConstant(*out, *y, val);
                    self.gate_to_simple_constraint(&cg);
                    self.all_gates.push(cg);
                } else if let Some(val) = self.constant_values.get(y) {
                    let val = val.to_vec();
                    let cg = AddConstant(*out, *x, val);
                    self.gate_to_simple_constraint(&cg);
                    self.all_gates.push(cg);
                } else {
                    self.gate_to_simple_constraint(&rewritten_gate);
                    self.all_gates.push(rewritten_gate);
                }
            }

            Mul(out, x, y) => {
                if self.perform_modular_reduction {
                    let correction_wire = self.append_wire();
                    self.mod_correction_wire.insert(*out, correction_wire);
                }
                if let Some(val) = self.constant_values.get(x) {
                    let val = val.to_vec();
                    let cg = MulConstant(*out, *y, val);
                    self.gate_to_simple_constraint(&cg);
                    self.all_gates.push(cg);
                } else if let Some(val) = self.constant_values.get(y) {
                    let val = val.to_vec();
                    let cg = MulConstant(*out, *x, val);
                    self.gate_to_simple_constraint(&cg);
                    self.all_gates.push(cg);
                } else {
                    self.gate_to_simple_constraint(&rewritten_gate);
                    self.all_gates.push(rewritten_gate);
                }
            }

            Copy(out, x) => {
                let gate = Copy(*out, *x);
                self.gate_to_simple_constraint(&gate);
                self.all_gates.push(gate);
            }

            AddConstant(out, x, value) => {
                if self.perform_modular_reduction {
                    let correction_wire = self.append_wire();
                    self.mod_correction_wire.insert(*out, correction_wire);
                }
                let gate = AddConstant(*out, *x, value.to_vec());
                self.gate_to_simple_constraint(&gate);
                self.all_gates.push(gate);
            }

            MulConstant(out, x, value) => {
                if self.perform_modular_reduction {
                    let correction_wire = self.append_wire();
                    self.mod_correction_wire.insert(*out, correction_wire);
                }

                let gate = MulConstant(*out, *x, value.to_vec());
                self.gate_to_simple_constraint(&gate);
                self.all_gates.push(gate);
            }

            And(_,_,_) => panic!("And should have been rewritten!"),
            Xor(_,_,_) => panic!("Xor should have been rewritten!"),
            Not(_,_) => panic!("Not should have been rewritten!"),

            Instance(_) => panic!("Instance should have been removed before relations!"),
            Witness(_) => panic!("Witness should have been removed before relations!"),
            Free(_,_) => panic!("Free should have been removed during wire deconfliction!"),

            AssertZero(x) => {
                let gate = AssertZero(*x);
                self.gate_to_simple_constraint(&gate);
                self.all_gates.push(gate);
            }

            _ => panic!("Not yet supported: {:?}", rewritten_gate),

        }
    }

    pub fn build_header_and_relation(
        &mut self,
        header: &Header,
        gates: &Vec<Gate>,
    ) -> (zkiCircuitHeader, zkiConstraintSystem) {
        let fm = self.extract_field_maximum(&header);

        let assignments: BTreeMap<WireId, Value> = self.instance_values.clone();

        for g in gates {
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
    // To do this inference, we need all of the gates (with constants/inputs/witness values removed by to_r1cs) 
    // and the field size.
    pub fn update_witness(
        &self,
        field_characteristic: &BigUint,
    ) -> zkiWitness {
        let to_map = |x: &BTreeMap<WireId, Value>| {
            x.iter()
                .map(|(k, v)| (*k, BigUint::from_bytes_le(v)))
                .collect()
        };

        let mut witness_assignments: BTreeMap<WireId, BigUint> = to_map(&self.witness_values);
        let mut all_assignments: BTreeMap<WireId, BigUint> = to_map(&self.instance_values);
        all_assignments.extend(witness_assignments.clone());

        // AssertZero adds no wires, so don't check it - this allows unwrap of output wire ID
        let all_gates: Vec<Gate> = self
            .all_gates
            .iter()
            .filter(|g| match g {
                AssertZero(_) => false,
                _ => true,
            })
            .map(|g| g.clone())
            .collect();

        for gate in all_gates {

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
                    if self.perform_modular_reduction {
                        let correction = sum.clone() / field_characteristic;
                        let correction_wire = self.mod_correction_wire.get(&out).unwrap();
                        witness_assignments.insert(*correction_wire, correction);
                    }
                    let oval = sum % field_characteristic;
                    all_assignments.insert(out, oval.clone());
                    witness_assignments.insert(out, oval);
                }
                Mul(out, x, y) => {
                    let xval = all_assignments.get(&x).unwrap();
                    let yval = all_assignments.get(&y).unwrap();
                    let prod = xval * yval;
                    if self.perform_modular_reduction {
                        let correction = prod.clone() / field_characteristic;
                        let correction_wire = self.mod_correction_wire.get(&out).unwrap();
                        witness_assignments.insert(*correction_wire, correction);
                    }
                    let oval = prod % field_characteristic;
                    all_assignments.insert(out, oval.clone());
                    witness_assignments.insert(out, oval);
                }

                AddConstant(out, x, value) => {
                    let xval = all_assignments.get(&x).unwrap();
                    let cval = BigUint::from_bytes_le(&value);
                    let sum = xval + cval;
                    if self.perform_modular_reduction {
                        let correction = sum.clone() / field_characteristic;
                        let correction_wire = self.mod_correction_wire.get(&out).unwrap();
                        witness_assignments.insert(*correction_wire, correction);
                    }
                    let oval = sum % field_characteristic;
                    all_assignments.insert(out, oval.clone());
                    witness_assignments.insert(out, oval);
                }
                MulConstant(out, x, value) => {
                    let xval = all_assignments.get(&x).unwrap();
                    let cval = BigUint::from_bytes_le(&value);
                    let prod = xval * cval;
                    if self.perform_modular_reduction {
                        let correction = prod.clone() / field_characteristic;
                        let correction_wire = self.mod_correction_wire.get(&out).unwrap();
                        witness_assignments.insert(*correction_wire, correction);
                    }
                    let oval = prod % field_characteristic;
                    all_assignments.insert(out, oval.clone());
                    witness_assignments.insert(out, oval);
                }

                And(_,_,_) => panic!("And should have been rewritten!"),
                Xor(_,_,_) => panic!("Xor should have been rewritten!"),
                Not(_,_) => panic!("Not should have been rewritten!"),
                Constant(_,_) => panic!("all_gates must have filtered out Constant gates!"),
                AssertZero(_) => panic!("all_gates must have filtered out AssertZero gates!"),
                Instance(_) => panic!("Instance should have been removed before relations!"),
                Witness(_) => panic!("Witness should have been removed before relations!"),
                Free(_,_) => panic!("Free should have been removed during wire deconfliction!"),

                _ => panic!("Not yet supported: {:?}", gate),

            }
        }

        let witness_assignments: BTreeMap<WireId, Value> = witness_assignments
            .iter()
            .map(|(id, value)| (
                *id,
                value.to_bytes_le()
            ))
            .collect();
        let assigned_variables = combine_variables(&witness_assignments);
        zkiWitness { assigned_variables }
    }
}

#[derive(Default)]
struct IdMapper {
    current_id_map: BTreeMap<WireId, WireId>,
    free_variable_id: WireId,
} 

impl IdMapper {
    pub fn to_new_id(&mut self, id: &WireId) -> WireId  {
        match self.current_id_map.entry(*id) {
            Entry::Occupied(o) => *o.get(),

            Entry::Vacant(v) => {
                let new_id = self.free_variable_id;
                self.free_variable_id += 1;
                v.insert(new_id.clone());
                new_id
            }
        }
    }

    pub fn free_ids(&mut self, first: &WireId, last: &Option<WireId>) {
        match last {
            None => {
                self.current_id_map.remove(first);
            },
            Some(last) => {
                for id in *first ..= *last {
                    self.current_id_map.remove(&id);
                }
            },
        }
    }
}

// Take a vector of gates and rewrite their input/output wires. 
// The resulting rewritten gates will remove Free() and instead have new wire IDs
// where the original would reuse freed IDs.
pub fn deconflict_gate_wires(
    gates: &Vec<Gate>,
) -> (Vec<Gate>, WireId) {

    let mut map = IdMapper {
        // skip id 0 for constant value of 1
        free_variable_id: 1,
        ..IdMapper::default()
    };

    let mut ret: Vec<Gate> = Vec::new();
    for gate in gates.iter() {
        // for all but Free(), register new wires into map, apply mapping to IDs, and append the rewritten ID gate
        match gate {
            Constant(id, val) => {
                let new_gate = Constant(map.to_new_id(id), val.clone());
                ret.push(new_gate);
            },

            AssertZero(id) => {
                let new_gate = AssertZero(map.to_new_id(id));
                ret.push(new_gate);
            },

            Copy(out, x) => {
                let new_gate = Copy(map.to_new_id(out), map.to_new_id(x));
                ret.push(new_gate);
            },

            Add(out, x, y) => {
                let new_gate = Add(map.to_new_id(out), 
                    map.to_new_id(x), 
                    map.to_new_id(y));
                ret.push(new_gate);
            },

            Mul(out, x, y) => {
                let new_gate = Mul(map.to_new_id(out), 
                    map.to_new_id(x), 
                    map.to_new_id(y));
                ret.push(new_gate);
            },

            AddConstant(out, x, constant) => {
                let new_gate = AddConstant(map.to_new_id(out), 
                    map.to_new_id(x), 
                    constant.to_vec());
                ret.push(new_gate);
            },
          
            MulConstant(out, x, constant) => {
                let new_gate = MulConstant(map.to_new_id(out), 
                    map.to_new_id(x), 
                    constant.to_vec());
                ret.push(new_gate);
            },

            And(out, x, y) => {
                let new_gate = And(map.to_new_id(out), 
                    map.to_new_id(x), 
                    map.to_new_id(y));
                ret.push(new_gate);
            },

            Xor(out, x, y) => {
                let new_gate = Xor(map.to_new_id(out), 
                    map.to_new_id(x), 
                    map.to_new_id(y));
                ret.push(new_gate);
            },

            Not(out, x) => {
                let new_gate = Not(map.to_new_id(out), map.to_new_id(x));
                ret.push(new_gate);
            },
           
            Instance(out) => {
                let new_gate = Instance(map.to_new_id(out));
                ret.push(new_gate);
            },
           
            Witness(out) => {
                let new_gate = Witness(map.to_new_id(out));
                ret.push(new_gate);
            },

            // if just first -> remove it;
            // if first, last -> remove range inclusive
            Free(first, last) => {
                map.free_ids(first, last);
            },

            _ => panic!("Not yet supported: {:?}", gate),
        }
    }

    (ret, map.free_variable_id)
}

// TODO: create a class for it, that can keep a state, so that it can convert many relations using
// the same wire numbering
pub fn to_r1cs(
    instance: &Instance,
    relation: &Relation,
    witness: &Witness,
    perform_modular_reduction: bool,
) -> (zkiCircuitHeader, zkiConstraintSystem, zkiWitness) {
    assert_eq!(instance.header, relation.header);
    assert_eq!(witness.header, relation.header);

    let relation = flatten_relation(relation);
    let (gates, free_variable_id) = deconflict_gate_wires(&relation.gates);

    let mut gc = GateConverter {
        free_variable_id, //new wires for possible mod reduction are added after uniquified ID values
        perform_modular_reduction,
        ..GateConverter::default()
    };

    let mut instance_index = 0;
    let mut witness_index = 0;
    // load instance and witness wire values, filtering their gates out of the result
    let relation_gates: Vec<Gate> = gates.iter().filter(|g| {
        match g {
            Instance(id) => {
                gc.instance_values.insert(*id, instance.common_inputs[instance_index].clone());
                instance_index += 1;
                false
            },
            Witness(id) => {
                gc.witness_values.insert(*id, witness.short_witness[witness_index].clone());
                witness_index += 1;
                false
            },
            _ => true,
        }}).cloned().collect();

    let (zki_header, zki_r1cs) = gc.build_header_and_relation(&instance.header, &relation_gates);

    let zki_witness = gc.update_witness(
        &BigUint::from_bytes_le(&relation.header.field_characteristic),
    );

    (zki_header, zki_r1cs, zki_witness)
}

#[cfg(test)]
use crate::producers::sink::MemorySink;
#[cfg(test)]
use crate::Source;
#[cfg(test)]
use crate::producers::from_r1cs::R1CSConverter;

#[cfg(test)]
fn assert_same_io_values(instance: &Instance, zki_header: &zkiCircuitHeader) -> crate::Result<()> {
    let zki_vals: Vec<BigUint> = zki_header.instance_variables
        .get_variables()
        .iter()
        .map(|v| BigUint::from_bytes_le(&v.value))
        .collect();
    let ir_vals: Vec<BigUint> = instance.common_inputs
        .iter()
        .map(|v| BigUint::from_bytes_le(&v))
        .collect();

    assert!(zki_vals.iter().all(|v| ir_vals.contains(v)));
    assert!(ir_vals.iter().all(|v| zki_vals.contains(v)));

    Ok(())
}

#[cfg(test)]
fn assert_same_witness_values(witness: &Witness, zki_witness: &zkiWitness) -> crate::Result<()> {
    let zki_vals: Vec<BigUint> = zki_witness.assigned_variables
        .get_variables()
        .iter()
        .map(|v| BigUint::from_bytes_le(&v.value))
        .collect();
    let ir_vals: Vec<BigUint> = witness.short_witness
        .iter()
        .map(|v| BigUint::from_bytes_le(&v))
        .collect();

    // zkif witness may (likely does) contain more witness values that IR could confirm
    assert!(ir_vals.iter().all(|v| zki_vals.contains(v)));

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

    let mut converter = R1CSConverter::new(MemorySink::default(), &zki_header);
    converter.ingest_witness(&zki_witness)?;
    converter.ingest_constraints(&zki_r1cs)?;

    let sink = converter.finish();
    let source: Source = sink.into();
    let messages = source.read_all_messages()?;
    let instance = messages.instances[0].clone();
    let relation = messages.relations[0].clone();
    let witness = messages.witnesses[0].clone();

    // now convert back into r1cs
    let (zki_header2, _, zki_witness2) = to_r1cs(&instance, &relation, &witness, false);

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

    let mut converter = R1CSConverter::new(MemorySink::default(), &zki_header);
    converter.ingest_witness(&zki_witness)?;
    converter.ingest_constraints(&zki_r1cs)?;

    let sink = converter.finish();
    let source: Source = sink.into();
    let messages = source.read_all_messages()?;
    let instance = messages.instances[0].clone();
    let relation = messages.relations[0].clone();
    let witness = messages.witnesses[0].clone();

    // now convert back into r1cs
    let (zki_header2, zki_r1cs2, zki_witness2) = to_r1cs(&instance, &relation, &witness, false);

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
    let (zki_header, zki_r1cs, zki_witness) = to_r1cs(&instance, &relation, &witness, false);

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

    let mut converter = R1CSConverter::new(MemorySink::default(), &zki_header);
    converter.ingest_witness(&zki_witness)?;
    converter.ingest_constraints(&zki_r1cs)?;

    let sink = converter.finish();
    let source: Source = sink.into();
    let messages = source.read_all_messages()?;
    let instance = messages.instances[0].clone();
    let relation = messages.relations[0].clone();
    let witness = messages.witnesses[0].clone();

    // now convert back into r1cs
    let (zki_header2, zki_r1cs2, zki_witness2) = to_r1cs(&instance, &relation, &witness, false);

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
    let (zki_header, zki_r1cs, zki_witness) = to_r1cs(&instance, &relation, &witness, false);

    println!("Header: {:?}", zki_header);
    println!("Witness: {:?}", zki_witness);
    println!("Constraints: {:?}", zki_r1cs);

    let mut simulator = zkinterface::consumers::simulator::Simulator::default();
    simulator.ingest_header(&zki_header)?;
    simulator.ingest_witness(&zki_witness)?;
    simulator.ingest_constraint_system(&zki_r1cs)?;

    Ok(())
}