use num_bigint::BigUint;
use std::ops::Sub;

use crate::structs::assignment::Assignment;
use crate::Gate::*;
use crate::{Gate, Header, Instance, Relation, Witness, Result};

use zkinterface::BilinearConstraint;
use zkinterface::CircuitHeader as zkiCircuitHeader;
use zkinterface::ConstraintSystem as zkiConstraintSystem;
use zkinterface::Variables as zkiVariables;
use zkinterface::Witness as zkiWitness;

// swapped from from_r1cs.rs
// field maximum is one less than field characteristic
pub fn extract_field_maximum(header: &Header) -> BigUint {
    let mut fm = BigUint::from_bytes_le(&header.field_characteristic);
    let one: u8 = 1;
    fm = fm.sub(one);
    return fm;
}

//instance variables: values explicitly set for a wire, not as R1CS constraints
// return the zkInterface Variables struct, plus the free_variable_id
pub fn combine_variables(assignments: &[Assignment]) -> zkiVariables {
    let mut ids: Vec<u64> = vec![];
    let mut values: Vec<u8> = vec![];

    for a in assignments {
        ids.push(a.id);
        values.extend(&a.value);
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

pub fn gate_to_constraint(gate: &Gate) -> BilinearConstraint {
    let (a, b, c) = match gate {
        // Note: Constant gate seems to be eclipsed by AddConstant and MulConstant - what are they for again?
        Constant(w, v) => (
            (vec![0], *v),
            (vec![0], vec![1]),
            (vec![*w], vec![1]),
        ),
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
            (vec![*x, 0], Vec::<u8>[1].append(value)),
            (vec![0], vec![1]),
            (vec![*out], vec![1]),
        ),
        // MulConstant(w, _, _) => Some(w),
        // And(w, _, _) => Some(w),
        // Xor(w, _, _) => Some(w),
        // Not(w, _) => Some(w),

        // AssertZero(_) => None

        // REMOVE THIS - prevent errors while in progress
        _ => ((vec![], vec![]), (vec![], vec![]), (vec![], vec![])),
    };
    BilinearConstraint {
        linear_combination_a: make_combination(a.0, a.1),
        linear_combination_b: make_combination(b.0, b.1),
        linear_combination_c: make_combination(c.0, c.1),
    }
}

pub fn to_r1cs(
    instance: &Instance,
    relation: &Relation,
) -> (zkiCircuitHeader, zkiConstraintSystem) {
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

    let constraints: Vec<BilinearConstraint> = relation
        .gates
        .iter()
        .map(|g| gate_to_constraint(g))
        .collect();

    let cs = zkiConstraintSystem { constraints };

    (zki_header, cs)
}

// Conversion works the same for the witness as for the I/O variables.
// No need to use the header since the zkInterface witness doesn't include one.
pub fn to_r1cs_witness(witness: Witness) -> zkiWitness {
    let assigned_variables = combine_variables(&witness.short_witness);

    zkiWitness { assigned_variables }
}



// #[cfg(test)]
// fn test


#[test]
fn test_with_validate() -> Result<()> {
    use zkinterface::producers::examples::example_constraints as zki_example_constraints;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;
    use crate::consumers::validator::Validator;

    // begin tests as with from_r1cs

    let zki_header =  zki_example_header_inputs(3, 4, 25);
    let zki_witness = zki_example_witness_inputs(3, 4);
    let zki_r1cs = zki_example_constraints();

    let (instance, relation) = crate::producers::from_r1cs::to_ir(&zki_header, &zki_r1cs);
    let witness = crate::producers::from_r1cs::to_witness(&zki_header, &zki_witness);

    let mut validator = Validator::new_as_prover();
    validator.ingest_instance(&instance);
    validator.ingest_witness(&witness);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    if violations.len() > 0 {
        eprintln!("Violations:\n- {}\n", violations.join("\n- "));
    }

    // now convert back into r1cs (there)
    let (zki_header2, zki_r1cs2) = to_r1cs(&instance, &relation);
    let zki_witness2 = to_r1cs_witness(witness);

    let mut validator2 = zkinterface::consumers::validator::Validator::new_as_prover();   
    validator2.ingest_constraint_system(&zki_r1cs2);
    validator2.ingest_witness(&zki_witness2);
    validator2.ingest_header(&zki_header2);

    let violations = validator2.get_violations();
    if violations.len() > 0 {
        eprintln!("Violations:\n- {}\n", violations.join("\n- "));
    }

    Ok(())
}



