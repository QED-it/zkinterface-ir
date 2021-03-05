use std::ops::Sub;
use num_bigint::BigUint;
use num_traits::One;

use crate::structs:: {WireId, assignment::Assignment};
use crate::{ Header, Instance, Relation, Witness, Result, Gate};
use crate::Gate::*;
use crate::producers::builder::{Builder, IBuilder};

use zkinterface::consumers::reader::Variable as zkiVariable;
use zkinterface::CircuitHeader as zkiCircuitHeader;
use zkinterface::Variables as zkiVariables;
use zkinterface::Witness as zkiWitness;
use zkinterface::ConstraintSystem as zkiConstraintSystem;
use zkinterface::StatementBuilder as zkiBuilder;
use zkinterface::BilinearConstraint;

// swapped from from_r1cs.rs
// field maximum is one less than field characteristic
pub fn extract_field_maximum(header: &Header) -> BigUint {

    let mut fm = BigUint::from_bytes_le(&header.field_characteristic);
    let one : u8 = 1;
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
        }
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
        // Constant(w, v) => Some(w),
        // Copy(w, _) => Some(w),
        Add(out, x, y) => (
            (vec![*x, *y], vec![1, 1]), 
            (vec![], vec![]), 
            (vec![*out], vec![1]),
        ),
        Mul(out, x, y) => (
            (vec![*x], vec![1]), 
            (vec![*y], vec![1]), 
            (vec![*out], vec![1]),
        ),
        // AddConstant(w, _, _) => Some(w),
        // MulConstant(w, _, _) => Some(w),
        // And(w, _, _) => Some(w),
        // Xor(w, _, _) => Some(w),
        // Not(w, _) => Some(w),

        // AssertZero(_) => None

        // REMOVE THIS - prevent errors while in progress
        _ => (
            (vec![], vec![]), 
            (vec![], vec![]), 
            (vec![], vec![]),
        ),
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
    let instance_variables= combine_variables(&assignments);

    // Remember that max returns a reference! Deref so no double borrow
    let var_max_id = *instance_variables.variable_ids.iter().max().unwrap();

    // Output wire is always greatest id of a gate; find the max
    let gate_max_id = relation.gates.iter().fold(0, |acc, gate| {
        match gate.get_output_wire_id() {
            Some(w) => std::cmp::max(acc, w),
            None => acc
        }
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


    let cs = zkiConstraintSystem {
        constraints: vec![],
    };


    (zki_header, cs)

}

