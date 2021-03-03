use std::ops::Sub;
use num_bigint::BigUint;
use num_traits::One;

use crate::Gate::*;
use crate::structs:: {WireId, assignment::Assignment};
use crate::{ Header, Instance, Relation, Witness, Result};
use crate::producers::builder::{Builder, IBuilder};

use zkinterface::consumers::reader::Variable as zkiVariable;
use zkinterface::CircuitHeader as zkiCircuitHeader;
use zkinterface::Variables as zkiVariables;
use zkinterface::Witness as zkiWitness;
use zkinterface::ConstraintSystem as zkiConstraintSystem;
use zkinterface::StatementBuilder as zkiBuilder;
use zkinterface::BilinearConstraint;

// swapped
pub fn extract_field_maximum(header: &Header) -> BigUint {

    let mut fm = BigUint::from_bytes_le(&header.field_characteristic);
    let one : u8 = 1;
    fm = fm.sub(one);
    return fm;
}

pub fn to_r1cs(
    instance: &Instance,
    relation: &Relation,
) -> (zkiCircuitHeader, zkiConstraintSystem) {

    let mut cs = zkiConstraintSystem {
        constraints: vec![],
    };

    assert_eq!(instance.header, relation.header);
    let fm = extract_field_maximum(&instance.header);


}

