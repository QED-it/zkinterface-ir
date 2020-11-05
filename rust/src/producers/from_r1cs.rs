use flatbuffers::{emplace_scalar, EndianScalar};
use std::mem::size_of;
use std::ops::Add;
use num_bigint::BigUint;
use num_traits::ToPrimitive;

use crate::Gate::*;
use crate::structs:: {WireId, assignment::Assignment};
use crate::{ Header, Instance, Relation, Witness,Result};
use crate::producers::builder::{Builder, IBuilder};
use crate::consumers::validator::Validator;
use crate::consumers::evaluator::Evaluator;

use zkinterface::consumers::reader::Variable as zkiVariable;
use zkinterface::CircuitHeader as zkiCircuitHeader;
use zkinterface::KeyValue as zkiKeyValue;
use zkinterface::Variables as zkiVariables;
use zkinterface::Witness as zkiWitness;
use zkinterface::{ConstraintSystem as zkiConstraintSystem, Variables};


pub fn zki_header_to_header(zki_header: &zkiCircuitHeader) -> Result<Header> {
    match &zki_header.field_maximum {
        None => Err("field_maximum must be provided".into()),

        Some(field_maximum) => {
            let mut fc: BigUint = BigUint::from_bytes_le(field_maximum);
            let one : u8 = 1;
            fc = fc.add(one);

            Ok(Header {
                field_characteristic: fc.to_bytes_le(),
                ..Header::default()
            })
        }
    }
}

pub fn zki_variables_to_vec_assignment(vars: &Variables) -> (Vec<Assignment>, bool) {
    let variable_ids_len = vars.variable_ids.len();
    let values_len = vars.get_variables().len();
    assert_eq!(
        variable_ids_len,
        values_len,
        "Number of variable ids and values must be equal."
    );

    if variable_ids_len == 0 && values_len == 0 {
        return (vec![], false);
    }

    let mut has_constant = false;
    let mut vec: Vec<Assignment> = Vec::new();
    for var in vars.get_variables().iter() {
        if var.id == 0 {
            assert_eq!(BigUint::from_bytes_le(var.value).to_u32().unwrap(), 1, "value for instance id:0 should be a constant 1");
            has_constant = true;
        }
        vec.push(Assignment {
            id: var.id,
            value: var.value.to_vec(),
        });
    }
    (vec, has_constant)
}

fn add_lc(b: &mut impl IBuilder, lc: &Vec<zkiVariable>) -> WireId {
    if lc.len() == 0 {
        // empty linear combination translates into an empty value
        return b.create_gate(Constant(0, vec![]));
    }

    let mut sum_id = build_term(b, &lc[0]);

    for term in &lc[1..] {
        let term_id = build_term(b, term);
        sum_id = b.create_gate(Add(0, sum_id, term_id));
    }

    sum_id
}

fn build_term(b: &mut impl IBuilder, term: &zkiVariable) -> WireId {
    if term.id == 0 {
        return b.create_gate(Constant(0, Vec::from(term.value)));
    }

    let val_id = b.create_gate(Constant(0, Vec::from(term.value)));
    return b.create_gate(Mul(0, term.id, val_id));
}

pub fn zki_r1cs_to_ir(
    zki_header: &zkiCircuitHeader,
    zki_r1cs: &zkiConstraintSystem,
) -> (Instance, Relation) {
    let header = zki_header_to_header(zki_header);
    assert!(header.is_ok());

    let (mut instance_assignment, has_constant) = zki_variables_to_vec_assignment(&zki_header.instance_variables);
    if !has_constant {
        // prepend the constant 1 as instance id:0
        instance_assignment.splice(
            0..0,
            vec![Assignment {
                id: 0,
                value: vec![1], //todo: is it good or size should be same as the other instance_variables?
            }],
        );
    }

    let i = Instance {
        header: header.as_ref().unwrap().clone(),
        common_inputs: instance_assignment,
    };

    let mut bb = Builder::new(zki_header.free_variable_id);
    let b = &mut bb;

    // Allocate negative one for negation.
    let max = zki_header.field_maximum.as_ref().unwrap();
    let neg_one_id = b.create_gate(Constant(0, max.clone()));

    // Convert each R1CS constraint into a graph of Add/Mul/Const/AssertZero gates.
    for constraint in &zki_r1cs.constraints {
        let sum_a_id = add_lc(b,&constraint.linear_combination_a.get_variables());
        let sum_b_id = add_lc(b,&constraint.linear_combination_b.get_variables());
        let sum_c_id = add_lc(b,&constraint.linear_combination_c.get_variables());

        let prod_id = b.create_gate(Mul(0, sum_a_id, sum_b_id));
        let neg_c_id = b.create_gate(Mul(0, neg_one_id, sum_c_id));
        let claim_zero_id = b.create_gate(Add(0, prod_id, neg_c_id));

        b.create_gate(AssertZero(claim_zero_id));
    }

    let r = Relation {
        header: header.unwrap().clone(),
        gates: b.gates.clone(),
    };

    (i, r)
}

pub fn zki_witness_to_witness(
    zki_header: &zkiCircuitHeader,
    zki_witness: &zkiWitness,
) -> Witness {
    let header = zki_header_to_header(zki_header);
    assert!(header.is_ok());

    let (witness, _) = zki_variables_to_vec_assignment(&zki_witness.assigned_variables);

    Witness {
        header: header.unwrap(),
        short_witness: witness,
    }
}

#[test]
fn test_r1cs_to_gates() {
    let zki_header = example_header();
    let zki_r1cs = example_constraints();

    // in zkInterface the instance is inside the header
    // in ir each msg in {instance, relation, witness} has an header
    let (instance, relation) = zki_r1cs_to_ir(&zki_header, &zki_r1cs);

    let zki_witness = example_witness();
    let witness = zki_witness_to_witness(&zki_header, &zki_witness);

    eprintln!();
    eprintln!("{}", instance.header.profile);
    eprintln!("{}", relation.header.profile);
    eprintln!("{}", witness.header.profile);
}


#[test]
fn test_with_validate() {
    let (instance, relation) = zki_r1cs_to_ir(&example_header(), &example_constraints());
    let witness = zki_witness_to_witness(&example_header(), &example_witness());

    let mut validator = Validator::new_as_prover();
    validator.ingest_instance(&instance);
    validator.ingest_witness(&witness);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    if violations.len() > 0 {
        eprintln!("Violations:\n- {}\n", violations.join("\n- "));
    }
}

#[test]
fn test_with_evaluator() -> Result<()> {
    let (instance, relation) = zki_r1cs_to_ir(&example_header(), &example_constraints());
    let witness = zki_witness_to_witness(&example_header(), &example_witness());

    let mut evaluator = Evaluator::default();
    evaluator.ingest_instance(&instance)?;
    evaluator.ingest_witness(&witness)?;
    evaluator.ingest_relation(&relation)?;

    Ok(())
}


// zkInterface examples:

pub const MODULUS: u64 = 101;
pub const NEG_ONE: u64 = MODULUS - 1;

pub fn example_header() -> zkiCircuitHeader {
    example_header_inputs(3, 4, 25)
}

/// A test circuit of inputs x,y,zz such that x^2 + y^2 = zz.
pub fn example_header_inputs(x: u32, y: u32, zz: u32) -> zkiCircuitHeader {
    zkiCircuitHeader {
        instance_variables: zkiVariables {
            variable_ids: vec![1, 2, 3], // x, y, zz
            values: Some(serialize_small(&[x, y, zz])),
        },
        free_variable_id: 6,
        field_maximum: Some(serialize_small(&[NEG_ONE])),
        configuration: Some(vec![zkiKeyValue::from(("Name", "r1cs_to_ir_example"))]),
    }
}

//todo: improve the example in zkI (add serialize_small)
pub fn example_constraints() -> zkiConstraintSystem {
    //let constraints_vec: &[((Vec<u64>, Vec<u8>), (Vec<u64>, Vec<Vec<u8>>), (Vec<u64>, Vec<u8>))] = &[
    let constraints_vec: &[((Vec<u64>, Vec<u8>), (Vec<u64>, Vec<u8>), (Vec<u64>, Vec<u8>))] = &[
        // (A ids values)  *  (B ids values)  =  (C ids values)
        ((vec![1], vec![1]), (vec![1], vec![1]), (vec![4], vec![1])),       // x * x = xx
        ((vec![2], vec![1]), (vec![2], vec![1]), (vec![5], vec![1])),       // y * y = yy
        ((vec![0], vec![1]), (vec![4, 5], serialize_small(&[1, 1])), (vec![3], vec![1])), // 1 * (xx + yy) = zz
    ];
    zkiConstraintSystem::from(constraints_vec)
}

pub fn example_witness() -> zkiWitness {
    example_witness_inputs(3, 4)
}

pub fn example_witness_inputs(x: u32, y: u32) -> zkiWitness {
    zkiWitness {
        assigned_variables: zkiVariables {
            variable_ids: vec![4, 5], // xx, yy
            values: Some(serialize_small(&[
                x * x, // var_4 = xx = x^2
                y * y, // var_5 = yy = y^2
            ])),
        },
    }
}

pub fn serialize_small<T: EndianScalar>(values: &[T]) -> Vec<u8> {
    let sz = size_of::<T>();
    let mut buf = vec![0u8; sz * values.len()];
    for i in 0..values.len() {
        emplace_scalar(&mut buf[sz * i..], values[i]);
    }
    buf
}
