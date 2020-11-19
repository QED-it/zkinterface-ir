use std::ops::Add;
use num_bigint::BigUint;
use num_traits::{ToPrimitive, One};

use crate::Gate::*;
use crate::structs:: {WireId, assignment::Assignment};
use crate::{ Header, Instance, Relation, Witness, Result};
use crate::producers::builder::{Builder, IBuilder};

use zkinterface::consumers::reader::Variable as zkiVariable;
use zkinterface::CircuitHeader as zkiCircuitHeader;
use zkinterface::Variables as zkiVariables;
use zkinterface::Witness as zkiWitness;
use zkinterface::ConstraintSystem as zkiConstraintSystem;


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

pub fn zki_variables_to_vec_assignment(vars: &zkiVariables) -> (Vec<Assignment>, bool) {
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
            assert!(BigUint::from_bytes_le(var.value).is_one(), "value for instance id:0 should be a constant 1");
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

pub fn to_ir(
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

pub fn to_witness(
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
fn test_r1cs_to_gates() -> Result<()> {
    use zkinterface::producers::examples::example_constraints as zki_example_constrains;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;

    let zki_header =  zki_example_header_inputs(3, 4, 25);
    let zki_r1cs = zki_example_constrains();
    let zki_witness = zki_example_witness_inputs(3, 4);

    // in zkInterface the instance is inside the header
    // in ir each msg in {instance, relation, witness} has an header
    let (instance, relation) = to_ir(&zki_header, &zki_r1cs);

    let witness = to_witness(&zki_header, &zki_witness);

    assert_header(&instance.header);
    assert_header(&relation.header);
    assert_header(&witness.header);

    // check instance
    assert_eq!(instance.common_inputs.len(), 4);
    assert_assignment(&instance.common_inputs[0],0,1);
    assert_assignment(&instance.common_inputs[1],1,3);
    assert_assignment(&instance.common_inputs[2],2,4);
    assert_assignment(&instance.common_inputs[3],3,25);

    // check witness
    assert_eq!(witness.short_witness.len(), 2);
    assert_assignment(&witness.short_witness[0],4,9);
    assert_assignment(&witness.short_witness[1],5,16);

    // check relation:
    assert_eq!(relation.gates.len(),33);
    Ok(())
}

#[cfg(test)]
fn assert_header(header: &Header) {
    assert_eq!(header.profile,"circ_arithmetic_simple");
    assert_eq!(header.version,"0.1.0");
    let fc = BigUint::from_bytes_le(&header.field_characteristic);
    assert_eq!(fc.to_u8().unwrap(), 101);
    assert_eq!(header.field_degree, 1);
}

#[cfg(test)]
fn assert_assignment(assign: &Assignment, id: WireId, value : u32) {
    assert_eq!(assign.id, id);
    let val0 = BigUint::from_bytes_le(&assign.value).to_u32().unwrap();
    assert_eq!(val0, value);
}


#[test]
fn test_with_validate() -> Result<()> {
    use zkinterface::producers::examples::example_constraints as zki_example_constrains;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;
    use crate::consumers::validator::Validator;

    let zki_header =  zki_example_header_inputs(3, 4, 25);
    let zki_witness = zki_example_witness_inputs(3, 4);
    let zki_r1cs = zki_example_constrains();

    let (instance, relation) = to_ir(&zki_header, &zki_r1cs);
    let witness = to_witness(&zki_header, &zki_witness);

    let mut validator = Validator::new_as_prover();
    validator.ingest_instance(&instance);
    validator.ingest_witness(&witness);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    if violations.len() > 0 {
        eprintln!("Violations:\n- {}\n", violations.join("\n- "));
    }
    Ok(())
}

#[test]
fn test_with_evaluator() -> Result<()> {
    use zkinterface::producers::examples::example_constraints as zki_example_constrains;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;
    use crate::consumers::evaluator::Evaluator;

    let zki_header =  zki_example_header_inputs(3, 4, 25);
    let zki_witness = zki_example_witness_inputs(3, 4);
    let zki_r1cs = zki_example_constrains();

    let (instance, relation) = to_ir(&zki_header, &zki_r1cs);
    let witness = to_witness(&zki_header, &zki_witness);

    let mut evaluator = Evaluator::default();
    evaluator.ingest_instance(&instance)?;
    evaluator.ingest_witness(&witness)?;
    evaluator.ingest_relation(&relation)?;

    Ok(())
}
