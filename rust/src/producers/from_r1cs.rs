use flatbuffers::{emplace_scalar, EndianScalar, read_scalar};
use std::mem::size_of;

use zkinterface::{ConstraintSystem as zkiConstraintSystem, Variables};
use zkinterface::CircuitHeader as zkiCircuitHeader;
use zkinterface::KeyValue as zkiKeyValue;
use zkinterface::Witness as zkiWitness;
use zkinterface::Variables as zkiVariables;
use zkinterface::consumers::reader::Variable as zkiVariable;

use crate::{Header, Relation, Instance, Witness, Gate};
use crate::structs::assignment::Assignment;
use crate::producers::examples::literal;
use crate::Result;
use crate::structs::{WireId, Value};
use std::ops::Add;
use crate::structs::gates::Gate::Constant;
use crate::Gate::*;

pub fn zki_header_to_header(zki_header: &zkiCircuitHeader) -> Result<Header> {
    match &zki_header.field_maximum {
        None => Err("field_maximum must be provided".into()),

        Some(field_maximum) => OK(Header {
            field_characteristic: vec![field_maximum + 1],
            ..Header::default()
        })
    }
}

pub fn zki_variables_to_vec_assignment(vars: &Variables) -> (Vec<Assignment>, bool) {
    let variable_ids_len = vars.variable_ids.len();
    let values_len = vars.get_variables().len();
    assert_eq!(variable_ids_len != values_len, format!("Number of variable ids and values must be equal."));

    if variable_ids_len == 0 && values_len == 0 {
        return (vec![], false);
    }

    let mut hasConstant = false;
    let mut vec: Vec<Assignment> = Vec::new();
    for var in vars.get_variables().iter() {
        if var.id == 0 {
            assert_eq!(var.value, 0, "instance id:0 value is not 0");
            hasConstant = true;
        }
        vec.extend_one(Assignment {
            id: var.id,
            value: var.value.to_vec(),
        });
    }
    (vec, hasConstant);
}

//todo: move to Gate
fn get_output_wire_id(gate: &Gate) -> WireId {
    use crate::Gate::*;
    match gate.into() {
        Constant(wireId, _) => wireId,
        AssertZero(wireId) => wireId,
        Copy(wireId, _) => wireId,
        Add(wireId, _, _) => wireId,
        Mul(wireId, _, _) => wireId,
        AddConstant(wireId, _, _) => wireId,
        MulConstant(wireId, _, _) => wireId,
        And(wireId, _, _) => wireId,
        Xor(wireId, _, _) => wireId,
        Not(wireId, _) => wireId,
    }
}

fn push_gate(mut gates: Vec<Gate>, gate: Gate) -> WireId {
    gates.push(gate);
    get_output_wire_id(&gate)
}

// will increase wireId by one
fn push_const_gate(mut gates: Vec<Gate>, mut out_wire_id: WireId, value: Value) -> WireId {
    gates.push(Constant(origId, value));
    let orig_id = out_wire_id;
    out_wire_id.add(1);
    orig_id
}

// will increase wireId by one
fn push_add_gate(mut gates: Vec<Gate>, mut out_wire_id: WireId, wire1: WireId, wire2: WireId) -> WireId {
    gates.push(Add(out_wire_id, wire1, wire2));
    let origId = out_wire_id;
    out_wire_id.add(1);
    origId
}

// will increase wireId by one
fn push_mul_gate(mut gates: Vec<Gate>, mut out_wire_id: WireId, wire1: WireId, wire2: WireId) -> WireId {
    gates.push(Mul(out_wire_id, wire1, wire2));
    let origId = out_wire_id;
    out_wire_id.add(1);
    origId
}

fn push_assert_zero_gate(mut gates: Vec<Gate>, in_wire_id: WireId) -> WireId {
    gates.push(AssertZero(in_wire_id));
    in_wire_id
}

pub fn zki_r1cs_to_ir(zki_header: &zkiCircuitHeader, zki_r1cs: &zkiConstraintSystem) -> Result<(Instance, Relation)> {
    let header = zki_header_to_header(zki_header);
    assert!(header.is_ok());

    let (mut instance_assignment, hasConstant) = zki_variables_to_vec_assignment(&zki_header.instance_variables);
    if !hasConstant {
        // prepend the constant 1 as instance id:0
        instance_assignment.splice(0..0, vec![Assignment { id: 0, value: vec![1] }]);
    }

    let i = Instance {
        header: header.unwrap(),
        common_inputs: instance_assignment.unwrap(),
    };

    let mut gates = Vec::<Gate>::new();

    let mut next_wire_id = zki_header.free_variable_id;

    // Allocate negative one for negation.
    let neg_one_id = push_const_gate(gates, next_wire_id, zki_header.field_maximum.unwrap());

    // Convert each R1CS constraint into a graph of Add/Mul/Const/AssertZero gates.
    for constraint in &zki_r1cs.constraints {
        let sum_a_id = add_lc(gates, next_wire_id, &constraint.linear_combination_a.get_variables());
        let sum_b_id = add_lc(gates, next_wire_id, &constraint.linear_combination_b.get_variables());
        let sum_c_id = add_lc(gates, next_wire_id, &constraint.linear_combination_c.get_variables());

        let prod_id = push_mul_gate(gates, next_wire_id, sum_a_id, sum_b_id);
        let neg_c_id = push_mul_gate(gates, next_wire_id, neg_one_id, sum_c_id);
        let claim_zero_id = push_add_gate(gates, next_wire_id, prod_id, neg_c_id);

        push_assert_zero_gate(gates, claim_zero_id);
    }

    fn add_lc(mut gates: Vec<Gate>, mut next_wire_id: u64, lc: &Vec<zkiVariable>) -> WireId {
        if lc.len() == 0 {
            //todo: add a comment about empty.
            return push_const_gate(gates, next_wire_id, vec![]);
        }

        let mut sum_id = build_term(gates, next_wire_id, &lc[0]);

        for term in &lc[1..] {
            let term_id = build_term(gates, next_wire_id, term);
            sum_id = push_add_gate(gates, next_wire_id, sum_id, term_id);
        }

        sum_id
    }

    fn build_term(mut gates: Vec<Gate>, mut next_wire_id: u64, term: &zkiVariable) -> WireId {
        //todo: if term.id == 0 -> use constant c, not Mul gate
        let val_id = push_const_gate(gates, next_wire_id, term.value.to_vec());
        return push_mul_gate(gates, next_wire_id, term.id, val_id);
    }

    let r = Relation {
        header: header.unwrap(),
        gates,
    };


    Ok((i, r))
}

pub fn zki_witness_to_witness(zki_header: &zkiCircuitHeader, zki_witness: &zkiWitness) -> Result<Witness> {
    let header = zki_header_to_header(zki_header);
    assert!(header.is_ok());

    let (witness, _) = zki_variables_to_vec_assignment(&zki_witness.assigned_variables);

    OK(Witness {
        header: header.unwrap(),
        short_witness: witness.unwrap(),
    })
}

#[test]
fn test_r1cs_to_gates() {
    let zki_header = example_header();
    let zki_r1cs = example_constraints();

    // in zkInterface the instance is inside the header
    // in ir each msg in {instance, relation, witness} has an header

    // todo: add constant 1 to the instance (id:0)
    // start counting from the free variable id in zki(for the gates)
    let (instance, relation) = zki_r1cs_to_ir(&zki_header, &zki_r1cs);

    let zki_witness = example_witness();
    let witness = zki_witness_to_witness(&zki_header, &zki_witness);

    eprintln!();
    eprintln!("{}", instance);
    eprintln!("{}", relation);


    let witness = example_witness();
    eprintln!("{}", witness);
}

pub const MODULUS: u64 = 101;
pub const NEG_ONE: u64 = MODULUS - 1;

pub fn example_header() -> zkiCircuitHeader {
    example_header_inputs(3, 4, 25)
}

/// A test circuit of inputs x,y,zz such that x^2 + y^2 = zz.
pub fn example_header_inputs(x: u32, y: u32, zz: u32) -> zkiCircuitHeader {
    zkiCircuitHeader {
        instance_variables: zkVariables {
            variable_ids: vec![1, 2, 3],  // x, y, zz
            values: Some(serialize_small(&[x, y, zz])),
        },
        free_variable_id: 6,
        field_maximum: Some(serialize_small(&[NEG_ONE])),
        configuration: Some(vec![
            zkiKeyValue::from(("Name", "r1cs_to_ir_example")),
        ]),
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

pub fn example_witness() -> zkWitness {
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

pub fn deserialize_small<T: EndianScalar>(encoded: &[u8]) -> T {
    if encoded.len() == size_of::<T>() {
        read_scalar(encoded)
    } else {
        let mut encoded = Vec::from(encoded);
        encoded.resize(size_of::<T>(), 0);
        read_scalar(&encoded)
    }
}
