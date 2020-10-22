use flatbuffers::{emplace_scalar, EndianScalar, read_scalar};
use std::mem::size_of;

use zkinterface::ConstraintSystem as zkiConstraintSystem;
use zkinterface::CircuitHeader as zkiCircuitHeader;
use zkinterface::KeyValue as zkiKeyValue;
use zkinterface::Witness as zkiWitness;
use zkinterface::Variables as zkiVariables;

use crate::{Header, Relation, Instance, Witness};
use crate::structs::assignment::Assignment;
use crate::producers::examples::literal;
use crate::Result;

pub fn zki_header_to_header(zki_header: &zkiCircuitHeader) -> Result<Header> {
    if zki_header.field_maximum.is_none(){
        return Err("field_maximum must be provided".into())
    }
    OK(Header{
        version: Header::default().version,
        profile: Header::default().profile, //todo: verify default values
        field_characteristic: vec![], //todo: add
        field_degree: deserialize_small(&zki_header.field_maximum.unwrap())
    })
}

pub fn zki_r1cs_to_ir(zki_header: &zkiCircuitHeader, zki_r1cs: &zkiConstraintSystem) -> Result<(Instance,Relation)> {
    use crate::Gate::*;
    let header = zki_header_to_header(zki_header);
    assert!(header.is_ok());

    let i =  Instance {
        header: header.unwrap(),
        common_inputs: vec![
            Assignment { id: 0, value: literal32(25) },
        ],
    };

    let r = Relation {
        header: header.unwrap(),
        gates: vec![
            Constant(3, literal32(MODULUS - 1)), // -1
            Mul(4, 1, 1),   // witness_1 squared
            Mul(5, 2, 2),   // witness_2 squared
            Add(6, 4, 5),   // sum of squares
            Mul(7, 0, 3),   // negative instance_0
            Add(8, 6, 7),   // sum - instance_0
            AssertZero(8),  // difference == 0
        ],
    };

    Ok((i,r))
}

pub fn zki_witness_to_witness(zki_header: &zkiCircuitHeader, zki_witness: &zkiWitness) -> Result<Witness> {
    let header = zki_header_to_header(zki_header);
    assert!(header.is_ok());

    let zki_variables = &zki_witness.assigned_variables;
    let variable_ids_len = zki_variables.variable_ids.len();
    let values_len = zki_variables.get_variables().len();

    if variable_ids_len == 0 && values_len == 0 {
        return OK(Witness {
            header: header.unwrap(),
            short_witness: Vec::new(),
        });
    }

    if variable_ids_len != values_len {
        return Err(format!("Number of variable ids and values must be equal. Provided {0} variable ids and {1} values",
                    variable_ids_len,
                    values_len
        ).into());
    }

    let mut s_v: Vec<Assignment> = Vec::new();
    for var_id in zki_variables.variable_ids.iter() {
        s_v.extend_one(Assignment {
            id: *var_id,
            value: literal(zki_variables.values[var_id]),
        });
    }

    OK(Witness {
        header: header.unwrap(),
        short_witness: s_v,
    })
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

pub fn example_constraints() -> zkiConstraintSystem {
    let constraints_vec: &[((Vec<u64>, Vec<u8>), (Vec<u64>, Vec<u8>), (Vec<u64>, Vec<u8>))] = &[
        // (A ids values)  *  (B ids values)  =  (C ids values)
        ((vec![1], vec![1]), (vec![1], vec![1]), (vec![4], vec![1])),       // x * x = xx
        ((vec![2], vec![1]), (vec![2], vec![1]), (vec![5], vec![1])),       // y * y = yy
        ((vec![0], vec![1]), (vec![4, 5], vec![1, 1]), (vec![3], vec![1])), // 1 * (xx + yy) = z
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
