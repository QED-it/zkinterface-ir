use flatbuffers::{emplace_scalar, read_scalar, EndianScalar};
use std::collections::HashMap;
use std::mem::size_of;

use crate::structs::inputs::Inputs;
use crate::structs::wire::WireListElement;
use crate::wirelist;
use crate::{Header, PrivateInputs, PublicInputs, Relation, TypeId};

pub fn example_header() -> Header {
    example_header_in_type(literal32(EXAMPLE_MODULUS))
}

pub fn example_public_inputs() -> PublicInputs {
    example_public_inputs_h(&example_header())
}

pub fn example_private_inputs() -> PrivateInputs {
    example_private_inputs_h(&example_header())
}

pub fn example_private_inputs_incorrect() -> PrivateInputs {
    example_private_inputs_incorrect_h(&example_header())
}

pub fn example_relation() -> Relation {
    example_relation_h(&example_header())
}

pub fn example_header_in_type(modulo: Vec<u8>) -> Header {
    Header::new(&[modulo])
}

// pythogarean example
pub fn example_public_inputs_h(header: &Header) -> PublicInputs {
    PublicInputs {
        header: header.clone(),
        inputs: vec![Inputs {
            values: vec![literal32(25), literal32(0), literal32(1)],
        }],
    }
}

pub fn example_private_inputs_h(header: &Header) -> PrivateInputs {
    PrivateInputs {
        header: header.clone(),
        inputs: vec![Inputs {
            values: vec![literal32(3), literal32(4), literal32(0)],
        }],
    }
}

pub fn example_private_inputs_incorrect_h(header: &Header) -> PrivateInputs {
    PrivateInputs {
        header: header.clone(),
        inputs: vec![Inputs {
            values: vec![
                literal32(3),
                literal32(4 + 1), // incorrect.
                literal32(1),
            ],
        }],
    }
}

pub fn example_relation_h(header: &Header) -> Relation {
    use crate::structs::function::{Function, FunctionBody};
    use crate::Gate::*;

    let type_id: TypeId = 0;

    Relation {
        header: header.clone(),
        plugins: vec![],
        functions: vec![Function::new(
            "com.example::mul".to_string(),
            HashMap::from([(type_id, 1)]),
            HashMap::from([(type_id, 2)]),
            HashMap::new(),
            HashMap::new(),
            FunctionBody::Gates(vec![Mul(type_id, 0, 1, 2)]),
        )],
        gates: vec![
            New(type_id, 0, 7),
            PrivateInput(type_id, 1),
            AnonCall(
                wirelist![type_id;0, 2, 4, 5, 6, 9, 10], // output
                wirelist![type_id;1],                    // input
                HashMap::from([(type_id, 3)]),           // public count
                HashMap::from([(type_id, 2)]),           // private count
                vec![
                    PublicInput(type_id, 0),
                    PrivateInput(type_id, 1),
                    Call(
                        "com.example::mul".to_string(),
                        wirelist![type_id;2],
                        wirelist![type_id;7; 2],
                    ),
                    Call(
                        "com.example::mul".to_string(),
                        wirelist![type_id;3],
                        wirelist![type_id;1; 2],
                    ),
                    Add(type_id, 4, 2, 3),
                    PrivateInput(type_id, 8),
                    AssertZero(type_id, 8),
                    PublicInput(type_id, 5),
                    AssertZero(type_id, 5),
                    PublicInput(type_id, 6),
                ],
            ),
            Constant(type_id, 3, encode_negative_one(&header.types[0])), // -1
            Call(
                "com.example::mul".to_string(),
                wirelist![type_id;7],
                wirelist![type_id;3, 0],
            ), // - public_input_0
            Add(type_id, 8, 6, 7),
            Delete(type_id, 0, Some(7)),
            Mul(type_id, 11, 8, 10),
            AssertZero(type_id, 11),
            Delete(type_id, 8, Some(11)),
        ],
    }
}

pub const EXAMPLE_MODULUS: u32 = 101;

pub fn literal<T: EndianScalar>(value: T) -> Vec<u8> {
    let mut buf = vec![0u8; size_of::<T>()];
    emplace_scalar(&mut buf[..], value);
    buf
}

pub fn literal32(v: u32) -> Vec<u8> {
    literal(v)
}

pub fn read_literal<T: EndianScalar>(encoded: &[u8]) -> T {
    if encoded.len() >= size_of::<T>() {
        read_scalar(encoded)
    } else {
        let mut encoded = Vec::from(encoded);
        encoded.resize(size_of::<T>(), 0);
        read_scalar(&encoded)
    }
}

pub fn encode_negative_one(modulo: &[u8]) -> Vec<u8> {
    let mut neg_one = modulo.to_owned();
    assert!(!neg_one.is_empty() && neg_one[0] > 0, "Invalid modulo");
    neg_one[0] -= 1;
    neg_one
}

#[test]
fn test_examples() {
    use crate::Source;

    let mut common_buf = Vec::<u8>::new();
    example_public_inputs().write_into(&mut common_buf).unwrap();
    example_relation().write_into(&mut common_buf).unwrap();

    let mut prover_buf = Vec::<u8>::new();
    example_private_inputs()
        .write_into(&mut prover_buf)
        .unwrap();

    let source = Source::from_buffers(vec![common_buf, prover_buf]);
    let messages = source.read_all_messages().unwrap();
    assert_eq!(messages.relations, vec![example_relation()]);
    assert_eq!(messages.public_inputs, vec![example_public_inputs()]);
    assert_eq!(messages.private_inputs, vec![example_private_inputs()]);
}
