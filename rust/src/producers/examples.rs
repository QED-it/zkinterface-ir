use flatbuffers::{emplace_scalar, read_scalar, EndianScalar};
use std::collections::HashMap;
use std::mem::size_of;

use crate::structs::inputs::Inputs;
use crate::structs::wire::WireListElement;
use crate::wirelist;
use crate::{FieldId, Header, Instance, Relation, Witness};

pub fn example_header() -> Header {
    example_header_in_field(literal32(EXAMPLE_MODULUS))
}

pub fn example_instance() -> Instance {
    example_instance_h(&example_header())
}

pub fn example_witness() -> Witness {
    example_witness_h(&example_header())
}

pub fn example_witness_incorrect() -> Witness {
    example_witness_incorrect_h(&example_header())
}

pub fn example_relation() -> Relation {
    example_relation_h(&example_header())
}

pub fn example_header_in_field(field_order: Vec<u8>) -> Header {
    Header::new(&[field_order])
}

// pythogarean example
pub fn example_instance_h(header: &Header) -> Instance {
    Instance {
        header: header.clone(),
        common_inputs: vec![Inputs {
            inputs: vec![literal32(25), literal32(0), literal32(1)],
        }],
    }
}

pub fn example_witness_h(header: &Header) -> Witness {
    Witness {
        header: header.clone(),
        short_witness: vec![Inputs {
            inputs: vec![literal32(3), literal32(4), literal32(0)],
        }],
    }
}

pub fn example_witness_incorrect_h(header: &Header) -> Witness {
    Witness {
        header: header.clone(),
        short_witness: vec![Inputs {
            inputs: vec![
                literal32(3),
                literal32(4 + 1), // incorrect.
                literal32(1),
            ],
        }],
    }
}

pub fn example_relation_h(header: &Header) -> Relation {
    use crate::structs::function::Function;
    use crate::Gate::*;

    let field_id: FieldId = 0;

    Relation {
        header: header.clone(),
        functions: vec![Function::new(
            "com.example::mul".to_string(),
            HashMap::from([(field_id, 1)]),
            HashMap::from([(field_id, 2)]),
            HashMap::new(),
            HashMap::new(),
            vec![Mul(field_id, 0, 1, 2)],
        )],
        gates: vec![
            Witness(field_id, 1),
            AnonCall(
                wirelist![field_id;0, 2, 4, 5, 6, 9, 10], // output
                wirelist![field_id;1],                    // input
                HashMap::from([(field_id, 3)]),           // instance count
                HashMap::from([(field_id, 2)]),           // witness count
                vec![
                    Instance(field_id, 0),
                    Witness(field_id, 1),
                    Call(
                        "com.example::mul".to_string(),
                        wirelist![field_id;2],
                        wirelist![field_id;7; 2],
                    ),
                    Call(
                        "com.example::mul".to_string(),
                        wirelist![field_id;3],
                        wirelist![field_id;1; 2],
                    ),
                    Add(field_id, 4, 2, 3),
                    Witness(field_id, 8),
                    AssertZero(field_id, 8),
                    Instance(field_id, 5),
                    AssertZero(field_id, 5),
                    Instance(field_id, 6),
                ],
            ),
            Constant(field_id, 3, encode_negative_one(&header.fields[0])), // -1
            Call(
                "com.example::mul".to_string(),
                wirelist![field_id;7],
                wirelist![field_id;3, 0],
            ), // - instance_0
            Add(field_id, 8, 6, 7),
            Free(field_id, 0, Some(7)),
            Mul(field_id, 11, 8, 10),
            AssertZero(field_id, 11),
            Free(field_id, 8, Some(11)),
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

pub fn encode_negative_one(field: &[u8]) -> Vec<u8> {
    let mut neg_one = field.to_owned();
    assert!(!neg_one.is_empty() && neg_one[0] > 0, "Invalid field order");
    neg_one[0] -= 1;
    neg_one
}

#[test]
fn test_examples() {
    use crate::Source;

    let mut common_buf = Vec::<u8>::new();
    example_instance().write_into(&mut common_buf).unwrap();
    example_relation().write_into(&mut common_buf).unwrap();

    let mut prover_buf = Vec::<u8>::new();
    example_witness().write_into(&mut prover_buf).unwrap();

    let source = Source::from_buffers(vec![common_buf, prover_buf]);
    let messages = source.read_all_messages().unwrap();
    assert_eq!(messages.relations, vec![example_relation()]);
    assert_eq!(messages.instances, vec![example_instance()]);
    assert_eq!(messages.witnesses, vec![example_witness()]);
}
