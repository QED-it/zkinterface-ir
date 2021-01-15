use flatbuffers::{emplace_scalar, EndianScalar, read_scalar};
use std::mem::size_of;

use crate::{Header, Relation, Instance, Witness};
use crate::structs::assignment::Assignment;


pub fn example_header() -> Header {
    Header {
        field_characteristic: literal32(MODULUS),
        ..Header::default()
    }
}

pub fn example_relation() -> Relation {
    use crate::Gate::*;

    Relation {
        header: example_header(),
        gates: vec![
            Constant(3, literal32(MODULUS - 1)), // -1
            Mul(4, 1, 1),   // witness_1 squared
            Mul(5, 2, 2),   // witness_2 squared
            Add(6, 4, 5),   // sum of squares
            Mul(7, 0, 3),   // negative instance_0
            Add(8, 6, 7),   // sum - instance_0
            Free(1, 1),     // free one variable
            Free(2, 7),     // free many variables
            AssertZero(8),  // difference == 0
            Free(8, 8),     // free remaining variables
        ],
    }
}

pub fn example_instance() -> Instance {
    Instance {
        header: example_header(),
        common_inputs: vec![
            Assignment { id: 0, value: literal32(25) },
        ],
    }
}

pub fn example_witness() -> Witness {
    Witness {
        header: example_header(),
        short_witness: vec![
            Assignment { id: 1, value: literal32(3) },
            Assignment { id: 2, value: literal32(4) },
        ],
    }
}


pub const MODULUS: u32 = 101;

pub fn neg(val: u32) -> u32 {
    MODULUS - (val % MODULUS)
}

pub fn literal<T: EndianScalar>(value: T) -> Vec<u8> {
    let mut buf = vec![0u8; size_of::<T>()];
    emplace_scalar(&mut buf[..], value);
    buf
}

fn literal32(v: u32) -> Vec<u8> { literal(v) }

pub fn read_literal<T: EndianScalar>(encoded: &[u8]) -> T {
    if encoded.len() >= size_of::<T>() {
        read_scalar(encoded)
    } else {
        let mut encoded = Vec::from(encoded);
        encoded.resize(size_of::<T>(), 0);
        read_scalar(&encoded)
    }
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
