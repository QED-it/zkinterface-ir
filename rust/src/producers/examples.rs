use flatbuffers::{emplace_scalar, read_scalar, EndianScalar};
use std::mem::size_of;
use num_bigint::BigUint;

use crate::{Header, Instance, Relation, Witness};
use crate::structs::relation::{ADD, MUL, FUNCTION, SWITCH, FOR, MULC};
use crate::structs::function::ForLoopBody;

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
    Header {
        field_characteristic: field_order,
        ..Header::default()
    }
}

// pythogarean example
pub fn example_instance_h(header: &Header) -> Instance {
    Instance {
        header: header.clone(),
        common_inputs: vec![literal32(25), literal32(0), literal32(1)],
    }
}

pub fn example_witness_h(header: &Header) -> Witness {
    let modulus = BigUint::from_bytes_le(&header.field_characteristic);
    let fibonacci_22 = BigUint::from(17711 as u64) % modulus;
    Witness {
        header: header.clone(),
        short_witness: vec![literal32(3), literal32(4), fibonacci_22.to_bytes_le()],
    }
}

pub fn example_witness_incorrect_h(header: &Header) -> Witness {
    Witness {
        header: header.clone(),
        short_witness: vec![
            literal32(3),
            literal32(4 + 1), // incorrect.
            literal32(40) // incorrect
        ],
    }
}

pub fn example_relation_h(header: &Header) -> Relation {
    use crate::Gate::*;
    use crate::structs::function::Function;
    use crate::structs::wire::{WireListElement::*};
    use crate::structs::iterators::{IterExprListElement::*, IterExprWireNumber::*};
    use crate::structs::function::CaseInvoke::*;

    Relation {
        header: header.clone(),
        gate_mask: ADD|MUL| MULC,
        feat_mask: FUNCTION|SWITCH|FOR,
        functions: vec![
            Function::new("com.example::mul".to_string(), 1, 2, 0, 0, vec![Mul(0, 1, 2)])
        ],
        gates: vec![
            Witness(1),
            Switch(
                1,                      // condition
                vec![Wire(0), Wire(2), WireRange(4, 6), WireRange(9, 11)],    // output wires
                vec![vec![3], vec![5]], // cases
                vec![
                    // branches
                    AbstractAnonCall (
                        // WireList, usize, usize, Vec<Gate>)
                        vec![Wire(1)],
                        3, 2,
                        vec![
                            Instance(0),  // In Global Namespace: Instance(0)
                            Witness(1),   // In Global Namespace: Witness(2)
                            Call("com.example::mul".to_string(), vec![Wire(2)], vec![Wire(8), Wire(8)]), // In Global Namespace: Mul(4, 1, 1)
                            Call("com.example::mul".to_string(), vec![Wire(3)], vec![Wire(1), Wire(1)]), // In Global Namespace: Mul(5, 2, 2)
                            Add(4, 2, 3), // In Global Namespace: Add(6, 4, 5)
                            Witness(5),
                            Instance(6),
                            Instance(7),
                        ]

                    ),
                    // remapping local-to-global namespaces: [0, 2, 4, 5, 6] || [1] = [0, 2, 4, 5, 6, 1]
                    AbstractAnonCall (
                        // WireList, usize, usize, Vec<Gate>)
                        vec![Wire(1)],
                        3, 2,
                        vec![
                            Instance(0),
                            Call("com.example::mul".to_string(), vec![Wire(1)], vec![Wire(8), Wire(0)]),
                            Witness(2),
                            Mul(3, 1, 2),
                            Add(4, 2, 3),
                            Instance(5),
                            Instance(6),
                            Witness(7),
                        ],
                    )
                ],
            ),
            Constant(3, encode_negative_one(&header)), // -1
            Call("com.example::mul".to_string(), vec![Wire(7)], vec![Wire(3), Wire(0)]), // - instance_0
            Add(8, 6, 7),                                        // sum - instance_0
            Free(0, Some(7)),                                    // Free all previous wires
            AssertZero(8),                                       // difference == 0

            For("i".into(), 0, 20, vec![WireRange(12, 32)],
                ForLoopBody::IterExprAnonCall(
                        vec![Single(IterExprAdd(Box::new(IterExprName("i".into())), Box::new(IterExprConst(12))))], // i + 12
                        vec![
                            Single(IterExprAdd(Box::new(IterExprName("i".into())), Box::new(IterExprConst(10)))),
                            Single(IterExprAdd(Box::new(IterExprName("i".into())), Box::new(IterExprConst(11)))),
                        ],
                        0, 0,
                        vec![Add(0, 1, 2)],
                )
            ),
            MulConstant(33, 32, encode_negative_one(&header)), // multiply by -1
            Add(34, 9, 33),
            AssertZero(34),
            Free(8, Some(34)),
        ],
    }
}

pub const EXAMPLE_MODULUS: u32 = 101;

pub fn literal<T: EndianScalar>(value: T) -> Vec<u8> {
    let mut buf = vec![0u8; size_of::<T>()];
    emplace_scalar(&mut buf[..], value);
    buf
}

fn literal32(v: u32) -> Vec<u8> {
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

pub fn encode_negative_one(header: &Header) -> Vec<u8> {
    let mut neg_one = header.field_characteristic.clone();
    assert!(neg_one.len() > 0 && neg_one[0] > 0, "Invalid field order");
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
