use crate::structs::function::ForLoopBody;
use crate::structs::relation::{AND, FOR, FUNCTION, NOT, SWITCH, XOR};
use crate::{Header, Instance, Relation, Witness};

pub fn example_instance() -> Instance {
    example_instance_h(&example_boolean_header())
}

pub fn example_witness() -> Witness {
    example_witness_h(&example_boolean_header())
}

pub fn example_witness_incorrect() -> Witness {
    example_witness_incorrect_h(&example_boolean_header())
}

pub fn example_relation() -> Relation {
    example_relation_h(&example_boolean_header())
}

pub fn example_boolean_header() -> Header {
    Header {
        field_characteristic: vec![2],
        ..Header::default()
    }
}

pub fn example_instance_h(header: &Header) -> Instance {
    Instance {
        header: header.clone(),
        common_inputs: vec![
            vec![0],
            vec![0],
            vec![0], // 0 on 3 bits
            vec![0],
            vec![0],
            vec![1], // 1 on 3 bits
            vec![0],
            vec![1],
        ],
    }
}

pub fn example_witness_h(header: &Header) -> Witness {
    Witness {
        header: header.clone(),
        short_witness: vec![
            vec![1],
            vec![0],
            vec![1], // 5 on 3 bits
            vec![0],
            vec![0],
        ],
    }
}

pub fn example_witness_incorrect_h(header: &Header) -> Witness {
    Witness {
        header: header.clone(),
        short_witness: vec![
            vec![1],
            vec![1],
            vec![1], // 7 on 3 bits
            vec![0],
            vec![0],
        ],
    }
}

pub fn example_relation_h(header: &Header) -> Relation {
    use crate::structs::function::CaseInvoke::*;
    use crate::structs::function::Function;
    use crate::structs::iterators::{IterExprListElement::*, IterExprWireNumber::*};
    use crate::structs::wire::{WireListElement, WireListElement::*};
    use crate::wirelist;
    use crate::Gate::*;

    Relation {
        header: header.clone(),
        gate_mask: AND | XOR | NOT,
        feat_mask: FUNCTION | SWITCH | FOR,
        functions: vec![
            // output: [W0, W1, W2] // one integer on 3 bits
            // input: I0=[W3, W4] and I1=[W5, W6] // two integers on 2 bits
            // output = I0 + I1 // one integer on 3 bits
            Function::new(
                "two_bit_adder".to_string(),
                3,
                4,
                0,
                0,
                vec![
                    Xor(2, 4, 6),
                    And(7, 4, 6),
                    Xor(8, 3, 5),
                    Xor(1, 7, 8),
                    And(9, 3, 5),
                    Not(10, 9),
                    And(11, 8, 7),
                    Not(12, 11),
                    And(13, 10, 12),
                    Not(0, 13),
                    Free(7, Some(13)),
                ],
            ),
        ],
        gates: vec![
            // Read Witnesses
            For(
                "i".into(),
                0,
                2,
                vec![WireRange(0, 2)],
                ForLoopBody::IterExprAnonCall(
                    vec![Single(IterExprName("i".into()))],
                    vec![],
                    0,
                    1,
                    vec![Witness(0)],
                ),
            ),
            // Read Instances
            For(
                "i".into(),
                3,
                8,
                vec![WireRange(3, 8)],
                ForLoopBody::IterExprAnonCall(
                    vec![Single(IterExprName("i".into()))],
                    vec![],
                    1,
                    0,
                    vec![Instance(0)],
                ),
            ),
            // Fibonacci from Instances values
            For(
                "i".into(),
                0,
                3,
                vec![WireRange(9, 20)],
                ForLoopBody::IterExprCall(
                    "two_bit_adder".to_string(),
                    vec![Range(
                        IterExprAdd(
                            Box::new(IterExprMul(
                                Box::new(IterExprName("i".into())),
                                Box::new(IterExprConst(3)),
                            )),
                            Box::new(IterExprConst(9)),
                        ), // 3i+9
                        IterExprAdd(
                            Box::new(IterExprMul(
                                Box::new(IterExprName("i".into())),
                                Box::new(IterExprConst(3)),
                            )),
                            Box::new(IterExprConst(11)),
                        ), // 3i+11
                    )], // (3i+9)..=(3i+11)
                    vec![
                        Single(IterExprAdd(
                            Box::new(IterExprMul(
                                Box::new(IterExprName("i".into())),
                                Box::new(IterExprConst(3)),
                            )),
                            Box::new(IterExprConst(4)),
                        )), // 3i+4
                        Single(IterExprAdd(
                            Box::new(IterExprMul(
                                Box::new(IterExprName("i".into())),
                                Box::new(IterExprConst(3)),
                            )),
                            Box::new(IterExprConst(5)),
                        )), // 3i+5
                        Single(IterExprAdd(
                            Box::new(IterExprMul(
                                Box::new(IterExprName("i".into())),
                                Box::new(IterExprConst(3)),
                            )),
                            Box::new(IterExprConst(7)),
                        )), // 3i+7
                        Single(IterExprAdd(
                            Box::new(IterExprMul(
                                Box::new(IterExprName("i".into())),
                                Box::new(IterExprConst(3)),
                            )),
                            Box::new(IterExprConst(8)),
                        )), // 3i+8
                    ],
                ),
            ),
            Free(3, Some(17)),
            // Check that 5 = Fib_4 = Wire[18..20] = Witness[0..2]
            Xor(21, 18, 0),
            Xor(22, 19, 1),
            Xor(23, 20, 2),
            AssertZero(21),
            AssertZero(22),
            AssertZero(23),
            Free(0, Some(2)),
            Free(18, Some(23)),
            Witness(24),
            Witness(25),
            Switch(
                24,                     // condition
                wirelist![26],          // outputs
                vec![vec![1], vec![0]], //cases
                vec![
                    // branches
                    AbstractAnonCall(
                        vec![], // inputs
                        2,
                        0, // instance_count, witness_count
                        vec![
                            // subcircuit
                            Instance(1),
                            Instance(2),
                            Xor(0, 1, 2),
                        ],
                    ),
                    AbstractAnonCall(
                        vec![], // inputs
                        2,
                        0, // instance_count, witness_count
                        vec![
                            // subcircuit
                            Instance(1),
                            Instance(2),
                            And(0, 1, 2),
                        ],
                    ),
                ],
            ),
            Xor(27, 26, 25),
            AssertZero(27),
            Free(24, Some(27)),
        ],
    }
}
#[test]
fn test_boolean_examples() {
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
