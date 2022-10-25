use std::collections::HashMap;

use crate::structs::function::ForLoopBody;
use crate::structs::inputs::Inputs;
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
    Header::new(&[vec![2]])
}

pub fn example_instance_h(header: &Header) -> Instance {
    Instance {
        header: header.clone(),
        common_inputs: vec![Inputs {
            inputs: vec![
                vec![0],
                vec![0],
                vec![0], // 0 on 3 bits
                vec![0],
                vec![0],
                vec![1], // 1 on 3 bits
                vec![0],
                vec![1],
            ],
        }],
    }
}

pub fn example_witness_h(header: &Header) -> Witness {
    Witness {
        header: header.clone(),
        short_witness: vec![Inputs {
            inputs: vec![
                vec![1],
                vec![0],
                vec![1], // 5 on 3 bits
                vec![0],
                vec![0],
            ],
        }],
    }
}

pub fn example_witness_incorrect_h(header: &Header) -> Witness {
    Witness {
        header: header.clone(),
        short_witness: vec![Inputs {
            inputs: vec![
                vec![1],
                vec![1],
                vec![1], // 7 on 3 bits
                vec![0],
                vec![0],
            ],
        }],
    }
}

pub fn example_relation_h(header: &Header) -> Relation {
    use crate::structs::function::CaseInvoke::*;
    use crate::structs::function::Function;
    use crate::structs::iterators::{IterExprListElement::*, IterExprWireNumber::*};
    use crate::structs::wire::{WireListElement, WireListElement::*};
    use crate::wirelist;
    use crate::Gate::*;

    let field_id: u8 = 0;

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
                HashMap::from([(field_id, 3)]),
                HashMap::from([(field_id, 4)]),
                HashMap::new(),
                HashMap::new(),
                vec![
                    Xor(field_id, 2, 4, 6),
                    And(field_id, 7, 4, 6),
                    Xor(field_id, 8, 3, 5),
                    Xor(field_id, 1, 7, 8),
                    And(field_id, 9, 3, 5),
                    Not(field_id, 10, 9),
                    And(field_id, 11, 8, 7),
                    Not(field_id, 12, 11),
                    And(field_id, 13, 10, 12),
                    Not(field_id, 0, 13),
                    Free(field_id, 7, Some(13)),
                ],
            ),
        ],
        gates: vec![
            // Read Witnesses
            For(
                "i".into(),
                0,
                2,
                vec![WireRange(field_id, 0, 2)],
                ForLoopBody::IterExprAnonCall(
                    field_id,
                    vec![Single(IterExprName("i".into()))],
                    vec![],
                    HashMap::new(),
                    HashMap::from([(field_id, 1)]),
                    vec![Witness(field_id, 0)],
                ),
            ),
            // Read Instances
            For(
                "i".into(),
                3,
                8,
                vec![WireRange(field_id, 3, 8)],
                ForLoopBody::IterExprAnonCall(
                    field_id,
                    vec![Single(IterExprName("i".into()))],
                    vec![],
                    HashMap::from([(field_id, 1)]),
                    HashMap::new(),
                    vec![Instance(field_id, 0)],
                ),
            ),
            // Fibonacci from Instances values
            For(
                "i".into(),
                0,
                3,
                vec![WireRange(field_id, 9, 20)],
                ForLoopBody::IterExprCall(
                    "two_bit_adder".to_string(),
                    field_id,
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
            Free(field_id, 3, Some(17)),
            // Check that 5 = Fib_4 = Wire[18..20] = Witness[0..2]
            Xor(field_id, 21, 18, 0),
            Xor(field_id, 22, 19, 1),
            Xor(field_id, 23, 20, 2),
            AssertZero(field_id, 21),
            AssertZero(field_id, 22),
            AssertZero(field_id, 23),
            Free(field_id, 0, Some(2)),
            Free(field_id, 18, Some(23)),
            Witness(field_id, 24),
            Witness(field_id, 25),
            Switch(
                field_id,
                24,                     // condition
                wirelist![field_id;26], // outputs
                vec![vec![1], vec![0]], //cases
                vec![
                    // branches
                    AbstractAnonCall(
                        vec![],                         // inputs
                        HashMap::from([(field_id, 2)]), // instance_count
                        HashMap::new(),                 // witness_count
                        vec![
                            // subcircuit
                            Instance(field_id, 1),
                            Instance(field_id, 2),
                            Xor(field_id, 0, 1, 2),
                        ],
                    ),
                    AbstractAnonCall(
                        vec![],                         // inputs
                        HashMap::from([(field_id, 2)]), // instance_count
                        HashMap::new(),                 // witness_count
                        vec![
                            // subcircuit
                            Instance(field_id, 1),
                            Instance(field_id, 2),
                            And(field_id, 0, 1, 2),
                        ],
                    ),
                ],
            ),
            Xor(field_id, 27, 26, 25),
            AssertZero(field_id, 27),
            Free(field_id, 24, Some(27)),
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

#[test]
fn test_validator_on_boolean_example() -> crate::Result<()> {
    use crate::consumers::validator::Validator;

    let instance = example_instance();
    let witness = example_witness();
    let relation = example_relation();

    let mut validator = Validator::new_as_prover();

    validator.ingest_instance(&instance);
    validator.ingest_witness(&witness);
    validator.ingest_relation(&relation);

    assert_eq!(validator.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_evaluator_on_boolean_example() -> crate::Result<()> {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};

    let relation = example_relation();
    let instance = example_instance();
    let witness = example_witness();

    let mut zkbackend = PlaintextBackend::default();
    let mut simulator: Evaluator<PlaintextBackend> = Evaluator::default();

    simulator.ingest_instance(&instance)?;
    simulator.ingest_witness(&witness)?;
    simulator.ingest_relation(&relation, &mut zkbackend)?;

    assert_eq!(simulator.get_violations(), Vec::<String>::new());

    Ok(())
}
