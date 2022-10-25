use std::collections::HashMap;

use crate::producers::examples::literal32;
use crate::structs::function::ForLoopBody;
use crate::structs::inputs::Inputs;
use crate::structs::wire::WireListElement;
use crate::wirelist;
use crate::{Header, Instance, Relation, Witness};

pub fn example_header_with_several_fields() -> Header {
    Header::new(&[literal32(101), literal32(7)])
}

pub fn example_instance_with_several_fields() -> Instance {
    Instance {
        header: example_header_with_several_fields(),
        common_inputs: vec![
            Inputs {
                inputs: vec![vec![25], vec![0], vec![1]],
            },
            Inputs {
                inputs: vec![vec![6], vec![1], vec![0]],
            },
        ],
    }
}

pub fn example_witness_with_several_fields() -> Witness {
    Witness {
        header: example_header_with_several_fields(),
        short_witness: vec![
            Inputs {
                inputs: vec![
                    vec![3],
                    vec![4],
                    vec![0],
                    literal32(17711 % 101), // Fibonacci 22
                ],
            },
            Inputs {
                inputs: vec![vec![4], vec![2], vec![3]],
            },
        ],
    }
}

pub fn example_incorrect_witness_with_several_fields() -> Witness {
    Witness {
        header: example_header_with_several_fields(),
        short_witness: vec![
            Inputs {
                inputs: vec![
                    vec![3],
                    vec![4],
                    vec![0],
                    vec![54], // not Fibonacci 22
                ],
            },
            Inputs {
                inputs: vec![vec![4], vec![2], vec![3]],
            },
        ],
    }
}

pub fn example_relation_with_several_fields() -> Relation {
    use crate::structs::function::Function;
    use crate::structs::iterators::{IterExprListElement::*, IterExprWireNumber::*};
    use crate::structs::wire::WireListElement::*;
    use crate::Gate::*;

    let field_id_101: u8 = 0;
    let field_id_7: u8 = 1;
    Relation {
        header: example_header_with_several_fields(),
        functions: vec![Function::new(
            "com.example::mul".to_string(),
            HashMap::from([(field_id_101, 1)]),
            HashMap::from([(field_id_101, 2)]),
            HashMap::new(),
            HashMap::new(),
            vec![Mul(field_id_101, 0, 1, 2)],
        )],
        gates: vec![
            Witness(field_id_101, 1),
            AnonCall(
                wirelist![field_id_101;0, 2, 4, 5, 6, 9, 10, 11], // output
                wirelist![field_id_101;1],                        // input
                HashMap::from([(field_id_101, 3)]),               // instance count
                HashMap::from([(field_id_101, 3)]),               // witness count
                vec![
                    Instance(field_id_101, 0), // In Global Namespace: Instance(0)
                    Witness(field_id_101, 1),  // In Global Namespace: Witness(2)
                    Call(
                        "com.example::mul".to_string(),
                        wirelist![field_id_101;2],
                        wirelist![field_id_101;8; 2],
                    ), // In Global Namespace: Mul(4, 1, 1)
                    Call(
                        "com.example::mul".to_string(),
                        wirelist![field_id_101;3],
                        wirelist![field_id_101;1; 2],
                    ), // In Global Namespace: Mul(5, 2, 2)
                    Add(field_id_101, 4, 2, 3), // In Global Namespace: Add(6, 4, 5)
                    Witness(field_id_101, 9),
                    AssertZero(field_id_101, 9), // This witness is indeed zero, so check that in a branch.
                    Instance(field_id_101, 6),
                    AssertZero(field_id_101, 6),
                    Instance(field_id_101, 7),
                    Witness(field_id_101, 5),
                ],
            ),
            Constant(field_id_101, 3, literal32(100)), // -1
            Call(
                "com.example::mul".to_string(),
                wirelist![field_id_101;7],
                wirelist![field_id_101;3, 0],
            ), // - instance_0
            Add(field_id_101, 8, 6, 7),                // sum - instance_0
            Free(field_id_101, 0, Some(7)),            // Free all previous wires
            AssertZero(field_id_101, 8),               // difference == 0
            For(
                "i".into(),
                0,
                20,
                vec![WireRange(field_id_101, 12, 32)],
                ForLoopBody::IterExprAnonCall(
                    field_id_101,
                    vec![Single(IterExprAdd(
                        Box::new(IterExprName("i".into())),
                        Box::new(IterExprConst(12)),
                    ))], // i + 12
                    vec![
                        Single(IterExprAdd(
                            Box::new(IterExprName("i".into())),
                            Box::new(IterExprConst(10)),
                        )),
                        Single(IterExprAdd(
                            Box::new(IterExprName("i".into())),
                            Box::new(IterExprConst(11)),
                        )),
                    ],
                    HashMap::new(),
                    HashMap::new(),
                    vec![Add(field_id_101, 0, 1, 2)],
                ),
            ),
            MulConstant(field_id_101, 33, 32, literal32(100)), // multiply by -1
            Add(field_id_101, 34, 9, 33),
            AssertZero(field_id_101, 34),
            // second useless loop that uses the same loop iterator
            For(
                "i".into(),
                35,
                50,
                vec![WireRange(field_id_101, 35, 50)],
                ForLoopBody::IterExprCall(
                    "com.example::mul".to_string(),
                    field_id_101,
                    vec![Single(IterExprName("i".into()))], // i
                    vec![
                        Single(IterExprSub(
                            Box::new(IterExprName("i".into())),
                            Box::new(IterExprConst(1)),
                        )),
                        Single(IterExprSub(
                            Box::new(IterExprName("i".into())),
                            Box::new(IterExprConst(2)),
                        )),
                    ],
                ),
            ),
            Free(field_id_101, 8, Some(50)),
            // Read Instances
            For(
                "i".into(),
                0,
                2,
                vec![WireRange(field_id_7, 0, 2)],
                ForLoopBody::IterExprAnonCall(
                    field_id_7,
                    vec![Single(IterExprName("i".into()))],
                    vec![],
                    HashMap::from([(field_id_7, 1)]),
                    HashMap::new(),
                    vec![Instance(field_id_7, 0)],
                ),
            ),
            // Read Witnesses
            For(
                "i".into(),
                3,
                5,
                vec![WireRange(field_id_7, 3, 5)],
                ForLoopBody::IterExprAnonCall(
                    field_id_7,
                    vec![Single(IterExprName("i".into()))],
                    vec![],
                    HashMap::new(),
                    HashMap::from([(field_id_7, 1)]),
                    vec![Witness(field_id_7, 0)],
                ),
            ),
            Add(field_id_7, 6, 0, 3), // 6 + 4 = 3 mod 7
            Mul(field_id_7, 7, 1, 4), // 1 * 2 = 2 mod 7
            Mul(field_id_7, 8, 2, 5), // 0 * 3 = 0 mod 7
            AssertZero(field_id_7, 8),
            Free(field_id_7, 0, Some(5)),
            Mul(field_id_7, 9, 6, 7),                     // 3 * 2 = 6 mod 7
            AddConstant(field_id_7, 10, 9, literal32(1)), // 6 + 1 = 0 mod 7
            AssertZero(field_id_7, 10),
            // Conversion from a big field to a small field
            Convert(
                wirelist![field_id_101; 51,52],
                wirelist![field_id_7;8, 7, 6], // [0 , 2, 3]_7 = 2 * 7 + 3 = 17
            ),
            AddConstant(field_id_101, 53, 52, literal32(84)),
            AssertZero(field_id_101, 53),
            AssertZero(field_id_101, 51),
            Free(field_id_7, 6, Some(10)),
            Free(field_id_101, 51, Some(53)),
            // Conversion from a small field to a big field
            Constant(field_id_101, 54, vec![9]),
            Convert(
                wirelist![field_id_7; 11, 12, 13], // [0, 1, 2]_7 (1 * 7 + 2 = 9)
                wirelist![field_id_101;54],        // [9]_101
            ),
            AddConstant(field_id_7, 14, 13, vec![5]), // 2 + 5 = 0 mod 7
            AddConstant(field_id_7, 15, 12, vec![6]), // 1 + 6 = 0 mod 7
            AssertZero(field_id_7, 11),
            AssertZero(field_id_7, 14),
            AssertZero(field_id_7, 15),
            Free(field_id_101, 54, None),
            Free(field_id_7, 11, Some(15)),
        ],
    }
}

#[test]
fn test_examples_with_several_fields() {
    use crate::Source;

    let mut common_buf = Vec::<u8>::new();
    example_instance_with_several_fields()
        .write_into(&mut common_buf)
        .unwrap();
    example_relation_with_several_fields()
        .write_into(&mut common_buf)
        .unwrap();

    let mut prover_buf = Vec::<u8>::new();
    example_witness_with_several_fields()
        .write_into(&mut prover_buf)
        .unwrap();

    let source = Source::from_buffers(vec![common_buf, prover_buf]);
    let messages = source.read_all_messages().unwrap();
    assert_eq!(
        messages.relations,
        vec![example_relation_with_several_fields()]
    );
    assert_eq!(
        messages.instances,
        vec![example_instance_with_several_fields()]
    );
    assert_eq!(
        messages.witnesses,
        vec![example_witness_with_several_fields()]
    );
}

#[test]
fn test_validator_with_several_fields() -> crate::Result<()> {
    use crate::consumers::validator::Validator;

    let instance = example_instance_with_several_fields();
    let witness = example_witness_with_several_fields();
    let relation = example_relation_with_several_fields();

    let mut validator = Validator::new_as_prover();

    validator.ingest_instance(&instance);
    validator.ingest_witness(&witness);
    validator.ingest_relation(&relation);

    assert_eq!(validator.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_validator_with_incorrect_convert_gates() -> crate::Result<()> {
    use crate::consumers::validator::Validator;
    use crate::Gate::*;

    let instance = example_instance_with_several_fields();
    let witness = example_witness_with_several_fields();

    let field_id_101: u8 = 0;
    let field_id_7: u8 = 1;
    let relation = Relation {
        header: example_header_with_several_fields(),
        functions: vec![],
        gates: vec![
            Instance(field_id_101, 0),
            Witness(field_id_101, 1),
            Instance(field_id_7, 0),
            // Error: in a convert gate, all inputs should belong to the same field
            Convert(
                wirelist![field_id_7; 1, 2],
                vec![
                    WireListElement::Wire(field_id_101, 0),
                    WireListElement::Wire(field_id_7, 0),
                ],
            ),
            // Error: in a convert gate, all outputs should belong to the same field
            Convert(
                vec![
                    WireListElement::Wire(field_id_101, 2),
                    WireListElement::Wire(field_id_7, 3),
                ],
                wirelist![field_id_7; 0],
            ),
            // Error: in a convert gate, input should not be empty
            Convert(wirelist![field_id_7; 4,5], vec![]),
            // Error: in a convert gate, output should not be empty
            Convert(vec![], wirelist![field_id_7; 0]),
        ],
    };

    let mut validator = Validator::new_as_prover();

    validator.ingest_instance(&instance);
    validator.ingest_witness(&witness);
    validator.ingest_relation(&relation);

    assert_eq!(
        validator.get_violations(),
        vec![
            "Gate::Convert: Error with input wires: Several fields",
            "Gate::Convert: Error with output wires: Several fields",
            "Gate::Convert: Error with input wires: Empty wirelist",
            "Gate::Convert: Error with output wires: Empty wirelist",
            "Too many Instance values (4 not consumed)",
            "Too many Witness values (6 not consumed)"
        ]
    );

    Ok(())
}

#[test]
fn test_evaluator_with_several_fields() -> crate::Result<()> {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};

    let relation = example_relation_with_several_fields();
    let instance = example_instance_with_several_fields();
    let witness = example_witness_with_several_fields();

    let mut zkbackend = PlaintextBackend::default();
    let mut simulator: Evaluator<PlaintextBackend> = Evaluator::default();

    simulator.ingest_instance(&instance)?;
    simulator.ingest_witness(&witness)?;
    simulator.ingest_relation(&relation, &mut zkbackend)?;

    assert_eq!(simulator.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_evaluator_with_incorrect_convert_gates() -> crate::Result<()> {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};
    use crate::Gate::*;

    let instance = example_instance_with_several_fields();
    let witness = example_witness_with_several_fields();

    let field_id_101: u8 = 0;
    let field_id_7: u8 = 1;

    // 1st test: impossible conversion
    let relation = Relation {
        header: example_header_with_several_fields(),
        functions: vec![],
        gates: vec![
            Instance(field_id_101, 0), // 25
            Witness(field_id_101, 1),  // 3
            // The following conversion is impossible
            Convert(wirelist![field_id_7; 0, 1], wirelist![field_id_101; 0, 1]),
            Free(field_id_101, 0, Some(1)),
            Free(field_id_7, 0, Some(1)),
        ],
    };

    let mut zkbackend = PlaintextBackend::default();
    let mut simulator: Evaluator<PlaintextBackend> = Evaluator::default();

    simulator.ingest_instance(&instance)?;
    simulator.ingest_witness(&witness)?;
    let should_be_err = simulator.ingest_relation(&relation, &mut zkbackend);
    assert!(should_be_err.is_err());
    assert_eq!(
        "Impossible conversion",
        should_be_err.err().unwrap().to_string()
    );

    // 2nd test: all input wires do not belong to the same field
    let relation = Relation {
        header: example_header_with_several_fields(),
        functions: vec![],
        gates: vec![
            Instance(field_id_101, 0), // 25
            Instance(field_id_7, 0),   // 6
            // Error: in the following convert gate, all input wires do not belong to the same field
            Convert(
                wirelist![field_id_101; 1, 2],
                vec![
                    WireListElement::Wire(field_id_101, 0),
                    WireListElement::Wire(field_id_7, 0),
                ],
            ),
        ],
    };

    let mut zkbackend = PlaintextBackend::default();
    let mut simulator: Evaluator<PlaintextBackend> = Evaluator::default();

    simulator.ingest_instance(&instance)?;
    simulator.ingest_witness(&witness)?;
    let should_be_err = simulator.ingest_relation(&relation, &mut zkbackend);
    assert!(should_be_err.is_err());
    assert_eq!(
        "Error with input wires of a Convert gate: Several fields",
        should_be_err.err().unwrap().to_string()
    );

    Ok(())
}
