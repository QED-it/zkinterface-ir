use std::collections::HashMap;

use crate::producers::examples::literal32;
use crate::structs::inputs::Inputs;
use crate::structs::wire::WireListElement;
use crate::wirelist;
use crate::{Header, PrivateInputs, PublicInputs, Relation};

pub fn example_header_with_several_fields() -> Header {
    Header::new(&[literal32(101), literal32(7)])
}

pub fn example_public_inputs_with_several_fields() -> PublicInputs {
    PublicInputs {
        header: example_header_with_several_fields(),
        inputs: vec![
            Inputs {
                values: vec![vec![25], vec![0], vec![1]],
            },
            Inputs {
                values: vec![vec![6], vec![1], vec![0]],
            },
        ],
    }
}

pub fn example_private_inputs_with_several_fields() -> PrivateInputs {
    PrivateInputs {
        header: example_header_with_several_fields(),
        inputs: vec![
            Inputs {
                values: vec![vec![3], vec![4], vec![0]],
            },
            Inputs {
                values: vec![vec![4], vec![2], vec![3]],
            },
        ],
    }
}

pub fn example_incorrect_private_inputs_with_several_fields() -> PrivateInputs {
    PrivateInputs {
        header: example_header_with_several_fields(),
        inputs: vec![
            Inputs {
                values: vec![
                    vec![3],
                    vec![5], // incorrect
                    vec![0],
                ],
            },
            Inputs {
                values: vec![vec![4], vec![2], vec![3]],
            },
        ],
    }
}

pub fn example_relation_with_several_fields() -> Relation {
    use crate::structs::function::Function;
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
            PrivateInput(field_id_101, 1),
            AnonCall(
                wirelist![field_id_101;0, 2, 4, 5, 6, 9, 10], // output
                wirelist![field_id_101;1],                    // input
                HashMap::from([(field_id_101, 3)]),           // public count
                HashMap::from([(field_id_101, 2)]),           // private count
                vec![
                    PublicInput(field_id_101, 0),
                    PrivateInput(field_id_101, 1),
                    Call(
                        "com.example::mul".to_string(),
                        wirelist![field_id_101;2],
                        wirelist![field_id_101;7; 2],
                    ),
                    Call(
                        "com.example::mul".to_string(),
                        wirelist![field_id_101;3],
                        wirelist![field_id_101;1; 2],
                    ),
                    Add(field_id_101, 4, 2, 3),
                    PrivateInput(field_id_101, 8),
                    AssertZero(field_id_101, 8),
                    PublicInput(field_id_101, 5),
                    AssertZero(field_id_101, 5),
                    PublicInput(field_id_101, 6),
                ],
            ),
            Constant(field_id_101, 3, literal32(100)), // -1
            Call(
                "com.example::mul".to_string(),
                wirelist![field_id_101;7],
                wirelist![field_id_101;3, 0],
            ), // - public_input_0
            Add(field_id_101, 8, 6, 7),
            Delete(field_id_101, 0, Some(7)),
            Mul(field_id_101, 11, 8, 10),
            AssertZero(field_id_101, 11),
            Delete(field_id_101, 8, Some(11)),
            // Read PublicInputs
            PublicInput(field_id_7, 0),
            PublicInput(field_id_7, 1),
            PublicInput(field_id_7, 2),
            // Read PrivateInputs
            PrivateInput(field_id_7, 3),
            PrivateInput(field_id_7, 4),
            PrivateInput(field_id_7, 5),
            Add(field_id_7, 6, 0, 3), // 6 + 4 = 3 mod 7
            Mul(field_id_7, 7, 1, 4), // 1 * 2 = 2 mod 7
            Mul(field_id_7, 8, 2, 5), // 0 * 3 = 0 mod 7
            AssertZero(field_id_7, 8),
            Delete(field_id_7, 0, Some(5)),
            Mul(field_id_7, 9, 6, 7),                     // 3 * 2 = 6 mod 7
            AddConstant(field_id_7, 10, 9, literal32(1)), // 6 + 1 = 0 mod 7
            AssertZero(field_id_7, 10),
            // Conversion from a big field to a small field
            Convert(
                wirelist![field_id_101; 12,13],
                wirelist![field_id_7;8, 7, 6], // [0 , 2, 3]_7 = 2 * 7 + 3 = 17
            ),
            AddConstant(field_id_101, 14, 13, literal32(84)),
            AssertZero(field_id_101, 14),
            AssertZero(field_id_101, 12),
            Delete(field_id_7, 6, Some(10)),
            Delete(field_id_101, 12, Some(14)),
            // Conversion from a small field to a big field
            Constant(field_id_101, 15, vec![9]),
            Convert(
                wirelist![field_id_7; 11, 12, 13], // [0, 1, 2]_7 (1 * 7 + 2 = 9)
                wirelist![field_id_101;15],        // [9]_101
            ),
            AddConstant(field_id_7, 14, 13, vec![5]), // 2 + 5 = 0 mod 7
            AddConstant(field_id_7, 15, 12, vec![6]), // 1 + 6 = 0 mod 7
            AssertZero(field_id_7, 11),
            AssertZero(field_id_7, 14),
            AssertZero(field_id_7, 15),
            Delete(field_id_101, 15, None),
            Delete(field_id_7, 11, Some(15)),
        ],
    }
}

#[test]
fn test_examples_with_several_fields() {
    use crate::Source;

    let mut common_buf = Vec::<u8>::new();
    example_public_inputs_with_several_fields()
        .write_into(&mut common_buf)
        .unwrap();
    example_relation_with_several_fields()
        .write_into(&mut common_buf)
        .unwrap();

    let mut prover_buf = Vec::<u8>::new();
    example_private_inputs_with_several_fields()
        .write_into(&mut prover_buf)
        .unwrap();

    let source = Source::from_buffers(vec![common_buf, prover_buf]);
    let messages = source.read_all_messages().unwrap();
    assert_eq!(
        messages.relations,
        vec![example_relation_with_several_fields()]
    );
    assert_eq!(
        messages.public_inputs,
        vec![example_public_inputs_with_several_fields()]
    );
    assert_eq!(
        messages.private_inputs,
        vec![example_private_inputs_with_several_fields()]
    );
}

#[test]
fn test_validator_with_several_fields() -> crate::Result<()> {
    use crate::consumers::validator::Validator;

    let public_inputs = example_public_inputs_with_several_fields();
    let private_inputs = example_private_inputs_with_several_fields();
    let relation = example_relation_with_several_fields();

    let mut validator = Validator::new_as_prover();

    validator.ingest_public_inputs(&public_inputs);
    validator.ingest_private_inputs(&private_inputs);
    validator.ingest_relation(&relation);

    assert_eq!(validator.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_validator_with_incorrect_convert_gates() -> crate::Result<()> {
    use crate::consumers::validator::Validator;
    use crate::Gate::*;

    let public_inputs = example_public_inputs_with_several_fields();
    let private_inputs = example_private_inputs_with_several_fields();

    let field_id_101: u8 = 0;
    let field_id_7: u8 = 1;
    let relation = Relation {
        header: example_header_with_several_fields(),
        functions: vec![],
        gates: vec![
            PublicInput(field_id_101, 0),
            PrivateInput(field_id_101, 1),
            PublicInput(field_id_7, 0),
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

    validator.ingest_public_inputs(&public_inputs);
    validator.ingest_private_inputs(&private_inputs);
    validator.ingest_relation(&relation);

    assert_eq!(
        validator.get_violations(),
        vec![
            "Gate::Convert: Error with input wires: Several fields",
            "Gate::Convert: Error with output wires: Several fields",
            "Gate::Convert: Error with input wires: Empty wirelist",
            "Gate::Convert: Error with output wires: Empty wirelist",
            "Too many public input values (4 not consumed)",
            "Too many private input values (5 not consumed)"
        ]
    );

    Ok(())
}

#[test]
fn test_evaluator_with_several_fields() -> crate::Result<()> {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};

    let relation = example_relation_with_several_fields();
    let public_inputs = example_public_inputs_with_several_fields();
    let private_inputs = example_private_inputs_with_several_fields();

    let mut zkbackend = PlaintextBackend::default();
    let mut simulator: Evaluator<PlaintextBackend> = Evaluator::default();

    simulator.ingest_public_inputs(&public_inputs)?;
    simulator.ingest_private_inputs(&private_inputs)?;
    simulator.ingest_relation(&relation, &mut zkbackend)?;

    assert_eq!(simulator.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_evaluator_with_incorrect_convert_gates() -> crate::Result<()> {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};
    use crate::Gate::*;

    let public_inputs = example_public_inputs_with_several_fields();
    let private_inputs = example_private_inputs_with_several_fields();

    let field_id_101: u8 = 0;
    let field_id_7: u8 = 1;

    // 1st test: impossible conversion
    let relation = Relation {
        header: example_header_with_several_fields(),
        functions: vec![],
        gates: vec![
            PublicInput(field_id_101, 0),  // 25
            PrivateInput(field_id_101, 1), // 3
            // The following conversion is impossible
            Convert(wirelist![field_id_7; 0, 1], wirelist![field_id_101; 0, 1]),
            Delete(field_id_101, 0, Some(1)),
            Delete(field_id_7, 0, Some(1)),
        ],
    };

    let mut zkbackend = PlaintextBackend::default();
    let mut simulator: Evaluator<PlaintextBackend> = Evaluator::default();

    simulator.ingest_public_inputs(&public_inputs)?;
    simulator.ingest_private_inputs(&private_inputs)?;
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
            PublicInput(field_id_101, 0), // 25
            PublicInput(field_id_7, 0),   // 6
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

    simulator.ingest_public_inputs(&public_inputs)?;
    simulator.ingest_private_inputs(&private_inputs)?;
    let should_be_err = simulator.ingest_relation(&relation, &mut zkbackend);
    assert!(should_be_err.is_err());
    assert_eq!(
        "Error with input wires of a Convert gate: Several fields",
        should_be_err.err().unwrap().to_string()
    );

    Ok(())
}
