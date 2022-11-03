use std::collections::HashMap;

use crate::producers::examples::literal32;
use crate::structs::inputs::Inputs;
use crate::structs::plugin::PluginBody;
use crate::structs::wire::WireListElement;
use crate::wirelist;
use crate::{Header, PrivateInputs, PublicInputs, Relation};

pub fn example_header_with_several_types() -> Header {
    Header::new(&[literal32(101), literal32(7)])
}

pub fn example_public_inputs_with_several_types() -> PublicInputs {
    PublicInputs {
        header: example_header_with_several_types(),
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

pub fn example_private_inputs_with_several_types() -> PrivateInputs {
    PrivateInputs {
        header: example_header_with_several_types(),
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

pub fn example_incorrect_private_inputs_with_several_types() -> PrivateInputs {
    PrivateInputs {
        header: example_header_with_several_types(),
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

pub fn example_relation_with_several_types() -> Relation {
    use crate::structs::function::{Function, FunctionBody};
    use crate::Gate::*;

    let type_id_101: u8 = 0;
    let type_id_7: u8 = 1;
    Relation {
        header: example_header_with_several_types(),
        plugins: vec!["vector".to_string()],
        functions: vec![
            Function::new(
                "com.example::mul".to_string(),
                HashMap::from([(type_id_101, 1)]),
                HashMap::from([(type_id_101, 2)]),
                HashMap::new(),
                HashMap::new(),
                FunctionBody::Gates(vec![Mul(type_id_101, 0, 1, 2)]),
            ),
            Function::new(
                "vector_add_7_3".to_string(),
                HashMap::from([(type_id_7, 3)]),
                HashMap::from([(type_id_7, 6)]),
                HashMap::new(),
                HashMap::new(),
                FunctionBody::PluginBody(PluginBody {
                    name: "vector".to_string(),
                    operation: "add".to_string(),
                    params: vec![type_id_7.to_string(), "3".to_string()],
                }),
            ),
        ],
        gates: vec![
            New(type_id_101, 0, 7),
            PrivateInput(type_id_101, 1),
            AnonCall(
                wirelist![type_id_101;0, 2, 4, 5, 6, 9, 10], // output
                wirelist![type_id_101;1],                    // input
                HashMap::from([(type_id_101, 3)]),           // public count
                HashMap::from([(type_id_101, 2)]),           // private count
                vec![
                    PublicInput(type_id_101, 0),
                    PrivateInput(type_id_101, 1),
                    Call(
                        "com.example::mul".to_string(),
                        wirelist![type_id_101;2],
                        wirelist![type_id_101;7; 2],
                    ),
                    Call(
                        "com.example::mul".to_string(),
                        wirelist![type_id_101;3],
                        wirelist![type_id_101;1; 2],
                    ),
                    Add(type_id_101, 4, 2, 3),
                    PrivateInput(type_id_101, 8),
                    AssertZero(type_id_101, 8),
                    PublicInput(type_id_101, 5),
                    AssertZero(type_id_101, 5),
                    PublicInput(type_id_101, 6),
                ],
            ),
            Constant(type_id_101, 3, literal32(100)), // -1
            Call(
                "com.example::mul".to_string(),
                wirelist![type_id_101;7],
                wirelist![type_id_101;3, 0],
            ), // - public_input_0
            Add(type_id_101, 8, 6, 7),
            Delete(type_id_101, 0, Some(7)),
            Mul(type_id_101, 11, 8, 10),
            AssertZero(type_id_101, 11),
            Delete(type_id_101, 8, Some(11)),
            New(type_id_101, 0, 5),
            // Read PublicInputs
            PublicInput(type_id_7, 0),
            PublicInput(type_id_7, 1),
            PublicInput(type_id_7, 2),
            // Read PrivateInputs
            PrivateInput(type_id_7, 3),
            PrivateInput(type_id_7, 4),
            PrivateInput(type_id_7, 5),
            Add(type_id_7, 6, 0, 3), // 6 + 4 = 3 mod 7
            Mul(type_id_7, 7, 1, 4), // 1 * 2 = 2 mod 7
            Mul(type_id_7, 8, 2, 5), // 0 * 3 = 0 mod 7
            AssertZero(type_id_7, 8),
            Delete(type_id_7, 0, Some(5)),
            Mul(type_id_7, 9, 6, 7),                     // 3 * 2 = 6 mod 7
            AddConstant(type_id_7, 10, 9, literal32(1)), // 6 + 1 = 0 mod 7
            AssertZero(type_id_7, 10),
            // Conversion from a big modulo to a small modulo
            Convert(
                wirelist![type_id_101; 12,13],
                wirelist![type_id_7;8, 7, 6], // [0 , 2, 3]_7 = 2 * 7 + 3 = 17
            ),
            AddConstant(type_id_101, 14, 13, literal32(84)),
            AssertZero(type_id_101, 14),
            AssertZero(type_id_101, 12),
            Delete(type_id_7, 6, Some(10)),
            Delete(type_id_101, 12, Some(14)),
            // Conversion from a small modulo to a big modulo
            Constant(type_id_101, 15, vec![9]),
            Convert(
                wirelist![type_id_7; 11, 12, 13], // [0, 1, 2]_7 (1 * 7 + 2 = 9)
                wirelist![type_id_101;15],        // [9]_101
            ),
            AddConstant(type_id_7, 14, 13, vec![5]), // 2 + 5 = 0 mod 7
            AddConstant(type_id_7, 15, 12, vec![6]), // 1 + 6 = 0 mod 7
            AssertZero(type_id_7, 11),
            AssertZero(type_id_7, 14),
            AssertZero(type_id_7, 15),
            Delete(type_id_101, 15, None),
            Delete(type_id_7, 11, Some(15)),
            // Test plugin(vector_add, 1, 3)
            Constant(type_id_7, 16, vec![1]),
            Constant(type_id_7, 17, vec![2]),
            Constant(type_id_7, 18, vec![3]),
            Constant(type_id_7, 19, vec![4]),
            Constant(type_id_7, 20, vec![5]),
            Constant(type_id_7, 21, vec![6]),
            Call(
                "vector_add_7_3".to_string(),
                vec![WireListElement::WireRange(type_id_7, 22, 24)], // [5, 0, 2] = [1, 2, 3] + [4, 5, 6] % 7
                vec![WireListElement::WireRange(type_id_7, 16, 21)],
            ),
            Add(type_id_7, 25, 22, 24), // 0 = 5 + 2 % 7
            AssertZero(type_id_7, 23),
            AssertZero(type_id_7, 25),
            Delete(type_id_7, 16, Some(25)),
        ],
    }
}

#[test]
fn test_examples_with_several_types() {
    use crate::Source;

    let mut common_buf = Vec::<u8>::new();
    example_public_inputs_with_several_types()
        .write_into(&mut common_buf)
        .unwrap();
    example_relation_with_several_types()
        .write_into(&mut common_buf)
        .unwrap();

    let mut prover_buf = Vec::<u8>::new();
    example_private_inputs_with_several_types()
        .write_into(&mut prover_buf)
        .unwrap();

    let source = Source::from_buffers(vec![common_buf, prover_buf]);
    let messages = source.read_all_messages().unwrap();
    assert_eq!(
        messages.relations,
        vec![example_relation_with_several_types()]
    );
    assert_eq!(
        messages.public_inputs,
        vec![example_public_inputs_with_several_types()]
    );
    assert_eq!(
        messages.private_inputs,
        vec![example_private_inputs_with_several_types()]
    );
}

#[test]
fn test_validator_with_several_types() -> crate::Result<()> {
    use crate::consumers::validator::Validator;

    let public_inputs = example_public_inputs_with_several_types();
    let private_inputs = example_private_inputs_with_several_types();
    let relation = example_relation_with_several_types();

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

    let public_inputs = example_public_inputs_with_several_types();
    let private_inputs = example_private_inputs_with_several_types();

    let type_id_101: u8 = 0;
    let type_id_7: u8 = 1;
    let relation = Relation {
        header: example_header_with_several_types(),
        plugins: vec![],
        functions: vec![],
        gates: vec![
            PublicInput(type_id_101, 0),
            PrivateInput(type_id_101, 1),
            PublicInput(type_id_7, 0),
            // Error: in a convert gate, all inputs should belong to the same type
            Convert(
                wirelist![type_id_7; 1, 2],
                vec![
                    WireListElement::Wire(type_id_101, 0),
                    WireListElement::Wire(type_id_7, 0),
                ],
            ),
            // Error: in a convert gate, all outputs should belong to the same type
            Convert(
                vec![
                    WireListElement::Wire(type_id_101, 2),
                    WireListElement::Wire(type_id_7, 3),
                ],
                wirelist![type_id_7; 0],
            ),
            // Error: in a convert gate, input should not be empty
            Convert(wirelist![type_id_7; 4,5], vec![]),
            // Error: in a convert gate, output should not be empty
            Convert(vec![], wirelist![type_id_7; 0]),
        ],
    };

    let mut validator = Validator::new_as_prover();

    validator.ingest_public_inputs(&public_inputs);
    validator.ingest_private_inputs(&private_inputs);
    validator.ingest_relation(&relation);

    assert_eq!(
        validator.get_violations(),
        vec![
            "Gate::Convert: Error with input wires: Several types",
            "Gate::Convert: Error with output wires: Several types",
            "Gate::Convert: Error with input wires: Empty wirelist",
            "Gate::Convert: Error with output wires: Empty wirelist",
            "Too many public input values (4 not consumed)",
            "Too many private input values (5 not consumed)"
        ]
    );

    Ok(())
}

#[test]
fn test_evaluator_with_several_types() -> crate::Result<()> {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};

    let relation = example_relation_with_several_types();
    let public_inputs = example_public_inputs_with_several_types();
    let private_inputs = example_private_inputs_with_several_types();

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

    let public_inputs = example_public_inputs_with_several_types();
    let private_inputs = example_private_inputs_with_several_types();

    let type_id_101: u8 = 0;
    let type_id_7: u8 = 1;

    // 1st test: impossible conversion
    let relation = Relation {
        header: example_header_with_several_types(),
        plugins: vec![],
        functions: vec![],
        gates: vec![
            PublicInput(type_id_101, 0),  // 25
            PrivateInput(type_id_101, 1), // 3
            // The following conversion is impossible
            Convert(wirelist![type_id_7; 0, 1], wirelist![type_id_101; 0, 1]),
            Delete(type_id_101, 0, Some(1)),
            Delete(type_id_7, 0, Some(1)),
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

    // 2nd test: all input wires do not belong to the same type
    let relation = Relation {
        header: example_header_with_several_types(),
        plugins: vec![],
        functions: vec![],
        gates: vec![
            PublicInput(type_id_101, 0), // 25
            PublicInput(type_id_7, 0),   // 6
            // Error: in the following convert gate, all input wires do not belong to the same type
            Convert(
                wirelist![type_id_101; 1, 2],
                vec![
                    WireListElement::Wire(type_id_101, 0),
                    WireListElement::Wire(type_id_7, 0),
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
        "Error with input wires of a Convert gate: Several types",
        should_be_err.err().unwrap().to_string()
    );

    Ok(())
}
