use crate::producers::examples::literal32;
use crate::structs::inputs::Inputs;
use crate::structs::plugin::PluginBody;
use crate::structs::wire::WireListElement;
use crate::{Header, PrivateInputs, PublicInputs, Relation};
use std::collections::HashMap;

pub fn example_header_with_several_types() -> Header {
    Header::new(&[literal32(7), literal32(101)])
}

pub fn example_public_inputs_with_several_types() -> PublicInputs {
    PublicInputs {
        header: example_header_with_several_types(),
        inputs: vec![
            Inputs {
                values: vec![vec![5]],
            },
            Inputs { values: vec![] },
        ],
    }
}

pub fn example_private_inputs_with_several_types() -> PrivateInputs {
    PrivateInputs {
        header: example_header_with_several_types(),
        inputs: vec![
            Inputs {
                values: vec![vec![3], vec![4]],
            },
            Inputs { values: vec![] },
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
                ],
            },
            Inputs { values: vec![] },
        ],
    }
}

pub fn example_relation_with_several_types() -> Relation {
    use crate::structs::count::Count;
    use crate::structs::function::{Function, FunctionBody};
    use crate::Gate::*;

    let type_id_7: u8 = 0;
    let type_id_101: u8 = 1;
    Relation {
        header: example_header_with_several_types(),
        plugins: vec!["vector".to_string()],
        functions: vec![
            Function::new(
                "square".to_string(),
                vec![Count::new(type_id_101, 1)],
                vec![Count::new(type_id_101, 1)],
                FunctionBody::Gates(vec![Mul(type_id_101, 0, 1, 1)]),
            ),
            Function::new(
                "vector_mul_7_2".to_string(),
                vec![Count::new(type_id_101, 2)],
                vec![Count::new(type_id_101, 2), Count::new(type_id_101, 2)],
                FunctionBody::PluginBody(PluginBody {
                    name: "vector".to_string(),
                    operation: "mul".to_string(),
                    params: vec![type_id_101.to_string(), "2".to_string()],
                    public_count: HashMap::new(),
                    private_count: HashMap::new(),
                }),
            ),
        ],
        gates: vec![
            // Right-triangle example
            New(type_id_101, 0, 8),
            PublicInput(type_id_7, 0),
            PrivateInput(type_id_7, 1),
            PrivateInput(type_id_7, 2),
            Convert(
                vec![WireListElement::Wire(type_id_101, 0)],
                vec![WireListElement::Wire(type_id_7, 0)],
            ),
            Convert(
                vec![WireListElement::Wire(type_id_101, 1)],
                vec![WireListElement::Wire(type_id_7, 1)],
            ),
            Convert(
                vec![WireListElement::Wire(type_id_101, 2)],
                vec![WireListElement::Wire(type_id_7, 2)],
            ),
            Delete(type_id_7, 0, Some(2)),
            Call(
                "square".to_string(),
                vec![WireListElement::Wire(type_id_101, 3)],
                vec![WireListElement::Wire(type_id_101, 0)],
            ),
            Call(
                "vector_mul_7_2".to_string(),
                vec![WireListElement::WireRange(type_id_101, 4, 5)],
                vec![
                    WireListElement::WireRange(type_id_101, 1, 2),
                    WireListElement::WireRange(type_id_101, 1, 2),
                ],
            ),
            Add(type_id_101, 6, 4, 5),
            MulConstant(type_id_101, 7, 3, vec![100]),
            Add(type_id_101, 8, 6, 7),
            AssertZero(type_id_101, 8),
            Delete(type_id_101, 0, Some(8)),
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
    use crate::wirelist;
    use crate::Gate::*;

    let public_inputs = example_public_inputs_with_several_types();
    let private_inputs = example_private_inputs_with_several_types();

    let type_id_7: u8 = 0;
    let type_id_101: u8 = 1;
    let relation = Relation {
        header: example_header_with_several_types(),
        plugins: vec![],
        functions: vec![],
        gates: vec![
            PublicInput(type_id_7, 0),
            PrivateInput(type_id_7, 1),
            PrivateInput(type_id_7, 2),
            Constant(type_id_101, 0, vec![3]),
            // Error: in a convert gate, all inputs should belong to the same type
            Convert(
                wirelist![type_id_7; 3, 4],
                vec![
                    WireListElement::Wire(type_id_101, 0),
                    WireListElement::Wire(type_id_7, 0),
                ],
            ),
            // Error: in a convert gate, all outputs should belong to the same type
            Convert(
                vec![
                    WireListElement::Wire(type_id_101, 1),
                    WireListElement::Wire(type_id_7, 5),
                ],
                wirelist![type_id_7; 0],
            ),
            // Error: in a convert gate, input should not be empty
            Convert(wirelist![type_id_7; 6], vec![]),
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
    use crate::wirelist;
    use crate::Gate::*;

    let public_inputs = example_public_inputs_with_several_types();
    let private_inputs = example_private_inputs_with_several_types();

    let type_id_7: u8 = 0;
    let type_id_101: u8 = 1;

    // 1st test: impossible conversion
    let relation = Relation {
        header: example_header_with_several_types(),
        plugins: vec![],
        functions: vec![],
        gates: vec![
            Constant(type_id_101, 0, vec![25]),
            // The following conversion is impossible
            Convert(wirelist![type_id_7; 0], wirelist![type_id_101; 0]),
            Delete(type_id_101, 0, None),
            Delete(type_id_7, 0, None),
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
            PublicInput(type_id_7, 0), // 5
            Constant(type_id_101, 0, vec![25]),
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
