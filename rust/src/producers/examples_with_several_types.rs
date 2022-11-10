use crate::producers::examples::literal32;
use crate::structs::inputs::Inputs;
use crate::structs::plugin::PluginBody;
use crate::structs::wirerange::WireRange;
use crate::structs::IR_VERSION;
use crate::{PrivateInputs, PublicInputs, Relation};
use std::collections::HashMap;

pub fn example_public_inputs_with_several_types() -> PublicInputs {
    PublicInputs {
        version: IR_VERSION.to_string(),
        types: vec![literal32(7), literal32(101)],
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
        version: IR_VERSION.to_string(),
        types: vec![literal32(7), literal32(101)],
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
        version: IR_VERSION.to_string(),
        types: vec![literal32(7), literal32(101)],
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
        version: IR_VERSION.to_string(),
        types: vec![literal32(7), literal32(101)],
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
            Convert(type_id_101, 0, 0, type_id_7, 0, 0),
            Convert(type_id_101, 1, 1, type_id_7, 1, 1),
            Convert(type_id_101, 2, 2, type_id_7, 2, 2),
            Delete(type_id_7, 0, 2),
            Call(
                "square".to_string(),
                vec![WireRange::new(3, 3)],
                vec![WireRange::new(0, 0)],
            ),
            Call(
                "vector_mul_7_2".to_string(),
                vec![WireRange::new(4, 5)],
                vec![WireRange::new(1, 2), WireRange::new(1, 2)],
            ),
            Add(type_id_101, 6, 4, 5),
            MulConstant(type_id_101, 7, 3, vec![100]),
            Add(type_id_101, 8, 6, 7),
            AssertZero(type_id_101, 8),
            Delete(type_id_101, 0, 8),
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

    let type_id_7: u8 = 0;
    let type_id_101: u8 = 1;

    // Test: impossible conversion
    let relation = Relation {
        version: IR_VERSION.to_string(),
        types: vec![literal32(7), literal32(101)],
        plugins: vec![],
        functions: vec![],
        gates: vec![
            Constant(type_id_101, 0, vec![25]),
            // The following conversion is impossible
            Convert(type_id_7, 0, 0, type_id_101, 0, 0),
            Delete(type_id_101, 0, 0),
            Delete(type_id_7, 0, 0),
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

    Ok(())
}
