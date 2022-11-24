use crate::producers::examples::literal32;
use crate::structs::conversion::Conversion;
use crate::structs::plugin::PluginBody;
use crate::structs::types::Type;
use crate::structs::wirerange::WireRange;
use crate::structs::IR_VERSION;
use crate::{PrivateInputs, PublicInputs, Relation};
use std::collections::HashMap;

pub fn example_public_inputs_with_several_types() -> Vec<PublicInputs> {
    vec![
        PublicInputs {
            version: IR_VERSION.to_string(),
            type_value: Type::Field(literal32(7)),
            inputs: vec![vec![5]],
        },
        PublicInputs {
            version: IR_VERSION.to_string(),
            type_value: Type::PluginType(
                "ring".to_string(),
                "type".to_string(),
                vec!["2".to_string(), "8".to_string()],
            ),
            inputs: vec![vec![30]],
        },
    ]
}

pub fn example_private_inputs_with_several_types() -> Vec<PrivateInputs> {
    vec![
        PrivateInputs {
            version: IR_VERSION.to_string(),
            type_value: Type::Field(literal32(7)),
            inputs: vec![vec![3], vec![4]],
        },
        PrivateInputs {
            version: IR_VERSION.to_string(),
            type_value: Type::Field(literal32(101)),
            inputs: vec![vec![25]],
        },
        PrivateInputs {
            version: IR_VERSION.to_string(),
            type_value: Type::PluginType(
                "ring".to_string(),
                "type".to_string(),
                vec!["2".to_string(), "8".to_string()],
            ),
            inputs: vec![vec![5], vec![10], vec![2]],
        },
    ]
}

pub fn example_incorrect_private_inputs_with_several_types() -> Vec<PrivateInputs> {
    vec![
        PrivateInputs {
            version: IR_VERSION.to_string(),
            type_value: Type::Field(literal32(7)),
            inputs: vec![vec![3], vec![5]], // instead of [3, 4]
        },
        PrivateInputs {
            version: IR_VERSION.to_string(),
            type_value: Type::Field(literal32(101)),
            inputs: vec![vec![25]],
        },
        PrivateInputs {
            version: IR_VERSION.to_string(),
            type_value: Type::PluginType(
                "ring".to_string(),
                "type".to_string(),
                vec!["2".to_string(), "8".to_string()],
            ),
            inputs: vec![vec![5], vec![10], vec![2]],
        },
    ]
}

pub fn example_relation_with_several_types() -> Relation {
    use crate::structs::count::Count;
    use crate::structs::function::{Function, FunctionBody};
    use crate::Gate::*;

    let type_id_7: u8 = 0;
    let type_id_101: u8 = 1;
    let type_id_ring: u8 = 2;
    Relation {
        version: IR_VERSION.to_string(),
        plugins: vec![
            "vector".to_string(),
            "assert_equal".to_string(),
            "ring".to_string(),
        ],
        types: vec![
            Type::Field(literal32(7)),
            Type::Field(literal32(101)),
            Type::PluginType(
                "ring".to_string(),
                "type".to_string(),
                vec!["2".to_string(), "8".to_string()],
            ),
        ],
        conversions: vec![Conversion::new(
            Count::new(type_id_101, 1),
            Count::new(type_id_7, 1),
        )],
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
            Function::new(
                "assert_equal_private".to_string(),
                vec![],
                vec![Count::new(type_id_101, 1)],
                FunctionBody::PluginBody(PluginBody {
                    name: "assert_equal".to_string(),
                    operation: "private".to_string(),
                    params: vec![type_id_101.to_string(), "1".to_string()],
                    public_count: HashMap::new(),
                    private_count: HashMap::from([(type_id_101, 1)]),
                }),
            ),
            Function::new(
                "ring_add".to_string(),
                vec![Count::new(type_id_ring, 1)],
                vec![Count::new(type_id_ring, 1), Count::new(type_id_ring, 1)],
                FunctionBody::PluginBody(PluginBody {
                    name: "ring".to_string(),
                    operation: "add".to_string(),
                    params: vec![type_id_ring.to_string()],
                    public_count: HashMap::new(),
                    private_count: HashMap::new(),
                }),
            ),
            Function::new(
                "ring_mul".to_string(),
                vec![Count::new(type_id_ring, 1)],
                vec![Count::new(type_id_ring, 1), Count::new(type_id_ring, 1)],
                FunctionBody::PluginBody(PluginBody {
                    name: "ring".to_string(),
                    operation: "mul".to_string(),
                    params: vec![type_id_ring.to_string()],
                    public_count: HashMap::new(),
                    private_count: HashMap::new(),
                }),
            ),
            Function::new(
                "ring_equal".to_string(),
                vec![],
                vec![Count::new(type_id_ring, 1), Count::new(type_id_ring, 1)],
                FunctionBody::PluginBody(PluginBody {
                    name: "ring".to_string(),
                    operation: "equal".to_string(),
                    params: vec![type_id_ring.to_string()],
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
            Call(
                "assert_equal_private".to_string(),
                vec![],
                vec![WireRange::new(3, 3)],
            ),
            Delete(type_id_101, 0, 8),
            // Test ring type
            PrivateInput(type_id_ring, 0),
            PrivateInput(type_id_ring, 1),
            PrivateInput(type_id_ring, 2),
            Call(
                "ring_add".to_string(),
                vec![WireRange::new(3, 3)],
                vec![WireRange::new(0, 0), WireRange::new(1, 1)],
            ),
            Call(
                "ring_mul".to_string(),
                vec![WireRange::new(4, 4)],
                vec![WireRange::new(2, 2), WireRange::new(3, 3)],
            ),
            PublicInput(type_id_ring, 5),
            Call(
                "ring_equal".to_string(),
                vec![],
                vec![WireRange::new(4, 4), WireRange::new(5, 5)],
            ),
            Delete(type_id_ring, 0, 5),
        ],
    }
}

#[test]
fn test_examples_with_several_types() {
    use crate::Source;

    let mut common_buf = Vec::<u8>::new();

    example_public_inputs_with_several_types()
        .iter()
        .for_each(|message| message.write_into(&mut common_buf).unwrap());
    example_relation_with_several_types()
        .write_into(&mut common_buf)
        .unwrap();

    let mut prover_buf = Vec::<u8>::new();
    example_private_inputs_with_several_types()
        .iter()
        .for_each(|message| message.write_into(&mut prover_buf).unwrap());

    let source = Source::from_buffers(vec![common_buf, prover_buf]);
    let messages = source.read_all_messages().unwrap();
    assert_eq!(
        messages.relations,
        vec![example_relation_with_several_types()]
    );
    assert_eq!(
        messages.public_inputs,
        example_public_inputs_with_several_types()
    );
    assert_eq!(
        messages.private_inputs,
        example_private_inputs_with_several_types()
    );
}

#[test]
fn test_validator_with_several_types() -> crate::Result<()> {
    use crate::consumers::validator::Validator;

    let public_inputs = example_public_inputs_with_several_types();
    let private_inputs = example_private_inputs_with_several_types();
    let relation = example_relation_with_several_types();

    let mut validator = Validator::new_as_prover();

    public_inputs
        .iter()
        .for_each(|inputs| validator.ingest_public_inputs(inputs));
    private_inputs
        .iter()
        .for_each(|inputs| validator.ingest_private_inputs(inputs));
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

    public_inputs
        .iter()
        .try_for_each(|inputs| simulator.ingest_public_inputs(inputs))?;
    private_inputs
        .iter()
        .try_for_each(|inputs| simulator.ingest_private_inputs(inputs))?;
    simulator.ingest_relation(&relation, &mut zkbackend)?;

    assert_eq!(simulator.get_violations(), Vec::<String>::new());

    Ok(())
}
