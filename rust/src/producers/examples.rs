/// This file contains an example with all features (plugins functions and types, conversions)
use crate::producers::simple_examples::literal32;
use crate::structs::conversion::Conversion;
use crate::structs::directives::Directive;
use crate::structs::plugin::PluginBody;
use crate::structs::types::Type;
use crate::structs::wirerange::WireRange;
use crate::structs::IR_VERSION;
use crate::{PrivateInputs, PublicInputs, Relation};
use std::collections::BTreeMap;

pub fn example_public_inputs() -> Vec<PublicInputs> {
    vec![
        PublicInputs {
            version: IR_VERSION.to_string(),
            type_value: Type::Field(literal32(7)),
            inputs: vec![vec![5]],
        },
        PublicInputs {
            version: IR_VERSION.to_string(),
            type_value: Type::PluginType(
                "zkif_ring".to_string(),
                "type".to_string(),
                vec!["2".to_string(), "8".to_string()],
            ),
            inputs: vec![vec![30]],
        },
    ]
}

pub fn example_private_inputs() -> Vec<PrivateInputs> {
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
                "zkif_ring".to_string(),
                "type".to_string(),
                vec!["2".to_string(), "8".to_string()],
            ),
            inputs: vec![vec![5], vec![10], vec![2]],
        },
    ]
}

pub fn example_incorrect_private_inputs() -> Vec<PrivateInputs> {
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
                "zkif_ring".to_string(),
                "type".to_string(),
                vec!["2".to_string(), "8".to_string()],
            ),
            inputs: vec![vec![5], vec![10], vec![2]],
        },
    ]
}

pub fn example_relation() -> Relation {
    use crate::structs::count::Count;
    use crate::structs::function::{Function, FunctionBody};
    use crate::Gate::*;

    let type_id_7: u8 = 0;
    let type_id_101: u8 = 1;
    let type_id_ring: u8 = 2;
    Relation {
        version: IR_VERSION.to_string(),
        plugins: vec![
            "zkif_vector".to_string(),
            "zkif_assert_equal".to_string(),
            "zkif_ring".to_string(),
        ],
        types: vec![
            Type::Field(vec![7]),
            Type::Field(vec![101]),
            Type::PluginType(
                "zkif_ring".to_string(),
                "type".to_string(),
                vec!["2".to_string(), "8".to_string()],
            ),
        ],
        conversions: vec![Conversion::new(
            Count::new(type_id_101, 1),
            Count::new(type_id_7, 1),
        )],
        directives: vec![
            Directive::Function(Function::new(
                "assert_equal_private".to_string(),
                vec![],
                vec![Count::new(type_id_101, 1)],
                FunctionBody::PluginBody(PluginBody {
                    name: "zkif_assert_equal".to_string(),
                    operation: "private".to_string(),
                    params: vec![type_id_101.to_string(), "1".to_string()],
                    public_count: BTreeMap::new(),
                    private_count: BTreeMap::from([(type_id_101, 1)]),
                }),
            )),
            // Right-triangle example
            Directive::Gate(New(type_id_101, 0, 8)),
            Directive::Gate(Public(type_id_7, 0)),
            Directive::Gate(Private(type_id_7, 1)),
            Directive::Gate(Private(type_id_7, 2)),
            Directive::Gate(Convert(type_id_101, 0, 0, type_id_7, 0, 0)),
            Directive::Gate(Convert(type_id_101, 1, 1, type_id_7, 1, 1)),
            Directive::Gate(Convert(type_id_101, 2, 2, type_id_7, 2, 2)),
            Directive::Gate(Delete(type_id_7, 0, 2)),
            Directive::Function(Function::new(
                "square".to_string(),
                vec![Count::new(type_id_101, 1)],
                vec![Count::new(type_id_101, 1)],
                FunctionBody::Gates(vec![Mul(type_id_101, 0, 1, 1)]),
            )),
            Directive::Gate(Call(
                "square".to_string(),
                vec![WireRange::new(3, 3)],
                vec![WireRange::new(0, 0)],
            )),
            Directive::Function(Function::new(
                "vector_mul_7_2".to_string(),
                vec![Count::new(type_id_101, 2)],
                vec![Count::new(type_id_101, 2), Count::new(type_id_101, 2)],
                FunctionBody::PluginBody(PluginBody {
                    name: "zkif_vector".to_string(),
                    operation: "mul".to_string(),
                    params: vec![type_id_101.to_string(), "2".to_string()],
                    public_count: BTreeMap::new(),
                    private_count: BTreeMap::new(),
                }),
            )),
            Directive::Gate(Call(
                "vector_mul_7_2".to_string(),
                vec![WireRange::new(4, 5)],
                vec![WireRange::new(1, 2), WireRange::new(1, 2)],
            )),
            Directive::Gate(Add(type_id_101, 6, 4, 5)),
            Directive::Gate(MulConstant(type_id_101, 7, 3, vec![100])),
            Directive::Gate(Add(type_id_101, 8, 6, 7)),
            Directive::Gate(AssertZero(type_id_101, 8)),
            Directive::Gate(Call(
                "assert_equal_private".to_string(),
                vec![],
                vec![WireRange::new(3, 3)],
            )),
            Directive::Gate(Delete(type_id_101, 0, 8)),
            // Test ring type
            Directive::Function(Function::new(
                "ring_add".to_string(),
                vec![Count::new(type_id_ring, 1)],
                vec![Count::new(type_id_ring, 1), Count::new(type_id_ring, 1)],
                FunctionBody::PluginBody(PluginBody {
                    name: "zkif_ring".to_string(),
                    operation: "add".to_string(),
                    params: vec![type_id_ring.to_string()],
                    public_count: BTreeMap::new(),
                    private_count: BTreeMap::new(),
                }),
            )),
            Directive::Function(Function::new(
                "ring_mul".to_string(),
                vec![Count::new(type_id_ring, 1)],
                vec![Count::new(type_id_ring, 1), Count::new(type_id_ring, 1)],
                FunctionBody::PluginBody(PluginBody {
                    name: "zkif_ring".to_string(),
                    operation: "mul".to_string(),
                    params: vec![type_id_ring.to_string()],
                    public_count: BTreeMap::new(),
                    private_count: BTreeMap::new(),
                }),
            )),
            Directive::Function(Function::new(
                "ring_equal".to_string(),
                vec![],
                vec![Count::new(type_id_ring, 1), Count::new(type_id_ring, 1)],
                FunctionBody::PluginBody(PluginBody {
                    name: "zkif_ring".to_string(),
                    operation: "equal".to_string(),
                    params: vec![type_id_ring.to_string()],
                    public_count: BTreeMap::new(),
                    private_count: BTreeMap::new(),
                }),
            )),
            Directive::Gate(Private(type_id_ring, 0)),
            Directive::Gate(Private(type_id_ring, 1)),
            Directive::Gate(Private(type_id_ring, 2)),
            Directive::Gate(Call(
                "ring_add".to_string(),
                vec![WireRange::new(3, 3)],
                vec![WireRange::new(0, 0), WireRange::new(1, 1)],
            )),
            Directive::Gate(Call(
                "ring_mul".to_string(),
                vec![WireRange::new(4, 4)],
                vec![WireRange::new(2, 2), WireRange::new(3, 3)],
            )),
            Directive::Gate(Public(type_id_ring, 5)),
            Directive::Gate(Call(
                "ring_equal".to_string(),
                vec![],
                vec![WireRange::new(4, 4), WireRange::new(5, 5)],
            )),
            Directive::Gate(Delete(type_id_ring, 0, 5)),
        ],
    }
}

#[test]
fn test_examples() {
    use crate::Source;

    let mut common_buf = Vec::<u8>::new();

    example_public_inputs()
        .iter()
        .for_each(|message| message.write_into(&mut common_buf).unwrap());
    example_relation().write_into(&mut common_buf).unwrap();

    let mut prover_buf = Vec::<u8>::new();
    example_private_inputs()
        .iter()
        .for_each(|message| message.write_into(&mut prover_buf).unwrap());

    let source = Source::from_buffers(vec![common_buf, prover_buf]);
    let messages = source.read_all_messages().unwrap();
    assert_eq!(messages.relations, vec![example_relation()]);
    assert_eq!(messages.public_inputs, example_public_inputs());
    assert_eq!(messages.private_inputs, example_private_inputs());
}

#[test]
fn test_validator() {
    use crate::consumers::validator::Validator;

    let public_inputs = example_public_inputs();
    let private_inputs = example_private_inputs();
    let relation = example_relation();

    let mut validator = Validator::new_as_prover();

    public_inputs
        .iter()
        .for_each(|inputs| validator.ingest_public_inputs(inputs));
    private_inputs
        .iter()
        .for_each(|inputs| validator.ingest_private_inputs(inputs));
    validator.ingest_relation(&relation);

    assert_eq!(validator.get_violations(), Vec::<String>::new());
}

#[test]
fn test_evaluator() {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};

    let relation = example_relation();
    let public_inputs = example_public_inputs();
    let private_inputs = example_private_inputs();

    let mut zkbackend = PlaintextBackend::default();
    let mut simulator: Evaluator<PlaintextBackend> = Evaluator::default();

    public_inputs
        .iter()
        .for_each(|inputs| simulator.ingest_public_inputs(inputs).unwrap());
    private_inputs
        .iter()
        .for_each(|inputs| simulator.ingest_private_inputs(inputs).unwrap());
    simulator
        .ingest_relation(&relation, &mut zkbackend)
        .unwrap();

    assert_eq!(simulator.get_violations(), Vec::<String>::new());
}
