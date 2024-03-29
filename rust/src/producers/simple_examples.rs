/// This file contains an example with only one Field type, without conversions and plugins.
/// Thus, this statement could be flatten or convert to R1CS
use flatbuffers::{emplace_scalar, read_scalar, EndianScalar};
use std::mem::size_of;

use crate::structs::directives::Directive;
use crate::structs::types::Type;
use crate::structs::wirerange::WireRange;
use crate::structs::IR_VERSION;
use crate::{PrivateInputs, PublicInputs, Relation, TypeId};

pub fn simple_example_public_inputs() -> PublicInputs {
    PublicInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(literal32(EXAMPLE_MODULUS)),
        inputs: vec![literal32(5)],
    }
}

pub fn simple_example_private_inputs() -> PrivateInputs {
    PrivateInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(literal32(EXAMPLE_MODULUS)),
        inputs: vec![literal32(3), literal32(4)],
    }
}

pub fn simple_example_incorrect_private_inputs() -> PrivateInputs {
    PrivateInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(literal32(EXAMPLE_MODULUS)),
        inputs: vec![
            literal32(3),
            literal32(4 + 1), // incorrect.
        ],
    }
}

pub fn simple_example_relation() -> Relation {
    use crate::structs::count::Count;
    use crate::structs::function::{Function, FunctionBody};
    use crate::Gate::*;

    let type_id: TypeId = 0;

    Relation {
        version: IR_VERSION.to_string(),
        plugins: vec![],
        types: vec![Type::Field(literal32(EXAMPLE_MODULUS))],
        conversions: vec![],
        directives: vec![
            Directive::Function(Function::new(
                "square".to_string(),
                vec![Count::new(type_id, 1)],
                vec![Count::new(type_id, 1)],
                FunctionBody::Gates(vec![Mul(type_id, 0, 1, 1)]),
            )),
            // Right-triangle example
            Directive::Gate(New(type_id, 0, 2)),
            Directive::Gate(Public(type_id, 0)),
            Directive::Gate(Private(type_id, 1)),
            Directive::Gate(Private(type_id, 2)),
            Directive::Gate(Call(
                "square".to_string(),
                vec![WireRange::new(3, 3)],
                vec![WireRange::new(0, 0)],
            )),
            Directive::Gate(Call(
                "square".to_string(),
                vec![WireRange::new(4, 4)],
                vec![WireRange::new(1, 1)],
            )),
            Directive::Gate(Call(
                "square".to_string(),
                vec![WireRange::new(5, 5)],
                vec![WireRange::new(2, 2)],
            )),
            Directive::Gate(Add(type_id, 6, 4, 5)),
            Directive::Gate(MulConstant(type_id, 7, 3, vec![100])),
            Directive::Gate(Add(type_id, 8, 6, 7)),
            Directive::Gate(AssertZero(type_id, 8)),
            Directive::Gate(Delete(type_id, 0, 2)),
            Directive::Gate(Delete(type_id, 3, 8)),
        ],
    }
}

pub const EXAMPLE_MODULUS: u32 = 101;

pub fn literal<T: EndianScalar>(value: T) -> Vec<u8> {
    let mut buf = vec![0u8; size_of::<T>()];
    emplace_scalar(&mut buf[..], value);
    buf
}

pub fn literal32(v: u32) -> Vec<u8> {
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

pub fn encode_negative_one(modulo: &[u8]) -> Vec<u8> {
    let mut neg_one = modulo.to_owned();
    assert!(!neg_one.is_empty() && neg_one[0] > 0, "Invalid modulo");
    neg_one[0] -= 1;
    neg_one
}

#[test]
fn test_simple_example() {
    use crate::Source;

    let mut common_buf = Vec::<u8>::new();
    simple_example_public_inputs()
        .write_into(&mut common_buf)
        .unwrap();
    simple_example_relation()
        .write_into(&mut common_buf)
        .unwrap();

    let mut prover_buf = Vec::<u8>::new();
    simple_example_private_inputs()
        .write_into(&mut prover_buf)
        .unwrap();

    let source = Source::from_buffers(vec![common_buf, prover_buf]);
    let messages = source.read_all_messages().unwrap();
    assert_eq!(messages.relations, vec![simple_example_relation()]);
    assert_eq!(messages.public_inputs, vec![simple_example_public_inputs()]);
    assert_eq!(
        messages.private_inputs,
        vec![simple_example_private_inputs()]
    );
}

#[test]
fn test_validator() {
    use crate::consumers::validator::Validator;

    let public_inputs = simple_example_public_inputs();
    let private_inputs = simple_example_private_inputs();
    let relation = simple_example_relation();

    let mut validator = Validator::new_as_prover();

    validator.ingest_public_inputs(&public_inputs);
    validator.ingest_private_inputs(&private_inputs);
    validator.ingest_relation(&relation);

    assert_eq!(validator.get_violations(), Vec::<String>::new());
}

#[test]
fn test_evaluator() {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};

    let relation = simple_example_relation();
    let public_inputs = simple_example_public_inputs();
    let private_inputs = simple_example_private_inputs();

    let mut zkbackend = PlaintextBackend::default();
    let mut simulator: Evaluator<PlaintextBackend> = Evaluator::default();

    simulator.ingest_public_inputs(&public_inputs).unwrap();
    simulator.ingest_private_inputs(&private_inputs).unwrap();
    simulator
        .ingest_relation(&relation, &mut zkbackend)
        .unwrap();

    assert_eq!(simulator.get_violations(), Vec::<String>::new());
}
