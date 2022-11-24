use num_bigint::BigUint;
use std::collections::HashMap;

use crate::consumers::evaluator::PlaintextType;
use crate::plugins::{assert_equal, ring, vector};
use crate::structs::count::Count;
use crate::structs::plugin::PluginBody;
use crate::Result;
use crate::TypeId;

pub fn evaluate_plugin_for_plaintext_backend(
    output_count: &[Count],
    input_count: &[Count],
    inputs: &[&BigUint],
    public_inputs: &HashMap<TypeId, Vec<BigUint>>,
    private_inputs: &HashMap<TypeId, Vec<BigUint>>,
    plugin_body: &PluginBody,
    types: &[PlaintextType],
) -> Result<Vec<BigUint>> {
    match (plugin_body.name.as_str(), plugin_body.operation.as_str()) {
        ("vector", "add") => vector::vector_add(
            output_count,
            input_count,
            inputs,
            public_inputs,
            private_inputs,
            &plugin_body.params,
            types,
        ),
        ("vector", "mul") => vector::vector_mul(
            output_count,
            input_count,
            inputs,
            public_inputs,
            private_inputs,
            &plugin_body.params,
            types,
        ),
        ("assert_equal", "public") => {
            // assert_equal_public returns `Ok(())` or an error.
            // If it returns an error, `evaluate_plugin_for_plaintext_backend` must return this error.
            // If it returns Ok(()), `evaluate_plugin_for_plaintext_backend` must return Ok(vec![]).
            // The vector is empty because the assert_equal plugin has no output value.
            assert_equal::assert_equal_public(
                output_count,
                input_count,
                inputs,
                public_inputs,
                private_inputs,
                &plugin_body.params,
            )?;
            Ok(vec![])
        }
        ("assert_equal", "private") => {
            // assert_equal_private returns `Ok(())` or an error.
            // If it returns an error, `evaluate_plugin_for_plaintext_backend` must return this error.
            // If it returns Ok(()), `evaluate_plugin_for_plaintext_backend` must return Ok(vec![]).
            // The vector is empty because the assert_equal plugin has no output value.
            assert_equal::assert_equal_private(
                output_count,
                input_count,
                inputs,
                public_inputs,
                private_inputs,
                &plugin_body.params,
            )?;
            Ok(vec![])
        }
        ("ring", "add") => {
            let output_value = ring::ring_add(
                output_count,
                input_count,
                inputs,
                public_inputs,
                private_inputs,
                &plugin_body.params,
                types,
            )?;
            Ok(vec![output_value])
        }
        ("ring", "mul") => {
            let output_value = ring::ring_mul(
                output_count,
                input_count,
                inputs,
                public_inputs,
                private_inputs,
                &plugin_body.params,
                types,
            )?;
            Ok(vec![output_value])
        }
        ("ring", "equal") => {
            // ring_equal returns `Ok(())` or an error.
            // If it returns an error, `evaluate_plugin_for_plaintext_backend` must return this error.
            // If it returns Ok(()), `evaluate_plugin_for_plaintext_backend` must return Ok(vec![]).
            // The vector is empty because the ring_equal plugin has no output value.
            ring::ring_equal(
                output_count,
                input_count,
                inputs,
                public_inputs,
                private_inputs,
                &plugin_body.params,
                types,
            )?;
            Ok(vec![])
        }
        _ => Err("Unknown plugin".into()),
    }
}
