use num_bigint::BigUint;
use std::collections::BTreeMap;

use crate::consumers::evaluator::PlaintextType;
use crate::plugins::{zkif_assert_equal, zkif_ring, zkif_vector};
use crate::structs::count::Count;
use crate::structs::plugin::PluginBody;
use crate::Result;
use crate::TypeId;

pub fn evaluate_plugin_for_plaintext_backend(
    output_count: &[Count],
    input_count: &[Count],
    inputs: &[&BigUint],
    public_inputs: &BTreeMap<TypeId, Vec<BigUint>>,
    private_inputs: &BTreeMap<TypeId, Vec<BigUint>>,
    plugin_body: &PluginBody,
    types: &[PlaintextType],
) -> Result<Vec<BigUint>> {
    match (plugin_body.name.as_str(), plugin_body.operation.as_str()) {
        ("zkif_vector", "add") => zkif_vector::zkif_vector_add(
            output_count,
            input_count,
            inputs,
            public_inputs,
            private_inputs,
            &plugin_body.params,
            types,
        ),
        ("zkif_vector", "mul") => zkif_vector::zkif_vector_mul(
            output_count,
            input_count,
            inputs,
            public_inputs,
            private_inputs,
            &plugin_body.params,
            types,
        ),
        ("zkif_assert_equal", "public") => {
            // zkif_assert_equal_public returns `Ok(())` or an error.
            // If it returns an error, `evaluate_plugin_for_plaintext_backend` must return this error.
            // If it returns Ok(()), `evaluate_plugin_for_plaintext_backend` must return Ok(vec![]).
            // The vector is empty because the zkif_assert_equal plugin has no output value.
            zkif_assert_equal::zkif_assert_equal_public(
                output_count,
                input_count,
                inputs,
                public_inputs,
                private_inputs,
                &plugin_body.params,
            )?;
            Ok(vec![])
        }
        ("zkif_assert_equal", "private") => {
            // zkif_assert_equal_private returns `Ok(())` or an error.
            // If it returns an error, `evaluate_plugin_for_plaintext_backend` must return this error.
            // If it returns Ok(()), `evaluate_plugin_for_plaintext_backend` must return Ok(vec![]).
            // The vector is empty because the zkif_assert_equal plugin has no output value.
            zkif_assert_equal::zkif_assert_equal_private(
                output_count,
                input_count,
                inputs,
                public_inputs,
                private_inputs,
                &plugin_body.params,
            )?;
            Ok(vec![])
        }
        ("zkif_ring", "add") => {
            let output_value = zkif_ring::zkif_ring_add(
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
        ("zkif_ring", "mul") => {
            let output_value = zkif_ring::zkif_ring_mul(
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
        ("zkif_ring", "equal") => {
            // zkif_ring_equal returns `Ok(())` or an error.
            // If it returns an error, `evaluate_plugin_for_plaintext_backend` must return this error.
            // If it returns Ok(()), `evaluate_plugin_for_plaintext_backend` must return Ok(vec![]).
            // The vector is empty because the zkif_ring_equal plugin has no output value.
            zkif_ring::zkif_ring_equal(
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

/// Convert a string number into a u64
/// The string number must matches the following regex
/// NUMBER_REGEX = “^((\d+)|(0x[0-9a-fA-F]+))$“
pub fn extract_number(param: &str) -> Result<u64> {
    if param.starts_with("0x") {
        u64::from_str_radix(param.trim_start_matches("0x"), 16).map_err(|e| e.into())
    } else {
        param.parse::<u64>().map_err(|e| e.into())
    }
}

#[test]
fn test_extract_number() {
    let param = "81335";
    let result = extract_number(param).unwrap();
    let expected_result = 81335;
    assert_eq!(result, expected_result);

    let param = "0xAf2b";
    let result = extract_number(param).unwrap();
    let expected_result = 44843;
    assert_eq!(result, expected_result);

    let param = "0x0";
    let result = extract_number(param).unwrap();
    let expected_result = 0;
    assert_eq!(result, expected_result);

    let param = "Af2b";
    let result = extract_number(param);
    assert!(result.is_err());

    let param = "0XAf2b";
    let result = extract_number(param);
    assert!(result.is_err());
}
