use num_bigint::BigUint;
use std::collections::HashMap;

use crate::structs::count::CountList;
use crate::Result;

fn vector_check<'a>(
    output_count: &'a CountList,
    input_count: &'a CountList,
    inputs: &'a [&BigUint],
    params: &'a [String],
    moduli: &'a [BigUint],
) -> Result<(usize, &'a BigUint)> {
    // Check that params are compliant with the plugin vector and the operation add/mul
    if params.len() != 2 {
        return Err(
            "plugin(vector, add/mul) must be declared with 2 params (type_id, length).".into(),
        );
    }
    let param_type_id = params[0].parse::<u8>()?;
    let param_len = params[1].parse::<usize>()?;
    if param_len == 0 {
        return Err("plugin(vector, add/mul) cannot be called without inputs.".into());
    }
    let modulo = moduli.get(param_type_id as usize).ok_or_else(|| {
        format!(
            "plugin(vector, add/mul) cannot be called with a type id ({}) which is not defined.",
            param_type_id
        )
    })?;

    // Check that `output_count` and `input_count` are compliant with `plugin(vector, add/mul, params)`
    if *output_count != HashMap::from([(param_type_id, param_len as u64)]) {
        return Err(format!(
            "When calling the plugin(vector, add/mul, {}, {}), the out parameter in the function signature must be equal to {}: {}.",
            param_type_id, param_len, param_type_id, param_len
        )
            .into());
    }

    if *input_count != HashMap::from([(param_type_id, 2 * param_len as u64)]) {
        return Err(format!(
            "When calling the plugin(vector, add/mul, {}, {}), the in parameter in the function signature must be equal to {}: {}.",
            param_type_id, param_len, param_type_id, 2*param_len
        )
            .into());
    }

    // Check that `inputs` is compliant with `plugin(vector, add/mul, params)`
    if inputs.len() != 2 * param_len {
        return Err(format!(
            "When calling the plugin(vector, add/mul, {}, {}), we should have {} input values (and not {}).",
            param_type_id, param_len, 2*param_len, inputs.len()
        )
            .into());
    }
    Ok((param_len, modulo))
}

/// @function(vector_add, @out: type_id: length, @in: type_id: length, type_id: length) @plugin(vector, add, type_id, length)
/// This function takes as input two vectors `in1` and `in2` of length `length` containing elements from type `type_id`,
/// This function returns one vector of length `length` such that `out[i] = in1[i] + in2[i] % type_modulo`
pub fn vector_add(
    output_count: &CountList,
    input_count: &CountList,
    inputs: &[&BigUint],
    params: &[String],
    moduli: &[BigUint],
) -> Result<Vec<BigUint>> {
    let (param_len, modulo) = vector_check(output_count, input_count, inputs, params, moduli)?;

    // Evaluate plugin(vector, add)
    let mut result = vec![];
    for i in 0..param_len {
        result.push((inputs[i] + inputs[i + param_len]) % modulo);
    }
    Ok(result)
}

/// @function(vector_mul, @out: type_id: length, @in: type_id: length, type_id: length) @plugin(vector, mul, type_id, length)
/// This function takes as input two vectors `in1` and `in2` of length `length` containing elements from type `type_id`,
/// This function returns one vector of length `length` such that `out[i] = in1[i] * in2[i] % type_modulo`
pub fn vector_mul(
    output_count: &CountList,
    input_count: &CountList,
    inputs: &[&BigUint],
    params: &[String],
    moduli: &[BigUint],
) -> Result<Vec<BigUint>> {
    let (param_len, modulo) = vector_check(output_count, input_count, inputs, params, moduli)?;

    // Evaluate plugin(vector, mul)
    let mut result = vec![];
    for i in 0..param_len {
        result.push((inputs[i] * inputs[i + param_len]) % modulo);
    }
    Ok(result)
}

#[test]
fn test_vector_check() {
    let output_count = HashMap::from([(0, 2)]);
    let input_count = HashMap::from([(0, 4)]);
    let inputs = [
        &BigUint::from_bytes_le(&[1]),
        &BigUint::from_bytes_le(&[2]),
        &BigUint::from_bytes_le(&[3]),
        &BigUint::from_bytes_le(&[4]),
    ];
    let moduli = [BigUint::from_bytes_le(&[7])];
    let params = ["0".to_string(), "2".to_string()];
    let result = vector_check(&output_count, &input_count, &inputs, &params, &moduli).unwrap();
    let expected_result = (2_usize, &BigUint::from_bytes_le(&[7]));
    assert_eq!(result, expected_result);

    // Try to use the plugin vector with an unknown type_id in params
    let incorrect_params = ["1".to_string(), "2".to_string()];
    let result = vector_check(
        &output_count,
        &input_count,
        &inputs,
        &incorrect_params,
        &moduli,
    );
    assert!(result.is_err());

    // Try to use the plugin vector with a type_id which cannot be parsed into u8
    let incorrect_params = ["a".to_string(), "2".to_string()];
    let result = vector_check(
        &output_count,
        &input_count,
        &inputs,
        &incorrect_params,
        &moduli,
    );
    assert!(result.is_err());

    // Try to use the plugin vector with a param_len equal to 0
    let incorrect_params = ["0".to_string(), "0".to_string()];
    let result = vector_check(
        &output_count,
        &input_count,
        &inputs,
        &incorrect_params,
        &moduli,
    );
    assert!(result.is_err());

    // Try to use the plugin vector with an incorrect output_count
    let incorrect_output_count = HashMap::from([(0, 3)]);
    let result = vector_check(
        &incorrect_output_count,
        &input_count,
        &inputs,
        &params,
        &moduli,
    );
    assert!(result.is_err());

    // Try to use the plugin vector with an incorrect input_count
    let incorrect_input_count = HashMap::from([(0, 6)]);
    let result = vector_check(
        &output_count,
        &incorrect_input_count,
        &inputs,
        &params,
        &moduli,
    );
    assert!(result.is_err());

    // Try to use the plugin vector with an incorrect number of inputs
    let incorrect_inputs = [
        &BigUint::from_bytes_le(&[1]),
        &BigUint::from_bytes_le(&[2]),
        &BigUint::from_bytes_le(&[3]),
    ];
    let result = vector_check(
        &output_count,
        &input_count,
        &incorrect_inputs,
        &params,
        &moduli,
    );
    assert!(result.is_err());
}

#[test]
fn test_vector_add() {
    let output_count = HashMap::from([(0, 2)]);
    let input_count = HashMap::from([(0, 4)]);
    let inputs = [
        &BigUint::from_bytes_le(&[1]),
        &BigUint::from_bytes_le(&[2]),
        &BigUint::from_bytes_le(&[3]),
        &BigUint::from_bytes_le(&[4]),
    ];
    let moduli = [BigUint::from_bytes_le(&[7])];
    let params = ["0".to_string(), "2".to_string()];
    let result = vector_add(&output_count, &input_count, &inputs, &params, &moduli).unwrap();
    let expected_result = vec![BigUint::from_bytes_le(&[4]), BigUint::from_bytes_le(&[6])];
    assert_eq!(result, expected_result);

    // Try to use the plugin(vector, add, params) with 3 parameters instead of 2
    let incorrect_params = ["1".to_string(), "2".to_string()];
    let result = vector_add(
        &output_count,
        &input_count,
        &inputs,
        &incorrect_params,
        &moduli,
    );
    assert!(result.is_err());
}

#[test]
fn test_vector_mul() {
    let output_count = HashMap::from([(0, 2)]);
    let input_count = HashMap::from([(0, 4)]);
    let inputs = [
        &BigUint::from_bytes_le(&[1]),
        &BigUint::from_bytes_le(&[2]),
        &BigUint::from_bytes_le(&[3]),
        &BigUint::from_bytes_le(&[4]),
    ];
    let moduli = [BigUint::from_bytes_le(&[7])];
    let params = ["0".to_string(), "2".to_string()];
    let result = vector_mul(&output_count, &input_count, &inputs, &params, &moduli).unwrap();
    let expected_result = vec![BigUint::from_bytes_le(&[3]), BigUint::from_bytes_le(&[1])];
    assert_eq!(result, expected_result);

    // Try to use the plugin(vector, add, params) with 3 parameters instead of 2
    let incorrect_params = ["1".to_string(), "2".to_string()];
    let result = vector_mul(
        &output_count,
        &input_count,
        &inputs,
        &incorrect_params,
        &moduli,
    );
    assert!(result.is_err());
}
