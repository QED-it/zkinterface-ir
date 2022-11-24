use num_bigint::BigUint;
use std::collections::HashMap;
use std::convert::TryFrom;

use crate::consumers::evaluator::PlaintextType;
use crate::structs::count::Count;
use crate::{Result, TypeId};

/// This function performs the following checks on vector_add/mul inputs.
/// - there is no public/private inputs
/// - `params` are compliant with the plugin vector and the operation add/mul
/// - `type_id` is defined and is a Field type
/// - `output_count` and `input_count` are compliant with `plugin(vector, add/mul, params)`
/// - `inputs` is compliant with `plugin(vector, add/mul, params)`
fn vector_check<'a>(
    output_count: &'a [Count],
    input_count: &'a [Count],
    inputs: &'a [&BigUint],
    public_inputs: &HashMap<TypeId, Vec<BigUint>>,
    private_inputs: &HashMap<TypeId, Vec<BigUint>>,
    params: &'a [String],
    types: &'a [PlaintextType],
) -> Result<(usize, &'a BigUint)> {
    // Check that there is no public/private inputs
    if !public_inputs.is_empty() {
        return Err("plugin(vector, add/mul) does not consume any public input.".into());
    }
    if !private_inputs.is_empty() {
        return Err("plugin(vector, add/mul) does not consume any private input.".into());
    }

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
    // Check that `type_id` is defined and is a Field type.
    let type_ = types.get(param_type_id as usize).ok_or_else(|| {
        format!(
            "plugin(vector, add/mul) cannot be called with a type id ({}) which is not defined.",
            param_type_id
        )
    })?;
    let modulo = match type_ {
        PlaintextType::Field(modulo) => modulo,
        PlaintextType::PluginType(_, _, _) => {
            return Err("plugin(vector, add/mul) cannot be called on a PluginType.".into())
        }
    };

    // Check that `output_count` and `input_count` are compliant with `plugin(vector, add/mul, params)`
    let expected_output_count = vec![Count::new(param_type_id, u64::try_from(param_len)?)];
    if *output_count != expected_output_count {
        return Err(format!(
            "When calling the plugin(vector, add/mul, {}, {}), the out parameter in the function signature must be equal to {:?} (and not {:?}).",
            param_type_id, param_len, expected_output_count, output_count
        )
            .into());
    }

    let expected_input_count = vec![
        Count::new(param_type_id, u64::try_from(param_len)?),
        Count::new(param_type_id, u64::try_from(param_len)?),
    ];
    if *input_count != expected_input_count {
        return Err(format!(
            "When calling the plugin(vector, add/mul, {}, {}), the in parameter in the function signature must be equal to {:?} (and not {:?}).",
            param_type_id, param_len, expected_input_count, input_count
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
    output_count: &[Count],
    input_count: &[Count],
    inputs: &[&BigUint],
    public_inputs: &HashMap<TypeId, Vec<BigUint>>,
    private_inputs: &HashMap<TypeId, Vec<BigUint>>,
    params: &[String],
    types: &[PlaintextType],
) -> Result<Vec<BigUint>> {
    let (param_len, modulo) = vector_check(
        output_count,
        input_count,
        inputs,
        public_inputs,
        private_inputs,
        params,
        types,
    )?;

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
    output_count: &[Count],
    input_count: &[Count],
    inputs: &[&BigUint],
    public_inputs: &HashMap<TypeId, Vec<BigUint>>,
    private_inputs: &HashMap<TypeId, Vec<BigUint>>,
    params: &[String],
    types: &[PlaintextType],
) -> Result<Vec<BigUint>> {
    let (param_len, modulo) = vector_check(
        output_count,
        input_count,
        inputs,
        public_inputs,
        private_inputs,
        params,
        types,
    )?;

    // Evaluate plugin(vector, mul)
    let mut result = vec![];
    for i in 0..param_len {
        result.push((inputs[i] * inputs[i + param_len]) % modulo);
    }
    Ok(result)
}

#[test]
fn test_vector_check() {
    let output_count = vec![Count::new(0, 2)];
    let input_count = vec![Count::new(0, 2), Count::new(0, 2)];
    let inputs = [
        &BigUint::from_bytes_le(&[1]),
        &BigUint::from_bytes_le(&[2]),
        &BigUint::from_bytes_le(&[3]),
        &BigUint::from_bytes_le(&[4]),
    ];
    let types = [PlaintextType::Field(BigUint::from_bytes_le(&[7]))];
    let params = ["0".to_string(), "2".to_string()];
    let result = vector_check(
        &output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    )
    .unwrap();
    let expected_result = (2_usize, &BigUint::from_bytes_le(&[7]));
    assert_eq!(result, expected_result);

    // Try to use the plugin vector with an unknown type_id in params
    let incorrect_params = ["1".to_string(), "2".to_string()];
    let result = vector_check(
        &output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &incorrect_params,
        &types,
    );
    assert!(result.is_err());

    // Try to use the plugin vector with a type_id which cannot be parsed into u8
    let incorrect_params = ["a".to_string(), "2".to_string()];
    let result = vector_check(
        &output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &incorrect_params,
        &types,
    );
    assert!(result.is_err());

    // Try to use the plugin vector with a param_len equal to 0
    let incorrect_params = ["0".to_string(), "0".to_string()];
    let result = vector_check(
        &output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &incorrect_params,
        &types,
    );
    assert!(result.is_err());

    // Try to use the plugin vector with an incorrect output_count
    let incorrect_output_count = vec![Count::new(0, 3)];
    let result = vector_check(
        &incorrect_output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    );
    assert!(result.is_err());

    // Try to use the plugin vector with an incorrect input_count
    let incorrect_input_count = vec![Count::new(0, 2), Count::new(0, 3)];
    let result = vector_check(
        &output_count,
        &incorrect_input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
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
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    );
    assert!(result.is_err());
}

#[test]
fn test_vector_add() {
    let output_count = vec![Count::new(0, 2)];
    let input_count = vec![Count::new(0, 2), Count::new(0, 2)];
    let inputs = [
        &BigUint::from_bytes_le(&[1]),
        &BigUint::from_bytes_le(&[2]),
        &BigUint::from_bytes_le(&[3]),
        &BigUint::from_bytes_le(&[4]),
    ];
    let types = [PlaintextType::Field(BigUint::from_bytes_le(&[7]))];
    let params = ["0".to_string(), "2".to_string()];
    let result = vector_add(
        &output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    )
    .unwrap();
    let expected_result = vec![BigUint::from_bytes_le(&[4]), BigUint::from_bytes_le(&[6])];
    assert_eq!(result, expected_result);

    // Try to use the plugin(vector, add, params) with 3 parameters instead of 2
    let incorrect_params = ["1".to_string(), "2".to_string()];
    let result = vector_add(
        &output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &incorrect_params,
        &types,
    );
    assert!(result.is_err());
}

#[test]
fn test_vector_mul() {
    let output_count = vec![Count::new(0, 2)];
    let input_count = vec![Count::new(0, 2), Count::new(0, 2)];
    let inputs = [
        &BigUint::from_bytes_le(&[1]),
        &BigUint::from_bytes_le(&[2]),
        &BigUint::from_bytes_le(&[3]),
        &BigUint::from_bytes_le(&[4]),
    ];
    let types = [PlaintextType::Field(BigUint::from_bytes_le(&[7]))];
    let params = ["0".to_string(), "2".to_string()];
    let result = vector_mul(
        &output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    )
    .unwrap();
    let expected_result = vec![BigUint::from_bytes_le(&[3]), BigUint::from_bytes_le(&[1])];
    assert_eq!(result, expected_result);

    // Try to use the plugin(vector, add, params) with 3 parameters instead of 2
    let incorrect_params = ["1".to_string(), "2".to_string()];
    let result = vector_mul(
        &output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &incorrect_params,
        &types,
    );
    assert!(result.is_err());
}
