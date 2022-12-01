use num_bigint::BigUint;
use num_traits::Pow;
use std::collections::HashMap;

use crate::consumers::evaluator::PlaintextType;
use crate::structs::count::Count;
use crate::{Result, TypeId};

/// This function performs the following checks on zkif_ring_add/mul/equal inputs.
/// - there is no public/private inputs
/// - `params` are compliant with the plugin zkif_ring and the operation add/mul/equal
/// - `type_id` is defined and is a zkif_ring type
/// - `inputs` is compliant with `plugin(zkif_ring, add/mul/equal, params)`
/// - `inputs` belongs to the targeted ring
fn zkif_ring_check(
    inputs: &[&BigUint],
    public_inputs: &HashMap<TypeId, Vec<BigUint>>,
    private_inputs: &HashMap<TypeId, Vec<BigUint>>,
    params: &[String],
    types: &[PlaintextType],
) -> Result<(u8, BigUint)> {
    // Check that there is no public/private inputs
    if !public_inputs.is_empty() {
        return Err("plugin(zkif_ring, add/mul/equal) does not consume any public input.".into());
    }
    if !private_inputs.is_empty() {
        return Err("plugin(zkif_ring, add/mul/equal) does not consume any private input.".into());
    }

    // Check that params are compliant with the plugin zkif_ring and the operation add/mul
    if params.len() != 1 {
        return Err(
            "plugin(zkif_ring, add/mul/equal) must be declared with 1 param (type_id).".into(),
        );
    }
    let param_type_id = params[0].parse::<u8>()?;

    // Check that `type_id` is defined and is a zkif_ring plugin type.
    let type_ = types.get(param_type_id as usize).ok_or_else(|| {
        format!(
            "plugin(zkif_ring, add/mul/equal) cannot be called with a type id ({}) which is not defined.",
            param_type_id
        )
    })?;
    let modulo = match type_ {
        PlaintextType::Field(_) => {
            return Err(
                "plugin(zkif_ring, add/mul/equal) should be called on a ring PluginType.".into(),
            )
        }
        PlaintextType::PluginType(name, operation, ring_params) => {
            if !(name == "zkif_ring" && operation == "type") {
                return Err(
                    "plugin(zkif_ring, add/mul/equal) should be called on a zkif_ring PluginType."
                        .into(),
                );
            }
            if ring_params.len() != 2 {
                return Err(
                    "plugin(zkif_ring, type) must be declared with 2 params (base, exponent)."
                        .into(),
                );
            }
            let ring_base = BigUint::from(ring_params[0].parse::<u64>()?);
            let ring_exponent = ring_params[1].parse::<u64>()?;
            Pow::pow(ring_base, ring_exponent)
        }
    };

    // Check that `inputs` is compliant with `plugin(zkif_ring, add/mul, params)`
    if inputs.len() != 2 {
        return Err(format!(
            "When calling the plugin(zkif_ring, add/mul/equal, {}), we should have 2 input values (and not {}).",
            param_type_id, inputs.len()
        )
            .into());
    }
    // Check that `inputs` belongs to the targeted ring
    if !inputs.iter().all(|input| *input < &modulo) {
        return Err("When calling the plugin(zkif_ring, add/mul/equal, {}), some inputs do not belong to the targeted ring".into());
    }
    Ok((param_type_id, modulo))
}

/// This function performs the following checks on zkif_ring_add/mul inputs.
/// - there is no public/private inputs
/// - `params` are compliant with the plugin zkif_ring and the operation add/mul
/// - `type_id` is defined and is a zkif_ring type
/// - `output_count` and `input_count` are compliant with `plugin(zkif_ring, add/mul, params)`
/// - `inputs` is compliant with `plugin(zkif_ring, add/mul, params)`
/// - `inputs` belongs to the targeted ring
pub fn zkif_ring_add_mul_check(
    output_count: &[Count],
    input_count: &[Count],
    inputs: &[&BigUint],
    public_inputs: &HashMap<TypeId, Vec<BigUint>>,
    private_inputs: &HashMap<TypeId, Vec<BigUint>>,
    params: &[String],
    types: &[PlaintextType],
) -> Result<BigUint> {
    let (param_type_id, modulo) =
        zkif_ring_check(inputs, public_inputs, private_inputs, params, types)?;

    // Check that `output_count` and `input_count` are compliant with `plugin(zkif_ring, add/mul, params)`
    let expected_output_count = vec![Count::new(param_type_id, 1)];
    if *output_count != expected_output_count {
        return Err(format!(
            "When calling the plugin(zkif_ring, add/mul, {}), the out parameter in the function signature must be equal to {:?} (and not {:?}).",
            param_type_id, expected_output_count, output_count
        )
            .into());
    }

    let expected_input_count = vec![Count::new(param_type_id, 1), Count::new(param_type_id, 1)];
    if *input_count != expected_input_count {
        return Err(format!(
            "When calling the plugin(zkif_ring, add/mul, {}), the in parameter in the function signature must be equal to {:?} (and not {:?}).",
            param_type_id,  expected_input_count, input_count
        )
            .into());
    }
    Ok(modulo)
}

/// This function performs the following checks on zkif_ring_equal inputs.
/// - there is no public/private inputs
/// - `params` are compliant with the plugin zkif_ring and the operation add/mul/equal
/// - `type_id` is defined and is a zkif_ring type
/// - `output_count` and `input_count` are compliant with `plugin(zkif_ring, equal, params)`
/// - `inputs` is compliant with `plugin(zkif_ring, add/mul/equal, params)`
/// - `inputs` belongs to the targeted ring
pub fn zkif_ring_equal_check(
    output_count: &[Count],
    input_count: &[Count],
    inputs: &[&BigUint],
    public_inputs: &HashMap<TypeId, Vec<BigUint>>,
    private_inputs: &HashMap<TypeId, Vec<BigUint>>,
    params: &[String],
    types: &[PlaintextType],
) -> Result<()> {
    let (param_type_id, _) = zkif_ring_check(inputs, public_inputs, private_inputs, params, types)?;

    // Check that `output_count` and `input_count` are compliant with `plugin(zkif_ring, add/mul, params)`
    let expected_output_count = vec![];
    if *output_count != expected_output_count {
        return Err(format!(
            "When calling the plugin(zkif_ring, equal, {}), the out parameter in the function signature must be empty (and not {:?}).",
            param_type_id, output_count
        )
            .into());
    }

    let expected_input_count = vec![Count::new(param_type_id, 1), Count::new(param_type_id, 1)];
    if *input_count != expected_input_count {
        return Err(format!(
            "When calling the plugin(zkif_ring, equal, {}), the in parameter in the function signature must be equal to {:?} (and not {:?}).",
            param_type_id,  expected_input_count, input_count
        )
            .into());
    }
    Ok(())
}

/// @function(ring_add, @out: type_id: 1, @in: type_id: 1, type_id: 1) @plugin(zkif_ring, add, type_id)
/// This function takes as input two elements `in1` and `in2` from type `type_id`,
/// This function returns one element such that `out = in1 + in2 % type_modulo`
pub fn zkif_ring_add(
    output_count: &[Count],
    input_count: &[Count],
    inputs: &[&BigUint],
    public_inputs: &HashMap<TypeId, Vec<BigUint>>,
    private_inputs: &HashMap<TypeId, Vec<BigUint>>,
    params: &[String],
    types: &[PlaintextType],
) -> Result<BigUint> {
    let modulo = zkif_ring_add_mul_check(
        output_count,
        input_count,
        inputs,
        public_inputs,
        private_inputs,
        params,
        types,
    )?;

    // Evaluate plugin(zkif_ring, add)
    Ok((inputs[0] + inputs[1]) % modulo)
}

/// @function(ring_mul, @out: type_id: 1, @in: type_id: 1, type_id: 1) @plugin(zkif_ring, mul, type_id)
/// This function takes as input two elements `in1` and `in2` from type `type_id`,
/// This function returns one element such that `out = in1 * in2 % type_modulo`
pub fn zkif_ring_mul(
    output_count: &[Count],
    input_count: &[Count],
    inputs: &[&BigUint],
    public_inputs: &HashMap<TypeId, Vec<BigUint>>,
    private_inputs: &HashMap<TypeId, Vec<BigUint>>,
    params: &[String],
    types: &[PlaintextType],
) -> Result<BigUint> {
    let modulo = zkif_ring_add_mul_check(
        output_count,
        input_count,
        inputs,
        public_inputs,
        private_inputs,
        params,
        types,
    )?;

    // Evaluate plugin(zkif_ring, mul)
    Ok((inputs[0] * inputs[1]) % modulo)
}

/// @function(ring_equal, @in: type_id: 1, type_id: 1) @plugin(zkif_ring, equal, type_id)
/// This function takes as input two elements `in1` and `in2` from type `type_id`,
/// This function returns an error if and only if `in1 != in2`
pub fn zkif_ring_equal(
    output_count: &[Count],
    input_count: &[Count],
    inputs: &[&BigUint],
    public_inputs: &HashMap<TypeId, Vec<BigUint>>,
    private_inputs: &HashMap<TypeId, Vec<BigUint>>,
    params: &[String],
    types: &[PlaintextType],
) -> Result<()> {
    zkif_ring_equal_check(
        output_count,
        input_count,
        inputs,
        public_inputs,
        private_inputs,
        params,
        types,
    )?;

    // Evaluate plugin(zkif_ring, equal)
    if inputs[0] == inputs[1] {
        Ok(())
    } else {
        Err(format!(
            "In plugin(zkif_ring, equal), inputs are not equal ({} != {})",
            inputs[0], inputs[1]
        )
        .into())
    }
}

#[test]
fn test_zkif_ring_check() {
    let inputs = [&BigUint::from_bytes_le(&[1]), &BigUint::from_bytes_le(&[2])];
    let types = [
        PlaintextType::Field(BigUint::from_bytes_le(&[7])),
        PlaintextType::PluginType(
            "zkif_ring".to_string(),
            "type".to_string(),
            vec!["2".to_string(), "6".to_string()],
        ),
    ];
    let params = ["1".to_string()];
    let result =
        zkif_ring_check(&inputs, &HashMap::new(), &HashMap::new(), &params, &types).unwrap();
    let expected_result = (1_u8, BigUint::from_bytes_le(&[64]));
    assert_eq!(result, expected_result);

    // Try to use the plugin zkif_ring with a Field type_id
    let incorrect_params = ["0".to_string()];
    let result = zkif_ring_check(
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &incorrect_params,
        &types,
    );
    assert!(result.is_err());

    // Try to use the plugin zkif_ring with an unknown type_id
    let incorrect_params = ["2".to_string()];
    let result = zkif_ring_check(
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &incorrect_params,
        &types,
    );
    assert!(result.is_err());

    // Try to use the plugin zkif_ring with a type_id which cannot be parsed into u8
    let incorrect_params = ["a".to_string()];
    let result = zkif_ring_check(
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &incorrect_params,
        &types,
    );
    assert!(result.is_err());

    // Try to use the plugin zkif_ring with an incorrect number of inputs
    let incorrect_inputs = [
        &BigUint::from_bytes_le(&[1]),
        &BigUint::from_bytes_le(&[2]),
        &BigUint::from_bytes_le(&[3]),
    ];
    let result = zkif_ring_check(
        &incorrect_inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    );
    assert!(result.is_err());

    // Try to use the plugin zkif_ring with inputs which does not belong to the targeted ring
    let incorrect_inputs = [
        &BigUint::from_bytes_le(&[200]),
        &BigUint::from_bytes_le(&[2]),
    ];
    let result = zkif_ring_check(
        &incorrect_inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    );
    assert!(result.is_err());
}

#[test]
fn test_zkif_ring_add_mul_check() {
    let output_count = vec![Count::new(1, 1)];
    let input_count = vec![Count::new(1, 1), Count::new(1, 1)];
    let inputs = [&BigUint::from_bytes_le(&[1]), &BigUint::from_bytes_le(&[2])];
    let types = [
        PlaintextType::Field(BigUint::from_bytes_le(&[7])),
        PlaintextType::PluginType(
            "zkif_ring".to_string(),
            "type".to_string(),
            vec!["2".to_string(), "6".to_string()],
        ),
    ];
    let params = ["1".to_string()];
    let result = zkif_ring_add_mul_check(
        &output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    )
    .unwrap();
    let expected_result = BigUint::from_bytes_le(&[64]);
    assert_eq!(result, expected_result);

    // Try to use the plugin zkif_vector with an incorrect input_count
    let incorrect_input_count = vec![Count::new(1, 1), Count::new(1, 2)];
    let result = zkif_ring_add_mul_check(
        &output_count,
        &incorrect_input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    );
    assert!(result.is_err());

    // Try to use the plugin zkif_vector with an incorrect input_count
    let incorrect_output_count = vec![Count::new(1, 2)];
    let result = zkif_ring_add_mul_check(
        &incorrect_output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    );
    assert!(result.is_err());
}

#[test]
fn test_zkif_ring_equal_check() {
    let output_count = vec![];
    let input_count = vec![Count::new(1, 1), Count::new(1, 1)];
    let inputs = [&BigUint::from_bytes_le(&[1]), &BigUint::from_bytes_le(&[2])];
    let types = [
        PlaintextType::Field(BigUint::from_bytes_le(&[7])),
        PlaintextType::PluginType(
            "zkif_ring".to_string(),
            "type".to_string(),
            vec!["2".to_string(), "6".to_string()],
        ),
    ];
    let params = ["1".to_string()];
    let result = zkif_ring_equal_check(
        &output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    );
    assert!(result.is_ok());

    // Try to use the plugin zkif_vector with an incorrect input_count
    let incorrect_input_count = vec![Count::new(1, 1), Count::new(1, 2)];
    let result = zkif_ring_equal_check(
        &output_count,
        &incorrect_input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    );
    assert!(result.is_err());

    // Try to use the plugin zkif_vector with an incorrect input_count
    let incorrect_output_count = vec![Count::new(1, 2)];
    let result = zkif_ring_equal_check(
        &incorrect_output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    );
    assert!(result.is_err());
}

#[test]
fn test_zkif_ring_add() {
    let output_count = vec![Count::new(1, 1)];
    let input_count = vec![Count::new(1, 1), Count::new(1, 1)];
    let inputs = [
        &BigUint::from_bytes_le(&[60]),
        &BigUint::from_bytes_le(&[10]),
    ];
    let types = [
        PlaintextType::Field(BigUint::from_bytes_le(&[7])),
        PlaintextType::PluginType(
            "zkif_ring".to_string(),
            "type".to_string(),
            vec!["2".to_string(), "6".to_string()],
        ),
    ];
    let params = ["1".to_string()];
    let result = zkif_ring_add(
        &output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    )
    .unwrap();
    let expected_result = BigUint::from_bytes_le(&[6]);
    assert_eq!(result, expected_result);
}

#[test]
fn test_zkif_ring_mul() {
    let output_count = vec![Count::new(1, 1)];
    let input_count = vec![Count::new(1, 1), Count::new(1, 1)];
    let inputs = [
        &BigUint::from_bytes_le(&[8]),
        &BigUint::from_bytes_le(&[10]),
    ];
    let types = [
        PlaintextType::Field(BigUint::from_bytes_le(&[7])),
        PlaintextType::PluginType(
            "zkif_ring".to_string(),
            "type".to_string(),
            vec!["2".to_string(), "6".to_string()],
        ),
    ];
    let params = ["1".to_string()];
    let result = zkif_ring_mul(
        &output_count,
        &input_count,
        &inputs,
        &HashMap::new(),
        &HashMap::new(),
        &params,
        &types,
    )
    .unwrap();
    let expected_result = BigUint::from_bytes_le(&[16]);
    assert_eq!(result, expected_result);
}
