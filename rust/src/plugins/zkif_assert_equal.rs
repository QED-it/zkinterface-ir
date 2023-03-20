use num_bigint::BigUint;
use num_traits::Zero;
use std::collections::BTreeMap;
use std::convert::TryFrom;

use crate::plugins::evaluate_plugin::extract_number;
use crate::structs::count::Count;
use crate::{Result, TypeId};

/// This function performs the following checks on zkif_assert_equal_public/private inputs.
/// - params are compliant with the plugin zkif_assert_equal and the operation public/private
/// - output_count is empty
/// - `input_count` is compliant with `plugin(zkif_assert_equal, public/private, params)`
/// - `inputs` is compliant with `plugin(zkif_assert_equal, public/private, params)`
/// - `public_inputs` and `private_inputs` are compliant with `plugin(zkif_assert_equal, public/private, params)`
fn zkif_assert_equal_check<'a>(
    output_count: &'a [Count],
    input_count: &'a [Count],
    inputs: &'a [&BigUint],
    public_inputs: &'a BTreeMap<TypeId, Vec<BigUint>>,
    private_inputs: &'a BTreeMap<TypeId, Vec<BigUint>>,
    params: &'a [String],
    is_public: bool,
) -> Result<&'a Vec<BigUint>> {
    // Check that params are compliant with the plugin zkif_assert_equal and the operation public/private
    if params.len() != 2 {
        return Err(
            "plugin(zkif_vector, add/mul) must be declared with 2 params (type_id, length).".into(),
        );
    }
    let param_type_id = u8::try_from(extract_number(&params[0])?)?;
    let param_len = usize::try_from(extract_number(&params[1])?)?;
    if param_len == 0 {
        return Err(
            "plugin(zkif_assert_equal, public/private) cannot be called without inputs.".into(),
        );
    }

    // Check that output_count is empty
    if !output_count.is_empty() {
        return Err("plugin(zkif_assert_equal, public/private) does not return any output.".into());
    }

    // Check that `input_count` is compliant with `plugin(zkif_assert_equal, public/private, params)`
    let expected_input_count = vec![Count::new(param_type_id, u64::try_from(param_len)?)];
    if *input_count != expected_input_count {
        return Err(format!(
            "When calling the plugin(zkif_assert_equal, public/private, {}, {}), the in parameter in the function signature must be equal to {:?} (and not {:?}).",
            param_type_id, param_len, expected_input_count, input_count
        )
            .into());
    }

    // Check that `inputs` is compliant with `plugin(zkif_assert_equal, public/private, params)`
    if inputs.len() != param_len {
        return Err(format!(
            "When calling the plugin(zkif_assert_equal, public/private, {}, {}), we should have {} input values (and not {}).",
            param_type_id, param_len, 2*param_len, inputs.len()
        )
            .into());
    }

    // Check that `public_inputs` and `private_inputs` are compliant with `plugin(zkif_assert_equal, public/private, params)`
    let values = if is_public {
        if !private_inputs.is_empty() {
            return Err(
                "plugin(zkif_assert_equal, public) does not consume any private input.".into(),
            );
        }
        // Check that we have only one key (type_id) in public_inputs
        if public_inputs.keys().len() != 1 {
            return Err("plugin(zkif_assert_equal, public, type_id, length) should consume `length` public inputs from `type_id`.".into());
        }
        let values = public_inputs.get(&param_type_id).ok_or("plugin(zkif_assert_equal, public, type_id, length) should consume `length` public inputs from `type_id`.")?;
        if values.len() != param_len {
            return Err("plugin(zkif_assert_equal, public, type_id, length) should consume `length` public inputs from `type_id`.".into());
        }
        values
    } else {
        if !public_inputs.is_empty() {
            return Err(
                "plugin(zkif_assert_equal, private) does not consume any public input.".into(),
            );
        }
        // Check that we have only one key (type_id) in private_inputs
        if private_inputs.keys().len() != 1 {
            return Err("plugin(zkif_assert_equal, private, type_id, length) should consume `length` private inputs from `type_id`.".into());
        }
        let values = private_inputs.get(&param_type_id).ok_or("plugin(zkif_assert_equal, private, type_id, length) should consume `length` private inputs from `type_id`.")?;
        if values.len() != param_len {
            return Err("plugin(zkif_assert_equal, private, type_id, length) should consume `length` private inputs from `type_id`.".into());
        }
        values
    };

    Ok(values)
}

/// @function(assert_equal_public, @in: type_id: length) @plugin(zkif_assert_equal, public, type_id, length)
/// This function takes as input one vector `in` of length `length` containing elements from type `type_id`,
/// it consumes `length` public inputs from type `type_id` and stores them in one vector `public_in`,
/// then it checks that `in` is equal to `public_in`.
pub fn zkif_assert_equal_public(
    output_count: &[Count],
    input_count: &[Count],
    inputs: &[&BigUint],
    public_inputs: &BTreeMap<TypeId, Vec<BigUint>>,
    private_inputs: &BTreeMap<TypeId, Vec<BigUint>>,
    params: &[String],
) -> Result<()> {
    let pub_inputs = zkif_assert_equal_check(
        output_count,
        input_count,
        inputs,
        public_inputs,
        private_inputs,
        params,
        true,
    )?;

    // Evaluate plugin(zkif_assert_equal, public)
    if inputs.len() != pub_inputs.len() {
        panic!("Unreachable case: it has already been checked in zkif_assert_equal_check")
    }
    if inputs.iter().zip(pub_inputs.iter()).all(|(val1, val2)| {
        let val = *val1 - val2;
        val.is_zero()
    }) {
        Ok(())
    } else {
        Err(
            "In plugin(zkif_assert_equal, public), all inputs are not equal to public inputs"
                .into(),
        )
    }
}

/// @function(assert_equal_private, @in: type_id: length) @plugin(zkif_assert_equal, private, type_id, length)
/// This function takes as input one vector `in` of length `length` containing elements from type `type_id`,
/// it consumes `length` private inputs from type `type_id` and stores them in one vector `private_in`,
/// then it checks that `in` is equal to `private_in`.
pub fn zkif_assert_equal_private(
    output_count: &[Count],
    input_count: &[Count],
    inputs: &[&BigUint],
    public_inputs: &BTreeMap<TypeId, Vec<BigUint>>,
    private_inputs: &BTreeMap<TypeId, Vec<BigUint>>,
    params: &[String],
) -> Result<()> {
    let priv_inputs = zkif_assert_equal_check(
        output_count,
        input_count,
        inputs,
        public_inputs,
        private_inputs,
        params,
        false,
    )?;

    // Evaluate plugin(zkif_assert_equal, private)
    if inputs.len() != priv_inputs.len() {
        panic!("Unreachable case: it has already been checked in assert_equal_check")
    }
    if inputs.iter().zip(priv_inputs.iter()).all(|(val1, val2)| {
        let val = *val1 - val2;
        val.is_zero()
    }) {
        Ok(())
    } else {
        Err(
            "In plugin(zkif_assert_equal, private), all inputs are not equal to private inputs"
                .into(),
        )
    }
}

#[test]
fn test_zkif_assert_equal_check() {
    let output_count = vec![];
    let input_count = vec![Count::new(1, 3)];
    let inputs = [
        &BigUint::from_bytes_le(&[1]),
        &BigUint::from_bytes_le(&[2]),
        &BigUint::from_bytes_le(&[3]),
    ];
    let public_inputs = BTreeMap::from([(
        1,
        vec![
            BigUint::from_bytes_le(&[1]),
            BigUint::from_bytes_le(&[2]),
            BigUint::from_bytes_le(&[3]),
        ],
    )]);
    let private_inputs = BTreeMap::new();
    let params = ["1".to_string(), "3".to_string()];
    let result = zkif_assert_equal_check(
        &output_count,
        &input_count,
        &inputs,
        &public_inputs,
        &private_inputs,
        &params,
        true,
    )
    .unwrap();
    let expected_result = vec![
        BigUint::from_bytes_le(&[1]),
        BigUint::from_bytes_le(&[2]),
        BigUint::from_bytes_le(&[3]),
    ];
    assert_eq!(*result, expected_result);

    let result = zkif_assert_equal_check(
        &output_count,
        &input_count,
        &inputs,
        &private_inputs,
        &public_inputs,
        &params,
        false,
    )
    .unwrap();
    assert_eq!(*result, expected_result);

    // Try to use the plugin zkif_assert_equal with not the correct number of params
    let incorrect_params = ["1".to_string(), "3".to_string(), "0".to_string()];
    let result = zkif_assert_equal_check(
        &output_count,
        &input_count,
        &inputs,
        &public_inputs,
        &private_inputs,
        &incorrect_params,
        true,
    );
    assert!(result.is_err());

    // Try to use the plugin zkif_assert_equal with a non empty output count
    let incorrect_output_count = vec![Count::new(0, 5)];
    let result = zkif_assert_equal_check(
        &incorrect_output_count,
        &input_count,
        &inputs,
        &public_inputs,
        &private_inputs,
        &params,
        true,
    );
    assert!(result.is_err());

    // Try to use the plugin zkif_assert_equal with an incorrect number of inputs
    let incorrect_inputs = [&BigUint::from_bytes_le(&[1]), &BigUint::from_bytes_le(&[2])];
    let result = zkif_assert_equal_check(
        &output_count,
        &input_count,
        &incorrect_inputs,
        &public_inputs,
        &private_inputs,
        &params,
        true,
    );
    assert!(result.is_err());

    // Try to use the plugin zkif_assert_equal with an incorrect number of public inputs
    let incorrect_public_inputs = BTreeMap::from([(
        1,
        vec![
            BigUint::from_bytes_le(&[1]),
            BigUint::from_bytes_le(&[2]),
            BigUint::from_bytes_le(&[3]),
            BigUint::from_bytes_le(&[4]),
        ],
    )]);
    let result = zkif_assert_equal_check(
        &output_count,
        &input_count,
        &inputs,
        &incorrect_public_inputs,
        &private_inputs,
        &params,
        true,
    );
    assert!(result.is_err());

    // Try to use the plugin zkif_assert_equal with an incorrect type_id for the public inputs
    let incorrect_public_inputs = BTreeMap::from([(
        2,
        vec![
            BigUint::from_bytes_le(&[1]),
            BigUint::from_bytes_le(&[2]),
            BigUint::from_bytes_le(&[3]),
        ],
    )]);
    let result = zkif_assert_equal_check(
        &output_count,
        &input_count,
        &inputs,
        &incorrect_public_inputs,
        &private_inputs,
        &params,
        true,
    );
    assert!(result.is_err());
}

#[test]
fn test_zkif_assert_equal_public() {
    let output_count = vec![];
    let input_count = vec![Count::new(1, 3)];
    let inputs = [
        &BigUint::from_bytes_le(&[1]),
        &BigUint::from_bytes_le(&[2]),
        &BigUint::from_bytes_le(&[3]),
    ];
    let public_inputs = BTreeMap::from([(
        1,
        vec![
            BigUint::from_bytes_le(&[1]),
            BigUint::from_bytes_le(&[2]),
            BigUint::from_bytes_le(&[3]),
        ],
    )]);
    let private_inputs = BTreeMap::new();
    let params = ["1".to_string(), "3".to_string()];
    let result = zkif_assert_equal_public(
        &output_count,
        &input_count,
        &inputs,
        &public_inputs,
        &private_inputs,
        &params,
    );
    assert!(result.is_ok());

    let incorrect_inputs = [
        &BigUint::from_bytes_le(&[1]),
        &BigUint::from_bytes_le(&[2]),
        &BigUint::from_bytes_le(&[4]),
    ];
    let result = zkif_assert_equal_public(
        &output_count,
        &input_count,
        &incorrect_inputs,
        &public_inputs,
        &private_inputs,
        &params,
    );
    assert!(result.is_err());
}

#[test]
fn test_zkif_assert_equal_private() {
    let output_count = vec![];
    let input_count = vec![Count::new(1, 3)];
    let inputs = [
        &BigUint::from_bytes_le(&[1]),
        &BigUint::from_bytes_le(&[2]),
        &BigUint::from_bytes_le(&[3]),
    ];
    let public_inputs = BTreeMap::new();
    let private_inputs = BTreeMap::from([(
        1,
        vec![
            BigUint::from_bytes_le(&[1]),
            BigUint::from_bytes_le(&[2]),
            BigUint::from_bytes_le(&[3]),
        ],
    )]);
    let params = ["1".to_string(), "3".to_string()];
    let result = zkif_assert_equal_private(
        &output_count,
        &input_count,
        &inputs,
        &public_inputs,
        &private_inputs,
        &params,
    );
    assert!(result.is_ok());

    let incorrect_inputs = [
        &BigUint::from_bytes_le(&[1]),
        &BigUint::from_bytes_le(&[2]),
        &BigUint::from_bytes_le(&[4]),
    ];
    let result = zkif_assert_equal_private(
        &output_count,
        &input_count,
        &incorrect_inputs,
        &public_inputs,
        &private_inputs,
        &params,
    );
    assert!(result.is_err());
}
