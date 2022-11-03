use num_bigint::BigUint;
use std::collections::HashMap;

use crate::structs::count::CountList;
use crate::Result;

// @function(vector_add, @out: field_id: length, @in: field_id: length, field_id: length) @plugin(vector, add, field_id, length)
// This function takes as input two vectors `in1` and `in2` of length `length` containing elements from field `field_id`,
// This function returns one vector of length `length` such that `out[i] = in1[i] + in2[i] % field`
pub fn vector_add(
    output_count: &CountList,
    input_count: &CountList,
    inputs: &[&BigUint],
    params: &[String],
    fields: &[BigUint],
) -> Result<Vec<BigUint>> {
    // Check that params are compliant with the plugin vector and the operation add
    if params.len() != 2 {
        return Err(
            "plugin(vector, add) must be declared with 2 params (field_id, length).".into(),
        );
    }
    let param_field_id: u8 = params[0].parse().unwrap();
    let param_len: usize = params[1].parse().unwrap();
    if param_len == 0 {
        return Err("plugin(vector, add) cannot be called without inputs.".into());
    }
    let field_characteristic = fields.get(param_field_id as usize).ok_or_else(|| {
        format!(
            "plugin(vector, add) cannot be called with a field id ({}) which is not defined.",
            param_field_id
        )
    })?;

    // Check that `output_count` and `input_count` are compliant with `plugin(vector, add, params)`
    if *output_count != HashMap::from([(param_field_id, param_len as u64)]) {
        return Err(format!(
            "When calling the plugin(vector, add, {}, {}), the out parameter in the function signature must be equal to {}: {}.",
            param_field_id, param_len, param_field_id, param_len
        )
        .into());
    }

    if *input_count != HashMap::from([(param_field_id, 2 * param_len as u64)]) {
        return Err(format!(
            "When calling the plugin(vector, add, {}, {}), the in parameter in the function signature must be equal to {}: {}.",
            param_field_id, param_len, param_field_id, 2*param_len
        )
            .into());
    }

    // Check that `inputs` is compliant with `plugin(vector, add, params)`
    if inputs.len() != 2 * param_len {
        return Err(format!(
            "When calling the plugin(vector, add, {}, {}), we should have {} input values (and not {}).",
            param_field_id, param_len, 2*param_len, inputs.len()
        )
            .into());
    }

    // Evaluate plugin(vector_add)
    let mut result: Vec<BigUint> = vec![];
    for i in 0..param_len {
        result.push((inputs[i] + inputs[i + param_len]) % field_characteristic);
    }
    Ok(result)
}
