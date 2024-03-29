use crate::sieve_ir_generated::sieve_ir as generated;
use crate::Result;
use flatbuffers::{FlatBufferBuilder, ForwardsUOffset, Vector, WIPOffset};
use num_bigint::BigUint;

/// A Value is a type element encoded least-significant-byte-first (little-endian). Trailing zeros may be omitted.
///
/// Example: the element `one` on a 32 bits type is encoded `[1, 0, 0, 0]`.
/// The compact form `[1]` is also valid.
pub type Value = Vec<u8>;

/// Convert from Flatbuffers references to owned structure.
pub fn try_from_value(g_value: generated::Value) -> Result<Value> {
    Ok(Vec::from(g_value.value().ok_or("Missing value")?))
}

/// Convert from a Flatbuffers vector to owned structures.
pub fn try_from_values_vector<'a>(
    g_vector: Vector<'a, ForwardsUOffset<generated::Value<'a>>>,
) -> Result<Vec<Value>> {
    let mut values = vec![];
    for i in 0..g_vector.len() {
        let g_a = g_vector.get(i);
        values.push(try_from_value(g_a)?);
    }
    Ok(values)
}

/// Add this structure into a Flatbuffers message builder.
pub fn build_value<'a>(
    builder: &mut FlatBufferBuilder<'a>,
    value: &Value,
) -> WIPOffset<generated::Value<'a>> {
    let value = builder.create_vector(&value[..]);
    generated::Value::create(builder, &generated::ValueArgs { value: Some(value) })
}

/// Add this structure into a Flatbuffers message builder.
pub fn build_values_vector<'a>(
    builder: &mut FlatBufferBuilder<'a>,
    values: &[Value],
) -> WIPOffset<Vector<'a, ForwardsUOffset<generated::Value<'a>>>> {
    let g_values: Vec<_> = values
        .iter()
        .map(|g_value| build_value(builder, g_value))
        .collect();
    builder.create_vector(&g_values)
}

pub fn value_to_biguint(value: &[u8]) -> BigUint {
    BigUint::from_bytes_le(value)
}

pub fn remove_trailing_zeros(value: &Value) -> Value {
    if let Some(last) = value.iter().rposition(|c| *c != 0) {
        value[0..=last].to_vec()
    } else {
        vec![]
    }
}

#[test]
fn test_remove_trailing_zeros() {
    let value: Value = vec![187, 5, 0, 0];
    let clean_value = remove_trailing_zeros(&value);
    let expected_value = vec![187, 5];
    assert_eq!(clean_value, expected_value);

    let value: Value = vec![0, 187, 0, 5, 0, 0];
    let clean_value = remove_trailing_zeros(&value);
    let expected_value = vec![0, 187, 0, 5];
    assert_eq!(clean_value, expected_value);
}
