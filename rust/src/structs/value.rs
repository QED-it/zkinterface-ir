use crate::sieve_ir_generated::sieve_ir as generated;
use crate::Result;
use flatbuffers::{FlatBufferBuilder, ForwardsUOffset, Vector, WIPOffset};
use num_bigint_dig;
use num_bigint_dig::prime::probably_prime;

/// A Value is a field element encoded least-significant-byte-first (little-endian). Trailing zeros may be omitted.
///
/// Example: the element `one` on a 32 bits fields is encoded `[1, 0, 0, 0]`.
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
pub fn build_value<'bldr>(
    builder: &mut FlatBufferBuilder<'bldr>,
    value: &Value,
) -> WIPOffset<generated::Value<'bldr>> {
    let value = builder.create_vector(&value[..]);
    generated::Value::create(builder, &generated::ValueArgs { value: Some(value) })
}

/// Add this structure into a Flatbuffers message builder.
pub fn build_values_vector<'bldr>(
    builder: &mut FlatBufferBuilder<'bldr>,
    values: &[Value],
) -> WIPOffset<Vector<'bldr, ForwardsUOffset<generated::Value<'bldr>>>> {
    let g_values: Vec<_> = values
        .iter()
        .map(|g_value| build_value(builder, g_value))
        .collect();
    builder.create_vector(&g_values)
}

pub fn is_probably_prime(value: &Value) -> bool {
    let value = num_bigint_dig::BigUint::from_bytes_le(value);
    probably_prime(&value, 10)
}

#[test]
fn test_is_probably_prime() {
    let value: Value = vec![187]; // 187=11*17
    assert!(!is_probably_prime(&value));

    let value: Value = vec![101];
    assert!(is_probably_prime(&value));
}
