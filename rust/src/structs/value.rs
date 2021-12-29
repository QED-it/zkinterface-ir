use crate::sieve_ir_generated::sieve_ir as g;
use crate::Result;
use flatbuffers::{FlatBufferBuilder, ForwardsUOffset, Vector, WIPOffset};

/// A Value is a field element encoded least-significant-byte-first (little-endian). Trailing zeros may be omitted.
///
/// Example: the element `one` on a 32 bits fields is encoded `[1, 0, 0, 0]`.
/// The compact form `[1]` is also valid.
pub type Value = Vec<u8>;

/// Convert from Flatbuffers references to owned structure.
pub fn try_from_value(g_value: g::Value) -> Result<Value> {
    Ok(Vec::from(g_value.value().ok_or_else(|| "Missing value")?))
}

/// Convert from a Flatbuffers vector to owned structures.
pub fn try_from_values_vector<'a>(
    g_vector: Vector<'a, ForwardsUOffset<g::Value<'a>>>,
) -> Result<Vec<Value>> {
    let mut values = vec![];
    for i in 0..g_vector.len() {
        let g_a = g_vector.get(i);
        values.push(try_from_value(g_a)?);
    }
    Ok(values)
}

/// Add this structure into a Flatbuffers message builder.
pub fn build_value<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    value: &'args Value,
) -> WIPOffset<g::Value<'bldr>> {
    let value = builder.create_vector(&value[..]);
    g::Value::create(builder, &g::ValueArgs { value: Some(value) })
}

/// Add this structure into a Flatbuffers message builder.
pub fn build_values_vector<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    values: &'args [Value],
) -> WIPOffset<Vector<'bldr, ForwardsUOffset<g::Value<'bldr>>>> {
    let g_values: Vec<_> = values
        .iter()
        .map(|g_value| build_value(builder, g_value))
        .collect();
    builder.create_vector(&g_values)
}
