use crate::sieve_ir_generated::sieve_ir as g;
use crate::WireId;
use flatbuffers::{FlatBufferBuilder, Vector, WIPOffset};

/// Convert from Flatbuffers references to owned structure.
pub fn from_id(g_wire: &g::Wire) -> WireId {
    g_wire.id()
}

/// Convert from a Flatbuffers vector to owned structures.
pub fn from_ids_vector<'a>(g_vector: &[g::Wire]) -> Vec<WireId> {
    g_vector.iter().map(|g_wire| g_wire.id()).collect()
}

/// Add this structure into a Flatbuffers message builder.
pub fn build_wire(id: WireId) -> g::Wire {
    g::Wire::new(id)
}

/// Add this structure into a Flatbuffers message builder.
pub fn build_wires_vector<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    wires: &'args [WireId],
) -> WIPOffset<Vector<'bldr, g::Wire>> {
    let g_wires: Vec<_> = wires.iter().map(|id| g::Wire::new(*id)).collect();
    builder.create_vector(&g_wires)
}
