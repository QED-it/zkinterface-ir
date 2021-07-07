use crate::Result;
use crate::sieve_ir_generated::sieve_ir as g;
use crate::WireId;
use serde::{Deserialize, Serialize};
use flatbuffers::{FlatBufferBuilder, Vector, WIPOffset};
use crate::serde::export::TryFrom;
use std::error::Error;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Deserialize, Serialize)]
pub enum WireListElement {
    Wire(WireId),
    WireRange(WireId, WireId),
}
use WireListElement::*;
use crate::flatbuffers::ForwardsUOffset;

pub type WireList = Vec<WireListElement>;

// =========================================
//                Wire
// =========================================

/// Convert from Flatbuffers references to owned structure.
pub fn from_id(g_wire: &g::Wire) -> WireId {
    g_wire.id()
}

/// Convert from a Flatbuffers vector to owned structures.
pub fn from_ids_vector<'a>(g_vector: &[g::Wire]) -> Vec<WireId> {
    g_vector.iter().map(|g_wire| g_wire.id()).collect()
}

/// Add this structure into a Flatbuffers message builder.
pub fn build_wire<'bldr: 'mut_bldr, 'mut_bldr>(
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    id: WireId,
) -> WIPOffset<g::Wire<'bldr>> {
    g::Wire::create(builder, &g::WireArgs {id})
}

/// Add this structure into a Flatbuffers message builder.
pub fn build_wires_vector<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    wires: &'args [WireId],
) -> WIPOffset<Vector<'bldr, ForwardsUOffset<g::Wire<'bldr>>>> {
    let g_wires: Vec<_> = wires.iter().map(|id| build_wire(builder, *id)).collect();
    builder.create_vector(&g_wires)
}


// =========================================
//              WireRange
// =========================================
/// Convert from Flatbuffers references to owned structure.
pub fn from_range(g_wirerange: &g::WireRange) -> Result<(WireId, WireId)> {
    Ok((
        g_wirerange.first().ok_or("Missing start value in range")?.id(),
        g_wirerange.last().ok_or("Missing end value in range")?.id(),
    ))
}

/// Convert from Flatbuffers references to owned structure.
pub fn build_range<'bldr: 'mut_bldr, 'mut_bldr>(
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    first: WireId,
    last: WireId,
) -> WIPOffset<g::WireRange<'bldr>> {
    let fbs_first = build_wire(builder, first);
    let fbs_last = build_wire(builder, last);
    g::WireRange::create(
        builder,
        &g::WireRangeArgs {
            first: Some(fbs_first),
            last: Some(fbs_last),
        }
    )
}

// =========================================
//       WireListElement + WireList
// =========================================

impl<'a> TryFrom<g::WireListElement<'a>> for WireListElement {
    type Error = Box<dyn Error>;

    fn try_from(element: g::WireListElement<'a>) -> Result<Self> {
        Ok(match element.element_type() {
            g::WireListElementU::NONE => return Err("Unknown type in WireListElement".into()),
            g::WireListElementU::Wire => Wire(element.element_as_wire().unwrap().id()),
            g::WireListElementU::WireRange => {
                let range = element.element_as_wire_range().unwrap();
                WireRange(
                    range.first().ok_or("Missing first value of range")?.id(),
                    range.last().ok_or("Missing last value of range")?.id(),
                )
            },
        })
    }
}

impl WireListElement {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::WireListElement<'bldr>> {
        match self {
            Wire(id) => {
                let wire = build_wire(builder, *id);
                g::WireListElement::create(
                    builder,
                    &g::WireListElementArgs{
                        element_type: g::WireListElementU::Wire,
                        element: Some(wire.as_union_value())
                })
            }
            WireRange(first, last) => {
                let wire_first = build_wire(builder, *first);
                let wire_last = build_wire(builder, *last);

                let range = g::WireRange::create(
                    builder,
                    &g::WireRangeArgs{
                        first: Some(wire_first),
                        last: Some(wire_last),
                    }
                );

                g::WireListElement::create(
                    builder,
                    &g::WireListElementArgs{
                        element_type: g::WireListElementU::WireRange,
                        element: Some(range.as_union_value())
                })
            }
        }
    }
}

impl<'a> TryFrom<g::WireList<'a>> for WireList {
    type Error = Box<dyn Error>;

    fn try_from(list: g::WireList<'a>) -> Result<Self> {
        let fbs_vector = list.elements().ok_or("Missing wire list")?;
        let mut elements = vec![];
        for i in 0..fbs_vector.len() {
            let a = fbs_vector.get(i);
            elements.push(WireListElement::try_from(a)?);
        }
        Ok(WireList::from(elements))
    }
}

/// Add a vector of this structure into a Flatbuffers message builder.
pub fn build_wire_list<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    elements: &'args WireList,
) -> WIPOffset<g::WireList<'bldr>> {
    let g_elements: Vec<_> = elements.iter().map(|element| element.build(builder)).collect();
    let g_vector = builder.create_vector(&g_elements);

    g::WireList::create(
        builder,
        &g::WireListArgs {
            elements: Some(g_vector)
        }
    )
}

