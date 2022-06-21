use crate::sieve_ir_generated::sieve_ir as g;
use crate::Result;
use crate::WireId;
use flatbuffers::{FlatBufferBuilder, ForwardsUOffset, Vector, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;

/// A WireListElement is either a single wire, or a range.
#[derive(Clone, Debug, Eq, PartialEq, Hash, Deserialize, Serialize)]
pub enum WireListElement {
    Wire(WireId),
    WireRange(WireId, WireId),
}
use WireListElement::*;

/// A WireList is simply a vector of WireListElement
/// #[derive(Clone)]
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
    g::Wire::create(builder, &g::WireArgs { id })
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
        g_wirerange
            .first()
            .ok_or_else(|| "Missing start value in range")?
            .id(),
        g_wirerange
            .last()
            .ok_or_else(|| "Missing end value in range")?
            .id(),
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
        },
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
                    range
                        .first()
                        .ok_or_else(|| "Missing first value of range")?
                        .id(),
                    range
                        .last()
                        .ok_or_else(|| "Missing last value of range")?
                        .id(),
                )
            }
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
                    &g::WireListElementArgs {
                        element_type: g::WireListElementU::Wire,
                        element: Some(wire.as_union_value()),
                    },
                )
            }
            WireRange(first, last) => {
                let range = build_range(builder, *first, *last);
                g::WireListElement::create(
                    builder,
                    &g::WireListElementArgs {
                        element_type: g::WireListElementU::WireRange,
                        element: Some(range.as_union_value()),
                    },
                )
            }
        }
    }
}

impl<'a> TryFrom<g::WireList<'a>> for WireList {
    type Error = Box<dyn Error>;

    fn try_from(list: g::WireList<'a>) -> Result<Self> {
        let fbs_vector = list.elements().ok_or_else(|| "Missing wire list")?;
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
    elements: &'args [WireListElement],
) -> WIPOffset<g::WireList<'bldr>> {
    let g_elements: Vec<_> = elements
        .iter()
        .map(|element| element.build(builder))
        .collect();
    let g_vector = builder.create_vector(&g_elements);

    g::WireList::create(
        builder,
        &g::WireListArgs {
            elements: Some(g_vector),
        },
    )
}

/// Expand a WireListElement into a vector of Result<WireId>.
pub fn expand_wirelistelement(wire: &WireListElement) -> Vec<Result<WireId>> {
    match wire {
        WireListElement::Wire(val) => vec![Ok(*val)],
        WireListElement::WireRange(first, last) => {
            if last <= first {
                vec![Err(format!(
                    "In WireRange, last WireId ({}) must be strictly greater than first WireId ({}).",
                    last, first
                )
                    .into())]
            } else {
                (*first..=*last).map(|item| Ok(item)).collect()
            }
        }
    }
}

/// Expand a WireList into a vector of individual WireId.
pub fn expand_wirelist(wirelist: &WireList) -> Result<Vec<WireId>> {
    let res = wirelist
        .iter()
        .flat_map(|wire| expand_wirelistelement(wire))
        .collect::<Result<Vec<WireId>>>()?;
    Ok(res)
}

#[test]
fn test_expand_wirelist() {
    let wirelist = vec![WireRange(0, 2), Wire(5)];
    let new_wirelist = expand_wirelist(&wirelist).unwrap();
    let correct_wirelist: Vec<WireId> = vec![0, 1, 2, 5];
    assert_eq!(new_wirelist, correct_wirelist);

    let wirelist = vec![WireRange(0, 1), WireRange(2, 2), Wire(5)];
    let new_wirelist = expand_wirelist(&wirelist);
    assert!(new_wirelist.is_err());

    let wirelist = vec![WireRange(0, 1), WireRange(4, 2), Wire(5)];
    let new_wirelist = expand_wirelist(&wirelist);
    assert!(new_wirelist.is_err());
}

pub fn wirelist_len(wirelist: &WireList) -> usize {
    wirelist
        .iter()
        .map(|wire| match wire {
            WireListElement::Wire(_) => 1,
            WireListElement::WireRange(first, last) => (*last as usize) - (*first as usize) + 1,
        })
        .sum()
}

/// Go through `wirelist` and replace `old_wire` by `new_wire`
/// Do not modify wirelist if old_wire does not belong to it (do not unroll WireRange)
pub(crate) fn replace_wire_in_wirelist(
    wirelist: &mut WireList,
    old_wire: WireId,
    new_wire: WireId,
) -> Result<()> {
    let mut wires = expand_wirelist(wirelist)?;
    let mut updated = false;
    for wire in wires.iter_mut() {
        if *wire == old_wire {
            *wire = new_wire;
            updated = true;
        }
    }
    if updated {
        *wirelist = wires.iter().map(|w| Wire(*w)).collect()
    }
    Ok(())
}

#[test]
fn test_replace_wire_in_wirelist() {
    let mut wirelist = vec![WireRange(0, 2), Wire(5)];
    replace_wire_in_wirelist(&mut wirelist, 4, 14).unwrap();
    let correct_wirelist = vec![WireRange(0, 2), Wire(5)];
    assert_eq!(wirelist, correct_wirelist);

    replace_wire_in_wirelist(&mut wirelist, 5, 15).unwrap();
    let correct_wirelist = vec![Wire(0), Wire(1), Wire(2), Wire(15)];
    assert_eq!(wirelist, correct_wirelist);

    let mut wirelist = vec![WireRange(0, 2), Wire(5)];
    replace_wire_in_wirelist(&mut wirelist, 1, 14).unwrap();
    let correct_wirelist = vec![Wire(0), Wire(14), Wire(2), Wire(5)];
    assert_eq!(wirelist, correct_wirelist);
}

/// Replace `wire` by `new_wire` if `wire` was equal to `old_wire`
pub(crate) fn replace_wire(wire: &mut WireId, old_wire: WireId, new_wire: WireId) {
    if *wire == old_wire {
        *wire = new_wire;
    }
}

#[test]
fn test_replace_wire() {
    let mut wire = 5;
    replace_wire(&mut wire, 3, 5);
    assert_eq!(wire, 5);
    replace_wire(&mut wire, 5, 8);
    assert_eq!(wire, 8);
}
