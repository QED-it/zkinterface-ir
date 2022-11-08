use crate::sieve_ir_generated::sieve_ir as generated;
use crate::Result;
use crate::{TypeId, WireId};
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;

/// A WireListElement is either a single wire, or a range.
#[derive(Clone, Debug, Eq, PartialEq, Hash, Deserialize, Serialize)]
pub enum WireListElement {
    Wire(TypeId, WireId),
    WireRange(TypeId, WireId, WireId),
}
use WireListElement::*;

/// A WireList is simply a vector of WireListElement
pub type WireList = Vec<WireListElement>;

// =========================================
//              Wire
// =========================================
/// Convert from Flatbuffers references to owned structure.
pub fn from_wire(g_wire: &generated::Wire) -> Result<(TypeId, WireId)> {
    Ok((g_wire.type_id(), g_wire.wire_id()))
}

/// Add this structure into a Flatbuffers message builder.
pub fn build_wire<'a>(
    builder: &mut FlatBufferBuilder<'a>,
    type_id: TypeId,
    wire_id: WireId,
) -> WIPOffset<generated::Wire<'a>> {
    generated::Wire::create(builder, &generated::WireArgs { type_id, wire_id })
}

// =========================================
//              WireRange
// =========================================
/// Convert from Flatbuffers references to owned structure.
pub fn from_range(g_wirerange: &generated::WireRange) -> Result<(TypeId, WireId, WireId)> {
    Ok((
        g_wirerange.type_id(),
        g_wirerange.first_id(),
        g_wirerange.last_id(),
    ))
}

/// Add this structure into a Flatbuffers message builder.
pub fn build_range<'a>(
    builder: &mut FlatBufferBuilder<'a>,
    type_id: TypeId,
    first_id: WireId,
    last_id: WireId,
) -> WIPOffset<generated::WireRange<'a>> {
    generated::WireRange::create(
        builder,
        &generated::WireRangeArgs {
            type_id,
            first_id,
            last_id,
        },
    )
}

// =========================================
//       WireListElement + WireList
// =========================================

impl<'a> TryFrom<generated::WireListElement<'a>> for WireListElement {
    type Error = Box<dyn Error>;

    fn try_from(element: generated::WireListElement<'a>) -> Result<Self> {
        Ok(match element.element_type() {
            generated::WireListElementU::NONE => {
                return Err("Unknown type in WireListElement".into())
            }
            generated::WireListElementU::Wire => {
                let wire = element.element_as_wire().unwrap();
                Wire(wire.type_id(), wire.wire_id())
            }
            generated::WireListElementU::WireRange => {
                let range = element.element_as_wire_range().unwrap();
                WireRange(range.type_id(), range.first_id(), range.last_id())
            }
        })
    }
}

impl WireListElement {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'a>(
        &self,
        builder: &mut FlatBufferBuilder<'a>,
    ) -> WIPOffset<generated::WireListElement<'a>> {
        match self {
            Wire(type_id, wire_id) => {
                let wire = build_wire(builder, *type_id, *wire_id);
                generated::WireListElement::create(
                    builder,
                    &generated::WireListElementArgs {
                        element_type: generated::WireListElementU::Wire,
                        element: Some(wire.as_union_value()),
                    },
                )
            }
            WireRange(type_id, first, last) => {
                let range = build_range(builder, *type_id, *first, *last);
                generated::WireListElement::create(
                    builder,
                    &generated::WireListElementArgs {
                        element_type: generated::WireListElementU::WireRange,
                        element: Some(range.as_union_value()),
                    },
                )
            }
        }
    }
}

impl<'a> TryFrom<generated::WireList<'a>> for WireList {
    type Error = Box<dyn Error>;

    fn try_from(list: generated::WireList<'a>) -> Result<Self> {
        let fbs_vector = list.elements().ok_or("Missing wire list")?;
        let mut elements = vec![];
        for i in 0..fbs_vector.len() {
            let a = fbs_vector.get(i);
            elements.push(WireListElement::try_from(a)?);
        }
        Ok(elements)
    }
}

/// Add a vector of this structure into a Flatbuffers message builder.
pub fn build_wire_list<'a>(
    builder: &mut FlatBufferBuilder<'a>,
    elements: &[WireListElement],
) -> WIPOffset<generated::WireList<'a>> {
    let g_elements: Vec<_> = elements
        .iter()
        .map(|element| element.build(builder))
        .collect();
    let g_vector = builder.create_vector(&g_elements);

    generated::WireList::create(
        builder,
        &generated::WireListArgs {
            elements: Some(g_vector),
        },
    )
}

/// Expand a WireListElement into a vector of Result<WireId>.
pub fn expand_wirelistelement(wire: &WireListElement) -> Vec<Result<(TypeId, WireId)>> {
    match wire {
        WireListElement::Wire(type_id, wire_id) => vec![Ok((*type_id, *wire_id))],
        WireListElement::WireRange(type_id, first, last) => {
            if last <= first {
                vec![Err(format!(
                    "In WireRange, last WireId ({}) must be strictly greater than first WireId ({}).",
                    last, first
                )
                    .into())]
            } else {
                (*first..=*last).map(|item| Ok((*type_id, item))).collect()
            }
        }
    }
}

/// Expand a WireList into a vector of individual WireId.
pub fn expand_wirelist(wirelist: &WireList) -> Result<Vec<(TypeId, WireId)>> {
    let res = wirelist
        .iter()
        .flat_map(expand_wirelistelement)
        .collect::<Result<Vec<(TypeId, WireId)>>>()?;
    Ok(res)
}

#[test]
fn test_expand_wirelist() {
    let wirelist = vec![WireRange(1, 0, 2), Wire(0, 5)];
    let new_wirelist = expand_wirelist(&wirelist).unwrap();
    let correct_wirelist: Vec<(TypeId, WireId)> = vec![(1, 0), (1, 1), (1, 2), (0, 5)];
    assert_eq!(new_wirelist, correct_wirelist);

    let wirelist = vec![WireRange(3, 0, 1), WireRange(1, 2, 2), Wire(0, 5)];
    let new_wirelist = expand_wirelist(&wirelist);
    assert!(new_wirelist.is_err());

    let wirelist = vec![WireRange(0, 0, 1), WireRange(1, 4, 2), Wire(2, 5)];
    let new_wirelist = expand_wirelist(&wirelist);
    assert!(new_wirelist.is_err());
}

pub fn wirelist_len(wirelist: &WireList) -> u64 {
    wirelist
        .iter()
        .map(|wire| match wire {
            WireListElement::Wire(_, _) => 1,
            WireListElement::WireRange(_, first, last) => *last - *first + 1,
        })
        .sum()
}

/// Go through `wirelist` and replace `old_wire` by `new_wire`
/// Do not modify wirelist if old_wire does not belong to it (do not unroll WireRange)
pub(crate) fn replace_wire_in_wirelist(
    wirelist: &mut WireList,
    type_id: TypeId,
    old_wire: WireId,
    new_wire: WireId,
) -> Result<()> {
    let mut wires = expand_wirelist(wirelist)?;
    let mut updated = false;
    for wire in wires.iter_mut() {
        if (wire.0 == type_id) && (wire.1 == old_wire) {
            wire.1 = new_wire;
            updated = true;
        }
    }
    if updated {
        *wirelist = wires.iter().map(|w| Wire(w.0, w.1)).collect()
    }
    Ok(())
}

#[test]
fn test_replace_wire_in_wirelist() {
    let mut wirelist = vec![WireRange(0, 0, 2), Wire(1, 5)];
    replace_wire_in_wirelist(&mut wirelist, 0, 4, 14).unwrap();
    let correct_wirelist = vec![WireRange(0, 0, 2), Wire(1, 5)];
    assert_eq!(wirelist, correct_wirelist);

    replace_wire_in_wirelist(&mut wirelist, 1, 5, 15).unwrap();
    let correct_wirelist = vec![Wire(0, 0), Wire(0, 1), Wire(0, 2), Wire(1, 15)];
    assert_eq!(wirelist, correct_wirelist);

    let mut wirelist = vec![WireRange(0, 0, 2), Wire(1, 5)];
    replace_wire_in_wirelist(&mut wirelist, 0, 1, 14).unwrap();
    let correct_wirelist = vec![Wire(0, 0), Wire(0, 14), Wire(0, 2), Wire(1, 5)];
    assert_eq!(wirelist, correct_wirelist);

    let mut wirelist = vec![WireRange(0, 0, 2), Wire(1, 1)];
    replace_wire_in_wirelist(&mut wirelist, 0, 1, 14).unwrap();
    let correct_wirelist = vec![Wire(0, 0), Wire(0, 14), Wire(0, 2), Wire(1, 1)];
    assert_eq!(wirelist, correct_wirelist);
}

/// Replace `wire` by `new_wire` if `wire` was equal to `old_wire` and `type_id` was equal to `old_type_id`
pub(crate) fn replace_wire_id(
    type_id: &TypeId,
    old_type_id: &TypeId,
    wire: &mut WireId,
    old_wire: WireId,
    new_wire: WireId,
) {
    if (*wire == old_wire) && (*type_id == *old_type_id) {
        *wire = new_wire;
    }
}

#[test]
fn test_replace_wire_id() {
    let mut wire = 5;
    replace_wire_id(&0, &0, &mut wire, 3, 5);
    assert_eq!(wire, 5);

    let mut wire = 5;
    replace_wire_id(&0, &0, &mut wire, 5, 8);
    assert_eq!(wire, 8);

    let mut wire = 8;
    replace_wire_id(&0, &1, &mut wire, 8, 10);
    assert_eq!(wire, 8);
}

pub fn wire_ids_to_wirelist(type_id: &TypeId, wire_ids: &[WireId]) -> WireList {
    wire_ids
        .iter()
        .map(|id| WireListElement::Wire(*type_id, *id))
        .collect()
}

pub fn is_one_type_wirelist(wirelist: &WireList) -> Result<TypeId> {
    if wirelist.is_empty() {
        return Err("Empty wirelist".into());
    }
    let common_type_id = match wirelist[0] {
        WireListElement::Wire(type_id, _) => type_id,
        WireListElement::WireRange(type_id, _, _) => type_id,
    };

    for element in wirelist {
        match element {
            WireListElement::Wire(type_id, _) => {
                if *type_id != common_type_id {
                    return Err("Several types".into());
                }
            }
            WireListElement::WireRange(type_id, _, _) => {
                if *type_id != common_type_id {
                    return Err("Several types".into());
                }
            }
        }
    }
    Ok(common_type_id)
}

pub fn wirelist_to_hashmap(wirelist: &WireList) -> HashMap<TypeId, u64> {
    let mut map = HashMap::new();
    wirelist.iter().for_each(|wire| match wire {
        WireListElement::Wire(type_id, _) => {
            let count = map.entry(*type_id).or_insert(0);
            *count += 1;
        }
        WireListElement::WireRange(type_id, first, last) => {
            let count = map.entry(*type_id).or_insert(0);
            *count += *last - *first + 1;
        }
    });
    map
}

#[test]
fn test_wirelist_to_hashmap() {
    let wirelist = vec![WireRange(0, 0, 2), Wire(1, 1), Wire(0, 4)];
    let result = wirelist_to_hashmap(&wirelist);
    let expected_map: HashMap<TypeId, u64> = HashMap::from([(0, 4), (1, 1)]);
    assert_eq!(result, expected_map);
}
