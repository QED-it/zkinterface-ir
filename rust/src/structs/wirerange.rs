use crate::sieve_ir_generated::sieve_ir as generated;
use crate::Result;
use crate::{Count, TypeId, WireId};
use flatbuffers::{FlatBufferBuilder, Vector, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Deserialize, Serialize)]
pub struct WireRange {
    pub first_id: WireId,
    pub last_id: WireId,
}

/// This function imports a FBS binary WireRange declaration into a Rust equivalent.
impl TryFrom<generated::WireRange> for WireRange {
    type Error = Box<dyn Error>;

    fn try_from(g_wire_range: generated::WireRange) -> Result<WireRange> {
        Ok(WireRange {
            first_id: g_wire_range.first_id(),
            last_id: g_wire_range.last_id(),
        })
    }
}

impl WireRange {
    /// Default constructor
    pub fn new(first_id: WireId, last_id: WireId) -> Self {
        WireRange { first_id, last_id }
    }

    /// Serialize this structure into a Flatbuffer message
    pub fn build(&self) -> generated::WireRange {
        generated::WireRange::new(self.first_id, self.last_id)
    }

    /// Import a vector of binary WireRange into a Rust vector of WireRange declarations.
    pub fn try_from_vector(g_wire_ranges: &[generated::WireRange]) -> Result<Vec<WireRange>> {
        g_wire_ranges
            .iter()
            .map(|g_wire_range| WireRange::try_from(*g_wire_range))
            .collect()
    }

    /// Build a vector a Rust WireRange into the associated FBS structure.
    pub fn build_vector<'a>(
        builder: &mut FlatBufferBuilder<'a>,
        wire_ranges: &[WireRange],
    ) -> WIPOffset<Vector<'a, generated::WireRange>> {
        let g_wire_ranges = wire_ranges
            .iter()
            .map(|wire_range| wire_range.build())
            .collect::<Vec<_>>();
        builder.create_vector(&g_wire_ranges)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct WireRangeWithType {
    pub type_id: TypeId,
    pub first_id: WireId,
    pub last_id: WireId,
}

impl WireRangeWithType {
    pub fn new(type_id: TypeId, first_id: WireId, last_id: WireId) -> Self {
        WireRangeWithType {
            type_id,
            first_id,
            last_id,
        }
    }
}

pub fn add_types_to_wire_ranges(
    wire_ranges: &[WireRange],
    counts: &[Count],
) -> Result<Vec<WireRangeWithType>> {
    if wire_ranges.len() != counts.len() {
        return Err("When calling add_types_to_wire_ranges, wire_ranges and counts must have the same length".into());
    }
    wire_ranges
        .iter()
        .zip(counts.iter())
        .map(|(wire_range, count)| {
            if (wire_range.last_id - wire_range.first_id + 1) != count.count {
                Err("When calling add_types_to_wire_ranges, wire_ranges and counts are not compatible".into())
            } else {
                Ok(WireRangeWithType{type_id: count.type_id, first_id: wire_range.first_id, last_id: wire_range.last_id})
            }
        })
        .collect()
}

#[test]
fn test_add_types_to_wire_ranges() {
    let wire_ranges = [WireRange::new(1, 3), WireRange::new(10, 15)];
    let counts = [Count::new(0, 3), Count::new(1, 6)];
    let result = add_types_to_wire_ranges(&wire_ranges, &counts).unwrap();
    let expected_result = vec![
        WireRangeWithType::new(0, 1, 3),
        WireRangeWithType::new(1, 10, 15),
    ];
    assert_eq!(result, expected_result);

    let wire_ranges = [WireRange::new(1, 3), WireRange::new(10, 15)];
    let counts = [Count::new(0, 3)];
    let result = add_types_to_wire_ranges(&wire_ranges, &counts);
    assert!(result.is_err());

    let wire_ranges = [WireRange::new(1, 3), WireRange::new(10, 15)];
    let counts = [Count::new(0, 3), Count::new(1, 5)];
    let result = add_types_to_wire_ranges(&wire_ranges, &counts);
    assert!(result.is_err());

    let wire_ranges = [];
    let counts = [];
    let result = add_types_to_wire_ranges(&wire_ranges, &counts).unwrap();
    let expected_result = vec![];
    assert_eq!(result, expected_result);
}

pub fn check_wire_ranges_with_counts(wire_ranges: &[WireRange], counts: &[Count]) -> bool {
    if wire_ranges.len() != counts.len() {
        false
    } else if wire_ranges.is_empty() {
        true
    } else {
        wire_ranges
            .iter()
            .zip(counts.iter())
            .all(|(wire_range, count)| {
                (wire_range.last_id - wire_range.first_id + 1) == count.count
            })
    }
}

#[test]
fn test_check_wire_ranges_with_counts() {
    let wire_ranges = [WireRange::new(1, 3), WireRange::new(10, 15)];
    let counts = [Count::new(0, 3), Count::new(1, 6)];
    assert!(check_wire_ranges_with_counts(&wire_ranges, &counts));

    let wire_ranges = [WireRange::new(1, 3), WireRange::new(10, 15)];
    let counts = [Count::new(0, 3), Count::new(1, 5)];
    assert!(!check_wire_ranges_with_counts(&wire_ranges, &counts));

    let wire_ranges = [];
    let counts = [];
    assert!(check_wire_ranges_with_counts(&wire_ranges, &counts));

    let wire_ranges = [WireRange::new(1, 3), WireRange::new(10, 15)];
    let counts = [Count::new(0, 3)];
    assert!(!check_wire_ranges_with_counts(&wire_ranges, &counts));
}
