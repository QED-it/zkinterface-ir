use crate::Result;
use flatbuffers::{FlatBufferBuilder, ForwardsUOffset, Vector, WIPOffset};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;

use crate::sieve_ir_generated::sieve_ir as generated;
use crate::structs::wire::{WireList, WireListElement};
use crate::TypeId;

#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Count {
    pub type_id: TypeId,
    pub count: u64,
}

/// This function imports a FBS binary Count declaration into a Rust equivalent.
impl<'a> TryFrom<generated::Count<'a>> for Count {
    type Error = Box<dyn Error>;

    fn try_from(g_count: generated::Count) -> Result<Count> {
        Ok(Count {
            type_id: g_count.type_id(),
            count: g_count.count(),
        })
    }
}

impl Count {
    /// Default constructor
    pub fn new(type_id: TypeId, count: u64) -> Self {
        Count { type_id, count }
    }

    /// Serialize this structure into a Flatbuffer message
    pub fn build<'a>(
        &self,
        builder: &mut FlatBufferBuilder<'a>,
    ) -> WIPOffset<generated::Count<'a>> {
        generated::Count::create(
            builder,
            &generated::CountArgs {
                type_id: self.type_id,
                count: self.count,
            },
        )
    }

    /// Import a vector of binary Count into a Rust vector of Count declarations.
    pub fn try_from_vector<'a>(
        g_counts: Vector<'a, ForwardsUOffset<generated::Count<'a>>>,
    ) -> Result<Vec<Count>> {
        g_counts.into_iter().map(Count::try_from).collect()
    }

    /// Build a vector a Rust Count into the associated FBS structure.
    pub fn build_vector<'a>(
        builder: &mut FlatBufferBuilder<'a>,
        counts: &[Count],
    ) -> WIPOffset<Vector<'a, ForwardsUOffset<generated::Count<'a>>>> {
        let g_counts = counts
            .iter()
            .map(|count| count.build(builder))
            .collect::<Vec<_>>();
        builder.create_vector(&g_counts)
    }
}

pub fn count_list_to_hashmap(count_list: &[Count]) -> HashMap<TypeId, u64> {
    let mut map = HashMap::new();
    count_list.iter().for_each(|count| {
        let current_count = map.entry(count.type_id).or_insert(0_u64);
        *current_count += count.count
    });
    map
}

#[test]
fn test_count_list_to_hashmap() {
    let countlist = vec![Count::new(1, 5), Count::new(0, 3), Count::new(1, 2)];
    let result = count_list_to_hashmap(&countlist);
    let expected_result: HashMap<TypeId, u64> = HashMap::from([(0, 3), (1, 7)]);
    assert_eq!(result, expected_result);
}

pub fn wirelist_to_count_list(wirelist: &WireList) -> Vec<Count> {
    wirelist
        .iter()
        .map(|wire_list_el| match wire_list_el {
            WireListElement::Wire(type_id, _) => Count {
                type_id: *type_id,
                count: 1,
            },
            WireListElement::WireRange(type_id, first, last) => Count {
                type_id: *type_id,
                count: *last - *first + 1,
            },
        })
        .collect()
}

#[test]
fn test_wirelist_to_count_list() {
    let wirelist = vec![
        WireListElement::WireRange(0, 0, 2),
        WireListElement::Wire(1, 1),
        WireListElement::Wire(0, 4),
    ];
    let result = wirelist_to_count_list(&wirelist);
    let expected_result = vec![Count::new(0, 3), Count::new(1, 1), Count::new(0, 1)];
    assert_eq!(result, expected_result);
}
