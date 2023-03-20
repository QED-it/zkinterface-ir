use crate::Result;
use flatbuffers::{FlatBufferBuilder, Vector, WIPOffset};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::error::Error;

use crate::sieve_ir_generated::sieve_ir as generated;
use crate::TypeId;

#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize, Hash, Ord, PartialOrd)]
pub struct Count {
    pub type_id: TypeId,
    pub count: u64,
}

/// This function imports a FBS binary Count declaration into a Rust equivalent.
impl TryFrom<generated::Count> for Count {
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
    pub fn build(&self) -> generated::Count {
        generated::Count::new(self.type_id, self.count)
    }

    /// Import a vector of binary Count into a Rust vector of Count declarations.
    pub fn try_from_vector(g_counts: &[generated::Count]) -> Result<Vec<Count>> {
        g_counts
            .iter()
            .map(|g_count| Count::try_from(*g_count))
            .collect()
    }

    /// Build a vector a Rust Count into the associated FBS structure.
    pub fn build_vector<'a>(
        builder: &mut FlatBufferBuilder<'a>,
        counts: &[Count],
    ) -> WIPOffset<Vector<'a, generated::Count>> {
        let g_counts = counts.iter().map(|count| count.build()).collect::<Vec<_>>();
        builder.create_vector(&g_counts)
    }
}

pub fn count_list_to_hashmap(count_list: &[Count]) -> BTreeMap<TypeId, u64> {
    let mut map = BTreeMap::new();
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
    let expected_result: BTreeMap<TypeId, u64> = BTreeMap::from([(0, 3), (1, 7)]);
    assert_eq!(result, expected_result);
}
