use crate::Result;
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;
use std::ops::Add;

use crate::sieve_ir_generated::sieve_ir as generated;
use crate::structs::wire::{build_field_id, WireList, WireListElement};
use crate::{FieldId, Value};

pub type CountList = HashMap<FieldId, u64>;

impl<'a> TryFrom<generated::CountList<'a>> for CountList {
    type Error = Box<dyn Error>;

    fn try_from(list: generated::CountList<'a>) -> Result<Self> {
        let fbs_vector_option = list.elements();
        let mut map: HashMap<FieldId, u64> = HashMap::new();
        if let Some(fbs_vector) = fbs_vector_option {
            for el in fbs_vector {
                let field_id = el.field_id().ok_or("Missing field_id in Count")?.id();
                let count = el.count();
                if count != 0 {
                    let element = map.entry(field_id).or_insert(0);
                    *element += count;
                }
            }
        }
        Ok(map)
    }
}

/// Add a vector of this structure into a Flatbuffers message builder.
pub fn build_count_list<'a>(
    builder: &mut FlatBufferBuilder<'a>,
    elements: &CountList,
) -> WIPOffset<generated::CountList<'a>> {
    let mut g_elements = vec![];
    for (field_id, count) in elements {
        let g_field_id = build_field_id(builder, *field_id);
        g_elements.push(generated::Count::create(
            builder,
            &generated::CountArgs {
                field_id: Some(g_field_id),
                count: *count,
            },
        ));
    }

    let g_vector = builder.create_vector(&g_elements);

    generated::CountList::create(
        builder,
        &generated::CountListArgs {
            elements: Some(g_vector),
        },
    )
}

pub fn count_len(count_list: &CountList) -> u64 {
    count_list.iter().fold(0u64, |acc, v| acc.add(v.1))
}

/// Create a `CountList` from a `WireList` by counting in `wirelist` the number of wires for each field.
pub fn wirelist_to_count_list(wirelist: &WireList) -> CountList {
    let mut map: HashMap<FieldId, u64> = HashMap::new();
    for wire in wirelist {
        match wire {
            WireListElement::Wire(field_id, _) => {
                let count = map.entry(*field_id).or_insert(0);
                *count += 1;
            }
            WireListElement::WireRange(field_id, first, last) => {
                if *first <= *last {
                    let count = map.entry(*field_id).or_insert(0);
                    *count += *last - *first + 1;
                }
            }
        }
    }
    map
}

#[test]
fn test_wirelist_to_count_list() {
    let wirelist: WireList = vec![
        WireListElement::Wire(0, 5),
        WireListElement::WireRange(3, 4, 6),
        WireListElement::WireRange(0, 2, 3),
    ];
    let countlist: CountList = HashMap::from([(0, 3), (3, 3)]);
    assert_eq!(wirelist_to_count_list(&wirelist), countlist);
}

/// Create a CountList from a vector of vector of Values.
/// The vector of vector of values contains the values for each field.
/// More specifically, `values[i]` is a vector containing the values for the field `i`.
/// `vector_of_values_to_count_list` returns a CountList which contains the number of values for each field.
pub fn vector_of_values_to_count_list(values: &[Vec<Value>]) -> CountList {
    let mut map: CountList = HashMap::new();
    for (i, el) in values.iter().enumerate() {
        if !el.is_empty() {
            map.insert(i as u8, el.len() as u64);
        }
    }
    map
}

#[test]
fn test_vector_of_values_to_count_list() {
    let values: Vec<Vec<Value>> = vec![
        vec![],
        vec![vec![4], vec![2, 5], vec![5, 8]],
        vec![],
        vec![vec![1]],
    ];
    let countlist: CountList = HashMap::from([(1, 3), (3, 1)]);
    assert_eq!(vector_of_values_to_count_list(&values), countlist);
}

/// Compare two `CountList` and keep the maximum count for each field.
/// `count_list_1` will be updated to contain the maximum count for each field
pub fn count_list_max(count_list_1: &mut CountList, count_list_2: &CountList) {
    for (field_id, count) in count_list_2 {
        let field_id_count = count_list_1.entry(*field_id).or_insert(0);
        if *field_id_count < *count {
            *field_id_count = *count;
        }
    }
}

#[test]
fn test_count_list_max() {
    let mut count_list_1: CountList = HashMap::from([(0, 5), (1, 2), (2, 1)]);
    let count_list_2: CountList = HashMap::from([(3, 3), (0, 1), (2, 2)]);
    let expected_count_list_max: CountList = HashMap::from([(0, 5), (1, 2), (2, 2), (3, 3)]);
    count_list_max(&mut count_list_1, &count_list_2);
    assert_eq!(count_list_1, expected_count_list_max);
}
