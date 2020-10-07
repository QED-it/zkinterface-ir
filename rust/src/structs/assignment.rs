use flatbuffers::{FlatBufferBuilder, WIPOffset, ForwardsUOffset, Vector};
use serde::{Deserialize, Serialize};
use crate::sieve_ir_generated::sieve_ir as g;
use super::{WireId, Value};

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Assignment {
    pub id: WireId,
    pub value: Value,
}

impl<'a> From<g::Assignment<'a>> for Assignment {
    /// Convert from Flatbuffers references to owned structure.
    fn from(g_assignment: g::Assignment) -> Assignment {
        Assignment {
            id: g_assignment.id().unwrap().id(),
            value: Vec::from(g_assignment.value().unwrap()),
        }
    }
}

impl Assignment {
    pub fn from_vector<'a>(g_vector: Vector<'a, ForwardsUOffset<g::Assignment<'a>>>
    ) -> Vec<Assignment> {
        let mut assignments = vec![];
        for i in 0..g_vector.len() {
            let g_a = g_vector.get(i);
            assignments.push(Assignment::from(g_a));
        }
        assignments
    }

    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::Assignment<'bldr>>
    {
        let value = builder.create_vector(&self.value[..]);
        g::Assignment::create(builder, &g::AssignmentArgs {
            id: Some(&g::Wire::new(self.id)),
            value: Some(value),
        })
    }

    /// Add this structure into a Flatbuffers message builder.
    pub fn build_vector<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
        assignments: &'args [Self],
    ) -> WIPOffset<Vector<'bldr, ForwardsUOffset<g::Assignment<'bldr>>>>
    {
        let g_assignments: Vec<_> = assignments.iter().map(|g_a|
            g_a.build(builder)
        ).collect();
        builder.create_vector(&g_assignments)
    }
}