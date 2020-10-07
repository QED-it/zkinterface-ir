use flatbuffers::{FlatBufferBuilder, WIPOffset};
use std::io::Write;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;

use crate::Result;
use crate::sieve_ir_generated::sieve_ir as g;
use super::gates::Gate;

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Relation {
    pub gates: Vec<Gate>,
}

impl<'a> From<g::Relation<'a>> for Relation {
    /// Convert from Flatbuffers references to owned structure.
    fn from(gen_relation: g::Relation) -> Relation {
        let mut relation = Relation {
            gates: vec![]
        };

        let gen_gates = gen_relation.gates().unwrap();
        for i in 0..gen_gates.len() {
            let gen_gate = gen_gates.get(i);
            relation.gates.push(Gate::from(gen_gate));
        }

        relation
    }
}

impl<'a> TryFrom<&'a [u8]> for Relation {
    type Error = Box<dyn Error>;

    fn try_from(buffer: &'a [u8]) -> Result<Self> {
        Ok(Self::from(
            g::get_size_prefixed_root_as_root(&buffer)
                .message_as_relation()
                .ok_or("Not a Relation message.")?))
    }
}

impl Relation {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::Root<'bldr>>
    {
        let gates: Vec<_> = self.gates.iter().map(|gate|
            gate.build(builder)
        ).collect();
        let gates = builder.create_vector(&gates);

        let relation = g::Relation::create(builder, &g::RelationArgs {
            num_wires: 0,
            num_short_witness: 0,
            num_common_inputs: 0,
            gates: Some(gates),
        });

        g::Root::create(builder, &g::RootArgs {
            message_type: g::Message::Relation,
            message: Some(relation.as_union_value()),
        })
    }

    /// Writes this Relation as a Flatbuffers message into the provided buffer.
    ///
    /// # Examples
    /// ```
    /// use sieve_ir::Relation;
    /// use std::convert::TryFrom;
    ///
    /// let relation = Relation::default();
    /// let mut buf = Vec::<u8>::new();
    /// relation.write_into(&mut buf).unwrap();
    /// let relation2 = Relation::try_from(&buf[..]).unwrap();
    /// assert_eq!(relation, relation2);
    /// ```
    pub fn write_into(&self, writer: &mut impl Write) -> Result<()> {
        let mut builder = FlatBufferBuilder::new();
        let message = self.build(&mut builder);
        builder.finish_size_prefixed(message, None);
        writer.write_all(builder.finished_data())?;
        Ok(())
    }
}
