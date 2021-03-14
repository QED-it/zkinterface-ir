use std::error::Error;
use std::convert::TryFrom;
use crate::Result;
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use std::io::Write;
use serde::{Deserialize, Serialize};

use crate::sieve_ir_generated::sieve_ir as g;
use super::header::Header;
use super::gates::Gate;

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Relation {
    pub header: Header,
    pub gates: Vec<Gate>,
}

impl<'a> TryFrom<g::Relation<'a>> for Relation {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_relation: g::Relation) -> Result<Relation> {
        let mut relation = Relation {
            header: Header::try_from(g_relation.header())?,
            gates: vec![],
        };

        let g_gates = g_relation.gates().ok_or("Missing gates")?;
        for i in 0..g_gates.len() {
            let gen_gate = g_gates.get(i);
            relation.gates.push(Gate::try_from(gen_gate)?);
        }

        Ok(relation)
    }
}

impl<'a> TryFrom<&'a [u8]> for Relation {
    type Error = Box<dyn Error>;

    fn try_from(buffer: &'a [u8]) -> Result<Relation> {
        Relation::try_from(
            g::get_size_prefixed_root_as_root(&buffer)
                .message_as_relation()
                .ok_or("Not a Relation message.")?)
    }
}

impl Relation {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::Root<'bldr>>
    {
        let header = Some(self.header.build(builder));
        let gates: Vec<_> = self.gates.iter().map(|gate|
            gate.build(builder)
        ).collect();
        let gates = builder.create_vector(&gates);

        let relation = g::Relation::create(builder, &g::RelationArgs {
            header,
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
    /// use zki_sieve::Relation;
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
