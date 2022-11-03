use crate::Result;
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;
use std::io::Write;

use super::gates::Gate;
use super::header::Header;
use crate::sieve_ir_generated::sieve_ir as generated;
use crate::structs::function::Function;

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Relation {
    pub header: Header,
    pub plugins: Vec<String>,
    pub functions: Vec<Function>,
    pub gates: Vec<Gate>,
}

impl<'a> TryFrom<generated::Relation<'a>> for Relation {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_relation: generated::Relation) -> Result<Relation> {
        let g_gates = g_relation.directives().ok_or("Missing directives")?;
        let functions = if let Some(g_functions) = g_relation.functions() {
            Function::try_from_vector(g_functions)?
        } else {
            vec![]
        };
        let mut plugins = Vec::new();
        if let Some(g_plugins) = g_relation.plugins() {
            g_plugins
                .iter()
                .for_each(|g_plugin| plugins.push(g_plugin.to_string()))
        }

        Ok(Relation {
            header: Header::try_from(g_relation.header())?,
            plugins,
            functions,
            gates: Gate::try_from_vector(g_gates)?,
        })
    }
}

impl<'a> TryFrom<&'a [u8]> for Relation {
    type Error = Box<dyn Error>;

    fn try_from(buffer: &'a [u8]) -> Result<Relation> {
        Relation::try_from(
            generated::get_size_prefixed_root_as_root(buffer)
                .message_as_relation()
                .ok_or("Not a Relation message.")?,
        )
    }
}

impl Relation {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'a>(&self, builder: &mut FlatBufferBuilder<'a>) -> WIPOffset<generated::Root<'a>> {
        let header = Some(self.header.build(builder));
        let directives = Gate::build_vector(builder, &self.gates);
        let functions = Function::build_vector(builder, &self.functions);
        let g_plugins = self
            .plugins
            .iter()
            .map(|plugin| builder.create_string(plugin))
            .collect::<Vec<WIPOffset<&str>>>();
        let g_plugins_vec = builder.create_vector(&g_plugins);

        let relation = generated::Relation::create(
            builder,
            &generated::RelationArgs {
                header,
                plugins: Some(g_plugins_vec),
                functions: Some(functions),
                directives: Some(directives),
            },
        );

        generated::Root::create(
            builder,
            &generated::RootArgs {
                message_type: generated::Message::Relation,
                message: Some(relation.as_union_value()),
            },
        )
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
        generated::finish_size_prefixed_root_buffer(&mut builder, message);
        writer.write_all(builder.finished_data())?;
        Ok(())
    }
}
