use crate::Result;
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;
use std::io::Write;

use crate::sieve_ir_generated::sieve_ir as generated;
use crate::structs::conversion::Conversion;
use crate::structs::directives::Directive;
use crate::structs::types::Type;

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Relation {
    pub version: String,
    pub plugins: Vec<String>,
    pub types: Vec<Type>,
    pub conversions: Vec<Conversion>,
    pub directives: Vec<Directive>,
}

impl<'a> TryFrom<generated::Relation<'a>> for Relation {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_relation: generated::Relation) -> Result<Relation> {
        let mut plugins = Vec::new();
        if let Some(g_plugins) = g_relation.plugins() {
            g_plugins
                .iter()
                .for_each(|g_plugin| plugins.push(g_plugin.to_string()))
        }

        Ok(Relation {
            version: g_relation.version().ok_or("Missing version")?.to_string(),
            plugins,
            types: Type::try_from_vector(g_relation.types().ok_or("Missing types")?)?,
            conversions: Conversion::try_from_vector(
                g_relation.conversions().ok_or("Missing conversions")?,
            )?,
            directives: Directive::try_from_vector(
                g_relation.directives().ok_or("Missing directives")?,
            )?,
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
        let g_version = builder.create_string(&self.version);
        let g_types = Type::build_vector(builder, &self.types);
        let g_directives = Directive::build_vector(builder, &self.directives);
        let g_plugins = self
            .plugins
            .iter()
            .map(|plugin| builder.create_string(plugin))
            .collect::<Vec<WIPOffset<&str>>>();
        let g_plugins_vec = builder.create_vector(&g_plugins);
        let g_conversions = Conversion::build_vector(builder, &self.conversions);

        let relation = generated::Relation::create(
            builder,
            &generated::RelationArgs {
                version: Some(g_version),
                plugins: Some(g_plugins_vec),
                types: Some(g_types),
                conversions: Some(g_conversions),
                directives: Some(g_directives),
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
