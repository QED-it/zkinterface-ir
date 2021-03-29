use crate::Result;
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;

use crate::sieve_ir_generated::sieve_ir as g;

#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Header {
    pub version: String,
    pub profile: String,
    pub field_characteristic: Vec<u8>,
    pub field_degree: u32,
}

impl Header {
    pub fn new(field_characteristic: Vec<u8>) -> Self {
        Self {
            field_characteristic,
            ..Self::default()
        }
    }
}

impl Default for Header {
    fn default() -> Self {
        Header {
            version: "0.1.0".to_string(),
            profile: "circ_arithmetic_simple".to_string(),
            field_characteristic: vec![],
            field_degree: 1,
        }
    }
}

impl<'a> TryFrom<Option<g::Header<'a>>> for Header {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_header: Option<g::Header>) -> Result<Header> {
        let g_header = g_header.ok_or("Missing header")?;
        Ok(Header {
            version: g_header.version().ok_or("Missing version")?.to_string(),
            profile: g_header.profile().ok_or("Missing profile")?.to_string(),
            field_characteristic: Vec::from(
                g_header
                    .field_characteristic()
                    .ok_or("Missing field characteristic")?,
            ),
            field_degree: g_header.field_degree(),
        })
    }
}

impl Header {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::Header<'bldr>> {
        let version = Some(builder.create_string(&self.version));
        let profile = Some(builder.create_string(&self.profile));
        let field_characteristic = Some(builder.create_vector(&self.field_characteristic));

        g::Header::create(
            builder,
            &g::HeaderArgs {
                version,
                profile,
                field_characteristic,
                field_degree: self.field_degree,
            },
        )
    }
}
