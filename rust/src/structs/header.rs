//! Helpers to write messages.

use flatbuffers::{FlatBufferBuilder, WIPOffset};
use serde::{Deserialize, Serialize};
use crate::sieve_ir_generated::sieve_ir as g;

#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Header {
    pub version: String,
    pub profile: String,
    pub field_characteristic: Vec<u8>,
    pub field_degree: u32,
}

impl Default for Header {
    fn default() -> Self {
        Header {
            version: "0".to_string(),
            profile: "arithmetic_circuit".to_string(),
            field_characteristic: vec![],
            field_degree: 1,
        }
    }
}

impl<'a> From<g::Header<'a>> for Header {
    /// Convert from Flatbuffers references to owned structure.
    fn from(g_header: g::Header) -> Header {
        Header {
            version: g_header.version().unwrap().to_string(),
            profile: g_header.profile().unwrap().to_string(),
            field_characteristic: Vec::from(g_header.field_characteristic().unwrap()),
            field_degree: g_header.field_degree(),
        }
    }
}

impl Header {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::Header<'bldr>>
    {
        let version = Some(builder.create_string(&self.version));
        let profile = Some(builder.create_string(&self.profile));
        let field_characteristic = Some(builder.create_vector(&self.field_characteristic));

        g::Header::create(builder, &g::HeaderArgs {
            version,
            profile,
            field_characteristic,
            field_degree: self.field_degree,
        })
    }
}
