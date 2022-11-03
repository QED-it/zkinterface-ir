use crate::Result;
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;

use super::value::{build_values_vector, try_from_values_vector, Value};
use crate::sieve_ir_generated::sieve_ir as generated;
use crate::structs::IR_VERSION;

#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Header {
    pub version: String,
    pub types: Vec<Value>,
}

impl Header {
    pub fn new(moduli: &[Value]) -> Self {
        Self {
            types: moduli.to_owned(),
            version: IR_VERSION.to_string(),
        }
    }
}

impl Default for Header {
    fn default() -> Self {
        Header::new(&[vec![2]])
    }
}

impl<'a> TryFrom<Option<generated::Header<'a>>> for Header {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_header: Option<generated::Header>) -> Result<Header> {
        let g_header = g_header.ok_or("Missing header")?;
        let g_types = g_header.types().ok_or("Missing types")?;
        Ok(Header {
            version: g_header.version().ok_or("Missing version")?.to_string(),
            types: try_from_values_vector(g_types)?,
        })
    }
}

impl Header {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'a>(
        &self,
        builder: &mut FlatBufferBuilder<'a>,
    ) -> WIPOffset<generated::Header<'a>> {
        let version = Some(builder.create_string(&self.version));
        let g_types = build_values_vector(builder, &self.types);

        generated::Header::create(
            builder,
            &generated::HeaderArgs {
                version,
                types: Some(g_types),
            },
        )
    }
}
