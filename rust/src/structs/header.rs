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
    pub fields: Vec<Value>,
}

impl Header {
    pub fn new(field_characteristics: &[Value]) -> Self {
        Self {
            fields: field_characteristics.to_owned(),
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
        let g_fields = g_header.fields().ok_or("Missing fields")?;
        Ok(Header {
            version: g_header.version().ok_or("Missing version")?.to_string(),
            fields: try_from_values_vector(g_fields)?,
        })
    }
}

impl Header {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr>(
        &self,
        builder: &mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<generated::Header<'bldr>> {
        let version = Some(builder.create_string(&self.version));
        let g_fields = build_values_vector(builder, &self.fields);

        generated::Header::create(
            builder,
            &generated::HeaderArgs {
                version,
                fields: Some(g_fields),
            },
        )
    }
}
