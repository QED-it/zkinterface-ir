//! Helpers to write messages.

use flatbuffers::{FlatBufferBuilder, WIPOffset};
use std::io::Write;
use serde::{Deserialize, Serialize};
use crate::sieve_ir_generated::sieve_ir as g;
use crate::Result;
use std::convert::TryFrom;
use std::error::Error;

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

impl<'a> TryFrom<&'a [u8]> for Header {
    type Error = Box<dyn Error>;

    fn try_from(buffer: &'a [u8]) -> Result<Self> {
        Ok(Self::from(
            g::get_size_prefixed_root_as_root(&buffer)
                .message_as_header()
                .ok_or("Not a Header message.")?))
    }
}

impl Header {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::Root<'bldr>>
    {
        let version = Some(builder.create_string(&self.version));
        let profile = Some(builder.create_string(&self.profile));
        let field_characteristic = Some(builder.create_vector(&self.field_characteristic));

        let header = g::Header::create(builder, &g::HeaderArgs {
            version,
            profile,
            field_characteristic,
            field_degree: self.field_degree,
        });

        g::Root::create(builder, &g::RootArgs {
            message_type: g::Message::Header,
            message: Some(header.as_union_value()),
        })
    }

    /// Writes this Header as a Flatbuffers message into the provided buffer.
    ///
    /// # Examples
    /// ```
    /// use sieve_ir::Header;
    /// use std::convert::TryFrom;
    ///
    /// let header = Header::default();
    /// let mut buf = Vec::<u8>::new();
    /// header.write_into(&mut buf).unwrap();
    /// let header2 = Header::try_from(&buf[..]).unwrap();
    /// assert_eq!(header, header2);
    /// ```
    pub fn write_into(&self, writer: &mut impl Write) -> Result<()> {
        let mut builder = FlatBufferBuilder::new();
        let message = self.build(&mut builder);
        builder.finish_size_prefixed(message, None);
        writer.write_all(builder.finished_data())?;
        Ok(())
    }
}
