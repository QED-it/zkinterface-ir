use crate::Result;
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;
use std::io::Write;

use crate::sieve_ir_generated::sieve_ir as generated;
use crate::structs::value::{
    build_value, build_values_vector, try_from_value, try_from_values_vector, Value,
};

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct PrivateInputs {
    pub version: String,
    pub type_: Value,
    pub inputs: Vec<Value>,
}

impl<'a> TryFrom<generated::PrivateInputs<'a>> for PrivateInputs {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_private_inputs: generated::PrivateInputs) -> Result<PrivateInputs> {
        Ok(PrivateInputs {
            version: g_private_inputs
                .version()
                .ok_or("Missing version")?
                .to_string(),
            type_: try_from_value(g_private_inputs.type_().ok_or("Missing type")?)?,
            inputs: try_from_values_vector(
                g_private_inputs.inputs().ok_or("Missing private inputs")?,
            )?,
        })
    }
}

impl<'a> TryFrom<&'a [u8]> for PrivateInputs {
    type Error = Box<dyn Error>;

    fn try_from(buffer: &'a [u8]) -> Result<PrivateInputs> {
        PrivateInputs::try_from(
            generated::get_size_prefixed_root_as_root(buffer)
                .message_as_private_inputs()
                .ok_or("Not a PrivateInputs message.")?,
        )
    }
}

impl PrivateInputs {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'a>(&self, builder: &mut FlatBufferBuilder<'a>) -> WIPOffset<generated::Root<'a>> {
        let g_version = builder.create_string(&self.version);
        let g_type = build_value(builder, &self.type_);
        let g_inputs = build_values_vector(builder, &self.inputs);

        let private_inputs = generated::PrivateInputs::create(
            builder,
            &generated::PrivateInputsArgs {
                version: Some(g_version),
                type_: Some(g_type),
                inputs: Some(g_inputs),
            },
        );

        generated::Root::create(
            builder,
            &generated::RootArgs {
                message_type: generated::Message::PrivateInputs,
                message: Some(private_inputs.as_union_value()),
            },
        )
    }

    /// Writes this PrivateInputs as a Flatbuffers message into the provided buffer.
    ///
    /// # Examples
    /// ```
    /// use zki_sieve::PrivateInputs;
    /// use std::convert::TryFrom;
    ///
    /// let private_inputs = PrivateInputs::default();
    /// let mut buf = Vec::<u8>::new();
    /// private_inputs.write_into(&mut buf).unwrap();
    /// let private_inputs2 = PrivateInputs::try_from(&buf[..]).unwrap();
    /// assert_eq!(private_inputs, private_inputs2);
    /// ```
    pub fn write_into(&self, writer: &mut impl Write) -> Result<()> {
        let mut builder = FlatBufferBuilder::new();
        let message = self.build(&mut builder);
        generated::finish_size_prefixed_root_buffer(&mut builder, message);
        writer.write_all(builder.finished_data())?;
        Ok(())
    }
}
