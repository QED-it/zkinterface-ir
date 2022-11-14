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
pub struct PublicInputs {
    pub version: String,
    pub type_: Value,
    pub inputs: Vec<Value>,
}

impl<'a> TryFrom<generated::PublicInputs<'a>> for PublicInputs {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_public_inputs: generated::PublicInputs) -> Result<PublicInputs> {
        Ok(PublicInputs {
            version: g_public_inputs
                .version()
                .ok_or("Missing version")?
                .to_string(),
            type_: try_from_value(g_public_inputs.type_().ok_or("Missing type")?)?,
            inputs: try_from_values_vector(
                g_public_inputs.inputs().ok_or("Missing public inputs")?,
            )?,
        })
    }
}

impl<'a> TryFrom<&'a [u8]> for PublicInputs {
    type Error = Box<dyn Error>;

    fn try_from(buffer: &'a [u8]) -> Result<PublicInputs> {
        PublicInputs::try_from(
            generated::get_size_prefixed_root_as_root(buffer)
                .message_as_public_inputs()
                .ok_or("Not a PublicInputs message.")?,
        )
    }
}

impl PublicInputs {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'a>(&self, builder: &mut FlatBufferBuilder<'a>) -> WIPOffset<generated::Root<'a>> {
        let g_version = builder.create_string(&self.version);
        let g_type = build_value(builder, &self.type_);
        let g_inputs = build_values_vector(builder, &self.inputs);

        let public_inputs = generated::PublicInputs::create(
            builder,
            &generated::PublicInputsArgs {
                version: Some(g_version),
                type_: Some(g_type),
                inputs: Some(g_inputs),
            },
        );

        generated::Root::create(
            builder,
            &generated::RootArgs {
                message_type: generated::Message::PublicInputs,
                message: Some(public_inputs.as_union_value()),
            },
        )
    }

    /// Writes this PublicInputs as a Flatbuffers message into the provided buffer.
    ///
    /// # Examples
    /// ```
    /// use zki_sieve::PublicInputs;
    /// use std::convert::TryFrom;
    ///
    /// let public_inputs = PublicInputs::default();
    /// let mut buf = Vec::<u8>::new();
    /// public_inputs.write_into(&mut buf).unwrap();
    /// let public_inputs2 = PublicInputs::try_from(&buf[..]).unwrap();
    /// assert_eq!(public_inputs, public_inputs2);
    /// ```
    pub fn write_into(&self, writer: &mut impl Write) -> Result<()> {
        let mut builder = FlatBufferBuilder::new();
        let message = self.build(&mut builder);
        generated::finish_size_prefixed_root_buffer(&mut builder, message);
        writer.write_all(builder.finished_data())?;
        Ok(())
    }
}
