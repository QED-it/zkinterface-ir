use crate::Result;
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;
use std::io::Write;

use super::header::Header;
use crate::sieve_ir_generated::sieve_ir as generated;
use crate::structs::inputs::Inputs;

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct PublicInputs {
    pub header: Header,
    pub inputs: Vec<Inputs>,
}

impl<'a> TryFrom<generated::PublicInputs<'a>> for PublicInputs {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_public_inputs: generated::PublicInputs) -> Result<PublicInputs> {
        let fbs_vector = g_public_inputs.inputs().ok_or("Missing public_inputs")?;
        let mut public_inputs: Vec<Inputs> = vec![];
        for g_inputs in fbs_vector {
            public_inputs.push(Inputs::try_from(g_inputs)?);
        }
        Ok(PublicInputs {
            header: Header::try_from(g_public_inputs.header())?,
            inputs: public_inputs,
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
    pub fn build<'bldr>(
        &self,
        builder: &mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<generated::Root<'bldr>> {
        let header = Some(self.header.build(builder));
        let g_inputs: Vec<_> = self
            .inputs
            .iter()
            .map(|inputs| inputs.build(builder))
            .collect();
        let g_vector = builder.create_vector(&g_inputs);

        let public_inputs = generated::PublicInputs::create(
            builder,
            &generated::PublicInputsArgs {
                header,
                inputs: Some(g_vector),
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

    pub fn get_public_inputs_len(&self) -> usize {
        self.inputs.iter().map(|inputs| inputs.values.len()).sum()
    }
}
