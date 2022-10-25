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
pub struct Witness {
    pub header: Header,
    pub short_witness: Vec<Inputs>,
}

impl<'a> TryFrom<generated::Witness<'a>> for Witness {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_witness: generated::Witness) -> Result<Witness> {
        let fbs_vector = g_witness.short_witness().ok_or("Missing short_witness")?;
        let mut short_witness: Vec<Inputs> = vec![];
        for g_inputs in fbs_vector {
            short_witness.push(Inputs::try_from(g_inputs)?);
        }
        Ok(Witness {
            header: Header::try_from(g_witness.header())?,
            short_witness,
        })
    }
}

impl<'a> TryFrom<&'a [u8]> for Witness {
    type Error = Box<dyn Error>;

    fn try_from(buffer: &'a [u8]) -> Result<Witness> {
        Witness::try_from(
            generated::get_size_prefixed_root_as_root(buffer)
                .message_as_witness()
                .ok_or("Not a Witness message.")?,
        )
    }
}

impl Witness {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr>(
        &self,
        builder: &mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<generated::Root<'bldr>> {
        let header = Some(self.header.build(builder));
        let g_inputs: Vec<_> = self
            .short_witness
            .iter()
            .map(|inputs| inputs.build(builder))
            .collect();
        let g_vector = builder.create_vector(&g_inputs);
        let witness = generated::Witness::create(
            builder,
            &generated::WitnessArgs {
                header,
                short_witness: Some(g_vector),
            },
        );

        generated::Root::create(
            builder,
            &generated::RootArgs {
                message_type: generated::Message::Witness,
                message: Some(witness.as_union_value()),
            },
        )
    }

    /// Writes this Witness as a Flatbuffers message into the provided buffer.
    ///
    /// # Examples
    /// ```
    /// use zki_sieve::Witness;
    /// use std::convert::TryFrom;
    ///
    /// let witness = Witness::default();
    /// let mut buf = Vec::<u8>::new();
    /// witness.write_into(&mut buf).unwrap();
    /// let witness2 = Witness::try_from(&buf[..]).unwrap();
    /// assert_eq!(witness, witness2);
    /// ```
    pub fn write_into(&self, writer: &mut impl Write) -> Result<()> {
        let mut builder = FlatBufferBuilder::new();
        let message = self.build(&mut builder);
        generated::finish_size_prefixed_root_buffer(&mut builder, message);
        writer.write_all(builder.finished_data())?;
        Ok(())
    }

    pub fn get_witness_len(&self) -> usize {
        self.short_witness
            .iter()
            .map(|inputs| inputs.inputs.len())
            .sum()
    }
}
