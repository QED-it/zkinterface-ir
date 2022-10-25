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
pub struct Instance {
    pub header: Header,
    pub common_inputs: Vec<Inputs>,
}

impl<'a> TryFrom<generated::Instance<'a>> for Instance {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_instance: generated::Instance) -> Result<Instance> {
        let fbs_vector = g_instance.common_inputs().ok_or("Missing common_inputs")?;
        let mut common_inputs: Vec<Inputs> = vec![];
        for g_inputs in fbs_vector {
            common_inputs.push(Inputs::try_from(g_inputs)?);
        }
        Ok(Instance {
            header: Header::try_from(g_instance.header())?,
            common_inputs,
        })
    }
}

impl<'a> TryFrom<&'a [u8]> for Instance {
    type Error = Box<dyn Error>;

    fn try_from(buffer: &'a [u8]) -> Result<Instance> {
        Instance::try_from(
            generated::get_size_prefixed_root_as_root(buffer)
                .message_as_instance()
                .ok_or("Not a Instance message.")?,
        )
    }
}

impl Instance {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr>(
        &self,
        builder: &mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<generated::Root<'bldr>> {
        let header = Some(self.header.build(builder));
        let g_inputs: Vec<_> = self
            .common_inputs
            .iter()
            .map(|inputs| inputs.build(builder))
            .collect();
        let g_vector = builder.create_vector(&g_inputs);

        let instance = generated::Instance::create(
            builder,
            &generated::InstanceArgs {
                header,
                common_inputs: Some(g_vector),
            },
        );

        generated::Root::create(
            builder,
            &generated::RootArgs {
                message_type: generated::Message::Instance,
                message: Some(instance.as_union_value()),
            },
        )
    }

    /// Writes this Instance as a Flatbuffers message into the provided buffer.
    ///
    /// # Examples
    /// ```
    /// use zki_sieve::Instance;
    /// use std::convert::TryFrom;
    ///
    /// let instance = Instance::default();
    /// let mut buf = Vec::<u8>::new();
    /// instance.write_into(&mut buf).unwrap();
    /// let instance2 = Instance::try_from(&buf[..]).unwrap();
    /// assert_eq!(instance, instance2);
    /// ```
    pub fn write_into(&self, writer: &mut impl Write) -> Result<()> {
        let mut builder = FlatBufferBuilder::new();
        let message = self.build(&mut builder);
        generated::finish_size_prefixed_root_buffer(&mut builder, message);
        writer.write_all(builder.finished_data())?;
        Ok(())
    }

    pub fn get_instance_len(&self) -> usize {
        self.common_inputs
            .iter()
            .map(|inputs| inputs.inputs.len())
            .sum()
    }
}
