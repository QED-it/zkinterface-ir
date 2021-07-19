use crate::Result;
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;
use std::io::Write;

use super::header::Header;
use crate::sieve_ir_generated::sieve_ir as g;
use crate::structs::value::{build_values_vector, try_from_values_vector, Value};

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Instance {
    pub header: Header,
    pub common_inputs: Vec<Value>,
}

impl<'a> TryFrom<g::Instance<'a>> for Instance {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_instance: g::Instance) -> Result<Instance> {
        Ok(Instance {
            header: Header::try_from(g_instance.header())?,
            common_inputs: try_from_values_vector(
                g_instance.common_inputs().ok_or("Missing common_input")?,
            )?,
        })
    }
}

impl<'a> TryFrom<&'a [u8]> for Instance {
    type Error = Box<dyn Error>;

    fn try_from(buffer: &'a [u8]) -> Result<Instance> {
        Instance::try_from(
            g::get_size_prefixed_root_as_root(&buffer)
                .message_as_instance()
                .ok_or("Not a Instance message.")?,
        )
    }
}

impl Instance {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::Root<'bldr>> {
        let header = Some(self.header.build(builder));
        let common_inputs = Some(build_values_vector(builder, &self.common_inputs));

        let instance = g::Instance::create(
            builder,
            &g::InstanceArgs {
                header,
                common_inputs,
            },
        );

        g::Root::create(
            builder,
            &g::RootArgs {
                message_type: g::Message::Instance,
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
        g::finish_size_prefixed_root_buffer(&mut builder, message);
        writer.write_all(builder.finished_data())?;
        Ok(())
    }
}
