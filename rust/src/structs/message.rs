use crate::sieve_ir_generated::sieve_ir as fb;
use crate::{Instance, Relation, Result, Witness};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;
use std::io::Write;

#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum Message {
    Instance(Instance),
    Witness(Witness),
    Relation(Relation),
}

impl<'a> TryFrom<&'a [u8]> for Message {
    type Error = Box<dyn Error>;

    fn try_from(buffer: &'a [u8]) -> Result<Self> {
        let msg = fb::get_size_prefixed_root_as_root(&buffer);

        Ok(match msg.message_type() {
            fb::Message::Instance => {
                let fb_instance = msg.message_as_instance().unwrap();
                Message::Instance(Instance::try_from(fb_instance)?)
            }
            fb::Message::Witness => {
                let fb_witness = msg.message_as_witness().unwrap();
                Message::Witness(Witness::try_from(fb_witness)?)
            }
            fb::Message::Relation => {
                let fb_relation = msg.message_as_relation().unwrap();
                Message::Relation(Relation::try_from(fb_relation)?)
            }
            fb::Message::NONE => return Err("Invalid message type".into()),
        })
    }
}

impl Message {
    /// Writes this Message as a Flatbuffers message into the provided buffer.
    ///
    /// # Examples
    /// ```
    /// use zki_sieve::{Instance, Message};
    /// use std::convert::TryFrom;
    ///
    /// let message = Message::Instance(Instance::default());
    /// let mut buf = Vec::<u8>::new();
    /// message.write_into(&mut buf).unwrap();
    /// let message2 = Message::try_from(&buf[..]).unwrap();
    /// assert_eq!(message, message2);
    /// ```
    pub fn write_into(&self, writer: &mut impl Write) -> Result<()> {
        match self {
            Message::Instance(instance) => instance.write_into(writer),
            Message::Witness(witness) => witness.write_into(writer),
            Message::Relation(relation) => relation.write_into(writer),
        }
    }
}
