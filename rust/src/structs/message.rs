use crate::sieve_ir_generated::sieve_ir as generated;
use crate::{PrivateInputs, PublicInputs, Relation, Result};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;
use std::io::Write;

#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum Message {
    PublicInputs(PublicInputs),
    PrivateInputs(PrivateInputs),
    Relation(Relation),
}

impl<'a> TryFrom<&'a [u8]> for Message {
    type Error = Box<dyn Error>;

    fn try_from(buffer: &'a [u8]) -> Result<Self> {
        let msg = generated::get_size_prefixed_root_as_root(buffer);

        Ok(match msg.message_type() {
            generated::Message::PublicInputs => {
                let fb_public_inputs = msg.message_as_public_inputs().unwrap();
                Message::PublicInputs(PublicInputs::try_from(fb_public_inputs)?)
            }
            generated::Message::PrivateInputs => {
                let fb_private_inputs = msg.message_as_private_inputs().unwrap();
                Message::PrivateInputs(PrivateInputs::try_from(fb_private_inputs)?)
            }
            generated::Message::Relation => {
                let fb_relation = msg.message_as_relation().unwrap();
                Message::Relation(Relation::try_from(fb_relation)?)
            }
            generated::Message::NONE => return Err("Invalid message type".into()),
        })
    }
}

impl Message {
    /// Writes this Message as a Flatbuffers message into the provided buffer.
    ///
    /// # Examples
    /// ```
    /// use zki_sieve::{PublicInputs, Message};
    /// use zki_sieve::structs::types::Type;
    /// use zki_sieve::structs::IR_VERSION;
    /// use std::convert::TryFrom;
    ///
    /// let message = Message::PublicInputs(PublicInputs {
    ///         version: IR_VERSION.to_string(),
    ///         type_value: Type::Field(vec![101]),
    ///         inputs: vec![
    ///             vec![3],
    ///             vec![4],
    ///         ],
    ///     });
    /// let mut buf = Vec::<u8>::new();
    /// message.write_into(&mut buf).unwrap();
    /// let message2 = Message::try_from(&buf[..]).unwrap();
    /// assert_eq!(message, message2);
    /// ```
    pub fn write_into(&self, writer: &mut impl Write) -> Result<()> {
        match self {
            Message::PublicInputs(public_inputs) => public_inputs.write_into(writer),
            Message::PrivateInputs(private_inputs) => private_inputs.write_into(writer),
            Message::Relation(relation) => relation.write_into(writer),
        }
    }
}
