use serde::{Serialize, Deserialize};
use std::error::Error;
use std::convert::TryFrom;
use crate::sieve_ir_generated::sieve_ir as fb;
use crate::{Result, Instance, Witness, Relation};


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
            fb::Message::NONE => {
                return Err("Invalid message type".into())
            }
        })
    }
}