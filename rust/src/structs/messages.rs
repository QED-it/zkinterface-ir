use std::error::Error;
use std::convert::TryFrom;
use crate::Result;
use serde::{Deserialize, Serialize};

use crate::sieve_ir_generated::sieve_ir as g;
use crate::{Reader, Instance,Witness,Relation};


#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Messages {
    pub instances: Vec<Instance>,
    pub witnesses: Vec<Witness>,
    pub relations: Vec<Relation>,
}

impl TryFrom<&Reader> for Messages {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers messages to owned structure.
    fn try_from(reader: &Reader) -> Result<Messages> {
        let mut messages = Messages::default();

        for msg in reader {
            match msg.message_type() {
                g::Message::Instance => {
                    let g_instance = msg.message_as_instance().unwrap();
                    messages.instances.push(
                        Instance::try_from(g_instance)?);
                }
                g::Message::Witness => {
                    let g_witness = msg.message_as_witness().unwrap();
                    messages.witnesses.push(
                        Witness::try_from(g_witness)?);
                }
                g::Message::Relation => {
                    let g_constraints = msg.message_as_relation().unwrap();
                    messages.relations.push(
                        Relation::try_from(g_constraints)?);
                }
                g::Message::NONE => {}
            }
        }
        Ok(messages)
    }
}
