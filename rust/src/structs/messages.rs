use serde::{Deserialize, Serialize};

use crate::consumers::reader::Reader;
use crate::sieve_ir_generated::sieve_ir as g;
use super::relation::Relation;
use super::witness::Witness;
use super::instance::Instance;


#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Messages {
    pub relations: Vec<Relation>,
    pub instances: Vec<Instance>,
    pub witnesses: Vec<Witness>,
}

impl From<&Reader> for Messages {
    /// Convert from Flatbuffers messages to owned structure.
    fn from(reader: &Reader) -> Messages {
        let mut messages = Messages::default();

        for msg in reader {
            match msg.message_type() {
                g::Message::Relation => {
                    let g_constraints = msg.message_as_relation().unwrap();
                    messages.relations.push(
                        Relation::from(g_constraints));
                }
                g::Message::Instance => {
                    let g_instance = msg.message_as_instance().unwrap();
                    messages.instances.push(
                        Instance::from(g_instance));
                }
                g::Message::Witness => {
                    let g_witness = msg.message_as_witness().unwrap();
                    messages.witnesses.push(
                        Witness::from(g_witness));
                }
                g::Message::NONE => {}
            }
        }
        messages
    }
}
