use crate::{Instance, Message, Relation, Witness};
use serde::{Deserialize, Serialize};

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Messages {
    pub instances: Vec<Instance>,
    pub witnesses: Vec<Witness>,
    pub relations: Vec<Relation>,
}

impl Messages {
    pub fn push_message(&mut self, msg: &Message) {
        match msg {
            Message::Instance(i) => self.instances.push(i.clone()),
            Message::Witness(w) => self.witnesses.push(w.clone()),
            Message::Relation(r) => self.relations.push(r.clone()),
        }
    }
}
