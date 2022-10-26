use crate::{Message, PrivateInputs, PublicInputs, Relation};
use serde::{Deserialize, Serialize};

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Messages {
    pub public_inputs: Vec<PublicInputs>,
    pub private_inputs: Vec<PrivateInputs>,
    pub relations: Vec<Relation>,
}

impl Messages {
    pub fn push_message(&mut self, msg: &Message) {
        match msg {
            Message::PublicInputs(i) => self.public_inputs.push(i.clone()),
            Message::PrivateInputs(w) => self.private_inputs.push(w.clone()),
            Message::Relation(r) => self.relations.push(r.clone()),
        }
    }
}
