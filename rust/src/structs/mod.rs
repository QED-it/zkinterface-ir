/// A structure representing a message of type Instance.
pub mod instance;

/// A structure representing a message of type Witness.
pub mod witness;

/// A structure representing a message of type Relation.
pub mod relation;

/// A common structure for message headers.
pub mod header;

/// Message is a wrapper for all types of messages in a common enum.
pub mod message;

/// Messages can hold all messages of a statement in a single convenient structure. This is used to convert full statements to or from JSON or YAML.
pub mod messages;

/// Gate is an enum that can represent all types of gates.
pub mod gates;

/// Value holds a value assigned to a wire.
pub mod value;

pub mod wire;
pub mod subcircuit;

/// Wires are identified by a numerical ID.
pub type WireId = u64;
