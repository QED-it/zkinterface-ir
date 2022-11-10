/// A common structure for inputs (used in public and private inputs).
pub mod inputs;

/// A structure representing a message of type PublicInputs.
pub mod public_inputs;

/// A structure representing a message of type PrivateInputs.
pub mod private_inputs;

/// A structure representing a message of type Relation.
pub mod relation;

/// Message is a wrapper for all types of messages in a common enum.
pub mod message;

/// Messages can hold all messages of a statement in a single convenient structure. This is used to convert full statements to or from JSON or YAML.
pub mod messages;

/// Gate is an enum that can represent all types of gates.
pub mod gates;

/// Value holds a value assigned to a wire.
pub mod value;

/// WireRange are used to defined a range of wires
pub mod wirerange;

/// Count are used to defined a (type_id, count)
/// It is used for all public/private count and for inputs/outputs count in function declarations
pub mod count;

/// Handles the Call gate stuff
pub mod function;

/// Handles the plugin stuff
pub mod plugin;

/// Wires are identified by a numerical ID.
pub type WireId = u64;

/// Types are identified by a numerical ID.
pub type TypeId = u8;

pub const IR_VERSION: &str = "2.0.0";
