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

/// Assignment holds a value assigned to a wire.
pub mod assignment;

/// Wires are identified by a numerical ID.
pub type WireId = u64;

/// A Value is a field element encoded least-significant-byte-first (little-endian). Trailing zeros may be omitted.
///
/// Example: the element `one` on a 32 bits fields is encoded `[1, 0, 0, 0]`.
/// The compact form `[1]` is also valid.
pub type Value = Vec<u8>;
