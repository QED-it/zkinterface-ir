//! zki_sieve is an implementation of SIEVE IR.
//!
//! It includes Rust data structures and functions to produce and consume statements.
//!
//! A set of tools to work with SIEVE IR statements is available as a CLI (command line interface).
//!
//! Install and get started with the commands:
//! ```text
//! cargo install --path .
//!
//! zki_sieve help
//! ```

pub extern crate flatbuffers;
pub extern crate serde;

/// Message reader and writer code generated from the FlatBuffers schema (../sieve_ir.fbs).
#[allow(unused_imports, clippy::all)]
pub mod sieve_ir_generated;

/// Implementation of the CLI.
pub mod cli;

/// Each IR data structure is mapped to a Rust struct.
///
/// These structures are pure values (no references) which may be easier
/// to work with than the no-copy versions found in sieve_ir_generated.
pub mod structs;

/// Tools and helpers to produce messages.
pub mod producers;

/// Tools and helpers to consume messages.
pub mod consumers;

/// Tools and helpers to evaluate plugins.
pub mod plugins;

// Exports.
pub use consumers::source::Source;
pub use producers::sink::{clean_workspace, FilesSink, Sink};
/// The extension of files containing messages encoded in FlatBuffers-binary.
pub use sieve_ir_generated::sieve_ir::ROOT_EXTENSION as FILE_EXTENSION;
pub use structs::{
    gates::Gate, header::Header, message::Message, messages::Messages,
    private_inputs::PrivateInputs, public_inputs::PublicInputs, relation::Relation, value::Value,
    TypeId, WireId,
};

/// Common definition of Result with generic errors.
pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

/// Creates a `WireList` containing the WireIds.
///
/// `wirelist!` allows `WireList` to be defined with the same syntax as array expressions.
/// There are two forms of this macro:
///
/// - Create a `WireList` containing a given list of WireIds:
///
/// ```
/// use zki_sieve::structs::wire::WireListElement;
/// use zki_sieve::wirelist;
/// let v = wirelist![5; 1, 2, 3];
/// assert_eq!(v[0], WireListElement::Wire(5, 1));
/// assert_eq!(v[1], WireListElement::Wire(5, 2));
/// assert_eq!(v[2], WireListElement::Wire(5, 3));
/// ```
///
/// - Create a `WireList` from a given WireId and size:
///
/// ```
/// use zki_sieve::structs::wire::WireListElement;
/// use zki_sieve::wirelist;
/// let v = wirelist![2; 1; 3];
/// assert_eq!(v, [WireListElement::Wire(2, 1), WireListElement::Wire(2, 1), WireListElement::Wire(2, 1)]);
/// ```
#[macro_export]
macro_rules! wirelist {
    ($type_id:expr; $elem:expr; $n:expr) => (
        vec![WireListElement::Wire($type_id, $elem); $n]
    );
    ($type_id:expr;  $( $x:expr ),*) => (
        vec![$(WireListElement::Wire($type_id, $x)),*]
    );
}
