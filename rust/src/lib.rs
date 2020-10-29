//! zki is an implementation of SIEVE IR.
//!
//! It includes Rust data structures and functions to produce and consume statements.
//!
//! A set of tools to work with SIEVE IR statements is available as a CLI (command line interface).
//!
//! Install and get started with the commands:
//! ```text
//! cargo install --path .
//!
//! zki help
//! ```

pub extern crate flatbuffers;
pub extern crate serde;

/// Message reader and writer code generated from the FlatBuffers schema (../sieve_ir.fbs).
#[allow(unused_imports)]
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

// Exports.
/// The extension of files containing messages encoded in FlatBuffers-binary.
pub use sieve_ir_generated::sieve_ir::ROOT_EXTENSION as FILE_EXTENSION;
pub use producers::sink::{
    Sink,
    FilesSink,
    clean_workspace,
};
pub use consumers::{
    source::Source,
};
pub use structs::{
    header::Header,
    relation::Relation,
    instance::Instance,
    witness::Witness,
    message::Message,
    messages::Messages,
    gates::Gate,
};

/// Common definition of Result with generic errors.
pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;