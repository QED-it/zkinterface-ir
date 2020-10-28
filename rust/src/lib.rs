pub extern crate flatbuffers;
pub extern crate serde;

/// Message reader and writer code generated from the FlatBuffers schema (../sieve_ir.fbs).
#[allow(unused_imports)]
pub mod sieve_ir_generated;

/// Implementation of the CLI.
pub mod cli;

/// Fully-owned version of each data structure.
/// These structures may be easier to work with than the
/// no-copy versions found in sieve_ir_generated and Reader.
pub mod structs;

/// Tools and helpers to produce messages.
pub mod producers;

/// Tools and helpers to consume messages.
pub mod consumers;

// Exports.
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

// Common definitions.
pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;