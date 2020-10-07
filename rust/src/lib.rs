pub extern crate flatbuffers;
pub extern crate serde;

#[allow(unused_imports)]
pub mod sieve_ir_generated;

/// Fully-owned version of each data structure.
/// These structures may be easier to work with than the
/// no-copy versions found in sieve_ir_generated and Reader.
pub mod structs;

pub mod consumers;
pub mod producers;

pub use consumers::reader::Reader;
pub use structs::{
    header::Header,
    relation::Relation,
    instance::Instance,
    witness::Witness,
    messages::Messages,
    gates::Gate,
};

// Common definitions.
use std::error::Error;

pub type Result<T> = std::result::Result<T, Box<dyn Error>>;