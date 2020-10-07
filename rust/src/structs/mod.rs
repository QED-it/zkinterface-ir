pub mod messages;

pub mod header;
pub mod relation;
pub mod witness;
pub mod instance;

pub mod gates;
pub mod assignment;

pub type WireId = u64;
pub type Value = Vec<u8>;