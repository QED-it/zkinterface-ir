/// A Source finds and loads messages, e.g. from files.
pub mod source;

/// The Validator checks the format and semantics of a statement.
pub mod validator;

/// The Evaluator determines whether a statement is true by evaluating the circuit using the short witness.
pub mod evaluator;

/// Stats aggregates statistics about a circuit.
pub mod stats;

/// Helper functions to read buffers.
pub mod utils;

// TODO: fix it.
pub mod flattening;

pub const TEMPORARY_WIRES_START: u64 = 1u64<<63;
