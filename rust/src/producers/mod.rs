/// A Sink stores produced messages, e.g. in files.
pub mod sink;
/// Example statements generator.
pub mod examples;

/// r1cs to ir converter
// TODO: port to inline-witness format.
// pub mod from_r1cs;

/// gates builder and interface
pub mod builder;
/// A variant of gates for use with a Builder.

pub mod build_gates;
