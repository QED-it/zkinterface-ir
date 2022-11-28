/// Example statements generator with several types.
pub mod examples;
/// Example statements generator.
pub mod simple_examples;

/// A Sink stores produced messages, e.g. in files.
pub mod sink;

/// A variant of gates for use with a GateBuilder.
pub mod build_gates;
/// r1cs to ir converter
pub mod from_r1cs;

/// gates builder and interface
pub mod builder;
