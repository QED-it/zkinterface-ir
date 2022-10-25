/// Example statements generator.
pub mod examples;
/// Example statements generator with several fields.
pub mod examples_with_several_fields;

/// A Sink stores produced messages, e.g. in files.
pub mod sink;

/// A variant of gates for use with a GateBuilder.
pub mod build_gates;
/// r1cs to ir converter
pub mod from_r1cs;

/// gates builder and interface
pub mod builder;
