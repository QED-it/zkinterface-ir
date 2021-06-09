use serde::{Deserialize, Serialize};

use crate::{Gate, Value, WireId};

/// BuildGate is similar to Gate but without output wires.
/// Useful in combination with GateBuilder.
#[derive(Clone, Debug, Eq, PartialEq, Hash, Deserialize, Serialize)]
pub enum BuildGate {
    Constant(Value),
    AssertZero(WireId),
    Copy(WireId),
    Add(WireId, WireId),
    Mul(WireId, WireId),
    AddConstant(WireId, Value),
    MulConstant(WireId, Value),
    And(WireId, WireId),
    Xor(WireId, WireId),
    Not(WireId),
    Instance(Value),
    Witness(Option<Value>),
    Free(WireId, Option<WireId>),
}

pub const NO_OUTPUT: WireId = WireId::MAX;

use std::hash::Hash;
use BuildGate::*;

impl BuildGate {
    pub fn with_output(self, outputs: &[WireId]) -> Gate {
        let output = outputs[0];
        match self {
            Constant(value) => Gate::Constant(output, value),
            AssertZero(input) => {
                assert_eq!(output, NO_OUTPUT);
                Gate::AssertZero(input)
            }
            Copy(input) => Gate::Copy(output, input),
            Add(left, right) => Gate::Add(output, left, right),
            Mul(left, right) => Gate::Mul(output, left, right),
            AddConstant(left, value) => Gate::AddConstant(output, left, value),
            MulConstant(left, value) => Gate::MulConstant(output, left, value),
            And(left, right) => Gate::And(output, left, right),
            Xor(left, right) => Gate::Xor(output, left, right),
            Not(input) => Gate::Not(output, input),
            Instance(_value) => Gate::Instance(output),
            Witness(_value) => Gate::Witness(output),
            Free(first, last) => {
                assert_eq!(output, NO_OUTPUT);
                Gate::Free(first, last)
            }
            Function() => {
                // TODO: check that outputs is indeed an array



            }
        }
    }

    pub fn has_output(&self) -> bool {
        match *self {
            AssertZero(_) => false,
            Free(_, _) => false,
            _ => true,
        }
    }
}
