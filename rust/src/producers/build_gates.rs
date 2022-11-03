use serde::{Deserialize, Serialize};

use crate::structs::wire::WireList;
use crate::{Gate, TypeId, Value, WireId};

/// BuildGate is similar to Gate but without output wires.
/// Useful in combination with GateBuilder.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum BuildGate {
    Constant(TypeId, Value),
    AssertZero(TypeId, WireId),
    Copy(TypeId, WireId),
    Add(TypeId, WireId, WireId),
    Mul(TypeId, WireId, WireId),
    AddConstant(TypeId, WireId, Value),
    MulConstant(TypeId, WireId, Value),
    PublicInput(TypeId, Option<Value>),
    PrivateInput(TypeId, Option<Value>),
    New(TypeId, WireId, WireId),
    Delete(TypeId, WireId, Option<WireId>),
}

pub const NO_OUTPUT: WireId = WireId::MAX;

use BuildGate::*;

impl BuildGate {
    pub fn with_output(self, output: WireId) -> Gate {
        match self {
            Constant(type_id, value) => Gate::Constant(type_id, output, value),
            AssertZero(type_id, input) => {
                assert_eq!(output, NO_OUTPUT);
                Gate::AssertZero(type_id, input)
            }
            Copy(type_id, input) => Gate::Copy(type_id, output, input),
            Add(type_id, left, right) => Gate::Add(type_id, output, left, right),
            Mul(type_id, left, right) => Gate::Mul(type_id, output, left, right),
            AddConstant(type_id, left, value) => Gate::AddConstant(type_id, output, left, value),
            MulConstant(type_id, left, value) => Gate::MulConstant(type_id, output, left, value),
            PublicInput(type_id, _) => Gate::PublicInput(type_id, output),
            PrivateInput(type_id, _) => Gate::PrivateInput(type_id, output),
            New(type_id, first, last) => {
                assert_eq!(output, NO_OUTPUT);
                Gate::New(type_id, first, last)
            }
            Delete(type_id, first, last) => {
                assert_eq!(output, NO_OUTPUT);
                Gate::Delete(type_id, first, last)
            }
        }
    }

    pub fn has_output(&self) -> bool {
        !matches!(*self, AssertZero(_, _) | Delete(_, _, _) | New(_, _, _))
    }

    pub fn get_type_id(&self) -> TypeId {
        match *self {
            Constant(type_id, _) => type_id,
            AssertZero(type_id, _) => type_id,
            Copy(type_id, _) => type_id,
            Add(type_id, _, _) => type_id,
            Mul(type_id, _, _) => type_id,
            AddConstant(type_id, _, _) => type_id,
            MulConstant(type_id, _, _) => type_id,
            PublicInput(type_id, _) => type_id,
            PrivateInput(type_id, _) => type_id,
            New(type_id, _, _) => type_id,
            Delete(type_id, _, _) => type_id,
        }
    }
}

/// BuildComplexGate is similar to a complex Gate (Call, or For) but without output wires.
/// Useful in combination with GateBuilder.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum BuildComplexGate {
    // Call(name, input_wires)
    Call(String, WireList),
    // Convert(output_type_id, count_output_wire, input_wires)
    Convert(TypeId, u64, WireList),
}

use BuildComplexGate::*;

impl BuildComplexGate {
    pub fn with_output(self, output: WireList) -> Gate {
        match self {
            Call(name, input_wires) => Gate::Call(name, output, input_wires),
            Convert(_, _, input) => Gate::Convert(output, input),
        }
    }
}
