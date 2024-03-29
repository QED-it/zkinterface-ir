use serde::{Deserialize, Serialize};

use crate::structs::wirerange::WireRange;
use crate::{Gate, TypeId, Value, WireId};

/// BuildGate is similar to Gate but without output wires.
/// Useful in combination with GateBuilder.
#[derive(Clone, Debug, Eq, PartialEq, Hash, Deserialize, Serialize)]
pub enum BuildGate {
    Constant(TypeId, Value),
    AssertZero(TypeId, WireId),
    Copy(TypeId, WireId),
    Add(TypeId, WireId, WireId),
    Mul(TypeId, WireId, WireId),
    AddConstant(TypeId, WireId, Value),
    MulConstant(TypeId, WireId, Value),
    Public(TypeId, Option<Value>),
    Private(TypeId, Option<Value>),
    New(TypeId, WireId, WireId),
    Delete(TypeId, WireId, WireId),
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
            Public(type_id, _) => Gate::Public(type_id, output),
            Private(type_id, _) => Gate::Private(type_id, output),
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
            Public(type_id, _) => type_id,
            Private(type_id, _) => type_id,
            New(type_id, _, _) => type_id,
            Delete(type_id, _, _) => type_id,
        }
    }
}

/// BuildComplexGate is similar to a complex Gate (Call, or For) but without output wires.
/// Useful in combination with GateBuilder.
#[derive(Clone, Debug, Eq, PartialEq, Hash, Deserialize, Serialize)]
pub enum BuildComplexGate {
    // Call(name, in_ids)
    Call(String, Vec<WireRange>),
    // Convert(out_type_id, out_wire_count, in_first_id, in_last_id)
    Convert(TypeId, u64, TypeId, WireId, WireId),
}

use BuildComplexGate::*;

impl BuildComplexGate {
    pub fn with_output(self, out_ids: Vec<WireRange>) -> Gate {
        match self {
            Call(name, in_ids) => Gate::Call(name, out_ids, in_ids),
            Convert(out_type_id, _, in_type_id, in_first_id, in_last_id) => {
                if out_ids.len() != 1 {
                    panic!(
                        "Not possible to create a Convert gate: out_ids must contain one WireRange"
                    )
                }
                let out_range = out_ids.get(0).unwrap();
                Gate::Convert(
                    out_type_id,
                    out_range.first_id,
                    out_range.last_id,
                    in_type_id,
                    in_first_id,
                    in_last_id,
                )
            }
        }
    }
}
