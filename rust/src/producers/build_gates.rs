use serde::{Deserialize, Serialize};

use crate::producers::builder::SwitchParams;
use crate::structs::{function::CaseInvoke, wire::WireList};
use crate::{FieldId, Gate, Value, WireId};

/// BuildGate is similar to Gate but without output wires.
/// Useful in combination with GateBuilder.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum BuildGate {
    Constant(FieldId, Value),
    AssertZero(FieldId, WireId),
    Copy(FieldId, WireId),
    Add(FieldId, WireId, WireId),
    Mul(FieldId, WireId, WireId),
    AddConstant(FieldId, WireId, Value),
    MulConstant(FieldId, WireId, Value),
    Instance(FieldId, Option<Value>),
    Witness(FieldId, Option<Value>),
    Free(FieldId, WireId, Option<WireId>),
}

pub const NO_OUTPUT: WireId = WireId::MAX;

use BuildGate::*;

impl BuildGate {
    pub fn with_output(self, output: WireId) -> Gate {
        match self {
            Constant(field, value) => Gate::Constant(field, output, value),
            AssertZero(field, input) => {
                assert_eq!(output, NO_OUTPUT);
                Gate::AssertZero(field, input)
            }
            Copy(field, input) => Gate::Copy(field, output, input),
            Add(field, left, right) => Gate::Add(field, output, left, right),
            Mul(field, left, right) => Gate::Mul(field, output, left, right),
            AddConstant(field, left, value) => Gate::AddConstant(field, output, left, value),
            MulConstant(field, left, value) => Gate::MulConstant(field, output, left, value),
            Instance(field, _) => Gate::Instance(field, output),
            Witness(field, _) => Gate::Witness(field, output),
            Free(field, first, last) => {
                assert_eq!(output, NO_OUTPUT);
                Gate::Free(field, first, last)
            }
        }
    }

    pub fn has_output(&self) -> bool {
        !matches!(*self, AssertZero(_, _) | Free(_, _, _))
    }

    pub fn get_field(&self) -> FieldId {
        match *self {
            Constant(field, _) => field,
            AssertZero(field, _) => field,
            Copy(field, _) => field,
            Add(field, _, _) => field,
            Mul(field, _, _) => field,
            AddConstant(field, _, _) => field,
            MulConstant(field, _, _) => field,
            Instance(field, _) => field,
            Witness(field, _) => field,
            Free(field, _, _) => field,
        }
    }
}

/// BuildComplexGate is similar to a complex Gate (Call, Switch or For) but without output wires.
/// Useful in combination with GateBuilder.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum BuildComplexGate {
    // Call(name, input_wires)
    Call(String, WireList),
    // Switch(condition_field, condition_wire, cases, branches, params)
    Switch(FieldId, WireId, Vec<Value>, Vec<CaseInvoke>, SwitchParams),
    // Convert(output_field, count_output_wire, input_wires)
    Convert(FieldId, u64, WireList),
}

use BuildComplexGate::*;

impl BuildComplexGate {
    pub fn with_output(self, output: WireList) -> Gate {
        match self {
            Call(name, input_wires) => Gate::Call(name, output, input_wires),
            Switch(condition_field, condition_wire, cases, branches, _) => {
                Gate::Switch(condition_field, condition_wire, output, cases, branches)
            }
            Convert(_, _, input) => Gate::Convert(output, input),
        }
    }
}
