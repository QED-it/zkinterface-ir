use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;

use crate::sieve_ir_generated::sieve_ir as generated;
use flatbuffers::{FlatBufferBuilder, ForwardsUOffset, Vector, WIPOffset};

use super::wire::{build_wire_list, WireList};
use crate::structs::count::{build_count_list, CountList};
use crate::{FieldId, Gate, Result};

// ******************************
//
//   Functions declaration
//    (used everywhere)
// ******************************

/// This structure handles the declaration of a function.
#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Function {
    pub name: String,
    pub output_count: CountList,
    pub input_count: CountList,
    pub instance_count: CountList,
    pub witness_count: CountList,
    pub body: Vec<Gate>,
}

/// This function imports a FBS binary Function declaration into a Rust equivalent.
impl<'a> TryFrom<generated::Function<'a>> for Function {
    type Error = Box<dyn Error>;

    fn try_from(g_function: generated::Function) -> Result<Function> {
        let g_directives = g_function
            .body()
            .ok_or("Missing reference implementation")?;
        let output_count = CountList::try_from(
            g_function
                .output_count()
                .ok_or("Missing output_count in Function")?,
        )?;
        let input_count = CountList::try_from(
            g_function
                .input_count()
                .ok_or("Missing input_count in Function")?,
        )?;
        let instance_count = CountList::try_from(
            g_function
                .instance_count()
                .ok_or("Missing instance_count in Function")?,
        )?;
        let witness_count = CountList::try_from(
            g_function
                .witness_count()
                .ok_or("Missing witness_count in Function")?,
        )?;

        Ok(Function {
            name: g_function.name().ok_or("Missing name")?.to_string(),
            output_count,
            input_count,
            instance_count,
            witness_count,
            body: Gate::try_from_vector(g_directives)?,
        })
    }
}

impl Function {
    /// Default constructor
    pub fn new(
        name: String,
        output_count: CountList,
        input_count: CountList,
        instance_count: CountList,
        witness_count: CountList,
        body: Vec<Gate>,
    ) -> Self {
        Function {
            name,
            output_count,
            input_count,
            instance_count,
            witness_count,
            body,
        }
    }

    /// Serialize this structure into a Flatbuffer message
    pub fn build<'bldr>(
        &self,
        builder: &mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<generated::Function<'bldr>> {
        let g_name = builder.create_string(&self.name);
        let g_body = Gate::build_vector(builder, &self.body);
        let g_output_count = build_count_list(builder, &self.output_count);
        let g_input_count = build_count_list(builder, &self.input_count);
        let g_instance_count = build_count_list(builder, &self.instance_count);
        let g_witness_count = build_count_list(builder, &self.witness_count);

        generated::Function::create(
            builder,
            &generated::FunctionArgs {
                name: Some(g_name),
                output_count: Some(g_output_count),
                input_count: Some(g_input_count),
                instance_count: Some(g_instance_count),
                witness_count: Some(g_witness_count),
                body: Some(g_body),
            },
        )
    }

    /// Import a vector of binary Functions into a Rust vector of Function declarations.
    pub fn try_from_vector<'a>(
        g_vector: Vector<'a, ForwardsUOffset<generated::Function<'a>>>,
    ) -> Result<Vec<Function>> {
        let mut functions = vec![];
        for i in 0..g_vector.len() {
            let g_a = g_vector.get(i);
            functions.push(Function::try_from(g_a)?);
        }
        Ok(functions)
    }

    /// Build a vector a Rust Functions into the associated FBS structure.
    pub fn build_vector<'bldr>(
        builder: &mut FlatBufferBuilder<'bldr>,
        functions: &[Function],
    ) -> WIPOffset<Vector<'bldr, ForwardsUOffset<generated::Function<'bldr>>>> {
        let g_functions: Vec<_> = functions.iter().map(|gate| gate.build(builder)).collect();
        builder.create_vector(&g_functions)
    }
}

// ******************************
//
//   CaseInvoke (used in switches)
//
// ******************************

/// This is the 'invocation' equivalent in the spec.
/// Ref. SIEVE-IR spec (3.7)
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum CaseInvoke {
    /// AbstractGateCall(name, input_wires)
    AbstractGateCall(String, WireList),
    /// AbstractAnonCall(input_wires, instance_count, witness_count, subcircuit)
    AbstractAnonCall(WireList, CountList, CountList, Vec<Gate>),
}

use CaseInvoke::*;

/// Import a FBS binary representation of an Invocation into a owned structure
/// It's mainly used in switches.
impl<'a> TryFrom<generated::CaseInvoke<'a>> for CaseInvoke {
    type Error = Box<dyn Error>;

    fn try_from(g_caseinvoke: generated::CaseInvoke) -> Result<CaseInvoke> {
        Ok(match g_caseinvoke.invocation_type() {
            generated::CaseInvokeU::NONE => return Err("No directive type".into()),
            generated::CaseInvokeU::AbstractGateCall => {
                from_gate_call(g_caseinvoke.invocation_as_abstract_gate_call().unwrap())?
            }
            generated::CaseInvokeU::AbstractAnonCall => {
                let gate_anon_call = g_caseinvoke.invocation_as_abstract_anon_call().unwrap();
                let g_subcircuit = gate_anon_call
                    .subcircuit()
                    .ok_or("Missing implementation")?;
                let subcircuit = Gate::try_from_vector(g_subcircuit)?;
                let instance_count = CountList::try_from(
                    gate_anon_call
                        .instance_count()
                        .ok_or("Missing instance_count in AbstractAnonCall")?,
                )?;
                let witness_count = CountList::try_from(
                    gate_anon_call
                        .witness_count()
                        .ok_or("Missing witness_count in AbstractAnonCall")?,
                )?;

                AbstractAnonCall(
                    WireList::try_from(gate_anon_call.input_wires().ok_or("Missing inputs")?)?,
                    instance_count,
                    witness_count,
                    subcircuit,
                )
            }
        })
    }
}

/// Dedicated helper function that imports a FBS AbstractGateCall into a
/// CaseInvoke internal structure.
pub fn from_gate_call(gate_call: generated::AbstractGateCall) -> Result<CaseInvoke> {
    Ok(AbstractGateCall(
        gate_call.name().ok_or("Missing function name.")?.into(),
        WireList::try_from(gate_call.input_wires().ok_or("Missing inputs")?)?,
    ))
}

impl CaseInvoke {
    /// Serialize this CaseInvoke into a Flatbuffer message
    pub fn build<'bldr>(
        &self,
        builder: &mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<generated::CaseInvoke<'bldr>> {
        match self {
            AbstractGateCall(name, input_wires) => {
                let g_directive = build_gate_call(builder, name, input_wires);
                generated::CaseInvoke::create(
                    builder,
                    &generated::CaseInvokeArgs {
                        invocation_type: generated::CaseInvokeU::AbstractGateCall,
                        invocation: Some(g_directive.as_union_value()),
                    },
                )
            }
            AbstractAnonCall(input_wires, instance_count, witness_count, subcircuit) => {
                let g_inputs = build_wire_list(builder, input_wires);
                let impl_gates = Gate::build_vector(builder, subcircuit);
                let g_instance_count = build_count_list(builder, instance_count);
                let g_witness_count = build_count_list(builder, witness_count);

                let g_directive = generated::AbstractAnonCall::create(
                    builder,
                    &generated::AbstractAnonCallArgs {
                        input_wires: Some(g_inputs),
                        instance_count: Some(g_instance_count),
                        witness_count: Some(g_witness_count),
                        subcircuit: Some(impl_gates),
                    },
                );

                generated::CaseInvoke::create(
                    builder,
                    &generated::CaseInvokeArgs {
                        invocation_type: generated::CaseInvokeU::AbstractAnonCall,
                        invocation: Some(g_directive.as_union_value()),
                    },
                )
            }
        }
    }

    /// Convert from Flatbuffers vector of CaseInvoke into a Rust vector of CaseInvoke.
    pub fn try_from_vector<'a>(
        g_vector: Vector<'a, ForwardsUOffset<generated::CaseInvoke<'a>>>,
    ) -> Result<Vec<CaseInvoke>> {
        let mut directives = vec![];
        for i in 0..g_vector.len() {
            let g_a = g_vector.get(i);
            directives.push(CaseInvoke::try_from(g_a)?);
        }
        Ok(directives)
    }

    /// Build a vector of this structure into a Flatbuffers message builder.
    pub fn build_vector<'bldr>(
        builder: &mut FlatBufferBuilder<'bldr>,
        directives: &[CaseInvoke],
    ) -> WIPOffset<Vector<'bldr, ForwardsUOffset<generated::CaseInvoke<'bldr>>>> {
        let g_directives: Vec<_> = directives
            .iter()
            .map(|directive| directive.build(builder))
            .collect();
        builder.create_vector(&g_directives)
    }
}

/// Dedicated helper function that exports a CaseInvoke internal structure into a
/// FBS AbstractGateCall message.
pub fn build_gate_call<'bldr>(
    builder: &mut FlatBufferBuilder<'bldr>,
    name: &str,
    input_wires: &WireList,
) -> WIPOffset<generated::AbstractGateCall<'bldr>> {
    let g_name = builder.create_string(name);
    let g_inputs = build_wire_list(builder, input_wires);
    generated::AbstractGateCall::create(
        builder,
        &generated::AbstractGateCallArgs {
            name: Some(g_name),
            input_wires: Some(g_inputs),
        },
    )
}

// ******************************
//
//   ForLoopBody (used in ..)
//
// ******************************
use crate::structs::iterators::IterExprList;

/// This is the 'invocation' equivalent in the spec.
/// Ref. SIEVE-IR spec (3.7)
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum ForLoopBody {
    /// IterExprCall(name, field_id, output_wires, input_wires)
    IterExprCall(String, FieldId, IterExprList, IterExprList),
    /// IterExprAnonCall(field_id, output_wires, input_wires, instance_count, witness_count, subcircuit)
    IterExprAnonCall(
        FieldId,
        IterExprList,
        IterExprList,
        CountList,
        CountList,
        Vec<Gate>,
    ),
}
