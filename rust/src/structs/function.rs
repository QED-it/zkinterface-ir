use std::convert::TryFrom;
use std::error::Error;
use serde::{Deserialize, Serialize};

use flatbuffers::{FlatBufferBuilder, Vector, WIPOffset, ForwardsUOffset};
use crate::sieve_ir_generated::sieve_ir as g;

use crate::{Result, Gate};
use crate::structs::wire::{WireList, build_wire_list};
use crate::structs::subcircuit::{try_from_block, build_block};

#[derive(Clone, Debug, Eq, PartialEq, Hash, Deserialize, Serialize)]
pub enum CaseInvoke {
    /// AbstractGateCall(name, input_wires)
    AbstractGateCall(String, WireList),
    /// AbstractAnonCall(input_wires, instance_count, witness_count, subcircuit)
    AbstractAnonCall(WireList, usize, usize, Vec<Gate>),
}

use CaseInvoke::*;

/// Convert from Flatbuffers references to owned structure.
impl<'a> TryFrom<g::CaseInvoke<'a>> for CaseInvoke {
    type Error = Box<dyn Error>;

    fn try_from(g_caseinvoke: g::CaseInvoke) -> Result<CaseInvoke> {
        Ok(match g_caseinvoke.invocation_type() {
            g::CaseInvokeU::NONE => return Err("No directive type".into()),
            g::CaseInvokeU::AbstractGateCall => {
                from_gate_call(g_caseinvoke.invocation_as_abstract_gate_call().unwrap())?
            }
            g::CaseInvokeU::AbstractAnonCall => {
                let gate_anon_call = g_caseinvoke.invocation_as_abstract_anon_call().unwrap();
                let g_subcircuit = gate_anon_call
                    .subcircuit()
                    .ok_or("Missing implementation")?;
                let subcircuit = try_from_block(g_subcircuit)?;
                AbstractAnonCall(
                    WireList::try_from(gate_anon_call.input_wires().ok_or("Missing inputs")?)?,
                    gate_anon_call.instance_count() as usize,
                    gate_anon_call.witness_count() as usize,
                    subcircuit,
                )
            }
        })
    }
}

pub fn from_gate_call(gate_call: g::AbstractGateCall) -> Result<CaseInvoke> {
    Ok(AbstractGateCall(
        gate_call.name().ok_or("Missing function name.")?.into(),
        WireList::try_from(gate_call.input_wires().ok_or("Missing inputs")?)?
    ))
}

impl CaseInvoke {
    /// Serialize this structure into a Flatbuffer message
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::CaseInvoke<'bldr>> {
        match self {
            AbstractGateCall(name, input_wires) => {
                let g_directive = build_gate_call(builder, name, input_wires);
                g::CaseInvoke::create(
                    builder,
                    &g::CaseInvokeArgs {
                        invocation_type: g::CaseInvokeU::AbstractGateCall,
                        invocation: Some(g_directive.as_union_value()),
                    },
                )
            }
            AbstractAnonCall(input_wires, instance_count, witness_count, subcircuit) => {
                let g_inputs = build_wire_list(builder, input_wires);
                let impl_gates = build_block(builder, subcircuit);
                let g_directive = g::AbstractAnonCall::create(
                    builder,
                    &g::AbstractAnonCallArgs {
                        input_wires: Some(g_inputs),
                        instance_count: *instance_count as u64,
                        witness_count: *witness_count as u64,
                        subcircuit: Some(impl_gates),
                    },
                );

                g::CaseInvoke::create(
                    builder,
                    &g::CaseInvokeArgs {
                        invocation_type: g::CaseInvokeU::AbstractAnonCall,
                        invocation: Some(g_directive.as_union_value()),
                    },
                )
            }
        }
    }


    /// Convert from Flatbuffers vector of directives into owned structure.
    pub fn try_from_vector<'a>(
        g_vector: Vector<'a, ForwardsUOffset<g::CaseInvoke<'a>>>,
    ) -> Result<Vec<CaseInvoke>> {
        let mut directives = vec![];
        for i in 0..g_vector.len() {
            let g_a = g_vector.get(i);
            directives.push(CaseInvoke::try_from(g_a)?);
        }
        Ok(directives)
    }

    /// Add a vector of this structure into a Flatbuffers message builder.
    pub fn build_vector<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
        directives: &'args [CaseInvoke],
    ) -> WIPOffset<Vector<'bldr, ForwardsUOffset<g::CaseInvoke<'bldr>>>> {
        let g_directives: Vec<_> = directives.iter().map(|directive| directive.build(builder)).collect();
        let g_vector = builder.create_vector(&g_directives);
        g_vector
    }
}


/// Add a vector of this structure into a Flatbuffers message builder.
pub fn build_gate_call<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    name: &str,
    input_wires: &WireList,
) -> WIPOffset<g::AbstractGateCall<'bldr>> {
    let g_name = builder.create_string(name);
    let g_inputs = build_wire_list(builder, input_wires);
    g::AbstractGateCall::create(
        builder,
        &g::AbstractGateCallArgs {
            name: Some(g_name),
            input_wires: Some(g_inputs),
        },
    )
}

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Function {
    pub name           :String,
    pub output_count   :usize,
    pub input_count    :usize,
    pub instance_count :usize,
    pub witness_count  :usize,
    pub body           :Vec<Gate>,
}

impl<'a> TryFrom<g::Function<'a>> for Function {
    type Error = Box<dyn Error>;

    fn try_from(g_function: g::Function) -> Result<Function> {
        let g_block = g_function
            .body()
            .ok_or("Missing reference implementation")?;

        Ok(Function {
            name: g_function.name().ok_or("Missing name") ?.to_string(),
            output_count: g_function.output_count() as usize,
            input_count: g_function.input_count() as usize,
            instance_count: g_function.instance_count() as usize,
            witness_count: g_function.witness_count() as usize,
            body: try_from_block(g_block)?,
        })
    }
}

impl Function {

    pub fn new(
        name           :String,
        output_count   :usize,
        input_count    :usize,
        instance_count :usize,
        witness_count  :usize,
        body           :Vec<Gate>,
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
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::Function<'bldr>> {
        let g_name = builder.create_string(&self.name);
        let g_body = build_block(builder, &self.body);

        g::Function::create(
            builder,
            &g::FunctionArgs {
                name: Some(g_name),
                output_count: self.output_count as u64,
                input_count: self.input_count as u64,
                instance_count: self.instance_count as u64,
                witness_count: self.witness_count as u64,
                body: Some(g_body),
            },
        )
    }

    pub fn try_from_vector<'a>(
        g_vector: Vector<'a, ForwardsUOffset<g::Function<'a>>>
    ) -> Result<Vec<Function>> {
        let mut functions = vec![];
        for i in 0..g_vector.len() {
            let g_a = g_vector.get(i);
            functions.push(Function::try_from(g_a)?);
        }
        Ok(functions)
    }

    pub fn build_vector<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
        functions: &'args [Function],
    ) -> WIPOffset<Vector<'bldr, ForwardsUOffset<g::Function<'bldr>>>> {
        let g_functions: Vec<_> = functions.iter().map(|gate| gate.build(builder)).collect();
        let g_vector = builder.create_vector(&g_functions);
        g_vector
    }
}