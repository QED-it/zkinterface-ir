use std::convert::TryFrom;
use std::error::Error;
use std::iter;
use serde::{Deserialize, Serialize};

use crate::Result;
use flatbuffers::{FlatBufferBuilder, Vector, WIPOffset, ForwardsUOffset};
use crate::sieve_ir_generated::sieve_ir as g;
use crate::sieve_ir_generated::sieve_ir::{Invocation, AbstractGateCall};
use crate::{WireId, Gate};
use crate::structs::wire::{from_ids_vector, build_wires_vector};

#[derive(Clone, Debug, Eq, PartialEq, Hash, Deserialize, Serialize)]
pub enum Directive {
    /// AbstractCall(name, input_wires)
    AbstractCall(String, Vec<WireId>),
    /// AbstractAnonCall(input_wires, instance_count, witness_count, subcircuit)
    AbstractAnonCall(Vec<WireId>, usize, usize, Vec<Gate>),
}

use Directive::*;

/// Convert from Flatbuffers references to owned structure.
impl<'a> TryFrom<g::Directive<'a>> for Directive {
    type Error = Box<dyn Error>;

    fn try_from(g_directive: g::Directive) -> Result<Directive> {
        Ok(match g_directive.gate_type() {
            Invocation::NONE => return Err("No directive type".into()),
            Invocation::AbstractGateCall => {
                from_gate_call(g_directive.gate_as_abstract_gate_call().unwrap())?
            }
            Invocation::AbstractAnonCall => {
                let gate_anon_call = g_directive.gate_as_abstract_anon_call().unwrap();
                let g_subcircuit = gate_anon_call
                    .subcircuit()
                    .ok_or("Missing implementation")?;
                let subcircuit = Gate::try_from_vector(g_subcircuit)?;
                AbstractAnonCall(
                    from_ids_vector(gate_anon_call.input_wires().ok_or("Missing inputs")?),
                    gate_anon_call.instance_count() as usize,
                    gate_anon_call.witness_count() as usize,
                    subcircuit,
                )
            }
        })
    }
}

pub fn from_gate_call(gate_call: g::AbstractGateCall) -> Result<Directive> {
    Ok(AbstractCall(
        gate_call.name().ok_or("Missing function name.")?.into(),
        from_ids_vector(gate_call.input_wires().ok_or("Missing inputs")?)
    ))
}

impl Directive {
    /// Serialize this structure into a Flatbuffer message
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::Directive<'bldr>> {
        match self {
            AbstractCall(name, input_wires) => {
                let g_directive = build_gate_call(builder, name, input_wires);
                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        gate_type: g::Invocation::AbstractGateCall,
                        gate: Some(g_directive.as_union_value()),
                    },
                )
            }
            AbstractAnonCall(input_wires, instance_count, witness_count, subcircuit) => {
                let g_inputs = build_wires_vector(builder, input_wires);
                let impl_gates = Gate::build_vector(builder, &subcircuit);
                let g_directive = g::AbstractAnonCall::create(
                    builder,
                    &g::AbstractAnonCallArgs {
                        input_wires: Some(g_inputs),
                        instance_count: *instance_count as u64,
                        witness_count: *witness_count as u64,
                        subcircuit: Some(impl_gates),
                    },
                );

                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        gate_type: g::Invocation::AbstractAnonCall,
                        gate: Some(g_directive.as_union_value()),
                    },
                )
            }
        }
    }


    /// Convert from Flatbuffers vector of directives into owned structure.
    pub fn try_from_vector<'a>(
        g_vector: Vector<'a, ForwardsUOffset<g::Directive<'a>>>,
    ) -> Result<Vec<Directive>> {
        let mut directives = vec![];
        for i in 0..g_vector.len() {
            let g_a = g_vector.get(i);
            directives.push(Directive::try_from(g_a)?);
        }
        Ok(directives)
    }

    /// Add a vector of this structure into a Flatbuffers message builder.
    pub fn build_vector<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
        directives: &'args [Directive],
    ) -> WIPOffset<Vector<'bldr, ForwardsUOffset<g::Directive<'bldr>>>> {
        let g_directives: Vec<_> = directives.iter().map(|directive| directive.build(builder)).collect();
        let g_vector = builder.create_vector(&g_directives);
        g_vector
    }
}


/// Add a vector of this structure into a Flatbuffers message builder.
pub fn build_gate_call<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    name: &str,
    input_wires: &[WireId],
) -> WIPOffset<AbstractGateCall<'bldr>> {
    let g_name = builder.create_string(name);
    let g_inputs = build_wires_vector(builder, input_wires);
    g::AbstractGateCall::create(
        builder,
        &g::AbstractGateCallArgs {
            name: Some(g_name),
            input_wires: Some(g_inputs),
        },
    )
}


pub fn translate_gates<'s>(subcircuit: &'s[Gate], output_input_wires: &'s[WireId]) -> impl Iterator<Item = Gate> + 's {
    subcircuit
        .iter()
        .flat_map(move |gate| iter::from_fn(move || match gate {
            Gate::Constant(out, val) => Some(Gate::Constant(output_input_wires[*out as usize], val.clone())),
            Gate::AssertZero(out) => Some(Gate::AssertZero(output_input_wires[*out as usize])),
            Gate::Copy(out, inp) => Some(Gate::Copy(output_input_wires[*out as usize], output_input_wires[*inp as usize])),
            Gate::Add(out, a, b) => Some(Gate::Add(output_input_wires[*out as usize], output_input_wires[*a as usize], output_input_wires[*b as usize])),
            Gate::Mul(out, a, b) => Some(Gate::Mul(output_input_wires[*out as usize], output_input_wires[*a as usize], output_input_wires[*b as usize])),
            Gate::AddConstant(out, a, val) => Some(Gate::AddConstant(output_input_wires[*out as usize], output_input_wires[*a as usize], val.clone())),
            Gate::MulConstant(out, a, val) => Some(Gate::MulConstant(output_input_wires[*out as usize], output_input_wires[*a as usize], val.clone())),
            Gate::And(out, a, b) => Some(Gate::And(output_input_wires[*out as usize], output_input_wires[*a as usize], output_input_wires[*b as usize])),
            Gate::Xor(out, a, b) => Some(Gate::Xor(output_input_wires[*out as usize], output_input_wires[*a as usize], output_input_wires[*b as usize])),
            Gate::Not(out, a) => Some(Gate::Not(output_input_wires[*out as usize], output_input_wires[*a as usize])),
            Gate::Instance(out) => Some(Gate::Instance(output_input_wires[*out as usize])),
            Gate::Witness(out) => Some(Gate::Witness(output_input_wires[*out as usize])),
            Gate::Free(from, end) => Some(Gate::Free(output_input_wires[*from as usize], end.map(|id| output_input_wires[id as usize]))),

            Gate::Call(outs,dir) => {
                match dir {
                    AbstractCall(name, input_wires) => Some(Gate::Call(
                        translate_vector_wires(outs, output_input_wires),
                        Directive::AbstractCall(name.clone(), translate_vector_wires(input_wires, output_input_wires))
                    )),
                    // This one should never happen
                    _ => None
                }
            }
            Gate::Switch(condition, output_wires, cases, branches) => {
                Some(Gate::Switch(
                    output_input_wires[*condition as usize],
                    translate_vector_wires(output_wires, output_input_wires),
                    cases.clone(),
                    branches.iter().map(|dir| translate_directive(dir, output_input_wires)).collect(),
                ))
            }

            // This one should never happen
            Gate::Function(..) => None,
            }))
}

fn translate_vector_wires(wires: &[WireId], output_input_wires: &[WireId]) -> Vec<WireId> {
    wires.iter().map(|id| output_input_wires[*id as usize]).collect()
}

fn translate_directive(directive: &Directive, output_input_wires: &[WireId]) -> Directive {
    match directive {
        AbstractCall(name, input_wires) => Directive::AbstractCall(name.clone(), translate_vector_wires(input_wires, output_input_wires)),
        AbstractAnonCall(input_wires, instance_count, witness_count, subcircuit) => {
            Directive::AbstractAnonCall(
                translate_vector_wires(input_wires, output_input_wires),
                *instance_count,
                *witness_count,
                translate_gates(subcircuit, output_input_wires).collect(),
            )
        }
    }
}