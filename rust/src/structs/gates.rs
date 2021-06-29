use crate::Result;
use flatbuffers::{FlatBufferBuilder, ForwardsUOffset, Vector, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;

use super::wire::{build_wires_vector, from_id, from_ids_vector};
use super::value::{try_from_values_vector, build_values_vector};
use crate::structs::subcircuit::{try_from_block_vector, build_block_vector};
use crate::sieve_ir_generated::sieve_ir as g;
use crate::sieve_ir_generated::sieve_ir::GateSet as gs;
use crate::{Value, WireId};

#[derive(Clone, Debug, Eq, PartialEq, Hash, Deserialize, Serialize)]
pub enum Gate {
    /// Constant(output, constant)
    Constant(WireId, Value),
    /// AssertZero(input)
    AssertZero(WireId),
    /// Copy(output, input)
    Copy(WireId, WireId),
    /// Add(output, input, input)
    Add(WireId, WireId, WireId),
    /// Mul(output, input, input)
    Mul(WireId, WireId, WireId),
    /// AddConstant(output, input, constant)
    AddConstant(WireId, WireId, Value),
    /// MulConstant(output, input, constant)
    MulConstant(WireId, WireId, Value),
    /// And(output, input, input)
    And(WireId, WireId, WireId),
    /// Xor(output, input, input)
    Xor(WireId, WireId, WireId),
    /// Not(output, input)
    Not(WireId, WireId),
    /// Instance(output)
    Instance(WireId),
    /// Witness(output)
    Witness(WireId),
    /// Free(first, last)
    /// If the option is not given, then only the first wire is freed, otherwise all wires between
    /// the first and the last INCLUSIVE are freed.
    Free(WireId, Option<WireId>),
    /// Function Gate for generic custom gates
    /// Function(name, output_count, input_count, instance_count, witness_count, directives).
    Function(String, usize, usize, usize, usize, Vec<Gate>),
    /// GateCall(name, output_wires, input_wires)
    Call(String, Vec<WireId>, Vec<WireId>),
    /// GateSwitch(condition, output_wires, input_wires, instance_count, witness_count, cases, branches)
    Switch(WireId, Vec<WireId>, Vec<WireId>, usize, usize, Vec<Value>, Vec<Vec<Gate>>),
    /// GateFor(start_val, end_val, instance_count, witness_count, output_mapping, input_mapping, body)
    ///   Mapping = [base, stride, size]
    For(u64, u64, usize, usize, Vec<(WireId, u64, usize)>, Vec<(WireId, u64, usize)>, Vec<Gate>),
}

use Gate::*;

impl<'a> TryFrom<g::Gate<'a>> for Gate {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(gen_gate: g::Gate) -> Result<Gate> {
        Ok(match gen_gate.gate_type() {
            gs::NONE => return Err("No gate type".into()),

            gs::GateConstant => {
                let gate = gen_gate.gate_as_gate_constant().unwrap();
                Constant(
                    gate.output().ok_or("Missing output")?.id(),
                    Vec::from(gate.constant().ok_or("Missing constant")?),
                )
            }

            gs::GateAssertZero => {
                let gate = gen_gate.gate_as_gate_assert_zero().unwrap();
                AssertZero(gate.input().ok_or("Missing input")?.id())
            }

            gs::GateCopy => {
                let gate = gen_gate.gate_as_gate_copy().unwrap();
                Copy(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.input().ok_or("Missing input")?.id(),
                )
            }

            gs::GateAdd => {
                let gate = gen_gate.gate_as_gate_add().unwrap();
                Add(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.left().ok_or("Missing left input")?.id(),
                    gate.right().ok_or("Missing right input")?.id(),
                )
            }

            gs::GateMul => {
                let gate = gen_gate.gate_as_gate_mul().unwrap();
                Mul(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.left().ok_or("Missing left input")?.id(),
                    gate.right().ok_or("Missing right input")?.id(),
                )
            }

            gs::GateAddConstant => {
                let gate = gen_gate.gate_as_gate_add_constant().unwrap();
                AddConstant(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.input().ok_or("Missing input")?.id(),
                    Vec::from(gate.constant().ok_or("Missing constant")?),
                )
            }

            gs::GateMulConstant => {
                let gate = gen_gate.gate_as_gate_mul_constant().unwrap();
                MulConstant(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.input().ok_or("Missing input")?.id(),
                    Vec::from(gate.constant().ok_or("Missing constant")?),
                )
            }

            gs::GateAnd => {
                let gate = gen_gate.gate_as_gate_and().unwrap();
                And(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.left().ok_or("Missing left input")?.id(),
                    gate.right().ok_or("Missing right input")?.id(),
                )
            }

            gs::GateXor => {
                let gate = gen_gate.gate_as_gate_xor().unwrap();
                Xor(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.left().ok_or("Missing left input")?.id(),
                    gate.right().ok_or("Missing right input")?.id(),
                )
            }

            gs::GateNot => {
                let gate = gen_gate.gate_as_gate_not().unwrap();
                Not(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.input().ok_or("Missing input")?.id(),
                )
            }

            gs::GateInstance => {
                let gate = gen_gate.gate_as_gate_instance().unwrap();
                Instance(gate.output().ok_or("Missing output")?.id())
            }

            gs::GateWitness => {
                let gate = gen_gate.gate_as_gate_witness().unwrap();
                Witness(gate.output().ok_or("Missing output")?.id())
            }

            gs::GateFree => {
                let gate = gen_gate.gate_as_gate_free().unwrap();
                Free(
                    gate.first().ok_or("Missing first wire")?.id(),
                    gate.last().map(|id| id.id()),
                )
            }

            gs::Function => {
                let gate = gen_gate.gate_as_function().unwrap();
                let g_directives = gate
                    .subcircuit()
                    .ok_or("Missing reference implementation")?;
                let directives = Gate::try_from_vector(g_directives)?;
                Function(
                    gate.name().ok_or("Missing name")?.to_string(),
                    gate.output_count() as usize,
                    gate.input_count() as usize,
                    gate.instance_count() as usize,
                    gate.witness_count() as usize,
                    directives,
                )
            }

            gs::GateCall => {
                let gate = gen_gate.gate_as_gate_call().unwrap();

                Call(
                    gate.name().ok_or("Missing function name.")?.into(),
                    from_ids_vector(gate.output_wires().ok_or("Missing outputs")?),
                    from_ids_vector(gate.input_wires().ok_or("Missing inputs")?),
                )
            }

            gs::GateSwitch => {
                let gate = gen_gate.gate_as_gate_switch().unwrap();

                let cases = try_from_values_vector(gate.cases()
                    .ok_or("Missing cases values")?)?;

                Switch(
                    from_id(gate.condition().ok_or("Missing condition wire.")?),
                    from_ids_vector(gate.output_wires().ok_or("Missing output wires")?),
                    from_ids_vector(gate.input_wires().ok_or("Missing input wires")?),
                    gate.instance_count() as usize,
                    gate.witness_count() as usize,
                    cases,
                    try_from_block_vector(gate.branches().ok_or("Missing branches")?)?,
                )
            }

            gs::GateFor => {
                let gate = gen_gate.gate_as_gate_for().unwrap();
                let output_mappings = gate.output_map().ok_or("missing output mappings")?;
                let input_mappings = gate.input_map().ok_or("missing input mappings")?;

                let output_map = output_mappings.iter().map(|mapping| (mapping.base().id(), mapping.stride().delta() , mapping.size_() as usize)).collect();
                let input_map = input_mappings.iter().map(|mapping| (mapping.base().id(), mapping.stride().delta(), mapping.size_() as usize)).collect();

                For(
                    gate.start_val(),
                    gate.end_val(),
                    gate.instance_count() as usize,
                    gate.witness_count() as usize,
                    output_map,
                    input_map,
                    Gate::try_from_vector(gate.body().ok_or("Missing body of for loop")?)?,
                )
            },
        })
    }
}

impl Gate {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::Gate<'bldr>> {
        match self {
            Constant(output, constant) => {
                let constant = builder.create_vector(constant);
                let gate = g::GateConstant::create(
                    builder,
                    &g::GateConstantArgs {
                        output: Some(&g::Wire::new(*output)),
                        constant: Some(constant),
                    },
                );
                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateConstant,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            AssertZero(input) => {
                let gate = g::GateAssertZero::create(
                    builder,
                    &g::GateAssertZeroArgs {
                        input: Some(&g::Wire::new(*input)),
                    },
                );
                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateAssertZero,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Copy(output, input) => {
                let gate = g::GateCopy::create(
                    builder,
                    &g::GateCopyArgs {
                        output: Some(&g::Wire::new(*output)),
                        input: Some(&g::Wire::new(*input)),
                    },
                );
                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateCopy,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Add(output, left, right) => {
                let gate = g::GateAdd::create(
                    builder,
                    &g::GateAddArgs {
                        output: Some(&g::Wire::new(*output)),
                        left: Some(&g::Wire::new(*left)),
                        right: Some(&g::Wire::new(*right)),
                    },
                );
                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateAdd,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Mul(output, left, right) => {
                let gate = g::GateMul::create(
                    builder,
                    &g::GateMulArgs {
                        output: Some(&g::Wire::new(*output)),
                        left: Some(&g::Wire::new(*left)),
                        right: Some(&g::Wire::new(*right)),
                    },
                );
                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateMul,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            AddConstant(output, input, constant) => {
                let constant = builder.create_vector(constant);
                let gate = g::GateAddConstant::create(
                    builder,
                    &g::GateAddConstantArgs {
                        output: Some(&g::Wire::new(*output)),
                        input: Some(&g::Wire::new(*input)),
                        constant: Some(constant),
                    },
                );
                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateAddConstant,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            MulConstant(output, input, constant) => {
                let constant = builder.create_vector(constant);
                let gate = g::GateMulConstant::create(
                    builder,
                    &g::GateMulConstantArgs {
                        output: Some(&g::Wire::new(*output)),
                        input: Some(&g::Wire::new(*input)),
                        constant: Some(constant),
                    },
                );
                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateMulConstant,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            And(output, left, right) => {
                let gate = g::GateAnd::create(
                    builder,
                    &g::GateAndArgs {
                        output: Some(&g::Wire::new(*output)),
                        left: Some(&g::Wire::new(*left)),
                        right: Some(&g::Wire::new(*right)),
                    },
                );
                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateAnd,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Xor(output, left, right) => {
                let gate = g::GateXor::create(
                    builder,
                    &g::GateXorArgs {
                        output: Some(&g::Wire::new(*output)),
                        left: Some(&g::Wire::new(*left)),
                        right: Some(&g::Wire::new(*right)),
                    },
                );
                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateXor,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Not(output, input) => {
                let gate = g::GateNot::create(
                    builder,
                    &g::GateNotArgs {
                        output: Some(&g::Wire::new(*output)),
                        input: Some(&g::Wire::new(*input)),
                    },
                );
                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateNot,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Instance(output) => {
                let gate = g::GateInstance::create(
                    builder,
                    &g::GateInstanceArgs {
                        output: Some(&g::Wire::new(*output)),
                    },
                );
                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateInstance,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Witness(output) => {
                let gate = g::GateWitness::create(
                    builder,
                    &g::GateWitnessArgs {
                        output: Some(&g::Wire::new(*output)),
                    },
                );
                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateWitness,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Free(first, last) => {
                let gate = g::GateFree::create(
                    builder,
                    &g::GateFreeArgs {
                        first: Some(&g::Wire::new(*first)),
                        last: last.map(|id| g::Wire::new(id)).as_ref(),
                    },
                );

                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateFree,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Function(
                name,
                output_count,
                input_count,
                instance_count,
                witness_count,
                subcircuit,
            ) => {
                let g_name = builder.create_string(name);
                let impl_gates = Gate::build_vector(builder, &subcircuit);
                let g_gate = g::Function::create(
                    builder,
                    &g::FunctionArgs {
                        name: Some(g_name),
                        output_count: *output_count as u64,
                        input_count: *input_count as u64,
                        instance_count: *instance_count as u64,
                        witness_count: *witness_count as u64,
                        subcircuit: Some(impl_gates)
                    },
                );

                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::Function,
                        gate: Some(g_gate.as_union_value()),
                    },
                )
            }

            Call(name, output_wires, input_wires) => {
                let g_name = builder.create_string(name);
                let g_outputs = build_wires_vector(builder, output_wires);
                let g_inputs = build_wires_vector(builder, input_wires);

                let g_gate = g::GateCall::create(
                    builder,
                    &g::GateCallArgs {
                        name: Some(g_name),
                        output_wires: Some(g_outputs),
                        input_wires: Some(g_inputs),
                    },
                );

                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateCall,
                        gate: Some(g_gate.as_union_value()),
                    },
                )
            }

            Switch(
                condition,
                outputs_list,
                inputs_list,
                instance_count,
                witness_count,
                cases,
                subcircuits
            ) => {

                let output_wires = build_wires_vector(builder, outputs_list);
                let input_wires = build_wires_vector(builder, inputs_list);
                let cases = build_values_vector(builder, cases);
                let branches = build_block_vector(builder, subcircuits);

                let gate = g::GateSwitch::create(
                    builder,
                    &g::GateSwitchArgs {
                        condition: Some(&g::Wire::new(*condition)),
                        output_wires: Some(output_wires),
                        input_wires: Some(input_wires),
                        instance_count: *instance_count as u64,
                        witness_count: *witness_count as u64,
                        cases: Some(cases),
                        branches: Some(branches),
                    },
                );

                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateSwitch,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            For(
                start_val,
                end_val,
                instance_count,
                witness_count,
                output_mappings,
                input_mappings,
                body
            ) => {

                let output_maps_tmp: Vec<_> = output_mappings.iter().map(|mapping| g::Mapping::new (
                    &g::Wire::new(mapping.0),
                    &g::WireDelta::new(mapping.1),
                    mapping.2 as u64,
                )).collect();
                let g_output_map = builder.create_vector(&output_maps_tmp);

                let input_maps_tmp: Vec<_> = input_mappings.iter().map(|mapping| g::Mapping::new (
                    &g::Wire::new(mapping.0),
                    &g::WireDelta::new(mapping.1),
                    mapping.2 as u64,
                )).collect();
                let g_input_map = builder.create_vector(&input_maps_tmp);

                let g_body = Gate::build_vector(builder, body);

                let gate = g::GateFor::create(
                    builder,
                    &g::GateForArgs {
                        start_val: *start_val,
                        end_val: *end_val,
                        instance_count: *instance_count as u64,
                        witness_count: *witness_count as u64,
                        output_map: Some(g_output_map),
                        input_map: Some(g_input_map),
                        body: Some(g_body),
                    },
                );

                g::Gate::create(
                    builder,
                    &g::GateArgs {
                        gate_type: gs::GateFor,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }
        }
    }

    /// Convert from a Flatbuffers vector of gates to owned structures.
    pub fn try_from_vector<'a>(
        g_vector: Vector<'a, ForwardsUOffset<g::Gate<'a>>>,
    ) -> Result<Vec<Gate>> {
        let mut gates = vec![];
        for i in 0..g_vector.len() {
            let g_a = g_vector.get(i);
            gates.push(Gate::try_from(g_a)?);
        }
        Ok(gates)
    }

    /// Add a vector of this structure into a Flatbuffers message builder.
    pub fn build_vector<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
        gates: &'args [Gate],
    ) -> WIPOffset<Vector<'bldr, ForwardsUOffset<g::Gate<'bldr>>>> {
        let g_gates: Vec<_> = gates.iter().map(|gate| gate.build(builder)).collect();
        let g_vector = builder.create_vector(&g_gates);
        g_vector
    }

    /// Returns the output wire id if exists.
    /// if not, returns None
    fn _get_output_wire_id(&self) -> Option<WireId> {
        match *self {
            Constant(w, _) => Some(w),
            Copy(w, _) => Some(w),
            Add(w, _, _) => Some(w),
            Mul(w, _, _) => Some(w),
            AddConstant(w, _, _) => Some(w),
            MulConstant(w, _, _) => Some(w),
            And(w, _, _) => Some(w),
            Xor(w, _, _) => Some(w),
            Not(w, _) => Some(w),
            Instance(w) => Some(w),
            Witness(w) => Some(w),

            AssertZero(_) => None,
            Free(_, _) => None,

            Function(_, _, _, _, _, _) => None,
            Call(_, _, _) => unimplemented!("Call gate"),
            Switch(_, _, _, _, _, _, _) =>  unimplemented!("Switch gate"),
            For(_, _, _, _, _, _, _) => unimplemented!("For loop"),
        }
    }
}
