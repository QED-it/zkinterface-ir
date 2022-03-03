use crate::Result;
use flatbuffers::{FlatBufferBuilder, ForwardsUOffset, Vector, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;

use super::function::{CaseInvoke, ForLoopBody};
use super::iterators::IterExprList;
use super::value::{build_values_vector, try_from_values_vector};
use super::wire::{build_wire, build_wire_list, from_id, WireList};
use crate::sieve_ir_generated::sieve_ir as g;
use crate::sieve_ir_generated::sieve_ir::DirectiveSet as ds;
use crate::{Value, WireId};

/// This one correspond to Directive in the FlatBuffers schema
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
    /// AnonCall(output_wires, input_wires, instance_count, witness_count, subcircuit)
    AnonCall(WireList, WireList, usize, usize, Vec<Gate>),
    /// GateCall(name, output_wires, input_wires)
    Call(String, WireList, WireList),
    /// GateSwitch(condition, output_wires, cases, branches)
    Switch(WireId, WireList, Vec<Value>, Vec<CaseInvoke>),
    /// GateFor(iterator_name, start_val, end_val, global_output_list, body)
    For(String, u64, u64, WireList, ForLoopBody),
}

use crate::structs::iterators::build_iterexpr_list;
use Gate::*;

impl<'a> TryFrom<g::Directive<'a>> for Gate {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(gen_gate: g::Directive) -> Result<Gate> {
        Ok(match gen_gate.directive_type() {
            ds::NONE => return Err("No gate type".into()),

            ds::GateConstant => {
                let gate = gen_gate.directive_as_gate_constant().unwrap();
                Constant(
                    gate.output().ok_or_else(|| "Missing output")?.id(),
                    Vec::from(gate.constant().ok_or_else(|| "Missing constant")?),
                )
            }

            ds::GateAssertZero => {
                let gate = gen_gate.directive_as_gate_assert_zero().unwrap();
                AssertZero(gate.input().ok_or_else(|| "Missing input")?.id())
            }

            ds::GateCopy => {
                let gate = gen_gate.directive_as_gate_copy().unwrap();
                Copy(
                    gate.output().ok_or_else(|| "Missing output")?.id(),
                    gate.input().ok_or_else(|| "Missing input")?.id(),
                )
            }

            ds::GateAdd => {
                let gate = gen_gate.directive_as_gate_add().unwrap();
                Add(
                    gate.output().ok_or_else(|| "Missing output")?.id(),
                    gate.left().ok_or_else(|| "Missing left input")?.id(),
                    gate.right().ok_or_else(|| "Missing right input")?.id(),
                )
            }

            ds::GateMul => {
                let gate = gen_gate.directive_as_gate_mul().unwrap();
                Mul(
                    gate.output().ok_or_else(|| "Missing output")?.id(),
                    gate.left().ok_or_else(|| "Missing left input")?.id(),
                    gate.right().ok_or_else(|| "Missing right input")?.id(),
                )
            }

            ds::GateAddConstant => {
                let gate = gen_gate.directive_as_gate_add_constant().unwrap();
                AddConstant(
                    gate.output().ok_or_else(|| "Missing output")?.id(),
                    gate.input().ok_or_else(|| "Missing input")?.id(),
                    Vec::from(gate.constant().ok_or_else(|| "Missing constant")?),
                )
            }

            ds::GateMulConstant => {
                let gate = gen_gate.directive_as_gate_mul_constant().unwrap();
                MulConstant(
                    gate.output().ok_or_else(|| "Missing output")?.id(),
                    gate.input().ok_or_else(|| "Missing input")?.id(),
                    Vec::from(gate.constant().ok_or_else(|| "Missing constant")?),
                )
            }

            ds::GateAnd => {
                let gate = gen_gate.directive_as_gate_and().unwrap();
                And(
                    gate.output().ok_or_else(|| "Missing output")?.id(),
                    gate.left().ok_or_else(|| "Missing left input")?.id(),
                    gate.right().ok_or_else(|| "Missing right input")?.id(),
                )
            }

            ds::GateXor => {
                let gate = gen_gate.directive_as_gate_xor().unwrap();
                Xor(
                    gate.output().ok_or_else(|| "Missing output")?.id(),
                    gate.left().ok_or_else(|| "Missing left input")?.id(),
                    gate.right().ok_or_else(|| "Missing right input")?.id(),
                )
            }

            ds::GateNot => {
                let gate = gen_gate.directive_as_gate_not().unwrap();
                Not(
                    gate.output().ok_or_else(|| "Missing output")?.id(),
                    gate.input().ok_or_else(|| "Missing input")?.id(),
                )
            }

            ds::GateInstance => {
                let gate = gen_gate.directive_as_gate_instance().unwrap();
                Instance(gate.output().ok_or_else(|| "Missing output")?.id())
            }

            ds::GateWitness => {
                let gate = gen_gate.directive_as_gate_witness().unwrap();
                Witness(gate.output().ok_or_else(|| "Missing output")?.id())
            }

            ds::GateFree => {
                let gate = gen_gate.directive_as_gate_free().unwrap();
                Free(
                    gate.first().ok_or_else(|| "Missing first wire")?.id(),
                    gate.last().map(|id| id.id()),
                )
            }

            ds::GateCall => {
                let gate = gen_gate.directive_as_gate_call().unwrap();

                Call(
                    gate.name().ok_or_else(|| "Missing function name.")?.into(),
                    WireList::try_from(gate.output_wires().ok_or_else(|| "Missing outputs")?)?,
                    WireList::try_from(gate.input_wires().ok_or_else(|| "Missing inputs")?)?,
                )
            }

            ds::GateAnonCall => {
                let gate = gen_gate.directive_as_gate_anon_call().unwrap();
                let inner = gate
                    .inner()
                    .ok_or_else(|| "Missing inner AbstractAnonCall")?;

                AnonCall(
                    WireList::try_from(gate.output_wires().ok_or_else(|| "Missing output wires")?)?,
                    WireList::try_from(inner.input_wires().ok_or_else(|| "Missing input wires")?)?,
                    inner.instance_count() as usize,
                    inner.witness_count() as usize,
                    Gate::try_from_vector(inner.subcircuit().ok_or_else(|| "Missing subcircuit")?)?,
                )
            }

            ds::GateSwitch => {
                let gate = gen_gate.directive_as_gate_switch().unwrap();

                let cases =
                    try_from_values_vector(gate.cases().ok_or_else(|| "Missing cases values")?)?;

                Switch(
                    from_id(&gate.condition().ok_or_else(|| "Missing condition wire.")?),
                    WireList::try_from(gate.output_wires().ok_or_else(|| "Missing output wires")?)?,
                    cases,
                    CaseInvoke::try_from_vector(
                        gate.branches().ok_or_else(|| "Missing branches")?,
                    )?,
                )
            }

            ds::GateFor => {
                let gate = gen_gate.directive_as_gate_for().unwrap();
                let output_list =
                    WireList::try_from(gate.outputs().ok_or_else(|| "missing output list")?)?;
                let body: ForLoopBody = match gate.body_type() {
                    g::ForLoopBody::NONE => return Err("Unknown body type".into()),
                    g::ForLoopBody::IterExprFunctionInvoke => {
                        let g_body = gate.body_as_iter_expr_function_invoke().unwrap();
                        ForLoopBody::IterExprCall(
                            g_body
                                .name()
                                .ok_or_else(|| "Missing function in function name")?
                                .to_string(),
                            IterExprList::try_from(
                                g_body.outputs().ok_or_else(|| "missing output list")?,
                            )?,
                            IterExprList::try_from(
                                g_body.inputs().ok_or_else(|| "missing input list")?,
                            )?,
                        )
                    }
                    g::ForLoopBody::IterExprAnonFunction => {
                        let g_body = gate.body_as_iter_expr_anon_function().unwrap();
                        ForLoopBody::IterExprAnonCall(
                            IterExprList::try_from(
                                g_body.outputs().ok_or_else(|| "missing output list")?,
                            )?,
                            IterExprList::try_from(
                                g_body.inputs().ok_or_else(|| "missing input list")?,
                            )?,
                            g_body.instance_count() as usize,
                            g_body.witness_count() as usize,
                            Gate::try_from_vector(g_body.body().ok_or_else(|| "Missing body")?)?,
                        )
                    }
                };

                For(
                    gate.iterator()
                        .ok_or_else(|| "Missing iterator name")?
                        .to_string(),
                    gate.first(),
                    gate.last(),
                    output_list,
                    body,
                )
            }
        })
    }
}

impl Gate {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::Directive<'bldr>> {
        match self {
            Constant(output, constant) => {
                let constant = builder.create_vector(constant);
                let g_output = build_wire(builder, *output);

                let gate = g::GateConstant::create(
                    builder,
                    &g::GateConstantArgs {
                        output: Some(g_output),
                        constant: Some(constant),
                    },
                );
                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateConstant,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            AssertZero(input) => {
                let g_input = build_wire(builder, *input);
                let gate = g::GateAssertZero::create(
                    builder,
                    &g::GateAssertZeroArgs {
                        input: Some(g_input),
                    },
                );
                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateAssertZero,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            Copy(output, input) => {
                let g_input = build_wire(builder, *input);
                let g_output = build_wire(builder, *output);
                let gate = g::GateCopy::create(
                    builder,
                    &g::GateCopyArgs {
                        output: Some(g_output),
                        input: Some(g_input),
                    },
                );
                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateCopy,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            Add(output, left, right) => {
                let g_left = build_wire(builder, *left);
                let g_right = build_wire(builder, *right);
                let g_output = build_wire(builder, *output);
                let gate = g::GateAdd::create(
                    builder,
                    &g::GateAddArgs {
                        output: Some(g_output),
                        left: Some(g_left),
                        right: Some(g_right),
                    },
                );
                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateAdd,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            Mul(output, left, right) => {
                let g_left = build_wire(builder, *left);
                let g_right = build_wire(builder, *right);
                let g_output = build_wire(builder, *output);
                let gate = g::GateMul::create(
                    builder,
                    &g::GateMulArgs {
                        output: Some(g_output),
                        left: Some(g_left),
                        right: Some(g_right),
                    },
                );
                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateMul,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            AddConstant(output, input, constant) => {
                let g_input = build_wire(builder, *input);
                let g_output = build_wire(builder, *output);
                let constant = builder.create_vector(constant);
                let gate = g::GateAddConstant::create(
                    builder,
                    &g::GateAddConstantArgs {
                        output: Some(g_output),
                        input: Some(g_input),
                        constant: Some(constant),
                    },
                );
                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateAddConstant,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            MulConstant(output, input, constant) => {
                let g_input = build_wire(builder, *input);
                let g_output = build_wire(builder, *output);
                let constant = builder.create_vector(constant);
                let gate = g::GateMulConstant::create(
                    builder,
                    &g::GateMulConstantArgs {
                        output: Some(g_output),
                        input: Some(g_input),
                        constant: Some(constant),
                    },
                );
                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateMulConstant,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            And(output, left, right) => {
                let g_left = build_wire(builder, *left);
                let g_right = build_wire(builder, *right);
                let g_output = build_wire(builder, *output);
                let gate = g::GateAnd::create(
                    builder,
                    &g::GateAndArgs {
                        output: Some(g_output),
                        left: Some(g_left),
                        right: Some(g_right),
                    },
                );
                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateAnd,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            Xor(output, left, right) => {
                let g_left = build_wire(builder, *left);
                let g_right = build_wire(builder, *right);
                let g_output = build_wire(builder, *output);
                let gate = g::GateXor::create(
                    builder,
                    &g::GateXorArgs {
                        output: Some(g_output),
                        left: Some(g_left),
                        right: Some(g_right),
                    },
                );
                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateXor,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            Not(output, input) => {
                let g_input = build_wire(builder, *input);
                let g_output = build_wire(builder, *output);
                let gate = g::GateNot::create(
                    builder,
                    &g::GateNotArgs {
                        output: Some(g_output),
                        input: Some(g_input),
                    },
                );
                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateNot,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            Instance(output) => {
                let g_output = build_wire(builder, *output);
                let gate = g::GateInstance::create(
                    builder,
                    &g::GateInstanceArgs {
                        output: Some(g_output),
                    },
                );
                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateInstance,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            Witness(output) => {
                let g_output = build_wire(builder, *output);
                let gate = g::GateWitness::create(
                    builder,
                    &g::GateWitnessArgs {
                        output: Some(g_output),
                    },
                );
                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateWitness,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            Free(first, last) => {
                let g_first = build_wire(builder, *first);
                let g_last = last.map(|id| build_wire(builder, id));
                let gate = g::GateFree::create(
                    builder,
                    &g::GateFreeArgs {
                        first: Some(g_first),
                        last: g_last,
                    },
                );

                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateFree,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            AnonCall(output_wires, input_wires, instance_count, witness_count, subcircuit) => {
                let g_outputs = build_wire_list(builder, output_wires);
                let g_inputs = build_wire_list(builder, input_wires);
                let g_subcircuit = Gate::build_vector(builder, subcircuit);

                let g_inner = g::AbstractAnonCall::create(
                    builder,
                    &g::AbstractAnonCallArgs {
                        input_wires: Some(g_inputs),
                        instance_count: *instance_count as u64,
                        witness_count: *witness_count as u64,
                        subcircuit: Some(g_subcircuit),
                    },
                );

                let g_gate = g::GateAnonCall::create(
                    builder,
                    &g::GateAnonCallArgs {
                        output_wires: Some(g_outputs),
                        inner: Some(g_inner),
                    },
                );

                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateAnonCall,
                        directive: Some(g_gate.as_union_value()),
                    },
                )
            }

            Call(name, output_wires, input_wires) => {
                let g_name = builder.create_string(name);
                let g_outputs = build_wire_list(builder, output_wires);
                let g_inputs = build_wire_list(builder, input_wires);

                let g_gate = g::GateCall::create(
                    builder,
                    &g::GateCallArgs {
                        name: Some(g_name),
                        output_wires: Some(g_outputs),
                        input_wires: Some(g_inputs),
                    },
                );

                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateCall,
                        directive: Some(g_gate.as_union_value()),
                    },
                )
            }

            Switch(condition, outputs_list, cases, branches) => {
                let g_condition = build_wire(builder, *condition);
                let g_output_wires = build_wire_list(builder, outputs_list);
                let g_cases = build_values_vector(builder, cases);
                let g_branches = CaseInvoke::build_vector(builder, branches);

                let gate = g::GateSwitch::create(
                    builder,
                    &g::GateSwitchArgs {
                        condition: Some(g_condition),
                        output_wires: Some(g_output_wires),
                        cases: Some(g_cases),
                        branches: Some(g_branches),
                    },
                );

                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateSwitch,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            For(iterator_name, start_val, end_val, global_output_list, body) => {
                let g_iterator_name = builder.create_string(iterator_name);
                let g_global_output_list = build_wire_list(builder, global_output_list);

                let gate = match body {
                    ForLoopBody::IterExprCall(name, output_wires, input_wires) => {
                        let g_name = builder.create_string(name);
                        let g_output_wires = build_iterexpr_list(builder, output_wires);
                        let g_input_wires = build_iterexpr_list(builder, input_wires);

                        let g_body = g::IterExprFunctionInvoke::create(
                            builder,
                            &g::IterExprFunctionInvokeArgs {
                                name: Some(g_name),
                                outputs: Some(g_output_wires),
                                inputs: Some(g_input_wires),
                            },
                        );

                        g::GateFor::create(
                            builder,
                            &g::GateForArgs {
                                outputs: Some(g_global_output_list),
                                iterator: Some(g_iterator_name),
                                first: *start_val,
                                last: *end_val,
                                body_type: g::ForLoopBody::IterExprFunctionInvoke,
                                body: Some(g_body.as_union_value()),
                            },
                        )
                    }
                    ForLoopBody::IterExprAnonCall(
                        output_wires,
                        input_wires,
                        instance_count,
                        witness_count,
                        subcircuit,
                    ) => {
                        let g_subcircuit = Gate::build_vector(builder, subcircuit);
                        let g_output_wires = build_iterexpr_list(builder, output_wires);
                        let g_input_wires = build_iterexpr_list(builder, input_wires);

                        let g_body = g::IterExprAnonFunction::create(
                            builder,
                            &g::IterExprAnonFunctionArgs {
                                outputs: Some(g_output_wires),
                                inputs: Some(g_input_wires),
                                instance_count: *instance_count as u64,
                                witness_count: *witness_count as u64,
                                body: Some(g_subcircuit),
                            },
                        );

                        g::GateFor::create(
                            builder,
                            &g::GateForArgs {
                                outputs: Some(g_global_output_list),
                                iterator: Some(g_iterator_name),
                                first: *start_val,
                                last: *end_val,
                                body_type: g::ForLoopBody::IterExprAnonFunction,
                                body: Some(g_body.as_union_value()),
                            },
                        )
                    }
                };

                g::Directive::create(
                    builder,
                    &g::DirectiveArgs {
                        directive_type: ds::GateFor,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }
        }
    }

    /// Convert from a Flatbuffers vector of gates to owned structures.
    pub fn try_from_vector<'a>(
        g_vector: Vector<'a, ForwardsUOffset<g::Directive<'a>>>,
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
    ) -> WIPOffset<Vector<'bldr, ForwardsUOffset<g::Directive<'bldr>>>> {
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

            AnonCall(_, _, _, _, _) => unimplemented!("AnonCall gate"),
            Call(_, _, _) => unimplemented!("Call gate"),
            Switch(_, _, _, _) => unimplemented!("Switch gate"),
            For(_, _, _, _, _) => unimplemented!("For loop"),
        }
    }
}
