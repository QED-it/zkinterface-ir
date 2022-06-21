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
use crate::structs::wire::{replace_wire, replace_wire_in_wirelist};
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

/// replace_output_wires goes through all gates in `gates` and `replace output_wires[i]` by `i` if it is
/// easily doable. Otherwise, replace_output_wires adds Copy gates `Copy(i, output_wires[i])` at the
/// end of `gates`.
///
/// If a `For` gate belongs to `gates`, it is not easily doable to replace output wires (especially
/// in IterExpr). Therefor, `replace_output_wires` will add the Copy gates `Copy(i, output_wires[i])`
/// at the end of `gates`.
///
/// If there is no For gate in `gates`, `replace_output_wires` will replace all `output_wires[i]`
/// by `i` in all gates in `gates`.
///
/// If a `Free` gate contains a output wire, `replace_output_wires` will return an error.
pub fn replace_output_wires(gates: &mut Vec<Gate>, output_wires: &Vec<WireId>) -> Result<()> {
    // It is not easily doable to replace a WireId in a For gate (especially in IterExpr).
    // Therefor, if one gate is a For gate, we will add Copy gates and not modify any WireId.
    for gate in gates.into_iter() {
        if let For(_, _, _, _, _) = gate {
            for i in 0..output_wires.len() {
                gates.push(Gate::Copy(i as u64, output_wires[i]));
            }
            return Ok(());
        }
    }

    // gates does not have a For gate.
    for i in 0..output_wires.len() {
        let old_wire = output_wires[i];
        let new_wire = i as u64;
        for gate in &mut *gates {
            match gate {
                Constant(ref mut output, _) => {
                    replace_wire(output, old_wire, new_wire);
                }
                Copy(ref mut output, ref mut input) => {
                    replace_wire(output, old_wire, new_wire);
                    replace_wire(input, old_wire, new_wire);
                }
                Add(ref mut output, ref mut left, ref mut right) => {
                    replace_wire(output, old_wire, new_wire);
                    replace_wire(left, old_wire, new_wire);
                    replace_wire(right, old_wire, new_wire);
                }
                Mul(ref mut output, ref mut left, ref mut right) => {
                    replace_wire(output, old_wire, new_wire);
                    replace_wire(left, old_wire, new_wire);
                    replace_wire(right, old_wire, new_wire);
                }
                AddConstant(ref mut output, ref mut input, _) => {
                    replace_wire(output, old_wire, new_wire);
                    replace_wire(input, old_wire, new_wire);
                }
                MulConstant(ref mut output, ref mut input, _) => {
                    replace_wire(output, old_wire, new_wire);
                    replace_wire(input, old_wire, new_wire);
                }
                And(ref mut output, ref mut left, ref mut right) => {
                    replace_wire(output, old_wire, new_wire);
                    replace_wire(left, old_wire, new_wire);
                    replace_wire(right, old_wire, new_wire);
                }
                Xor(ref mut output, ref mut left, ref mut right) => {
                    replace_wire(output, old_wire, new_wire);
                    replace_wire(left, old_wire, new_wire);
                    replace_wire(right, old_wire, new_wire);
                }
                Not(ref mut output, ref mut input) => {
                    replace_wire(output, old_wire, new_wire);
                    replace_wire(input, old_wire, new_wire);
                }
                Instance(ref mut output) => {
                    replace_wire(output, old_wire, new_wire);
                }
                Witness(ref mut output) => {
                    replace_wire(output, old_wire, new_wire);
                }
                AssertZero(ref mut wire) => {
                    replace_wire(wire, old_wire, new_wire);
                }
                Free(ref mut first, ref mut option_last) => match option_last {
                    Some(last) => {
                        if *first <= old_wire && *last >= old_wire {
                            return Err(format!("It is forbidden to free an output wire !").into());
                        }
                    }
                    None => {
                        if *first == old_wire {
                            return Err(format!("It is forbidden to free an output wire !").into());
                        }
                    }
                },
                AnonCall(ref mut outputs, ref mut inputs, _, _, _) => {
                    replace_wire_in_wirelist(outputs, old_wire, new_wire)?;
                    replace_wire_in_wirelist(inputs, old_wire, new_wire)?;
                }
                Call(_, ref mut outputs, ref mut inputs) => {
                    replace_wire_in_wirelist(outputs, old_wire, new_wire)?;
                    replace_wire_in_wirelist(inputs, old_wire, new_wire)?;
                }
                Switch(ref mut condition, ref mut outputs, _, ref mut branches) => {
                    if *condition == old_wire {
                        *condition = new_wire;
                    }
                    replace_wire_in_wirelist(outputs, old_wire, new_wire)?;
                    for branch in branches {
                        match *branch {
                            CaseInvoke::AbstractAnonCall(ref mut inputs, _, _, _) => {
                                replace_wire_in_wirelist(inputs, old_wire, new_wire)?;
                            }
                            CaseInvoke::AbstractGateCall(_, ref mut inputs) => {
                                replace_wire_in_wirelist(inputs, old_wire, new_wire)?;
                            }
                        };
                    }
                }
                For(_, _, _, _, _) => {
                    // At the beginning of this method, we check if there is at least one For gate.
                    // If it is the case, we add Copy gates and return
                    // Therefor, this case is unreachable !!!
                    panic!("Unreachable case in replace_output_wires method.")
                }
            }
        }
    }
    Ok(())
}

#[test]
fn test_replace_output_wires() {
    use crate::structs::function::CaseInvoke;
    use crate::structs::wire::WireListElement::*;

    let mut gates = vec![
        Instance(4),
        Witness(5),
        Constant(6, vec![15]),
        Add(7, 4, 5),
        Free(4, Some(5)),
        Mul(8, 6, 7),
        Call(
            "custom".to_string(),
            vec![WireRange(9, 12)],
            vec![WireRange(6, 8)],
        ),
        AssertZero(12),
        Switch(
            6,
            vec![Wire(13), Wire(14), Wire(15)],
            vec![vec![2], vec![5]],
            vec![
                CaseInvoke::AbstractGateCall("function_branch0".to_string(), vec![WireRange(6, 8)]),
                CaseInvoke::AbstractGateCall("function_branch1".to_string(), vec![Wire(10)]),
            ],
        ),
    ];
    let output_wires = vec![6, 11, 12, 15];
    replace_output_wires(&mut gates, &output_wires).unwrap();
    let correct_gates = vec![
        Instance(4),
        Witness(5),
        Constant(0, vec![15]),
        Add(7, 4, 5),
        Free(4, Some(5)),
        Mul(8, 0, 7),
        Call(
            "custom".to_string(),
            vec![Wire(9), Wire(10), Wire(1), Wire(2)],
            vec![Wire(0), Wire(7), Wire(8)],
        ),
        AssertZero(2),
        Switch(
            0,
            vec![Wire(13), Wire(14), Wire(3)],
            vec![vec![2], vec![5]],
            vec![
                CaseInvoke::AbstractGateCall(
                    "function_branch0".to_string(),
                    vec![Wire(0), Wire(7), Wire(8)],
                ),
                CaseInvoke::AbstractGateCall("function_branch1".to_string(), vec![Wire(10)]),
            ],
        ),
    ];
    assert_eq!(gates, correct_gates);
}

#[test]
fn test_replace_output_wires_with_for() {
    use crate::structs::iterators::{IterExprListElement::*, IterExprWireNumber::*};
    use crate::structs::wire::WireListElement::*;

    let mut gates = vec![
        For(
            "i".into(),
            10,
            12,
            vec![WireRange(10, 12)],
            ForLoopBody::IterExprAnonCall(
                vec![Single(IterExprName("i".into()))],
                vec![],
                0,
                1,
                vec![Witness(0)],
            ),
        ),
        Xor(13, 10, 11),
        AssertZero(13),
    ];
    let output_wires = vec![10, 11, 12, 13];
    replace_output_wires(&mut gates, &output_wires).unwrap();
    let correct_gates = vec![
        For(
            "i".into(),
            10,
            12,
            vec![WireRange(10, 12)],
            ForLoopBody::IterExprAnonCall(
                vec![Single(IterExprName("i".into()))],
                vec![],
                0,
                1,
                vec![Witness(0)],
            ),
        ),
        Xor(13, 10, 11),
        AssertZero(13),
        Copy(0, 10),
        Copy(1, 11),
        Copy(2, 12),
        Copy(3, 13),
    ];
    assert_eq!(gates, correct_gates);
}

#[test]
fn test_replace_output_wires_with_forbidden_free() {
    let mut gates = vec![
        Xor(2, 4, 6),
        And(7, 4, 6),
        Xor(8, 3, 5),
        Xor(9, 7, 8),
        And(10, 3, 5),
        Not(11, 10),
        Free(7, Some(9)),
    ];
    let output_wires = vec![8, 4];
    let test = replace_output_wires(&mut gates, &output_wires);
    assert!(test.is_err());

    let mut gates = vec![
        Xor(2, 4, 6),
        And(7, 4, 6),
        Free(4, None),
        Xor(8, 3, 5),
        Xor(9, 7, 8),
        And(10, 3, 5),
        Not(11, 10),
    ];
    let output_wires = vec![8, 4];
    let test = replace_output_wires(&mut gates, &output_wires);
    assert!(test.is_err());
}
