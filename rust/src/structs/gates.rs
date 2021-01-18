use std::error::Error;
use std::convert::TryFrom;
use crate::Result;
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use serde::{Deserialize, Serialize};

use crate::sieve_ir_generated::sieve_ir as g;
use crate::sieve_ir_generated::sieve_ir::GateSet as gs;
use super::{WireId, Value};


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
    /// Free(begin, end)
    /// If the option is not given, then only the first wire is freed, otherwise all wires between
    /// the first and the last INCLUSIVE are freed.
    Free(WireId, Option<WireId>),
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
                    Vec::from(gate.constant().ok_or("Missing constant")?))
            }

            gs::GateAssertZero => {
                let gate = gen_gate.gate_as_gate_assert_zero().unwrap();
                AssertZero(
                    gate.input().ok_or("Missing input")?.id())
            }

            gs::GateCopy => {
                let gate = gen_gate.gate_as_gate_copy().unwrap();
                Copy(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.input().ok_or("Missing input")?.id())
            }

            gs::GateAdd => {
                let gate = gen_gate.gate_as_gate_add().unwrap();
                Add(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.left().ok_or("Missing left input")?.id(),
                    gate.right().ok_or("Missing right input")?.id())
            }

            gs::GateMul => {
                let gate = gen_gate.gate_as_gate_mul().unwrap();
                Mul(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.left().ok_or("Missing left input")?.id(),
                    gate.right().ok_or("Missing right input")?.id())
            }

            gs::GateAddConstant => {
                let gate = gen_gate.gate_as_gate_add_constant().unwrap();
                AddConstant(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.input().ok_or("Missing input")?.id(),
                    Vec::from(gate.constant().ok_or("Missing constant")?))
            }

            gs::GateMulConstant => {
                let gate = gen_gate.gate_as_gate_mul_constant().unwrap();
                MulConstant(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.input().ok_or("Missing input")?.id(),
                    Vec::from(gate.constant().ok_or("Missing constant")?))
            }

            gs::GateAnd => {
                let gate = gen_gate.gate_as_gate_and().unwrap();
                And(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.left().ok_or("Missing left input")?.id(),
                    gate.right().ok_or("Missing right input")?.id())
            }

            gs::GateXor => {
                let gate = gen_gate.gate_as_gate_xor().unwrap();
                Xor(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.left().ok_or("Missing left input")?.id(),
                    gate.right().ok_or("Missing right input")?.id())
            }

            gs::GateNot => {
                let gate = gen_gate.gate_as_gate_not().unwrap();
                Not(
                    gate.output().ok_or("Missing output")?.id(),
                    gate.input().ok_or("Missing input")?.id())
            }

            gs::GateFree => {
                let gate = gen_gate.gate_as_gate_free().unwrap();
                Free(
                    gate.first().ok_or("Missing first wire")?.id(),
                    gate.last().map(|id| id.id())
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
    ) -> WIPOffset<g::Gate<'bldr>>
    {
        match self {
            Constant(output, constant) => {
                let constant = builder.create_vector(constant);
                let gate = g::GateConstant::create(builder, &g::GateConstantArgs {
                    output: Some(&g::Wire::new(*output)),
                    constant: Some(constant),
                });
                g::Gate::create(builder, &g::GateArgs {
                    gate_type: gs::GateConstant,
                    gate: Some(gate.as_union_value()),
                })
            }

            AssertZero(input) => {
                let gate = g::GateAssertZero::create(builder, &g::GateAssertZeroArgs {
                    input: Some(&g::Wire::new(*input)),
                });
                g::Gate::create(builder, &g::GateArgs {
                    gate_type: gs::GateAssertZero,
                    gate: Some(gate.as_union_value()),
                })
            }

            Copy(output, input) => {
                let gate = g::GateCopy::create(builder, &g::GateCopyArgs {
                    output: Some(&g::Wire::new(*output)),
                    input: Some(&g::Wire::new(*input)),
                });
                g::Gate::create(builder, &g::GateArgs {
                    gate_type: gs::GateCopy,
                    gate: Some(gate.as_union_value()),
                })
            }

            Add(output, left, right) => {
                let gate = g::GateAdd::create(builder, &g::GateAddArgs {
                    output: Some(&g::Wire::new(*output)),
                    left: Some(&g::Wire::new(*left)),
                    right: Some(&g::Wire::new(*right)),
                });
                g::Gate::create(builder, &g::GateArgs {
                    gate_type: gs::GateAdd,
                    gate: Some(gate.as_union_value()),
                })
            }

            Mul(output, left, right) => {
                let gate = g::GateMul::create(builder, &g::GateMulArgs {
                    output: Some(&g::Wire::new(*output)),
                    left: Some(&g::Wire::new(*left)),
                    right: Some(&g::Wire::new(*right)),
                });
                g::Gate::create(builder, &g::GateArgs {
                    gate_type: gs::GateMul,
                    gate: Some(gate.as_union_value()),
                })
            }

            AddConstant(output, input, constant) => {
                let constant = builder.create_vector(constant);
                let gate = g::GateAddConstant::create(builder, &g::GateAddConstantArgs {
                    output: Some(&g::Wire::new(*output)),
                    input: Some(&g::Wire::new(*input)),
                    constant: Some(constant),
                });
                g::Gate::create(builder, &g::GateArgs {
                    gate_type: gs::GateAddConstant,
                    gate: Some(gate.as_union_value()),
                })
            }

            MulConstant(output, input, constant) => {
                let constant = builder.create_vector(constant);
                let gate = g::GateMulConstant::create(builder, &g::GateMulConstantArgs {
                    output: Some(&g::Wire::new(*output)),
                    input: Some(&g::Wire::new(*input)),
                    constant: Some(constant),
                });
                g::Gate::create(builder, &g::GateArgs {
                    gate_type: gs::GateMulConstant,
                    gate: Some(gate.as_union_value()),
                })
            }

            And(output, left, right) => {
                let gate = g::GateAnd::create(builder, &g::GateAndArgs {
                    output: Some(&g::Wire::new(*output)),
                    left: Some(&g::Wire::new(*left)),
                    right: Some(&g::Wire::new(*right)),
                });
                g::Gate::create(builder, &g::GateArgs {
                    gate_type: gs::GateAnd,
                    gate: Some(gate.as_union_value()),
                })
            }

            Xor(output, left, right) => {
                let gate = g::GateXor::create(builder, &g::GateXorArgs {
                    output: Some(&g::Wire::new(*output)),
                    left: Some(&g::Wire::new(*left)),
                    right: Some(&g::Wire::new(*right)),
                });
                g::Gate::create(builder, &g::GateArgs {
                    gate_type: gs::GateXor,
                    gate: Some(gate.as_union_value()),
                })
            }

            Not(output, input) => {
                let gate = g::GateNot::create(builder, &g::GateNotArgs {
                    output: Some(&g::Wire::new(*output)),
                    input: Some(&g::Wire::new(*input)),
                });
                g::Gate::create(builder, &g::GateArgs {
                    gate_type: gs::GateNot,
                    gate: Some(gate.as_union_value()),
                })
            }

            Free(first, last) => {
                let gate = g::GateFree::create(builder, &g::GateFreeArgs {
                    first: Some(&g::Wire::new(*first)),
                    // last: Some(&g::Wire::new(last.unwrap_or(*first))),
                    last: last.map(|id| g::Wire::new(id)).as_ref()
                });

                g::Gate::create(builder, &g::GateArgs {
                    gate_type: gs::GateFree,
                    gate: Some(gate.as_union_value()),
                })
            }
        }
    }

    /// Returns the output wire id if exists.
    /// if not, returns None
    ///
    /// # Examples
    ///
    /// a simple example
    /// ```
    ///
    ///  use zki::Gate::*;
    ///  let g = Add(0,1,2);
    ///  let wire_id = g.get_output_wire_id();
    ///
    /// ```
    pub fn get_output_wire_id(&self) -> Option<WireId> {
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

            AssertZero(_) => None,
            Free(_, _) => None
        }
    }
}
