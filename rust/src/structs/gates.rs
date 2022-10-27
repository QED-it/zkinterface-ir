use crate::Result;
use flatbuffers::{FlatBufferBuilder, ForwardsUOffset, Vector, WIPOffset};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;

use super::wire::WireList;
use super::wire::{build_field_id, build_wire_id, build_wire_list};
use crate::sieve_ir_generated::sieve_ir as generated;
use crate::sieve_ir_generated::sieve_ir::DirectiveSet as ds;
use crate::structs::count::{build_count_list, CountList};
use crate::structs::wire::{expand_wirelist, replace_wire_id, replace_wire_in_wirelist};
use crate::{FieldId, Value, WireId};

/// This one correspond to Directive in the FlatBuffers schema
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum Gate {
    /// Constant(field_id, output, constant)
    Constant(FieldId, WireId, Value),
    /// AssertZero(field_id, input)
    AssertZero(FieldId, WireId),
    /// Copy(field_id, output, input)
    Copy(FieldId, WireId, WireId),
    /// Add(field_id, output, input, input)
    Add(FieldId, WireId, WireId, WireId),
    /// Mul(field_id, output, input, input)
    Mul(FieldId, WireId, WireId, WireId),
    /// AddConstant(field_id, output, input, constant)
    AddConstant(FieldId, WireId, WireId, Value),
    /// MulConstant(field_id, output, input, constant)
    MulConstant(FieldId, WireId, WireId, Value),
    /// PublicInput(field_id, output)
    PublicInput(FieldId, WireId),
    /// PrivateInput(field_id, output)
    PrivateInput(FieldId, WireId),
    /// Delete(field_id, first, last)
    /// If the option is not given, then only the first wire is deleted, otherwise all wires between
    /// the first and the last INCLUSIVE are deleted.
    Delete(FieldId, WireId, Option<WireId>),
    /// Convert(output, input)
    Convert(WireList, WireList),
    /// AnonCall(output_wires, input_wires, public_count, private_count, subcircuit)
    AnonCall(WireList, WireList, CountList, CountList, Vec<Gate>),
    /// GateCall(name, output_wires, input_wires)
    Call(String, WireList, WireList),
}

use Gate::*;

impl<'a> TryFrom<generated::Directive<'a>> for Gate {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(gen_gate: generated::Directive) -> Result<Gate> {
        Ok(match gen_gate.directive_type() {
            ds::NONE => return Err("No gate type".into()),

            ds::GateConstant => {
                let gate = gen_gate.directive_as_gate_constant().unwrap();
                Constant(
                    gate.field_id().ok_or("Missing field id")?.id(),
                    gate.output().ok_or("Missing output")?.id(),
                    Vec::from(gate.constant().ok_or("Missing constant")?),
                )
            }

            ds::GateAssertZero => {
                let gate = gen_gate.directive_as_gate_assert_zero().unwrap();
                AssertZero(
                    gate.field_id().ok_or("Missing field id")?.id(),
                    gate.input().ok_or("Missing input")?.id(),
                )
            }

            ds::GateCopy => {
                let gate = gen_gate.directive_as_gate_copy().unwrap();
                Copy(
                    gate.field_id().ok_or("Missing field id")?.id(),
                    gate.output().ok_or("Missing output")?.id(),
                    gate.input().ok_or("Missing input")?.id(),
                )
            }

            ds::GateAdd => {
                let gate = gen_gate.directive_as_gate_add().unwrap();
                Add(
                    gate.field_id().ok_or("Missing field id")?.id(),
                    gate.output().ok_or("Missing output")?.id(),
                    gate.left().ok_or("Missing left input")?.id(),
                    gate.right().ok_or("Missing right input")?.id(),
                )
            }

            ds::GateMul => {
                let gate = gen_gate.directive_as_gate_mul().unwrap();
                Mul(
                    gate.field_id().ok_or("Missing field id")?.id(),
                    gate.output().ok_or("Missing output")?.id(),
                    gate.left().ok_or("Missing left input")?.id(),
                    gate.right().ok_or("Missing right input")?.id(),
                )
            }

            ds::GateAddConstant => {
                let gate = gen_gate.directive_as_gate_add_constant().unwrap();
                AddConstant(
                    gate.field_id().ok_or("Missing field id")?.id(),
                    gate.output().ok_or("Missing output")?.id(),
                    gate.input().ok_or("Missing input")?.id(),
                    Vec::from(gate.constant().ok_or("Missing constant")?),
                )
            }

            ds::GateMulConstant => {
                let gate = gen_gate.directive_as_gate_mul_constant().unwrap();
                MulConstant(
                    gate.field_id().ok_or("Missing field id")?.id(),
                    gate.output().ok_or("Missing output")?.id(),
                    gate.input().ok_or("Missing input")?.id(),
                    Vec::from(gate.constant().ok_or("Missing constant")?),
                )
            }

            ds::GatePublicInput => {
                let gate = gen_gate.directive_as_gate_public_input().unwrap();
                PublicInput(
                    gate.field_id().ok_or("Missing field id")?.id(),
                    gate.output().ok_or("Missing output")?.id(),
                )
            }

            ds::GatePrivateInput => {
                let gate = gen_gate.directive_as_gate_private_input().unwrap();
                PrivateInput(
                    gate.field_id().ok_or("Missing field id")?.id(),
                    gate.output().ok_or("Missing output")?.id(),
                )
            }

            ds::GateDelete => {
                let gate = gen_gate.directive_as_gate_delete().unwrap();
                Delete(
                    gate.field_id().ok_or("Missing field id")?.id(),
                    gate.first().ok_or("Missing first wire")?.id(),
                    gate.last().map(|id| id.id()),
                )
            }

            ds::GateConvert => {
                let gate = gen_gate.directive_as_gate_convert().unwrap();
                Convert(
                    WireList::try_from(gate.output().ok_or("Missing outputs")?)?,
                    WireList::try_from(gate.input().ok_or("Missing inputs")?)?,
                )
            }

            ds::GateCall => {
                let gate = gen_gate.directive_as_gate_call().unwrap();

                Call(
                    gate.name().ok_or("Missing function name.")?.into(),
                    WireList::try_from(gate.output_wires().ok_or("Missing outputs")?)?,
                    WireList::try_from(gate.input_wires().ok_or("Missing inputs")?)?,
                )
            }

            ds::GateAnonCall => {
                let gate = gen_gate.directive_as_gate_anon_call().unwrap();
                let inner = gate.inner().ok_or("Missing inner AbstractAnonCall")?;

                AnonCall(
                    WireList::try_from(gate.output_wires().ok_or("Missing output wires")?)?,
                    WireList::try_from(inner.input_wires().ok_or("Missing input wires")?)?,
                    CountList::try_from(inner.public_count().ok_or("Missing public count")?)?,
                    CountList::try_from(inner.private_count().ok_or("Missing private count")?)?,
                    Gate::try_from_vector(inner.subcircuit().ok_or("Missing subcircuit")?)?,
                )
            }
        })
    }
}

impl Gate {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr>(
        &self,
        builder: &mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<generated::Directive<'bldr>> {
        match self {
            Constant(field_id, output, constant) => {
                let g_field_id = build_field_id(builder, *field_id);
                let g_constant = builder.create_vector(constant);
                let g_output = build_wire_id(builder, *output);

                let gate = generated::GateConstant::create(
                    builder,
                    &generated::GateConstantArgs {
                        field_id: Some(g_field_id),
                        output: Some(g_output),
                        constant: Some(g_constant),
                    },
                );
                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: ds::GateConstant,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            AssertZero(field_id, input) => {
                let g_field_id = build_field_id(builder, *field_id);
                let g_input = build_wire_id(builder, *input);
                let gate = generated::GateAssertZero::create(
                    builder,
                    &generated::GateAssertZeroArgs {
                        field_id: Some(g_field_id),
                        input: Some(g_input),
                    },
                );
                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: ds::GateAssertZero,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            Copy(field_id, output, input) => {
                let g_field_id = build_field_id(builder, *field_id);
                let g_input = build_wire_id(builder, *input);
                let g_output = build_wire_id(builder, *output);
                let gate = generated::GateCopy::create(
                    builder,
                    &generated::GateCopyArgs {
                        field_id: Some(g_field_id),
                        output: Some(g_output),
                        input: Some(g_input),
                    },
                );
                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: ds::GateCopy,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            Add(field_id, output, left, right) => {
                let g_field_id = build_field_id(builder, *field_id);
                let g_left = build_wire_id(builder, *left);
                let g_right = build_wire_id(builder, *right);
                let g_output = build_wire_id(builder, *output);
                let gate = generated::GateAdd::create(
                    builder,
                    &generated::GateAddArgs {
                        field_id: Some(g_field_id),
                        output: Some(g_output),
                        left: Some(g_left),
                        right: Some(g_right),
                    },
                );
                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: ds::GateAdd,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            Mul(field_id, output, left, right) => {
                let g_field_id = build_field_id(builder, *field_id);
                let g_left = build_wire_id(builder, *left);
                let g_right = build_wire_id(builder, *right);
                let g_output = build_wire_id(builder, *output);
                let gate = generated::GateMul::create(
                    builder,
                    &generated::GateMulArgs {
                        field_id: Some(g_field_id),
                        output: Some(g_output),
                        left: Some(g_left),
                        right: Some(g_right),
                    },
                );
                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: ds::GateMul,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            AddConstant(field_id, output, input, constant) => {
                let g_field_id = build_field_id(builder, *field_id);
                let g_input = build_wire_id(builder, *input);
                let g_output = build_wire_id(builder, *output);
                let constant = builder.create_vector(constant);
                let gate = generated::GateAddConstant::create(
                    builder,
                    &generated::GateAddConstantArgs {
                        field_id: Some(g_field_id),
                        output: Some(g_output),
                        input: Some(g_input),
                        constant: Some(constant),
                    },
                );
                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: ds::GateAddConstant,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            MulConstant(field_id, output, input, constant) => {
                let g_field_id = build_field_id(builder, *field_id);
                let g_input = build_wire_id(builder, *input);
                let g_output = build_wire_id(builder, *output);
                let constant = builder.create_vector(constant);
                let gate = generated::GateMulConstant::create(
                    builder,
                    &generated::GateMulConstantArgs {
                        field_id: Some(g_field_id),
                        output: Some(g_output),
                        input: Some(g_input),
                        constant: Some(constant),
                    },
                );
                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: ds::GateMulConstant,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            PublicInput(field_id, output) => {
                let g_field_id = build_field_id(builder, *field_id);
                let g_output = build_wire_id(builder, *output);
                let gate = generated::GatePublicInput::create(
                    builder,
                    &generated::GatePublicInputArgs {
                        field_id: Some(g_field_id),
                        output: Some(g_output),
                    },
                );
                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: ds::GatePublicInput,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            PrivateInput(field_id, output) => {
                let g_field_id = build_field_id(builder, *field_id);
                let g_output = build_wire_id(builder, *output);
                let gate = generated::GatePrivateInput::create(
                    builder,
                    &generated::GatePrivateInputArgs {
                        field_id: Some(g_field_id),
                        output: Some(g_output),
                    },
                );
                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: ds::GatePrivateInput,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            Delete(field_id, first, last) => {
                let g_field_id = build_field_id(builder, *field_id);
                let g_first = build_wire_id(builder, *first);
                let g_last = last.map(|id| build_wire_id(builder, id));
                let gate = generated::GateDelete::create(
                    builder,
                    &generated::GateDeleteArgs {
                        field_id: Some(g_field_id),
                        first: Some(g_first),
                        last: g_last,
                    },
                );

                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: ds::GateDelete,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            Convert(output, input) => {
                let g_output = build_wire_list(builder, output);
                let g_input = build_wire_list(builder, input);
                let gate = generated::GateConvert::create(
                    builder,
                    &generated::GateConvertArgs {
                        output: Some(g_output),
                        input: Some(g_input),
                    },
                );

                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: ds::GateConvert,
                        directive: Some(gate.as_union_value()),
                    },
                )
            }

            AnonCall(output_wires, input_wires, public_count, private_count, subcircuit) => {
                let g_outputs = build_wire_list(builder, output_wires);
                let g_inputs = build_wire_list(builder, input_wires);
                let g_subcircuit = Gate::build_vector(builder, subcircuit);
                let g_public_count = build_count_list(builder, public_count);
                let g_private_count = build_count_list(builder, private_count);

                let g_inner = generated::AbstractAnonCall::create(
                    builder,
                    &generated::AbstractAnonCallArgs {
                        input_wires: Some(g_inputs),
                        public_count: Some(g_public_count),
                        private_count: Some(g_private_count),
                        subcircuit: Some(g_subcircuit),
                    },
                );

                let g_gate = generated::GateAnonCall::create(
                    builder,
                    &generated::GateAnonCallArgs {
                        output_wires: Some(g_outputs),
                        inner: Some(g_inner),
                    },
                );

                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: ds::GateAnonCall,
                        directive: Some(g_gate.as_union_value()),
                    },
                )
            }

            Call(name, output_wires, input_wires) => {
                let g_name = builder.create_string(name);
                let g_outputs = build_wire_list(builder, output_wires);
                let g_inputs = build_wire_list(builder, input_wires);

                let g_gate = generated::GateCall::create(
                    builder,
                    &generated::GateCallArgs {
                        name: Some(g_name),
                        output_wires: Some(g_outputs),
                        input_wires: Some(g_inputs),
                    },
                );

                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: ds::GateCall,
                        directive: Some(g_gate.as_union_value()),
                    },
                )
            }
        }
    }

    /// Convert from a Flatbuffers vector of gates to owned structures.
    pub fn try_from_vector<'a>(
        g_vector: Vector<'a, ForwardsUOffset<generated::Directive<'a>>>,
    ) -> Result<Vec<Gate>> {
        let mut gates = vec![];
        for i in 0..g_vector.len() {
            let g_a = g_vector.get(i);
            gates.push(Gate::try_from(g_a)?);
        }
        Ok(gates)
    }

    /// Add a vector of this structure into a Flatbuffers message builder.
    pub fn build_vector<'bldr>(
        builder: &mut FlatBufferBuilder<'bldr>,
        gates: &[Gate],
    ) -> WIPOffset<Vector<'bldr, ForwardsUOffset<generated::Directive<'bldr>>>> {
        let g_gates: Vec<_> = gates.iter().map(|gate| gate.build(builder)).collect();
        builder.create_vector(&g_gates)
    }

    /// Returns the output wire id if exists.
    /// if not, returns None
    fn _get_output_wire_id(&self) -> Option<WireId> {
        match *self {
            Constant(_, w, _) => Some(w),
            Copy(_, w, _) => Some(w),
            Add(_, w, _, _) => Some(w),
            Mul(_, w, _, _) => Some(w),
            AddConstant(_, w, _, _) => Some(w),
            MulConstant(_, w, _, _) => Some(w),
            PublicInput(_, w) => Some(w),
            PrivateInput(_, w) => Some(w),

            AssertZero(_, _) => None,
            Delete(_, _, _) => None,

            Convert(_, _) => unimplemented!("Convert gate"),
            AnonCall(_, _, _, _, _) => unimplemented!("AnonCall gate"),
            Call(_, _, _) => unimplemented!("Call gate"),
        }
    }
}

/// replace_output_wires goes through all gates in `gates` and `replace output_wires[i]` by `i`.
///
/// If a `Delete` gate contains an output wire, `replace_output_wires` will return an error.
pub fn replace_output_wires(gates: &mut Vec<Gate>, output_wires: &WireList) -> Result<()> {
    let expanded_output_wires = expand_wirelist(output_wires)?;
    let mut map: HashMap<FieldId, WireId> = HashMap::new();

    // gates does not have a For gate.
    for (old_field_id, old_wire) in expanded_output_wires {
        let count = map.entry(old_field_id).or_insert(0);
        let new_wire = *count;
        *count += 1;
        for gate in &mut *gates {
            match gate {
                Constant(ref field_id, ref mut output, _) => {
                    replace_wire_id(field_id, &old_field_id, output, old_wire, new_wire);
                }
                Copy(ref field_id, ref mut output, ref mut input) => {
                    replace_wire_id(field_id, &old_field_id, output, old_wire, new_wire);
                    replace_wire_id(field_id, &old_field_id, input, old_wire, new_wire);
                }
                Add(ref field_id, ref mut output, ref mut left, ref mut right) => {
                    replace_wire_id(field_id, &old_field_id, output, old_wire, new_wire);
                    replace_wire_id(field_id, &old_field_id, left, old_wire, new_wire);
                    replace_wire_id(field_id, &old_field_id, right, old_wire, new_wire);
                }
                Mul(ref field_id, ref mut output, ref mut left, ref mut right) => {
                    replace_wire_id(field_id, &old_field_id, output, old_wire, new_wire);
                    replace_wire_id(field_id, &old_field_id, left, old_wire, new_wire);
                    replace_wire_id(field_id, &old_field_id, right, old_wire, new_wire);
                }
                AddConstant(ref field_id, ref mut output, ref mut input, _) => {
                    replace_wire_id(field_id, &old_field_id, output, old_wire, new_wire);
                    replace_wire_id(field_id, &old_field_id, input, old_wire, new_wire);
                }
                MulConstant(ref field_id, ref mut output, ref mut input, _) => {
                    replace_wire_id(field_id, &old_field_id, output, old_wire, new_wire);
                    replace_wire_id(field_id, &old_field_id, input, old_wire, new_wire);
                }
                PublicInput(ref field_id, ref mut output) => {
                    replace_wire_id(field_id, &old_field_id, output, old_wire, new_wire);
                }
                PrivateInput(ref field_id, ref mut output) => {
                    replace_wire_id(field_id, &old_field_id, output, old_wire, new_wire);
                }
                AssertZero(ref field_id, ref mut wire) => {
                    replace_wire_id(field_id, &old_field_id, wire, old_wire, new_wire);
                }
                Delete(ref field_id, ref mut first, ref mut option_last) => match option_last {
                    Some(last) => {
                        if (*first <= old_wire && *last >= old_wire) && (*field_id == old_field_id)
                        {
                            return Err("It is forbidden to delete an output wire !".into());
                        }
                    }
                    None => {
                        if (*first == old_wire) && (*field_id == old_field_id) {
                            return Err("It is forbidden to delete an output wire !".into());
                        }
                    }
                },
                Convert(ref mut output, ref mut input) => {
                    replace_wire_in_wirelist(output, old_field_id, old_wire, new_wire)?;
                    replace_wire_in_wirelist(input, old_field_id, old_wire, new_wire)?;
                }
                AnonCall(ref mut outputs, ref mut inputs, _, _, _) => {
                    replace_wire_in_wirelist(outputs, old_field_id, old_wire, new_wire)?;
                    replace_wire_in_wirelist(inputs, old_field_id, old_wire, new_wire)?;
                }
                Call(_, ref mut outputs, ref mut inputs) => {
                    replace_wire_in_wirelist(outputs, old_field_id, old_wire, new_wire)?;
                    replace_wire_in_wirelist(inputs, old_field_id, old_wire, new_wire)?;
                }
            }
        }
    }
    Ok(())
}

#[test]
fn test_replace_output_wires() {
    use crate::structs::wire::WireListElement::*;

    let mut gates = vec![
        PublicInput(0, 4),
        PrivateInput(0, 5),
        Constant(0, 6, vec![15]),
        PublicInput(1, 6),
        Add(0, 7, 4, 5),
        Delete(0, 4, Some(5)),
        Mul(0, 8, 6, 7),
        Call(
            "custom".to_string(),
            vec![WireRange(0, 9, 12)],
            vec![WireRange(0, 6, 8)],
        ),
        AssertZero(0, 12),
    ];
    let output_wires = vec![Wire(0, 6), WireRange(0, 11, 12), Wire(0, 15)];
    replace_output_wires(&mut gates, &output_wires).unwrap();
    let correct_gates = vec![
        PublicInput(0, 4),
        PrivateInput(0, 5),
        Constant(0, 0, vec![15]),
        PublicInput(1, 6),
        Add(0, 7, 4, 5),
        Delete(0, 4, Some(5)),
        Mul(0, 8, 0, 7),
        Call(
            "custom".to_string(),
            vec![Wire(0, 9), Wire(0, 10), Wire(0, 1), Wire(0, 2)],
            vec![Wire(0, 0), Wire(0, 7), Wire(0, 8)],
        ),
        AssertZero(0, 2),
    ];
    assert_eq!(gates, correct_gates);
}

#[test]
fn test_replace_output_wires_with_forbidden_delete() {
    use crate::structs::wire::WireListElement::*;

    let mut gates = vec![
        Add(0, 2, 4, 6),
        Mul(0, 7, 4, 6),
        Add(0, 8, 3, 5),
        Add(0, 9, 7, 8),
        Mul(0, 10, 3, 5),
        AddConstant(0, 11, 10, vec![1]),
        Delete(0, 7, Some(9)),
    ];
    let output_wires = vec![Wire(0, 8), Wire(0, 4)];
    let test = replace_output_wires(&mut gates, &output_wires);
    assert!(test.is_err());

    let mut gates = vec![
        Add(0, 2, 4, 6),
        Mul(0, 7, 4, 6),
        Delete(0, 4, None),
        Add(0, 8, 3, 5),
        Add(0, 9, 7, 8),
        Mul(0, 10, 3, 5),
        AddConstant(0, 11, 10, vec![1]),
    ];
    let output_wires = vec![Wire(0, 8), Wire(0, 4)];
    let test = replace_output_wires(&mut gates, &output_wires);
    assert!(test.is_err());
}
