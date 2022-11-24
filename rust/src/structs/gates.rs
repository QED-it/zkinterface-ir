use crate::Result;
use flatbuffers::{FlatBufferBuilder, ForwardsUOffset, Vector, WIPOffset};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::error::Error;

use crate::sieve_ir_generated::sieve_ir as generated;
use crate::sieve_ir_generated::sieve_ir::GateSet as gs;
use crate::structs::function::FunctionCounts;
use crate::structs::wirerange::{add_types_to_wire_ranges, WireRange, WireRangeWithType};
use crate::{TypeId, Value, WireId};

/// This one correspond to Gate in the FlatBuffers schema
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum Gate {
    /// Constant(type_id, output, constant)
    Constant(TypeId, WireId, Value),
    /// AssertZero(type_id, input)
    AssertZero(TypeId, WireId),
    /// Copy(type_id, output, input)
    Copy(TypeId, WireId, WireId),
    /// Add(type_id, output, input, input)
    Add(TypeId, WireId, WireId, WireId),
    /// Mul(type_id, output, input, input)
    Mul(TypeId, WireId, WireId, WireId),
    /// AddConstant(type_id, output, input, constant)
    AddConstant(TypeId, WireId, WireId, Value),
    /// MulConstant(type_id, output, input, constant)
    MulConstant(TypeId, WireId, WireId, Value),
    /// PublicInput(type_id, output)
    PublicInput(TypeId, WireId),
    /// PrivateInput(type_id, output)
    PrivateInput(TypeId, WireId),
    /// New(type_id, first, last)
    /// Allocate in a contiguous space all wires between the first and the last INCLUSIVE.
    New(TypeId, WireId, WireId),
    /// Delete(type_id, first, last)
    /// All wires between the first and the last INCLUSIVE are deleted.
    /// Last could be equal to first when we would like to delete only one wire
    Delete(TypeId, WireId, WireId),
    /// Convert(out_type_id, out_first_id, out_last_id, in_type_id, in_first_id, in_last_id)
    Convert(TypeId, WireId, WireId, TypeId, WireId, WireId),
    /// GateCall(name, out_ids, in_ids)
    Call(String, Vec<WireRange>, Vec<WireRange>),
}

use Gate::*;

impl<'a> TryFrom<generated::Gate<'a>> for Gate {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(gen_gate: generated::Gate) -> Result<Gate> {
        Ok(match gen_gate.gate_type() {
            gs::NONE => return Err("No gate type".into()),

            gs::GateConstant => {
                let gate = gen_gate.gate_as_gate_constant().unwrap();
                Constant(
                    gate.type_id(),
                    gate.out_id(),
                    Vec::from(gate.constant().ok_or("Missing constant")?),
                )
            }

            gs::GateAssertZero => {
                let gate = gen_gate.gate_as_gate_assert_zero().unwrap();
                AssertZero(gate.type_id(), gate.in_id())
            }

            gs::GateCopy => {
                let gate = gen_gate.gate_as_gate_copy().unwrap();
                Copy(gate.type_id(), gate.out_id(), gate.in_id())
            }

            gs::GateAdd => {
                let gate = gen_gate.gate_as_gate_add().unwrap();
                Add(
                    gate.type_id(),
                    gate.out_id(),
                    gate.left_id(),
                    gate.right_id(),
                )
            }

            gs::GateMul => {
                let gate = gen_gate.gate_as_gate_mul().unwrap();
                Mul(
                    gate.type_id(),
                    gate.out_id(),
                    gate.left_id(),
                    gate.right_id(),
                )
            }

            gs::GateAddConstant => {
                let gate = gen_gate.gate_as_gate_add_constant().unwrap();
                AddConstant(
                    gate.type_id(),
                    gate.out_id(),
                    gate.in_id(),
                    Vec::from(gate.constant().ok_or("Missing constant")?),
                )
            }

            gs::GateMulConstant => {
                let gate = gen_gate.gate_as_gate_mul_constant().unwrap();
                MulConstant(
                    gate.type_id(),
                    gate.out_id(),
                    gate.in_id(),
                    Vec::from(gate.constant().ok_or("Missing constant")?),
                )
            }

            gs::GatePublicInput => {
                let gate = gen_gate.gate_as_gate_public_input().unwrap();
                PublicInput(gate.type_id(), gate.out_id())
            }

            gs::GatePrivateInput => {
                let gate = gen_gate.gate_as_gate_private_input().unwrap();
                PrivateInput(gate.type_id(), gate.out_id())
            }

            gs::GateNew => {
                let gate = gen_gate.gate_as_gate_new().unwrap();
                New(gate.type_id(), gate.first_id(), gate.last_id())
            }

            gs::GateDelete => {
                let gate = gen_gate.gate_as_gate_delete().unwrap();
                Delete(gate.type_id(), gate.first_id(), gate.last_id())
            }

            gs::GateConvert => {
                let gate = gen_gate.gate_as_gate_convert().unwrap();
                Convert(
                    gate.out_type_id(),
                    gate.out_first_id(),
                    gate.out_last_id(),
                    gate.in_type_id(),
                    gate.in_first_id(),
                    gate.in_last_id(),
                )
            }

            gs::GateCall => {
                let gate = gen_gate.gate_as_gate_call().unwrap();

                Call(
                    gate.name().ok_or("Missing function name.")?.into(),
                    WireRange::try_from_vector(gate.out_ids().ok_or("Missing out ids")?)?,
                    WireRange::try_from_vector(gate.in_ids().ok_or("Missing in ids")?)?,
                )
            }
        })
    }
}

impl Gate {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'a>(&self, builder: &mut FlatBufferBuilder<'a>) -> WIPOffset<generated::Gate<'a>> {
        match self {
            Constant(type_id, output, constant) => {
                let g_constant = builder.create_vector(constant);

                let gate = generated::GateConstant::create(
                    builder,
                    &generated::GateConstantArgs {
                        type_id: *type_id,
                        out_id: *output,
                        constant: Some(g_constant),
                    },
                );
                generated::Gate::create(
                    builder,
                    &generated::GateArgs {
                        gate_type: gs::GateConstant,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            AssertZero(type_id, input) => {
                let gate = generated::GateAssertZero::create(
                    builder,
                    &generated::GateAssertZeroArgs {
                        type_id: *type_id,
                        in_id: *input,
                    },
                );
                generated::Gate::create(
                    builder,
                    &generated::GateArgs {
                        gate_type: gs::GateAssertZero,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Copy(type_id, output, input) => {
                let gate = generated::GateCopy::create(
                    builder,
                    &generated::GateCopyArgs {
                        type_id: *type_id,
                        out_id: *output,
                        in_id: *input,
                    },
                );
                generated::Gate::create(
                    builder,
                    &generated::GateArgs {
                        gate_type: gs::GateCopy,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Add(type_id, output, left, right) => {
                let gate = generated::GateAdd::create(
                    builder,
                    &generated::GateAddArgs {
                        type_id: *type_id,
                        out_id: *output,
                        left_id: *left,
                        right_id: *right,
                    },
                );
                generated::Gate::create(
                    builder,
                    &generated::GateArgs {
                        gate_type: gs::GateAdd,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Mul(type_id, output, left, right) => {
                let gate = generated::GateMul::create(
                    builder,
                    &generated::GateMulArgs {
                        type_id: *type_id,
                        out_id: *output,
                        left_id: *left,
                        right_id: *right,
                    },
                );
                generated::Gate::create(
                    builder,
                    &generated::GateArgs {
                        gate_type: gs::GateMul,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            AddConstant(type_id, output, input, constant) => {
                let constant = builder.create_vector(constant);
                let gate = generated::GateAddConstant::create(
                    builder,
                    &generated::GateAddConstantArgs {
                        type_id: *type_id,
                        out_id: *output,
                        in_id: *input,
                        constant: Some(constant),
                    },
                );
                generated::Gate::create(
                    builder,
                    &generated::GateArgs {
                        gate_type: gs::GateAddConstant,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            MulConstant(type_id, output, input, constant) => {
                let constant = builder.create_vector(constant);
                let gate = generated::GateMulConstant::create(
                    builder,
                    &generated::GateMulConstantArgs {
                        type_id: *type_id,
                        out_id: *output,
                        in_id: *input,
                        constant: Some(constant),
                    },
                );
                generated::Gate::create(
                    builder,
                    &generated::GateArgs {
                        gate_type: gs::GateMulConstant,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            PublicInput(type_id, output) => {
                let gate = generated::GatePublicInput::create(
                    builder,
                    &generated::GatePublicInputArgs {
                        type_id: *type_id,
                        out_id: *output,
                    },
                );
                generated::Gate::create(
                    builder,
                    &generated::GateArgs {
                        gate_type: gs::GatePublicInput,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            PrivateInput(type_id, output) => {
                let gate = generated::GatePrivateInput::create(
                    builder,
                    &generated::GatePrivateInputArgs {
                        type_id: *type_id,
                        out_id: *output,
                    },
                );
                generated::Gate::create(
                    builder,
                    &generated::GateArgs {
                        gate_type: gs::GatePrivateInput,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            New(type_id, first, last) => {
                let gate = generated::GateNew::create(
                    builder,
                    &generated::GateNewArgs {
                        type_id: *type_id,
                        first_id: *first,
                        last_id: *last,
                    },
                );

                generated::Gate::create(
                    builder,
                    &generated::GateArgs {
                        gate_type: gs::GateNew,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Delete(type_id, first, last) => {
                let gate = generated::GateDelete::create(
                    builder,
                    &generated::GateDeleteArgs {
                        type_id: *type_id,
                        first_id: *first,
                        last_id: *last,
                    },
                );

                generated::Gate::create(
                    builder,
                    &generated::GateArgs {
                        gate_type: gs::GateDelete,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Convert(
                out_type_id,
                out_first_id,
                out_last_id,
                in_type_id,
                in_first_id,
                in_last_id,
            ) => {
                let gate = generated::GateConvert::create(
                    builder,
                    &generated::GateConvertArgs {
                        out_type_id: *out_type_id,
                        out_first_id: *out_first_id,
                        out_last_id: *out_last_id,
                        in_type_id: *in_type_id,
                        in_first_id: *in_first_id,
                        in_last_id: *in_last_id,
                    },
                );

                generated::Gate::create(
                    builder,
                    &generated::GateArgs {
                        gate_type: gs::GateConvert,
                        gate: Some(gate.as_union_value()),
                    },
                )
            }

            Call(name, out_ids, in_ids) => {
                let g_name = builder.create_string(name);
                let g_out_ids = WireRange::build_vector(builder, out_ids);
                let g_in_ids = WireRange::build_vector(builder, in_ids);

                let g_gate = generated::GateCall::create(
                    builder,
                    &generated::GateCallArgs {
                        name: Some(g_name),
                        out_ids: Some(g_out_ids),
                        in_ids: Some(g_in_ids),
                    },
                );

                generated::Gate::create(
                    builder,
                    &generated::GateArgs {
                        gate_type: gs::GateCall,
                        gate: Some(g_gate.as_union_value()),
                    },
                )
            }
        }
    }

    /// Convert from a Flatbuffers vector of gates to owned structures.
    pub fn try_from_vector<'a>(
        g_vector: Vector<'a, ForwardsUOffset<generated::Gate<'a>>>,
    ) -> Result<Vec<Gate>> {
        let mut gates = vec![];
        for i in 0..g_vector.len() {
            let g_a = g_vector.get(i);
            gates.push(Gate::try_from(g_a)?);
        }
        Ok(gates)
    }

    /// Add a vector of this structure into a Flatbuffers message builder.
    pub fn build_vector<'a>(
        builder: &mut FlatBufferBuilder<'a>,
        gates: &[Gate],
    ) -> WIPOffset<Vector<'a, ForwardsUOffset<generated::Gate<'a>>>> {
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
            New(_, _, _) => unimplemented!("New gate"),

            Convert(_, _, _, _, _, _) => unimplemented!("Convert gate"),
            Call(_, _, _) => unimplemented!("Call gate"),
        }
    }
}

/// replace_output_wires goes through all gates in `gates` and replace `output_wires[i]` by `i`.
/// If `output_wires[i]` belongs to a wire range (in New, Call, Convert gates),
/// add `Copy(i, output_wires[i])` at the end of gates and do not modify other gates containing `output_wires[i]`.
///
/// If a `Delete` gate contains an output wire, `replace_output_wires` will return an error.
pub fn replace_output_wires(
    gates: &mut Vec<Gate>,
    out_wires: &[WireRangeWithType],
    known_functions: &HashMap<String, FunctionCounts>,
) -> Result<()> {
    // It is not easily doable to replace a WireId in a wire range.
    // Therefor, if an output wire belongs to a wire range, we will add a Copy gate and not modify this WireId.
    let mut do_no_modify_wires: HashSet<(TypeId, WireId)> = HashSet::new();
    for gate in gates.iter() {
        match gate {
            New(type_id, first_id, last_id) => {
                for wire_id in *first_id..=*last_id {
                    do_no_modify_wires.insert((*type_id, wire_id));
                }
            }
            Call(name, out_ids, in_ids) => {
                let func_params = FunctionCounts::get_function_counts(known_functions, name)?;
                let out_ids_with_types =
                    add_types_to_wire_ranges(out_ids, &func_params.output_count)?;
                out_ids_with_types.iter().for_each(|wire_range_with_type| {
                    (wire_range_with_type.first_id..=wire_range_with_type.last_id).for_each(
                        |wire_id| {
                            do_no_modify_wires.insert((wire_range_with_type.type_id, wire_id));
                        },
                    );
                });
                let in_ids_with_types = add_types_to_wire_ranges(in_ids, &func_params.input_count)?;
                in_ids_with_types.iter().for_each(|wire_range_with_type| {
                    (wire_range_with_type.first_id..=wire_range_with_type.last_id).for_each(
                        |wire_id| {
                            do_no_modify_wires.insert((wire_range_with_type.type_id, wire_id));
                        },
                    );
                });
            }
            Convert(
                out_type_id,
                out_first_id,
                out_last_id,
                in_type_id,
                in_first_id,
                in_last_id,
            ) => {
                (*out_first_id..=*out_last_id).for_each(|wire_id| {
                    do_no_modify_wires.insert((*out_type_id, wire_id));
                });
                (*in_first_id..=*in_last_id).for_each(|wire_id| {
                    do_no_modify_wires.insert((*in_type_id, wire_id));
                });
            }
            _ => (),
        }
    }

    let mut map: HashMap<TypeId, WireId> = HashMap::new();
    for wire_range_with_type in out_wires.iter() {
        let old_type_id = wire_range_with_type.type_id;
        for old_wire in wire_range_with_type.first_id..=wire_range_with_type.last_id {
            let count = map.entry(old_type_id).or_insert(0);
            let new_wire = *count;
            *count += 1;

            // If the old_wire is in a wire range, we add a Copy gate and not modify this WireId in other gates.
            if do_no_modify_wires.contains(&(old_type_id, old_wire)) {
                gates.push(Copy(old_type_id, new_wire, old_wire));
                continue;
            }

            for gate in &mut *gates {
                match gate {
                    Constant(ref type_id, ref mut output, _) => {
                        replace_wire_id(type_id, &old_type_id, output, old_wire, new_wire);
                    }
                    Copy(ref type_id, ref mut output, ref mut input) => {
                        replace_wire_id(type_id, &old_type_id, output, old_wire, new_wire);
                        replace_wire_id(type_id, &old_type_id, input, old_wire, new_wire);
                    }
                    Add(ref type_id, ref mut output, ref mut left, ref mut right) => {
                        replace_wire_id(type_id, &old_type_id, output, old_wire, new_wire);
                        replace_wire_id(type_id, &old_type_id, left, old_wire, new_wire);
                        replace_wire_id(type_id, &old_type_id, right, old_wire, new_wire);
                    }
                    Mul(ref type_id, ref mut output, ref mut left, ref mut right) => {
                        replace_wire_id(type_id, &old_type_id, output, old_wire, new_wire);
                        replace_wire_id(type_id, &old_type_id, left, old_wire, new_wire);
                        replace_wire_id(type_id, &old_type_id, right, old_wire, new_wire);
                    }
                    AddConstant(ref type_id, ref mut output, ref mut input, _) => {
                        replace_wire_id(type_id, &old_type_id, output, old_wire, new_wire);
                        replace_wire_id(type_id, &old_type_id, input, old_wire, new_wire);
                    }
                    MulConstant(ref type_id, ref mut output, ref mut input, _) => {
                        replace_wire_id(type_id, &old_type_id, output, old_wire, new_wire);
                        replace_wire_id(type_id, &old_type_id, input, old_wire, new_wire);
                    }
                    PublicInput(ref type_id, ref mut output) => {
                        replace_wire_id(type_id, &old_type_id, output, old_wire, new_wire);
                    }
                    PrivateInput(ref type_id, ref mut output) => {
                        replace_wire_id(type_id, &old_type_id, output, old_wire, new_wire);
                    }
                    AssertZero(ref type_id, ref mut wire) => {
                        replace_wire_id(type_id, &old_type_id, wire, old_wire, new_wire);
                    }
                    Delete(ref type_id, ref mut first, ref mut last) => {
                        if (*first <= old_wire && *last >= old_wire) && (*type_id == old_type_id) {
                            return Err("It is forbidden to delete an output wire !".into());
                        }
                    }
                    // Convert, Call and New gates have already been treated at the beginning of the loop
                    // by adding Copy gate if (old_type_id, old_wire) belongs to those gates.
                    _ => (),
                }
            }
        }
    }
    Ok(())
}

#[test]
fn test_replace_output_wires() {
    use crate::Count;

    let mut gates = vec![
        New(0, 4, 4),
        PublicInput(0, 4),
        PrivateInput(0, 5),
        Constant(0, 6, vec![15]),
        PublicInput(1, 6),
        Add(0, 7, 4, 5),
        Delete(0, 4, 4),
        Mul(0, 8, 6, 7),
        Call(
            "custom".to_string(),
            vec![WireRange::new(9, 12)],
            vec![WireRange::new(7, 8)],
        ),
        AssertZero(0, 12),
    ];
    let output_wires = vec![
        WireRangeWithType::new(0, 4, 6),
        WireRangeWithType::new(0, 12, 12),
    ];
    let known_functions = HashMap::from([(
        "custom".to_string(),
        FunctionCounts {
            input_count: vec![Count::new(0, 2)],
            output_count: vec![Count::new(0, 4)],
            public_count: HashMap::new(),
            private_count: HashMap::new(),
        },
    )]);
    replace_output_wires(&mut gates, &output_wires, &known_functions).unwrap();
    let correct_gates = vec![
        New(0, 4, 4),
        PublicInput(0, 4),
        PrivateInput(0, 1),
        Constant(0, 2, vec![15]),
        PublicInput(1, 6),
        Add(0, 7, 4, 1),
        Delete(0, 4, 4),
        Mul(0, 8, 2, 7),
        Call(
            "custom".to_string(),
            vec![WireRange::new(9, 12)],
            vec![WireRange::new(7, 8)],
        ),
        AssertZero(0, 12),
        Copy(0, 0, 4),
        Copy(0, 3, 12),
    ];
    assert_eq!(gates, correct_gates);
}

#[test]
fn test_replace_output_wires_with_forbidden_delete() {
    let mut gates = vec![
        Add(0, 2, 4, 6),
        Mul(0, 7, 4, 6),
        Add(0, 8, 3, 5),
        Add(0, 9, 7, 8),
        Mul(0, 10, 3, 5),
        AddConstant(0, 11, 10, vec![1]),
        Delete(0, 7, 9),
    ];
    let output_wires = vec![
        WireRangeWithType::new(0, 8, 8),
        WireRangeWithType::new(0, 4, 4),
    ];
    let test = replace_output_wires(&mut gates, &output_wires, &HashMap::new());
    assert!(test.is_err());

    let mut gates = vec![
        Add(0, 2, 4, 6),
        Mul(0, 7, 4, 6),
        Delete(0, 4, 4),
        Add(0, 8, 3, 5),
        Add(0, 9, 7, 8),
        Mul(0, 10, 3, 5),
        AddConstant(0, 11, 10, vec![1]),
    ];
    let output_wires = vec![
        WireRangeWithType::new(0, 8, 8),
        WireRangeWithType::new(0, 4, 4),
    ];
    let test = replace_output_wires(&mut gates, &output_wires, &HashMap::new());
    assert!(test.is_err());
}

/// Replace `wire` by `new_wire` if `wire` was equal to `old_wire` and `type_id` was equal to `old_type_id`
pub(crate) fn replace_wire_id(
    type_id: &TypeId,
    old_type_id: &TypeId,
    wire: &mut WireId,
    old_wire: WireId,
    new_wire: WireId,
) {
    if (*wire == old_wire) && (*type_id == *old_type_id) {
        *wire = new_wire;
    }
}

#[test]
fn test_replace_wire_id() {
    let mut wire = 5;
    replace_wire_id(&0, &0, &mut wire, 3, 5);
    assert_eq!(wire, 5);

    let mut wire = 5;
    replace_wire_id(&0, &0, &mut wire, 5, 8);
    assert_eq!(wire, 8);

    let mut wire = 8;
    replace_wire_id(&0, &1, &mut wire, 8, 10);
    assert_eq!(wire, 8);
}
