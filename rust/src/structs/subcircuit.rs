use crate::Result;
use flatbuffers::{FlatBufferBuilder, Vector, WIPOffset, ForwardsUOffset};
use crate::sieve_ir_generated::sieve_ir as g;
use crate::{WireId, Gate};
use crate::structs::wire::{WireList, WireListElement};
use crate::structs::function::CaseInvoke;

/// Convert from Flatbuffers references to owned structure.
pub fn try_from_block(g_block: g::Block) -> Result<Vec<Gate>> {
    let ret = Gate::try_from_vector(g_block.block().ok_or("Missing subcircuit in block")?)?;
    Ok(ret)
}

/// Serialize this structure into a Flatbuffer message
pub fn build_block<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    block: &'args [Gate],
) -> WIPOffset<g::Block<'bldr>> {
    let impl_gates = Gate::build_vector(builder, block);
    g::Block::create(
        builder,
        &g::BlockArgs {
            block: Some(impl_gates),
        }
    )
}

/// Convert from Flatbuffers vector of directives into owned structure.
pub fn try_from_block_vector<'a>(
    g_vector: Vector<'a, ForwardsUOffset<g::Block<'a>>>,
) -> Result<Vec<Vec<Gate>>> {
    let mut directives = vec![];
    for i in 0..g_vector.len() {
        let g_a = g_vector.get(i);
        directives.push(try_from_block(g_a)?);
    }
    Ok(directives)
}

/// Add a vector of this structure into a Flatbuffers message builder.
pub fn build_block_vector<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    directives: &'args [Vec<Gate>],
) -> WIPOffset<Vector<'bldr, ForwardsUOffset<g::Block<'bldr>>>> {
    let g_directives: Vec<_> = directives.iter()
        .map(|directive| {
            let impl_gates = Gate::build_vector(builder, directive);
            g::Block::create(
                builder,
                &g::BlockArgs {
                    block: Some(impl_gates),
                }
            )}
        ).collect();
    let g_vector = builder.create_vector(&g_directives);
    g_vector
}

pub fn translate_gates<'s>(
    subcircuit: &'s[Gate],
    output_input_wires: &'s[WireId],
    free_temporary_wire: &'s mut WireId
) -> impl Iterator<Item = Gate> + 's {
    subcircuit
        .iter()
        .map(move |gate| translate_gate(gate, output_input_wires, free_temporary_wire))
}

/* TODO: when given an index out of the size of $translation_vector, we should expand
   this translation_vector upto the given index, and fill the added positions with
   free wires.
*/
macro_rules! translate_or_temp {
        ($translation_vector:ident, $wire_name:expr, $free_temp_wire:expr) => {
            $translation_vector
                .get($wire_name as usize)
                .map_or_else(
                    || {
                        let temp = $free_temp_wire;
                        $free_temp_wire += 1;
                        temp
                    },
                    |a| *a
                )
        };
    }

fn translate_gate(gate: &Gate, output_input_wires: &[WireId], free_temporary_wire: &mut WireId) -> Gate {
    macro_rules! translate {
        ($wire_name:expr) => {
            translate_or_temp!(output_input_wires, $wire_name, *free_temporary_wire)
        };
    }
    match gate {
        Gate::Constant(out, val) => Gate::Constant(translate!(*out), val.clone()),
        Gate::AssertZero(out) => Gate::AssertZero(translate!(*out)),
        Gate::Copy(out, inp) => Gate::Copy(translate!(*out), translate!(*inp)),
        Gate::Add(out, a, b) => Gate::Add(translate!(*out), translate!(*a), translate!(*b)),
        Gate::Mul(out, a, b) => Gate::Mul(translate!(*out), translate!(*a), translate!(*b)),
        Gate::AddConstant(out, a, val) => Gate::AddConstant(translate!(*out), translate!(*a), val.clone()),
        Gate::MulConstant(out, a, val) => Gate::MulConstant(translate!(*out), translate!(*a), val.clone()),
        Gate::And(out, a, b) => Gate::And(translate!(*out), translate!(*a), translate!(*b)),
        Gate::Xor(out, a, b) => Gate::Xor(translate!(*out), translate!(*a), translate!(*b)),
        Gate::Not(out, a) => Gate::Not(translate!(*out), translate!(*a)),
        Gate::Instance(out) => Gate::Instance(translate!(*out)),
        Gate::Witness(out) => Gate::Witness(translate!(*out)),
        Gate::Free(from, end) => Gate::Free(translate!(*from), end.map(|id| translate!(id))),

        Gate::Call(name, outs,ins) =>
            Gate::Call(
                name.clone(),
                translate_wirelist(outs, output_input_wires, free_temporary_wire),
                translate_wirelist(ins, output_input_wires, free_temporary_wire)
            ),

        Gate::Switch(condition, output_wires, cases, branches) => {
            let translated_branches = branches.iter()
                .map(|case| {
                    match case {
                        CaseInvoke::AbstractGateCall(name, inputs)
                            => CaseInvoke::AbstractGateCall(
                                   name.clone(),
                                   translate_wirelist(inputs, output_input_wires, free_temporary_wire)
                        ),
                        CaseInvoke::AbstractAnonCall(inputs, ins, wit, sub)
                            => CaseInvoke::AbstractAnonCall(translate_wirelist(inputs, output_input_wires, free_temporary_wire), *ins, *wit, sub.clone()),
                    }
                }).collect();

            Gate::Switch(
                translate!(*condition),
                translate_wirelist(output_wires, output_input_wires, free_temporary_wire),
                cases.clone(),
                translated_branches,
            )
        }

/*
        Gate::For(
            start_val,
            end_val,
            instance_count,
            witness_count,
            output_mapping,
            input_mapping,
            body
        ) => {
            Gate::For(
                *start_val,
                *end_val,
                *instance_count,
                *witness_count,
                output_mapping.iter().map(|mapping| (translate_wire(mapping.0, output_input_wires), mapping.1, mapping.2)).collect(),
                input_mapping.iter().map(|mapping| (translate_wire(mapping.0, output_input_wires), mapping.1, mapping.2)).collect(),
                body.clone(),
            )
        }

 */
    }
}

fn translate_wirelist(wires: &WireList, output_input_wires: &[WireId], free_temporary_wire: &mut WireId) -> WireList {
    wires
        .iter()
        .flat_map(move |id|
            translate_wirelistelement(id, output_input_wires, free_temporary_wire)
        ).collect()
}

// TODO implement it using iterator exclusively
fn translate_wirelistelement<'w>(
    wire: &'w WireListElement,
    output_input_wires: &'w [WireId],
    free_temporary_wire: &mut WireId
) -> Vec<WireListElement>  {
    match wire {
        WireListElement::Wire(val) => vec![WireListElement::Wire(translate_or_temp!(output_input_wires, *val, *free_temporary_wire))],
        WireListElement::WireRange(first, last) =>
            (*first..*last).into_iter()
                .map(|val| WireListElement::Wire(translate_or_temp!(output_input_wires, val, *free_temporary_wire))).collect()
    }
}

#[test]
fn test_translate_gate() -> Result<()> {
    use crate::Gate::*;
    use crate::producers::examples::{encode_negative_one, example_header};

    let gates = vec![
        Witness(1),
        Switch(
            1,                      // condition
            vec![0, 2, 4, 5, 6],    // output wires
            vec![1],
            1,
            1,
            vec![vec![3], vec![5]], // cases
            vec![
                // branches
                vec![
                    Instance(0),  // In Global Namespace: Instance(0)
                    Witness(1),   // In Global Namespace: Witness(2)
                    Call("example/mul".to_string(), vec![2], vec![5, 5]), // In Global Namespace: Mul(4, 1, 1)
                    Call("example/mul".to_string(), vec![3], vec![1, 1]), // In Global Namespace: Mul(5, 2, 2)
                    Add(4, 2, 3), // In Global Namespace: Add(6, 4, 5)
                ],
                // remapping local-to-global namespaces: [0, 2, 4, 5, 6] || [1] = [0, 2, 4, 5, 6, 1]
                vec![
                    Instance(0),
                    Call("example/mul".to_string(), vec![1], vec![5, 0]),
                    Witness(2),
                    Mul(3, 1, 2),
                    Add(4, 2, 3),
                ],
            ],
        ),
        Constant(3, encode_negative_one(&example_header())), // -1
        Call("example/mul".to_string(), vec![7], vec![3, 0]), // - instance_0
        Add(8, 6, 7),                                        // sum - instance_0
        Free(0, Some(7)),                                    // Free all previous wires
        AssertZero(8),                                       // difference == 0
    ];

    let output_input_wires = vec![42, 43, 44, 45, 46, 47, 48, 49, 50];
    let expected = vec![
        Witness(43),
        Switch(
            43,                      // condition
            vec![42, 44, 46, 47, 48],    // output wires
            vec![43],
            1,
            1,
            vec![vec![3], vec![5]], // cases
            vec![
                // branches
                vec![
                    Instance(0),  // In Global Namespace: Instance(0)
                    Witness(1),   // In Global Namespace: Witness(2)
                    Call("example/mul".to_string(), vec![2], vec![5, 5]), // In Global Namespace: Mul(4, 1, 1)
                    Call("example/mul".to_string(), vec![3], vec![1, 1]), // In Global Namespace: Mul(5, 2, 2)
                    Add(4, 2, 3), // In Global Namespace: Add(6, 4, 5)
                ],
                // remapping local-to-global namespaces: [0, 2, 4, 5, 6] || [1] = [0, 2, 4, 5, 6, 1]
                vec![
                    Instance(0),
                    Call("example/mul".to_string(), vec![1], vec![5, 0]),
                    Witness(2),
                    Mul(3, 1, 2),
                    Add(4, 2, 3),
                ],
            ],
        ),
        Constant(45, encode_negative_one(&example_header())), // -1
        Call("example/mul".to_string(), vec![49], vec![45, 42]), // - instance_0
        Add(50, 48, 49),                                        // sum - instance_0
        Free(42, Some(49)),                                    // Free all previous wires
        AssertZero(50),                                       // difference == 0
    ];

    let translated: Vec<Gate> = translate_gates(&gates, &output_input_wires).collect();
    assert_eq!(translated, expected);

    Ok(())
}

#[test]
fn test_wire_mapping_expansion() -> Result<()> {
    let wire_mappings: Vec<(u64, u64, usize)> = vec![(5, 0, 1), (10, 100, 2), (60, 100, 3)];
    let expanded = expand_wire_mappings(&wire_mappings, 1);

    let expected: Vec<u64> = vec![5, 110, 111, 160, 161, 162];

    assert_eq!(expanded, expected);

    Ok(())
}
