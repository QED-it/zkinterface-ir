use crate::Result;
use flatbuffers::{FlatBufferBuilder, Vector, WIPOffset, ForwardsUOffset};
use crate::sieve_ir_generated::sieve_ir as g;
use crate::{WireId, Gate};

/// Convert from Flatbuffers references to owned structure.
pub fn try_from_block(g_block: g::Block) -> Result<Vec<Gate>> {
    let ret = Gate::try_from_vector(g_block.block().ok_or("Missing subcircuit in block")?)?;
    Ok(ret)
}

/// Serialize this structure into a Flatbuffer message
pub fn build_block<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
    block: &'args [Gate],
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
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

pub fn translate_gates<'s>(subcircuit: &'s[Gate], output_input_wires: &'s[WireId]) -> impl Iterator<Item = Gate> + 's {
    subcircuit
        .iter()
        .map(move |gate| translate_gate(gate, output_input_wires))
}

fn translate_gate(gate: &Gate, output_input_wires: &[WireId]) -> Gate {
    match gate {
        Gate::Constant(out, val) => Gate::Constant(output_input_wires[*out as usize], val.clone()),
        Gate::AssertZero(out) => Gate::AssertZero(output_input_wires[*out as usize]),
        Gate::Copy(out, inp) => Gate::Copy(output_input_wires[*out as usize], output_input_wires[*inp as usize]),
        Gate::Add(out, a, b) => Gate::Add(output_input_wires[*out as usize], output_input_wires[*a as usize], output_input_wires[*b as usize]),
        Gate::Mul(out, a, b) => Gate::Mul(output_input_wires[*out as usize], output_input_wires[*a as usize], output_input_wires[*b as usize]),
        Gate::AddConstant(out, a, val) => Gate::AddConstant(output_input_wires[*out as usize], output_input_wires[*a as usize], val.clone()),
        Gate::MulConstant(out, a, val) => Gate::MulConstant(output_input_wires[*out as usize], output_input_wires[*a as usize], val.clone()),
        Gate::And(out, a, b) => Gate::And(output_input_wires[*out as usize], output_input_wires[*a as usize], output_input_wires[*b as usize]),
        Gate::Xor(out, a, b) => Gate::Xor(output_input_wires[*out as usize], output_input_wires[*a as usize], output_input_wires[*b as usize]),
        Gate::Not(out, a) => Gate::Not(output_input_wires[*out as usize], output_input_wires[*a as usize]),
        Gate::Instance(out) => Gate::Instance(output_input_wires[*out as usize]),
        Gate::Witness(out) => Gate::Witness(output_input_wires[*out as usize]),
        Gate::Free(from, end) => Gate::Free(output_input_wires[*from as usize], end.map(|id| output_input_wires[id as usize])),

        Gate::Call(name, outs,ins) =>
            Gate::Call(name.clone(), translate_vector_wires(outs, output_input_wires), translate_vector_wires(ins, output_input_wires)),

        Gate::Switch(condition, output_wires, input_wires, instance_count, witness_count, cases, branches) => {
            let new_output_wires = translate_vector_wires(output_wires, output_input_wires);
            let new_input_wires = translate_vector_wires(input_wires, output_input_wires);
            let new_output_input_wires = [&new_output_wires[..], &new_input_wires[..]].concat();

            Gate::Switch(
                output_input_wires[*condition as usize],
                new_output_wires,
                new_input_wires,
                *instance_count,
                *witness_count,
                cases.clone(),
                branches.iter().map(|branch| translate_gates(branch, &new_output_input_wires).collect()).collect(),
            )
        }

        // This one should never happen
        Gate::Function(..) => panic!("Function should not be defined within bodies."),

        Gate::For(..) => unimplemented!(),
    }
}

fn translate_vector_wires(wires: &[WireId], output_input_wires: &[WireId]) -> Vec<WireId> {
    wires.iter().map(|id| output_input_wires[*id as usize]).collect()
}

pub fn expand_wire_mappings(mappings: &[(WireId, u64, usize)], current_index: u64) -> Vec<WireId> {
    mappings.iter().flat_map(|mapping| {
        (0..mapping.2).into_iter().map(move |j| mapping.0 + mapping.1 * current_index + j as u64)
    }).collect()

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


    let expected = vec![
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
                    Instance(0),
                    Witness(2),
                    Call("example/mul".to_string(), vec![4], vec![1, 1]),
                    Call("example/mul".to_string(), vec![5], vec![2, 2]),
                    Add(6, 4, 5),
                ],
                vec![
                    Instance(0),
                    Call("example/mul".to_string(), vec![2], vec![1, 0]),
                    Witness(4),
                    Mul(5, 2, 4),
                    Add(6, 4, 5),
                ],
            ],
        ),
        Constant(3, encode_negative_one(&example_header())), // -1
        Call("example/mul".to_string(), vec![7], vec![3, 0]), // - instance_0
        Add(8, 6, 7),                                        // sum - instance_0
        Free(0, Some(7)),                                    // Free all previous wires
        AssertZero(8),                                       // difference == 0
    ];

    let output_input_wires = (0..9).into_iter().collect::<Vec<u64>>();
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
