use crate::{WireId, Gate};
use crate::structs::wire::{WireList, WireListElement, expand_wirelist};
use crate::structs::function::CaseInvoke;


/// This functions will act on each gate of an iterable list of Gate, and will translate each of
/// from the inner workspace into the outer one using the output/input vector of wires given as
/// parameters. It will return an iterator.
/// If a wireId in the inner workspace is bigger than the size of the output/input vector, then
/// it's considered as a temporary wire. Temporary wires are stored starting at 2^63. It will
/// "allocate" as many temporary wires as needed by incrementing 'free_temporary_wire' as many
/// times.
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

/// This function translate a single Gate from the inner workspace into the outer workspace
/// using the output/input vector. If temporary wires are needed, it will pick one new one, and will
/// increase the 'free_temporary_wire' reference.
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

        Gate::AnonCall(output_wires, input_wires, instance_count, witness_count, subcircuit) =>
            Gate::AnonCall(
                translate_wirelist(output_wires, output_input_wires, free_temporary_wire),
                translate_wirelist(input_wires, output_input_wires, free_temporary_wire),
                *instance_count, *witness_count,
                subcircuit.clone()
            ),


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

        Gate::For(
            iterator_name,
            start_val,
            end_val,
            global_output_list,
            body
        ) => {
            Gate::For(
                iterator_name.clone(),
                *start_val,
                *end_val,
                translate_wirelist(global_output_list, output_input_wires, free_temporary_wire),
                body.clone(),
            )
        }
    }
}

/// This helper function will translate a (not necessarily expanded) WireList from the inner workspace
/// to the outer one using the output/input vector of wires.
/// It will return an expanded WireList of each individual translated wire.
fn translate_wirelist(wires: &WireList, output_input_wires: &[WireId], free_temporary_wire: &mut WireId) -> WireList {
    expand_wirelist(wires)
        .iter()
        .map(|id|
            WireListElement::Wire(translate_or_temp!(output_input_wires, *id, *free_temporary_wire))
        ).collect()
}

#[cfg(test)]
use crate::Result;

#[test]
fn test_translate_gate() -> Result<()> {
    use crate::Gate::*;
    use crate::producers::examples::{encode_negative_one, example_header};
    use crate::structs::wire::WireListElement::*;
    use crate::structs::function::CaseInvoke::*;

    let gates = vec![
            Witness(1),
            Switch(
                1,                      // condition
                vec![Wire(0), Wire(2), WireRange(4, 6)],    // output wires, with both individual+range
                vec![vec![3], vec![5]], // cases
                vec![
                    // branches
                    AbstractAnonCall (
                        // WireList, usize, usize, Vec<Gate>)
                        vec![Wire(1)],
                        1, 1,
                        vec![
                            Instance(0),  // In Global Namespace: Instance(0)
                            Witness(1),   // In Global Namespace: Witness(2)
                            Call("example/mul".to_string(), vec![Wire(2)], vec![Wire(5), Wire(5)]), // In Global Namespace: Mul(4, 1, 1)
                            Call("example/mul".to_string(), vec![Wire(3)], vec![Wire(1), Wire(1)]), // In Global Namespace: Mul(5, 2, 2)
                            Add(4, 2, 3), // In Global Namespace: Add(6, 4, 5)
                        ]

                    ),
                    // remapping local-to-global namespaces: [0, 2, 4, 5, 6] || [1] = [0, 2, 4, 5, 6, 1]
                    AbstractAnonCall (
                        // WireList, usize, usize, Vec<Gate>)
                        vec![Wire(1)],
                        1, 1,
                        vec![
                            Instance(0),
                            Call("example/mul".to_string(), vec![Wire(1)], vec![Wire(5), Wire(0)]),
                            Witness(2),
                            Mul(3, 1, 2),
                            Add(4, 2, 3),
                        ],
                    )
                ],
            ),
            Constant(3, encode_negative_one(&example_header())), // -1
            Call("example/mul".to_string(), vec![Wire(7)], vec![Wire(3), Wire(0)]), // - instance_0
            Add(8, 6, 7),                                        // sum - instance_0
            Free(0, Some(7)),                                    // Free all previous wires
            AssertZero(8),                                       // difference == 0
        ];

    let output_input_wires = vec![42, 43, 44, 45, 46, 47, 48, 49, 50];
    let expected = vec![
        Witness(43),
        Switch(
            43,                      // condition
            vec![Wire(42), Wire(44), Wire(46), Wire(47), Wire(48)],    // output wires
            vec![vec![3], vec![5]], // cases
            vec![
                // branches
                AbstractAnonCall (
                    // WireList, usize, usize, Vec<Gate>)
                    vec![Wire(43)],
                    1, 1,
                    vec![
                        Instance(0),  // In Global Namespace: Instance(0)
                        Witness(1),   // In Global Namespace: Witness(2)
                        Call("example/mul".to_string(), vec![Wire(2)], vec![Wire(5), Wire(5)]), // In Global Namespace: Mul(4, 1, 1)
                        Call("example/mul".to_string(), vec![Wire(3)], vec![Wire(1), Wire(1)]), // In Global Namespace: Mul(5, 2, 2)
                        Add(4, 2, 3), // In Global Namespace: Add(6, 4, 5)
                    ]

                ),
                // remapping local-to-global namespaces: [0, 2, 4, 5, 6] || [1] = [0, 2, 4, 5, 6, 1]
                AbstractAnonCall (
                    // WireList, usize, usize, Vec<Gate>)
                    vec![Wire(43)],
                    1, 1,
                    vec![
                        Instance(0),
                        Call("example/mul".to_string(), vec![Wire(1)], vec![Wire(5), Wire(0)]),
                        Witness(2),
                        Mul(3, 1, 2),
                        Add(4, 2, 3),
                    ],
                )
            ],
        ),
        Constant(45, encode_negative_one(&example_header())), // -1
        Call("example/mul".to_string(), vec![Wire(49)], vec![Wire(45), Wire(42)]), // - instance_0
        Add(50, 48, 49),                                        // sum - instance_0
        Free(42, Some(49)),                                    // Free all previous wires
        AssertZero(50),                                       // difference == 0
    ];

    let mut free_wire = 1u64<<32;
    let translated: Vec<Gate> = translate_gates(&gates, &output_input_wires, &mut free_wire).collect();
    assert_eq!(translated, expected);

    Ok(())
}
