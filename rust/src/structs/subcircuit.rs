use crate::{WireId, Gate};
use crate::structs::relation::Relation;
use crate::structs::wire::{WireList, WireListElement, expand_wirelist, WireListElement::{Wire}};
use crate::structs::function::{ForLoopBody,CaseInvoke};
use crate::structs::iterators::evaluate_iterexpr_list;
use std::collections::HashMap;
use std::cell::Cell;

/// This macro returns the translated value of a wire into the outer namespace.
///  If the given wire id is out of bound, then it's considered as a temporary wire.
macro_rules! translate_or_temp {
        ($translation_vector:ident, $wire_name:expr, $free_temp_wire:expr) => {{
            while $translation_vector.len() <= $wire_name as usize {
                $translation_vector.push($free_temp_wire.get());
                $free_temp_wire.set($free_temp_wire.get() + 1);
            }
            $translation_vector[$wire_name as usize]
        }};
    }


/// This function translate a single Gate from the inner workspace into the outer workspace
/// using the output/input vector. If temporary wires are needed, it will pick one new one, and will
/// increase the 'free_temporary_wire' reference.
pub(crate) fn translate_gate(gate: &Gate, output_input_wires: &mut Vec<WireId>, free_temporary_wire: &Cell<WireId>) -> Gate {
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

        Gate::For(..) => {
            panic!("translate_gate: For loops should have been unrolled.");
            /*
            Gate::For(
                iterator_name.clone(),
                *start_val,
                *end_val,
                translate_wirelist(global_output_list, output_input_wires, free_temporary_wire),
                body.clone(),
            )

             */
        }
    }
}

/// This helper function will translate a (not necessarily expanded) WireList from the inner workspace
/// to the outer one using the output/input vector of wires.
/// It will return an expanded WireList of each individual translated wire.
fn translate_wirelist(wires: &WireList, output_input_wires: &mut Vec<WireId>, free_temporary_wire: &Cell<WireId>) -> WireList {
    expand_wirelist(wires)
        .iter()
        .map(|id|
            WireListElement::Wire(translate_or_temp!(output_input_wires, *id, *free_temporary_wire))
        ).collect()
}

// Auxiliary function that is used for unrolling
fn list_from_vec(wires : &Vec<WireId>) -> WireList {
    let mut result = Vec::new();
    for wire in wires {
        result.push(Wire(wire.clone()))
    };
    result
}

/// This internal function performs the unrolling of all loops.
/// No need for any renaming, just the evaluation of wire id expression with loop indices
/// Removing the loops as preprocessing to ir_flattening is key to handling nested loops.
/// It does not interfere with the flattening of functions and switches,
/// which can be done in a second pass and do necessitate renamings and introducing auxiliary wires and gates.

fn unroll(gate  : Gate,
          known_iterators: &mut HashMap<String, u64>,
          new_gates : &mut Vec<Gate>) {
    match gate{
        Gate::AnonCall(output_wires,input_wires,instance_count,witness_count,subcircuit) => {
            let mut new_subcircuit = Vec::new();
            for gate in subcircuit {
                unroll(gate, known_iterators, &mut new_subcircuit);
            }
            new_gates.push(Gate::AnonCall(output_wires,input_wires,instance_count,witness_count, new_subcircuit))
        }

        Gate::Switch(wire_id, output_wires , values, branches) => {
            let mut new_branches = Vec::new();
            for branch in branches {
                let new_branch =
                    match branch {
                        CaseInvoke::AbstractAnonCall(input_wires, instance_count, witness_count, branch) => {
                            let mut new_branch = Vec::new();
                            for gate in branch {
                                unroll(gate, known_iterators, &mut new_branch);
                            }
                            CaseInvoke::AbstractAnonCall(input_wires, instance_count, witness_count, new_branch)
                        }
                        a => { a }
                    };
                new_branches.push(new_branch);
            }
            new_gates.push(Gate::Switch(wire_id, output_wires, values, new_branches))
        }

        Gate::For(name, start_val, end_val, _, body) => {

            // The line below, and the restoration of original_val further down,
            // is necessary for handling loop shadowing (for i ... { for (int i ... }})
            // but Rust complains...

            let original_val = known_iterators.get(&name).cloned();
            
            for i in start_val as usize..=(end_val as usize) {
                known_iterators.insert(name.clone(), i as u64);

                let fgate = match &body {
                    ForLoopBody::IterExprCall(name, outputs, inputs) => {
                        let expanded_outputs = evaluate_iterexpr_list(&outputs, known_iterators);
                        let expanded_inputs  = evaluate_iterexpr_list(&inputs, known_iterators);
                        Gate::Call(name.clone(), list_from_vec(&expanded_outputs), list_from_vec(&expanded_inputs))
                    }
                    ForLoopBody::IterExprAnonCall(
                        outputs,
                        inputs,
                        a,
                        b,
                        subcircuit
                    ) => {
                        let expanded_outputs = evaluate_iterexpr_list(&outputs, known_iterators);
                        let expanded_inputs  = evaluate_iterexpr_list(&inputs, known_iterators);
                        Gate::AnonCall(list_from_vec(&expanded_outputs), list_from_vec(&expanded_inputs), *a, *b, subcircuit.clone())
                    }
                };
                unroll(fgate, known_iterators, new_gates);
            };
            known_iterators.remove(&name);

            if let Some(w) = original_val {
                known_iterators.insert(name.clone(), w.clone());
            }

        },
        a => { new_gates.push(a) }
    }
}

/// Public version of the above, simple interface on gate vectors

pub(crate) fn unroll_gate(gate : Gate) -> Vec<Gate> {
    let mut new_gates = Vec::new();
    let mut known_iterators = Default::default();
    unroll(gate, &mut known_iterators, &mut new_gates);
    new_gates
}

/// Public version of the above, simple interface on gate vectors

pub fn unrolling(gates : &[Gate], known_iterators: &mut HashMap<String, u64>) -> Vec<Gate> {
    let mut new_gates = Vec::new();
    for gate in gates {
        unroll(gate.clone(), known_iterators, &mut new_gates);
    }
    new_gates
}

/// Another public version of the above, simple interface on relation

pub fn unrolling_rel(relation : &Relation) -> Relation {
    Relation {
        header   : relation.header.clone(),
        gate_mask: relation.gate_mask,
        feat_mask: relation.feat_mask,
        functions: relation.functions.clone(),
        gates    : unrolling(&relation.gates, &mut HashMap::new()),
    }
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

    let mut output_input_wires = vec![42, 43, 44, 45, 46, 47, 48, 49, 50];
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

    let mut free_wire_tmp = 1u64<<32;
    let free_wire = Cell::from_mut(&mut free_wire_tmp);
    let translated: Vec<Gate> = gates.iter().map(|gate| translate_gate(gate, &mut output_input_wires, &free_wire)).collect();
    assert_eq!(translated, expected);

    Ok(())
}
