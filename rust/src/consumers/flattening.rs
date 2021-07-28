use crate::{Gate, WireId};
use std::collections::HashMap;
use crate::structs::subcircuit::translate_gates;
use crate::structs::wire::expand_wirelist;
use std::cell::Cell;
use crate::structs::gates::Gate::AnonCall;
use crate::structs::function::ForLoopBody;
use crate::structs::iterators::evaluate_iterexpr_list;

// (output_count, input_count, instance_count, witness_count, subcircuit)
type FunctionDeclare = (usize, usize, usize, usize, Vec<Gate>);

pub fn flatten_gate(
    gate: Gate,
    known_functions: &HashMap<String, FunctionDeclare>,
    known_iterators: &HashMap<String, u64>,
    free_temporary_wire: &Cell<WireId>,
) -> Vec<Gate> {
    match gate {

        Gate::AnonCall(
            output_wires,
            input_wires,
            _, _,
            subcircuit
        ) => {
            let expanded_output_wires = expand_wirelist(&output_wires);
            let expanded_input_wires = expand_wirelist(&input_wires);
            let mut output_input_wires = [expanded_output_wires, expanded_input_wires].concat();

            translate_gates(&subcircuit, &mut output_input_wires, free_temporary_wire)
                .flat_map(move |inner_gate| flatten_gate(inner_gate, known_functions, known_iterators, free_temporary_wire))
                .collect::<Vec<Gate>>()
        }

        Gate::Call(name, output_wires, input_wires) => {
            if let Some(declaration) = known_functions.get(&name) {
                // When a function is 'executed', then it's a completely new context, meaning
                // that iterators defined previously, will not be forwarded to the function.
                // This is why we set an empty (HashMap::default()) set of iterators.
                return flatten_gate(
                    AnonCall(output_wires, input_wires, declaration.2, declaration.3, declaration.4.clone()),
                    known_functions,
                    &HashMap::default(),
                    free_temporary_wire);
            } else {
                // The function is not known, so this should either panic, or return an empty vector.
                // We will consider that this means the original circuit is not valid, while
                // it should have been tested beforehand
                return vec![];
            }
        }

        Gate::For(
            iterator_name,
            start_val,
            end_val,
            _,
            body
        ) => {
            (start_val as usize..=(end_val as usize))
                .into_iter()
                .flat_map(|i| {
                    let mut local_known_iterators = known_iterators.clone();
                    local_known_iterators.insert(iterator_name.clone(), i as u64);

                    let (subcircuit, expanded_outputs, expanded_inputs, use_same_context) = match &body {
                        ForLoopBody::IterExprCall(name, outputs, inputs) => {
                            let expanded_outputs = evaluate_iterexpr_list(&outputs, &local_known_iterators);
                            let expanded_inputs = evaluate_iterexpr_list(&inputs, &local_known_iterators);


                            if let Some(declaration) = known_functions.get(name) {
                                // When a function is 'executed', then it's a completely new context, meaning
                                // that iterators defined previously, will not be forwarded to the function.
                                (&declaration.4, expanded_outputs, expanded_inputs, false)
                            } else {
                                // The function is not known, so this should either panic, or return an empty vector.
                                // We will consider that this means the original circuit is not valid, while
                                // it should have been tested beforehand
                                return vec![];
                            }
                        }

                        ForLoopBody::IterExprAnonCall(
                            output_wires,
                            input_wires,
                            _, _,
                            subcircuit
                        ) => {
                            let expanded_outputs = evaluate_iterexpr_list(&output_wires, &local_known_iterators);
                            let expanded_inputs = evaluate_iterexpr_list(&input_wires, &local_known_iterators);

                            // In the case of a AnonCall, it's just like a block, where the context is
                            // forwarded from the outer workspace, into the inner one.
                            (subcircuit, expanded_outputs, expanded_inputs, true)
                        }
                    };

                    let mut output_input_wires = [expanded_outputs, expanded_inputs].concat();
                    let null_hashmap: HashMap<String, u64> = HashMap::default();

                    translate_gates(subcircuit, &mut output_input_wires, free_temporary_wire)
                        .flat_map(move |inner_gate|
                            flatten_gate(
                                inner_gate,
                                known_functions,
                                if use_same_context {&local_known_iterators} else {&null_hashmap},
                                free_temporary_wire
                            )
                        ).collect::<Vec<Gate>>()
                }).collect::<Vec<Gate>>()
        }
/*
        Gate::Switch(_, _, _, _) => {}
*/
        _ => vec![gate],
    }
}


pub fn flatten_subcircuit(
    gates: &[Gate],
    known_functions: &HashMap<String, FunctionDeclare>,
    known_iterators: &HashMap<String, u64>,
    free_temporary_wire: &Cell<WireId>
) -> Vec<Gate> {
    gates
        .into_iter()
        .flat_map(move |gate| flatten_gate(gate.clone(), known_functions, known_iterators, free_temporary_wire))
        .collect()
}
