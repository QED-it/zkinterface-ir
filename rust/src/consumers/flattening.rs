use crate::{Gate, WireId};
use std::collections::HashMap;
use crate::structs::subcircuit::translate_gates;
use crate::structs::wire::expand_wirelist;
use std::cell::Cell;
use crate::structs::gates::Gate::AnonCall;

// (output_count, input_count, instance_count, witness_count, subcircuit)
type FunctionDeclare = (usize, usize, usize, usize, Vec<Gate>);

pub fn flatten_gate<'a>(
    gate: Gate,
    known_functions: &'a HashMap<String, FunctionDeclare>,
    free_temporary_wire: &'a Cell<WireId>,
) -> Box<dyn Iterator<Item=Gate> + 'a> {
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

            let iter = translate_gates(&subcircuit, &mut output_input_wires, free_temporary_wire)
                .flat_map(move |inner_gate| flatten_gate(inner_gate, known_functions, free_temporary_wire)).collect::<Vec<Gate>>();
            Box::new(
                iter.into_iter()
            )
        }

        Gate::Call(name, output_wires, input_wires) => {
            if let Some(declaration) = known_functions.get(&name) {
                return flatten_gate(AnonCall(output_wires, input_wires, declaration.2, declaration.3, declaration.4.clone()), known_functions, free_temporary_wire);
            } else {
                return Box::new(std::iter::empty());
            }
        }
/*
        Gate::For(
            iterator_name,
            start_val,
            end_val,
            global_output_list,
            body
        ) => {
            (start_val as usize..=(end_val as usize))
                .into_iter()
                .flat_map(|_| )
        }

        Gate::Switch(_, _, _, _) => {}
*/
        _ => Box::new(std::iter::once(gate)),
    }
}

pub fn flatten_subcircuit<'a>(
    gates: &'a [Gate],
    known_functions: &'a HashMap<String, FunctionDeclare>,
    free_temporary_wire: &'a Cell<WireId>
) -> impl Iterator<Item=Gate> + 'a {
    gates
        .into_iter()
        .flat_map(move |gate| -> Box<dyn Iterator<Item=Gate> + 'a> {
            flatten_gate(gate.clone(), known_functions, free_temporary_wire)
        })
}



