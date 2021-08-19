use std::cell::Cell;
use crate::structs::function::{ForLoopBody,CaseInvoke};
use crate::structs::relation::{contains_feature, Relation, SIMPLE, ADD, MUL, ADDC, MULC, XOR, AND};
use crate::{Gate, WireId};
use crate::consumers::{flattening::tmp_wire, TEMPORARY_WIRES_START};


pub fn exp_definable_gate(
    gate: Gate,                          // The gate to be rewritten (if need be)
    free_temporary_wire: &Cell<WireId>,  // Cell containing the id of the first available temp wire; acts as a global ref
    gate_mask          : u16,            // The mask for the gates that are allowed
    output_gates       : &mut Vec<Gate>  // Accumulator where we have written the flattened version of the gates seen before gate, on top of which we'll push the flattened version of gate
) {
    let ggate = gate.clone();
    match gate {
	Gate::AnonCall(output_wires,input_wires,instance_count,witness_count,subcircuit) =>
	{
	    let mut new_subcircuit = Vec::new();
	    for innergate in subcircuit {
                exp_definable_gate(innergate, free_temporary_wire, gate_mask, &mut new_subcircuit);
	    }
	    output_gates.push(Gate::AnonCall(output_wires.clone(),input_wires.clone(), instance_count, witness_count, new_subcircuit));
	},

	Gate::Switch(wire_id, output_wires, values, branches) => {
	    let mut new_branches = Vec::new();
	    for branch in branches {
		match branch {
		    CaseInvoke::AbstractAnonCall(input_wires, instance_count, witness_count, body) => {
	                let mut new_body = Vec::new();
	                for innergate in body {
                            exp_definable_gate(innergate, free_temporary_wire, gate_mask, &mut new_body);
	                }
	                new_branches.push(CaseInvoke::AbstractAnonCall(input_wires.clone(), instance_count, witness_count, new_body));
		    }
                    a => new_branches.push(a.clone())
		}
	    }
	    output_gates.push(Gate::Switch(wire_id, output_wires, values, new_branches));
	},
        
	Gate::For(name,start_val,end_val,output_wires,body) => {
	    let new_body = match body {
                ForLoopBody::IterExprAnonCall(
                    output_wires,
                    input_wires,
                    instance_count,
                    witness_count,
                    subcircuit
                ) => {
	            let mut new_subcircuit = Vec::new();
	            for innergate in subcircuit {
                        exp_definable_gate(innergate, free_temporary_wire, gate_mask, &mut new_subcircuit);
	            }
	            ForLoopBody::IterExprAnonCall(
                        output_wires.clone(),
                        input_wires.clone(),
                        instance_count,
                        witness_count,
                        new_subcircuit)
                }
                a => a.clone()
            };
	    output_gates.push(Gate::For(name,start_val,end_val,output_wires,new_body));
	},

	Gate::Add(wire_id_out,wire_id1,wire_id2) => {
            if !contains_feature(gate_mask, ADD) { // Has to be in field of characteristic 2
                let bool_gate = Gate::Xor(wire_id_out,wire_id1,wire_id2);
                exp_definable_gate(bool_gate, free_temporary_wire, gate_mask, output_gates); // We recurse on it in case Xor is not authorized
            } else {
                output_gates.push(gate);
            }
	},

        Gate::AddConstant(wire_id_out,wire_id,cst) => {
            if !contains_feature(gate_mask, ADDC) {
                let tmp = tmp_wire(free_temporary_wire);
	        output_gates.push(Gate::Constant(tmp, cst));
                let add_gate = Gate::Add(wire_id_out,wire_id,tmp);
                exp_definable_gate(add_gate, free_temporary_wire, gate_mask, output_gates); // We recurse on it in case Add is not authorized
            } else {
                output_gates.push(ggate);
            }
	},

	Gate::Mul(wire_id_out,wire_id1,wire_id2) => {
            if !contains_feature(gate_mask, MUL) { // Has to be in field of characteristic 2
                let bool_gate = Gate::And(wire_id_out,wire_id1,wire_id2);
                exp_definable_gate(bool_gate, free_temporary_wire, gate_mask, output_gates); // We recurse on it in case And is not authorized
            } else {
                output_gates.push(gate);
            }
	},

        Gate::MulConstant(wire_id_out,wire_id,cst) => {
            if !contains_feature(gate_mask, MULC) {
                let tmp = tmp_wire(free_temporary_wire);
	        output_gates.push(Gate::Constant(tmp, cst));
                let mul_gate = Gate::Mul(wire_id_out,wire_id,tmp);
                exp_definable_gate(mul_gate, free_temporary_wire, gate_mask, output_gates); // We recurse on it in case Mul is not authorized
            } else {
                output_gates.push(ggate);
            }
	},

        Gate::And(wire_id_out,wire_id1,wire_id2) => {
            if !contains_feature(gate_mask, AND) { // Has to be in field of characteristic 2
                if contains_feature(gate_mask, MUL) {
                    let arith_gate = Gate::Mul(wire_id_out,wire_id1,wire_id2);
                    output_gates.push(arith_gate);
                } else { // otherwise we may loop
                    panic!();
                }
            } else {
                output_gates.push(gate);
            }
	},
        
    	Gate::Xor(wire_id_out, wire_id1, wire_id2) => {
            if !contains_feature(gate_mask, XOR) { // Has to be in field of characteristic 2
                if contains_feature(gate_mask, ADD) {
                    let arith_gate = Gate::Add(wire_id_out,wire_id1,wire_id2);
                    output_gates.push(arith_gate);
                } else { // otherwise we may loop
                    panic!();
                }
            } else {
                output_gates.push(gate);
            }
	},
	_ => { output_gates.push(gate) }
    }
}


pub fn exp_definable_from(relation : &Relation, gate_mask : u16, tmp_wire_start : u64) -> Relation {

    let mut gates = Vec::new();
    let mut free_temporary_wire = Cell::new(tmp_wire_start);

    for inner_gate in &relation.gates {
        exp_definable_gate(inner_gate.clone(), &mut free_temporary_wire, gate_mask, &mut gates);
    }

    Relation {
        header   : relation.header.clone(),
        gate_mask: gate_mask,
        feat_mask: SIMPLE,
        functions: vec![],
        gates    : gates,
    }
}

pub fn exp_definable(relation : &Relation, gate_mask : u16) -> Relation {
    exp_definable_from(relation, gate_mask, TEMPORARY_WIRES_START)
}
