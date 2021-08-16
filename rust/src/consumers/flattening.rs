// use crate::structs::wire::WireListElement;
use std::convert::TryFrom;
use crate::{Gate, WireId};
// use std::iter;
use std::collections::HashMap;
use num_bigint::{BigUint, BigInt};
use num_integer::Integer;
use crate::structs::subcircuit::translate_gates;
use crate::Value;
use crate::structs::wire::{expand_wirelist,// WireList,
                           WireListElement::{Wire,WireRange}};
use std::cell::Cell;
use crate::structs::gates::Gate::*;
use crate::structs::function::{ForLoopBody,CaseInvoke};
use crate::structs::iterators::evaluate_iterexpr_list;
use crate::structs::relation::Relation;
use crate::consumers::evaluator::get_known_functions;
use crate::consumers::TEMPORARY_WIRES_START;

// (output_count, input_count, instance_count, witness_count, subcircuit)
type FunctionDeclare = (usize, usize, usize, usize, Vec<Gate>);

fn tmp_wire(free_temporary_wire: &Cell<WireId>) -> u64 {
    let new_wire = free_temporary_wire.get();
    free_temporary_wire.set(free_temporary_wire.get() + 1);
    new_wire
}
    
pub fn flatten_gate(
    gate: Gate,
    known_functions: &HashMap<String, FunctionDeclare>,
    known_iterators: &HashMap<String, u64>,
    free_temporary_wire: &Cell<WireId>,
    modulus: Value,
    instance_wires  :&mut Vec<WireId>,
    witness_wires   :&mut Vec<WireId>,
    instance_counter:&Cell<u64>,
    witness_counter :&Cell<u64>,
) -> Vec<Gate> {
    match gate {

        Gate::Instance(wire_id) => {
            let ic = instance_counter.get() as usize;
            if ic < instance_wires.len() {
	        let instance_wire = instance_wires[ic];
                instance_counter.set(instance_counter.get() + 1);
	        vec![Gate::Copy(wire_id, instance_wire)]
            } else {
                instance_wires.push(wire_id);
                instance_counter.set(instance_counter.get() + 1);
                vec![gate]
            }
	},

    	Gate::Witness(wire_id) => {
            let wc = witness_counter.get() as usize;
            if wc < witness_wires.len() {
	        let witness_wire = witness_wires[wc];
                witness_counter.set(witness_counter.get() + 1);
	        vec![Gate::Copy(wire_id, witness_wire)]
            } else {
                witness_wires.push(wire_id);
                witness_counter.set(witness_counter.get() + 1);
                vec![gate]
            }
        },

        Gate::AnonCall(
            output_wires,
            input_wires,
            _, _,
            subcircuit
        ) => {
            let expanded_output_wires  = expand_wirelist(&output_wires);
            let expanded_input_wires   = expand_wirelist(&input_wires);
            let mut output_input_wires = [expanded_output_wires, expanded_input_wires].concat();

            let outputs = translate_gates(&subcircuit, &mut output_input_wires, free_temporary_wire)
                .flat_map(move |inner_gate| flatten_gate(inner_gate,
                                                         known_functions,
                                                         known_iterators,
                                                         free_temporary_wire,
                                                         modulus.clone(),
                                                         instance_wires,
                                                         witness_wires,
                                                         instance_counter,
                                                         witness_counter))
                .collect::<Vec<Gate>>();
	    outputs
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
                    free_temporary_wire,
		    modulus.clone(),
                    instance_wires,
                    witness_wires,
                    instance_counter,
                    witness_counter);
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
            let outputs = (start_val as usize..=(end_val as usize))
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
                        .flat_map(|inner_gate|
                            flatten_gate(
                                inner_gate,
                                known_functions,
                                if use_same_context {&local_known_iterators} else {&null_hashmap},
                                free_temporary_wire,
				modulus.clone(),
                                instance_wires,
                                witness_wires,
                                instance_counter,
                                witness_counter)
                        ).collect::<Vec<Gate>>()
                }).collect::<Vec<Gate>>();
	    outputs
        }

        Gate::Switch(wire_id, output_wires, cases, branches) => {
	    let mut instance_counts = Vec::new();
	    let mut witness_counts  = Vec::new();
	    let mut global_gates    = Vec::new();
   	    let expanded_output_wires = expand_wirelist(&output_wires);

            let instance_counter_at_start = instance_counter.get();
            let witness_counter_at_start  = witness_counter.get();
	    
            // println!("Reducing {:?} <- switch {:?} [{:?}]\n", expanded_output_wires, wire_id, cases);
	    for branch in branches {
                // println!("Initial pass rewrites\n{:?}", branch);
		// maps inner context to outer context
		// also get input, witness count
		let new_gates =
		    match branch {
		        CaseInvoke::AbstractGateCall(name, input_wires) => {
			    if let Some(declaration) = known_functions.get(&name){
		     	        let expanded_input_wires = expand_wirelist(&input_wires);
     			        let mut output_input_wires = [expanded_output_wires.clone(), expanded_input_wires].concat();
			        instance_counts.push(declaration.2);
			        witness_counts.push(declaration.3);
			        let gates = translate_gates(&declaration.4, &mut output_input_wires, free_temporary_wire).collect::<Vec<Gate>>();
			        gates
			    }
			    else{
			        vec![]
			    }
		        },
		        CaseInvoke::AbstractAnonCall(input_wires, instance_count, witness_count, branch) => {
			    let expanded_input_wires = expand_wirelist(&input_wires);
			    let mut output_input_wires = [expanded_output_wires.clone(), expanded_input_wires].concat();
			    instance_counts.push(instance_count);
			    witness_counts.push(witness_count);
			    let gates = translate_gates(&branch, &mut output_input_wires, free_temporary_wire).collect::<Vec<Gate>>();
			    gates
		        }
		        
		    };
                // println!("to\n{:?}\n", new_gates);
		global_gates.push(new_gates);
	    }
	    let mut gates:Vec<Gate> = Vec::new(); // Where we produce the flattening of the switch

            // We get the max number of instance pulls and witness pulls across branches
   	    let max_instance = *instance_counts.iter().max().unwrap();
	    let max_witness  = *witness_counts.iter().max().unwrap();
            
	    // Introduce a constant wire containing -1 (i.e. modulus - 1) to be used later
	    // subtract in little endian
	    let neg_one_wire = tmp_wire(free_temporary_wire);

	    //new_modulus  and exponent vars assigned to modulus - 1
	    //convert little endian Vec<u8> to an integer you can subtract from
	    let modulus_bigint        = BigUint::from_bytes_le(&modulus[..]);
	    let modulus_minus_1       = modulus_bigint - BigUint::from(1u32);
	    let modulus_minus_1_value = modulus_minus_1.to_bytes_le();
	    //convert moudulus_minus_1 to little endian
	    
	    gates.push(Gate::Constant(neg_one_wire, modulus_minus_1_value.clone()));

	    // Introduce a constant wire containing 1
   	    let one_wire_id = tmp_wire(free_temporary_wire);
	    gates.push(Gate::Constant(one_wire_id, [1,0,0,0].to_vec()));

	    let mut temp_maps = Vec::new();

	    for (i,branch_gates) in global_gates.iter().enumerate() {

                let mut new_branch_gates = Vec::new();

                // println!("Second pass rewrites {:?}", branch_gates);

                /****** the first thing we do is compute the weight of the branch, ***********/
		/* i.e. a wire assigned the value 1 - ($0 - 42)^(p-1) for a switch on $0, case value 42,
                where p is the field's characteristic */

                //assign case value (42) to temp wire
                // For some reason, gate AddConstant fails (not authorized?)
		let case_id_wire = tmp_wire(free_temporary_wire);
		new_branch_gates.push(Gate::Constant(case_id_wire,cases[i].clone()));

		//multiply case_id_wire by -1 to get -42
		let negative_case_id = tmp_wire(free_temporary_wire);
		new_branch_gates.push(Gate::Mul(negative_case_id, case_id_wire, neg_one_wire));

		//compute $0 - 42, assigning it to base_wire
		let base_wire = tmp_wire(free_temporary_wire);
		new_branch_gates.push(Gate::Add(base_wire, wire_id, negative_case_id));

                // Now we do the exponentiation base_wire^(p - 1), where base_wire has been assigned value ($0 - 42),
                // calling the fast exponentiation function exp, which return a bunch of gates doing the exponentiation job.
                // By convention, exp assigns the output value to the first available temp wire at the point of call.
                // Therefore, we remember what this wire is:
		let exp_wire   = free_temporary_wire.get(); // We are not using this wire yet (no need to bump free_temporary_wire, exp will do it)
		let exp_as_int = BigUint::from_bytes_le(&modulus_minus_1_value[..]);
                let exp_gates  = exp(base_wire, exp_as_int, free_temporary_wire);
		new_branch_gates = [new_branch_gates, exp_gates ].concat();

                // multiply by -1 to compute (- ($0 - 42)^(p-1))
		let neg_exp_wire = tmp_wire(free_temporary_wire);
		new_branch_gates.push(Gate::Mul(neg_exp_wire,neg_one_wire,exp_wire));

                // For some reason, gate AddConstant fails (not authorized?)
		// gates.push(Gate::AddConstant(free_temporary_wire.get(), neg_exp_wire, [1,0,0,0].to_vec()));
		let weight_wire_id = tmp_wire(free_temporary_wire);
		new_branch_gates.push(Gate::Add(weight_wire_id, neg_exp_wire, one_wire_id));
		//********* end weight ************************/


                // Now, after the first pass above, the new gates are using the global output wire ids to write their outputs
                // They shouldn't: each branch is writing on them while each branch should write its outputs on temp wires,
                // and the final outputs should be the weighted sums of these temp wires.
                // For the branch we're in, we take as many temp wires as there are outputs for the switch, and keep a renaming map
                let mut map:HashMap<WireId,WireId> = HashMap::new();
		for o in &expanded_output_wires{
		    let tmp = tmp_wire(free_temporary_wire);
		    map.insert(*o,tmp);
		}
		// println!("map {:?}", &map);
		//nnswap global outputs for the temp outputs of the branch in each gate
		// let instance_counter = Cell::new(0);
		// let witness_counter  = Cell::new(0);
		let mut defined_outputs = Vec::new();
		for inner_gate in branch_gates {
		    let new_gate = swap_gate_outputs(inner_gate.clone(),
                                                     &map,
                                                     // &mut instance_wires,
                                                     // &mut witness_wires,
                                                     // &instance_counter,
                                                     // &witness_counter,
                                                     &mut defined_outputs);
		    // println!("line 265 {:?} to {:?} \n", inner_gate, new_gate);
		    //if it is assert_zero, then add a multiplication gate before it
		    match new_gate{
			Gate::AssertZero(wire_id) => {
			    let new_temp_wire = tmp_wire(free_temporary_wire);
			    new_branch_gates.push(Gate::Mul(new_temp_wire, wire_id, weight_wire_id));
			    new_branch_gates.push(Gate::AssertZero(new_temp_wire));
			},
			_ => {
			    new_branch_gates.push(new_gate.clone());
			}
		    }
		}

		// at this point, we have replaced outputs for temps in all top level gates
		// now we produce the weighted version of those output temps
                // for each temp wire in map, we want to multiply it by weight_wire_id and assign it to a new temp wire
		let mut new_temps        = Vec::new();
		let mut output_wires_vec = Vec::new();
		for o in defined_outputs {
		    let new_output_temp = tmp_wire(free_temporary_wire);
		    let temp = map.get(&o).unwrap();
		    new_branch_gates.push(Gate::Mul(new_output_temp,*temp,weight_wire_id));
		    //reassign output to weighted temp
		    new_temps.push(new_output_temp);
		    output_wires_vec.push(o);
		}
		for (index,_temp) in new_temps.iter().enumerate(){
		    map.insert(output_wires_vec[index],new_temps[index]);
		}
		temp_maps.push(map);
                instance_counter.set(instance_counter_at_start);
                witness_counter.set(witness_counter_at_start);
                for gate in new_branch_gates {
                    let recurs_gates = flatten_gate(gate,
                                                    known_functions,
                                                    known_iterators,
                                                    free_temporary_wire,
                                                    modulus.clone(),
                                                    instance_wires,
                                                    witness_wires,
                                                    instance_counter,
                                                    witness_counter);
                    gates = [gates, recurs_gates ].concat();
                }
	    }
	    
            // Now we define the real outputs of the switch as the weighted sums of the branches' outputs
	    for output in &expanded_output_wires{
                // Weighted sum starts with 0
		let mut sum_wire = tmp_wire(free_temporary_wire);
		gates.push(Gate::Constant(sum_wire, vec![0,0,0,0]));
		for map in &temp_maps {
		    let new_temp = tmp_wire(free_temporary_wire);
		    gates.push(Gate::Add(new_temp, sum_wire, *map.get(&output).unwrap()));
		    sum_wire = new_temp;
		}
		gates.push(Gate::Copy(*output,sum_wire));
	    }
            instance_counter.set(instance_counter_at_start + (max_instance as u64));
            witness_counter.set(witness_counter_at_start + (max_witness as u64));
	    gates
	    
	},
        _ => vec![gate],
    }
}


/* Example run:

exponent = 7
free_wire = Cell<12>

output     <- 12
free_wire  <- Cell<13> 
output_rec <- 13
exp(wire_id, 3, Cell<13>)
   output     <- 13
   free_wire  <- Cell<14>
   output_rec <- 14
   gates = exp(wire_id, 1, Cell<14>)
        return Vec<Copy(14,wire_id)>
   Gate::Mul(15, 14, 14)    // squaring
   Gate::Mul(13,wire_id,15) // 3 was odd
Gate::Mul(16, 13, 13)    // squaring
Gate::Mul(12,wire_id,16) // 7 was odd    
**/

fn exp(wire_id:WireId,exponent:BigUint,free_wire:&Cell<WireId>) -> Vec<Gate>{
    if exponent == BigUint::from(1u32) {
	let wire = tmp_wire(free_wire);
	return vec![Copy(wire,wire_id)];
    }
    let output      = tmp_wire(free_wire); // We reserve the first available wire for our own output
    let output_rec  = free_wire.get(); // We remember where the recursive call will place its output
    let big_int_div = BigInt::from(exponent.clone()).div_floor(&BigInt::from(2u32));
    let mut gates   = exp(wire_id, BigUint::try_from(big_int_div).ok().unwrap(), free_wire);

    // Exponent was even: we just square the result of the recursive call
    if exponent.clone() % BigUint::from(2u32) == BigUint::from(0u32) {
	gates.push(Gate::Mul(output,output_rec,output_rec));
    }
    else{ // Exponent was odd: we square the result of the recursive call and multiply by wire_id
	let temp_output = tmp_wire(free_wire);
	gates.push(Gate::Mul(temp_output, output_rec,output_rec));
	gates.push(Gate::Mul(output, wire_id, temp_output));
    }
    return gates;
}
    
fn swap_gate_outputs(gate:Gate,
                     map:&HashMap<WireId,WireId>,
                     defined_outputs:&mut Vec<WireId>) -> Gate {
	match gate{
	   Gate::AnonCall(output_wires,input_wires,instance_count,witness_count,subcircuit) =>
	    {
		let mut new_wires = Vec::new();
		for wire in output_wires {
       		    let temp_wire =
			match wire{
			    Wire(wire_id) => Wire(*map.get(&wire_id).unwrap()),
			    WireRange(s,e) => WireRange(*map.get(&s).unwrap(),*map.get(&e).unwrap())
			};
		    new_wires.push(temp_wire);
		}
		let mut new_inputs = Vec::new();
		for input in input_wires{
		    match input {
		        Wire(wire_id) => {
		            if let Some(w) = map.get(&wire_id){
		        	new_inputs.push(Wire(*w));
		            }
		            else{
		        	new_inputs.push(Wire(wire_id));
		            }
		        },
		        WireRange(s,e) => {
		            for i in s..e+1{
		        	if let Some(w) = map.get(&i){
		        	    new_inputs.push(Wire(*w));
		        	}
		        	else{
		        	    new_inputs.push(Wire(i));
		        	}
		            }
		        }
                    }
		}
		Gate::AnonCall(new_wires,new_inputs,instance_count,witness_count,subcircuit.clone())
		
	    },
	    Gate::Call(name,output_wires,input_wires) => {
		let mut new_wires = Vec::new();
		for wire in output_wires {
    		    let temp_wire =
			match wire{
			    Wire(wire_id) => Wire(*map.get(&wire_id).unwrap()),
			    WireRange(s,e) => WireRange(*map.get(&s).unwrap(),*map.get(&e).unwrap())
			};
		    new_wires.push(temp_wire);
		}
		let mut new_inputs = Vec::new();
		for input in input_wires{
		    match input {
			Wire(wire_id) => {
			    if let Some(w) = map.get(&wire_id){
				new_inputs.push(Wire(*w));
			    }
			    else{
				new_inputs.push(Wire(wire_id));
			    }
			},
			WireRange(s,e) => {
			    for i in s..e+1{
				if let Some(w) = map.get(&i){
				    new_inputs.push(Wire(*w));
				}
				else{
				    new_inputs.push(Wire(i));
				}
			    }
			}
		    }
		}
		Gate::Call(name,new_wires,new_inputs)
	    },
	    Gate::Switch(wire_id, output_wires , values, cases) => {
		let mut new_wires = Vec::new();
		for wire in output_wires {
		    let temp_wire =
			match wire{
			    Wire(wire_id) => Wire(*map.get(&wire_id).unwrap()),
			    WireRange(s,e) => WireRange(*map.get(&s).unwrap(),*map.get(&e).unwrap())
			};
			    
		    new_wires.push(temp_wire);
		}
		Gate::Switch(wire_id,new_wires,values,cases)
	    
	    },
	    Gate::For(name,start_val,end_val,output_wires,body) => {
		let mut new_wires = Vec::new();
		for wire in output_wires {
		    let temp_wire =
			match wire{
			    Wire(wire_id) => Wire(*map.get(&wire_id).unwrap()),
			    WireRange(start,end) => WireRange(*map.get(&start).unwrap(),*map.get(&end).unwrap())
			};
		    new_wires.push(temp_wire);
		}
		Gate::For(name,start_val,end_val,new_wires,body)
	    },
	    Gate::Constant(wire_id,value) => {
		defined_outputs.push(wire_id);
		Gate::Constant(*map.get(&wire_id).unwrap(),value)
	    },
	    Gate::Copy(wire_id,value) => {
		defined_outputs.push(wire_id);
		Gate::Copy(*map.get(&wire_id).unwrap(),value)
	    },
	    Gate::Add(wire_id_out,wire_id1,wire_id2) => {
		defined_outputs.push(wire_id_out);
		let mut in_1:WireId = wire_id1;
		let mut in_2:WireId = wire_id2;
		if let Some(w) = map.get(&in_1){
		    in_1 = *w;
		}
		if let Some(w) = map.get(&in_2){
		    in_2 =* w;
		}

		Gate::Add(*map.get(&wire_id_out).unwrap(),in_1,in_2)
	    },
   	    Gate::Mul(wire_id_out,wire_id1,wire_id2) => {
		defined_outputs.push(wire_id_out);
		let mut in_1:WireId = wire_id1;
		let mut in_2:WireId = wire_id2;
		if let Some(w) = map.get(&in_1){
		    in_1 = *w;
		}
		if let Some(w) = map.get(&in_2){
		    in_2 = *w;
		}
		Gate::Mul(*map.get(&wire_id_out).unwrap(),in_1,in_2)
	    },
    	    Gate::AddConstant(wire_id_out,wire_id_in,value) => {
		defined_outputs.push(wire_id_out);
		let mut in_1:WireId = wire_id_in;
		if let Some(w) = map.get(&in_1){
		    in_1 = *w;
		}
		Gate::AddConstant(*map.get(&wire_id_out).unwrap(),in_1,value)
	    },
    	    Gate::MulConstant(wire_id_out,wire_id_in, value) => {
		defined_outputs.push(wire_id_out);
		let mut in_1:WireId = wire_id_in;
		if let Some(w) = map.get(&in_1){
		    in_1 = *w;
		}
		Gate::MulConstant(*map.get(&wire_id_out).unwrap(),in_1,value)
	    },
    	    Gate::And(wire_id_out,wire_id1,wire_id2) => {
		defined_outputs.push(wire_id_out);
		let mut in_1:WireId = wire_id1;
		let mut in_2:WireId = wire_id2;
		if let Some(w) = map.get(&in_1){
		    in_1 = *w;
		}
		if let Some(w) = map.get(&in_2){
		    in_2 =* w;
		}
		Gate::And(*map.get(&wire_id_out).unwrap(),in_1,in_2)
	    },
    	    Gate::Xor(wire_id_out, wire_id1, wire_id2) => {
		defined_outputs.push(wire_id_out);
		let mut in_1:WireId = wire_id1;
		let mut in_2:WireId = wire_id2;
		if let Some(w) = map.get(&in_1){
		    in_1 = *w;
		}
		if let Some(w) = map.get(&in_2){
		    in_2 = *w;
		}
		Gate::Xor(*map.get(&wire_id_out).unwrap(),in_1,in_2)
	    },	    
    	    Gate::Not(wire_id,value) => {
		defined_outputs.push(wire_id);
		Gate::Not(*map.get(&wire_id).unwrap(),value)
	    },
    	    Gate::Instance(wire_id) => {
		defined_outputs.push(wire_id);
		Gate::Instance(*map.get(&wire_id).unwrap())
	    },
    	    Gate::Witness(wire_id) => {
		defined_outputs.push(wire_id);
		Gate::Witness(*map.get(&wire_id).unwrap())
	    },
    	    Gate::Free(wire_id,value) => {
		defined_outputs.push(wire_id);
		Gate::Free(*map.get(&wire_id).unwrap(),value)
	    },
	    Gate::AssertZero(wire_id) => {
		if let Some(w) = map.get(&wire_id){
		    return Gate::AssertZero(*w);
		}
		else{
		    return Gate::AssertZero(wire_id);
		}
	    }
	}
    }





/*
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
*/

pub fn flatten_relation(relation : &Relation) -> Relation {
    let field_order = relation.header.field_characteristic.clone();
    let known_functions = get_known_functions(&relation);
    let known_iterators = Default::default();

    let flattened_gates:Vec<Gate> = relation.gates.iter().flat_map(move |inner_gate| flatten_gate(
        inner_gate.clone(),
        &known_functions,
        &known_iterators,
        &Cell::new(TEMPORARY_WIRES_START),
        field_order.clone(),
        &mut Vec::new(),
        &mut Vec::new(),
        &Cell::new(0),
        &Cell::new(0)
    )).collect::<Vec<Gate>>();

    return
        Relation {
            header   : relation.header.clone(),
            gate_mask: relation.gate_mask,
            feat_mask: relation.feat_mask,
            functions: Vec::new(),
            gates: flattened_gates,
        };

}
