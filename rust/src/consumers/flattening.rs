use crate::structs::wire::WireListElement;
use std::convert::TryFrom;
use crate::{Gate, WireId};
use std::iter;
use std::collections::HashMap;
use crate::structs::subcircuit::translate_gates;
 use crate::Value;
use crate::structs::wire::{expand_wirelist,WireList,WireListElement::{Wire,WireRange}};
use std::cell::Cell;
use crate::structs::gates::Gate::*;
use crate::structs::function::{ForLoopBody,CaseInvoke};
use crate::structs::iterators::evaluate_iterexpr_list;
use num_bigint::{BigUint, ToBigUint, BigInt};
use num_integer::Integer;

// (output_count, input_count, instance_count, witness_count, subcircuit)
type FunctionDeclare = (usize, usize, usize, usize, Vec<Gate>);

pub fn flatten_gate(
    gate: Gate,
    known_functions: &HashMap<String, FunctionDeclare>,
    known_iterators: &HashMap<String, u64>,
    free_temporary_wire: &Cell<WireId>,
    modulus: Value
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

            let outputs = translate_gates(&subcircuit, &mut output_input_wires, free_temporary_wire)
                .flat_map(move |inner_gate| flatten_gate(inner_gate, known_functions, known_iterators, free_temporary_wire, modulus.clone()))
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
		    modulus.clone() );
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
				modulus.clone()
                            )
                        ).collect::<Vec<Gate>>()
                }).collect::<Vec<Gate>>();
	    outputs
        }

        Gate::Switch(wireId, output_wires, cases, branches) => {
	    let mut instance_counts = Vec::new();
	    let mut witness_counts = Vec::new();
	    let mut global_gates = Vec::new();
   	    let expanded_output_wires = expand_wirelist(&output_wires);

	    
	    for branch in branches{
		//maps inner context to outer context
		//also get input, witness count
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
		    CaseInvoke::AbstractAnonCall(input_wires,instance_count,witness_count,branch) => {
			let expanded_input_wires = expand_wirelist(&input_wires);
			let mut output_input_wires = [expanded_output_wires.clone(), expanded_input_wires].concat();
			instance_counts.push(instance_count);
			witness_counts.push(witness_count);
			let gates = translate_gates(&branch, &mut output_input_wires, free_temporary_wire).collect::<Vec<Gate>>();
			gates
		    }
		   
		 };
		global_gates.push(new_gates);
	    }
	    let mut gates:Vec<Gate> = Vec::new();
   	    let max_instance = *instance_counts.iter().max().unwrap();
	    let max_witness = *witness_counts.iter().max().unwrap();
	    let mut instance_wires = Vec::new();
	    for _ in 0..max_instance{
		let new_wire = free_temporary_wire.get();
		println!("new instance wire {:?}",new_wire);
		let instance_gate = Gate::Instance(new_wire);
		free_temporary_wire.set(free_temporary_wire.get() + 1);
		instance_wires.push(new_wire);
		gates.push(instance_gate);
	    }

	    let mut witness_wires = Vec::new();
	    for _ in 0..max_witness{
		let new_wire = free_temporary_wire.get();
		println!("new witness wire {:?}", new_wire);
		let witness_gate = Gate::Witness(new_wire);
		witness_wires.push(new_wire);
		free_temporary_wire.set(free_temporary_wire.get() + 1);
		gates.push(witness_gate);
	    }

	    //assign -1 to a wire to be used later - modulus - 1
	    // subtract in little endian
	    let neg_one_wire = free_temporary_wire.get();

	    //	    new_modulus  and exponent vars assigned to modulus - 1
	    //convert little endian Vec<u8> to an integer you can subtract from
	    let modulus_bigint = BigUint::from_bytes_le(&modulus[..]);
	    let modulus_minus_1 = modulus_bigint - BigUint::from(1u32);
	    let modulus_minus_1_value = modulus_minus_1.to_bytes_le();
	    //convert moudulus_minus_1 to little endian
	    
	    gates.push(Gate::Constant(neg_one_wire,modulus_minus_1_value.clone()));
	    free_temporary_wire.set(free_temporary_wire.get() + 1);

   	    let one_wireId = free_temporary_wire.get();
	    gates.push(Gate::Constant(one_wireId, [1,0,0,0].to_vec()));
	    free_temporary_wire.set(free_temporary_wire.get() + 1);

	    
	    let mut temp_maps = Vec::new();
	    for (i,branch_gates) in global_gates.iter().enumerate(){
		/** construct gates for computing 1 - ($0 - 42)^(p-1) **/
		//assign case id to temp wire
		let caseIdWire = free_temporary_wire.get();
		gates.push(Gate::Constant(caseIdWire,cases[i].clone()));
		free_temporary_wire.set(free_temporary_wire.get() + 1);

		//multiply caseIdWire by -1 to get - 42
		let negative_caseId = free_temporary_wire.get();
		gates.push(Gate::Mul(negative_caseId, caseIdWire, neg_one_wire));

		free_temporary_wire.set(free_temporary_wire.get() + 1);

		//compute $0 - 42 
		let baseWire = free_temporary_wire.get();
		gates.push(Gate::Add(baseWire, wireId, negative_caseId));
		free_temporary_wire.set(free_temporary_wire.get() + 1);
		let exp_wire = free_temporary_wire.get();
		//(base_wire)^(p - 1) - base_wire has value of ($0 - 42)
		//exp outputs its value to free_temporary_wire at the point of call
		let exp_as_int = BigUint::from_bytes_le(&modulus_minus_1_value[..]);
		gates = [gates,exp(baseWire,exp_as_int,free_temporary_wire)].concat();


		let neg_exp_wire = free_temporary_wire.get();
		gates.push(Gate::Mul(neg_exp_wire,neg_one_wire,exp_wire));
		free_temporary_wire.set(free_temporary_wire.get() + 1);


		//		gates.push(Gate::AddConstant(free_temporary_wire.get(), neg_exp_wire, [1,0,0,0].to_vec()));
		gates.push(Gate::Add(free_temporary_wire.get(), neg_exp_wire, one_wireId));
		let weight_wire_id = free_temporary_wire.get();
		free_temporary_wire.set(free_temporary_wire.get() + 1);
		//********* end weight ************************/
		let mut map:HashMap<WireId,WireId> = HashMap::new();
		for o in &expanded_output_wires{
		    map.insert(*o,free_temporary_wire.get());
		    free_temporary_wire.set(free_temporary_wire.get() + 1);
		}
		//nnswap global outputs for the temp outputs of the branch in each gate
		let mut instance_counter = 0;
		let mut witness_counter = 0;
		let mut defined_outputs = Vec::new();
		println!("map {:?}", &map);
		 for inner_gate in branch_gates{
		     let new_gate = swap_gate_outputs(inner_gate.clone(),&map,&mut instance_wires,&mut witness_wires,instance_counter,witness_counter,&mut defined_outputs);
		     println!("original gate line 242 {:?} \n", inner_gate);
		     println!("new gate line 243 {:?} \n", new_gate);
		     //if it is assert_zero, then add a multiplication gate before it
		     match new_gate{
			 Gate::AssertZero(wireId) => {
			     let new_temp_wire = free_temporary_wire.get();
			     gates.push(Gate::Mul(new_temp_wire, wireId, weight_wire_id));
			     gates.push(Gate::AssertZero(new_temp_wire));
			     free_temporary_wire.set(free_temporary_wire.get() + 1);
			 },
			 _ => {
			     gates.push(new_gate.clone());
			 }
		     }
		 }
		//at this point, we have replaced outputs for temps in all top level gates
		//now for  each temp wire in map, we want to multiply it by weight_wire_id and assign it to a new temp wire
		let mut new_temps = Vec::new();
		let mut output_wires_vec = Vec::new();
		for o in defined_outputs{
		    let new_output_temp = free_temporary_wire.get();
		    let temp = map.get(&o).unwrap();


		    gates.push(Gate::Mul(new_output_temp,*temp,weight_wire_id));
		    //reassign output to weighted temp
		    new_temps.push(new_output_temp);
		    output_wires_vec.push(o);
		    free_temporary_wire.set(free_temporary_wire.get() + 1);
		}
		for (index,temp) in new_temps.iter().enumerate(){
		    map.insert(output_wires_vec[index],new_temps[index]);
		}
		temp_maps.push(map);
	    }
	    

	    for output in &expanded_output_wires{
		let mut left_summand = free_temporary_wire.get();
		gates.push(Gate::Constant(left_summand, vec![0,0,0,0]));
		free_temporary_wire.set(free_temporary_wire.get() + 1);
		let mut new_temp = 0;
		for map in &temp_maps{
		    new_temp = free_temporary_wire.get();
		    gates.push(Gate::Add(new_temp, left_summand, *map.get(&output).unwrap()));
		    left_summand = new_temp;
		    free_temporary_wire.set(new_temp + 1);
		}
		gates.push(Gate::Copy(*output,new_temp));
	    }

	    
	    let outputs = gates.iter().flat_map(move |inner_gate| flatten_gate(inner_gate.clone(),known_functions,known_iterators,free_temporary_wire,modulus.clone())).collect();

	    outputs
	    
	},
        _ => vec![gate],
    }
}

fn get_wire_id(wire:WireListElement) -> Vec<u64>{
    match wire{
	Wire(wireId) => vec![wireId],
	WireRange(s,e) => {
	    let mut v  = Vec::new();
	    for i in s..e{
		v.push(i);
	    }
	    v
	}
    }
}
/* modulus = 7
free_wire = Cell<12>

output <- 12
free_wire <-13 
output_rec <- 13
exp(wireId, 3, Cell<13>)
   output <- Cell<13>
   free_wire <- Cell<14>
   output_rec <- Cell<14>
   gates = exp(wireId, 1, Cell<14>)
        return Vec<Copy(14,wireId)>
   gates.push((Gate::Mul(15, 14, 14)))
   Gate::Mul(13,wireId,15)
Mul(16, 13, 13) -> Mul(next temp wire, output, output)

    


  **/
fn exp(wireId:WireId,exponent:BigUint,free_wire:&Cell<WireId>) -> Vec<Gate>{
    if  exponent == BigUint::from(1u32){
	let wire = free_wire.get();
	free_wire.set(free_wire.get() + 1);
	return vec![Copy(wire,wireId)];
    }
    let output = free_wire.get();
    free_wire.set(free_wire.get() + 1);

    let output_rec = free_wire.get();
    let bigIntDiv = BigInt::from(exponent.clone()).div_floor(&BigInt::from(2u32));
    let mut gates = exp(wireId,BigUint::try_from(bigIntDiv).ok().unwrap(),free_wire);

    if(exponent.clone() % BigUint::from(2u32) == BigUint::from(0u32)){
	gates.push(Gate::Mul(output,output_rec,output_rec));
    }
    else{
	let temp_output = free_wire.get();
	free_wire.set(free_wire.get() + 1);
	gates.push(Gate::Mul(temp_output, output_rec,output_rec));
	gates.push(Gate::Mul(output, wireId, temp_output));
    }
    return gates;
}
    fn get_output_wire_list(gate:Gate) -> WireList{
	match gate {
	    Gate::AnonCall(output_wires,input_wires,instance_count,witness_count,subcircuit) => output_wires,
    	    Gate::Call(name,output_wires,input_wires) => output_wires,
	    Gate::Switch(wireId,wireList,values,cases) => wireList,
	    Gate::For(name,start_val,end_val,global_output_list,body) => global_output_list,
	    _ => vec![]
	}
    }
    
fn swap_gate_outputs(gate:Gate,map:&HashMap<WireId,WireId>,instance_wires:&mut Vec<WireId>, witness_wires:&mut Vec<WireId>,mut witness_counter:u64, mut instance_counter: u64,defined_outputs:&mut Vec<WireId>) -> Gate {
	match gate{
	   Gate::AnonCall(output_wires,input_wires,instance_count,witness_count,subcircuit) =>
	    {
		let mut new_wires = Vec::new();
		for wire in output_wires {
       		    let temp_wire =
			match wire{
			    Wire(wireId) => Wire(*map.get(&wireId).unwrap()),
			    WireRange(s,e) => WireRange(*map.get(&s).unwrap(),*map.get(&e).unwrap())
			};
		    new_wires.push(temp_wire);
		}
		let mut new_inputs = Vec::new();
		for input in input_wires{
		    let new_input =
			match input {
			    Wire(wireId) => {
				if let Some(w) = map.get(&wireId){
				    new_inputs.push(Wire(*w));
				}
				else{
				    new_inputs.push(Wire(wireId));
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
			};
		}
		Gate::AnonCall(new_wires,new_inputs,instance_count,witness_count,subcircuit.clone())
		
	    },
	    Gate::Call(name,output_wires,input_wires) => {
		let mut new_wires = Vec::new();
		for wire in output_wires {
    		    let temp_wire =
			match wire{
			    Wire(wireId) => Wire(*map.get(&wireId).unwrap()),
			    WireRange(s,e) => WireRange(*map.get(&s).unwrap(),*map.get(&e).unwrap())
			};
		    new_wires.push(temp_wire);
		}
		let mut new_inputs = Vec::new();
		for input in input_wires{
		    let new_input =
			match input {
			    Wire(wireId) => {
				if let Some(w) = map.get(&wireId){
				    new_inputs.push(Wire(*w));
				}
				else{
				    new_inputs.push(Wire(wireId));
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
			};
		}
		Gate::Call(name,new_wires,new_inputs)
	    },
	    Gate::Switch(wireId, output_wires , values, cases) => {
		let mut new_wires = Vec::new();
		for wire in output_wires {
		    let temp_wire =
			match wire{
			    Wire(wireId) => Wire(*map.get(&wireId).unwrap()),
			    WireRange(s,e) => WireRange(*map.get(&s).unwrap(),*map.get(&e).unwrap())
			};
			    
		    new_wires.push(temp_wire);
		}
		Gate::Switch(wireId,new_wires,values,cases)
	    
	    },
	    Gate::For(name,start_val,end_val,output_wires,body) => {
		let mut new_wires = Vec::new();
		for wire in output_wires {
		    let temp_wire =
			match wire{
			    Wire(wireId) => Wire(*map.get(&wireId).unwrap()),
			    WireRange(start,end) => WireRange(*map.get(&start).unwrap(),*map.get(&start).unwrap())
			};
		    new_wires.push(temp_wire);
		}
		Gate::For(name,start_val,end_val,new_wires,body)
	    },
	    Gate::Constant(wireId,value) => {
		defined_outputs.push(wireId);
		Gate::Constant(*map.get(&wireId).unwrap(),value)
	    },
	    Gate::Copy(wireId,value) => {
		defined_outputs.push(wireId);
		Gate::Copy(*map.get(&wireId).unwrap(),value)
	    },
	    Gate::Add(wireIdOut,wireId1,wireId2) => {
		defined_outputs.push(wireIdOut);
		let mut in_1:WireId = wireId1;
		let mut in_2:WireId = wireId2;
		if let Some(w) = map.get(&in_1){
		    in_1 = *w;
		}
		if let Some(w) = map.get(&in_2){
		    in_2 =* w;
		}

		Gate::Add(*map.get(&wireIdOut).unwrap(),in_1,in_2)
	    },
   	    Gate::Mul(wireIdOut,wireId1,wireId2) => {
		defined_outputs.push(wireIdOut);
		let mut in_1:WireId = wireId1;
		let mut in_2:WireId = wireId2;
		if let Some(w) = map.get(&in_1){
		    in_1 = *w;
		}
		if let Some(w) = map.get(&in_2){
		    in_2 = *w;
		}
		Gate::Mul(*map.get(&wireIdOut).unwrap(),in_1,in_2)
	    },
    	    Gate::AddConstant(wireIdOut,wireIdIn,value) => {
		defined_outputs.push(wireIdOut);
		let mut in_1:WireId = wireIdIn;
		if let Some(w) = map.get(&in_1){
		    in_1 = *w;
		}
		Gate::AddConstant(*map.get(&wireIdOut).unwrap(),in_1,value)
	    },
    	    Gate::MulConstant(wireIdOut,wireIdIn, value) => {
		defined_outputs.push(wireIdOut);
		let mut in_1:WireId = wireIdIn;
		if let Some(w) = map.get(&in_1){
		    in_1 = *w;
		}
		Gate::MulConstant(*map.get(&wireIdOut).unwrap(),in_1,value)
	    },
    	    Gate::And(wireIdOut,wireId1,wireId2) => {
		defined_outputs.push(wireIdOut);
		let mut in_1:WireId = wireId1;
		let mut in_2:WireId = wireId2;
		if let Some(w) = map.get(&in_1){
		    in_1 = *w;
		}
		if let Some(w) = map.get(&in_2){
		    in_2 =* w;
		}
		Gate::And(*map.get(&wireIdOut).unwrap(),in_1,in_2)
	    },
    	    Gate::Xor(wireIdOut, wireId1, wireId2) => {
		defined_outputs.push(wireIdOut);
		let mut in_1:WireId = wireId1;
		let mut in_2:WireId = wireId2;
		if let Some(w) = map.get(&in_1){
		    in_1 = *w;
		}
		if let Some(w) = map.get(&in_2){
		    in_2 = *w;
		}
		Gate::Xor(*map.get(&wireIdOut).unwrap(),in_1,in_2)
	    },	    
    	    Gate::Not(wireId,value) => {
		defined_outputs.push(wireId);
		Gate::Not(*map.get(&wireId).unwrap(),value)
	    },
    	    Gate::Instance(wireId) => {
		defined_outputs.push(wireId);
		let instance_wire = instance_wires[instance_counter as usize];
		Gate::Copy(*map.get(&wireId).unwrap(),instance_wire)
	    },
    	    Gate::Witness(wireId) => {
		defined_outputs.push(wireId);
		let witness_wire = witness_wires[witness_counter as usize];
		Gate::Copy(*map.get(&wireId).unwrap(),witness_wire)
	    },
    	    Gate::Free(wireId,value) => {
		defined_outputs.push(wireId);
		Gate::Free(*map.get(&wireId).unwrap(),value)
	    },
	    Gate::AssertZero(wireId) => {
		if let Some(w) = map.get(&wireId){
		    return Gate::AssertZero(*w);
		}
		else{
		    return Gate::AssertZero(wireId);
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
