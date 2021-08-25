use std::cmp;
use std::collections::HashMap;
use std::cell::Cell;
use num_bigint::{BigUint, BigInt};
use num_integer::Integer;
use crate::structs::subcircuit::translate_gates;
use crate::structs::wire::{expand_wirelist, WireListElement, WireListElement::{Wire}};
use crate::structs::gates::Gate::*;
use crate::structs::function::{ForLoopBody,CaseInvoke};
use crate::structs::iterators::evaluate_iterexpr_list;
use crate::structs::relation::{contains_feature, get_known_functions, Relation, BOOL, ARITH, SIMPLE};
use crate::{Gate, WireId, Value};
use crate::consumers::validator::Validator;

// (output_count, input_count, instance_count, witness_count, subcircuit)
type FunctionDeclare = (usize, usize, usize, usize, Vec<Gate>);

// A struct for the arguments of the flattening function that don't change with recursive calls
// (otherwise the calls become very heavy...)

struct FlatArgs<'a> {
    modulus    : &'a Value,
    is_boolean : bool,  // whether it's a Boolean circuit (as opposed to an arithmetic one)
    one        : &'a Value, // Don't want to compute 1 and -1 at every recursive call...
    minus_one  : &'a Value,
    known_functions : &'a HashMap<String, FunctionDeclare>, // These are fixed throughout the traversal
    instance_wires  : Vec<WireId>, // All instance data pulled before gate (incl. in previous switch branches)
    witness_wires   : Vec<WireId>, // All witness data pulled before gate (incl. in previous switch branches)
    instance_counter: Cell<u64>,   // How many instance pulls have been done before gate (excl. in previous switch branches), semantically (can be smaller than instance_wires length)
    witness_counter : Cell<u64>,   // How many instance pulls have been done before gate (excl. in previous switch branches), semantically (can be smaller than witness_wires length)
    output_gates    : &'a mut Vec<Gate>, // Accumulator where we have written the flattened version of the gates seen before gate, on top of which we'll push the flattened version of gate
}

// Getting a new temp wire id (u64).
// Looks at a reference containing the first available wire id; bumping it by 1.

pub fn tmp_wire(free_wire: &Cell<WireId>) -> u64 {
    let new_wire = free_wire.get();
    free_wire.set(free_wire.get() + 1);
    new_wire
}

// Useful auxiliary functions

fn minus(modulus: &Value, x : &Value) -> Value {
    //convert little endian Vec<u8> to an integer you can subtract from
    let modulus_bigint  = BigUint::from_bytes_le(&modulus[..]);
    let x_bigint        = BigUint::from_bytes_le(&x[..]);
    let modulus_minus_x = modulus_bigint - x_bigint;
    //convert moudulus_minus_x to little endian
    return modulus_minus_x.to_bytes_le()
}


fn add(is_boolean : bool, output : WireId, lhs : WireId, rhs : WireId, gates : &mut Vec<Gate>) {
    if is_boolean {
        gates.push(Gate::Xor(output, lhs, rhs));
    } else {
        gates.push(Gate::Add(output, lhs, rhs));
    }
}

fn add_c(is_boolean : bool, output : WireId, input : WireId, cst : Value, free_temporary_wire: &Cell<WireId>, gates : &mut Vec<Gate>) {
    if is_boolean {
        let tmp = tmp_wire(free_temporary_wire);
        gates.push(Gate::Constant(tmp, cst));
        gates.push(Gate::Xor(output, input, tmp));
    } else {
        gates.push(Gate::AddConstant(output, input, cst));
    }
}

fn mul(is_boolean : bool, output : WireId, lhs : WireId, rhs : WireId, gates : &mut Vec<Gate>) {
    if is_boolean {
        gates.push(Gate::And(output, lhs, rhs));
    } else {
        gates.push(Gate::Mul(output, lhs, rhs));
    }
}

fn mul_c(is_boolean : bool, output : WireId, input : WireId, cst : Value, free_temporary_wire: &Cell<WireId>, gates : &mut Vec<Gate>) {
    if is_boolean {
        let tmp = tmp_wire(free_temporary_wire);
        gates.push(Gate::Constant(tmp, cst));
        gates.push(Gate::And(output, input, tmp));
    } else {
        gates.push(Gate::MulConstant(output, input, cst));
    }
}

pub fn flatten_gate(
    gate: Gate,                                          // The gate to be flattened
    known_iterators: &HashMap<String, u64>,              // The list of iterators previously defined
    known_functions : &HashMap<String, FunctionDeclare>, // defined functions
    free_temporary_wire: &Cell<WireId>,                  // free temporary wires
    modulus    : &Value,                                 // modulus used
    is_boolean : bool,  // whether it's a Boolean circuit (as opposed to an arithmetic one)
    one        : Option<Value>, // If 1 and -1 are given by the caller, then use them, otherwise compute them
    minus_one  : Option<Value>,
) -> Vec<Gate> {
    let mut ret: Vec<Gate> = Vec::new();
    let one = one.unwrap_or_else(|| vec![1]);
    let minus_one = minus_one.unwrap_or_else(|| minus(modulus, &one));
    let mut args = FlatArgs {
        modulus,
        is_boolean,
        one: &one,
        minus_one: &minus_one,
        known_functions,
        instance_wires: vec![],
        witness_wires: vec![],
        instance_counter: Cell::new(0),
        witness_counter: Cell::new(0),
        output_gates: &mut ret,
    };


    flatten_gate_internal(
        gate,
        known_iterators,
        free_temporary_wire,
        &mut args,
    );

    ret
}

fn flatten_gate_internal(
    gate: Gate,                             // The gate to be flattened
    known_iterators: &HashMap<String, u64>, // This changes in recursive calls
    free_temporary_wire: &Cell<WireId>, // Cell containing the id of the first available temp wire; acts as a global ref
    args: &mut FlatArgs                     // The other arguments that don't change in recursive calls
) {
    match gate {

        Gate::Instance(wire_id) => {
            let ic = args.instance_counter.get() as usize;
            if ic < args.instance_wires.len() { // The instance data we want has already been pulled and is stored in a wire; we just copy it.
                let instance_wire = args.instance_wires[ic];
                args.instance_counter.set(args.instance_counter.get() + 1);
                args.output_gates.push(Gate::Copy(wire_id, instance_wire));
            } else { // The instance data we want hasn't been pulled yet; we pull it with an Instance gate.
                args.instance_wires.push(wire_id);
                args.instance_counter.set(args.instance_counter.get() + 1);
                args.output_gates.push(gate);
            }
        },

        Gate::Witness(wire_id) => { // The witness data we want has already been pulled and is stored in a wire; we just copy it.
            let wc = args.witness_counter.get() as usize;
            if wc < args.witness_wires.len() {
                let witness_wire = args.witness_wires[wc];
                args.witness_counter.set(args.witness_counter.get() + 1);
                args.output_gates.push(Gate::Copy(wire_id, witness_wire));
            } else { // The witness data we want hasn't been pulled yet; we pull it with a Witness gate.
                args.witness_wires.push(wire_id);
                args.witness_counter.set(args.witness_counter.get() + 1);
                args.output_gates.push(gate);
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

            for inner_gate in translate_gates(&subcircuit, &mut output_input_wires, free_temporary_wire) {
                flatten_gate_internal(inner_gate, known_iterators, free_temporary_wire, args);
            }
        }

        Gate::Call(name, output_wires, input_wires) => {
            if let Some(declaration) = args.known_functions.get(&name) {
                // When a function is 'executed', then it's a completely new context, meaning
                // that iterators defined previously, will not be forwarded to the function.
                // This is why we set an empty (HashMap::default()) set of iterators.
                flatten_gate_internal(
                    AnonCall(output_wires, input_wires, declaration.2, declaration.3, declaration.4.clone()),
                    &HashMap::default(),
                    free_temporary_wire,
                    args);
            } else {
                // The function is not known, so this should either panic, or return an empty vector.
                // We will consider that this means the original circuit is not valid, while
                // it should have been tested beforehand
            }
        }

        Gate::For(
            iterator_name,
            start_val,
            end_val,
            _,
            body
        ) => {
            let range = (start_val as usize..=(end_val as usize)).into_iter();
            for i in range {
                let mut local_known_iterators = known_iterators.clone();
                local_known_iterators.insert(iterator_name.clone(), i as u64);

                let (subcircuit, expanded_outputs, expanded_inputs, use_same_context) = match &body {
                    ForLoopBody::IterExprCall(name, outputs, inputs) => {
                        let expanded_outputs = evaluate_iterexpr_list(&outputs, &local_known_iterators);
                        let expanded_inputs  = evaluate_iterexpr_list(&inputs, &local_known_iterators);

                        if let Some(declaration) = args.known_functions.get(name) {
                            // When a function is 'executed', then it's a completely new context, meaning
                            // that iterators defined previously, will not be forwarded to the function.
                            (&declaration.4, expanded_outputs, expanded_inputs, false)
                        } else {
                            // The function is not known, so this should either panic, or return an empty vector.
                            // We will consider that this means the original circuit is not valid, while
                            // it should have been tested beforehand
                            panic!();
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
                for inner_gate in translate_gates(subcircuit, &mut output_input_wires, free_temporary_wire) {
                    flatten_gate_internal(
                        inner_gate,
                        if use_same_context {&local_known_iterators} else {&null_hashmap},
                        free_temporary_wire,
                        args);
                }
            }
        }

        Gate::Switch(wire_id, output_wires, cases, branches) => {
            let expanded_output_wires = expand_wirelist(&output_wires);

            let mut global_gates = Vec::new(); // vector of vectors of gates, the inner gates of each branch

            let instance_counter_at_start = args.instance_counter.get();
            let witness_counter_at_start  = args.witness_counter.get();
            let mut max_instance = 0; // how many instance pulls will be done by the switch
            let mut max_witness  = 0; // how many witness pulls will be done by the switch
            
            // println!("Reducing {:?} <- switch {:?} [{:?}]\n", expanded_output_wires, wire_id, cases);
            for branch in branches {
                // println!("Initial pass rewrites\n{:?}", branch);
                // maps inner context to outer context
                // also computes max_instance, max_witness
                match branch {
                    CaseInvoke::AbstractGateCall(name, input_wires) => {
                        if let Some(declaration) = args.known_functions.get(&name){
                            let expanded_input_wires = expand_wirelist(&input_wires);
                            let mut output_input_wires = [expanded_output_wires.clone(), expanded_input_wires].concat();
                            max_instance = cmp::max(max_instance, declaration.2);
                            max_witness  = cmp::max(max_witness, declaration.3);
                            let gates = translate_gates(&declaration.4, &mut output_input_wires, free_temporary_wire).collect::<Vec<Gate>>();
                            global_gates.push(gates);
                        } else {
                            panic!();
                        }
                    },
                    CaseInvoke::AbstractAnonCall(input_wires, instance_count, witness_count, branch) => {
                        let expanded_input_wires = expand_wirelist(&input_wires);
                        let mut output_input_wires = [expanded_output_wires.clone(), expanded_input_wires].concat();
                        max_instance = cmp::max(max_instance, instance_count);
                        max_witness  = cmp::max(max_witness, witness_count);
                        let gates = translate_gates(&branch, &mut output_input_wires, free_temporary_wire).collect::<Vec<Gate>>();
                        global_gates.push(gates);
                    }
                }
            }
            
            let mut temp_maps = Vec::new();

            for (i,branch_gates) in global_gates.iter().enumerate() {

                let mut new_branch_gates = Vec::new();

                // println!("Second pass rewrites {:?}", branch_gates);

                /****** the first thing we do is compute the weight of the branch, ***********/
                /* i.e. a wire assigned the value 1 - ($0 - 42)^(p-1) for a switch on $0, case value 42,
                where p is the field's characteristic */

                let minus_val = minus(&args.modulus, &cases[i]);

                //compute $0 - 42, assigning it to base_wire
                let base_wire = tmp_wire(free_temporary_wire);
                add_c(args.is_boolean, base_wire, wire_id, minus_val, free_temporary_wire, &mut new_branch_gates);

                // Now we do the exponentiation base_wire^(p - 1), where base_wire has been assigned value ($0 - 42),
                // calling the fast exponentiation function exp, which return a bunch of gates doing the exponentiation job.
                // By convention, exp assigns the output value to the first available temp wire at the point of call.
                // Therefore, we remember what this wire is:
                let exp_wire   = free_temporary_wire.get(); // We are not using this wire yet (no need to bump free_temporary_wire, exp will do it)
                let exp_as_uint = BigUint::from_bytes_le(&args.minus_one);
                let exp_as_int  = BigInt::from(exp_as_uint);
                exp(args.is_boolean, base_wire, exp_as_int, free_temporary_wire, &mut new_branch_gates);

                // multiply by -1 to compute (- ($0 - 42)^(p-1))
                let neg_exp_wire = tmp_wire(free_temporary_wire);
                mul_c(args.is_boolean, neg_exp_wire, exp_wire, args.minus_one.clone(), free_temporary_wire, &mut new_branch_gates);

                // Adding 1 to compute (1 - ($0 - 42)^(p-1))
                let weight_wire_id = tmp_wire(free_temporary_wire);
                add_c(args.is_boolean, weight_wire_id, neg_exp_wire, args.one.clone(), free_temporary_wire, &mut new_branch_gates);
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
                let mut defined_outputs = Vec::new();
                for inner_gate in branch_gates {
                    let new_gate = swap_gate_outputs(inner_gate.clone(), &map, &mut defined_outputs);
                    // println!("line 265 {:?} to {:?} \n", inner_gate, new_gate);
                    //if it is assert_zero, then add a multiplication gate before it
                    match new_gate {
                        Gate::AssertZero(wire_id) => {
                            let new_temp_wire = tmp_wire(free_temporary_wire);
                            mul(args.is_boolean, new_temp_wire, wire_id, weight_wire_id, &mut new_branch_gates);
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
                    mul(args.is_boolean, new_output_temp,*temp,weight_wire_id, &mut new_branch_gates);
                    //reassign output to weighted temp
                    new_temps.push(new_output_temp);
                    output_wires_vec.push(o);
                }
                for (index,_temp) in new_temps.iter().enumerate(){
                    map.insert(output_wires_vec[index],new_temps[index]);
                }
                temp_maps.push(map);
                args.instance_counter.set(instance_counter_at_start);
                args.witness_counter.set(witness_counter_at_start);
                for gate in new_branch_gates {
                    flatten_gate_internal(gate, known_iterators, free_temporary_wire, args);
                }
            }
            
            // Now we define the real outputs of the switch as the weighted sums of the branches' outputs
            for output in &expanded_output_wires{
                // Weighted sum starts with 0
                let mut sum_wire = tmp_wire(free_temporary_wire);
                args.output_gates.push(Gate::Constant(sum_wire, vec![0,0,0,0]));
                for map in &temp_maps {
                    let new_temp = tmp_wire(free_temporary_wire);
                    add(args.is_boolean, new_temp, sum_wire, *map.get(&output).unwrap(), args.output_gates);
                    sum_wire = new_temp;
                }
                args.output_gates.push(Gate::Copy(*output,sum_wire));
            }
            args.instance_counter.set(instance_counter_at_start + (max_instance as u64));
            args.witness_counter.set(witness_counter_at_start + (max_witness as u64));
            
        },
        _ => args.output_gates.push(gate),
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

fn exp(is_boolean : bool, wire_id : WireId, exponent : BigInt, free_temporary_wire : &Cell<u64>, gates : &mut Vec<Gate>) {
    if exponent == BigInt::from(1u32) {
        let wire = tmp_wire(free_temporary_wire);
        gates.push(Copy(wire,wire_id));
    } else {
        let output      = tmp_wire(free_temporary_wire); // We reserve the first available wire for our own output
        let output_rec  = free_temporary_wire.get(); // We remember where the recursive call will place its output
        let big_int_div = exponent.div_floor(&BigInt::from(2u32));
        exp(is_boolean, wire_id, big_int_div, free_temporary_wire, gates);

        // Exponent was even: we just square the result of the recursive call
        if exponent % BigInt::from(2u32) == BigInt::from(0u32) {
            mul(is_boolean, output, output_rec, output_rec, gates);
        }
        else{ // Exponent was odd: we square the result of the recursive call and multiply by wire_id
            let temp_output = tmp_wire(free_temporary_wire);
            mul(is_boolean, temp_output, output_rec, output_rec, gates);
            mul(is_boolean, output, wire_id, temp_output, gates);
        }
    }
}

// Applies a map to a list of wires
fn swap_vec(wires: &Vec<WireListElement>,
            map  : &HashMap<WireId,WireId>) -> Vec<WireListElement> {
    let expanded_wires = expand_wirelist(&wires);
    let mut new_wires  = Vec::new();
    for wire in expanded_wires {
        let new_wire =
            if let Some(w) = map.get(&wire){
                Wire(*w)
            } else {
                Wire(wire)
            };
        new_wires.push(new_wire);
    }
    new_wires
}

fn swap_gate_outputs(gate:Gate,
                     map:&HashMap<WireId,WireId>,
                     defined_outputs:&mut Vec<WireId>) -> Gate {
    match gate{
        Gate::AnonCall(output_wires,input_wires,instance_count,witness_count,subcircuit) =>
        {
            let new_outputs = swap_vec(&output_wires,map);
            let new_inputs  = swap_vec(&input_wires,map);
            Gate::AnonCall(new_outputs,new_inputs,instance_count,witness_count,subcircuit.clone())
        },
        Gate::Call(name,output_wires,input_wires) => {
            let new_outputs = swap_vec(&output_wires,map);
            let new_inputs  = swap_vec(&input_wires,map);
            Gate::Call(name,new_outputs,new_inputs)
        },
        Gate::Switch(wire_id, output_wires , values, cases) => {
            let new_outputs = swap_vec(&output_wires,map);
            Gate::Switch(wire_id,new_outputs,values,cases)
        },
        Gate::For(name,start_val,end_val,output_wires,body) => {
            let new_outputs = swap_vec(&output_wires,map);
            Gate::For(name,start_val,end_val,new_outputs,body)
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
                in_2 = *w;
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
                in_2 = *w;
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

// TODO: some functions may be defined in a previous relation, but can be used in this one,
// so give a mutable HashMap instead as parameter.
pub fn flatten_relation_from(relation : &Relation, tmp_wire_start : u64) -> (Relation, u64) {

    let modulus = relation.header.field_characteristic.clone();
    let one : Value = [1,0,0,0].to_vec(); // = True for Boolean circuits
    let minus_one : Value = minus(&modulus, &one); // should work for Booleans too, being True as well
    let known_functions = get_known_functions(&relation);
    let mut flattened_gates = Vec::new();
    let mut args = FlatArgs {
        modulus    : &modulus,
        is_boolean : contains_feature(relation.gate_mask, BOOL),
        one        : &one,
        minus_one  : &minus_one,
        known_functions : &known_functions,
        instance_wires  : Vec::new(),
        witness_wires   : Vec::new(),
        instance_counter: Cell::new(0),
        witness_counter : Cell::new(0),
        output_gates    : &mut flattened_gates
    };


    let known_iterators         = Default::default();
    let mut free_temporary_wire = Cell::new(tmp_wire_start);

    for inner_gate in &relation.gates {
        flatten_gate_internal(inner_gate.clone(), &known_iterators, &mut free_temporary_wire, &mut args);
    }

    (Relation {
        header   : relation.header.clone(),
        gate_mask: if args.is_boolean {BOOL} else {ARITH},
        feat_mask: SIMPLE,
        functions: vec![],
        gates    : flattened_gates,
    },
     free_temporary_wire.get())

}

// TODO make this function return a (Relation, u64) instead
pub fn flatten_relation(relation : &Relation) -> Relation {
    let mut validator = Validator::new_as_verifier();
    validator.ingest_relation(relation);
    let tmp_wire_start = validator.get_tws();
    return flatten_relation_from(relation, tmp_wire_start).0;
}

#[test]
fn test_validate_flattening() -> crate::Result<()> {
    use crate::producers::examples::*;

    let instance = example_instance();
    let witness = example_witness();
    let relation = example_relation();

    let new_relation = flatten_relation(&relation);
    let mut new_val  = Validator::new_as_prover();
    new_val.ingest_instance(&instance);
    new_val.ingest_witness(&witness);
    new_val.ingest_relation(&new_relation);
    let violations = new_val.get_violations();

    assert_eq!(violations, Vec::<String>::new());

    Ok(())
}

#[test]
fn test_evaluate_flattening() -> crate::Result<()> {
    use crate::producers::examples::*;
    use crate::consumers::evaluator::Evaluator;

    let relation = example_relation();
    let instance = example_instance();
    let witness = example_witness();

    let new_relation = flatten_relation(&relation);

    let mut new_simulator = Evaluator::default();
    let _ = new_simulator.ingest_instance(&instance);
    let _ = new_simulator.ingest_witness(&witness);
    let _ = new_simulator.ingest_relation(&new_relation);

    assert_eq!(new_simulator.get_violations().len(), 0);

    Ok(())
}

