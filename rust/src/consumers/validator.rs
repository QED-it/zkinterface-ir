use crate::{FieldId, Gate, Header, Instance, Message, Relation, Result, WireId, Witness};
use num_bigint::BigUint;
use num_traits::identities::One;
use std::collections::{BTreeSet, HashMap, HashSet};

use crate::consumers::evaluator::get_field;
use crate::structs::count::{count_list_max, wirelist_to_count_list, CountList};
use crate::structs::function::{CaseInvoke, ForLoopBody};
use crate::structs::iterators::evaluate_iterexpr_list;
use crate::structs::value::is_probably_prime;
use crate::structs::wire::{
    expand_wirelist, is_one_field_wirelist, wire_ids_to_wirelist, WireList,
};
use regex::Regex;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::rc::Rc;

type Field = BigUint;

// Tips: to write regex, use the following website (and select Python as the type of REGEX
//    https://regex101.com/r/V3ROjH/1

/// Used to check the validity of the version.
const VERSION_REGEX: &str = r"^\d+.\d+.\d+$";
/// Used to check the validity of names of functions / iterators
const NAMES_REGEX: &str = r"^[a-zA-Z_][\w]*(?:(?:\.|:{2})[a-zA-Z_][\w]*)*$";
const IMPLEMENTED_CHECKS: &str = r"
Here is the list of implemented semantic/syntactic checks:

Header Validation
 - Ensure that the characteristic is strictly greater than 1.
 - Ensure that the characteristic is a prime.
 - Ensure that the field degree is exactly 1.
 - Ensure that the version string has the correct format (e.g. matches the following regular expression “^\d+.\d+.\d+$”).
 - Ensure header messages are coherent.
     - Versions should be identical.
     - Field characteristic and field degree should be the same.

Relation Validation
 - Ensure that the defined gateset is either 'arithmetic' (or a subset) or 'boolean' (or a subset).
     - If boolean (or subset), checks that the field characteristic is exactly 2.

Inputs Validation (Instances / Witnesses)
 - Ensure that Instance gates are given a value in Instance messages.
 - Ensure that Witness gates are given a value in Witness messages (prover only).
 - Ensure that all Instance and Witness are consumed at the end of the circuit
 - Ensure that the value they are set to is indeed encoding an element lying in the underlying field.
   For degree 1 fields, it can be achieved by ensuring that the encoded value is strictly smaller than the field characteristic.

Gates Validation
 - Ensure that gates used are coherent with the profile.
   - @not/@and/@xor are not allowed with 'arithmetic'.
   - @add/@addc/@mul/@mulc are not allowed with 'boolean'.
 - Ensure constants given in @addc/@mulc are actual field elements.
 - Ensure input wires of gates map to an already set variable.
 - Enforce Single Static Assignment by checking that the same wire is used only once as an output wire.
 - Ensure that @function/@for/@switch are indeed allowed if they are encountered in the circuit.
 - Ensure that for Free gates of the format @free(first, last), we have (last > first).
 - Ensure that start (first) and stop (last) conditions in loop verify that (last > first).

WireRange Validation
 - Ensure that for WireRange(first, last) that (last > first).
";

#[derive(Clone)]
pub struct Validator {
    as_prover: bool,

    instance_queue_len: CountList,
    witness_queue_len: CountList,
    live_wires: BTreeSet<(FieldId, WireId)>,

    got_header: bool,
    header_version: String,

    fields: Vec<Field>,

    // name => (output_count, input_count, instance_count, witness_count, subcircuit)
    known_functions: Rc<RefCell<HashMap<String, (CountList, CountList, CountList, CountList)>>>,
    known_iterators: Rc<RefCell<HashMap<String, u64>>>,

    violations: Vec<String>,
}

impl Default for Validator {
    fn default() -> Self {
        Self {
            as_prover: Default::default(),
            instance_queue_len: Default::default(),
            witness_queue_len: Default::default(),
            live_wires: Default::default(),
            got_header: Default::default(),
            header_version: Default::default(),
            fields: Default::default(),
            known_functions: Rc::new(RefCell::new(HashMap::default())),
            known_iterators: Rc::new(RefCell::new(HashMap::default())),
            violations: Default::default(),
        }
    }
}

impl Validator {
    pub fn new_as_verifier() -> Validator {
        Validator::default()
    }

    pub fn new_as_prover() -> Validator {
        Validator {
            as_prover: true,
            ..Default::default()
        }
    }

    pub fn print_implemented_checks() {
        println!("{}", IMPLEMENTED_CHECKS);
    }

    pub fn get_violations(mut self) -> Vec<String> {
        self.ensure_all_instance_values_consumed();
        self.ensure_all_witness_values_consumed();
        if !self.live_wires.is_empty() {
            println!("WARNING: few variables were not freed.");
        }
        self.violations
    }

    pub fn get_strict_violations(&self) -> &Vec<String> {
        &self.violations
    }

    pub fn how_many_violations(&self) -> usize {
        self.violations.len()
    }

    pub fn ingest_message(&mut self, msg: &Message) {
        match msg {
            Message::Instance(i) => self.ingest_instance(i),
            Message::Witness(w) => self.ingest_witness(w),
            Message::Relation(r) => self.ingest_relation(r),
        }
    }

    fn ingest_header(&mut self, header: &Header) {
        if self.got_header {
            // in this case, ensure that headers are compatible
            if self.fields.len() != header.fields.len() {
                self.violate("The fields are not consistent across headers.");
            }
            for (previous_field, new_field) in self.fields.iter().zip(header.fields.iter()) {
                if previous_field != &BigUint::from_bytes_le(new_field) {
                    self.violate("The fields are not consistent across headers.");
                    break;
                }
            }

            if self.header_version != header.version {
                self.violate("The profile version is not consistent across headers.");
            }
        } else {
            self.got_header = true;

            // Check validity of field_characteristic
            for field in &header.fields {
                let biguint_field = BigUint::from_bytes_le(field).clone();
                self.fields.push(biguint_field.clone());
                if biguint_field.cmp(&One::one()) != Ordering::Greater {
                    self.violate("All fields should be > 1");
                }
                if !is_probably_prime(field) {
                    self.violate("All fields should be a prime.")
                }
            }

            // check header version
            let re = Regex::new(VERSION_REGEX).unwrap();
            if !re.is_match(header.version.trim()) {
                self.violate("The profile version should match the following format <major>.<minor>.<patch>.");
            }
            self.header_version = header.version.clone();

            // Initialize instance_queue_len and witness_queue_len
            self.instance_queue_len = HashMap::new();
            self.witness_queue_len = HashMap::new();
        }
    }

    pub fn ingest_instance(&mut self, instance: &Instance) {
        self.ingest_header(&instance.header);

        // Provide values on the queue available for Instance gates.
        for (i, instances_per_field) in instance.common_inputs.iter().enumerate() {
            assert!(i <= u8::MAX as usize);
            let i = i as u8;
            // Check values.
            for value in &instances_per_field.inputs {
                self.ensure_value_in_field(&i, value, || format!("instance value {:?}", value));
            }
            let count = self.instance_queue_len.entry(i).or_insert(0);
            *count += instances_per_field.inputs.len() as u64;
        }
    }

    pub fn ingest_witness(&mut self, witness: &Witness) {
        if !self.as_prover {
            self.violate("As verifier, got an unexpected Witness message.");
        }
        self.ingest_header(&witness.header);

        // Provide values on the queue available for Witness gates.
        for (i, witnesses_per_field) in witness.short_witness.iter().enumerate() {
            assert!(i <= u8::MAX as usize);
            let i = i as u8;
            // Check values.
            for value in &witnesses_per_field.inputs {
                self.ensure_value_in_field(&i, value, || format!("witness value {:?}", value));
            }
            let count = self.witness_queue_len.entry(i).or_insert(0);
            *count += witnesses_per_field.inputs.len() as u64;
        }
    }

    pub fn ingest_relation(&mut self, relation: &Relation) {
        self.ingest_header(&relation.header);

        for f in relation.functions.iter() {
            let (name, output_count, input_count, instance_count, witness_count) = (
                f.name.clone(),
                f.output_count.clone(),
                f.input_count.clone(),
                f.instance_count.clone(),
                f.witness_count.clone(),
            );

            // Check that the name follows the proper REGEX
            let re = Regex::new(NAMES_REGEX).unwrap();
            if !re.is_match(name.trim()) {
                self.violate(format!(
                    "The function name ({}) should match the proper format ({}).",
                    name, NAMES_REGEX
                ));
            }

            // Just record the signature first.
            if self.known_functions.borrow().contains_key(&name) {
                self.violate(format!(
                    "A function with the name '{}' already exists",
                    name
                ));
                continue;
            } else {
                self.known_functions.borrow_mut().insert(
                    name.clone(),
                    (
                        output_count.clone(),
                        input_count.clone(),
                        instance_count.clone(),
                        witness_count.clone(),
                    ),
                );
            }
            // Now validate the subcircuit.
            self.ingest_subcircuit(
                &f.body,
                &output_count,
                &input_count,
                &instance_count,
                &witness_count,
                false,
            );
        }

        for gate in &relation.gates {
            self.ingest_gate(gate);
        }
    }

    fn ingest_gate(&mut self, gate: &Gate) {
        use Gate::*;

        match gate {
            Constant(field_id, out, value) => {
                self.ensure_value_in_field(field_id, value, || {
                    "Gate::Constant constant".to_string()
                });
                self.ensure_undefined_and_set(field_id, *out);
            }

            AssertZero(field_id, inp) => {
                self.ensure_defined_and_set(field_id, *inp);
            }

            Copy(field_id, out, inp) => {
                self.ensure_defined_and_set(field_id, *inp);
                self.ensure_undefined_and_set(field_id, *out);
            }

            Add(field_id, out, left, right) => {
                self.ensure_defined_and_set(field_id, *left);
                self.ensure_defined_and_set(field_id, *right);

                self.ensure_undefined_and_set(field_id, *out);
            }

            Mul(field_id, out, left, right) => {
                self.ensure_defined_and_set(field_id, *left);
                self.ensure_defined_and_set(field_id, *right);

                self.ensure_undefined_and_set(field_id, *out);
            }

            AddConstant(field_id, out, inp, constant) => {
                self.ensure_value_in_field(field_id, constant, || {
                    format!("Gate::AddConstant_{}", *out)
                });
                self.ensure_defined_and_set(field_id, *inp);
                self.ensure_undefined_and_set(field_id, *out);
            }

            MulConstant(field_id, out, inp, constant) => {
                self.ensure_value_in_field(field_id, constant, || {
                    format!("Gate::MulConstant_{}", *out)
                });
                self.ensure_defined_and_set(field_id, *inp);
                self.ensure_undefined_and_set(field_id, *out);
            }

            And(field_id, out, left, right) => {
                self.ensure_defined_and_set(field_id, *left);
                self.ensure_defined_and_set(field_id, *right);
                self.ensure_undefined_and_set(field_id, *out);
            }

            Xor(field_id, out, left, right) => {
                self.ensure_defined_and_set(field_id, *left);
                self.ensure_defined_and_set(field_id, *right);
                self.ensure_undefined_and_set(field_id, *out);
            }

            Not(field_id, out, inp) => {
                self.ensure_defined_and_set(field_id, *inp);
                self.ensure_undefined_and_set(field_id, *out);
            }

            Instance(field_id, out) => {
                self.declare(field_id, *out);
                // Consume value.
                self.consume_instance(field_id, 1);
            }

            Witness(field_id, out) => {
                self.declare(field_id, *out);
                // Consume value.
                self.consume_witness(field_id, 1);
            }

            Free(field_id, first, last) => {
                // first < last
                if let Some(last_id) = last {
                    if last_id <= first {
                        self.violate(format!(
                            "For Free gates, last WireId ({}) must be strictly greater than first WireId ({}).",
                            last_id, first
                        ));
                    }
                }
                // all wires between first and last INCLUSIVE
                for wire_id in *first..=last.unwrap_or(*first) {
                    self.ensure_defined_and_set(field_id, wire_id);
                    self.remove(field_id, wire_id);
                }
            }

            Convert(output_wires, input_wires) => {
                // Check that input_wires is not empty and all input wires belong to the same field
                if let Err(err) = is_one_field_wirelist(input_wires) {
                    self.violate(format!("Gate::Convert: Error with input wires: {}", err))
                }

                // Check that output_wires is not empty and all output wires belong to the same field
                if let Err(err) = is_one_field_wirelist(output_wires) {
                    self.violate(format!("Gate::Convert: Error with output wires: {}", err))
                }

                // Expand input and output wires
                let expanded_outputs = expand_wirelist(output_wires).unwrap_or_else(|err| {
                    self.violate(err.to_string());
                    vec![]
                });
                let expanded_inputs = expand_wirelist(input_wires).unwrap_or_else(|err| {
                    self.violate(err.to_string());
                    vec![]
                });

                // Check inputs are already set
                expanded_inputs.iter().for_each(|(field_id, wire_id)| {
                    self.ensure_defined_and_set(field_id, *wire_id)
                });

                // Set the output wires as defined
                expanded_outputs.iter().for_each(|(field_id, wire_id)| {
                    self.ensure_undefined_and_set(field_id, *wire_id)
                });
            }

            AnonCall(output_wires, input_wires, instance_count, witness_count, subcircuit) => {
                let expanded_outputs = expand_wirelist(output_wires).unwrap_or_else(|err| {
                    self.violate(err.to_string());
                    vec![]
                });
                let expanded_inputs = expand_wirelist(input_wires).unwrap_or_else(|err| {
                    self.violate(err.to_string());
                    vec![]
                });

                // Check inputs are already set
                expanded_inputs.iter().for_each(|(field_id, wire_id)| {
                    self.ensure_defined_and_set(field_id, *wire_id)
                });
                // ingest it and validate it.
                self.ingest_subcircuit(
                    subcircuit,
                    &wirelist_to_count_list(output_wires),
                    &wirelist_to_count_list(input_wires),
                    instance_count,
                    witness_count,
                    true,
                );

                // Now, consume instances and witnesses from self.
                self.consume_instance_count(instance_count);
                self.consume_witness_count(witness_count);

                // set the output wires as defined, since we checked they were in each branch.
                expanded_outputs.iter().for_each(|(field_id, wire_id)| {
                    self.ensure_undefined_and_set(field_id, *wire_id)
                });
            }

            Call(name, output_wires, input_wires) => {
                // - Check exists
                // - Outputs and inputs match function signature
                // - define outputs, check inputs
                // - consume witness.
                let expanded_outputs = expand_wirelist(output_wires).unwrap_or_else(|err| {
                    self.violate(err.to_string());
                    vec![]
                });
                let expanded_inputs = expand_wirelist(input_wires).unwrap_or_else(|err| {
                    self.violate(err.to_string());
                    vec![]
                });

                expanded_inputs.iter().for_each(|(field_id, wire_id)| {
                    self.ensure_defined_and_set(field_id, *wire_id)
                });

                let (instance_count, witness_count) = self
                    .ingest_call(name, output_wires, input_wires)
                    .unwrap_or((HashMap::new(), HashMap::new()));

                // Now, consume instances and witnesses from self.
                self.consume_instance_count(&instance_count);
                self.consume_witness_count(&witness_count);

                // set the output wires as defined, since we checked they were in each branch.
                expanded_outputs.iter().for_each(|(field_id, wire_id)| {
                    self.ensure_undefined_and_set(field_id, *wire_id)
                });
            }

            Switch(condition_field, condition_wire, output_wires, cases, branches) => {
                self.ensure_defined_and_set(condition_field, *condition_wire);

                // Ensure that the number of cases value match the number of subcircuits.
                if cases.len() != branches.len() {
                    self.violate("Gate::Switch: The number of cases value does not match the number of branches.");
                }

                // If there is no branch, just return.
                // If the list of output wires is not empty, then it's an issue.
                if cases.is_empty() {
                    if !output_wires.is_empty() {
                        self.violate("Switch: no case given while non-empty list of output wires.");
                    }
                    return;
                }

                // Ensure each value of cases are in the proper field.
                let mut cases_set = HashSet::new();
                for case in cases {
                    self.ensure_value_in_field(condition_field, case, || {
                        format!(
                            "Gate::Switch case value: ({}: {})",
                            *condition_field,
                            Field::from_bytes_le(case)
                        )
                    });
                    cases_set.insert(Field::from_bytes_le(case));
                }

                // ensure that there is no duplicate in cases.
                if cases_set.len() != cases.len() {
                    self.violate("Gate::Switch: The cases values contain duplicates.");
                }

                let (mut max_instance_count, mut max_witness_count) =
                    (HashMap::new(), HashMap::new());

                let expanded_outputs = expand_wirelist(output_wires).unwrap_or_else(|err| {
                    self.violate(err.to_string());
                    vec![]
                });

                // 'Validate' each branch of the switch independently, and perform checks
                for branch in branches {
                    let (instance_count, witness_count) = match branch {
                        CaseInvoke::AbstractGateCall(name, inputs) => {
                            let expanded_inputs = expand_wirelist(inputs).unwrap_or_else(|err| {
                                self.violate(err.to_string());
                                vec![]
                            });
                            expanded_inputs.iter().for_each(|(field_id, wire_id)| {
                                self.ensure_defined_and_set(field_id, *wire_id)
                            });
                            self.ingest_call(name, output_wires, inputs)
                                .unwrap_or((HashMap::new(), HashMap::new()))
                        }
                        CaseInvoke::AbstractAnonCall(
                            inputs,
                            instance_count,
                            witness_count,
                            subcircuit,
                        ) => {
                            let expanded_inputs = expand_wirelist(inputs).unwrap_or_else(|err| {
                                self.violate(err.to_string());
                                vec![]
                            });
                            expanded_inputs.iter().for_each(|(field_id, wire_id)| {
                                self.ensure_defined_and_set(field_id, *wire_id)
                            });

                            self.ingest_subcircuit(
                                subcircuit,
                                &wirelist_to_count_list(output_wires),
                                &wirelist_to_count_list(inputs),
                                instance_count,
                                witness_count,
                                true,
                            );
                            (instance_count.clone(), witness_count.clone())
                        }
                    };

                    count_list_max(&mut max_instance_count, &instance_count);
                    count_list_max(&mut max_witness_count, &witness_count);
                }

                // Now, consume instances and witnesses from self.
                self.consume_instance_count(&max_instance_count);
                self.consume_witness_count(&max_witness_count);

                // set the output wires as defined, since we checked they were in each branch.
                expanded_outputs.iter().for_each(|(field_id, wire_id)| {
                    self.ensure_undefined_and_set(field_id, *wire_id)
                });
            }

            For(iterator_name, start_val, end_val, global_output_list, body) => {
                if *end_val < *start_val {
                    self.violate(format!("In a For loop, the end value ({}) must be strictly greater than the start value ({}).", *end_val, *start_val));
                    return;
                }

                if self.known_iterators.borrow().contains_key(iterator_name) {
                    self.violate("Iterator already used in this context.");
                    return;
                }

                let re = Regex::new(NAMES_REGEX).unwrap();
                if !re.is_match(iterator_name) {
                    self.violate(format!(
                        "The iterator name ({}) should match the following format ({}).",
                        iterator_name, NAMES_REGEX
                    ));
                }

                for i in *start_val..=*end_val {
                    self.known_iterators
                        .borrow_mut()
                        .insert(iterator_name.clone(), i);

                    match body {
                        ForLoopBody::IterExprCall(name, field_id, outputs, inputs) => {
                            let expanded_outputs =
                                evaluate_iterexpr_list(outputs, &*self.known_iterators.borrow());
                            let expanded_inputs =
                                evaluate_iterexpr_list(inputs, &*self.known_iterators.borrow());
                            expanded_inputs
                                .iter()
                                .for_each(|id| self.ensure_defined_and_set(field_id, *id));
                            let (instance_count, witness_count) = self
                                .ingest_call(
                                    name,
                                    &wire_ids_to_wirelist(field_id, &expanded_outputs),
                                    &wire_ids_to_wirelist(field_id, &expanded_inputs),
                                )
                                .unwrap_or((HashMap::new(), HashMap::new()));

                            // Now, consume instances and witnesses from self, and set the output wires
                            expanded_outputs
                                .iter()
                                .for_each(|id| self.ensure_undefined_and_set(field_id, *id));
                            self.consume_instance_count(&instance_count);
                            self.consume_witness_count(&witness_count);
                        }
                        ForLoopBody::IterExprAnonCall(
                            field_id,
                            output_wires,
                            input_wires,
                            instance_count,
                            witness_count,
                            subcircuit,
                        ) => {
                            let expanded_outputs = evaluate_iterexpr_list(
                                output_wires,
                                &*self.known_iterators.borrow(),
                            );
                            let expanded_inputs = evaluate_iterexpr_list(
                                input_wires,
                                &*self.known_iterators.borrow(),
                            );
                            expanded_inputs
                                .iter()
                                .for_each(|id| self.ensure_defined_and_set(field_id, *id));

                            self.ingest_subcircuit(
                                subcircuit,
                                &HashMap::from([(*field_id, expanded_outputs.len() as u64)]),
                                &HashMap::from([(*field_id, expanded_inputs.len() as u64)]),
                                instance_count,
                                witness_count,
                                true,
                            );

                            // Now, consume instances and witnesses from self, and set the output wires
                            expanded_outputs
                                .iter()
                                .for_each(|id| self.ensure_undefined_and_set(field_id, *id));
                            self.consume_instance_count(instance_count);
                            self.consume_witness_count(witness_count);
                        }
                    }
                }
                self.known_iterators.borrow_mut().remove(iterator_name);

                // Ensure that each global output wire has been set in one of the loops.
                let expanded_global_outputs =
                    expand_wirelist(global_output_list).unwrap_or_else(|err| {
                        self.violate(err.to_string());
                        vec![]
                    });
                expanded_global_outputs
                    .iter()
                    .for_each(|(field_id, wire_id)| {
                        self.ensure_defined_and_set(field_id, *wire_id)
                    });
            }
        }
    }

    /// Ingest an equivalent of the AbstractGateCall, along with the expanded outputs list.
    /// It will not set the output_wires as defined in the current validator, as well as it will not
    /// consume instances and witnesses of the current validator. It's up to the caller
    /// to do so whenever necessary.
    /// It returns the tuple (instance_count, witness_count)
    fn ingest_call(
        &mut self,
        name: &str,
        output_wires: &WireList,
        input_wires: &WireList,
    ) -> Result<(CountList, CountList)> {
        if !self.known_functions.borrow().contains_key(name) {
            self.violate(format!("Unknown Function gate {}", name));
            return Err("This function does not exist.".into());
        }

        let (output_count, input_count, instance_count, witness_count) =
            self.known_functions.borrow().get(name).cloned().unwrap();

        if output_count != wirelist_to_count_list(output_wires) {
            self.violate("Call: number of output wires mismatch.");
        }

        if input_count != wirelist_to_count_list(input_wires) {
            self.violate("Call: number of input wires mismatch.");
        }

        Ok((instance_count, witness_count))
    }

    /// This function will check the semantic validity of all the gates in the subcircuit.
    /// It will ensure that all input variable are currently well defined before entering this
    /// subcircuit, and will check that the subcircuit actually correctly set the given number of
    /// output wires, and that instance and witness variables declared are actually consumed.
    /// To do so, it creates a local validator, and appends the violations found by it to the
    /// current validator object.
    /// NOTE: it will @b not consume instance / witness from the current validator, and will @b not
    /// set output variables of the current validator as set.
    /// If required, this should be done in the caller function.
    fn ingest_subcircuit(
        &mut self,
        subcircuit: &[Gate],
        output_count: &CountList,
        input_count: &CountList,
        instance_count: &CountList,
        witness_count: &CountList,
        use_same_scope: bool,
    ) {
        let mut current_validator = Validator {
            as_prover: self.as_prover,
            instance_queue_len: instance_count.clone(),
            witness_queue_len: if self.as_prover {
                witness_count.clone()
            } else {
                HashMap::new()
            },
            live_wires: Default::default(),
            got_header: self.got_header,
            header_version: self.header_version.clone(),
            fields: self.fields.clone(),
            known_functions: self.known_functions.clone(),
            known_iterators: if use_same_scope {
                self.known_iterators.clone()
            } else {
                Default::default()
            },
            violations: vec![],
        };

        // input wires should be already defined, and they are numbered from
        // output_wire, so we will artificially define them in the inner
        // validator.
        for (field_id, count) in input_count.iter() {
            let first_idx = *output_count.get(field_id).unwrap_or(&0);
            for wire_id in first_idx..(first_idx + *count) {
                current_validator.live_wires.insert((*field_id, wire_id));
            }
        }

        for x in subcircuit.iter() {
            current_validator.ingest_gate(x);
        }

        // ensure that all output wires are set.
        for (output_field, count) in output_count.iter() {
            (0..*count)
                .for_each(|id| current_validator.ensure_defined_and_set(output_field, id as u64));
        }

        self.violations.append(&mut current_validator.violations);
        if current_validator.instance_queue_len.values().sum::<u64>() != 0 {
            self.violate(
                "The subcircuit has not consumed all the instance variables it should have.",
            )
        }
        if current_validator.witness_queue_len.values().sum::<u64>() != 0 {
            self.violate(
                "The subcircuit has not consumed all the witness variables it should have.",
            )
        }
    }

    fn is_defined(&self, field_id: &FieldId, id: WireId) -> bool {
        self.live_wires.contains(&(*field_id, id))
    }

    fn declare(&mut self, field_id: &FieldId, id: WireId) {
        self.live_wires.insert((*field_id, id));
    }

    fn remove(&mut self, field_id: &FieldId, id: WireId) {
        if !self.live_wires.remove(&(*field_id, id)) {
            self.violate(format!("The variable ({}: {}) is being freed, but was not defined previously, or has been already freed", *field_id,id));
        }
    }

    fn consume_instance(&mut self, field_id: &FieldId, how_many: u64) {
        if how_many == 0 {
            return;
        }
        if let Some(count) = self.instance_queue_len.get_mut(field_id) {
            if *count >= how_many {
                *count -= how_many;
            } else {
                *count = 0;
                self.violate("Not enough Instance value to consume.");
            }
        } else {
            self.violate("Not enough Instance value to consume.");
        }
    }

    fn consume_instance_count(&mut self, instance_count: &CountList) {
        for (field_id, count) in instance_count.iter() {
            self.consume_instance(field_id, *count);
        }
    }

    fn consume_witness(&mut self, field_id: &FieldId, how_many: u64) {
        if self.as_prover {
            if how_many == 0 {
                return;
            }
            if let Some(count) = self.witness_queue_len.get_mut(field_id) {
                if *count >= how_many {
                    *count -= how_many;
                } else {
                    *count = 0;
                    self.violate("Not enough Witness value to consume.");
                }
            } else {
                self.violate("Not enough Witness value to consume.");
            }
        }
    }

    fn consume_witness_count(&mut self, witness_count: &CountList) {
        if self.as_prover {
            for (field_id, count) in witness_count.iter() {
                self.consume_witness(field_id, *count);
            }
        }
    }

    fn ensure_defined_and_set(&mut self, field_id: &FieldId, id: WireId) {
        if !self.is_defined(field_id, id) {
            if self.as_prover {
                // in this case, this is a violation, since all variables must have been defined
                // previously
                self.violate(format!(
                    "The wire ({}: {}) is used but was not assigned a value, or has been freed already.",
                    *field_id,
                    id
                ));
            }
            // this line is useful to avoid having many times the same message if the validator already
            // detected that this wire was not previously initialized.
            self.declare(field_id, id);
        }
    }

    fn ensure_undefined(&mut self, field_id: &FieldId, id: WireId) {
        if self.is_defined(field_id, id) {
            self.violate(format!(
                "The wire ({}: {}) has already been initialized before. This violates the SSA property.",
                *field_id,
                id
            ));
        }
    }

    fn ensure_undefined_and_set(&mut self, field_id: &FieldId, id: WireId) {
        self.ensure_undefined(field_id, id);
        // define it.
        self.declare(field_id, id);
    }

    fn ensure_value_in_field(
        &mut self,
        field_id: &FieldId,
        value: &[u8],
        name: impl Fn() -> String,
    ) {
        if value.is_empty() {
            self.violate(format!("The {} is empty.", name()));
        }

        let field = get_field(field_id, &self.fields);
        let field = match field {
            Err(_) => {
                self.violate(format!("Field {} is not defined.", *field_id));
                return;
            }
            Ok(field) => field,
        };
        let int = &Field::from_bytes_le(value);
        if int >= field {
            let msg = format!(
                "The {} cannot be represented in the field specified in Header ({} >= {}).",
                name(),
                int,
                field
            );
            self.violate(msg);
        }
    }

    fn ensure_all_instance_values_consumed(&mut self) {
        let instances_not_consumed: u64 = self.instance_queue_len.values().sum::<u64>();
        if instances_not_consumed != 0 {
            self.violate(format!(
                "Too many Instance values ({} not consumed)",
                instances_not_consumed
            ));
        }
    }

    fn ensure_all_witness_values_consumed(&mut self) {
        if !self.as_prover {
            return;
        }
        let witnesses_not_consumed: u64 = self.witness_queue_len.values().sum::<u64>();
        if witnesses_not_consumed != 0 {
            self.violate(format!(
                "Too many Witness values ({} not consumed)",
                witnesses_not_consumed
            ));
        }
    }

    fn violate(&mut self, msg: impl Into<String>) {
        self.violations.push(msg.into());
        // println!("{}", msg.into());
    }
}

#[test]
fn test_validator() -> Result<()> {
    use crate::producers::examples::*;

    let instance = example_instance();
    let witness = example_witness();
    let relation = example_relation();

    let mut validator = Validator::new_as_prover();

    validator.ingest_instance(&instance);
    validator.ingest_witness(&witness);
    validator.ingest_relation(&relation);

    assert_eq!(validator.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_validator_as_verifier() -> Result<()> {
    use crate::producers::examples::*;

    let instance = example_instance();
    let relation = example_relation();

    let mut validator = Validator::new_as_verifier();

    validator.ingest_instance(&instance);
    validator.ingest_relation(&relation);

    assert_eq!(validator.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_validator_violations() -> Result<()> {
    use crate::producers::examples::*;

    let mut instance = example_instance();
    let mut witness = example_witness();
    let mut relation = example_relation();

    // Create a violation by using a value too big for the field.
    instance.common_inputs[0].inputs[0] = instance.header.fields[0].clone();
    // Create a violation by omitting a witness value.
    witness.short_witness[0].inputs.pop().unwrap();
    // Create a violation by using different headers.
    relation.header.fields = vec![vec![10]];

    let mut validator = Validator::new_as_prover();
    validator.ingest_instance(&instance);
    validator.ingest_witness(&witness);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    assert_eq!(
        violations,
        vec![
            "The instance value [101, 0, 0, 0] cannot be represented in the field specified in Header (101 >= 101).",
            "The fields are not consistent across headers.",
            "Not enough Witness value to consume.",
        ]
    );

    Ok(())
}

#[test]
fn test_validator_free_violations() -> Result<()> {
    use crate::producers::examples::*;

    let instance = example_instance();
    let witness = example_witness();
    let mut relation = example_relation();

    relation.gates.push(Gate::Free(0, 1, Some(2)));
    relation.gates.push(Gate::Free(0, 4, None));

    let mut validator = Validator::new_as_prover();
    validator.ingest_instance(&instance);
    validator.ingest_witness(&witness);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    assert_eq!(
        violations,
        vec![
            "The wire (0: 1) is used but was not assigned a value, or has been freed already.",
            "The wire (0: 2) is used but was not assigned a value, or has been freed already.",
            "The wire (0: 4) is used but was not assigned a value, or has been freed already.",
        ]
    );

    Ok(())
}
