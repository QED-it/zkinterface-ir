use crate::{Gate, Message, PrivateInputs, PublicInputs, Relation, TypeId, WireId};
use num_bigint::BigUint;
use num_traits::identities::One;
use std::collections::{BTreeSet, HashMap, HashSet};

use crate::structs::conversion::Conversion;
use crate::structs::count::{count_list_to_hashmap, Count};
use crate::structs::directives::Directive;
use crate::structs::function::{FunctionBody, FunctionCounts};
use crate::structs::types::Type;
use crate::structs::value::{is_probably_prime, value_to_biguint};
use crate::structs::wirerange::add_types_to_wire_ranges;
use regex::Regex;
use std::cmp::Ordering;
use std::convert::TryFrom;

type TypeElement = BigUint;

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
 - Ensure that the version string has the correct format (e.g. matches the following regular expression “^\d+.\d+.\d+$”).
 - Ensure header messages are coherent.
     - Versions should be identical.
     - Type moduli should be the same.

Inputs Validation (PublicInputs / PrivateInputs)
 - Ensure that PublicInput gates are given a value in public_inputs messages.
 - Ensure that PrivateInput gates are given a value in private_inputs messages (prover only).
 - Ensure that all public and private inputs are consumed at the end of the circuit
 - Ensure that the value they are set to is indeed encoding an element lying in the underlying type.
   It can be achieved by ensuring that the encoded value is strictly smaller than the type modulo.

Gates Validation
 - Ensure constants given in @addc/@mulc are actual type elements.
 - Ensure input wires of gates map to an already set variable.
 - Enforce Single Static Assignment by checking that the same wire is used only once as an output wire.
 - Ensure that for New gates of the format @new(first, last), we have (last > first).
 - Ensure that for New gates of the format @(first, last), all wires between first and last inclusive
   are not already set.
 - Ensure that for Delete gates of the format @delete(first, last), we have (last > first).
 - Ensure that when deleting contiguous space wires (previously allocated with @new), the first and
   the last parameters match the preceding new directive.

WireRange Validation
 - Ensure that for WireRange(first, last) that (last > first).
";

#[derive(Clone, Default)]
pub struct Validator {
    as_prover: bool,

    public_inputs_counts: HashMap<ValidatorType, u64>,
    private_inputs_counts: HashMap<ValidatorType, u64>,
    live_wires: BTreeSet<(TypeId, WireId)>,
    // (type_id, first_wire, last_wire)
    allocations: HashSet<(TypeId, WireId, WireId)>,

    got_header: bool,
    header_version: String,

    types: Vec<ValidatorType>,

    known_plugins: HashSet<String>,
    // name => (output_count, input_count, public_count, private_count)
    known_functions: HashMap<String, FunctionCounts>,
    known_conversions: HashSet<Conversion>,

    violations: Vec<String>,
}

/// A `ValidatorType` is similar to a `Type` except that the value in `Type::Field` is a `TypeElement` instead of a `Value`
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum ValidatorType {
    Field(TypeElement),
    PluginType(String, String, Vec<String>),
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
        self.ensure_all_public_values_consumed();
        self.ensure_all_private_values_consumed();
        if !self.live_wires.is_empty() {
            println!("WARNING: few variables were not deleted.");
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
            Message::PublicInputs(i) => self.ingest_public_inputs(i),
            Message::PrivateInputs(w) => self.ingest_private_inputs(w),
            Message::Relation(r) => self.ingest_relation(r),
        }
    }

    fn ingest_header(&mut self, version: &str, types: &[Type]) {
        if self.got_header {
            // in this case, ensure that headers are compatible
            if self.types.len() != types.len() {
                self.violate("The types are not consistent across resources.");
            }
            let equal_types =
                self.types
                    .iter()
                    .zip(types.iter())
                    .all(
                        |(previous_type, new_type)| match (previous_type, new_type) {
                            (ValidatorType::Field(previous_modulo), Type::Field(new_modulo)) => {
                                previous_modulo == &value_to_biguint(new_modulo)
                            }
                            (
                                ValidatorType::PluginType(
                                    previous_name,
                                    previous_operation,
                                    previous_params,
                                ),
                                Type::PluginType(new_name, new_operation, new_params),
                            ) => {
                                previous_name == new_name
                                    && previous_operation == new_operation
                                    && previous_params == new_params
                            }
                            (ValidatorType::Field(_), Type::PluginType(_, _, _)) => false,
                            (ValidatorType::PluginType(_, _, _), Type::Field(_)) => false,
                        },
                    );
            if !equal_types {
                self.violate("The types are not consistent across resources.");
            }

            if self.header_version != *version {
                self.violate("The profile version is not consistent across resources.");
            }
        } else {
            self.got_header = true;

            // Check validity of type values
            for type_value in types {
                match type_value {
                    Type::Field(modulo) => {
                        let biguint_modulo = value_to_biguint(modulo);
                        self.types
                            .push(ValidatorType::Field(biguint_modulo.clone()));
                        if biguint_modulo.cmp(&One::one()) != Ordering::Greater {
                            self.violate("All Field type should have a modulo > 1");
                        }
                        if !is_probably_prime(modulo) {
                            self.violate("All Field type should should have a prime modulo.")
                        }
                    }
                    Type::PluginType(name, operation, params) => self.types.push(
                        ValidatorType::PluginType(name.clone(), operation.clone(), params.clone()),
                    ),
                };
            }

            // check version
            let re = Regex::new(VERSION_REGEX).unwrap();
            if !re.is_match(version.trim()) {
                self.violate("The profile version should match the following format <major>.<minor>.<patch>.");
            }
            self.header_version = version.to_string();
        }
    }

    pub fn ingest_public_inputs(&mut self, public_inputs: &PublicInputs) {
        // If the type is a Field, ensure that all values belong to this field.
        let validator_type = match &public_inputs.type_value {
            Type::Field(modulo) => {
                let biguint_modulo = value_to_biguint(modulo);
                let validator_type = ValidatorType::Field(biguint_modulo);
                public_inputs.inputs.iter().for_each(|value| {
                    self.ensure_value_in_type(&validator_type, value, || {
                        format!("public value {:?}", value)
                    })
                });
                validator_type
            }
            Type::PluginType(name, operation, params) => {
                ValidatorType::PluginType(name.clone(), operation.clone(), params.clone())
            }
        };
        // Update public_inputs_counts
        let count = self.public_inputs_counts.entry(validator_type).or_insert(0);
        *count += public_inputs.inputs.len() as u64;
    }

    pub fn ingest_private_inputs(&mut self, private_inputs: &PrivateInputs) {
        if !self.as_prover {
            self.violate("As verifier, got an unexpected PrivateInputs message.");
        }

        // If the type is a Field, ensure that all values belong to this field.
        let validator_type = match &private_inputs.type_value {
            Type::Field(modulo) => {
                let biguint_modulo = value_to_biguint(modulo);
                let validator_type = ValidatorType::Field(biguint_modulo);
                private_inputs.inputs.iter().for_each(|value| {
                    self.ensure_value_in_type(&validator_type, value, || {
                        format!("private value {:?}", value)
                    })
                });
                validator_type
            }
            Type::PluginType(name, operation, params) => {
                ValidatorType::PluginType(name.clone(), operation.clone(), params.clone())
            }
        };
        // Update private_inputs_counts
        let count = self
            .private_inputs_counts
            .entry(validator_type)
            .or_insert(0);
        *count += private_inputs.inputs.len() as u64;
    }

    pub fn ingest_relation(&mut self, relation: &Relation) {
        self.ingest_header(&relation.version, &relation.types);

        relation.plugins.iter().for_each(|plugin| {
            self.known_plugins.insert(plugin.clone());
        });

        relation.conversions.iter().for_each(|conversion| {
            self.known_conversions.insert(conversion.clone());
        });

        for directive in relation.directives.iter() {
            match directive {
                Directive::Function(function) => {
                    let (name, output_count, input_count) = (
                        function.name.clone(),
                        function.output_count.clone(),
                        function.input_count.clone(),
                    );

                    // Check that the name follows the proper REGEX
                    let re = Regex::new(NAMES_REGEX).unwrap();
                    if !re.is_match(name.trim()) {
                        self.violate(format!(
                            "The function name ({}) should match the proper format ({}).",
                            name, NAMES_REGEX
                        ));
                    }

                    // Validate the body
                    let (public_count, private_count) = match &function.body {
                        FunctionBody::Gates(gates) => {
                            // Validate the subcircuit for custom functions
                            // And return public_count and private_count
                            self.ingest_custom_function(gates, &output_count, &input_count)
                        }
                        FunctionBody::PluginBody(plugin_body) => {
                            // Check that the plugin name has been declared
                            if !self.known_plugins.contains(&plugin_body.name) {
                                self.violate(format!(
                                    "The plugin '{}' has not been declared",
                                    plugin_body.name
                                ));
                            }
                            (
                                plugin_body.public_count.clone(),
                                plugin_body.private_count.clone(),
                            )
                        }
                    };

                    // Record the signature first.
                    if self.known_functions.contains_key(&name) {
                        self.violate(format!(
                            "A function with the name '{}' already exists",
                            name
                        ));
                        continue;
                    } else {
                        self.known_functions.insert(
                            name.clone(),
                            FunctionCounts {
                                output_count: output_count.clone(),
                                input_count: input_count.clone(),
                                public_count: public_count.clone(),
                                private_count: private_count.clone(),
                            },
                        );
                    }
                }
                Directive::Gate(gate) => {
                    self.ingest_gate(gate);
                }
            };
        }
    }

    fn ingest_gate(&mut self, gate: &Gate) {
        use Gate::*;

        match gate {
            Constant(type_id, out, value) => {
                self.ensure_value_in_type_id(type_id, value, || {
                    "Gate::Constant constant".to_string()
                });
                self.ensure_undefined_and_set(type_id, *out);
            }

            AssertZero(type_id, inp) => {
                self.ensure_defined_and_set(type_id, *inp);
            }

            Copy(type_id, out, inp) => {
                self.ensure_defined_and_set(type_id, *inp);
                self.ensure_undefined_and_set(type_id, *out);
            }

            Add(type_id, out, left, right) => {
                self.ensure_defined_and_set(type_id, *left);
                self.ensure_defined_and_set(type_id, *right);

                self.ensure_undefined_and_set(type_id, *out);
            }

            Mul(type_id, out, left, right) => {
                self.ensure_defined_and_set(type_id, *left);
                self.ensure_defined_and_set(type_id, *right);

                self.ensure_undefined_and_set(type_id, *out);
            }

            AddConstant(type_id, out, inp, constant) => {
                self.ensure_value_in_type_id(type_id, constant, || {
                    format!("Gate::AddConstant_{}", *out)
                });
                self.ensure_defined_and_set(type_id, *inp);
                self.ensure_undefined_and_set(type_id, *out);
            }

            MulConstant(type_id, out, inp, constant) => {
                self.ensure_value_in_type_id(type_id, constant, || {
                    format!("Gate::MulConstant_{}", *out)
                });
                self.ensure_defined_and_set(type_id, *inp);
                self.ensure_undefined_and_set(type_id, *out);
            }

            PublicInput(type_id, out) => {
                self.declare(type_id, *out);
                // Consume value.
                self.consume_public_inputs(type_id, 1);
            }

            PrivateInput(type_id, out) => {
                self.declare(type_id, *out);
                // Consume value.
                self.consume_private_inputs(type_id, 1);
            }

            New(type_id, first, last) => {
                // Ensure first < last
                if *last <= *first {
                    self.violate(format!(
                        "For New gates, last WireId ({}) should be strictly greater than first WireId ({}).",
                        *last, *first
                    ));
                }
                // Ensure wires have not already been allocated
                if self.belong_to_allocations(type_id, first, last) {
                    self.violate("For New gates, wires should not have already been allocated.");
                }

                // Ensure wires are not already set
                for wire_id in *first..=*last {
                    self.ensure_undefined(type_id, wire_id);
                }
                // Add this new wire range into wire_allocations
                self.allocations.insert((*type_id, *first, *last));
            }

            Delete(type_id, first, last) => {
                // first < last
                if *last < *first {
                    self.violate(format!(
                            "For Delete gates, last WireId ({}) should be greater than first WireId ({}).",
                            *last, *first
                        ));
                }

                // Check whether some whole allocations belong to Delete WireRange
                // If so, remove those allocations from the list of allocations `self.allocations`
                self.allocations
                    .retain(|(alloc_type_id, alloc_first, alloc_last)| {
                        !(*alloc_type_id == *type_id
                            && *first <= *alloc_first
                            && *alloc_last <= *last)
                    });

                // Check whether some wires belong to an allocation
                // If it is the case, we have a violation (we try to delete a portion of an allocation)
                if self.belong_to_allocations(type_id, first, last) {
                    self.violate(
                        "For Delete gates, we should not delete a portion of an allocation.",
                    );
                }

                // For all wires between first and last INCLUSIVE
                // - check that they are already set
                // - delete them
                for wire_id in *first..=*last {
                    self.ensure_defined_and_set(type_id, wire_id);
                    self.remove(type_id, wire_id);
                }
            }

            Convert(
                out_type_id,
                out_first_id,
                out_last_id,
                in_type_id,
                in_first_id,
                in_last_id,
            ) => {
                // Check that out_ids is not empty
                if *out_first_id > *out_last_id {
                    self.violate(format!(
                        "Gate::Convert: Error with out_ids: first_id({}) > last_id({})",
                        *out_first_id, *out_last_id
                    ));
                    return;
                }
                // Check that in_ids is not empty
                if *in_first_id > *in_last_id {
                    self.violate(format!(
                        "Gate::Convert: Error with in_ids: first_id({}) > last_id({})",
                        *in_first_id, *in_last_id
                    ));
                    return;
                }

                // Check conversion has been declared
                if !self.known_conversions.contains(&Conversion::new(
                    Count::new(*out_type_id, *out_last_id - out_first_id + 1),
                    Count::new(*in_type_id, *in_last_id - *in_first_id + 1),
                )) {
                    self.violate(format!(
                        "Gate::Convert: used an undeclared conversion Conversion({}:{}, {}:{})",
                        *out_type_id,
                        *out_last_id - *out_first_id + 1,
                        *in_type_id,
                        *in_last_id - *in_first_id + 1
                    ))
                }

                // Check inputs are already set
                (*in_first_id..=*in_last_id)
                    .for_each(|wire_id| self.ensure_defined_and_set(in_type_id, wire_id));

                // Check inputs belong to a single allocation
                if !self.belong_to_a_single_allocation(in_type_id, in_first_id, in_last_id) {
                    self.violate("Gate::Convert: all inputs do not belong to the same allocation)")
                }

                // Check that all outputs belong to a single allocation or no output belongs to an allocation
                // If no output belongs to allocations, add this output WireRange to the list of allocations
                // If outputs belong to different allocations, we have a violation
                if !self.belong_to_a_single_allocation(out_type_id, out_first_id, out_last_id) {
                    if self.belong_to_allocations(out_type_id, out_first_id, out_last_id) {
                        self.violate(
                            "Gate::Convert: outputs should not belong to different allocations)",
                        )
                    } else {
                        // No output belongs to allocations
                        self.allocations
                            .insert((*out_type_id, *out_first_id, *out_last_id));
                    }
                }

                // Set the output wires as defined
                (*out_first_id..=*out_last_id)
                    .for_each(|wire_id| self.ensure_undefined_and_set(out_type_id, wire_id));
            }

            Call(name, out_ids, in_ids) => {
                // - Check exists
                // - Outputs and inputs match function signature
                // - define outputs, check inputs
                // - consume public/private inputs.

                // Check that function is declared and retrieve its parameters
                let function_counts_result =
                    FunctionCounts::get_function_counts(&self.known_functions, name);

                match function_counts_result {
                    Err(_) => self.violate(format!("Unknown Function gate {}", name)),
                    Ok(func_counts) => {
                        // Ensure inputs are already set and all wires in an input WireRange belongs
                        // to the same allocation
                        let in_ids_with_types_result =
                            add_types_to_wire_ranges(in_ids, &func_counts.input_count);
                        if let Ok(in_ids_with_types) = in_ids_with_types_result {
                            in_ids_with_types.iter().for_each(|wire_range_with_type| {
                                // Ensure inputs are already set
                                (wire_range_with_type.first_id..=wire_range_with_type.last_id)
                                    .for_each(|wire_id| {
                                        self.ensure_defined_and_set(
                                            &wire_range_with_type.type_id,
                                            wire_id,
                                        );
                                    });
                                // Ensure all wires in an input WireRange belong to the same allocation
                                if !self.belong_to_a_single_allocation(&wire_range_with_type.type_id, &wire_range_with_type.first_id, &wire_range_with_type.last_id) {
                                    self.violate("Call: all wires in an input WireRange should belong to the same allocation");
                                }
                            });
                        } else {
                            self.violate("Call: number of input wires mismatch.");
                        }

                        // Consume public/private inputs from self.
                        self.consume_public_count(&func_counts.public_count);
                        self.consume_private_count(&func_counts.private_count);

                        // Set the output wires as defined and check out_ids WireRange allocations
                        let out_ids_with_types_result =
                            add_types_to_wire_ranges(out_ids, &func_counts.output_count);
                        if let Ok(out_ids_with_types) = out_ids_with_types_result {
                            // Set the output wires as defined
                            out_ids_with_types.iter().for_each(|wire_range_with_type| {
                                (wire_range_with_type.first_id..=wire_range_with_type.last_id)
                                    .for_each(|wire_id| {
                                        self.ensure_undefined_and_set(
                                            &wire_range_with_type.type_id,
                                            wire_id,
                                        );
                                    });
                                // Check that for each output WireRange, all wires belong to a
                                // single allocation or no wire belongs to an allocation.
                                // If for an output WireRange, no wire belongs to allocations, add
                                // this output WireRange to the list of allocations
                                // If for an output WireRange, wires belong to different allocations, we have a violation
                                if !self.belong_to_a_single_allocation(&wire_range_with_type.type_id, &wire_range_with_type.first_id, &wire_range_with_type.last_id) {
                                    if self.belong_to_allocations(&wire_range_with_type.type_id, &wire_range_with_type.first_id, &wire_range_with_type.last_id) {
                                        self.violate(
                                            "Call: in an output WireRange, wires should not belong to different allocations)")
                                    } else {
                                        // No wire belongs to allocations
                                        self.allocations
                                            .insert((wire_range_with_type.type_id, wire_range_with_type.first_id, wire_range_with_type.last_id));
                                    }
                                }
                            });
                        } else {
                            self.violate("Call: number of output wires mismatch.");
                        }
                    }
                }
            }
        }
    }

    /// This function will check the semantic validity of all the gates in the subcircuit.
    /// It will ensure that all input variable are currently well defined before entering this
    /// subcircuit, and will check that the subcircuit actually correctly set the given number of
    /// output wires.
    /// To do so, it creates a local validator, and appends the violations found by it to the
    /// current validator object.
    /// It will return the number of consumed public/private inputs
    fn ingest_custom_function(
        &mut self,
        subcircuit: &[Gate],
        output_count: &[Count],
        input_count: &[Count],
    ) -> (HashMap<TypeId, u64>, HashMap<TypeId, u64>) {
        // Count public/private input consumed in this function
        let mut public_count = HashMap::new();
        let mut private_count = HashMap::new();
        for gate in subcircuit.iter() {
            match gate {
                Gate::PublicInput(type_id, _) => {
                    let count = public_count.entry(*type_id).or_insert(0);
                    *count += 1;
                }
                Gate::PrivateInput(type_id, _) => {
                    let count = private_count.entry(*type_id).or_insert(0);
                    *count += 1;
                }
                Gate::Call(name, _, _) => {
                    let function_counts_result =
                        FunctionCounts::get_function_counts(&self.known_functions, name);

                    match function_counts_result {
                        Err(_) => self.violate(format!("Unknown Function gate {}", name)),
                        Ok(function_counts) => {
                            // Update public_count with number of public inputs consumed by this function
                            function_counts
                                .public_count
                                .iter()
                                .for_each(|(type_id, count)| {
                                    let curr_count = public_count.entry(*type_id).or_insert(0);
                                    *curr_count += *count;
                                });
                            // Update private_count with number of private inputs consumed by this function
                            function_counts
                                .private_count
                                .iter()
                                .for_each(|(type_id, count)| {
                                    let curr_count = private_count.entry(*type_id).or_insert(0);
                                    *curr_count += *count;
                                });
                        }
                    }
                }
                _ => (), /* DO NOTHING */
            };
        }

        // Type => count
        let mut public_count_with_type = HashMap::new();
        let mut private_count_with_type = HashMap::new();
        for (type_id, count) in public_count.iter() {
            let type_option = self.types.get(usize::try_from(*type_id).unwrap());
            match type_option {
                Some(validator_type) => {
                    public_count_with_type.insert(validator_type.clone(), *count);
                }
                None => {
                    self.violate(format!("Unknown type_id {}", type_id));
                }
            }
        }
        for (type_id, count) in private_count.iter() {
            let type_option = self.types.get(usize::try_from(*type_id).unwrap());
            match type_option {
                Some(validator_type) => {
                    private_count_with_type.insert(validator_type.clone(), *count);
                }
                None => {
                    self.violate(format!("Unknown type_id {}", type_id));
                }
            }
        }

        // Create a new validator to evaluate the custom function circuit
        let mut current_validator = Validator {
            as_prover: self.as_prover,
            public_inputs_counts: public_count_with_type.clone(),
            private_inputs_counts: if self.as_prover {
                private_count_with_type.clone()
            } else {
                HashMap::new()
            },
            live_wires: Default::default(),
            allocations: Default::default(),
            got_header: self.got_header,
            header_version: self.header_version.clone(),
            types: self.types.clone(),
            known_plugins: self.known_plugins.clone(),
            known_conversions: self.known_conversions.clone(),
            known_functions: self.known_functions.clone(),
            violations: vec![],
        };

        // input wires should be already defined, and they are numbered from
        // output_wire, so we will artificially define them in the inner
        // validator.
        let output_count_map = count_list_to_hashmap(output_count);
        let input_count_map = count_list_to_hashmap(input_count);
        for (type_id, count) in input_count_map.iter() {
            let first_idx = *output_count_map.get(type_id).unwrap_or(&0);
            for wire_id in first_idx..(first_idx + *count) {
                current_validator.live_wires.insert((*type_id, wire_id));
            }
        }

        // Evaluate the subcircuit
        for gate in subcircuit.iter() {
            current_validator.ingest_gate(gate);
        }

        // ensure that all output wires are set.
        for (output_type_id, count) in output_count_map.iter() {
            (0..*count).for_each(|id| current_validator.ensure_defined_and_set(output_type_id, id));
        }

        // Copy violations from current_validator to the main validator
        self.violations.append(&mut current_validator.violations);

        (public_count, private_count)
    }

    fn is_defined(&self, type_id: &TypeId, id: WireId) -> bool {
        self.live_wires.contains(&(*type_id, id))
    }

    fn declare(&mut self, type_id: &TypeId, id: WireId) {
        self.live_wires.insert((*type_id, id));
    }

    fn remove(&mut self, type_id: &TypeId, id: WireId) {
        if !self.live_wires.remove(&(*type_id, id)) {
            self.violate(format!("The variable ({}: {}) is being deleted, but was not defined previously, or has been already deleted", *type_id,id));
        }
    }

    fn consume_inputs(
        &mut self,
        type_id: &TypeId,
        how_many: u64,
        is_public: bool, // public or private inputs ?
    ) {
        let err_message = if is_public {
            "Not enough public input value to consume."
        } else {
            "Not enough private input value to consume."
        };
        if how_many == 0 {
            return;
        }
        let type_option = self.types.get(usize::try_from(*type_id).unwrap());
        match type_option {
            Some(validator_type) => {
                let count_option = if is_public {
                    self.public_inputs_counts.get_mut(validator_type)
                } else {
                    self.private_inputs_counts.get_mut(validator_type)
                };
                if let Some(count) = count_option {
                    if *count >= how_many {
                        *count -= how_many;
                    } else {
                        *count = 0;
                        self.violate(err_message);
                    }
                } else {
                    self.violate(err_message);
                }
            }
            None => self.violate(format!("Unknown type_id {}", type_id)),
        }
    }

    fn consume_public_inputs(&mut self, type_id: &TypeId, how_many: u64) {
        self.consume_inputs(type_id, how_many, true);
    }

    fn consume_public_count(&mut self, public_count: &HashMap<TypeId, u64>) {
        public_count
            .iter()
            .for_each(|(type_id, count)| self.consume_public_inputs(type_id, *count));
    }

    fn consume_private_inputs(&mut self, type_id: &TypeId, how_many: u64) {
        if self.as_prover {
            self.consume_inputs(type_id, how_many, false);
        }
    }

    fn consume_private_count(&mut self, private_count: &HashMap<TypeId, u64>) {
        if self.as_prover {
            private_count
                .iter()
                .for_each(|(type_id, count)| self.consume_private_inputs(type_id, *count));
        }
    }

    fn ensure_defined_and_set(&mut self, type_id: &TypeId, id: WireId) {
        if !self.is_defined(type_id, id) {
            if self.as_prover {
                // in this case, this is a violation, since all variables must have been defined
                // previously
                self.violate(format!(
                    "The wire ({}: {}) is used but was not assigned a value, or has been deleted already.",
                    *type_id,
                    id
                ));
            }
            // this line is useful to avoid having many times the same message if the validator already
            // detected that this wire was not previously initialized.
            self.declare(type_id, id);
        }
    }

    fn ensure_undefined(&mut self, type_id: &TypeId, id: WireId) {
        if self.is_defined(type_id, id) {
            self.violate(format!(
                "The wire ({}: {}) has already been initialized before. This violates the SSA property.",
                *type_id,
                id
            ));
        }
    }

    fn ensure_undefined_and_set(&mut self, type_id: &TypeId, id: WireId) {
        self.ensure_undefined(type_id, id);
        // define it.
        self.declare(type_id, id);
    }

    /// This function checks that the `value` belong to the type `type_id`.
    fn ensure_value_in_type_id(
        &mut self,
        type_id: &TypeId,
        value: &[u8],
        name: impl Fn() -> String,
    ) {
        let type_option = self.types.get(usize::try_from(*type_id).unwrap());
        let validator_type = match type_option {
            Some(validator_type) => validator_type.clone(),
            None => {
                self.violate(format!("Type id {} is not defined.", *type_id));
                return;
            }
        };
        self.ensure_value_in_type(&validator_type, value, name);
    }

    /// This function checks that the `value` belong to the ValidatorType `validator_type`.
    fn ensure_value_in_type(
        &mut self,
        validator_type: &ValidatorType,
        value: &[u8],
        name: impl Fn() -> String,
    ) {
        if value.is_empty() {
            self.violate(format!("The {} is empty.", name()));
        }

        // If `validator_type` is a Field type, check that `value` belongs to this field.
        if let ValidatorType::Field(modulo) = validator_type {
            let int = &TypeElement::from_bytes_le(value);
            if int >= modulo {
                let msg = format!(
                    "The {} cannot be represented in the Field type specified ({} >= {}).",
                    name(),
                    int,
                    modulo
                );
                self.violate(msg);
            }
        }
    }

    fn ensure_all_public_values_consumed(&mut self) {
        let public_inputs_not_consumed: u64 = self.public_inputs_counts.values().sum::<u64>();
        if public_inputs_not_consumed != 0 {
            self.violate(format!(
                "Too many public input values ({} not consumed)",
                public_inputs_not_consumed
            ));
        }
    }

    fn ensure_all_private_values_consumed(&mut self) {
        if !self.as_prover {
            return;
        }
        let private_inputs_not_consumed: u64 = self.private_inputs_counts.values().sum::<u64>();
        if private_inputs_not_consumed != 0 {
            self.violate(format!(
                "Too many private input values ({} not consumed)",
                private_inputs_not_consumed
            ));
        }
    }

    fn violate(&mut self, msg: impl Into<String>) {
        self.violations.push(msg.into());
    }

    /// This function checks whether all wires between first_id and last_id belongs to the same allocation
    fn belong_to_a_single_allocation(
        &self,
        type_id: &TypeId,
        first_id: &WireId,
        last_id: &WireId,
    ) -> bool {
        if *first_id == *last_id {
            return true;
        }
        self.allocations
            .iter()
            .any(|(alloc_type_id, alloc_first, alloc_last)| {
                *alloc_type_id == *type_id && *alloc_first <= *first_id && *last_id <= *alloc_last
            })
    }

    /// This function checks whether at least one wire between first_id and last_id belongs to an allocation
    fn belong_to_allocations(&self, type_id: &TypeId, first_id: &WireId, last_id: &WireId) -> bool {
        for wire_id in *first_id..=*last_id {
            if self
                .allocations
                .iter()
                .any(|(alloc_type_id, alloc_first, alloc_last)| {
                    *alloc_type_id == *type_id && *alloc_first <= wire_id && wire_id <= *alloc_last
                })
            {
                return true;
            }
        }
        false
    }
}

#[test]
fn test_validator() -> crate::Result<()> {
    use crate::producers::examples::*;

    let public_inputs = example_public_inputs();
    let private_inputs = example_private_inputs();
    let relation = example_relation();

    let mut validator = Validator::new_as_prover();

    validator.ingest_public_inputs(&public_inputs);
    validator.ingest_private_inputs(&private_inputs);
    validator.ingest_relation(&relation);

    assert_eq!(validator.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_validator_as_verifier() -> crate::Result<()> {
    use crate::producers::examples::*;

    let public_inputs = example_public_inputs();
    let relation = example_relation();

    let mut validator = Validator::new_as_verifier();

    validator.ingest_public_inputs(&public_inputs);
    validator.ingest_relation(&relation);

    assert_eq!(validator.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_validator_violations() -> crate::Result<()> {
    use crate::producers::examples::*;

    let mut public_inputs = example_public_inputs();
    let mut private_inputs = example_private_inputs();
    let relation = example_relation();

    // Create a violation by using a value too big for the type.
    public_inputs.inputs[0] = literal32(EXAMPLE_MODULUS);
    // Create a violation by omitting a private input value.
    private_inputs.inputs.pop().unwrap();

    let mut validator = Validator::new_as_prover();
    validator.ingest_public_inputs(&public_inputs);
    validator.ingest_private_inputs(&private_inputs);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    assert_eq!(
        violations,
        vec![
            "The public value [101, 0, 0, 0] cannot be represented in the Field type specified (101 >= 101).",
            "Not enough private input value to consume.",
        ]
    );

    Ok(())
}

#[test]
fn test_validator_delete_violations() -> crate::Result<()> {
    use crate::producers::examples::*;

    let public_inputs = example_public_inputs();
    let private_inputs = example_private_inputs();
    let mut relation = example_relation();

    relation
        .directives
        .push(Directive::Gate(Gate::Delete(0, 1, 2)));
    relation
        .directives
        .push(Directive::Gate(Gate::Delete(0, 4, 4)));

    let mut validator = Validator::new_as_prover();
    validator.ingest_public_inputs(&public_inputs);
    validator.ingest_private_inputs(&private_inputs);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    assert_eq!(
        violations,
        vec![
            "The wire (0: 1) is used but was not assigned a value, or has been deleted already.",
            "The wire (0: 2) is used but was not assigned a value, or has been deleted already.",
            "The wire (0: 4) is used but was not assigned a value, or has been deleted already.",
        ]
    );

    Ok(())
}

#[test]
fn test_validator_memory_management_violations() -> crate::Result<()> {
    use crate::producers::examples::literal32;
    use crate::structs::function::Function;
    use crate::structs::plugin::PluginBody;
    use crate::structs::wirerange::WireRange;
    use crate::structs::IR_VERSION;

    let mut relation = Relation {
        version: IR_VERSION.to_string(),
        plugins: vec!["vector".to_string()],
        types: vec![Type::Field(literal32(101))],
        conversions: vec![],
        directives: vec![
            Directive::Function(Function::new(
                "square".to_string(),
                vec![Count::new(0, 1)],
                vec![Count::new(0, 1)],
                FunctionBody::Gates(vec![Gate::Mul(0, 0, 1, 1)]),
            )),
            Directive::Function(Function::new(
                "vector_add_2".to_string(),
                vec![Count::new(0, 2)],
                vec![Count::new(0, 2), Count::new(0, 2)],
                FunctionBody::PluginBody(PluginBody {
                    name: "vector".to_string(),
                    operation: "add".to_string(),
                    params: vec!["0".to_string(), "2".to_string()],
                    public_count: HashMap::new(),
                    private_count: HashMap::new(),
                }),
            )),
        ],
    };

    // Violation: Try to delete a portion of an allocation
    relation
        .directives
        .push(Directive::Gate(Gate::Constant(0, 0, vec![0])));
    relation
        .directives
        .push(Directive::Gate(Gate::New(0, 1, 4)));
    relation
        .directives
        .push(Directive::Gate(Gate::Constant(0, 1, vec![0])));
    relation
        .directives
        .push(Directive::Gate(Gate::Constant(0, 2, vec![0])));
    relation
        .directives
        .push(Directive::Gate(Gate::Constant(0, 3, vec![0])));
    relation
        .directives
        .push(Directive::Gate(Gate::Constant(0, 4, vec![0])));
    relation
        .directives
        .push(Directive::Gate(Gate::Delete(0, 0, 2)));

    // Violation: New gate contains a wire already set
    relation
        .directives
        .push(Directive::Gate(Gate::Constant(0, 8, vec![0])));
    relation
        .directives
        .push(Directive::Gate(Gate::New(0, 6, 10)));

    // Violation: New gate contains a wire already allocated
    relation
        .directives
        .push(Directive::Gate(Gate::New(0, 10, 11)));

    // Violation: in an input WireRange of a Call gate, all wires do not belong to the same allocation
    relation
        .directives
        .push(Directive::Gate(Gate::New(0, 20, 22)));
    relation
        .directives
        .push(Directive::Gate(Gate::Constant(0, 20, vec![1])));
    relation
        .directives
        .push(Directive::Gate(Gate::Constant(0, 21, vec![1])));
    relation
        .directives
        .push(Directive::Gate(Gate::Constant(0, 22, vec![1])));
    relation
        .directives
        .push(Directive::Gate(Gate::Constant(0, 23, vec![1])));
    relation.directives.push(Directive::Gate(Gate::Call(
        "vector_add_2".to_string(),
        vec![WireRange::new(24, 25)],
        vec![WireRange::new(20, 21), WireRange::new(22, 23)],
    )));

    let mut validator = Validator::new_as_prover();
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    assert_eq!(
        violations,
        vec![
            "For Delete gates, we should not delete a portion of an allocation.",
            "The wire (0: 8) has already been initialized before. This violates the SSA property.",
            "For New gates, wires should not have already been allocated.",
            "Call: all wires in an input WireRange should belong to the same allocation"
        ]
    );

    Ok(())
}

#[test]
fn test_validator_convert_violations() {
    use crate::producers::examples::literal32;
    use crate::structs::IR_VERSION;

    let public_inputs = PublicInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(literal32(7)),
        inputs: vec![vec![5]],
    };
    let private_inputs = PrivateInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(literal32(101)),
        inputs: vec![vec![10]],
    };
    let relation = Relation {
        version: IR_VERSION.to_string(),
        plugins: vec!["vector".to_string()],
        types: vec![Type::Field(literal32(7)), Type::Field(literal32(101))],
        conversions: vec![Conversion::new(Count::new(1, 1), Count::new(0, 1))],
        directives: vec![
            Directive::Gate(Gate::PublicInput(0, 0)),
            // Violation: in_ids is empty (in_first_id > in_last_id)
            Directive::Gate(Gate::Convert(1, 0, 0, 0, 1, 0)),
            Directive::Gate(Gate::Delete(0, 0, 0)),
            // Violation: use an undeclared conversion
            Directive::Gate(Gate::PrivateInput(1, 10)),
            // Violation: use an undeclared conversion
            Directive::Gate(Gate::Convert(0, 10, 10, 1, 10, 10)),
            Directive::Gate(Gate::Delete(1, 10, 10)),
            Directive::Gate(Gate::Delete(0, 10, 10)),
        ],
    };

    let mut validator = Validator::new_as_prover();
    validator.ingest_public_inputs(&public_inputs);
    validator.ingest_private_inputs(&private_inputs);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    assert_eq!(
        violations,
        vec![
            "Gate::Convert: Error with in_ids: first_id(1) > last_id(0)",
            "Gate::Convert: used an undeclared conversion Conversion(0:1, 1:1)",
        ]
    );
}
