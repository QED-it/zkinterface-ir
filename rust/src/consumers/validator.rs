use num_bigint::BigUint;
use num_bigint_dig;
use num_bigint_dig::prime::probably_prime;
use regex::Regex;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::convert::TryFrom;

use crate::structs::conversion::Conversion;
use crate::structs::count::{count_list_to_hashmap, Count};
use crate::structs::directives::Directive;
use crate::structs::function::{FunctionBody, FunctionCounts};
use crate::structs::types::Type;
use crate::structs::value::value_to_biguint;
use crate::structs::wirerange::{add_types_to_wire_ranges, WireRange};
use crate::{Gate, Message, PrivateInputs, PublicInputs, Relation, TypeId, WireId};

type TypeElement = BigUint;

// Tips: to write regex, use the following website (and select Python as the type of REGEX
//    https://regex101.com/r/AHPOmp/1

/// Used to check the validity of the version.
const VERSION_REGEX: &str = r"^\d+.\d+.\d+$";
/// Used to check the validity of names of functions, names of plugins, names of operation, string params in plugin
const NAMES_REGEX: &str = r"^[a-zA-Z_][\w]*(?:(?:\.|:{2})[a-zA-Z_][\w]*)*$";
const NUMERICAL_REGEX: &str = r"^[0-9]+$";
const IMPLEMENTED_CHECKS: &str = r"
VERSION_REGEX = “^\d+.\d+.\d+“
STRING_REGEX = “^[a-zA-Z_][\w]*(?:(?:\.|:{2})[a-zA-Z_][\w]*)*$“
INTEGER_REGEX = “^[0-9]+“

Here is the list of implemented semantic/syntactic checks:

Header Validation
 - Ensure that the version string matches VERSION_REGEX
 - Ensure that all messages (Relation, PublicInputs, PrivateInputs) have the same version

Type Validation
 - Ensure that for Field type, modulo is prime (that implies modulo is strictly greater than 1)
 - Ensure for Plugin type, that
     - plugin name has been declared
     - name, operation and string parameters match STRING_REGEX
     - numerical parameters match INTEGER_REGEX

Inputs Validation (PublicInputs / PrivateInputs Messages)
 - Ensure that there are enough values in PublicInputs messages for the evaluation of the circuit.
 - Ensure that there are enough values in PrivateInputs messages for the evaluation of the circuit (prover only).
 - Ensure that all public and private inputs are consumed at the end of the circuit
 - Ensure that all public and private inputs are encoding elements lying in the underlying type.
   It can be achieved by ensuring that for Field type, the encoded value is strictly smaller than the type modulo.

Plugin declaration Validation
 - Ensure that all declared plugin names match STRING_REGEX

Conversion declaration Validation
 - Ensure that for each conversion declaration, input and output types have been defined in types and are Field types
 - Ensure that for each conversion declaration, input and output counts are strictly greater than 0

Count Validation
 - Ensure that Count.count is strictly greater than 0
 - Ensure that Count.type is defined in types

Function declaration Validation
 - Ensure that the function name matches STRING_REGEX
 - Ensure that input and output counts are correct
   - Count.count is strictly greated than 0
   - Count.type is defined in types
 - Ensure for Plugin function, that
     - plugin name has been declared
     - name, operation and string parameters match STRING_REGEX
     - numerical parameters match INTEGER_REGEX

Standard Gate Validation (@add, @mul, @addc, @mulc, @copy, @constant, @assert_zero)
 - Ensure that the type index refers to a Field type
 - Ensure that constants given in @addc/@mulc/@constant are actual type elements
   (i.e. the encoded value is strictly smaller than the type modulo)
 - Ensure that input wires of gates map to an already set variable.
 - Ensure that output wires of gates are unset and have not been previously deleted (Single Static Assignment)

Memory Management Validation
 - Ensure for New/Delete gates of the format @new/delete(type_id, first, last), that we have (last >= first).
 - Ensure for New gates of the format @new(type_id, first, last), that all wires between first and
   last inclusive are not already allocated, set or have not been previously deleted.
 - Ensure for Delete gates of the format @delete(type_id, first, last), that all wires between first
   and last inclusive are previously been set and have not been previously deleted.
 - Ensure that no Delete gate deallocates only part of an allocation. 

WireRange Validation (used in @convert and @call gates)
 - Ensure for WireRange(first, last) that (last >= first).

Convert Gate Validation
 - Ensure that each convert gate maps to a declared conversion
 - Ensure that input and output types are Field types
 - Ensure that input wires are set
 - Ensure that input wires are part of a single allocation
 - Ensure that output wires are unset and have not been previously deleted
 - Ensure that output wires are either part of a single allocation or are all unallocated
   (if they are unallocated, the output range will be implicitly allocated as with @new gate)

Call Gate Validation
 - Ensure that each call gate maps to a declared function
 - Ensure that input wires are set
 - Ensure for each input range, that wires are part of a single allocation
 - Ensure that output wires are unset and have not been previously deleted
 - Ensure for each output range, that wires are either part of a single allocation or are all unallocated
   (if they are unallocated, the output range will be implicitly allocated as with @new gate)
";

#[derive(Clone, Default)]
pub struct Validator {
    as_prover: bool,

    public_inputs_counts: HashMap<ValidatorType, u64>,
    private_inputs_counts: HashMap<ValidatorType, u64>,
    live_wires: BTreeSet<(TypeId, WireId)>,
    deleted_wires: BTreeSet<(TypeId, WireId)>,
    // (type_id, first_wire, last_wire)
    allocations: HashSet<(TypeId, WireId, WireId)>,

    version: String,

    types: Vec<ValidatorType>,

    known_plugins: HashSet<String>,
    known_conversions: HashSet<Conversion>,
    // name => (output_count, input_count, public_count, private_count)
    known_functions: HashMap<String, FunctionCounts>,

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

    pub fn ingest_public_inputs(&mut self, public_inputs: &PublicInputs) {
        // Check version
        self.check_version(&public_inputs.version);

        // Convert Type into ValidatorType and check inputs if the type is a Field type
        let validator_type = match &public_inputs.type_value {
            Type::Field(modulo) => {
                // If the type is a Field, ensure that all values belong to this field.
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

        // Check ValidatorType
        // (normally, we ingest public inputs before ingesting relations. Thus, no plugin has already
        // been declared)
        self.check_validator_type(&validator_type);

        // Update public_inputs_counts
        let count = self.public_inputs_counts.entry(validator_type).or_insert(0);
        *count += public_inputs.inputs.len() as u64;
    }

    pub fn ingest_private_inputs(&mut self, private_inputs: &PrivateInputs) {
        if !self.as_prover {
            self.violate("As verifier, got an unexpected PrivateInputs message.");
        }

        // Check version
        self.check_version(&private_inputs.version);

        // Convert Type into ValidatorType and check inputs if the type is a Field type
        let validator_type = match &private_inputs.type_value {
            Type::Field(modulo) => {
                // If the type is a Field, ensure that all values belong to this field.
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

        // Check ValidatorType
        // (normally, we ingest public inputs before ingesting relations. Thus, no plugin has already
        // been declared)
        self.check_validator_type(&validator_type);

        // Update private_inputs_counts
        let count = self
            .private_inputs_counts
            .entry(validator_type)
            .or_insert(0);
        *count += private_inputs.inputs.len() as u64;
    }

    pub fn ingest_relation(&mut self, relation: &Relation) {
        // Check version
        self.check_version(&relation.version);

        // If it is the first relation (types is empty), we have to check and ingest types, plugins and conversions
        if self.types.is_empty() {
            // Check and ingest plugins
            relation.plugins.iter().for_each(|plugin_name| {
                // Check plugin name
                let re_str = Regex::new(NAMES_REGEX).unwrap();
                if !re_str.is_match(plugin_name) {
                    self.violate(format!(
                        "The plugin name ({}) should match the proper format ({}).",
                        plugin_name, NAMES_REGEX
                    ));
                }
                // Insert plugin name into known_plugins
                self.known_plugins.insert(plugin_name.clone());
            });
            // Check and ingest types
            relation.types.iter().for_each(|ir_type| {
                // Convert IR type into ValidatorType
                let validator_type = match &ir_type {
                    Type::Field(modulo) => {
                        let biguint_modulo = value_to_biguint(modulo);
                        ValidatorType::Field(biguint_modulo)
                    }
                    Type::PluginType(name, operation, params) => {
                        ValidatorType::PluginType(name.clone(), operation.clone(), params.clone())
                    }
                };
                // Check type
                if !self.check_validator_type(&validator_type) {
                    self.violate(
                        "When declaring a Plugin type, the plugin name should be declared",
                    );
                }
                // Insert ValidatorType into types
                self.types.push(validator_type);
            });
            // Check and ingest conversions
            relation.conversions.iter().for_each(|conversion| {
                // Check conversion
                // Check input type is defined and is a Field type
                self.ensure_field_type(&conversion.input_count.type_id);
                // Check output type is defined and is a Field type
                self.ensure_field_type(&conversion.output_count.type_id);
                // Check input count > 0
                if conversion.input_count.count == 0 {
                    self.violate(
                        "When declaring a conversion, the input count should be strictly greater than 0.",
                    );
                }
                // Check output count > 0
                if conversion.output_count.count == 0 {
                    self.violate(
                        "When declaring a conversion, the output count should be strictly greater than 0.",
                    );
                }
                // Insert conversion into conversions
                self.known_conversions.insert(conversion.clone());
            });
        } else {
            // It is not the first relation that we ingest, plugins, types and conversion must be empty
            if !relation.plugins.is_empty() {
                self.violate("It is not the first ingested relation, plugins should be empty.");
            }
            if !relation.types.is_empty() {
                self.violate("It is not the first ingested relation, types should be empty.");
            }
            if !relation.conversions.is_empty() {
                self.violate("It is not the first ingested relation, conversions should be empty.");
            }
        }

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

                    // Check input_count
                    input_count.iter().for_each(|count| self.check_count(count));

                    // Check output_count
                    output_count
                        .iter()
                        .for_each(|count| self.check_count(count));

                    // Validate the body
                    let (public_count, private_count) = match &function.body {
                        FunctionBody::Gates(gates) => {
                            // Validate the subcircuit for custom functions
                            // And return public_count and private_count
                            self.ingest_custom_function(gates, &output_count, &input_count)
                        }
                        FunctionBody::PluginBody(plugin_body) => {
                            // Check plugin name, operation and params have the correct format
                            self.check_plugin_content(
                                &plugin_body.name,
                                &plugin_body.operation,
                                &plugin_body.params,
                            );

                            // Check plugin public and private counts
                            plugin_body
                                .public_count
                                .iter()
                                .for_each(|(type_id, count)| {
                                    self.check_count(&Count::new(*type_id, *count))
                                });
                            plugin_body
                                .private_count
                                .iter()
                                .for_each(|(type_id, count)| {
                                    self.check_count(&Count::new(*type_id, *count))
                                });

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

                    // Record the signature
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
                self.ensure_field_type(type_id);
                self.ensure_value_in_type_id(type_id, value, || {
                    "Gate::Constant constant".to_string()
                });
                self.ensure_undefined_not_deleted_and_set(type_id, *out);
            }

            AssertZero(type_id, inp) => {
                self.ensure_field_type(type_id);
                self.ensure_defined_and_set(type_id, *inp);
            }

            Copy(type_id, out, inp) => {
                self.ensure_field_type(type_id);
                self.ensure_defined_and_set(type_id, *inp);
                self.ensure_undefined_not_deleted_and_set(type_id, *out);
            }

            Add(type_id, out, left, right) => {
                self.ensure_field_type(type_id);
                self.ensure_defined_and_set(type_id, *left);
                self.ensure_defined_and_set(type_id, *right);

                self.ensure_undefined_not_deleted_and_set(type_id, *out);
            }

            Mul(type_id, out, left, right) => {
                self.ensure_field_type(type_id);
                self.ensure_defined_and_set(type_id, *left);
                self.ensure_defined_and_set(type_id, *right);

                self.ensure_undefined_not_deleted_and_set(type_id, *out);
            }

            AddConstant(type_id, out, inp, constant) => {
                self.ensure_field_type(type_id);
                self.ensure_value_in_type_id(type_id, constant, || {
                    format!("Gate::AddConstant_{}", *out)
                });
                self.ensure_defined_and_set(type_id, *inp);
                self.ensure_undefined_not_deleted_and_set(type_id, *out);
            }

            MulConstant(type_id, out, inp, constant) => {
                self.ensure_field_type(type_id);
                self.ensure_value_in_type_id(type_id, constant, || {
                    format!("Gate::MulConstant_{}", *out)
                });
                self.ensure_defined_and_set(type_id, *inp);
                self.ensure_undefined_not_deleted_and_set(type_id, *out);
            }

            Public(type_id, out) => {
                self.ensure_undefined_not_deleted_and_set(type_id, *out);
                // Consume value.
                self.consume_public_inputs(type_id, 1);
            }

            Private(type_id, out) => {
                self.ensure_undefined_not_deleted_and_set(type_id, *out);
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
                // Ensure wires have not already been deleted and are not already set
                (*first..=*last).for_each(|wire_id| {
                    self.ensure_not_deleted(type_id, wire_id);
                    self.ensure_undefined(type_id, wire_id)
                });

                // Ensure wires have not already been allocated
                if self.belong_to_allocations(type_id, first, last) {
                    self.violate("For New gates, wires should not have already been allocated.");
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
                // - remove them (i.e. remove them from live_wires and add them into deleted_wires)
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
                self.ensure_field_type(out_type_id);
                self.ensure_field_type(in_type_id);

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

                // Ensure inputs are already set
                (*in_first_id..=*in_last_id)
                    .for_each(|wire_id| self.ensure_defined_and_set(in_type_id, wire_id));

                // Ensure inputs belong to a single allocation
                if !self.belong_to_a_single_allocation(in_type_id, in_first_id, in_last_id) {
                    self.violate("Gate::Convert: all inputs do not belong to the same allocation)")
                }

                // Ensure that output wires are unset, have not already been deleted and set them
                (*out_first_id..=*out_last_id).for_each(|wire_id| {
                    self.ensure_undefined_not_deleted_and_set(out_type_id, wire_id)
                });

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
            }

            Call(name, out_ids, in_ids) => {
                // Check input and output WireRanges
                out_ids
                    .iter()
                    .for_each(|wire_range| self.check_wire_range(wire_range));
                in_ids
                    .iter()
                    .for_each(|wire_range| self.check_wire_range(wire_range));

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

                        // Ensure that the output wires have not already been deleted
                        // Set the output wires as defined
                        // Check out_ids WireRange allocations
                        let out_ids_with_types_result =
                            add_types_to_wire_ranges(out_ids, &func_counts.output_count);
                        if let Ok(out_ids_with_types) = out_ids_with_types_result {
                            out_ids_with_types.iter().for_each(|wire_range_with_type| {
                                (wire_range_with_type.first_id..=wire_range_with_type.last_id)
                                    .for_each(|wire_id| {
                                        // Ensure that output wires are unset, have not already been deleted and set them
                                        self.ensure_undefined_not_deleted_and_set(
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
                Gate::Public(type_id, _) => {
                    let count = public_count.entry(*type_id).or_insert(0);
                    *count += 1;
                }
                Gate::Private(type_id, _) => {
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
            deleted_wires: Default::default(),
            allocations: Default::default(),
            version: self.version.clone(),
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
        self.deleted_wires.insert((*type_id, id));
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

    /// This function ensures that type is defined and is a Field type
    fn ensure_field_type(&mut self, type_id: &TypeId) {
        let type_option = self.types.get(usize::try_from(*type_id).unwrap());
        match type_option {
            Some(validator_type) => match validator_type {
                ValidatorType::Field(_) => (),
                ValidatorType::PluginType(_, _, _) => {
                    self.violate(
                        "Standard and conversion gates should be used with Field type only",
                    );
                }
            },
            None => {
                self.violate(format!("Type id {} is not defined.", *type_id));
            }
        };
    }

    /// This function
    /// - ensures that (type_id, id) is undefined
    /// - ensures that (type_id, id) has not already been deleted
    /// - deines (type_id, id)
    fn ensure_undefined_not_deleted_and_set(&mut self, type_id: &TypeId, id: WireId) {
        self.ensure_undefined(type_id, id);
        self.ensure_not_deleted(type_id, id);
        // define it.
        self.declare(type_id, id);
    }

    fn ensure_not_deleted(&mut self, type_id: &TypeId, id: WireId) {
        if self.deleted_wires.contains(&(*type_id, id)) {
            self.violate(format!(
                "The wire ({}: {}) has been deleted before. It is no longer possible to re-use it.",
                *type_id, id
            ));
        }
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

    /// This function performs the following checks
    /// - name, operation and string parameters match the NAMES_REGEX
    /// - numerical parameters match the NUMERICAL_REGEX
    fn check_plugin_content(&mut self, name: &str, operation: &str, params: &[String]) {
        let re_str = Regex::new(NAMES_REGEX).unwrap();
        let re_num = Regex::new(NUMERICAL_REGEX).unwrap();
        if !re_str.is_match(name) {
            self.violate(format!(
                "The plugin name ({}) should match the proper format ({}).",
                name, NAMES_REGEX
            ));
        }
        if !re_str.is_match(operation) {
            self.violate(format!(
                "The plugin operation ({}) should match the proper format ({}).",
                operation, NAMES_REGEX
            ));
        }
        params.iter().for_each(|param| {
            if !(re_str.is_match(param) || re_num.is_match(param)) {
                self.violate(format!(
                    "Each plugin param ({}) should match the proper format ({} or {}).",
                    param, NAMES_REGEX, NUMERICAL_REGEX
                ));
            }
        });
    }

    /// The function performs the following checks on a ValidatorType
    /// Ensure that for Field type, modulo is prime
    /// Ensure for Plugin type, that
    /// - name, operation and string parameters have the correct format
    ///   (i.e. matches the following regular expression “^[a-zA-Z_][\w]*(?:(?:\.|:{2})[a-zA-Z_][\w]*)*$“)
    /// - numerical parameters have the correct format
    ///   (i.e. matches the following regular expression “^[1-9][0-9]*$“)
    /// This function returns
    /// - for Field type, true
    /// - for Plugin type, true if the plugin name has been declared and false otherwise
    fn check_validator_type(&mut self, validator_type: &ValidatorType) -> bool {
        match validator_type {
            ValidatorType::Field(modulo) => {
                if !is_probably_prime(modulo) {
                    self.violate("All Field type should have a prime modulo.")
                }
                true
            }
            ValidatorType::PluginType(name, operation, params) => {
                self.check_plugin_content(name, operation, params);
                self.known_plugins.contains(name)
            }
        }
    }

    /// This function
    /// - if there is no version, stores version in self.version and ensures that version matches VERSION_REGEX
    /// - If there is a version, check that self.version and version are identical
    fn check_version(&mut self, version: &str) {
        if self.version.is_empty() {
            let re_version = Regex::new(VERSION_REGEX).unwrap();
            if !re_version.is_match(version) {
                self.violate(format!(
                    "The version ({}) should match the proper format ({}).",
                    version, VERSION_REGEX
                ));
            }
            self.version = version.to_string();
        } else if self.version != *version {
            self.violate("The version is not consistent across resources.");
        }
    }

    /// This function ensures that
    /// - the type in count has been defined
    /// - the count in count is strictly greater than 0
    fn check_count(&mut self, count: &Count) {
        if (count.type_id as usize) >= self.types.len() {
            self.violate("In a Count, count.type_id is not defined.");
        }
        if count.count == 0 {
            self.violate("In a Count, count.count should be strictly greater than 0.");
        }
    }

    /// This function ensures that wire_range.first_id <= wire_range.last_id
    fn check_wire_range(&mut self, wire_range: &WireRange) {
        if wire_range.first_id > wire_range.last_id {
            self.violate("In a WireRange, last_id should be greater than first_id.");
        }
    }
}

#[test]
fn test_validator() {
    use crate::producers::examples::*;

    let public_inputs = example_public_inputs();
    let private_inputs = example_private_inputs();
    let relation = example_relation();

    let mut validator = Validator::new_as_prover();

    public_inputs
        .iter()
        .for_each(|inputs| validator.ingest_public_inputs(inputs));
    private_inputs
        .iter()
        .for_each(|inputs| validator.ingest_private_inputs(inputs));
    validator.ingest_relation(&relation);

    assert_eq!(validator.get_violations(), Vec::<String>::new());
}

#[test]
fn test_validator_as_verifier() {
    use crate::producers::examples::*;

    let public_inputs = example_public_inputs();
    let relation = example_relation();

    let mut validator = Validator::new_as_verifier();

    public_inputs
        .iter()
        .for_each(|inputs| validator.ingest_public_inputs(inputs));
    validator.ingest_relation(&relation);

    assert_eq!(validator.get_violations(), Vec::<String>::new());
}

#[test]
fn test_validator_violations() {
    use crate::structs::IR_VERSION;

    let public_inputs = PublicInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(vec![7]),
        // Create a violation by using a value too big for the type.
        inputs: vec![vec![7]],
    };
    let private_inputs = PrivateInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(vec![7]),
        inputs: vec![vec![3]],
    };
    let relation = Relation {
        version: IR_VERSION.to_string(),
        plugins: vec![],
        types: vec![Type::Field(vec![7])],
        conversions: vec![],
        directives: vec![
            Directive::Gate(Gate::Private(0, 0)),
            // Create a violation by omitting a private input value.
            Directive::Gate(Gate::Private(0, 1)),
            // Consume all public inputs
            Directive::Gate(Gate::Public(0, 2)),
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
            "The public value [7] cannot be represented in the Field type specified (7 >= 7).",
            "Not enough private input value to consume.",
        ]
    );
}

#[test]
fn test_validator_delete_violations() {
    use crate::producers::simple_examples::*;

    let public_inputs = simple_example_public_inputs();
    let private_inputs = simple_example_private_inputs();
    let mut relation = simple_example_relation();

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
}

#[test]
fn test_validator_memory_management_violations() {
    use crate::structs::function::Function;
    use crate::structs::plugin::PluginBody;
    use crate::structs::wirerange::WireRange;
    use crate::structs::IR_VERSION;

    let mut relation = Relation {
        version: IR_VERSION.to_string(),
        plugins: vec!["vector".to_string()],
        types: vec![Type::Field(vec![101])],
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
}

#[test]
fn test_validator_convert_violations() {
    use crate::structs::IR_VERSION;

    let public_inputs = PublicInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(vec![7]),
        inputs: vec![vec![5]],
    };
    let private_inputs = PrivateInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(vec![101]),
        inputs: vec![vec![10]],
    };
    let relation = Relation {
        version: IR_VERSION.to_string(),
        plugins: vec!["vector".to_string()],
        types: vec![Type::Field(vec![7]), Type::Field(vec![101])],
        conversions: vec![Conversion::new(Count::new(1, 1), Count::new(0, 1))],
        directives: vec![
            Directive::Gate(Gate::Public(0, 0)),
            // Violation: in_ids is empty (in_first_id > in_last_id)
            Directive::Gate(Gate::Convert(1, 0, 0, 0, 1, 0)),
            Directive::Gate(Gate::Delete(0, 0, 0)),
            // Violation: use an undeclared conversion
            Directive::Gate(Gate::Private(1, 10)),
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

pub fn is_probably_prime(value: &BigUint) -> bool {
    let value = num_bigint_dig::BigUint::from_bytes_le(&value.to_bytes_le());
    probably_prime(&value, 10)
}

#[test]
fn test_is_probably_prime() {
    let value = BigUint::from_bytes_le(&[187]); // 187=11*17
    assert!(!is_probably_prime(&value));

    let value = BigUint::from_bytes_le(&[101]);
    assert!(is_probably_prime(&value));

    let value = BigUint::from_bytes_le(&[1]);
    assert!(!is_probably_prime(&value));

    let value = BigUint::from_bytes_le(&[0]);
    assert!(!is_probably_prime(&value));
}

#[test]
fn test_validator_with_functions_with_several_input_output_types() {
    use crate::structs::function::Function;
    use crate::structs::IR_VERSION;

    let public_inputs = PublicInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(vec![7]),
        inputs: vec![vec![3], vec![5], vec![5]],
    };
    let private_inputs = PrivateInputs {
        version: IR_VERSION.to_string(),
        type_value: Type::Field(vec![101]),
        inputs: vec![vec![10], vec![20], vec![100]],
    };
    let relation = Relation {
        version: IR_VERSION.to_string(),
        plugins: vec![],
        types: vec![Type::Field(vec![7]), Type::Field(vec![101])],
        conversions: vec![
            Conversion::new(Count::new(0, 1), Count::new(1, 1)),
            Conversion::new(Count::new(1, 1), Count::new(0, 1)),
        ],
        directives: vec![
            Directive::Gate(Gate::New(0, 0, 1)),
            Directive::Gate(Gate::New(1, 0, 1)),
            Directive::Gate(Gate::Public(0, 0)),
            Directive::Gate(Gate::Public(0, 1)),
            Directive::Gate(Gate::Private(1, 0)),
            Directive::Gate(Gate::Private(1, 1)),
            Directive::Function(Function::new(
                "custom".to_string(),
                vec![Count::new(0, 1), Count::new(1, 1)],
                vec![Count::new(0, 2), Count::new(1, 2)],
                FunctionBody::Gates(vec![
                    // 3 + 5 = 1 mod 7
                    Gate::Add(0, 3, 1, 2),
                    // 1 mod 7 -> 1 mod 101
                    Gate::Convert(1, 0, 0, 0, 3, 3),
                    // 10 + 20 = 30 mod 101
                    Gate::Add(1, 3, 1, 2),
                    // 30 mod 101 -> 2 mod 7
                    Gate::Convert(0, 0, 0, 1, 3, 3),
                ]),
            )),
            Directive::Gate(Gate::Call(
                "custom".to_string(),
                vec![WireRange::new(2, 2), WireRange::new(2, 2)],
                vec![WireRange::new(0, 1), WireRange::new(0, 1)],
            )),
            Directive::Gate(Gate::Public(0, 3)),
            Directive::Gate(Gate::Add(0, 4, 2, 3)),
            Directive::Gate(Gate::AssertZero(0, 4)),
            Directive::Gate(Gate::Private(1, 3)),
            Directive::Gate(Gate::Add(1, 4, 2, 3)),
            Directive::Gate(Gate::AssertZero(1, 4)),
            Directive::Gate(Gate::Delete(0, 0, 4)),
            Directive::Gate(Gate::Delete(1, 0, 4)),
        ],
    };

    let mut validator = Validator::new_as_prover();
    validator.ingest_public_inputs(&public_inputs);
    validator.ingest_private_inputs(&private_inputs);
    validator.ingest_relation(&relation);

    assert_eq!(validator.get_violations(), Vec::<String>::new());
}
