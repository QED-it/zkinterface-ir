use crate::{Gate, Header, Message, PrivateInputs, PublicInputs, Relation, Result, TypeId, WireId};
use num_bigint::BigUint;
use num_traits::identities::One;
use std::collections::{BTreeSet, HashMap, HashSet};

use crate::consumers::evaluator::get_modulo;
use crate::structs::count::{wirelist_to_count_list, CountList};
use crate::structs::function::FunctionBody;
use crate::structs::value::is_probably_prime;
use crate::structs::wire::{expand_wirelist, is_one_type_wirelist, WireList};
use regex::Regex;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::rc::Rc;

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

#[derive(Clone)]
pub struct Validator {
    as_prover: bool,

    public_inputs_queue_len: CountList,
    private_inputs_queue_len: CountList,
    live_wires: BTreeSet<(TypeId, WireId)>,
    // (type_id, first_wire, last_wire)
    new_gates: HashSet<(TypeId, WireId, WireId)>,

    got_header: bool,
    header_version: String,

    moduli: Vec<TypeElement>,

    known_plugins: HashSet<String>,
    // name => (output_count, input_count, public_count, private_count)
    known_functions: Rc<RefCell<HashMap<String, FunctionCount>>>,

    violations: Vec<String>,
}

#[derive(Clone)]
pub struct FunctionCount {
    output_count: CountList,
    input_count: CountList,
    public_count: CountList,
    private_count: CountList,
}

impl Default for Validator {
    fn default() -> Self {
        Self {
            as_prover: Default::default(),
            public_inputs_queue_len: Default::default(),
            private_inputs_queue_len: Default::default(),
            live_wires: Default::default(),
            new_gates: Default::default(),
            got_header: Default::default(),
            header_version: Default::default(),
            moduli: Default::default(),
            known_plugins: Default::default(),
            known_functions: Rc::new(RefCell::new(HashMap::default())),
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

    fn ingest_header(&mut self, header: &Header) {
        if self.got_header {
            // in this case, ensure that headers are compatible
            if self.moduli.len() != header.types.len() {
                self.violate("The types are not consistent across headers.");
            }
            for (previous_modulo, new_modulo) in self.moduli.iter().zip(header.types.iter()) {
                if previous_modulo != &BigUint::from_bytes_le(new_modulo) {
                    self.violate("The types are not consistent across headers.");
                    break;
                }
            }

            if self.header_version != header.version {
                self.violate("The profile version is not consistent across headers.");
            }
        } else {
            self.got_header = true;

            // Check validity of type values
            for modulo in &header.types {
                let biguint_modulo = BigUint::from_bytes_le(modulo).clone();
                self.moduli.push(biguint_modulo.clone());
                if biguint_modulo.cmp(&One::one()) != Ordering::Greater {
                    self.violate("All type moduli should be > 1");
                }
                if !is_probably_prime(modulo) {
                    self.violate("All type moduli should be a prime.")
                }
            }

            // check header version
            let re = Regex::new(VERSION_REGEX).unwrap();
            if !re.is_match(header.version.trim()) {
                self.violate("The profile version should match the following format <major>.<minor>.<patch>.");
            }
            self.header_version = header.version.clone();

            // Initialize public/private_inputs_queue_len
            self.public_inputs_queue_len = HashMap::new();
            self.private_inputs_queue_len = HashMap::new();
        }
    }

    pub fn ingest_public_inputs(&mut self, public_inputs: &PublicInputs) {
        self.ingest_header(&public_inputs.header);

        // Provide values on the queue available for PublicInput gates.
        for (i, public_inputs_per_type) in public_inputs.inputs.iter().enumerate() {
            assert!(i <= u8::MAX as usize);
            let i = i as u8;
            // Check values.
            for value in &public_inputs_per_type.values {
                self.ensure_value_in_type(&i, value, || format!("public value {:?}", value));
            }
            let count = self.public_inputs_queue_len.entry(i).or_insert(0);
            *count += public_inputs_per_type.values.len() as u64;
        }
    }

    pub fn ingest_private_inputs(&mut self, private_inputs: &PrivateInputs) {
        if !self.as_prover {
            self.violate("As verifier, got an unexpected PrivateInputs message.");
        }
        self.ingest_header(&private_inputs.header);

        // Provide values on the queue available for PrivateInput gates.
        for (i, private_inputs_per_type) in private_inputs.inputs.iter().enumerate() {
            assert!(i <= u8::MAX as usize);
            let i = i as u8;
            // Check values.
            for value in &private_inputs_per_type.values {
                self.ensure_value_in_type(&i, value, || format!("private value {:?}", value));
            }
            let count = self.private_inputs_queue_len.entry(i).or_insert(0);
            *count += private_inputs_per_type.values.len() as u64;
        }
    }

    pub fn ingest_relation(&mut self, relation: &Relation) {
        self.ingest_header(&relation.header);

        relation.plugins.iter().for_each(|plugin| {
            self.known_plugins.insert(plugin.clone());
        });

        for f in relation.functions.iter() {
            let (name, output_count, input_count, public_count, private_count) = (
                f.name.clone(),
                f.output_count.clone(),
                f.input_count.clone(),
                f.public_count.clone(),
                f.private_count.clone(),
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
                    FunctionCount {
                        output_count: output_count.clone(),
                        input_count: input_count.clone(),
                        public_count: public_count.clone(),
                        private_count: private_count.clone(),
                    },
                );
            }
            // Now validate the body
            match &f.body {
                FunctionBody::Gates(gates) => {
                    // Validate the subcircuit for custom functions
                    self.ingest_subcircuit(
                        gates,
                        &output_count,
                        &input_count,
                        &public_count,
                        &private_count,
                    );
                }
                FunctionBody::PluginBody(plugin_body) => {
                    // Check that the plugin name has been declared
                    if !self.known_plugins.contains(&plugin_body.name) {
                        self.violate(format!(
                            "The plugin '{}' has not been declared",
                            plugin_body.name
                        ));
                    }
                }
            }
        }

        for gate in &relation.gates {
            self.ingest_gate(gate);
        }
    }

    fn ingest_gate(&mut self, gate: &Gate) {
        use Gate::*;

        match gate {
            Constant(type_id, out, value) => {
                self.ensure_value_in_type(type_id, value, || "Gate::Constant constant".to_string());
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
                self.ensure_value_in_type(type_id, constant, || {
                    format!("Gate::AddConstant_{}", *out)
                });
                self.ensure_defined_and_set(type_id, *inp);
                self.ensure_undefined_and_set(type_id, *out);
            }

            MulConstant(type_id, out, inp, constant) => {
                self.ensure_value_in_type(type_id, constant, || {
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
                if last <= first {
                    self.violate(format!(
                        "For New gates, last WireId ({}) must be strictly greater than first WireId ({}).",
                        last, first
                    ));
                }
                // Ensure wires have not already been allocated by another New gate
                for wire_id in *first..=*last {
                    if self.belong_to_new_gates(type_id, &wire_id) {
                        self.violate(
                            "For New gates, wires must not have already been allocated by another New gate.");
                        break;
                    }
                }
                // Ensure wires are not already set
                for wire_id in *first..=*last {
                    self.ensure_undefined(type_id, wire_id);
                }
                // Add this new wire range into new_gates
                self.new_gates.insert((*type_id, *first, *last));
            }

            Delete(type_id, first, last) => {
                // first < last
                if let Some(last_id) = last {
                    if last_id <= first {
                        self.violate(format!(
                            "For Delete gates, last WireId ({}) must be strictly greater than first WireId ({}).",
                            last_id, first
                        ));
                    }
                    // Check whether the Delete gate match a preceding New gate
                    // If so, remove the preceding New gate from new_wire_ranges
                    if self.new_gates.contains(&(*type_id, *first, *last_id)) {
                        self.new_gates.remove(&(*type_id, *first, *last_id));
                    }
                }

                // Check whether some wires belong to a New gate
                // If a New gate matches this Delete gate, we have already removed it from new_gates HashSet.
                // Thus, if a wire still belongs to a New gate, it means that we have a violation.
                for wire_id in *first..=last.unwrap_or(*first) {
                    if self.belong_to_new_gates(type_id, &wire_id) {
                        self.violate(format!(
                            "For Delete gates, ({}:{}) cannot be deleted because it has been allocated by a New gate and this Delete gate does not match it.",
                            type_id, wire_id
                        ));
                        break;
                    }
                }

                // For all wires between first and last INCLUSIVE
                // - check that they are already set
                // - delete them
                for wire_id in *first..=last.unwrap_or(*first) {
                    self.ensure_defined_and_set(type_id, wire_id);
                    self.remove(type_id, wire_id);
                }
            }

            Convert(output_wires, input_wires) => {
                // Check that input_wires is not empty and all input wires belong to the same type
                if let Err(err) = is_one_type_wirelist(input_wires) {
                    self.violate(format!("Gate::Convert: Error with input wires: {}", err))
                }

                // Check that output_wires is not empty and all output wires belong to the same type
                if let Err(err) = is_one_type_wirelist(output_wires) {
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
                expanded_inputs
                    .iter()
                    .for_each(|(type_id, wire_id)| self.ensure_defined_and_set(type_id, *wire_id));

                // Set the output wires as defined
                expanded_outputs.iter().for_each(|(type_id, wire_id)| {
                    self.ensure_undefined_and_set(type_id, *wire_id)
                });
            }

            AnonCall(output_wires, input_wires, public_count, private_count, subcircuit) => {
                let expanded_outputs = expand_wirelist(output_wires).unwrap_or_else(|err| {
                    self.violate(err.to_string());
                    vec![]
                });
                let expanded_inputs = expand_wirelist(input_wires).unwrap_or_else(|err| {
                    self.violate(err.to_string());
                    vec![]
                });

                // Check inputs are already set
                expanded_inputs
                    .iter()
                    .for_each(|(type_id, wire_id)| self.ensure_defined_and_set(type_id, *wire_id));
                // ingest it and validate it.
                self.ingest_subcircuit(
                    subcircuit,
                    &wirelist_to_count_list(output_wires),
                    &wirelist_to_count_list(input_wires),
                    public_count,
                    private_count,
                );

                // Now, consume public/private inputs from self.
                self.consume_public_count(public_count);
                self.consume_private_count(private_count);

                // set the output wires as defined, since we checked they were in each branch.
                expanded_outputs.iter().for_each(|(type_id, wire_id)| {
                    self.ensure_undefined_and_set(type_id, *wire_id)
                });
            }

            Call(name, output_wires, input_wires) => {
                // - Check exists
                // - Outputs and inputs match function signature
                // - define outputs, check inputs
                // - consume public/private inputs.
                let expanded_outputs = expand_wirelist(output_wires).unwrap_or_else(|err| {
                    self.violate(err.to_string());
                    vec![]
                });
                let expanded_inputs = expand_wirelist(input_wires).unwrap_or_else(|err| {
                    self.violate(err.to_string());
                    vec![]
                });

                expanded_inputs
                    .iter()
                    .for_each(|(type_id, wire_id)| self.ensure_defined_and_set(type_id, *wire_id));

                let (public_count, private_count) = self
                    .ingest_call(name, output_wires, input_wires)
                    .unwrap_or((HashMap::new(), HashMap::new()));

                // Now, consume public/private inputs from self.
                self.consume_public_count(&public_count);
                self.consume_private_count(&private_count);

                // set the output wires as defined, since we checked they were in each branch.
                expanded_outputs.iter().for_each(|(type_id, wire_id)| {
                    self.ensure_undefined_and_set(type_id, *wire_id)
                });
            }
        }
    }

    /// Ingest an equivalent of the AbstractGateCall, along with the expanded outputs list.
    /// It will not set the output_wires as defined in the current validator, as well as it will not
    /// consume public and private inputs of the current validator. It's up to the caller
    /// to do so whenever necessary.
    /// It returns the tuple (public_count, private_count)
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

        let function_count = self.known_functions.borrow().get(name).cloned().unwrap();

        if function_count.output_count != wirelist_to_count_list(output_wires) {
            self.violate("Call: number of output wires mismatch.");
        }

        if function_count.input_count != wirelist_to_count_list(input_wires) {
            self.violate("Call: number of input wires mismatch.");
        }

        Ok((function_count.public_count, function_count.private_count))
    }

    /// This function will check the semantic validity of all the gates in the subcircuit.
    /// It will ensure that all input variable are currently well defined before entering this
    /// subcircuit, and will check that the subcircuit actually correctly set the given number of
    /// output wires, and that public and private inputs variables declared are actually consumed.
    /// To do so, it creates a local validator, and appends the violations found by it to the
    /// current validator object.
    /// NOTE: it will @b not consume public/private inputs from the current validator, and will @b not
    /// set output variables of the current validator as set.
    /// If required, this should be done in the caller function.
    fn ingest_subcircuit(
        &mut self,
        subcircuit: &[Gate],
        output_count: &CountList,
        input_count: &CountList,
        public_count: &CountList,
        private_count: &CountList,
    ) {
        let mut current_validator = Validator {
            as_prover: self.as_prover,
            public_inputs_queue_len: public_count.clone(),
            private_inputs_queue_len: if self.as_prover {
                private_count.clone()
            } else {
                HashMap::new()
            },
            live_wires: Default::default(),
            new_gates: Default::default(),
            got_header: self.got_header,
            header_version: self.header_version.clone(),
            moduli: self.moduli.clone(),
            known_plugins: self.known_plugins.clone(),
            known_functions: self.known_functions.clone(),
            violations: vec![],
        };

        // input wires should be already defined, and they are numbered from
        // output_wire, so we will artificially define them in the inner
        // validator.
        for (type_id, count) in input_count.iter() {
            let first_idx = *output_count.get(type_id).unwrap_or(&0);
            for wire_id in first_idx..(first_idx + *count) {
                current_validator.live_wires.insert((*type_id, wire_id));
            }
        }

        for x in subcircuit.iter() {
            current_validator.ingest_gate(x);
        }

        // ensure that all output wires are set.
        for (output_type_id, count) in output_count.iter() {
            (0..*count)
                .for_each(|id| current_validator.ensure_defined_and_set(output_type_id, id as u64));
        }

        self.violations.append(&mut current_validator.violations);
        if current_validator
            .public_inputs_queue_len
            .values()
            .sum::<u64>()
            != 0
        {
            self.violate(
                "The subcircuit has not consumed all the public inputs variables it should have.",
            )
        }
        if current_validator
            .private_inputs_queue_len
            .values()
            .sum::<u64>()
            != 0
        {
            self.violate(
                "The subcircuit has not consumed all the private inputs variables it should have.",
            )
        }
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

    fn consume_public_inputs(&mut self, type_id: &TypeId, how_many: u64) {
        if how_many == 0 {
            return;
        }
        if let Some(count) = self.public_inputs_queue_len.get_mut(type_id) {
            if *count >= how_many {
                *count -= how_many;
            } else {
                *count = 0;
                self.violate("Not enough public input value to consume.");
            }
        } else {
            self.violate("Not enough public input value to consume.");
        }
    }

    fn consume_public_count(&mut self, public_count: &CountList) {
        for (type_id, count) in public_count.iter() {
            self.consume_public_inputs(type_id, *count);
        }
    }

    fn consume_private_inputs(&mut self, type_id: &TypeId, how_many: u64) {
        if self.as_prover {
            if how_many == 0 {
                return;
            }
            if let Some(count) = self.private_inputs_queue_len.get_mut(type_id) {
                if *count >= how_many {
                    *count -= how_many;
                } else {
                    *count = 0;
                    self.violate("Not enough private input value to consume.");
                }
            } else {
                self.violate("Not enough private input value to consume.");
            }
        }
    }

    fn consume_private_count(&mut self, private_count: &CountList) {
        if self.as_prover {
            for (type_id, count) in private_count.iter() {
                self.consume_private_inputs(type_id, *count);
            }
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

    fn ensure_value_in_type(&mut self, type_id: &TypeId, value: &[u8], name: impl Fn() -> String) {
        if value.is_empty() {
            self.violate(format!("The {} is empty.", name()));
        }

        let modulo = get_modulo(type_id, &self.moduli);
        let modulo = match modulo {
            Err(_) => {
                self.violate(format!("Type id {} is not defined.", *type_id));
                return;
            }
            Ok(modulo) => modulo,
        };
        let int = &TypeElement::from_bytes_le(value);
        if int >= modulo {
            let msg = format!(
                "The {} cannot be represented in the type specified in Header ({} >= {}).",
                name(),
                int,
                modulo
            );
            self.violate(msg);
        }
    }

    fn ensure_all_public_values_consumed(&mut self) {
        let public_inputs_not_consumed: u64 = self.public_inputs_queue_len.values().sum::<u64>();
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
        let private_inputs_not_consumed: u64 = self.private_inputs_queue_len.values().sum::<u64>();
        if private_inputs_not_consumed != 0 {
            self.violate(format!(
                "Too many private input values ({} not consumed)",
                private_inputs_not_consumed
            ));
        }
    }

    fn violate(&mut self, msg: impl Into<String>) {
        self.violations.push(msg.into());
        // println!("{}", msg.into());
    }

    fn belong_to_new_gates(&self, type_id: &TypeId, wire_id: &WireId) -> bool {
        for (new_type_id, first, last) in self.new_gates.iter() {
            if (*new_type_id == *type_id) && *first <= *wire_id && *last >= *wire_id {
                return true;
            }
        }
        false
    }
}

#[test]
fn test_validator() -> Result<()> {
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
fn test_validator_as_verifier() -> Result<()> {
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
fn test_validator_violations() -> Result<()> {
    use crate::producers::examples::*;

    let mut public_inputs = example_public_inputs();
    let mut private_inputs = example_private_inputs();
    let mut relation = example_relation();

    // Create a violation by using a value too big for the type.
    public_inputs.inputs[0].values[0] = public_inputs.header.types[0].clone();
    // Create a violation by omitting a private input value.
    private_inputs.inputs[0].values.pop().unwrap();
    // Create a violation by using different headers.
    relation.header.types = vec![vec![10]];

    let mut validator = Validator::new_as_prover();
    validator.ingest_public_inputs(&public_inputs);
    validator.ingest_private_inputs(&private_inputs);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    assert_eq!(
        violations,
        vec![
            "The public value [101, 0, 0, 0] cannot be represented in the type specified in Header (101 >= 101).",
            "The types are not consistent across headers.",
            "Not enough private input value to consume.",
        ]
    );

    Ok(())
}

#[test]
fn test_validator_delete_violations() -> Result<()> {
    use crate::producers::examples::*;

    let public_inputs = example_public_inputs();
    let private_inputs = example_private_inputs();
    let mut relation = example_relation();

    relation.gates.push(Gate::Delete(0, 1, Some(2)));
    relation.gates.push(Gate::Delete(0, 4, None));

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
fn test_validator_new_violations() -> Result<()> {
    use crate::producers::examples::*;

    let public_inputs = example_public_inputs();
    let private_inputs = example_private_inputs();
    let mut relation = example_relation();

    // Violation: Delete contains a wire allocated with a New gate but does not match this New gate
    relation.gates.push(Gate::Constant(0, 99, vec![0]));
    relation.gates.push(Gate::New(0, 100, 103));
    relation.gates.push(Gate::Constant(0, 100, vec![0]));
    relation.gates.push(Gate::Constant(0, 101, vec![0]));
    relation.gates.push(Gate::Constant(0, 102, vec![0]));
    relation.gates.push(Gate::Constant(0, 103, vec![0]));
    relation.gates.push(Gate::Delete(0, 99, Some(101)));

    // Violation: New gate contains a wire already set
    relation.gates.push(Gate::Constant(0, 107, vec![0]));
    relation.gates.push(Gate::New(0, 105, 109));

    // Violation: New gate contains a wire already allocated in another New gate
    relation.gates.push(Gate::New(0, 109, 110));

    let mut validator = Validator::new_as_prover();
    validator.ingest_public_inputs(&public_inputs);
    validator.ingest_private_inputs(&private_inputs);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    assert_eq!(
        violations,
        vec![
            "For Delete gates, (0:100) cannot be deleted because it has been allocated by a New gate and this Delete gate does not match it.",
            "The wire (0: 107) has already been initialized before. This violates the SSA property.",
            "For New gates, wires must not have already been allocated by another New gate."
        ]
    );

    Ok(())
}
