use crate::{
    FieldId, Gate, Header, Message, PrivateInputs, PublicInputs, Relation, Result, WireId,
};
use num_bigint::BigUint;
use num_traits::identities::One;
use std::collections::{BTreeSet, HashMap};

use crate::consumers::evaluator::get_field;
use crate::structs::count::{wirelist_to_count_list, CountList};
use crate::structs::value::is_probably_prime;
use crate::structs::wire::{expand_wirelist, is_one_field_wirelist, WireList};
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
 - Ensure constants given in @addc/@mulc are actual field elements.
 - Ensure input wires of gates map to an already set variable.
 - Enforce Single Static Assignment by checking that the same wire is used only once as an output wire.
 - Ensure that for Free gates of the format @free(first, last), we have (last > first).

WireRange Validation
 - Ensure that for WireRange(first, last) that (last > first).
";

#[derive(Clone)]
pub struct Validator {
    as_prover: bool,

    public_inputs_queue_len: CountList,
    private_inputs_queue_len: CountList,
    live_wires: BTreeSet<(FieldId, WireId)>,

    got_header: bool,
    header_version: String,

    fields: Vec<Field>,

    // name => (output_count, input_count, public_count, private_count, subcircuit)
    known_functions: Rc<RefCell<HashMap<String, (CountList, CountList, CountList, CountList)>>>,

    violations: Vec<String>,
}

impl Default for Validator {
    fn default() -> Self {
        Self {
            as_prover: Default::default(),
            public_inputs_queue_len: Default::default(),
            private_inputs_queue_len: Default::default(),
            live_wires: Default::default(),
            got_header: Default::default(),
            header_version: Default::default(),
            fields: Default::default(),
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
            Message::PublicInputs(i) => self.ingest_public_inputs(i),
            Message::PrivateInputs(w) => self.ingest_private_inputs(w),
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

            // Initialize public/private_inputs_queue_len
            self.public_inputs_queue_len = HashMap::new();
            self.private_inputs_queue_len = HashMap::new();
        }
    }

    pub fn ingest_public_inputs(&mut self, public_inputs: &PublicInputs) {
        self.ingest_header(&public_inputs.header);

        // Provide values on the queue available for PublicInput gates.
        for (i, public_inputs_per_field) in public_inputs.inputs.iter().enumerate() {
            assert!(i <= u8::MAX as usize);
            let i = i as u8;
            // Check values.
            for value in &public_inputs_per_field.values {
                self.ensure_value_in_field(&i, value, || format!("public value {:?}", value));
            }
            let count = self.public_inputs_queue_len.entry(i).or_insert(0);
            *count += public_inputs_per_field.values.len() as u64;
        }
    }

    pub fn ingest_private_inputs(&mut self, private_inputs: &PrivateInputs) {
        if !self.as_prover {
            self.violate("As verifier, got an unexpected PrivateInputs message.");
        }
        self.ingest_header(&private_inputs.header);

        // Provide values on the queue available for PrivateInput gates.
        for (i, private_inputs_per_field) in private_inputs.inputs.iter().enumerate() {
            assert!(i <= u8::MAX as usize);
            let i = i as u8;
            // Check values.
            for value in &private_inputs_per_field.values {
                self.ensure_value_in_field(&i, value, || format!("private value {:?}", value));
            }
            let count = self.private_inputs_queue_len.entry(i).or_insert(0);
            *count += private_inputs_per_field.values.len() as u64;
        }
    }

    pub fn ingest_relation(&mut self, relation: &Relation) {
        self.ingest_header(&relation.header);

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
                    (
                        output_count.clone(),
                        input_count.clone(),
                        public_count.clone(),
                        private_count.clone(),
                    ),
                );
            }
            // Now validate the subcircuit.
            self.ingest_subcircuit(
                &f.body,
                &output_count,
                &input_count,
                &public_count,
                &private_count,
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

            PublicInput(field_id, out) => {
                self.declare(field_id, *out);
                // Consume value.
                self.consume_public_inputs(field_id, 1);
            }

            PrivateInput(field_id, out) => {
                self.declare(field_id, *out);
                // Consume value.
                self.consume_private_inputs(field_id, 1);
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
                expanded_inputs.iter().for_each(|(field_id, wire_id)| {
                    self.ensure_defined_and_set(field_id, *wire_id)
                });
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

                let (public_count, private_count) = self
                    .ingest_call(name, output_wires, input_wires)
                    .unwrap_or((HashMap::new(), HashMap::new()));

                // Now, consume public/private inputs from self.
                self.consume_public_count(&public_count);
                self.consume_private_count(&private_count);

                // set the output wires as defined, since we checked they were in each branch.
                expanded_outputs.iter().for_each(|(field_id, wire_id)| {
                    self.ensure_undefined_and_set(field_id, *wire_id)
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

        let (output_count, input_count, public_count, private_count) =
            self.known_functions.borrow().get(name).cloned().unwrap();

        if output_count != wirelist_to_count_list(output_wires) {
            self.violate("Call: number of output wires mismatch.");
        }

        if input_count != wirelist_to_count_list(input_wires) {
            self.violate("Call: number of input wires mismatch.");
        }

        Ok((public_count, private_count))
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
            got_header: self.got_header,
            header_version: self.header_version.clone(),
            fields: self.fields.clone(),
            known_functions: self.known_functions.clone(),
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

    fn consume_public_inputs(&mut self, field_id: &FieldId, how_many: u64) {
        if how_many == 0 {
            return;
        }
        if let Some(count) = self.public_inputs_queue_len.get_mut(field_id) {
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
        for (field_id, count) in public_count.iter() {
            self.consume_public_inputs(field_id, *count);
        }
    }

    fn consume_private_inputs(&mut self, field_id: &FieldId, how_many: u64) {
        if self.as_prover {
            if how_many == 0 {
                return;
            }
            if let Some(count) = self.private_inputs_queue_len.get_mut(field_id) {
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
            for (field_id, count) in private_count.iter() {
                self.consume_private_inputs(field_id, *count);
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

    // Create a violation by using a value too big for the field.
    public_inputs.inputs[0].values[0] = public_inputs.header.fields[0].clone();
    // Create a violation by omitting a witness value.
    private_inputs.inputs[0].values.pop().unwrap();
    // Create a violation by using different headers.
    relation.header.fields = vec![vec![10]];

    let mut validator = Validator::new_as_prover();
    validator.ingest_public_inputs(&public_inputs);
    validator.ingest_private_inputs(&private_inputs);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    assert_eq!(
        violations,
        vec![
            "The public value [101, 0, 0, 0] cannot be represented in the field specified in Header (101 >= 101).",
            "The fields are not consistent across headers.",
            "Not enough private input value to consume.",
        ]
    );

    Ok(())
}

#[test]
fn test_validator_free_violations() -> Result<()> {
    use crate::producers::examples::*;

    let public_inputs = example_public_inputs();
    let private_inputs = example_private_inputs();
    let mut relation = example_relation();

    relation.gates.push(Gate::Free(0, 1, Some(2)));
    relation.gates.push(Gate::Free(0, 4, None));

    let mut validator = Validator::new_as_prover();
    validator.ingest_public_inputs(&public_inputs);
    validator.ingest_private_inputs(&private_inputs);
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
