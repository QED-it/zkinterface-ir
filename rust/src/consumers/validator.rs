use crate::{Header, Relation, Instance, Witness, Messages, Gate};

use std::collections::HashMap;
use num_bigint::BigUint;
use num_traits::identities::One;

use std::cmp::Ordering;
use regex::Regex;

type Var = u64;
type Field = BigUint;

#[derive(Copy, Clone, PartialEq)]
enum Status {
    Undefined,
    Defined,
    Used,
}

use Status::*;

const VERSION_REGEX: &str = r"^\d+.\d+.\d+$";
const IMPLEMENTED_CHECKS: &str = r"
Here is the list of implemented semantic/syntactic checks:
Header Validation
 - Ensure that the characteristic is strictly greater than 1.
 - Ensure that the field degree is exactly 1.
 - Ensure that the version string has the correct format (e.g. matches the following regular expression “^\d+.\d+.\d+$”)
 - Ensure that the profile name is either circ_arithmetic_simple or circ_boolean_simple.
 - Ensure header messages are coherent
     - Profile names should be identical
     - Versions should be identical
     - Field characteristic and field degree should be the same

Inputs Validation (Instances / Witnesses)
 - Ensure that they are not defined more than once
 - Ensure that they are not assigned a value more than once
 - Ensure that the value they are set to is indeed encoding an element lying in the underlying field. For degree 1 fields, it can be achieved by ensuring that the encoded value is strictly smaller than the field characteristic.

Gates Validation
 - Ensure that gates used are coherent with the profile
   - @not/@and/@xor are not allowed with circ_arithmetic_simple
   - @add/@addc/@mul/@mulc are not allowed with circ_boolean_simple
 - Ensure constants used are actual field elements
   - In Assignment, or @addc/@mulc
 - Ensure input wires of gates map to an already set variable
 - Enforce Single Static Assignment by checking that the same wire is used only once as an output wire.
";


#[derive(Clone, Default)]
pub struct Validator {
    as_prover: bool,

    variables: HashMap<Var, Status>,
    got_header: bool,

    field_characteristic: Field,
    field_degree: usize,
    field_bytelen: usize,   // length in bytes of a base field element

    header_profile: String,
    header_version: String,

    is_arithmetic_circuit: bool,

    violations: Vec<String>,
}

impl Validator {
    pub fn new_as_verifier() -> Validator {
        Validator::default()
    }

    pub fn new_as_prover() -> Validator {
        Validator { as_prover: true, ..Self::default() }
    }

    pub fn print_implemented_checks() {
        println!("{}", IMPLEMENTED_CHECKS);
    }

    pub fn ingest_messages(&mut self, messages: &Messages) {
        for instance in &messages.instances {
            self.ingest_instance(instance);
        }
        if self.as_prover {
            for witness in &messages.witnesses {
                self.ingest_witness(witness);
            }
        }
        for relation in &messages.relations {
            self.ingest_relation(relation);
        }
    }

    pub fn get_violations(mut self) -> Vec<String> {
        self.ensure_all_variables_used();
        self.violations
    }

    pub fn ingest_header(&mut self, header: &Header) {
        if self.got_header {
            // in this case, ensure that headers are compatible
            if self.field_characteristic != BigUint::from_bytes_le(&header.field_characteristic) {
                self.violate("The field_characteristic field is not consistent accross headers.");
            }
            if self.field_degree != header.field_degree as usize {
                self.violate("The field_degree is not consistent accross headers.");
            }

            if self.header_profile != header.profile {
                self.violate("The profile name is not consistent across headers.");
            }
            if self.header_version != header.version {
                self.violate("The profile version is not consistent across headers.");
            }
        } else {
            self.got_header = true;

            // Check validity of field_characteristic
            self.field_characteristic = BigUint::from_bytes_le(&header.field_characteristic);
            if self.field_characteristic.cmp(&One::one()) != Ordering::Greater {
                self.violate("The field_characteristic should be > 1");
            }
            self.field_bytelen = header.field_characteristic.len();
            // TODO: check if prime, or in a list of pre-defined primes.


            self.field_degree = header.field_degree as usize;
            if self.field_degree != 1 {
                self.violate("field_degree must be = 1");
            }

            // check Header profile
            self.header_profile = header.profile.clone();
            match &self.header_profile.trim()[..] {
                "circ_arithmetic_simple" => {
                    self.is_arithmetic_circuit = true;
                }
                "circ_boolean_simple" => {
                    self.is_arithmetic_circuit = false;
                }
                _ => {
                    self.violate("The profile name should match either 'circ_arithmetic_simple' or 'circ_boolean_simple'.");
                }
            }

            // check header version
            let re = Regex::new(VERSION_REGEX).unwrap();
            if !re.is_match(header.version.trim()) {
                self.violate("The profile version should match the following format <major>.<minor>.<patch>.");
            }
            self.header_version = header.version.clone();
        }
    }

    pub fn ingest_instance(&mut self, instance: &Instance) {
        self.ingest_header(&instance.header);

        // Set instance variable values.
        for var in instance.common_inputs.iter() {
            self.define(var.id, &var.value, || format!("value of the instance variable_{}", var.id));
            self.set_status(var.id, Used);
        }
    }

    pub fn ingest_witness(&mut self, witness: &Witness) {
        if !self.as_prover {
            self.violate("As verifier, got an unexpected Witness message.");
        }

        self.ingest_header(&witness.header);

        for var in witness.short_witness.iter() {
            self.define(var.id, &var.value, || format!("value of the witness variable_{}", var.id));
            self.set_status(var.id, Used);
        }
    }

    pub fn ingest_relation(&mut self, relation: &Relation) {
        self.ingest_header(&relation.header);

        for gate in &relation.gates {
            match gate {
                Gate::Constant(out, value) => {
                    self.ensure_value_in_field(value, || "Gate::Constant constant".to_string());
                    self.ensure_undefined_and_set(*out);
                }

                Gate::AssertZero(inp) => {
                    self.ensure_defined_and_set(*inp);
                }

                Gate::Copy(out, inp) => {
                    self.ensure_defined_and_set(*inp);
                    self.ensure_undefined_and_set(*out);
                }

                Gate::Add(out, left, right) => {
                    self.ensure_arithmetic("Add");

                    self.ensure_defined_and_set(*left);
                    self.ensure_defined_and_set(*right);

                    self.ensure_undefined_and_set(*out);
                }

                Gate::Mul(out, left, right) => {
                    self.ensure_arithmetic("Mul");

                    self.ensure_defined_and_set(*left);
                    self.ensure_defined_and_set(*right);

                    self.ensure_undefined_and_set(*out);
                }

                Gate::AddConstant(out, inp, constant) => {
                    self.ensure_arithmetic("AddConstant");
                    self.ensure_value_in_field(constant, || format!("Gate::AddConstant_{}", *out));
                    self.ensure_defined_and_set(*inp);
                    self.ensure_undefined_and_set(*out);
                }

                Gate::MulConstant(out, inp, constant) => {
                    self.ensure_arithmetic("MulConstant");
                    self.ensure_value_in_field(constant, || format!("Gate::MulConstant_{}", *out));
                    self.ensure_defined_and_set(*inp);
                    self.ensure_undefined_and_set(*out);
                }

                Gate::And(out, left, right) => {
                    self.ensure_boolean("And");
                    self.ensure_defined_and_set(*left);
                    self.ensure_defined_and_set(*right);
                    self.ensure_undefined_and_set(*out);
                }

                Gate::Xor(out, left, right) => {
                    self.ensure_boolean("Xor");

                    self.ensure_defined_and_set(*left);
                    self.ensure_defined_and_set(*right);
                    self.ensure_undefined_and_set(*out);
                }

                Gate::Not(out, inp) => {
                    self.ensure_boolean("Not");

                    self.ensure_defined_and_set(*inp);
                    self.ensure_undefined_and_set(*out);
                }
            }
        }
    }

    fn status(&mut self, id: Var) -> Status {
        *self.variables.entry(id).or_insert(Undefined)
    }

    fn set_status(&mut self, id: Var, status: Status) {
        self.variables.insert(id, status);
    }

    fn define(&mut self, id: Var, value: &[u8], name: impl Fn() -> String) {
        self.ensure_value_in_field(value, &name);
        if self.status(id) != Undefined {
            self.violate(format!("Multiple definition of the {}", name()));
        }
        self.set_status(id, Defined);
    }

    fn ensure_defined_and_set(&mut self, id: Var) {
        if (self.status(id) == Undefined) && (self.as_prover) {
            // TODO check that the 'id' identifies a witness variable
            // because instance variable SHOULD be defined, even if self.as_prover == false.
            self.violate(format!("The witness variable_{} is used but was not assigned a value", id));
        }
        self.set_status(id, Used);
    }

    fn ensure_undefined_and_set(&mut self, id: Var) {
        if self.status(id) != Undefined {
            self.violate(format!("The variable_{} has already been assigned a value.", id));
        }

        self.set_status(id, Used);
    }

    fn ensure_value_in_field(&mut self, value: &[u8], name: impl Fn() -> String) {
        if value.len() == 0 {
            self.violate(format!("The {} is empty.", name()));
        }

        let int = &Field::from_bytes_le(value);
        if int >= &self.field_characteristic {
            let msg = format!("The {} cannot be represented in the field specified in Header ({} >= {}).", name(), int, self.field_characteristic);
            self.violate(msg);
        }
    }

    fn ensure_arithmetic(&mut self, gate_name: impl Into<String>) {
        if !self.is_arithmetic_circuit {
            self.violate(format!("Arithmetic gate found ({}), while boolean circuit.", &gate_name.into()[..]));
        }
    }

    fn ensure_boolean(&mut self, gate_name: impl Into<String>) {
        if self.is_arithmetic_circuit {
            self.violate(format!("Boolean gate found ({}), while arithmetic circuit.", &gate_name.into()[..]));
        }
    }

    fn ensure_all_variables_used(&mut self) {
        for (id, status) in self.variables.iter() {
            match *status {
                Undefined => self.violations.push(format!("variable_{} was accessed but not defined.", id)),
                Defined => self.violations.push(format!("variable_{} was defined but not used.", id)),
                Used => { /* ok */ }
            }
        }
    }

    fn violate(&mut self, msg: impl Into<String>) {
        self.violations.push(msg.into());
    }
}


#[test]
fn test_validator() -> crate::Result<()> {
    use crate::producers::examples::*;

    let instance = example_instance();
    let witness = example_witness();
    let relation = example_relation();

    let mut validator = Validator::new_as_prover();
    validator.ingest_instance(&instance);
    validator.ingest_witness(&witness);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    assert_eq!(violations.len(), 0);

    Ok(())
}

#[test]
fn test_validator_violations() -> crate::Result<()> {
    use crate::producers::examples::*;

    let mut instance = example_instance();
    let mut witness = example_witness();
    let mut relation = example_relation();

    // Create a violation by using a value too big for the field.
    instance.common_inputs[0].value = instance.header.field_characteristic.clone();
    // Create a violation by using an incorrect ID.
    witness.short_witness[0].id += 10;
    // Create a violation by using different headers.
    relation.header.field_characteristic = vec![10];

    let mut validator = Validator::new_as_prover();
    validator.ingest_instance(&instance);
    validator.ingest_witness(&witness);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    assert_eq!(violations.len(), 3);

    Ok(())
}