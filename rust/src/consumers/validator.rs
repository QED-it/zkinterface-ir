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
            let re = Regex::new(r"^\d+.\d+.\d+$").unwrap();
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
                    self.set_status(*out, Used);
                }

                Gate::AssertZero(inp) => {
                    self.ensure_defined(*inp);
                    self.set_status(*inp, Used);
                }

                Gate::Copy(out, inp) => {
                    self.ensure_defined(*inp);
                    self.set_status(*inp, Used);
                    self.set_status(*out, Used);
                }

                Gate::Add(out, left, right) => {
                    self.ensure_arithmetic("Add");

                    self.ensure_defined(*left);
                    self.set_status(*left, Used);

                    self.ensure_defined(*right);
                    self.set_status(*right, Used);

                    self.set_status(*out, Used);
                }

                Gate::Mul(out, left, right) => {
                    self.ensure_arithmetic("Mul");

                    self.ensure_defined(*left);
                    self.set_status(*left, Used);

                    self.ensure_defined(*right);
                    self.set_status(*right, Used);
                    
                    self.set_status(*out, Used);
                }

                Gate::AddConstant(out, inp, constant) => {
                    self.ensure_arithmetic("AddConstant");
                    self.ensure_value_in_field(constant, || format!("Gate::AddConstant_{}", *out));
                    self.ensure_defined(*inp);
                    self.set_status(*inp, Used);
                    self.set_status(*out, Used);
                }

                Gate::MulConstant(out, inp, constant) => {
                    self.ensure_arithmetic("MulConstant");
                    self.ensure_value_in_field(constant, || format!("Gate::MulConstant_{}", *out));
                    self.ensure_defined(*inp);
                    self.set_status(*inp, Used);
                    self.set_status(*out, Used);
                }

                Gate::And(out, left, right) => {
                    self.ensure_boolean("And");
                    self.ensure_defined(*left);
                    self.set_status(*left, Used);

                    self.ensure_defined(*right);
                    self.set_status(*right, Used);
                    
                    self.set_status(*out, Used);
                }

                Gate::Xor(out, left, right) => {
                    self.ensure_boolean("Xor");

                    self.ensure_defined(*left);
                    self.set_status(*left, Used);

                    self.ensure_defined(*right);
                    self.set_status(*right, Used);
                    
                    self.set_status(*out, Used);
                }

                Gate::Not(out, inp) => {
                    self.ensure_boolean("Not");

                    self.ensure_defined(*inp);
                    self.set_status(*inp, Used);
                    self.set_status(*out, Used);
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

    fn ensure_defined(&mut self, id: Var) {
        if (self.status(id) == Undefined) && (self.as_prover) {
            self.violate(format!("The witness variable_{} is used but was not assigned a value", id));
        }
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
    let relations = example_relation();

    let mut validator = Validator::new_as_prover();
    validator.ingest_instance(&instance);
    validator.ingest_witness(&witness);
    validator.ingest_relation(&relations);

    let violations = validator.get_violations();
    if violations.len() > 0 {
        eprintln!("Violations:\n- {}\n", violations.join("\n- "));
    }

    Ok(())
}