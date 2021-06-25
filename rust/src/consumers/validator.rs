use crate::{Gate, Header, Instance, Message, Relation, Witness, WireId};

use num_bigint::{BigUint, ToBigUint};
use num_traits::identities::One;
use std::collections::{HashSet, HashMap};

use regex::Regex;
use std::cmp::Ordering;

type Var = u64;
type Field = BigUint;

const VERSION_REGEX: &str = r"^\d+.\d+.\d+$";
const IMPLEMENTED_CHECKS: &str = r"
Here is the list of implemented semantic/syntactic checks:

Header Validation
 - Ensure that the characteristic is strictly greater than 1.
 - Ensure that the field degree is exactly 1.
 - Ensure that the version string has the correct format (e.g. matches the following regular expression “^\d+.\d+.\d+$”).
 - Ensure that the profile name is either circ_arithmetic_simple or circ_boolean_simple.
     - If circ_boolean_simple, checks that the field characteristic is exactly 2.
 - Ensure header messages are coherent.
     - Profile names should be identical.
     - Versions should be identical.
     - Field characteristic and field degree should be the same.

Inputs Validation (Instances / Witnesses)
 - Ensure that Instance gates are given a value in Instance messages.
 - Ensure that Witness gates are given a value in Witness messages (prover only).
 - Ensure that no unused Instance or Witness values are given.
 - Ensure that the value they are set to is indeed encoding an element lying in the underlying field. For degree 1 fields, it can be achieved by ensuring that the encoded value is strictly smaller than the field characteristic.

Gates Validation
 - Ensure that gates used are coherent with the profile.
   - @not/@and/@xor are not allowed with circ_arithmetic_simple.
   - @add/@addc/@mul/@mulc are not allowed with circ_boolean_simple.
 - Ensure constants given in @addc/@mulc are actual field elements.
 - Ensure input wires of gates map to an already set variable.
 - Enforce Single Static Assignment by checking that the same wire is used only once as an output wire.
";

#[derive(Clone, Default)]
pub struct Validator {
    as_prover: bool,

    instance_queue_len: usize,
    witness_queue_len: usize,
    live_wires: HashSet<Var>,

    got_header: bool,
    header_profile: String,
    header_version: String,
    is_arithmetic_circuit: bool,

    field_characteristic: Field,
    field_degree: usize,

    // name => (output_count, input_count, instance_count, witness_count, subcircuit)
    known_functions: HashMap<String, (usize, usize, usize, usize, Vec<Gate>)>,

    violations: Vec<String>,
}

impl Validator {
    pub fn new_as_verifier() -> Validator {
        Validator::default()
    }

    pub fn new_as_prover() -> Validator {
        Validator {
            as_prover: true,
            ..Self::default()
        }
    }

    pub fn print_implemented_checks() {
        println!("{}", IMPLEMENTED_CHECKS);
    }

    pub fn get_violations(mut self) -> Vec<String> {
        self.ensure_all_instance_values_consumed();
        self.ensure_all_witness_values_consumed();
        if self.live_wires.len() != 0 {
            println!("WARNING: few variables were not freed.");
        }
        self.violations
    }

    pub fn ingest_message(&mut self, msg: &Message) {
        match msg {
            Message::Instance(i) => self.ingest_instance(&i),
            Message::Witness(w) => self.ingest_witness(&w),
            Message::Relation(r) => self.ingest_relation(&r),
        }
    }

    fn ingest_header(&mut self, header: &Header) {
        if self.got_header {
            // in this case, ensure that headers are compatible
            if self.field_characteristic != BigUint::from_bytes_le(&header.field_characteristic) {
                self.violate("The field_characteristic field is not consistent across headers.");
            }
            if self.field_degree != header.field_degree as usize {
                self.violate("The field_degree is not consistent across headers.");
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
                    if self.field_characteristic != 2.to_biguint().unwrap() {
                        self.violate("With profile 'circ_boolean_simple', the field characteristic can only be 2.");
                    }
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

        // Check values.
        for value in instance.common_inputs.iter() {
            self.ensure_value_in_field(value, || format!("instance value {:?}", value));
        }
        // Provide values on the queue available for Instance gates.
        self.instance_queue_len += instance.common_inputs.len();
    }

    pub fn ingest_witness(&mut self, witness: &Witness) {
        if !self.as_prover {
            self.violate("As verifier, got an unexpected Witness message.");
        }
        self.ingest_header(&witness.header);

        // Check values.
        for value in witness.short_witness.iter() {
            self.ensure_value_in_field(value, || format!("witness value {:?}", value));
        }
        // Provide values on the queue available for Witness gates.
        self.witness_queue_len += witness.short_witness.len();
    }

    pub fn ingest_relation(&mut self, relation: &Relation) {
        self.ingest_header(&relation.header);

        for gate in &relation.gates {
            self.ingest_gate(gate);
        }
    }

    fn ingest_gate(&mut self, gate: &Gate) {
        use Gate::*;

        match gate {
            Constant(out, value) => {
                self.ensure_value_in_field(value, || "Gate::Constant constant".to_string());
                self.ensure_undefined_and_set(*out);
            }

            AssertZero(inp) => {
                self.ensure_defined_and_set(*inp);
            }

            Copy(out, inp) => {
                self.ensure_defined_and_set(*inp);
                self.ensure_undefined_and_set(*out);
            }

            Add(out, left, right) => {
                self.ensure_arithmetic("Add");

                self.ensure_defined_and_set(*left);
                self.ensure_defined_and_set(*right);

                self.ensure_undefined_and_set(*out);
            }

            Mul(out, left, right) => {
                self.ensure_arithmetic("Mul");

                self.ensure_defined_and_set(*left);
                self.ensure_defined_and_set(*right);

                self.ensure_undefined_and_set(*out);
            }

            AddConstant(out, inp, constant) => {
                self.ensure_arithmetic("AddConstant");
                self.ensure_value_in_field(constant, || format!("Gate::AddConstant_{}", *out));
                self.ensure_defined_and_set(*inp);
                self.ensure_undefined_and_set(*out);
            }

            MulConstant(out, inp, constant) => {
                self.ensure_arithmetic("MulConstant");
                self.ensure_value_in_field(constant, || format!("Gate::MulConstant_{}", *out));
                self.ensure_defined_and_set(*inp);
                self.ensure_undefined_and_set(*out);
            }

            And(out, left, right) => {
                self.ensure_boolean("And");
                self.ensure_defined_and_set(*left);
                self.ensure_defined_and_set(*right);
                self.ensure_undefined_and_set(*out);
            }

            Xor(out, left, right) => {
                self.ensure_boolean("Xor");

                self.ensure_defined_and_set(*left);
                self.ensure_defined_and_set(*right);
                self.ensure_undefined_and_set(*out);
            }

            Not(out, inp) => {
                self.ensure_boolean("Not");

                self.ensure_defined_and_set(*inp);
                self.ensure_undefined_and_set(*out);
            }

            Instance(out) => {
                self.declare(*out);
                // Consume value.
                self.consume_instance(1);
            }

            Witness(out) => {
                self.declare(*out);
                // Consume value.
                self.consume_witness(1);
            }

            Free(first, last) => {
                // all wires between first and last INCLUSIVE
                for wire_id in *first..=last.unwrap_or(*first) {
                    self.ensure_defined_and_set(wire_id);
                    self.remove(wire_id);
                }
            }

            Function(name, output_count, input_count, instance_count, witness_count, subcircuit) => {
                // Just record the signature.
                // The validation will be done when the function will be called.
                if self.known_functions.contains_key(name) {
                    self.violate(format!("A function with the name '{}' already exists", name));
                } else {
                    self.known_functions.insert(name.clone(), (*output_count, *input_count, *instance_count, *witness_count, subcircuit.clone()));
                }
            }

            Call(name, output_wires, input_wires) => {
                // - Check exists
                // - Outputs and inputs match function signature
                // - define outputs, check inputs
                // - consume witness.

                let (output_count, input_count, instance_count, witness_count, subcircuit)
                    = if let Some(function_signature) = self.known_functions.get(name).cloned() {
                    function_signature
                } else {
                    self.violate(format!("Unknown Function gate {}", name));
                    return;
                };

                if output_count != output_wires.len() {
                    self.violate("AbstractCall: number of output wires mismatch.");
                }

                if input_count != input_wires.len() {
                    self.violate("AbstractCall: number of input wires mismatch.");
                }

                self.ingest_subcircuit(&subcircuit, output_wires, input_wires, instance_count, witness_count);

                // Now, consume instances and witnesses from self.
                self.consume_instance(instance_count);
                self.consume_witness(witness_count);
                // set the output wires as defined, since we checked they were in each branch.
                output_wires.iter().for_each(|id| self.ensure_undefined_and_set(*id));
            }

            Switch(condition, output_wires, input_wires, instance_count, witness_count, cases, branches) => {
                self.ensure_defined_and_set(*condition);

                // Ensure that the number of cases value match the number of subcircuits.
                if cases.len() != branches.len() {
                    self.violate("Gate::Switch: The number of cases value does not match the number of branches.");
                }

                // If there is no branch, just return.
                // If the list of output wires is not empty, then it's an issue.
                // TODO: check if this is semantic failure.
                if cases.len() == 0 {
                    return;
                }

                // Ensure each value of cases are in the proper field.
                let mut cases_set = HashSet::new();
                for case in cases {
                    self.ensure_value_in_field(case, || format!("Gate::Switch case value: {}", Field::from_bytes_le(case)));
                    cases_set.insert(Field::from_bytes_le(case));
                }

                // ensure that there is no duplicate in cases.
                if cases_set.len() != cases.len() {
                    self.violate("Gate::Switch: The cases values contain duplicates.");
                }

                // 'Execute' each branch of the switch independently, and perform checks
                for branch in branches {
                    // check the current branch
                    self.ingest_subcircuit(branch, output_wires, input_wires, *instance_count, *witness_count);
                }


                // Now, consume instances and witnesses from self.
                self.consume_instance(*instance_count);
                self.consume_witness(*witness_count);
                // set the output wires as defined, since we checked they were in each branch.
                output_wires.iter().for_each(|id| self.ensure_undefined_and_set(*id));
            }

            For(_, _, _, _, _, _, _) => unimplemented!(),
        }
    }

    /// Read the Directive and check its syntactic and semantic validity, returns (input_nbr, instance_nbr, witness_nbr)
    fn ingest_subcircuit(&mut self, subcircuit: &[Gate], output_wires: &[WireId], input_wires: &[WireId], instance_count: usize, witness_count: usize) {
        let mut current_validator = self.clone();
        current_validator.instance_queue_len = instance_count;
        current_validator.witness_queue_len = if self.as_prover {witness_count} else {0};
        current_validator.live_wires = Default::default();
        current_validator.violations = vec![];

        input_wires.iter().for_each(|id| self.ensure_defined_and_set(*id));

        // input wires should be already defined, and they are numbered from
        // output_wire, so we will artificially define them in the inner
        // validator.
        let output_count = output_wires.len();
        let input_count = input_wires.len();

        for wire in output_count..(output_count+input_count) {
            current_validator.live_wires.insert(wire as u64);
        }

        for x in subcircuit.iter() {
            current_validator.ingest_gate(x);
        }

        // ensure that all output wires are set.
        (0..output_count).for_each(|id| current_validator.ensure_defined_and_set(id as u64));

        self.violations.append(&mut current_validator.violations);
        if current_validator.instance_queue_len != 0 {
            self.violate("The subcircuit has not consumed all the instance variables it should have.")
        }
        if current_validator.witness_queue_len != 0 {
            self.violate("The subcircuit has not consumed all the witness variables it should have.")
        }
    }

    fn is_defined(&self, id: Var) -> bool {
        self.live_wires.contains(&id)
    }

    fn declare(&mut self, id: Var) {
        self.live_wires.insert(id);
    }

    fn remove(&mut self, id: Var) {
        if !self.live_wires.remove(&id) {
            self.violate(format!("The variable {} is being freed, but was not defined previously, or has been already freed", id));
        }
    }

    fn consume_instance(&mut self, how_many :usize) {
        if self.instance_queue_len >= how_many {
            self.instance_queue_len -= how_many;
        } else {
            self.violate("Not enough Instance value to consume.");
        }
    }

    fn consume_witness(&mut self, how_many :usize) {
        if self.as_prover {
            if self.witness_queue_len >= how_many {
                self.witness_queue_len -= how_many;
            } else {
                self.violate("Not enough Witness value to consume.");
            }
        }
    }

    fn ensure_defined_and_set(&mut self, id: Var) {
        if !self.is_defined(id) {
            if self.as_prover {
                // in this case, this is a violation, since all variables must have been defined
                // previously
                self.violate(format!(
                    "The wire {} is used but was not assigned a value, or has been freed already.",
                    id
                ));
            }
            // this line is useful to avoid having many times the same message if the validator already
            // detected that this wire was not previously initialized.
            self.declare(id);
        }
    }

    fn ensure_undefined(&mut self, id: Var) {
        if self.is_defined(id) {
            self.violate(format!(
                "The wire {} has already been initialized before. This violates the SSA property.",
                id
            ));
        }
    }

    fn ensure_undefined_and_set(&mut self, id: Var) {
        self.ensure_undefined(id);
        // define it.
        self.declare(id);
    }

    fn ensure_value_in_field(&mut self, value: &[u8], name: impl Fn() -> String) {
        if value.len() == 0 {
            self.violate(format!("The {} is empty.", name()));
        }

        let int = &Field::from_bytes_le(value);
        if int >= &self.field_characteristic {
            let msg = format!(
                "The {} cannot be represented in the field specified in Header ({} >= {}).",
                name(),
                int,
                self.field_characteristic
            );
            self.violate(msg);
        }
    }

    fn ensure_arithmetic(&mut self, gate_name: impl Into<String>) {
        if !self.is_arithmetic_circuit {
            self.violate(format!(
                "Arithmetic gate found ({}), while boolean circuit.",
                &gate_name.into()[..]
            ));
        }
    }

    fn ensure_boolean(&mut self, gate_name: impl Into<String>) {
        if self.is_arithmetic_circuit {
            self.violate(format!(
                "Boolean gate found ({}), while arithmetic circuit.",
                &gate_name.into()[..]
            ));
        }
    }

    fn ensure_all_instance_values_consumed(&mut self) {
        if self.instance_queue_len > 0 {
            self.violate(format!(
                "Too many Instance values ({} not consumed)",
                self.instance_queue_len
            ));
        }
    }

    fn ensure_all_witness_values_consumed(&mut self) {
        if self.as_prover && self.witness_queue_len > 0 {
            self.violate(format!(
                "Too many Witness values ({} not consumed)",
                self.witness_queue_len
            ));
        }
    }

    fn violate(&mut self, msg: impl Into<String>) {
        self.violations.push(msg.into());
        // println!("{}", msg.into());
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
    assert_eq!(violations, Vec::<String>::new());

    Ok(())
}

#[test]
fn test_validator_violations() -> crate::Result<()> {
    use crate::producers::examples::*;

    let mut instance = example_instance();
    let mut witness = example_witness();
    let mut relation = example_relation();

    // Create a violation by using a value too big for the field.
    instance.common_inputs[0] = instance.header.field_characteristic.clone();
    // Create a violation by omitting a witness value.
    witness.short_witness.pop().unwrap();
    // Create a violation by using different headers.
    relation.header.field_characteristic = vec![10];

    let mut validator = Validator::new_as_prover();
    validator.ingest_instance(&instance);
    validator.ingest_witness(&witness);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    assert_eq!(violations, vec![
        "The instance value [101, 0, 0, 0] cannot be represented in the field specified in Header (101 >= 101).",
        "The field_characteristic field is not consistent across headers.",
        "Not enough Witness value to consume.",
    ]);

    Ok(())
}

#[test]
fn test_validator_free_violations() -> crate::Result<()> {
    use crate::producers::examples::*;

    let instance = example_instance();
    let witness = example_witness();
    let mut relation = example_relation();

    relation.gates.push(Gate::Free(1, Some(2)));
    relation.gates.push(Gate::Free(4, None));

    let mut validator = Validator::new_as_prover();
    validator.ingest_instance(&instance);
    validator.ingest_witness(&witness);
    validator.ingest_relation(&relation);

    let violations = validator.get_violations();
    assert_eq!(
        violations,
        vec![
            "The wire 1 is used but was not assigned a value, or has been freed already.",
            "The wire 2 is used but was not assigned a value, or has been freed already.",
            "The wire 4 is used but was not assigned a value, or has been freed already.",
        ]
    );

    Ok(())
}
