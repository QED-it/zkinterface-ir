use crate::{Result, Value};
use crate::consumers::evaluator::ZKBackend;
use zkinterface::{BilinearConstraint, StatementBuilder, Sink, Variables};
use zkinterface::ConstraintSystem as zkiConstraintSystem;
use zkinterface::Variables as zkiVariables;
use zkinterface::Witness as zkiWitness;

use num_bigint::BigUint;
use num_traits::{One, Zero};
use std::collections::BTreeMap;

pub struct ToR1CSConverter<S: Sink> {
    builder: StatementBuilder<S>,
    constraints: zkiConstraintSystem,
    constraints_per_message: usize,
    witnesses: Option<zkiWitness>,
    all_assignment: Option<BTreeMap<u64, BigUint>>,
    src_modulus: BigUint,
    byte_len: usize,
    minus_one: u64,
    one: u64,
    zero: u64,
}

impl<S: Sink> ToR1CSConverter<S> {
    pub fn new(sink: S, use_witness: bool) -> Self {
        ToR1CSConverter {
            builder: StatementBuilder::new(sink),
            constraints: zkiConstraintSystem::default(),
            constraints_per_message: 100000,
            witnesses: if use_witness {Some(zkiWitness { assigned_variables: Variables { variable_ids: vec![], values: Some(vec![]) } })} else { None },
            all_assignment: if use_witness {Some(Default::default())} else { None },
            src_modulus: BigUint::zero(),
            byte_len: 0,
            minus_one: 0,
            one: 0,
            zero: 0,
        }
    }

    fn push_constraint(&mut self, co: BilinearConstraint) -> zkinterface::Result<()> {
        self.constraints.constraints.push(co);

        if self.constraints.constraints.len() >= self.constraints_per_message {
            let cs = std::mem::replace(&mut self.constraints, zkiConstraintSystem::default());
            self.builder.push_constraints(cs)?;
        }
        Ok(())
    }

    fn push_witness(&mut self, wire: u64, value: &BigUint) {
        if self.witnesses.is_some() {
            let witness = self.witnesses.as_mut().unwrap();
            witness.assigned_variables.variable_ids.push(wire);
            witness.assigned_variables.values.as_mut().unwrap().append(&mut pad_le_u8_vec(value.to_bytes_le(), self.byte_len));

            if witness.assigned_variables.variable_ids.len() > self.constraints_per_message {
                let wit = std::mem::replace(&mut self.witnesses, Some(zkiWitness::default()));
                let _ = self.builder.push_witness(wit.unwrap());
            }
        }
    }

    fn make_assignment(&mut self, r1cs_wire: u64, val: Option<BigUint>) -> Result<()> {
        if self.all_assignment.is_some() {
            // if self.all_assignment is different from None, then all value must be known (instances / witnesses)
            let val = val.ok_or_else(|| "The value should have been given.")?;

            self.all_assignment.as_mut().unwrap().insert(r1cs_wire, val);
        }
        Ok(())
    }

    pub fn finish(mut self) -> Result<()> {
        self.builder.finish_header()?;
        self.builder.push_constraints(self.constraints)?;
        if let Some(witness) = self.witnesses {
            self.builder.push_witness(witness.to_owned())?;
        }
        Ok(())
    }
}

impl<S: Sink> ZKBackend for ToR1CSConverter<S> {
    type Wire = u64;
    type FieldElement = BigUint;

    fn from_bytes_le(val: &[u8]) -> Result<Self::FieldElement> {
        Ok(BigUint::from_bytes_le(val))
    }

    fn set_field(&mut self, modulus: &[u8], degree: u32, _is_boolean: bool) -> Result<()> {
        self.src_modulus = BigUint::from_bytes_le(modulus);

        let minus_one_buf = (&self.src_modulus - BigUint::one()).to_bytes_le();
        self.byte_len = minus_one_buf.len();
        // Allocate constants 0 / 1 / -1
        self.minus_one = self.builder.allocate_instance_var(&pad_le_u8_vec(minus_one_buf.clone(), self.byte_len));
        self.make_assignment(self.minus_one, Some(Self::from_bytes_le(&minus_one_buf)?))?;

        self.zero = self.builder.allocate_instance_var(&pad_le_u8_vec(vec![0], self.byte_len));
        self.make_assignment(self.zero, Some(BigUint::zero()))?;

        // self.one = self.builder.allocate_instance_var(&pad_le_u8_vec(vec![1], self.byte_len));
        self.one = 0;
        self.make_assignment(self.one, Some(BigUint::one()))?;
        self.builder.header.field_maximum = Some(minus_one_buf);

        // dummy constraints to force use -1 / 0 / 1
        self.push_constraint(BilinearConstraint {
            linear_combination_a: make_combination(vec![self.one, self.minus_one], vec![1, 1]),
            linear_combination_b: make_combination(vec![self.one], vec![1]),
            linear_combination_c: make_combination(vec![self.zero], vec![1]),
        })?;
        if degree != 1 {
            Err("Degree higher than 1 are not supported".into())
        } else {
            Ok(())
        }
    }

    fn one(&self) -> Self::Wire {
        self.one
    }

    fn minus_one(&self) -> Self::Wire {
        self.minus_one
    }

    fn zero(&self) -> Self::Wire {
        self.zero
    }

    fn constant(&mut self, val: Self::FieldElement) -> Result<Self::Wire> {
        let id = self.builder.allocate_instance_var(&pad_le_u8_vec(val.to_bytes_le(), self.byte_len));
        self.make_assignment(id, Some(val))?;
        Ok(id)
    }

    fn assert_zero(&mut self, wire: &Self::Wire) -> Result<()> {
        self.push_constraint(BilinearConstraint {
            linear_combination_a: make_combination(vec![*wire], vec![1]),
            linear_combination_b: make_combination(vec![self.one], vec![1]),
            linear_combination_c: make_combination(vec![self.one], vec![0]),
        })
    }

    fn add(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        let out = self.builder.allocate_var();

        if self.all_assignment.is_some() && self.witnesses.is_some() {
            let assignment = self.all_assignment.as_ref().unwrap();
            // in this case, compute the exact value of the 'correction' to apply.
            let a_val = assignment.get(a).ok_or_else(|| "Add(a): Value does not exist.")?;
            let b_val = assignment.get(b).ok_or_else(|| "Add(b): Value does not exist.")?;

            let sum = a_val + b_val;
            let o_val = sum % &self.src_modulus;

            self.push_witness(out, &o_val);

            self.all_assignment.as_mut().unwrap().insert(out, o_val);
        }
        self.push_constraint(BilinearConstraint {
            linear_combination_a: make_combination(vec![out], vec![1]),
            linear_combination_b: make_combination(vec![self.one], vec![1]),
            linear_combination_c: make_combination(vec![*a, *b], vec![1, 1]),
        })?;
        Ok(out)
    }

    fn multiply(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        let out = self.builder.allocate_var();

        if self.all_assignment.is_some() && self.witnesses.is_some() {
            let assignment = self.all_assignment.as_ref().unwrap();
            // in this case, compute the exact value of the 'correction' to apply.
            let a_val = assignment.get(a).ok_or_else(|| "Multiply(a): Value does not exist.")?;
            let b_val = assignment.get(b).ok_or_else(|| "Multiply(b): Value does not exist.")?;

            let product = a_val * b_val;

            let o_val = product % &self.src_modulus;

            self.push_witness(out, &o_val);

            self.all_assignment.as_mut().unwrap().insert(out, o_val);
        }

        self.push_constraint(BilinearConstraint {
            linear_combination_a: make_combination(vec![*a], vec![1]),
            linear_combination_b: make_combination(vec![*b], vec![1]),
            linear_combination_c: make_combination(vec![out], vec![1]),
        })?;
        Ok(out)
    }

    fn add_constant(&mut self, a: &Self::Wire, b: Self::FieldElement) -> Result<Self::Wire> {
        let out = self.builder.allocate_var();

        if self.all_assignment.is_some() && self.witnesses.is_some() {
            let assignment = self.all_assignment.as_ref().unwrap();
            // in this case, compute the exact value of the 'correction' to apply.
            let a_val = assignment.get(a).ok_or_else(|| "AddConstant: Value does not exist.")?;

            let sum = a_val + &b;

            let o_val = sum % &self.src_modulus;

            self.push_witness(out, &o_val);

            self.all_assignment.as_mut().unwrap().insert(out, o_val);
        }
        self.push_constraint(BilinearConstraint {
            linear_combination_a: make_combination(vec![out], vec![1]),
            linear_combination_b: make_combination(vec![self.one], vec![1]),
            linear_combination_c: make_combination(vec![*a, self.one], pad_to_max(vec![vec![1], b.to_bytes_le()])),
        })?;

        Ok(out)
    }

    fn mul_constant(&mut self, a: &Self::Wire, b: Self::FieldElement) -> Result<Self::Wire> {
        let out = self.builder.allocate_var();

        if self.all_assignment.is_some() && self.witnesses.is_some() {
            let assignment = self.all_assignment.as_ref().unwrap();
            // in this case, compute the exact value of the 'correction' to apply.
            let a_val = assignment.get(a).ok_or_else(|| "MulConstant: Value does not exist.")?;

            let product = a_val * &b;

            let o_val = product % &self.src_modulus;

            self.push_witness(out, &o_val);

            self.all_assignment.as_mut().unwrap().insert(out, o_val);
        }
        self.push_constraint(BilinearConstraint {
            linear_combination_a: make_combination(vec![*a], b.to_bytes_le()),
            linear_combination_b: make_combination(vec![self.one], vec![1]),
            linear_combination_c: make_combination(vec![out], vec![1]),
        })?;

        Ok(out)
    }

    fn and(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        self.multiply(a, b)
    }

    fn xor(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        self.add(a, b)
    }

    fn not(&mut self, a: &Self::Wire) -> Result<Self::Wire> {
        self.add(a, &self.one())
    }

    fn instance(&mut self, val: Self::FieldElement) -> Result<Self::Wire> {
        let id = self.builder.allocate_instance_var(&pad_le_u8_vec(val.to_bytes_le(), self.byte_len));
        self.make_assignment(id, Some(val))?;
        Ok(id)
    }

    fn witness(&mut self, val: Option<Self::FieldElement>) -> Result<Self::Wire> {
        let id = self.builder.allocate_var();
        if self.witnesses.is_none() ^ val.is_none() {
            return Err("Inconsistency.".into());
        }

        self.make_assignment(id, val.clone())?;
        if self.witnesses.is_some() {
            self.push_witness(id, &val.unwrap());
        }
        Ok(id)
    }
}

// pad a little-endian vector of value out to len
// takes vec by value intentionally so ownership can be returned if same length
fn pad_le_u8_vec(v: Value, len: usize) -> Value {
    let vlen = v.len();
    assert!(vlen <= len);
    match vlen.cmp(&len) {
        std::cmp::Ordering::Equal => v.to_vec(),
        // pad as little-endian: add zeroes after set values
        std::cmp::Ordering::Less => [v, vec![0; len - vlen]].concat(),
        // impossible to pad to a shorter length
        std::cmp::Ordering::Greater => panic!("Vector is bigger than expected."),
    }
}

fn make_combination(ids: Vec<u64>, coefficients: Vec<u8>) -> zkiVariables {
    zkiVariables {
        variable_ids: ids,
        values: Some(coefficients),
    }
}

pub fn pad_to_max(vals: Vec<Value>) -> Vec<u8> {
    let max_len = vals.iter().map(|v| v.len()).max().unwrap();

    vals.into_iter()
        .flat_map(|v| pad_le_u8_vec(v, max_len))
        .collect::<Vec<u8>>()
}


#[cfg(test)]
use crate::{
    Source, Instance, Witness, Message,
    producers::{sink::MemorySink, from_r1cs::FromR1CSConverter},
    consumers::evaluator::Evaluator,
};
#[cfg(test)]
use zkinterface::{
    CircuitHeader as zkiCircuitHeader,
    WorkspaceSink, Workspace,
};
#[cfg(test)]
use std::{
    path::PathBuf,
    collections::HashSet,
};


#[cfg(test)]
fn assert_same_io_values(instances: &[Instance], zki_headers: &[zkiCircuitHeader]) -> crate::Result<()> {
    let zki_vals: HashSet<BigUint> = zki_headers.iter()
        .flat_map(|zki_header| {
                zki_header.instance_variables
                    .get_variables()
                    .iter()
                    .map(|v| BigUint::from_bytes_le(&v.value))
                    .collect::<Vec<BigUint>>()
            }
        ).collect();
    let ir_vals: HashSet<BigUint> = instances.iter()
        .flat_map(|instance| {
            instance.common_inputs
                .iter()
                .map(|v| BigUint::from_bytes_le(v))
                .collect::<Vec<BigUint>>()
            }
        ).collect();

    // zkif instances may contain more instance values that IR, since IR constants are translated
    // into ZKIF instances.
    assert!(zki_vals.is_superset(&ir_vals));

    Ok(())
}

#[cfg(test)]
fn assert_same_witness_values(witnesses: &[Witness], zki_witness: &[zkiWitness]) -> crate::Result<()> {
    let zki_vals: HashSet<BigUint> = zki_witness.iter()
        .flat_map(|zki_witness| {
            zki_witness.assigned_variables
                .get_variables()
                .iter()
                .map(|v| BigUint::from_bytes_le(&v.value))
                .collect::<Vec<BigUint>>()
            }
        ).collect();

    let ir_vals: HashSet<BigUint> = witnesses.iter()
        .flat_map(|witness| {
            witness.short_witness
                .iter()
                .map(|v| BigUint::from_bytes_le(v))
                .collect::<Vec<BigUint>>()
        }
        ).collect();

    // zkif witness may (likely does) contain more witness values that IR could confirm
    assert!(zki_vals.is_superset(&ir_vals));

    Ok(())
}

#[test]
fn test_tor1cs_check_witnesses_instances() -> crate::Result<()> {
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;
    use zkinterface::producers::examples::example_constraints as zki_example_constraints;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;

    let output_directory = "local/test_tor1cs_check_witnesses_instances";

    // begin tests as with from_r1cs

    let zki_header = zki_example_header_inputs(3, 4, 25);
    let zki_witness = zki_example_witness_inputs(3, 4);
    let zki_r1cs = zki_example_constraints();

    let mut converter = FromR1CSConverter::new(MemorySink::default(), &zki_header);
    converter.ingest_witness(&zki_witness)?;
    converter.ingest_constraints(&zki_r1cs)?;

    let source: Source = converter.finish().into();
    let ir_messages = source.read_all_messages()?;
    
    let mut to_r1cs = ToR1CSConverter::new(WorkspaceSink::new(&output_directory)?, true);
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut to_r1cs);
    to_r1cs.finish()?;
    let r1cs_violations = evaluator.get_violations();
    assert_eq!(r1cs_violations.len(), 0);

    // now convert back into r1cs
    let workspace = Workspace::from_dir(&PathBuf::from(&output_directory))?;
    let zki_messages = workspace.read_all_messages();


    assert_same_io_values(&ir_messages.instances, &zki_messages.circuit_headers)?;

    assert_same_witness_values(&ir_messages.witnesses, &zki_messages.witnesses)?;

    Ok(())
}

#[test]
fn test_tor1cs_validate_2ways_conversion_same_field() -> crate::Result<()> {
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;
    use zkinterface::producers::examples::example_constraints as zki_example_constraints;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;

    let output_directory = "local/test_tor1cs_validate_2ways_conversion_same_field";

    // begin tests as with from_r1cs

    let zki_header = zki_example_header_inputs(3, 4, 25);
    let zki_witness = zki_example_witness_inputs(3, 4);
    let zki_r1cs = zki_example_constraints();

    let mut converter = FromR1CSConverter::new(MemorySink::default(), &zki_header);
    converter.ingest_witness(&zki_witness)?;
    converter.ingest_constraints(&zki_r1cs)?;

    let source: Source = converter.finish().into();

    let mut to_r1cs = ToR1CSConverter::new(WorkspaceSink::new(&output_directory)?, true);
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut to_r1cs);
    to_r1cs.finish()?;
    let r1cs_violations = evaluator.get_violations();
    assert_eq!(r1cs_violations.len(), 0);

    let workspace = Workspace::from_dir(&PathBuf::from(&output_directory))?;

    // First check that the constraint system is semantically valid
    let mut validator = zkinterface::consumers::validator::Validator::new_as_prover();
    for message in workspace.iter_messages() {
        validator.ingest_message(&message);
    }

    // FromR1CSConverter defines a constant that may be used in some circuit.
    // It's allowed in IR, but not in R1CS. So when converting back to R1CS the example circuit,
    // we should have exaclty one violation, which states the unused variable.
    let validator_violations = validator.get_violations();
    assert_eq!(validator_violations.len(), 1);
    assert_eq!(validator_violations[0], "variable_3 was defined but not used.");

    // Then check that the constraint system is verified
    let mut simulator = zkinterface::consumers::simulator::Simulator::default();
    for message in workspace.iter_messages() {
        simulator.ingest_message(&message);
    }

    let simulator_violations = simulator.get_violations();
    assert_eq!(simulator_violations.len(), 0);

    Ok(())
}

#[test]
fn test_tor1cs_validate_converted_circuit_same_field() -> crate::Result<()> {
    // This time use an example in straight IR
    use crate::producers::examples::*;

    let output_directory = "local/test_tor1cs_validate_converted_circuit_same_field";

    let messages = vec![Ok(Message::Instance(example_instance())), Ok(Message::Witness(example_witness())), Ok(Message::Relation(example_relation()))];

    let mut to_r1cs = ToR1CSConverter::new(WorkspaceSink::new(&output_directory)?, true);
    let evaluator = Evaluator::from_messages(messages.into_iter(), &mut to_r1cs);
    to_r1cs.finish()?;
    let r1cs_violations = evaluator.get_violations();
    assert_eq!(r1cs_violations.len(), 0);


    let workspace = Workspace::from_dir(&PathBuf::from(&output_directory))?;

    // First check that the constraint system is semantically valid
    let mut validator = zkinterface::consumers::validator::Validator::new_as_prover();
    for message in workspace.iter_messages() {
        validator.ingest_message(&message);
    }

    let violations = validator.get_violations();
    if violations.len() > 0 {
        let msg = format!("Violations:\n- {}\n", violations.join("\n- "));
        panic!("{}", msg);
    }

    // Then check that the constraint system is verified
    let mut simulator = zkinterface::consumers::simulator::Simulator::default();
    for message in workspace.iter_messages() {
        simulator.ingest_message(&message);
    }

    let simulator_violations = simulator.get_violations();
    assert_eq!(simulator_violations.len(), 0);

    Ok(())
}
/*
#[test]
fn test_tor1cs_validate_2ways_conversion_bigger_field() -> crate::Result<()> {
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;
    use zkinterface::producers::examples::example_constraints as zki_example_constraints;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;

    let output_directory = "local/test_tor1cs_validate_2ways_conversion_bigger_field";

    // begin tests as with from_r1cs

    let zki_header = zki_example_header_inputs(3, 4, 25);
    let zki_witness = zki_example_witness_inputs(3, 4);
    let zki_r1cs = zki_example_constraints();

    let mut converter = FromR1CSConverter::new(MemorySink::default(), &zki_header);
    converter.ingest_witness(&zki_witness)?;
    converter.ingest_constraints(&zki_r1cs)?;

    let source: Source = converter.finish().into();

    let mut to_r1cs = ToR1CSConverter::new(WorkspaceSink::new(&output_directory)?, true, true);
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut to_r1cs);
    to_r1cs.finish()?;
    let r1cs_violations = evaluator.get_violations();
    assert_eq!(r1cs_violations.len(), 0);

    let workspace = Workspace::from_dir(&PathBuf::from(&output_directory))?;
/*
    // First check that the constraint system is semantically valid
    let mut validator = zkinterface::consumers::validator::Validator::new_as_prover();
    for message in workspace.iter_messages() {
        validator.ingest_message(&message);
    }

    // FromR1CSConverter defines a constant that may be used in some circuit.
    // It's allowed in IR, but not in R1CS. So when converting back to R1CS the example circuit,
    // we should have exaclty one violation, which states the unused variable.
    let validator_violations = validator.get_violations();
    assert_eq!(validator_violations.len(), 1);
    assert_eq!(validator_violations[0], "variable_3 was defined but not used.");
*/
    // Then check that the constraint system is verified
    let mut simulator = zkinterface::consumers::simulator::Simulator::default();
    for mut message in workspace.iter_messages() {
        match message {
            zkinterface::Message::Header(ref mut zkiCircuitHeader) => zkiCircuitHeader.field_maximum = Some(BigUint::from(2305843009213693950 as u64).to_bytes_le()),
            _ => {},
        }
        simulator.ingest_message(&message);
    }

    let simulator_violations = simulator.get_violations();
    println!("{:?}", simulator_violations);
    assert_eq!(simulator_violations.len(), 0);

    Ok(())
}

#[test]
fn test_tor1cs_validate_converted_circuit_bigger_field() -> crate::Result<()> {
    // This time use an example in straight IR
    use crate::producers::examples::*;

    let output_directory = "local/test_tor1cs_validate_2ways_conversion_bigger_field";

    let messages = vec![Ok(Message::Instance(example_instance())), Ok(Message::Witness(example_witness())), Ok(Message::Relation(example_relation()))];

    let mut to_r1cs = ToR1CSConverter::new(WorkspaceSink::new(&output_directory)?, true, true);
    let evaluator = Evaluator::from_messages(messages.into_iter(), &mut to_r1cs);
    to_r1cs.finish()?;
    let r1cs_violations = evaluator.get_violations();
    assert_eq!(r1cs_violations.len(), 0);


    let workspace = Workspace::from_dir(&PathBuf::from(&output_directory))?;
/*
    // First check that the constraint system is semantically valid
    let mut validator = zkinterface::consumers::validator::Validator::new_as_prover();
    for message in workspace.iter_messages() {
        validator.ingest_message(&message);
    }

    let violations = validator.get_violations();
    if violations.len() > 0 {
        let msg = format!("Violations:\n- {}\n", violations.join("\n- "));
        panic!("{}", msg);
    }
*/
    // Then check that the constraint system is verified
    let mut simulator = zkinterface::consumers::simulator::Simulator::default();
    for mut message in workspace.iter_messages() {
        match message {
            zkinterface::Message::Header(ref mut zkiCircuitHeader) => zkiCircuitHeader.field_maximum = Some(BigUint::from(2305843009213693950 as u64).to_bytes_le()),
            _ => {},
        }
        simulator.ingest_message(&message);
    }

    let simulator_violations = simulator.get_violations();
    println!("{:?}", simulator_violations);
    assert_eq!(simulator_violations.len(), 0);

    Ok(())
}
*/
