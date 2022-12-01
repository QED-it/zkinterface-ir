use crate::consumers::evaluator::ZKBackend;
use crate::structs::count::Count;
use crate::structs::plugin::PluginBody;
use crate::structs::types::Type;
use crate::{Result, TypeId, Value, WireId};
use zkinterface::ConstraintSystem as zkiConstraintSystem;
use zkinterface::Variables as zkiVariables;
use zkinterface::Witness as zkiWitness;
use zkinterface::{BilinearConstraint, Sink, StatementBuilder, Variables};

use num_bigint::BigUint;
use num_traits::{One, Zero};
use std::collections::{BTreeMap, HashMap};

pub struct ToR1CSConverter<S: Sink> {
    builder: StatementBuilder<S>,
    constraints: zkiConstraintSystem,
    constraints_per_message: usize,
    use_witness: bool,
    witnesses: zkiWitness,
    all_assignment: BTreeMap<WireId, BigUint>,
    use_correction: bool,
    src_modulus: BigUint,
    byte_len: usize,
    one: u64,
}

impl<S: Sink> ToR1CSConverter<S> {
    pub fn new(sink: S, use_witness: bool, use_correction: bool) -> Self {
        ToR1CSConverter {
            builder: StatementBuilder::new(sink),
            constraints: zkiConstraintSystem::default(),
            constraints_per_message: 100000,
            use_witness,
            witnesses: zkiWitness {
                assigned_variables: Variables {
                    variable_ids: vec![],
                    values: Some(vec![]),
                },
            },
            all_assignment: Default::default(),
            use_correction,
            src_modulus: BigUint::zero(),
            byte_len: 0,
            one: 0,
        }
    }

    fn push_constraint(&mut self, co: BilinearConstraint) -> zkinterface::Result<()> {
        self.constraints.constraints.push(co);

        if self.constraints.constraints.len() >= self.constraints_per_message {
            let cs = std::mem::take(&mut self.constraints);
            self.builder.push_constraints(cs)?;
        }
        Ok(())
    }

    fn push_witness(&mut self, wire: u64, value: &BigUint) {
        if self.use_witness {
            self.witnesses.assigned_variables.variable_ids.push(wire);
            self.witnesses
                .assigned_variables
                .values
                .as_mut()
                .unwrap()
                .append(&mut pad_le_u8_vec(value.to_bytes_le(), self.byte_len));

            if self.witnesses.assigned_variables.variable_ids.len() > self.constraints_per_message {
                let wit = std::mem::take(&mut self.witnesses);
                let _ = self.builder.push_witness(wit);
            }
        }
    }

    fn make_assignment(&mut self, r1cs_wire: u64, val: Option<BigUint>) -> Result<()> {
        if self.use_witness {
            // if self.use_witness is true, then all value must be known (instances / witnesses)
            let val = val.ok_or("The value should have been given.")?;

            self.all_assignment.insert(r1cs_wire, val);
        }
        Ok(())
    }

    pub fn finish(mut self) -> Result<()> {
        self.builder.finish_header()?;
        self.builder.push_constraints(self.constraints)?;
        if self.use_witness {
            self.builder.push_witness(self.witnesses.to_owned())?;
        }
        Ok(())
    }
}

impl<S: Sink> ZKBackend for ToR1CSConverter<S> {
    // Wire id
    type Wire = u64;
    type TypeElement = BigUint;

    fn from_bytes_le(val: &[u8]) -> Result<Self::TypeElement> {
        Ok(value_to_biguint(val))
    }

    fn set_types(&mut self, types: &[Type]) -> Result<()> {
        if self.src_modulus.is_zero() {
            if types.len() != 1 {
                return Err("One type must be defined to convert to R1CS.".into());
            }
            let first_type = types
                .get(0)
                .ok_or("One type must be defined to convert to R1CS.")?;
            let mut modulus: &[u8] =
                match first_type {
                    Type::Field(modulo) => modulo,
                    Type::PluginType(_, _, _) => return Err(
                        "To convert to R1CS, one type must be defined and it must be a Field type."
                            .into(),
                    ),
                };
            // This assumes that finite field elements can be zero padded in their byte reprs. For prime
            // types, this assumes that the byte representation is little-endian.
            while modulus.last() == Some(&0) {
                modulus = &modulus[0..modulus.len() - 1];
            }

            // modulus
            self.src_modulus = value_to_biguint(modulus);

            self.byte_len = modulus.len();

            self.one = 0; // spec convention
            self.make_assignment(self.one, Some(BigUint::one()))?;

            self.builder.header.field_maximum = Some(self.minus_one(&0)?.to_bytes_le());
        }

        Ok(())
    }

    fn one(&self) -> Result<Self::TypeElement> {
        Ok(BigUint::one())
    }

    fn minus_one(&self, type_id: &TypeId) -> Result<Self::TypeElement> {
        if *type_id != 0 {
            return Err("All type ids must be equal to 0 to be able to convert into R1CS.".into());
        }
        if self.src_modulus.is_zero() {
            return Err("Modulus is not initiated, used `set_types()` before calling.".into());
        }
        Ok(&self.src_modulus - self.one()?)
    }

    fn zero(&self) -> Result<Self::TypeElement> {
        Ok(BigUint::zero())
    }

    fn copy(&mut self, type_id: &TypeId, wire: &Self::Wire) -> Result<Self::Wire> {
        if *type_id != 0 {
            return Err("All type ids must be equal to 0 to be able to convert into R1CS.".into());
        }
        Ok(*wire)
    }

    fn constant(&mut self, type_id: &TypeId, val: Self::TypeElement) -> Result<Self::Wire> {
        if *type_id != 0 {
            return Err("All type ids must be equal to 0 to be able to convert into R1CS.".into());
        }
        let id = self
            .builder
            .allocate_instance_var(&pad_le_u8_vec(val.to_bytes_le(), self.byte_len));
        self.make_assignment(id, Some(val))?;
        Ok(id)
    }

    fn assert_zero(&mut self, type_id: &TypeId, wire: &Self::Wire) -> Result<()> {
        if *type_id != 0 {
            return Err("All type ids must be equal to 0 to be able to convert into R1CS.".into());
        }
        self.push_constraint(BilinearConstraint {
            linear_combination_a: make_combination(vec![*wire], vec![1]),
            linear_combination_b: make_combination(vec![self.one], vec![1]),
            linear_combination_c: make_combination(vec![self.one], vec![0]),
        })
    }

    fn add(&mut self, type_id: &TypeId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        if *type_id != 0 {
            return Err("All types ids must be equal to 0 to be able to convert into R1CS.".into());
        }
        let out = self.builder.allocate_var();
        let correction_wire = if self.use_correction {
            self.builder.allocate_var()
        } else {
            0
        };

        if self.use_witness {
            // in this case, compute the exact value of the 'correction' to apply.
            let a_val = self
                .all_assignment
                .get(a)
                .ok_or("Add(a): Value does not exist.")?;
            let b_val = self
                .all_assignment
                .get(b)
                .ok_or("Add(b): Value does not exist.")?;

            let sum = a_val + b_val;
            let correction = &sum / &self.src_modulus;
            let o_val = sum % &self.src_modulus;

            if self.use_correction {
                self.push_witness(correction_wire, &correction);
            }
            self.push_witness(out, &o_val);

            self.all_assignment.insert(out, o_val);
        }

        if self.use_correction {
            self.push_constraint(BilinearConstraint {
                linear_combination_a: make_combination(
                    vec![out, correction_wire],
                    pad_to_max(vec![vec![1], self.src_modulus.to_bytes_le()]),
                ),
                linear_combination_b: make_combination(vec![self.one], vec![1]),
                linear_combination_c: make_combination(vec![*a, *b], vec![1, 1]),
            })?;
        } else {
            self.push_constraint(BilinearConstraint {
                linear_combination_a: make_combination(vec![out], vec![1]),
                linear_combination_b: make_combination(vec![self.one], vec![1]),
                linear_combination_c: make_combination(vec![*a, *b], vec![1, 1]),
            })?;
        }
        Ok(out)
    }

    fn multiply(&mut self, type_id: &TypeId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        if *type_id != 0 {
            return Err("All type ids must be equal to 0 to be able to convert into R1CS.".into());
        }
        let out = self.builder.allocate_var();
        let correction_wire = if self.use_correction {
            self.builder.allocate_var()
        } else {
            0
        };

        if self.use_witness {
            // in this case, compute the exact value of the 'correction' to apply.
            let a_val = self
                .all_assignment
                .get(a)
                .ok_or("Multiply(a): Value does not exist.")?;
            let b_val = self
                .all_assignment
                .get(b)
                .ok_or("Multiply(b): Value does not exist.")?;

            let product = a_val * b_val;
            let correction = &product / &self.src_modulus;
            let o_val = product % &self.src_modulus;

            if self.use_correction {
                self.push_witness(correction_wire, &correction);
            }
            self.push_witness(out, &o_val);

            self.all_assignment.insert(out, o_val);
        }
        if self.use_correction {
            self.push_constraint(BilinearConstraint {
                linear_combination_a: make_combination(vec![*a], vec![1]),
                linear_combination_b: make_combination(vec![*b], vec![1]),
                linear_combination_c: make_combination(
                    vec![out, correction_wire],
                    pad_to_max(vec![vec![1], self.src_modulus.to_bytes_le()]),
                ),
            })?;
        } else {
            self.push_constraint(BilinearConstraint {
                linear_combination_a: make_combination(vec![*a], vec![1]),
                linear_combination_b: make_combination(vec![*b], vec![1]),
                linear_combination_c: make_combination(vec![out], vec![1]),
            })?;
        }
        Ok(out)
    }

    fn add_constant(
        &mut self,
        type_id: &TypeId,
        a: &Self::Wire,
        b: Self::TypeElement,
    ) -> Result<Self::Wire> {
        if *type_id != 0 {
            return Err("All type ids must be equal to 0 to be able to convert into R1CS.".into());
        }
        let out = self.builder.allocate_var();
        let correction_wire = if self.use_correction {
            self.builder.allocate_var()
        } else {
            0
        };

        if self.use_witness {
            // in this case, compute the exact value of the 'correction' to apply.
            let a_val = self
                .all_assignment
                .get(a)
                .ok_or("AddConstant: Value does not exist.")?;

            let sum = a_val + &b;
            let correction = &sum / &self.src_modulus;
            let o_val = sum % &self.src_modulus;

            if self.use_correction {
                self.push_witness(correction_wire, &correction);
            }
            self.push_witness(out, &o_val);

            self.all_assignment.insert(out, o_val);
        }

        if self.use_correction {
            self.push_constraint(BilinearConstraint {
                linear_combination_a: make_combination(
                    vec![out, correction_wire],
                    pad_to_max(vec![vec![1], self.src_modulus.to_bytes_le()]),
                ),
                linear_combination_b: make_combination(vec![self.one], vec![1]),
                linear_combination_c: make_combination(
                    vec![*a, self.one],
                    pad_to_max(vec![vec![1], b.to_bytes_le()]),
                ),
            })?;
        } else {
            self.push_constraint(BilinearConstraint {
                linear_combination_a: make_combination(vec![out], vec![1]),
                linear_combination_b: make_combination(vec![self.one], vec![1]),
                linear_combination_c: make_combination(
                    vec![*a, self.one],
                    pad_to_max(vec![vec![1], b.to_bytes_le()]),
                ),
            })?;
        }

        Ok(out)
    }

    fn mul_constant(
        &mut self,
        type_id: &TypeId,
        a: &Self::Wire,
        b: Self::TypeElement,
    ) -> Result<Self::Wire> {
        if *type_id != 0 {
            return Err("All type ids must be equal to 0 to be able to convert into R1CS.".into());
        }
        let out = self.builder.allocate_var();
        let correction_wire = if self.use_correction {
            self.builder.allocate_var()
        } else {
            0
        };

        if self.use_witness {
            // in this case, compute the exact value of the 'correction' to apply.
            let a_val = self
                .all_assignment
                .get(a)
                .ok_or("MulConstant: Value does not exist.")?;

            let product = a_val * &b;
            let correction = &product / &self.src_modulus;
            let o_val = product % &self.src_modulus;

            if self.use_correction {
                self.push_witness(correction_wire, &correction);
            }
            self.push_witness(out, &o_val);

            self.all_assignment.insert(out, o_val);
        }
        if self.use_correction {
            self.push_constraint(BilinearConstraint {
                linear_combination_a: make_combination(vec![*a], b.to_bytes_le()),
                linear_combination_b: make_combination(vec![self.one], vec![1]),
                linear_combination_c: make_combination(
                    vec![out, correction_wire],
                    pad_to_max(vec![vec![1], self.src_modulus.to_bytes_le()]),
                ),
            })?;
        } else {
            self.push_constraint(BilinearConstraint {
                linear_combination_a: make_combination(vec![*a], b.to_bytes_le()),
                linear_combination_b: make_combination(vec![self.one], vec![1]),
                linear_combination_c: make_combination(vec![out], vec![1]),
            })?;
        }

        Ok(out)
    }

    fn public_input(&mut self, type_id: &TypeId, val: Self::TypeElement) -> Result<Self::Wire> {
        if *type_id != 0 {
            return Err("All type ids must be equal to 0 to be able to convert into R1CS.".into());
        }
        let id = self
            .builder
            .allocate_instance_var(&pad_le_u8_vec(val.to_bytes_le(), self.byte_len));
        self.make_assignment(id, Some(val))?;
        Ok(id)
    }

    fn private_input(
        &mut self,
        type_id: &TypeId,
        val: Option<Self::TypeElement>,
    ) -> Result<Self::Wire> {
        if *type_id != 0 {
            return Err("All type ids must be equal to 0 to be able to convert into R1CS.".into());
        }
        let id = self.builder.allocate_var();
        if !self.use_witness ^ val.is_none() {
            return Err("Inconsistency.".into());
        }

        self.make_assignment(id, val.clone())?;
        if self.use_witness {
            self.push_witness(id, &val.unwrap());
        }
        Ok(id)
    }

    fn gate_new(&mut self, _: &TypeId, _: WireId, _: WireId) -> Result<()> {
        Ok(())
    }

    fn convert(
        &mut self,
        _output_type: &TypeId,
        _output_wire_count: u64,
        _input_type: &TypeId,
        _inputs: &[&Self::Wire],
    ) -> Result<Vec<Self::Wire>> {
        Err("Not possible to convert to R1CS circuit containing convert gates".into())
    }

    fn evaluate_plugin(
        &mut self,
        _output_count: &[Count],
        _input_count: &[Count],
        _inputs: &[&Self::Wire],
        _public_inputs: &HashMap<TypeId, Vec<Self::TypeElement>>,
        _private_inputs: &HashMap<TypeId, Vec<Self::TypeElement>>,
        _plugin_body: &PluginBody,
    ) -> Result<Vec<Self::Wire>> {
        Err("Not possible to convert to R1CS circuit circuit containing plugin calls".into())
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

use crate::structs::value::value_to_biguint;
#[cfg(test)]
use crate::{
    consumers::evaluator::Evaluator,
    producers::{from_r1cs::FromR1CSConverter, sink::MemorySink},
    Message, PrivateInputs, PublicInputs, Source,
};
#[cfg(test)]
use std::{collections::HashSet, path::PathBuf};
#[cfg(test)]
use zkinterface::{CircuitHeader as zkiCircuitHeader, Workspace, WorkspaceSink};

#[cfg(test)]
fn assert_same_io_values(
    public_inputs: &[PublicInputs],
    zki_headers: &[zkiCircuitHeader],
) -> Result<()> {
    let zki_vals: HashSet<BigUint> = zki_headers
        .iter()
        .flat_map(|zki_header| {
            zki_header
                .instance_variables
                .get_variables()
                .iter()
                .map(|v| value_to_biguint(v.value))
                .collect::<Vec<BigUint>>()
        })
        .collect();
    let ir_vals: HashSet<BigUint> = public_inputs
        .iter()
        .flat_map(|pub_inp| {
            pub_inp
                .inputs
                .iter()
                .map(|v| value_to_biguint(v))
                .collect::<Vec<BigUint>>()
        })
        .collect();

    // zkif instances may contain more instance values that IR, since IR constants are translated
    // into ZKIF instances.
    assert!(zki_vals.is_superset(&ir_vals));

    Ok(())
}

#[cfg(test)]
fn assert_same_witness_values(
    private_inputs: &[PrivateInputs],
    zki_witness: &[zkiWitness],
) -> Result<()> {
    let zki_vals: HashSet<BigUint> = zki_witness
        .iter()
        .flat_map(|zki_witness| {
            zki_witness
                .assigned_variables
                .get_variables()
                .iter()
                .map(|v| value_to_biguint(v.value))
                .collect::<Vec<BigUint>>()
        })
        .collect();

    let ir_vals: HashSet<BigUint> = private_inputs
        .iter()
        .flat_map(|priv_inp| {
            priv_inp
                .inputs
                .iter()
                .map(|v| value_to_biguint(v))
                .collect::<Vec<BigUint>>()
        })
        .collect();

    // zkif witness may (likely does) contain more witness values that IR could confirm
    assert!(zki_vals.is_superset(&ir_vals));

    Ok(())
}

#[test]
fn test_to_r1cs_check_public_private_inputs() -> Result<()> {
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;
    use zkinterface::producers::examples::example_constraints as zki_example_constraints;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;

    let output_directory = "local/test_to_r1cs_check_public_private_inputs";

    // begin tests as with from_r1cs

    let zki_header = zki_example_header_inputs(3, 4, 25);
    let zki_witness = zki_example_witness_inputs(3, 4);
    let zki_r1cs = zki_example_constraints();

    let mut converter = FromR1CSConverter::new(MemorySink::default(), &zki_header);
    converter.ingest_witness(&zki_witness)?;
    converter.ingest_constraints(&zki_r1cs)?;

    let source: Source = converter.finish().into();
    let ir_messages = source.read_all_messages()?;

    let mut to_r1cs = ToR1CSConverter::new(WorkspaceSink::new(&output_directory)?, true, false);
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut to_r1cs);
    to_r1cs.finish()?;
    assert_eq!(evaluator.get_violations(), Vec::<String>::new());

    // now convert back into r1cs
    let workspace = Workspace::from_dir(&PathBuf::from(&output_directory))?;
    let zki_messages = workspace.read_all_messages();

    assert_same_io_values(&ir_messages.public_inputs, &zki_messages.circuit_headers)?;

    assert_same_witness_values(&ir_messages.private_inputs, &zki_messages.witnesses)?;

    Ok(())
}

#[test]
fn test_to_r1cs_validate_two_ways_conversion_same_field() -> Result<()> {
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;
    use zkinterface::producers::examples::example_constraints as zki_example_constraints;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;

    let output_directory = "local/test_to_r1cs_validate_two_ways_conversion_same_field";

    // begin tests as with from_r1cs

    let zki_header = zki_example_header_inputs(3, 4, 25);
    let zki_witness = zki_example_witness_inputs(3, 4);
    let zki_r1cs = zki_example_constraints();

    let mut converter = FromR1CSConverter::new(MemorySink::default(), &zki_header);
    converter.ingest_witness(&zki_witness)?;
    converter.ingest_constraints(&zki_r1cs)?;

    let source: Source = converter.finish().into();

    let mut to_r1cs = ToR1CSConverter::new(WorkspaceSink::new(&output_directory)?, true, false);
    let evaluator = Evaluator::from_messages(source.iter_messages(), &mut to_r1cs);
    to_r1cs.finish()?;
    assert_eq!(evaluator.get_violations(), Vec::<String>::new());

    let workspace = Workspace::from_dir(&PathBuf::from(&output_directory))?;

    // First check that the constraint system is semantically valid
    let mut validator = zkinterface::consumers::validator::Validator::new_as_prover();
    for message in workspace.iter_messages() {
        validator.ingest_message(&message);
    }

    let validator_violations = validator.get_violations();
    assert_eq!(validator_violations.len(), 1);
    assert_eq!(
        validator_violations[0],
        "variable_1 was defined but not used."
    );

    // Then check that the constraint system is verified
    let mut simulator = zkinterface::consumers::simulator::Simulator::default();
    for message in workspace.iter_messages() {
        simulator.ingest_message(&message);
    }

    assert_eq!(simulator.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_to_r1cs_validate_converted_circuit_same_field() -> Result<()> {
    // This time use an example in straight IR
    use crate::producers::simple_examples::*;

    let output_directory = "local/test_to_r1cs_validate_converted_circuit_same_field";

    let messages = vec![
        Ok(Message::PublicInputs(simple_example_public_inputs())),
        Ok(Message::PrivateInputs(simple_example_private_inputs())),
        Ok(Message::Relation(simple_example_relation())),
    ];

    let mut to_r1cs = ToR1CSConverter::new(WorkspaceSink::new(&output_directory)?, true, false);
    let evaluator = Evaluator::from_messages(messages.into_iter(), &mut to_r1cs);
    to_r1cs.finish()?;
    assert_eq!(evaluator.get_violations(), Vec::<String>::new());

    let workspace = Workspace::from_dir(&PathBuf::from(&output_directory))?;

    // First check that the constraint system is semantically valid
    let mut validator = zkinterface::consumers::validator::Validator::new_as_prover();
    for message in workspace.iter_messages() {
        validator.ingest_message(&message);
    }

    let violations = validator.get_violations();
    if !violations.is_empty() {
        let msg = format!("Violations:\n- {}\n", violations.join("\n- "));
        panic!("{}", msg);
    }

    // Then check that the constraint system is verified
    let mut simulator = zkinterface::consumers::simulator::Simulator::default();
    for message in workspace.iter_messages() {
        simulator.ingest_message(&message);
    }

    assert_eq!(simulator.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_to_r1cs_validate_two_ways_conversion_bigger_field() -> Result<()> {
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;
    use zkinterface::producers::examples::example_constraints as zki_example_constraints;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;

    let output_directory = "local/test_to_r1cs_validate_two_ways_conversion_bigger_field";

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

    // First check that the constraint system is semantically valid
    let mut validator = zkinterface::consumers::validator::Validator::new_as_prover();
    for mut message in workspace.iter_messages() {
        if let zkinterface::Message::Header(ref mut header) = message {
            header.field_maximum = Some(BigUint::from(2305843009213693951_u64).to_bytes_le());
        }
        validator.ingest_message(&message);
    }

    let validator_violations = validator.get_violations();
    println!("{:?}", validator_violations);
    assert_eq!(validator_violations.len(), 1);
    assert_eq!(
        validator_violations[0],
        "variable_1 was defined but not used."
    );

    // Then check that the constraint system is verified
    let mut simulator = zkinterface::consumers::simulator::Simulator::default();
    for mut message in workspace.iter_messages() {
        if let zkinterface::Message::Header(ref mut header) = message {
            header.field_maximum = Some(BigUint::from(2305843009213693951_u64).to_bytes_le());
        }
        simulator.ingest_message(&message);
    }

    let simulator_violations = simulator.get_violations();
    assert_eq!(simulator_violations.len(), 0);

    Ok(())
}

#[test]
fn test_to_r1cs_validate_converted_circuit_bigger_field() -> Result<()> {
    // This time use an example in straight IR
    use crate::producers::simple_examples::*;

    let output_directory = "local/test_to_r1cs_validate_converted_circuit_bigger_field";

    let messages = vec![
        Ok(Message::PublicInputs(simple_example_public_inputs())),
        Ok(Message::PrivateInputs(simple_example_private_inputs())),
        Ok(Message::Relation(simple_example_relation())),
    ];

    let mut to_r1cs = ToR1CSConverter::new(WorkspaceSink::new(&output_directory)?, true, true);
    let evaluator = Evaluator::from_messages(messages.into_iter(), &mut to_r1cs);
    to_r1cs.finish()?;
    let r1cs_violations = evaluator.get_violations();
    assert_eq!(r1cs_violations.len(), 0);

    let workspace = Workspace::from_dir(&PathBuf::from(&output_directory))?;

    // First check that the constraint system is semantically valid
    let mut validator = zkinterface::consumers::validator::Validator::new_as_prover();
    for mut message in workspace.iter_messages() {
        if let zkinterface::Message::Header(ref mut header) = message {
            header.field_maximum = Some(BigUint::from(2305843009213693951_u64).to_bytes_le());
        }
        validator.ingest_message(&message);
    }

    let violations = validator.get_violations();
    if !violations.is_empty() {
        let msg = format!("Violations:\n- {}\n", violations.join("\n- "));
        panic!("{}", msg);
    }

    // Then check that the constraint system is verified
    let mut simulator = zkinterface::consumers::simulator::Simulator::default();
    for mut message in workspace.iter_messages() {
        if let zkinterface::Message::Header(ref mut header) = message {
            header.field_maximum = Some(BigUint::from(2305843009213693951_u64).to_bytes_le());
        }
        simulator.ingest_message(&message);
    }

    let simulator_violations = simulator.get_violations();
    assert_eq!(simulator_violations.len(), 0);

    Ok(())
}
