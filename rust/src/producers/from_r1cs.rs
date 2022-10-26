use num_bigint::BigUint;
use num_traits::One;
use std::ops::Add;

use crate::producers::builder::{BuildGate, GateBuilder, GateBuilderT};
use crate::{Header, Result, Sink, WireId};
use BuildGate::*;

use std::collections::BTreeMap;
use zkinterface::consumers::reader::Variable as zkiVariable;
use zkinterface::CircuitHeader as zkiCircuitHeader;
use zkinterface::ConstraintSystem as zkiConstraintSystem;
use zkinterface::Witness as zkiWitness;

const FIELD_ID: u8 = 0;

pub struct FromR1CSConverter<S: Sink> {
    b: GateBuilder<S>,
    // Useful to know which variable in R1CS is associated to which WireId in IR circuit.
    r1cs_to_ir_wire: BTreeMap<u64, WireId>,
    minus_one: WireId,
}

impl<S: Sink> FromR1CSConverter<S> {
    /// Create a new R1CSConverter instance.
    /// the Sink is used to tell where to 'write' the output circuit
    /// the ZKI CircuitHeader will be used to preallocate things
    pub fn new(sink: S, zki_header: &zkiCircuitHeader) -> Self {
        let mut conv = Self {
            b: GateBuilder::new(sink, zki_header_to_header(zki_header).unwrap()),
            r1cs_to_ir_wire: Default::default(),
            minus_one: 0,
        };

        // allocate constant '1' to IR wire '0'.
        let one = conv.b.create_gate(Constant(FIELD_ID, vec![1])).unwrap();
        assert_eq!(one, 0);
        conv.r1cs_to_ir_wire.insert(0, one);

        // allocate constant '-1'.
        conv.minus_one = conv
            .b
            .create_gate(Constant(
                FIELD_ID,
                zki_header.field_maximum.as_ref().unwrap().clone(),
            ))
            .unwrap();

        // allocate all the instance variables with their respective values.
        for var in zki_header.instance_variables.get_variables().iter() {
            if var.id == 0 {
                assert!(
                    BigUint::from_bytes_le(var.value).is_one(),
                    "value for public input id:0 should be a constant 1"
                );
            } else {
                let wire = conv
                    .b
                    .create_gate(PublicInput(FIELD_ID, Some(var.value.to_vec())))
                    .unwrap();
                conv.r1cs_to_ir_wire.insert(var.id, wire);
            }
        }

        // preallocate wire id which will contain witness variables.
        for var in zki_header.list_witness_ids() {
            let wire = conv.b.create_gate(PrivateInput(FIELD_ID, None)).unwrap();
            conv.r1cs_to_ir_wire.insert(var, wire);
        }

        conv
    }

    fn build_term(&mut self, term: &zkiVariable) -> Result<WireId> {
        let const_0: Vec<u8> = vec![0];
        let non_empty_term_value = if !term.value.is_empty() {
            term.value
        } else {
            &const_0
        };
        if term.id == 0 {
            return self
                .b
                .create_gate(Constant(FIELD_ID, Vec::from(non_empty_term_value)));
        }

        let val_id = self
            .b
            .create_gate(Constant(FIELD_ID, Vec::from(non_empty_term_value)))?;
        if let Some(term_id) = self.r1cs_to_ir_wire.get(&term.id) {
            Ok(self.b.create_gate(Mul(FIELD_ID, *term_id, val_id))?)
        } else {
            Err(format!("The WireId {} has not been defined yet.", term.id).into())
        }
    }

    fn add_lc(&mut self, lc: &[zkiVariable]) -> Result<WireId> {
        if lc.is_empty() {
            // empty linear combination translates into a 0 value
            return self.b.create_gate(Constant(FIELD_ID, vec![0]));
        }

        let mut sum_id = self.build_term(&lc[0])?;

        for term in &lc[1..] {
            let term_id = self.build_term(term)?;
            sum_id = self.b.create_gate(Add(FIELD_ID, sum_id, term_id))?;
        }

        Ok(sum_id)
    }

    pub fn ingest_constraints(&mut self, zki_r1cs: &zkiConstraintSystem) -> Result<()> {
        // Convert each R1CS constraint into a graph of Add/Mul/Const/AssertZero gates.
        for constraint in &zki_r1cs.constraints {
            let sum_a_id = self.add_lc(&constraint.linear_combination_a.get_variables())?;
            let sum_b_id = self.add_lc(&constraint.linear_combination_b.get_variables())?;
            let sum_c_id = self.add_lc(&constraint.linear_combination_c.get_variables())?;

            let prod_id = self.b.create_gate(Mul(FIELD_ID, sum_a_id, sum_b_id))?;
            let neg_c_id = self
                .b
                .create_gate(Mul(FIELD_ID, self.minus_one, sum_c_id))?;
            let claim_zero_id = self.b.create_gate(Add(FIELD_ID, prod_id, neg_c_id))?;

            self.b.create_gate(AssertZero(FIELD_ID, claim_zero_id))?;
        }

        Ok(())
    }

    pub fn ingest_witness(&mut self, zki_witness: &zkiWitness) -> Result<()> {
        for var in &zki_witness.assigned_variables.get_variables() {
            if !self.r1cs_to_ir_wire.contains_key(&var.id) {
                return Err(format!("The ZKI witness id {} does not exist.", var.id).into());
            }
            self.b
                .push_private_input_value(FIELD_ID, var.value.to_vec())?;
        }

        Ok(())
    }

    pub fn finish(self) -> S {
        self.b.finish()
    }
}

fn zki_header_to_header(zki_header: &zkiCircuitHeader) -> Result<Header> {
    match &zki_header.field_maximum {
        None => Err("field_maximum must be provided".into()),

        Some(field_maximum) => {
            let mut fc: BigUint = BigUint::from_bytes_le(field_maximum);
            let one: u8 = 1;
            fc = fc.add(one);

            Ok(Header::new(&[fc.to_bytes_le()]))
        }
    }
}

#[cfg(test)]
use crate::consumers::evaluator::Evaluator;
#[cfg(test)]
use crate::consumers::evaluator::PlaintextBackend;
#[cfg(test)]
use crate::consumers::stats::Stats;
#[cfg(test)]
use crate::producers::sink::MemorySink;

#[cfg(test)]
fn stats(conv: FromR1CSConverter<MemorySink>) -> Stats {
    use crate::Source;

    let sink = conv.finish();
    let source: Source = sink.into();
    Stats::from_messages(source.iter_messages())
}

#[test]
fn test_r1cs_to_gates() -> Result<()> {
    use crate::Source;
    use num_traits::ToPrimitive;
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;
    use zkinterface::producers::examples::example_constraints as zki_example_constraints;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;

    let zki_header = zki_example_header_inputs(3, 4, 25);
    let zki_r1cs = zki_example_constraints();
    let zki_witness = zki_example_witness_inputs(3, 4);

    let ir_header = zki_header_to_header(&zki_header)?;
    assert_header(&ir_header);

    let mut converter = FromR1CSConverter::new(MemorySink::default(), &zki_header);

    converter.ingest_witness(&zki_witness)?;
    converter.ingest_constraints(&zki_r1cs)?;

    let source: Source = converter.finish().into();
    let mut interp = PlaintextBackend::default();
    let eval = Evaluator::from_messages(source.iter_messages(), &mut interp);

    // check instance
    macro_rules! get_val {
        ($field_id: expr, $idx:expr) => {{
            eval.get($field_id, $idx).unwrap().to_u32().unwrap()
        }};
    }

    assert_eq!(get_val!(0, 0), 1);
    assert_eq!(get_val!(0, 1), 100);
    assert_eq!(get_val!(0, 2), 3);
    assert_eq!(get_val!(0, 3), 4);
    assert_eq!(get_val!(0, 4), 25);

    // check witness
    assert_eq!(get_val!(0, 5), 9);
    assert_eq!(get_val!(0, 6), 16);

    assert_eq!(eval.get_violations(), Vec::<String>::new());
    Ok(())
}

#[cfg(test)]
fn assert_header(header: &Header) {
    use crate::structs::IR_VERSION;
    use num_traits::ToPrimitive;

    assert_eq!(header.version, IR_VERSION);
    let fc = BigUint::from_bytes_le(&header.fields[0]);
    assert_eq!(fc.to_u32().unwrap(), 101);
}

#[test]
fn test_r1cs_stats() -> Result<()> {
    use crate::consumers::stats::GateStats;
    use zkinterface::producers::examples::example_circuit_header_inputs as zki_example_header_inputs;
    use zkinterface::producers::examples::example_constraints as zki_example_constraints;
    use zkinterface::producers::examples::example_witness_inputs as zki_example_witness_inputs;

    let zki_header = zki_example_header_inputs(3, 4, 25);
    let zki_r1cs = zki_example_constraints();
    let zki_witness = zki_example_witness_inputs(3, 4);

    let ir_header = zki_header_to_header(&zki_header)?;
    assert_header(&ir_header);

    let mut converter = FromR1CSConverter::new(MemorySink::default(), &zki_header);

    converter.ingest_witness(&zki_witness)?;
    converter.ingest_constraints(&zki_r1cs)?;

    let stats = stats(converter);

    let expected_stats = Stats {
        moduli: vec![vec![101]],
        gate_stats: GateStats {
            public_variables: 3,
            private_variables: 2,
            constants_gates: 12,
            assert_zero_gates: 3,
            copy_gates: 0,
            add_gates: 4,
            mul_gates: 15,
            add_constant_gates: 0,
            mul_constant_gates: 0,
            variables_freed: 0,
            functions_defined: 0,
            functions_called: 0,
            convert_gates: 0,
            public_inputs_messages: 1,
            private_inputs_messages: 1,
            relation_messages: 1,
        },
        functions: Default::default(),
    };

    assert_eq!(expected_stats, stats);
    Ok(())
}
