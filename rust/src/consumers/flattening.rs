use crate::consumers::evaluator::{get_field, ZKBackend};
use crate::producers::build_gates::BuildGate;
use crate::producers::builder::{GateBuilder, GateBuilderT};
use crate::{FieldId, Header, Result, Sink, Value, WireId};
use num_bigint::BigUint;
use num_traits::{One, Zero};

// TODO instead of using WireId, use something implementing Drop, which will call the corresponding
// Delete gate when the wire is no more needed.

#[derive(Default)]
pub struct IRFlattener<S: Sink> {
    sink: Option<S>,
    b: Option<GateBuilder<S>>,
    moduli: Vec<BigUint>,
}

impl<S: Sink> IRFlattener<S> {
    pub fn new(sink: S) -> Self {
        IRFlattener {
            sink: Some(sink),
            b: None,
            moduli: vec![],
        }
    }

    pub fn finish(mut self) -> S {
        self.b.take().unwrap().finish()
    }
}

impl<S: Sink> Drop for IRFlattener<S> {
    fn drop(&mut self) {
        if self.b.is_some() {
            self.b.take().unwrap().finish();
        }
    }
}

impl<S: Sink> ZKBackend for IRFlattener<S> {
    type Wire = WireId;
    type FieldElement = BigUint;

    fn from_bytes_le(val: &[u8]) -> Result<Self::FieldElement> {
        Ok(BigUint::from_bytes_le(val))
    }

    fn set_fields(&mut self, moduli: &[Value]) -> Result<()> {
        if self.b.is_none() {
            let header = Header::new(moduli);
            for modulus in moduli {
                self.moduli.push(BigUint::from_bytes_le(modulus));
            }
            self.b = Some(GateBuilder::new(self.sink.take().unwrap(), header));
        }
        Ok(())
    }

    fn one(&self) -> Result<Self::FieldElement> {
        Ok(BigUint::one())
    }

    fn minus_one(&self, field_id: &FieldId) -> Result<Self::FieldElement> {
        if self.moduli.is_empty() {
            return Err("Moduli is not initiated, used `set_fields()` before calling.".into());
        }
        let field = get_field(field_id, &self.moduli)?;
        Ok(field - self.one()?)
    }

    fn zero(&self) -> Result<Self::FieldElement> {
        Ok(BigUint::zero())
    }

    fn copy(&mut self, field_id: &FieldId, wire: &Self::Wire) -> Result<Self::Wire> {
        if self.b.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.b
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::Copy(*field_id, *wire))
    }

    fn constant(&mut self, field_id: &FieldId, val: Self::FieldElement) -> Result<Self::Wire> {
        if self.b.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.b
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::Constant(*field_id, val.to_bytes_le()))
    }

    fn assert_zero(&mut self, field_id: &FieldId, wire: &Self::Wire) -> Result<()> {
        if self.b.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.b
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::AssertZero(*field_id, *wire))?;
        Ok(())
    }

    fn add(&mut self, field_id: &FieldId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        if self.b.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.b
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::Add(*field_id, *a, *b))
    }

    fn multiply(
        &mut self,
        field_id: &FieldId,
        a: &Self::Wire,
        b: &Self::Wire,
    ) -> Result<Self::Wire> {
        if self.b.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.b
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::Mul(*field_id, *a, *b))
    }

    fn add_constant(
        &mut self,
        field_id: &FieldId,
        a: &Self::Wire,
        b: Self::FieldElement,
    ) -> Result<Self::Wire> {
        if self.b.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.b
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::AddConstant(*field_id, *a, b.to_bytes_le()))
    }

    fn mul_constant(
        &mut self,
        field_id: &FieldId,
        a: &Self::Wire,
        b: Self::FieldElement,
    ) -> Result<Self::Wire> {
        if self.b.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.b
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::MulConstant(*field_id, *a, b.to_bytes_le()))
    }

    fn public_input(&mut self, field_id: &FieldId, val: Self::FieldElement) -> Result<Self::Wire> {
        if self.b.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.b
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::PublicInput(*field_id, Some(val.to_bytes_le())))
    }

    fn private_input(
        &mut self,
        field_id: &FieldId,
        val: Option<Self::FieldElement>,
    ) -> Result<Self::Wire> {
        if self.b.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        let value = val.map(|v| v.to_bytes_le());
        self.b
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::PrivateInput(*field_id, value))
    }

    fn convert(
        &mut self,
        _output_field: &FieldId,
        _output_wire_count: u64,
        _input_field: &FieldId,
        _inputs: &[&Self::Wire],
    ) -> Result<Vec<Self::Wire>> {
        Err("Not possible to flatten circuit containing convert gates".into())
    }
}

#[test]
fn test_validate_flattening() -> crate::Result<()> {
    use crate::consumers::evaluator::Evaluator;
    use crate::consumers::validator::Validator;
    use crate::producers::examples::*;
    use crate::producers::sink::MemorySink;
    use crate::Source;

    let public_inputs = example_public_inputs();
    let private_inputs = example_private_inputs();
    let relation = example_relation();

    let mut flattener = IRFlattener::new(MemorySink::default());
    let mut evaluator = Evaluator::default();

    evaluator.ingest_public_inputs(&public_inputs)?;
    evaluator.ingest_private_inputs(&private_inputs)?;
    evaluator.ingest_relation(&relation, &mut flattener)?;

    let s: Source = flattener.finish().into();

    let mut val = Validator::new_as_prover();
    for message in s.iter_messages() {
        val.ingest_message(&message?);
    }

    assert_eq!(val.get_violations(), Vec::<String>::new());

    Ok(())
}

#[test]
fn test_evaluate_flattening() -> crate::Result<()> {
    use crate::consumers::evaluator::{Evaluator, PlaintextBackend};
    use crate::producers::examples::*;
    use crate::producers::sink::MemorySink;
    use crate::Source;

    let relation = example_relation();
    let public_inputs = example_public_inputs();
    let private_inputs = example_private_inputs();

    let mut flattener = IRFlattener::new(MemorySink::default());
    let mut evaluator = Evaluator::default();

    evaluator.ingest_public_inputs(&public_inputs)?;
    evaluator.ingest_private_inputs(&private_inputs)?;
    evaluator.ingest_relation(&relation, &mut flattener)?;

    let s: Source = flattener.finish().into();

    let mut interpreter = PlaintextBackend::default();
    let new_simulator = Evaluator::from_messages(s.iter_messages(), &mut interpreter);

    assert_eq!(new_simulator.get_violations(), Vec::<String>::new());

    Ok(())
}
