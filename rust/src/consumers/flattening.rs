use crate::consumers::evaluator::ZKBackend;
use crate::producers::build_gates::BuildGate;
use crate::producers::builder::{GateBuilder, GateBuilderT};
use crate::structs::count::Count;
use crate::structs::plugin::PluginBody;
use crate::structs::types::Type;
use crate::structs::value::value_to_biguint;
use crate::{Result, Sink, TypeId, WireId};
use num_bigint::BigUint;
use num_traits::{One, Zero};
use std::collections::BTreeMap;

// TODO instead of using WireId, use something implementing Drop, which will call the corresponding
// Delete gate when the wire is no more needed.

#[derive(Default)]
pub struct IRFlattener<S: Sink> {
    sink: Option<S>,
    gate_builder: Option<GateBuilder<S>>,
    moduli: Vec<BigUint>,
}

impl<S: Sink> IRFlattener<S> {
    pub fn new(sink: S) -> Self {
        IRFlattener {
            sink: Some(sink),
            gate_builder: None,
            moduli: vec![],
        }
    }

    pub fn finish(mut self) -> S {
        self.gate_builder.take().unwrap().finish()
    }
}

impl<S: Sink> Drop for IRFlattener<S> {
    fn drop(&mut self) {
        if self.gate_builder.is_some() {
            self.gate_builder.take().unwrap().finish();
        }
    }
}

impl<S: Sink> ZKBackend for IRFlattener<S> {
    type Wire = WireId;
    type TypeElement = BigUint;

    fn from_bytes_le(val: &[u8]) -> Result<Self::TypeElement> {
        Ok(value_to_biguint(val))
    }

    fn set_types(&mut self, types: &[Type]) -> Result<()> {
        if self.gate_builder.is_none() {
            types
                .iter()
                .try_for_each::<_, Result<()>>(|type_value| match type_value {
                    Type::Field(modulo) => {
                        self.moduli.push(value_to_biguint(modulo));
                        Ok(())
                    }
                    Type::PluginType(_, _, _) => {
                        Err("Not possible to flatten circuit containing plugin types".into())
                    }
                })?;
            self.gate_builder = Some(GateBuilder::new(self.sink.take().unwrap(), &[], types, &[]));
        }
        Ok(())
    }

    fn one(&self) -> Result<Self::TypeElement> {
        Ok(BigUint::one())
    }

    fn minus_one(&self, type_id: &TypeId) -> Result<Self::TypeElement> {
        if self.moduli.is_empty() {
            return Err("Moduli is not initiated, used `set_types()` before calling.".into());
        }
        let modulo = self
            .moduli
            .get(*type_id as usize)
            .ok_or(format!("Type id {} is not defined.", *type_id))?;
        Ok(modulo - self.one()?)
    }

    fn zero(&self) -> Result<Self::TypeElement> {
        Ok(BigUint::zero())
    }

    fn copy(&mut self, type_id: &TypeId, wire: &Self::Wire) -> Result<Self::Wire> {
        if self.gate_builder.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.gate_builder
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::Copy(*type_id, *wire))
    }

    fn constant(&mut self, type_id: &TypeId, val: Self::TypeElement) -> Result<Self::Wire> {
        if self.gate_builder.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.gate_builder
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::Constant(*type_id, val.to_bytes_le()))
    }

    fn assert_zero(&mut self, type_id: &TypeId, wire: &Self::Wire) -> Result<()> {
        if self.gate_builder.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.gate_builder
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::AssertZero(*type_id, *wire))?;
        Ok(())
    }

    fn add(&mut self, type_id: &TypeId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        if self.gate_builder.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.gate_builder
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::Add(*type_id, *a, *b))
    }

    fn multiply(&mut self, type_id: &TypeId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        if self.gate_builder.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.gate_builder
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::Mul(*type_id, *a, *b))
    }

    fn add_constant(
        &mut self,
        type_id: &TypeId,
        a: &Self::Wire,
        b: Self::TypeElement,
    ) -> Result<Self::Wire> {
        if self.gate_builder.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.gate_builder
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::AddConstant(*type_id, *a, b.to_bytes_le()))
    }

    fn mul_constant(
        &mut self,
        type_id: &TypeId,
        a: &Self::Wire,
        b: Self::TypeElement,
    ) -> Result<Self::Wire> {
        if self.gate_builder.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.gate_builder
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::MulConstant(*type_id, *a, b.to_bytes_le()))
    }

    fn public_input(&mut self, type_id: &TypeId, val: Self::TypeElement) -> Result<Self::Wire> {
        if self.gate_builder.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        self.gate_builder
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::Public(*type_id, Some(val.to_bytes_le())))
    }

    fn private_input(
        &mut self,
        type_id: &TypeId,
        val: Option<Self::TypeElement>,
    ) -> Result<Self::Wire> {
        if self.gate_builder.is_none() {
            panic!("Builder has not been properly initialized.");
        }
        let value = val.map(|v| v.to_bytes_le());
        self.gate_builder
            .as_mut()
            .unwrap()
            .create_gate(BuildGate::Private(*type_id, value))
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
        Err("Not possible to flatten circuit containing convert gates".into())
    }

    fn evaluate_plugin(
        &mut self,
        _output_count: &[Count],
        _input_count: &[Count],
        _inputs: &[&Self::Wire],
        _public_inputs: &BTreeMap<TypeId, Vec<Self::TypeElement>>,
        _private_inputs: &BTreeMap<TypeId, Vec<Self::TypeElement>>,
        _plugin_body: &PluginBody,
    ) -> Result<Vec<Self::Wire>> {
        Err("Not possible to flatten circuit containing plugin calls".into())
    }
}

#[test]
fn test_validate_flattening() -> crate::Result<()> {
    use crate::consumers::evaluator::Evaluator;
    use crate::consumers::validator::Validator;
    use crate::producers::simple_examples::*;
    use crate::producers::sink::MemorySink;
    use crate::Source;

    let public_inputs = simple_example_public_inputs();
    let private_inputs = simple_example_private_inputs();
    let relation = simple_example_relation();

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
    use crate::producers::simple_examples::*;
    use crate::producers::sink::MemorySink;
    use crate::Source;

    let relation = simple_example_relation();
    let public_inputs = simple_example_public_inputs();
    let private_inputs = simple_example_private_inputs();

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
