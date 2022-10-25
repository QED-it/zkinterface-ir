use crate::consumers::evaluator::ZKBackend;
use crate::consumers::flattening::IRFlattener;
use crate::structs::relation::{contains_feature, ADD, ADDC, AND, MUL, MULC, NOT, XOR};
use crate::{FieldId, Result, Sink, Value};

#[derive(Default)]
pub struct ExpandDefinable<S: Sink> {
    inner: IRFlattener<S>,
    gate_mask: u16,
}

impl<S: Sink> ExpandDefinable<S> {
    pub fn new(sink: S, gate_mask: u16) -> Self {
        ExpandDefinable {
            inner: IRFlattener::new(sink),
            gate_mask,
        }
    }
    pub fn finish(self) -> S {
        self.inner.finish()
    }
}

impl<S: Sink> ZKBackend for ExpandDefinable<S> {
    type Wire = <IRFlattener<S> as ZKBackend>::Wire;
    type FieldElement = <IRFlattener<S> as ZKBackend>::FieldElement;

    fn from_bytes_le(val: &[u8]) -> Result<Self::FieldElement> {
        IRFlattener::<S>::from_bytes_le(val)
    }

    fn set_fields(&mut self, moduli: &[Value], is_boolean: bool) -> Result<()> {
        self.inner.set_fields(moduli, is_boolean)
    }

    fn one(&self) -> Result<Self::FieldElement> {
        self.inner.one()
    }

    fn minus_one(&self, field_id: &FieldId) -> Result<Self::FieldElement> {
        self.inner.minus_one(field_id)
    }

    fn zero(&self) -> Result<Self::FieldElement> {
        self.inner.zero()
    }

    fn copy(&mut self, field_id: &FieldId, wire: &Self::Wire) -> Result<Self::Wire> {
        self.inner.copy(field_id, wire)
    }

    fn constant(&mut self, field_id: &FieldId, val: Self::FieldElement) -> Result<Self::Wire> {
        self.inner.constant(field_id, val)
    }

    fn assert_zero(&mut self, field_id: &FieldId, wire: &Self::Wire) -> Result<()> {
        self.inner.assert_zero(field_id, wire)
    }

    fn add(&mut self, field_id: &FieldId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        if !contains_feature(self.gate_mask, ADD) {
            if !contains_feature(self.gate_mask, XOR) {
                panic!("Cannot replace ADD by XOR if XOR is not supported.");
            }
            self.inner.xor(field_id, a, b)
        } else {
            self.inner.add(field_id, a, b)
        }
    }

    fn multiply(
        &mut self,
        field_id: &FieldId,
        a: &Self::Wire,
        b: &Self::Wire,
    ) -> Result<Self::Wire> {
        if !contains_feature(self.gate_mask, MUL) {
            if !contains_feature(self.gate_mask, AND) {
                panic!("Cannot replace MUL by AND if AND is not supported.");
            }
            self.inner.and(field_id, a, b)
        } else {
            self.inner.multiply(field_id, a, b)
        }
    }

    fn add_constant(
        &mut self,
        field_id: &FieldId,
        a: &Self::Wire,
        b: Self::FieldElement,
    ) -> Result<Self::Wire> {
        if !contains_feature(self.gate_mask, ADDC) {
            let tmp = self.constant(field_id, b)?;
            self.add(field_id, a, &tmp)
        } else {
            self.inner.add_constant(field_id, a, b)
        }
    }

    fn mul_constant(
        &mut self,
        field_id: &FieldId,
        a: &Self::Wire,
        b: Self::FieldElement,
    ) -> Result<Self::Wire> {
        if !contains_feature(self.gate_mask, MULC) {
            let tmp = self.constant(field_id, b)?;
            self.multiply(field_id, a, &tmp)
        } else {
            self.inner.mul_constant(field_id, a, b)
        }
    }

    fn and(&mut self, field_id: &FieldId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        if !contains_feature(self.gate_mask, AND) {
            if !contains_feature(self.gate_mask, MUL) {
                panic!("Cannot replace AND by MUL if MUL is not supported.");
            }
            self.multiply(field_id, a, b)
        } else {
            self.inner.and(field_id, a, b)
        }
    }

    fn xor(&mut self, field_id: &FieldId, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        if !contains_feature(self.gate_mask, XOR) {
            if !contains_feature(self.gate_mask, ADD) {
                panic!("Cannot replace XOR by ADD if ADD is not supported.");
            }
            self.add(field_id, a, b)
        } else {
            self.inner.xor(field_id, a, b)
        }
    }

    fn not(&mut self, field_id: &FieldId, a: &Self::Wire) -> Result<Self::Wire> {
        if !contains_feature(self.gate_mask, NOT) {
            if !contains_feature(self.gate_mask, ADD) {
                panic!("Cannot replace NOT by ADD if ADD is not supported.");
            }
            self.add_constant(field_id, a, self.one()?)
        } else {
            self.inner.not(field_id, a)
        }
    }

    fn instance(&mut self, field_id: &FieldId, val: Self::FieldElement) -> Result<Self::Wire> {
        self.inner.instance(field_id, val)
    }

    fn witness(
        &mut self,
        field_id: &FieldId,
        val: Option<Self::FieldElement>,
    ) -> Result<Self::Wire> {
        self.inner.witness(field_id, val)
    }

    fn convert(
        &mut self,
        output_field: &FieldId,
        output_wire_count: u64,
        input_field: &FieldId,
        inputs: &[&Self::Wire],
    ) -> Result<Vec<Self::Wire>> {
        self.inner
            .convert(output_field, output_wire_count, input_field, inputs)
    }
}
