use crate::{Sink, Result};
use crate::consumers::flattening::IRFlattener;
use crate::consumers::evaluator::ZKBackend;
use crate::structs::relation::{contains_feature, ADD, MUL, ADDC, MULC, XOR, AND, NOT};

#[derive(Default)]
pub struct ExpandDefinable<S: Sink> {
    inner: IRFlattener<S>,
    gate_mask: u16,
}

impl<S: Sink> ExpandDefinable<S> {
    pub fn new (sink: S, gate_mask: u16) -> Self {
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

    fn set_field(&mut self, modulus: &[u8], degree: u32, is_boolean: bool) -> Result<()> {
        self.inner.set_field(modulus, degree, is_boolean)
    }

    fn one(&self) -> Self::Wire {
        self.inner.one()
    }

    fn minus_one(&self) -> Self::Wire {
        self.inner.minus_one()
    }

    fn zero(&self) -> Self::Wire {
        self.inner.zero()
    }

    fn copy(&mut self, wire: &Self::Wire) -> Self::Wire{ wire.clone() }

    fn constant(&mut self, val: Self::FieldElement) -> Result<Self::Wire> {
        self.inner.constant(val)
    }

    fn assert_zero(&mut self, wire: &Self::Wire) -> Result<()> {
        self.inner.assert_zero(wire)
    }

    fn add(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        if !contains_feature(self.gate_mask, ADD) {
            if !contains_feature(self.gate_mask, XOR) {
                panic!("Cannot replace ADD by XOR if XOR is not supported.");
            }
            self.inner.xor(a, b)
        } else {
            self.inner.add(a, b)
        }
    }

    fn multiply(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        if !contains_feature(self.gate_mask, MUL) {
            if !contains_feature(self.gate_mask, AND) {
                panic!("Cannot replace MUL by AND if AND is not supported.");
            }
            self.inner.and(a, b)
        } else {
            self.inner.multiply(a, b)
        }
    }

    fn add_constant(&mut self, a: &Self::Wire, b: Self::FieldElement) -> Result<Self::Wire> {
        if !contains_feature(self.gate_mask, ADDC) {
            let tmp = self.constant(b)?;
            self.add(a, &tmp)
        } else {
            self.inner.add_constant(a, b)
        }
    }

    fn mul_constant(&mut self, a: &Self::Wire, b: Self::FieldElement) -> Result<Self::Wire> {
        if !contains_feature(self.gate_mask, MULC) {
            let tmp = self.constant(b)?;
            self.multiply(a, &tmp)
        } else {
            self.inner.mul_constant(a, b)
        }
    }

    fn and(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        if !contains_feature(self.gate_mask, AND) {
            if !contains_feature(self.gate_mask, MUL) {
                panic!("Cannot replace AND by MUL if MUL is not supported.");
            }
            self.multiply(a, b)
        } else {
            self.inner.and(a, b)
        }
    }

    fn xor(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        if !contains_feature(self.gate_mask, XOR) {
            if !contains_feature(self.gate_mask, ADD) {
                panic!("Cannot replace XOR by ADD if ADD is not supported.");
            }
            self.add(a, b)
        } else {
            self.inner.xor(a, b)
        }
    }

    fn not(&mut self, a: &Self::Wire) -> Result<Self::Wire> {
        if !contains_feature(self.gate_mask, NOT) {
            if !contains_feature(self.gate_mask, ADD) {
                panic!("Cannot replace NOT by ADD if ADD is not supported.");
            }
            let tmp = self.one();
            self.add(a, &tmp)
        } else {
            self.inner.not(a)
        }
    }

    fn instance(&mut self, val: Self::FieldElement) -> Result<Self::Wire> {
        self.inner.instance(val)
    }

    fn witness(&mut self, val: Option<Self::FieldElement>) -> Result<Self::Wire> {
        self.inner.witness(val)
    }
}

