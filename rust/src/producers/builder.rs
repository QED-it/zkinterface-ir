use crate::Gate;
use crate::structs::WireId;
use crate::Gate::*;

pub trait IBuilder {
    fn free_id(&self) -> WireId;

    fn alloc(&mut self) -> WireId;
    fn push_gate(&mut self, allocated_gate: Gate);

    fn create_gate(&mut self, non_allocated_gate: Gate) -> WireId;
}

#[derive(Default)]
pub struct Builder {
    pub gates: Vec<Gate>,
    free_id: WireId,
}

impl IBuilder for Builder {
    fn free_id(&self) -> WireId {
        self.free_id
    }

    fn alloc(&mut self) -> WireId {
        let id = self.free_id;
        self.free_id += 1;
        id
    }

    fn push_gate(&mut self, allocated_gate: Gate) {
        self.gates.push(allocated_gate);
    }


    /// Allocates a new wire id for the output and creates a new gate,
    /// Returns the newly allocated WireId
    /// The new gate must be provided with output_id == zero.
    ///
    /// # Panics
    ///
    /// output id for the gate is not zero.
    ///
    /// # Examples
    ///
    /// a simple example
    /// ```
    ///
    /// use sieve_ir::structs::gates::Gate::Constant;
    /// let new_id = b.create_gate(&Constant(0,vec![1]));
    ///
    /// ```
    /// an example for a gate without output_id
    /// ```
    ///
    /// use sieve_ir::structs::gates::Gate::AssertZero;
    /// let new_id = b.create_gate(&AssertZero(5));
    ///
    /// ```
    fn create_gate(&mut self, non_allocated_gate: Gate) -> WireId {
        if !has_output(&non_allocated_gate) {
            self.push_gate(non_allocated_gate);
            return 0;
        }
        assert_eq!(get_output_wire_id(&non_allocated_gate), 0);
        let new_id = self.alloc();
        let allocated_gate = with_output(&non_allocated_gate,new_id);
        self.push_gate(allocated_gate);
        new_id
    }
}

impl Builder{
    pub fn new(free_id: u64) -> Builder {
        Builder {
            gates: Vec::<Gate>::new(),
            free_id
        }
    }
}


pub fn get_output_wire_id(gate: &Gate) -> WireId {
    match gate.into() {
        Constant(wireId, _) => wireId,
        Copy(wireId, _) => wireId,
        Add(wireId, _, _) => wireId,
        Mul(wireId, _, _) => wireId,
        AddConstant(wireId, _, _) => wireId,
        MulConstant(wireId, _, _) => wireId,
        And(wireId, _, _) => wireId,
        Xor(wireId, _, _) => wireId,
        Not(wireId, _) => wireId,
        AssertZero(_) => panic!("no output id for AssertZero gate"),
    }
}

fn has_output(gate: &Gate) -> bool {
    match gate.into() {
        AssertZero(_) => false,
        Constant(_, _) => true,
        Copy(_, _) => true,
        Add(_, _, _) => true,
        Mul(_, _, _) => true,
        AddConstant(_, _, _) => true,
        MulConstant(_, _, _) => true,
        And(_, _, _) => true,
        Xor(_, _, _) => true,
        Not(_, _) => true,
    }
}

fn with_output(non_allocated_gate: &Gate, output_id: WireId) -> Gate {
    assert_eq!(get_output_wire_id(non_allocated_gate), 0, "output wire must be 0 for a non allocated gate");
    match non_allocated_gate.into() {
        Constant(_, v) => Constant(output_id, v),
        Copy(_, w) => Copy(output_id,w),
        Add(_, w1, w2) => Add(output_id, w1, w2),
        Mul(_, w1, w2) => Mul(output_id, w1, w2),
        AddConstant(_, w, v) => AddConstant(output_id,w,v),
        MulConstant(_, w, v) => MulConstant(output_id,w,v),
        And(_, w1, w2) => And(output_id, w1, w2),
        Xor(_, w1, w2) => Xor(output_id, w1, w2),
        Not(_, w) => Not(output_id,w),

        AssertZero(_) => panic!("AssertZero has no output gate"),
    }
}