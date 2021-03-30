use crate::Gate;
use crate::structs::WireId;
use crate::Gate::*;

pub trait IBuilder {
    fn free_id(&self) -> WireId;

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
    ///
    /// use zki_sieve::structs::gates::Gate::Constant;
    /// use zki_sieve::producers::builder::Builder;
    /// use zki_sieve::producers::builder::IBuilder;
    ///
    /// let free_variable_id = 5;
    /// let mut b = Builder::new(free_variable_id);
    ///
    /// let new_id = b.create_gate(Constant(0,vec![1]));
    ///
    /// ```
    /// an example for a gate without output_id
    /// ```
    ///
    /// use zki_sieve::structs::gates::Gate::AssertZero;
    /// use zki_sieve::producers::builder::Builder;
    /// use zki_sieve::producers::builder::IBuilder;
    ///
    /// let free_variable_id = 5;
    /// let mut b = Builder::new(free_variable_id);
    ///
    /// let new_id = b.create_gate(AssertZero(5));
    ///
    /// ```
    fn create_gate(&mut self, non_allocated_gate: Gate) -> WireId {
        if non_allocated_gate.get_output_wire_id().is_none() {
            self.push_gate(non_allocated_gate);
            return 0;
        }
        assert_eq!(non_allocated_gate.get_output_wire_id().unwrap(), 0, "output wire id should be zero for the new gate");
        let new_id = self.alloc();
        self.push_gate_with_output(&non_allocated_gate, new_id);
        new_id
    }
}

impl Builder {
    /// new a new builder

    pub fn new(free_id: u64) -> Builder {
        Builder {
            gates: Vec::<Gate>::new(),
            free_id
        }
    }

    /// alloc allocates a new wire id

    fn alloc(&mut self) -> WireId {
        let id = self.free_id;
        self.free_id += 1;
        id
    }

    /// push_gate pushes a gate to the gates array, the gate's input and output wires must be pre-allocated

    fn push_gate(&mut self, allocated_gate: Gate) {
        self.gates.push(allocated_gate);
    }

    /// create_gate_with_output ataches the specified wire_id as an output to the gate and pushed
    /// it into the gates array
    ///
    /// # Panics
    ///
    /// output id for the given gate is not zero.

    fn push_gate_with_output(&mut self, non_allocated_gate: &Gate, output_id: WireId) {
        assert_eq!(non_allocated_gate.get_output_wire_id().unwrap(), 0, "output wire must be 0 for a non allocated gate");

        match non_allocated_gate {
            Constant(_, v) => self.push_gate(Constant(output_id, v.clone())),
            Copy(_, w) => self.push_gate(Copy(output_id, w.clone())),
            Add(_, w1, w2) => self.push_gate(Add(output_id, w1.clone(), w2.clone())),
            Mul(_, w1, w2) => self.push_gate(Mul(output_id, w1.clone(), w2.clone())),
            AddConstant(_, w, v) => self.push_gate(AddConstant(output_id, w.clone(), v.clone())),
            MulConstant(_, w, v) => self.push_gate(MulConstant(output_id, w.clone(), v.clone())),
            And(_, w1, w2) => self.push_gate(And(output_id, w1.clone(), w2.clone())),
            Xor(_, w1, w2) => self.push_gate(Xor(output_id, w1.clone(), w2.clone())),
            Not(_, w) => self.push_gate(Not(output_id, w.clone())),

            AssertZero(_) => panic!("AssertZero has no output gate"),
            Free(_, _) => panic!("Free has no output gate"),
        }
    }
}

