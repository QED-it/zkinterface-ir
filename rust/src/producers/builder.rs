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
    /// use zki::structs::gates::Gate::Constant;
    /// use zki::producers::builder::Builder;
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
    /// use zki::structs::gates::Gate::AssertZero;
    /// use zki::producers::builder::Builder;
    ///
    /// let free_variable_id = 5;
    /// let mut b = Builder::new(free_variable_id);
    ///
    /// let new_id = b.create_gate(AssertZero(5));
    ///
    /// ```
    fn create_gate(&mut self, non_allocated_gate: Gate) -> WireId {
        if ! non_allocated_gate.has_output() {
            self.push_gate(non_allocated_gate);
            return 0;
        }
        assert_eq!(non_allocated_gate.get_output_wire_id(), 0);
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

    fn alloc(&mut self) -> WireId {
        let id = self.free_id;
        self.free_id += 1;
        id
    }

    fn push_gate(&mut self, allocated_gate: Gate) {
        self.gates.push(allocated_gate);
    }
}

fn with_output(non_allocated_gate: &Gate, output_id: WireId) -> Gate {
    assert_eq!(non_allocated_gate.get_output_wire_id(), 0, "output wire must be 0 for a non allocated gate");
    match non_allocated_gate {
        Constant(_, v) => Constant(output_id, v.clone()),
        Copy(_, w) => Copy(output_id, w.clone()),
        Add(_, w1, w2) => Add(output_id, w1.clone(), w2.clone()),
        Mul(_, w1, w2) => Mul(output_id, w1.clone(), w2.clone()),
        AddConstant(_, w, v) => AddConstant(output_id, w.clone(), v.clone()),
        MulConstant(_, w, v) => MulConstant(output_id, w.clone(), v.clone()),
        And(_, w1, w2) => And(output_id, w1.clone(), w2.clone()),
        Xor(_, w1, w2) => Xor(output_id, w1.clone(), w2.clone()),
        Not(_, w) => Not(output_id, w.clone()),

        AssertZero(_) => panic!("AssertZero has no output gate"),
    }
}