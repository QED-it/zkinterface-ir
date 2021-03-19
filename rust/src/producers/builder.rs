use crate::{WireId, Gate};
use super::build_gates::NO_OUTPUT;

pub use super::build_gates::BuildGate;


pub trait IBuilder {
    /// Allocates a new wire id for the output and creates a new gate,
    /// Returns the newly allocated WireId.
    ///
    /// # Examples
    ///
    /// ```
    /// use zki_sieve::producers::builder::{IBuilder, Builder, BuildGate};
    /// use BuildGate::*;
    ///
    /// let mut b = Builder::default();
    ///
    /// let my_id = b.create_gate(Constant(vec![0]));
    /// b.create_gate(AssertZero(my_id));
    /// ```
    fn create_gate(&mut self, non_allocated_gate: BuildGate) -> WireId;
}

#[derive(Default)]
pub struct Builder {
    pub gates: Vec<Gate>,
    free_id: WireId,
}

impl IBuilder for Builder {
    fn create_gate(&mut self, gate: BuildGate) -> WireId {
        let out_id = if gate.has_output() {
            self.alloc()
        } else {
            NO_OUTPUT
        };

        self.push_gate(
            gate.with_output(out_id));

        out_id
    }
}

impl Builder {
    /// new creates a new builder. Wire IDs start at free_id (usually 0).
    pub fn new(free_id: u64) -> Builder {
        Builder {
            gates: Vec::<Gate>::new(),
            free_id,
        }
    }

    /// alloc allocates a new wire ID.
    pub fn alloc(&mut self) -> WireId {
        let id = self.free_id;
        self.free_id += 1;
        id
    }

    /// push_gate records a gate. The input and output wires must have been allocated.
    fn push_gate(&mut self, allocated_gate: Gate) {
        self.gates.push(allocated_gate);
    }
}
