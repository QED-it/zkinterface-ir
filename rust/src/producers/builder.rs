use std::mem::take;

pub use super::build_gates::BuildGate;
use super::build_gates::NO_OUTPUT;
use crate::producers::sink::MemorySink;
use crate::structs::value::Value;
use crate::{Gate, Header, Instance, Relation, Sink, WireId, Witness};
use std::cell::{RefCell, Cell};
use crate::structs::relation::{ARITH, FUNCTION, FOR, SWITCH};

pub trait GateBuilderT {
    /// Allocates a new wire id for the output and creates a new gate,
    /// Returns the newly allocated WireId.
    fn create_gate(&self, gate: BuildGate) -> WireId;
}

/// MessageBuilder builds messages by buffering sequences of gates and witness/instance values.
/// Flush completed messages to a Sink.
/// finish() must be called.
struct MessageBuilder<S: Sink> {
    sink: S,

    instance: Instance,
    witness: Witness,
    relation: Relation,

    /// Maximum number of gates or witness or instance values to hold at once.
    /// Default 100,000 or ~12MB of memory.
    /// Size estimation: 40 per witness + 40 per instance + 48 per gate = 128 bytes.
    pub max_len: usize,
}

impl<S: Sink> MessageBuilder<S> {
    fn new(sink: S, header: Header) -> Self {
        MessageBuilder::new_with_functionalities(sink, header, ARITH, FUNCTION|FOR|SWITCH)
    }

    fn new_with_functionalities(sink: S, header: Header, gateset: u16, features: u16) -> Self {
        Self {
            sink,
            instance: Instance {
                header: header.clone(),
                common_inputs: vec![],
            },
            witness: Witness {
                header: header.clone(),
                short_witness: vec![],
            },
            relation: Relation {
                header: header.clone(),
                gate_mask: gateset,
                feat_mask: features,
                gates: vec![],
            },
            max_len: 100 * 1000,
        }
    }

    fn push_instance_value(&mut self, value: Value) {
        self.instance.common_inputs.push(value);
        if self.instance.common_inputs.len() == self.max_len {
            self.flush_instance();
        }
    }

    fn push_witness_value(&mut self, value: Value) {
        self.witness.short_witness.push(value);
        if self.witness.short_witness.len() == self.max_len {
            self.flush_witness();
        }
    }

    fn push_gate(&mut self, gate: Gate) {
        self.relation.gates.push(gate);
        if self.relation.gates.len() == self.max_len {
            self.flush_relation();
        }
    }

    fn flush_instance(&mut self) {
        self.sink.push_instance_message(&self.instance).unwrap();
        self.instance.common_inputs.clear();
    }

    fn flush_witness(&mut self) {
        self.sink.push_witness_message(&self.witness).unwrap();
        self.witness.short_witness.clear();
    }

    fn flush_relation(&mut self) {
        self.sink.push_relation_message(&self.relation).unwrap();
        self.relation.gates.clear();
    }

    fn finish(mut self) -> S {
        if !self.instance.common_inputs.is_empty() {
            self.flush_instance();
        }
        if !self.witness.short_witness.is_empty() {
            self.flush_witness();
        }
        if !self.relation.gates.is_empty() {
            self.flush_relation();
        }
        self.sink
    }
}

/// GateBuilder allocates wire IDs, builds gates, and tracks instance and witness values.
///
/// # Example
/// ```
/// use zki_sieve::producers::builder::{GateBuilderT, GateBuilder, BuildGate::*};
/// use zki_sieve::producers::sink::MemorySink;
/// use zki_sieve::Header;
///
/// let mut b = GateBuilder::new(MemorySink::default(), Header::default());
///
/// let my_id = b.create_gate(Constant(vec![0]));
/// b.create_gate(AssertZero(my_id));
/// ```
pub struct GateBuilder<S: Sink> {
    msg_build: RefCell<MessageBuilder<S>>,

    free_id: Cell<WireId>,
}

impl<S: Sink> GateBuilderT for GateBuilder<S> {
    fn create_gate(&self, mut gate: BuildGate) -> WireId {
        let out_id = if gate.has_output() {
            self.alloc()
        } else {
            NO_OUTPUT
        };

        match gate {
            BuildGate::Instance(ref mut value) => {
                self.push_instance_value(take(value));
            }
            BuildGate::Witness(Some(ref mut value)) => {
                self.push_witness_value(take(value));
            }
            _ => {}
        }

        self.msg_build.borrow_mut().push_gate(gate.with_output(out_id));

        out_id
    }
}

impl<S: Sink> GateBuilder<S> {
    /// new creates a new builder.
    pub fn new(sink: S, header: Header) -> Self {
        GateBuilder {
            msg_build: RefCell::new(MessageBuilder::new(sink, header)),
            free_id: Cell::new(0),
        }
    }

    /// alloc allocates a new wire ID.
    fn alloc(&self) -> WireId {
        let id = self.free_id.take();
        self.free_id.set(id + 1);
        id
    }

    pub(crate) fn push_witness_value(&self, val: Value) {
        self.msg_build.borrow_mut().push_witness_value(val);
    }

    pub(crate) fn push_instance_value(&self, val: Value) {
        self.msg_build.borrow_mut().push_instance_value(val);
    }

    pub fn finish(self) -> S {
        self.msg_build.into_inner().finish()
    }
}

pub fn new_example_builder() -> GateBuilder<MemorySink> {
    GateBuilder::new(MemorySink::default(), Header::default())
}
