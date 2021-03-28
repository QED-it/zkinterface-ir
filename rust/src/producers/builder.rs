use super::build_gates::NO_OUTPUT;
use crate::{Gate, Header, Instance, Relation, Sink, WireId, Witness};

pub use super::build_gates::BuildGate;
use crate::producers::sink::MemorySink;
use crate::structs::assignment::Assignment;

/// MessageBuilder builds messages gate by gate.
/// Flush completed messages to a Sink.
struct MessageBuilder<S: Sink> {
    sink: S,

    instance: Instance,
    witness: Witness,
    relation: Relation,
}

impl<S: Sink> MessageBuilder<S> {
    fn new(sink: S, header: Header) -> Self {
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
                gates: vec![],
            },
        }
    }

    fn push_instance_value(&mut self, assignment: Assignment) {
        self.instance.common_inputs.push(assignment);
    }

    fn push_witness_value(&mut self, assignment: Assignment) {
        self.witness.short_witness.push(assignment);
    }

    fn push_gate(&mut self, gate: Gate) {
        self.relation.gates.push(gate);
    }
    fn finish(mut self) -> S {
        self.sink.push_instance_message(&self.instance).unwrap();
        self.sink.push_witness_message(&self.witness).unwrap();
        self.sink.push_relation_message(&self.relation).unwrap();
        self.sink
    }
}

/// Builder allocates wire IDs, builds gates, and tracks instance and witness values.
///
/// # Example
/// ```
/// use zki_sieve::producers::builder::{Builder, BuildGate::*};
/// use zki_sieve::producers::sink::MemorySink;
/// use zki_sieve::Header;
///
/// let mut b = Builder::new(MemorySink::default(), Header::default());
///
/// let my_id = b.create_gate(Constant(vec![0]));
/// b.create_gate(AssertZero(my_id));
/// ```
pub struct Builder<S: Sink> {
    msg_build: MessageBuilder<S>,

    free_id: WireId,
}

impl<S: Sink> Builder<S> {
    /// new creates a new builder.
    pub fn new(sink: S, header: Header) -> Self {
        Self {
            msg_build: MessageBuilder::new(sink, header),
            free_id: 0,
        }
    }

    /// Allocates a new wire id for the output and creates a new gate,
    /// Returns the newly allocated WireId.
    pub fn create_gate(&mut self, gate: BuildGate) -> WireId {
        let out_id = if gate.has_output() {
            self.alloc()
        } else {
            NO_OUTPUT
        };

        match &gate {
            BuildGate::Instance(value) => {
                self.msg_build.push_instance_value(Assignment {
                    id: out_id,
                    value: value.clone(),
                });
            }
            BuildGate::Witness(Some(value)) => {
                self.msg_build.push_witness_value(Assignment {
                    id: out_id,
                    value: value.clone(),
                });
            }
            _ => {}
        }

        self.msg_build.push_gate(gate.with_output(out_id));

        out_id
    }

    /// alloc allocates a new wire ID.
    fn alloc(&mut self) -> WireId {
        let id = self.free_id;
        self.free_id += 1;
        id
    }

    pub fn finish(self) -> S {
        self.msg_build.finish()
    }
}

pub fn new_example_builder() -> Builder<MemorySink> {
    Builder::new(MemorySink::default(), Header::default())
}
