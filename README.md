# zkInterface IR, a toolbox for zero-knowledge interoperability

This user guide is aimed at implementors of zero-knowledge systems
and details how to integrate their systems using the zkInterface SIEVE IR toolbox
(see the [specification](spec/sieve_ir_spec.pdf)).
In this guide, we first generate example SIEVE IR statements, and we then consume them. These steps can serve 
as a starting point for a new implementation in a statement generator, or in a proving system, respectively.

## Introduction

The zkInterface SIEVE IR is an Intermediate Representation to communicate a zero-knwoledge statement
from a statement generator called a frontend to a proving system called a backend.
The frontend transforms high level statements into the IR.
The backend is the consumer of the IR: it is an interaction between a Prover and a Verifier,
when the Prover wishes to prove a statement to the Verifier, without revealing a secret component of the proof.
For details on this zkInterface SIEVE IR, see the [specification](spec/sieve_ir_spec.pdf).
This IR focuses on a circuit-and-gates format.
It is possible to convert between IR and R1CS with `zkif-to-ir` and `ir-to-zkif` commands.
For R1CS systems, there exists a similar toolbox in 
[github.com/QED-it/zkinterface](https://github.com/QED-it/zkinterface).

## Information Flow

To communicate a statement, three types of information are transmitted:

- A description of computation as a circuit of gates connected through wires, called the _Relation_.

- A list of values used as input to the circuit by the prover side of the proving system, called the _PrivateInputs_.

- A list of values used as input to the circuit both by the prover and the verifier, called the _PublicInputs_.

The exact structure of this information is specified in a FlatBuffers schema called `sieve_ir.fbs` in this repository, 
along with inline documentation. See the respective structures: Relation, PublicInputs, PrivateInputs.

In this guide, the structures are stored in intermediary files for ease and clarity. However, streaming implementations 
without storage are also supported.

## Multi Field Circuits

To most practitioners of ZK, a single prime field is chosen at the beginning of a proof and used
throughout. However, for some applications it is desirable to use multiple primes for different
elements within a single larger proof. For example a large and expensive prime may be needed
to verify public-key signatures, while a medium sized prime is necessary for large scale business
logic.

To accommodate these applications, the SIEVE IR allows for multiple fields within a single
relation. Occasionally information from one field will be required in another. IT is modeled using
a conversion gate with inputs in one field and outputs in another.

## Plugins

We woud like to easily be able to extend the IR with some new (complex) features.
Unfortunately, each update in the basic syntax forces frontends and backends to update
their IR generator and parser. We would like to avoid this burden while increasing expressibility
of the language. To resolve this issue, we create `plugin` functions which allow calls
from the IR into backend specific functionality.

## First step: getting familiar with existing tools

### Install

```bash
git clone git@github.com:QED-it/zkinterface-ir.git
cd zkinterface-ir/rust/
cargo install --path .

zki_sieve help

This will print a list of available commands.
```

### A producer: example generator

The command below generates an example statement.
It stores it into files in the working directory (customizable, see `zki_sieve help`).

    zki_sieve example

    …
    Writing ./000_public_inputs_0.sieve
    Writing ./000_public_inputs_1.sieve
    Writing ./001_private_inputs_0.sieve
    Writing ./001_private_inputs_1.sieve
    Writing ./001_private_inputs_2.sieve
    Writing ./002_relation.sieve


### A consumer: validator and evaluator

The `validate` command validates that the statement is properly formatted in compliance
with the specification (see section Circuit Semantics and Validity in the [specification](spec/sieve_ir_spec.pdf).

The `evaluate` command acts as a simulator in place of a proving system,
and reports whether a prover could convince a verifier that the statement is true.
That is, it performs the computation described by the circuit and checks whether the private inputs satisfy the circuit.

    $ zki_sieve validate
    
    …
    Reading ./000_public_inputs_0.sieve
    Reading ./000_public_inputs_1.sieve
    Reading ./001_private_inputs_0.sieve
    Reading ./001_private_inputs_1.sieve
    Reading ./001_private_inputs_2.sieve
    Reading ./002_relation.sieve

    The statement is COMPLIANT with the specification!

 And the evaluator,

    $ zki_sieve evaluate
    
    …
    Reading ./000_public_inputs_0.sieve
    Reading ./000_public_inputs_1.sieve
    Reading ./001_private_inputs_0.sieve
    Reading ./001_private_inputs_1.sieve
    Reading ./001_private_inputs_2.sieve
    Reading ./002_relation.sieve

    The statement is TRUE!

There is a command `zki_sieve valid-eval-metrics` which performs all checks at once.


### A consumer: format to human-readable YAML

The command below reads the statement and prints a textual representation of it.
It uses the YAML format, which is similar to JSON but easier to read and write.
It is one-to-one equivalent to the information formatted with FlatBuffers.


    $ zki_sieve to-yaml
    # Or zki_sieve to-json


### A producer: converter from R1CS

This repository includes a converter that reads a statement encoded in the R1CS profile and produces an equivalent 
statement in the circuit IR profile.

To convert from R1CS (a zkInterface owned structure), follow the following example:

```rust

let zki_header = zki_example_header_inputs(3, 4, 25);
let zki_r1cs = zki_example_constraints();
let zki_witness = zki_example_witness_inputs(3, 4);
// The `MemorySink` is used to tell that the resulting circuit will be stored in memory
// see FilesSink to store it in files.
let mut converter = FromR1CSConverter::new(MemorySink::default(), &zki_header);
converter.ingest_witness(&zki_witness)?;
converter.ingest_constraints(&zki_r1cs)?;
```

A full example can be found as part of the `test_r1cs_to_gates()` in `from_r1cs.rs`.

This functionality is also available as a command-line tool.
```
$ zki_sieve zkif-to-ir [other options] 
```

## Second step: implementing a new integration

### Example code.

An easy way to start a new integration is to explore the source code of the library, which is itself called from the CLI commands.
The entry points are the functions called `main_…` in the file `src/bin/zki_sieve.rs`.
Additional example code can be found in the `test_…` functions in the directory `src/producers/` and `src/consumers/`.

### Basic API

All information to be transmitted between systems is in data structures formally specified by the FlatBuffers schema.
The simplest Rust API available is a straight one-to-one mapping of these structures.

A producer can create a `Relation` structure and populate its `directives` vector with a number of `Gate` and `Function` declaration, in compliance with the specification.

A consumer can iterate over `Relation.directives` and act on the different directive types using, e.g., a `match` construct.

Implementations should expect to produce or receive not one but a stream of these structures in order to process very large statements with limited memory.

### Advanced API

This API has been written to allow backends to easily operate on IR without having to care of
the complexity, and future upgrades of IR specifications, nor the internal structures described in the 
previous section.

The core of it is the `ZKBackend` trait defined in `consumers/evaluator.rs`.
It has to be implemented by any backend wanting to deal with IR.
See for example `PlaintextBackend` for a simple (but unsecure) implementation of this trait.

This trait is currently used in many cases throughout this project, such as:
  - the circuit evaluator to ensure that a circuit is verified
  - the IR to R1CS converter  

This API is the recommended way for backends to operate on SIEVE IR, since it's independent from the internal structures,
and will decrease the development overhead of new versions of the standard.

### Low-level serialization

It is not necessary to use the above APIs to integrate SIEVE IR.
Any implementation of FlatBuffers can be used directly instead
(a custom implementation is doable because the encoding is simple, but that would be a last resort).
See [google.github.io/flatbuffers/](https://google.github.io/flatbuffers/) for existing tools, in particular the code generator `flatc`.
This is the recommended approach for systems written in languages other than Rust.

The file `sieve_ir_generated.rs` could be generated with the command `make fbs` after installing flatc version 1.12.0.
```bash
# Install flatc version 1.12.0
git clone https://github.com/google/flatbuffers.git.
cd flatbuffers
git checkout v1.12.0
cmake -G "Unix Makefiles"
make
sudo ln -s $(realpath flatc) /usr/local/bin/flatc
```

## Acknowledgments

This material is based upon work supported by the Defense Advanced Research Projects Agency
(DARPA) under Contract No. HR001120C0085. Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the
views of the Defense Advanced Research Projects Agency (DARPA).
