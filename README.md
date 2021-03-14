# Getting Started with the SIEVE IR toolbox

This user guide is aimed at implementors of zero-knowledge systems and details how to integrate their systems using the zkInterface SIEVE IR toolbox. For details on the zkInterface framework see [the original spec](https://github.com/QED-it/zkinterface/blob/master/zkInterface.pdf), and for details on the SIEVE IR format, see the [standard specification](https://github.mit.edu/sieve-all/collaboration/blob/master/ir/proposals/IR0%20Proposed%20Specification%20Draft.pdf).

This guide uses the zkInterface SIEVE IR supporting library for the Rust programming language, and its companion command-line interface (CLI). It focuses on a circuit-and-gates format. For R1CS systems, see similar content in [github.com/QED-it/zkinterface](https://github.com/QED-it/zkinterface).

zkInterface SIEVE IR is a method to communicate a zero-knowledge statement from a statement generator to a proving system. In this guide, we first generate example SIEVE IR statements, and we then consume them. These steps can serve as a starting point for a new implementation in a statement generator, or in a proving system, respectively.

## Information Flow

To communicate a statement, three types of information are transmitted:

- A set of parameters and knobs that define the type of the statement being used, called the _Header_.

- A description of computation as a circuit of gates connected through wires, called the _Relation_.

- A witness used as input to the circuit by the prover side of the proving system, caleld the _Witness_.

- An instance used as input to the circuit both by the prover and the verifier, called the _Instance_.

The exact structure of this information is specified in a FlatBuffers schema called `sieve_ir.fbs` in this repository, along with inline documentation. See the respective structures: Header, Relation, Instance, Witness.

In this guide, the structures are stored in intermediary files for ease and clarity. However, streaming implementations without storage are also supported.

## First step: getting familiar with existing tools

### Install

```bash
git clone git@github.mit.edu:sieve-all/zkinterface-sieve.git
cd zkinterface-sieve/rust/
cargo install --path .

zki_sieve help

This will print a list of available commands (your mileage may vary depending on your environment).
```

### A producer: example generator

The command below generates an example statement. It stores it into files in the working directory (customizable, see `zki_sieve help`). The profile AC (Arithmetic Circuit) was selected.

    zki_sieve example

    …
    Writing ./000_instance.sieve
    Writing ./001_witness.sieve
    Writing ./002_relation.sieve


### A consumer: validator and evaluator

The `Validate` command validates that the statement is properly formatted in compliance with the selected profile, as specified by the semantics and syntax of Section 5 of the [SIEVE IR specification](https://github.mit.edu/sieve-all/collaboration/blob/master/ir/proposals/IR0%20Proposed%20Specification%20Draft.pdf).

The `Evaluate` command acts as a simulator in place of a proving system, and reports whether a prover could convince a verifier that the statement is true. That is, it performs the computation described by the circuit and checks whether the witness satisfies the circuit.

    zki_sieve validate
    
    …
    Reading ./000_instance.sieve
    Reading ./001_witness.sieve
    Reading ./002_relation.sieve

    The statement is COMPLIANT with the specification!

 And the evaluator,

    zki_sieve evaluate
    
    …
    Reading ./000_instance.sieve
    Reading ./001_witness.sieve
    Reading ./002_relation.sieve

    The statement is TRUE!

There is a command `zki_sieve valid-eval-metrics` which performs all checks at once.


### A consumer: format to human-readable YAML

The command below reads the statement and prints a textual representation of it. It uses the YAML format, which is similar to JSON but easier to read and write. It is one-to-one equivalent to the information formatted with FlatBuffers.


    zki_sieve to-yaml
    # Or zki_sieve to-json

    …
    Reading ./000_instance.sieve
    Reading ./001_witness.sieve
    Reading ./002_relation.sieve
    ---
    instances:
      - header:
          version: 0.1.0
          profile: circ_arithmetic_simple
          field_characteristic:
            - 101
            - 0
            - 0
            - 0
          field_degree: 1
        common_inputs:
          - id: 0
            value:
              - 25
              - 0
              - 0
              - 0
    witnesses:
      - header:
          version: 0.1.0
          profile: circ_arithmetic_simple
          field_characteristic:
            - 101
            - 0
            - 0
            - 0
          field_degree: 1
        short_witness:
          - id: 1
            value:
              - 3
              - 0
              - 0
              - 0
          - id: 2
            value:
              - 4
              - 0
              - 0
              - 0
    relations:
      - header:
          version: 0.1.0
          profile: circ_arithmetic_simple
          field_characteristic:
            - 101
            - 0
            - 0
            - 0
          field_degree: 1
        gates:
          - Constant:
              - 3
              - - 100
                - 0
                - 0
                - 0
          - Mul:
              - 4
              - 1
              - 1
          - Mul:
              - 5
              - 2
              - 2
          - Add:
              - 6
              - 4
              - 5
          - Mul:
              - 7
              - 0
              - 3
          - Add:
              - 8
              - 6
              - 7
          - AssertZero: 8


### A producer: converter from R1CS

This repository includes a converter that reads a statement encoded in the R1CS profile and produces an equivalent statement in the arithmetic circuit profile.

To convert from R1CS (a zkInterface owned structure), follow the following example:

```rust
let zki_header =  zki_example_header_inputs(3, 4, 25);
let zki_r1cs = zki_example_constrains();
let zki_witness = zki_example_witness_inputs(3, 4);

// in zkInterface the instance is inside the header
// in ir each msg in {instance, relation, witness} has an header
let (instance, relation) = to_ir(&zki_header, &zki_r1cs);

let witness = to_witness(&zki_header, &zki_witness);
```

A full example can be found as part of the `test_r1cs_to_gates()` in `from_r1cs.rs`.

Also, example usage with the validator and the evaluator can be found in `test_with_evaluator()` and `test_with_validate()`.

## Second step: implementing a new integration

### Example code.

An easy way to start a new integration is to explore the source code of the library, which is itself called from the CLI commands. The entry points are the functions called `main_…` in the file `src/bin/zki_sieve.rs`.  Additional example code can be found in the `test_…` functions in the directory `src/producers/` and `src/consumers/`.

### Basic API

All information to be transmitted between systems is in data structures formally specified by the FlatBuffers schema. The simplest Rust API available is a straight one-to-one mapping of these structures.

A producer can create a `Relation` structure and populate its `gates` vector with a number of `Gate`, in compliance with the specification.

A consumer can iterate over `Relation.gates` and act on the different gate types using, e.g., a `match` construct.

Implementations should expect to produce or receive not one but a stream of these structures in order to process very large statements with limited memory.


### Low-level serialization

It is not necessary to use the above APIs to integrate SIEVE IR. Any implementation of FlatBuffers can be used directly instead (a custom implementation is doable because the encoding is simple, but that would be a last resort). See [google.github.io/flatbuffers/](https://google.github.io/flatbuffers/) for existing tools, in particular the code generator `flatc`. This is the recommended approach for systems written in languages other than Rust.
