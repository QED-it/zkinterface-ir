# zkInterface IR, a toolbox for zero-knowledge interoperability

This user guide is aimed at implementors of zero-knowledge systems and details how to integrate their systems using the zkInterface SIEVE IR toolbox
(see the [standard specification](https://github.com/sieve-zk/ir)).
In this guide, we first generate example SIEVE IR statements, and we then consume them. These steps can serve 
as a starting point for a new implementation in a statement generator, or in a proving system, respectively.

## Introduction

The zkInterface SIEVE IR is an Intermediate Representation to communicate a zero-knwoledge statement
from a statement generator called a frontend to a proving system called a backend.
The frontend transforms high level statements into the IR.
The backend is the consumer of the IR: it is an interaction between a Prover and a Verifier,
when the Prover wishes to prove a statement to the Verifier, without revealing a secret component of the proof.
For details on this zkInterface SIEVE IR, see the
[standard specification](https://github.com/sieve-zk/ir).
This IR focuses on a circuit-and-gates format.
It is possible to convert between IR and R1CS with `zkif-to-ir` and `ir-to-zkif` commands.
For R1CS systems, there exists a similar toolbox in 
[github.com/QED-it/zkinterface](https://github.com/QED-it/zkinterface).

## Information Flow

To communicate a statement, three types of information are transmitted:

- A set of parameters and knobs that define the type of the statement being used, called the _Header_.

- A description of computation as a circuit of gates connected through wires, called the _Relation_.

- A list of values used as input to the circuit by the prover side of the proving system, called the _PrivateInputs_.

- A list of values used as input to the circuit both by the prover and the verifier, called the _PublicInputs_.

The exact structure of this information is specified in a FlatBuffers schema called `sieve_ir.fbs` in this repository, 
along with inline documentation. See the respective structures: Header, Relation, PublicInputs, PrivateInputs.

In this guide, the structures are stored in intermediary files for ease and clarity. However, streaming implementations 
without storage are also supported.

## First step: getting familiar with existing tools

### Install

```bash
git clone git@github.com:QED-it/zkinterface-ir.git
cd zkinterface-ir/rust/
cargo install --path .

zki_sieve help

This will print a list of available commands (your mileage may vary depending on your environment).
```

### A producer: example generator

The command below generates an example statement. It stores it into files in the working directory (customizable, see `zki_sieve help`).

    zki_sieve example

    …
    Writing ./000_public_inputs.sieve
    Writing ./001_private_inputs.sieve
    Writing ./002_relation.sieve


### A consumer: validator and evaluator

The `validate` command validates that the statement is properly formatted in compliance with the selected profile, as specified by the semantics and syntax of Section 5 of the [SIEVE IR specification](https://github.com/sieve-zk/ir).

The `evaluate` command acts as a simulator in place of a proving system, and reports whether a prover could convince a verifier that the statement is true. That is, it performs the computation described by the circuit and checks whether the private inputs satisfy the circuit.

    $ zki_sieve validate
    
    
    Reading ./000_public_inputs.sieve
    Reading ./001_private_inputs.sieve
    Reading ./002_relation.sieve

    The statement is COMPLIANT with the specification!

 And the evaluator,

    $ zki_sieve evaluate
    
    …
    Reading ./000_public_inputs.sieve
    Reading ./001_private_inputs.sieve
    Reading ./002_relation.sieve

    The statement is TRUE!

There is a command `zki_sieve valid-eval-metrics` which performs all checks at once.


### A consumer: format to human-readable YAML

The command below reads the statement and prints a textual representation of it. It uses the YAML format, which is similar to JSON but easier to read and write. It is one-to-one equivalent to the information formatted with FlatBuffers.


    $ zki_sieve to-yaml
    # Or zki_sieve to-json

    Reading ./000_public_inputs.sieve
    Reading ./001_private_inputs.sieve
    Reading ./002_relation.sieve
    ---
    public_inputs:
      - header:
          version: 1.0.0
          fields:
            - - 101
              - 0
              - 0
              - 0
            - - 7
              - 0
              - 0
              - 0
        inputs:
          - values:
              - - 25
              - - 0
              - - 1
          - values:
              - - 6
              - - 1
              - - 0
    private_inputs:
      - header:
          version: 1.0.0
          fields:
            - - 101
              - 0
              - 0
              - 0
            - - 7
              - 0
              - 0
              - 0
        inputs:
          - values:
              - - 3
              - - 4
              - - 0
          - values:
              - - 4
              - - 2
              - - 3
    relations:
      - header:
          version: 1.0.0
          fields:
            - - 101
              - 0
              - 0
              - 0
            - - 7
              - 0
              - 0
              - 0
        functions:
          - name: "com.example::mul"
            output_count:
              0: 1
            input_count:
              0: 2
            public_count: {}
            private_count: {}
            body:
              Gates:
                - Mul:
                    - 0
                    - 0
                    - 1
                    - 2
          - name: vector_add_7_3
            output_count:
              1: 3
            input_count:
              1: 6
            public_count: {}
            private_count: {}
            body:
              PluginBody:
                name: vector
                operation: add
                params:
                  - "1"
                  - "3"
        gates:
          - New:
              - 0
              - 0
              - 7
          - PrivateInput:
              - 0
              - 1
          - AnonCall:
              - - Wire:
                    - 0
                    - 0
                - Wire:
                    - 0
                    - 2
                - Wire:
                    - 0
                    - 4
                - Wire:
                    - 0
                    - 5
                - Wire:
                    - 0
                    - 6
                - Wire:
                    - 0
                    - 9
                - Wire:
                    - 0
                    - 10
              - - Wire:
                    - 0
                    - 1
              - 0: 3
              - 0: 2
              - - PublicInput:
                    - 0
                    - 0
                - PrivateInput:
                    - 0
                    - 1
                - Call:
                    - "com.example::mul"
                    - - Wire:
                          - 0
                          - 2
                    - - Wire:
                          - 0
                          - 7
                      - Wire:
                          - 0
                          - 7
                - Call:
                    - "com.example::mul"
                    - - Wire:
                          - 0
                          - 3
                    - - Wire:
                          - 0
                          - 1
                      - Wire:
                          - 0
                          - 1
                - Add:
                    - 0
                    - 4
                    - 2
                    - 3
                - PrivateInput:
                    - 0
                    - 8
                - AssertZero:
                    - 0
                    - 8
                - PublicInput:
                    - 0
                    - 5
                - AssertZero:
                    - 0
                    - 5
                - PublicInput:
                    - 0
                    - 6
          - Constant:
              - 0
              - 3
              - - 100
                - 0
                - 0
                - 0
          - Call:
              - "com.example::mul"
              - - Wire:
                    - 0
                    - 7
              - - Wire:
                    - 0
                    - 3
                - Wire:
                    - 0
                    - 0
          - Add:
              - 0
              - 8
              - 6
              - 7
          - Delete:
              - 0
              - 0
              - 7
          - Mul:
              - 0
              - 11
              - 8
              - 10
          - AssertZero:
              - 0
              - 11
          - Delete:
              - 0
              - 8
              - 11
          - PublicInput:
              - 1
              - 0
          - PublicInput:
              - 1
              - 1
          - PublicInput:
              - 1
              - 2
          - PrivateInput:
              - 1
              - 3
          - PrivateInput:
              - 1
              - 4
          - PrivateInput:
              - 1
              - 5
          - Add:
              - 1
              - 6
              - 0
              - 3
          - Mul:
              - 1
              - 7
              - 1
              - 4
          - Mul:
              - 1
              - 8
              - 2
              - 5
          - AssertZero:
              - 1
              - 8
          - Delete:
              - 1
              - 0
              - 5
          - Mul:
              - 1
              - 9
              - 6
              - 7
          - AddConstant:
              - 1
              - 10
              - 9
              - - 1
                - 0
                - 0
                - 0
          - AssertZero:
              - 1
              - 10
          - Convert:
              - - Wire:
                    - 0
                    - 12
                - Wire:
                    - 0
                    - 13
              - - Wire:
                    - 1
                    - 8
                - Wire:
                    - 1
                    - 7
                - Wire:
                    - 1
                    - 6
          - AddConstant:
              - 0
              - 14
              - 13
              - - 84
                - 0
                - 0
                - 0
          - AssertZero:
              - 0
              - 14
          - AssertZero:
              - 0
              - 12
          - Delete:
              - 1
              - 6
              - 10
          - Delete:
              - 0
              - 12
              - 14
          - Constant:
              - 0
              - 15
              - - 9
          - Convert:
              - - Wire:
                    - 1
                    - 11
                - Wire:
                    - 1
                    - 12
                - Wire:
                    - 1
                    - 13
              - - Wire:
                    - 0
                    - 15
          - AddConstant:
              - 1
              - 14
              - 13
              - - 5
          - AddConstant:
              - 1
              - 15
              - 12
              - - 6
          - AssertZero:
              - 1
              - 11
          - AssertZero:
              - 1
              - 14
          - AssertZero:
              - 1
              - 15
          - Delete:
              - 0
              - 15
              - ~
          - Delete:
              - 1
              - 11
              - 15
          - Constant:
              - 1
              - 16
              - - 1
          - Constant:
              - 1
              - 17
              - - 2
          - Constant:
              - 1
              - 18
              - - 3
          - Constant:
              - 1
              - 19
              - - 4
          - Constant:
              - 1
              - 20
              - - 5
          - Constant:
              - 1
              - 21
              - - 6
          - Call:
              - vector_add_7_3
              - - WireRange:
                    - 1
                    - 22
                    - 24
              - - WireRange:
                    - 1
                    - 16
                    - 21
          - Add:
              - 1
              - 25
              - 22
              - 24
          - AssertZero:
              - 1
              - 23
          - AssertZero:
              - 1
              - 25
          - Delete:
              - 1
              - 16
              - 25



### A producer: converter from R1CS

This repository includes a converter that reads a statement encoded in the R1CS profile and produces an equivalent 
statement in the arithmetic circuit profile.

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
$ zki_sieve zkif-to-ir [oter options] 
```

### A producer: IR to IR-simple converter

The converter in `consumers/flattening.rs` allows to convert any IR circuit into 
an equivalent circuit that make use of only simple gates (i.e. 
`Functions` are unrolled). This operation (conversion from IR to
IR simple) is sometimes called 'flattening'. 

Here is a simple code to call this converter:

```rust
let relation = example_relation();
let public_inputs = example_public_inputs();
let private_inputs = example_private_inputs();

let mut flattener = IRFlattener::new(MemorySink::default());
let mut evaluator = Evaluator::default();

evaluator.ingest_public_inputs(&public_inputs)?;
evaluator.ingest_private_inputs(&private_inputs)?;
evaluator.ingest_relation(&relation, &mut flattener)?;
```

This functionality is also available as a command-line tool.
```
$ zki_sieve flatten [other options] 
```

## Second step: implementing a new integration

### Example code.

An easy way to start a new integration is to explore the source code of the library, which is itself called from the CLI commands. The entry points are the functions called `main_…` in the file `src/bin/zki_sieve.rs`.  Additional example code can be found in the `test_…` functions in the directory `src/producers/` and `src/consumers/`.

### Basic API

All information to be transmitted between systems is in data structures formally specified by the FlatBuffers schema. The simplest Rust API available is a straight one-to-one mapping of these structures.

A producer can create a `Relation` structure and populate its `gates` vector with a number of `Gate`, in compliance with the specification.

A consumer can iterate over `Relation.gates` and act on the different gate types using, e.g., a `match` construct.

Implementations should expect to produce or receive not one but a stream of these structures in order to process very large statements with limited memory.

### Advanced API

This API has been written to allow backends to easily operate on IR without having to care of
the complexity, and future upgrades of IR specifications, nor the internal structures described in the 
previous section.

The core of it is the `ZKBackend` trait defined in `consumers/evaluator.rs`. It has to be implemented by any backend
wanting to deal with IR.
See for example `PlaintextBackend` for a simple (but unsecure) implementation of this trait.

This trait is currently used in many cases throughout this project, such as:
  - the circuit evaluator to ensure that a circuit is verified
  - the IR to IR-simple converter
  - the IR to R1CS converter  

This API is the recommended way for backends to operate on SIEVE IR, since it's independent from the internal structures,
and will decrease the development overhead of new versions of the standard.

### Low-level serialization

It is not necessary to use the above APIs to integrate SIEVE IR. Any implementation of FlatBuffers can be used directly instead (a custom implementation is doable because the encoding is simple, but that would be a last resort). See [google.github.io/flatbuffers/](https://google.github.io/flatbuffers/) for existing tools, in particular the code generator `flatc`. This is the recommended approach for systems written in languages other than Rust.

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
Approved for Public Release, Distribution Unlimited.
