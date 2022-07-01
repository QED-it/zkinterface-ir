# zkInterface IR, a toolbox for zero-knowledge interoperability

This user guide is aimed at implementors of zero-knowledge systems and details how to integrate their systems using the zkInterface SIEVE IR toolbox
(see the [standard specification](https://github.mit.edu/sieve-all/collaboration/blob/master/ir/proposals/IR0%20Proposed%20Specification%20Draft.pdf)).
In this guide, we first generate example SIEVE IR statements, and we then consume them. These steps can serve 
as a starting point for a new implementation in a statement generator, or in a proving system, respectively.

## Introduction

The zkInterface SIEVE IR is an Intermediate Representation to communicate a zero-knwoledge statement
from a statement generator called a frontend to a proving system called a backend.
The frontend transforms high level statements into the IR.
The backend is the consumer of the IR: it is an interaction between a Prover and a Verifier,
when the Prover wishes to prove a statement to the Verifier, without revealing a secret component of the proof.
For details on this zkInterface SIEVE IR, see the
[standard specification](https://github.mit.edu/sieve-all/collaboration/blob/master/ir/proposals/IR0%20Proposed%20Specification%20Draft.pdf).
This IR focuses on a circuit-and-gates format.
It is possible to convert between IR and R1CS with `zkif-to-ir` and `ir-to-zkif` commands.
For R1CS systems, there exists a similar toolbox in 
[github.com/QED-it/zkinterface](https://github.com/QED-it/zkinterface).

## Information Flow

To communicate a statement, three types of information are transmitted:

- A set of parameters and knobs that define the type of the statement being used, called the _Header_.

- A description of computation as a circuit of gates connected through wires, called the _Relation_.

- A witness used as input to the circuit by the prover side of the proving system, called the _Witness_.

- An instance used as input to the circuit both by the prover and the verifier, called the _Instance_.

The exact structure of this information is specified in a FlatBuffers schema called `sieve_ir.fbs` in this repository, 
along with inline documentation. See the respective structures: Header, Relation, Instance, Witness.

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

The command below generates an example statement. It stores it into files in the working directory (customizable, see `zki_sieve help`). The profile AC (Arithmetic Circuit) was selected.

    zki_sieve example

    …
    Writing ./000_instance.sieve
    Writing ./001_witness.sieve
    Writing ./002_relation.sieve


### A consumer: validator and evaluator

The `validate` command validates that the statement is properly formatted in compliance with the selected profile, as specified by the semantics and syntax of Section 5 of the [SIEVE IR specification](https://github.mit.edu/sieve-all/collaboration/blob/master/ir/proposals/IR0%20Proposed%20Specification%20Draft.pdf).

The `Evaluate` command acts as a simulator in place of a proving system, and reports whether a prover could convince a verifier that the statement is true. That is, it performs the computation described by the circuit and checks whether the witness satisfies the circuit.

    $ zki_sieve validate
    
    
    Reading ./000_instance.sieve
    Reading ./001_witness.sieve
    Reading ./002_relation.sieve

    The statement is COMPLIANT with the specification!

 And the evaluator,

    $ zki_sieve evaluate
    
    …
    Reading ./000_instance.sieve
    Reading ./001_witness.sieve
    Reading ./002_relation.sieve

    The statement is TRUE!

There is a command `zki_sieve valid-eval-metrics` which performs all checks at once.


### A consumer: format to human-readable YAML

The command below reads the statement and prints a textual representation of it. It uses the YAML format, which is similar to JSON but easier to read and write. It is one-to-one equivalent to the information formatted with FlatBuffers.


    $ zki_sieve to-yaml
    # Or zki_sieve to-json

    Reading ./000_instance.sieve
    Reading ./001_witness.sieve
    Reading ./002_relation.sieve
    ---
    instances:
      - header:
          version: 1.0.0
          field_characteristic:
            - 101
          field_degree: 1
        common_inputs:
          - - 25
            - 0
            - 0
            - 0
          - - 0
            - 0
            - 0
            - 0
          - - 1
            - 0
            - 0
            - 0
    witnesses:
      - header:
          version: 1.0.0
          field_characteristic:
            - 101
          field_degree: 1
        short_witness:
          - - 3
            - 0
            - 0
            - 0
          - - 4
            - 0
            - 0
            - 0
          - - 0
            - 0
            - 0
            - 0
          - - 36
    relations:
      - header:
          version: 1.0.0
          field_characteristic:
            - 101
          field_degree: 1
        gate_mask: 13
        feat_mask: 28672
        functions:
          - name: "com.example::mul"
            output_count: 1
            input_count: 2
            instance_count: 0
            witness_count: 0
            body:
              - Mul:
                  - 0
                  - 1
                  - 2
        gates:
          - Witness: 1
          - Switch:
              - 1
              - - Wire: 0
                - Wire: 2
                - WireRange:
                    - 4
                    - 6
                - WireRange:
                    - 9
                    - 11
              - - - 3
                - - 5
              - - AbstractAnonCall:
                    - - Wire: 1
                    - 3
                    - 3
                    - - Instance: 0
                      - Witness: 1
                      - Call:
                          - "com.example::mul"
                          - - Wire: 2
                          - - Wire: 8
                            - Wire: 8
                      - Call:
                          - "com.example::mul"
                          - - Wire: 3
                          - - Wire: 1
                            - Wire: 1
                      - Add:
                          - 4
                          - 2
                          - 3
                      - Witness: 9
                      - AssertZero: 9
                      - Instance: 6
                      - AssertZero: 6
                      - Instance: 7
                      - Witness: 5
                - AbstractAnonCall:
                    - - Wire: 1
                    - 3
                    - 2
                    - - Instance: 0
                      - Call:
                          - "com.example::mul"
                          - - Wire: 1
                          - - Wire: 8
                            - Wire: 0
                      - Witness: 2
                      - Mul:
                          - 3
                          - 1
                          - 2
                      - Add:
                          - 4
                          - 2
                          - 3
                      - Instance: 5
                      - Instance: 6
                      - Witness: 7
                      - AssertZero: 5
                      - AssertZero: 0
          - Constant:
              - 3
              - - 100
          - Call:
              - "com.example::mul"
              - - Wire: 7
              - - Wire: 3
                - Wire: 0
          - Add:
              - 8
              - 6
              - 7
          - Free:
              - 0
              - 7
          - AssertZero: 8
          - For:
              - i
              - 0
              - 20
              - - WireRange:
                    - 12
                    - 32
              - IterExprAnonCall:
                  - - Single:
                        IterExprAdd:
                          - IterExprName: i
                          - IterExprConst: 12
                  - - Single:
                        IterExprAdd:
                          - IterExprName: i
                          - IterExprConst: 10
                    - Single:
                        IterExprAdd:
                          - IterExprName: i
                          - IterExprConst: 11
                  - 0
                  - 0
                  - - Add:
                        - 0
                        - 1
                        - 2
          - MulConstant:
              - 33
              - 32
              - - 100
          - Add:
              - 34
              - 9
              - 33
          - AssertZero: 34
          - For:
              - i
              - 35
              - 50
              - - WireRange:
                    - 35
                    - 50
              - IterExprCall:
                  - "com.example::mul"
                  - - Single:
                        IterExprName: i
                  - - Single:
                        IterExprSub:
                          - IterExprName: i
                          - IterExprConst: 1
                    - Single:
                        IterExprSub:
                          - IterExprName: i
                          - IterExprConst: 2
          - Free:
              - 8
              - 50
    



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
an equivalent circuit that make use of only simple gates (i.e. `GateFor`/`GateSwitch`
`Functions` are unrolled/multiplexed/inlined). This operation (conversion from IR to
IR simple) is sometimes called 'flattening'. 

Here is a simple code to call this converter:

```rust
let relation = example_relation();
let instance = example_instance();
let witness = example_witness();

let mut flattener = IRFlattener::new(MemorySink::default());
let mut evaluator = Evaluator::default();

evaluator.ingest_instance(&instance)?;
evaluator.ingest_witness(&witness)?;
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
