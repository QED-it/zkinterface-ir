# Unreleased

Schema (.fbs):
- remove
  - gate_set and features from Relation struct
  - Boolean gates (and, xor, not)
  - AnonCall gate
  - Switch anf For loop
  - instance/witness counts from custom function signature
  - FieldId and use direcltly uint64
  - Header table and move its content into Relation, PublicInputs, PrivateInputs
  - field_id from WireRange
  - WireList and use directly [WireRange]
- rename
  - Instance/Wirness by Public/Private
  - Free gate by Delete gate
  - FieldId by TypeId
  - some field names in gates
- replace
  - field_id by tyep_id which refers to either a Field or a PluginType
  - table by struct for WireRange
- create
  - Count struct to store pairs (type_id, count)
    (Count is used in Conversion declaration, PluginBody, Function declaration)
  - Conversion struct to store conversion declaration
  - Directive table to store either a Gate or a Function declaration
  - Type table to store either a Field or a PluginType
  - PluginType table and PluginBody table to store a Plugin type and a Plugin function parameters
- add
  - conversions and plugins names list in Relation to declared used conversions and plugins
  - types list in Relation to declared used types (a type is either a Field or a PluginType)
  - type_id (ubyte) into standard gates
  - Convert and New gates
  - plugin function (now a Function declaration is either a custom function or a Plugin function)
  - type into Public/PrivateInputs (we have now one Public/PrivateInputs Message per type)
- merge functions and directives into directives
  (now gates and functions declarations are intermingled into a list of directives)

Rust:
- remove ExpandDefinable
- Structures, Builder, Validator and Evaluator adapted to the new schema.
- add some custom plugins (prefixed by zkif)
- update validator with all new validity checks (especially on memory management)
- update examples in src/examples directory
- update README
- refactor
  - remove get and set macros in Evaluator

# SIEVE IR v1.0.1, 2022-08

Rust:
- gateset and features are now mandatory when creating a GateBuilder with new method (breaking change)
- add some checks for the validator to be in accordance with IR v1.0.1

# v3.0.0, 2022-04

Schema (.fbs):
- Add schema compilation in Makefile (`make fbs`)

Rust:
- Boolean example (`zki_sieve bool-example [--incorrect]`)
- Move to_r1cs converter from producers to consumers
- Add function (sub-circuit) into the builder with a FunctionBuilder
- Format the entire project with rustfmt
- Add SwitchBuilder
- When declaring a function, limit the use of Copy gates
- Add wirelist macro

# v2.0.0, 2021-12

Rust:
- Brand New API to evaluate SIEVE IR circuits. The circuit is now evaluated using a VM, each gate evaluation is 
delegated to a class implementing the `ZKBackend` trait.
- IR-to-IRsimple converter
- IR-to-R1CS converter
- R1CS-to-IR converter


# v1.0.0, 2021-08

Schema (.fbs):
- Switch / For / Functions statements.
- Iterator Expressions
- List and Range of wires

Rust:
- Structures, Validator and Evaluator adapted to the new schema.


# v0.2.0, 2021-04

Schema (.fbs):
- Directives GateInstance and GateWitness (@instance and @witness).
- Witness and instance values as ordered streams.
- GateFree (@free(wire)).

Rust:
- Structures, Validator and Evaluator adapted to the new schema.
- Improved Builder and Sink API.

# v0.1.1, 2021-03

Rust:
- Configurable field order (`zki_sieve example --field-order=101`)
- Example with an incorrect witness (`zki_sieve example --incorrect`)
- Rename library to zki_sieve

# v0.1.0, 2020-09
