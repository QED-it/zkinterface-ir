# Unreleased

Rust:
- add field switching (breaking change)
    - Header contains now a list of fields
    - Instances/witnesses contains now one list of values per field
    - Add field id into gates and WireListElement
    - Add convert gate
- remove ExpandDefinable
- remove gate_set and features from Relation struct
- remove Boolean gates (and, xor, not)
- remove Switch
- remove For loop
- rename Instance/Witness by PublicInputs/PrivateInputs
- rename Free gate by Delete gate
- add New gate
- add plugins
- rename FieldId by TypeId
- remove AnonCall gate
- in FBS, remove CountList table and use directly [Count]
- remove public/private counts from custom function signature
- add public/private counts into PluginBody
- in FBS, remove WireId and TypeId tables and use directly ubyte and uint64
- in FBS, remove Header table and move its content into Relation, PublicInputs, PrivateInputs
- in FBS, rename some field names in gates
- in FBS, replace WireList by [WireRange] and remove type_id from WireRange
- add conversion declarations in Relation
- split public/private inputs files into one file per type
- update conversion to be compliant with the last version of the specs

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
