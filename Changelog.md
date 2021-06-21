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
