use crate::Result;
use flatbuffers::{FlatBufferBuilder, ForwardsUOffset, Vector, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;

use crate::sieve_ir_generated::sieve_ir as generated;
use crate::structs::function::Function;
use crate::structs::gates::Gate;

#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum Directive {
    Function(Function),
    Gate(Gate),
}

impl<'a> TryFrom<generated::Directive<'a>> for Directive {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_directive: generated::Directive) -> Result<Directive> {
        let directive = match g_directive.directive_type() {
            generated::DirectiveSet::NONE => return Err("Unknown directive".into()),
            generated::DirectiveSet::Function => {
                let function = Function::try_from(g_directive.directive_as_function().unwrap())?;
                Directive::Function(function)
            }
            generated::DirectiveSet::Gate => {
                let gate = Gate::try_from(g_directive.directive_as_gate().unwrap())?;
                Directive::Gate(gate)
            }
        };
        Ok(directive)
    }
}

impl Directive {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'a>(
        &self,
        builder: &mut FlatBufferBuilder<'a>,
    ) -> WIPOffset<generated::Directive<'a>> {
        match self {
            Directive::Function(function) => {
                let g_function = function.build(builder);
                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: generated::DirectiveSet::Function,
                        directive: Some(g_function.as_union_value()),
                    },
                )
            }
            Directive::Gate(gate) => {
                let g_gate = gate.build(builder);

                generated::Directive::create(
                    builder,
                    &generated::DirectiveArgs {
                        directive_type: generated::DirectiveSet::Gate,
                        directive: Some(g_gate.as_union_value()),
                    },
                )
            }
        }
    }

    /// Import a vector of binary Directives into a Rust vector of Directive declarations.
    pub fn try_from_vector<'a>(
        g_directives: Vector<'a, ForwardsUOffset<generated::Directive<'a>>>,
    ) -> Result<Vec<Directive>> {
        g_directives
            .iter()
            .map(Directive::try_from)
            .collect::<Result<Vec<_>>>()
    }

    /// Build a vector of Rust Directives into the associated FBS structure.
    pub fn build_vector<'a>(
        builder: &mut FlatBufferBuilder<'a>,
        directives: &[Directive],
    ) -> WIPOffset<Vector<'a, ForwardsUOffset<generated::Directive<'a>>>> {
        let g_directives: Vec<_> = directives
            .iter()
            .map(|directive| directive.build(builder))
            .collect();
        builder.create_vector(&g_directives)
    }
}
