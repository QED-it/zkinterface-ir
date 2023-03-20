use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::error::Error;

use crate::sieve_ir_generated::sieve_ir as generated;
use flatbuffers::{FlatBufferBuilder, ForwardsUOffset, Vector, WIPOffset};

use crate::structs::count::Count;
use crate::structs::plugin::PluginBody;
use crate::{Gate, Result, TypeId};

// ******************************
//
//   FunctionBody
//
// ******************************
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum FunctionBody {
    Gates(Vec<Gate>),
    PluginBody(PluginBody),
}

// ******************************
//
//   Function
//
// ******************************
/// This structure handles the declaration of a function.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Function {
    pub name: String,
    pub output_count: Vec<Count>,
    pub input_count: Vec<Count>,
    pub body: FunctionBody,
}

/// This function imports a FBS binary Function declaration into a Rust equivalent.
impl<'a> TryFrom<generated::Function<'a>> for Function {
    type Error = Box<dyn Error>;

    fn try_from(g_function: generated::Function) -> Result<Function> {
        let output_count = Count::try_from_vector(
            g_function
                .output_count()
                .ok_or("Missing output_count in Function")?,
        )?;
        let input_count = Count::try_from_vector(
            g_function
                .input_count()
                .ok_or("Missing input_count in Function")?,
        )?;

        let g_body_type = g_function.body_type();
        let body = match g_body_type {
            generated::FunctionBody::NONE => return Err("Unknown type in FunctionBody".into()),
            generated::FunctionBody::Gates => {
                let g_gates = g_function.body_as_gates().unwrap();
                FunctionBody::Gates(Gate::try_from_vector(
                    g_gates.gates().ok_or("Missing gates")?,
                )?)
            }
            generated::FunctionBody::PluginBody => {
                let plugin_body = g_function.body_as_plugin_body().unwrap();
                FunctionBody::PluginBody(PluginBody::try_from(plugin_body)?)
            }
        };

        Ok(Function {
            name: g_function.name().ok_or("Missing name")?.to_string(),
            output_count,
            input_count,
            body,
        })
    }
}

impl Function {
    /// Default constructor
    pub fn new(
        name: String,
        output_count: Vec<Count>,
        input_count: Vec<Count>,
        body: FunctionBody,
    ) -> Self {
        Function {
            name,
            output_count,
            input_count,
            body,
        }
    }

    /// Serialize this structure into a Flatbuffer message
    pub fn build<'a>(
        &self,
        builder: &mut FlatBufferBuilder<'a>,
    ) -> WIPOffset<generated::Function<'a>> {
        let g_name = builder.create_string(&self.name);
        let g_output_count = Count::build_vector(builder, &self.output_count);
        let g_input_count = Count::build_vector(builder, &self.input_count);
        let (g_body_type, g_body) = match &self.body {
            FunctionBody::Gates(gates) => {
                let g_gates_vector = Gate::build_vector(builder, gates);
                let g_gates = generated::Gates::create(
                    builder,
                    &generated::GatesArgs {
                        gates: Some(g_gates_vector),
                    },
                );
                let g_body = g_gates.as_union_value();
                (generated::FunctionBody::Gates, g_body)
            }
            FunctionBody::PluginBody(plugin_body) => {
                let g_plugin_body = plugin_body.build(builder);
                let g_body = g_plugin_body.as_union_value();
                (generated::FunctionBody::PluginBody, g_body)
            }
        };

        generated::Function::create(
            builder,
            &generated::FunctionArgs {
                name: Some(g_name),
                output_count: Some(g_output_count),
                input_count: Some(g_input_count),
                body_type: g_body_type,
                body: Some(g_body),
            },
        )
    }

    /// Import a vector of binary Functions into a Rust vector of Function declarations.
    pub fn try_from_vector<'a>(
        g_vector: Vector<'a, ForwardsUOffset<generated::Function<'a>>>,
    ) -> Result<Vec<Function>> {
        let mut functions = vec![];
        for i in 0..g_vector.len() {
            let g_a = g_vector.get(i);
            functions.push(Function::try_from(g_a)?);
        }
        Ok(functions)
    }

    /// Build a vector a Rust Functions into the associated FBS structure.
    pub fn build_vector<'a>(
        builder: &mut FlatBufferBuilder<'a>,
        functions: &[Function],
    ) -> WIPOffset<Vector<'a, ForwardsUOffset<generated::Function<'a>>>> {
        let g_functions: Vec<_> = functions.iter().map(|gate| gate.build(builder)).collect();
        builder.create_vector(&g_functions)
    }
}

/// FunctionCounts contains the number of inputs, outputs, public/private inputs of a function.
#[derive(Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct FunctionCounts {
    pub input_count: Vec<Count>,
    pub output_count: Vec<Count>,
    pub public_count: BTreeMap<TypeId, u64>,
    pub private_count: BTreeMap<TypeId, u64>,
}

impl FunctionCounts {
    /// This function returns the FunctionCounts of the function with name `name`.
    /// If no function with name `name` belongs to the BTreeMap `known_functions`, then it returns an error.
    pub fn get_function_counts(
        known_functions: &BTreeMap<String, Self>,
        name: &str,
    ) -> Result<Self> {
        match known_functions.get(name) {
            None => Err(format!("Function {} does not exist !", name).into()),
            Some(v) => Ok(v.clone()),
        }
    }

    pub fn check(
        &self,
        name: &str,
        input_count: Option<Vec<Count>>,
        output_count: Option<Vec<Count>>,
        public_count: Option<BTreeMap<TypeId, u64>>,
        private_count: Option<BTreeMap<TypeId, u64>>,
    ) -> Result<()> {
        if let Some(count) = input_count {
            if count != self.input_count {
                return Err(format!(
                    "Function {} has {:?} inputs and is called with {:?} inputs.",
                    name, self.input_count, count
                )
                .into());
            }
        }
        if let Some(count) = output_count {
            if count != self.output_count {
                return Err(format!(
                    "Function {} has {:?} outputs and is called with {:?} outputs.",
                    name, self.output_count, count
                )
                .into());
            }
        }
        if let Some(count) = public_count {
            if count != self.public_count {
                return Err(format!(
                    "Function {} has {:?} public inputs and is called with {:?} public inputs.",
                    name, self.public_count, count
                )
                .into());
            }
        }
        if let Some(count) = private_count {
            if count != self.private_count {
                return Err(format!(
                    "Function {} has {:?} private inputs and is called with {:?} private inputs.",
                    name, self.private_count, count
                )
                .into());
            }
        }
        Ok(())
    }
}
