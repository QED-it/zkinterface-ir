use crate::Result;
use flatbuffers::{FlatBufferBuilder, ForwardsUOffset, Vector, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;

use crate::sieve_ir_generated::sieve_ir as generated;
use crate::structs::value::{build_value, remove_trailing_zeros, try_from_value, Value};

#[derive(Clone, Debug, Hash, Eq, PartialEq, Deserialize, Serialize)]
pub enum Type {
    Field(Value),
    // PluginType(name, operation, params)
    PluginType(String, String, Vec<String>),
}

impl<'a> TryFrom<generated::Type<'a>> for Type {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_type: generated::Type) -> Result<Type> {
        let type_ = match g_type.element_type() {
            generated::TypeU::NONE => return Err("Unknown type".into()),
            generated::TypeU::Field => {
                let value = try_from_value(
                    g_type
                        .element_as_field()
                        .unwrap()
                        .modulo()
                        .ok_or("Missing modulo in Filed")?,
                )?;
                Type::Field(value)
            }
            generated::TypeU::PluginType => {
                let g_plugin_type = g_type.element_as_plugin_type().unwrap();
                let name = g_plugin_type.name().ok_or("Missing name in PluginType.")?;
                let operation = g_plugin_type
                    .operation()
                    .ok_or("Missing operation in PluginType.")?;
                let g_params = g_plugin_type
                    .params()
                    .ok_or("Missing params in PluginType.")?;
                let params = g_params.iter().map(|param| param.to_string()).collect();
                Type::PluginType(name.to_string(), operation.to_string(), params)
            }
        };
        Ok(type_)
    }
}

impl Type {
    pub fn new_field_type(modulo: Value) -> Self {
        Type::Field(modulo)
    }

    pub fn new_plugin_type(name: String, operation: String, params: Vec<String>) -> Self {
        Type::PluginType(name, operation, params)
    }

    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'a>(&self, builder: &mut FlatBufferBuilder<'a>) -> WIPOffset<generated::Type<'a>> {
        match self {
            Type::Field(value) => {
                let g_value = build_value(builder, value);

                let g_field = generated::Field::create(
                    builder,
                    &generated::FieldArgs {
                        modulo: Some(g_value),
                    },
                );
                generated::Type::create(
                    builder,
                    &generated::TypeArgs {
                        element_type: generated::TypeU::Field,
                        element: Some(g_field.as_union_value()),
                    },
                )
            }
            Type::PluginType(name, operation, params) => {
                let g_name = builder.create_string(name);
                let g_operation = builder.create_string(operation);
                let g_params = params
                    .iter()
                    .map(|param| builder.create_string(param))
                    .collect::<Vec<_>>();
                let g_params_vec = builder.create_vector(&g_params);

                let g_plugin_type = generated::PluginType::create(
                    builder,
                    &generated::PluginTypeArgs {
                        name: Some(g_name),
                        operation: Some(g_operation),
                        params: Some(g_params_vec),
                    },
                );

                generated::Type::create(
                    builder,
                    &generated::TypeArgs {
                        element_type: generated::TypeU::PluginType,
                        element: Some(g_plugin_type.as_union_value()),
                    },
                )
            }
        }
    }

    /// Import a vector of binary Type into a Rust vector of Type declarations.
    pub fn try_from_vector<'a>(
        g_vector: Vector<'a, ForwardsUOffset<generated::Type<'a>>>,
    ) -> Result<Vec<Type>> {
        let mut types = vec![];
        for i in 0..g_vector.len() {
            let g_a = g_vector.get(i);
            types.push(Type::try_from(g_a)?);
        }
        Ok(types)
    }

    /// Build a vector of a Rust Types into the associated FBS structure.
    pub fn build_vector<'a>(
        builder: &mut FlatBufferBuilder<'a>,
        types: &[Type],
    ) -> WIPOffset<Vector<'a, ForwardsUOffset<generated::Type<'a>>>> {
        let g_types: Vec<_> = types.iter().map(|type_| type_.build(builder)).collect();
        builder.create_vector(&g_types)
    }

    /// This function removes trailing zeros to `Type::Field`
    pub fn cleaned_type(&self) -> Self {
        match self {
            Type::Field(modulo) => {
                let cleaned_modulo = remove_trailing_zeros(modulo);
                Type::Field(cleaned_modulo)
            }
            Type::PluginType(_, _, _) => self.clone(),
        }
    }
}
