use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;

use crate::sieve_ir_generated::sieve_ir as generated;
use flatbuffers::{FlatBufferBuilder, WIPOffset};

use crate::Result;

/// This structure handles the declaration of a PluginBody.
#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct PluginBody {
    pub name: String,
    pub operation: String,
    pub params: Vec<String>,
}

/// This function imports a FBS binary PluginBody declaration into a Rust equivalent.
impl<'a> TryFrom<generated::PluginBody<'a>> for PluginBody {
    type Error = Box<dyn Error>;

    fn try_from(g_plugin_body: generated::PluginBody) -> Result<PluginBody> {
        let g_params = g_plugin_body.params().ok_or("Missing plugin params")?;
        let params = g_params.iter().map(|param| param.to_string()).collect();
        Ok(PluginBody {
            name: g_plugin_body
                .name()
                .ok_or("Missing plugin name")?
                .to_string(),
            operation: g_plugin_body
                .operation()
                .ok_or("Missing plugin operation")?
                .to_string(),
            params,
        })
    }
}

impl PluginBody {
    /// Default constructor
    pub fn new(name: String, operation: String, params: Vec<String>) -> Self {
        PluginBody {
            name,
            operation,
            params,
        }
    }

    /// Serialize this structure into a Flatbuffer message
    pub fn build<'a>(
        &self,
        builder: &mut FlatBufferBuilder<'a>,
    ) -> WIPOffset<generated::PluginBody<'a>> {
        let g_name = builder.create_string(&self.name);
        let g_operation = builder.create_string(&self.operation);
        let g_params = self
            .params
            .iter()
            .map(|param| builder.create_string(param))
            .collect::<Vec<WIPOffset<&str>>>();
        let g_params_vec = builder.create_vector(&g_params);

        generated::PluginBody::create(
            builder,
            &generated::PluginBodyArgs {
                name: Some(g_name),
                operation: Some(g_operation),
                params: Some(g_params_vec),
            },
        )
    }
}
