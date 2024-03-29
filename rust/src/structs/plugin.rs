use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::error::Error;

use crate::sieve_ir_generated::sieve_ir as generated;
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use itertools::Itertools;

use crate::structs::count::{count_list_to_hashmap, Count};
use crate::{Result, TypeId};

/// This structure handles the declaration of a PluginBody.
#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct PluginBody {
    pub name: String,
    pub operation: String,
    pub params: Vec<String>,
    pub public_count: BTreeMap<TypeId, u64>,
    pub private_count: BTreeMap<TypeId, u64>,
}

/// This function imports a FBS binary PluginBody declaration into a Rust equivalent.
impl<'a> TryFrom<generated::PluginBody<'a>> for PluginBody {
    type Error = Box<dyn Error>;

    fn try_from(g_plugin_body: generated::PluginBody) -> Result<PluginBody> {
        let g_params = g_plugin_body.params().ok_or("Missing plugin params")?;
        let params = g_params.iter().map(|param| param.to_string()).collect();

        let public_count = Count::try_from_vector(
            g_plugin_body
                .public_count()
                .ok_or("Missing public_count in PluginBody")?,
        )?;
        if !public_count.iter().map(|count| count.type_id).all_unique() {
            return Err("All type ids must be unique in a PluginBody::public_count".into());
        }
        let public_count_map = count_list_to_hashmap(&public_count);

        let private_count = Count::try_from_vector(
            g_plugin_body
                .private_count()
                .ok_or("Missing private_count in PluginBody")?,
        )?;
        if !private_count.iter().map(|count| count.type_id).all_unique() {
            return Err("All type ids must be unique in a PluginBody::private_count".into());
        }
        let private_count_map = count_list_to_hashmap(&private_count);

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
            public_count: public_count_map,
            private_count: private_count_map,
        })
    }
}

impl PluginBody {
    /// Default constructor
    pub fn new(
        name: String,
        operation: String,
        params: Vec<String>,
        public_count: BTreeMap<TypeId, u64>,
        private_count: BTreeMap<TypeId, u64>,
    ) -> Self {
        PluginBody {
            name,
            operation,
            params,
            public_count,
            private_count,
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
            .collect::<Vec<_>>();
        let g_params_vec = builder.create_vector(&g_params);
        let public_count = self
            .public_count
            .iter()
            .map(|(type_id, count)| Count::new(*type_id, *count))
            .collect::<Vec<_>>();
        let g_public_count = Count::build_vector(builder, &public_count);
        let private_count = self
            .private_count
            .iter()
            .map(|(type_id, count)| Count::new(*type_id, *count))
            .collect::<Vec<_>>();
        let g_private_count = Count::build_vector(builder, &private_count);

        generated::PluginBody::create(
            builder,
            &generated::PluginBodyArgs {
                name: Some(g_name),
                operation: Some(g_operation),
                params: Some(g_params_vec),
                public_count: Some(g_public_count),
                private_count: Some(g_private_count),
            },
        )
    }
}
