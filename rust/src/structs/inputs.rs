use crate::Result;
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;

use crate::sieve_ir_generated::sieve_ir as generated;
use crate::structs::value::{build_values_vector, try_from_values_vector, Value};

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Inputs {
    pub values: Vec<Value>,
}

impl<'a> TryFrom<generated::Inputs<'a>> for Inputs {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_inputs: generated::Inputs) -> Result<Inputs> {
        Ok(Inputs {
            values: try_from_values_vector(g_inputs.values().ok_or("Missing values")?)?,
        })
    }
}

impl Inputs {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr>(
        &self,
        builder: &mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<generated::Inputs<'bldr>> {
        let values = Some(build_values_vector(builder, &self.values));

        generated::Inputs::create(builder, &generated::InputsArgs { values })
    }
}
