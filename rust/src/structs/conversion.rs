use crate::Result;
use flatbuffers::{FlatBufferBuilder, Vector, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;

use crate::sieve_ir_generated::sieve_ir as generated;
use crate::structs::count::Count;

#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize, Hash)]
pub struct Conversion {
    pub output_count: Count,
    pub input_count: Count,
}

/// This function imports a FBS binary Conversion declaration into a Rust equivalent.
impl<'a> TryFrom<generated::Conversion> for Conversion {
    type Error = Box<dyn Error>;

    fn try_from(g_conversion: generated::Conversion) -> Result<Conversion> {
        Ok(Conversion {
            output_count: Count::try_from(*g_conversion.output_count())?,
            input_count: Count::try_from(*g_conversion.input_count())?,
        })
    }
}

impl Conversion {
    /// Default constructor
    pub fn new(output_count: Count, input_count: Count) -> Self {
        Conversion {
            output_count,
            input_count,
        }
    }

    /// Serialize this structure into a Flatbuffer message
    pub fn build(&self) -> generated::Conversion {
        let g_output_count = self.output_count.build();
        let g_input_count = self.input_count.build();
        generated::Conversion::new(&g_output_count, &g_input_count)
    }

    /// Import a vector of binary Conversion into a Rust vector of Conversion declarations.
    pub fn try_from_vector(g_conversions: &[generated::Conversion]) -> Result<Vec<Conversion>> {
        g_conversions
            .iter()
            .map(|conversion| Conversion::try_from(*conversion))
            .collect()
    }

    /// Build a vector a Rust Conversion into the associated FBS structure.
    pub fn build_vector<'a>(
        builder: &mut FlatBufferBuilder<'a>,
        conversions: &[Conversion],
    ) -> WIPOffset<Vector<'a, generated::Conversion>> {
        let g_conversions = conversions
            .iter()
            .map(|conversion| conversion.build())
            .collect::<Vec<_>>();
        builder.create_vector(&g_conversions)
    }
}
