use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;

use crate::sieve_ir_generated::sieve_ir as generated;
use flatbuffers::{FlatBufferBuilder, ForwardsUOffset, Vector, WIPOffset};

use crate::structs::count::{build_count_list, CountList};
use crate::{Gate, Result};

// ******************************
//
//   Functions declaration
//    (used everywhere)
// ******************************

/// This structure handles the declaration of a function.
#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Function {
    pub name: String,
    pub output_count: CountList,
    pub input_count: CountList,
    pub public_count: CountList,
    pub private_count: CountList,
    pub body: Vec<Gate>,
}

/// This function imports a FBS binary Function declaration into a Rust equivalent.
impl<'a> TryFrom<generated::Function<'a>> for Function {
    type Error = Box<dyn Error>;

    fn try_from(g_function: generated::Function) -> Result<Function> {
        let g_directives = g_function
            .body()
            .ok_or("Missing reference implementation")?;
        let output_count = CountList::try_from(
            g_function
                .output_count()
                .ok_or("Missing output_count in Function")?,
        )?;
        let input_count = CountList::try_from(
            g_function
                .input_count()
                .ok_or("Missing input_count in Function")?,
        )?;
        let public_count = CountList::try_from(
            g_function
                .public_count()
                .ok_or("Missing public_count in Function")?,
        )?;
        let private_count = CountList::try_from(
            g_function
                .private_count()
                .ok_or("Missing private_count in Function")?,
        )?;

        Ok(Function {
            name: g_function.name().ok_or("Missing name")?.to_string(),
            output_count,
            input_count,
            public_count,
            private_count,
            body: Gate::try_from_vector(g_directives)?,
        })
    }
}

impl Function {
    /// Default constructor
    pub fn new(
        name: String,
        output_count: CountList,
        input_count: CountList,
        public_count: CountList,
        private_count: CountList,
        body: Vec<Gate>,
    ) -> Self {
        Function {
            name,
            output_count,
            input_count,
            public_count,
            private_count,
            body,
        }
    }

    /// Serialize this structure into a Flatbuffer message
    pub fn build<'bldr>(
        &self,
        builder: &mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<generated::Function<'bldr>> {
        let g_name = builder.create_string(&self.name);
        let g_body = Gate::build_vector(builder, &self.body);
        let g_output_count = build_count_list(builder, &self.output_count);
        let g_input_count = build_count_list(builder, &self.input_count);
        let g_public_count = build_count_list(builder, &self.public_count);
        let g_private_count = build_count_list(builder, &self.private_count);

        generated::Function::create(
            builder,
            &generated::FunctionArgs {
                name: Some(g_name),
                output_count: Some(g_output_count),
                input_count: Some(g_input_count),
                public_count: Some(g_public_count),
                private_count: Some(g_private_count),
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
    pub fn build_vector<'bldr>(
        builder: &mut FlatBufferBuilder<'bldr>,
        functions: &[Function],
    ) -> WIPOffset<Vector<'bldr, ForwardsUOffset<generated::Function<'bldr>>>> {
        let g_functions: Vec<_> = functions.iter().map(|gate| gate.build(builder)).collect();
        builder.create_vector(&g_functions)
    }
}
