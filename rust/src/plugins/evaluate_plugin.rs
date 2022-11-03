use num_bigint::BigUint;

use crate::plugins::vector;
use crate::structs::count::CountList;
use crate::structs::plugin::PluginBody;
use crate::Result;

pub fn evaluate_plugin_for_plaintext_backend(
    output_count: &CountList,
    input_count: &CountList,
    inputs: &[&BigUint],
    plugin_body: &PluginBody,
    fields: &[BigUint],
) -> Result<Vec<BigUint>> {
    match (plugin_body.name.as_str(), plugin_body.operation.as_str()) {
        ("vector", "add") => vector::vector_add(
            output_count,
            input_count,
            inputs,
            &plugin_body.params,
            fields,
        ),
        _ => Err("Unknown plugin".into()),
    }
}
