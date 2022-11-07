use num_bigint::BigUint;

use crate::plugins::vector;
use crate::structs::count::Count;
use crate::structs::plugin::PluginBody;
use crate::Result;

pub fn evaluate_plugin_for_plaintext_backend(
    output_count: &[Count],
    input_count: &[Count],
    inputs: &[&BigUint],
    plugin_body: &PluginBody,
    moduli: &[BigUint],
) -> Result<Vec<BigUint>> {
    match (plugin_body.name.as_str(), plugin_body.operation.as_str()) {
        ("vector", "add") => vector::vector_add(
            output_count,
            input_count,
            inputs,
            &plugin_body.params,
            moduli,
        ),
        ("vector", "mul") => vector::vector_mul(
            output_count,
            input_count,
            inputs,
            &plugin_body.params,
            moduli,
        ),
        _ => Err("Unknown plugin".into()),
    }
}
