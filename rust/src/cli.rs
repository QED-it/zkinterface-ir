extern crate serde;
extern crate serde_json;

use std::io::{stdout, copy};
use std::path::{Path, PathBuf};
use std::fs::File;
use structopt::StructOpt;
use num_bigint::BigUint;

use crate::{Messages, Result, Source};
use crate::consumers::{
    evaluator::Evaluator,
    validator::Validator,
    stats::Stats,
    source::{list_workspace_files, has_sieve_extension},
};

const ABOUT: &str = "
This is a collection of tools to work with zero-knowledge statements encoded in SIEVE IR messages.

The tools below work within a workspace directory given after the tool name (`workspace` in the examples below), or in the current working directory by default. To read from stdin or write to stdout, pass a dash - instead of a filename.

Create an example statement:
    zki_sieve example workspace

Print a statement in different forms:
    zki_sieve to-text workspace
    zki_sieve to-json workspace
    zki_sieve to-yaml workspace

Validate and evaluate a proving system:
    zki_sieve valid-eval-metrics workspace

";

use structopt::clap::AppSettings::*;

#[derive(Debug, StructOpt)]
#[structopt(
name = "zki_sieve",
about = "zkInterface toolbox for SIEVE IR.",
long_about = ABOUT,
setting(DontCollapseArgsInUsage),
setting(ColoredHelp)
)]
pub struct Options {
    /// Which tool to run.
    ///
    /// example       Produce example statements.
    ///
    /// to-text       Print the content in a human-readable form.
    ///
    /// to-json       Convert to JSON on a single line.
    ///
    /// to-yaml       Convert to YAML.
    ///
    /// validate      Validate the format and semantics of a statement, as seen by a verifier.
    ///
    /// evaluate      Evaluate a circuit as prover to check that the statement is true, i.e. the witness satisfies the circuit.
    ///
    /// metrics       Calculate statistics about the circuit.
    ///
    /// valid-eval-metrics    Combined validate, evaluate, and metrics.
    ///
    /// list-validations    Lists all the checks performed by the validator.
    ///
    /// cat           Concatenate .sieve files to stdout to pipe to another program.
    #[structopt(default_value = "help")]
    tool: String,

    /// The tools work in a workspace directory containing .sieve files.
    ///
    /// Alternatively, a list of .sieve files can be provided explicitly.
    ///
    /// The dash - means either write to stdout or read from stdin.
    #[structopt(default_value = ".")]
    paths: Vec<PathBuf>,

    /// Which field to use when generating circuits.
    #[structopt(short, long, default_value = "101")]
    pub field_order: BigUint,

    /// `example --incorect` will generate an incorrect witness useful for negative tests.
    #[structopt(long)]
    pub incorrect: bool,
}

pub fn cli(options: &Options) -> Result<()> {
    match &options.tool[..] {
        "example" => main_example(options),
        "to-text" => main_text(&load_messages(options)?),
        "to-json" => main_json(&load_messages(options)?),
        "to-yaml" => main_yaml(&load_messages(options)?),
        "validate" => main_validate(&stream_messages(options)?),
        "evaluate" => main_evaluate(&stream_messages(options)?),
        "metrics" => main_metrics(&stream_messages(options)?),
        "valid-eval-metrics" => main_valid_eval_metrics(&stream_messages(options)?),
        "list-validations" => main_list_validations(),
        "cat" => main_cat(options),
        "simulate" => Err("`simulate` was renamed to `evaluate`".into()),
        "stats" => Err("`stats` was renamed to `metrics`".into()),
        "help" => {
            Options::clap().print_long_help()?;
            eprintln!("\n");
            Ok(())
        }
        _ => {
            Options::clap().print_long_help()?;
            eprintln!("\n");
            Err(format!("Unknown command {}", &options.tool).into())
        }
    }
}


fn load_messages(opts: &Options) -> Result<Messages> {
    stream_messages(opts)?.read_all_messages()
}

fn stream_messages(opts: &Options) -> Result<Source> {
    let mut source = Source::from_dirs_and_files(&opts.paths)?;
    source.print_filenames = true;
    Ok(source)
}


fn main_example(opts: &Options) -> Result<()> {
    use crate::producers::examples::*;
    use crate::{Sink, FilesSink};

    let header = example_header_in_field(opts.field_order.to_bytes_le());
    let instance = example_instance_h(&header);
    let relation = example_relation_h(&header);
    let witness = if opts.incorrect {
        example_witness_incorrect_h(&header)
    } else {
        example_witness_h(&header)
    };

    if opts.paths.len() != 1 {
        return Err("Specify a single directory where to write examples.".into());
    }
    let out_dir = &opts.paths[0];

    if out_dir == Path::new("-") {
        instance.write_into(&mut stdout())?;
        witness.write_into(&mut stdout())?;
        relation.write_into(&mut stdout())?;
    } else if has_sieve_extension(out_dir) {
        let mut file = File::create(out_dir)?;
        instance.write_into(&mut file)?;
        witness.write_into(&mut file)?;
        relation.write_into(&mut file)?;
        eprintln!("Written Instance, Witness, and Relation into {}", out_dir.display());
    } else {
        let mut sink = FilesSink::new(out_dir)?;
        sink.print_filenames = true;
        sink.push_instance(&instance)?;
        sink.push_witness(&witness)?;
        sink.push_relation(&relation)?;
    }
    Ok(())
}

fn main_cat(opts: &Options) -> Result<()> {
    for path in list_workspace_files(&opts.paths)? {
        let mut file = File::open(&path)?;
        let mut stdout = stdout();
        copy(&mut file, &mut stdout)?;
    }
    Ok(())
}

fn main_text(_messages: &Messages) -> Result<()> {
    Err("Text form is not implemented yet.".into())
}

fn main_json(messages: &Messages) -> Result<()> {
    serde_json::to_writer(stdout(), messages)?;
    println!();
    Ok(())
}

fn main_yaml(messages: &Messages) -> Result<()> {
    serde_yaml::to_writer(stdout(), messages)?;
    println!();
    Ok(())
}

fn main_list_validations() -> Result<()> {
    Validator::print_implemented_checks();
    Ok(())
}

fn main_validate(source: &Source) -> Result<()> {
    // Validate semantics as verifier.
    let mut validator = Validator::new_as_prover();
    for msg in source.iter_messages() {
        validator.ingest_message(&msg?);
    }
    print_violations(&validator.get_violations(), "COMPLIANT with the specification")
}

fn main_evaluate(source: &Source) -> Result<()> {
    // Validate semantics as verifier.
    let mut evaluator = Evaluator::default();
    for msg in source.iter_messages() {
        evaluator.ingest_message(&msg?);
    }
    print_violations(&evaluator.get_violations(), "TRUE")
}

fn main_metrics(source: &Source) -> Result<()> {
    let mut stats = Stats::default();
    for msg in source.iter_messages() {
        stats.ingest_message(&msg?);
    }
    serde_json::to_writer_pretty(stdout(), &stats)?;
    println!();
    Ok(())
}

/// Joint validate, evaluate, and metrics.
fn main_valid_eval_metrics(source: &Source) -> Result<()> {
    // Validate semantics as prover.
    let mut validator = Validator::new_as_prover();
    // Check whether the statement is true.
    let mut evaluator = Evaluator::default();
    // Measure metrics on the circuit.
    let mut stats = Stats::default();

    // Feed messages to all consumers (read files or stdin only once).
    for msg in source.iter_messages() {
        let msg = msg?;
        validator.ingest_message(&msg);
        evaluator.ingest_message(&msg);
        stats.ingest_message(&msg);
    }

    let res1 = print_violations(&validator.get_violations(), "COMPLIANT with the specification");
    let res2 = print_violations(&evaluator.get_violations(), "TRUE");
    let res3 = serde_json::to_writer_pretty(stdout(), &stats);
    println!();

    res1?;
    res2?;
    res3?;
    Ok(())
}

fn print_violations(errors: &[String], what_it_is_supposed_to_be: &str) -> Result<()> {
    eprintln!();
    if errors.len() > 0 {
        eprintln!("The statement is NOT {}!", what_it_is_supposed_to_be);
        eprintln!("Violations:\n- {}\n", errors.join("\n- "));
        Err(format!("Found {} violations.", errors.len()).into())
    } else {
        eprintln!("The statement is {}!", what_it_is_supposed_to_be);
        Ok(())
    }
}


#[test]
fn test_cli() -> Result<()> {
    use std::fs::remove_dir_all;

    let workspace = PathBuf::from("local/test_cli");
    let _ = remove_dir_all(&workspace);

    cli(&Options {
        tool: "example".to_string(),
        paths: vec![workspace.clone()],
        field_order: BigUint::from(101 as u32),
        incorrect: false,
    })?;

    cli(&Options {
        tool: "valid-eval-metrics".to_string(),
        paths: vec![workspace.clone()],
        field_order: BigUint::from(101 as u32),
        incorrect: false,
    })?;

    Ok(())
}
