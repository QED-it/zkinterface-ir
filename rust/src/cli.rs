extern crate serde;
extern crate serde_json;

use std::io::{stdin, stdout, copy};
use std::path::{Path, PathBuf};
use std::fs::File;
use structopt::StructOpt;
use std::convert::TryFrom;

use crate::{Reader, Messages, Result, Workspace};
use crate::consumers::{
    evaluator::Evaluator,
    validator::Validator,
    stats::Stats,
    workspace::{list_workspace_files, has_sieve_extension},
};

const ABOUT: &str = "
This is a collection of tools to work with zero-knowledge statements encoded in SIEVE IR messages.

The tools below work within a workspace directory given after the tool name (`workspace` in the examples below), or in the current working directory by default. To read from stdin or write to stdout, pass a dash - instead of a filename.

Create an example statement:
    sieve_ir example workspace
Or:
    sieve_ir example - > workspace/example.sieve

Print a statement in different forms:
    sieve_ir to-json workspace
    sieve_ir to-yaml workspace
    sieve_ir explain workspace

Simulate a proving system:
    sieve_ir stats       workspace
    sieve_ir validate    workspace
    sieve_ir evaluate    workspace
    sieve_ir fake_prove  workspace
    sieve_ir fake_verify workspace

Write all the statement files to stdout (to pipe to another program):
    sieve_ir cat workspace

";

use structopt::clap::AppSettings::*;

#[derive(Debug, StructOpt)]
#[structopt(
name = "sieve_ir",
about = "SIEVE IR toolbox.",
long_about = ABOUT,
setting(DontCollapseArgsInUsage),
setting(ColoredHelp)
)]
pub struct Options {
    /// Which tool to run.
    ///
    /// example       Create example statements.
    ///
    /// cat           Write .sieve files to stdout.
    ///
    /// to-json       Convert to JSON on a single line.
    ///
    /// to-yaml       Convert to YAML.
    ///
    /// to-text       Print the content in a human-readable form.
    ///
    /// validate      Validate the format and semantics of a statement, as seen by a verifier.
    /// 
    /// list-checks   Lists all the checks performed by the validator.
    ///
    /// evaluate      Simulate a proving system as prover by verifying that the statement is true.
    ///
    /// stats         Calculate statistics about the circuit.
    #[structopt(default_value = "help")]
    tool: String,

    /// The tools work in a workspace directory containing .sieve files.
    ///
    /// Alternatively, a list of .sieve files can be provided explicitly.
    ///
    /// The dash - means either write to stdout or read from stdin.
    #[structopt(default_value = ".")]
    paths: Vec<PathBuf>,
}

pub fn cli(options: &Options) -> Result<()> {
    match &options.tool[..] {
        "example" => main_example(options),
        "to-text" => main_text(&load_messages(options)?),
        "to-json" => main_json(&load_messages(options)?),
        "to-yaml" => main_yaml(&load_messages(options)?),
        "list-checks" => main_list_check(),
        "validate" => main_validate(&stream_messages(options)?),
        "evaluate" => main_evaluate(&stream_messages(options)?),
        "simulate" => Err("`simulate` was renamed to `evaluate`".into()),
        "stats" => main_stats(&load_messages(options)?),
        "cat" => main_cat(options),
        "fake_prove" => main_fake_prove(&load_messages(options)?),
        "fake_verify" => main_fake_verify(&load_messages(options)?),
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


fn load_messages(opts: &Options) -> Result<Reader> {
    let mut reader = Reader::default();

    for path in list_workspace_files(&opts.paths)? {
        if path == Path::new("-") {
            eprintln!("Loading from stdin");
            reader.read_from(&mut stdin())?;
        } else {
            eprintln!("Loading file {}", path.display());
            reader.read_file(path)?;
        }
    }
    eprintln!();

    Ok(reader)
}

fn stream_messages(opts: &Options) -> Result<Workspace> {
    Workspace::from_dirs_and_files(&opts.paths)
}

fn main_example(opts: &Options) -> Result<()> {
    use crate::producers::examples::*;
    use crate::{Sink, WorkspaceSink};

    if opts.paths.len() != 1 {
        return Err("Specify a single directory where to write examples.".into());
    }
    let out_dir = &opts.paths[0];

    if out_dir == Path::new("-") {
        example_instance().write_into(&mut stdout())?;
        example_witness().write_into(&mut stdout())?;
        example_relation().write_into(&mut stdout())?;
    } else if has_sieve_extension(out_dir) {
        let mut file = File::create(out_dir)?;
        example_instance().write_into(&mut file)?;
        example_witness().write_into(&mut file)?;
        example_relation().write_into(&mut file)?;
        eprintln!("Written Instance, Witness, and Relation into {}", out_dir.display());
    } else {
        let mut sink = WorkspaceSink::new(out_dir)?;
        sink.print_filenames = true;
        sink.push_instance(&example_instance())?;
        sink.push_witness(&example_witness())?;
        sink.push_relation(&example_relation())?;
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

fn main_text(_reader: &Reader) -> Result<()> {
    Err("Text form is not implemented yet.".into())
}

fn main_json(reader: &Reader) -> Result<()> {
    let messages = Messages::try_from(reader)?;
    serde_json::to_writer(stdout(), &messages)?;
    println!();
    Ok(())
}

fn main_yaml(reader: &Reader) -> Result<()> {
    let messages = Messages::try_from(reader)?;
    serde_yaml::to_writer(stdout(), &messages)?;
    println!();
    Ok(())
}

fn main_list_check() -> Result<()> {
    Validator::print_implemented_checks();
    Ok(())
}

fn main_validate(ws: &Workspace) -> Result<()> {
    // Validate semantics as verifier.
    let mut validator = Validator::new_as_verifier();
    for msg in ws.iter_messages() {
        validator.ingest_message(&msg?);
    }
    print_violations(&validator.get_violations(), "COMPLIANT with the specification")
}

fn main_evaluate(ws: &Workspace) -> Result<()> {
    // Validate semantics as prover.
    let mut validator = Validator::new_as_prover();
    // Check whether the statement is true.
    let mut evaluator = Evaluator::default();

    // Must validate and evaluate in parallel to support stdin.
    for msg in ws.iter_messages() {
        let msg = msg?;
        validator.ingest_message(&msg);
        evaluator.ingest_message(&msg);
    }

    let result_val = print_violations(&validator.get_violations(), "COMPLIANT with the specification");
    print_violations(&evaluator.get_violations(), "TRUE")?;
    result_val
}

fn print_violations(errors: &[String], what_it_is_supposed_to_be: &str) -> Result<()> {
    if errors.len() > 0 {
        eprintln!("The statement is NOT {}!", what_it_is_supposed_to_be);
        eprintln!("Violations:\n- {}\n", errors.join("\n- "));
        Err(format!("Found {} violations.", errors.len()).into())
    } else {
        eprintln!("The statement is {}!", what_it_is_supposed_to_be);
        Ok(())
    }
}

fn main_stats(reader: &Reader) -> Result<()> {
    let messages = Messages::try_from(reader)?;
    let mut stats = Stats::new();
    stats.ingest_messages(&messages);
    serde_json::to_writer_pretty(stdout(), &stats)?;
    println!();
    Ok(())
}


fn main_fake_prove(_: &Reader) -> Result<()> {
    unimplemented!();
    /*
    let mut file = File::create("fake_proof")?;
    // TODO: write witness as fake proof.
    eprintln!("Fake proof written to file `fake_proof`.");
    Ok(())
     */
}

fn main_fake_verify(_: &Reader) -> Result<()> {
    unimplemented!();
    /*
    let mut file = File::open("fake_proof")?;
    let mut proof = String::new();
    file.read_to_string(&mut proof)?;
    // TODO: Simulate with the witness given in the fake proof.
    eprintln!("Fake proof verified!");
    Ok(())
     */
}

#[test]
fn test_cli() -> Result<()> {
    use std::fs::remove_dir_all;

    let workspace = PathBuf::from("local/test_cli");
    let _ = remove_dir_all(&workspace);

    cli(&Options {
        tool: "example".to_string(),
        paths: vec![workspace.clone()],
    })?;

    cli(&Options {
        tool: "evaluate".to_string(),
        paths: vec![workspace.clone()],
    })?;

    Ok(())
}
