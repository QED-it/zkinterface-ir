extern crate sieve_ir;

use sieve_ir::{Result, cli::{cli, Options}};
use structopt::StructOpt;

fn main() -> Result<()> {
    cli(&Options::from_args())
}
