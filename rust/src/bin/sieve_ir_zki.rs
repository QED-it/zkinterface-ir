extern crate sieve_ir_zki;

use sieve_ir_zki::{Result, cli::{cli, Options}};
use structopt::StructOpt;

fn main() -> Result<()> {
    cli(&Options::from_args())
}
