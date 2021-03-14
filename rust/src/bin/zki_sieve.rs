extern crate zki_sieve;

use zki_sieve::{Result, cli::{cli, Options}};
use structopt::StructOpt;

fn main() -> Result<()> {
    cli(&Options::from_args())
}
