extern crate zki_sieve;

use structopt::StructOpt;
use zki_sieve::{
    cli::{cli, Options},
    Result,
};

fn main() -> Result<()> {
    cli(&Options::from_args())
}
