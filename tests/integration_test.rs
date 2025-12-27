#![cfg(test)]

use anyhow::Result;
use insta::assert_snapshot;
use test_case::{test_case, test_matrix};

// static  INCLUDE: Vec<String> = vec!["tests/test_files/lib".into()];
lazy_static::lazy_static! {
    static ref INCLUDE: Vec<String> = vec!["tests/test_files/lib".into()];
}

#[test_case("trivial.ser" => matches Ok(1))]
#[test_case("prime_sieve.ser" => matches Ok(8181807856294299570))]
#[test_case("interfaces.ser" => matches Ok(12))]
#[test_case("babbage.ser" => matches Ok(25264))]
#[test_case("linkedlist.ser" => matches Ok(55))]
#[test_case("prime_conspiricy.ser" => matches Ok(7942686168))]
#[test_case("highly_composites.ser" => matches Ok(355168))]
#[test_case("generic.ser" => matches Ok(6))]
#[test_case("generic_func.ser" => matches Ok(540))]
#[test_case("generic_method.ser" => matches Ok(12))]
#[test_case("fib_stream.ser" => matches Ok(1346268))]
#[test_case("mat_fib.ser" => matches Ok(1346268))]
#[test_case("placeholder.ser" => matches Ok(11))]
#[test_case("isolated/extern.ser" => matches Ok(1))]
pub fn test_file_run(file: &str) -> Result<usize> {
    let file = format!("tests/test_files/{}", file);

    let mut out = Vec::new();
    let code = serenity::run_file(&file, INCLUDE.clone(), &mut out)?;
    Ok(code as usize)
}

#[derive(Clone, Copy, Debug)]
enum Mode {
    Scan,
    Parse,
    Compile,
}

#[test_matrix(
    ["trivial.ser",
    "prime_sieve.ser",
    "interfaces.ser",
    "babbage.ser",
    "linkedlist.ser",
    "prime_conspiricy.ser",
    "highly_composites.ser",
    "generic.ser",
    "generic_func.ser",
    "generic_method.ser",
    "fib_stream.ser",
    "mat_fib.ser",
    "placeholder.ser",
    "isolated/extern.ser"],
    [Mode::Scan, Mode::Parse, Mode::Compile]
)]
fn test_file_artifacts(file: &str, mode: Mode) -> Result<()> {
    let file = format!("tests/test_files/{}", file);
    let result = match mode {
        Mode::Scan => serenity::scan(&file)?
            .iter()
            .fold(String::new(), |acc, x| acc + &format!("{}\n", x)),
        Mode::Parse => serenity::parse(&file, INCLUDE.clone())?,
        Mode::Compile => serenity::compile(&file, INCLUDE.clone())?,
    };

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_suffix(format!("{}_{:?}", file, mode));
    let _g = settings.bind_to_scope();

    assert_snapshot!(result);

    Ok(())
}
