#![feature(test)] // Required to enable benchmarking in Rust
extern crate test;
use test::Bencher;

use anyhow::{bail, Ok, Result};
use insta::assert_snapshot;
use test_case::{test_case, test_matrix};

// #[test_case("trivial.ser" => matches Ok(1))]
// #[test_case("prime_sieve.ser" => matches Ok(8181807856294299570))]
// #[test_case("interfaces.ser" => matches Ok(12))]
// #[test_case("babbage.ser" => matches Ok(25264))]
// #[test_case("linkedlist.ser" => matches Ok(55))]
// #[test_case("prime_conspiricy.ser" => matches Ok(7942686168))]
// #[test_case("highly_composites.ser" => matches Ok(355168))]
// #[test_case("generic.ser" => matches Ok(6))]
// #[test_case("generic_func.ser" => matches Ok(540))]
#[bench]
pub fn test_file_run(b: &mut Bencher) {
    b.iter(|| {
        let file = format!("test_files/{}", "generic_func.ser");

        let mut out = Vec::new();
        let code = serenity::run_file(&file, &mut out).unwrap();

        assert!(code == 540);
    });
}

// #[test_matrix(
//     ["trivial.ser", "prime_sieve.ser", "interfaces.ser", "babbage.ser", "linkedlist.ser", "prime_conspiricy.ser", "highly_composites.ser", "generic.ser", "generic_func.ser"],
//     ["scan", "parse", "compile"]
// )]
// fn test_file_artifacts(file : &str, mode: &str) -> Result<()> {
//     let file = format!("tests/{}", file);
//     let result = match mode {
//         "scan" => serenity::scan(&file)?.iter().fold(String::new(), |acc, x| acc + &format!("{}\n", x)),
//         "parse" => serenity::parse(&file)?,
//         "compile" => serenity::compile(&file)?,
//         _ => bail!("Invalid mode"),
//     };

//     let mut settings = insta::Settings::clone_current();
//     settings.set_snapshot_suffix(format!("{}_{}", file, mode));
//     let _guard = settings.bind_to_scope();

//     match mode {
//         "scan" => assert_snapshot!(result),
//         "parse" => assert_snapshot!(result),
//         "compile" => assert_snapshot!(result),
//         _ => bail!("Invalid mode"),
//     }

//     Ok(())
// }