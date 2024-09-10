#![cfg(test)]

use anyhow::Result;
use insta::{assert_debug_snapshot, assert_snapshot};
use test_case::test_case;

#[test_case("trivial.ser", 1; "trivial")]
#[test_case("prime_sieve.ser", 8181807856294299570; "prime_sieve")]
#[test_case("interfaces.ser", 12; "interfaces")]
#[test_case("babbage.ser", 25264; "babbage")]
#[test_case("linkedlist.ser", 55; "linkedlist")]
#[test_case("prime_conspiricy.ser", 7942686168; "prime_conspiricy")]
#[test_case("highly_composites.ser", 355168; "highly_composites")]
pub fn test_file_run(file : &str, ecode: i64) -> Result<()> {

    let file = format!("tests/{}", file);

    let mut out = Vec::new();
    let code = serenity::run_file(&file, &mut out)?;

    assert!(code == ecode, "Expected: {}, Got: {}", ecode, code);    
    
    Ok(())
}

#[test_case("trivial.ser"; "trivial")]
#[test_case("prime_sieve.ser"; "prime_sieve")]
#[test_case("interfaces.ser"; "interfaces")]
#[test_case("babbage.ser"; "babbage")]
#[test_case("linkedlist.ser"; "linkedlist")]
#[test_case("prime_conspiricy.ser"; "prime_conspiricy")]
#[test_case("highly_composites.ser"; "highly_composites")]
pub fn test_file_scan(file : &str) -> Result<()> {
    let file = format!("tests/{}", file);
    let tokens = serenity::scan(&file)?;

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_suffix(file);
    let _guard = settings.bind_to_scope();

    assert_debug_snapshot!(tokens);
    Ok(())
}

#[test_case("trivial.ser"; "trivial")]
#[test_case("prime_sieve.ser"; "prime_sieve")]
#[test_case("interfaces.ser"; "interfaces")]
#[test_case("babbage.ser"; "babbage")]
#[test_case("linkedlist.ser"; "linkedlist")]
#[test_case("prime_conspiricy.ser"; "prime_conspiricy")]
#[test_case("highly_composites.ser"; "highly_composites")]
pub fn test_file_parse(file : &str) -> Result<()> {
    let file = format!("tests/{}", file);
    let ast = serenity::parse(&file)?;

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_suffix(file);
    let _guard = settings.bind_to_scope();

    assert_snapshot!(ast);
    Ok(())
}

#[test_case("trivial.ser"; "trivial")]
#[test_case("prime_sieve.ser"; "prime_sieve")]
#[test_case("interfaces.ser"; "interfaces")]
#[test_case("babbage.ser"; "babbage")]
#[test_case("linkedlist.ser"; "linkedlist")]
#[test_case("prime_conspiricy.ser"; "prime_conspiricy")]
#[test_case("highly_composites.ser"; "highly_composites")]
pub fn test_file_compile(file : &str) -> Result<()> {
    let file = format!("tests/{}", file);
    let ir = serenity::compile(&file)?;

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_suffix(file);
    let _guard = settings.bind_to_scope();

    assert_snapshot!(ir);
    Ok(())
}

