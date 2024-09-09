#![cfg(test)]

use anyhow::Result;
use test_case::test_case;

#[test_case("trivial.ser", 1; "trivial")]
#[test_case("prime_sieve.ser", 8181807856294299570; "prime_sieve")]
#[test_case("interfaces.ser", 12; "interfaces")]
#[test_case("babbage.ser", 25264; "babbage")]
#[test_case("linkedlist.ser", 55; "linkedlist")]
#[test_case("prime_conspiricy.ser", 7942686168; "prime_conspiricy")]
#[test_case("highly_composites.ser", 355168; "highly_composites")]
pub fn test_file(file : &str, ecode: i64) -> Result<()> {

    let file = format!("tests/{}", file);

    let mut out = Vec::new();
    // let _guard = serenity::set_log_verbosity(2)?;
    let code = serenity::run_file(&file, &mut out)?;

    assert!(code == ecode, "Expected: {}, Got: {}", ecode, code);    
    
    Ok(())
}