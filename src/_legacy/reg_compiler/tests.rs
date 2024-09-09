#![cfg(test)]

use crate::parser::{Parser, SerenityParser};

use super::*;
use insta::Settings;
use test_case::test_case;

const SIMPLE_PROGRAM : &str = 
"fn main() {
    print 5;
}";

#[test_case(SIMPLE_PROGRAM; "simple_program")]
fn test_compile(prog: &str) {
    let pr = SerenityParser::parse(prog.into(), "top".into()).unwrap();
    let bytecode = compile(pr, &HashMap::default());
    insta::assert_debug_snapshot!(bytecode)
}

#[test_case("int", "2" ; "int")]
#[test_case("float", "2.0" ; "float")]
#[test_case("bool", "true" ; "bool")]
#[test_case("*char", "\"hello\"" ; "SharedString")]
#[test_case("fun() -> int", "lambda() -> int {return 2;}" ; "fun")]
#[test_case("fun(int) -> int", "lambda(x : int) -> int {return x;}" ; "fun with param")]
#[test_case("fun(int, int) -> int", "lambda(x : int, y : int) -> int {return x + y;}" ; "fun with params")]
fn good_assignment_type_tests_locals(typ: &str, val: &str) {
    let parsed = SerenityParser::parse(
        format!("fun main() {{ var x : {} = {}; }}", typ, val).into(),
        "test".into(),
    ).unwrap();
    
    let bytecode = compile(parsed, &HashMap::default()).expect("Should compile");
    let mut settings = Settings::clone_current();
    settings.set_snapshot_suffix(format!("{typ}_{val}"));
    let _guard = settings.bind_to_scope();
    insta::assert_debug_snapshot!(bytecode)
}

#[test_case("int", "2.0" ; "int")]
#[test_case("float", "2" ; "float")]
#[test_case("bool", "2" ; "bool")]
#[test_case("*char", "2" ; "SharedString")]
#[test_case("fun() -> int", "2" ; "fun")]
#[test_case("fun(int) -> int", "2" ; "fun with param")]
#[test_case("fun(int, int) -> int", "2" ; "fun with params")]
fn bad_assignment_type_tests_locals(typ: &str, val: &str) {
    let parsed = SerenityParser::parse(
        format!("fun main() {{ var x : {} = {}; }}", typ, val).into(),
        "test".into(),
    ).unwrap();
    
    let err = compile(parsed, &HashMap::default()).expect_err("Shouldn't compile");
    let mut settings = Settings::clone_current();
    settings.set_snapshot_suffix(format!("{typ}_{val}"));
    settings.add_filter(":[0-9]+", "[LINE]");
    let _guard = settings.bind_to_scope();
    insta::assert_debug_snapshot!(err)
}

#[test_case("int", "2" ; "int")]
#[test_case("float", "2.0" ; "float")]
#[test_case("bool", "true" ; "bool")]
#[test_case("char *", "\"hello\"" ; "char *")]
#[test_case("fun() -> int", "lambda() -> int {return 2;}" ; "fun")]
#[test_case("fun(int) -> int", "lambda(x : int) -> int {return x;}" ; "fun with param")]
#[test_case("fun(int, int) -> int", "lambda(x : int, y : int) -> int {return x + y;}" ; "fun with params")]
fn good_return_type_tests(typ: &str, val: &str) {

    let parsed = SerenityParser::parse(
        format!("fun t() -> {} {{ return {}; }} fun main() {{}}", typ, val).into(),
        "test".into(),
    ).unwrap();
    let err = compile(parsed, &HashMap::default()).expect("Should compile");
    let mut settings = Settings::clone_current();
    settings.set_snapshot_suffix(format!("{typ}_{val}"));
    let _guard = settings.bind_to_scope();
    insta::assert_debug_snapshot!(err)
}


#[test_case("int", "2.0" ; "int")]
#[test_case("float", "2" ; "float")]
#[test_case("bool", "2" ; "bool")]
#[test_case("char *", "2" ; "char *")]
#[test_case("fun() -> int", "2" ; "fun")]
#[test_case("fun(int) -> int", "2" ; "fun with param")]
#[test_case("fun(int, int) -> int", "2" ; "fun with params")]
fn bad_return_type_tests(typ: &str, val: &str) {
    let parsed = SerenityParser::parse(
        format!("fun t() -> {} {{ return {}; }} fun main() {{}}", typ, val).into(),
        "test".into(),
    ).unwrap();
    let err = compile(parsed, &HashMap::default()).expect_err("Shouldn't compile");
    let mut settings = Settings::clone_current();
    settings.set_snapshot_suffix(format!("{typ}_{val}"));
    settings.add_filter(":[0-9]+", "[LINE]");
    let _guard = settings.bind_to_scope();
    insta::assert_debug_snapshot!(err)
}

// Test ternary operator with valid types
#[test_case("true", "2", "3" ; "valid types")]
#[test_case("false", "\"hello\"", "\"world\"" ; "valid types 2")]
#[test_case("true", "2.0", "3.0" ; "valid types 3")]
fn ternary_operator_valid_types(cond_val: &str, true_val: &str, false_val: &str) {
    let parsed = SerenityParser::parse(
        format!(
            "fun main() {{ var result = {} ? {} : {}; }}",
            cond_val, true_val, false_val
        ).into(),
        "test".into(),
    ).unwrap();

    let bytecode = compile(parsed, &HashMap::default()).expect("Should compile");
    let mut settings = Settings::clone_current();
    settings.set_snapshot_suffix(format!("{cond_val}_{true_val}_{false_val}"));
    let _guard = settings.bind_to_scope();
    insta::assert_debug_snapshot!(bytecode)


}

// Test ternary operator with invalid types
#[test_case("2", "2", "3" ; "valid true branch type for bool condition")]
#[test_case("true", "2", "3.14" ; "invalid true branch type for bool condition")]
#[test_case("false", "3.14", "2" ; "invalid false branch type for bool condition")]
fn ternary_operator_invalid_types(cond_val: &str, true_val: &str, false_val: &str) {
    let parsed = SerenityParser::parse(
        format!(
            "fun main() {{ var result = {} ? {} : {}; }}",
            cond_val, true_val, false_val
        ).into(),
        "test".into(),
    ).unwrap();

    let err = compile(parsed, &HashMap::default()).expect_err("Shouldn't compile");
    let mut settings = Settings::clone_current();
    settings.set_snapshot_suffix(format!("{cond_val}_{true_val}_{false_val}"));
    settings.add_filter(":[0-9]+", "[LINE]");
    let _guard = settings.bind_to_scope();
    insta::assert_debug_snapshot!(err)
}