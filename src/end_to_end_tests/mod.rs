
use test_case::test_case;
use crate::{parser::{Parser, SerenityParser}, vm::{InterpretResult, VM}};

#[test_case("int", "2" ; "int")]
#[test_case("float", "2.0" ; "float")]
#[test_case("bool", "true" ; "bool")]
#[test_case("string", "\"hello\"" ; "string")]
#[test_case("fun() -> int", "lambda() -> int {return 2;}" ; "fun")]
#[test_case("fun(int) -> int", "lambda(x : int) -> int {return x;}" ; "fun with param")]
#[test_case("fun(int, int) -> int", "lambda(x : int, y : int) -> int {return x + y;}" ; "fun with params")]
fn good_assignment_type_tests_locals(typ: &str, val : &str) {
    let mut vm = VM::new();
    let parsed = SerenityParser::parse(format!("fun main() {{ var x : {} = {}; }}", typ, val), "test".to_string());
    let res = vm.interpret(parsed);
    assert!(if let InterpretResult::Ok = res {true} else {false});
}

#[test_case("int", "2.0" ; "int")]
#[test_case("float", "2" ; "float")]
#[test_case("bool", "2" ; "bool")]
#[test_case("string", "2" ; "string")]
#[test_case("fun() -> int", "2" ; "fun")]
#[test_case("fun(int) -> int", "2" ; "fun with param")]
#[test_case("fun(int, int) -> int", "fun(float, bool) -> int" ; "fun with params")]
fn bad_assignment_type_tests_locals(typ: &str, val : &str) {
    let mut vm = VM::new();
    let parsed = SerenityParser::parse(format!("fun main()  {{ var x : {} = {}; }}();", typ, val), "test".to_string());
    let res = vm.interpret(parsed);
    assert!(if let InterpretResult::CompileError = res {true} else {false});
}

#[test_case("int", "2" ; "int")]
#[test_case("float", "2.0" ; "float")]
#[test_case("bool", "true" ; "bool")]
#[test_case("string", "\"hello\"" ; "string")]
#[test_case("fun() -> int", "lambda() -> int {return 2;}" ; "fun")]
#[test_case("fun(int) -> int", "lambda(x : int) -> int {return x;}" ; "fun with param")]
#[test_case("fun(int, int) -> int", "lambda(x : int, y : int) -> int {return x + y;}" ; "fun with params")]
fn good_return_type_tests(typ: &str, val : &str) {
    let mut vm = VM::new();
    let parsed = SerenityParser::parse(format!("fun t() -> {} {{ return {}; }} fun main() {{}}", typ, val), "test".to_string());
    let res = vm.interpret(parsed);
    assert!(if let InterpretResult::Ok = res {true} else {false});
}


#[test_case("int", "2.0" ; "int")]
#[test_case("float", "2" ; "float")]
#[test_case("bool", "2" ; "bool")]
#[test_case("string", "2" ; "string")]
#[test_case("fun() -> int", "2" ; "fun")]
#[test_case("fun(int) -> int", "2" ; "fun with param")]
#[test_case("fun(int, int) -> int", "2" ; "fun with params")]
fn bad_return_type_tests(typ: &str, val : &str) {
    let mut vm = VM::new();
    let parsed = SerenityParser::parse(format!("fun t() -> {} {{ return {}; }} fun main(){{}}", typ, val), "test".to_string());
    let res = vm.interpret(parsed);
    assert!(if let InterpretResult::CompileError = res {true} else {false});
}

// Test ternary operator with valid types
#[test_case("true", "2", "3" ; "valid types")]
#[test_case("false", "\"hello\"", "\"world\"" ; "valid types 2")]
#[test_case("true", "2.0", "3.0" ; "valid types 3")]
fn ternary_operator_valid_types(cond_val: &str, true_val: &str, false_val: &str) {
    let mut vm = VM::new();
    let parsed = SerenityParser::parse(format!("fun main() {{ var result = {} ? {} : {}; }}", cond_val, true_val, false_val), "test".to_string());
    let res = vm.interpret(parsed);
    assert!(if let InterpretResult::Ok = res {true} else {false});
}

// Test ternary operator with invalid types
#[test_case("2", "2", "3" ; "valid true branch type for bool condition")]
#[test_case("true", "2", "3.14" ; "invalid true branch type for bool condition")]
#[test_case("false", "3.14", "2" ; "invalid false branch type for bool condition")]
fn ternary_operator_invalid_types(cond_val: &str, true_val: &str, false_val: &str) {
    let mut vm = VM::new();
    let parsed = SerenityParser::parse(format!("fun main() {{ var result = {} ? {} : {}; }}", cond_val, true_val, false_val), "test".to_string());
    let res = vm.interpret(parsed);
    assert!(if let InterpretResult::CompileError = res {true} else {false});
}

// Test cumtrapz function
#[test_case("src/end_to_end_tests/cumtrapz.ser"; "cumtrapz")]
#[test_case("src/end_to_end_tests/closure_hell.ser"; "closure hell")]
#[test_case("src/end_to_end_tests/test7.ser"; "struct stuff")]
#[test_case("src/end_to_end_tests/branching_good.ser"; "branching good")]
fn full_files_succeed(f: &str) {
    let mut vm = VM::new();
    let source = std::fs::read_to_string(f).expect("Failed to read file");
    let parsed = SerenityParser::parse(source, f.to_string());
    let res = vm.interpret(parsed);
    println!("{:?}", res);
    assert!(if let InterpretResult::Ok = res {true} else {false});
}

// Test full files tthat fail
#[test_case("src/end_to_end_tests/branching_bad.ser"; "branching bad")]
fn full_files_fail(f: &str) {
    let mut vm = VM::new();
    let source = std::fs::read_to_string(f).expect("Failed to read file");
    let parsed = SerenityParser::parse(source, f.to_string());
    let res = vm.interpret(parsed);
    assert!(if let InterpretResult::Ok = res {false} else {true});
}

