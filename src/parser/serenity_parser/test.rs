#![cfg(test)]
use insta::{self, assert_debug_snapshot};
use test_case::test_case;

use super::*;

#[test_case( "var a: int;", false;"var_decl_simple_type")]
#[test_case( "var a: int", true;"var_decl_simple_type_missing_semicolon")]
#[test_case( "var : int;", true;"var_decl_simple_type_missing_name")]
#[test_case( "var a int;", true;"var_decl_simple_type_missing_colon")]
#[test_case( "var a: int = 5;", false;"var_decl_simple_type_with_init")]
#[test_case( "var a: int = 5", true;"var_decl_simple_type_with_init_missing_semicolon")]
#[test_case( "var a int = 5;", true;"var_decl_simple_type_with_init_missing_colon")]
#[test_case( "var : int = 5;", true;"var_decl_simple_type_with_init_missing_name")]
#[test_case( "{ var i = 5; }", false;"block_statement")]
#[test_case( "if (x > 8) { return 5; }", false;"if_statement")]
#[test_case("while (x > 8) { return 5; }", false;"while_statement")]
#[test_case("for (var i = 0; i < 10; i = i + 1) { return i; }", false;"for_statement")]
#[test_case("for (var i = 0; i < 10; i = i + 1) { continue; }", false;"continue_statement")]
#[test_case("for (var i = 0; i < 10; i = i + 1) { break; }", false;"break_statement")]
#[test_case("return 5;", false;"return_statement")]
#[test_case("5;", false;"literal_expression")]
#[test_case("string;", false;"string_expression")]
#[test_case("5 + 5;", false;"binary_expression")]
#[test_case("-5;", false;"unary_expression")]
#[test_case("*ref;", false;"deref_expression")]
#[test_case("&val;", false;"ref_expression")]
#[test_case("arr[6];", false;"index_expression")]
#[test_case("5 ? 5 : 5;", false;"ternary_expression")]
#[test_case("ref;", false;"var_expression")]
#[test_case("call(arg1, arg2);", false;"call_expression")]
#[test_case("x = 5;", false;"assign_expression")]
#[test_case("x and y;", false;"and_expression")]
#[test_case("x or y;", false;"or_expression")]
#[test_case("x == y;", false;"eq_expression")]
#[test_case("x != y;", false;"neq_expression")]
#[test_case("x < y;", false;"lt_expression")]
#[test_case("x <= y;", false;"lte_expression")]
#[test_case("x > y;", false;"gt_expression")]
#[test_case("x >= y;", false;"gte_expression")]
#[test_case("x.y;", false;"dot_expression")]
#[test_case("lambda(x: int) -> int { return x + 5; };", false;"lambda_expression")]
#[test_case("let add2 = add(x, _);", false;"partial_call_closure")]
fn test_parser(snippet: &str, should_fail: bool) {
    let res = SerenityParser::parse(snippet.into(), "test".into(), vec![]);

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_suffix(snippet.to_string());
    let _g = settings.bind_to_scope();

    if should_fail {
        assert!(res.is_err());
        let Err(err) = res else { unreachable!() };
        assert_debug_snapshot!(err);
    } else {
        assert!(res.is_ok());
        let Ok(ast) = res else { unreachable!() };
        assert_debug_snapshot!(ast);
    }
}

// Additional comprehensive parser tests
#[test_case("fn main() -> int { return 0; }", false;"function_declaration")]
#[test_case("fn add(x: int, y: int) -> int { return x + y; }", false;"function_with_params")]
#[test_case("const PI: float = 3.14;", false;"const_declaration")]
#[test_case("let mut x: int = 5;", false;"mutable_var")]
#[test_case("let x = 1 + 2 * 3;", false;"operator_precedence")]
#[test_case("let x = (1 + 2) * 3;", false;"parentheses_precedence")]
#[test_case("let a = true and false or true;", false;"logical_operators")]
#[test_case("func(a, b, c);", false;"multiple_args")]
#[test_case("if (x) { a; } else { b; }", false;"if_else_statement")]
#[test_case("if (x) { a; } else if (y) { b; } else { c; }", false;"if_else_if_chain")]
#[test_case("type Point struct { x: int, y: int };", false;"struct_declaration")]
#[test_case("type Point struct { x: int, y: int };", false;"struct_declaration_impl")]
#[test_case("type Iterator interface { };", false;"interface_declaration")]
#[test_case("#ptr;", false;"cast_expression")]
#[test_case("cast(ptr, int);", false;"cast_expression2")]
#[test_case("arr[0] = 5;", false;"array_assignment")]
#[test_case("obj.field = 10;", false;"field_assignment")]
#[test_case("obj.method();", false;"method_call")]
#[test_case("sizeof(int);", false;"sizeof_expression")]
#[test_case("var a: [int; 2];", false;"array_type_constant_index")]
#[test_case("var p: *int*;", false;"pointer_both_sides_star_invalid")]
#[test_case("var a: [int; -1];", true;"array_negative_index")]
#[test_case("type S struct { x: int }; var s: struct S<int>;", true;"struct_generic_mismatch")]
#[test_case("var u: struct Unknown;", true;"unknown_struct_type")]
#[test_case("var f: fun[int](int) -> int;", false;"function_type_with_captures")]
#[test_case("var p: int*;", false;"pointer_trailing_star")]
#[test_case("var a: impl Foo;", true;"impl_self_struct_type")]
#[test_case("type<T> S struct { x: T }; var s: struct S<int, int>;", true;"struct_generic_arity_mismatch")]
#[test_case("var f: fun[int]();", false;"function_type_captures_no_params")]
fn test_parser_extended(snippet: &str, should_fail: bool) {
    let res = SerenityParser::parse(snippet.into(), "test".into(), vec![]);

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_suffix(format!("extended_{snippet}"));
    let _g = settings.bind_to_scope();

    if should_fail {
        assert!(res.is_err());
    } else {
        assert!(res.is_ok());
    }
}

#[test]
fn test_empty_block() {
    let res = SerenityParser::parse("{ }".into(), "test".into(), vec![]);
    assert!(res.is_ok());
}

#[test]
fn test_nested_blocks() {
    let res = SerenityParser::parse("{ { { } } }".into(), "test".into(), vec![]);
    assert!(res.is_ok());
}

#[test]
fn test_multiple_statements() {
    let res = SerenityParser::parse(
        "var a: int; var b: int; var c: int;".into(),
        "test".into(),
        vec![],
    );
    assert!(res.is_ok());
}

#[test]
fn test_arithmetic_chain() {
    let res = SerenityParser::parse("1 + 2 - 3 * 4 / 5;".into(), "test".into(), vec![]);
    assert!(res.is_ok());
}

#[test]
fn test_comparison_chain() {
    let res = SerenityParser::parse("a < b and b < c and c < d;".into(), "test".into(), vec![]);
    assert!(res.is_ok());
}
