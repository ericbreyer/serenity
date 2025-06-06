#![cfg(test)]
use super::*;

use insta::{self, assert_debug_snapshot};
use test_case::test_case;

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
fn test_parser(snippet: &str, should_fail: bool) {
    let res = SerenityParser::parse(snippet.into(), "test".into());

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
