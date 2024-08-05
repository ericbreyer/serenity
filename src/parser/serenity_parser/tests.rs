#![cfg(test)]
use super::*;

use test_case::test_case;
use insta::{self, assert_debug_snapshot};

#[test_case( "var a: int;", false;"var_decl_simple_type")]
#[test_case( "var a: int", true;"var_decl_simple_type_missing_semicolon")]
#[test_case( "var : int;", true;"var_decl_simple_type_missing_name")]
#[test_case( "var a int;", true;"var_decl_simple_type_missing_colon")]
#[test_case( "var a: int = 5;", false;"var_decl_simple_type_with_init")]
#[test_case( "var a: int = 5", true;"var_decl_simple_type_with_init_missing_semicolon")]
#[test_case( "var a int = 5;", true;"var_decl_simple_type_with_init_missing_colon")]
#[test_case( "var : int = 5;", true;"var_decl_simple_type_with_init_missing_name")]
fn test_parser(snippet: &str, should_fail: bool) {
    let res = SerenityParser::parse(snippet.into(), "test".into());

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_suffix(format!("{snippet}"));
    let _guard = settings.bind_to_scope();


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
