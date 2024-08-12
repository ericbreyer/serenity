// #![cfg(test)]

// use insta::Settings;
// use test_case::test_case;

// #[test_case("test12.ser"; "test12")]
// #[test_case("test11.ser"; "test11")]
// #[test_case("test10.ser"; "test10")]
// #[test_case("test6.ser"; "test6")]
// #[test_case("test5.ser"; "test5")]
// #[test_case("test4.ser"; "test4")]
// #[test_case("test3.ser"; "test3")]
// #[test_case("test2.ser"; "test2")]
// pub fn test_file(file : &str) {

//     let mut out = Vec::new();
//     serenity::run_file(file, &mut out).unwrap();

//     let mut settings = Settings::clone_current();
//     settings.set_snapshot_suffix(format!("{file}"));
//     let _guard = settings.bind_to_scope();
//     insta::assert_snapshot!(std::str::from_utf8(&out).unwrap());
// }
