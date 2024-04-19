use crate::{common::ParseResult};

mod serenity_parser;

pub trait Parser {
    fn parse(source: String, name: String) -> ParseResult;
}

pub use serenity_parser::SerenityParser;