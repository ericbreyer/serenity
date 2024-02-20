use crate::{common::ParseResult, lexer::Lexer};

mod serenity_parser;

pub trait Parser {
    fn parse(lexer: Lexer) -> ParseResult;
}

pub use serenity_parser::SerenityParser;