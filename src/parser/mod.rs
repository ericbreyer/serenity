use crate::{common::ParseResult, lexer::lexer};

mod serenity_parser;

pub trait Parser {
    fn parse(lexer: lexer) -> ParseResult;
}

pub use serenity_parser::SerenityParser;