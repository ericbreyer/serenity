

use crate::prelude::*;

mod serenity_parser;

pub trait Parser {
    fn parse(source: SharedString, name: SharedString) -> Result<ParseResult, SharedString>;
}

pub use serenity_parser::SerenityParser;
