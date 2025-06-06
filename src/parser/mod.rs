use crate::prelude::*;
use anyhow::Result;

mod serenity_parser;

pub trait Parser {
    fn parse(source: SharedString, name: SharedString) -> Result<ParseResult>;
}

pub use serenity_parser::SerenityParser;
