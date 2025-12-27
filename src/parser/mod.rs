use anyhow::Result;

use crate::prelude::*;

mod serenity_parser;

pub trait Parser {
    fn parse(
        source: SharedString,
        name: SharedString,
        include_paths: Vec<String>,
    ) -> Result<ParseResult>;
}

pub use serenity_parser::SerenityParser;
