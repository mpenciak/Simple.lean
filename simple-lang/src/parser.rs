//! Here we define the parser

use crate::{
    ast::{self, Definition, ImportStatement, NamespaceDecl, NamespaceModifier, TopLevel},
    errors,
    lexer::{Keyword, Token, Tokens},
};
use nom::{
    self,
    branch::alt,
    combinator::{all_consuming, map_res, value},
    error::Error,
    multi::many0,
    sequence::{preceded, tuple},
    Err, IResult, Parser,
};

fn parse_file(input: Tokens) -> IResult<Tokens, Vec<TopLevel>> {
    all_consuming(many0(parse_top_level)).parse(input)
}

fn parse_top_level(input: Tokens) -> IResult<Tokens, TopLevel> {
    alt((
        parse_definition.map(|def| TopLevel::Definition(def)),
        parse_import.map(|imp| TopLevel::Import(imp)),
        parse_namespace_modifier.map(|modif| TopLevel::NamespaceModifier(modif)),
        parse_namespace_decl.map(|decl| TopLevel::NamespaceDecl(decl)),
    ))
    .parse(input)
}

fn parse_definition(input: Tokens) -> IResult<Tokens, Definition> {
    todo!()
}

fn parse_import(input: Tokens) -> IResult<Tokens, ImportStatement> {
    let (input, (keyword, filename)) = ((parse_keyword, parse_identifier)).parse(input)?;
    if matches!(keyword, Keyword::Import) {
        Ok((input, ImportStatement { file: filename }))
    } else {
        Err(Err::Error(Error {
            input,
            code: nom::error::ErrorKind::Char,
        }))
    }
}

fn parse_namespace_modifier(input: Tokens) -> IResult<Tokens, NamespaceModifier> {
    todo!()
}

fn parse_namespace_decl(input: Tokens) -> IResult<Tokens, NamespaceDecl> {
    todo!()
}

fn parse_identifier(input: Tokens) -> IResult<Tokens, String> {
    todo!()
}

fn parse_keyword(input: Tokens) -> IResult<Tokens, Keyword> {
    todo!()
}
