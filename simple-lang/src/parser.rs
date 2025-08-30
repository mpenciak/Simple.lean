//! Here we define the parser

use crate::{
    ast::{Definition, ImportStatement, NamespaceDecl, NamespaceModifier, TopLevel},
    errors::{self, parser::Error},
    lexer::{Identifier, Keyword, Token, Tokens},
};
use nom::{
    self, branch::alt, combinator::all_consuming, multi::many0, sequence::preceded,
    Err::Error as NomError, IResult, Input, Parser,
};

type ParseResult<'a, T> = IResult<Tokens<'a>, T, errors::parser::Error<Tokens<'a>>>;

trait TokenExtract: Sized {
    fn extract(token: Token) -> Option<Self>;
}

macro_rules! impl_token_extract {
    ($ty:ty, $branch:ident) => {
        impl TokenExtract for $ty {
            fn extract(token: Token) -> Option<Self> {
                match token {
                    Token::$branch(value) => Some(value),
                    _ => None,
                }
            }
        }
    };
}

macro_rules! parse_variant {
    ($ty:ty) => {
        parse_atom::<$ty>()
    };
}

impl_token_extract!(Identifier, Identifier);
impl_token_extract!(Keyword, Keyword);

pub fn parse_file(input: Tokens) -> ParseResult<Vec<TopLevel>> {
    all_consuming(many0(parse_top_level)).parse(input)
}

fn parse_top_level(input: Tokens) -> ParseResult<TopLevel> {
    alt((
        parse_definition.map(TopLevel::Definition),
        parse_import.map(TopLevel::Import),
        parse_namespace_modifier.map(TopLevel::NamespaceModifier),
        parse_namespace_decl.map(TopLevel::NamespaceDecl),
    ))
    .parse(input)
}

fn parse_definition(input: Tokens) -> ParseResult<Definition> {
    todo!()
}

fn parse_import(input: Tokens) -> ParseResult<ImportStatement> {
    let (input, filename) =
        preceded(expect_keyword(Keyword::Import), parse_identifier).parse(input)?;
    Ok((input, ImportStatement { file: filename }))
}

fn parse_namespace_modifier(input: Tokens) -> ParseResult<NamespaceModifier> {
    todo!()
}

fn parse_namespace_decl(input: Tokens) -> ParseResult<NamespaceDecl> {
    todo!()
}

fn parse_identifier(input: Tokens) -> ParseResult<Identifier> {
    parse_variant!(Identifier).parse(input)
}

fn parse_keyword(input: Tokens) -> ParseResult<Keyword> {
    parse_variant!(Keyword).parse(input)
}

fn expect_keyword(expected: Keyword) -> impl Fn(Tokens) -> ParseResult<Keyword> {
    move |input: Tokens| {
        let (input, keyword) = parse_keyword(input)?;
        if keyword == expected {
            Ok((input, keyword))
        } else {
            Err(NomError(Error::expected_keyword(expected, keyword)))
        }
    }
}

fn parse_atom<T: TokenExtract>() -> impl Fn(Tokens) -> ParseResult<T> {
    move |input: Tokens| match input.iter_elements().next() {
        Some(token) => match T::extract(token) {
            Some(value) => Ok((input.take_from(1), value)),
            None => Err(NomError(Error::tag(input))),
        },
        None => Err(NomError(Error::eof(input))),
    }
}
