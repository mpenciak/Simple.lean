//! Here we define the parser

use crate::{
    ast::{
        Block, Definition, Expr, FieldDef, FunctionDef, GlobalDef, ImplBlock, ImportStatement,
        NamespaceDecl, NamespaceModifier, Parameter, Statement, StructDef, TopLevel, TypeAlias,
        TypeExpr,
    },
    errors::{self, parser::Error},
    lexer::{Delimiter, Identifier, Keyword, Punctuation, Token, Tokens},
};
use nom::{
    self,
    branch::alt,
    combinator::all_consuming,
    multi::{many0, separated_list0},
    sequence::preceded,
    Err::Error as NomError,
    IResult, Input, Parser,
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

impl_token_extract!(Identifier, Identifier);
impl_token_extract!(Keyword, Keyword);
impl_token_extract!(Delimiter, Delimiter);

macro_rules! parse_variant {
    ($ty:ty) => {
        parse_atom::<$ty>()
    };
}

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
    alt((
        parse_func_def.map(Definition::Function),
        parse_type_alias.map(Definition::TypeAlias),
        parse_struct_def.map(Definition::Struct),
        parse_impl.map(Definition::Impl),
        parse_global.map(Definition::GlobalDef),
    ))
    .parse(input)
}

fn parse_func_def(input: Tokens) -> ParseResult<FunctionDef> {
    let (input, (_, name, params, _, return_type, body)) = (
        expect_keywords(vec![Keyword::Def]),
        parse_identifier,
        surrounded_by(
            Delimiter::LeftParen,
            |tokens| {
                separated_list0(
                    expect_atomic_token(Token::Punctuation(Punctuation::Comma)),
                    parse_parameter,
                )
                .parse(tokens)
            },
            Delimiter::RightParen,
        ),
        expect_atomic_token(Token::Punctuation(Punctuation::Arrow)),
        parse_type_expr,
        parse_block,
    )
        .parse(input)?;

    let func_def = FunctionDef {
        name,
        params,
        return_type,
        body,
    };

    Ok((input, func_def))
}

fn parse_parameter(input: Tokens) -> ParseResult<Parameter> {
    let (input, (name, _, type_expr)) = (
        parse_identifier,
        expect_atomic_token(Token::Punctuation(Punctuation::Colon)),
        parse_type_expr,
    )
        .parse(input)?;

    let param = Parameter { name, type_expr };

    Ok((input, param))
}

fn parse_block(input: Tokens) -> ParseResult<Block> {
    separated_list0(
        expect_atomic_token(Token::Punctuation(Punctuation::Semicolon)),
        parse_statement,
    )
    .map(|statements| Block { statements })
    .parse(input)
}

fn parse_statement(input: Tokens) -> ParseResult<Statement> {
    todo!()
}

fn parse_type_alias(input: Tokens) -> ParseResult<TypeAlias> {
    let (input, (_, name, _, type_expr)) = (
        expect_keywords(vec![Keyword::Type]),
        parse_identifier,
        expect_atomic_token(Token::Assign),
        parse_type_expr,
    )
        .parse(input)?;

    let alias = TypeAlias { name, type_expr };

    Ok((input, alias))
}

fn parse_struct_def(input: Tokens) -> ParseResult<StructDef> {
    let (input, (_, name, fields)) = (
        expect_keywords(vec![Keyword::Struct]),
        parse_identifier,
        surrounded_by(
            Delimiter::LeftBrace,
            |tokens| many0(parse_struct_field).parse(tokens),
            Delimiter::RightBrace,
        ),
    )
        .parse(input)?;

    let struct_def = StructDef { name, fields };

    Ok((input, struct_def))
}

fn parse_struct_field(input: Tokens) -> ParseResult<FieldDef> {
    let (input, (name, _, type_expr, _)) = (
        parse_identifier,
        expect_atomic_token(Token::Punctuation(Punctuation::Colon)),
        parse_type_expr,
        expect_atomic_token(Token::Punctuation(Punctuation::Comma)),
    )
        .parse(input)?;

    let field_def = FieldDef { name, type_expr };

    Ok((input, field_def))
}

fn parse_impl(input: Tokens) -> ParseResult<ImplBlock> {
    let (input, (_, struct_name, methods)) = (
        expect_keywords(vec![Keyword::Impl]),
        parse_identifier,
        surrounded_by(
            Delimiter::LeftBrace,
            |tokens| many0(parse_func_def).parse(tokens),
            Delimiter::RightBrace,
        ),
    )
        .parse(input)?;

    let impl_block = ImplBlock {
        struct_name,
        methods,
    };

    Ok((input, impl_block))
}

fn parse_global(input: Tokens) -> ParseResult<GlobalDef> {
    let (input, (_, name, _, type_expr, _, expr)) = (
        expect_keywords(vec![Keyword::Global]),
        parse_identifier,
        expect_atomic_token(Token::Punctuation(Punctuation::Colon)),
        parse_type_expr,
        expect_atomic_token(Token::Assign),
        parse_expr,
    )
        .parse(input)?;

    let global_def = GlobalDef {
        name,
        type_expr: type_expr,
        value: expr,
    };

    Ok((input, global_def))
}

fn parse_type_expr(input: Tokens) -> ParseResult<TypeExpr> {
    todo!()
}

fn parse_import(input: Tokens) -> ParseResult<ImportStatement> {
    let (input, filename) =
        preceded(expect_keywords(vec![Keyword::Import]), parse_identifier).parse(input)?;
    Ok((input, ImportStatement { file: filename }))
}

fn parse_namespace_modifier(input: Tokens) -> ParseResult<NamespaceModifier> {
    let (input, (keyword, identifer)) =
        (parse_variant!(Keyword), parse_variant!(Identifier)).parse(input)?;

    match keyword {
        Keyword::Open => Ok((input, NamespaceModifier::Open(identifer))),
        Keyword::Close => Ok((input, NamespaceModifier::Close(identifer))),
        _ => Err(NomError(Error::expected_keyword(
            vec![Keyword::Open, Keyword::Close],
            keyword,
        ))),
    }
}

fn parse_expr(input: Tokens) -> ParseResult<Expr> {
    todo!()
}

fn expect_delimiter(del: Delimiter) -> impl FnMut(Tokens) -> ParseResult<Delimiter> {
    move |input: Tokens| {
        let (input, seen) = parse_variant!(Delimiter).parse(input)?;
        if seen == del {
            Ok((input, seen))
        } else {
            Err(NomError(Error::tag(input)))
        }
    }
}

// TODO: Not super happy about this, want to use `delimited` here
fn surrounded_by<F, O>(
    left: Delimiter,
    mut inside: F,
    right: Delimiter,
) -> impl FnMut(Tokens) -> ParseResult<O>
where
    F: FnMut(Tokens) -> ParseResult<O>,
{
    move |input| {
        let (input, _) = expect_delimiter(left)(input)?;
        let (input, out) = inside(input)?;
        let (input, _) = expect_delimiter(right)(input)?;
        Ok((input, out))
    }
}

fn parse_namespace_decl(input: Tokens) -> ParseResult<NamespaceDecl> {
    let (input, (_, namespace, decls)) = (
        expect_keywords(vec![Keyword::Namespace]),
        parse_identifier,
        surrounded_by(
            Delimiter::LeftBrace,
            |tokens| many0(parse_top_level).parse(tokens),
            Delimiter::RightBrace,
        ),
    )
        .parse(input)?;

    Ok((
        input,
        NamespaceDecl {
            name: namespace,
            body: decls,
        },
    ))
}

fn parse_identifier(input: Tokens) -> ParseResult<Identifier> {
    parse_variant!(Identifier).parse(input)
}

fn parse_keyword(input: Tokens) -> ParseResult<Keyword> {
    parse_variant!(Keyword).parse(input)
}

fn expect_keywords(expected: Vec<Keyword>) -> impl Fn(Tokens) -> ParseResult<Keyword> {
    move |input: Tokens| {
        let (input, keyword) = parse_keyword(input)?;
        if expected.contains(&keyword) {
            Ok((input, keyword))
        } else {
            Err(NomError(Error::expected_keyword(expected.clone(), keyword)))
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

fn expect_atomic_token(token: Token) -> impl FnMut(Tokens) -> ParseResult<Token> {
    move |input: Tokens| match input.iter_elements().next() {
        Some(seen) if seen == token => Ok((input.take_from(1), seen.clone())),
        Some(_) => Err(NomError(Error::tag(input))),
        None => Err(NomError(Error::eof(input))),
    }
}
