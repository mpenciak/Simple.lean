//! Here we define the Lexer

use std::iter::{Cloned, Enumerate};
use std::slice::Iter;

use logos::{Lexer, Logos};
use nom::Input;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"\s+")]
#[logos(skip r"//[^\n]*\n?")]
pub enum Token {
    // Literals
    #[regex(r"0x[0-9a-fA-F]+", LiteralNumber::parse_hex)]
    #[regex(r"0b[01]+", LiteralNumber::parse_bin)]
    #[regex(r"[1-9][0-9]*", LiteralNumber::parse_dec)]
    Number(LiteralNumber),

    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", LiteralFloat::parse_float)]
    #[regex(r"[0-9]+[eE][+-]?[0-9]+", LiteralFloat::parse_scientific)]
    Float(LiteralFloat),

    #[regex(r"'[^'\\]'|'\\.'", |lex| lex.slice().chars().nth(1))]
    Char(char),

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("()")]
    Unit,

    // Strings
    #[regex(r#""(?:[^"]|\\")*""#, |lex| {
        let s = lex.slice();
        let content = &s[1..s.len() - 1];
        let unescaped = content.replace(r#"\""#, "\"");
        Some(unescaped)
    })]
    String(String),

    // Assignment
    #[token("=")]
    Assign,

    // Delimiters
    #[regex(r"(|)|\{|\}|[|]", Delimiter::parse)]
    Delimiter(Delimiter),

    // Punctuation
    #[regex(r";|\||:|\.|,|->", Punctuation::parse)]
    Punctuation(Punctuation),

    // Keywords
    #[regex(
        r"(?x)
        type|mut|const|for|in|while|continue|break|if|
        let|elif|else|def|return|struct|impl|import|
        open|close|namespace|global|fun|List|Tuple|Map",
        Keyword::parse
    )]
    Keyword(Keyword),

    #[regex(
        r"(?x)
        u8|u16|u32|u64|i8|i16|i32|i64|Float|Bool|String|Char|Unit
        ",
        CoreType::parse
    )]
    TypeWord(CoreType),

    // Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(Identifier),

    // NOTE: currently we filter out comments at the lexer level
    // // Comment
    // #[regex(r"//[^\n]*\n?", |lex| lex.slice().to_string())]
    // LineComment(String),

    // Unary Operators
    #[regex(r"-|!", UnaryOperator::parse, priority = 3)]
    UnaryOp(UnaryOperator),

    // Binary Operators
    #[regex(r"\+|\*|/|-|&&|\|\||\+\+", BinaryOperator::parse)]
    BinaryOp(BinaryOperator),

    // Comparison Operators
    #[regex(r"==|!=|<|<=|>|>=", ComparisonOperator::parse)]
    ComparisonOp(ComparisonOperator),
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralNumber {
    DecInt(i64),
    HexInt(i64),
    BinInt(i64),
}

impl LiteralNumber {
    fn parse_dec(lex: &mut Lexer<Token>) -> Option<Self> {
        lex.slice().parse::<i64>().ok().map(LiteralNumber::DecInt)
    }

    fn parse_hex(lex: &mut Lexer<Token>) -> Option<Self> {
        lex.slice().parse::<i64>().ok().map(LiteralNumber::HexInt)
    }

    fn parse_bin(lex: &mut Lexer<Token>) -> Option<Self> {
        lex.slice().parse::<i64>().ok().map(LiteralNumber::BinInt)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralFloat {
    Float(f64),
    Scientific(f64),
}

impl LiteralFloat {
    fn parse_float(lex: &mut Lexer<Token>) -> Option<Self> {
        if let Ok(value) = lex.slice().parse::<f64>() {
            Some(LiteralFloat::Float(value))
        } else {
            None
        }
    }

    fn parse_scientific(lex: &mut Lexer<Token>) -> Option<Self> {
        if let Ok(value) = lex.slice().parse::<f64>() {
            Some(LiteralFloat::Scientific(value))
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Delimiter {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
}

impl Delimiter {
    fn parse(lex: &mut Lexer<Token>) -> Option<Self> {
        match lex.slice() {
            "(" => Some(Delimiter::LeftParen),
            ")" => Some(Delimiter::RightParen),
            "{" => Some(Delimiter::LeftBrace),
            "}" => Some(Delimiter::RightBrace),
            "[" => Some(Delimiter::LeftBracket),
            "]" => Some(Delimiter::RightBracket),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Punctuation {
    Semicolon,
    Pipe,
    Colon,
    Dot,
    Comma,
    Arrow,
}

impl Punctuation {
    fn parse(lex: &mut Lexer<Token>) -> Option<Self> {
        match lex.slice() {
            ";" => Some(Punctuation::Semicolon),
            "|" => Some(Punctuation::Pipe),
            ":" => Some(Punctuation::Colon),
            "." => Some(Punctuation::Dot),
            "," => Some(Punctuation::Comma),
            "->" => Some(Punctuation::Arrow),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Keyword {
    Type,
    Mut,
    Const,
    For,
    In,
    While,
    Continue,
    Break,
    If,
    Elif,
    Else,
    Let,
    Def,
    Return,
    Struct,
    Impl,
    Import,
    Open,
    Close,
    Namespace,
    Global,
    Fun,
    List,
    Map,
    Tuple,
}

impl Keyword {
    fn parse(lex: &mut Lexer<Token>) -> Option<Self> {
        match lex.slice() {
            "type" => Some(Keyword::Type),
            "mut" => Some(Keyword::Mut),
            "const" => Some(Keyword::Const),
            "for" => Some(Keyword::For),
            "in" => Some(Keyword::In),
            "while" => Some(Keyword::While),
            "continue" => Some(Keyword::Continue),
            "break" => Some(Keyword::Break),
            "if" => Some(Keyword::If),
            "elif" => Some(Keyword::Elif),
            "else" => Some(Keyword::Else),
            "let" => Some(Keyword::Let),
            "def" => Some(Keyword::Def),
            "return" => Some(Keyword::Return),
            "struct" => Some(Keyword::Struct),
            "impl" => Some(Keyword::Impl),
            "import" => Some(Keyword::Import),
            "open" => Some(Keyword::Open),
            "close" => Some(Keyword::Close),
            "namespace" => Some(Keyword::Namespace),
            "global" => Some(Keyword::Global),
            "fun" => Some(Keyword::Fun),
            "List" => Some(Keyword::List),
            "Tuple" => Some(Keyword::Tuple),
            "Map" => Some(Keyword::Map),
            _ => None,
        }
    }
}

// Type system
#[derive(Debug, PartialEq, Clone)]
pub enum CoreType {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Fp,
    Bool,
    Str,
    Char,
    Unit,
}

impl CoreType {
    fn parse(lex: &mut Lexer<Token>) -> Option<Self> {
        match lex.slice() {
            "u8" => Some(Self::U8),
            "u16" => Some(Self::U16),
            "u32" => Some(Self::U32),
            "u64" => Some(Self::U64),
            // TODO: Finish this when I'm back on the internet for autocomplete
            _ => None,
        }
    }
}

pub type Identifier = String;

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    Negative,
    Not,
}

impl UnaryOperator {
    fn parse(lex: &mut Lexer<Token>) -> Option<Self> {
        match lex.slice() {
            "-" => Some(UnaryOperator::Negative),
            "!" => Some(UnaryOperator::Not),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Mul,
    Sub,
    Div,
    And,
    Or,
    Append,
}

impl BinaryOperator {
    fn parse(lex: &mut Lexer<Token>) -> Option<Self> {
        match lex.slice() {
            "+" => Some(BinaryOperator::Add),
            "*" => Some(BinaryOperator::Mul),
            "-" => Some(BinaryOperator::Sub),
            "/" => Some(BinaryOperator::Div),
            "&&" => Some(BinaryOperator::And),
            "||" => Some(BinaryOperator::Or),
            "++" => Some(BinaryOperator::Append),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComparisonOperator {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl ComparisonOperator {
    fn parse(lex: &mut Lexer<Token>) -> Option<Self> {
        match lex.slice() {
            "==" => Some(ComparisonOperator::Eq),
            "!=" => Some(ComparisonOperator::Ne),
            "<" => Some(ComparisonOperator::Lt),
            "<=" => Some(ComparisonOperator::Le),
            ">" => Some(ComparisonOperator::Gt),
            ">=" => Some(ComparisonOperator::Ge),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Tokens<'a> {
    data: &'a [Token],
}

impl<'a> Input for Tokens<'a> {
    type Item = Token;
    type Iter = Cloned<Iter<'a, Token>>;
    type IterIndices = Enumerate<Self::Iter>;

    fn input_len(&self) -> usize {
        self.data.len()
    }

    fn take(&self, index: usize) -> Self {
        Tokens {
            data: &self.data[..index],
        }
    }

    fn take_from(&self, index: usize) -> Self {
        Tokens {
            data: &self.data[index..],
        }
    }

    fn take_split(&self, index: usize) -> (Self, Self) {
        (
            Tokens {
                data: &self.data[..index],
            },
            Tokens {
                data: &self.data[index..],
            },
        )
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.data.iter().position(|item| predicate(item.clone()))
    }

    fn iter_elements(&self) -> Self::Iter {
        self.data.iter().cloned()
    }

    fn iter_indices(&self) -> Self::IterIndices {
        self.data.iter().cloned().enumerate()
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if count <= self.data.len() {
            Ok(count)
        } else {
            Err(nom::Needed::Unknown)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let mut lexer = Lexer::new("let x = 5;");

        assert_eq!(lexer.next(), Some(Ok(Token::Keyword(Keyword::Let))));
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("x".to_string()))));
        assert_eq!(lexer.next(), Some(Ok(Token::Assign)));
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::Number(LiteralNumber::DecInt(5))))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::Punctuation(Punctuation::Semicolon)))
        );
    }
}
