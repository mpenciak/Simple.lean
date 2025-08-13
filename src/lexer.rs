//! Here we define the Lexer

use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"\s+")]
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
        Some(String::from(unescaped))
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
        open|close|namespace",
        Keyword::parse
    )]
    Keyword(Keyword),

    // Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    // Comment
    #[regex(r"//[^\n]*\n?", |lex| lex.slice().to_string())]
    LineComment(String),

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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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
            _ => None,
        }
    }
}

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

// pub struct Lexer<'a> {
//     inner: logos::Lexer<'a, Token>,
// }
//
// impl<'a> Lexer<'a> {
//     pub fn new(input: &'a str) -> Self {
//         Self {
//             inner: Token::lexer(input),
//         }
//     }
//
//     pub fn next_token(&mut self) -> Option<Result<Token, LexError>> {
//         self.inner.next().map(|result| {
//             result.map_err(|_| LexError::InvalidToken {
//                 span: self.inner.span(),
//             })
//         })
//     }
//
//     pub fn span(&self) -> std::ops::Range<usize> {
//         self.inner.span()
//     }
// }
//
// #[derive(Debug, Clone, PartialEq)]
// pub enum LexError {
//     InvalidToken { span: std::ops::Range<usize> },
// }
//
// impl std::fmt::Display for LexError {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             LexError::InvalidToken { span } => {
//                 write!(f, "Invalid token at position {}..{}", span.start, span.end)
//             }
//         }
//     }
// }
//
// impl std::error::Error for LexError {}
