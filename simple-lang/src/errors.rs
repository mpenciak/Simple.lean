//! This module defines the errors of this crate

pub mod lexer {
    pub enum Error {
        All,
    }
}

pub mod parser {
    use nom::error::{ContextError, ErrorKind, FromExternalError, ParseError};

    use crate::lexer::Keyword;

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum Error<I> {
        Nom {
            kind: ErrorKind,
            at: I,
        },
        ExpectedKeyword {
            expected: Vec<Keyword>,
            found: Keyword,
        },
        Tag {
            at: I,
        },
    }

    impl<I> Error<I> {
        pub fn expected_keyword(expected: Vec<Keyword>, found: Keyword) -> Self {
            Self::ExpectedKeyword { expected, found }
        }

        pub fn from_error_kind(input: I, kind: ErrorKind) -> Self {
            Self::Nom { kind, at: input }
        }

        pub fn eof(input: I) -> Self {
            Self::Nom {
                kind: ErrorKind::Eof,
                at: input,
            }
        }

        pub fn tag(at: I) -> Self {
            Self::Tag { at }
        }
    }

    impl<I> ParseError<I> for Error<I> {
        fn from_error_kind(input: I, kind: ErrorKind) -> Self {
            Self::Nom { kind, at: input }
        }

        fn append(_input: I, _kind: ErrorKind, other: Self) -> Self {
            other // NOTE: this looks like the standard way of doing it, but it
                  // might be worth exploring alternatives
        }
    }

    impl<I> ContextError<I> for Error<I> {
        fn add_context(_input: I, _ctx: &'static str, other: Self) -> Self {
            other // NOTE: this looks like the standard way of doing it, but it
                  // might be worth exploring alternatives
        }
    }

    impl<I, E> FromExternalError<I, E> for Error<I> {
        fn from_external_error(input: I, kind: ErrorKind, _e: E) -> Self {
            Self::Nom { kind, at: input } // NOTE: this looks like the standard way of doing it, but it
                                          // might be worth exploring alternatives
        }
    }
}
