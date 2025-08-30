//! This module defines the errors of this crate

mod Lexer {
    pub enum Error {
        All,
    }
}

mod parser {
    use nom::error::{ErrorKind, ParseError};
    pub enum Error {
        All,
    }

    impl<I> ParseError<I> for Error {
        fn from_error_kind(input: I, kind: ErrorKind) -> Self {
            todo!()
        }

        fn append(input: I, kind: ErrorKind, other: Self) -> Self {
            todo!()
        }
    }
}
