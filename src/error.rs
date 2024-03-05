use core::fmt;
use crate::syntax::Rule;

pub enum Error {
    // todo use this error or anyhow
    Multiple(Vec<Error>),
    SyntaxError(pest::error::Error<Rule>),
    General(String, (usize, usize)),
}

impl From<pest::error::Error<Rule>> for Error {
    fn from(value: pest::error::Error<Rule>) -> Self {
        Error::SyntaxError(value)
    }
}


impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Multiple(errors) => {
                for error in errors {
                    error.fmt(f)?;
                }
                Ok(())
            },
            Self::SyntaxError(arg0) => arg0.fmt(f),
            Self::General(msg, line_col) => {
                f.write_str(&format!("{msg} ({}:{})", line_col.0, line_col.1))
            }
        }
    }
}

