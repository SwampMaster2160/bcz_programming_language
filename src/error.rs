use std::fmt::Display;

pub enum Error {
	InvalidShortArgument(String),
	InvalidLongArgument(String),
	NoOptionContinuation,
}

impl Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Error::InvalidShortArgument(arg) => write!(f, "invalid short argument \"{}\"", arg),
			Error::InvalidLongArgument(arg) => write!(f, "invalid long argument \"{}\"", arg),
			Error::NoOptionContinuation => write!(f, "no option continuation"),
		}
	}
}