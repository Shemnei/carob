use crate::span::ByteSpan;

macro_rules! tokens {
	(
		$(
			$( #[doc = $doc:literal] )*
			$name:ident $( { $( $tyname:ident : $ty:ty )+ } )? $( = $symbol:literal )?,
		)+
	) => {
		#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
		pub enum TokenKind {
			$(
				$( #[doc = $doc] )*
				$name $( { $( $tyname : $ty )+ } )?,
			)+
		}

		impl TokenKind {
			pub const fn name(&self) -> &'static str {
				#[allow(unused_variables)]
				match self {
					$( Self::$name $( { $( $tyname , )+ } )? => stringify!($name) , )+
				}
			}

			pub const fn symbol(&self) -> Option<char> {
				#[allow(unused_variables)]
				match self {
					$( Self::$name $( { $( $tyname , )+ } )? => tokens!(@nvl $( $symbol )? ) , )+
				}
			}
		}

		impl ::std::fmt::Display for TokenKind {
			fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
				f.write_str(self.name())
			}
		}
	};
	(@nvl $symbol:literal) => { Some($symbol) };
	(@nvl) => { None };
}

tokens! {
	// Single byte tokens
	/// `(`
	LeftParen = '(',
	/// `)`
	RightParen = ')',
	/// `{`
	LeftBrace = '{',
	/// `}`
	RightBrace = '}',
	/// `,`
	Comma = ',',
	/// `.`
	Dot = '.',
	/// `-`
	Hyphen = '-',
	/// `+`
	Plus = '+',
	/// `:`
	Colon = ':',
	/// `;`
	Semicolon = ';',
	/// `/`
	Slash = '/',
	/// `*`
	Asterisk = '*',
	/// `^`
	Caret = '^',
	/// `!`
	ExclamationMark = '!',
	/// `@`
	AtSign = '@',
	/// e.g. `\n`
	Eol = '\n',

	// Identifier
	/// In this phase everything is an identifier.
	/// e.g. `option`
	Ident,

	// Literals
	Literal { kind: LiteralKind },
	// CHECK: Dates, Paths, Tags, Links, boolean, Key
	// These could also be just in the parser

	Unknown,

	// End of file
	Eof = '\0',

	// Invisible
	Whitespace,
	Comment,
}

impl TokenKind {
	pub const fn literal(kind: LiteralKind) -> Self {
		Self::Literal { kind }
	}

	pub const fn kind_str(&self) -> &str {
		if let Self::Literal { kind } = self {
			kind.name()
		} else {
			self.name()
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LiteralKind {
	/// e.g. `2`
	Integer,
	/// e.g. `2.0`
	Float,
	/// "..."
	String { terminated: bool },
	/// true/false
	Boolean { value: bool },
}

impl LiteralKind {
	pub const fn name(&self) -> &'static str {
		match self {
			LiteralKind::Integer => "Integer",
			LiteralKind::Float => "Float",
			LiteralKind::String { .. } => "String",
			LiteralKind::Boolean { .. } => "Boolean",
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token {
	pub(crate) span: ByteSpan,
	pub(crate) kind: TokenKind,
}

impl Token {
	pub const fn new(span: ByteSpan, kind: TokenKind) -> Self {
		Self { span, kind }
	}

	pub const fn span(&self) -> &ByteSpan {
		&self.span
	}

	pub const fn kind(&self) -> &TokenKind {
		&self.kind
	}
}
