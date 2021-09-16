use crate::keyword::Keyword;
use crate::span::ByteSpan;
use crate::token::Token;
use crate::transaction::Flag;

macro_rules! directives {
	(
		$(
			$( #[doc = $doc:literal] )*
			$name:ident {
				$(
					$( #[doc = $attrdoc:literal] )*
					$attrname:ident: $attrty:ty ,
				)+
			} $( : $keyword:path )? ,
		)+
	) => {
		#[derive(Debug, Clone, PartialEq)]
		pub enum DirectiveKind {
			$(
				$( #[doc = $doc] )*
				$name {
					$(
						$( #[doc = $attrdoc] )*
						$attrname: $attrty ,
					)+
				},
			)+
		}

		impl DirectiveKind {
			pub const fn name(&self) -> &'static str {
				match self {
					$( Self::$name { .. }  => stringify!($name) , )+
				}
			}

			pub const fn keyword(&self) -> Option<Keyword> {
				match self {
					$( Self::$name { .. }  =>  directives!(@key $( $keyword )? ) , )+
				}
			}
		}

		impl ::std::fmt::Display for DirectiveKind {
			fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
				f.write_str(self.name())
			}
		}
	};
	(@key $keyword:path) => { Some($keyword) };
	(@key ) => { None };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UncheckedDate {
	pub(crate) year_sign: Option<Token>,
	pub(crate) year: Token,
	pub(crate) month: Token,
	pub(crate) day: Token,
}

impl UncheckedDate {
	pub const fn new(year_sign: Option<Token>, year: Token, month: Token, day: Token) -> Self {
		Self { year_sign, year, month, day }
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UncheckedPath(Vec<Token>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NumberExpr {
	Literal(Token),
	Unary { sign: Token, number: Box<Self> },
	Binary { left: Box<Self>, operand: Token, right: Box<Self> },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UncheckedAmount {
	pub(crate) value: NumberExpr,
	pub(crate) commodity: Token,
}

// TODO: tag values with bytespan
directives! {
	/// `yyyy-mm-dd open SEGMENT:SEGMENT`
	Open {
		date: UncheckedDate,
		account: UncheckedPath,
		commodity_constraints: Vec<Token>,
		booking_method: Option<Token>,
	} : Keyword::Open,

	/// `close`
	Close {
		date: UncheckedDate,
		account: UncheckedPath,
	} : Keyword::Close,

	/// `commodity`
	Commodity {
		date: UncheckedDate,
		commodity: Token,
	} : Keyword::Commodity,

	/// postings of a Transaction
	Posting {
		flag: Flag,
		account: UncheckedPath,
		amount: Option<UncheckedAmount>,
		// TODO: `{cost}`
		// TODO: `@ price`
	},

	/// `txn`
	Transaction {
		date: UncheckedDate,
		flag: Flag,
		payee: Option<UncheckedPath>,
		description: Option<Token>,
		postings: Vec<Directive>,
	} : Keyword::Transaction,

	/// `pushtag`
	PushTag {
		tag: Token,
	} : Keyword::PushTag,

	/// `poptag`
	PopTag {
		tag: Token,
	} : Keyword::PopTag,

	/// `balance`
	Balance {
		date: UncheckedDate,
		account: UncheckedPath,
		amount: UncheckedAmount,
	} : Keyword::Balance,

	/// `pad`
	Pad {
		date: UncheckedDate,
		dst_account: UncheckedPath,
		src_account: UncheckedPath,
	} : Keyword::Pad,

	/// `note`
	Note {
		date: UncheckedDate,
		account: UncheckedPath,
		description: Token,
	} : Keyword::Note,

	/// `document`
	Document {
		date: UncheckedDate,
		account: UncheckedPath,
		document_path: Token,
	} : Keyword::Document,

	/// `price`
	Price {
		date: UncheckedDate,
		commodity: Token,
		price: UncheckedAmount,
	} : Keyword::Price,

	/// `event`
	Event {
		date: UncheckedDate,
		name: Token,
		value: Token,
	} : Keyword::Event,

	/// `query`
	Query {
		date: UncheckedDate,
		name: Token,
		query: Token,
	} : Keyword::Query,

	/// `custom`
	Custom {
		date: UncheckedDate,
		typ: Token,
		// TODO: Vec<Literal>
	} : Keyword::Custom,

	/// `option`
	Option {
		name: Token,
		value: Token,
	} : Keyword::Option,

	/// `plugin`
	Plugin {
		name: Token,
		configuration: Option<Token>,
	} : Keyword::Plugin,

	/// `include`
	Include {
		path: Token,
	} : Keyword::Include,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Directive {
	span: ByteSpan,
	kind: DirectiveKind,
	// TODO: bundle the bellow in Metadata
	// TODO: attributes: AttributeMap,
	// TODO: tags: TagSet,
	// TODO: links: LinkSet,
}

impl Directive {
	pub const fn new(span: ByteSpan, kind: DirectiveKind) -> Self {
		Self { span, kind }
	}

	pub const fn span(&self) -> &ByteSpan {
		&self.span
	}

	pub const fn kind(&self) -> &DirectiveKind {
		&self.kind
	}
}
