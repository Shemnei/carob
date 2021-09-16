use crate::amount::UncheckedAmount;
use crate::commodity::UncheckedCommodity;
use crate::date::UncheckedDate;
use crate::keyword::Keyword;
use crate::metadata::Tag;
use crate::path::UncheckedPath;
use crate::payee::UncheckedPayee;
use crate::span::ByteSpan;
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

// TODO: tag values with bytespan
directives! {
	/// `yyyy-mm-dd open SEGMENT:SEGMENT`
	Open {
		date: UncheckedDate,
		account: UncheckedPath,
		commodity_constraints: Vec<UncheckedCommodity>,
		booking_method: Option<String>,
	} : Keyword::Open,

	/// `close`
	Close {
		date: UncheckedDate,
		account: UncheckedPath,
	} : Keyword::Close,

	/// `commodity`
	Commodity {
		date: UncheckedDate,
		commodity: UncheckedCommodity,
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
		payee: Option<UncheckedPayee>,
		description: Option<String>,
		postings: Vec<Directive>,
	} : Keyword::Transaction,

	/// `pushtag`
	PushTag {
		tag: Tag,
	} : Keyword::PushTag,

	/// `poptag`
	PopTag {
		tag: Tag,
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
		description: String,
	} : Keyword::Note,

	/// `document`
	Document {
		date: UncheckedDate,
		account: UncheckedPath,
		document_path: String,
	} : Keyword::Document,

	/// `price`
	Price {
		date: UncheckedDate,
		commodity: UncheckedCommodity,
		price: UncheckedAmount,
	} : Keyword::Price,

	/// `event`
	Event {
		date: UncheckedDate,
		name: String,
		value: String,
	} : Keyword::Event,

	/// `query`
	Query {
		date: UncheckedDate,
		name: String,
		query: String,
	} : Keyword::Query,

	/// `custom`
	Custom {
		date: UncheckedDate,
		typ: String,
		// TODO: Vec<Literal>
	} : Keyword::Custom,

	/// `option`
	Option {
		name: String,
		value: String,
	} : Keyword::Option,

	/// `plugin`
	Plugin {
		name: String,
		configuration: Option<String>,
	} : Keyword::Plugin,

	/// `include`
	Include {
		path: String,
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
