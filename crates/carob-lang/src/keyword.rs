macro_rules! keywords {
	(
		$(
			$( #[doc = $doc:literal] )*
			$name:ident = $strrep:literal,
		)+
	) => {
		#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
		pub enum Keyword {
			$(
				$( #[doc = $doc] )*
				$name ,
			)+
		}

		impl Keyword {
			pub fn from_str_opt(s: &str) -> ::std::option::Option<Self> {
				match s {
					$( $strrep => Some(Self::$name) , )+
					_ => None,
				}
			}
		}
	};
}

keywords! {
	/// `open`
	Open = "open",
	/// `close`
	Close = "close",
	/// `commodity`
	Commodity = "commodity",
	/// `txn`
	Transaction = "txn",
	/// `pushtag`
	PushTag = "pushtag",
	/// `poptag`
	PopTag = "poptag",
	/// `balance`
	Balance = "balance",
	/// `pad`
	Pad = "pad",
	/// `note`
	Note = "note",
	/// `document`
	Document = "document",
	/// `price`
	Price = "price",
	/// `event`
	Event = "event",
	/// `query`
	Query = "query",
	/// `custom`
	Custom = "custom",
	/// `option`
	Option = "option",
	/// `plugin`
	Plugin = "plugin",
	/// `include`
	Include = "include",
}
