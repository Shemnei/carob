#![allow(dead_code, rustdoc::private_intra_doc_links)]
#![deny(
    // Documentation
	// TODO: rustdoc::broken_intra_doc_links,
	// TODO: rustdoc::missing_crate_level_docs,
	// TODO: missing_docs,
	// TODO: clippy::missing_docs_in_private_items,

    // Other
	deprecated_in_future,
	exported_private_dependencies,
	future_incompatible,
	missing_copy_implementations,
	missing_debug_implementations,
	private_in_public,
	rust_2018_compatibility,
	rust_2018_idioms,
	trivial_casts,
	trivial_numeric_casts,
	unstable_features,
	unused_import_braces,
	unused_qualifications,

	// clippy attributes
	clippy::missing_const_for_fn,
	clippy::redundant_pub_crate,
	clippy::use_self
)]
#![cfg_attr(docsrs, feature(doc_cfg), feature(doc_alias))]

// TODO: decide how "far" to go with unicode (e.g. accept different unicode digits / quotation marks)
// TODO: make something like small_str
// TODO: add literal enum with:
//    Strings
//    Accounts
//    Currency
//    Dates (datetime.date)
//    Tags
//    Numbers (Decimal)
//    Amount (beancount.core.amount.Amount)
//    Boolean
// TODO: intern all literals as they may appear often
// TODO: be able to define decimal separator

// FEATURE STOP
// TODO: split into files
// TODO: switch from byte cursor to char cursor for Lexer
// TODO: Parser should emit Directives (statement)
// TODO: There soulde also be types (e.g. date)

pub(crate) mod diagnostic;
pub(crate) mod directive;
pub(crate) mod keyword;
pub(crate) mod lex;
pub(crate) mod parse;
pub(crate) mod pos;
pub(crate) mod session;
pub(crate) mod source;
pub(crate) mod span;
pub(crate) mod token;

#[cfg(test)]
mod demo;

pub(crate) mod util {
	pub mod ascii {
		pub const fn is_ascii(byte: u8) -> bool {
			byte < 128
		}

		/// # Link
		///
		/// <https://en.wikipedia.org/wiki/Ascii#Control_characters>
		pub const fn is_ascii_control(byte: u8) -> bool {
			matches!(byte, 0..=32 | 127)
		}

		/// # Note
		///
		/// This does __not__ match end-of-line characters.
		///
		/// # Link
		///
		/// <https://en.wikipedia.org/wiki/Whitespace_character>
		pub const fn is_ascii_whitespace(byte: u8) -> bool {
			matches!(byte, b'\t' | b' ')
		}

		/// # Note
		///
		/// This does __not__ match white space characters.
		///
		/// # Link
		///
		/// <https://en.wikipedia.org/wiki/Newline#Unicode>
		pub const fn is_ascii_eol(byte: u8) -> bool {
			matches!(byte, b'\n' | b'\x0b' | b'\x0c' | b'\r')
		}

		pub const fn is_ascii_quotation_mark(byte: u8) -> bool {
			matches!(byte, b'"' | b'\'')
		}

		pub const fn get_closing_quotation_mark(byte: u8) -> Option<u8> {
			let value = match byte {
				b'"' => b'"',
				b'\'' => b'\'',
				_ => return None,
			};

			Some(value)
		}

		pub const fn is_ascii_escape(byte: u8) -> bool {
			matches!(byte, b'\\')
		}

		pub const fn is_ascii_digit(byte: u8) -> bool {
			matches!(byte, b'0'..=b'9')
		}

		pub const fn is_ascii_number_ignore(byte: u8) -> bool {
			matches!(byte, b'_' | b',')
		}

		pub const fn is_ascii_number_decimal_separator(byte: u8) -> bool {
			matches!(byte, b'.')
		}

		pub const fn is_ascii_number_sign_negative(byte: u8) -> bool {
			matches!(byte, b'-')
		}

		pub const fn is_ascii_number_sign_positive(byte: u8) -> bool {
			matches!(byte, b'+')
		}

		pub const fn is_ascii_number_sign(byte: u8) -> bool {
			is_ascii_number_sign_negative(byte) || is_ascii_number_sign_positive(byte)
		}

		pub const fn is_ascii_number_exponent(byte: u8) -> bool {
			matches!(byte, b'e' | b'E')
		}

		#[cfg(test)]
		mod tests {
			use super::*;

			#[test]
			fn quotation_mark_opening_has_closing() {
				for byte in 0..255 {
					if !is_ascii(byte) {
						break;
					}

					if is_ascii_quotation_mark(byte) {
						assert!(get_closing_quotation_mark(byte).is_some());
					}
				}
			}
		}
	}

	pub mod utf8 {
		/// Script to convert unicode value to bytes:
		///
		/// ```python
		/// '\u2028'.encode('utf-8')
		/// ```

		pub const fn unicode_len(byte: u8) -> Option<usize> {
			if is_unicode_start(byte) {
				Some((byte >> 4).count_ones() as usize)
			} else {
				None
			}
		}

		pub const fn is_unicode_start(byte: u8) -> bool {
			matches!(byte >> 4, 0b1100 | 0b1110 | 0b1111)
		}

		pub const fn is_unicode_continuation(byte: u8) -> bool {
			byte & 0b1100_0000 == 0b1000_0000
		}

		/// # Link
		///
		/// <https://en.wikipedia.org/wiki/Unicode_control_characters>
		pub fn is_unicode_control(_value: u32) -> bool {
			todo!()
		}

		/// # Note
		///
		/// This does __not__ match end-of-line characters.
		///
		/// # Link
		///
		/// <https://en.wikipedia.org/wiki/Whitespace_character>
		pub const fn is_unicode_whitespace(value: u32) -> bool {
			matches!(
				value,
				0xc2a0 | 0xe19a80 | 0xe28080..=0xe2808a | 0xe280af | 0xe2819f | 0xe38080
			)
		}

		/// # Note
		///
		/// This does __not__ match white space characters.
		///
		/// # Link
		///
		/// <https://en.wikipedia.org/wiki/Newline#Unicode>
		pub const fn is_unicode_eol(value: u32) -> bool {
			matches!(value, 0xc285 | 0xe280a8 | 0xe280a9)
		}
	}

	pub mod slice {
		/// # Note
		///
		/// This does __not__ match end-of-line characters.
		pub const fn is_any_whitespace(bytes: &[u8]) -> Option<usize> {
			match *bytes {
				// Ascii
				[b'\t' | b' ', ..] => Some(1),
				// Unicode - 2b
				[b'\xc2', b'\xa0', ..] => Some(2),
				// Unicode - 3b
				[b'\xe1', b'\x9a', b'\x80', ..]
				| [b'\xe2', b'\x80', b'\x80'..=b'\x8a', ..]
				| [b'\xe2', b'\x80', b'\xaf', ..]
				| [b'\xe2', b'\x81', b'\x9f', ..]
				| [b'\xe3', b'\x80', b'\x80', ..] => Some(3),
				_ => None,
			}
		}

		/// # Note
		///
		/// This does __not__ match white space characters.
		pub const fn is_any_eol(bytes: &[u8]) -> Option<usize> {
			match *bytes {
				// Ascii
				[b'\n' | b'\x0b' | b'\x0c' | b'\r', ..] => Some(1),
				// Unicode - 2b
				[b'\xc2', b'\x85', ..] => Some(2),
				// Unicode - 3b
				[b'\xe2', b'\x80', b'\xa8' | b'\xa9', ..] => Some(3),
				_ => None,
			}
		}
	}
}

mod date {
	use std::fmt;

	#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
	pub struct UncheckedDate {
		pub(crate) year: i16,
		pub(crate) month: u8,
		pub(crate) day: u8,
	}

	impl UncheckedDate {
		pub const fn from_ymd(year: i16, month: u8, day: u8) -> Self {
			Self { year, month, day }
		}
	}

	impl fmt::Display for UncheckedDate {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			write!(f, "{}-{}-{}", self.year, self.month, self.day)
		}
	}
}

mod path {
	use std::fmt;

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct UncheckedPath {
		segments: Vec<String>,
	}

	impl fmt::Display for UncheckedPath {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			f.write_str(self.segments.join(":").as_str())
		}
	}
}

mod commodity {
	use std::fmt;

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct UncheckedCommodity(String);

	impl fmt::Display for UncheckedCommodity {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			f.write_str(&self.0)
		}
	}
}

mod payee {
	use std::fmt;

	use crate::path::UncheckedPath;

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub enum UncheckedPayee {
		String(String),
		Path(UncheckedPath),
	}

	impl fmt::Display for UncheckedPayee {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			match self {
				Self::String(s) => f.write_str(s),
				Self::Path(p) => fmt::Display::fmt(p, f),
			}
		}
	}
}

mod amount {
	use std::fmt;

	use crate::commodity::UncheckedCommodity;

	#[derive(Debug, Clone, PartialEq)]
	pub struct UncheckedAmount {
		// TODO: Use BigF/f64/i64 enum
		value: f64,
		commodity: UncheckedCommodity,
	}

	impl fmt::Display for UncheckedAmount {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			write!(f, "{} {}", self.value, self.commodity)
		}
	}
}

mod transaction {
	use std::fmt;

	#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
	pub enum Flag {
		Completed,
		Incomplete,
	}

	impl Flag {
		pub const fn symbol(&self) -> char {
			match self {
				Self::Completed => '*',
				Self::Incomplete => '!',
			}
		}
	}

	impl Default for Flag {
		fn default() -> Self {
			Self::Completed
		}
	}

	impl fmt::Display for Flag {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			f.write_str(self.symbol().to_string().as_str())
		}
	}
}

mod metadata {
	use std::fmt;

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct Tag(pub(crate) String);

	impl fmt::Display for Tag {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			f.write_str(&self.0)
		}
	}

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct Link(pub(crate) String);

	impl fmt::Display for Link {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			f.write_str(&self.0)
		}
	}

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct Attribute {
		pub(crate) key: String,
		pub(crate) value: String,
	}

	impl fmt::Display for Attribute {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			write!(f, "{}: {}", self.key, self.value)
		}
	}
}
