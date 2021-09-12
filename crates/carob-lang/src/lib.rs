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

mod pos {
	pub type Width = u32;

	pub trait Pos {
		fn from_u32(value: u32) -> Self;
		fn from_usize(value: usize) -> Self;

		fn as_u32(&self) -> u32;
		fn as_usize(&self) -> usize;
	}

	macro_rules! pos {
		( struct $pos:ident ) => {
			#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
			pub struct $pos(pub(crate) $crate::pos::Width);

			impl $pos {
				pub const fn new(value: $crate::pos::Width) -> Self {
					Self(value)
				}
			}

			impl $crate::pos::Pos for $pos {
				fn from_u32(value: u32) -> Self {
					Self(value)
				}

				fn from_usize(value: usize) -> Self {
					Self(value as $crate::pos::Width)
				}

				fn as_u32(&self) -> u32 {
					self.0
				}

				fn as_usize(&self) -> usize {
					self.0 as usize
				}
			}

			impl ::std::fmt::Display for $pos {
				fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
					::std::fmt::Display::fmt(&self.0, f)
				}
			}

			impl ::std::convert::From<u32> for $pos {
				fn from(value: u32) -> Self {
					Self::from_u32(value)
				}
			}

			impl ::std::convert::From<usize> for $pos {
				fn from(value: usize) -> Self {
					Self::from_usize(value)
				}
			}

			impl ::std::convert::From<$pos> for u32 {
				fn from(value: $pos) -> Self {
					value.0
				}
			}

			impl ::std::convert::From<$pos> for usize {
				fn from(value: $pos) -> Self {
					value.0 as usize
				}
			}
		};
	}

	pos!(struct BytePos);
}

mod span {
	use std::ops::Range;

	use crate::pos::BytePos;

	pub type OffsetWidth = i32;

	pub trait Span {
		type Pos: crate::pos::Pos;

		fn with_low(&self, low: Self::Pos) -> Self;
		fn with_high(&self, high: Self::Pos) -> Self;

		fn offset_low(&self, offset: OffsetWidth) -> Self;
		fn offset_high(&self, offset: OffsetWidth) -> Self;
		fn offset(&self, offset: OffsetWidth) -> Self;

		fn union(&self, other: &Self) -> Self;

		fn as_range(&self) -> Range<usize>;
	}

	macro_rules! span {
		( struct $span:ident ( $ty:ty )) => {
			#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
			pub struct $span {
				pub(crate) low: $ty,
				pub(crate) high: $ty,
			}

			impl $span {
				pub fn new(mut low: $ty, mut high: $ty) -> Self {
					if low > high {
						::std::mem::swap(&mut low, &mut high);
					}

					Self { low, high }
				}

				pub fn from_pos<L, H>(low: L, high: H) -> Self
				where
					L: ::std::convert::Into<$ty>,
					H: ::std::convert::Into<$ty>,
				{
					Self::new(low.into(), high.into())
				}
			}

			impl $crate::span::Span for $span {
				type Pos = $ty;

				fn with_low(&self, low: <Self as $crate::span::Span>::Pos) -> Self {
					Self::new(low, self.high)
				}

				fn with_high(&self, high: <Self as $crate::span::Span>::Pos) -> Self {
					Self::new(self.low, high)
				}

				fn offset_low(&self, offset: $crate::span::OffsetWidth) -> Self {
					let low = $crate::pos::Pos::from_u32(
						($crate::pos::Pos::as_u32(&self.low) as i32 + offset) as u32,
					);

					Self::new(low, self.high)
				}

				fn offset_high(&self, offset: $crate::span::OffsetWidth) -> Self {
					let high = $crate::pos::Pos::from_u32(
						($crate::pos::Pos::as_u32(&self.high) as i32 + offset) as u32,
					);

					Self::new(self.low, high)
				}

				fn offset(&self, offset: OffsetWidth) -> Self {
					let low = $crate::pos::Pos::from_u32(
						($crate::pos::Pos::as_u32(&self.low) as i32 + offset) as u32,
					);

					let high = $crate::pos::Pos::from_u32(
						($crate::pos::Pos::as_u32(&self.high) as i32 + offset) as u32,
					);

					Self::new(low, high)
				}

				fn union(&self, other: &Self) -> Self {
					let low = ::std::cmp::min(self.low, other.low);
					let high = ::std::cmp::max(self.high, other.high);

					// NOTE: As these ranges are already checked (low <= high)
					// we dont need to check here again.
					Self { low, high }
				}

				fn as_range(&self) -> ::std::ops::Range<usize> {
					::std::ops::Range {
						start: $crate::pos::Pos::as_usize(&self.low),
						end: $crate::pos::Pos::as_usize(&self.high),
					}
				}
			}

			impl ::std::ops::RangeBounds<$ty> for $span {
				fn start_bound(&self) -> ::std::ops::Bound<&$ty> {
					::std::ops::Bound::Included(&self.low)
				}

				fn end_bound(&self) -> ::std::ops::Bound<&$ty> {
					::std::ops::Bound::Excluded(&self.high)
				}
			}
		};
	}

	span!(struct ByteSpan(BytePos));
}

mod cursor {
	use crate::byteutil;
	use crate::pos::{BytePos, Pos as _};

	#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
	pub struct Cursor<'a> {
		bytes: &'a [u8],
		pos: usize,
	}

	impl<'a> Cursor<'a> {
		pub const fn new(bytes: &'a [u8]) -> Self {
			Self { bytes, pos: 0 }
		}

		pub fn nth(&self, nth: usize) -> Option<u8> {
			(nth < self.bytes.len()).then(|| self.bytes[nth])
		}

		pub fn first(&self) -> Option<u8> {
			self.nth(0)
		}

		pub fn second(&self) -> Option<u8> {
			self.nth(1)
		}

		pub fn third(&self) -> Option<u8> {
			self.nth(1)
		}

		pub fn advance(&mut self, amount: usize) {
			let amount = ::std::cmp::min(amount, self.bytes.len());
			unsafe { self.advance_unchecked(amount) }
		}

		pub unsafe fn advance_unchecked(&mut self, amount: usize) {
			self.pos += amount;
			self.bytes = &self.bytes[amount..];
		}

		pub fn consume(&mut self) -> Option<u8> {
			let consumed = self.first()?;

			debug_assert_eq!(consumed, self.bytes[0]);

			unsafe { self.advance_unchecked(1) };

			Some(consumed)
		}

		pub fn consume_while<F>(&mut self, mut f: F) -> Option<()>
		where
			F: FnMut(u8) -> bool,
		{
			while f(self.first()?) {
				let _ = self.consume().expect("Cursor failed to consume a valid byte");
			}

			Some(())
		}

		pub fn consume_ascii_whitespace(&mut self) -> bool {
			if matches!(self.first(), Some(b) if byteutil::is_ascii_whitespace(b)) {
				unsafe {
					self.advance_unchecked(1);
				}

				true
			} else {
				false
			}
		}

		pub fn consume_utf8_whitespace(&mut self) -> bool {
			self.consume_ascii_whitespace() || {
				let width = match &self.bytes {
					&[0xc2, 0x85, ..] | &[0xc2, 0xa0, ..] => 2,
					&[0xe1, 0x9a, 0x80, ..]
					| &[0xe2, 0x80, 0x80..=0x8a, ..]
					| &[0xe2, 0x80, 0xa8 | 0xa9, ..]
					| &[0xe2, 0x80, 0xaf, ..]
					| &[0xe2, 0x81, 0x9f, ..]
					| &[0xe3, 0x80, 0x80, ..] => 3,
					_ => return false,
				};

				unsafe {
					self.advance_unchecked(width);
				}

				true
			}
		}

		pub fn pos(&self) -> BytePos {
			BytePos::from_usize(self.pos)
		}
	}

	#[cfg(test)]
	mod tests {
		use super::*;

		#[test]
		fn consume() {
			let content = "Hello World!";
			let mut cursor = Cursor::new(content.as_bytes());

			assert_eq!(cursor.first(), Some(b'H'));
			assert_eq!(cursor.consume(), Some(b'H'));

			assert_eq!(cursor.first(), Some(b'e'));
			assert_eq!(cursor.consume(), Some(b'e'));

			assert_eq!(cursor.first(), Some(b'l'));
			assert_eq!(cursor.consume(), Some(b'l'));

			assert_eq!(cursor.first(), Some(b'l'));
			assert_eq!(cursor.consume(), Some(b'l'));

			assert_eq!(cursor.first(), Some(b'o'));
			assert_eq!(cursor.consume(), Some(b'o'));

			assert_eq!(cursor.first(), Some(b' '));
			assert_eq!(cursor.consume(), Some(b' '));

			assert_eq!(cursor.first(), Some(b'W'));
			assert_eq!(cursor.consume(), Some(b'W'));

			assert_eq!(cursor.first(), Some(b'o'));
			assert_eq!(cursor.consume(), Some(b'o'));

			assert_eq!(cursor.first(), Some(b'r'));
			assert_eq!(cursor.consume(), Some(b'r'));

			assert_eq!(cursor.first(), Some(b'l'));
			assert_eq!(cursor.consume(), Some(b'l'));

			assert_eq!(cursor.first(), Some(b'd'));
			assert_eq!(cursor.consume(), Some(b'd'));

			assert_eq!(cursor.first(), Some(b'!'));
			assert_eq!(cursor.consume(), Some(b'!'));

			assert_eq!(cursor.first(), None);
			assert_eq!(cursor.consume(), None);

			assert_eq!(cursor.first(), None);
			assert_eq!(cursor.consume(), None);
		}
	}
}

mod token {
	#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
	pub enum TokenKind {}
}

mod byteutil {
	pub const fn is_ascii(byte: u8) -> bool {
		byte < 128
	}

	// https://en.wikipedia.org/wiki/Whitespace_character
	pub const fn is_ascii_whitespace(byte: u8) -> bool {
		matches!(byte, b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ')
	}

	// https://en.wikipedia.org/wiki/Ascii#Control_characters
	pub const fn is_ascii_control(byte: u8) -> bool {
		matches!(byte, 0..=32 | 127)
	}

	// https://en.wikipedia.org/wiki/Whitespace_character
	pub const fn is_utf8_whitespace(value: u32) -> bool {
		matches!(value, |0xc285| 0xc2a0 | 0xe19a80 | 0xe28080
			..=0xe2808a | 0xe280a8 | 0xe280a9 | 0xe280af | 0xe2819f | 0xe38080)
	}

	pub const fn is_whitespace(first: u8, second: Option<u8>, third: Option<u8>) -> bool {
		is_ascii_whitespace(first)
			|| if let Some(second) = second {
				let value = if let Some(third) = third {
					u32::from_be_bytes([first, second, third, 0])
				} else {
					u32::from_be_bytes([first, second, 0, 0])
				};
				is_utf8_whitespace(value)
			} else {
				false
			}
	}
}

mod lex {
	use crate::cursor::Cursor;
	use crate::pos::BytePos;

	pub struct Lexer<'a> {
		cursor: Cursor<'a>,
	}

	impl<'a> Lexer<'a> {
		pub const fn new(bytes: &'a [u8]) -> Self {
			Self { cursor: Cursor::new(bytes) }
		}

		pub fn from_bytes<B>(bytes: &'a B) -> Self
		where
			B: 'a + AsRef<[u8]>,
		{
			Self::new(bytes.as_ref())
		}

		fn pos(&self) -> BytePos {
			self.cursor.pos()
		}

		fn consume_utf8_whitespaces(&mut self) {
			while self.cursor.consume_utf8_whitespace() {}
		}
	}

	#[cfg(test)]
	mod tests {
		use super::*;

		#[test]
		fn consume_whitespaces() {
			fn test_char(c: char) {
				let s = c.to_string();
				let mut lexer = Lexer::from_bytes(&s);

				lexer.consume_utf8_whitespaces();

				assert_eq!(
					lexer.pos(),
					BytePos(s.len() as u32),
					"Whitespace check for `{:?}` failed",
					c
				);
			}

			#[rustfmt::skip]
			let content = "\t\n\u{000b}\u{000c}\r\u{0085}\u{00a0}\u{1680}\u{2000}\u{2001}\u{2002}\u{2003}\u{2004}\u{2005}\u{2006}\u{2007}\u{2008}\u{2009}\u{200a}\u{2028}\u{2029}\u{202f}\u{205f}\u{3000}";

			content.chars().for_each(|c| test_char(c));

			let mut lexer = Lexer::from_bytes(&content);

			lexer.consume_utf8_whitespaces();

			assert_eq!(
				lexer.pos(),
				BytePos(content.len() as u32),
				"Whitespace check for `{:?}` failed",
				content
			);
		}
	}
}
