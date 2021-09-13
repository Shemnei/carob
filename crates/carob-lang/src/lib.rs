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

#[cfg(test)]
mod demo;

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

			impl ::std::fmt::Display for $span {
				fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
					write!(f, "{}..{}", self.low, self.high)
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

		pub fn consume_utf8_char(&mut self) -> Option<Result<u32, String>> {
			// TODO: tests
			// TODO: proper error struct
			macro_rules! bubble {
				( $res:expr ) => {
					match $res {
						Ok(x) => x,
						Err(err) => return Some(Err(err)),
					}
				};
			}

			let first = self.nth(0)?;

			let (len, value) = if first >= 0b01_00_0000 {
				// Is unicode
				let len = match first >> 4 {
					0b1100 => 2,
					0b1110 => 3,
					0b1111 => 4,
					_ => 0, // invalid length
				};

				if len < 2 {
					// Either stepped in mid sequence or invalid utf8
					return Some(Err(format!(
						"Found invalid length for utf-8 unicode character `{}`",
						len
					)));
				}

				let mut value = (first & 0b1111) as u32;

				for idx in 1..=len {
					let byte = bubble!(self.nth(idx).ok_or_else(|| format!(
						"Expected utf-8 unicode character with length `{}` but got eof at length \
						 `{}`",
						len, idx
					)));

					if byte & 0b10_00_0000 != 0b10_00_0000 {
						return Some(Err(format!(
							"Expected utf-8 unicode character with length `{}` but got non-utf8 \
							 character at length `{}`",
							len, idx
						)));
					}

					value <<= 6;
					value |= (byte & 0b00_11_1111) as u32;
				}

				(len, value)
			} else {
				// Is ascii
				(1, first as u32)
			};

			unsafe {
				self.advance_unchecked(len);
			}

			Some(Ok(value))
		}

		pub fn consume_while<F>(&mut self, mut f: F) -> bool
		where
			F: FnMut(u8) -> bool,
		{
			while let Some(byte) = self.first() {
				if f(byte) {
					let _ = self.consume().expect("Cursor failed to consume a valid byte");
				} else {
					return false;
				}
			}

			true
		}

		/// # NOTE
		///
		/// This does __not__ consume an end-of-line character.
		pub fn consume_ascii_whitespace_no_eol(&mut self) -> bool {
			if matches!(self.first(), Some(b) if byteutil::is_ascii_whitespace_no_eol(b)) {
				unsafe {
					self.advance_unchecked(1);
				}

				true
			} else {
				false
			}
		}

		/// # NOTE
		///
		/// This does consume an end-of-line character.
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

		/// # NOTE
		///
		/// This does __not__ consume an end-of-line character.
		pub fn consume_utf8_whitespace_no_eol(&mut self) -> bool {
			let width = match &self.bytes {
				&[0xc2, 0xa0, ..] => 2,
				&[0xe1, 0x9a, 0x80, ..]
				| &[0xe2, 0x80, 0x80..=0x8a, ..]
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

		/// # NOTE
		///
		/// This does consume an end-of-line character.
		pub fn consume_utf8_whitespace(&mut self) -> bool {
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

		/// # NOTE
		///
		/// This does __not__ consume a new line character `\n` as this is a
		/// special token.
		pub fn consume_any_whitespace_no_eol(&mut self) -> bool {
			self.consume_ascii_whitespace_no_eol() || self.consume_utf8_whitespace_no_eol()
		}

		/// # NOTE
		///
		/// This does consume a new line character `\n`.
		pub fn consume_any_whitespace(&mut self) -> bool {
			self.consume_ascii_whitespace() || self.consume_utf8_whitespace()
		}

		/// # NOTE
		///
		/// This does __not__ consume a new line character `\n` as this is a
		/// special token.
		pub fn consume_any_none_whitespace(&mut self) -> bool {
			if matches!(self.first(), Some(b) if !byteutil::is_ascii_whitespace(b))
				&& !matches!(
					self.bytes,
					&[0xc2, 0x85, ..]
						| &[0xc2, 0xa0, ..] | &[0xe1, 0x9a, 0x80, ..]
						| &[0xe2, 0x80, 0x80..=0x8a, ..]
						| &[0xe2, 0x80, 0xa8 | 0xa9, ..]
						| &[0xe2, 0x80, 0xaf, ..]
						| &[0xe2, 0x81, 0x9f, ..]
						| &[0xe3, 0x80, 0x80, ..]
				) {
				unsafe {
					self.advance_unchecked(1);
				}

				true
			} else {
				false
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

mod byteutil {
	pub const fn is_ascii(byte: u8) -> bool {
		byte < 128
	}

	// https://en.wikipedia.org/wiki/Whitespace_character
	/// # NOTE
	///
	/// This does __not__ match end-of-line characters as these are
	/// special tokens.
	pub const fn is_ascii_whitespace_no_eol(byte: u8) -> bool {
		matches!(byte, b'\t' | b' ')
	}

	pub const fn is_ascii_whitespace(byte: u8) -> bool {
		is_ascii_whitespace_no_eol(byte) || matches!(byte, b'\n' | b'\x0b' | b'\x0c' | b'\r')
	}

	// https://en.wikipedia.org/wiki/Ascii#Control_characters
	pub const fn is_ascii_control(byte: u8) -> bool {
		matches!(byte, 0..=32 | 127)
	}

	/// # NOTE
	///
	/// This does __not__ match end-of-line characters as these are
	/// special tokens.
	pub const fn is_utf8_whitespace_no_eol(value: u32) -> bool {
		matches!(value, |0xc2a0| 0xe19a80 | 0xe28080..=0xe2808a | 0xe280af | 0xe2819f | 0xe38080)
	}

	// https://en.wikipedia.org/wiki/Whitespace_character
	pub const fn is_utf8_whitespace(value: u32) -> bool {
		is_utf8_whitespace_no_eol(value)
			|| matches!(value, |0xc285| 0xc2a0 | 0xe19a80 | 0xe28080
				..=0xe2808a | 0xe280a8 | 0xe280a9 | 0xe280af | 0xe2819f | 0xe38080)
	}

	pub const fn is_ascii_digit(byte: u8) -> bool {
		matches!(byte, b'0'..=b'9')
	}

	pub const fn is_ascii_quotation_mark(byte: u8) -> bool {
		// 0-9
		matches!(byte, b'"')
	}

	pub const fn get_closing_quotation_mark(byte: u8) -> Option<u8> {
		// 0-9
		let closing = match byte {
			b'"' => b'"',
			_ => return None,
		};

		Some(closing)
	}

	pub const fn is_ascii_escape(byte: u8) -> bool {
		matches!(byte, b'\\')
	}

	pub const fn is_ascii_number_ignore(byte: u8) -> bool {
		matches!(byte, b'_' | b',')
	}

	pub const fn is_ascii_number_decimal_separator(byte: u8) -> bool {
		matches!(byte, b'.')
	}

	pub const fn is_ascii_number_sign_negative(byte: u8) -> bool {
		matches!(byte, b'-' | b'+')
	}

	pub const fn is_ascii_number_sign(byte: u8) -> bool {
		is_ascii_number_sign_negative(byte) || matches!(byte, b'+')
	}

	pub const fn is_ascii_number_exponent(byte: u8) -> bool {
		matches!(byte, b'e')
	}
}

mod token {
	use crate::span::ByteSpan;

	macro_rules! tokens {
		(
			$(
				$( #[doc = $doc:literal] )*
				$name:ident $( { $( $tyname:ident : $ty:ty )+ } )?,
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
						$( Self::$name $( { $( $tyname , )+ } )? => stringify!($name), )+
					}
				}
			}

			impl ::std::fmt::Display for TokenKind {
				fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
					#[allow(unused_variables)]
					match self {
						$( Self::$name $( { $( $tyname , )+ } )? => f.write_str(self.name()), )+
					}
				}
			}
		};
	}

	tokens! {
		// Single byte tokens
		/// `(`
		LeftParen,
		/// `)`
		RightParen,
		/// `{`
		LeftBrace,
		/// `}`
		RightBrace,
		/// `,`
		Comma,
		/// `.`
		Dot,
		/// `-`
		Hyphen,
		/// `+`
		Plus,
		/// `:`
		Colon,
		/// `;`
		Semicolon,
		/// `/`
		Slash,
		/// `*`
		Asterisk,
		/// `^`
		Caret,
		/// `!`
		ExclamationMark,
		/// `@`
		AtSign,
		/// e.g. `\n`
		Eol,

		// Identifier
		/// In this phase everything is an identifier.
		/// e.g. `option`
		Ident,

		// Literals
		Literal{ kind: LiteralKind},
		// CHECK: Dates, Paths, Tags, Links, boolean, Key
		// These could also be just in the parser

		// End of file
		Eof,

		// Unknown
		Unknown,
	}

	#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
	pub enum LiteralKind {
		/// e.g. `2`
		Integer,
		/// e.g. `2.0`
		Float,
		/// "..."
		String { terminated: bool },
	}

	#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
	pub struct Token {
		span: ByteSpan,
		kind: TokenKind,
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
}

mod lex {
	use crate::byteutil;
	use crate::cursor::Cursor;
	use crate::pos::BytePos;
	use crate::span::ByteSpan;
	use crate::token::{LiteralKind, Token, TokenKind};

	#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

		pub fn next_token(&mut self) -> Token {
			self.consume_whitespaces_no_eol();

			let start = self.cursor.pos();

			// TODO: be able to define decimal separator

			// 1) Check 1 byte symbols
			// 2) Check String start
			// 3) Check Number start
			// 4) Check Keywords
			// 5) Is literal
			match (self.cursor.consume(), self.cursor.first()) {
				(Some(b'('), _) => self.emit_token(start, TokenKind::LeftParen),
				(Some(b')'), _) => self.emit_token(start, TokenKind::RightParen),
				(Some(b'{'), _) => self.emit_token(start, TokenKind::LeftBrace),
				(Some(b'}'), _) => self.emit_token(start, TokenKind::RightParen),
				(Some(b','), _) => self.emit_token(start, TokenKind::Comma),
				// TODO: Check if digit after that and if so parse as number
				(Some(b'.'), _) => self.emit_token(start, TokenKind::Dot),
				(Some(b'-'), _) => self.emit_token(start, TokenKind::Hyphen),
				(Some(b'+'), _) => self.emit_token(start, TokenKind::Plus),
				(Some(b':'), _) => self.emit_token(start, TokenKind::Colon),
				(Some(b';'), _) => self.emit_token(start, TokenKind::Semicolon),
				(Some(b'/'), _) => self.emit_token(start, TokenKind::Slash),
				(Some(b'*'), _) => self.emit_token(start, TokenKind::Asterisk),
				(Some(b'^'), _) => self.emit_token(start, TokenKind::Caret),
				(Some(b'!'), _) => self.emit_token(start, TokenKind::ExclamationMark),
				(Some(b'@'), _) => self.emit_token(start, TokenKind::AtSign),
				// EOL
				(Some(b'\n'), _) | (Some(b'\x0b'), _) | (Some(b'\x0c'), _) | (Some(b'\r'), _) => {
					self.emit_token(start, TokenKind::Eol)
				}
				(Some(b'\xc2'), Some(b'\x85')) => {
					// Consume second byte
					unsafe {
						self.cursor.advance_unchecked(1);
					}

					self.emit_token(start, TokenKind::Eol)
				}
				(Some(b'\xe2'), Some(b'\x80'))
					if matches!(self.cursor.third(), Some(b'\xa8' | b'\xa9')) =>
				{
					// Consume second and third byte
					unsafe {
						self.cursor.advance_unchecked(2);
					}

					self.emit_token(start, TokenKind::Eol)
				}

				// Number
				(Some(b), _) if byteutil::is_ascii_digit(b) => self.lex_number(start, b),

				// String
				(Some(b), _) if byteutil::is_ascii_quotation_mark(b) => self.lex_string(start, b),

				// Identifier
				(Some(_), _) => self.lex_ident(start),

				// Eof
				(None, _) => self.emit_eof(start),
			}
		}

		// [0-9]+(.[0-9]*)?([+-]e[0-9]+)
		fn lex_number(&mut self, start: BytePos, first: u8) -> Token {
			// e.g. 1_100,000.10e-10
			// after e no more `.`

			debug_assert!(byteutil::is_ascii_digit(first));

			let mut kind = LiteralKind::Integer;

			let _ = self.cursor.consume_while(|b| {
				byteutil::is_ascii_digit(b) || byteutil::is_ascii_number_ignore(b)
			});

			if matches!(self.cursor.first(), Some(b) if byteutil::is_ascii_number_decimal_separator(b))
			{
				kind = LiteralKind::Float;

				let separator = self.cursor.consume();
				debug_assert!(
					matches!(separator, Some(b) if byteutil::is_ascii_number_decimal_separator(b))
				);

				let _ = self.cursor.consume_while(|b| {
					byteutil::is_ascii_digit(b) || byteutil::is_ascii_number_ignore(b)
				});
			}

			if matches!(self.cursor.first(), Some(b) if byteutil::is_ascii_number_exponent(b))
				|| matches!((self.cursor.first(), self.cursor.second()), (Some(a), Some(b)) if byteutil::is_ascii_number_sign(a) && byteutil::is_ascii_number_exponent(b))
			{
				// TODO: check that there is a digit after the exponent before consuming
				if matches!(self.cursor.first(), Some(b) if byteutil::is_ascii_number_sign(b)) {
					if matches!(self.cursor.first(), Some(b) if byteutil::is_ascii_number_sign_negative(b))
					{
						kind = LiteralKind::Float;
					}

					let sign = self.cursor.consume();
					debug_assert!(matches!(sign, Some(b) if byteutil::is_ascii_number_sign(b)));
				}

				let exponent = self.cursor.consume();
				debug_assert!(matches!(exponent, Some(b) if byteutil::is_ascii_number_exponent(b)));

				let _ = self.cursor.consume_while(|b| {
					byteutil::is_ascii_digit(b) || byteutil::is_ascii_number_ignore(b)
				});
			}

			self.emit_token(start, TokenKind::Literal { kind })
		}

		fn lex_string(&mut self, start: BytePos, first: u8) -> Token {
			debug_assert!(byteutil::is_ascii_quotation_mark(first));

			let closing = byteutil::get_closing_quotation_mark(first)
				.expect("Found no closing quotation mark");

			let mut terminated = false;

			while let Some(byte) = self.cursor.consume() {
				if byteutil::is_ascii_escape(byte) {
					if self.cursor.consume().is_none() {
						// Consume escaped byte. We dont care about utf-8 unicode
						// as these bytes would always be than any ascii char.
						break;
					}
				} else if byte == closing {
					terminated = true;
					break;
				}
			}

			self.emit_token(start, TokenKind::Literal { kind: LiteralKind::String { terminated } })
		}

		fn lex_ident(&mut self, start: BytePos) -> Token {
			while self.cursor.consume_any_none_whitespace() {}
			self.emit_token(start, TokenKind::Ident)
		}

		fn emit_token(&self, start: BytePos, kind: TokenKind) -> Token {
			Token::new(ByteSpan::new(start, self.cursor.pos()), kind)
		}

		fn emit_eof(&self, start: BytePos) -> Token {
			self.emit_token(start, TokenKind::Eof)
		}

		fn pos(&self) -> BytePos {
			self.cursor.pos()
		}

		fn consume_whitespaces_no_eol(&mut self) {
			while self.cursor.consume_any_whitespace_no_eol() {}
		}
	}

	#[cfg(test)]
	mod tests {
		use super::*;

		#[test]
		fn consume_whitespaces() {
			fn test_whitespace_char(c: char) {
				let s = c.to_string();
				let mut lexer = Lexer::from_bytes(&s);

				lexer.consume_whitespaces_no_eol();

				assert_eq!(
					lexer.pos(),
					BytePos(s.len() as u32),
					"Whitespace check for `{:?}` failed",
					c
				);
			}

			fn test_eol_char(c: char) {
				let s = c.to_string();
				let mut lexer = Lexer::from_bytes(&s);

				lexer.consume_whitespaces_no_eol();

				assert_eq!(lexer.pos(), BytePos(0), "Eol check for `{:?}` failed", c);
			}

			#[rustfmt::skip]
			let whitespace_content = "\t\u{00a0}\u{1680}\u{2000}\u{2001}\u{2002}\u{2003}\u{2004}\u{2005}\u{2006}\u{2007}\u{2008}\u{2009}\u{200a}\u{202f}\u{205f}\u{3000}";

			whitespace_content.chars().for_each(|c| test_whitespace_char(c));

			#[rustfmt::skip]
			let eol_content = "\n\u{000b}\u{000c}\r\u{0085}\u{2028}\u{2029}";
			eol_content.chars().for_each(|c| test_eol_char(c));
		}

		#[test]
		fn lex_string() {
			let content = r#""Hello \" World""#;

			let mut lexer = Lexer::new(&content.as_bytes()[1..]);

			lexer.lex_string(BytePos(0), b'"');

			assert_eq!(
				lexer.pos(),
				BytePos((content.len() - 1) as u32),
				"String lex check for `{:?}` failed",
				content
			);
		}
	}
}

mod keywords {
	macro_rules! keywords {
		(
			$(
				$( #[doc = $doc:literal] )*
				$name:ident $(= $strrep:literal)?,
			)+
		) => {
			#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
			pub enum Keyword {
				$(
					$( #[doc = $doc] )*
					$name ,
				)+
			}
		};
	}

	keywords! {
		/// `open`
		Open,
		/// `close`
		Close,
		/// `commodity`
		Commodity,
		/// `txn`
		Transaction = "txn",
		/// `pushtag`
		PushTag,
		/// `poptag`
		PopTag,
		/// `balance`
		Balance,
		/// `pad`
		Pad,
		/// `note`
		Note,
		/// `document`
		Document,
		/// `price`
		Price,
		/// `event`
		Event,
		/// `query`
		Query,
		/// `custom`
		Custom,
		/// `option`
		Option,
		/// `plugin`
		Plugin,
		/// `include`
		Include,
	}
}
