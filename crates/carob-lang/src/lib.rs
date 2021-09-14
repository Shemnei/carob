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

			macro_rules! display {
				( $trait:path ) => {
					impl $trait for $pos {
						fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
							<_>::fmt(&self.0, f)
						}
					}
				};
			}

			display!(::std::fmt::Display);
			display!(::std::fmt::Binary);
			display!(::std::fmt::Octal);
			display!(::std::fmt::LowerHex);
			display!(::std::fmt::UpperHex);
			display!(::std::fmt::LowerExp);
			display!(::std::fmt::UpperExp);

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

			impl ::std::convert::From<($ty, $ty)> for $span {
				fn from((low, high): ($ty, $ty)) -> Self {
					Self::new(low, high)
				}
			}

			impl ::std::convert::From<::std::ops::Range<$ty>> for $span {
				fn from(range: ::std::ops::Range<$ty>) -> Self {
					Self::new(range.start, range.end)
				}
			}
		};
	}

	span!(struct ByteSpan(BytePos));
}

mod cursor {
	use crate::pos::{BytePos, Pos as _};
	use crate::util::slice;

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

		pub fn consume_until<F>(&mut self, mut f: F) -> bool
		where
			F: FnMut(u8) -> bool,
		{
			while let Some(byte) = self.first() {
				if !f(byte) {
					let _ = self.consume().expect("Cursor failed to consume a valid byte");
				} else {
					return false;
				}
			}

			true
		}

		pub fn consume_any_whitespace(&mut self) -> bool {
			if let Some(width) = slice::is_any_whitespace(self.bytes) {
				unsafe {
					self.advance_unchecked(width);
				}

				true
			} else {
				false
			}
		}

		pub fn consume_any_eol(&mut self) -> bool {
			if let Some(width) = slice::is_any_eol(self.bytes) {
				unsafe {
					self.advance_unchecked(width);
				}

				true
			} else {
				false
			}
		}

		pub fn consume_until_eol(&mut self) -> bool {
			while self.first().is_some() {
				if slice::is_any_eol(self.bytes).is_some() {
					return false;
				} else {
					unsafe {
						self.advance_unchecked(1);
					}
				}
			}

			true
		}

		pub fn consume_until_whitespace_eol(&mut self) -> bool {
			while self.first().is_some() {
				if slice::is_any_whitespace(self.bytes).is_some()
					|| slice::is_any_eol(self.bytes).is_some()
				{
					return false;
				} else {
					unsafe {
						self.advance_unchecked(1);
					}
				}
			}

			true
		}

		pub fn byte_pos(&self) -> BytePos {
			BytePos::from_usize(self.pos)
		}

		pub const fn pos(&self) -> usize {
			self.pos
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

mod token {
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

		// End of file
		Eof = '\0',

		// Invisible
		Whitespace,
		Comment,
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
	use crate::cursor::Cursor;
	use crate::pos::{BytePos, Pos};
	use crate::span::ByteSpan;
	use crate::token::{LiteralKind, Token, TokenKind};
	use crate::util::ascii;

	#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
	pub struct Lexer<'a> {
		bytes: &'a [u8],
		cursor: Cursor<'a>,
		emit_invisible: bool,
	}

	impl<'a> Lexer<'a> {
		pub const fn new(bytes: &'a [u8]) -> Self {
			Self { bytes, cursor: Cursor::new(bytes), emit_invisible: false }
		}

		pub fn from_bytes<B>(bytes: &'a B) -> Self
		where
			B: 'a + AsRef<[u8]>,
		{
			Self::new(bytes.as_ref())
		}

		pub fn next_token(&mut self) -> Token {
			let start = self.cursor.byte_pos();

			self.consume_whitespaces();

			if self.emit_invisible && self.cursor.byte_pos() > start {
				return self.emit_token(start, TokenKind::Whitespace);
			}

			let start = self.cursor.byte_pos();

			// Check if there is a line comment next and if so consume until
			// eol.
			if matches!(self.cursor.first(), Some(b';')) {
				self.cursor.consume_until_eol();
				if self.emit_invisible {
					return self.emit_token(start, TokenKind::Comment);
				}
			}

			let start = self.cursor.byte_pos();

			// Check eol here because in the `match` the first byte is already
			// poped and thus the cursor cant detect any eol.
			if self.cursor.consume_any_eol() {
				return self.emit_token(start, TokenKind::Eol);
			}

			// TODO: be able to define decimal separator

			// 1) Check 1 byte symbols
			// 2) Check String start
			// 3) Check Number start
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

				// Number
				(Some(b), _) if ascii::is_ascii_digit(b) => self.lex_number(start, b),

				// String
				(Some(b), _) if ascii::is_ascii_quotation_mark(b) => self.lex_string(start, b),

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

			debug_assert!(ascii::is_ascii_digit(first));

			let mut kind = LiteralKind::Integer;

			let _ = self
				.cursor
				.consume_while(|b| ascii::is_ascii_digit(b) || ascii::is_ascii_number_ignore(b));

			if matches!(self.cursor.first(), Some(b) if ascii::is_ascii_number_decimal_separator(b))
			{
				kind = LiteralKind::Float;

				let separator = self.cursor.consume();
				debug_assert!(
					matches!(separator, Some(b) if ascii::is_ascii_number_decimal_separator(b))
				);

				let _ = self.cursor.consume_while(|b| {
					ascii::is_ascii_digit(b) || ascii::is_ascii_number_ignore(b)
				});
			}

			if matches!(self.cursor.first(), Some(b) if ascii::is_ascii_number_exponent(b))
				|| matches!((self.cursor.first(), self.cursor.second()), (Some(a), Some(b)) if ascii::is_ascii_number_sign(a) && ascii::is_ascii_number_exponent(b))
			{
				// TODO: check that there is a digit after the exponent before consuming
				if matches!(self.cursor.first(), Some(b) if ascii::is_ascii_number_sign(b)) {
					if matches!(self.cursor.first(), Some(b) if ascii::is_ascii_number_sign_negative(b))
					{
						kind = LiteralKind::Float;
					}

					let sign = self.cursor.consume();
					debug_assert!(matches!(sign, Some(b) if ascii::is_ascii_number_sign(b)));
				}

				let exponent = self.cursor.consume();
				debug_assert!(matches!(exponent, Some(b) if ascii::is_ascii_number_exponent(b)));

				let _ = self.cursor.consume_while(|b| {
					ascii::is_ascii_digit(b) || ascii::is_ascii_number_ignore(b)
				});
			}

			self.emit_token(start, TokenKind::Literal { kind })
		}

		fn lex_string(&mut self, start: BytePos, first: u8) -> Token {
			debug_assert!(ascii::is_ascii_quotation_mark(first));

			let closing = ascii::get_closing_quotation_mark(first)
				.unwrap_or_else(|| panic!("Found no closing quotation mark for `0x{:x}`", start));

			let mut terminated = false;

			while let Some(byte) = self.cursor.consume() {
				if ascii::is_ascii_escape(byte) {
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
			self.cursor.consume_until_whitespace_eol();

			// Check if boolean
			let content = &self.bytes[start.as_usize()..self.cursor.pos()];

			if content.eq_ignore_ascii_case(b"true") {
				self.emit_token(
					start,
					TokenKind::Literal { kind: LiteralKind::Boolean { value: true } },
				)
			} else if content.eq_ignore_ascii_case(b"false") {
				self.emit_token(
					start,
					TokenKind::Literal { kind: LiteralKind::Boolean { value: false } },
				)
			} else {
				self.emit_token(start, TokenKind::Ident)
			}
		}

		fn emit_token(&self, start: BytePos, kind: TokenKind) -> Token {
			Token::new(ByteSpan::new(start, self.cursor.byte_pos()), kind)
		}

		fn emit_eof(&self, start: BytePos) -> Token {
			self.emit_token(start, TokenKind::Eof)
		}

		fn pos(&self) -> BytePos {
			self.cursor.byte_pos()
		}

		fn consume_whitespaces(&mut self) {
			while self.cursor.consume_any_whitespace() {}
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

				lexer.consume_whitespaces();

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

				lexer.consume_whitespaces();

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

mod source {
	use std::path::PathBuf;

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct Origin {
		path: PathBuf,
		// File from which it was included
		parent: Option<PathBuf>,
	}

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct Source {
		origin: Origin,
		content: String,
	}
}

mod diagnostic {
	use std::borrow::Cow;
	use std::cmp::Ordering;

	use crate::span::ByteSpan;

	#[repr(u8)]
	#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
	pub enum Severity {
		Bug,
		Error,
		Warning,
		Note,
		Help,
	}

	impl Severity {
		pub const fn is_hard_error(&self) -> bool {
			matches!(self, Self::Bug | Self::Error)
		}

		const fn to_cmp_value(self) -> u8 {
			match self {
				Severity::Bug => 5,
				Severity::Error => 4,
				Severity::Warning => 3,
				Severity::Note => 2,
				Severity::Help => 1,
			}
		}
	}

	impl PartialOrd<Self> for Severity {
		fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
			u8::partial_cmp(&self.to_cmp_value(), &rhs.to_cmp_value())
		}
	}

	impl Ord for Severity {
		fn cmp(&self, rhs: &Self) -> Ordering {
			self.partial_cmp(rhs).expect("Failed to cmp with Ord")
		}
	}

	impl std::fmt::Display for Severity {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			let s = match self {
				Severity::Bug => "bug",
				Severity::Error => "error",
				Severity::Warning => "warning",
				Severity::Note => "note",
				Severity::Help => "help",
			};

			f.write_str(s)
		}
	}

	#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
	pub enum Code {}

	impl Code {
		pub fn message(&self) -> Option<&'static str> {
			todo!()
		}

		pub fn url(&self) -> Option<&'static str> {
			todo!()
		}
	}

	impl std::fmt::Display for Code {
		fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			todo!()
		}
	}

	#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
	pub enum LabelKind {
		Primary,
		Secondary,
	}

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct Label {
		pub(crate) kind: LabelKind,
		pub(crate) span: ByteSpan,
		pub(crate) message: Option<Cow<'static, str>>,
	}

	impl Label {
		pub const fn new(
			kind: LabelKind,
			span: ByteSpan,
			message: Option<Cow<'static, str>>,
		) -> Self {
			Self { kind, span, message }
		}

		pub fn primary<S: Into<ByteSpan>>(span: S) -> Self {
			Self::new(LabelKind::Primary, span.into(), None)
		}

		pub fn secondary<S: Into<ByteSpan>>(span: S) -> Self {
			Self::new(LabelKind::Secondary, span.into(), None)
		}

		pub fn with_message<M: Into<Cow<'static, str>>>(mut self, message: M) -> Self {
			self.message = Some(message.into());
			self
		}
	}

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct Diagnostic {
		pub(crate) severity: Severity,
		pub(crate) code: Option<Code>,
		pub(crate) message: Option<Cow<'static, str>>,
		pub(crate) labels: Vec<Label>,
		pub(crate) notes: Vec<Cow<'static, str>>,
	}

	impl Diagnostic {
		pub fn new(
			severity: Severity,
			code: Option<Code>,
			message: Option<Cow<'static, str>>,
			labels: Vec<Label>,
			notes: Vec<Cow<'static, str>>,
		) -> Self {
			Self { severity, code, message, labels, notes }
		}

		pub fn bug() -> Self {
			Self::new(Severity::Bug, None, None, Vec::new(), Vec::new())
		}

		pub fn error() -> Self {
			Self::new(Severity::Error, None, None, Vec::new(), Vec::new())
		}

		pub fn warning() -> Self {
			Self::new(Severity::Warning, None, None, Vec::new(), Vec::new())
		}

		pub fn note() -> Self {
			Self::new(Severity::Note, None, None, Vec::new(), Vec::new())
		}

		pub fn help() -> Self {
			Self::new(Severity::Help, None, None, Vec::new(), Vec::new())
		}

		pub const fn with_code(mut self, code: Code) -> Self {
			self.code = Some(code);
			self
		}

		pub fn with_message<M: Into<Cow<'static, str>>>(mut self, message: M) -> Self {
			self.message = Some(message.into());
			self
		}

		pub fn with_label(mut self, label: Label) -> Self {
			self.labels.push(label);
			self
		}

		pub fn with_labels(mut self, labels: Vec<Label>) -> Self {
			self.labels.extend(labels.into_iter());
			self
		}

		pub fn with_note<N: Into<Cow<'static, str>>>(mut self, note: N) -> Self {
			self.notes.push(note.into());
			self
		}

		pub fn with_notes<N: Into<Cow<'static, str>>>(mut self, notes: Vec<N>) -> Self {
			self.notes.extend(notes.into_iter().map(|n| n.into()));
			self
		}
	}

	mod fmt {}
}

mod session {
	use crate::diagnostic::Diagnostic;
	use crate::source::Source;

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct Session {
		source: Source,
		diagnostics: Vec<Diagnostic>,
		failed: bool,
	}

	impl Session {
		pub const fn new(source: Source) -> Self {
			Self { source, diagnostics: Vec::new(), failed: false }
		}

		pub fn add_diagnositic(&mut self, diagnostic: Diagnostic) {
			self.diagnostics.push(diagnostic)
		}

		pub fn set_failed(&mut self) {
			self.failed = true;
		}
	}
}

mod keyword {
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
}

mod date {
	use std::fmt;

	#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
	pub struct UncheckedDate {
		year: i16,
		month: u8,
		day: u8,
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
	pub struct Tag(String);

	impl fmt::Display for Tag {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			f.write_str(&self.0)
		}
	}

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct Link(String);

	impl fmt::Display for Link {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			f.write_str(&self.0)
		}
	}

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct Attribute {
		key: String,
		value: String,
	}

	impl fmt::Display for Attribute {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			write!(f, "{}: {}", self.key, self.value)
		}
	}
}

mod directive {
	use std::path::PathBuf;

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
			document_path: PathBuf,
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
			path: PathBuf,
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
}

mod parse {
	use crate::diagnostic::Diagnostic;
	use crate::directive::Directive;
	use crate::session::Session;

	type Result<T, E = Diagnostic> = std::result::Result<T, E>;

	pub struct Parser {
		session: Session,
	}

	impl Parser {
		pub fn next_directive(&mut self) -> Option<Directive> {
			loop {
				match self.parse_top_level_directive()? {
					Ok(directive) => return Some(directive),
					Err(diagnostic) => {
						self.emit_diagnostic(diagnostic);
						self.recover();
					}
				}
			}
		}

		// Allowed: Destructors can not be const
		#[allow(clippy::missing_const_for_fn)]
		pub fn into_session(self) -> Session {
			self.session
		}

		fn emit_diagnostic(&mut self, diagnostic: Diagnostic) {
			if diagnostic.severity.is_hard_error() {
				self.session.set_failed();
			}

			self.session.add_diagnositic(diagnostic);
		}

		fn recover(&mut self) {
			// Skip to tokens `Eol` followed by `Date`
			todo!()
		}

		fn parse_top_level_directive(&mut self) -> Option<Result<Directive>> {
			todo!()
		}
	}
}
