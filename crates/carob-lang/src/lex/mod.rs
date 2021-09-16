mod cursor;

use self::cursor::Cursor;
use crate::pos::{BytePos, Pos as _};
use crate::span::ByteSpan;
use crate::token::{LiteralKind, Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
	bytes: &'a [u8],
	cursor: Cursor<'a>,
	emit_invisible: bool,
}

impl<'a> Lexer<'a> {
	pub fn new(s: &'a str) -> Self {
		Self { bytes: s.as_bytes(), cursor: Cursor::new(s), emit_invisible: false }
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
		if matches!(self.cursor.first(), Some(';')) {
			self.cursor.consume_until_eol();
			if self.emit_invisible {
				return self.emit_token(start, TokenKind::Comment);
			}
		}

		let start = self.cursor.byte_pos();

		// Check eol here because in the `match` the first byte is already
		// poped and thus the cursor cant detect any eol.
		self.cursor.consume_while_eol();

		if self.cursor.byte_pos() > start {
			return self.emit_token(start, TokenKind::Eol);
		}

		// 1) Check 1 byte symbols
		// 2) Check String start
		// 3) Check Number start
		// 5) Is literal
		match (self.cursor.consume(), self.cursor.first()) {
			(Some('('), _) => self.emit_token(start, TokenKind::LeftParen),
			(Some(')'), _) => self.emit_token(start, TokenKind::RightParen),
			(Some('{'), _) => self.emit_token(start, TokenKind::LeftBrace),
			(Some('}'), _) => self.emit_token(start, TokenKind::RightParen),
			(Some(','), _) => self.emit_token(start, TokenKind::Comma),
			// TODO: Check if digit after that and if so parse as number
			(Some('.'), _) => self.emit_token(start, TokenKind::Dot),
			(Some('-'), _) => self.emit_token(start, TokenKind::Hyphen),
			(Some('+'), _) => self.emit_token(start, TokenKind::Plus),
			(Some(':'), _) => self.emit_token(start, TokenKind::Colon),
			(Some(';'), _) => self.emit_token(start, TokenKind::Semicolon),
			(Some('/'), _) => self.emit_token(start, TokenKind::Slash),
			(Some('*'), _) => self.emit_token(start, TokenKind::Asterisk),
			(Some('^'), _) => self.emit_token(start, TokenKind::Caret),
			(Some('!'), _) => self.emit_token(start, TokenKind::ExclamationMark),
			(Some('@'), _) => self.emit_token(start, TokenKind::AtSign),

			// Number
			(Some(c), _) if char::is_ascii_digit(&c) => self.lex_number(start, c),

			// String
			(Some(c @ '"'), _) => self.lex_string(start, c),

			// Identifier
			(Some(c), _) if char::is_alphabetic(c) => self.lex_ident(start),

			(Some(_), _) => self.emit_token(start, TokenKind::Unknown),

			// Eof
			(None, _) => self.emit_eof(start),
		}
	}

	// [0-9]+(.[0-9]*)?([+-]e[0-9]+)
	fn lex_number(&mut self, start: BytePos, first: char) -> Token {
		// e.g. 1_100,000.10e-10
		// after e no more `.`

		debug_assert!(char::is_ascii_digit(&first));

		let mut kind = LiteralKind::Integer;

		self.consume_digits();

		if matches!(self.cursor.first(), Some('.')) {
			kind = LiteralKind::Float;

			let separator = self.cursor.consume();
			debug_assert_eq!(separator, Some('.'));

			self.consume_digits();
		}

		if matches!(self.cursor.first(), Some('e' | 'E')) {
			let exponent = self.cursor.consume();
			debug_assert!(matches!(exponent, Some('e' | 'E')));

			// TODO: check that there is a digit after the exponent before consuming
			if matches!(self.cursor.first(), Some('-' | '+')) {
				let sign = self.cursor.consume();
				debug_assert!(matches!(sign, Some('-' | '+')));

				if matches!(sign, Some('-')) {
					kind = LiteralKind::Float;
				}
			}

			self.consume_digits();
		}

		self.emit_token(start, TokenKind::literal(kind))
	}

	fn lex_string(&mut self, start: BytePos, first: char) -> Token {
		debug_assert_eq!(first, '"');

		let mut terminated = false;

		while let Some(c) = self.cursor.consume() {
			if c == '\\' {
				let _ = self.cursor.consume();
			} else if c == '"' {
				terminated = true;
				break;
			}
		}

		self.emit_token(start, TokenKind::literal(LiteralKind::String { terminated }))
	}

	fn lex_ident(&mut self, start: BytePos) -> Token {
		self.cursor.consume_while(|c| char::is_alphanumeric(c) || c == '_' || c == '-');

		// Check if boolean
		let content = &self.bytes[start.as_usize()..self.cursor.offset()];

		if content.eq_ignore_ascii_case(b"true") {
			self.emit_token(start, TokenKind::literal(LiteralKind::Boolean { value: true }))
		} else if content.eq_ignore_ascii_case(b"false") {
			self.emit_token(start, TokenKind::literal(LiteralKind::Boolean { value: false }))
		} else {
			self.emit_token(start, TokenKind::Ident)
		}
	}

	fn consume_digits(&mut self) {
		let _ = self.cursor.consume_while(|c| char::is_ascii_digit(&c) || c == '_' || c == ',');
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
			let mut lexer = Lexer::new(&s);

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
			let mut lexer = Lexer::new(&s);

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

		let mut lexer = Lexer::new(&content[1..]);

		lexer.lex_string(BytePos(0), '"');

		assert_eq!(
			lexer.pos(),
			BytePos((content.len() - 1) as u32),
			"String lex check for `{:?}` failed",
			content
		);
	}
}
