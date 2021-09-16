use std::borrow::Cow;

use crate::pos::{BytePos, Pos as _};
use crate::span::ByteSpan;
use crate::token::{Token, TokenKind};

// TODO: option/functions for skipping invisilbe tokens

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Cursor<'a> {
	// TODO: either collect all at the start or do it lazily via an iter
	tokens: &'a [Token],
	pos: usize,
}

impl<'a> Cursor<'a> {
	pub const fn new(tokens: &'a [Token]) -> Self {
		Self { tokens, pos: 0 }
	}

	pub fn nth(&self, nth: usize) -> Option<&Token> {
		self.tokens.get(self.pos + nth)
	}

	pub fn first(&self) -> Option<&Token> {
		self.nth(0)
	}

	pub fn first_or_eof(&self) -> Cow<'_, Token> {
		if let Some(token) = self.nth(0) {
			Cow::Borrowed(token)
		} else {
			Cow::Owned(Token::new(
				ByteSpan::new(BytePos::from_usize(self.pos), BytePos::from_usize(self.pos)),
				TokenKind::Eof,
			))
		}
	}

	pub fn second(&self) -> Option<&Token> {
		self.nth(1)
	}

	pub unsafe fn advance_unchecked(&mut self, amount: usize) {
		debug_assert!(self.pos + amount <= self.tokens.len());

		self.pos += amount;
	}

	pub fn advance(&mut self, amount: usize) {
		let amount = ::std::cmp::min(self.absolute_idx(amount), self.tokens.len());

		unsafe {
			self.advance_unchecked(amount);
		}
	}

	pub fn consume(&mut self) -> Option<Token> {
		let consumed = *self.first()?;

		debug_assert_eq!(consumed, self.tokens[self.pos]);

		unsafe {
			self.advance_unchecked(1);
		}

		Some(consumed)
	}

	pub fn consume_while<F>(&mut self, mut f: F) -> bool
	where
		F: FnMut(&Token) -> bool,
	{
		while let Some(token) = self.first() {
			if f(token) {
				unsafe {
					self.advance_unchecked(1);
				}
			} else {
				return false;
			}
		}

		true
	}

	pub fn consume_until<F>(&mut self, mut f: F) -> bool
	where
		F: FnMut(&Token) -> bool,
	{
		while let Some(token) = self.first() {
			if !f(token) {
				unsafe {
					self.advance_unchecked(1);
				}
			} else {
				return false;
			}
		}

		true
	}

	pub const fn pos(&self) -> usize {
		self.pos
	}

	const fn absolute_idx(&self, relative: usize) -> usize {
		self.pos + relative
	}
}
