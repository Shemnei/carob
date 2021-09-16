use std::str::CharIndices;

use crate::pos::{BytePos, Pos as _};
use crate::util::slice;

#[derive(Debug, Clone)]
pub struct Cursor<'a> {
	chars: CharIndices<'a>,
}

impl<'a> Cursor<'a> {
	pub fn new(s: &'a str) -> Self {
		Self { chars: s.char_indices() }
	}

	pub fn nth(&self, nth: usize) -> Option<char> {
		self.chars.clone().nth(nth).map(|(_, c)| c)
	}

	pub fn first(&self) -> Option<char> {
		self.nth(0)
	}

	pub fn second(&self) -> Option<char> {
		self.nth(1)
	}

	pub fn third(&self) -> Option<char> {
		self.nth(2)
	}

	pub fn consume(&mut self) -> Option<char> {
		self.chars.next().map(|(_, c)| c)
	}

	pub fn consume_while<F>(&mut self, mut f: F) -> bool
	where
		F: FnMut(char) -> bool,
	{
		while let Some(c) = self.first() {
			if f(c) {
				let _ = self.consume().expect("Cursor failed to consume a valid char");
			} else {
				return false;
			}
		}

		true
	}

	pub fn consume_while_eol(&mut self) -> bool {
		while self.first().is_some() {
			if slice::is_any_eol(self.chars.as_str().as_bytes()).is_some() {
				let _ = self.consume();
			} else {
				break;
			}
		}

		true
	}

	pub fn consume_until<F>(&mut self, mut f: F) -> bool
	where
		F: FnMut(char) -> bool,
	{
		while let Some(c) = self.first() {
			if !f(c) {
				let _ = self.consume().expect("Cursor failed to consume a valid char");
			} else {
				return false;
			}
		}

		true
	}

	pub fn consume_any_whitespace(&mut self) -> bool {
		if slice::is_any_whitespace(self.chars.as_str().as_bytes()).is_some() {
			let _ = self.consume();
			true
		} else {
			false
		}
	}

	pub fn consume_any_eol(&mut self) -> bool {
		if slice::is_any_eol(self.chars.as_str().as_bytes()).is_some() {
			let _ = self.consume();
			true
		} else {
			false
		}
	}

	pub fn consume_until_eol(&mut self) -> bool {
		while self.first().is_some() {
			if slice::is_any_eol(self.chars.as_str().as_bytes()).is_some() {
				return false;
			} else {
				let _ = self.consume();
			}
		}

		true
	}

	pub fn consume_until_whitespace_eol(&mut self) -> bool {
		while self.first().is_some() {
			if slice::is_any_whitespace(self.chars.as_str().as_bytes()).is_some()
				|| slice::is_any_eol(self.chars.as_str().as_bytes()).is_some()
			{
				return false;
			} else {
				let _ = self.consume();
			}
		}

		true
	}

	pub fn byte_pos(&self) -> BytePos {
		BytePos::from_usize(self.chars.offset())
	}

	pub fn offset(&self) -> usize {
		self.chars.offset()
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn consume() {
		let content = "Hello World!";
		let mut cursor = Cursor::new(content);

		assert_eq!(cursor.first(), Some('H'));
		assert_eq!(cursor.consume(), Some('H'));

		assert_eq!(cursor.first(), Some('e'));
		assert_eq!(cursor.consume(), Some('e'));

		assert_eq!(cursor.first(), Some('l'));
		assert_eq!(cursor.consume(), Some('l'));

		assert_eq!(cursor.first(), Some('l'));
		assert_eq!(cursor.consume(), Some('l'));

		assert_eq!(cursor.first(), Some('o'));
		assert_eq!(cursor.consume(), Some('o'));

		assert_eq!(cursor.first(), Some(' '));
		assert_eq!(cursor.consume(), Some(' '));

		assert_eq!(cursor.first(), Some('W'));
		assert_eq!(cursor.consume(), Some('W'));

		assert_eq!(cursor.first(), Some('o'));
		assert_eq!(cursor.consume(), Some('o'));

		assert_eq!(cursor.first(), Some('r'));
		assert_eq!(cursor.consume(), Some('r'));

		assert_eq!(cursor.first(), Some('l'));
		assert_eq!(cursor.consume(), Some('l'));

		assert_eq!(cursor.first(), Some('d'));
		assert_eq!(cursor.consume(), Some('d'));

		assert_eq!(cursor.first(), Some('!'));
		assert_eq!(cursor.consume(), Some('!'));

		assert_eq!(cursor.first(), None);
		assert_eq!(cursor.consume(), None);

		assert_eq!(cursor.first(), None);
		assert_eq!(cursor.consume(), None);
	}
}
