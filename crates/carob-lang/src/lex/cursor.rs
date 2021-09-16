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

		unsafe {
			self.advance_unchecked(amount);
		}
	}

	pub unsafe fn advance_unchecked(&mut self, amount: usize) {
		debug_assert!(amount <= self.bytes.len());

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

	pub fn consume_while_eol(&mut self) -> bool {
		while self.first().is_some() {
			if let Some(width) = slice::is_any_eol(self.bytes) {
				unsafe {
					self.advance_unchecked(width);
				}
			} else {
				break;
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
