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
	unsafe_code,
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

		pub fn from_bytes<B>(bytes: &'a B) -> Self
		where
			B: 'a + AsRef<[u8]>,
		{
			Self::new(bytes.as_ref())
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

		pub fn consume(&mut self) -> Option<u8> {
			let consumed = self.first()?;

			debug_assert_eq!(consumed, self.bytes[0]);

			self.pos += 1;
			self.bytes = &self.bytes[1..];

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

		pub fn pos(&self) -> BytePos {
			BytePos::from_usize(self.pos)
		}
	}

	#[cfg(test)]
	mod tests {
		use super::*;

		#[test]
		fn new() {
			let cursor = Cursor::from_bytes(&"test");
		}
	}
}
