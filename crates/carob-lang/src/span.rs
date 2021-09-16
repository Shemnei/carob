use std::ops::Range;

use crate::pos::{BytePos, Pos as _};

pub type OffsetWidth = i32;

pub trait Span {
	type Pos: crate::pos::Pos;
	type IdxOutput: ?Sized;

	fn with_low(&self, low: Self::Pos) -> Self;
	fn with_high(&self, high: Self::Pos) -> Self;

	fn offset_low(&self, offset: OffsetWidth) -> Self;
	fn offset_high(&self, offset: OffsetWidth) -> Self;
	fn offset(&self, offset: OffsetWidth) -> Self;

	fn union(&self, other: &Self) -> Self;

	fn len(&self) -> usize;

	fn index<'a>(&self, s: &'a str) -> &'a Self::IdxOutput;

	fn as_range(&self) -> Range<usize>;
}

macro_rules! span {
	( struct $span:ident ( $ty:ty : $idxo:ty )) => {
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
			type IdxOutput = $idxo;
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

			fn len(&self) -> usize {
				self.high.as_usize() - self.low.as_usize()
			}

			fn index<'a>(&self, s: &'a str) -> &'a <Self as $crate::span::Span>::IdxOutput {
				&s[self.low.as_usize()..self.high.as_usize()]
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

span!(struct ByteSpan(BytePos : str));
