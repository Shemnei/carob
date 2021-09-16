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
