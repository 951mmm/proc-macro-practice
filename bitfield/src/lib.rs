use std::{error::Error, fmt::Display};

// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
pub use bitfield_impl::*;

mod checks;
pub use checks::*;
pub trait Specifier {
    const BITS: u8;
    type Uint;
    type Mod8Type;
}

pub trait UintSpecifier {
    type Uint;
}

generate_bits! {}

pub struct Byte;
impl Specifier for Byte {
    const BITS: u8 = 8;
    type Uint = u8;
    type Mod8Type = ZeroMod8;
}

pub const DISCRIMINANTS_LEN: usize = 2usize.pow(8);
pub trait DiscriminantsSpecifier {
    type Uint;
    const DISCRIMINANTS: [Self::Uint; DISCRIMINANTS_LEN];
}

pub trait BitfieldSpecifier {
    type Bitfield: Sized + Specifier;
    type This;
    type FromBitfieldReturn;
    fn from_bitfield(bit_unit: <Self::Bitfield as Specifier>::Uint) -> Self::FromBitfieldReturn;
    fn to_bitfield(this: &Self::This) -> <Self::Bitfield as Specifier>::Uint;
}

impl BitfieldSpecifier for bool {
    type Bitfield = B1;
    type This = Self;
    type FromBitfieldReturn = Result<Self::This>;
    fn from_bitfield(bit_unit: <Self::Bitfield as Specifier>::Uint) -> Self::FromBitfieldReturn {
        match bit_unit {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(Unrecognized::new(u64::from(bit_unit))),
        }
    }
    fn to_bitfield(this: &Self::This) -> <Self::Bitfield as Specifier>::Uint {
        match this {
            false => 0,
            true => 1,
        }
    }
}

#[derive(Debug)]
pub struct Unrecognized(u64);

impl Display for Unrecognized {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("unrecognized value: {}\n", self.raw_value()))
    }
}

impl Error for Unrecognized {}
impl Unrecognized {
    pub fn new(raw_value: u64) -> Self {
        Self(raw_value)
    }
    pub fn raw_value(&self) -> u64 {
        self.0
    }
}

pub type Result<T> = std::result::Result<T, Unrecognized>;

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_specifier() {
        assert_eq!(<B61 as Specifier>::BITS, 61);
    }
    #[test]
    fn test_bit_specifier() {
        enum E {
            A = 0,
            B,
        }

        impl BitfieldSpecifier for E {
            type Bitfield = B50;
            type This = Self;
            type FromBitfieldReturn = Result<Self::This>;
            fn from_bitfield(
                bit_unit: <Self::Bitfield as Specifier>::Uint,
            ) -> Self::FromBitfieldReturn {
                match bit_unit {
                    0 => Ok(Self::A),
                    1 => Ok(Self::B),
                    _ => Err(Unrecognized(bit_unit)),
                }
            }
            fn to_bitfield(this: &Self::This) -> <Self::Bitfield as Specifier>::Uint {
                match this {
                    Self::A => 0,
                    Self::B => 1,
                }
            }
        }
    }
}
