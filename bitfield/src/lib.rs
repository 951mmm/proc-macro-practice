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
pub use bitfield_impl::bitfield;
use seq::seq;
pub trait Specifier {
    const BITS: u8;
}

// seq!(N in 1..64 {
//     pub struct B~N;
//     impl Specifier for B~N {
//         const BITS: u8 = N;
//     }
// });
pub struct B1;
impl Specifier for B1 {
    const BITS: u8 = 1;
}

#[cfg(test)]
mod tests {
    use crate::{Specifier, B1};

    #[test]
    fn test_specifier() {
        assert_eq!(<B1 as Specifier>::BITS, 1);
    }
}
