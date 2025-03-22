use seq::seq;
pub trait Specifier {
    const BITS: u8;
}

seq!(N in 1..64 {
    pub struct B~N;
    impl Specifier for B~N {
        const BITS: u8 = N;
    }
});

fn main() {}
