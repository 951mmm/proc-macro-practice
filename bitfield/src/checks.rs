use bitfield_impl::generate_mod8_types;

pub trait TotalSizeIsMultipleOfEightBits {}
pub trait Mod8Specifier {
    type Mod8Type;
}

generate_mod8_types! {}

#[cfg(test)]
mod tests {
    use super::*;
    fn assert<N: TotalSizeIsMultipleOfEightBits>(_n: N) {}
    #[test]
    fn test_add() {
        let a = SevenMod8 {};
        let b = OneMod8 {};
        assert(a + b);
    }
}
