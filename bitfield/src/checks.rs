use bitfield_impl::generate_mod8_types;

pub trait TotalSizeIsMultipleOfEightBits {}

generate_mod8_types! {}

pub trait DiscriminantInRange {
    type PlaceHolder;
}
pub struct True;
pub struct False;
impl DiscriminantInRange for True {
    type PlaceHolder = ();
}
pub type DiscriminantInRangeChecker<T> = <T as DiscriminantInRange>::PlaceHolder;
pub trait ConditionResult {
    type Result;
}

impl ConditionResult for [(); 1] {
    type Result = True;
}

impl ConditionResult for [(); 0] {
    type Result = False;
}

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

    #[test]
    fn test_condition() {
        type a = <<[(); (2 < 3) as usize] as  ConditionResult>::Result   as DiscriminantInRange>::PlaceHolder;
        let _: a;
    }
}
