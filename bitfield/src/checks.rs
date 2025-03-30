use bitfield_impl::generate_mod8_types;

pub trait TotalSizeIsMultipleOfEightBits {
    type PlaceHolder;
}

pub type TotalSizeIsMultipleOfEightBitsChecker<T> =
    <T as TotalSizeIsMultipleOfEightBits>::PlaceHolder;

pub trait Mod8Result {
    type Result;
}

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

    #[test]
    fn test_condition() {
        type A = <<[(); (2 < 3) as usize] as  ConditionResult>::Result   as DiscriminantInRange>::PlaceHolder;
        let _: A;
    }
}
