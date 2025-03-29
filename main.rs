// Bitfield enums with any discriminant (implicit or explicit) outside of the
// range 0..2^BITS should fail to compile.

use std::ops::Add;

use bitfield::*;

const F: isize = 1;

#[derive(BitfieldSpecifier)]
pub enum DeliveryMode {
    Fixed = F,
    Lowest,
    SMI,
    RemoteRead,
    NMI,
    Init,
    Startup,
    External,
}

pub struct True;

pub struct False;

impl Add<True> for False {
    type Output = False;
    fn add(self, _rhs: True) -> Self::Output {
        False
    }
}

impl Add<True> for True {
    type Output = True;
    fn add(self, rhs: True) -> Self::Output {
        True
    }
}

fn main() {
    let bits = DeliveryMode::to_bitfield(&DeliveryMode::Startup);
    println!("bits is: {}", bits >> 3);
}
