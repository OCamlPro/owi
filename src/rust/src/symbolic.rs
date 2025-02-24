use std::ops::RangeBounds;

use crate::{assert, stop_exploration, sys};

pub trait Symbolic : Sized {
    fn symbol() -> Self;

    fn symbol_so_that(f: impl FnOnce(&Self) -> bool) -> Self {
        let x = Self::symbol();
        assert(f(&x));
        x
    }
}

macro_rules! impl_symbolic {
    ($type:ty, $sys_fn:ident) => {
        impl Symbolic for $type {
            fn symbol() -> Self {
                (unsafe { sys::$sys_fn() } as $type)
            }
        }
    };
}

impl_symbolic!(bool, bool_symbol);
impl_symbolic!(u8, i8_symbol);
impl_symbolic!(i8, i8_symbol);
impl_symbolic!(i32, i32_symbol);
impl_symbolic!(u32, i32_symbol);
impl_symbolic!(f32, f32_symbol);
impl_symbolic!(i64, i64_symbol);
impl_symbolic!(u64, i64_symbol);
impl_symbolic!(f64, f64_symbol);

impl Symbolic for char {
    fn symbol() -> Self {
        let x = u32::symbol();
        let c = char::from_u32(x).unwrap_or_else(|| stop_exploration());
        c
    }
}

impl<const N : usize, T: Symbolic> Symbolic for [T; N] {
    fn symbol() -> Self {
        core::array::from_fn(|_| T::symbol())
    }
}

pub trait SymbolicInBounds : Symbolic + PartialOrd {
    fn symbol_in<R: RangeBounds<Self>>(range: R) -> Self {
        Self::symbol_so_that(|x| range.contains(x))
    }
}

impl<T: Symbolic + PartialOrd> SymbolicInBounds for T {}
