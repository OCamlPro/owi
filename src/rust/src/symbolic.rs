use std::ops::RangeBounds;

use crate::{assume, stop_exploration, sys};

pub trait Symbolic: Sized {
    fn symbol() -> Self;

    fn symbol_so_that(f: impl FnOnce(&Self) -> bool) -> Self {
        let x = Self::symbol();
        assume(f(&x));
        x
    }
}

macro_rules! impl_symbolic_primitive {
    ($type:ty, $sys_fn:ident) => {
        impl Symbolic for $type {
            fn symbol() -> Self {
                (unsafe { sys::$sys_fn() } as $type)
            }
        }
    };
}

impl_symbolic_primitive!(bool, bool_symbol);
impl_symbolic_primitive!(u8, i8_symbol);
impl_symbolic_primitive!(i8, i8_symbol);
impl_symbolic_primitive!(i32, i32_symbol);
impl_symbolic_primitive!(u32, i32_symbol);
impl_symbolic_primitive!(f32, f32_symbol);
impl_symbolic_primitive!(i64, i64_symbol);
impl_symbolic_primitive!(u64, i64_symbol);
impl_symbolic_primitive!(f64, f64_symbol);

impl Symbolic for char {
    fn symbol() -> Self {
        let x = u32::symbol();
        let c = char::from_u32(x).unwrap_or_else(|| stop_exploration());
        c
    }
}

impl<const N: usize, T: Symbolic> Symbolic for [T; N] {
    fn symbol() -> Self {
        core::array::from_fn(|_| T::symbol())
    }
}

impl Symbolic for () {
    fn symbol() -> Self {
        ()
    }
}

macro_rules! impl_symbolic_tuple {
    ($($name:ident)+) => {
        impl<$($name: Symbolic,)+> Symbolic for ($($name,)+) {
            fn symbol() -> Self {
                ($($name::symbol(),)+)
            }
        }
    };
}

impl_symbolic_tuple!(A);
impl_symbolic_tuple!(A B);
impl_symbolic_tuple!(A B C);
impl_symbolic_tuple!(A B C D);
impl_symbolic_tuple!(A B C D E);
impl_symbolic_tuple!(A B C D E F);
impl_symbolic_tuple!(A B C D E F G);
impl_symbolic_tuple!(A B C D E F G H);
impl_symbolic_tuple!(A B C D E F G H I);
impl_symbolic_tuple!(A B C D E F G H I J);
impl_symbolic_tuple!(A B C D E F G H I J K);
impl_symbolic_tuple!(A B C D E F G H I J K L);

impl<T: Symbolic> Symbolic for Option<T> {
    fn symbol() -> Self {
        let b = bool::symbol();
        if b {
            Some(T::symbol())
        } else {
            None
        }
    }
}

pub trait SymbolicInBounds: Symbolic + PartialOrd {
    fn symbol_in<R: RangeBounds<Self>>(range: R) -> Self {
        Self::symbol_so_that(|x| range.contains(x))
    }
}

impl<T: Symbolic + PartialOrd> SymbolicInBounds for T {}
