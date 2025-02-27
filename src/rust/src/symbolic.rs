use std::{fmt::Debug, ops::RangeBounds};

use crate::{assume, stop_exploration, sys};

pub struct DebugWrap<'a, T>(&'a T);

impl<'a, T: Debug> Debug for DebugWrap<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.0, f)
    }
}

pub trait Symbolic: Sized
where
    for<'a> DebugWrap<'a, Self>: Debug,
{
    fn symbol_inner() -> Self;

    #[track_caller]
    fn symbol() -> Self {
        let x = Self::symbol_inner();
        if crate::in_replay_mode() {
            let location = std::panic::Location::caller();
            let msg = format!(
                "Symbol created at {location} was {debug_wrap:?}\n",
                debug_wrap = DebugWrap(&x)
            );
            crate::write_to_stdout(msg.as_bytes());
        }
        x
    }

    fn symbol_so_that(f: impl FnOnce(&Self) -> bool) -> Self {
        let x = Self::symbol();
        assume(f(&x));
        x
    }
}

macro_rules! impl_symbolic_primitive {
    ($type:ty, $sys_fn:ident) => {
        impl Symbolic for $type {
            fn symbol_inner() -> Self {
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
    fn symbol_inner() -> Self {
        let x = u32::symbol();
        let c = char::from_u32(x).unwrap_or_else(|| stop_exploration());
        c
    }
}

impl<const N: usize, T: Symbolic + Debug> Symbolic for [T; N] {
    fn symbol_inner() -> Self {
        core::array::from_fn(|_| T::symbol_inner())
    }
}

impl Symbolic for () {
    fn symbol_inner() -> Self {
        ()
    }
}

macro_rules! impl_symbolic_tuple {
    ($($name:ident)+) => {
        impl<$($name: Symbolic + Debug,)+> Symbolic for ($($name,)+) {
            fn symbol_inner() -> Self {
                ($($name::symbol_inner(),)+)
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

impl<T: Symbolic + Debug> Symbolic for Option<T> {
    fn symbol_inner() -> Self {
        let b = bool::symbol_inner();
        if b {
            Some(T::symbol_inner())
        } else {
            None
        }
    }
}

pub trait SymbolicInBounds: Symbolic + PartialOrd + Debug {
    fn symbol_in<R: RangeBounds<Self>>(range: R) -> Self {
        Self::symbol_so_that(|x| range.contains(x))
    }
}

impl<T: Symbolic + PartialOrd + Debug> SymbolicInBounds for T {}
