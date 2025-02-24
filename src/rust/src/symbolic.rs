use crate::{stop_exploration, sys};

pub trait Symbolic {
    fn symbol() -> Self;
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

impl_symbolic!(u8, i8_symbol);
impl_symbolic!(i8, i8_symbol);
impl_symbolic!(i32, i32_symbol);
impl_symbolic!(u32, i32_symbol);
impl_symbolic!(f32, f32_symbol);

impl Symbolic for char {
    fn symbol() -> Self {
        let x = u32::symbol();
        let c = char::from_u32(x).unwrap_or_else(|| stop_exploration());
        c
    }
}
