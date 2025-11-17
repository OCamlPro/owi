use crate::Symbolic;

pub trait Harnessable {
    type Ret;
    fn run(self) -> Self::Ret;
}

pub fn execute_symbolically<F: Harnessable>(f: F) -> F::Ret {
    f.run()
}

macro_rules! impl_harnessable_arity {
    ($($name:ident)+) => {
        impl<$($name: Symbolic + core::fmt::Debug,)+ Ret> Harnessable for fn($($name,)+) -> Ret {
            type Ret = Ret;

            fn run(self) -> Self::Ret {
                (self)($($name::symbol(),)+)
            }
        }
    };
}

impl_harnessable_arity!(A);
impl_harnessable_arity!(A B);
impl_harnessable_arity!(A B C);
impl_harnessable_arity!(A B C D);
impl_harnessable_arity!(A B C D E);
impl_harnessable_arity!(A B C D E F);
impl_harnessable_arity!(A B C D E F G);
impl_harnessable_arity!(A B C D E F G H);
impl_harnessable_arity!(A B C D E F G H I);
impl_harnessable_arity!(A B C D E F G H I J);
impl_harnessable_arity!(A B C D E F G H I J K);
impl_harnessable_arity!(A B C D E F G H I J K L);
