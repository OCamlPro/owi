pub mod alloc;
mod symbolic;
pub use symbolic::{Symbolic, SymbolicInBounds};

pub mod harness;

mod sys {
    #[link(wasm_import_module = "symbolic")]
    extern "C" {
        pub(super) fn bool_symbol() -> bool;
        pub(super) fn i8_symbol() -> i32;
        pub(super) fn i32_symbol() -> u32;
        pub(super) fn f32_symbol() -> f32;
        pub(super) fn i64_symbol() -> u64;
        pub(super) fn f64_symbol() -> f64;
        pub(super) fn assert(condition: bool);
        pub(super) fn assume(condition: bool);
    }

    #[link(wasm_import_module = "summaries")]
    extern "C" {
        pub fn alloc(base: *mut u8, size: u32) -> *mut u8;
        pub fn dealloc(base: *mut u8) -> *mut u8;
        pub fn abort() -> !;
    }
}

#[deprecated(
    note = "*_symbol functions have been deprecated in favor of the Symbolic trait, use instead: `u8::symbol()`."
)]
pub fn u8_symbol() -> u8 {
    Symbolic::symbol()
}

#[deprecated(
    note = "*_symbol functions have been deprecated in favor of the Symbolic trait, use instead: `u32::symbol()`."
)]
pub fn u32_symbol() -> u32 {
    Symbolic::symbol()
}

#[deprecated(
    note = "*_symbol functions have been deprecated in favor of the Symbolic trait, use instead: `f32::symbol()`."
)]
pub fn f32_symbol() -> f32 {
    Symbolic::symbol()
}

#[deprecated(
    note = "*_symbol functions have been deprecated in favor of the Symbolic trait, use instead: `char::symbol()`."
)]
pub fn char_symbol() -> char {
    Symbolic::symbol()
}

pub fn assert(b: bool) {
    unsafe { sys::assert(b) }
}

pub fn assume(b: bool) {
    unsafe { sys::assume(b) }
}

pub fn stop_exploration() -> ! {
    unsafe {
        sys::abort();
    }
}

pub fn ascii_char() -> char {
    let code = u8::symbol_in(..=127);
    unsafe { char::from_u32_unchecked(code as u32) }
}
