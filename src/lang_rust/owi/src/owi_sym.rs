pub mod alloc;
mod symbolic;
pub use symbolic::{LazyArray, Symbolic, SymbolicInBounds};

pub mod harness;

mod sys {
    #[link(wasm_import_module = "owi")]
    extern "C" {
        pub(super) fn bool_symbol() -> bool;
        pub(super) fn i8_symbol() -> i32;
        pub(super) fn i32_symbol() -> u32;
        pub(super) fn f32_symbol() -> f32;
        pub(super) fn i64_symbol() -> u64;
        pub(super) fn f64_symbol() -> f64;
        pub(super) fn assert(condition: bool);
        pub(super) fn assume(condition: bool);
        pub fn in_replay_mode() -> u32;
        pub fn print_char(byte: u32);
        pub fn alloc(base: *mut u8, size: u32) -> *mut u8;
        pub fn dealloc(base: *mut u8) -> *mut u8;
        pub fn abort() -> !;
    }
}

pub fn assert(b: bool) {
    unsafe { sys::assert(b) }
}

pub fn assume(b: bool) {
    unsafe { sys::assume(b) }
}

pub(crate) fn in_replay_mode() -> bool {
    unsafe { sys::in_replay_mode() == 1 }
}

pub fn write_to_stdout(s: &[u8]) {
    s.iter().copied().for_each(|b| unsafe {
        sys::print_char(b as u32);
    });
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
