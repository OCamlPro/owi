pub mod alloc;
mod symbolic;

pub use symbolic::{LazyArray, Symbolic, SymbolicInBounds};

pub mod harness;

mod sys {
    #[link(wasm_import_module = "owi")]
    extern "C" {
        // Basic Symbols
        pub(super) fn i32_symbol() -> i32;
        pub(super) fn i64_symbol() -> i64;
        pub(super) fn f32_symbol() -> f32;
        pub(super) fn f64_symbol() -> f64;

        // Logic and Control
        pub(super) fn assume(condition: i32);
        pub(super) fn assert(condition: i32);
        pub(super) fn abort() -> !;

        // Utilities
        pub(super) fn in_replay_mode() -> i32;
        pub(super) fn print_char(byte: i32);

        // Coverage and Scoping
        pub(super) fn open_scope_of_length(ptr: *const u8, len: u32);
        pub(super) fn close_scope();

        // Memory management
        pub(super) fn alloc(base: *mut u8, size: u32) -> *mut u8;
        pub(super) fn dealloc(base: *mut u8) -> *mut u8;
    }
}

fn wasm_i32_bool_repr(b: bool) -> i32 {
    if b {
        1
    } else {
        0
    }
}

fn bool_from_wasm_i32_repr(b: i32) -> bool {
    b != 0
}

pub fn assert(b: bool) {
    unsafe { sys::assert(wasm_i32_bool_repr(b)) }
}

pub fn assume(b: bool) {
    unsafe { sys::assume(wasm_i32_bool_repr(b)) }
}

pub(crate) fn in_replay_mode() -> bool {
    bool_from_wasm_i32_repr(unsafe { sys::in_replay_mode() })
}

pub fn write_to_stdout(s: &[u8]) {
    s.iter().copied().for_each(|b| unsafe {
        sys::print_char(b as i32);
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

pub fn with_scope<T>(scope_name: &str, f: impl FnOnce() -> T) -> T {
    unsafe {
        sys::open_scope_of_length(scope_name.as_ptr(), scope_name.len().try_into().unwrap());
    }
    let res = f();
    unsafe {
        sys::close_scope();
    };
    res
}
