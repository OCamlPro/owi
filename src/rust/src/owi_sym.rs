use std::alloc::{GlobalAlloc, Layout};

mod sys {
    #[link(wasm_import_module = "symbolic")]
    extern "C" {
        pub(super) fn i8_symbol() -> u8;
        pub(super) fn i32_symbol() -> u32;
        pub(super) fn f32_symbol() -> f32;
        pub(super) fn assert(condition: bool);
        pub(super) fn assume(condition: bool);
    }

    #[link(wasm_import_module = "summaries")]
    extern "C" {
        pub fn alloc(base: *mut u8, size: u32) -> *mut u8;
        pub fn dealloc(base: *mut u8) -> *mut u8;
    }
}

pub fn stop_exploration() -> ! {
    unsafe {
        sys::assume(false);
        std::process::abort()
    }
}

pub fn u8_symbol() -> u8 {
    unsafe { sys::i8_symbol() }
}

pub fn u32_symbol() -> u32 {
    unsafe { sys::i32_symbol() }
}

pub fn f32_symbol() -> f32 {
    unsafe { sys::f32_symbol() }
}

pub fn char_symbol() -> char {
    let x = u32_symbol();
    let c = char::from_u32(x).unwrap_or_else(|| stop_exploration());
    c
}

pub fn assert(b: bool) {
    unsafe { sys::assert(b) }
}

pub fn assume(b: bool) {
    unsafe { sys::assume(b) }
}

pub struct OwiTrackingAllocator;

unsafe impl GlobalAlloc for OwiTrackingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let alloc = std::alloc::System.alloc(layout);
        sys::alloc(alloc, layout.size() as u32)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        let ptr = sys::dealloc(ptr);
        std::alloc::System.dealloc(ptr, layout);
    }
}
