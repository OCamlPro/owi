use std::alloc::{GlobalAlloc, Layout};

use crate::sys;

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
