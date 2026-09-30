#![no_main]

use owi_sym::Symbolic;

#[no_mangle]
pub extern "C" fn fun () -> i32 {
    let x = i32::symbol();
    owi_sym::assume(x < 4);
    owi_sym::assume(x > 1);
    owi_sym::assert(x >= 4);
    x
}
