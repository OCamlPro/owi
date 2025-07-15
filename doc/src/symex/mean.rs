#![no_main]

fn mean1(x: i32, y: i32) -> i32 {
    (x & y) + ((x ^ y) >> 1)
}

fn mean2(x: i32, y: i32) -> i32 {
    (x + y) / 2
}

#[no_mangle]
pub extern "C" fn check(x : i32, y: i32) {
  owi_sym::assert(mean1(x, y) == mean2(x, y))
}
