#![no_main]

fn f(x: usize) -> i32 {

  let arr = [1, 2, 0, 4];

  if x < arr.len() {
    return 10 / arr[x];
  }

  -1
}

use owi_sym::Symbolic;

#[no_mangle]
pub extern "C" fn check() -> () {
  let x = u32::symbol(); // TODO: replace by usize::symbol()
  let res = f(x.try_into().unwrap());
}
