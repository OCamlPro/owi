#![no_main]

#[no_mangle]
pub extern "C" fn f(x: usize) -> i32 {

  let arr = [1, 2, 0, 4];

  if x < arr.len() {
    return 10 / arr[x];
  }

  -1
}
