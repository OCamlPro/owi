extern "owi" fn i32_symbol() i32;
extern "owi" fn assert(bool) void;

fn mean1(x: i32, y: i32) i32 {
  return (x & y) + ((x ^ y) >> 1);
}

fn mean2(x: i32, y: i32) i32 {
  return @divTrunc(x + y, 2);
}

export fn check(x: i32, y: i32) void {
  assert(mean1(x, y) == mean2(x, y));
}
