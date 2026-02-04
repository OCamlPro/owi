fn f(x: usize) i32 {

  const arr = [_]i32{ 1, 2, 0, 4 };

  if (x < arr.len) {
    return @divTrunc(10, arr[x]);
  }

  return -1;
}

extern "owi" fn i32_symbol() usize;

export fn check() void {
  const x: usize = i32_symbol();
  _ = &f(x);
}
