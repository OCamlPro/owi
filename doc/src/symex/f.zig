export fn f(x: usize) i32 {

  const arr = [_]i32{ 1, 2, 0, 4 };

  if (x < arr.len) {
    return @divTrunc(10, arr[x]);
  }

  return -1;
}
