extern "owi" fn i32_symbol() i32;
extern "owi" fn assume(bool) void;
extern "owi" fn assert(bool) void;

export fn fun() i32 {
    const n: i32 = i32_symbol();
    assume(n > 3);
    assume(n < 5);
    assert(n >= 5);
    return n;
}
