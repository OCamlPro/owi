// TODO: replace this by a proper include of the owi header?
extern "symbolic" fn i32_symbol() i32;
extern "symbolic" fn assume(bool) void;
extern "symbolic" fn assert(bool) void;

fn fibonacci(n: i32) i32 {
    if (n < 0) {
        @panic("expected a positive number");
    }
    if (n <= 2) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

pub fn main() void {
    const n: i32 = i32_symbol();
    assume(n > 0);
    assume(n < 10);
    const result = fibonacci(n);
    assert(result != 21);
}
