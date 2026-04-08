use owi_sym::Symbolic;

fn mean1(x: i32, y: i32) -> i32 {
    (x + y) / 2
}

fn mean2(x: i32, y: i32) -> i32 {
    (x & y) + ((x ^ y) >> 1)
}

fn main() {
    let x = i32::named_symbol("x");
    let y = i32::named_symbol("y");
    owi_sym::assert(mean1(x, y) == mean2(x, y))
}
