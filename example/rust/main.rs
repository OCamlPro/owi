fn mean_wrong(x: i32, y: i32) -> i32 {
    (x + y) / 2
}

fn mean_correct(x: i32, y: i32) -> i32 {
    (x & y) + ((x ^ y) >> 1)
}

fn main() {
    let x = owi_sym::u32_symbol() as i32;
    let y = owi_sym::u32_symbol() as i32;
    owi_sym::assert(mean_wrong(x, y) == mean_correct(x, y))
}
