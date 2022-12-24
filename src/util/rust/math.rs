pub fn modulus(a: i32, b: i32) -> i32 {
    ((a % b) + b) % b
}

pub fn triangle_num(n: i32) -> i32 {
    (n * (n + 1)) / 2
}
