pub trait Math {
    fn modulo(&self, modulus: Self) -> Self;
    fn triangle_num(&self) -> Self;
}

impl Math for i32 {
    fn modulo(&self, modulus: Self) -> Self {
        ((self % modulus) + modulus) % modulus
    }

    fn triangle_num(&self) -> Self {
        (self * (self + 1)) / 2
    }
}

impl Math for i64 {
    fn modulo(&self, modulus: Self) -> Self {
        ((self % modulus) + modulus) % modulus
    }

    fn triangle_num(&self) -> Self {
        (self * (self + 1)) / 2
    }
}

impl Math for u16 {
    fn modulo(&self, modulus: Self) -> Self {
        ((self % modulus) + modulus) % modulus
    }

    fn triangle_num(&self) -> Self {
        (self * (self + 1)) / 2
    }
}

impl Math for u32 {
    fn modulo(&self, modulus: Self) -> Self {
        ((self % modulus) + modulus) % modulus
    }

    fn triangle_num(&self) -> Self {
        (self * (self + 1)) / 2
    }
}

impl Math for usize {
    fn modulo(&self, modulus: Self) -> Self {
        ((self % modulus) + modulus) % modulus
    }

    fn triangle_num(&self) -> Self {
        (self * (self + 1)) / 2
    }
}
