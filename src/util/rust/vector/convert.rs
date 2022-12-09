use crate::vector::Vec2;

impl From<(i32, i32)> for Vec2 {
    fn from(value: (i32, i32)) -> Self {
        Vec2::new(value.0, value.1)
    }
}

impl From<(u32, u32)> for Vec2 {
    fn from(value: (u32, u32)) -> Self {
        Vec2::new(value.0 as i32, value.1 as i32)
    }
}
