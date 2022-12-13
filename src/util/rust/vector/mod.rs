mod cmp;
mod convert;
mod ops;

#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct Vec2 {
    pub x: i32,
    pub y: i32,
}

impl Vec2 {
    #[inline]
    pub const fn new(x: i32, y: i32) -> Self {
        Vec2 { x, y }
    }

    pub fn abs(&self) -> Self {
        Vec2::new(
            self.x.abs(),
            self.y.abs(),
        )
    }

    pub fn clamp(&self, min: i32, max: i32) -> Self {
        Vec2::new(
            self.x.clamp(min, max),
            self.y.clamp(min, max),
        )
    }
    
    pub fn offset(&self, x: i32, y: i32) -> Self {
        Vec2::new(
            self.x + x,
            self.y + y,
        )
    }

    pub fn distance(&self, from: &Vec2) -> i32 {
        let difference = (from - self).abs();
        difference.x + difference.y
    }
}
