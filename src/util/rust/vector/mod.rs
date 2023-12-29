mod cmp;
mod convert;
mod ops;

//
// Vec2
//
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

    #[inline]
    pub fn abs(&self) -> Self {
        Vec2::new(self.x.abs(), self.y.abs())
    }

    #[inline]
    pub fn clamp(&self, min: i32, max: i32) -> Self {
        Vec2::new(self.x.clamp(min, max), self.y.clamp(min, max))
    }

    #[inline]
    pub fn distance(&self, from: &Vec2) -> i32 {
        let difference = (from - self).abs();
        difference.x + difference.y
    }

    #[inline]
    pub fn offset(&self, x: i32, y: i32) -> Self {
        Vec2::new(self.x + x, self.y + y)
    }

    #[inline]
    pub fn offset_mut(&mut self, x: i32, y: i32) {
        self.x += x;
        self.y += y;
    }

    pub const fn offsets() -> [Self; 4] {
        [
            Vec2::new(1, 0),
            Vec2::new(-1, 0),
            Vec2::new(0, 1),
            Vec2::new(0, -1),
        ]
    }

    #[inline]
    pub fn n_left(&self, n: i32) -> Self {
        self.offset(-n, 0)
    }

    #[inline]
    pub fn n_right(&self, n: i32) -> Self {
        self.offset(n, 0)
    }

    #[inline]
    pub fn n_up(&self, n: i32) -> Self {
        self.offset(0, -n)
    }

    #[inline]
    pub fn n_down(&self, n: i32) -> Self {
        self.offset(0, n)
    }

    #[inline]
    pub fn left(&self) -> Self {
        self.n_left(1)
    }

    #[inline]
    pub fn right(&self) -> Self {
        self.n_right(1)
    }

    #[inline]
    pub fn up(&self) -> Self {
        self.n_up(1)
    }

    #[inline]
    pub fn down(&self) -> Self {
        self.n_down(1)
    }

    pub fn n_neighbours(&self, n: i32) -> [Self; 4] {
        [
            self.n_left(n),
            self.n_right(n),
            self.n_up(n),
            self.n_down(n),
        ]
    }

    #[inline]
    pub fn neighbours(&self) -> [Self; 4] {
        self.n_neighbours(1)
    }

    pub fn neighbours_diagonal(&self) -> [Self; 8] {
        [
            self.n_left(1),
            self.n_right(1),
            self.n_up(1),
            self.n_down(1),
            self.offset(-1, -1),
            self.offset(1, -1),
            self.offset(-1, 1),
            self.offset(1, 1),
        ]
    }
}

//
// Vec3
//
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct Vec3 {
    pub x: i32,
    pub y: i32,
    pub z: i32,
}

impl Vec3 {
    #[inline]
    pub const fn new(x: i32, y: i32, z: i32) -> Self {
        Vec3 { x, y, z }
    }

    pub fn abs(&self) -> Self {
        Vec3::new(self.x.abs(), self.y.abs(), self.y.abs())
    }

    pub fn clamp(&self, min: i32, max: i32) -> Self {
        Vec3::new(
            self.x.clamp(min, max),
            self.y.clamp(min, max),
            self.z.clamp(min, max),
        )
    }

    pub fn offset(&self, x: i32, y: i32, z: i32) -> Self {
        Vec3::new(self.x + x, self.y + y, self.z + z)
    }

    pub fn distance(&self, from: &Vec3) -> i32 {
        let difference = (from - self).abs();
        difference.x + difference.y + difference.z
    }

    pub fn offsets() -> [Self; 6] {
        [
            Vec3::new(1, 0, 0),
            Vec3::new(-1, 0, 0),
            Vec3::new(0, 1, 0),
            Vec3::new(0, -1, 0),
            Vec3::new(0, 0, 1),
            Vec3::new(0, 0, -1),
        ]
    }

    pub fn n_neighbours(&self, n: i32) -> [Self; 6] {
        [
            self.offset(n, 0, 0),
            self.offset(-n, 0, 0),
            self.offset(0, n, 0),
            self.offset(0, -n, 0),
            self.offset(0, 0, n),
            self.offset(0, 0, -n),
        ]
    }

    pub fn neighbours(&self) -> [Self; 6] {
        self.n_neighbours(1)
    }
}
