use std::cmp::Ordering;
use crate::vector::{Vec2, Vec3};

//
// Vec2
//
impl PartialEq<i32> for Vec2 {
    fn eq(&self, other: &i32) -> bool {
        &self.x == other && &self.y == other
    }

    fn ne(&self, other: &i32) -> bool {
        &self.x != other || &self.y != other
    }
}

impl PartialOrd<i32> for Vec2 {
    fn partial_cmp(&self, _other: &i32) -> Option<Ordering> {
        None
    }

    fn gt(&self, other: &i32) -> bool {
        &self.x > other && &self.y > other
    }

    fn lt(&self, other: &i32) -> bool {
        &self.x < other && &self.y < other
    }

    fn ge(&self, other: &i32) -> bool {
        &self.x >= other && &self.y >= other
    }

    fn le(&self, other: &i32) -> bool {
        &self.x <= other && &self.y <= other
    }
}

//
// Vec3
//
impl PartialEq<i32> for Vec3 {
    fn eq(&self, other: &i32) -> bool {
        &self.x == other && &self.y == other && &self.z == other
    }

    fn ne(&self, other: &i32) -> bool {
        &self.x != other || &self.y != other || &self.z != other
    }
}

impl PartialOrd<i32> for Vec3 {
    fn partial_cmp(&self, _other: &i32) -> Option<Ordering> {
        None
    }

    fn gt(&self, other: &i32) -> bool {
        &self.x > other && &self.y > other && &self.z > other
    }

    fn lt(&self, other: &i32) -> bool {
        &self.x < other && &self.y < other && &self.z < other
    }

    fn ge(&self, other: &i32) -> bool {
        &self.x >= other && &self.y >= other && &self.z >= other
    }

    fn le(&self, other: &i32) -> bool {
        &self.x <= other && &self.y <= other && &self.z <= other
    }
}
