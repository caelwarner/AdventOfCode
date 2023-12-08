use std::ops;
use crate::vector::Vec3;

pub trait Array3D<T> {
    fn around_pos<F>(&self, pos: &Vec3, f: F)
        where
            F: FnMut(&T, Vec3);

    fn around_pos_mut<F>(&mut self, pos: &Vec3, f: F)
        where
            F: FnMut(&mut T, Vec3);
}

impl<T> Array3D<T> for Vec<Vec<Vec<T>>> {
    fn around_pos<F>(&self, pos: &Vec3, mut f: F)
        where
            F: FnMut(&T, Vec3),
    {
        // Above
        if pos.y > 0 {
            f(&self[&pos.offset(0, -1, 0)], pos.offset(0, -1, 0));
        }

        // Below
        if pos.y < self.len() as i32 - 1 {
            f(&self[&pos.offset(0, 1, 0)], pos.offset(0, 1, 0));
        }

        // Left
        if pos.x > 0 {
            f(&self[&pos.offset(-1, 0, 0)], pos.offset(-1, 0, 0));
        }

        // Right
        if pos.x < self[0].len() as i32 - 1 {
            f(&self[&pos.offset(1, 0, 0)], pos.offset(1, 0, 0));
        }

        // Forward
        if pos.z > 0 {
            f(&self[&pos.offset(0, 0, -1)], pos.offset(0, 0, -1));
        }

        if pos.z < self[0][0].len() as i32 - 1 {
            f(&self[&pos.offset(0, 0, 1)], pos.offset(0, 0, 1));
        }
    }

    fn around_pos_mut<F>(&mut self, pos: &Vec3, mut f: F)
        where
            F: FnMut(&mut T, Vec3),
    {
        // Above
        if pos.y > 0 {
            f(&mut self[&pos.offset(0, -1, 0)], pos.offset(0, -1, 0));
        }

        // Below
        if pos.y < self.len() as i32 - 1 {
            f(&mut self[&pos.offset(0, 1, 0)], pos.offset(0, 1, 0));
        }

        // Left
        if pos.x > 0 {
            f(&mut self[&pos.offset(-1, 0, 0)], pos.offset(-1, 0, 0));
        }

        // Right
        if pos.x < self[0].len() as i32 - 1 {
            f(&mut self[&pos.offset(1, 0, 0)], pos.offset(1, 0, 0));
        }

        // Forward
        if pos.z > 0 {
            f(&mut self[&pos.offset(0, 0, -1)], pos.offset(0, 0, -1));
        }

        if pos.z < self[0][0].len() as i32 - 1 {
            f(&mut self[&pos.offset(0, 0, 1)], pos.offset(0, 0, 1));
        }
    }
}

impl<T> ops::Index<Vec3> for Vec<Vec<Vec<T>>> {
    type Output = T;

    fn index(&self, index: Vec3) -> &Self::Output {
        &self[index.y as usize][index.x as usize][index.z as usize]
    }
}

impl<T> ops::Index<&Vec3> for Vec<Vec<Vec<T>>> {
    type Output = T;

    fn index(&self, index: &Vec3) -> &Self::Output {
        &self[index.y as usize][index.x as usize][index.z as usize]
    }
}

impl<T> ops::IndexMut<Vec3> for Vec<Vec<Vec<T>>> {
    fn index_mut(&mut self, index: Vec3) -> &mut Self::Output {
        &mut self[index.y as usize][index.x as usize][index.z as usize]
    }
}

impl<T> ops::IndexMut<&Vec3> for Vec<Vec<Vec<T>>> {
    fn index_mut(&mut self, index: &Vec3) -> &mut Self::Output {
        &mut self[index.y as usize][index.x as usize][index.z as usize]
    }
}
