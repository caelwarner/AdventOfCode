use std::ops;
use crate::vector::Vec2;

pub trait TwoDimensional<T> {
    fn around_pos<F>(&mut self, pos: &Vec2, f: F)
    where
        F: FnMut(&mut T, Vec2);

    fn around_pos_diagonally<F>(&mut self, pos: &Vec2, f: F)
    where
        F: FnMut(&mut T, Vec2);
}

impl<T> TwoDimensional<T> for Vec<Vec<T>> {
    fn around_pos<F>(&mut self, pos: &Vec2, mut f: F)
    where
        F: FnMut(&mut T, Vec2)
    {
        // Above
        if pos.y > 0 {
            f(&mut self[&pos.offset(0, -1)], pos.offset(0, -1));
        }

        // Below
        if pos.y < self.len() as i32 - 1 {
            f(&mut self[&pos.offset(0, 1)], pos.offset(0, 1));
        }

        // Left
        if pos.x > 0 {
            f(&mut self[&pos.offset(-1, 0)], pos.offset(-1, 0));
        }

        // Right
        if pos.x < self[0].len() as i32 - 1 {
            f(&mut self[&pos.offset(1, 0)], pos.offset(1, 0));
        }
    }

    fn around_pos_diagonally<F>(&mut self, pos: &Vec2, mut f: F)
    where
        F: FnMut(&mut T, Vec2)
    {
        self.around_pos(pos, &mut f);

        // Above Left
        if pos.y > 0 && pos.x > 0 {
            f(&mut self[&pos.offset(-1, -1)], pos.offset(-1, -1));
        }

        // Above Right
        if pos.y > 0 && pos.x < self[0].len() as i32 - 1 {
            f(&mut self[&pos.offset(-1, 1)], pos.offset(-1, 1));
        }

        // Below Left
        if pos.y < self.len() as i32 - 1 && pos.x > 0 {
            f(&mut self[&pos.offset(1, -1)], pos.offset(1, -1));
        }

        // Below Right
        if pos.y < self.len() as i32 - 1 && pos.x < self[0].len() as i32 - 1 {
            f(&mut self[&pos.offset(1, 1)], pos.offset(1, 1));
        }
    }
}

impl<T> ops::Index<Vec2> for Vec<Vec<T>> {
    type Output = T;

    fn index(&self, index: Vec2) -> &Self::Output {
        &self[index.y as usize][index.x as usize]
    }
}

impl<T> ops::Index<&Vec2> for Vec<Vec<T>> {
    type Output = T;

    fn index(&self, index: &Vec2) -> &Self::Output {
        &self[index.y as usize][index.x as usize]
    }
}

impl<T> ops::IndexMut<Vec2> for Vec<Vec<T>> {
    fn index_mut(&mut self, index: Vec2) -> &mut Self::Output {
        &mut self[index.y as usize][index.x as usize]
    }
}

impl<T> ops::IndexMut<&Vec2> for Vec<Vec<T>> {
    fn index_mut(&mut self, index: &Vec2) -> &mut Self::Output {
        &mut self[index.y as usize][index.x as usize]
    }
}
