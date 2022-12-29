use crate::vector::{Vec2, Vec3};
use std::ops;

//
// TwoDimensional
//
pub trait TwoDimensional<'a, T: 'a> {
    fn height(&self) -> usize;
    fn width(&self) -> usize;
    fn width_at(&self, y: usize) -> Option<usize>;
    fn vget(&self, v: Vec2) -> Option<&T>;
    fn row(&'a self, y: usize) -> impl Iterator<Item = &'a T>;
    fn col(&'a self, x: usize) -> impl Iterator<Item = &'a T>;
    fn row_mut(&'a mut self, y: usize) -> impl Iterator<Item = &'a mut T>;
    fn col_mut(&'a mut self, x: usize) -> impl Iterator<Item = &'a mut T>;

    fn around_pos<F>(&self, pos: &Vec2, f: F)
    where
        F: FnMut(&T, Vec2);

    fn around_pos_diagonally<F>(&self, pos: &Vec2, f: F)
    where
        F: FnMut(&T, Vec2);

    fn around_pos_mut<F>(&mut self, pos: &Vec2, f: F)
    where
        F: FnMut(&mut T, Vec2);

    fn around_pos_diagonally_mut<F>(&mut self, pos: &Vec2, f: F)
    where
        F: FnMut(&mut T, Vec2);
}

impl<'a, T: 'a> TwoDimensional<'a, T> for Vec<Vec<T>> {
    fn height(&self) -> usize {
        self.len()
    }

    fn width(&self) -> usize {
        self.width_at(0).unwrap_or(0)
    }

    fn width_at(&self, y: usize) -> Option<usize> {
        Some(self.get(y)?.len())
    }

    fn vget(&self, v: Vec2) -> Option<&T> {
        self.get(v.y as usize)?.get(v.x as usize)
    }

    fn row(&'a self, y: usize) -> impl Iterator<Item = &'a T> {
        self.get(y).expect("row to exist").iter()
    }

    fn col(&'a self, x: usize) -> impl Iterator<Item = &'a T> {
        self.iter()
            .map(move |row| row.get(x).expect("column to exist"))
        // match it.any(|o| o.is_none()) {
        //     true => None,
        //     false => Some(it.filter_map(|o| o)),
        // }
    }

    fn row_mut(&'a mut self, y: usize) -> impl Iterator<Item = &'a mut T> {
        self.get_mut(y).expect("row to exist").iter_mut()
    }

    fn col_mut(&'a mut self, x: usize) -> impl Iterator<Item = &'a mut T> {
        self.iter_mut()
            .map(move |row| row.get_mut(x).expect("column to exist"))
    }

    fn around_pos<F>(&self, pos: &Vec2, mut f: F)
    where
        F: FnMut(&T, Vec2),
    {
        // Above
        if pos.y > 0 {
            f(&self[&pos.offset(0, -1)], pos.offset(0, -1));
        }

        // Below
        if pos.y < self.len() as i32 - 1 {
            f(&self[&pos.offset(0, 1)], pos.offset(0, 1));
        }

        // Left
        if pos.x > 0 {
            f(&self[&pos.offset(-1, 0)], pos.offset(-1, 0));
        }

        // Right
        if pos.x < self[0].len() as i32 - 1 {
            f(&self[&pos.offset(1, 0)], pos.offset(1, 0));
        }
    }

    fn around_pos_diagonally<F>(&self, pos: &Vec2, mut f: F)
    where
        F: FnMut(&T, Vec2),
    {
        self.around_pos(pos, &mut f);

        // Above Left
        if pos.y > 0 && pos.x > 0 {
            f(&self[&pos.offset(-1, -1)], pos.offset(-1, -1));
        }

        // Above Right
        if pos.y > 0 && pos.x < self[0].len() as i32 - 1 {
            f(&self[&pos.offset(-1, 1)], pos.offset(-1, 1));
        }

        // Below Left
        if pos.y < self.len() as i32 - 1 && pos.x > 0 {
            f(&self[&pos.offset(1, -1)], pos.offset(1, -1));
        }

        // Below Right
        if pos.y < self.len() as i32 - 1 && pos.x < self[0].len() as i32 - 1 {
            f(&self[&pos.offset(1, 1)], pos.offset(1, 1));
        }
    }

    fn around_pos_mut<F>(&mut self, pos: &Vec2, mut f: F)
    where
        F: FnMut(&mut T, Vec2),
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

    fn around_pos_diagonally_mut<F>(&mut self, pos: &Vec2, mut f: F)
    where
        F: FnMut(&mut T, Vec2),
    {
        self.around_pos_mut(pos, &mut f);

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

//
// ThreeDimensional
//
pub trait ThreeDimensional<T> {
    fn around_pos<F>(&self, pos: &Vec3, f: F)
    where
        F: FnMut(&T, Vec3);

    fn around_pos_mut<F>(&mut self, pos: &Vec3, f: F)
    where
        F: FnMut(&mut T, Vec3);
}

impl<T> ThreeDimensional<T> for Vec<Vec<Vec<T>>> {
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
