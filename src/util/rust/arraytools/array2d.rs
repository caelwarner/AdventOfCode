use std::fmt::{Debug, Display};
use std::ops;

use itertools::Itertools;
use num::PrimInt;

use crate::arraytools::iter::{ColIter2D, ColIterMut2D, FlatIter2D, FlatIterMut2D, Neighbours2D, NeighboursMut2D, RowIter2D, RowIterMut2D};
use crate::vector::Vec2;

pub trait Array2D<'a, T: 'a> {
    fn height(&self) -> i32;
    fn width(&self) -> i32;
    fn width_at<N: PrimInt>(&self, y: N) -> Option<i32>;

    fn uheight(&self) -> usize;
    fn uwidth(&self) -> usize;
    fn uwidth_at<N: PrimInt>(&self, y: N) -> Option<usize>;

    fn v_get(&self, v: &Vec2) -> Option<&T>;
    fn v_get_mut(&mut self, v: &Vec2) -> Option<&mut T>;

    fn row<N: PrimInt>(&self, y: N) -> RowIter2D<'_, T>;
    fn row_mut<N: PrimInt>(&mut self, y: N) -> RowIterMut2D<'_, T>;
    fn col<N: PrimInt>(&self, x: N) -> ColIter2D<'_, T>;
    fn col_mut<N: PrimInt>(&mut self, x: N) -> ColIterMut2D<'_, T>;

    fn neighbours(&self, pos: &Vec2) -> Neighbours2D<'_, T>;
    fn neighbours_diagonally(&self, pos: &Vec2) -> Neighbours2D<'_, T>;
    fn neighbours_mut(&mut self, pos: &Vec2) -> NeighboursMut2D<'_, T>;
    fn neighbours_diagonally_mut(&mut self, pos: &Vec2) -> NeighboursMut2D<'_, T>;

    fn around_pos<F>(&self, pos: &Vec2, f: F)
        where F: FnMut(&T, Vec2);

    fn around_pos_diagonally<F>(&self, pos: &Vec2, f: F)
        where F: FnMut(&T, Vec2);

    fn around_pos_mut<F>(&mut self, pos: &Vec2, f: F)
        where F: FnMut(&mut T, Vec2);

    fn around_pos_diagonally_mut<F>(&mut self, pos: &Vec2, f: F)
        where F: FnMut(&mut T, Vec2);

    fn flat_iter(&self) -> FlatIter2D<'_, T>;
    fn flat_iter_mut(&mut self) -> FlatIterMut2D<'_, T>;

    fn insert_row_default(&mut self, y: i32, default: T)
        where T: Clone;

    fn insert_col_default(&mut self, x: i32, default: T)
        where T: Clone;

    fn rotate(&self, rotate: Rotation) -> Vec<Vec<T>>
        where T: Clone;

    fn rotate_into(&self, rotate: Rotation, into: &mut Vec<Vec<T>>)
        where T: Clone;

    fn print(&self)
        where T: Display;
}

impl<'a, T: 'a> Array2D<'a, T> for Vec<Vec<T>> {
    #[inline]
    fn height(&self) -> i32 {
        self.uheight() as i32
    }

    #[inline]
    fn width(&self) -> i32 {
        self.uwidth() as i32
    }

    #[inline]
    fn width_at<N: PrimInt>(&self, y: N) -> Option<i32> {
        self.uwidth_at(y).map(|width| width as i32)
    }

    #[inline]
    fn uheight(&self) -> usize {
        self.len()
    }

    #[inline]
    fn uwidth(&self) -> usize {
        self.uwidth_at(0usize).unwrap_or(0)
    }

    fn uwidth_at<N: PrimInt>(&self, y: N) -> Option<usize> {
        Some(self.get(y.to_usize().expect("y value that can be converted to usize"))?.len())
    }

    #[inline]
    fn v_get(&self, pos: &Vec2) -> Option<&T> {
        self.get(pos.y as usize)?.get(pos.x as usize)
    }

    #[inline]
    fn v_get_mut(&mut self, pos: &Vec2) -> Option<&mut T> {
        self.get_mut(pos.y as usize)?.get_mut(pos.x as usize)
    }

    #[inline]
    fn row<N: PrimInt>(&self, y: N) -> RowIter2D<'_, T> {
        RowIter2D::new(self, y.to_i32().expect("y value that can be converted to i32"))
    }

    #[inline]
    fn row_mut<N: PrimInt>(&mut self, y: N) -> RowIterMut2D<'_, T> {
        RowIterMut2D::new(self, y.to_i32().expect("y value that can be converted to i32"))
    }

    #[inline]
    fn col<N: PrimInt>(&self, x: N) -> ColIter2D<'_, T> {
        ColIter2D::new(self, x.to_i32().expect("x value that can be converted to i32"))
    }

    #[inline]
    fn col_mut<N: PrimInt>(&mut self, x: N) -> ColIterMut2D<'_, T> {
        ColIterMut2D::new(self, x.to_i32().expect("x value that can be converted to i32"))
    }

    #[inline]
    fn neighbours(&self, pos: &Vec2) -> Neighbours2D<'_, T> {
        Neighbours2D::new(self, pos.neighbours().into_iter().collect())
    }

    #[inline]
    fn neighbours_diagonally(&self, pos: &Vec2) -> Neighbours2D<'_, T> {
        Neighbours2D::new(self, pos.neighbours_diagonal().into_iter().collect())
    }

    #[inline]
    fn neighbours_mut(&mut self, pos: &Vec2) -> NeighboursMut2D<'_, T> {
        NeighboursMut2D::new(self, pos.neighbours().into_iter().collect())
    }

    #[inline]
    fn neighbours_diagonally_mut(&mut self, pos: &Vec2) -> NeighboursMut2D<'_, T> {
        NeighboursMut2D::new(self, pos.neighbours_diagonal().into_iter().collect())
    }

    #[inline]
    fn around_pos<F>(&self, pos: &Vec2, mut f: F)
        where F: FnMut(&T, Vec2),
    {
        self.neighbours(pos).for_each(|(element, neighbour)| f(element, neighbour));
    }

    #[inline]
    fn around_pos_diagonally<F>(&self, pos: &Vec2, mut f: F)
        where F: FnMut(&T, Vec2),
    {
        self.neighbours_diagonally(pos).for_each(|(element, neighbour)| f(element, neighbour));
    }

    #[inline]
    fn around_pos_mut<F>(&mut self, pos: &Vec2, mut f: F)
        where F: FnMut(&mut T, Vec2),
    {
        self.neighbours_mut(pos).for_each(|(element, neighbour)| f(element, neighbour));
    }

    #[inline]
    fn around_pos_diagonally_mut<F>(&mut self, pos: &Vec2, mut f: F)
        where F: FnMut(&mut T, Vec2),
    {
        self.neighbours_diagonally_mut(pos).for_each(|(element, neighbour)| f(element, neighbour));
    }

    #[inline]
    fn flat_iter(&self) -> FlatIter2D<'_, T> {
        FlatIter2D::new(self)
    }

    #[inline]
    fn flat_iter_mut(&mut self) -> FlatIterMut2D<'_, T> {
        FlatIterMut2D::new(self)
    }

    #[inline]
    fn insert_row_default(&mut self, y: i32, default: T)
        where T: Clone,
    {
        self.insert(y as usize, vec![default; self.width() as usize]);
    }

    #[inline]
    fn insert_col_default(&mut self, x: i32, default: T)
        where T: Clone,
    {
        self.iter_mut().for_each(|row| row.insert(x as usize, default.clone()))
    }

    /// # Examples
    ///
    /// ```
    /// use util::arraytools::Array2D;
    /// use util::arraytools::array2d::Rotation;
    ///
    /// let array2d = vec![
    ///     vec![1, 2, 3],
    ///     vec![4, 5, 6],
    ///     vec![7, 8, 9],
    /// ];
    ///
    /// let array2d_rotated_right = vec![
    ///     vec![7, 4, 1],
    ///     vec![8, 5, 2],
    ///     vec![9, 6, 3],
    /// ];
    /// assert_eq!(array2d_rotated_right, array2d.rotate(Rotation::Right));
    ///
    /// let array2d_rotated_upside_down = vec![
    ///     vec![9, 8, 7],
    ///     vec![6, 5, 4],
    ///     vec![3, 2, 1],
    /// ];
    /// assert_eq!(array2d_rotated_upside_down, array2d.rotate(Rotation::UpsideDown));
    ///
    /// let array2d_rotated_left = vec![
    ///     vec![3, 6, 9],
    ///     vec![2, 5, 8],
    ///     vec![1, 4, 7],
    /// ];
    /// assert_eq!(array2d_rotated_left, array2d.rotate(Rotation::Left));
    ///
    /// assert_eq!(array2d, array2d.rotate(Rotation::Left).rotate(Rotation::Right));
    /// assert_eq!(
    ///     array2d,
    ///     // Rotated right 4 times
    ///     array2d.rotate(Rotation::Right).rotate(Rotation::Right).rotate(Rotation::Right).rotate(Rotation::Right),
    /// );
    /// ```
    fn rotate(&self, rotate: Rotation) -> Vec<Vec<T>>
        where T: Clone
    {
        match AbsoluteRotation::from(rotate) {
            AbsoluteRotation::R90 => {
                (0..self.width())
                    .map(|x| self.col(x).cloned().rev().collect_vec())
                    .collect_vec()
            }
            AbsoluteRotation::R180 => {
                (0..self.height())
                    .rev()
                    .map(|y| self.row(y).cloned().rev().collect_vec())
                    .collect_vec()
            }
            AbsoluteRotation::R270 => {
                (0..self.width())
                    .rev()
                    .map(|x| self.col(x).cloned().collect_vec())
                    .collect_vec()
            }
        }
    }

    fn rotate_into(&self, rotate: Rotation, into: &mut Vec<Vec<T>>)
        where T: Clone
    {
        match AbsoluteRotation::from(rotate) {
            AbsoluteRotation::R90 => {
                if into.uheight() < self.uwidth() {
                    panic!("array2d into has invalid height dimension");
                }

                for x in 0..self.uwidth() {
                    into[x].clear();
                    self.col(x).cloned().rev().collect_into(&mut into[x]);
                }
            }
            AbsoluteRotation::R180 => {
                if into.uheight() < self.uheight() {
                    panic!("array2d into has invalid height dimension");
                }

                for y in (0..self.uheight()).rev() {
                    into[y].clear();
                    self.row(y).cloned().rev().collect_into(&mut into[y]);
                }

                (0..self.height())
                    .rev()
                    .map(|y| self.row(y).cloned().rev().collect_vec())
                    .collect_vec();
                todo!();
            }
            AbsoluteRotation::R270 => {
                (0..self.width())
                    .rev()
                    .map(|x| self.col(x).cloned().collect_vec())
                    .collect_vec();
                todo!();
            }
        }
    }

    #[inline]
    fn print(&self)
        where T: Display
    {
        let out = self.iter()
            .map(|row| row.iter().join(" "))
            .join("\n");

        println!("{}\n", out);
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

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Rotation {
    Right,
    Right90,
    Right180,
    Right270,
    Left,
    Left90,
    Left180,
    Left270,
    UpsideDown,
}

impl Rotation {
    /// # Examples
    ///
    /// ```
    /// use util::arraytools::array2d::Rotation;
    ///
    /// assert_eq!(Some(Rotation::Right), Rotation::from_degrees(90));
    /// assert_eq!(Some(Rotation::UpsideDown), Rotation::from_degrees(180));
    /// assert_eq!(Some(Rotation::Left), Rotation::from_degrees(270));
    /// assert_eq!(None, Rotation::from_degrees(34));
    /// assert_eq!(Some(Rotation::Left), Rotation::from_degrees(-90));
    /// assert_eq!(Some(Rotation::Right), Rotation::from_degrees(1890));
    /// assert_eq!(None, Rotation::from_degrees(0));
    /// ```
    pub fn from_degrees(degrees: i32) -> Option<Self> {
        let degrees = degrees.rem_euclid(360);

        return if degrees == 90 {
            Some(Rotation::Right)
        } else if degrees == 180 {
            Some(Rotation::UpsideDown)
        } else if degrees == 270 {
            Some(Rotation::Left)
        } else {
            None
        };
    }
}

impl From<Rotation> for AbsoluteRotation {
    fn from(value: Rotation) -> Self {
        match value {
            Rotation::Right | Rotation::Right90 | Rotation::Left270 => AbsoluteRotation::R90,
            Rotation::Right180 | Rotation::Left180 | Rotation::UpsideDown => AbsoluteRotation::R180,
            Rotation::Right270 | Rotation::Left | Rotation::Left90 => AbsoluteRotation::R270,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum AbsoluteRotation {
    R90,
    R180,
    R270,
}
