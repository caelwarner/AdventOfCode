use std::ops;
use crate::arraytools::iter::{ColIter2D, ColIterMut2D, FlatIter2D, FlatIterMut2D, Neighbours2D, NeighboursMut2D, RowIter2D, RowIterMut2D};
use crate::vector::Vec2;

pub trait Array2D<'a, T: 'a> {
    fn height(&self) -> i32;
    fn width(&self) -> i32;
    fn width_at(&self, y: i32) -> Option<i32>;

    fn v_get(&self, v: &Vec2) -> Option<&T>;
    fn v_get_mut(&mut self, v: &Vec2) -> Option<&mut T>;

    fn row(&self, y: i32) -> RowIter2D<'_, T>;
    fn row_mut(&mut self, y: i32) -> RowIterMut2D<'_, T>;
    fn col(&self, x: i32) -> ColIter2D<'_, T>;
    fn col_mut(&mut self, x: i32) -> ColIterMut2D<'_, T>;

    fn neighbours(&self, pos: &Vec2) -> Neighbours2D<'_, T>;
    fn neighbours_diagonally(&self, pos: &Vec2) -> Neighbours2D<'_, T>;
    fn neighbours_mut(&mut self, pos: &Vec2) -> NeighboursMut2D<'_, T>;
    fn neighbours_diagonally_mut(&mut self, pos: &Vec2) -> NeighboursMut2D<'_, T>;

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

    fn flat_iter(&self) -> FlatIter2D<'_, T>;
    fn flat_iter_mut(&mut self) -> FlatIterMut2D<'_, T>;
}

impl<'a, T: 'a> Array2D<'a, T> for Vec<Vec<T>> {
    fn height(&self) -> i32 {
        self.len() as i32
    }

    fn width(&self) -> i32 {
        self.width_at(0).unwrap_or(0) as i32
    }

    fn width_at(&self, y: i32) -> Option<i32> {
        Some(self.get(y as usize)?.len() as i32)
    }

    fn v_get(&self, pos: &Vec2) -> Option<&T> {
        self.get(pos.y as usize)?.get(pos.x as usize)
    }

    fn v_get_mut(&mut self, pos: &Vec2) -> Option<&mut T> {
        self.get_mut(pos.y as usize)?.get_mut(pos.x as usize)
    }

    fn row(&self, y: i32) -> RowIter2D<'_, T> {
        RowIter2D::new(self, y)
    }

    fn row_mut(&mut self, y: i32) -> RowIterMut2D<'_, T> {
        RowIterMut2D::new(self, y)
    }

    fn col(&self, x: i32) -> ColIter2D<'_, T> {
        ColIter2D::new(self, x)
    }

    fn col_mut(&mut self, x: i32) -> ColIterMut2D<'_, T> {
        ColIterMut2D::new(self, x)
    }

    fn neighbours(&self, pos: &Vec2) -> Neighbours2D<'_, T> {
        Neighbours2D::new(self, pos.neighbours().into_iter().collect())
    }

    fn neighbours_diagonally(&self, pos: &Vec2) -> Neighbours2D<'_, T> {
        Neighbours2D::new(self, pos.neighbours_diagonal().into_iter().collect())
    }

    fn neighbours_mut(&mut self, pos: &Vec2) -> NeighboursMut2D<'_, T> {
        NeighboursMut2D::new(self, pos.neighbours().into_iter().collect())
    }

    fn neighbours_diagonally_mut(&mut self, pos: &Vec2) -> NeighboursMut2D<'_, T> {
        NeighboursMut2D::new(self, pos.neighbours_diagonal().into_iter().collect())
    }

    fn around_pos<F>(&self, pos: &Vec2, mut f: F)
        where
            F: FnMut(&T, Vec2),
    {
        self.neighbours(pos).for_each(|(element, neighbour)| f(element, neighbour));
    }

    fn around_pos_diagonally<F>(&self, pos: &Vec2, mut f: F)
        where
            F: FnMut(&T, Vec2),
    {
        self.neighbours_diagonally(pos).for_each(|(element, neighbour)| f(element, neighbour));
    }

    fn around_pos_mut<F>(&mut self, pos: &Vec2, mut f: F)
        where
            F: FnMut(&mut T, Vec2),
    {
        self.neighbours_mut(pos).for_each(|(element, neighbour)| f(element, neighbour));
    }

    fn around_pos_diagonally_mut<F>(&mut self, pos: &Vec2, mut f: F)
        where
            F: FnMut(&mut T, Vec2),
    {
        self.neighbours_diagonally_mut(pos).for_each(|(element, neighbour)| f(element, neighbour));
    }

    fn flat_iter(&self) -> FlatIter2D<'_, T> {
        FlatIter2D::new(self)
    }

    fn flat_iter_mut(&mut self) -> FlatIterMut2D<'_, T> {
        FlatIterMut2D::new(self)
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
