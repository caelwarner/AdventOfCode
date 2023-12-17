use crate::arraytools::Array2D;
use crate::vector::Vec2;

//
// RowIter2D
//
pub struct RowIter2D<'a, T: 'a> {
    array2d: &'a Vec<Vec<T>>,
    pos: Vec2,
}

impl<'a, T: 'a> RowIter2D<'a, T> {
    #[inline]
    pub const fn new(array2d: &'a Vec<Vec<T>>, y: i32) -> Self {
        Self {
            array2d,
            pos: Vec2::new(0, y),
        }
    }
}

impl<'a, T: 'a> Iterator for RowIter2D<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.array2d.v_get(&self.pos);
        self.pos += (1, 0);
        next
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = (self.array2d.width_at(self.pos.y).unwrap_or(0) - self.pos.x) as usize;
        (size, Some(size))
    }
}

impl<'a, T: 'a> DoubleEndedIterator for RowIter2D<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let next = self.array2d.v_get(&self.pos);
        self.pos -= (1, 0);
        next
    }
}

impl<'a, T: 'a> ExactSizeIterator for RowIter2D<'a, T> {}

pub struct RowIterMut2D<'a, T: 'a> {
    array2d: &'a mut Vec<Vec<T>>,
    pos: Vec2,
}

impl<'a, T: 'a> RowIterMut2D<'a, T> {
    #[inline]
    pub fn new(array2d: &'a mut Vec<Vec<T>>, y: i32) -> Self {
        Self {
            array2d,
            pos: Vec2::new(0, y),
        }
    }
}

impl<'a, T> Iterator for RowIterMut2D<'a, T>
{
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        // Check if ptr[x] exists, it can't be less than zero
        if self.pos.x >= self.array2d.width_at(self.pos.y)? {
            return None;
        }

        // width_at ensures the row exists
        let ptr = self.array2d[self.pos.y as usize].as_mut_ptr();
        
        let x = self.pos.x as usize;
        self.pos += (1, 0);

        // SAFETY: Just checked to see if ptr[x] exists
        Some(unsafe { &mut *ptr.add(x) })
    }
}

//
// ColIter2D
//
pub struct ColIter2D<'a, T: 'a> {
    array2d: &'a Vec<Vec<T>>,
    pos: Vec2,
}

impl<'a, T: 'a> ColIter2D<'a, T> {
    #[inline]
    pub const fn new(array2d: &'a Vec<Vec<T>>, x: i32) -> Self {
        Self {
            array2d,
            pos: Vec2::new(x, 0),
        }
    }
}

impl<'a, T: 'a> Iterator for ColIter2D<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.array2d.v_get(&self.pos);
        self.pos += (0, 1);
        next
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = (self.array2d.height() - self.pos.y) as usize;
        (size, Some(size))
    }
}

impl<'a, T: 'a> DoubleEndedIterator for ColIter2D<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let next = self.array2d.v_get(&self.pos);
        self.pos -= (0, 1);
        next
    }
}

impl<'a, T: 'a> ExactSizeIterator for ColIter2D<'a, T> {}

pub struct ColIterMut2D<'a, T: 'a> {
    array2d: &'a mut Vec<Vec<T>>,
    pos: Vec2,
}

impl<'a, T: 'a> ColIterMut2D<'a, T> {
    #[inline]
    pub fn new(array2d: &'a mut Vec<Vec<T>>, x: i32) -> Self {
        Self {
            array2d,
            pos: Vec2::new(x, 0),
        }
    }
}

impl<'a, T> Iterator for ColIterMut2D<'a, T>
{
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        // Check if ptr[x] exists
        if self.pos.x < 0 || self.pos.x >= self.array2d.width_at(self.pos.y)? {
            return None;
        }

        // width_at ensures that the row exists
        let ptr = self.array2d[self.pos.y as usize].as_mut_ptr();

        let x = self.pos.x as usize;
        self.pos += (0, 1);

        // SAFETY: Just checked to see if ptr[x] exists
        Some(unsafe { &mut *ptr.add(x) })
    }
}

//
// Neighbours2D
//
pub struct Neighbours2D<'a, T: 'a> {
    array2d: &'a Vec<Vec<T>>,
    neighbours: Vec<Vec2>,
    i: usize,
}

impl<'a, T: 'a> Neighbours2D<'a, T> {
    #[inline]
    pub const fn new(array2d: &'a Vec<Vec<T>>, neighbours: Vec<Vec2>) -> Self {
        Self {
            array2d,
            neighbours,
            i: 0,
        }
    }
}

impl<'a, T: 'a> Iterator for Neighbours2D<'a, T> {
    type Item = (&'a T, Vec2);

    fn next(&mut self) -> Option<Self::Item> {
        while self.i < self.neighbours.len() {
            let pos = self.neighbours[self.i];

            if let Some(next) = self.array2d.v_get(&pos) {
                self.i += 1;
                return Some((next, pos))
            }

            self.i += 1;
        }

        None
    }
}

pub struct NeighboursMut2D<'a, T: 'a> {
    array2d: &'a mut Vec<Vec<T>>,
    neighbours: Vec<Vec2>,
    i: usize,
}

impl<'a, T: 'a> NeighboursMut2D<'a, T> {
    #[inline]
    pub fn new(array2d: &'a mut Vec<Vec<T>>, neighbours: Vec<Vec2>) -> Self {
        Self {
            array2d,
            neighbours,
            i: 0,
        }
    }
}

impl<'a, T: 'a> Iterator for NeighboursMut2D<'a, T> {
    type Item = (&'a mut T, Vec2);

    fn next(&mut self) -> Option<Self::Item> {
        while self.i < self.neighbours.len() {
            let pos = self.neighbours[self.i];

            if let Some(row) = self.array2d.get_mut(pos.y as usize) {
                // Check if ptr[x] exists
                if pos.x < 0 || pos.x >= row.len() as i32 {
                    continue;
                }

                self.i += 1;
                let ptr = row.as_mut_ptr();

                // SAFETY: Just checked to see if ptr[x] exists
                return Some((unsafe { &mut *ptr.add(pos.x as usize) }, pos));
            }

            self.i += 1;
        }

        None
    }
}

//
// FlatIter2D
//
#[inline]
fn flat_iter_next_pos<T>(array2d: &Vec<Vec<T>>, mut pos: Vec2) -> Option<Vec2> {
    while pos.y < array2d.height() {
        if pos.x < array2d.width_at(pos.y)? {
            break;
        } else {
            pos.x = 0;
            pos += (0, 1);
        }
    }

    Some(pos)
}

pub struct FlatIter2D<'a, T: 'a> {
    array2d: &'a Vec<Vec<T>>,
    pos: Vec2,
}

impl<'a, T: 'a> FlatIter2D<'a, T> {
    #[inline]
    pub const fn new(array2d: &'a Vec<Vec<T>>) -> Self {
        Self {
            array2d,
            pos: Vec2::new(0, 0),
        }
    }
}

impl<'a, T: 'a> Iterator for FlatIter2D<'a, T> {
    type Item = (&'a T, Vec2);

    fn next(&mut self) -> Option<Self::Item> {
        self.pos = flat_iter_next_pos(self.array2d, self.pos)?;

        let next = Some((self.array2d.v_get(&self.pos)?, self.pos));
        self.pos += (1, 0);
        next
    }
}

pub struct FlatIterMut2D<'a, T: 'a> {
    array2d: &'a mut Vec<Vec<T>>,
    pos: Vec2,
}

impl<'a, T: 'a> FlatIterMut2D<'a, T> {
    #[inline]
    pub fn new(array2d: &'a mut Vec<Vec<T>>) -> Self {
        Self {
            array2d,
            pos: Vec2::new(0, 0),
        }
    }
}

impl<'a, T: 'a> Iterator for FlatIterMut2D<'a, T> {
    type Item = (&'a mut T, Vec2);

    fn next(&mut self) -> Option<Self::Item> {
        self.pos = flat_iter_next_pos(self.array2d, self.pos)?;

        let ptr = self.array2d.get_mut(self.pos.y as usize)?.as_mut_ptr();

        // SAFETY: get_mut call will return early if outside y bounds,
        //         loop above ensures that x is within bounds
        let next = Some((unsafe { &mut *ptr.add(self.pos.x as usize) }, self.pos));

        self.pos += (1, 0);
        next
    }
}
