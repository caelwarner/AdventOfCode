use std::collections::HashSet;
use util::arraytools::Array2D;
use util::input_as_2d_char_vec;
use util::run::{run, Part};
use util::vector::Vec2;

fn main() {
    run(Part::One, || tiles_energized(Vec2::new(0, 0), Vec2::new(1, 0), &input_as_2d_char_vec!()));
    run(Part::Two, || most_tiles_energized(input_as_2d_char_vec!()));
}

fn most_tiles_energized(input: Vec<Vec<char>>) -> usize {
    (0..input.width()).map(|x| (Vec2::new(x, 0), Vec2::new(0, 1)))
        .chain((0..input.height()).map(|y| (Vec2::new(0, y), Vec2::new(1, 0))))
        .chain((0..input.width()).map(|x| (Vec2::new(x, input.height() - 1), Vec2::new(0, -1))))
        .chain((0..input.height()).map(|y| (Vec2::new(input.width() - 1, y), Vec2::new(-1, 0))))
        .map(|(pos, direction)| tiles_energized(pos, direction, &input))
        .max()
        .unwrap()
}

fn tiles_energized(pos: Vec2, direction: Vec2, input: &Vec<Vec<char>>) -> usize {
    let mut energized = HashSet::new();
    energize_tiles(pos, direction, &input, &mut energized);
    
    energized.iter().map(|(p, _)| p).collect::<HashSet<_>>().len()
}

fn energize_tiles(
    mut pos: Vec2,
    mut direction: Vec2,
    grid: &Vec<Vec<char>>,
    energized: &mut HashSet<(Vec2, Vec2)>,
) {
    while pos.in_bounds(&grid) && !energized.contains(&(pos, direction)) {
        energized.insert((pos, direction));
        
        let c = grid[pos];
        if c == '/' {
            direction = (-direction.y, -direction.x).into();
            
        } else if c == '\\' {
            direction = (direction.y, direction.x).into();
            
        } else if c == '-' && direction.y != 0 {
            energize_tiles(pos.left(), (-1, 0).into(), grid, energized);
            energize_tiles(pos.right(), (1, 0).into(), grid, energized);
            
            break;
        
        } else if c == '|' && direction.x != 0 {
            energize_tiles(pos.up(), (0, -1).into(), grid, energized);
            energize_tiles(pos.down(), (0, 1).into(), grid, energized);
         
            break;
        }
        
        pos += direction;
    }
}
