use itertools::Itertools;

use util::arraytools::{Array2D, Rotation};
use util::input_as_2d_char_vec;
use util::itertools::AdventItertools;
use util::run::{Part, run};
use util::vector::Vec2;

fn main() {
    run(Part::One, || distance_between_galaxies(input_as_2d_char_vec!(), 2));
    run(Part::Two, || distance_between_galaxies(input_as_2d_char_vec!(), 1_000_000));
}

fn distance_between_galaxies(input: Vec<Vec<char>>, expansion_factor: i32) -> u64 {
    let expanded_rows = expand_rows(&input, expansion_factor);
    let expanded_cols = expand_rows(&input.rotate(Rotation::Right), expansion_factor);

    let galaxies = input.flat_iter()
        .filter(|(&c, _)| c == '#')
        .map(|(_, pos)| Vec2::new(expanded_cols[pos.x as usize], expanded_rows[pos.y as usize]))
        .collect::<Vec<Vec2>>();

    galaxies.iter()
        .cartesian_product(galaxies.iter())
        .map(|(g1, g2)| g1.distance(&g2) as u64)
        .sum::<u64>() / 2
}

#[inline]
fn expand_rows(image: &Vec<Vec<char>>, expansion_factor: i32) -> Vec<i32> {
    image.iter()
        .map(|row| if row.iter().all(|&c| c == '.') { expansion_factor } else { 1 })
        .prefix_sum()
        .collect()
}