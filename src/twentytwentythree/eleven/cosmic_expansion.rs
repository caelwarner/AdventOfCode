use itertools::Itertools;
use util::arraytools::Array2D;
use util::input_as_2d_char_vec;
use util::vector::Vec2;

fn main() {
    println!("Part 1: {}", distance_between_galaxies(input_as_2d_char_vec!(), 2));
    println!("Part 2: {}", distance_between_galaxies(input_as_2d_char_vec!(), 1_000_000));
}

fn distance_between_galaxies(input: Vec<Vec<char>>, expansion_factor: u32) -> u64 {
    let expanded_rows = (0..input.height())
        .scan(0, |acc, i| {
            if input.row(i).all(|&c| c == '.') { *acc += expansion_factor - 1 }
            Some(*acc)
        })
        .collect_vec();

    let expanded_cols = (0..input.width())
        .scan(0, |acc, i| {
            if input.col(i).all(|&c| c == '.') { *acc += expansion_factor - 1 }
            Some(*acc)
        })
        .collect_vec();

    let galaxies = input.flat_iter()
        .filter(|(&c, _)| c == '#')
        .map(|(_, pos)| pos + (expanded_cols[pos.x as usize], expanded_rows[pos.y as usize]))
        .collect::<Vec<Vec2>>();

    galaxies.iter()
        .cartesian_product(galaxies.iter())
        .map(|(g1, g2)| g1.distance(&g2) as u64)
        .sum::<u64>() / 2
}
