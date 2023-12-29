use itertools::Itertools;

use util::arraytools::{Array2D, Rotation};
use util::input_as_str_vec;
use util::run::{Part, run};

fn main() {
    run(Part::One, || summarize_pattern_notes(input_as_str_vec!(), 0));
    run(Part::Two, || summarize_pattern_notes(input_as_str_vec!(), 1));
}

fn summarize_pattern_notes(input: Vec<&str>, valid_lines_offset: i32) -> i32 {
    input.split(|&s| s == "")
        .map(|pattern| pattern.iter().map(|line| line.chars().collect_vec()).collect_vec())
        .map(|pattern| {
            find_vertical_reflections(&pattern)
                .find(|&(count, _)| count as i32 == pattern.height() - valid_lines_offset)
                .map(|(_, reflection)| reflection)
                .unwrap_or_else(|| {
                    find_vertical_reflections(&pattern.rotate(Rotation::Left))
                        .find(|&(count, _)| count as i32 == pattern.width() - valid_lines_offset)
                        .map(|(_, reflection)| reflection * 100)
                        .expect("No vertical or horizontal reflections found")
                })
        })
        .sum()
}

fn find_vertical_reflections(pattern: &Vec<Vec<char>>) -> impl Iterator<Item=(usize, i32)> {
    (0..pattern.height())
        .flat_map(|y| {
            (1..pattern.width())
                .filter(|x| {
                    pattern.row(y).take(*x as usize).rev()
                        .zip(pattern.row(y).skip(*x as usize))
                        .all(|(a, b)| a == b)
                })
                .collect_vec()
        })
        .sorted()
        .dedup_with_count()
}
