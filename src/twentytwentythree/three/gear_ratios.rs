use std::collections::HashMap;
use itertools::Itertools;
use util::arraytools::Array2D;
use util::input_as_2d_char_vec;
use util::vector::Vec2;

fn main() {
    println!("{}", sum_gear_ratios(input_as_2d_char_vec!()))
}

fn sum_part_numbers(input: Vec<Vec<char>>) -> u32 {
    get_part_numbers(&input).iter()
        .filter(|(pos, num)| {
            (0..num.ilog10() + 1).any(|i| {
                input.neighbours_diagonally(&pos.n_left(i as i32))
                    .any(|(c, _)| !c.is_ascii_digit() && *c != '.')
            })
        })
        .map(|(_, num)| num)
        .sum()
}

fn sum_gear_ratios(input: Vec<Vec<char>>) -> u32 {
    let part_numbers = get_part_numbers(&input);

    input.flat_iter()
        .filter(|(c, _)| **c == '*')
        .filter_map(|(_, pos)| {
            input.neighbours_diagonally(&pos)
                .filter(|(c, _)| c.is_ascii_digit())
                .map(|(_, neighbour)| {
                    (0..3).find_map(|i| part_numbers.get(&neighbour.n_right(i))).unwrap().clone()
                })
                .unique()
                .collect_tuple::<(u32, u32)>()
        })
        .map(|(first, last)| first * last)
        .sum()
}

fn get_part_numbers(input: &Vec<Vec<char>>) -> HashMap<Vec2, u32> {
    input.flat_iter()
        .filter_map(|(c, pos)| c.to_digit(10).map(|num| (pos, num)))
        .coalesce(|(a_pos, a), (b_pos, b)| {
            if a_pos == b_pos.left() {
                Ok((b_pos, a * 10 + b))
            } else {
                Err(((a_pos, a), (b_pos, b)))
            }
        })
        .collect()
}
