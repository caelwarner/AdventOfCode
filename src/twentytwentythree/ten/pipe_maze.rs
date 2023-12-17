use std::cmp::min;
use std::collections::{HashMap, HashSet};
use itertools::Itertools;
use num::Integer;
use util::arraytools::Array2D;
use util::input_as_2d_char_vec;
use util::vector::Vec2;

fn main() {
    println!("Part 1: {}", furthest_point_from_starting(input_as_2d_char_vec!()));
    println!("Part 2: {}", enclosed_tiles(input_as_2d_char_vec!()));
}

fn furthest_point_from_starting(input: Vec<Vec<char>>) -> u32 {
    build_main_pipe(&input).len() as u32 / 2
}

fn enclosed_tiles(mut input: Vec<Vec<char>>) -> u32 {
    let pipe = build_main_pipe(&input);

    input.flat_iter_mut()
        .filter(|(_, pos)| !pipe.contains(pos))
        .for_each(|(tile, _)| *tile = '.');

    input.flat_iter()
        .filter(|(&tile, pos)| {
            tile == '.' &&
            barriers(input.row(pos.y).take(pos.x as usize).filter(|&&t| t != '.').counts(), '|').is_odd() &&
            barriers(input.col(pos.x).take(pos.y as usize).filter(|&&t| t != '.').counts(), '-').is_odd()
        })
        .count() as u32
}

fn build_main_pipe(input: &Vec<Vec<char>>) -> HashSet<Vec2> {
    let mut pipe = HashSet::new();

    let start = input.flat_iter().find(|(&c, _)| c == 'S').unwrap().1;
    pipe.insert(start);

    let mut prev = start;
    let mut pos = input.neighbours(&start)
        .filter(|(&tile, _)| tile != '.')
        .find(|(&tile, neighbour)| pipe_directions(tile, neighbour).iter().any(|&connect| connect == start))
        .unwrap().1;

    while pos != start {
        let next = pipe_directions(input[pos], &pos)
            .into_iter()
            .find(|&next| next != prev)
            .unwrap();

        pipe.insert(pos);
        prev = pos;
        pos = next;
    }

    pipe
}

fn pipe_directions(tile: char, pos: &Vec2) -> [Vec2; 2] {
    match tile {
        '|' => [pos.up(), pos.down()],
        '-' => [pos.left(), pos.right()],
        'L' => [pos.right(), pos.up()],
        'J' => [pos.left(), pos.up()],
        '7' => [pos.left(), pos.down()],
        'F' => [pos.right(), pos.down()],
        _ => panic!("Invalid pipe tile"),
    }
}

fn barriers(counts: HashMap<&char, usize>, straight: char) -> usize {
    min(*counts.get(&'L').unwrap_or(&0), *counts.get(&'7').unwrap_or(&0)) +
    min(*counts.get(&'F').unwrap_or(&0), *counts.get(&'J').unwrap_or(&0)) +
    *counts.get(&straight).unwrap_or(&0)
}
