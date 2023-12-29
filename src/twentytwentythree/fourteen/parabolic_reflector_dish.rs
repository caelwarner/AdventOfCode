use std::collections::BTreeMap;

use itertools::Itertools;

use util::arraytools::{Array2D, Rotation};
use util::input_as_2d_char_vec;
use util::run::{Part, run};

fn main() {
    run(Part::One, || load_on_north_support_beams(input_as_2d_char_vec!()));
    run(Part::Two, || load_on_north_support_beams_cycles(input_as_2d_char_vec!()));
}

fn load_on_north_support_beams(mut input: Vec<Vec<char>>) -> i32 {
    input = input.rotate(Rotation::Right);
    tilt(&mut input);

    input.flat_iter()
        .filter(|(&c, _)| c == 'O')
        .map(|(_, pos)| pos.x + 1)
        .sum()
}

fn load_on_north_support_beams_cycles(mut input: Vec<Vec<char>>) -> i32 {
    let mut seen: BTreeMap<Vec<Vec<char>>, i32> = BTreeMap::new();
    let mut rotated = vec![vec!['.'; input.height() as usize]; input.width() as usize];

    for i in 0..10000 {
        for _ in 0..4 {
            input.rotate_into(Rotation::Right, &mut rotated);
            (input, rotated) = (rotated, input);
            tilt(&mut input);
        }

        if let Some(&first) = seen.get(&input) {
            let length = i - first;
            let dish = seen.iter()
                .sorted_by_key(|(_, &j)| -j)
                .find(|&(_, &j)| j % length == (1_000_000_000 - 1) % length)
                .unwrap().0;

            return dish.flat_iter()
                .filter(|(&c, _)| c == 'O')
                .map(|(_, pos)| input.height() - pos.y)
                .sum::<i32>();
        } else {
            seen.insert(input.clone(), i);
        }
    }

    panic!("No cycle found!");
}

#[inline]
fn tilt(dish: &mut Vec<Vec<char>>) {
    for row in dish {
        row.split_mut(|&c| c == '#').for_each(|group| group.sort());

        // *row = row.split(|&c| c == '#')
        //     .flat_map(|group| group.iter().copied().sorted().chain(['#']))
        //     .collect_vec();
        // row.pop();
    }
}
