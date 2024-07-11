use array_init::array_init;

use util::input_as_str;
use util::run::{Part, run};

fn main() {
    run(Part::One, || hash_initialization_sequence(input_as_str!()));
    run(Part::Two, || find_focusing_power(input_as_str!()));
}

fn hash_initialization_sequence(input: &str) -> usize {
    input.split(',')
        .map(hash)
        .sum()
}

fn find_focusing_power(input: &str) -> usize {
    let mut map: [Vec<(String, usize)>; 256] = array_init(|_| Vec::new());

    input.split(',')
        .for_each(|step| {
            if let Some((label, _)) = step.split_once('-') {
                map[hash(label)].retain(|(l, _)| label != l);
            } else if let Some((label, focal_length)) = step.split_once('=') {
                if let Some((_, f)) = map[hash(label)].iter_mut().find(|(l, _)| label == l) {
                    *f = focal_length.parse::<usize>().unwrap();
                } else {
                    map[hash(label)].push((
                        label.to_string(),
                        focal_length.parse::<usize>().unwrap(),
                    ));
                }
            }
        });

    map.iter()
        .enumerate()
        .flat_map(|(i, container)| {
            container.iter()
                .enumerate()
                .map(move |(j, lens)| (i + 1) * (j + 1) * lens.1)
        })
        .sum()
}

#[inline]
fn hash(value: &str) -> usize {
    value.chars().fold(0, |acc, c| ((acc + c as usize) * 17) % 256)
}
