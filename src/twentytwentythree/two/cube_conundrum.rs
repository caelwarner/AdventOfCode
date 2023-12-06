use itertools::Itertools;
use util::input_as_str_vec;

fn main() {
    println!("{}", sum_power_of_fewest_cubes(input_as_str_vec!()))
}

fn sum_possible_games(input: Vec<&str>) -> u32 {
    input.iter().enumerate()
        .filter(|(_, game)| {
            !game.split_once(": ").unwrap().1
                .split("; ")
                .flat_map(|set| set.split(", "))
                .map_into::<Cube>()
                .any(|cube| cube.amount > cube.max_amount)
        })
        .map(|(i, _)| (i as u32) + 1)
        .sum()
}

fn sum_power_of_fewest_cubes(input: Vec<&str>) -> u32 {
    input.iter()
        .map(|game| {
            game.split_once(": ").unwrap().1
                .split("; ")
                .flat_map(|set| set.split(", "))
                .map_into::<Cube>()
                .into_group_map_by(|cube| cube.max_amount).iter()
                .map(|(_, group)| group.iter().map(|cube| cube.amount).max().unwrap())
                .product::<u32>()
        })
        .sum()
}

#[derive(Debug)]
struct Cube {
    amount: u32,
    max_amount: u32,
}

impl From<&str> for Cube {
    fn from(value: &str) -> Self {
        let split = value.split_once(" ").unwrap();

        Self {
            amount: split.0.parse::<u32>().unwrap(),
            max_amount: match split.1 {
                "red" => 12,
                "green" => 13,
                "blue" => 14,
                _ => panic!("{} is an invalid color", split.1),
            },
        }
    }
}
