use itertools::Itertools;
use util::input_as_str_vec;

fn main() {
    println!("Part 1: {}", sum_points(input_as_str_vec!()));
    println!("Part 2: {}", count_total_scratchcards(input_as_str_vec!()));
}

fn sum_points(input: Vec<&str>) -> u32 {
    parse_cards(input).iter()
        .filter(|(_, won_nums)| *won_nums > 0)
        .map(|(_, won_nums)| 2u32.pow(won_nums - 1))
        .sum()
}

fn count_total_scratchcards(input: Vec<&str>) -> u32 {
    let mut cards = parse_cards(input);

    for i in 0..cards.len() {
        let (count, won_nums) = cards[i];

        for j in 1..=won_nums as usize {
            cards[i + j].0 += count;
        }
    }

    cards.iter().map(|(count, _)| count).sum()
}

fn parse_cards(input: Vec<&str>) -> Vec<(u32, u32)> {
    input.iter()
        .filter_map(|line| line.split_once(": ").unwrap().1.split_once(" | "))
        .map(|(winning, nums)| {
            winning.split_whitespace()
                .cartesian_product(nums.split_whitespace())
                .filter(|(a, b)| a == b)
                .count()
        })
        .map(|n| (1, n as u32))
        .collect()
}
