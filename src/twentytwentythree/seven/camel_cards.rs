use itertools::Itertools;
use util::input_as_str_vec;

fn main() {
    println!("Part 1: {}", winnings(input_as_str_vec!()));
    println!("Part 2: {}", wildcard_winnings(input_as_str_vec!()));
}

fn winnings(input: Vec<&str>) -> u32 {
    let cards = input.iter()
        .filter_map(|line| line.split_once(" "))
        .map(|(hand, bid)| {
            (
                hand.chars()
                    .sorted()
                    .dedup_with_count()
                    .map(|(count, _)| count.pow(2) as u32)
                    .sum(),
                hand.to_string().replace('A', "Z").replace('K', "Y").replace('Q', "X").replace('T', "B"),
                bid.parse().unwrap(),
            )
        })
        .collect_vec();

    sum_winnings(cards)
}

fn wildcard_winnings(input: Vec<&str>) -> u32 {
    let cards = input.iter()
        .filter_map(|line| line.split_once(" "))
        .map(|(hand, bid)| {
            let mut counts = hand.chars().counts();

            if hand != "JJJJJ" {
                let jokers = counts.remove(&'J').unwrap_or(0);
                *counts.iter_mut()
                    .max_by_key(|(_, &mut c)| c)
                    .unwrap().1 += jokers;
            }

            (
                counts.iter()
                    .map(|(_, count)| count.pow(2) as u32)
                    .sum(),
                hand.replace('A', "Z").replace('K', "Y").replace('Q', "X").replace('J', "1"),
                bid.parse().unwrap(),
            )
        })
        .collect_vec();

    sum_winnings(cards)
}

fn sum_winnings(cards: Vec<(u32, String, u32)>) -> u32 {
    cards.iter()
        .sorted()
        .enumerate()
        .map(|(i, hand)| (i as u32 + 1) * hand.2)
        .sum()
}
