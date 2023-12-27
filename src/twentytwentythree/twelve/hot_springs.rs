use std::collections::HashMap;
use std::iter::once;

use itertools::Itertools;

use util::input_as_str_vec;

fn main() {
    println!("Part 1: {}", find_all_arrangements(input_as_str_vec!()));
    println!("Part 2: {}", find_all_arrangements_unfolded(input_as_str_vec!()));
}

fn find_all_arrangements(input: Vec<&str>) -> u64 {
    input.iter()
        .copied()
        .map_into::<SpringRecord>()
        .map(|SpringRecord(damaged, accurate)| find_arrangements(&mut HashMap::new(), &damaged, &accurate))
        .sum()
}

fn find_all_arrangements_unfolded(input: Vec<&str>) -> u64 {
    input.iter()
        .copied()
        .map_into::<SpringRecord>()
        .map(|SpringRecord(damaged, accurate)| {
            find_arrangements(
                &mut HashMap::new(),
                &damaged.iter()
                    .copied()
                    .chain(once('?'))
                    .cycle()
                    .take(((damaged.len() + 1) * 5) - 1)
                    .collect_vec(),
                &accurate.repeat(5),
            )
        })
        .sum()
}

fn find_arrangements(cache: &mut HashMap<(usize, usize), u64>, record: &[char], accurate: &[usize]) -> u64 {
    let mut arrangements = 0;
    if let Some(value) = cache.get(&(record.len(), accurate.len())) {
        return *value;
    }

    if accurate.len() == 0 {
        return if record.contains(&'#') { 0 } else { 1 };
    }

    // Ensure there are not more damaged springs in accurate then there are total springs in record
    if accurate.iter().sum::<usize>() + accurate.len() - 1 > record.len() {
        return 0;
    }

    if record[0] == '.' || record[0] == '?' {
        arrangements += find_arrangements(cache, &record[1..], accurate);
    }

    if (record[0] == '#' || record[0] == '?') && !record.iter().take(accurate[0]).any(|&c| c == '.') {
        if accurate[0] == record.len() {
            return 1;
        } else if record[accurate[0]] != '#' {
            arrangements += find_arrangements(cache, &record[(accurate[0] + 1)..], &accurate[1..]);
        }
    }

    cache.insert((record.len(), accurate.len()), arrangements);
    arrangements
}

struct SpringRecord(Vec<char>, Vec<usize>);

impl From<&str> for SpringRecord {
    fn from(value: &str) -> Self {
        let (record, accurate) = value.split_once(" ").unwrap();
        SpringRecord(
            record.chars().collect_vec(),
            accurate.split(",").map(|n| n.parse::<usize>().unwrap()).collect_vec(),
        )
    }
}
