use std::collections::HashMap;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::iter::once;

use itertools::Itertools;

use util::input_as_str_vec;

fn main() {
    println!("Part 1: {}", find_all_arrangements(input_as_str_vec!()));
    println!("Part 2: {}", find_all_arrangements_unfolded(input_as_str_vec!()));
}

fn find_all_arrangements(input: Vec<&str>) -> u64 {
    let mut cache = HashMap::new();

    input.iter()
        .map_into::<SpringRecord>()
        .map(|SpringRecord(damaged, accurate)| find_arrangements(&mut cache, &damaged, &accurate))
        .sum()
}

fn find_all_arrangements_unfolded(input: Vec<&str>) -> u64 {
    let mut cache = HashMap::new();

    input.iter()
        .map_into::<SpringRecord>()
        .map(|SpringRecord(damaged, accurate)| {
            find_arrangements(
                &mut cache,
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

fn find_arrangements(cache: &mut HashMap<u64, u64>, record: &[char], accurate: &[usize]) -> u64 {
    let mut arrangements = 0;
    let hash = hash(record, accurate);

    if cache.contains_key(&hash) {
        return cache[&hash];
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

    cache.insert(hash, arrangements);
    arrangements
}

fn hash(record: &[char], accurate: &[usize]) -> u64 {
    let mut hasher = DefaultHasher::new();
    record.hash(&mut hasher);
    accurate.hash(&mut hasher);

    hasher.finish()
}

struct SpringRecord(Vec<char>, Vec<usize>);

impl From<&&str> for SpringRecord {
    fn from(value: &&str) -> Self {
        let (record, accurate) = value.split_once(" ").unwrap();
        SpringRecord(
            record.chars().collect_vec(),
            accurate.split(",").map(|n| n.parse::<usize>().unwrap()).collect_vec(),
        )
    }
}
