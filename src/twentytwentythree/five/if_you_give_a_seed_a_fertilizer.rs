#![feature(iter_array_chunks)]

use std::ops::Range;
use itertools::Itertools;
use util::input_as_str;

fn main() {
    println!("Part 1: {}", find_lowest_location(input_as_str!()));
    println!("Part 2: {}", find_lowest_location_range(input_as_str!()));
}

fn find_lowest_location(input: &str) -> i64 {
    let input = input.replace("\r", "");
    let (seeds, maps) = input.split_once("\n\n").unwrap();
    let mut seeds = seeds.split_whitespace()
        .skip(1)
        .filter_map(|seed| seed.parse::<i64>().ok())
        .collect_vec();

    maps.split("\n\n")
        .map(|map| {
            map.lines()
                .skip(1)
                .map_into::<MapEntry>()
                .collect_vec()
        })
        .for_each(|map| {
            seeds.iter_mut().for_each(|seed| {
                *seed = map.iter()
                    .find(|entry| entry.source.contains(seed))
                    .map_or(*seed, |entry| *seed + entry.destination)
            });
        });

    *seeds.iter().min().unwrap()
}

fn find_lowest_location_range(input: &str) -> i64 {
    let input = input.replace("\r", "");
    let (seeds, maps) = input.split_once("\n\n").unwrap();
    let seeds = seeds.split_whitespace()
        .skip(1)
        .filter_map(|seed| seed.parse::<i64>().ok())
        .array_chunks()
        .map(|seed: [i64; 2]| (seed[0]..(seed[0] + seed[1])))
        .collect_vec();

    let maps = maps.split("\n\n")
        .map(|map| {
            map.lines()
                .skip(1)
                .map_into::<MapEntry>()
                .map(|entry| {
                    MapEntry {
                        source: (entry.source.start + entry.destination)..(entry.source.end + entry.destination),
                        destination: -entry.destination,
                    }
                })
                .collect_vec()
        })
        .collect_vec();

    for i in 1..i64::MAX {
        let mut location = i;

        maps.iter().rev().for_each(|map| {
            location = map.iter()
                .find(|entry| entry.source.contains(&location))
                .map_or(location, |entry| location + entry.destination);
        });

        if seeds.iter().find(|seed| seed.contains(&location)).is_some() {
            return i;
        }
    }

    panic!("Reached i64::MAX, and no solution found");
}

#[derive(Debug)]
struct MapEntry {
    source: Range<i64>,
    destination: i64,
}

impl From<&str> for MapEntry {
    fn from(value: &str) -> Self {
        let mut it = value.split_whitespace().filter_map(|n| n.parse::<i64>().ok());
        let destination = it.next().unwrap();
        let source = it.next().unwrap();
        let range = it.next().unwrap();

        Self {
            source: source..(source + range),
            destination: destination - source,
        }
    }
}
