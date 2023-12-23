use std::iter::once;
use std::time::Instant;
use itertools::{EitherOrBoth, Itertools, unfold};
use util::input_as_str_vec;
use util::itertools::AdventItertools;

fn main() {
    // println!("Part 1: {}", damaged_spring_arrangements(input_as_str_vec!()));
    println!("Part 2: {}", damaged_spring_arrangements_recursive(input_as_str_vec!()));
}

fn damaged_spring_arrangements(input: Vec<&str>) -> u32 {
    input.iter()
        .filter_map(|line| line.split_once(" "))
        .map(|(dr, record)| (
            dr.chars().collect_vec(),
            record.split(",").map(|n| n.parse::<u32>().unwrap()).collect_vec(),
        ))
        .map(|(mut damaged, accurate)| {
            let unknown = damaged.iter()
                .enumerate()
                .filter(|(_, &c)| c == '?')
                .map(|(i, _)| i)
                .collect_vec();

            (0..2usize.pow(unknown.len() as u32))
                .filter(|&i| {
                    unfold(i, |i| {
                        let next = if *i != 0 { Some(*i & 1) } else { None };
                        *i = *i >> 1;
                        next
                    }).pad_using(unknown.len(), |_| 0)
                        .enumerate()
                        .for_each(|(i, bit)| {
                            damaged[unknown[i]] = if bit == 1 { '#' } else { '.' };
                        });

                    damaged.iter()
                        .dedup_with_count()
                        .filter(|(_, &c)| c == '#')
                        .zip_longest(accurate.iter())
                        .all(|elems| {
                            if let EitherOrBoth::Both((record, _), &count) = elems {
                                record == count as usize
                            } else {
                                false
                            }
                        })
                    })
                .count() as u32
        })
        .sum()
}

fn damaged_spring_arrangements_recursive(input: Vec<&str>) -> u64 {
    let start = Instant::now();

    let out = input.iter()
        .filter_map(|line| line.split_once(" "))
        .map(|(damaged, accurate)| (
            damaged.chars().collect_vec(),
            accurate.split(",").map(|n| n.parse::<u32>().unwrap()).collect_vec(),
        ))
        .map(|(damaged, accurate)| {
            let b = find_all_arrangements(damaged.clone(), &accurate);
            let a = find_all_arrangements(damaged.clone().into_iter().chain(once('?')).chain(damaged.clone().into_iter()).collect(), &accurate.clone().into_iter().chain(accurate.clone().into_iter()).collect());

            b * (a / b).pow(4)
        })
        .inspect_dbg()
        .sum();

    dbg!(Instant::now() - start);
    out
}

fn find_all_arrangements(mut record: Vec<char>, accurate: &Vec<u32>) -> u64 {
    let unknown = record.iter()
        .enumerate()
        .filter(|(_, &c)| c == '?')
        .map(|(i, _)| i)
        .collect_vec();

    let springs = record.iter().filter(|&&c| c == '#').count() as u32;
    let max_springs = accurate.iter().sum();

    find_all_arrangements_recursive('#', &mut record, &unknown, &accurate, max_springs, springs + 1) +
    find_all_arrangements_recursive('.', &mut record, &unknown, &accurate, max_springs, springs)
}

fn find_all_arrangements_recursive(next: char, record: &mut Vec<char>, unknown: &[usize], accurate: &Vec<u32>, max_springs: u32, springs: u32) -> u64 {
    record[*unknown.first().unwrap()] = next;

    if unknown.len() == 1 {
        let valid = record.iter()
            .dedup_with_count()
            .filter(|(_, &c)| c == '#')
            .zip_longest(accurate.iter())
            .all(|elems| {
                if let EitherOrBoth::Both((record, _), &count) = elems {
                    record == count as usize
                } else {
                    false
                }
            });

        return if valid { 1 } else { 0 }
    }

    if springs < max_springs {
        find_all_arrangements_recursive('#', record, &unknown[1..], accurate, max_springs, springs + 1) +
        find_all_arrangements_recursive('.', record, &unknown[1..], accurate, max_springs, springs)
    } else {
        find_all_arrangements_recursive('.', record, &unknown[1..], accurate, max_springs, springs)
    }
}
