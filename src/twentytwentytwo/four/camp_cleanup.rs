use std::ops::RangeInclusive;
use util::input_as_str_vec;

fn main() {
    println!("{}", count_overlapping_ranges(input_as_str_vec!()))
}

fn count_enclosed_ranges(input: Vec<&str>) -> u32 {
    input.check_ranges(|range0, range1| {
        range0.contains(range1.start()) && range0.contains(range1.end())
            || range1.contains(range0.start()) && range1.contains(range0.end())
    })
}

fn count_overlapping_ranges(input: Vec<&str>) -> u32 {
    input.check_ranges(|range0, range1| {
        range0.contains(range1.start())
            || range0.contains(range1.end())
            || range1.contains(range0.start())
            || range1.contains(range0.end())
    })
}

type CleanupRange = RangeInclusive<u32>;

trait CleanupRanges {
    fn check_ranges(&self, predicate: fn(CleanupRange, CleanupRange) -> bool) -> u32;
}

impl CleanupRanges for Vec<&str> {
    fn check_ranges(&self, predicate: fn(CleanupRange, CleanupRange) -> bool) -> u32 {
        self.iter()
            .map(|line| {
                let ranges = line.split_once(",").unwrap();

                predicate(to_range(ranges.0), to_range(ranges.1))
            })
            .filter(|b| *b)
            .count() as u32
    }
}


fn to_range(range: &str) -> CleanupRange {
    let min_max = range.split_once("-").unwrap();

    min_max.0.parse::<u32>().unwrap()..=min_max.1.parse::<u32>().unwrap()
}
