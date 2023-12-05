use itertools::Itertools;
use util::input_as_str_vec;
use util::itertools::AdventIter;

const NUMBERS: &[(&str, u32)] = &[
    ("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9),
    ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6), ("7", 7), ("8", 8), ("9", 9)
];

fn main() {
    println!("{}", sum_calibration_values_advanced(input_as_str_vec!()));
}

fn sum_calibration_values(input: Vec<&str>) -> u32 {
    input.iter()
        .map(|line| {
            let digits: Vec<u32> = line.chars()
                .filter_map(|c| c.to_digit(10))
                .collect();

            (digits.first().unwrap() * 10) + digits.last().unwrap()
        })
        .sum()
}

fn sum_calibration_values_advanced(input: Vec<&str>) -> u32 {
    input.iter()
        .map(|line| {
            let v: Vec<_> = NUMBERS.iter()
                .flat_map(|(str, num)|
                    line.match_indices(str)
                        .map(move |(i, _)| (i, *num)))
                .sorted()
                .first_last()
                .collect();

            (v.first().unwrap().1 * 10) + v.last().unwrap().1
        })
        .sum()
}
