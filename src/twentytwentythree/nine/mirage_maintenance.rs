use itertools::Itertools;
use util::input_as_str_vec;

fn main() {
    println!("Part 1: {}", extrapolate_future_oasis_values(input_as_str_vec!()));
    println!("Part 2: {}", extrapolate_past_oasis_values(input_as_str_vec!()));
}

fn extrapolate_future_oasis_values(input: Vec<&str>) -> i32 {
    input.iter()
        .map(|history| predict_next(
            history.split_whitespace()
                .filter_map(|n| n.parse().ok())
                .collect_vec()
        ))
        .sum()
}

fn extrapolate_past_oasis_values(input: Vec<&str>) -> i32 {
    input.iter()
        .map(|history| predict_next(
            history.split_whitespace()
                .filter_map(|n| n.parse().ok())
                .rev()
                .collect_vec()
        ))
        .sum()
}

fn predict_next(mut history: Vec<i32>) -> i32 {
    let mut predict = 0;

    while history.iter().any(|&value| value != 0) {
        predict += history.last().unwrap();
        history = history.iter()
            .tuple_windows()
            .map(|(v1, v2)| v2 - v1)
            .collect_vec()
    }

    predict
}
