use itertools::Itertools;
use util::input_as_str_vec;

fn main() {
    println!("Part 1: {}", win_strategies_product(input_as_str_vec!()));
    println!("Part 2: {}", win_strategies_product_big_race(input_as_str_vec!()));
}

fn win_strategies_product(input: Vec<&str>) -> u32 {
    input[0].split_whitespace().skip(1).filter_map(|n| n.parse::<u32>().ok())
        .zip(input[1].split_whitespace().skip(1).filter_map(|n| n.parse::<u32>().ok()))
        .map(|(time, distance)| {
            (1..time).into_iter().filter(|t| t * (time - t) > distance).count() as u32
        })
        .product()
}

fn win_strategies_product_big_race(input: Vec<&str>) -> u64 {
    let time = input[0].split_whitespace().skip(1).join("").parse::<f64>().unwrap();
    let distance = input[1].split_whitespace().skip(1).join("").parse::<f64>().unwrap();

    let d = (time * time) - 4.0f64 * distance;
    let r1 = (-time + d.sqrt()) / -2.0;
    let r2 = (-time - d.sqrt()) / -2.0;

    r2 as u64 - r1 as u64 - if r1.fract() == 0.0 && r2.fract() == 0.0 { 1 } else { 0 }
}
