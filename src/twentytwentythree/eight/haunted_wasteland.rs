use std::collections::HashMap;
use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;
use num::integer::lcm;
use regex::Regex;
use util::input_as_str_vec;
use util::regextools::CapturesTools;

fn main() {
    println!("Part 1: {}", steps_to_reach_end(input_as_str_vec!()));
    println!("Part 2: {}", ghost_steps_to_reach_end(input_as_str_vec!()));
}

fn steps_to_reach_end(input: Vec<&str>) -> u64 {
    path_loop_sizes(input)[0]
}

fn ghost_steps_to_reach_end(input: Vec<&str>) -> u64 {
    path_loop_sizes(input).into_iter()
        .reduce(|loop1, loop2| lcm(loop1, loop2))
        .unwrap()
}

fn path_loop_sizes(input: Vec<&str>) -> Vec<u64> {
    let re = Regex::new(r"(\w+) = \((\w+), (\w+)\)").unwrap();

    let nodes = input.iter()
        .skip(2)
        .map(|line| {
            let captures = re.captures(line).unwrap();
            (captures.to_string(1), (captures.to_string(2), captures.to_string(3)))
        })
        .collect::<HashMap<_, _>>();

    nodes.keys()
        .filter(|key| key.ends_with("A"))
        .sorted()
        .map(|start| {
            input[0].chars()
                .cycle()
                .fold_while((start.as_str(), 0), |(current, steps), dir| {
                    if current.ends_with("Z") {
                        Done((current, steps))
                    } else {
                        match dir {
                            'L' => Continue((nodes[current].0.as_str(), steps + 1)),
                            'R' => Continue((nodes[current].1.as_str(), steps + 1)),
                            _ => panic!("Invalid direction"),
                        }
                    }
                })
                .into_inner().1
        })
        .collect_vec()
}
