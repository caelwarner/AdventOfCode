use std::collections::HashSet;
use util::input_as_2d_u32_vec;

fn main() {
    println!("{}", part2(input_as_2d_u32_vec!()));
}

fn part1(input: Vec<Vec<u32>>) -> u32 {
    let mut visible_trees: HashSet<(usize, usize)> = HashSet::new();

    // Left side visibility check
    for row in 0..input.len() {
        let mut tallest_tree: i32 = -1;
        for col in 0..input[row].len() {
            let tree = input[row][col] as i32;

            if tree > tallest_tree {
                visible_trees.insert((row, col));
                tallest_tree = tree;

                if tree == 9 {
                    break;
                }
            }
        }
    }

    // Right side visibility check
    for row in 0..input.len() {
        let mut tallest_tree: i32 = -1;
        for col in (0..input[row].len()).rev() {
            let tree = input[row][col] as i32;

            if tree > tallest_tree {
                visible_trees.insert((row, col));
                tallest_tree = tree;

                if tree == 9 {
                    break;
                }
            }
        }
    }

    // Top side visibility check
    for col in 0..input[0].len() {
        let mut tallest_tree: i32 = -1;
        for row in 0..input.len() {
            let tree = input[row][col] as i32;

            if tree > tallest_tree {
                visible_trees.insert((row, col));
                tallest_tree = tree;

                if tree == 9 {
                    break;
                }
            }
        }
    }

    // Bottom side visibility check
    for col in 0..input[0].len() {
        let mut tallest_tree: i32 = -1;
        for row in (0..input.len()).rev() {
            let tree = input[row][col] as i32;

            if tree > tallest_tree {
                visible_trees.insert((row, col));
                tallest_tree = tree;

                if tree == 9 {
                    break;
                }
            }
        }
    }

    visible_trees.len() as u32
}

fn part2(input: Vec<Vec<u32>>) -> u32 {
    let mut scenic_scores: HashSet<u32> = HashSet::new();

    for row in 1..input.len() - 1 {
        for col in 1..input[row].len() - 1 {
            let tree = input[row][col];

            let mut right_visibility: u32 = 0;
            for right in row + 1..input.len() {
                right_visibility += 1;
                if input[right][col] >= tree {
                    break;
                }
            }

            let mut left_visibility: u32 = 0;
            for left in (0..row).rev() {
                left_visibility += 1;
                if input[left][col] >= tree {
                    break;
                }
            }

            let mut down_visibility: u32 = 0;
            for down in col + 1..input.len() {
                down_visibility += 1;
                if input[row][down] >= tree {
                    break;
                }
            }

            let mut up_visibility: u32 = 0;
            for up in (0..col).rev() {
                up_visibility += 1;
                if input[row][up] >= tree {
                    break;
                }
            }

            scenic_scores.insert(right_visibility * left_visibility * down_visibility * up_visibility);
        }
    }

    scenic_scores.iter().max().unwrap().clone()
}
