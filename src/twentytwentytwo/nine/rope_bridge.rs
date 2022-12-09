use std::collections::HashSet;
use util::input_as_str_vec;
use util::vector::Vec2;

fn main() {
    println!("{}", simulate_two_knot_rope(input_as_str_vec!()))
}

fn simulate_two_knot_rope(input: Vec<&str>) -> u32 {
    simulate_rope(&mut [Vec2::default(); 2], input)
}

fn simulate_ten_knot_rope(input: Vec<&str>) -> u32 {
    simulate_rope(&mut [Vec2::default(); 10], input)
}

fn simulate_rope(rope: &mut [Vec2], input: Vec<&str>) -> u32 {
    let mut visited_positions: HashSet<Vec2> = HashSet::new();

    for line in input {
        let instruction = line.split_once(" ").unwrap();

        let direction = to_pos_offset(instruction.0);
        let distance = instruction.1.parse::<usize>().unwrap();

        for _ in 0..distance {
            rope[0] += direction;

            for i in 0..rope.len() - 1 {
                if !rope[i + 1].is_touching(&rope[i]) {
                    rope[i + 1].follow(&rope[i].clone());
                } else {
                    break;
                }
            }

            visited_positions.insert(rope.last().unwrap().clone());
        }
    }

    visited_positions.len() as u32
}

fn to_pos_offset(input: &str) -> Vec2 {
    match input {
        "R" => (1, 0).into(),
        "L" => (-1, 0).into(),
        "U" => (0, 1).into(),
        "D" => (0, -1).into(),
        _ => panic!("Not a valid instruction"),
    }
}

trait Knot {
    fn is_touching(&self, other: &Vec2) -> bool;
    fn follow(&mut self, to_follow: &Vec2);
}

impl Knot for Vec2 {
    fn is_touching(&self, other: &Vec2) -> bool {
        (self - other).abs() <= 1
    }

    fn follow(&mut self, to_follow: &Vec2) {
        *self += (to_follow - self).clamp(-1, 1);
    }
}
