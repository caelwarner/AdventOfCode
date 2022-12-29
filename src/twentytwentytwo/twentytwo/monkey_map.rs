use crate::Instruction::{Left, Move, Right};
use itertools::Itertools;
use util::input_as_str_vec;
use util::multidimensional::TwoDimensional;
use util::vector::Vec2;

type Map = Vec<Vec<char>>;

fn main() {
    println!("{}", secret_password(input_as_str_vec!()));
}

fn secret_password(input: Vec<&str>) -> i32 {
    let map = into_map(&input);
    let instructions = into_instructions(&input);

    let mut direction = 0;
    let mut pos = Vec2 {
        x: map[0].iter().position(|&c| c != ' ').unwrap() as i32,
        y: 0,
    };

    for instruction in &instructions {
        if let Move(distance) = instruction {
            for _ in 0..*distance {
                let mut next = instruction.move_forward(direction, &pos);
                next = try_wrap(direction, next, &map);

                if map[next] == '#' {
                    break;
                }

                pos = next;
            }
        } else {
            direction += instruction.turn();
        }
    }

    (pos.y + 1) * 1000 + (pos.x + 1) * 4 + direction.rem_euclid(4)
}

fn into_map(input: &Vec<&str>) -> Map {
    let mut map = input
        .iter()
        .map_while(|&line| match line {
            "" => None,
            _ => Some(line.chars().collect()),
        })
        .collect::<Map>();

    let width = map.iter().map(|r| r.len()).max().unwrap();
    for row in &mut map {
        row.append(&mut vec![' '; width - row.len()]);
    }

    map
}

fn into_instructions(input: &Vec<&str>) -> Vec<Instruction> {
    input
        .last()
        .unwrap()
        .split_inclusive(&['L', 'R'])
        .flat_map(|s| {
            let split = s.split_at(s.len() - 1);
            [split.0, split.1]
        })
        .filter(|&s| s != "")
        .map_into()
        .collect()
}

fn try_wrap(direction: i32, mut pos: Vec2, map: &Map) -> Vec2 {
    let original = pos;
    if map.vget(pos).unwrap_or(&' ') != &' ' {
        return original;
    }

    let y_modulo = map.height() as i32;
    let x_modulo = map.width_at(pos.y.rem_euclid(y_modulo) as usize).unwrap() as i32;
    let offset = direction_to_offset(direction);

    loop {
        pos = Vec2::new(pos.x.rem_euclid(x_modulo), pos.y.rem_euclid(y_modulo));

        if map[pos] != ' ' {
            break;
        } else if map[pos] == '#' {
            return original - offset;
        }

        pos += offset;
    }

    pos
}

fn direction_to_offset(direction: i32) -> Vec2 {
    match direction.rem_euclid(4) {
        0 => Vec2::new(1, 0),
        1 => Vec2::new(0, 1),
        2 => Vec2::new(-1, 0),
        3 => Vec2::new(0, -1),
        _ => panic!("Invalid direction"),
    }
}

#[derive(Debug)]
enum Instruction {
    Move(i32),
    Left,
    Right,
}

impl From<&str> for Instruction {
    fn from(value: &str) -> Self {
        match value {
            "L" => Left,
            "R" => Right,
            _ => Move(value.parse::<i32>().unwrap()),
        }
    }
}

impl Instruction {
    fn move_forward(&self, direction: i32, pos: &Vec2) -> Vec2 {
        if let Move(_) = self {
            return pos + &direction_to_offset(direction);
        }

        panic!("Can't move on 'Left' or 'Right' instruction");
    }

    fn turn(&self) -> i32 {
        match self {
            Left => -1,
            Right => 1,
            Move(_) => panic!("Can't turn on 'Move' instruction"),
        }
    }
}
