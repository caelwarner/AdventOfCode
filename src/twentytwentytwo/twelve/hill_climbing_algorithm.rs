use std::collections::HashSet;

use util::input_as_2d_char_vec;
use util::arraytools::Array2D;
use util::vector::Vec2;

fn main() {
    println!("{}", most_scenic_path(input_as_2d_char_vec!()))
}

fn shortest_path_to_peak(input: Vec<Vec<char>>) -> u32 {
    let mut start = Vec2::default();
    let mut end = Vec2::default();

    for y in 0..input.len() {
        for x in 0..input[y].len() {
            if input[y][x] == 'S' {
                start = Vec2::new(x as i32, y as i32);
            } else if input[y][x] == 'E' {
                end = Vec2::new(x as i32, y as i32);
            }
        }
    }

    let mut hill = to_hill(&input);

    lowest_cost_path(&mut hill, &start, &end).unwrap()
}

fn most_scenic_path(input: Vec<Vec<char>>) -> u32 {
    let mut end = Vec2::default();
    let mut lowest_points: Vec<Vec2> = vec![];

    for y in 0..input.len() {
        for x in 0..input[y].len() {
            if input[y][x] == 'E' {
                end = Vec2::new(x as i32, y as i32);
            } else if x == 0 && input[y][x] == 'a' || input[y][x] == 'S' {
                lowest_points.push(Vec2::new(x as i32, y as i32));
            }
        }
    }

    let mut hill = to_hill(&input);
    let mut shortest_paths: HashSet<u32> = HashSet::new();

    for lowest_point in lowest_points {
        if let Some(path) = lowest_cost_path(&mut hill, &lowest_point, &end) {
            shortest_paths.insert(path);
        }
    }

    shortest_paths.iter().min().unwrap().clone()
}

fn to_hill(input: &Vec<Vec<char>>) -> Vec<Vec<Node>> {
    input.iter()
        .map(|row| {
            row.iter()
                .map(|c| {
                    Node::new(
                        match c {
                            'S' => 0,
                            'E' => 25,
                            _ => *c as u32 - 97,
                        }
                    )
                })
                .collect::<Vec<Node>>()
        })
        .collect::<Vec<Vec<Node>>>()
}

fn lowest_cost_path(hill: &mut Vec<Vec<Node>>, start: &Vec2, end: &Vec2) -> Option<u32> {
    let mut open: HashSet<Vec2> = HashSet::new();
    let mut closed: HashSet<Vec2> = HashSet::new();
    open.insert(start.clone());

    loop {
        if open.len() == 0 {
            return None;
        }

        let current = open
            .iter()
            .min_by_key(|pos| hill[pos.clone()].f_cost())
            .unwrap()
            .clone();

        open.remove(&current);
        closed.insert(current);

        if &current == end {
            let path_cost = hill[end].path_cost(&hill);

            for row in hill {
                for node in row {
                    node.parent = None;
                }
            }

            return Some(path_cost);
        }

        let current_election = hill[current].elevation;
        let current_g_cost = hill[current].g_cost;

        hill.around_pos_mut(&current, |neighbour, neighbour_pos| {
            if closed.contains(&neighbour_pos) || neighbour.elevation > current_election + 1 {
                return;
            }

            let movement_cost = current_g_cost + 1;
            if movement_cost < neighbour.g_cost || !open.contains(&neighbour_pos) {
                neighbour.g_cost = movement_cost;
                neighbour.h_cost = neighbour_pos.distance(&end) as u32;

                neighbour.parent = Some(current.clone());
                open.insert(neighbour_pos);
            }
        });
    }
}

#[derive(Debug)]
struct Node {
    elevation: u32,
    parent: Option<Vec2>,
    g_cost: u32,
    h_cost: u32,
}

impl Node {
    #[inline]
    fn new(elevation: u32) -> Self {
        Node {
            elevation,
            parent: None,
            g_cost: 0,
            h_cost: 0,
        }
    }

    #[inline]
    fn f_cost(&self) -> u32 {
        self.g_cost + self.h_cost
    }

    fn path_cost(&self, hill: &Vec<Vec<Node>>) -> u32 {
        if let Some(parent) = self.parent {
            hill[parent].path_cost(hill) + 1
        } else {
            0
        }
    }
}
