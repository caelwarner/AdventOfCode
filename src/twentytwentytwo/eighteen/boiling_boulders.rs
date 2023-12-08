use std::collections::{HashSet, VecDeque};
use itertools::Itertools;
use util::arraytools::Array3D;
use util::input_as_str_vec;
use util::vector::Vec3;

fn main() {
    println!("{}", boulder_exterior_surface_area(input_as_str_vec!()));
}

fn boulder_surface_area(input: Vec<&str>) -> u32 {
    let cubes = to_cubes(input);
    let mut surface_area = cubes.len() as u32 * 6;

    for cube in &cubes {
        for neighbour in cube.neighbours() {
            if cubes.contains(&neighbour) {
                surface_area -= 1;
            }
        }
    }

    surface_area
}

fn boulder_exterior_surface_area(input: Vec<&str>) -> u32 {
    let mut boulder: Vec<Vec<Vec<bool>>> = vec![vec![vec![false; 22]; 22]; 22];
    let cubes = to_cubes(input);

    cubes.iter().for_each(|cube| boulder[cube.offset(1, 1, 1)] = true);

    traverse_boulder(&boulder)
}

fn to_cubes(input: Vec<&str>) -> Vec<Vec3> {
    input.iter()
        .map(|vec| vec
            .split(",")
            .collect_tuple::<(&str, &str, &str)>()
            .unwrap()
            .into()
        ).collect::<Vec<Vec3>>()
}

fn traverse_boulder(boulder: &Vec<Vec<Vec<bool>>>) -> u32 {
    let mut exterior_surfaces: HashSet<(Vec3, Vec3)> = HashSet::new();
    let mut queue: VecDeque<Vec3> = VecDeque::new();
    let mut visited: HashSet<Vec3> = HashSet::new();

    queue.push_back(Vec3::default());
    visited.insert(Vec3::default());

    while !queue.is_empty() {
        let cube = queue.pop_front().unwrap();

        boulder.around_pos(&cube, |b, neighbour| {
            if *b {
                exterior_surfaces.insert((cube, neighbour));
            } else if !visited.contains(&neighbour) {
                queue.push_back(neighbour);
                visited.insert(neighbour);
            }
        })
    }

    exterior_surfaces.len() as u32
}
