use util::input_as_str_vec;
use util::vector::Vec2;

const SOURCE: Vec2 = Vec2::new(500, 0);

fn main() {
    println!("{}", max_static_sand_particles_with_floor(input_as_str_vec!()));
}

fn max_static_sand_particles(input: Vec<&str>) -> u32 {
    let mut static_sand: u32 = 0;

    let (rocks, width, height) = to_rock_positions(input);
    let mut reservoir = to_reservoir(rocks, width, height);

    loop {
        let mut sand = SOURCE.clone();

        loop {
            if sand.y as usize == reservoir.len() - 1 {
                return static_sand;
            }

            if reservoir[sand.offset(0, 1)] == '.' {
                sand += Vec2::new(0, 1);

            } else if reservoir[sand.offset(-1, 1)] == '.' {
                sand += Vec2::new(-1, 1);

            } else if reservoir[sand.offset(1, 1)] == '.' {
                sand += Vec2::new(1, 1);

            } else {
                reservoir[sand] = '=';
                break;
            }
        }

        static_sand += 1;
    }
}

fn max_static_sand_particles_with_floor(input: Vec<&str>) -> u32 {
    let mut static_sand: u32 = 0;

    let (rocks, width, height) = to_rock_positions(input);
    let mut reservoir = to_reservoir(rocks, width * 2, height + 2);

    // Draw floor
    for i in 0..reservoir.last().unwrap().len() {
        reservoir.last_mut().unwrap()[i] = '#';
    }

    loop {
        let mut sand = SOURCE;

        loop {
            if reservoir[sand.offset(0, 1)] == '.' {
                sand += Vec2::new(0, 1);

            } else if reservoir[sand.offset(-1, 1)] == '.' {
                sand += Vec2::new(-1, 1);

            } else if reservoir[sand.offset(1, 1)] == '.' {
                sand += Vec2::new(1, 1);

            } else if sand == SOURCE {
                return static_sand + 1;

            } else {
                reservoir[sand] = '=';
                break;
            }
        }

        static_sand += 1;
    }
}

fn to_rock_positions(input: Vec<&str>) -> (Vec<Vec<Vec2>>, usize, usize) {
    let mut rocks: Vec<Vec<Vec2>> = vec![];

    let mut width: usize = 0;
    let mut height: usize = 0;

    for line in input {
        let mut rock: Vec<Vec2> = vec![];

        for pos in line.split(" -> ") {
            let vec: Vec2 = pos.split_once(",").unwrap().into();

            if vec.x + 1 > width as i32 { width = vec.x as usize + 1 }
            if vec.y + 1 > height as i32 { height = vec.y as usize + 1 }

            rock.push(vec);
        }

        rocks.push(rock);
    }

    (rocks, width, height)
}

fn to_reservoir(rocks: Vec<Vec<Vec2>>, width: usize, height: usize) -> Vec<Vec<char>> {
    let mut reservoir: Vec<Vec<char>> = vec![vec!['.'; width]; height];

    // Mark rocks
    for rock in rocks {
        for i in 0..rock.len() - 1 {
            let a = rock[i];
            let b = rock[i + 1];

            // Draw vertical line
            if a.x == b.x {
                for mut y in 0..=(a.y - b.y).abs() {
                    if a.y > b.y { y *= -1; }
                    reservoir[a.offset(0, y)] = '#';
                }

            // Draw horizontal line
            } else if a.y == b.y {
                for mut x in 0..=(a.x - b.x).abs() {
                    if a.x > b.x { x *= -1; }
                    reservoir[a.offset(x, 0)] = '#';
                }
            }
        }
    }

    reservoir
}
