use util::input_as_str;
use util::vector::Vec2;

const SHAPES: [&[Vec2]; 5] = [
    &[Vec2::new(0, 0), Vec2::new(1, 0), Vec2::new(2, 0), Vec2::new(3, 0)],
    &[Vec2::new(0, 1), Vec2::new(1, 0), Vec2::new(1, 1), Vec2::new(1, 2), Vec2::new(2, 1)],
    &[Vec2::new(0, 0), Vec2::new(1, 0), Vec2::new(2, 0), Vec2::new(2, 1), Vec2::new(2, 2)],
    &[Vec2::new(0, 0), Vec2::new(0, 1), Vec2::new(0, 2), Vec2::new(0, 3)],
    &[Vec2::new(0, 0), Vec2::new(0, 1), Vec2::new(1, 0), Vec2::new(1, 1)],
];

fn main() {
    println!("{}", height_of_rocks(input_as_str!(), 1_000_000_000_000));
}

fn height_of_rocks(input: &str, rocks: u64) -> u64 {
    let mut states = vec![State::default(); input.len() * 4 * 5 * 4 * 7 * 4];
    let mut chamber = vec![vec!['.'; 7]; 25000];
    let mut prev_rocks = vec![0usize; 4];

    let jet_pattern = input.chars().collect::<Vec<char>>();
    let mut jet: usize = 0;

    let mut height: u64 = 0;
    let mut height_offset: u64 = 0;
    let mut fallen_rocks: u64 = 0;

    loop {
        let mut rock = Rock::new(Vec2::new(2, height as i32 + 3), fallen_rocks as usize % 5);

        loop {
            let horizontal = match jet_pattern[jet % jet_pattern.len()] {
                '<' => -1,
                '>' => 1,
                _ => panic!("Non valid char"),
            };

            jet += 1;

            rock.move_horizontal(horizontal, &chamber);
            if !rock.try_move_vertical(-1, &mut chamber) {
                if states.len() == 0 {
                    break;
                }

                prev_rocks.pop();
                prev_rocks.insert(0, jet % jet_pattern.len() * 5 * 7 + rock.shape * 7 + rock.pos.x as usize);

                let key = prev_rocks[3] * 4 + prev_rocks[2] * 3 + prev_rocks[1] * 2 + prev_rocks[0];

                if states[key] != State::default() {
                    let original = states[key];
                    let rocks_difference = fallen_rocks - original.fallen_rocks;
                    let height_difference = height - original.height;

                    let repeat_cycle = (rocks - original.fallen_rocks) / rocks_difference;

                    fallen_rocks = repeat_cycle * rocks_difference + original.fallen_rocks;
                    height_offset = (repeat_cycle - 1) * height_difference;

                    states.clear();

                } else {
                    states[key] = State { fallen_rocks, height };
                }

                break;
            };
        }

        height = find_height(height, &chamber);
        fallen_rocks += 1;

        if fallen_rocks >= rocks {
            break;
        }
    }

    height + height_offset
}

fn find_height(mut height: u64, chamber: &Vec<Vec<char>>) -> u64 {
    loop {
        if chamber[height as usize] == vec!['.'; 7] {
            return height;
        }

        height += 1;
    }
}

#[derive(Clone)]
struct Rock {
    pos: Vec2,
    shape: usize,
}

impl Rock {
    #[inline]
    const fn new(pos: Vec2, shape: usize) -> Self {
        Rock {
            pos,
            shape,
        }
    }

    fn move_horizontal(&mut self, x: i32, chamber: &Vec<Vec<char>>) {
        for shape in SHAPES[self.shape] {
            let pos = self.pos.offset(x, 0) + shape.clone();

            if pos.x < 0 || pos.x > 6 || chamber[pos] != '.' {
                return;
            }
        }

        self.pos.x += x;
    }

    fn try_move_vertical(&mut self, y: i32, chamber: &mut Vec<Vec<char>>) -> bool {
        for shape in SHAPES[self.shape] {
            let pos = self.pos.offset(0, y) + shape.clone();

            if pos.y < 0 || chamber[pos] != '.' {
                for o in SHAPES[self.shape] {
                    chamber[&self.pos + o] = '#'
                }

                return false;
            }
        }

        self.pos.y += y;
        true
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
struct State {
    fallen_rocks: u64,
    height: u64,
}
