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
    println!("{}", height_of_rocks(input_as_str!()))
}

fn height_of_rocks(input: &str) -> u32 {
    let mut chamber = vec![vec!['.'; 7]; 5000];
    let mut shape: usize = 0;

    let jet_pattern = input.chars().collect::<Vec<char>>();
    let mut jet: usize = 0;

    let mut height: u32 = 0;


    loop {
        let mut rock = Rock::new(Vec2::new(2, height as i32 + 3), SHAPES[shape % 5]);

        loop {
            let horizontal = match jet_pattern[jet % jet_pattern.len()] {
                '<' => -1,
                '>' => 1,
                _ => panic!("Non valid char"),
            };

            jet += 1;

            rock.move_horizontal(horizontal, &chamber);
            if !rock.try_move_vertical(-1, &mut chamber) {
                break;
            };
        }

        for i in (0..30).rev() {
            for j in 0..7 {
                print!("{}", &chamber[i][j]);
            }

            println!();
        }

        dbg!(jet);
        println!();
        println!();


        height = find_height(height, &chamber);

        shape += 1;
        if shape >= 37 {
            break;
        }
    }

    height
}

fn find_height(mut height: u32, chamber: &Vec<Vec<char>>) -> u32 {
    loop {
        if chamber[height as usize] != vec!['.'; 7] {
            height += 1;
            continue;
        }

        return height;
    }
}

struct Rock<'a> {
    pos: Vec2,
    shape: &'a [Vec2],
}

impl<'a> Rock<'a> {
    #[inline]
    const fn new(pos: Vec2, shape: &'a [Vec2]) -> Self {
        Rock {
            pos,
            shape,
        }
    }

    fn move_horizontal(&mut self, x: i32, chamber: &Vec<Vec<char>>) {
        for offset in self.shape {
            let pos = self.pos.offset(x, 0) + offset.clone();

            if pos.x < 0 || pos.x > 6 || chamber[pos] == '#' {
                return;
            }
        }

        self.pos.x += x;
    }

    fn try_move_vertical(&mut self, y: i32, chamber: &mut Vec<Vec<char>>) -> bool {
        for offset in self.shape {
            let pos = self.pos.offset(0, y) + offset.clone();


            if pos.y < 0 || chamber[pos] == '#' {
                for o in self.shape {
                    chamber[&self.pos + o] = '#'
                }

                return false;
            }
        }

        self.pos.y += y;
        true
    }
}
