use util::input_as_str_vec;

fn main() {
    println!("{}", find_top_crates_advanced(input_as_str_vec!()));
}

fn find_top_crates(input: Vec<&str>) -> String {
    let empty_line = find_empty_line(&input);
    let mut crates = parse_crates(&input[0..empty_line]);

    crates.move_crates(&input[(empty_line + 1)..], |crates, instruction| {
        for _ in 0..instruction.quantity {
            let moved_crate = crates[instruction.from].pop().unwrap();
            crates[instruction.to].push(moved_crate);
        }
    });

    crates.get_top_crates()
}

fn find_top_crates_advanced(input: Vec<&str>) -> String {
    let empty_line = find_empty_line(&input);
    let mut crates = parse_crates(&input[0..empty_line]);

    crates.move_crates(&input[(empty_line + 1)..], |crates, instruction| {
        let split = crates[instruction.from].len() - instruction.quantity;

        let mut moved_crates = crates[instruction.from]
            .drain(split..)
            .collect::<Vec<char>>();

        crates[instruction.to].append(&mut moved_crates);
    });

    crates.get_top_crates()
}

struct Instruction {
    quantity: usize,
    from: usize,
    to: usize,
}

trait CargoCrane {
    fn move_crates(&mut self, instructions: &[&str], procedure: fn(&mut Vec<Vec<char>>, Instruction));
    fn get_top_crates(&self) -> String;
}

impl CargoCrane for Vec<Vec<char>> {
    fn move_crates(&mut self, instructions: &[&str], procedure: fn(&mut Vec<Vec<char>>, Instruction)) {
        for line in instructions {
            let mut instruction: Vec<usize> = vec![];

            for s in line.split(" ") {
                if let Ok(num) = s.parse::<usize>() {
                    instruction.push(num);
                }
            }

            procedure(self, Instruction {
                quantity: instruction[0],
                from: instruction[1] - 1,
                to: instruction[2] - 1,
            });
        }
    }

    fn get_top_crates(&self) -> String {
        let mut top_crates = String::new();

        for stack in self {
            top_crates.push(stack.last().unwrap().clone());
        }

        top_crates
    }
}

fn find_empty_line(input: &Vec<&str>) -> usize {
    input
        .iter()
        .position(|&line| line == "")
        .unwrap()
}

fn parse_crates(input: &[&str]) -> Vec<Vec<char>> {
    let stack_count = input
        .last().unwrap()
        .chars()
        .nth_back(1).unwrap()
        .to_digit(10).unwrap() as usize;

    let mut crates: Vec<Vec<char>> = vec![];
    for _ in 0..stack_count {
        crates.push(Vec::new());
    }

    for i in (0..input.len()).rev() {
        for (j, c) in input[i].char_indices() {
            if j == 0 || (j - 1) % 4 != 0 || c == ' ' {
                continue;
            }

            // Add crate to correct stack based of off char index
            crates[(j - 1) / 4].push(c);
        }
    }

    crates
}
