use util::input_as_str_vec;

fn main() {
    println!("{}", print_output_image(input_as_str_vec!()));
}

fn sum_signal_strengths(input: Vec<&str>) -> i32 {
    let mut cycle = 0;
    let mut register = 1;
    let mut signal_strengths = 0;

    for line in input {
        if line.starts_with("noop") {
            signal_strengths += next_cycle(&mut cycle, &register);
        } else {
            let (_, value) = line.split_once(" ").unwrap();

            signal_strengths += next_cycle(&mut cycle, &register);
            signal_strengths += next_cycle(&mut cycle, &register);

            register += value.parse::<i32>().unwrap();
        }
    }

    signal_strengths
}

fn print_output_image(input: Vec<&str>) -> u32 {
    let mut register = 1;

    let mut line: usize = 0;
    let mut instruction_end: usize = 0;
    let mut instruction = "";

    for i in 0..240 {
        if instruction_end == i {
            if instruction.starts_with("addx") {
                let (_, value) = instruction.split_once(" ").unwrap();
                register += value.parse::<i32>().unwrap();
            }

            instruction = input[line];
            line += 1;

            instruction_end = match instruction {
                "noop" => i + 1,
                _ => i + 2,
            };
        }

        if (register - 1..=register + 1).contains(&((i % 40) as i32)) {
            print!("{}", '#');
        } else {
            print!("{}", '.');
        }

        if (i + 1) % 40 == 0 {
            println!();
        }
    }

    0
}

fn next_cycle(cycle: &mut i32, register: &i32) -> i32 {
    *cycle += 1;

    if [20, 60, 100, 140, 180, 220].contains(&cycle) {
        *cycle * register
    } else {
        0
    }
}
