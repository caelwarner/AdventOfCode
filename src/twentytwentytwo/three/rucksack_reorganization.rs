use util::input_as_str_vec;

fn main() {
    println!("{}", badges_priority(input_as_str_vec!()));
}

fn common_components_priority(input: Vec<&str>) -> u32 {
    let mut priority: u32 = 0;

    for line in input {
        let second_rucksack = line.split_at(line.len() / 2).1;
        let mut chars = line.chars();

        for _ in 0..(line.len() / 2) {
            let c = chars.next().unwrap();

            if second_rucksack.contains(c) {
                priority += to_priority(c);
                break;
            }
        }
    }

    priority
}

fn badges_priority(input: Vec<&str>) -> u32 {
    let mut priority: u32 = 0;

    for i in (0..input.len()).step_by(3) {
        let mut all_chars: Vec<char> = Vec::new();

        for j in 0..3 {
            for c in input[i + j].chars() {
                all_chars.push(c);
            }
        }

        for j in 0..3 {
            let mut line_chars: Vec<char> = Vec::new();

            for c in input[i + j].chars() {
                line_chars.push(c);
            }

            all_chars.retain(|c| line_chars.contains(c));
        }

        priority += to_priority(all_chars.remove(0));
    }

    priority
}

fn to_priority(c: char) -> u32 {
    return if c.is_uppercase() {
        c as u32 - 38
    } else {
        c as u32 - 96
    }
}
