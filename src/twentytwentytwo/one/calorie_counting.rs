use util::input_as_str_vec;

fn main() {
    println!("{}", find_top_three_calories(input_as_str_vec!()));
}

fn find_max_calories(input: Vec<&str>) -> u32 {
    let mut max_calories: u32 = 0;
    let mut current_calories: u32 = 0;

    for line in input {
        if line != "" {
            current_calories += line.parse::<u32>().unwrap();
            continue;
        }

        if current_calories > max_calories {
            max_calories = current_calories;
        }

        current_calories = 0;
    }

    max_calories
}

fn find_top_three_calories(input: Vec<&str>) -> u32 {
    let mut calories: Vec<u32> = vec![];
    let mut current_calories: u32 = 0;

    for line in input {
        if line != "" {
            current_calories += line.parse::<u32>().unwrap();
            continue;
        }

        calories.push(current_calories);
        current_calories = 0;
    }

    calories.sort_by(|a, b| b.cmp(a));
    calories[0..=2].iter().sum()
}
