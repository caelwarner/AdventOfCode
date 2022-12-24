use util::input_as_str_vec;
use util::math::Math;

fn main() {
    println!("{}", follow_strategy_guide_correctly(input_as_str_vec!()));
}

fn follow_strategy_guide(input: Vec<&str>) -> u32 {
    let mut score: u32 = 0;

    for line in input {
        let mut chars = line.chars();

        let opponent = to_value(chars.next().unwrap());
        let own = to_value(chars.skip(1).next().unwrap());

        score += calculate_round_score(own, opponent);
    }

    score
}

fn follow_strategy_guide_correctly(input: Vec<&str>) -> u32 {
    let mut score: u32 = 0;

    for line in input {
        let mut chars = line.chars();

        let opponent = to_value(chars.next().unwrap());
        let outcome = to_outcome(chars.skip(1).next().unwrap());
        let own = (opponent + outcome).modulo(3);

        score += calculate_round_score(own, opponent);
    }

    score
}

fn calculate_round_score(own: i32, opponent: i32) -> u32 {
    // Win
    if (own - 1).modulo(3) == opponent {
        return 7 + own as u32;

    // Draw
    } else if own == opponent {
        return 4 + own as u32;
    }

    // Lose
    own as u32 + 1
}

fn to_value(input: char) -> i32 {
    match input {
        'A' | 'X' => 0,
        'B' | 'Y' => 1,
        'C' | 'Z' => 2,
        _ => panic!("Wrong input char"),
    }
}

fn to_outcome(input: char) -> i32 {
    match input {
        'X' => -1,
        'Y' => 0,
        'Z' => 1,
        _ => panic!("Wrong input char"),
    }
}
