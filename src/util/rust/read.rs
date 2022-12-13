#[macro_export]
macro_rules! input_as_str {
    () => {
        include_str!("input.txt")
    }
}

#[macro_export]
macro_rules! input_as_str_vec {
    () => {
        include_str!("input.txt")
            .lines()
            .collect::<Vec<&str>>()
    }
}

#[macro_export]
macro_rules! input_as_2d_u32_vec {
    () => {
        include_str!("input.txt")
            .lines()
            .map(|line| {
                line.chars()
                    .map(|c| c.to_digit(10).unwrap())
                    .collect::<Vec<u32>>()
            })
            .collect::<Vec<Vec<u32>>>()
    }
}

#[macro_export]
macro_rules! input_as_2d_char_vec {
    () => {
        include_str!("input.txt")
            .lines()
            .map(|line| {
                line.chars()
                .collect::<Vec<char>>()
            })
            .collect::<Vec<Vec<char>>>()
    }
}
