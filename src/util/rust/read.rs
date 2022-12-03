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
            .map(|line| line.trim())
            .collect::<Vec<&str>>()
    }
}
