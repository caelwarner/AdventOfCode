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
