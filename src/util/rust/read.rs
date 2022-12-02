use std::fs;

pub fn as_string(path: &str) -> String {
    fs::read_to_string("src/".to_owned() + path).expect("File can't be read!")
}

pub fn as_string_vec(path: &str) -> Vec<String> {
    as_string(path).lines().map(|line| line.trim().to_string()).collect()
}
