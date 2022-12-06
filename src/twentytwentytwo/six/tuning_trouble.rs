use util::input_as_str;
use itertools::Itertools;

fn main() {
    println!("{}", start_of_message(input_as_str!()));
}

fn start_of_packet(input: &str) -> usize {
    find_first_unique_slice(input, 4).unwrap()
}

fn start_of_message(input: &str) -> usize {
    find_first_unique_slice(input, 14).unwrap()
}

fn find_first_unique_slice(input: &str, slice_length: usize) -> Option<usize> {
    for i in 0..(input.len() - slice_length) {
        if &input[i..(i + slice_length)].chars().unique().count() == &slice_length {
            return Some(i + slice_length);
        }
    }

    None
}
