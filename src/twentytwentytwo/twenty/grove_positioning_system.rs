use std::ptr;
use util::input_as_num_vec;
use util::math::Math;

const DECRYPTION_KEY: i64 = 811589153;

fn main() {
    println!("{}", grove_coordinates(input_as_num_vec!(i64), 10, true));
}

fn grove_coordinates(mut input: Vec<i64>, mix_amount: u32, decrypt: bool) -> i64 {
    if decrypt {
        for num in &mut input {
            *num *= DECRYPTION_KEY;
        }
    }

    let mut list = input.iter().map(|n| n).collect::<Vec<&i64>>();
    let original = list.clone();

    for _ in 0..mix_amount {
        mix(&mut list, &original);
    }

    let zero_pos = list.iter().position(|n| **n == 0).unwrap();
    list[(zero_pos + 1000) % list.len()] + list[(zero_pos + 2000) % list.len()] + list[(zero_pos + 3000) % list.len()]
}

fn mix<'a>(list: &mut Vec<&'a i64>, original: &Vec<&'a i64>) {
    for num in original {
        let i = list.iter().position(|n| ptr::eq(*n, *num)).unwrap();
        list.remove(i);
        list.insert((i as i64 + **num).modulo(list.len() as i64) as usize, num);
    }
}
