#![feature(let_chains)]

use std::cmp::Ordering;
use std::collections::VecDeque;

use util::input_as_str_vec;

fn main() {
    println!("{}", sort_packets(input_as_str_vec!()));
}

fn correct_pairs(input: Vec<&str>) -> u32 {
    let mut pairs: Vec<(Packet, Packet)> = vec![];

    for i in (0..input.len()).step_by(3) {
        pairs.push((
            (&mut input[i].chars().collect::<VecDeque<char>>()).into(),
            (&mut input[i + 1].chars().collect::<VecDeque<char>>()).into(),
        ));
    }

    pairs
        .iter()
        .map(|pair| compare_packets(&pair.0, &pair.1).unwrap())
        .enumerate()
        .map(|(i, correct)| if correct { i as u32 + 1 } else { 0 })
        .sum()
}

fn sort_packets(input: Vec<&str>) -> u32 {
    let mut packets: Vec<Packet> = vec![];

    let divider_packets = vec![
        Packet::List(vec![Packet::List(vec![Packet::Integer(2)])]),
        Packet::List(vec![Packet::List(vec![Packet::Integer(6)])]),
    ];

    packets.append(&mut divider_packets.clone());

    for line in input {
        if line.len() != 0 {
            packets.push((&mut line.chars().collect::<VecDeque<char>>()).into());
        }
    }

    packets.sort_by(|a, b| {
        match compare_packets(a, b).unwrap() {
            true => Ordering::Less,
            false => Ordering::Greater,
        }
    });

    packets
        .iter()
        .enumerate()
        .map(|(i, packet)| {
            if divider_packets.contains(packet) { i as u32 + 1 } else { 1 }
        })
        .product()
}

#[derive(Debug, Clone, PartialEq)]
enum Packet {
    Integer(u32),
    List(Vec<Packet>),
}

impl From<&mut VecDeque<char>> for Packet {
    fn from(value: &mut VecDeque<char>) -> Self {
        if value[0] == ',' {
            value.pop_front();
        }

        if value[0] != '[' {
            let num = if value[0] == '1' && value[1] == '0' {
                value.pop_front();
                value.pop_front();

                10 as u32
            } else {
                value.pop_front().unwrap().to_digit(10).unwrap()
            };

            return Packet::Integer(num);
        }

        value.pop_front();
        let mut packets: Vec<Packet> = vec![];

        while value[0] != ']' {
            packets.push(value.into())
        }

        value.pop_front();

        Packet::List(packets)
    }
}

fn compare_packets(left: &Packet, right: &Packet) -> Option<bool> {
    if let Packet::Integer(left_num) = left && let Packet::Integer(right_num) = right {
        return if left_num == right_num {
            None
        } else {
            Some(left_num < right_num)
        }
    }

    let left_packets = match left {
        Packet::Integer(_) => vec![left.clone()],
        Packet::List(packets) => packets.clone(),
    };

    let right_packets = match right {
        Packet::Integer(_) => vec![right.clone()],
        Packet::List(packets) => packets.clone(),
    };

    for i in 0..left_packets.len() {
        if i >= right_packets.len() {
            return Some(false);
        }

        let correct = compare_packets(&left_packets[i], &right_packets[i]);
        if correct.is_some() {
            return correct;
        }
    }

    return if left_packets.len() < right_packets.len() {
        Some(true)
    } else {
        None
    }
}
