use itertools::Itertools;
use util::input_as_str_vec;

use crate::Operation::{Add, Multiply};
use crate::OperationComponent::{Constant, Previous};

fn main() {
    println!("{}", monkey_business_high_worry(input_as_str_vec!()));
}

fn monkey_business(input: Vec<&str>) -> u64 {
    let mut monkeys: Vec<Monkey> = vec![];

    for i in (0..input.len()).step_by(7) {
        monkeys.push((&input[i..i + 7]).into())
    }

    for _ in 0..20 {
        complete_round(&mut monkeys, false, 3);
    }

    top_two_product(&monkeys)
}

fn monkey_business_high_worry(input: Vec<&str>) -> u64 {
    let mut monkeys: Vec<Monkey> = vec![];

    for i in (0..input.len()).step_by(7) {
        monkeys.push((&input[i..i + 7]).into())
    }

    let modulus: u128 = monkeys
        .iter()
        .map(|monkey| monkey.test.divisor)
        .product();

    for _ in 0..10_000 {
        complete_round(&mut monkeys, true, modulus);
    }

    top_two_product(&monkeys)
}

fn complete_round(monkeys: &mut Vec<Monkey>, modulo: bool, divisor: u128) {
    for i in 0..monkeys.len() {
        for _ in 0..monkeys[i].items.len() {
            let item = monkeys[i].items.remove(0);

            let mut throw_item = monkeys[i].operation.evalute(item.clone());
            throw_item = match modulo {
                true => throw_item % divisor,
                false => throw_item / divisor,
            };

            let throw_pos = monkeys[i].test.throw_pos(throw_item);

            monkeys[throw_pos].items.push(throw_item);
            monkeys[i].items_inspected += 1;
        }
    }
}

fn top_two_product(monkeys: &Vec<Monkey>) -> u64 {
    monkeys
        .iter()
        .map(|monkey| monkey.items_inspected)
        .sorted()
        .rev()
        .take(2)
        .product()
}

#[derive(Debug)]
struct Monkey {
    items: Vec<u128>,
    operation: Operation,
    test: Test,
    items_inspected: u64,
}

impl From<&[&str]> for Monkey {
    fn from(value: &[&str]) -> Self {
        let (_, starting_items) = value[1].split_once(": ").unwrap();
        let items = starting_items
            .split(", ")
            .map(|num|
                num.parse::<u128>().unwrap()
            ).collect::<Vec<u128>>();

        let (_, operation) = value[2].split_once("new = ").unwrap();

        Monkey {
            items,
            operation: operation.into(),
            test: (&value[3..7]).into(),
            items_inspected: 0,
        }
    }
}

#[derive(Debug)]
enum Operation {
    Add(OperationComponent, OperationComponent),
    Multiply(OperationComponent, OperationComponent),
}

impl Operation {
    fn evalute(&self, prev: u128) -> u128 {
        match self {
            Add(a, b) => a.get(prev) + b.get(prev),
            Multiply(a, b) => a.get(prev) * b.get(prev),
        }
    }
}

impl From<&str> for Operation {
    fn from(value: &str) -> Self {
        let (a, remaining) = value.split_once(" ").unwrap();
        let (operator, b) = remaining.split_once(" ").unwrap();

        match operator {
            "+" => Add(a.into(), b.into()),
            "*" => Multiply(a.into(), b.into()),
            _ => panic!("Unsupported operation"),
        }
    }
}

#[derive(Debug)]
enum OperationComponent {
    Previous,
    Constant(u128),
}

impl OperationComponent {
    fn get(&self, prev: u128) -> u128 {
        match self {
            Previous => prev,
            Constant(num) => num.clone(),
        }
    }
}

impl From<&str> for OperationComponent {
    fn from(value: &str) -> Self {
        match value {
            "old" => Previous,
            num => Constant(num.parse::<u128>().unwrap()),
        }
    }
}

#[derive(Debug)]
struct Test {
    divisor: u128,
    throw_true: usize,
    throw_false: usize,
}

impl Test {
    fn throw_pos(&self, dividend: u128) -> usize {
        if dividend % self.divisor == 0 {
            self.throw_true
        } else {
            self.throw_false
        }
    }
}

impl From<&[&str]> for Test {
    fn from(value: &[&str]) -> Self {
        let (_, divisible_by) = value[0].split_once("by ").unwrap();
        let divisor = divisible_by.parse::<u128>().unwrap();

        let (_, true_monkey) = value[1].split_once("monkey ").unwrap();
        let throw_true = true_monkey.parse::<usize>().unwrap();

        let (_, false_monkey) = value[2].split_once("monkey ").unwrap();
        let throw_false = false_monkey.parse::<usize>().unwrap();

        Test {
            divisor,
            throw_true,
            throw_false,
        }
    }
}
