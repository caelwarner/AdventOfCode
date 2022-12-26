use dot::{Edges, Id, LabelText, Nodes};
use lazy_static::lazy_static;
use regex::Regex;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fs::File;
use util::input_as_str_vec;
use util::regextools::CapturesTools;

type Monkeys = HashMap<String, Monkey>;
type Path = Vec<String>;

fn main() {
    println!("{}", human_yell(input_as_str_vec!()));
}

#[allow(dead_code)]
fn root_monkey_yell(input: Vec<&str>) -> i64 {
    let monkeys = into_monkeys(input);

    monkeys["root"].evaluate(&monkeys)
}

fn human_yell(input: Vec<&str>) -> i64 {
    let monkeys = into_monkeys(input);

    let mut path: Path = vec![];
    monkeys["root"].path_to_human(&mut path, &monkeys);
    let result = monkeys["root"].get_match(&path, &monkeys);

    monkeys[&path[0]].solve_for_human(result, 1, &path, &monkeys)
}

#[allow(dead_code)]
fn render_monkey_graph(input: Vec<&str>) {
    let monkeys = into_monkeys(input);

    let mut file = File::create("output.dot").unwrap();
    dot::render(
        &MonkeyGraph(
            monkeys
                .iter()
                .map(|(n, m)| (n.clone(), m.clone()))
                .collect(),
        ),
        &mut file,
    )
    .unwrap();
}

fn into_monkeys(input: Vec<&str>) -> Monkeys {
    let mut monkeys = HashMap::new();

    for line in input {
        let (name, monkey) = line.split_once(": ").unwrap();
        monkeys.insert(name.to_string(), monkey.into());
    }

    monkeys
}

#[derive(Clone, Debug, PartialEq)]
enum Monkey {
    Number(i64),
    Calculation(String, String, String),
}

impl From<&str> for Monkey {
    fn from(value: &str) -> Self {
        lazy_static! {
            static ref RE: Regex = Regex::new("(\\w+) (.) (\\w+)").unwrap();
        }

        if let Ok(num) = value.parse::<i64>() {
            return Monkey::Number(num);
        }

        let captures = RE.captures(value).unwrap();
        Monkey::Calculation(
            captures.to_string(1),
            captures.to_string(2),
            captures.to_string(3),
        )
    }
}

impl Monkey {
    fn evaluate(&self, monkeys: &Monkeys) -> i64 {
        match self {
            Monkey::Number(n) => n.clone(),
            Monkey::Calculation(a, operation, b) => {
                let a_result = monkeys[a].evaluate(monkeys);
                let b_result = monkeys[b].evaluate(monkeys);

                let result = match operation.as_str() {
                    "+" => a_result + b_result,
                    "-" => a_result - b_result,
                    "*" => a_result * b_result,
                    "/" => a_result / b_result,
                    _ => panic!(),
                };

                result
            }
        }
    }

    fn path_to_human(&self, path: &mut Path, monkeys: &Monkeys) {
        let mut branch = |next: &String| {
            path.push(next.clone());
            monkeys[next].path_to_human(path, monkeys);
            if path.last().unwrap() == "humn" {
                return;
            }
            path.pop();
        };

        if let Monkey::Calculation(a, _, b) = self {
            branch(a);
            branch(b);
        }
    }

    fn get_match(&self, path: &Path, monkeys: &Monkeys) -> i64 {
        if let Monkey::Calculation(a, _, b) = self {
            return if &path[0] != a {
                monkeys[a].evaluate(monkeys)
            } else {
                monkeys[b].evaluate(monkeys)
            };
        }

        0
    }

    fn solve_for_human(&self, result: i64, i: usize, path: &Path, monkeys: &Monkeys) -> i64 {
        if let Monkey::Calculation(a, operation, b) = self {
            let known = if &path[i] != a { a } else { b };
            let unknown = if &path[i] != a { b } else { a };
            let constant = monkeys[known].evaluate(monkeys);

            let is_known_left = if known == a { -1 } else { 1 };
            let x = match operation.as_str() {
                "+" => result - constant,
                "-" => (result * is_known_left) + constant,
                "*" => result / constant,
                "/" => (result * is_known_left) * constant,
                _ => panic!(),
            };

            return monkeys[unknown].solve_for_human(x, i + 1, path, monkeys);
        }

        result
    }
}

// Graph Visualization
struct MonkeyGraph(Vec<(String, Monkey)>);

impl<'a> dot::Labeller<'a, (String, Monkey), (String, String)> for MonkeyGraph {
    fn graph_id(&'a self) -> Id<'a> {
        Id::new("monkey_math").unwrap()
    }

    fn node_id(&'a self, n: &(String, Monkey)) -> Id<'a> {
        Id::new(n.0.clone()).unwrap()
    }

    fn node_label(&'a self, n: &(String, Monkey)) -> LabelText<'a> {
        match &n.1 {
            Monkey::Number(num) => LabelText::label(num.to_string()),
            Monkey::Calculation(_, op, _) => LabelText::label(op),
        }
        .prefix_line(LabelText::label(&n.0))
    }
}

impl<'a> dot::GraphWalk<'a, (String, Monkey), (String, String)> for MonkeyGraph {
    fn nodes(&'a self) -> Nodes<'a, (String, Monkey)> {
        Cow::Owned(self.0.iter().map(|(n, m)| (n.clone(), m.clone())).collect())
    }

    fn edges(&'a self) -> Edges<'a, (String, String)> {
        let mut edges = vec![];

        for (name, monkey) in &self.0 {
            if let Monkey::Calculation(a, _, b) = monkey {
                edges.push((name.clone(), a.clone()));
                edges.push((name.clone(), b.clone()));
            }
        }

        Cow::Owned(edges)
    }

    fn source(&'a self, edge: &(String, String)) -> (String, Monkey) {
        self.0
            .iter()
            .find(|(name, _)| name == &edge.0)
            .unwrap()
            .clone()
    }

    fn target(&'a self, edge: &(String, String)) -> (String, Monkey) {
        self.0
            .iter()
            .find(|(name, _)| name == &edge.1)
            .unwrap()
            .clone()
    }
}
