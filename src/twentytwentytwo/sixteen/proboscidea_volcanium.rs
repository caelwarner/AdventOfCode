use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use bitvec::bitarr;
use bitvec::prelude::{BitArray, Lsb0};
use itertools::Itertools;
use regex::Regex;
use util::input_as_str_vec;

fn main() {
    println!("{}", release_max_pressure_with_elephant(input_as_str_vec!()));
}

fn release_max_pressure(input: Vec<&str>) -> u32 {
    let mut valves = to_valves(input);
    let working_valves = generate_reduced_graph(&mut valves);

    let mut cache: Vec<u16> = vec![0; (2 as usize).pow(16) * 16 * 32 * 2];
    working_valves[0].clone().borrow().release_pressure(
        &mut cache,
        &working_valves[0].clone(),
        bitarr![u16, Lsb0; 0; 16],
        31,
        false
    ) as u32
}

fn release_max_pressure_with_elephant(input: Vec<&str>) -> u32 {
    let mut valves = to_valves(input);
    let working_valves = generate_reduced_graph(&mut valves);

    let mut cache: Vec<u16> = vec![0; (2 as usize).pow(16) * 16 * 32 * 2];
    working_valves[0].clone().borrow().release_pressure(
        &mut cache,
        &working_valves[0].clone(),
        bitarr![u16, Lsb0; 0; 16],
        27,
        true
    ) as u32
}

fn to_valves(input: Vec<&str>) -> HashMap<&str, Rc<RefCell<Valve>>> {
    let mut valves: HashMap<&str, Rc<RefCell<Valve>>> = HashMap::new();
    let re = Regex::new(".+ ([A-Z]{2}).+=(\\d+);.+ valves? (.+)").unwrap();

    for line in input {
        let captures = re.captures(line).unwrap();
        let name = captures.get(1).map_or("", |s| s.as_str());

        valves.insert(
            name,
            Valve::new(
                name.to_string(),
                captures.get(2).map_or(0, |s| s.as_str().parse::<u32>().unwrap()),
                captures.get(3).map_or(vec![], |s| s.as_str()
                    .split(", ")
                    .map(|s| s.into())
                    .collect()),
            )
        );
    }

    valves
}

fn generate_reduced_graph(valves: &mut HashMap<&str, Rc<RefCell<Valve>>>) -> Vec<Rc<RefCell<Valve>>> {
    let mut working_valves = valves.iter()
        .filter(|(_, valve)| valve.borrow().flow_rate > 0)
        .sorted_by_key(|(name, _)| name.to_string())
        .map(|(_, valve)| valve.clone())
        .collect::<Vec<Rc<RefCell<Valve>>>>();

    working_valves.insert(0, valves.get("AA").unwrap().clone());

    for i in 0..working_valves.len() {
        working_valves[i].borrow_mut().id = i;
    }

    for working_valve in &working_valves {
        let distances = working_valve.borrow().calculate_distances(&valves, &working_valves);
        working_valve.borrow_mut().populate_edges(&distances, &working_valves);
    }

    working_valves
}

#[derive(Debug)]
struct Valve {
    id: usize,
    name: String,
    flow_rate: u32,
    tunnels: Vec<String>,
    edges: Vec<Edge>,
}

impl Valve {
    #[inline]
    fn new(name: String, flow_rate: u32, tunnels: Vec<String>) -> Rc<RefCell<Valve>> {
        Rc::new(RefCell::new(Valve {
            id: 0,
            name,
            flow_rate,
            tunnels,
            edges: vec![],
        }))
    }

    // Dijsktra's Algorithm
    fn calculate_distances(&self, valves: &HashMap<&str, Rc<RefCell<Valve>>>, working_valves: &Vec<Rc<RefCell<Valve>>>) -> Vec<u32> {
        let mut unvisited: Vec<&str> = vec![];
        let mut distances: HashMap<String, u32> = HashMap::new();

        for name in valves.keys() {
            unvisited.push(name);
            distances.insert(name.to_string(), u32::MAX);
        }

        distances.entry(self.name.clone()).and_modify(|d| *d = 0);

        while unvisited.len() > 0 {
            unvisited.sort_by_key(|name| distances.get(name.to_owned()).unwrap());
            let closest = unvisited.remove(0);

            let tunnels = if closest != self.name.as_str() {
                valves.get(closest).unwrap().borrow().tunnels.clone()
            } else {
                self.tunnels.clone()
            };

            for tunnel in tunnels {
                let new_path = distances.get(closest).unwrap() + 1;
                let path = distances.get_mut(tunnel.as_str()).unwrap();

                if new_path < path.clone() {
                    *path = new_path;
                }
            }
        }

        working_valves.iter()
            .map(|valve| {
                distances.get(valve.borrow().name.as_str())
                    .unwrap()
                    .clone()
            })
            .collect()
    }

    fn populate_edges(&mut self, distances: &Vec<u32>, working_valves: &Vec<Rc<RefCell<Valve>>>) {
        for i in 0..working_valves.len() {
            self.edges.push(
                Edge {
                    valve: working_valves[i].clone(),
                    distance: distances[i],
                }
            )
        }
    }

    fn release_pressure(
        &self,
        cache: &mut Vec<u16>,
        starting_valve: &Rc<RefCell<Valve>>,
        mut visited: BitArray<[u16; 1]>,
        minutes: u32,
        elephant: bool
    ) -> u16 {
        let mut max_pressure: u16 = 0;

        visited.set(self.id, true);

        let key = visited.data[0] as usize * 16 * 32 * 2 +
            self.id as usize * 32 * 2 +
            minutes as usize * 2 +
            elephant as usize;

        if cache[key] != 0 {
            return cache[key];
        }

        for edge in &self.edges {
            if edge.distance >= minutes - 1 || visited[edge.valve.borrow().id] {
                continue;
            }

            let pressure = edge.valve.borrow().release_pressure(
                cache,
                starting_valve,
                visited,
                minutes - edge.distance - 1,
                elephant
            );

            if pressure > max_pressure {
                max_pressure = pressure;
            }
        }

        if max_pressure == 0 && elephant {
            max_pressure = starting_valve.borrow().release_pressure(
                cache,
                starting_valve,
                visited,
                27,
                false
            );
        }

        max_pressure += self.flow_rate as u16 * (minutes as u16 - 1);
        cache[key] = max_pressure;

        max_pressure
    }
}

#[derive(Debug)]
struct Edge {
    valve: Rc<RefCell<Valve>>,
    distance: u32,
}
