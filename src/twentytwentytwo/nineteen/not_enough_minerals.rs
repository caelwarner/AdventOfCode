use std::collections::HashMap;
use itertools::min;
use regex::Regex;
use util::input_as_str_vec;
use util::math::Math;
use util::regextools::CapturesTools;

use crate::Material::{Clay, Geodes, Obsidian, Ore};

fn main() {
    println!("{}", top_three_blueprints(input_as_str_vec!()));
}

fn blueprint_quality_level(input: Vec<&str>) -> u32 {
    let blueprints = to_blueprints(input);
    let mut quality_level: u32 = 0;

    for blueprint in blueprints {
        quality_level += max_geodes(
            &mut 0,
            &mut HashMap::new(),
            24,
            &blueprint,
            Inventory::new(),
            &[16, 7, 4, 0]
        ) as u32 * blueprint.id;
    }

    quality_level
}

fn top_three_blueprints(input: Vec<&str>) -> u32 {
    let blueprints = to_blueprints(input);
    let mut geodes: u32 = 1;

    for i in 0..3 {
        geodes *= max_geodes(
            &mut 0,
            &mut HashMap::new(),
            32,
            &blueprints[i],
            Inventory::new(),
            &[22, 12, 5, 0]
        ) as u32;
    }

    geodes
}

fn to_blueprints(input: Vec<&str>) -> Vec<Blueprint> {
    let mut blueprints: Vec<Blueprint> = vec![];
    let re = Regex::new(".+ (\\d+).+ (\\d+).+ (\\d+).+ (\\d+).+ (\\d+).+ (\\d+).+ (\\d+)").unwrap();

    for line in input {
        let captures = re.captures(line).unwrap();

        let robots = [
            Robot::new(Item::new(Ore), (
                Item::amount(Ore, captures.parse::<u16>(2)),
                Item::zero()
            )),
            Robot::new(Item::new(Clay), (
                Item::amount(Ore, captures.parse::<u16>(3)),
                Item::zero()
            )),
            Robot::new(Item::new(Obsidian), (
                Item::amount(Ore, captures.parse::<u16>(4)),
                Item::amount(Clay, captures.parse::<u16>(5)),
            )),
            Robot::new(Item::new(Geodes), (
                Item::amount(Ore, captures.parse::<u16>(6)),
                Item::amount(Obsidian, captures.parse::<u16>(7)),
            )),
        ];

        blueprints.push(Blueprint::new(captures.parse::<u32>(1), robots));
    }

    blueprints
}

fn max_geodes(
    best: &mut u16,
    cache: &mut HashMap<usize, u16>,
    minutes: u16,
    blueprint: &Blueprint,
    mut inventory: Inventory,
    time_threshold: &[u16; 4],
) -> u16 {
    let mut geodes: u16 = 0;

    // Check memoization to see if we've been here before
    let key = inventory.serialize(minutes);
    if cache.contains_key(&key) {
        return cache[&key];
    }

    // Check to see if branch is at end
    if minutes <= 1 {
        *best = (inventory.materials[Geodes as usize] + inventory.robots[Geodes as usize]).max(*best);
        return inventory.materials[Geodes as usize] + inventory.robots[Geodes as usize];
    }

    // Check to see if even with branches best outcome if we could be the best
    let a = (inventory.robots[Geodes as usize] + minutes).triangle_num();
    let b = inventory.robots[Geodes as usize].triangle_num();
    if a - b + inventory.materials[Geodes as usize] < *best {
        return 0 ;
    }

    // For each material
    // If there are enough resources
    // Build robot
    for robot in &blueprint.robots {
        if !inventory.has(&robot.costs.0) || !inventory.has(&robot.costs.1) {
            continue;
        }

        let i = robot.produces.material as usize;
        if inventory.robots[i] < robot.max && minutes >= time_threshold[i] {
            let mut new_inventory = inventory.clone();

            new_inventory.materials[robot.costs.0.material as usize] -= robot.costs.0.amount;
            new_inventory.materials[robot.costs.1.material as usize] -= robot.costs.1.amount;

            new_inventory.update();
            new_inventory.robots[i] += 1;

            geodes = geodes.max(max_geodes(best, cache, minutes - 1, blueprint, new_inventory, time_threshold));
        }
    }

    // Don't build anything
    if geodes == 0 {
        inventory.update();
        geodes = geodes.max(max_geodes(best, cache, minutes - 1, blueprint, inventory, time_threshold));
    }

    cache.insert(key, geodes);

    geodes
}

struct Blueprint {
    id: u32,
    robots: [Robot; 4],
}

impl Blueprint {
    fn new(id: u32, mut robots: [Robot; 4]) -> Self {
        let max_ore_usage = robots.iter()
            .map(|robot| robot.costs.0.amount)
            .max()
            .unwrap();

        robots[Ore as usize].max = max_ore_usage;
        robots[Clay as usize].max = robots[Obsidian as usize].costs.1.amount;
        robots[Obsidian as usize].max = robots[Geodes as usize].costs.1.amount;

        Blueprint { id, robots }
    }
}

struct Robot {
    produces: Item,
    costs: (Item, Item),
    max: u16,
}

impl Robot {
    const fn new(produces: Item, costs: (Item, Item)) -> Self {
        Robot { produces, costs, max: u16::MAX }
    }
}

struct Item {
    material: Material,
    amount: u16,
}

impl Item {
    const fn new(material: Material) -> Self {
        Item { material, amount: 1 }
    }

    const fn amount(material: Material, amount: u16) -> Self {
        Item { material, amount }
    }

    const fn zero() -> Self {
        Item { material: Ore, amount: 0 }
    }
}

#[derive(Copy, Clone)]
enum Material {
    Ore = 0,
    Clay = 1,
    Obsidian = 2,
    Geodes = 3,
}

#[derive(Clone)]
struct Inventory {
    materials: [u16; 4],
    robots: [u16; 4],
}

impl Inventory {
    const fn new() -> Self {
        Inventory {
            materials: [0; 4],
            robots: [1, 0, 0, 0],
        }
    }

    fn update(&mut self) {
        for i in 0..4 {
            self.materials[i] += self.robots[i];
        }
    }

    fn has(&self, item: &Item) -> bool {
        self.materials[item.material as usize] >= item.amount
    }

    fn serialize(&self, minutes: u16) -> usize {
        self.materials[3].min(24) as usize * 24 * 24 * 24 * 12 * 12 * 12 * 12 * 32 +
        self.materials[2].min(24) as usize * 24 * 24 * 12 * 12 * 12 * 12 * 32 +
        self.materials[1].min(24) as usize * 24 * 12 * 12 * 12 * 12 * 32 +
        self.materials[0].min(24) as usize * 12 * 12 * 12 * 12 * 32 +
        self.robots[3].min(12) as usize * 12 * 12 * 12 * 32 +
        self.robots[2].min(12) as usize * 12 * 12 * 32 +
        self.robots[1].min(12) as usize * 12 * 32 +
        self.robots[0].min(12) as usize * 32 +
        minutes as usize
    }
}
