use util::input_as_str_vec;
use util::vector::Vec2;

fn main() {
    println!("{}", beacon_tuning_frequency(input_as_str_vec!()));
}

fn non_beacon_positions_in_row(input: Vec<&str>) -> u32 {
    let mut non_beacon_positions: u32 = 0;

    let (sensors, beacons, mut width) = parse_input(input);

    width += width / 2;

    // Brute force only takes 5-10 seconds
    for x in -width..width {
        let pos = Vec2::new(x, 2_000_000);

        for sensor in &sensors {
            if sensor.pos.distance(&pos) <= sensor.closest_beacon_distance && !beacons.contains(&pos) {
                non_beacon_positions += 1;
                break;
            }
        }
    }

    non_beacon_positions
}

fn beacon_tuning_frequency(input: Vec<&str>) -> u128 {
    let mut beacon_position: Vec2 = Vec2::default();

    let (sensors, _, _) = parse_input(input);

    'x_loop: for x in 0..=4_000_000 {
        let mut y: i32 = 0;

        'y_loop: while y <= 4_000_000 {
            let pos = Vec2::new(x, y);

            for sensor in &sensors {
                if sensor.pos.distance(&pos) <= sensor.closest_beacon_distance {
                    y = sensor.closest_beacon_distance - (sensor.pos.x - pos.x).abs() + sensor.pos.y + 1;

                    continue 'y_loop;
                }
            }

            beacon_position = pos;
            break 'x_loop;
        }
    }

    beacon_position.x as u128 * 4_000_000 + beacon_position.y as u128
}

fn parse_input(input: Vec<&str>) -> (Vec<Sensor>, Vec<Vec2>, i32) {
    let mut sensors: Vec<Sensor> = vec![];
    let mut beacons: Vec<Vec2> = vec![];
    let mut width: i32 = 0;

    for line in input {
        let positions = line
            .split(&['=', ',', ':'])
            .filter_map(|s| s.parse::<i32>().ok())
            .collect::<Vec<i32>>();

        if positions[0] > width {
            width = positions[0];
        }

        beacons.push(Vec2::new(positions[2], positions[3]));

        sensors.push(Sensor::new(
            Vec2::new(positions[0], positions[1]),
            &beacons.last().unwrap(),
        ));
    }

    (sensors, beacons, width)
}

struct Sensor {
    pos: Vec2,
    closest_beacon_distance: i32,
}

impl Sensor {
    fn new(pos: Vec2, closest_beacon: &Vec2) -> Self {
        Sensor {
            pos,
            closest_beacon_distance: pos.distance(closest_beacon),
        }
    }
}
