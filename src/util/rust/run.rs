use std::fmt::{Display, Formatter};
use std::time::Instant;

#[inline]
pub fn run<D, F>(part: Part, f: F)
    where
        D: Display,
        F: FnOnce() -> D,
{
    let start = Instant::now();
    let out = f();
    let end = Instant::now() - start;
    println!(
        "\x1b[0;1m{}: \x1b[0;32m{}\x1b[0;30m | \x1b[0;1mTime elapsed: \x1b[0;36m{:}ms\x1b[0m",
        part,
        out,
        end.as_secs_f64() * 1000.0,
    );
}

pub enum Part {
    One,
    Two,
}

impl Display for Part {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Part {}",
            match self {
                Part::One => 1,
                Part::Two => 2,
            },
        )
    }
}
