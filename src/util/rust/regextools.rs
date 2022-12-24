use std::fmt::Debug;
use std::str::FromStr;
use regex::Captures;

pub trait CapturesTools {
    fn as_str(&self, i: usize) -> &str;
    fn parse<F: FromStr>(&self, i: usize) -> F where <F as FromStr>::Err: Debug;
}

impl CapturesTools for Captures<'_> {
    fn as_str(&self, i: usize) -> &str {
        self.get(i).unwrap().as_str()
    }

    fn parse<F: FromStr>(&self, i: usize) -> F where <F as FromStr>::Err: Debug {
        self.get(i).unwrap().as_str().parse::<F>().unwrap()
    }
}
