pub trait AdventIter: Iterator {
    fn first_last(self) -> FirstLast<Self> where Self: Sized;
}

impl <I: Iterator> AdventIter for I {
    fn first_last(self) -> FirstLast<Self> where Self: Sized {
        FirstLast::new(self)
    }
}

pub struct FirstLast<I> where I: Iterator {
    first: FirstLastState,
    iter: I,
}

enum FirstLastState {
    First,
    Last,
    Finished,
}

impl <I: Iterator> FirstLast<I> {
    fn new(iter: I) -> Self {
        Self {
            first: FirstLastState::First,
            iter,
        }
    }
}

impl <I> Iterator for FirstLast<I> where I: Iterator {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        match &self.first {
            FirstLastState::First => {
                self.first = FirstLastState::Last;
                self.iter.next()
            },
            FirstLastState::Last => {
                self.first = FirstLastState::Finished;

                let mut last = self.iter.next();
                while let Some(next) = self.iter.next() {
                    last = Some(next);
                }
                last
            },
            FirstLastState::Finished => None,
        }
    }
}
