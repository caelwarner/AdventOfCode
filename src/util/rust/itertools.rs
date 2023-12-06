pub trait AdventItertools: Iterator {
    fn first_last(self) -> Option<FirstLast<Self::Item>>
        where Self::Item: Clone;
}

impl <I> AdventItertools for I
    where I: Iterator {
    fn first_last(mut self) -> Option<FirstLast<I::Item>>
        where I::Item: Clone
    {
        if let Some(first) = self.next() {
            if let Some(last) = self.last() {
                Some(FirstLast {
                    first,
                    last,
                })
            } else {
                Some(FirstLast {
                    first: first.clone(),
                    last: first,
                })
            }
        } else {
            None
        }
    }
}

pub struct FirstLast<I: Clone> {
    pub first: I,
    pub last: I,
}
