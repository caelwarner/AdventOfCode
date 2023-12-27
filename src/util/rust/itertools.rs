use std::fmt::Debug;

pub struct InspectDebug<I: Iterator> {
    iter: I,
}

impl<I: Iterator> InspectDebug<I> {
    #[inline]
    const fn new(iter: I) -> Self {
        Self {
            iter,
        }
    }
}

impl<I> Iterator for InspectDebug<I>
where
    I: Iterator,
    I::Item: Debug,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let elem = self.iter.next()?;
        dbg!(&elem);
        Some(elem)
    }
}

impl<I> DoubleEndedIterator for InspectDebug<I>
where
    I: DoubleEndedIterator,
    I::Item: Debug,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        let elem = self.iter.next_back()?;
        dbg!(&elem);
        Some(elem)
    }
}

#[derive(Debug, Clone)]
pub struct FirstLast<I: Clone> {
    pub first: I,
    pub last: I,
}

pub trait AdventItertools: Iterator {
    fn inspect_dbg(self) -> InspectDebug<Self>
        where Self: Sized,
              Self::Item: Debug;

    fn first_last(&mut self) -> Option<FirstLast<Self::Item>>
        where Self::Item: Clone;
}

impl<I> AdventItertools for I
    where I: Iterator
{
    fn inspect_dbg(self) -> InspectDebug<Self>
    where
        Self: Sized,
        Self::Item: Debug,
    {
        InspectDebug::new(self)
    }

    fn first_last(&mut self) -> Option<FirstLast<Self::Item>>
        where Self::Item: Clone
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
