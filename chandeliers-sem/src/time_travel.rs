use std::fmt;

use crate::nillable::*;

#[derive(Debug, Clone, Copy, Default)]
pub struct O<T> {
    pub current: Nillable<T>,
}

#[derive(Debug, Clone, Default)]
pub struct S<N, T> {
    pub current: Nillable<T>,
    pub previous: N,
}

impl<T: fmt::Display> fmt::Display for O<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.current)
    }
}
impl<N: fmt::Display, T: fmt::Display> fmt::Display for S<N, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}->{}", self.previous, self.current)
    }
}

macro_rules! o {
    ($e:expr) => {
        O { current: $e }
    };
}

macro_rules! s {
    ($e:expr, $n:expr) => {
        S {
            current: $e,
            previous: $n,
        }
    };
}

pub trait Ago<T> {
    fn ago(&self, dt: usize) -> Nillable<T>;
}

impl<T: Clone> Ago<T> for O<T> {
    #[track_caller]
    #[inline(always)]
    fn ago(&self, dt: usize) -> Nillable<T> {
        match dt {
            0 => panic!(
                "Cannot look into the past at distance 0, use the current environment instead"
            ),
            1 => self.current.clone(),
            _ => panic!("Tried to look too much into the past"),
        }
    }
}

impl<N, T: Clone> Ago<T> for S<N, T>
where
    N: Ago<T>,
{
    #[track_caller]
    #[inline(always)]
    fn ago(&self, dt: usize) -> Nillable<T> {
        match dt {
            0 => panic!(
                "Cannot look into the past at distance 0, use the current environment instead"
            ),
            1 => self.current.clone(),
            dt => self.previous.ago(dt - 1),
        }
    }
}

// History: forget one step
trait Downcast<T> {
    fn downcast(self) -> T;
}

impl<T> Downcast<O<T>> for S<O<T>, T> {
    #[inline(always)]
    fn downcast(self) -> O<T> {
        o!(self.current)
    }
}

impl<N, T> Downcast<S<N, T>> for S<S<N, T>, T>
where
    S<N, T>: Downcast<N>,
{
    #[inline(always)]
    fn downcast(self) -> S<N, T> {
        s!(self.current, self.previous.downcast())
    }
}

// Prepend to history
pub trait Extend<N, T>: Sized {
    // Required methods
    fn extend(self, new: Nillable<T>) -> N;
    // Provided methods
    fn extend_undefined(self) -> N {
        self.extend(Nillable::Nil)
    }
}

impl<T> Extend<S<O<T>, T>, T> for O<T> {
    #[inline(always)]
    fn extend(self, new: Nillable<T>) -> S<O<T>, T> {
        s!(new, self)
    }
}

impl<N, T> Extend<S<S<N, T>, T>, T> for S<N, T>
where
    N: Extend<S<N, T>, T>,
{
    #[inline(always)]
    fn extend(self, new: Nillable<T>) -> S<S<N, T>, T> {
        s!(new, self.previous.extend(self.current))
    }
}

// Shift history by 1
pub trait Update<T>: Sized {
    // Required method
    fn update(self, new: Nillable<T>) -> Self;
    fn redefine(self, new: Nillable<T>) -> Self;
    // Provided methods
    fn update_mut(&mut self, new: Nillable<T>) {
        replace_with::replace_with_or_abort(self, |s| s.update(new));
    }
    fn update_undefined(self) -> Self {
        self.update(Nillable::Nil)
    }
    fn update_mut_undefined(&mut self) {
        self.update_mut(Nillable::Nil);
    }

    fn redefine_mut(&mut self, new: Nillable<T>) {
        replace_with::replace_with_or_abort(self, |s| s.redefine(new));
    }
}

impl<T> Update<T> for O<T> {
    #[inline(always)]
    fn update(self, new: Nillable<T>) -> Self {
        Self { current: new }
    }
    #[inline(always)]
    fn redefine(mut self, new: Nillable<T>) -> Self {
        self.current = new;
        self
    }
}

impl<N, T> Update<T> for S<N, T>
where
    S<N, T>: Downcast<N>,
{
    #[inline(always)]
    fn update(self, new: Nillable<T>) -> Self {
        s!(new, self.downcast())
    }
    #[inline(always)]
    fn redefine(mut self, new: Nillable<T>) -> Self {
        self.current = new;
        self
    }
}

// History of arbitrary values
pub trait IntoHistory: Sized {
    #[inline(always)]
    fn into_history(self) -> O<Self> {
        o!(Nillable::Defined(self))
    }
    #[inline(always)]
    fn empty_history() -> O<Self> {
        o!(Nillable::Nil)
    }
}
impl<T: Sized> IntoHistory for T {}
