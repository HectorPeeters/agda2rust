use std::cell::Cell;
use std::fmt;
use std::lazy::OnceCell;
use std::ops::Deref;

// Variation of the std::lazy::Lazy type which supports fake lazy values for when the closure computing the lazy value would capture a local variable.

pub enum Lazy<T, F = fn() -> T> {
    Proper {
        cell: OnceCell<T>,
        init: Cell<Option<F>>,
    },
    Facade {
        value: T,
    },
}

impl<T: fmt::Debug, F> fmt::Debug for Lazy<T, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Proper { cell, init } => f
                .debug_struct("Lazy")
                .field("cell", &cell)
                .field("init", &"..")
                .finish(),
            Self::Facade { value } => f.debug_struct("Lazy").field("value", &value).finish(),
        }
    }
}

impl<T, F> Lazy<T, F> {
    pub const fn new(init: F) -> Lazy<T, F> {
        Lazy::Proper {
            cell: OnceCell::new(),
            init: Cell::new(Some(init)),
        }
    }

    pub const fn facade(value: T) -> Lazy<T, F> {
        Lazy::Facade { value }
    }
}

impl<T, F: FnOnce() -> T> Lazy<T, F> {
    pub fn force(this: &Lazy<T, F>) -> &T {
        match this {
            Self::Proper { cell, init } => cell.get_or_init(|| match init.take() {
                Some(f) => f(),
                None => panic!("`Lazy` instance has previously been poisoned"),
            }),
            Self::Facade { value } => &value,
        }
    }
}

impl<T, F: FnOnce() -> T> Deref for Lazy<T, F> {
    type Target = T;
    fn deref(&self) -> &T {
        Lazy::force(self)
    }
}

impl<T: Default> Default for Lazy<T> {
    fn default() -> Lazy<T> {
        Lazy::new(T::default)
    }
}
