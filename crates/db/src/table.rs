//! Table abstraction for database storage.

use holo_base::SharedString;

pub trait TableKey: Clone + Eq + Ord {}

pub trait TableValue: Clone {}

pub trait Table {
    type Key: TableKey;
    type Value: TableValue;

    fn name(&self) -> SharedString;

    fn get(&self, key: &Self::Key) -> Option<Self::Value>;

    fn set(&mut self, key: Self::Key, value: Self::Value);

    fn remove(&mut self, key: &Self::Key) -> Option<Self::Value>;

    fn is_empty(&self) -> bool;

    fn len(&self) -> usize;
}
