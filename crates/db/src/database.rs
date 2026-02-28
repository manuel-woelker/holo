//! Database abstraction.

use std::path::PathBuf;
use holo_base::{SharedString};
use holo_base::Result;


pub trait DatabaseTransaction {
    fn get(self, table: &str, keys: &dyn Iterator<Item=&str>) -> Result<Vec<Box<[u8]>>>;
    fn put(self, table: &str, entries: &dyn Iterator<Item=(&str, &[u8])>) -> Result<()>;
}

pub trait Database {
//    type Table: Table;
    type Transaction: DatabaseTransaction;

    fn open(path: PathBuf, table_names: Vec<SharedString>) -> Self;

    fn begin_tx(self) -> Box<dyn DatabaseTransaction>;
}
