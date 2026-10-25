//! The ledger as one transaction sees it.
//!
//! The ledger holds tens of rows, so a transaction loads all of them. The
//! [`View`]'s mutators change its copy and record the change in the batch
//! the transaction commits.

use std::collections::BTreeMap;
use std::path::Path;

use serde::{Deserialize, Serialize};
use tokio::runtime::Runtime;
use turbokv::{Db, WriteBatch};

pub(super) const SCHEMA_KEY: &str = "schema";
/// Bumped only for changes an older rene could misread. Fields added to a
/// row are not such changes: unknown fields are ignored.
const SCHEMA: u64 = 1;
const SESSION: &str = "session/";

/// A session's row. Informational: liveness comes from the session lock.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SessionRow {
    pub pid: u32,
    /// Unix seconds.
    pub started: u64,
}

/// A copy of the ledger, for inspection and tests.
#[derive(Debug, Default, Clone)]
pub struct Snapshot {
    /// Session id to its row.
    pub sessions: BTreeMap<String, SessionRow>,
}

pub(super) struct View {
    pub(super) sessions: BTreeMap<String, SessionRow>,
    /// What the transaction changed.
    pub(super) batch: WriteBatch,
}

impl View {
    /// Load the ledger `db`, at `path`. A ledger a newer rene wrote is an
    /// error; a fresh one gets this rene's schema.
    pub(super) fn load(runtime: &Runtime, db: &Db, path: &Path) -> Result<View, String> {
        let read = |e: turbokv::DbError| format!("cannot read the cache ledger: {e}");
        let mut batch = WriteBatch::new();
        match runtime.block_on(db.get(SCHEMA_KEY)).map_err(read)? {
            None => batch.put(SCHEMA_KEY, SCHEMA.to_string()),
            Some(raw) => {
                let found = String::from_utf8_lossy(&raw).trim().parse::<u64>().ok();
                if found.is_none_or(|found| found > SCHEMA) {
                    return Err(format!(
                        "the cache ledger `{}` was written by a newer rene (schema `{}`, this \
                         rene knows {SCHEMA})",
                        path.display(),
                        String::from_utf8_lossy(&raw).trim()
                    ));
                }
            }
        }
        let mut sessions = BTreeMap::new();
        for (key, value) in runtime.block_on(db.scan_prefix(SESSION)).map_err(read)? {
            let id = String::from_utf8_lossy(&key[SESSION.len()..]).into_owned();
            match serde_json::from_slice(&value) {
                Ok(row) => {
                    sessions.insert(id, row);
                }
                // The row is informational; the session's lock decides
                // whether it lives, so an unreadable row keeps its place.
                Err(_) => {
                    sessions.insert(id, SessionRow { pid: 0, started: 0 });
                }
            }
        }
        Ok(View { sessions, batch })
    }

    pub(super) fn set_session(&mut self, id: &str, row: SessionRow) {
        self.batch.put(
            format!("{SESSION}{id}"),
            serde_json::to_vec(&row).expect("a session row serializes"),
        );
        self.sessions.insert(id.to_owned(), row);
    }

    pub(super) fn snapshot(&self) -> Snapshot {
        Snapshot {
            sessions: self.sessions.clone(),
        }
    }
}
