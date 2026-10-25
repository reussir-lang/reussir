//! The ledger as one transaction sees it.
//!
//! The ledger holds tens of rows, so a transaction loads all of them. The
//! [`View`]'s mutators change its copy and record the change in the batch
//! the transaction commits. The total size of the cache is summed from the
//! live rows whenever it is asked for, so it cannot drift from them.

use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use serde::{Deserialize, Serialize};
use tokio::runtime::Runtime;
use turbokv::{Db, WriteBatch};

use crate::cache::EntryId;

pub(super) const SCHEMA_KEY: &str = "schema";
/// Bumped only for changes an older rene could misread: a new entry kind,
/// or a row whose meaning changes. Fields added to a row are not such
/// changes: unknown fields are ignored.
const SCHEMA: u64 = 1;
const SESSION: &str = "session/";
const PIN: &str = "pin/";
const ENTRY: &str = "entry/";

/// A session's row. Informational: liveness comes from the session lock.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SessionRow {
    pub pid: u32,
    /// Unix seconds.
    pub started: u64,
}

/// An entry row: `{"state": "live", "size": …, "last_used": …}`, with an
/// `owner` in the transient states.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Entry {
    #[serde(flatten)]
    pub state: State,
    /// Bytes on disk, measured when the entry was completed; 0 while it is
    /// being built.
    pub size: u64,
    /// Unix seconds of the last lookup.
    pub last_used: u64,
}

/// An entry row's state.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub enum State {
    /// Being built by the session `owner`, which holds the entry's lock.
    Building { owner: String },
    /// Complete on disk.
    Live,
    /// Chosen for removal by the session `owner`; its directory may be gone
    /// already.
    Pruning { owner: String },
}

impl Entry {
    /// The session owning a transient state.
    pub fn owner(&self) -> Option<&str> {
        match &self.state {
            State::Building { owner } | State::Pruning { owner } => Some(owner),
            State::Live => None,
        }
    }
}

/// A copy of the ledger, for inspection and tests.
#[derive(Debug, Default, Clone)]
pub struct Snapshot {
    pub entries: BTreeMap<EntryId, Entry>,
    /// `(session, entry)` pairs: the session wants the entry.
    pub pins: BTreeSet<(String, EntryId)>,
    /// Session id to its row.
    pub sessions: BTreeMap<String, SessionRow>,
    /// The summed size of the live entries.
    pub total: u64,
}

pub(super) struct View {
    /// Unix seconds when the transaction started.
    pub(super) now: u64,
    pub(super) entries: BTreeMap<EntryId, Entry>,
    pub(super) pins: BTreeSet<(String, EntryId)>,
    pub(super) sessions: BTreeMap<String, SessionRow>,
    /// What the transaction changed.
    pub(super) batch: WriteBatch,
}

impl View {
    /// Load the ledger `db`, at `path`. A ledger a newer rene wrote is an
    /// error; a fresh one gets this rene's schema.
    pub(super) fn load(runtime: &Runtime, db: &Db, path: &Path) -> Result<View, String> {
        let read = |e: turbokv::DbError| format!("cannot read the cache ledger: {e}");
        let scan = |prefix: &str| runtime.block_on(db.scan_prefix(prefix)).map_err(read);
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
        let mut entries = BTreeMap::new();
        for (key, value) in scan(ENTRY)? {
            let name = String::from_utf8_lossy(&key[ENTRY.len()..]).into_owned();
            let row = EntryId::parse(&name).zip(serde_json::from_slice::<Entry>(&value).ok());
            match row {
                Some((id, entry)) => {
                    entries.insert(id, entry);
                }
                None => {
                    // A row this rene cannot read is dropped, not guessed
                    // at. Its directory, if any, has no row then: the next
                    // session that wants it adopts it, or a pruner removes
                    // it once it is old.
                    tracing::debug!(row = %name, "dropping an unreadable cache ledger row");
                    batch.delete(key);
                }
            }
        }
        let mut pins = BTreeSet::new();
        for (key, _) in scan(PIN)? {
            let name = String::from_utf8_lossy(&key[PIN.len()..]).into_owned();
            let pin = name
                .split_once('/')
                .and_then(|(session, entry)| Some((session.to_owned(), EntryId::parse(entry)?)));
            match pin {
                Some(pin) => {
                    pins.insert(pin);
                }
                None => batch.delete(key),
            }
        }
        let mut sessions = BTreeMap::new();
        for (key, value) in scan(SESSION)? {
            let id = String::from_utf8_lossy(&key[SESSION.len()..]).into_owned();
            // The row is informational; the session's lock decides whether
            // it lives, so an unreadable row keeps its place.
            let row = serde_json::from_slice(&value).unwrap_or(SessionRow { pid: 0, started: 0 });
            sessions.insert(id, row);
        }
        Ok(View {
            now: super::now_secs(),
            entries,
            pins,
            sessions,
            batch,
        })
    }

    /// The summed size of the live entries.
    pub(super) fn total(&self) -> u64 {
        self.entries
            .values()
            .filter(|entry| entry.state == State::Live)
            .map(|entry| entry.size)
            .sum()
    }

    pub(super) fn set_entry(&mut self, id: &EntryId, entry: Entry) {
        self.batch.put(
            format!("{ENTRY}{id}"),
            serde_json::to_vec(&entry).expect("an entry row serializes"),
        );
        self.entries.insert(id.clone(), entry);
    }

    pub(super) fn add_pin(&mut self, session: &str, id: &EntryId) {
        if self.pins.insert((session.to_owned(), id.clone())) {
            self.batch.put(format!("{PIN}{session}/{id}"), "");
        }
    }

    pub(super) fn remove_pin(&mut self, session: &str, id: &EntryId) {
        if self.pins.remove(&(session.to_owned(), id.clone())) {
            self.batch.delete(format!("{PIN}{session}/{id}"));
        }
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
            entries: self.entries.clone(),
            pins: self.pins.clone(),
            sessions: self.sessions.clone(),
            total: self.total(),
        }
    }
}
