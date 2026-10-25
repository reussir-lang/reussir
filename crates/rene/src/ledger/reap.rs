//! Reaping: repairing after dead sessions, the first step of every
//! transaction.
//!
//! A session is dead when its lock is free, or its lock file is gone: the
//! [`FileLock`] contract means a file disappears only once its session let
//! go of it. A lock that cannot be probed counts as held, since a reaper
//! must never act on a session it cannot prove dead.
//!
//! A dead session's pins go, and each row it owned is repaired from two
//! facts, whether the entry's lock is free and whether its directory is
//! there:
//!
//! | row | condition | repair |
//! |---|---|---|
//! | `building` | entry lock free | discard the directory, delete the row |
//! | `building` | entry lock held | none: the holder takes the row over |
//! | `pruning` | directory present | back to `live`, its last use kept |
//! | `pruning` | directory gone | delete the row |
//!
//! The session's row goes only once no entry row names it, so that a later
//! reaper can still tell the owner of a row it left alone is dead.

use super::view::{State, View};
use super::{SESSIONS_DIR, session_lock};
use crate::cache::{Cache, EntryId, FileLock, Probe};

impl View {
    /// Repair after every dead session but `me`, then delete the lock files
    /// that no session row names and nobody holds.
    pub(super) fn reap(&mut self, cache: &Cache, me: &str) {
        let others: Vec<String> = self
            .sessions
            .keys()
            .filter(|id| *id != me)
            .cloned()
            .collect();
        for id in others {
            let path = session_lock(cache, &id);
            match FileLock::probe(&path) {
                Ok(Probe::Held) => {}
                Ok(Probe::Absent) => self.bury(cache, &id),
                Ok(Probe::Free(lock)) => {
                    self.bury(cache, &id);
                    remove(lock);
                }
                Err(e) => tracing::debug!("cannot probe `{}`: {e}", path.display()),
            }
        }
        self.sweep_session_locks(cache, me);
    }

    /// Repair after the dead session `id` (see the module docs).
    fn bury(&mut self, cache: &Cache, id: &str) {
        self.remove_pins_of(id);
        let owned: Vec<EntryId> = self
            .entries
            .iter()
            .filter(|(_, entry)| entry.owner() == Some(id))
            .map(|(entry, _)| entry.clone())
            .collect();
        for entry in owned {
            match self.entries[&entry].state {
                State::Building { .. } => self.abandon(cache, &entry),
                State::Pruning { .. } if cache.entry(&entry).exists() => {
                    let mut revived = self.entries[&entry].clone();
                    revived.state = State::Live;
                    self.set_entry(&entry, revived);
                }
                State::Pruning { .. } => self.remove_entry(&entry),
                State::Live => {}
            }
        }
        if !self.names(id) {
            self.remove_session(id);
        }
    }

    /// Discard the half-built entry `id`, whose builder died, unless a live
    /// session has taken its lock over since. The entry's lock is held
    /// while its directory is renamed out, and its file deleted after.
    fn abandon(&mut self, cache: &Cache, id: &EntryId) {
        let lock_path = cache.lock_path(id);
        let lock = match FileLock::probe(&lock_path) {
            Ok(Probe::Free(lock)) => Some(lock),
            Ok(Probe::Absent) => None,
            Ok(Probe::Held) => return,
            Err(e) => {
                tracing::debug!("cannot probe `{}`: {e}", lock_path.display());
                return;
            }
        };
        let dir = cache.entry(id);
        if let Err(e) = self.discard(&dir) {
            // Kept for the next reaper to try again.
            tracing::debug!("cannot move `{}` out of the cache: {e}", dir.display());
            return;
        }
        self.remove_entry(id);
        if let Some(lock) = lock {
            remove(lock);
        }
    }

    /// Delete the free lock files without a session row: a rene that
    /// crashed between locking its file and writing its row, or one whose
    /// file outlived its row. A file being created, not yet locked, may go
    /// too: its session then locks a fresh one.
    fn sweep_session_locks(&self, cache: &Cache, me: &str) {
        let Ok(files) = std::fs::read_dir(cache.root().join(SESSIONS_DIR)) else {
            return;
        };
        for file in files.flatten() {
            let name = file.file_name();
            let Some(id) = name.to_str().and_then(|name| name.strip_suffix(".lock")) else {
                continue;
            };
            if id == me || self.sessions.contains_key(id) {
                continue;
            }
            if let Ok(Probe::Free(lock)) = FileLock::probe(&file.path()) {
                remove(lock);
            }
        }
    }
}

/// Delete a lock file the reaper holds.
fn remove(lock: FileLock) {
    let path = lock.path().to_owned();
    if let Err(e) = lock.remove() {
        tracing::debug!("cannot delete `{}`: {e}", path.display());
    }
}
