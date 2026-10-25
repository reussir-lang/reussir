//! Pruning: removing what no build will miss (docs/design/cache-ledger.md).
//!
//! A pruner removes the live entries last used more than `max_age` ago that
//! nobody pins, and the untracked directories (completed without the
//! ledger) whose modification time is as old and that nobody pins or
//! locks. Only one pruner runs at a time: `<root>/prune.lock` is tried,
//! never waited on.
//!
//! 1. **Mark**, in one transaction: flip the victims' rows to `pruning`,
//!    owned by the pruner, and list the untracked directories.
//! 2. **Remove**, one victim per transaction: if its row is still this
//!    pruner's, rename the directory out and delete the row in the same
//!    batch. A session that asked for the entry meanwhile reclaimed it, so
//!    its row reads live and it is skipped. The renames happen inside the
//!    transactions because reclaiming decides from the row and the
//!    directory too; a rename outside could take a directory from under a
//!    session that just reclaimed it.

use std::time::{Duration, UNIX_EPOCH};

use super::Session;
use super::reap::remove;
use super::view::{Entry, State, View};
use crate::cache::{self, Cache, EntryId, FileLock, Probe};
use crate::config::human_size;

/// The file only one pruner at a time holds, under the cache root.
const PRUNE_LOCK: &str = "prune.lock";

/// What a prune did, or (dry run) would do.
#[derive(Debug, Default)]
pub struct Pruned {
    /// Every entry removed, with its size.
    pub removed: Vec<(EntryId, u64)>,
    /// Old entries kept because a running build pins them.
    pub in_use: usize,
    /// Entries kept because they were used recently.
    pub recent: usize,
    /// The total size of the live entries afterwards.
    pub total: u64,
    /// Another rene was pruning, so this one did nothing.
    pub skipped: bool,
}

impl Pruned {
    /// "3 entries (1.2 GiB)".
    pub fn summary(&self) -> String {
        let bytes: u64 = self.removed.iter().map(|(_, size)| size).sum();
        let n = self.removed.len();
        let entries = if n == 1 { "entry" } else { "entries" };
        format!("{n} {entries} ({})", human_size(bytes))
    }
}

/// What the mark found.
struct Marked {
    /// The rows now this pruner's to remove (or that would be, in a dry
    /// run), with their sizes.
    victims: Vec<(EntryId, u64)>,
    /// The old untracked directories nobody pins.
    untracked: Vec<EntryId>,
    pruned: Pruned,
}

impl Session {
    /// Remove every entry no running build uses that went unused for longer
    /// than `max_age` (see the module docs); `dry_run` only reports what
    /// would go. An error when the ledger cannot be used.
    pub async fn prune(&self, max_age: Duration, dry_run: bool) -> Result<Pruned, String> {
        let unusable = || {
            format!(
                "the cache ledger in `{}` is unusable",
                self.cache.root().display()
            )
        };
        let path = self.cache.root().join(PRUNE_LOCK);
        let Some(_one_pruner) = FileLock::try_lock(&path)
            .map_err(|e| format!("cannot lock `{}`: {e}", path.display()))?
        else {
            return Ok(Pruned {
                skipped: true,
                ..Pruned::default()
            });
        };
        let marked = self
            .transact(|view| mark(view, &self.cache, &self.id, max_age, dry_run))
            .await
            .ok_or_else(unusable)?;
        let mut pruned = marked.pruned;
        let untracked = marked
            .untracked
            .into_iter()
            .map(|id| {
                let size = cache::dir_size(&self.cache.entry(&id));
                (id, size)
            })
            .collect::<Vec<_>>();
        if dry_run {
            pruned.removed = marked.victims.into_iter().chain(untracked).collect();
            return Ok(pruned);
        }
        for (id, size) in marked.victims {
            let remove = |view: &mut View| {
                let removed = remove_marked(view, &self.cache, &self.id, &id);
                (removed, view.total())
            };
            let (removed, total) = self.transact(remove).await.ok_or_else(unusable)?;
            pruned.total = total;
            if removed {
                pruned.removed.push((id, size));
            }
        }
        for (id, size) in untracked {
            let removed = self
                .transact(|view| remove_untracked(view, &self.cache, &id))
                .await
                .ok_or_else(unusable)?;
            if removed {
                pruned.removed.push((id, size));
            }
        }
        Ok(pruned)
    }
}

/// Choose what goes: the live rows last used before the cutoff and the
/// untracked directories last modified before it, unless pinned. Unless
/// `dry_run`, the rows become `pruning`, owned by the pruner `me`.
fn mark(view: &mut View, cache: &Cache, me: &str, max_age: Duration, dry_run: bool) -> Marked {
    let cutoff = view.now.saturating_sub(max_age.as_secs());
    let pinned = |id: &EntryId| view.pins.iter().any(|(_, pin)| pin == id);
    let mut pruned = Pruned::default();
    let mut victims = Vec::new();
    for (id, entry) in &view.entries {
        if entry.state != State::Live {
            continue;
        }
        if entry.last_used > cutoff {
            pruned.recent += 1;
        } else if pinned(id) {
            pruned.in_use += 1;
        } else {
            victims.push((id.clone(), entry.size));
        }
    }
    let mut untracked = Vec::new();
    for (id, modified) in entry_dirs(cache) {
        if view.entries.contains_key(&id) {
            continue;
        }
        if modified > cutoff {
            pruned.recent += 1;
        } else if pinned(&id) || locked(cache, &id) {
            pruned.in_use += 1;
        } else {
            untracked.push(id);
        }
    }
    if !dry_run {
        for (id, _) in &victims {
            let entry = Entry {
                state: State::Pruning {
                    owner: me.to_owned(),
                },
                ..view.entries[id].clone()
            };
            view.set_entry(id, entry);
        }
    }
    pruned.total = view.total();
    Marked {
        victims,
        untracked,
        pruned,
    }
}

/// Remove the victim `id` if its row is still this pruner's: rename its
/// directory out and delete the row and the entry's lock file. Whether it
/// went.
fn remove_marked(view: &mut View, cache: &Cache, me: &str, id: &EntryId) -> bool {
    let Some(entry) = view.entries.get(id).cloned() else {
        return false;
    };
    if !matches!(&entry.state, State::Pruning { owner } if owner == me) {
        // Reclaimed by a session that asked for it meanwhile.
        return false;
    }
    let dir = cache.entry(id);
    if let Err(e) = view.discard(&dir) {
        // Transient on Windows (a scanner holding a handle): keep the
        // entry, and try again another time.
        tracing::debug!("cannot move `{}` out of the cache: {e}", dir.display());
        view.set_entry(
            id,
            Entry {
                state: State::Live,
                ..entry
            },
        );
        return false;
    }
    view.remove_entry(id);
    remove_lock(cache, id);
    true
}

/// Remove the untracked directory `id` if it still has no row, nobody pins
/// it, and nobody holds its lock, which is held while it is renamed out.
/// Whether it went.
fn remove_untracked(view: &mut View, cache: &Cache, id: &EntryId) -> bool {
    if view.entries.contains_key(id) || view.pins.iter().any(|(_, pin)| pin == id) {
        return false;
    }
    let lock = match FileLock::probe(&cache.lock_path(id)) {
        Ok(Probe::Free(lock)) => Some(lock),
        Ok(Probe::Absent) => None,
        Ok(Probe::Held) | Err(_) => return false,
    };
    let dir = cache.entry(id);
    if let Err(e) = view.discard(&dir) {
        tracing::debug!("cannot move `{}` out of the cache: {e}", dir.display());
        return false;
    }
    if let Some(lock) = lock {
        remove(lock);
    }
    true
}

/// Whether someone holds the lock of the entry `id`: a rene building it
/// without the ledger.
fn locked(cache: &Cache, id: &EntryId) -> bool {
    matches!(
        FileLock::probe(&cache.lock_path(id)),
        Ok(Probe::Held) | Err(_)
    )
}

/// Every entry directory in the cache, with its modification time in Unix
/// seconds.
fn entry_dirs(cache: &Cache) -> Vec<(EntryId, u64)> {
    let mut found = Vec::new();
    for kind in cache::kind::ALL {
        let Ok(names) = std::fs::read_dir(cache.root().join(kind)) else {
            continue;
        };
        for name in names.flatten() {
            let Some(id) = name
                .file_name()
                .to_str()
                .and_then(|name| EntryId::parse(&format!("{kind}/{name}")))
            else {
                continue;
            };
            let Ok(meta) = name.metadata() else { continue };
            if meta.is_dir() {
                found.push((id, unix_secs(&meta)));
            }
        }
    }
    found
}

/// Delete the entry's lock file, if nobody holds it.
fn remove_lock(cache: &Cache, id: &EntryId) {
    if let Ok(Probe::Free(lock)) = FileLock::probe(&cache.lock_path(id)) {
        remove(lock);
    }
}

fn unix_secs(meta: &std::fs::Metadata) -> u64 {
    meta.modified()
        .ok()
        .and_then(|time| time.duration_since(UNIX_EPOCH).ok())
        .map_or(0, |d| d.as_secs())
}

#[cfg(test)]
mod tests {
    use super::super::tests::{block_on, build, cache, entry_names as names, id, lock};
    use super::*;

    const MONTH: Duration = Duration::from_secs(30 * 24 * 60 * 60);

    /// Make the entry `id` look unused for a year.
    async fn age(session: &Session, id: &EntryId) {
        session
            .transact(|view| {
                let mut entry = view.entries[id].clone();
                entry.last_used = view.now - 365 * 24 * 60 * 60;
                view.set_entry(id, entry);
            })
            .await
            .unwrap();
    }

    #[test]
    fn prune_removes_old_entries_nobody_pins() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        let [old, new, mine, theirs] = ["old", "new", "mine", "theirs"].map(id);
        block_on(async {
            let a = Session::start(&cache).await;
            for (entry, size) in [(&old, 10), (&new, 20), (&mine, 30), (&theirs, 40)] {
                build(&cache, &a, entry, size).await;
            }
            for entry in [&old, &new, &theirs] {
                a.unpin(entry).await;
            }
            let b = Session::start(&cache).await;
            assert!(b.lookup(&theirs).await.is_some());
            // Pins alone protect `mine` and `theirs`.
            for entry in [&old, &mine, &theirs] {
                age(&a, entry).await;
            }

            let dry = a.prune(MONTH, true).await.unwrap();
            assert_eq!(dry.removed, [(old.clone(), 10)]);
            assert_eq!((dry.in_use, dry.recent), (2, 1));
            assert!(cache.entry(&old).is_dir());

            let pruned = a.prune(MONTH, false).await.unwrap();
            assert_eq!(pruned.removed, [(old.clone(), 10)]);
            assert_eq!(pruned.summary(), "1 entry (10 B)");
            assert_eq!(pruned.total, 20 + 30 + 40);
            assert!(!pruned.skipped);
            // The directory, its trash, and its lock file are all gone.
            assert_eq!(
                names(&cache),
                [
                    "mine",
                    "mine.lock",
                    "new",
                    "new.lock",
                    "theirs",
                    "theirs.lock"
                ]
            );
            let snapshot = a.snapshot().await.unwrap();
            assert_eq!(snapshot.entries.len(), 3);
            assert!(snapshot.entries.values().all(|e| e.state == State::Live));
            assert_eq!(snapshot.total, 90);
        });
    }

    #[test]
    fn a_victim_reclaimed_between_mark_and_remove_survives() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        let e = id("e");
        block_on(async {
            let a = Session::start(&cache).await;
            build(&cache, &a, &e, 5).await;
            a.unpin(&e).await;
            let marked = a
                .transact(|view| mark(view, &cache, &a.id, Duration::ZERO, false))
                .await
                .unwrap();
            assert_eq!(marked.victims, [(e.clone(), 5)]);
            assert_eq!(a.snapshot().await.unwrap().total, 0);

            let b = Session::start(&cache).await;
            assert_eq!(b.lookup(&e).await, Some(super::super::Lookup::Live));
            let removed = a.transact(|view| remove_marked(view, &cache, &a.id, &e));
            assert_eq!(removed.await, Some(false));
            assert!(cache.entry(&e).is_dir());
            let snapshot = a.snapshot().await.unwrap();
            assert_eq!(snapshot.entries[&e].state, State::Live);
            assert_eq!(snapshot.total, 5);
        });
    }

    /// A removal that cannot commit renames its victim back: the row under
    /// prune keeps its directory, so a session asking for the entry
    /// reclaims it instead of waiting for the pruner to exit.
    #[test]
    fn a_removal_that_cannot_commit_puts_the_victim_back() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        let e = id("e");
        block_on(async {
            let a = Session::start(&cache).await;
            build(&cache, &a, &e, 5).await;
            a.unpin(&e).await;
            a.transact(|view| mark(view, &cache, &a.id, Duration::ZERO, false))
                .await
                .unwrap();
            a.store.as_ref().unwrap().fail_commit.set(true);
            let removed = a.transact(|view| remove_marked(view, &cache, &a.id, &e));
            assert_eq!(removed.await, None);
            assert!(!a.enabled());
            assert!(cache.entry(&e).is_dir());

            let b = Session::start(&cache).await;
            assert_eq!(b.lookup(&e).await, Some(super::super::Lookup::Live));
            assert_eq!(b.snapshot().await.unwrap().total, 5);
        });
    }

    #[test]
    fn only_one_session_prunes_at_a_time() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        block_on(async {
            let a = Session::start(&cache).await;
            let _held = FileLock::try_lock(&cache.root().join(PRUNE_LOCK))
                .unwrap()
                .unwrap();
            assert!(a.prune(Duration::ZERO, false).await.unwrap().skipped);
        });
    }

    /// A directory without a row (completed without the ledger) is old by
    /// its modification time. It goes unless someone pins it or holds its
    /// lock.
    #[test]
    fn prune_removes_old_untracked_directories() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        let [loose, wanted, building] = ["loose", "wanted", "building"].map(id);
        for entry in [&loose, &wanted, &building] {
            std::fs::create_dir_all(cache.entry(entry)).unwrap();
            std::fs::write(cache.entry(entry).join("file"), "1234").unwrap();
        }
        block_on(async {
            let a = Session::start(&cache).await;
            let b = Session::start(&cache).await;
            assert!(b.lookup(&wanted).await.is_some());
            let _held = lock(&cache, &building);

            let dry = a.prune(Duration::ZERO, true).await.unwrap();
            assert_eq!(dry.removed, [(loose.clone(), 4)]);
            assert_eq!(dry.in_use, 2);
            let pruned = a.prune(Duration::ZERO, false).await.unwrap();
            assert_eq!(pruned.removed, [(loose.clone(), 4)]);
            assert!(!cache.entry(&loose).exists());
            assert!(cache.entry(&wanted).is_dir() && cache.entry(&building).is_dir());
            // Recent untracked directories are kept.
            assert_eq!(a.prune(MONTH, false).await.unwrap().recent, 2);
        });
    }
}
