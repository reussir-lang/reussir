//! Pruning and repair (docs/design/cache-ledger.md, "Pruning, repair and
//! sweeping").
//!
//! A prune is one transaction over the whole ledger ([`Session::prune`]).
//! It forgets the live rows whose directories are missing. With an age, it
//! renames to the trash the directories of the entries unused for that
//! long that no live session pins, and deletes their rows. It trashes every
//! directory at an entry path without a row, debris of a transaction that
//! did not commit cleanly, and stamps `pruned`. The ledger serializes it
//! with every other transaction, so there is no prune lock, and a prune
//! that does not commit renames everything back.
//!
//! A repair is a prune without an age: it removes no entry, only what
//! disagrees. One that finds `pruned` fresh, because another rene has just
//! pruned, does nothing.

use std::collections::BTreeSet;
use std::io;
use std::path::Path;
use std::time::Duration;

use super::entry::{ENTRY, read_row, stat};
use super::{EntryId, Pins, Session, Tag, Txn, unusable};
use crate::config::human_size;

/// The row holding the Unix seconds of the last prune that committed.
const PRUNED: &str = "pruned";

/// How old `pruned` grows before a repair is due.
pub const REPAIR_INTERVAL: Duration = Duration::from_secs(60 * 60);

/// What a prune removed, or in a dry run would remove.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Pruned {
    /// The entries removed, with their sizes.
    pub removed: Vec<(EntryId, u64)>,
    /// Entries old enough to go, kept because a live session pins them.
    pub in_use: usize,
    /// Entries kept because they were used within the age.
    pub recent: usize,
    /// The size of the live entries afterwards.
    pub total: u64,
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

impl Session {
    /// Prune the entries of `kinds` in one transaction (see the module
    /// docs): with `max_age`, remove those unused for that long that no
    /// live session pins, keeping a victim whose directory cannot be
    /// renamed; without, only repair. A dry run reports what would go, and
    /// renames, deletes and stamps nothing.
    pub async fn prune(
        &self,
        kinds: &[&'static str],
        max_age: Option<Duration>,
        dry_run: bool,
    ) -> Result<Pruned, String> {
        let op =
            |txn: &mut Txn<'_>, pins: &mut Pins| self.prune_in(txn, pins, kinds, max_age, dry_run);
        self.transact(op).await
    }

    fn prune_in(
        &self,
        txn: &mut Txn<'_>,
        pins: &Pins,
        kinds: &[&'static str],
        max_age: Option<Duration>,
        dry_run: bool,
    ) -> Result<Pruned, String> {
        let fresh = max_age.is_none() && !repair_due(txn)?;
        let cutoff = max_age.map(|age| txn.now().saturating_sub(age.as_secs()));
        let (mut pruned, mut live) = (Pruned::default(), BTreeSet::new());
        for (key, raw) in txn.scan(ENTRY)? {
            let row = read_row(txn, &key, &raw)?;
            let Some(id) = EntryId::parse(&key[ENTRY.len()..], kinds) else {
                return Err(unusable(txn.path, format_args!("cannot read row `{key}`")));
            };
            let dir = id.dir(self.root());
            if !fresh && !stat(&dir)?.is_some_and(|meta| meta.is_dir()) {
                if !dry_run {
                    txn.delete(&key);
                }
                continue;
            }
            let victim = match cutoff {
                Some(cutoff) if row.last_used > cutoff => {
                    pruned.recent += 1;
                    false
                }
                Some(_) if pins.pinned(&id.to_string()) => {
                    pruned.in_use += 1;
                    false
                }
                Some(_) => dry_run || self.trash(txn, &dir).is_ok(),
                None => false,
            };
            if victim {
                pruned.removed.push((id, row.size));
                if !dry_run {
                    txn.delete(&key);
                }
            } else {
                pruned.total = pruned.total.saturating_add(row.size);
                live.insert(id);
            }
        }
        if dry_run || fresh {
            return Ok(pruned);
        }
        for kind in kinds {
            for id in entry_paths(self.root(), kind, kinds)? {
                if !live.contains(&id) {
                    // Debris that cannot be moved stays for the next repair.
                    let _ = self.trash(txn, &id.dir(self.root()));
                }
            }
        }
        txn.put(PRUNED, txn.now().to_string());
        Ok(pruned)
    }

    /// Rename `dir` to this session's trash. On Windows that may fail for a
    /// while, as when a scanner holds a file under `dir`.
    fn trash(&self, txn: &mut Txn<'_>, dir: &Path) -> io::Result<()> {
        let trash = self.sibling(dir, Tag::Trash);
        txn.discard(dir, trash).inspect_err(|e| {
            tracing::debug!("cannot move `{}` to the trash: {e}", dir.display());
        })
    }
}

/// Whether `pruned` is missing, older than [`REPAIR_INTERVAL`], or from
/// the future (a clock set back), so that a repair is due.
pub(super) fn repair_due(txn: &Txn<'_>) -> Result<bool, String> {
    let Some(raw) = txn.get(PRUNED)? else {
        return Ok(true);
    };
    let stamp: u64 = String::from_utf8_lossy(&raw)
        .parse()
        .map_err(|_| unusable(txn.path, format_args!("cannot read row `{PRUNED}`")))?;
    let now = txn.now();
    Ok(stamp > now || now - stamp >= REPAIR_INTERVAL.as_secs())
}

/// The entry paths in the directory of `kind` under `root`.
fn entry_paths(root: &Path, kind: &str, kinds: &[&'static str]) -> Result<Vec<EntryId>, String> {
    let dir = root.join(kind);
    let names = match std::fs::read_dir(&dir) {
        Ok(names) => names,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(format!("cannot list `{}`: {e}", dir.display())),
    };
    let name = |entry: std::fs::DirEntry| format!("{kind}/{}", entry.file_name().display());
    let ids = names
        .flatten()
        .filter_map(|entry| EntryId::parse(&name(entry), kinds));
    Ok(ids.collect())
}

#[cfg(test)]
mod tests {
    use super::super::entry::encode;
    use super::*;
    use crate::ledger::tests::block_on;

    const KINDS: &[&str] = &["deps", "rt"];
    const MONTH: Duration = Duration::from_secs(30 * 24 * 60 * 60);

    fn start(root: &Path) -> Session {
        block_on(Session::start(root)).unwrap()
    }

    fn id(key: &str) -> EntryId {
        EntryId::parse(&format!("deps/{key}"), KINDS).unwrap()
    }

    /// Publish `id`, of `size` bytes; unless `recent`, last used in 1970.
    fn publish(session: &Session, id: &EntryId, size: u64, recent: bool) {
        let staging = session.sibling(&id.dir(session.root()), Tag::Stage);
        std::fs::create_dir_all(&staging).unwrap();
        block_on(session.publish(id, &staging, size, "null")).unwrap();
        let key = id.row_key();
        block_on(session.transact(|txn, _| {
            let mut row = read_row(txn, &key, &txn.get(&key)?.unwrap())?;
            row.last_used = if recent { row.last_used } else { 0 };
            txn.put(&key, encode(&row));
            Ok(())
        }))
        .unwrap();
    }

    fn pin(session: &Session, id: &EntryId) {
        let op = |_: &mut Txn<'_>, pins: &mut Pins| {
            pins.pin(&id.to_string());
            Ok(())
        };
        block_on(session.transact(op)).unwrap();
    }

    fn prune(session: &Session, max_age: Option<Duration>, dry_run: bool) -> Pruned {
        block_on(session.prune(KINDS, max_age, dry_run)).unwrap()
    }

    /// The entry rows' keys and the `pruned` stamp.
    fn rows(session: &Session) -> (Vec<String>, Option<Vec<u8>>) {
        let scan = |txn: &mut Txn<'_>, _: &mut Pins| Ok((txn.scan(ENTRY)?, txn.get(PRUNED)?));
        let (rows, stamp) = block_on(session.transact(scan)).unwrap();
        (rows.into_iter().map(|(key, _)| key).collect(), stamp)
    }

    /// The names in the `deps` kind directory, sorted.
    fn names(root: &Path) -> Vec<String> {
        let dir = std::fs::read_dir(root.join("deps")).unwrap();
        let mut names: Vec<String> = dir
            .map(|entry| entry.unwrap().file_name().into_string().unwrap())
            .collect();
        names.sort();
        names
    }

    #[test]
    fn old_unpinned_entries_go_and_pinned_or_recent_ones_stay() {
        let tmp = tempfile::tempdir().unwrap();
        let [a, b] = [(); 2].map(|()| start(tmp.path()));
        let [old, new, mine, theirs] = ["old", "new", "mine", "theirs"].map(id);
        for (entry, size) in [(&old, 10), (&new, 20), (&mine, 30), (&theirs, 40)] {
            publish(&a, entry, size, entry == &new);
        }
        pin(&a, &mine);
        pin(&b, &theirs);

        let dry = prune(&a, Some(MONTH), true);
        let expected = Pruned {
            removed: vec![(old.clone(), 10)],
            in_use: 2,
            recent: 1,
            total: 90,
        };
        assert_eq!(dry, expected);
        assert_eq!(names(tmp.path()), ["mine", "new", "old", "theirs"]);
        let (keys, stamp) = rows(&a);
        assert_eq!((keys.len(), stamp), (4, None), "a dry run changes nothing");

        assert_eq!(prune(&a, Some(MONTH), false), expected);
        assert_eq!(expected.summary(), "1 entry (10 B)");
        assert_eq!(names(tmp.path()), ["mine", "new", "theirs"]);
        let (keys, stamp) = rows(&a);
        assert_eq!((keys.len(), stamp.is_some()), (3, true));

        // A second prune in a row finds nothing; once `b` ends, its pin
        // no longer keeps its entry.
        let again = prune(&a, Some(MONTH), false);
        assert_eq!((again.removed.len(), again.total), (0, 90));
        drop(b);
        assert_eq!(prune(&a, Some(MONTH), false).removed, [(theirs, 40)]);
    }

    #[test]
    fn a_repair_removes_only_what_disagrees_and_is_due_hourly() {
        let tmp = tempfile::tempdir().unwrap();
        let session = start(tmp.path());
        let [kept, lost] = ["kept", "lost"].map(id);
        publish(&session, &kept, 1, false);
        publish(&session, &lost, 2, false);
        std::fs::remove_dir_all(lost.dir(tmp.path())).unwrap();
        let debris = id("debris").dir(tmp.path());
        std::fs::create_dir_all(debris.join("half")).unwrap();

        let repaired = Pruned {
            total: 1,
            ..Pruned::default()
        };
        assert_eq!(prune(&session, None, false), repaired);
        assert_eq!(names(tmp.path()), ["kept"]);
        let (keys, stamp) = rows(&session);
        assert_eq!(
            (keys, stamp.is_some()),
            (vec!["entry/deps/kept".to_owned()], true)
        );

        // Within the hour a repair does nothing; once the stamp is an hour
        // old, or from a clock set back, it repairs again.
        std::fs::create_dir(&debris).unwrap();
        assert_eq!(prune(&session, None, false), repaired);
        assert!(debris.is_dir());
        for skew in [-3600, 60] {
            block_on(session.transact(|txn, _| {
                let stamp = txn.now().checked_add_signed(skew).unwrap();
                txn.put(PRUNED, stamp.to_string());
                Ok(())
            }))
            .unwrap();
            assert_eq!(prune(&session, None, false), repaired);
            assert!(!debris.exists(), "{skew}");
            std::fs::create_dir(&debris).unwrap();
        }
        // A prune with an age is never skipped.
        assert_eq!(prune(&session, Some(MONTH), false).removed, [(kept, 1)]);
        assert!(names(tmp.path()).is_empty());
    }

    #[test]
    fn a_prune_that_does_not_commit_renames_everything_back() {
        let tmp = tempfile::tempdir().unwrap();
        let session = start(tmp.path());
        publish(&session, &id("old"), 1, false);
        std::fs::create_dir(id("debris").dir(tmp.path())).unwrap();
        session.ledger.fail_commit.set(true);
        assert!(block_on(session.prune(KINDS, Some(Duration::ZERO), false)).is_err());
        assert_eq!(names(tmp.path()), ["debris", "old"]);
        assert_eq!(rows(&session), (vec!["entry/deps/old".to_owned()], None));
    }
}
