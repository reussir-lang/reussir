//! Entries in the ledger (docs/design/cache-ledger.md, "Entries" and "The
//! entry protocol").
//!
//! An entry is a live row `entry/<kind>/<key>` together with its directory
//! `<root>/<kind>/<key>`. A row exists only for a complete entry, so it has
//! no state: it holds the directory's size, its last use, and the
//! consumer's record of what the directory holds. A directory enters an
//! entry path only by the rename in the transaction that writes its row
//! ([`Session::publish`]).
//!
//! A transaction that did not commit cleanly can leave two disagreements,
//! and each is repaired where it is found, never adopted: a live row whose
//! directory is gone is forgotten by the lookup that finds it, and a
//! directory at an entry path without a row is debris, which the next
//! publication of the key discards.

use std::fmt;
use std::fs::Metadata;
use std::io;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};
use serde_json::value::RawValue;

use super::{Session, Tag, Txn, unusable};

/// The prefix of the entry rows.
pub(super) const ENTRY: &str = "entry/";

/// One entry: the directory `<root>/<kind>/<key>`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EntryId {
    /// The sort of entry, and the directory under the root holding every
    /// entry of that sort.
    pub kind: &'static str,
    /// A hash of everything the entry's contents depend on.
    pub key: String,
}

/// An entry row.
#[derive(Debug, Serialize, Deserialize)]
pub struct Row {
    /// The bytes of the directory, measured when it was published.
    pub size: u64,
    /// Unix seconds of the last lookup.
    pub last_used: u64,
    /// The consumer's JSON describing the directory.
    pub record: Box<RawValue>,
}

/// A live entry, as [`Session::lookup`] found it.
#[derive(Debug)]
pub struct Live {
    pub dir: PathBuf,
    /// The record, as JSON.
    pub record: String,
}

/// What [`Session::publish`] did.
#[derive(Debug, PartialEq, Eq)]
pub enum Published {
    /// The staging directory is the entry now. `total` is the size of
    /// every live entry, this one included.
    Placed { total: u64 },
    /// A live entry was there already, and stays; the staging directory
    /// was discarded.
    Kept,
}

impl EntryId {
    /// The entry `text` names, as [`Display`](fmt::Display) writes it:
    /// `<kind>/<key>`, with a kind of `kinds` and an alphanumeric key.
    /// `None` for anything else, so a corrupt row never names a path
    /// outside the cache.
    pub fn parse(text: &str, kinds: &[&'static str]) -> Option<EntryId> {
        let (kind, key) = text.split_once('/')?;
        let kind = kinds.iter().find(|known| **known == kind)?;
        let plain = !key.is_empty() && key.bytes().all(|b| b.is_ascii_alphanumeric());
        plain.then(|| EntryId {
            kind,
            key: key.to_owned(),
        })
    }

    /// The entry's directory in the cache at `root`. Pure: nothing is
    /// created.
    pub fn dir(&self, root: &Path) -> PathBuf {
        root.join(self.kind).join(&self.key)
    }

    /// The key of the entry's row.
    pub(super) fn row_key(&self) -> String {
        format!("{ENTRY}{self}")
    }
}

impl fmt::Display for EntryId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.kind, self.key)
    }
}

impl Session {
    /// Look the entry `id` up in one transaction: pin it, whether or not it
    /// exists, so that nothing removes it while this session wants it; and
    /// if its row is live, stamp its last use and check its directory. A
    /// row whose directory is missing is forgotten, and the entry is then
    /// as absent as one never published.
    pub async fn lookup(&self, id: &EntryId) -> Result<Option<Live>, String> {
        let (name, key, dir) = (id.to_string(), id.row_key(), id.dir(self.root()));
        self.transact(|txn, pins| {
            pins.pin(&name);
            let Some(raw) = txn.get(&key)? else {
                return Ok(None);
            };
            let mut row = read_row(txn, &key, &raw)?;
            if !stat(&dir)?.is_some_and(|meta| meta.is_dir()) {
                txn.delete(&key);
                return Ok(None);
            }
            row.last_used = txn.now();
            txn.put(&key, encode(&row));
            Ok(Some(Live {
                dir: dir.clone(),
                record: row.record.get().to_owned(),
            }))
        })
        .await
    }

    /// Publish the complete, durable directory `staging`, of `size` bytes,
    /// as the entry `id` described by the JSON `record`, in one
    /// transaction. A live entry that is there is kept, and `staging`
    /// discarded. Otherwise whatever lies at the entry path is debris and
    /// is discarded, `staging` is renamed into place, and the row written.
    /// The rename need not be durable: if a power loss undoes it, the next
    /// lookup forgets the row.
    pub async fn publish(
        &self,
        id: &EntryId,
        staging: &Path,
        size: u64,
        record: &str,
    ) -> Result<Published, String> {
        let record = RawValue::from_string(record.to_owned())
            .map_err(|e| format!("the record of `{id}` is not JSON: {e}"))?;
        let (key, dir) = (id.row_key(), id.dir(self.root()));
        self.transact(|txn, _| {
            let live = match txn.get(&key)? {
                Some(raw) => {
                    read_row(txn, &key, &raw)?;
                    true
                }
                None => false,
            };
            let found = stat(&dir)?;
            if live && found.as_ref().is_some_and(Metadata::is_dir) {
                let trash = self.sibling(&dir, Tag::Trash);
                txn.discard(staging, trash)
                    .map_err(|e| moving(staging, e))?;
                return Ok(Published::Kept);
            }
            if found.is_some() {
                let trash = self.sibling(&dir, Tag::Trash);
                txn.discard(&dir, trash).map_err(|e| moving(&dir, e))?;
            }
            txn.rename(staging, &dir).map_err(|e| moving(staging, e))?;
            let row = Row {
                size,
                last_used: txn.now(),
                record: record.clone(),
            };
            txn.put(&key, encode(&row));
            let mut total = size;
            for (other, raw) in txn.scan(ENTRY)? {
                if other != key {
                    total = total.saturating_add(read_row(txn, &other, &raw)?.size);
                }
            }
            Ok(Published::Placed { total })
        })
        .await
    }

    /// Drop this session's pin on the entry `id`, which it no longer
    /// reads.
    pub async fn unpin(&self, id: &EntryId) -> Result<(), String> {
        let name = id.to_string();
        self.transact(|_, pins| {
            pins.unpin(&name);
            Ok(())
        })
        .await
    }
}

/// Decode the entry row `raw` at `key`. A row this rene cannot read makes
/// the ledger unusable: it is never guessed at or dropped.
pub(super) fn read_row(txn: &Txn<'_>, key: &str, raw: &[u8]) -> Result<Row, String> {
    serde_json::from_slice(raw)
        .map_err(|e| unusable(txn.path, format_args!("cannot read row `{key}`: {e}")))
}

pub(super) fn encode(row: &Row) -> Vec<u8> {
    serde_json::to_vec(row).expect("an entry row serializes")
}

/// What lies at `path`, without following a link.
pub(super) fn stat(path: &Path) -> Result<Option<Metadata>, String> {
    match std::fs::symlink_metadata(path) {
        Ok(meta) => Ok(Some(meta)),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(format!("cannot inspect `{}`: {e}", path.display())),
    }
}

fn moving(path: &Path, e: io::Error) -> String {
    format!("cannot move `{}`: {e}", path.display())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::CACHE_DIR_ENV;
    use crate::ledger::tests::block_on;

    const KINDS: &[&str] = &["deps", "rt"];

    fn start(root: &Path) -> Session {
        block_on(Session::start(root)).unwrap()
    }

    fn id(key: &str) -> EntryId {
        EntryId {
            kind: "deps",
            key: key.to_owned(),
        }
    }

    /// A staging directory of `session` for `id`, holding `content`.
    fn stage(session: &Session, id: &EntryId, content: &str) -> PathBuf {
        let staging = session.sibling(&id.dir(session.root()), Tag::Stage);
        std::fs::create_dir_all(&staging).unwrap();
        std::fs::write(staging.join("file"), content).unwrap();
        staging
    }

    fn publish(session: &Session, id: &EntryId, size: u64, record: &str) -> Published {
        let staging = stage(session, id, record);
        block_on(session.publish(id, &staging, size, record)).unwrap()
    }

    fn row(session: &Session, id: &EntryId) -> Option<Row> {
        let raw = block_on(session.transact(|txn, _| txn.get(&id.row_key()))).unwrap();
        raw.map(|raw| serde_json::from_slice(&raw).unwrap())
    }

    fn pinned(session: &Session, id: &EntryId) -> bool {
        block_on(session.transact(|_, pins| Ok(pins.pinned(&id.to_string())))).unwrap()
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
    fn a_lookup_pins_before_the_entry_exists_and_stamps_its_use() {
        let tmp = tempfile::tempdir().unwrap();
        let session = start(tmp.path());
        let e = id("e");
        assert!(block_on(session.lookup(&e)).unwrap().is_none());
        assert!(pinned(&session, &e));
        assert_eq!(
            publish(&session, &e, 1, "{}"),
            Published::Placed { total: 1 }
        );
        block_on(session.transact(|txn, _| {
            let row = Row {
                last_used: 0,
                ..row_of(txn, &e)
            };
            txn.put(&e.row_key(), encode(&row));
            Ok(())
        }))
        .unwrap();
        block_on(session.lookup(&e)).unwrap().unwrap();
        assert!(row(&session, &e).unwrap().last_used > 0);
    }

    fn row_of(txn: &Txn<'_>, id: &EntryId) -> Row {
        serde_json::from_slice(&txn.get(&id.row_key()).unwrap().unwrap()).unwrap()
    }

    #[test]
    fn a_published_entry_is_found_by_another_session() {
        let tmp = tempfile::tempdir().unwrap();
        let [a, b] = [(); 2].map(|()| start(tmp.path()));
        let e = id("e");
        assert!(block_on(a.lookup(&e)).unwrap().is_none());
        publish(&a, &e, 3, r#"{"files": ["file"]}"#);
        let live = block_on(b.lookup(&e)).unwrap().unwrap();
        assert_eq!(live.dir, e.dir(tmp.path()));
        assert_eq!(live.record, r#"{"files": ["file"]}"#);
        assert_eq!(
            std::fs::read_to_string(live.dir.join("file")).unwrap(),
            live.record
        );
        assert_eq!(row(&b, &e).unwrap().size, 3);
        assert_eq!(names(tmp.path()), ["e"]);
    }

    #[test]
    fn a_live_row_without_its_directory_is_forgotten() {
        let tmp = tempfile::tempdir().unwrap();
        let session = start(tmp.path());
        let e = id("e");
        publish(&session, &e, 1, "{}");
        std::fs::remove_dir_all(e.dir(tmp.path())).unwrap();
        assert!(block_on(session.lookup(&e)).unwrap().is_none());
        assert!(row(&session, &e).is_none());
        assert!(pinned(&session, &e));
    }

    #[test]
    fn debris_at_the_entry_path_is_trashed_by_the_next_publication() {
        let tmp = tempfile::tempdir().unwrap();
        let session = start(tmp.path());
        let e = id("e");
        let debris = e.dir(tmp.path());
        std::fs::create_dir_all(debris.join("half")).unwrap();
        assert!(block_on(session.lookup(&e)).unwrap().is_none());
        publish(&session, &e, 1, "{}");
        assert_eq!(names(tmp.path()), ["e"]);
        assert!(!debris.join("half").exists() && debris.join("file").is_file());
    }

    #[test]
    fn a_failed_publication_renames_everything_back() {
        let tmp = tempfile::tempdir().unwrap();
        let session = start(tmp.path());
        let e = id("e");
        let debris = e.dir(tmp.path());
        std::fs::create_dir_all(debris.join("half")).unwrap();
        let staging = stage(&session, &e, "new");
        session.ledger.fail_commit.set(true);
        assert!(block_on(session.publish(&e, &staging, 1, "{}")).is_err());
        assert!(debris.join("half").is_dir() && staging.join("file").is_file());
        assert_eq!(names(tmp.path()).len(), 2);
        assert!(row(&session, &e).is_none());
    }

    #[test]
    fn a_publication_that_meets_a_live_entry_keeps_it() {
        let tmp = tempfile::tempdir().unwrap();
        let [a, b] = [(); 2].map(|()| start(tmp.path()));
        let e = id("e");
        publish(&a, &e, 1, r#""first""#);
        assert_eq!(publish(&b, &e, 2, r#""second""#), Published::Kept);
        assert_eq!(names(tmp.path()), ["e"]);
        let live = block_on(b.lookup(&e)).unwrap().unwrap();
        assert_eq!(live.record, r#""first""#);
        assert_eq!(row(&b, &e).unwrap().size, 1);
    }

    #[test]
    fn unpin_drops_only_that_pin() {
        let tmp = tempfile::tempdir().unwrap();
        let [a, b] = [(); 2].map(|()| start(tmp.path()));
        let [d, e] = [id("d"), id("e")];
        for (session, entry) in [(&a, &d), (&a, &e), (&b, &e)] {
            assert!(block_on(session.lookup(entry)).unwrap().is_none());
        }
        block_on(a.unpin(&e)).unwrap();
        let rows = block_on(a.transact(|txn, _| txn.scan("pin/"))).unwrap();
        let mut rows: Vec<String> = rows.into_iter().map(|(key, _)| key).collect();
        rows.sort();
        let mut expected = [
            format!("pin/{}/deps/d", a.id()),
            format!("pin/{}/deps/e", b.id()),
        ];
        expected.sort();
        assert_eq!(rows, expected);
    }

    #[test]
    fn the_total_sums_the_live_rows() {
        let tmp = tempfile::tempdir().unwrap();
        let session = start(tmp.path());
        assert_eq!(
            publish(&session, &id("a"), 5, "{}"),
            Published::Placed { total: 5 }
        );
        assert_eq!(
            publish(&session, &id("b"), 7, "{}"),
            Published::Placed { total: 12 }
        );
        assert_eq!(publish(&session, &id("b"), 9, "{}"), Published::Kept);
    }

    #[test]
    fn an_unreadable_row_fails_the_transaction() {
        let tmp = tempfile::tempdir().unwrap();
        let session = start(tmp.path());
        let e = id("e");
        block_on(session.transact(|txn, _| {
            txn.put(&e.row_key(), "not a row");
            Ok(())
        }))
        .unwrap();
        let err = block_on(session.lookup(&e)).unwrap_err();
        assert!(
            err.contains("entry/deps/e") && err.contains(CACHE_DIR_ENV),
            "{err}"
        );
        let err = publish_err(&session, &id("other"));
        assert!(err.contains("entry/deps/e"), "{err}");
    }

    fn publish_err(session: &Session, id: &EntryId) -> String {
        let staging = stage(session, id, "x");
        block_on(session.publish(id, &staging, 1, "{}")).unwrap_err()
    }

    #[test]
    fn parse_takes_only_known_kinds_and_plain_keys() {
        let e = EntryId {
            kind: "rt",
            key: "abc123".to_owned(),
        };
        assert_eq!(e.to_string(), "rt/abc123");
        assert_eq!(EntryId::parse("rt/abc123", KINDS), Some(e.clone()));
        assert_eq!(e.dir(Path::new("/c")), Path::new("/c/rt/abc123"));
        for text in [
            "rt",
            "rt/",
            "rt/a/b",
            "rt/..",
            "rt/.",
            "rt/a.b",
            "core-src/abc",
            "/abc",
        ] {
            assert_eq!(EntryId::parse(text, KINDS), None, "{text}");
        }
    }
}
