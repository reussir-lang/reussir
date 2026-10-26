//! Sessions: one per rene that uses the cache (docs/design/cache-ledger.md,
//! "Sessions and reaping").
//!
//! A session's liveness is its OS lock on `sessions/<sid>.lock`, held for
//! the whole life of the process, never a pid or a timestamp. A session
//! starts without the ledger, by taking that lock, and ends without it, by
//! removing the lock file ([`FileLock::remove`]), so ending and crashing
//! differ only in whether the file stays. The file locks' contract makes a
//! free or missing lock file proof that the session is over.
//!
//! Every transaction of a session reaps first ([`Session::transact`]): the
//! pins of every other session whose lock is free or gone are deleted. One
//! code path thus serves every way a session ends, and every build runs it.
//! Everything else a dead session leaves carries its id in its name
//! ([`Session::sibling`]), for the sweep to recognize.

use std::cell::Cell;
use std::collections::{BTreeMap, BTreeSet};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use super::{Ledger, Txn, unusable};
use crate::lock::{self, FileLock, Probe};

/// The directory of the session lock files under the cache root.
pub const SESSIONS_DIR: &str = "sessions";

/// The prefix of the pin rows: `pin/<sid>/<name>` says that session
/// `<sid>` wants what `<name>` names.
const PIN: &str = "pin/";

/// This rene's session in the cache at one root. Dropping it ends the
/// session.
#[derive(Debug)]
pub struct Session {
    root: PathBuf,
    id: String,
    pub(super) ledger: Ledger,
    /// Held for the session's life; taken by `Drop`.
    lock: Option<FileLock>,
    /// The number of the next [`sibling`](Self::sibling).
    next: Cell<u64>,
}

/// What a [`Session::sibling`] is for.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tag {
    /// A directory being built, renamed into place once complete.
    Stage,
    /// A directory renamed out of the cache, to be deleted.
    Trash,
}

/// The pins one transaction sees, after reaping: this session's, which
/// [`pin`](Self::pin) and [`unpin`](Self::unpin) change, and those of the
/// other live sessions. The changes are written when the operation
/// succeeds.
#[derive(Debug, Default)]
pub struct Pins {
    mine: BTreeSet<String>,
    loaded: BTreeSet<String>,
    others: BTreeSet<String>,
}

impl Session {
    /// Start a session in the cache rooted at `root`, which must exist:
    /// refuse a root on a network file system, whose locks cannot be
    /// trusted, then create and lock `sessions/<sid>.lock`. Both failures
    /// are errors naming the root.
    pub async fn start(root: &Path) -> Result<Session, String> {
        check_local(root, lock::remote_file_system(root))?;
        let ledger = Ledger::new(root)?;
        let id = new_id();
        let path = lock_path(root, &id);
        let dir = root.join(SESSIONS_DIR);
        std::fs::create_dir_all(&dir)
            .map_err(|e| unusable(root, format_args!("cannot create `{}`: {e}", dir.display())))?;
        // A reaper or sweeper probing the fresh file holds it for an
        // instant, and one deleting it makes the identity check start over.
        let lock = FileLock::lock(&path, |_| {})
            .await
            .map_err(|e| unusable(root, format_args!("cannot lock `{}`: {e}", path.display())))?;
        Ok(Session {
            root: root.to_owned(),
            id,
            ledger,
            lock: Some(lock),
            next: Cell::new(0),
        })
    }

    /// 16 hex digits, unique across processes and reboots.
    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    /// Run `op` as one ledger transaction of this session, after reaping
    /// the pins of dead sessions. `op` reads and changes the pins through
    /// [`Pins`]; only the `pin/` rows are read for it.
    pub async fn transact<T>(
        &self,
        mut op: impl FnMut(&mut Txn<'_>, &mut Pins) -> Result<T, String>,
    ) -> Result<T, String> {
        self.ledger
            .transact(|txn| {
                let mut pins = reap(txn, &self.root, &self.id)?;
                let value = op(txn, &mut pins)?;
                pins.write(txn, &self.id);
                Ok(value)
            })
            .await
    }

    /// A fresh path beside `dir`, owned by this session:
    /// `<name>.<stage|trash>-<sid>-<n>`.
    pub fn sibling(&self, dir: &Path, tag: Tag) -> PathBuf {
        let n = self.next.replace(self.next.get() + 1);
        let tag = match tag {
            Tag::Stage => "stage",
            Tag::Trash => "trash",
        };
        let mut name = dir.file_name().unwrap_or_default().to_os_string();
        name.push(format!(".{tag}-{}-{n}", self.id));
        dir.with_file_name(name)
    }

    /// End the session as a crash does: release the lock, keep the file.
    #[cfg(test)]
    pub(crate) fn crash(mut self) {
        drop(self.lock.take());
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        if let Some(lock) = self.lock.take() {
            let path = lock.path().to_owned();
            if let Err(e) = lock.remove() {
                tracing::debug!("cannot remove `{}`: {e}", path.display());
            }
        }
    }
}

impl Pins {
    /// Pin `name` for this session.
    pub fn pin(&mut self, name: &str) {
        self.mine.insert(name.to_owned());
    }

    /// Drop this session's pin on `name`, if any.
    pub fn unpin(&mut self, name: &str) {
        self.mine.remove(name);
    }

    /// Whether a live session, this one included, pins `name`.
    pub fn pinned(&self, name: &str) -> bool {
        self.mine.contains(name) || self.others.contains(name)
    }

    fn write(&self, txn: &mut Txn<'_>, me: &str) {
        for name in self.mine.difference(&self.loaded) {
            txn.put(&format!("{PIN}{me}/{name}"), "");
        }
        for name in self.loaded.difference(&self.mine) {
            txn.delete(&format!("{PIN}{me}/{name}"));
        }
    }
}

/// The file and directory names a [`Session::sibling`] made: the name it
/// was made beside, its tag, and the owning session's id. `None` for any
/// other name.
pub fn parse_sibling(name: &str) -> Option<(&str, Tag, &str)> {
    let (base, suffix) = name.rsplit_once('.')?;
    let (tag, owner) = suffix.split_once('-')?;
    let tag = match tag {
        "stage" => Tag::Stage,
        "trash" => Tag::Trash,
        _ => return None,
    };
    let (sid, n) = owner.split_once('-')?;
    let numbered = !n.is_empty() && n.bytes().all(|b| b.is_ascii_digit());
    (!base.is_empty() && is_id(sid) && numbered).then_some((base, tag, sid))
}

/// Whether session `sid` of the cache at `root` is alive: its lock is held.
/// A free lock file is removed while held, and a missing one means the
/// session ended. A lock that cannot be probed counts as held, since
/// nothing acts on a session it has not proven dead.
pub fn alive(root: &Path, sid: &str) -> bool {
    let path = lock_path(root, sid);
    match FileLock::probe(&path) {
        Ok(Probe::Held) => true,
        Ok(Probe::Absent) => false,
        Ok(Probe::Free(lock)) => {
            if let Err(e) = lock.remove() {
                tracing::debug!("cannot remove `{}`: {e}", path.display());
            }
            false
        }
        Err(e) => {
            tracing::debug!("cannot probe `{}`: {e}", path.display());
            true
        }
    }
}

/// Load the pins and delete those of every dead session but `me`.
fn reap(txn: &mut Txn<'_>, root: &Path, me: &str) -> Result<Pins, String> {
    let mut sessions: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for (key, _) in txn.scan(PIN)? {
        match key[PIN.len()..].split_once('/') {
            Some((sid, name)) if is_id(sid) && !name.is_empty() => {
                let names = sessions.entry(sid.to_owned()).or_default();
                names.insert(name.to_owned());
            }
            _ => return Err(unusable(txn.path, format_args!("cannot read row `{key}`"))),
        }
    }
    let mut pins = Pins::default();
    for (sid, names) in sessions {
        if sid == me {
            pins.loaded.clone_from(&names);
            pins.mine = names;
        } else if alive(root, &sid) {
            pins.others.extend(names);
        } else {
            for name in names {
                txn.delete(&format!("{PIN}{sid}/{name}"));
            }
        }
    }
    Ok(pins)
}

/// Refuse a root that [`lock::remote_file_system`] found remote.
fn check_local(root: &Path, remote: io::Result<Option<&str>>) -> Result<(), String> {
    match remote {
        Ok(None) => Ok(()),
        Ok(Some(kind)) => Err(unusable(
            root,
            format_args!("it is on a network file system ({kind}), whose locks rene cannot trust"),
        )),
        Err(e) => Err(unusable(root, format_args!("cannot inspect it: {e}"))),
    }
}

fn lock_path(root: &Path, sid: &str) -> PathBuf {
    root.join(SESSIONS_DIR).join(format!("{sid}.lock"))
}

/// A session id: 16 hex digits hashed from the pid, the time and a
/// counter, so it never contains a separator.
fn new_id() -> String {
    static COUNT: AtomicU64 = AtomicU64::new(0);
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |d| d.as_nanos());
    let mut hasher = blake3::Hasher::new();
    hasher.update(&std::process::id().to_le_bytes());
    hasher.update(&nanos.to_le_bytes());
    hasher.update(&COUNT.fetch_add(1, Ordering::Relaxed).to_le_bytes());
    hasher.finalize().to_hex()[..16].to_owned()
}

fn is_id(text: &str) -> bool {
    text.len() == 16 && text.bytes().all(|b| matches!(b, b'0'..=b'9' | b'a'..=b'f'))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::CACHE_DIR_ENV;
    use crate::ledger::tests::block_on;

    fn pin(session: &Session, name: &str) {
        let op = |_: &mut Txn<'_>, pins: &mut Pins| {
            pins.pin(name);
            Ok(())
        };
        block_on(session.transact(op)).unwrap();
    }

    /// The pin rows, as `(sid, name)`, after one transaction of `session`.
    fn pin_rows(session: &Session) -> Vec<(String, String)> {
        let rows = block_on(session.transact(|txn, _| txn.scan(PIN))).unwrap();
        let rows = rows.into_iter().map(|(key, _)| {
            let (sid, name) = key[PIN.len()..].split_once('/').unwrap();
            (sid.to_owned(), name.to_owned())
        });
        rows.collect()
    }

    #[test]
    fn a_session_holds_its_lock_until_it_ends() {
        let tmp = tempfile::tempdir().unwrap();
        let session = block_on(Session::start(tmp.path())).unwrap();
        let path = lock_path(tmp.path(), session.id());
        assert!(matches!(FileLock::probe(&path).unwrap(), Probe::Held));
        assert!(alive(tmp.path(), session.id()));
        drop(session);
        assert!(!path.exists());
    }

    #[test]
    fn the_pins_of_an_ended_session_are_reaped() {
        let tmp = tempfile::tempdir().unwrap();
        let [a, b] = [(); 2].map(|()| block_on(Session::start(tmp.path())).unwrap());
        pin(&a, "x/1");
        pin(&b, "x/2");
        assert_eq!(pin_rows(&b).len(), 2, "a live session keeps its pins");
        drop(a);
        let reaped = block_on(b.transact(|_, pins| Ok(pins.pinned("x/1")))).unwrap();
        assert!(!reaped);
        assert_eq!(pin_rows(&b), [(b.id().to_owned(), "x/2".to_owned())]);
    }

    #[test]
    fn a_crashed_session_is_reaped_and_its_file_removed() {
        let tmp = tempfile::tempdir().unwrap();
        let [dead, b] = [(); 2].map(|()| block_on(Session::start(tmp.path())).unwrap());
        pin(&dead, "x/1");
        let path = lock_path(tmp.path(), dead.id());
        dead.crash();
        assert!(path.is_file());
        block_on(b.transact(|_, _| Ok(()))).unwrap();
        assert!(!path.exists());
        assert!(pin_rows(&b).is_empty());
    }

    /// Reaping probes only the sessions with pins: a session that has just
    /// locked its file, and a dead one without pins, are the sweep's.
    #[test]
    fn a_session_without_pins_is_left_alone() {
        let tmp = tempfile::tempdir().unwrap();
        let session = block_on(Session::start(tmp.path())).unwrap();
        let starting = lock_path(tmp.path(), &new_id());
        let held = FileLock::try_lock(&starting).unwrap().unwrap();
        let dead = lock_path(tmp.path(), &new_id());
        drop(FileLock::try_lock(&dead).unwrap().unwrap());
        block_on(session.transact(|_, _| Ok(()))).unwrap();
        assert!(starting.is_file() && dead.is_file());
        drop(held);
    }

    #[test]
    fn a_root_that_cannot_hold_session_locks_is_refused() {
        let tmp = tempfile::tempdir().unwrap();
        std::fs::write(tmp.path().join(SESSIONS_DIR), "").unwrap();
        let err = block_on(Session::start(tmp.path())).unwrap_err();
        assert!(
            err.contains(&format!("`{}`", tmp.path().display())),
            "{err}"
        );
        assert!(err.contains(CACHE_DIR_ENV), "{err}");
    }

    #[test]
    fn a_root_on_a_network_file_system_is_refused() {
        let root = Path::new("/home/me/.cache/rene");
        let err = check_local(root, Ok(Some("NFS"))).unwrap_err();
        assert!(err.contains("`/home/me/.cache/rene`"), "{err}");
        assert!(err.contains("network file system (NFS)"), "{err}");
        assert!(err.contains(CACHE_DIR_ENV), "{err}");
        assert!(check_local(root, Ok(None)).is_ok());
        assert!(check_local(root, Err(io::Error::other("gone"))).is_err());
    }

    #[test]
    fn siblings_are_unique_and_recognized() {
        let tmp = tempfile::tempdir().unwrap();
        let session = block_on(Session::start(tmp.path())).unwrap();
        let entry = tmp.path().join("rt").join("0123abcd");
        let made =
            [Tag::Stage, Tag::Stage, Tag::Trash].map(|tag| (session.sibling(&entry, tag), tag));
        assert_ne!(made[0].0, made[1].0);
        for (path, tag) in &made {
            assert_eq!(path.parent(), entry.parent());
            let name = path.file_name().unwrap().to_str().unwrap();
            assert_eq!(parse_sibling(name), Some(("0123abcd", *tag, session.id())));
        }
        let sid = session.id();
        for name in [
            "0123abcd".to_owned(),
            "0123abcd.lock".to_owned(),
            format!("0123abcd.stage-{sid}"),
            format!("0123abcd.stage-{sid}-"),
            format!("0123abcd.stage-{sid}-x"),
            format!("0123abcd.other-{sid}-1"),
            format!("0123abcd.trash-{}-1", &sid[1..]),
            format!("0123abcd.trash-g{}-1", &sid[1..]),
            format!(".stage-{sid}-1"),
        ] {
            assert_eq!(parse_sibling(&name), None, "{name}");
        }
    }

    #[test]
    fn session_ids_are_unique_hex() {
        let ids: BTreeSet<String> = (0..1000).map(|_| new_id()).collect();
        assert_eq!(ids.len(), 1000);
        assert!(ids.iter().all(|id| is_id(id)));
    }
}
