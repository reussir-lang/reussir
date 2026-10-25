//! The one protocol every consumer of the cache uses an entry through.
//!
//! A consumer names the entry it wants and says how to recognize a complete
//! one: its `ready` check, which may also take what it needs from the entry
//! (parse a record, copy files out). [`Cache::acquire`] then
//!
//! 1. runs `ready` with no lock: a complete entry never changes in place,
//!    so using one needs no coordination;
//! 2. otherwise takes the entry's lock, `<kind>/<key>.lock`, waiting for the
//!    rene that holds it and saying so once;
//! 3. runs `ready` again, since whoever held the lock may just have
//!    completed the entry, and otherwise hands the consumer a [`Slot`] that
//!    owns the lock, to build the entry through.
//!
//! So concurrent builds wait for one build of an entry instead of all
//! running it, and only the holder of an entry's lock ever creates, clears,
//! stages, or publishes it. `ready` is the only judge of completeness, so an
//! entry an interrupted build left behind is rebuilt, not trusted. An entry
//! is completed in one of two ways:
//!
//! - **staged**: built in a private sibling ([`Slot::stage`]) and renamed
//!   into place ([`Slot::publish`]), for contents that can move;
//! - **in place**: cleared ([`Slot::clear`]), built where it is, and marked
//!   complete by a record written atomically and last ([`Slot::commit`]),
//!   for contents that cannot move once built (a cargo target directory).
//!
//! A stale directory is never deleted where it lies. It is renamed to a
//! trash sibling first, so the entry's path names either nothing or a whole
//! directory, and a deletion that fails halfway leaves nothing behind there.
//!
//! A complete entry's record vouches for its files by size
//! ([`RecordedFile`]), which `ready` checks: outputs are not synced to disk
//! before the record is, so after a power loss a record can outlive the
//! contents of a file it names.
//!
//! # With a ledger session
//!
//! A cache [`with_session`](Cache::with_session) runs the same protocol
//! through the session's ledger (docs/design/cache-ledger.md), here and
//! nowhere else, so consumers never see it:
//!
//! 1. the entry is looked up first, which pins it for the session. Only an
//!    entry whose row is live is checked with no lock; one being removed
//!    is waited for, saying so once; any other goes to the lock;
//! 2. under the lock, the ledger rechecks with `ready`'s verdict and the
//!    entry's size: a complete entry is used, and adopted if no row
//!    tracked it, while an incomplete one becomes this session's to build;
//! 3. completing the entry ([`Slot::publish`], [`Slot::commit`]) registers
//!    it, live and sized, before the lock is released.
//!
//! An entry used [`Usage::Copied`] is unpinned as soon as it has been copied
//! out or published; one used [`Usage::InPlace`] stays pinned until the
//! session ends. A disabled session has no opinion, and the protocol runs
//! as without one.

use std::cell::Cell;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::time::Duration;

use serde::{Deserialize, Serialize};

use super::lock::FileLock;
use super::{Cache, EntryId, Tag};
use crate::ledger::{Lookup, Recheck, Session};

/// How often a request retries an entry another rene is removing.
const REMOVAL_POLL: Duration = Duration::from_millis(100);

/// How a consumer uses an entry once it has it, which says how long the
/// entry must outlive the request: how long the session pins it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Usage {
    /// Read in place for the rest of the build (sources, a runtime bake).
    InPlace,
    /// Copied out at once, after which the build never reads it again
    /// (compiled artifacts restored into a build directory).
    Copied,
}

/// An entry a consumer asks for.
pub struct Request<'a> {
    pub id: EntryId,
    pub usage: Usage,
    /// The work that builds the entry, for the message shown while waiting
    /// for another rene doing it: "baking reussir-rt".
    pub what: &'a str,
    /// Shows that message, so callers with live progress bars can print
    /// around them.
    pub notify: &'a dyn Fn(&str),
}

/// What [`Cache::acquire`] found.
pub enum Acquired<T> {
    /// The entry is complete, and this is what `ready` took from it.
    Ready(T),
    /// The entry is missing or incomplete, and the caller builds it.
    Build(Slot),
}

/// The right to build one entry: it holds the entry's lock, which dropping
/// the slot releases.
#[derive(Debug)]
#[must_use = "the entry's lock is released when the slot is dropped"]
pub struct Slot {
    dir: PathBuf,
    lock: FileLock,
    /// The entry's id and usage, for the session to register it.
    id: EntryId,
    usage: Usage,
    session: Option<Rc<Session>>,
}

/// A private directory an entry is staged in, deleted when dropped unless
/// [`Slot::publish`] moved it into place.
#[derive(Debug)]
#[must_use = "a staging directory is deleted when dropped"]
pub struct Staging {
    path: PathBuf,
    published: bool,
}

/// A file an entry's record vouches for, with its size when the entry was
/// completed: a file cut short is not the file the record vouches for.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RecordedFile {
    /// Relative to the entry's directory, so it survives publication.
    pub path: PathBuf,
    pub len: u64,
}

impl RecordedFile {
    /// Record `file`, which lies in the entry `dir`, as it is now.
    pub fn measure(dir: &Path, file: &Path) -> Result<Self, String> {
        let path = file
            .strip_prefix(dir)
            .map_err(|_| format!("`{}` lies outside `{}`", file.display(), dir.display()))?;
        let meta = std::fs::metadata(file)
            .map_err(|e| format!("cannot read `{}`: {e}", file.display()))?;
        Ok(RecordedFile {
            path: path.to_owned(),
            len: meta.len(),
        })
    }

    /// Whether the file is still in the entry `dir`, at its recorded size.
    pub fn intact(&self, dir: &Path) -> bool {
        std::fs::metadata(dir.join(&self.path))
            .is_ok_and(|meta| meta.is_file() && meta.len() == self.len)
    }
}

impl Cache {
    /// Use the entry `request` names, or get the right to build it (see the
    /// module docs). `ready` judges whether the entry in a directory is
    /// complete and takes what the caller needs from it.
    pub async fn acquire<T>(
        &self,
        request: &Request<'_>,
        mut ready: impl FnMut(&Path) -> Option<T>,
    ) -> Result<Acquired<T>, String> {
        let dir = self.entry(&request.id);
        let session = self.session().filter(|session| session.enabled());
        let id = &request.id;
        // One message per request for each wait, however often it loops.
        let (said_locked, said_removing) = (Cell::new(false), Cell::new(false));
        let removing = || {
            if !said_removing.replace(true) {
                (request.notify)(&format!(
                    "waiting for another rene to finish removing `{id}` from the cache"
                ));
            }
        };
        loop {
            let lookup = match session {
                Some(session) => session.lookup(id).await,
                None => None,
            };
            match lookup {
                Some(Lookup::Wait) => {
                    removing();
                    compio::runtime::time::sleep(REMOVAL_POLL).await;
                    continue;
                }
                Some(Lookup::Build) => {}
                Some(Lookup::Live) | None => {
                    if let Some(found) = ready(&dir) {
                        self.done(request).await;
                        return Ok(Acquired::Ready(found));
                    }
                }
            }
            let lock = self.lock(request, &said_locked).await?;
            // Whoever held the lock may have just completed the entry.
            let found = ready(&dir);
            let recheck = match session {
                Some(session) => {
                    let size = found.as_ref().map(|_| super::dir_size(&dir));
                    session.recheck(id, size).await
                }
                None => None,
            };
            match (recheck, found) {
                (Some(Recheck::Wait), _) => {
                    drop(lock);
                    removing();
                    compio::runtime::time::sleep(REMOVAL_POLL).await;
                }
                (Some(Recheck::Live) | None, Some(found)) => {
                    drop(lock);
                    self.done(request).await;
                    return Ok(Acquired::Ready(found));
                }
                _ => {
                    return Ok(Acquired::Build(Slot {
                        dir,
                        lock,
                        id: id.clone(),
                        usage: request.usage,
                        session: self.session.clone(),
                    }));
                }
            }
        }
    }

    /// Take the lock of the entry `request` names, saying once per request
    /// that it waits for the rene holding it.
    async fn lock(&self, request: &Request<'_>, said: &Cell<bool>) -> Result<FileLock, String> {
        let lock_path = self.lock_path(&request.id);
        let kind_dir = lock_path.parent().expect("a lock file lies in its kind");
        std::fs::create_dir_all(kind_dir)
            .map_err(|e| format!("cannot create `{}`: {e}", kind_dir.display()))?;
        FileLock::lock(&lock_path, |path| {
            if !said.replace(true) {
                (request.notify)(&format!(
                    "waiting for another rene to finish {} (lock `{}`)",
                    request.what,
                    path.display()
                ));
            }
        })
        .await
        .map_err(|e| format!("cannot lock `{}`: {e}", lock_path.display()))
    }

    /// The request has the entry it asked for: unpin it if it was copied
    /// out.
    async fn done(&self, request: &Request<'_>) {
        if let (Some(session), Usage::Copied) = (self.session(), request.usage) {
            session.unpin(&request.id).await;
        }
    }
}

impl Slot {
    /// The entry's directory.
    pub fn dir(&self) -> &Path {
        &self.dir
    }

    /// A fresh, empty staging directory beside the entry.
    pub fn stage(&self) -> Result<Staging, String> {
        let path = super::sibling(&self.dir, Tag::Tmp);
        // Only the lock holder stages, so a directory by this name was left
        // by a crashed process whose pid this one reuses.
        let _ = std::fs::remove_dir_all(&path);
        std::fs::create_dir(&path)
            .map_err(|e| format!("cannot create `{}`: {e}", path.display()))?;
        Ok(Staging {
            path,
            published: false,
        })
    }

    /// Move the complete `staging` into place, replacing whatever stale
    /// directory the entry held, register the entry, and release the lock.
    /// Returns the entry's directory.
    pub async fn publish(self, mut staging: Staging) -> Result<PathBuf, String> {
        let stale = discard(&self.dir)?;
        super::publish_dir(&staging.path, &self.dir)?;
        staging.published = true;
        let dir = self.complete().await;
        delete(stale);
        Ok(dir)
    }

    /// Make the entry an empty directory, for a build in place.
    pub fn clear(&self) -> Result<(), String> {
        let stale = discard(&self.dir)?;
        std::fs::create_dir_all(&self.dir)
            .map_err(|e| format!("cannot create `{}`: {e}", self.dir.display()))?;
        delete(stale);
        Ok(())
    }

    /// Complete an entry built in place: write its `marker` file atomically,
    /// after everything else, register the entry, and release the lock.
    /// Returns the entry's directory.
    pub async fn commit(self, marker: &str, bytes: &[u8]) -> Result<PathBuf, String> {
        super::write_atomically(&self.dir.join(marker), bytes)?;
        Ok(self.complete().await)
    }

    /// The entry is complete on disk: register it, live and sized, then
    /// release the lock, and unpin it if it is not read in place.
    async fn complete(self) -> PathBuf {
        let Slot {
            dir,
            lock,
            id,
            usage,
            session,
        } = self;
        if let Some(session) = session.filter(|session| session.enabled()) {
            session.register(&id, super::dir_size(&dir)).await;
            drop(lock);
            if usage == Usage::Copied {
                session.unpin(&id).await;
            }
        }
        dir
    }
}

impl Staging {
    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for Staging {
    fn drop(&mut self) {
        if !self.published {
            let _ = std::fs::remove_dir_all(&self.path);
        }
    }
}

/// Rename the directory at `dir`, if any, to a trash sibling, returning it.
fn discard(dir: &Path) -> Result<Option<PathBuf>, String> {
    if !dir.exists() {
        return Ok(None);
    }
    let trash = super::sibling(dir, Tag::Trash);
    std::fs::rename(dir, &trash)
        .map_err(|e| format!("cannot move `{}` out of the way: {e}", dir.display()))?;
    Ok(Some(trash))
}

/// Delete a discarded directory. Failing leaves a trash sibling, which
/// nothing reads.
fn delete(trash: Option<PathBuf>) {
    if let Some(trash) = trash
        && let Err(e) = std::fs::remove_dir_all(&trash)
    {
        tracing::debug!("cannot delete `{}`: {e}", trash.display());
    }
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;
    use std::time::Duration;

    use super::*;

    fn block_on<T>(fut: impl Future<Output = T>) -> T {
        compio::runtime::Runtime::new().unwrap().block_on(fut)
    }

    fn request(notify: &dyn Fn(&str)) -> Request<'_> {
        Request {
            id: EntryId::new("kind", "key"),
            usage: Usage::InPlace,
            what: "testing",
            notify,
        }
    }

    /// A complete entry when its marker is there.
    fn marked(dir: &Path) -> Option<String> {
        std::fs::read_to_string(dir.join("marker")).ok()
    }

    /// The names in the entry's kind directory, sorted.
    fn kind_dir(cache: &Cache) -> Vec<String> {
        let mut names: Vec<String> = std::fs::read_dir(cache.root().join("kind"))
            .unwrap()
            .map(|entry| entry.unwrap().file_name().into_string().unwrap())
            .collect();
        names.sort();
        names
    }

    /// Readers of a complete entry take no lock, so they never queue behind
    /// a build of anything.
    #[test]
    fn a_ready_entry_is_used_without_the_lock() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = Cache::at(tmp.path()).unwrap();
        let id = EntryId::new("kind", "key");
        std::fs::create_dir_all(cache.entry(&id)).unwrap();
        std::fs::write(cache.entry(&id).join("marker"), "done").unwrap();
        let _held = FileLock::try_lock(&cache.lock_path(&id)).unwrap().unwrap();

        let (tx, rx) = std::sync::mpsc::channel();
        let root = cache.root().to_owned();
        std::thread::spawn(move || {
            let cache = Cache::at(root).unwrap();
            let notified = Cell::new(false);
            let notify = |_: &str| notified.set(true);
            let acquired = block_on(cache.acquire(&request(&notify), marked)).unwrap();
            let Acquired::Ready(found) = acquired else {
                panic!("a complete entry was rebuilt");
            };
            tx.send((found, notified.get())).unwrap();
        });
        let (found, notified) = rx.recv_timeout(Duration::from_secs(30)).unwrap();
        assert_eq!(found, "done");
        assert!(!notified);
    }

    /// The slot holds the lock: a second acquirer waits, says so once, and
    /// then finds the entry the first one completed.
    #[test]
    fn a_second_acquirer_waits_for_the_build() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = Cache::at(tmp.path()).unwrap();
        let Acquired::Build(slot) = block_on(cache.acquire(&request(&|_| {}), marked)).unwrap()
        else {
            panic!("an absent entry is ready");
        };

        let (tx, rx) = std::sync::mpsc::channel();
        let waiter = {
            let root = cache.root().to_owned();
            std::thread::spawn(move || {
                let cache = Cache::at(root).unwrap();
                let messages = std::cell::RefCell::new(Vec::new());
                let notify = |msg: &str| {
                    messages.borrow_mut().push(msg.to_owned());
                    tx.send(()).unwrap();
                };
                let acquired = block_on(cache.acquire(&request(&notify), marked)).unwrap();
                let Acquired::Ready(found) = acquired else {
                    panic!("the waiter rebuilt the entry");
                };
                (found, messages.into_inner())
            })
        };
        rx.recv_timeout(Duration::from_secs(30)).unwrap();
        let staging = slot.stage().unwrap();
        std::fs::write(staging.path().join("marker"), "built").unwrap();
        assert!(!waiter.is_finished());
        block_on(slot.publish(staging)).unwrap();

        let (found, messages) = waiter.join().unwrap();
        assert_eq!(found, "built");
        assert_eq!(messages.len(), 1, "{messages:#?}");
        assert!(messages[0].contains("waiting for another rene to finish testing"));
        assert!(messages[0].contains("key.lock"), "{}", messages[0]);
    }

    /// Publishing replaces an incomplete entry whole and leaves no staging
    /// or trash behind; a staging directory that is never published goes
    /// away by itself.
    #[test]
    fn publishing_replaces_an_incomplete_entry() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = Cache::at(tmp.path()).unwrap();
        let dir = cache.entry(&EntryId::new("kind", "key"));
        std::fs::create_dir_all(dir.join("half")).unwrap();

        let Acquired::Build(slot) = block_on(cache.acquire(&request(&|_| {}), marked)).unwrap()
        else {
            panic!("an incomplete entry is ready");
        };
        let abandoned = slot.stage().unwrap();
        let abandoned_path = abandoned.path().to_owned();
        std::fs::write(abandoned_path.join("marker"), "abandoned").unwrap();
        drop(abandoned);
        assert!(!abandoned_path.exists());

        let staging = slot.stage().unwrap();
        std::fs::write(staging.path().join("marker"), "built").unwrap();
        assert_eq!(block_on(slot.publish(staging)).unwrap(), dir);
        assert_eq!(marked(&dir).as_deref(), Some("built"));
        assert!(!dir.join("half").exists());
        assert_eq!(kind_dir(&cache), ["key", "key.lock"]);
        // The lock went with the slot.
        assert!(
            FileLock::try_lock(&cache.lock_path(&EntryId::new("kind", "key")))
                .unwrap()
                .is_some()
        );
    }

    /// A recorded file is intact while it is there at its recorded size,
    /// wherever its entry's directory has moved.
    #[test]
    fn a_recorded_file_is_intact_only_at_its_size() {
        let tmp = tempfile::tempdir().unwrap();
        let staging = tmp.path().join("staging");
        std::fs::create_dir_all(staging.join("sub")).unwrap();
        std::fs::write(staging.join("sub/artifact"), "artifact").unwrap();
        let recorded = RecordedFile::measure(&staging, &staging.join("sub/artifact")).unwrap();
        assert_eq!(recorded.path, Path::new("sub/artifact"));
        assert!(RecordedFile::measure(&staging, &tmp.path().join("elsewhere")).is_err());

        let entry = tmp.path().join("entry");
        std::fs::rename(&staging, &entry).unwrap();
        assert!(recorded.intact(&entry));
        std::fs::write(entry.join("sub/artifact"), "art").unwrap();
        assert!(!recorded.intact(&entry));
        std::fs::remove_file(entry.join("sub/artifact")).unwrap();
        assert!(!recorded.intact(&entry));
    }

    /// An entry built in place starts empty and is complete only once its
    /// marker is written, last.
    #[test]
    fn an_entry_built_in_place_is_complete_once_committed() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = Cache::at(tmp.path()).unwrap();
        let dir = cache.entry(&EntryId::new("kind", "key"));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("stale"), "").unwrap();

        let Acquired::Build(slot) = block_on(cache.acquire(&request(&|_| {}), marked)).unwrap()
        else {
            panic!("an incomplete entry is ready");
        };
        slot.clear().unwrap();
        assert_eq!(std::fs::read_dir(&dir).unwrap().count(), 0);
        std::fs::write(dir.join("artifact"), "").unwrap();
        assert!(marked(&dir).is_none());
        assert_eq!(block_on(slot.commit("marker", b"baked")).unwrap(), dir);

        let acquired = block_on(cache.acquire(&request(&|_| {}), marked)).unwrap();
        assert!(matches!(acquired, Acquired::Ready(found) if found == "baked"));
        assert_eq!(kind_dir(&cache), ["key", "key.lock"]);
    }

    /// With a session, every entry the cache hands out is recorded: what is
    /// built is registered live at its size, entries read in place stay
    /// pinned, and copied ones are unpinned once copied or published.
    #[test]
    fn a_session_records_the_entries_it_uses() {
        let tmp = tempfile::tempdir().unwrap();
        let bare = Cache::at(tmp.path()).unwrap();
        block_on(async {
            let session = Rc::new(Session::start(&bare).await);
            let cache = bare.clone().with_session(session.clone());
            let [sources, built, restored] = [
                (crate::cache::kind::CORE_SRC, "sources", Usage::InPlace),
                (crate::cache::kind::DEPS, "built", Usage::Copied),
                (crate::cache::kind::DEPS, "restored", Usage::Copied),
            ]
            .map(|(kind, key, usage)| Request {
                id: EntryId::new(kind, key),
                usage,
                what: "testing",
                notify: &|_| {},
            });
            for request in [&sources, &built] {
                let Acquired::Build(slot) = cache.acquire(request, marked).await.unwrap() else {
                    panic!("an absent entry is ready");
                };
                let staging = slot.stage().unwrap();
                std::fs::write(staging.path().join("marker"), "12345").unwrap();
                slot.publish(staging).await.unwrap();
            }
            // Completed without the ledger: adopted, at its size.
            let dir = cache.entry(&restored.id);
            std::fs::create_dir_all(&dir).unwrap();
            std::fs::write(dir.join("marker"), "123").unwrap();
            let acquired = cache.acquire(&restored, marked).await.unwrap();
            assert!(matches!(acquired, Acquired::Ready(found) if found == "123"));

            let snapshot = session.snapshot().await.unwrap();
            let sizes: Vec<u64> = [&sources, &built, &restored]
                .map(|request| &snapshot.entries[&request.id])
                .iter()
                .map(|entry| {
                    assert_eq!(entry.state, crate::ledger::State::Live);
                    entry.size
                })
                .collect();
            assert_eq!(sizes, [5, 5, 3]);
            let pinned: Vec<&EntryId> = snapshot.pins.iter().map(|(_, id)| id).collect();
            assert_eq!(pinned, [&sources.id]);
        });
    }
}
