//! The one protocol every consumer uses an entry through
//! (docs/design/cache-ledger.md, "The entry protocol"). Consumers never see
//! the ledger.
//!
//! A consumer names the entry it wants and how it uses it ([`Request`]),
//! and says what to do with a live one. [`Cache::acquire`] then
//!
//! 1. takes the entry's lock, `<kind>/<key>.lock`, waiting for whoever holds
//!    it (a rene building the entry, or one looking it up), and says once
//!    what it waits for if that lasts a second;
//! 2. looks the entry up in one transaction, which pins it for this session
//!    whether or not it exists, and stamps its use. A live entry is handed,
//!    with its record, to the consumer once the lock is released; otherwise
//!    the consumer gets a [`Slot`], which keeps the lock, to build it.
//!
//! The lookup happens under the lock, so it is also the recheck: whoever
//! held the lock has published the entry or given up. The builder fills a
//! fresh staging directory ([`Slot::stage`]), which [`Slot::publish`] makes
//! durable, measures, and publishes in one transaction with the consumer's
//! record, before it releases the lock. A staging directory that is never
//! published is deleted ([`Staging`]).
//!
//! An entry used [`Usage::Copied`] is unpinned as soon as it has been
//! copied out or published; one used [`Usage::InPlace`] stays pinned until
//! the session ends. Two rules keep the protocol free of deadlocks: a
//! session never waits for an entry lock while it holds another one, and
//! nobody waits for anything while holding the ledger.

use std::path::{Path, PathBuf};

use serde::Serialize;
use serde::de::DeserializeOwned;

use super::{Cache, durable};
use crate::ledger::{EntryId, LEDGER_DIR, Published, Tag, unusable};
use crate::lock::FileLock;

/// How a consumer uses an entry, which says how long the session pins it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Usage {
    /// Read in place for the rest of the build (sources, a runtime bake).
    InPlace,
    /// Copied out at once, and never read again (compiled artifacts
    /// restored into a build directory).
    Copied,
}

/// An entry a consumer asks for.
pub struct Request<'a> {
    pub id: EntryId,
    pub usage: Usage,
    /// The work that builds the entry, for the notice shown while waiting
    /// for another rene doing it: "baking reussir-rt".
    pub what: &'a str,
    /// Shows that notice, so that callers with live progress bars can print
    /// around them.
    pub notify: &'a dyn Fn(&str),
}

/// What [`Cache::acquire`] found.
pub enum Acquired<T> {
    /// The entry is live, and this is what the consumer made of it.
    Ready(T),
    /// The entry is absent, and the caller builds it.
    Build(Slot),
}

/// The right to build one entry: it holds the entry's lock, which dropping
/// the slot releases.
#[derive(Debug)]
#[must_use = "the entry's lock is released when the slot is dropped"]
pub struct Slot {
    cache: Cache,
    id: EntryId,
    usage: Usage,
    lock: FileLock,
}

/// A fresh directory an entry is built in, deleted when dropped unless
/// [`Slot::publish`] took it.
#[derive(Debug)]
#[must_use = "a staging directory is deleted when dropped"]
pub struct Staging {
    path: PathBuf,
    published: bool,
}

impl Cache {
    /// Use the entry `request` names, or get the right to build it (see the
    /// module docs). A live entry's directory and its record, decoded as
    /// `R`, go to `use_entry`, after the entry's lock is released.
    pub async fn acquire<R: DeserializeOwned, T>(
        &self,
        request: &Request<'_>,
        use_entry: impl FnOnce(&Path, R) -> Result<T, String>,
    ) -> Result<Acquired<T>, String> {
        let id = &request.id;
        let lock = self.lock(request).await?;
        let Some(live) = self.session().lookup(id).await? else {
            return Ok(Acquired::Build(Slot {
                cache: self.clone(),
                id: id.clone(),
                usage: request.usage,
                lock,
            }));
        };
        drop(lock);
        let record = serde_json::from_str(&live.record).map_err(|e| {
            let ledger = self.root().join(LEDGER_DIR);
            unusable(
                &ledger,
                format_args!("cannot read the record of `{id}`: {e}"),
            )
        })?;
        let value = use_entry(&live.dir, record)?;
        if request.usage == Usage::Copied {
            self.session().unpin(id).await?;
        }
        Ok(Acquired::Ready(value))
    }

    /// The total size of the cache's entries after this session's latest
    /// publication that placed an entry; `None` if none did.
    pub fn total_published(&self) -> Option<u64> {
        self.published.get()
    }

    /// Take the lock of the entry `request` names, saying once what it
    /// waits for if the wait lasts a second.
    async fn lock(&self, request: &Request<'_>) -> Result<FileLock, String> {
        let path = lock_path(self.root(), &request.id);
        let kind_dir = path.parent().expect("an entry's lock lies in its kind");
        std::fs::create_dir_all(kind_dir).map_err(|e| {
            let kind_dir = kind_dir.display();
            unusable(self.root(), format_args!("cannot create `{kind_dir}`: {e}"))
        })?;
        let waiting = |path: &Path| {
            let (what, path) = (request.what, path.display());
            (request.notify)(&format!(
                "waiting for another rene to finish {what} (lock `{path}`)"
            ));
        };
        FileLock::lock(&path, waiting).await.map_err(|e| {
            unusable(
                self.root(),
                format_args!("cannot lock `{}`: {e}", path.display()),
            )
        })
    }
}

impl Slot {
    /// A fresh, empty directory beside the entry, which only this session
    /// touches, to build the entry in.
    pub fn stage(&self) -> Result<Staging, String> {
        let dir = self.id.dir(self.cache.root());
        let path = self.cache.session().sibling(&dir, Tag::Stage);
        std::fs::create_dir(&path)
            .map_err(|e| format!("cannot create `{}`: {e}", path.display()))?;
        Ok(Staging {
            path,
            published: false,
        })
    }

    /// Publish the complete `staging` as the entry, described by `record`:
    /// make it durable and measure it, then, in one transaction, rename it
    /// into place and write the entry's row. A live entry that is there
    /// already stays, and `staging` is discarded. Then release the lock, and
    /// unpin an entry used [`Usage::Copied`]. Returns the entry's
    /// directory.
    pub async fn publish(
        self,
        mut staging: Staging,
        record: &impl Serialize,
    ) -> Result<PathBuf, String> {
        let Slot {
            cache,
            id,
            usage,
            lock,
        } = self;
        let size = durable(&staging.path).map_err(|e| {
            let path = staging.path.display();
            format!("cannot write `{path}` to the disk: {e}")
        })?;
        let record = serde_json::to_string(record)
            .map_err(|e| format!("cannot encode the record of `{id}`: {e}"))?;
        let published = cache
            .session()
            .publish(&id, &staging.path, size, &record)
            .await?;
        staging.published = true;
        if let Published::Placed { total } = published {
            cache.published.set(Some(total));
        }
        drop(lock);
        if usage == Usage::Copied {
            cache.session().unpin(&id).await?;
        }
        Ok(id.dir(cache.root()))
    }
}

impl Staging {
    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for Staging {
    /// Delete an unpublished staging directory. One that cannot be deleted
    /// is the sweep's once the session has ended.
    fn drop(&mut self) {
        if !self.published
            && let Err(e) = std::fs::remove_dir_all(&self.path)
            && e.kind() != std::io::ErrorKind::NotFound
        {
            tracing::debug!("cannot delete `{}`: {e}", self.path.display());
        }
    }
}

/// The lock file of the entry `id`: `<kind>/<key>.lock`.
fn lock_path(root: &Path, id: &EntryId) -> PathBuf {
    root.join(id.kind).join(format!("{}.lock", id.key))
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::time::Duration;

    use serde_json::{Value, json};

    use super::*;
    use crate::cache::kind;
    use crate::ledger::Row;

    fn block_on<T>(fut: impl Future<Output = T>) -> T {
        compio::runtime::Runtime::new().unwrap().block_on(fut)
    }

    fn open(root: &Path) -> Cache {
        block_on(Cache::open(root)).unwrap()
    }

    fn id(kind: &'static str, key: &str) -> EntryId {
        EntryId {
            kind,
            key: key.to_owned(),
        }
    }

    fn request<'a>(id: &EntryId, usage: Usage, notify: &'a dyn Fn(&str)) -> Request<'a> {
        Request {
            id: id.clone(),
            usage,
            what: "testing",
            notify,
        }
    }

    /// The slot of `id` in `cache`, which must be absent.
    fn slot(cache: &Cache, id: &EntryId, usage: Usage) -> Slot {
        let acquired = block_on(cache.acquire(&request(id, usage, &|_| {}), |_, _: Value| Ok(())));
        let Acquired::Build(slot) = acquired.unwrap() else {
            panic!("an absent entry is ready");
        };
        slot
    }

    /// Acquire `id` in `cache`, which must be absent, and publish `files`
    /// for it with `record`.
    fn build(cache: &Cache, id: &EntryId, usage: Usage, files: &[(&str, &str)], record: &Value) {
        let slot = slot(cache, id, usage);
        let staging = slot.stage().unwrap();
        for (name, content) in files {
            let path = staging.path().join(name);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, content).unwrap();
        }
        let dir = block_on(slot.publish(staging, record)).unwrap();
        assert_eq!(dir, id.dir(cache.root()));
    }

    /// The names this session pins.
    fn pins(cache: &Cache) -> Vec<String> {
        let prefix = format!("pin/{}/", cache.session().id());
        let rows = block_on(cache.session().transact(|txn, _| txn.scan(&prefix))).unwrap();
        rows.into_iter()
            .map(|(key, _)| key[prefix.len()..].to_owned())
            .collect()
    }

    fn row(cache: &Cache, id: &EntryId) -> Row {
        let key = format!("entry/{id}");
        let raw = block_on(cache.session().transact(|txn, _| txn.get(&key)));
        serde_json::from_slice(&raw.unwrap().unwrap()).unwrap()
    }

    fn lock_is_free(cache: &Cache, id: &EntryId) -> bool {
        FileLock::try_lock(&lock_path(cache.root(), id))
            .unwrap()
            .is_some()
    }

    #[test]
    fn a_live_entry_is_used_with_its_record_once_the_lock_is_released() {
        let tmp = tempfile::tempdir().unwrap();
        let (builder, reader) = (open(tmp.path()), open(tmp.path()));
        let e = id(kind::RT, "e");
        build(
            &builder,
            &e,
            Usage::InPlace,
            &[("f", "built")],
            &json!({"n": 1}),
        );
        assert!(lock_is_free(&builder, &e));

        let acquired = block_on(reader.acquire(
            &request(&e, Usage::InPlace, &|_| panic!("nothing to wait for")),
            |dir, record: Value| {
                assert!(lock_is_free(&reader, &e), "used under the lock");
                Ok((std::fs::read_to_string(dir.join("f")).unwrap(), record))
            },
        ));
        let Acquired::Ready((content, record)) = acquired.unwrap() else {
            panic!("a live entry is built again");
        };
        assert_eq!((content.as_str(), record), ("built", json!({"n": 1})));
        assert!(lock_is_free(&reader, &e));
    }

    /// The slot holds the lock: a second acquirer waits, says so once
    /// after a second, and then uses what the first one published.
    #[test]
    fn a_second_acquirer_waits_for_the_build_and_says_so_once() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = open(tmp.path());
        let e = id(kind::DEPS, "e");
        let slot = slot(&cache, &e, Usage::Copied);

        let (tx, rx) = std::sync::mpsc::channel();
        let waiter = {
            let (root, e) = (tmp.path().to_owned(), e.clone());
            std::thread::spawn(move || {
                let cache = open(&root);
                let messages = RefCell::new(Vec::new());
                let notify = |message: &str| {
                    messages.borrow_mut().push(message.to_owned());
                    tx.send(()).unwrap();
                };
                let acquired = block_on(cache.acquire(
                    &request(&e, Usage::Copied, &notify),
                    |dir, record: Value| Ok((dir.join("f").is_file(), record)),
                ));
                let Acquired::Ready(found) = acquired.unwrap() else {
                    panic!("the waiter built the entry again");
                };
                (found, messages.into_inner())
            })
        };
        let start = std::time::Instant::now();
        rx.recv_timeout(Duration::from_secs(30)).unwrap();
        assert!(start.elapsed() >= Duration::from_millis(900));
        let staging = slot.stage().unwrap();
        std::fs::write(staging.path().join("f"), "built").unwrap();
        assert!(!waiter.is_finished());
        block_on(slot.publish(staging, &json!("record"))).unwrap();

        let (found, messages) = waiter.join().unwrap();
        assert_eq!(found, (true, json!("record")));
        assert_eq!(messages.len(), 1, "{messages:#?}");
        assert!(messages[0].contains("waiting for another rene to finish testing"));
        assert!(messages[0].contains("e.lock"), "{}", messages[0]);
    }

    #[test]
    fn an_abandoned_slot_leaves_no_staging_and_frees_the_lock() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = open(tmp.path());
        let e = id(kind::RT, "e");
        let slot = slot(&cache, &e, Usage::InPlace);
        let staging = slot.stage().unwrap();
        let path = staging.path().to_owned();
        std::fs::create_dir(path.join("half")).unwrap();
        assert!(!lock_is_free(&cache, &e));
        drop(staging);
        drop(slot);
        assert!(!path.exists());
        assert!(lock_is_free(&cache, &e));
        let names: Vec<_> = std::fs::read_dir(tmp.path().join(kind::RT))
            .unwrap()
            .map(|entry| entry.unwrap().file_name())
            .collect();
        assert_eq!(names, ["e.lock"]);
        // Nothing was published, so the next acquirer builds.
        let again =
            block_on(cache.acquire(&request(&e, Usage::InPlace, &|_| {}), |_, _: Value| Ok(())));
        assert!(matches!(again.unwrap(), Acquired::Build(_)));
    }

    #[test]
    fn publication_records_the_measured_size() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = open(tmp.path());
        assert_eq!(cache.total_published(), None);
        let (a, b) = (id(kind::RT, "a"), id(kind::DEPS, "b"));
        build(
            &cache,
            &a,
            Usage::InPlace,
            &[("x", "12345"), ("sub/y", "123")],
            &json!(null),
        );
        assert_eq!(row(&cache, &a).size, 8);
        assert_eq!(cache.total_published(), Some(8));
        build(&cache, &b, Usage::Copied, &[("z", "1234")], &json!(null));
        assert_eq!(row(&cache, &b).size, 4);
        assert_eq!(cache.total_published(), Some(12));
    }

    #[test]
    fn copied_entries_are_unpinned_and_entries_read_in_place_stay_pinned() {
        let tmp = tempfile::tempdir().unwrap();
        let (builder, reader) = (open(tmp.path()), open(tmp.path()));
        let (source, copied) = (id(kind::CORE_SRC, "s"), id(kind::DEPS, "c"));
        build(
            &builder,
            &source,
            Usage::InPlace,
            &[("f", "")],
            &json!(null),
        );
        build(&builder, &copied, Usage::Copied, &[("f", "")], &json!(null));
        assert_eq!(pins(&builder), ["core-src/s"]);

        for (entry, usage) in [(&source, Usage::InPlace), (&copied, Usage::Copied)] {
            let acquired =
                block_on(reader.acquire(&request(entry, usage, &|_| {}), |_, _: Value| Ok(())));
            assert!(matches!(acquired.unwrap(), Acquired::Ready(())));
        }
        assert_eq!(pins(&reader), ["core-src/s"]);
    }
}
