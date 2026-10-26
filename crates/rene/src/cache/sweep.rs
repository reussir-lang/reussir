//! The sweep: deleting what ended sessions left outside the ledger
//! (docs/design/cache-ledger.md, "Pruning, repair and sweeping").
//!
//! A session names the directories it builds in and renames out after
//! itself ([`Session::sibling`](crate::ledger::Session::sibling)), and
//! nothing but that session touches them while it lives. So once the
//! session has ended, its lock free or gone, they are leftovers, and the
//! sweep decides by liveness alone: no transaction, no ages, and concurrent
//! sweeps are harmless. It also deletes free lock files nobody needs: those
//! of entries that are not there, and those of ended sessions, each taken
//! first, as the file locks' contract requires.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use crate::ledger::{EntryId, SESSIONS_DIR, alive, parse_sibling};
use crate::lock::{FileLock, Probe};

/// What a [`sweep`] deleted.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Swept {
    /// Staging and trash directories of ended sessions.
    pub dirs: usize,
    /// Free lock files of entries that are not there, and of ended
    /// sessions.
    pub locks: usize,
}

/// Sweep the cache at `root`: in `sessions/` and the directories of
/// `kinds`, delete what ended sessions left (see the module docs). Names
/// the sweep does not know are left alone, and a failure to delete one
/// only waits for the next sweep.
pub fn sweep(root: &Path, kinds: &[&'static str]) -> Swept {
    let mut swept = Swept::default();
    for (name, path) in names(&root.join(SESSIONS_DIR)) {
        if name.ends_with(".lock") && remove_free(&path) {
            swept.locks += 1;
        }
    }
    let mut live = BTreeMap::new();
    for kind in kinds {
        for (name, path) in names(&root.join(kind)) {
            if let Some((_, _, sid)) = parse_sibling(&name) {
                let owner_lives = *live
                    .entry(sid.to_owned())
                    .or_insert_with(|| alive(root, sid));
                if !owner_lives && delete(&path) {
                    swept.dirs += 1;
                }
            } else if let Some(key) = name.strip_suffix(".lock")
                && let Some(id) = EntryId::parse(&format!("{kind}/{key}"), kinds)
                && std::fs::symlink_metadata(id.dir(root)).is_err()
                && remove_free(&path)
            {
                swept.locks += 1;
            }
        }
    }
    swept
}

/// The names in `dir` that are UTF-8, with their paths; none if it cannot
/// be listed.
fn names(dir: &Path) -> Vec<(String, PathBuf)> {
    let Ok(names) = std::fs::read_dir(dir) else {
        return Vec::new();
    };
    let names = names.flatten().map(|name| (name.file_name(), name.path()));
    let names = names.filter_map(|(name, path)| Some((name.into_string().ok()?, path)));
    names.collect()
}

/// Delete the lock file at `path`, taking it first, if nobody holds it.
/// Whether it went.
fn remove_free(path: &Path) -> bool {
    let Ok(Probe::Free(lock)) = FileLock::probe(path) else {
        return false;
    };
    reported(path, lock.remove())
}

/// Delete the directory tree, or the file, at `path`. Whether it went.
fn delete(path: &Path) -> bool {
    let deleted = match std::fs::symlink_metadata(path) {
        Ok(meta) if meta.is_dir() => std::fs::remove_dir_all(path),
        _ => std::fs::remove_file(path),
    };
    reported(path, deleted)
}

fn reported(path: &Path, deleted: std::io::Result<()>) -> bool {
    let failed = |e: &std::io::Error| tracing::debug!("cannot delete `{}`: {e}", path.display());
    deleted.inspect_err(failed).is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::{Session, Tag};

    fn block_on<T>(fut: impl Future<Output = T>) -> T {
        compio::runtime::Runtime::new().unwrap().block_on(fut)
    }

    fn names_in(dir: &Path) -> Vec<String> {
        let mut names: Vec<String> = names(dir).into_iter().map(|(name, _)| name).collect();
        names.sort();
        names
    }

    #[test]
    fn the_sweep_deletes_what_ended_sessions_left_and_nothing_else() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();
        let [live, dead] = [(); 2].map(|()| block_on(Session::start(root)).unwrap());
        let deps = root.join("deps");
        let in_deps = |name: &str| deps.join(name);
        // A live session's staging and an ended one's trash, which still
        // has its lock file, and staging of one whose lock file is gone.
        let staging = live.sibling(&in_deps("a"), Tag::Stage);
        let trash = dead.sibling(&in_deps("a"), Tag::Trash);
        let gone = in_deps("b.stage-0123456789abcdef-0");
        for dir in [&staging, &trash.join("sub"), &gone] {
            std::fs::create_dir_all(dir).unwrap();
        }
        let dead_lock = root.join(SESSIONS_DIR).join(format!("{}.lock", dead.id()));
        dead.crash();
        // An entry and its lock, an orphaned lock, a held one, and names
        // the sweep does not know.
        std::fs::create_dir(in_deps("entry")).unwrap();
        for name in ["entry.lock", "orphan.lock", "held.lock", "x.y.lock"] {
            std::fs::write(in_deps(name), "").unwrap();
        }
        let held = FileLock::try_lock(&in_deps("held.lock")).unwrap().unwrap();
        std::fs::write(root.join(SESSIONS_DIR).join("notes"), "").unwrap();

        let swept = sweep(root, &["deps", "rt"]);
        assert_eq!(swept, Swept { dirs: 2, locks: 2 });
        let staged = staging.file_name().unwrap().to_str().unwrap();
        let mut kept = vec!["entry", "entry.lock", "held.lock", "x.y.lock", staged];
        kept.sort();
        assert_eq!(names_in(&deps), kept);
        assert!(!dead_lock.exists());
        let live_lock = format!("{}.lock", live.id());
        assert_eq!(
            names_in(&root.join(SESSIONS_DIR)),
            [live_lock.as_str(), "notes"]
        );
        // Once free, a lock without its entry goes too.
        drop(held);
        assert_eq!(sweep(root, &["deps"]), Swept { dirs: 0, locks: 1 });
    }
}
