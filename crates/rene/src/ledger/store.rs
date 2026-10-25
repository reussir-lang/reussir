//! One ledger transaction: open, load, reap, operate, commit, close.
//!
//! TurboKV runs on Tokio while rene runs on compio, so the store keeps a
//! private current-thread runtime that runs only inside its calls, as the
//! build directory's database does ([`crate::db`]). Every call blocks on it
//! briefly; the only wait that can be long, for another session's
//! transaction, polls on compio instead ([`super::Session`]).
//!
//! A transaction never deletes a directory tree while it holds the ledger.
//! A directory leaving the cache is renamed to a trash sibling inside the
//! transaction ([`View::discard`]), which is atomic and cheap, and deleted
//! once the ledger is closed. If the commit fails, every such rename is
//! undone first, so the directories match the rows the ledger still holds.

#[cfg(test)]
use std::cell::Cell;
use std::path::{Path, PathBuf};

use tokio::runtime::Runtime;
use turbokv::{Db, DbError, DbOptions};

use super::LEDGER_DIR;
use super::view::{Move, View};
use crate::cache::Cache;

#[derive(Debug)]
pub(super) struct Store {
    path: PathBuf,
    runtime: Runtime,
    /// Make the next commit fail, as a full disk would.
    #[cfg(test)]
    pub(super) fail_commit: Cell<bool>,
}

impl Store {
    /// The store of the ledger under the cache root `root`. Opens nothing.
    pub(super) fn new(root: &Path) -> Result<Store, String> {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .map_err(|e| format!("cannot start the cache ledger's runtime: {e}"))?;
        Ok(Store {
            path: root.join(LEDGER_DIR),
            runtime,
            #[cfg(test)]
            fail_commit: Cell::new(false),
        })
    }

    /// Open the ledger for one transaction, or `None` while another
    /// session has it open.
    pub(super) fn try_open(&self) -> Result<Option<Db>, String> {
        // Every batch is synced before it is acknowledged: a committed
        // transaction survives a power loss.
        match self
            .runtime
            .block_on(Db::open_with_options(&self.path, DbOptions::paranoid()))
        {
            Ok(db) => Ok(Some(db)),
            Err(DbError::DirectoryLocked { .. }) => Ok(None),
            Err(e) => Err(format!(
                "cannot open the cache ledger `{}`: {e}",
                self.path.display()
            )),
        }
    }

    /// Run `op` as session `me` on the ledger `db`, opened by
    /// [`try_open`](Self::try_open): load every row, reap the dead sessions
    /// ([`super::reap`]), run `op`, commit what it and the reaper changed in
    /// one batch, close, and delete what they discarded.
    pub(super) fn run<T>(
        &self,
        db: Db,
        cache: &Cache,
        me: &str,
        op: impl FnOnce(&mut View) -> T,
    ) -> Result<T, String> {
        let result = (|| {
            let mut view = View::load(&self.runtime, &db, &self.path)?;
            view.reap(cache, me);
            let value = op(&mut view);
            match self.commit(&db, &view) {
                Ok(()) => Ok((value, view.moves)),
                Err(e) => {
                    undo(&view.moves);
                    Err(e)
                }
            }
        })();
        if let Err(e) = self.runtime.block_on(db.close()) {
            tracing::debug!("cannot close the cache ledger: {e}");
        }
        let (value, moves) = result?;
        for Move { trash, .. } in moves {
            if let Err(e) = std::fs::remove_dir_all(&trash) {
                tracing::debug!("cannot delete `{}`: {e}", trash.display());
            }
        }
        Ok(value)
    }

    fn commit(&self, db: &Db, view: &View) -> Result<(), String> {
        let failed = |e: &dyn std::fmt::Display| {
            format!(
                "cannot write the cache ledger `{}`: {e}",
                self.path.display()
            )
        };
        #[cfg(test)]
        if self.fail_commit.take() {
            return Err(failed(&"the test made the commit fail"));
        }
        if view.batch.is_empty() {
            return Ok(());
        }
        self.runtime
            .block_on(db.write_batch(&view.batch))
            .map_err(|e| failed(&e))
    }
}

/// Put back what a transaction that failed to commit discarded, newest
/// first.
fn undo(moves: &[Move]) {
    for Move { from, trash } in moves.iter().rev() {
        if let Err(e) = std::fs::rename(trash, from) {
            tracing::warn!(
                "cannot move `{}` back to `{}`: {e}",
                trash.display(),
                from.display()
            );
        }
    }
}
