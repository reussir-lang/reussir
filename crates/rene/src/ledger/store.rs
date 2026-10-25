//! One ledger transaction: open, load, operate, commit, close.
//!
//! TurboKV runs on Tokio while rene runs on compio, so the store keeps a
//! private current-thread runtime that runs only inside its calls, as the
//! build directory's database does ([`crate::db`]). Every call blocks on it
//! briefly; the only wait that can be long, for another session's
//! transaction, polls on compio instead ([`super::Session`]).

use std::path::{Path, PathBuf};

use tokio::runtime::Runtime;
use turbokv::{Db, DbError, DbOptions};

use super::LEDGER_DIR;
use super::view::View;

pub(super) struct Store {
    path: PathBuf,
    runtime: Runtime,
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

    /// Run `op` on the ledger `db`, opened by [`try_open`](Self::try_open):
    /// load every row, run `op`, commit what it changed in one batch, and
    /// close.
    pub(super) fn run<T>(&self, db: Db, op: impl FnOnce(&mut View) -> T) -> Result<T, String> {
        let result = (|| {
            let mut view = View::load(&self.runtime, &db, &self.path)?;
            let value = op(&mut view);
            if !view.batch.is_empty() {
                self.runtime
                    .block_on(db.write_batch(&view.batch))
                    .map_err(|e| {
                        format!(
                            "cannot write the cache ledger `{}`: {e}",
                            self.path.display()
                        )
                    })?;
            }
            Ok(value)
        })();
        if let Err(e) = self.runtime.block_on(db.close()) {
            tracing::debug!("cannot close the cache ledger: {e}");
        }
        result
    }
}
