//! Evaluating the Nickel programs rene reads: package manifests
//! ([`crate::manifest`]) and the per-user configuration
//! ([`crate::config`]).
//!
//! Both are full Nickel programs, not data files: they may import other
//! files, apply contracts, and merge records, and rene sees only the fully
//! evaluated value. Errors carry Nickel's diagnostics rendered as plain
//! text (no ANSI: they end up in logs and test output).

use std::path::Path;

use nickel_lang::{Context, ErrorFormat, Expr};
use serde::de::DeserializeOwned;

/// A program evaluated for export.
pub struct Evaluated {
    ctx: Context,
    expr: Expr,
}

/// Evaluate `source`, the contents of the file at `path`, for export.
///
/// The source name is the real path, so `import` resolves relative to the
/// file and diagnostics point at it; the file's directory is also an
/// explicit import root as a fallback. Evaluating for export honors
/// `not_exported`, letting a file keep private helper fields out of the
/// value it presents.
pub fn evaluate(path: &Path, source: &str) -> Result<Evaluated, String> {
    let mut ctx = Context::new().with_source_name(path.display().to_string());
    if let Some(dir) = path.parent().filter(|d| !d.as_os_str().is_empty()) {
        ctx = ctx.with_added_import_paths(vec![dir.as_os_str().to_owned()]);
    }
    let expr = ctx.eval_deep_for_export(source).map_err(|e| render(&e))?;
    Ok(Evaluated { ctx, expr })
}

impl Evaluated {
    /// The value as compact JSON.
    pub fn to_json(&self) -> Result<String, String> {
        self.ctx.expr_to_json(&self.expr).map_err(|e| render(&e))
    }

    /// The value as a `T`.
    pub fn to_serde<T: DeserializeOwned>(&self) -> Result<T, String> {
        self.expr.to_serde().map_err(|e| e.to_string())
    }
}

/// Render a Nickel error's diagnostics as plain text.
fn render(error: &nickel_lang::Error) -> String {
    let mut out = Vec::new();
    match error.format(&mut out, ErrorFormat::Text) {
        Ok(()) => String::from_utf8_lossy(&out).trim_end().to_owned(),
        Err(e) => format!("(diagnostics unavailable: {e})"),
    }
}
