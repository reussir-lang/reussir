//! `rene`: the Reussir package manager.
//!
//! A package is a directory with a `rene.ncl` manifest — a Nickel program
//! evaluating to the package description (name, dependencies, …) — and
//! Reussir sources. Targets may name independent crate-root files, falling
//! back to `src/lib.rr` when no path is given. Builds run in a build directory
//! (`reussir-build/` by default) whose status lives in a TurboKV
//! database: the baked bundled `reussir-rt` runtime ([`rt`]), the package's
//! source graph as `rrc --scan-deps` reported it ([`deps`]), and the built
//! products of the manifest's declared targets ([`compile`]). Dependencies
//! resolve over local paths ([`resolve`]), and `rene new` scaffolds a fresh
//! package ([`new`]). What every build has in common, such as the sources of
//! the libraries bundled inside rene ([`bundled`]), lives once per user in
//! the [`cache`].

pub mod bundled;
pub mod cache;
pub mod compile;
pub mod db;
pub mod deps;
pub mod exec;
pub mod fresh;
pub mod ledger;
pub mod lock;
pub mod manifest;
pub mod new;
pub mod plan;
pub mod pool;
pub mod resolve;
pub mod rt;
pub mod tables;
