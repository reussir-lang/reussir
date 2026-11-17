#!/usr/bin/env bash
# RUSTC_WRAPPER for CI: sccache, except for outputs that are not worth a
# place in the shared bucket.
#
#   - the `rene` crate embeds the runtime and library bundles, which are
#     already compressed, so each build stores some 90 MB and a change to
#     the library or the runtime stores it again for every platform;
#   - a static library is a link of the crate with all its dependencies.
set -euo pipefail

prev=
for arg in "$@"; do
  case "$prev $arg" in
    "--crate-name rene" | "--crate-type staticlib") exec "$@" ;;
  esac
  prev=$arg
done
exec sccache "$@"
