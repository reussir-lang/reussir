#!/usr/bin/env bash
# Download the pinned prebuilt PJRT CPU plugin for this runner and verify its
# digest. Building XLA is far beyond CI's budget; the plugin only needs to
# match the C API header the runtime is compiled against, which comes from
# the same pinned XLA revision.
#
# Usage: fetch-pjrt-cpu-plugin.sh <directory>
# Prints nothing on success; the plugin lands at <directory>/libpjrt_cpu.so.
set -euo pipefail

dest=${1:?usage: fetch-pjrt-cpu-plugin.sh <directory>}
release=https://github.com/zml/pjrt-artifacts/releases/download/manual-2026-09-08T13-23-00Z

case "$(uname -m)" in
  x86_64)
    asset=pjrt-cpu_linux-amd64.tar.gz
    sha256=f99324db6146208672f5b1d420333d0ea26e3968d7abfa83a802b40276f855d3
    ;;
  aarch64)
    asset=pjrt-cpu_linux-arm64.tar.gz
    sha256=06884bda9f029999a58fb5f9cbe207c436c39ecaf7ef904600f6b3e92aba2926
    ;;
  *)
    echo "no pinned PJRT CPU plugin for $(uname -m)" >&2
    exit 1
    ;;
esac

mkdir -p "$dest"
curl --fail --silent --show-error --location --retry 3 \
  --output "$dest/$asset" "$release/$asset"
echo "$sha256  $dest/$asset" | sha256sum --check --quiet
tar -xzf "$dest/$asset" -C "$dest"
rm "$dest/$asset"
test -f "$dest/libpjrt_cpu.so"
