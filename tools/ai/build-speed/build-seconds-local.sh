#!/usr/bin/env bash
# B1.2 of tools/ai/build-speed-progress.org while CI is disabled: the
# compiled-toolchain cold build that run-heavy.sh build-identity timed on
# the current HEAD (empty unit cache, empty toolchain .elc directory) took
# at most MAX seconds.
# Usage: build-seconds-local.sh MAX
set -u
max=${1:?usage: build-seconds-local.sh MAX}
root=$(git rev-parse --show-toplevel) || exit 2
bash "$root/tools/ai/main-ci-reds/check-heavy.sh" build-identity ||
  { echo "no passing build-identity result for HEAD"; exit 1; }
secs=$(sed -n 's/^compiled: \([0-9]*\) s$/\1/p' "$root/target/main-ci-reds/build-identity.log")
[ -n "$secs" ] || { echo "compiled build time not found"; exit 1; }
echo "cold reader build, compiled toolchain: ${secs}s (limit ${max}s)"
[ "$secs" -le "$max" ]
