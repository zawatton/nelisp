#!/bin/sh
# Compare the measurement baseline with the blocked candidate. Override
# BOOT_IMAGE_BASELINE and BOOT_IMAGE_BIN to select other local build artifacts.
set -eu
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root"
candidate=${BOOT_IMAGE_BIN:-$root/target/boot-image/candidate4}
baseline=${BOOT_IMAGE_BASELINE:-$root/target/boot-image/base-dynamic}
test_file=test/standalone-exit-stash-root-check.el
export TMPDIR="$root/target/tmp"
run_case() {
  label=$1; shift
  code=0
  "$@" --load "$test_file" >"target/boot-image/reproduce-$label.out" 2>"target/boot-image/reproduce-$label.err" || code=$?
  printf '%s exit=%s\n' "$label" "$code"
  cat "target/boot-image/reproduce-$label.out"
  cat "target/boot-image/reproduce-$label.err" >&2
}
run_case baseline env EVAL_BIN="$baseline" "$baseline"
run_case baseline-cold env EVAL_BIN="$baseline" "$baseline" --cold-load-from "$baseline.cold"
run_case candidate-plain env -u EVAL_BIN "$candidate"
run_case candidate-env env EVAL_BIN="$candidate" "$candidate"
