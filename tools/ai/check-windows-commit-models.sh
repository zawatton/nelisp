#!/usr/bin/env bash
# Check the production-helper model and both genuine behavioral mutations.
set -eu
ulimit -v 8388608
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
out="${NDTW_COMMIT_MODEL_OUT:-$repo_root/target/ai/windows-commit-models}"
emacs_bin="${NDTW_EMACS_BIN:-emacs}"
mkdir -p "$out"
for mode in none premature per-object; do
  set +e
  if test "$mode" = none; then
    env -u NDTW_COMMIT_MUTATION timeout 60 "$emacs_bin" -Q --batch \
      -l "$repo_root/test/standalone-windows-chunk-commit-check.el" \
      > "$out/$mode.log" 2>&1
  else
    NDTW_COMMIT_MUTATION="$mode" timeout 60 "$emacs_bin" -Q --batch \
      -l "$repo_root/test/standalone-windows-chunk-commit-check.el" \
      > "$out/$mode.log" 2>&1
  fi
  status=$?
  set -e
  if test "$mode" = none; then
    test "$status" -eq 0
    rg -q 'COMMIT-CACHE-PASS tests=[1-9][0-9]* mutation=none' "$out/$mode.log"
  else
    test "$status" -eq 1
    rg -q "COMMIT-CACHE-FAIL mutation=$mode failed=[1-9][0-9]* of tests=[1-9][0-9]*" "$out/$mode.log"
  fi
  printf 'COMMIT-MODEL-CONTROL-PASS mode=%s exit=%s\n' "$mode" "$status"
done
