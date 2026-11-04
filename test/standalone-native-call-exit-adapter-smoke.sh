#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
binary="${1:-$repo_root/target/nelisp}"
native_load_path_args=()
if [[ -n "${NELISP_NATIVE_LOAD_DIR:-}" ]]; then
  native_load_path_args=(-L "$NELISP_NATIVE_LOAD_DIR")
fi
if [[ -n "${NELISP_EXPECTED_SHA256:-}" ]]; then
  actual_hash=$(sha256sum "$binary" | cut -d ' ' -f 1)
  [[ "$actual_hash" == "$NELISP_EXPECTED_SHA256" ]] || {
    printf 'reader hash mismatch: expected %s, got %s\n' \
      "$NELISP_EXPECTED_SHA256" "$actual_hash" >&2
    exit 1
  }
fi

actual=$("$binary" \
  "${native_load_path_args[@]}" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$repo_root/test/standalone-native-call-exit-adapter-driver.el" \
  --eval '(princ (prin1-to-string (nelisp-test-native-call-exit-adapter-smoke)))')

if [[ "$actual" != t ]]; then
  printf 'native call exit adapter smoke expected t, got %s\n' "$actual" >&2
  exit 1
fi

printf 'native-call-exit-adapter: PASS (runtime contract, current-cell lookup/redefinition, normal/signal/throw payload identity across GC, exactly-once cleanup, post-exit normal call, malformed index, stale ticket, red mutation)\n'
