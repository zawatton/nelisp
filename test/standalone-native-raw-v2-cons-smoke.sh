#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
binary="${NELISP_BIN:?Set NELISP_BIN to the dedicated runtime-reload reader}"
binary="$(realpath "$binary")"
expected_binary_sha="${NELISP_EXPECTED_BINARY_SHA256:?Set the expected reader SHA256}"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/nelisp-raw-v2-cons.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT

binary_sha="$(sha256sum "$binary" | awk '{print $1}')"
[[ "$binary_sha" == "$expected_binary_sha" ]]

export NELISP_CONS_REPO_ROOT="$repo_root"
export NELISP_CONS_SOURCE="$tmpdir/cons-probe.el"
export NELISP_CONS_ARTIFACT="$tmpdir/cons-probe.nelr"
export NELISP_CONS_BINARY_SHA="$binary_sha"

emacs --batch -Q \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  --load "$script_dir/nelisp-native-raw-v2-cons-build.el" \
  >/dev/null

set +e
actual=$("$binary" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$script_dir/nelisp-native-raw-v2-cons-driver.el" 2>"$tmpdir/stderr")
status=$?
set -e

if [[ "$status" -ne 0 ]]; then
    cat "$tmpdir/stderr" >&2
    printf 'raw-v2 CONS driver exited %d; stdout was %s\n' "$status" "$actual" >&2
    exit "$status"
fi
if [[ "$actual" != "t" ]]; then
    printf 'raw-v2 CONS smoke expected t, got %s\n' "$actual" >&2
    exit 1
fi
printf '%s\n' 'raw-v2-cons-import: PASS (machine ELF CONS gateway, evaluator identity through GC, mutation visibility, malformed-index and import-mutation refusals)'
