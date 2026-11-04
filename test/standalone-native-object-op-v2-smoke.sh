#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
binary="${NELISP_BINARY:-${1:-}}"
[[ -n "$binary" && -x "$binary" ]] || { printf 'Set NELISP_BINARY to the dedicated v2 reader\n' >&2; exit 2; }
binary="$(realpath "$binary")"
binary_sha="$(sha256sum "$binary" | awk '{print $1}')"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/nelisp-object-op-v2.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT

export NELISP_OBJECT_OP_REPO_ROOT="$repo_root"
export NELISP_OBJECT_OP_SOURCE="$tmpdir/object-op-probe.el"
export NELISP_OBJECT_OP_ARTIFACT="$tmpdir/object-op-probe.nelr"
export NELISP_OBJECT_OP_BINARY_SHA="$binary_sha"

emacs --batch -Q \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  --load "$script_dir/nelisp-native-raw-v2-object-op-build.el" >/dev/null

actual=$("$binary" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$script_dir/nelisp-native-raw-v2-object-op-driver.el")
[[ "$actual" == t ]] || { printf 'object-op smoke expected t, got %s\n' "$actual" >&2; exit 1; }
printf '%s\n' 'native-object-op-v2: PASS (machine ELF CAR/CDR dispatch, exact manifest, identity through GC, nil, wrong-type, no output mutation)'
