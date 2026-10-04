#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-bc-production.XXXXXX")
artifact="$artifact_dir/materialized-argument.neln"
bad_artifact="$artifact_dir/malformed.neln"
mismatch_artifact="$artifact_dir/mismatched-dialect.neln"
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-native-compiler-production: missing executable: $binary" >&2
  exit 2
fi

# The check runs in the supplied standalone binary. No host-generated file or
# dialect attestation participates in the runtime proof.
marker_status=$(NELISP_REPO_ROOT="$repo_root" "$binary" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  --eval '(if (equal nelisp-bytecode-runtime-dialect-id
                     "GNU Emacs 31.1; inventory-sha256=147da590c9f5bdcf190b5b410c6af878c793ac89e07eafa4ae9f05a9b7aa7bcb")
              42 1)')
if [[ "$marker_status" != 42 ]]; then
  printf 'standalone-bytecode-native-compiler-production: embedded marker check returned %s (expected 42)\n' \
    "$marker_status" >&2
  exit 1
fi

actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_BC_ENTRY_ARTIFACT="$artifact" \
  NELISP_BC_ENTRY_BAD_ARTIFACT="$bad_artifact" \
  NELISP_BC_ENTRY_MISMATCH_ARTIFACT="$mismatch_artifact" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-compiler-in-runtime.el" \
    --eval '(princ (prin1-to-string (nelisp-test-public-bytecode-native-entry)))')

if [[ "$actual" != t ]]; then
  printf 'standalone-bytecode-native-compiler-production: expected t, got %s\n' \
    "$actual" >&2
  exit 1
fi
echo "standalone-bytecode-native-compiler-production: PASS (embedded GNU Emacs 31.1 marker, public compile/load/call, VM/native eq across GC, malformed and mismatched identity refusals)"
