#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-bc-public-runtime.XXXXXX")
artifact="$artifact_dir/materialized-argument.neln"
bad_artifact="$artifact_dir/malformed.neln"
mismatch_artifact="$artifact_dir/mismatched-dialect.neln"
attestation="$artifact_dir/verified-dialect.el"
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-native-compiler-in-runtime: missing executable: $binary" >&2
  exit 2
fi

NELISP_REPO_ROOT="$repo_root" NELISP_DIALECT_ATTESTATION="$attestation" \
  emacs --batch -Q \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  --eval '(progn
            (load (expand-file-name "scripts/nelisp-standalone-build.el"
                                    (getenv "NELISP_REPO_ROOT")) nil nil t)
            (unless nelisp-standalone--verified-bytecode-dialect-id
              (error "host bytecode identity is not pinned"))
            (with-temp-file (getenv "NELISP_DIALECT_ATTESTATION")
              (prin1 (list (quote defconst)
                           (quote nelisp-bytecode-runtime-dialect-id)
                           nelisp-standalone--verified-bytecode-dialect-id)
                     (current-buffer))
              (insert "\n")))' \
  2>"$artifact_dir/host-gate.log"
echo "standalone dialect attestation SHA256: $(sha256sum "$attestation" | cut -d' ' -f1)"

actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_BC_ENTRY_ARTIFACT="$artifact" \
  NELISP_BC_ENTRY_BAD_ARTIFACT="$bad_artifact" \
  NELISP_BC_ENTRY_MISMATCH_ARTIFACT="$mismatch_artifact" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
    --load "$attestation" \
    --load "$repo_root/test/standalone-bytecode-native-compiler-in-runtime.el" \
    --eval '(princ (prin1-to-string (nelisp-test-public-bytecode-native-entry)))')

if [[ "$actual" != t ]]; then
  printf 'standalone-bytecode-native-compiler-in-runtime: expected t, got %s\n' \
    "$actual" >&2
  exit 1
fi
echo "standalone-bytecode-native-compiler-in-runtime: PASS (build-verified dialect, materialized required arg, .neln load/call, VM/native eq across GC, malformed and mutated identity refusals)"
