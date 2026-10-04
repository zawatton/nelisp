#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-bc-public-branch.XXXXXX")
branch_artifact="$artifact_dir/packed-branch.neln"
join_artifact="$artifact_dir/packed-join.neln"
call_artifact="$artifact_dir/unsupported-call.neln"
bad_artifact="$artifact_dir/malformed.neln"
attestation="$artifact_dir/verified-dialect.el"
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-native-public-branch: missing executable: $binary" >&2
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
  NELISP_PUBLIC_BRANCH_ARTIFACT="$branch_artifact" \
  NELISP_PUBLIC_JOIN_ARTIFACT="$join_artifact" \
  NELISP_PUBLIC_CALL_ARTIFACT="$call_artifact" \
  NELISP_PUBLIC_BAD_ARTIFACT="$bad_artifact" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
    --load "$attestation" \
    --load "$repo_root/test/standalone-bytecode-native-public-branch-in-runtime.el" \
    --eval '(princ (prin1-to-string (nelisp-test-public-packed-branch-entry)))')

if [[ "$actual" != t ]]; then
  printf 'standalone-bytecode-native-public-branch: expected t, got %s\n' \
    "$actual" >&2
  exit 1
fi
echo "standalone-bytecode-native-public-branch: PASS (build-verified dialect, public packed 257 compile, ELF load/call, both branch and join arms, VM/native identity across GC, effect/call and malformed refusals)"
