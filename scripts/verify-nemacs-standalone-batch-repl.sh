#!/usr/bin/env bash
set -euo pipefail

repo_root="${REPO_ROOT:-$(pwd)}"
nelisp_bin="${NELISP_BIN:-$repo_root/target/nelisp}"
bootstrap_repl="${NEMACS_BOOTSTRAP_REPL:-$repo_root/build/nemacs-bootstrap.repl}"
out="$(mktemp "${TMPDIR:-/tmp}/nemacs-standalone-batch.XXXXXX.out")"
trap 'rm -f "$out"' EXIT

test -x "$nelisp_bin"
test -r "$bootstrap_repl"
status=0
NEMACS_NELISP="$nelisp_bin" NELISP_HOME="${NELISP_ROOT:-$(dirname "$(dirname "$nelisp_bin")")}" \
  NEMACS_BOOTSTRAP_REPL="$bootstrap_repl" NEMACS_DISABLE_COLD_CACHE=1 \
  "$repo_root/bin/emacs-lib/nemacs" --driver=nelisp --batch --no-banner \
  --eval '(when (and (featurep (quote nemacs-main)) command-line-processed)
            (princ "NEMACS-STANDALONE-BOOT=ok\n"))' >"$out" 2>&1 || status=$?
cat "$out"
if ((status)); then exit "$status"; fi
grep -q '^NEMACS-STANDALONE-BOOT=ok$' "$out"
! grep -q 'uncaught error:' "$out"
