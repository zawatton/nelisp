#!/usr/bin/env bash
set -euo pipefail

repo_root="${REPO_ROOT:-$(pwd)}"
nelisp_bin="${NELISP_BIN:-$repo_root/vendor/nelisp/target/nelisp}"
nelisp_root="${NELISP_ROOT:-$(dirname "$(dirname "$nelisp_bin")")}"
bootstrap_repl="${NEMACS_BOOTSTRAP_REPL:-$repo_root/build/nemacs-bootstrap.repl}"

python3 "$repo_root/scripts/probe-vendor-class-a-standalone.py" \
  --repo-root "$repo_root" \
  --nelisp-bin "$nelisp_bin" \
  --nelisp-root "$nelisp_root" \
  --bootstrap-repl "$bootstrap_repl" \
  "$@"
