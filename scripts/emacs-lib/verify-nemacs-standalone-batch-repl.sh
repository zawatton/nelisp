#!/usr/bin/env bash
set -euo pipefail

repo_root="${REPO_ROOT:-$(pwd)}"
nelisp_bin="${NELISP_BIN:-$repo_root/vendor/nelisp/target/nelisp}"
nelisp_root="${NELISP_ROOT:-$(dirname "$(dirname "$nelisp_bin")")}"
bootstrap_repl="${NEMACS_BOOTSTRAP_REPL:-$repo_root/build/nemacs-bootstrap.repl}"

tmp="$(mktemp "${TMPDIR:-/tmp}/nemacs-standalone-batch.XXXXXX.repl")"
out="$(mktemp "${TMPDIR:-/tmp}/nemacs-standalone-batch.XXXXXX.out")"
trap 'rm -f "$tmp" "$out"' EXIT

append_eval_source_file() {
  local file="$1"
  python3 - "$file" <<'PY'
import pathlib
import sys

text = pathlib.Path(sys.argv[1]).read_text(encoding="utf-8")
escaped = (
    text
    .replace("\\", "\\\\")
    .replace('"', '\\"')
    .replace("\n", "\\n")
    .replace("\r", "\\r")
)
print(f'(nelisp--eval-source-string "{escaped}")')
PY
}

if [[ ! -x "$nelisp_bin" ]]; then
  echo "verify-nemacs-standalone-batch: nelisp binary is not executable: $nelisp_bin" >&2
  exit 1
fi

if [[ ! -r "$bootstrap_repl" ]]; then
  echo "verify-nemacs-standalone-batch: bootstrap REPL input is not readable: $bootstrap_repl" >&2
  exit 1
fi

: > "$tmp"
prelude="$nelisp_root/scripts/nelisp-stdlib-prelude.el"
if [[ -r "$prelude" ]]; then
  append_eval_source_file "$prelude" >> "$tmp"
fi
printf '%s\n' "(unless (boundp 'load-file-name) (defvar load-file-name nil))" >> "$tmp"
printf '%s\n' "(unless (boundp 'buffer-file-name) (defvar buffer-file-name nil))" >> "$tmp"
printf '%s\n' "(unless (boundp 'default-directory) (defvar default-directory \"$repo_root/\"))" >> "$tmp"
printf '%s\n' "(setq native-comp-enable-subr-trampolines nil comp-enable-subr-trampolines nil)" >> "$tmp"
printf '%s\n' "(unless (boundp 'load-path) (defvar load-path nil))" >> "$tmp"
printf '%s\n' "(setq nelisp-emacs-vendor-root \"$repo_root/vendor\")" >> "$tmp"
printf '%s\n' "(setq load-path (list \"$repo_root/src\" \"$repo_root/vendor/emacs-lisp\" \"$repo_root/vendor/emacs-lisp/emacs-lisp\"))" >> "$tmp"
cat "$bootstrap_repl" >> "$tmp"
printf '\n' >> "$tmp"
printf '%s\n' "(setq init-file-user nil package-enable-at-startup nil)" >> "$tmp"
printf '%s\n' "(load \"$repo_root/src/emacs-fns.el\" nil 'no-message)" >> "$tmp"
printf '%s\n' "(load \"$repo_root/src/nemacs-main.el\" nil 'no-message)" >> "$tmp"
printf '%s\n' "(require 'emacs-cl-macros)" >> "$tmp"
printf '%s\n' "(require 'nemacs-main)" >> "$tmp"
printf '%s\n' "(setq nemacs-main-options (list :batch t :no-banner t :driver 'nelisp :images nil :load-path nil :load nil :eval-forms (list \"(if (and (fboundp (quote nemacs-batch-main)) (featurep (quote nemacs-main))) (nelisp--write-stdout-bytes \\\"NEMACS-STANDALONE-BOOT=ok\\\\n\\\") (nelisp--write-stdout-bytes \\\"NEMACS-STANDALONE-BOOT=fail\\\\n\\\"))\") :funcall nil :args nil))" >> "$tmp"
printf '%s\n' "(when (fboundp 'install-sigint-handler) (install-sigint-handler))" >> "$tmp"
printf '%s\n' "(unless nemacs-initialized (nemacs-init t))" >> "$tmp"
printf '%s\n' "(nemacs-main--apply-options)" >> "$tmp"
printf '%s\n' "0" >> "$tmp"

set +e
"$nelisp_bin" --repl --no-prompt --no-print < "$tmp" > "$out" 2>&1
rc=$?
set -e

cat "$out"

if [[ "$rc" -ne 0 ]]; then
  echo "NEMACS-STANDALONE-BOOT=fail exit=$rc" >&2
  exit "$rc"
fi

grep -q '^NEMACS-STANDALONE-BOOT=ok$' "$out"

if grep -q 'uncaught error:' "$out"; then
  echo "NEMACS-STANDALONE-BOOT=fail uncaught-error-detected" >&2
  exit 1
fi
