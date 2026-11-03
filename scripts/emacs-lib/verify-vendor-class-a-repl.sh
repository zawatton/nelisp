#!/usr/bin/env bash
set -euo pipefail

repo_root="${REPO_ROOT:-$(pwd)}"
nelisp_bin="${NELISP_BIN:-$repo_root/vendor/nelisp/target/nelisp}"
nelisp_root="${NELISP_ROOT:-$(dirname "$(dirname "$nelisp_bin")")}"
bootstrap_repl="${NEMACS_BOOTSTRAP_REPL:-$repo_root/build/nemacs-bootstrap.repl}"
vendor_limit="${VENDOR_CLASS_A_LIMIT:-58}"
vendor_strict="${VENDOR_CLASS_A_STRICT_ELISP:-nil}"

tmp="$(mktemp "${TMPDIR:-/tmp}/nemacs-vendor-class-a.XXXXXX.repl")"
out="$(mktemp "${TMPDIR:-/tmp}/nemacs-vendor-class-a.XXXXXX.out")"
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
  echo "verify-vendor-class-a: nelisp binary is not executable: $nelisp_bin" >&2
  exit 1
fi

if [[ ! -r "$bootstrap_repl" ]]; then
  echo "verify-vendor-class-a: bootstrap REPL input is not readable: $bootstrap_repl" >&2
  exit 1
fi

: > "$tmp"
prelude="$nelisp_root/scripts/nelisp-stdlib-prelude.el"
if [[ -r "$prelude" ]]; then
  append_eval_source_file "$prelude" >> "$tmp"
fi
bootstrap_runtime_anchor="$repo_root/src/nemacs-main.el"
printf '%s\n' "(setq load-file-name \"$bootstrap_runtime_anchor\")" >> "$tmp"
printf '%s\n' "(setq buffer-file-name \"$bootstrap_runtime_anchor\")" >> "$tmp"
printf '%s\n' "(setq nelisp-emacs-vendor-root \"$repo_root/vendor\")" >> "$tmp"
printf '%s\n' "(setq load-path (list \"$repo_root/src\" \"$repo_root/scripts\" \"$repo_root/vendor/emacs-lisp\" \"$repo_root/vendor/emacs-lisp/emacs-lisp\" \"$repo_root/vendor/emacs-lisp/vc\" \"$repo_root/vendor/emacs-lisp/international\" \"$repo_root/vendor/emacs-lisp/cedet\" \"$repo_root/vendor/emacs-lisp/calendar\" \"$repo_root/vendor/emacs-lisp/calc\" \"$repo_root/vendor/emacs-lisp/erc\" \"$repo_root/vendor/emacs-lisp/eshell\"))" >> "$tmp"
cat "$bootstrap_repl" >> "$tmp"
printf '\n' >> "$tmp"
append_eval_source_file "$repo_root/scripts/vendor-class-a-smoke.el" >> "$tmp"
printf '%s\n' "(setq vendor-class-a-smoke-default-limit $vendor_limit)" >> "$tmp"
printf '%s\n' "(setq vendor-class-a-smoke-strict $vendor_strict)" >> "$tmp"
printf '%s\n' "(vendor-class-a-smoke-batch)" >> "$tmp"

set +e
"$nelisp_bin" --repl --no-prompt --no-print < "$tmp" > "$out" 2>&1
rc=$?
set -e

cat "$out"

if [[ "$rc" -ne 0 ]]; then
  echo "VENDOR-CLASS-A-STANDALONE=fail exit=$rc" >&2
  exit "$rc"
fi

if grep -q 'uncaught error:' "$out"; then
  echo "VENDOR-CLASS-A-STANDALONE=fail uncaught-error-detected" >&2
  exit 1
fi

echo "VENDOR-CLASS-A-STANDALONE=ok repl-summary"
