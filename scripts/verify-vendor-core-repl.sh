#!/usr/bin/env bash
set -euo pipefail

repo_root="${REPO_ROOT:-$(pwd)}"
nelisp_bin="${NELISP_BIN:-$repo_root/vendor/nelisp/target/nelisp}"
nelisp_root="${NELISP_ROOT:-$(dirname "$(dirname "$nelisp_bin")")}"
bootstrap_repl="${NEMACS_BOOTSTRAP_REPL:-$repo_root/build/nemacs-bootstrap.repl}"
vendor_modules="${VENDOR_CORE_MODULES:-}"
vendor_limit="${VENDOR_CORE_LIMIT:-0}"
vendor_strict="${VENDOR_CORE_STRICT_ELISP:-t}"

tmp="$(mktemp "${TMPDIR:-/tmp}/nemacs-vendor-core.XXXXXX.repl")"
out="$(mktemp "${TMPDIR:-/tmp}/nemacs-vendor-core.XXXXXX.out")"
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
  echo "verify-vendor-core: nelisp binary is not executable: $nelisp_bin" >&2
  exit 1
fi

if [[ ! -r "$bootstrap_repl" ]]; then
  echo "verify-vendor-core: bootstrap REPL input is not readable: $bootstrap_repl" >&2
  exit 1
fi

: > "$tmp"
prelude="$nelisp_root/scripts/nelisp-stdlib-prelude.el"
if [[ -r "$prelude" ]]; then
  append_eval_source_file "$prelude" >> "$tmp"
fi
cat "$bootstrap_repl" >> "$tmp"
printf '\n' >> "$tmp"
printf '%s\n' "(setq nelisp-emacs-vendor-root \"$repo_root/vendor\")" >> "$tmp"
printf '%s\n' "(setq load-path (list \"$repo_root/src\" \"$repo_root/scripts\" \"$repo_root/vendor/emacs-lisp\" \"$repo_root/vendor/emacs-lisp/emacs-lisp\" \"$repo_root/vendor/emacs-lisp/vc\"))" >> "$tmp"
append_eval_source_file "$repo_root/scripts/vendor-core-smoke.el" >> "$tmp"
printf '%s\n' "(setq vendor-core-smoke-module-spec \"$vendor_modules\")" >> "$tmp"
printf '%s\n' "(setq vendor-core-smoke-default-limit $vendor_limit)" >> "$tmp"
printf '%s\n' "(setq vendor-core-smoke-strict $vendor_strict)" >> "$tmp"
printf '%s\n' "(vendor-core-smoke-batch)" >> "$tmp"
printf '%s\n' ",quit" >> "$tmp"

set +e
"$nelisp_bin" --repl --no-prompt --no-print < "$tmp" > "$out" 2>&1
rc=$?
set -e

cat "$out"

if [[ "$rc" -ne 0 ]]; then
  echo "VENDOR-CORE-STANDALONE=fail exit=$rc" >&2
  exit "$rc"
fi

summary="$(grep -E 'vendor-core-summary .*failures=0([^0-9]|$)' "$out" | tail -n 1 || true)"
if [[ -z "$summary" ]]; then
  echo "VENDOR-CORE-STANDALONE=fail summary-missing-or-failures" >&2
  exit 1
fi

echo "VENDOR-CORE-STANDALONE=ok repl-summary"
