#!/usr/bin/env bash
set -euo pipefail
root=${1:-.}
hits=$(rg -n --glob '*.el' --glob '*.elc' --glob '*.sh' --glob '*.py' \
  "\\((require|load)(['\"]|[[:space:]])+['\"]?nelisp-emacs-[[:alnum:]-]+" \
  "$root/lisp" "$root/src" "$root/scripts" || true)
if [[ -n "$hits" ]]; then
  printf '%s\n' "$hits" >&2
  printf 'GATE-COUNT checked=1 findings=1\n'
  exit 1
fi
count=$(find "$root/lisp" "$root/src" "$root/scripts" -type f \( -name '*.el' -o -name '*.elc' -o -name '*.sh' -o -name '*.py' \) | wc -l)
printf 'GATE-COUNT checked=%s findings=0\n' "$count"
