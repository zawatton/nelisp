#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
binary="${NELISP_BIN:-${1:-$repo_root/target/nelisp}}"
if [[ ! -x "$binary" ]]; then
  echo "standalone-ash-arity-smoke: missing executable: $binary" >&2
  exit 2
fi

probe='(list (condition-case e (ash 1) (wrong-number-of-arguments e))
            (ash 4 1)
            (condition-case e (ash 1 2 3) (wrong-number-of-arguments e)))'
host="$(emacs --batch -Q --eval "(princ $probe)")"
standalone="$("$binary" --eval "$probe")"
if [[ "$host" != "$standalone" ]]; then
  printf 'GNU Emacs 31.1: %s\nStandalone:     %s\n' "$host" "$standalone" >&2
  exit 1
fi
printf 'PASS ash fixed arity parity: %s\n' "$standalone"
