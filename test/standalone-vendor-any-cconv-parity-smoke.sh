#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
nelisp_bin="${NELISP_BIN:-$repo_root/target/nelisp}"
emacs_bin="${EMACS:-emacs}"
run_dir="$(mktemp -d)"
trap 'rm -rf "$run_dir"' EXIT

if [[ ! -x "$nelisp_bin" ]]; then
  printf 'standalone binary is not executable: %s\n' "$nelisp_bin" >&2
  exit 2
fi

expression="(list (any (lambda (x) (eq x 'hit)) '(miss hit later)) (any (lambda (_x) nil) '(miss hit)) (cconv-closure-convert '(lambda (x) (+ x 1))))"
host_output="$("$emacs_bin" -Q --batch --eval "(progn (require 'cconv) (prin1 $expression))")"
standalone_output="$(cd "$run_dir" && "$nelisp_bin" \
  --load "$repo_root/scripts/nelisp-stdlib-prelude.el" \
  --load "$repo_root/vendor/emacs-lisp/emacs-lisp/cconv.el" \
  --eval "(prin1 $expression)")"

if [[ "$host_output" != "$standalone_output" ]]; then
  printf 'Host:       %s\nStandalone: %s\n' "$host_output" "$standalone_output" >&2
  exit 1
fi

printf 'GNU Emacs / standalone parity: %s\n' "$standalone_output"
