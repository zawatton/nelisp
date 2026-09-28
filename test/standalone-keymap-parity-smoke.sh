#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
binary="${NELISP_BIN:-$repo_root/target/nelisp}"
if [[ ! -x "$binary" ]]; then
  echo "standalone-keymap-parity-smoke: missing executable: $binary" >&2
  exit 2
fi

actual="$($binary --eval '(progn (defvar-keymap demo-map :doc "demo" "C-c n" (quote ignore)) (let ((direct (make-sparse-keymap))) (define-key direct (kbd "C-c n") (quote ignore)) (global-set-key (kbd "C-c n") (quote ignore)) (list (list (keymapp demo-map) (lookup-key demo-map (kbd "C-c n"))) (list (lookup-key direct (kbd "C-c n")) (lookup-key direct (key-parse "C-c n"))) (lookup-key (current-global-map) (kbd "C-c n")))))')"
expected='((t ignore) (ignore ignore) ignore)'
if [[ "$actual" != "$expected" ]]; then
  echo "standalone-keymap-parity-smoke: expected $expected, got $actual" >&2
  exit 1
fi

echo "standalone-keymap-parity-smoke: PASS"
