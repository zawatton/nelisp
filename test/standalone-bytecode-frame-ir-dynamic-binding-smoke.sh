#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$(realpath "$repo_root/../standalone-reader-fix/target/nelisp")"}
emacs_bin=${EMACS_31_1_BINARY:-"$(command -v emacs-31.1 || true)"}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-frame-dynbind.XXXXXX")
source_file="$artifact_dir/dynamic-binding-fixture.el"
elc_file="$artifact_dir/dynamic-binding-fixture.elc"
payload_file="$artifact_dir/dynamic-binding-payload.el"
cleanup() {
  find "$artifact_dir" -type f -delete
  rmdir "$artifact_dir"
}
trap cleanup EXIT

if [[ ! -x "$binary" ]]; then
  echo "dynamic-bind-frame-ir: missing executable: $binary" >&2
  exit 2
fi
if [[ -z "$emacs_bin" || ! -x "$emacs_bin" ]]; then
  echo "dynamic-bind-frame-ir: GNU Emacs 31.1 executable not found" >&2
  exit 2
fi

cat >"$source_file" <<'ELISP'
;;; -*- lexical-binding: t; -*-
(defvar nelisp-frame-ir-smoke-special nil)
(defun nelisp-frame-ir-smoke-reader () nelisp-frame-ir-smoke-special)
(defun nelisp-frame-ir-smoke-entry (value)
  (let ((nelisp-frame-ir-smoke-special value))
    (nelisp-frame-ir-smoke-reader)))
ELISP

FRAME_IR_SOURCE="$source_file" FRAME_IR_ELC="$elc_file" \
FRAME_IR_PAYLOAD="$payload_file" "$emacs_bin" --batch -Q \
  --eval '(unless (equal emacs-version "31.1") (kill-emacs 2))' \
  --eval '(unless (byte-compile-file (getenv "FRAME_IR_SOURCE")) (kill-emacs 3))' \
  --eval '(load (getenv "FRAME_IR_ELC"))' \
  --eval '(let* ((function (symbol-function (quote nelisp-frame-ir-smoke-entry))) (code (aref function 1)) (constants (aref function 2))) (unless (and (= (aref function 0) 257) (equal (append code nil) (quote (137 24 193 32 41 135))) (= (length constants) 2)) (kill-emacs 4)) (with-temp-file (getenv "FRAME_IR_PAYLOAD") (prin1 (list code constants (aref function 0)) (current-buffer)) (terpri)))'

if [[ ! -s "$elc_file" || ! -s "$payload_file" ]]; then
  echo "dynamic-bind-frame-ir: GNU byte compiler did not produce .elc payload" >&2
  exit 1
fi

actual=$(NELISP_FRAME_IR_PAYLOAD="$payload_file" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --eval '(require (quote nelisp-bytecode-frame-ir))' \
  --load "$repo_root/test/standalone-bytecode-frame-ir-dynamic-binding-driver.el" \
  --eval '(prin1 (nelisp-test-dynamic-binding-frame-ir))')

if [[ "$actual" != t ]]; then
  printf 'dynamic-bind-frame-ir: expected t, got %s\n' "$actual" >&2
  exit 1
fi
echo "dynamic-bind-frame-ir: PASS (GNU 31.1 .elc to source-free frame IR, dynamic bind/unbind effects, binding joins, malformed controls)"
