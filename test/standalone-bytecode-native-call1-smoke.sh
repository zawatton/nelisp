#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
emacs_cmd="${EMACS:-emacs}"
binary_input="${CALL1_BIN:-${NELISP_BIN:-${NELISP_BINARY:-}}}"
[[ -n "$binary_input" ]] || { printf '%s\n' 'Set CALL1_BIN (or NELISP_BIN/NELISP_BINARY) to the attested reader' >&2; exit 2; }
binary="$(realpath "$binary_input")"
[[ -x "$binary" ]]
expected_sha="${CALL1_EXPECTED_SHA256:-aea8ea77673f590d5716b5861a37366c849706e6fa05f54c6813f1cd9872db4a}"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/nelisp-call1.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT
source="$tmpdir/call1-source.el"
elc="$tmpdir/call1-source.elc"
artifact="$tmpdir/call1.nelr"

actual_sha="$(sha256sum "$binary" | awk '{print $1}')"
[[ "$actual_sha" == "$expected_sha" ]]
emacs_version="$("$emacs_cmd" --batch -Q --eval '(princ emacs-version)')"
[[ "$emacs_version" == 31.1* ]]
cat >"$source" <<'EOF'
;;; -*- lexical-binding: t; -*-
(defun call1-probe-function (argument)
  "固定 CALL1 bytecode fixture."
  (call1-probe-callee argument))
(defun call1-probe-callee (argument) argument)
(provide 'call1-probe-fixture)
EOF
export CALL1_SOURCE="$source"
"$emacs_cmd" --batch -Q --eval '(byte-compile-file (getenv "CALL1_SOURCE"))'
[[ -s "$elc" ]]
rm "$source"
[[ ! -e "$source" ]]
"$emacs_cmd" --batch -Q -l "$elc" --eval \
  '(unless (equal (call1-probe-function 17) 17) (error "GNU oracle failed"))'
export CALL1_ELC="$elc" CALL1_ARTIFACT="$artifact" CALL1_RUNTIME_SHA="$expected_sha"
set +e
"$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$script_dir/standalone-bytecode-native-call1-driver.el" \
  >"$tmpdir/stdout" 2>"$tmpdir/stderr"
status=$?
set -e
if [[ $status -ne 0 ]] || ! rg -q '^CALL1-SOURCEFREE-TOKEN-GC-SIGNAL-THROW-MUTATION-CLEANUP-PASS$' "$tmpdir/stdout" || \
   rg -qi '(^|\s)(error|fatal):' "$tmpdir/stderr"; then
  cat "$tmpdir/stdout"
  cat "$tmpdir/stderr"
  exit 1
fi
cat "$tmpdir/stdout"
