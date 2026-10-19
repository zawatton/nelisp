#!/usr/bin/env bash
set -euo pipefail

# SPDX-License-Identifier: GPL-3.0-or-later

repo_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
binary=${NELISP_BIN:-${1:-$repo_root/target/nelisp}}
if [[ ! -x $binary ]]; then
  echo "standalone-bytecode-varset-constant-smoke: missing executable: $binary" >&2
  exit 2
fi

command=("$binary")
if [[ -f $binary.cold ]]; then
  command+=(--cold-load-from "$binary.cold")
fi

form='(let ((code (unibyte-string 137 16 135)))
  (setq nelisp-bvm-varset-probe (quote old))
  (let* ((constant-t
          (condition-case error-data
              (funcall (make-byte-code 257 code [t] 2) (quote changed))
            (setting-constant error-data)))
         (constant-nil
          (condition-case error-data
              (funcall (make-byte-code 257 code [nil] 2) (quote changed))
            (setting-constant error-data)))
         (normal
          (funcall (make-byte-code 257 code [nelisp-bvm-varset-probe] 2)
                   (quote changed)))
         (normal-state (list normal nelisp-bvm-varset-probe))
         (heap-value (list (quote payload)))
         (gc-setter
          (make-byte-code 257 (unibyte-string 193 32 136 137 137 16 135)
                          [nelisp-bvm-varset-probe garbage-collect] 3))
         (gc-result (funcall gc-setter heap-value)))
    (garbage-collect)
    (list constant-t constant-nil normal-state
          (list (eq gc-result heap-value)
                (eq nelisp-bvm-varset-probe heap-value)
                nelisp-bvm-varset-probe))))'

mkdir -p "$repo_root/target/tmp"
error_file=$(mktemp "$repo_root/target/tmp/bytecode-varset-constant.XXXXXX")
trap 'rm -f "$error_file"' EXIT
set +e
actual=$("${command[@]}" --eval "$form" 2>"$error_file")
child_status=$?
set -e

if (( child_status != 0 )); then
  echo "standalone-bytecode-varset-constant-smoke: evaluator exited $child_status" >&2
  cat "$error_file" >&2
  exit 1
fi

expected='((setting-constant t) (setting-constant nil) (changed changed) (t t (payload)))'
if [[ $actual != "$expected" ]]; then
  echo "standalone-bytecode-varset-constant-smoke: result mismatch: $actual" >&2
  exit 1
fi

echo "standalone-bytecode-varset-constant-smoke: PASS"
