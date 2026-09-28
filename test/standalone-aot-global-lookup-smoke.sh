#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
host_emacs=${EMACS:-emacs}
timeout_seconds=${NELISP_TIMEOUT_SECONDS:-60}

if [[ ! -x "$binary" ]]; then
  echo "standalone-aot-global-lookup-smoke: missing executable: $binary" >&2
  exit 2
fi

host_expected=$("$host_emacs" -Q --batch \
  --eval '(defvar aot-probe-global 123)' \
  -l "$repo_root/test/fixtures/aot-global-lookup-functions.el" \
  --eval '(progn (setq aot-probe-global 123) (princ (prin1-to-string (list (nl_probe_global_value) (nl_probe_max_fixnum) (let ((aot-probe-global 456)) (nl_probe_global_value))))))')
expected="(123 2305843009213693951 456)"
if [[ "$host_expected" != "$expected" ]]; then
  echo "standalone-aot-global-lookup-smoke: unexpected Host values: $host_expected" >&2
  exit 1
fi

actual=$(timeout "${timeout_seconds}s" env NELISP_REPO_ROOT="$repo_root" \
  "$binary" --eval \
  "(load (concat (getenv \"NELISP_REPO_ROOT\") \"/test/standalone-aot-global-lookup-driver.el\") nil nil t)")
expected='(123 123 2305843009213693951 2305843009213693951 456 456 (0 123 1) 2305843009213693951)t'
if [[ "$actual" != "$expected" ]]; then
  echo "standalone-aot-global-lookup-smoke: unexpected VM/native results: $actual" >&2
  exit 1
fi
echo "standalone-aot-global-lookup-smoke: PASS (Host, VM, native, dynamic binding, post-GC)"
