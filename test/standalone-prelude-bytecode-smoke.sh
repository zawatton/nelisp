#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
binary="${NELISP_BIN:-$repo_root/target/nelisp}"
report="$repo_root/target/nelisp-prelude-bytecode-report.tsv"

host_expected="$(emacs --batch -Q --eval \
  '(princ (list (car-safe (quote (a b))) (car-safe 5) (cdr-safe (quote (a b))) (cdr-safe 5)))' \
  2>/dev/null)"
actual="$("$binary" --eval \
  '(list (car-safe (quote (a b))) (car-safe 5) (cdr-safe (quote (a b))) (cdr-safe 5))')"
compiled="$("$binary" --eval \
  '(list (byte-code-function-p (symbol-function (quote car-safe))) (byte-code-function-p (symbol-function (quote cdr-safe))))')"
if [[ "$actual" != "$host_expected" || "$compiled" != '(t t)' ]]; then
  echo "standalone-prelude-bytecode-smoke: host=$host_expected vm=$actual compiled=$compiled" >&2
  exit 1
fi

if [[ ! -s "$report" ]]; then
  echo "standalone-prelude-bytecode-smoke: missing adoption report: $report" >&2
  exit 1
fi
grep -Fqx $'# compiler\t31.1' "$report"
awk -F '\t' '$3 == "car-safe" && $1 == "scripts/nelisp-stdlib-prelude.el" && $2 ~ /^[0-9]+$/ && $4 == "adopt" && length($6) == 64 { ok=1 } END { exit !ok }' "$report"
adopted="$(awk -F '\t' '$4 == "adopt" { n++ } END { print n+0 }' "$report")"
rejected="$(awk -F '\t' '$4 == "reject" { n++ } END { print n+0 }' "$report")"
if (( adopted < 16 || rejected < 200 )); then
  echo "standalone-prelude-bytecode-smoke: incomplete report: adopted=$adopted rejected=$rejected" >&2
  exit 1
fi

echo "standalone-prelude-bytecode-smoke: PASS (host parity, $adopted adopted, $rejected rejected)"
