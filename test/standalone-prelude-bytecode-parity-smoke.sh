#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
compiled_bin="${NELISP_BIN:-$repo_root/target/nelisp}"
source_bin="${NELISP_PRELUDE_SOURCE_ONLY_BIN:-}"
fixtures="$repo_root/test/nelisp-prelude-bytecode-parity-fixtures.tsv"
report="${NELISP_PARITY_REPORT:-$repo_root/target/nelisp-prelude-bytecode-parity-results.tsv}"

if [[ ! -x "$compiled_bin" || ! -x "$source_bin" ]]; then
  echo "standalone-prelude-bytecode-parity-smoke: set NELISP_BIN and NELISP_PRELUDE_SOURCE_ONLY_BIN to executable readers" >&2
  exit 2
fi

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT
NELISP_PRELUDE_PARITY_HOST_SOURCE=1 emacs --batch -Q -L "$repo_root/scripts" \
  --eval '(setq load-prefer-newer t)' \
  -l nelisp-prelude-bytecode -l "$repo_root/test/nelisp-prelude-bytecode-parity.el" \
  > "$tmp_dir/host.tsv"
"$source_bin" --load "$repo_root/test/nelisp-prelude-bytecode-parity.el" \
  > "$tmp_dir/source.tsv"
"$compiled_bin" --load "$repo_root/test/nelisp-prelude-bytecode-parity.el" \
  > "$tmp_dir/compiled.tsv"

mkdir -p "$(dirname "$report")"
paste "$tmp_dir/host.tsv" \
  <(sed '$d' "$tmp_dir/source.tsv") \
  <(sed '$d' "$tmp_dir/compiled.tsv") \
  <(tail -n +2 "$fixtures") |
awk -F '\t' -v OFS='\t' -v output="$report" '
  BEGIN {
    print "name", "case", "host-result", "source-result", "compiled-result",
          "host-cell", "source-cell", "compiled-cell", "decision", "reason" > output
    failed = 0
  }
  {
    aligned = ($1 == $5 && $1 == $9 && $1 == $13 && $2 == $14 &&
               $3 == $15 && $4 != "" && $8 != "" && $12 != "")
    equal = ($3 == $7 && $3 == $11 && $3 !~ /:error/ &&
             $7 !~ /:error/ && $11 !~ /:error/)
    active = ($12 == "bytecode")
    observed = (equal && active) ? "pass" : "reject"
    if (!aligned || observed != $16 || $12 != $18) {
      failed++
      printf "standalone-prelude-bytecode-parity-smoke: drift at %s (expected %s/%s, got %s/%s)\n",
             $1, $16, $18, observed, $12 > "/dev/stderr"
    }
    print $1, $2, $3, $7, $11, $4, $8, $12, $16, $17 >> output
    count++
    if ($16 == "pass") adopted++
    if ($8 == "bytecode") source_bytecode++
  }
  END {
    if (count != 108) {
      printf "standalone-prelude-bytecode-parity-smoke: expected 108 fixtures, got %d\n", count > "/dev/stderr"
      failed++
    }
    if (source_bytecode != 0) {
      printf "standalone-prelude-bytecode-parity-smoke: source-only control exposed %d byte-code candidates\n", source_bytecode > "/dev/stderr"
      failed++
    }
    if (failed) exit 1
    printf "standalone-prelude-bytecode-parity-smoke: PASS (%d active byte-code functions; %d explicit rejections)\n",
           adopted, count - adopted
  }'
