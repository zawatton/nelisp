#!/bin/sh
# Doc 210 S10.3 end to end: the S6.10 measurement (Host/VM/JIT equality,
# native calls > 0, timing) passes, the handler-bearing native body agrees
# with host GNU Emacs 31.1 in every scenario (test/nelisp-eln-s610-smoke.sh
# scenarios|forced), the authenticated slots and the artifact bytes are
# guarded (tamper, mutation), and the S6.22 corpus validator passes with all
# 19 fixed-corpus functions native.
#
# Usage: NELISP_BIN=<bin> [ELN_PROGRESS_BIN=<bin>] sh test/nelisp-eln-s610-e2e.sh
# The S6.10 and S6.22 commands are read from the ledger, never duplicated.
# The S6.22 validator needs the current evidence file
# (`make eln-s6-corpus-evidence ELN_PROGRESS_BIN=<bin>').
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-${ELN_PROGRESS_BIN:-$repo/target/nelisp}}
ledger=$repo/tools/ai/eln-progress.org
[ -x "$binary" ] || { echo "NELISP_BIN is not executable: $binary" >&2; exit 2; }
ELN_PROGRESS_BIN=$binary
export ELN_PROGRESS_BIN
NELISP_BIN=$binary
export NELISP_BIN
cd "$repo"

# ledger_cmd ID : the `cmd:' line of criterion ID, without the prefix.
ledger_cmd() {
  awk -v id="$1" '
    $0 ~ "^\\*\\* " id " " { in_crit = 1; next }
    /^\*\* / { in_crit = 0 }
    in_crit && /^cmd: / { sub(/^cmd: /, ""); print; exit }' "$ledger"
}

run_ledger_cmd() {
  cmd=$(ledger_cmd "$1")
  [ -n "$cmd" ] || { echo "criterion $1 has no cmd in the ledger" >&2; exit 1; }
  out=$(sh -c "$cmd" 2>&1) || { printf '%s\n' "$out" | tail -8 >&2; echo "$1 command failed" >&2; exit 1; }
  printf '%s\n' "$out"
}

echo "== S6.10 measurement"
measure=$(run_ledger_cmd S6.10)
printf '%s\n' "$measure" | grep '^S6_MEASURE_RESULT '
printf '%s\n' "$measure" | grep -q '^S6_MEASURE_RESULT function=byte-compile-form status=PASS ' || {
  echo "S6.10 did not report PASS" >&2; exit 1; }
raw=$(printf '%s\n' "$measure" | sed -n 's/.* native_raw_calls=\([0-9]*\).*/\1/p')
dispatch=$(printf '%s\n' "$measure" | sed -n 's/.* native_dispatch_calls=\([0-9]*\).*/\1/p')
[ "${raw:-0}" -gt 0 ] && [ "${dispatch:-0}" -gt 0 ] || {
  echo "S6.10 made no native calls" >&2; exit 1; }
echo "S10_MEASURE_NATIVE_CALLS=PASS"

echo "== handler-bearing scenarios, host vs NeLisp"
sh "$script_dir/nelisp-eln-s610-smoke.sh" scenarios
sh "$script_dir/nelisp-eln-s610-smoke.sh" forced
sh "$script_dir/nelisp-eln-s610-smoke.sh" tamper
sh "$script_dir/nelisp-eln-s610-smoke.sh" mutation

echo "== S6.22 corpus validator"
validate=$(run_ledger_cmd S6.22)
printf '%s\n' "$validate" | tail -3
printf '%s\n' "$validate" | grep -q 'S6.22 PASS: 19/19' || {
  echo "S6.22 validator did not report 19/19" >&2; exit 1; }
evidence=${ELN_S6_EVIDENCE:-$repo/target/progress/s6-corpus-evidence.json}
grep -q 'byte-compile-form' "$evidence" || {
  echo "evidence has no byte-compile-form row" >&2; exit 1; }
echo "S10_CORPUS_ALL_NATIVE=PASS"
echo "NELISP-ELN-S610-E2E-PASS"
