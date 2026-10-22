#!/usr/bin/env bash
# integ-wh-fchecks.sh: batch F's own fixture checks (tools/ai/batch-f-check.sh) against the
# merged dynamic binary, which fills both linkage slots (the dry run builds no static one).
# One line per check; the last line is "FCHECKS PASS n/n" or "FCHECKS FAIL k of n".
# Left out: the checks that need batch E's build as a negative control (bridge-test,
# vm-memory, vm-stress, vm-loop) and the ones that look at F's own commit pins.  The
# marker-arith and guard checks run without their negative controls, which need F's
# pre-fix builds (target/integ/f2, old-d29b53025) that this worktree does not have.
cd "$(dirname "$0")/../.." || exit 2
ROOT=$PWD
DYN=${INTEG_WH_DYN:-$PWD/target/integ/nelisp-dyn}
GNU=${GNU_EMACS:-/usr/local/bin/emacs}
OUT=$ROOT/target/integ-wh; mkdir -p "$OUT"
[ -x "$DYN" ] || { echo "missing $DYN"; exit 2; }
export BATCH_F_DYN=$DYN BATCH_F_STATIC=$DYN

# F's final (b994d7227): GNU-compiled arithmetic opcodes on markers and library cells.
chk_marker_arith() {
  local a=test/standalone-bytecode-marker-arith-check b=test/standalone-bytecode-library-arith-check t n rc
  for t in $a.el $a.expected $b.el $b.expected test/fixtures/bytecode-marker-arith.eld; do
    [ -f "$t" ] || { echo "FAIL missing $t"; return 1; }
  done
  (ulimit -v 8388608; timeout 300 "$GNU" -Q --batch --load $a.el > "$OUT/marker-arith.gnu.out" 2> "$OUT/marker-arith.gnu.err")
  { cmp -s $a.expected "$OUT/marker-arith.gnu.out" && [ ! -s "$OUT/marker-arith.gnu.err" ]; } ||
    { echo "FAIL GNU no longer prints the pin $a.expected"; return 1; }
  for t in $a $b; do
    n=$(basename $t)
    (ulimit -v 8388608; LC_ALL=C.UTF-8 timeout 300 "$DYN" --load $t.el > "$OUT/$n.dyn.out" 2> "$OUT/$n.dyn.err"); rc=$?
    if ! { [ $rc = 0 ] && [ ! -s "$OUT/$n.dyn.err" ] && cmp -s $t.expected "$OUT/$n.dyn.out"; }; then
      echo "FAIL $n: rc=$rc, $(grep -c '^FAIL' "$OUT/$n.dyn.out") interpreter mismatches, $(diff $t.expected "$OUT/$n.dyn.out" | grep -c '^[<>]') differing lines"
      return 1
    fi
  done
  echo "PASS marker-arith (GNU pin and interpreter equal; library cells honoured; negative control not run)"
}

# F's final: every direct callee of a staged compiler-macro, gv-expander or gv-setter handler
# is defined in the binary.
chk_guard() {
  local n
  (ulimit -v 8388608; "$GNU" --batch -Q -L src -L scripts -l nelisp-staged-handler-guard \
     -f nelisp-staged-handler-guard-batch -- "$ROOT" "$DYN" "$OUT/guard.dyn.txt" > /dev/null 2>&1) ||
    { echo "FAIL the guard fails: $(grep '^FAIL' "$OUT/guard.dyn.txt" | head -2 | tr '\n' ' ')"; return 1; }
  n=$(sed -n 's/^declarations: \([0-9]*\) .*/\1/p' "$OUT/guard.dyn.txt")
  [ "${n:-0}" -ge 32 ] || { echo "FAIL the guard read only ${n:-0} declarations (32 expected)"; return 1; }
  (ulimit -v 8388608; timeout 300 "$GNU" --batch -Q -L src -L scripts -L test -l ert -l test/nelisp-staged-handler-guard-test.el \
     -f ert-run-tests-batch-and-exit > "$OUT/ert-staged-handler-guard.log" 2>&1) ||
    { echo "FAIL guard tests: $(grep -E '^(Ran|   FAILED)' "$OUT/ert-staged-handler-guard.log" | head -3 | tr '\n' ' ')"; return 1; }
  echo "PASS guard ($n declarations; negative control not run)"
}

checks=("boot"
        "gnu buffer" "gnu error" "gnu shadow" "gnu variables" "gnu load" "gnu printer" "gnu syntax"
        "casefold" "e-fixtures" "closures" "syntax-rt"
        "fixture batch-f-doc-closure C.UTF-8" "fixture batch-f-skip-load C.UTF-8"
        "fixture batch-f-keymap-cmacro C.UTF-8" "fixture batch-f-keymap-cmacro-invalid C.UTF-8"
        "casetab" "census" "codegen-align"
        "@marker-arith" "@guard")
fail=0; n=0
for c in "${checks[@]}"; do
  n=$((n + 1)); s=$(date +%s)
  if [ "${c:0:1}" = "@" ]; then
    fn="chk_$(printf '%s' "${c:1}" | tr '-' '_')"
    out=$($fn 2>&1); rc=$?
  else
    # shellcheck disable=SC2086
    out=$(bash tools/ai/batch-f-check.sh $c 2>&1); rc=$?
  fi
  [ $rc = 0 ] || fail=$((fail + 1))
  printf '%-44s rc=%s %3ss  %s\n' "$c" "$rc" "$(( $(date +%s) - s ))" "$(printf '%s\n' "$out" | tail -n 1 | cut -c1-150)"
done
if [ $fail = 0 ]; then echo "FCHECKS PASS $n/$n"; else echo "FCHECKS FAIL $fail of $n"; fi
exit $((fail > 0))
