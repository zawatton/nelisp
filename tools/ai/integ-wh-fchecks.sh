#!/usr/bin/env bash
# integ-wh-fchecks.sh: batch F's own fixture checks (tools/ai/batch-f-check.sh) against the
# merged dynamic binary, which fills both linkage slots (the dry run builds no static one).
# One line per check; the last line is "FCHECKS PASS n/n" or "FCHECKS FAIL k of n".
# Left out: the checks that need batch E's build as a negative control (bridge-test,
# vm-memory, vm-stress, vm-loop) and the ones that look at F's own commit pins.
cd "$(dirname "$0")/../.." || exit 2
DYN=${INTEG_WH_DYN:-$PWD/target/integ/nelisp-dyn}
[ -x "$DYN" ] || { echo "missing $DYN"; exit 2; }
export BATCH_F_DYN=$DYN BATCH_F_STATIC=$DYN
checks=("boot"
        "gnu buffer" "gnu error" "gnu shadow" "gnu variables" "gnu load" "gnu printer" "gnu syntax"
        "casefold" "e-fixtures" "closures" "syntax-rt"
        "fixture batch-f-doc-closure C.UTF-8" "fixture batch-f-skip-load C.UTF-8"
        "casetab" "census" "codegen-align")
fail=0; n=0
for c in "${checks[@]}"; do
  n=$((n + 1)); s=$(date +%s)
  # shellcheck disable=SC2086
  out=$(bash tools/ai/batch-f-check.sh $c 2>&1); rc=$?
  [ $rc = 0 ] || fail=$((fail + 1))
  printf '%-44s rc=%s %3ss  %s\n' "$c" "$rc" "$(( $(date +%s) - s ))" "$(printf '%s\n' "$out" | tail -n 1 | cut -c1-150)"
done
if [ $fail = 0 ]; then echo "FCHECKS PASS $n/$n"; else echo "FCHECKS FAIL $fail of $n"; fi
exit $((fail > 0))
