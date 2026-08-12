#!/usr/bin/env bash
# nelisp-form-cost-gate.sh — assert RATIOS between forms, not absolute times.
#
# WHY RATIOS
#
# A type checker cannot catch a form that is correct but pays for its work
# twice.  `dotimes' did exactly that for an unknown length of time: the special
# form materialised `(number-sequence 0 (1- N))' and mapc'd a closure over it,
# so counting to N allocated an N-element list first.  Nothing was ill-typed;
# it cost 3x and nobody noticed.
#
# Absolute timings cannot gate that, because they move with the machine and the
# load.  Ratios do not.  Measured 2026-08-11/12, dotimes vs a hand-written
# while loop:
#
#     through the 116 MB flat image   3.7x
#     on the bare reader, no image    3.0x
#     under three competing CPU hogs  same conclusion
#
# Same verdict on every platform and every load.  So the gate asserts "dotimes
# costs about what the loop it should compile to costs", and it would have
# failed on the day the mapc shape landed.
#
# USAGE
#
#   nelisp-form-cost-gate.sh [--bin PATH] [--n ITER] [--reps R]
#
# Exit 0 when every ratio is within budget, 1 otherwise.  Every case prints its
# measured ratio whether it passes or fails -- a gate that only speaks up on
# failure teaches nothing about the margin it is holding.
set -uo pipefail
export LC_ALL=C

BIN="${GATE_BIN:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/target/nelisp}"
N=20000
REPS=3
VMLIMIT="${GATE_VMLIMIT:-8388608}"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/form-cost-gate.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

while [ "$#" -gt 0 ]; do
  case "$1" in
    --bin) BIN="$2"; shift 2 ;;
    --n) N="$2"; shift 2 ;;
    --reps) REPS="$2"; shift 2 ;;
    -h|--help) sed -n '2,32p' "$0"; exit 0 ;;
    *) echo "form-cost-gate: unknown option $1" >&2; exit 2 ;;
  esac
done
[ -x "$BIN" ] || { echo "form-cost-gate: not executable: $BIN" >&2; exit 2; }

# Body must leave the loop counter in `i' so the run proves it looped; a form
# that silently did nothing would otherwise post an excellent time.
run_ms() {
  local body="$1" file="$WORK/b.el" i ms best=999999 out count
  printf '(progn (setq kk 0) (setq zl (list 1 2 3)) (let ((n %s) (i 0)) %s (nelisp--write-stdout-bytes (concat "GC count=" (prin1-to-string i) "\\n"))))\n' \
    "$N" "$body" > "$file"
  for ((i = 0; i < REPS; i++)); do
    local t0 t1
    t0=$(date +%s%3N)
    out=$( ulimit -v "$VMLIMIT"; timeout 300 "$BIN" --load "$file" 2>&1 )
    t1=$(date +%s%3N)
    count=$(printf '%s' "$out" | sed -n 's/^GC count=\([0-9-]*\).*/\1/p' | head -1)
    [ "$count" = "$N" ] || { echo "BAD"; return 1; }
    ms=$((t1 - t0))
    [ "$ms" -lt "$best" ] && best=$ms
  done
  echo "$best"
}

BASE=$(run_ms '(while (< i n) (setq i (1+ i)))')
[ "$BASE" = "BAD" ] && { echo "form-cost-gate: baseline loop did not run" >&2; exit 1; }
EMPTY=$(run_ms '(setq i n)')
[ "$EMPTY" = "BAD" ] && EMPTY=0
echo "[form-cost-gate] bin=$(sha256sum "$BIN" | cut -c1-16) n=$N reps=$REPS"
echo "[form-cost-gate] startup=${EMPTY}ms  bare while loop=${BASE}ms"

fails=0
# CASE NAME | BUDGET | FORM UNDER TEST | REFERENCE FORM
#
# The reference is what the form under test SHOULD compile to.  Budget is the
# ratio at which the difference stops being noise and starts being a defect.
check() {
  local name="$1" budget="$2" test_body="$3" ref_body="$4" floor_body="${5:-}"
  local t r f ratio verdict
  t=$(run_ms "$test_body"); r=$(run_ms "$ref_body")
  # Each case subtracts ITS OWN floor.  Using one global floor made the dolist
  # case report 1.75x from a 7ms-vs-4ms difference -- the ratio of two noise
  # samples.  A case whose setup costs more than its body needs that setup
  # measured, not the process startup.
  if [ -n "$floor_body" ]; then f=$(run_ms "$floor_body"); else f="$EMPTY"; fi
  if [ "$t" = "BAD" ] || [ "$r" = "BAD" ] || [ "$f" = "BAD" ]; then
    printf '%-24s BAD (loop did not run)\n' "$name"; fails=$((fails + 1)); return
  fi
  local tn=$((t - f)) rn=$((r - f))
  # Below this, the two numbers are samples of the noise floor and their ratio
  # means nothing.  Say so instead of failing the build on it.
  if [ "$tn" -lt 40 ] || [ "$rn" -lt 40 ]; then
    printf '%-24s INCONCLUSIVE (test=%sms ref=%sms floor=%sms -- raise --n)\n' \
      "$name" "$t" "$r" "$f"
    return
  fi
  ratio=$(awk -v a="$tn" -v b="$rn" 'BEGIN{printf "%.2f", a/b}')
  verdict=$(awk -v x="$ratio" -v b="$budget" 'BEGIN{print (x<=b)?"ok":"FAIL"}')
  [ "$verdict" = "FAIL" ] && fails=$((fails + 1))
  printf '%-24s %6.2fx (budget %sx)  test=%sms ref=%sms floor=%sms  %s\n' \
    "$name" "$ratio" "$budget" "$t" "$r" "$f" "$verdict"
}

# dotimes must cost about what the let+while it should lower to costs.
# Measured 3.0x before the 2026-08-12 fix, 1.05x after.
check dotimes-vs-while 1.50 \
  '(progn (dotimes (j n) (setq kk (1+ kk))) (setq i n))' \
  '(let ((j 0)) (while (< j n) (setq kk (1+ kk)) (setq j (1+ j))) (setq i n))'

# dolist keeps its mapc shape deliberately (measured 11% over while, which does
# not pay for a rewrite), so this budget is about catching a REGRESSION, not
# about demanding the rewrite.
check dolist-vs-while 1.60 \
  '(progn (setq zl (number-sequence 0 (1- n))) (dolist (x zl) (setq kk (1+ kk))) (setq i n))' \
  '(progn (setq zl (number-sequence 0 (1- n))) (let ((l zl)) (while l (let ((x (car l))) (setq kk (1+ kk))) (setq l (cdr l)))) (setq i n))' \
  '(progn (setq zl (number-sequence 0 (1- n))) (setq i n))'

echo
if [ "$fails" -eq 0 ]; then
  echo "[form-cost-gate] PASS"
else
  echo "[form-cost-gate] FAIL ($fails case(s) over budget)"
  echo "[form-cost-gate] A ratio blowing up means the form stopped lowering to"
  echo "[form-cost-gate] its reference shape -- read the special form, not the profiler."
fi
exit $((fails > 0))
