#!/bin/sh
# Checks for tools/ai/reprs-progress.org.  Usage: check.sh KIND BINARY [ARGS...]
# SURVEY4 (env, optional) is the survey4 directory (drv/*.el row drivers + pre4.el).
HERE=$(cd "$(dirname "$0")" && pwd)
KIND=$1; BIN=$(realpath "$2"); shift 2
S4=${SURVEY4:-$HERE/../../../../resume-4943-20261024/target/survey4}
case $KIND in
parity)   # parity NAME : corpus NAME.el equal to GNU Emacs 31.1
  bash "$HERE/run-parity.sh" "$BIN" "$HERE/$1.el" ;;
prog)     # prog FILE : the probe FILE must print only "OK" lines (exit 0 when no FAIL)
  out=$(cd "$(mktemp -d)" && ulimit -v 8388608 && timeout 120 "$BIN" --load "$HERE/$1" -- 2>&1)
  echo "$out" | head -20
  echo "$out" | grep -q '^FAIL' && exit 1
  echo "$out" | grep -q '^OK' || exit 1
  exit 0 ;;
row)      # row ROW N MAX_US : survey4 row ROW, N trips, at most MAX_US microseconds per trip
  line=$(SURVEY4=$(realpath "$S4") bash "$HERE/rowtime.sh" "$BIN" "$1" "$2")
  echo "$line"
  echo "$line" | python3 -c "
import re,sys
m=re.search(r'elapsed=([0-9.]+)',sys.stdin.read()); n=$2; mx=$3
us=float(m.group(1))/n*1e6 if m else 1e18
print('us/trip=%.3f max=%s'%(us,mx)); sys.exit(0 if us<=mx else 1)" ;;
rowo)     # rowo ROW N MAX_US : like row, with tools/ai/reprs/case-overlay.el loaded first (see case-overlay-check.py)
  d=$(mktemp -d)
  printf '(load "%s" nil t)\n(load "%s" nil t)\n' "$HERE/case-overlay.el" "$S4/drv/$1.el" > "$d/wrap.el"
  line=$(cd "$S4/drv" && ulimit -v 8388608 && BENCH_N=$2 timeout 300 "$BIN" --load "$d/wrap.el" -- 2>&1 | grep BENCH-CHECK)
  echo "$line"
  echo "$line" | python3 -c "
import re,sys
m=re.search(r'elapsed=([0-9.]+)',sys.stdin.read()); n=$2; mx=$3
us=float(m.group(1))/n*1e6 if m else 1e18
print('us/trip=%.3f max=%s'%(us,mx)); sys.exit(0 if us<=mx else 1)" ;;
sweep)    # sweep : the case sweep (every character to #x2ffff) equals GNU's hash, with the case overlay loaded
  d=$(mktemp -d)
  printf '(require (quote subr-x))\n(load "%s" nil t)\n' "$HERE/case-overlay.el" > "$d/nl.el"; cat "$HERE/casesweep.el" >> "$d/nl.el"; echo '(princ (format "%S\n" (sweep)))' >> "$d/nl.el"
  cp "$HERE/casesweep.el" "$d/gnu.el"; echo '(princ (format "%S\n" (sweep)))' >> "$d/gnu.el"
  g=$(cd $d && timeout 300 /usr/local/bin/emacs -Q --batch -l gnu.el 2>&1 | tail -1)
  n=$(cd $d && ulimit -v 8388608 && timeout 600 "$BIN" --load nl.el -- 2>&1 | head -1)
  echo "gnu: $g"; echo "nl:  $n"; [ -n "$g" ] && [ "$g" = "$n" ] ;;
weak)     # weak : the critical weak-reference fixture, 24/24 (stdout equal to the pinned expectation, no stderr)
  W=$(realpath "$HERE/../../../../resume-4943-20261024/target/resume-acceptance/d7-critical-weak24")
  d=$(mktemp -d); (cd "$d" && ulimit -v 8388608 && timeout 90 "$BIN" --load "$W/probe.el" -- > out 2> err); rc=$?
  [ $rc -eq 0 ] && [ ! -s "$d/err" ] && cmp -s "$d/out" "$W/expected" && echo "weak 24/24 PASS" ;;
gnu56)    # gnu56 : the GNU eqhash fixture, 56/56
  F=$(realpath "$HERE/../../../../resume-4943-20261024/tools/ai/integ-wh-eqhash-fixture.sh"); d=$(mktemp -d)
  out=$(cd "$d" && ulimit -v 8388608 && EQHASH_FIXTURE_OUT="$d/o" EQHASH_FIXTURE_TIMEOUT=120 EQHASH_FIXTURE_KILL_AFTER=5 timeout 150 bash "$F" "$BIN" 2>"$d/err")
  echo "$out"; [ "$out" = "EQHASH-FIXTURE 56/56" ] && [ ! -s "$d/err" ] ;;
buffer)   # buffer : the buffer unification smoke, 21/21
  d=$(mktemp -d); out=$(cd "$HERE/../../.." && ulimit -v 8388608 && timeout 120 "$BIN" --load test/nelisp-buffer-unification-standalone-smoke.el -- 2>"$d/err")
  echo "$out" | tail -3; echo "$out" | grep -q 'GATE-COUNT checked=21 findings=0' && echo "$out" | grep -q 'nelisp-buffer-unification-standalone-smoke: PASS' && [ ! -s "$d/err" ] ;;
speedwin) # speedwin : closurevm 24, switch corpus and features equal GNU (tools/ai/gnu-speed-win-check)
  d=$(mktemp -d); (cd "$HERE/../../.." && TMPDIR=$d ulimit -v 8388608 && TMPDIR=$d timeout 600 sh tools/ai/gnu-speed-win-check/check.sh "$BIN") ;;
*) echo "unknown kind $KIND" >&2; exit 2 ;;
esac
