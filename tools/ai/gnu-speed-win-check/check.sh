#!/bin/sh
# Real-Windows (MSYS2) or wine check of the GNU-speed review branch.
# Usage: sh tools/ai/gnu-speed-win-check/check.sh PATH/TO/nelisp.exe [runner]
#   runner: empty on Windows, "wine" on Linux.
# Expected outputs were produced by GNU Emacs 31.1 (features.expected) and by
# GNU on the switch fixture (switch.expected).
cd "$(dirname "$0")/../../.." || exit 1
EXE=$1; RUN=$2; D=tools/ai/gnu-speed-win-check; O=${TMPDIR:-/tmp}/gnu-speed-win-check; mkdir -p "$O"
[ -n "$EXE" ] || { echo "usage: check.sh PATH/TO/nelisp.exe [wine]"; exit 2; }
fail=0
$RUN "$EXE" --version | tr -d '\r'
CV_REQUIRE_VM=1 $RUN "$EXE" --load test/nelisp-closurevm-fixture.el -- 2>&1 | tr -d '\r' > "$O/closurevm.out"
if grep -q 'CLOSUREVM COMPLETE checked=24' "$O/closurevm.out"; then echo "closurevm: PASS (24)"; else echo "closurevm: FAIL (see $O/closurevm.out)"; fail=1; fi
$RUN "$EXE" --load test/nelisp-bytecode-switch-fixture.el -- 2>"$O/switch.err" | tr -d '\r' > "$O/switch.out"
if cmp -s "$O/switch.out" $D/switch.expected; then echo "switch corpus: PASS (equals GNU)"; else echo "switch corpus: FAIL (diff $O/switch.out $D/switch.expected)"; fail=1; fi
$RUN "$EXE" --load $D/features.el -- 2>&1 | tr -d '\r' | grep -E '^(hash|maphash|mapc|switch|cond-switch|calls|closure|dyn) \(' > "$O/features.out"
if cmp -s "$O/features.out" $D/features.expected; then echo "features: PASS (8 lines equal GNU)"; else echo "features: FAIL (diff $O/features.out $D/features.expected)"; fail=1; fi
exit $fail
