#!/bin/sh
# Usage: check-fixtures.sh NELISP-BINARY -- the lane-rule fixtures (weak 24/24, GNU 56/56, AFRT 24/25 with the known red
# exit-hook-result-gc, buffer 21/21) via fixtures.py, which pins the inputs of the integ-1036 tree (FIXROOT).
root=$(cd "$(dirname "$0")/../../.." && pwd); out=$root/target/vmcore/fixtures-$(basename "$(dirname "$(dirname "$1")")")
rm -rf "$out"; export FIXOUT=$out; ( ulimit -v 8388608; timeout 580 python3 "$root/tools/ai/vmcore/fixtures.py" "$1" ) 2>&1 | tail -6; [ -f "$out/report.json" ] && grep -q EXPECTED_BASELINE_RED_REMAINS "$out/report.json"
