#!/bin/sh
# S2.14: build20 prints vectors starting with a symbol, [sym ..] list heads and quote forms natively
# (no whole-object decline), within 2x of plain lists, and the 320-case print differential has no
# divergence beyond the pre-existing build18 set (print-gensym "#:" and bare-comma cases).
cd "$(dirname "$0")" || exit 1
test "$(grep -c 'native=ok' decline-b20.out)" -eq 5 || exit 1
python3 -I - <<'P' || exit 1
import re, sys
t = {m.group(1).strip(): float(m.group(2)) for m in re.finditer(r"^(.{28}) native=\S+\s+native-ms=\s*\S+ prin1-ms=\s*(\S+)", open("decline-b20.out").read(), re.M)}
lists = t["lists"]
for k in ("vectors starting w/ symbol", "list whose car is [sym ..]"):
    if t[k] > 2 * lists: print("too slow", k, t[k], lists); sys.exit(1)
P
python3 -I compare.py print-gnu.out b18-print-cases320.out | grep '^DIFF' | sort > .a
python3 -I compare.py print-gnu.out b20-print-cases.out | grep '^DIFF' | sort > .b
python3 -I compare.py print-gnu.out b20-print-cases.out | grep -qx 'compared A=320 B=320 mismatches=36' && cmp -s .a .b; r=$?; rm -f .a .b
test $r -eq 0 || exit 1
echo PASS
