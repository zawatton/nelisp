#!/bin/sh
# nelisp-alloc-sites-smoke.sh --- the per-call-site allocation counter
#
# Runs `tools/ai/nelisp-ai.sh alloc-sites' on a script with five windows and
# checks its JSON report:
# - every window is complete: the allocations the breakpoint recorded equal
#   the allocator's own success counters;
# - windows do not bleed into each other: 20 global reads in a loop allocate
#   as much more than 10 as 10 do more than 0;
# - sizes are read: a 40-element vector's window asked for at least 320 bytes;
# - every allocation has a caller chain, and the chains add up to the count.
# NELISP_ALLOC_SITES_SCRIPT runs another copy of the gdb script, which is how
# a broken copy was shown to fail here.

set -eu

smoke_root=$(cd "$(dirname "$0")/.." && pwd)
cd "$smoke_root"

if ! command -v gdb >/dev/null 2>&1; then
    echo "GATE-SKIP nelisp-alloc-sites-smoke needs gdb"
    exit 0
fi
smoke_bin=${NELISP_BIN:-target/nelisp}
if [ ! -x "$smoke_bin" ]; then
    echo "nelisp-alloc-sites-smoke: no binary at $smoke_bin (make standalone-reader)" >&2
    exit 1
fi

smoke_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-alloc-sites-smoke.XXXXXX")
trap 'rm -rf "$smoke_dir"' EXIT

cat > "$smoke_dir/windows.el" <<'EOF'
;;; windows.el --- alloc-sites smoke fixture -*- lexical-binding: t; -*-
(defvar as-global 1)
(defun as-read (n)
  (let ((i 0) (sink nil)) (while (< i n) (setq sink as-global) (setq i (1+ i)))))
(as-read 5)
;; One top-level form per window: between separate forms the reader parses
;; the next one, and that parsing would be counted in the window too.
(progn (nelisp--debug-switch 24) (nelisp--debug-switch 25))
(progn (nelisp--debug-switch 24) (as-read 0) (nelisp--debug-switch 25))
(progn (nelisp--debug-switch 24) (as-read 10) (nelisp--debug-switch 25))
(progn (nelisp--debug-switch 24) (as-read 20) (nelisp--debug-switch 25))
(progn (nelisp--debug-switch 24) (make-vector 40 nil) (nelisp--debug-switch 25))
(princ "ALLOC-SITES-FIXTURE-DONE\n")
EOF

set +e
NELISP_BIN=$smoke_bin tools/ai/nelisp-ai.sh alloc-sites --report "$smoke_dir/report.json" \
    "$smoke_dir/windows.el" > "$smoke_dir/out.txt" 2>&1
smoke_rc=$?
set -e
cat "$smoke_dir/out.txt"
if [ "$smoke_rc" -ne 0 ]; then
    echo "nelisp-alloc-sites-smoke: alloc-sites exited with status $smoke_rc" >&2
    exit 1
fi
if ! grep -q '^ALLOC-SITES-FIXTURE-DONE' "$smoke_dir/out.txt"; then
    echo "nelisp-alloc-sites-smoke: the fixture did not run to its end" >&2
    exit 1
fi

python3 - "$smoke_dir/report.json" <<'EOF'
import json
import sys

with open(sys.argv[1]) as handle:
    report = json.load(handle)
windows = report["windows"]


def fail(message):
    sys.exit("nelisp-alloc-sites-smoke: " + message)


if report["errors"]:
    fail("errors: %s" % report["errors"])
if len(windows) != 5:
    fail("expected 5 windows, got %d" % len(windows))
for window in windows:
    counted = sum(window["counters"].values())
    if not window["complete"] or counted != window["count"]:
        fail("window %d recorded %d allocations, the allocator counted %d"
             % (window["index"], window["count"], counted))
    if sum(chain["count"] for chain in window["chains"]) != window["count"]:
        fail("window %d: chains do not add up to its count" % window["index"])
    if sum(site["count"] for site in window["sites"]) != window["count"]:
        fail("window %d: callers do not add up to its count" % window["index"])
    if any(not chain["frames"] for chain in window["chains"]):
        fail("window %d has an allocation without a caller" % window["index"])
empty, zero, ten, twenty, vector = windows
if ten["count"] <= zero["count"]:
    fail("10 reads allocated %d, no more than 0 reads (%d)" % (ten["count"], zero["count"]))
if twenty["count"] - ten["count"] != ten["count"] - zero["count"]:
    fail("windows bleed: 0, 10 and 20 reads allocated %d, %d and %d"
         % (zero["count"], ten["count"], twenty["count"]))
if vector["bytes"] < 320:
    fail("the 40-element vector window asked for only %d bytes" % vector["bytes"])
print("ALLOC-SITES-SMOKE-PASS")
EOF
