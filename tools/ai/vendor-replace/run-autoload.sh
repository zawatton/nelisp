#!/usr/bin/env bash
# Usage: VENDOR_BIN=BINARY run-autoload.sh -- exit 0 iff probe-autoload.el prints PASS.
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../.." || exit 2
out=$( (ulimit -v 8388608; timeout 300 "$VENDOR_BIN" --load "$here/probe-autoload.el" -- 2>&1) )
printf '%s\n' "$out" | grep -a -m1 '^PROBE-'
printf '%s\n' "$out" | grep -aq '^PROBE-[A-Z]*-PASS'
