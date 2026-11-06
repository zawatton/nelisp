#!/usr/bin/env bash
# Usage: VENDOR_BIN=BINARY run.sh FEATURE LIBRARY KEYFN
# Exit 0 iff no NeLisp copy of LIBRARY.el shadows the vendored one in the
# tree and the binary serves FEATURE from its vendored GNU file.
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
cd "$root" || exit 2
shadow=$(find packages standalone-compat lisp src scripts -name "$2.el" 2>/dev/null)
if [ -n "$shadow" ]; then echo "PROBE-VENDOR-FAIL shadowed by $shadow"; exit 1; fi
out=$( (ulimit -v 8388608; timeout 300 "$VENDOR_BIN" --load "$here/probe.el" -- "$@" 2>&1) )
printf '%s\n' "$out" | grep -a -m1 '^PROBE-VENDOR-'
printf '%s\n' "$out" | grep -aq '^PROBE-VENDOR-PASS'
