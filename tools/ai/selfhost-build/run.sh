#!/usr/bin/env bash
# Usage: run.sh PROBE [ARG...]  -- run tools/ai/selfhost-build/PROBE.el on
# $SELFHOST_BIN from the repository root under the 8 GiB cap and print its
# output; exit 0 iff the output contains PROBE-<NAME>-PASS.
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)" || exit 1
root="$(cd "$here/../../.." && pwd)" || exit 1
probe="${1:?usage: run.sh PROBE [ARG...]}"; shift
[ -x "${SELFHOST_BIN:-}" ] || { echo "SELFHOST_BIN is not an executable"; exit 2; }
out="$(cd "$root" && ulimit -v 8388608 && ulimit -c 0 && \
  timeout "${PROBE_TIMEOUT:-50}" "$SELFHOST_BIN" --load "$here/$probe.el" -- "$@" 2>&1)"
printf '%s\n' "$out" | grep -E '^(OK|FAIL|PROBE-)' | head -40
tag="PROBE-$(printf '%s' "$probe" | sed 's/^probe-//' | tr a-z A-Z)-PASS"
printf '%s\n' "$out" | grep -q "^$tag" 
