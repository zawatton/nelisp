#!/bin/sh
# Usage: REGION_IMAGE=/path/to/image tools/bench/region/check.sh [--negative]
# Runs probe.el on the image and diffs against expected.txt (exit 0 iff equal).
# With --negative, diffs against a deliberately corrupted expectation and
# exits 0 iff that diff FAILS (proves the comparison can go red).
D=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$D/../../.." && pwd)
IMAGE=${REGION_IMAGE:?set REGION_IMAGE to the built image}
T=$(mktemp -d) || exit 2
trap 'rm -rf "$T"' EXIT
(ulimit -v 8388608; ulimit -c 0; cd "$ROOT" && timeout 600 "$IMAGE" --load tools/bench/region/probe.el < /dev/null >"$T/out" 2>"$T/err")
echo "probe exit=$?"
if [ "$1" = "--negative" ]; then
  sed '1s/$/ CORRUPTED/' "$D/expected.txt" >"$T/bad"
  if diff -u "$T/bad" "$T/out" >/dev/null; then echo "NEGATIVE CONTROL FAILED: corrupted expectation matched"; exit 1; fi
  echo "negative control ok: corrupted expectation rejected"; exit 0
fi
diff -u "$D/expected.txt" "$T/out" || { head -5 "$T/err"; exit 1; }
echo "region probe: identical"
