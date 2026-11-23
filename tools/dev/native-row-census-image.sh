#!/bin/bash
# native-row-census-image.sh REPO BINARY DUMP [BASELINE_FILE]
#
# Image-level native-row gate.  Boots BINARY from the image DUMP (--cold-load-from, then
# nemacs-runtime-dump-reinit), runs tools/dev/native-row-census-image.el (fixed warm-up calls,
# then a census of every row by the current closure record) and prints
#   native-row-census-image: rows=N native=N vm=N norec=N   plus a `VM NAME' line per vm row.
# Exit 0 iff vm <= the number in BASELINE_FILE (default tools/dev/native-row-census-image.baseline,
# first integer on the first non-comment line); 1 when vm exceeds it; 2 when the census did not
# complete.  Growth is the failure: lower the baseline when vm drops.  Complements
# tools/ai/native-row-census.sh, which checks the stored rows against the sources, not the image.
# Env: NRCI_CPU (taskset cpu list), NRCI_TIMEOUT (seconds, default 300).
set -u
REPO=${1:?usage: native-row-census-image.sh REPO BINARY DUMP [BASELINE_FILE]}
BIN=${2:?missing BINARY}; DUMP=${3:?missing DUMP}
REPO=$(cd "$REPO" && pwd) || exit 2
BASE=${4:-$REPO/tools/dev/native-row-census-image.baseline}
[ -x "$BIN" ] && [ -f "$DUMP" ] && [ -f "$BASE" ] || { echo "native-row-census-image: missing binary, dump or baseline" >&2; exit 2; }
limit=$(grep -v '^[[:space:]]*#' "$BASE" | grep -Eo '[0-9]+' | head -1)
[ -n "$limit" ] || { echo "native-row-census-image: no number in $BASE" >&2; exit 2; }
ulimit -v 8388608
out=$(mktemp) || exit 2
EL="$REPO/tools/dev/native-row-census-image.el"
FORM="(progn (nemacs-runtime-dump-reinit) (load \"$EL\" nil t t) (kill-emacs 0))"
(cd "$REPO" && NELISP_HOME=${NELISP_HOME:-$REPO} timeout "${NRCI_TIMEOUT:-300}" ${NRCI_CPU:+taskset -c $NRCI_CPU} \
   "$BIN" --cold-load-from "$DUMP" --eval "$FORM" < /dev/null > "$out" 2>&1)
rc=$?
line=$(grep '^native-row-census-image:' "$out")
if [ -z "$line" ]; then
  echo "native-row-census-image: did not complete (rc=$rc); last lines:" >&2; tail -5 "$out" >&2
  rm -f "$out"; exit 2
fi
grep -E '^(native-row-census-image:|VM )' "$out"
vm=$(echo "$line" | sed -n 's/.* vm=\([0-9]*\).*/\1/p')
rm -f "$out"
if [ "$vm" -gt "$limit" ]; then
  echo "native-row-census-image: FAIL vm=$vm > baseline $limit"; exit 1
fi
echo "native-row-census-image: OK vm=$vm <= baseline $limit"
