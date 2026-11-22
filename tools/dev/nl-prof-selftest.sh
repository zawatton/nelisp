#!/usr/bin/env bash
# Self-test for nl-prof: exact call counts through recursion, catch/throw and signal.  Exit 0 = pass.
# Needs the same env as nl-prof.sh (NL_PROF_BIN, NL_PROF_DUMP).
here=$(cd "$(dirname "$0")" && pwd)
out=$("$here/nl-prof.sh" "$here/nl-prof-examples/syn-form.el" --setup "$here/nl-prof-examples/syn-setup.el" \
      --prefix pfs- --runs 50 --top 20 2>&1) || { echo "$out"; echo "FAIL: run"; exit 1; }
row() { echo "$out" | awk -v n="$1" '$1==n && $2 ~ /^[0-9]+$/ {print $2}'; }
fail=0
chk() { [ "$(row "$1")" = "$2" ] || { echo "FAIL: $1 calls $(row "$1") != $2"; fail=1; }; }
chk pfs-top 50; chk pfs-rec 250; chk pfs-leaf 400; chk pfs-mid 50; chk pfs-thrower 50; chk pfs-signal 50
echo "$out" | grep -q "pfs-rec -> pfs-rec" || { echo "FAIL: recursive edge missing"; fail=1; }
[ $fail = 0 ] && echo "nl-prof selftest: PASS" || { echo "$out"; exit 1; }
