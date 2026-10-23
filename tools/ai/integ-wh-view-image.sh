#!/usr/bin/env bash
# integ-wh-view-image.sh [BIN]: string views held by containers survive an image dump and restore.
# Dumps a process whose cons cells hold the VIEW box of a string (not the String WORD) and a modified case table,
# restores the image and checks the strings, the case table and two collections.  Exit 0 on "VIEW-IMAGE PASS".
# Before the dump rewrote such fields to the String WORD the restored view held the dumping process's header and
# sdata addresses and the first collection crashed (SIGSEGV): the library images of the census.
cd "$(dirname "$0")/../.." || exit 2
BIN=$(realpath "${1:-${INTEG_WH_DYN:-target/integ/nelisp-dyn}}") || exit 2
O=${VIEW_IMAGE_WORK:-$PWD/target/integ-wh/view-image}; mkdir -p "$O"; rm -f "$O/view.flat"
(ulimit -v 8388608; VIEW_IMAGE_OUT=$O/view.flat timeout 300 "$BIN" --load tools/ai/integ-wh-view-image-a.el > "$O/a.out" 2> "$O/a.err")
rc=$?
grep -q '^VIEW-IMAGE-A dump [1-9]' "$O/a.out" || { echo "VIEW-IMAGE FAIL dump side rc=$rc: $(tail -n 2 "$O/a.out" "$O/a.err" | tr '\n' ' ' | cut -c1-200)"; exit 1; }
(ulimit -v 8388608; timeout 300 "$BIN" --cold-load-from "$O/view.flat" --load tools/ai/integ-wh-view-image-b.el > "$O/b.out" 2> "$O/b.err")
rc=$?
tail -n 1 "$O/b.out" | grep -q '^VIEW-IMAGE PASS' && { tail -n 1 "$O/b.out"; exit 0; }
echo "VIEW-IMAGE FAIL restore side rc=$rc: $(grep 'FAIL' "$O/b.out" | head -n 3 | tr '\n' ' ' | cut -c1-240)"; exit 1
