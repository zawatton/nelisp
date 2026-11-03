#!/bin/bash
# Doc 238 A0.2: the census table recorded on the census build (21c) covers the
# eight ON-driver rows (startup and workload sections), the reader probe and the
# print differential; the census build's driver results and standalone-check
# failure set equal build18's (the census does not change behaviour).
D=$(cd "$(dirname "$0")" && pwd)/b21c; I=$HOME/.cache/tmp/nelisp-sbcl-impl-builds/build-on21c/nelisp-dynamic
[ "$(sha256sum "$I" | cut -d' ' -f1)" = "$(cat "$D/image.sha256")" ] || exit 1
grep -q '^nested-function-call2 rc=0 route=named-fx-default result=' "$D/rows.txt" || exit 1
# nested-function-call2 runs through nested-primary-on.el, which has no census hook.
for r in integer-sum function-call2 vector-write vector-read vector-allocate64 string-slice16 string-concat16 hash-get1024; do
  grep -q "^$r rc=0 route=named-fx-default result=" "$D/rows.txt" || exit 1
  for ph in startup workload; do grep -qP "^$r\t$ph\tnl_word_data\t" "$D/census-rows.tsv" || exit 1; done
done
grep -qP '^CENSUS reader\tnl_word_data\tlegacy-view' "$D/census-reader.txt" || exit 1
grep -qP '^CENSUS print\tnl_word_data\t' "$D/census-print.txt" || exit 1
grep -q '^rc=' "$D/gates-summary.txt" && cmp -s "$D/gates.fail" "$D/gates-b18.fail"
