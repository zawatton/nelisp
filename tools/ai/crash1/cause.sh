#!/bin/sh
# crash1 S1: demonstrate each cause on the BASELINE (v17) binary.  usage: cause.sh V17BIN WHAT
# WHAT: sort | replace | file | all.  PASS iff the documented failure is observed.
BIN=$1; WHAT=$2
W=${CRASH1_WORK:-target/crash1/w}; mkdir -p "$W"
nl() { ( ulimit -v 8388608; timeout 120 "$BIN" --batch --eval "$1" --eval '(ignore)' 2>&1 ); echo "rc=$?"; }
case $WHAT in
  sort)
    # The fixed 4 MiB BSS root stack: two [value key] frames cost 32 bytes per element.
    grep -q '(+ (data-addr nl_rootstack_region) 4194304)' lisp/nelisp-cc-rootstack.el || exit 1
    git show HEAD:lisp/nelisp-cc-sort.el | grep -q 'm5_fx_arg_words env (\* n 2)' || exit 1
    nl '(princ (length (sort (make-list 131000 1) (function <))))' | grep -q 'rc=0' || exit 1
    nl '(princ (length (sort (make-list 131072 1) (function <))))' | grep -q 'rc=139' || exit 1
    nl '(princ (length (sort (make-vector 131072 3) (function <))))' | grep -q 'rc=139' || exit 1 ;;
  replace)
    # O(n^2) garbage from the per-character concat loop: dies under the 8 GiB cap, while apply/concat itself is fine.
    nl '(princ (length (string-replace "b" "bb" (make-string 100000 ?a))))' | grep -q 'rc=0' || exit 1
    nl '(princ (length (string-replace "b" "bb" (make-string 120000 ?a))))' | grep -q 'rc=139' || exit 1
    nl '(princ (length (apply (function concat) (make-list 1000000 "ab"))))' | grep -q '2000000.*rc=0' || exit 1 ;;
  file)
    # Fixed 8 MiB scratch buffer; the same cutoff with a smaller address-space limit, so it is not the cap.
    f=$W/b1-12m.txt; head -c 12000000 /dev/zero | tr '\0' x > "$f"
    nl "(princ (with-temp-buffer (insert-file-contents \"$f\") (buffer-size)))" | grep -q '8388608.*rc=0' || exit 1
    ( ulimit -v 4194304; timeout 120 "$BIN" --batch --eval "(princ (with-temp-buffer (insert-file-contents \"$f\") (buffer-size)))" --eval '(ignore)' 2>&1 ) | grep -q '^8388608$' || exit 1
    nl "(princ (with-temp-buffer (call-process \"cat\" nil t nil \"$f\") (buffer-size)))" | grep -q '8388608.*rc=0' || exit 1
    git show HEAD:scripts/nelisp-standalone-build.el | grep -q '(alloc-bytes 8388608 1)' || exit 1 ;;
  all) for w in sort replace file; do sh "$0" "$BIN" $w || exit 1; done ;;
esac
