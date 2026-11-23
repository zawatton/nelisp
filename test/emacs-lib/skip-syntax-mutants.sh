#!/bin/bash
# Mutation procedure for the skip-syntax fastmap cache (F293 review M1-M3b).
# Builds an overlay from the CURRENT source (the defvars + defuns from
# `emacs-syntax-table--skip-map-cache' up to the install block), applies one
# mutant at a time with an exact-text replacement (a mutant that does not change
# the overlay aborts), loads it over the baked image and runs the skip-syntax
# ERT tests.  Expected: BASE green; every mutant RED (at least one FAIL).
# No bake or build is needed.  Usage: NELISP_HOME=LANE sh this  (LANE has target/nelisp and
# build/nemacs-runtime.nlri).  Exit 0 only when BASE is green and all mutants are red.
set -u
root=${NELISP_HOME:?}; here=$(cd "$(dirname "$0")/../.." && pwd)
src=$here/packages/emacs-lib-core/src/emacs-syntax-table.el
tst=$here/packages/emacs-lib-core/test/emacs-syntax-table-test.el
tmp=$(mktemp -d) || exit 2; trap 'rm -rf "$tmp"' EXIT HUP INT TERM
awk '/^\(defvar emacs-syntax-table--skip-map-cache/{on=1} /^\(when \(and \(fboundp .nelisp--skip-syntaxes\)/{on=0} on' "$src" >"$tmp/base.el"
[ -s "$tmp/base.el" ] || { echo "FAIL overlay extraction"; exit 2; }
names=$(grep -oE '^\(ert-deftest emacs-syntax-table-test/skip-syntax[^ ]*' "$tst" | awk '{print $2}' | tr '\n' ' ')
cat >"$tmp/r.el" <<EOT
(load "$tst" nil t t)
(let ((pass 0) (fail 0))
  (dolist (tn '($names))
    (let ((r (condition-case e (progn (funcall (ert-test-body (ert-get-test tn))) 'PASS) (error (list 'FAIL e)))))
      (if (eq r 'PASS) (setq pass (1+ pass)) (setq fail (1+ fail)) (princ (format "FAIL %s\n" tn)))))
  (princ (format "ERT pass=%d fail=%d\n" pass fail)))
EOT
run() { # overlay -> prints "ERT pass=N fail=M" plus FAIL lines
  ( cd "$root"; ulimit -v 8388608
    NELISP_HOME=$root timeout 900 taskset -c 9,25 target/nelisp exec-runtime-image build/nemacs-runtime.nlri \
      "(progn (load \"$1\" nil t t) (load \"$tmp/r.el\" nil t t))" </dev/null 2>&1 | grep -E '^(FAIL|ERT)' ) }
mutant() { # name, exact old text, exact new text
  python3 - "$tmp/base.el" "$tmp/$1.el" "$2" "$3" <<'PY' || return 1
import sys
s=open(sys.argv[1]).read(); o=sys.argv[3]; n=sys.argv[4]
if s.count(o)!=1: sys.exit("mutant text not found exactly once: "+o)
open(sys.argv[2],'w').write(s.replace(o,n,1))
PY
}
rc=0
b=$(run "$tmp/base.el"); echo "BASE: $(echo "$b" | tail -1)"
echo "$b" | grep -q 'fail=0' || { echo "FAIL base not green"; echo "$b"; rc=1; }
chk() { r=$(run "$tmp/$1.el"); if echo "$r" | grep -q 'fail=0'; then echo "SURVIVED $1 ($2)"; rc=1; else echo "RED $1 ($2): $(echo "$r" | grep '^FAIL' | sed 's/FAIL emacs-syntax-table-test\///' | tr '\n' ' ')"; fi; }
mutant M1 "(setq emacs-syntax-table--skip-last-syntax (copy-sequence syntax)" "(setq emacs-syntax-table--skip-last-syntax syntax" && chk M1 "last-syntax aliases the caller string"
mutant M2 "(puthash (copy-sequence syntax) map" "(puthash syntax map" && chk M2 "hash key not copied"
mutant M3a "(clrhash emacs-syntax-table--skip-map-cache)" "nil" && chk M3a "bound never enforced"
mutant M3b "(>= (hash-table-count emacs-syntax-table--skip-map-cache) 64)" "(>= (hash-table-count emacs-syntax-table--skip-map-cache) 1)" && chk M3b "cleared on every miss"
mutant M4 "(setq map (make-vector 256 neg))" "(setq map (make-vector 256 nil))" && chk M4 "leading ^ not complemented"
exit $rc
