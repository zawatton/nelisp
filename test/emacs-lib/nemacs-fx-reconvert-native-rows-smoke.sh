#!/bin/sh
# Smoke for `nemacs-runtime-image-preload--reconvert-native-rows' (F284/F285, F293).
# A row function whose word-VM record was compiled before its callees existed
# (F284: nconc before cddr) is not converted to its native row; the pass
# compiles it again and converts it, and a row that calls it links to the new
# record.  The fixture is `delete' and `delete-dups' (which calls `delete'):
# nconc is a native subr since the 9d92ebc17 row regeneration.  The stale state is forced here so the check does not depend on boot
# order.  (The stale `none' of `nelisp--rris' needs the regexp library of a
# replayed image and is checked by the regfix probes; the generic stale-none
# retry is covered by nemacs-fx-retry-stale-none-smoke.sh.)
#  1. Fix check: retry-stale-fx then reconvert -> delete and delete-dups native,
#     no converted row lost (superset), delete-dups still correct.
#  2. Discriminating control (NEMACS_RECONV_MODE=retry-only): the SAME stale
#     state with ONLY the pre-existing retry-stale-fx pass must still FAIL, so
#     only the new reconvert pass can have made check 1 pass.
#  3. Restore on failure: when the re-registration of a converted row fails,
#     the old record and the converted membership survive.  Control: the same
#     scenario against the pre-fix reconvert-row (commit 103fc8509) loses it.
# Usage: NELISP_HOME=ROOT NELISP_BIN=BIN sh this
set -u
bin=${NELISP_BIN:?}; root=${NELISP_HOME:?}
here=$(cd "$(dirname "$0")/../.." && pwd)
tmp=$(mktemp -d) || exit 2
trap 'rm -rf "$tmp"' EXIT HUP INT TERM
cat >"$tmp/t.el" <<EOT
;;; -*- lexical-binding: t; -*-
(load "$here/scripts/nemacs-runtime-image-preload.el" nil t t)
(defun rc-body (f) (nthcdr 3 (nelisp--closure-cons (symbol-function f))))
;; convert writes -(16+ID): ID 0 gives -16, so the bound is inclusive.
(defun rc-native-p (f)
  (let ((r (gethash (rc-body f) nelisp--fx-table)))
    (and (vectorp r) (integerp (aref r 1)) (<= (aref r 1) -16) (memq f nelisp-nc--converted) t)))
;; Force the stale state: a plain VM record, not converted.
(dolist (f '(delete delete-dups))
  (setq nelisp-nc--converted (delq f nelisp-nc--converted))
  (remhash (rc-body f) nelisp--fx-table) (remhash (rc-body f) nelisp--fx-pending)
  (nelisp-nc--orig-register-fn f (symbol-function f)))
(let* ((mode (or (getenv "NEMACS_RECONV_MODE") "full"))
       (before (list (rc-native-p 'delete) (rc-native-p 'delete-dups)))
       (conv0 (copy-sequence nelisp-nc--converted))
       (n (progn
            (nemacs-runtime-image-preload--retry-stale-fx)
            (unless (equal mode "retry-only")
              (nemacs-runtime-image-preload--reconvert-native-rows)))))
  (princ (format "stale-before=%s delete-native=%s dups-native=%s dups-ok=%s reconverted=%s superset=%s\n"
                 (equal before '(nil nil)) (rc-native-p 'delete) (rc-native-p 'delete-dups)
                 (equal (delete-dups (list 1 2 1 3)) '(1 2 3)) (and (integerp n) (>= n 1))
                 (let ((ok t)) (dolist (c conv0) (unless (memq c nelisp-nc--converted) (setq ok nil))) ok))))
EOT
# Scenario 3: needs a converted native row first (passes of scenario 1), then a failing re-registration.
mk3() { # \$1 = file with reconvert-row definition to load last (empty = the current one)
cat >"$tmp/r.el" <<EOT
;;; -*- lexical-binding: t; -*-
(load "$here/scripts/nemacs-runtime-image-preload.el" nil t t)
(defun rc-body (f) (nthcdr 3 (nelisp--closure-cons (symbol-function f))))
(dolist (f '(delete delete-dups))
  (setq nelisp-nc--converted (delq f nelisp-nc--converted))
  (remhash (rc-body f) nelisp--fx-table) (remhash (rc-body f) nelisp--fx-pending)
  (nelisp-nc--orig-register-fn f (symbol-function f)))
(nemacs-runtime-image-preload--retry-stale-fx)
(nemacs-runtime-image-preload--reconvert-native-rows)
$1
(let* ((body (rc-body 'delete-dups))
       (old (gethash body nelisp--fx-table))
       (orig (symbol-function 'nelisp--fx-register))
       (r 'unset))
  (fset 'nelisp--fx-register (lambda (&rest _) (error "boom")))
  (setq r (condition-case nil (nemacs-runtime-image-preload--reconvert-row 'delete-dups) (error 'signalled)))
  (fset 'nelisp--fx-register orig)
  (princ (format "was-native=%s restored=%s converted=%s result=%s\n"
                 (and (vectorp old) (integerp (aref old 1)) (<= (aref old 1) -16) t)
                 (eq (gethash body nelisp--fx-table) old)
                 (and (memq 'delete-dups nelisp-nc--converted) t) r)))
EOT
}
run() { NELISP_HOME=$root "$bin" --load "$1" 2>&1 </dev/null | head -1; }
fail=0
want="stale-before=t delete-native=t dups-native=t dups-ok=t reconverted=t superset=t"
out=$(run "$tmp/t.el")
if [ "$out" = "$want" ]; then echo "PASS reconvert-native-rows"; else echo "FAIL reconvert ($out)"; fail=1; fi
neg=$(NEMACS_RECONV_MODE=retry-only run "$tmp/t.el")
case "$neg" in
  *"delete-native=nil"*) echo "PASS control-retry-only-still-stale ($neg)";;
  *) echo "FAIL control-retry-only-not-stale ($neg)"; fail=1;;
esac
mk3 ""
want3="was-native=t restored=t converted=t result=nil"
o3=$(run "$tmp/r.el")
if [ "$o3" = "$want3" ]; then echo "PASS restore-on-failure"; else echo "FAIL restore-on-failure ($o3)"; fail=1; fi
# Control: load the pre-fix reconvert-row (103fc8509) over the current one; the record must be lost.
git -C "$here" show 103fc8509:scripts/nemacs-runtime-image-preload.el >"$tmp/prefix.el" 2>/dev/null
mk3 "(load \"$tmp/prefix.el\" nil t t)"
c3=$(run "$tmp/r.el")
case "$c3" in
  "$want3"|"") echo "FAIL control-prefix-not-red ($c3)"; fail=1;;
  *) echo "PASS control-prefix-loses-record ($c3)";;
esac
exit $fail
