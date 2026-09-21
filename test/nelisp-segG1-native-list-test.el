;;; nelisp-segG1-native-list-test.el --- native nth/nthcdr/last/butlast/mapcar gate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Segment G1: `nth', `nthcdr', `last', `butlast' and `mapcar' each became
;; a reader builtin (`scripts/nelisp-standalone-build.el', the
;; `nelisp-standalone--applyfn-fast-list-helpers' dispatch arms) instead
;; of the interpreted `defun' `scripts/nelisp-stdlib-prelude.el' used to
;; run on every call.  Every case below was checked against GNU Emacs
;; 31.1 before being written in, and each is run against the actual
;; built `target/nelisp' binary the same way
;; `nelisp-append-fast-path-test.el' already does for `append', so a
;; regression in a native fast path or its interpreted fallback/delegate
;; shows up here rather than in a consumer three layers away.
;;
;; The `butlast'/`append' shape below (a NON-FINAL argument that is
;; itself an improper list two conses deep, e.g. `(1 2 . 3)') is the
;; exact one that caught a real bug in `wf_append_prepend' during
;; development: a helper that builds a fresh cons spine bottom-up fed
;; the `bf_wrong_type_listp' sentinel (a fixed return value, not a slot
;; address) into `nelisp_cons_construct' as if it were a real pointer,
;; producing a wrong answer instead of signalling.  `butlast' cannot hit
;; that shape (`bf_length_checked' validates properness before any
;; cons-spine build begins), but the case is kept here as a permanent
;; regression probe for the general failure mode.

;;; Code:

(require 'ert)

(defun nelisp-segG1-native--standalone-eval (expression)
  "Evaluate EXPRESSION on the built standalone reader and return its output."
  (let ((binary (expand-file-name "target/nelisp" default-directory)))
    (unless (file-executable-p binary)
      (ert-skip "target/nelisp is not built; standalone-reader gate owns it"))
    (with-temp-buffer
      (let ((rc (call-process binary nil t nil "--eval" expression)))
        (unless (= rc 0)
          (ert-fail (format "standalone expression failed: rc=%S output=%S"
                            rc (buffer-string))))
        (string-trim-right (buffer-string))))))

(defmacro nelisp-segG1-native--deftest (name expr expected)
  "Define an ERT test NAME asserting EXPR prints as EXPECTED on the standalone."
  `(ert-deftest ,name ()
     (should (equal (nelisp-segG1-native--standalone-eval ,expr) ,expected))))

;; -- nth -------------------------------------------------------------

(nelisp-segG1-native--deftest
 nelisp-segG1-nth-negative-index-clamps-to-zero
 "(nth -1 '(1 2 3))"
 "1")

(nelisp-segG1-native--deftest
 nelisp-segG1-nth-past-end-is-nil
 "(nth 5 '(1 2 3))"
 "nil")

(nelisp-segG1-native--deftest
 nelisp-segG1-nth-improper-list-names-the-whole-list
 "(condition-case e (nth 2 '(1 . 2)) (error e))"
 "(wrong-type-argument listp (1 . 2))")

(nelisp-segG1-native--deftest
 nelisp-segG1-nth-non-integer-signals-integerp
 "(condition-case e (nth \"a\" '(1 2)) (error e))"
 "(wrong-type-argument integerp \"a\")")

;; -- nthcdr ------------------------------------------------------------

(nelisp-segG1-native--deftest
 nelisp-segG1-nthcdr-zero-on-non-list-is-identity
 "(nthcdr 0 5)"
 "5")

(nelisp-segG1-native--deftest
 nelisp-segG1-nthcdr-negative-is-identity
 "(nthcdr -1 '(1 2 3))"
 "(1 2 3)"
 )

(nelisp-segG1-native--deftest
 nelisp-segG1-nthcdr-stopping-exactly-on-improper-tail-is-not-an-error
 "(nthcdr 1 '(1 . 2))"
 "2")

(nelisp-segG1-native--deftest
 nelisp-segG1-nthcdr-improper-list-names-the-whole-list
 "(condition-case e (nthcdr 5 '(1 2 . 3)) (error e))"
 "(wrong-type-argument listp (1 2 . 3))")

;; -- last ----------------------------------------------------------------

(nelisp-segG1-native--deftest
 nelisp-segG1-last-non-cons-is-identity
 "(last t)"
 "t")

(nelisp-segG1-native--deftest
 nelisp-segG1-last-zero-n-is-nil
 "(last '(1 2 3) 0)"
 "nil")

(nelisp-segG1-native--deftest
 nelisp-segG1-last-negative-n-is-nil
 "(last '(1 2 3) -1)"
 "nil")

(nelisp-segG1-native--deftest
 nelisp-segG1-last-n-exceeds-length-returns-whole-list
 "(last '(1 2 3) 100)"
 "(1 2 3)")

(nelisp-segG1-native--deftest
 nelisp-segG1-last-improper-list-tolerant-safe-length
 "(last '(1 . 2))"
 "(1 . 2)")

;; -- butlast ---------------------------------------------------------------

(nelisp-segG1-native--deftest
 nelisp-segG1-butlast-nonpositive-n-is-unchecked-identity
 "(butlast 5 0)"
 "5")

(nelisp-segG1-native--deftest
 nelisp-segG1-butlast-n-exceeds-length-is-nil
 "(butlast '(1 2 3) 10)"
 "nil")

(nelisp-segG1-native--deftest
 nelisp-segG1-butlast-improper-list-signals-listp
 "(condition-case e (butlast '(1 . 2)) (error e))"
 "(wrong-type-argument listp 2)"
 )

(nelisp-segG1-native--deftest
 nelisp-segG1-butlast-non-sequence-signals-sequencep
 "(condition-case e (butlast 5) (error e))"
 "(wrong-type-argument sequencep 5)")

(nelisp-segG1-native--deftest
 nelisp-segG1-butlast-two-conses-deep-improper-tail-names-the-tail
 "(condition-case e (butlast '(1 2 3 . 4)) (error e))"
 "(wrong-type-argument listp 4)")

;; -- mapcar ------------------------------------------------------------

(nelisp-segG1-native--deftest
 nelisp-segG1-mapcar-symbol-fn-resolves-and-calls
 "(mapcar #'1+ '(1 2 3))"
 "(2 3 4)")

(nelisp-segG1-native--deftest
 nelisp-segG1-mapcar-closure-fn-resolves-and-calls
 "(let ((k 10)) (mapcar (lambda (x) (+ x k)) '(1 2 3)))"
 "(11 12 13)")

(nelisp-segG1-native--deftest
 nelisp-segG1-mapcar-vector-seq-delegates-correctly
 "(mapcar #'1+ [1 2 3])"
 "(2 3 4)")

(nelisp-segG1-native--deftest
 nelisp-segG1-mapcar-string-seq-delegates-correctly
 "(mapcar #'1+ \"ab\")"
 "(98 99)")

(nelisp-segG1-native--deftest
 nelisp-segG1-mapcar-improper-list-names-the-offending-tail
 "(condition-case e (mapcar #'identity '(1 2 . 3)) (error e))"
 "(wrong-type-argument listp 3)")

(nelisp-segG1-native--deftest
 nelisp-segG1-mapcar-unbound-symbol-fn-signals-void-function
 "(condition-case e (mapcar 'no-such-fn-segg1 '(1)) (error e))"
 "(void-function no-such-fn-segg1)")

(nelisp-segG1-native--deftest
 nelisp-segG1-mapcar-preserves-call-order
 "(let (acc) (mapcar (lambda (x) (push x acc)) '(1 2 3)) (nreverse acc))"
 "(1 2 3)")

(nelisp-segG1-native--deftest
 nelisp-segG1-mapcar-empty-list-is-nil
 "(mapcar #'1+ nil)"
 "nil")

;;; nelisp-segG1-native-list-test.el ends here
