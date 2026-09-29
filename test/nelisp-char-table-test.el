;;; nelisp-char-table-test.el --- host-Emacs control for Doc 186 char-tables  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 186 (docs/design/186-char-tables.org) adds a thin Elisp-visible
;; constructor/accessor layer -- `make-char-table', `char-table-p',
;; `char-table-subtype', `char-table-parent'/`set-char-table-parent',
;; `char-table-extra-slot'/`set-char-table-extra-slot', `char-table-
;; range'/`set-char-table-range' -- entirely inside `scripts/nelisp-
;; standalone-build.el's `bf_*' native dispatch (see the "Doc 186
;; P0/P1/P2" block there).  There is no elisp-level `defun' for any of
;; these names anywhere in this tree: they exist ONLY as compiled
;; dispatch arms in the standalone binary, so there is nothing here to
;; `load'/`fmakunbound'/shadow the way test/nelisp-hooks-map-fixnum-
;; test.el has to for prelude-level names that collide with host
;; builtins.
;;
;; What this file DOES do: every `should' below runs against host
;; Emacs's OWN native char-table implementation (already present,
;; unrelated to NeLisp) and pins the exact values/errors the standalone
;; implementation was built to match -- the "host-Emacs behavior
;; control" the Doc 186 implementation's against-the-bug evidence leans
;; on.  Every value asserted here was independently verified against a
;; real Emacs 30.1 batch run before being written into `scripts/nelisp-
;; standalone-build.el's `bf_*' arms (see that file's own char-table
;; block comments for the same cross-checks, e.g. `char-table-range' on
;; a (FROM . TO) cons returning the value AT FROM, or the exact
;; `(wrong-type-argument char-table-p OBJ)' shape).
;;
;; The actual against-the-bug check for NeLisp's own implementation --
;; void-function `make-char-table' before this branch, working after --
;; is `nelisp-standalone--reader-char-table-smoke' (scripts/nelisp-
;; standalone-build.el), run against the built `target/nelisp' binary
;; via `make standalone-reader-test' (wired into that gate's dolist).
;; This file cannot exercise that path: `make-char-table' is not
;; `fboundp' anywhere host Emacs would look for a NeLisp definition to
;; load, and does not need to be -- host Emacs's own native char-table
;; is the ground truth these tests pin.

;;; Code:

(require 'ert)

(ert-deftest nelisp-char-table/construct-and-aref ()
  "Doc 186 §6.1: construct + `char-table-p' + default-value `aref'."
  (should (char-table-p (make-char-table 'test nil)))
  (should (eq 'DEFAULT (aref (make-char-table 'test 'DEFAULT) ?a))))

(ert-deftest nelisp-char-table/aset-then-aref ()
  "Doc 186 §6.1: `aset' then `aref' round-trips; a miss falls to default."
  (let ((tbl (make-char-table 'test nil)))
    (aset tbl ?a 1)
    (should (eq 1 (aref tbl ?a)))
    (should (eq nil (aref tbl ?b)))))

(ert-deftest nelisp-char-table/parent-fallback ()
  "Doc 186 §6.1/P1: an unset char falls through to the parent table."
  (let ((parent (make-char-table 'test 'from-parent))
        (child (make-char-table 'test nil)))
    (set-char-table-parent child parent)
    (should (eq 'from-parent (aref child ?z)))))

(ert-deftest nelisp-char-table/parent-accessor ()
  "P1: `char-table-parent' reads back what `set-char-table-parent' wrote,
and nil clears the link."
  (let ((parent (make-char-table 'test 'from-parent))
        (child (make-char-table 'test nil)))
    (should (eq nil (char-table-parent child)))
    (set-char-table-parent child parent)
    (should (eq parent (char-table-parent child)))
    (set-char-table-parent child nil)
    (should (eq nil (char-table-parent child)))
    (should (eq nil (aref child ?z)))))

(ert-deftest nelisp-char-table/subtype ()
  "P1: `char-table-subtype' returns the symbol `make-char-table' was
constructed with."
  (should (eq 'my-subtype (char-table-subtype (make-char-table 'my-subtype nil)))))

(ert-deftest nelisp-char-table/extra-slot ()
  "P1: extra-slot get/set round-trips; an out-of-range N signals
`args-out-of-range'.  The `char-table-extra-slots' property has to be
registered on SUBTYPE *before* `make-char-table' -- Emacs reads it at
construction time to size the extra-slot vector (measured: setting it
afterward left slot 0 out of range)."
  (put 'test-with-extras 'char-table-extra-slots 5)
  (let ((tbl (make-char-table 'test-with-extras nil)))
    (should (eq nil (char-table-extra-slot tbl 0)))
    (set-char-table-extra-slot tbl 0 'hi)
    (should (eq 'hi (char-table-extra-slot tbl 0)))
    (should-error (char-table-extra-slot tbl 20) :type 'args-out-of-range)))

(ert-deftest nelisp-char-table/range-single-and-cons ()
  "P2, verified against host Emacs: `char-table-range' on a single char
is `aref'; on a (FROM . TO) cons it returns the value AT FROM, even
when the range is not uniform."
  (let ((tbl (make-char-table 'test nil)))
    (aset tbl ?a 5) (aset tbl ?b 2) (aset tbl ?c 9)
    (should (eq 5 (char-table-range tbl ?a)))
    (should (eq 5 (char-table-range tbl (cons ?a ?c))))
    (should (eq 2 (char-table-range tbl (cons ?b ?c))))))

(ert-deftest nelisp-char-table/set-range-cons ()
  "P2: `set-char-table-range' over a (FROM . TO) cons sets every char in
the (inclusive) span and returns VALUE."
  (let ((tbl (make-char-table 'test nil)))
    (should (eq 42 (set-char-table-range tbl (cons ?a ?c) 42)))
    (should (eq 42 (aref tbl ?a)))
    (should (eq 42 (aref tbl ?b)))
    (should (eq 42 (aref tbl ?c)))))

(ert-deftest nelisp-char-table/range-nil-is-default-slot ()
  "GNU semantics, verified against host Emacs: RANGE = nil reads/writes the
default slot.  `make-char-table' fills every slot with INIT, so changing the
default does not change what an untouched char answers; it only shows
through slots that hold nil."
  (let ((tbl (make-char-table 'test 'DEF)))
    (should (eq 'DEF (char-table-range tbl nil)))
    (should (eq 'NEWDEF (set-char-table-range tbl nil 'NEWDEF)))
    (should (eq 'NEWDEF (char-table-range tbl nil)))
    (should (eq 'DEF (aref tbl ?z)))
    (aset tbl ?z nil)
    (should (eq 'NEWDEF (aref tbl ?z)))))

(ert-deftest nelisp-char-table/huge-ranges-have-no-ceiling ()
  "The 4096-entry ceiling is gone: any range is one interval operation."
  (let ((tbl (make-char-table 'test nil)))
    (should (eq 'p (set-char-table-range tbl (cons #x100 4194303) 'p)))
    (should (eq nil (aref tbl #xff)))
    (should (eq 'p (aref tbl #x100)))
    (should (eq 'p (aref tbl 4194303)))
    (set-char-table-range tbl (cons 5000 9000) 'q)
    (should (eq 'p (aref tbl 4999)))
    (should (eq 'q (aref tbl 5000)))
    (should (eq 'q (aref tbl 9000)))
    (should (eq 'p (aref tbl 9001)))
    (should (eq 7 (set-char-table-range tbl t 7)))
    (should (eq 7 (aref tbl 0)))
    (should (eq 7 (aref tbl 4194303)))))

(ert-deftest nelisp-char-table/range-errors ()
  (let ((tbl (make-char-table 'test nil)))
    (should-error (set-char-table-range tbl (cons 0 4194304) 1)
                  :type 'wrong-type-argument)
    (should-error (aref tbl -1) :type 'wrong-type-argument)
    (should-error (char-table-range tbl 'a) :type 'error)
    (should (eq 1 (set-char-table-range tbl (cons 9 5) 1)))
    (should (eq nil (aref tbl 7)))))

(ert-deftest nelisp-char-table/wrong-type-argument ()
  "A non-char-table signals `(wrong-type-argument char-table-p OBJ)' --
the exact predicate-name shape `bf_wrong_type_char_table_p' in
scripts/nelisp-standalone-build.el was built to match."
  (should-error (char-table-parent 5) :type 'wrong-type-argument)
  (should (equal (should-error (char-table-parent 5) :type 'wrong-type-argument)
                 '(wrong-type-argument char-table-p 5))))

(ert-deftest nelisp-char-table/wrong-number-of-arguments ()
  "SUBTYPE is mandatory; `(make-char-table)' signals cleanly rather than
misbehaving -- the shape `bf_wrong_number_of_args_make_char_table' in
scripts/nelisp-standalone-build.el was built to match."
  (should-error (make-char-table) :type 'wrong-number-of-arguments))

(provide 'nelisp-char-table-test)

;;; nelisp-char-table-test.el ends here
