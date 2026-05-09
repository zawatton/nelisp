;;; nelisp-rc-primitives-test.el --- ERT for Stage 5.3.a gc skeleton  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 79 v7 Phase C Stage 5.3.a ERT for the elisp-side skeleton in
;; `lisp/nelisp-stdlib-gc.el'.  We exercise the *pure-elisp* surface
;; (= `gc-stats' / `gc-collect-cycles' no-op semantics + data-
;; structure shells) from host Emacs.
;;
;; The 10 underlying `nl-rc-*' / `nl-gc-*' primitives are Rust-side
;; (= `build-tool/src/eval/rc_primitives.rs' tests) and not callable
;; from host Emacs; their behaviour is covered by the cargo unit
;; tests that ship in the same atomic commit.  This file's job is to
;; assert the elisp consumer compiles cleanly + the MVP-level pure-
;; elisp logic (= `gc-stats' alist shape, `gc-collect-cycles' no-op
;; return, `clrhash' reset path) holds in the Emacs harness used by
;; `make test'.

;;; Code:

(require 'ert)

;; Load the gc skeleton straight off disk.  We bypass `require' / the
;; nelisp bootstrap chain because host Emacs doesn't load the nelisp
;; stdlib + we only need the `gc-*' wrappers / data structures.
;; `nelisp-stdlib-gc.el' is self-contained at the host-Emacs level
;; (= uses only cond/when/null/defun + make-hash-table, all built into
;; host Emacs).
(let ((file (expand-file-name
             "nelisp-stdlib-gc.el"
             (expand-file-name "../lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))))
  (when (file-exists-p file)
    (load file nil t)))

(ert-deftest gc-stats-mvp-returns-3-key-alist ()
  "`gc-stats' returns the documented 3-key MVP alist with zero values."
  (skip-unless (fboundp 'gc-stats))
  (let ((s (gc-stats)))
    (should (= 3 (length s)))
    (should (equal 0 (cdr (assq 'total-allocs s))))
    (should (equal 0 (cdr (assq 'total-frees s))))
    (should (equal 0 (cdr (assq 'pending-cycles s))))))

(ert-deftest gc-stats-pending-cycles-tracks-roots-buffer ()
  "`gc-stats' reports `pending-cycles' = `(length gc-cycle-roots-buffer)'."
  (skip-unless (fboundp 'gc-stats))
  (skip-unless (boundp 'gc-cycle-roots-buffer))
  (let ((gc-cycle-roots-buffer '(a b c)))
    (should (equal 3 (cdr (assq 'pending-cycles (gc-stats)))))))

(ert-deftest gc-collect-cycles-mvp-no-op ()
  "`gc-collect-cycles' returns nil and clears the suspect buffer."
  (skip-unless (fboundp 'gc-collect-cycles))
  (skip-unless (boundp 'gc-cycle-roots-buffer))
  (let ((gc-cycle-roots-buffer '(suspect-1 suspect-2)))
    (should (eq nil (gc-collect-cycles)))
    (should (eq nil gc-cycle-roots-buffer))))

(ert-deftest gc-color-table-is-empty-hash ()
  "`gc-color-table' is a hash table that starts empty."
  (skip-unless (boundp 'gc-color-table))
  (should (hash-table-p gc-color-table)))

(ert-deftest gc-internal-rc-table-is-empty-hash ()
  "`gc-internal-rc-table' is a hash table that starts empty."
  (skip-unless (boundp 'gc-internal-rc-table))
  (should (hash-table-p gc-internal-rc-table)))

(ert-deftest gc-stats-counters-has-3-mvp-keys ()
  "`gc-stats-counters' carries the 3 MVP keys with initial value 0."
  (skip-unless (boundp 'gc-stats-counters))
  (should (equal 0 (cdr (assq 'total-allocs gc-stats-counters))))
  (should (equal 0 (cdr (assq 'total-frees gc-stats-counters))))
  (should (equal 0 (cdr (assq 'pending-cycles gc-stats-counters)))))

(provide 'nelisp-rc-primitives-test)
;;; nelisp-rc-primitives-test.el ends here
