;;; cases.el --- known answers for the fallback classifier  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Six handlers whose classification is known, so the classifier can be
;; shown to answer correctly rather than only ever producing an aggregate
;; nobody can check.  Run by `make fallback-inventory-selftest', which
;; points the scanner here instead of at the tree.
;;
;; This exists because the aggregate was wrong in both directions and
;; nothing noticed: 23 handlers that re-raise or print to stderr were
;; counted as silent, and 18 that mention a logger which writes only under
;; a profiling flag were counted as innocent.  A number nobody can check is
;; a number nobody checks -- it sat at 91 for as long as it existed.
;;
;; Expected: silent-fallback 3, ignore-errors 1, bare-handler 1, dbg-note 0.
;; Nothing here is loaded or called; it is read.

;;; Code:

;; Silent: swallows the error and answers nil.
(defun fx-silent-nil ()
  (condition-case e (fx-thing) (error nil)))

;; Not silent: raises a MORE specific error through a two-line wrapper
;; around `signal'.  Counting these cost 13 false positives in
;; nelisp-jit-strategy.el.
(defun fx-reraise-wrapper ()
  (condition-case e (fx-thing) (error (nelisp--signal-wrong-type 'a e))))

;; Not silent: reports to stderr through this tree's own printer.
(defun fx-reports-stderr ()
  (condition-case e (fx-thing) (error (nelisp-artifact--print-error "x"))))

;; Silent: the logger it mentions writes only when
;; `nelisp-artifact-profile-stages' is set, which it is not in a normal
;; run.  Mentioning a logger that says nothing is not a trace.
(defun fx-conditional-logger ()
  (condition-case e (fx-thing) (error (nelisp-artifact--profile-log "s" 0))))

;; Both: the error object is discarded before anyone could record it.
(defun fx-bare-and-silent ()
  (condition-case nil (fx-thing) (error nil)))

;; Counted apart, because it is often a deliberate probe.
(defun fx-ignore-errors ()
  (ignore-errors (fx-thing)))

;;; cases.el ends here
