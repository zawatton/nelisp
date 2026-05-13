;;; nelisp-env.el --- Elisp-side env (Doc 102 Phase 2)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 102 Phase 2 — elisp-side replacement for the value /
;; function / plist / constant cells in
;; `build-tool/src/eval/env.rs's `globals' HashMap.
;;
;; Phase 2.a (this file): ship the elisp surface (= SymbolEntry
;; record + env record + the 11 globals-table operations).  No
;; routing yet — the existing Rust `env.rs::Env' is unchanged,
;; tests exercise this file standalone via the `nelisp-stdlib-
;; fast-hash' substrate.
;;
;; Phase 2.b (future session): flip the 11 `env_shim.rs' slim
;; primitives to consult an elisp-side env instead of the Rust
;; `HashMap'.  That commit ships the actual Rust LOC reduction
;; (= ~50 LOC of `globals: HashMap<String, SymbolEntry>' helpers
;; obsoleted).
;;
;; Storage layouts:
;;
;;   env record (`Sexp::Record' type-tag `nelisp-env'):
;;     slot 0 = GLOBALS  : fast-hash-table mapping string → symbol-entry
;;     slot 1 = (reserved for frames stack — Phase 3)
;;     slot 2 = (reserved for extern_builtins — Phase 6)
;;
;;   symbol-entry record (`Sexp::Record' type-tag `symbol-entry'):
;;     slot 0 = VALUE     : Sexp or `nelisp--unbound-marker'
;;     slot 1 = FUNCTION  : Sexp or `nelisp--unbound-marker'
;;     slot 2 = PLIST     : Sexp (list, default nil)
;;     slot 3 = CONSTANT  : Sexp::T / Sexp::Nil
;;
;; The `unbound-marker' sentinel is shared with the fast-hash
;; module via `lisp/nelisp-stdlib-fast-hash.el's defvar.  See Doc
;; 102 §2.1 + §5.3 for the rationale (= uninterned symbol,
;; immune to `(intern "nelisp--unbound-marker")' collision).

;;; Code:

(require 'nelisp-stdlib-fast-hash)

;; ---- Symbol entry construction + accessors ----

(defun nelisp-env--make-symbol-entry
    (&optional value function plist constant)
  "Build a fresh symbol-entry record.
VALUE / FUNCTION default to `nelisp--unbound-marker' (= the
Option::None sentinel).  PLIST defaults to nil (= empty plist).
CONSTANT defaults to nil (= mutable cell).  Doc 102 §2.1 spec."
  (nelisp--make-record 'symbol-entry
                       (or value nelisp--unbound-marker)
                       (or function nelisp--unbound-marker)
                       (or plist nil)
                       (or constant nil)))

(defun nelisp-env--symbol-entry-p (obj)
  "Return non-nil iff OBJ is a symbol-entry record."
  (and (recordp obj)
       (eq (nelisp--record-type obj) 'symbol-entry)))

(defun nelisp-env--symbol-entry-value (entry)
  "Return ENTRY's value cell (= may be the unbound-marker sentinel)."
  (nelisp--record-ref entry 0))

(defun nelisp-env--symbol-entry-function (entry)
  "Return ENTRY's function cell (= may be the unbound-marker sentinel)."
  (nelisp--record-ref entry 1))

(defun nelisp-env--symbol-entry-plist (entry)
  "Return ENTRY's plist (= proper list)."
  (nelisp--record-ref entry 2))

(defun nelisp-env--symbol-entry-constant-p (entry)
  "Return non-nil iff ENTRY is marked constant (= setq rejects writes)."
  (nelisp--record-ref entry 3))

(defun nelisp-env--symbol-entry-set-value (entry value)
  "Set ENTRY's value cell.  Returns VALUE."
  (nelisp--record-set entry 0 value)
  value)

(defun nelisp-env--symbol-entry-set-function (entry func)
  "Set ENTRY's function cell.  Returns FUNC."
  (nelisp--record-set entry 1 func)
  func)

;; ---- Env record construction + introspection ----

(defun nelisp-env-make (&optional globals-bucket-count)
  "Build a fresh empty env record.
GLOBALS-BUCKET-COUNT is forwarded to `nelisp--fast-hash-make' as
the globals table size hint (= default 1024).  Doc 102 §2.1
env record layout."
  (let ((globals (nelisp--fast-hash-make globals-bucket-count)))
    ;; Two extra slots reserved for the Phase 3+ frames stack
    ;; and Phase 6+ extern_builtins table.  Filled with nil for
    ;; now; downstream phases set them as the migration lands.
    (nelisp--make-record 'nelisp-env globals nil nil)))

(defun nelisp-env-p (obj)
  "Return non-nil iff OBJ is a nelisp-env record."
  (and (recordp obj)
       (eq (nelisp--record-type obj) 'nelisp-env)))

(defun nelisp-env--globals (env)
  "Return the globals fast-hash-table of ENV."
  (nelisp--record-ref env 0))

(defun nelisp-env-count (env)
  "Return the number of bound symbols in ENV's globals table."
  (nelisp--fast-hash-count (nelisp-env--globals env)))

;; ---- Value cell ops ----

(defun nelisp-env--ensure-entry (env name)
  "Return ENV's entry for NAME, creating an empty entry if absent.
Used by the `set' / `defvar' paths that auto-vivify."
  (let* ((globals (nelisp-env--globals env))
         (entry (nelisp--fast-hash-get globals name)))
    (unless entry
      (setq entry (nelisp-env--make-symbol-entry))
      (nelisp--fast-hash-put globals name entry))
    entry))

(defun nelisp-env-lookup-value (env name)
  "Return ENV's value cell for NAME, or signal `unbound-variable'
when the cell is absent or holds `nelisp--unbound-marker'.
Mirrors `env.rs::Env::lookup_value' semantics (Doc 102 §1.1
behaviour contract)."
  (let ((entry (nelisp--fast-hash-get (nelisp-env--globals env) name)))
    (cond
     ((null entry)
      (signal 'void-variable (list name)))
     (t
      (let ((v (nelisp-env--symbol-entry-value entry)))
        (if (eq v nelisp--unbound-marker)
            (signal 'void-variable (list name))
          v))))))

(defun nelisp-env-set-value (env name value)
  "Set ENV[NAME] = VALUE.  Returns VALUE.
Auto-vivifies the entry if absent.  Signals `setting-constant'
when the entry is flagged constant.  Mirrors
`env.rs::Env::set_value' globals-fallback path (= Phase 2.a does
not yet model the lexical frame stack; Phase 3 adds it)."
  (let* ((globals (nelisp-env--globals env))
         (existing (nelisp--fast-hash-get globals name)))
    (when (and existing
               (nelisp-env--symbol-entry-constant-p existing))
      (signal 'setting-constant (list name)))
    (let ((entry (or existing (nelisp-env--make-symbol-entry))))
      (nelisp-env--symbol-entry-set-value entry value)
      (unless existing
        (nelisp--fast-hash-put globals name entry))
      value)))

(defun nelisp-env-clear-value (env name)
  "Drop ENV[NAME]'s value cell.  Mirrors `env.rs::Env::clear_value'
/ user-visible `makunbound'.  Preserves the constant flag so a
later `defconst' re-bind still errors.  Returns nil."
  (let* ((globals (nelisp-env--globals env))
         (entry (nelisp--fast-hash-get globals name)))
    (when entry
      (nelisp-env--symbol-entry-set-value entry nelisp--unbound-marker))
    nil))

;; ---- Function cell ops ----

(defun nelisp-env-lookup-function (env name)
  "Return ENV[NAME]'s function cell, or signal `unbound-function'."
  (let ((entry (nelisp--fast-hash-get (nelisp-env--globals env) name)))
    (cond
     ((null entry)
      (signal 'void-function (list name)))
     (t
      (let ((f (nelisp-env--symbol-entry-function entry)))
        (if (eq f nelisp--unbound-marker)
            (signal 'void-function (list name))
          f))))))

(defun nelisp-env-set-function (env name func)
  "Set ENV[NAME]'s function cell to FUNC.  Mirrors `defun' /
`defalias'.  Returns FUNC."
  (let ((entry (nelisp-env--ensure-entry env name)))
    (nelisp-env--symbol-entry-set-function entry func)))

(defun nelisp-env-clear-function (env name)
  "Drop ENV[NAME]'s function cell.  Mirrors `fmakunbound'.
Returns nil."
  (let* ((globals (nelisp-env--globals env))
         (entry (nelisp--fast-hash-get globals name)))
    (when entry
      (nelisp-env--symbol-entry-set-function
       entry nelisp--unbound-marker))
    nil))

;; ---- Predicates ----

(defun nelisp-env-is-bound (env name)
  "Return non-nil iff ENV[NAME] has a value cell (= `boundp')."
  (let ((entry (nelisp--fast-hash-get (nelisp-env--globals env) name)))
    (and entry
         (not (eq (nelisp-env--symbol-entry-value entry)
                  nelisp--unbound-marker)))))

(defun nelisp-env-is-fbound (env name)
  "Return non-nil iff ENV[NAME] has a function cell (= `fboundp')."
  (let ((entry (nelisp--fast-hash-get (nelisp-env--globals env) name)))
    (and entry
         (not (eq (nelisp-env--symbol-entry-function entry)
                  nelisp--unbound-marker)))))

;; ---- defvar / defconst ----

(defun nelisp-env-defvar (env name value &optional is-constant)
  "Idempotent `defvar' / `defconst'.  Sets ENV[NAME]'s value cell
to VALUE only if currently `unbound-marker' (= Elisp `defvar'
re-evaluation semantics).  When IS-CONSTANT is non-nil, also
flags the entry as constant.  Returns NAME."
  (let* ((entry (nelisp-env--ensure-entry env name)))
    (when (eq (nelisp-env--symbol-entry-value entry)
              nelisp--unbound-marker)
      (nelisp-env--symbol-entry-set-value entry value))
    (when is-constant
      (nelisp--record-set entry 3 t))
    name))

(provide 'nelisp-env)

;;; nelisp-env.el ends here
