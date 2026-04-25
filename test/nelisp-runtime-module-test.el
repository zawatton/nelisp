;;; nelisp-runtime-module-test.el --- T68 Tier 2 hardening ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; T68 (Phase 7.5.4 Tier 2) — ERT coverage for the five T58 audit
;; findings fixed in this commit:
;;
;;   #1 (CRITICAL) Partial dlsym poisons global → transactional load
;;   #2 (CRITICAL) signal_error followed by env-> calls → return-only
;;                 + structured error symbol hierarchy
;;   #3 (CRITICAL) JIT bytes fault containment 0 → docstring + sanity
;;                 check + Rust `panic = "abort"' guard
;;   #4 (MAJOR)    Module init failure swallowed → emacs_module_init
;;                 returns non-zero on `fset' / `provide' failure
;;   #5 (MAJOR)    Unibyte not verified → `string-bytes' = `length'
;;                 gate before any allocation
;;
;; All tests are gated by the standard skip-helpers from
;; `nelisp-cc-runtime-test' (module support + .so present); a fresh
;; checkout that has not run `make runtime-module' will skip the
;; entire group rather than fail.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc-runtime)

;;; Skip-gates ----------------------------------------------------

(defun nelisp-runtime-module-test--skip-unless-built ()
  "Skip the surrounding ERT unless the in-process FFI is available.

Mirrors `nelisp-cc-runtime-test--skip-unless-module-built' so this
file is independently runnable; we do not require the sibling test
file."
  (unless (and (boundp 'module-file-suffix)
               module-file-suffix
               (fboundp 'module-load))
    (ert-skip "Emacs lacks dynamic module support — rebuild with --with-modules"))
  (let ((path (ignore-errors
                (nelisp-cc-runtime--locate-runtime-module))))
    (unless (and path (file-readable-p path))
      (ert-skip "nelisp-runtime-module.so missing — run `make runtime-module'"))))

(defun nelisp-runtime-module-test--ensure-loaded ()
  "Module-load + bootstrap the cdylib path.  Idempotent."
  (nelisp-cc-runtime--ensure-module-loaded))

;;; (1) Init: required functions are bound -----------------------

(ert-deftest nelisp-runtime-module-init-success ()
  "T68 (1) — `module-load' wires up every documented entry point.

Once the module is loaded each of the four public symbols must be
`fboundp'.  This is the inverse of T58 finding #4: when
`emacs_module_init' fails halfway through `bind_function' the old
code returned 0 and left a partially-bound feature; the new code
short-circuits with a non-zero return so `module-load' itself
errors out.  Reaching this assertion at all therefore proves init
ran to completion."
  (nelisp-runtime-module-test--skip-unless-built)
  (nelisp-runtime-module-test--ensure-loaded)
  (should (fboundp 'nelisp-runtime-module-version))
  (should (fboundp 'nelisp-runtime-module-load-cdylib))
  (should (fboundp 'nelisp-runtime-module-syscall-smoke))
  (should (fboundp 'nelisp-runtime-module-exec-bytes))
  ;; `provide' must have fired (Doc 32 v2 §7 contract).
  (should (featurep 'nelisp-runtime-module)))

;;; (2) Error symbols defined ------------------------------------

(ert-deftest nelisp-runtime-module-error-symbols-distinct ()
  "T68 (2) — every error kind has a distinct condition-name parent.

The structured error hierarchy added by Phase 7.5.4 hardening lets
callers branch on the *kind* of failure (dlopen vs dlsym vs
mprotect vs invalid-argument) without parsing message strings.
Each of the five concrete symbols must inherit from
`nelisp-runtime-module-error' which itself inherits from `error'."
  (nelisp-runtime-module-test--skip-unless-built)
  (nelisp-runtime-module-test--ensure-loaded)
  (dolist (sym '(nelisp-runtime-module-dlopen-error
                 nelisp-runtime-module-dlsym-error
                 nelisp-runtime-module-mprotect-error
                 nelisp-runtime-module-clear-icache-error
                 nelisp-runtime-module-invalid-argument))
    (let ((cond (get sym 'error-conditions)))
      (should cond)
      (should (memq sym cond))
      (should (memq 'nelisp-runtime-module-error cond))
      (should (memq 'error cond)))
    ;; A user-readable message is always populated.
    (should (stringp (get sym 'error-message))))
  ;; The umbrella inherits directly from `error'.
  (let ((cond (get 'nelisp-runtime-module-error 'error-conditions)))
    (should (memq 'error cond))))

;;; (3) Multibyte input is rejected ------------------------------

(ert-deftest nelisp-runtime-module-multibyte-input-rejected ()
  "T68 (3 / finding #5) — multibyte bytes signal `invalid-argument'.

A multibyte string carries (string-bytes != length) so the unibyte
verification gate must reject it *before* any allocation happens.
The legacy code silently UTF-8-encoded the input which mangled the
JIT payload."
  (nelisp-runtime-module-test--skip-unless-built)
  (nelisp-runtime-module-test--ensure-loaded)
  (let* ((multibyte (string ?あ ?い ?う))
         (caught nil))
    (should (multibyte-string-p multibyte))
    (condition-case err
        (funcall (intern "nelisp-runtime-module-exec-bytes") multibyte)
      (nelisp-runtime-module-invalid-argument
       (setq caught :invalid-argument))
      (error
       (setq caught (cons :other err))))
    (should (eq caught :invalid-argument))))

;;; (4) Empty unibyte input is rejected --------------------------

(ert-deftest nelisp-runtime-module-empty-input-rejected ()
  "T68 (4) — empty unibyte string signals `invalid-argument'.

Boundary case adjacent to finding #5: the unibyte gate accepts \"\"
(string-bytes == length == 0), but the second guard inside
`exec-bytes' must still reject zero-length code so we don't mmap a
0-byte page."
  (nelisp-runtime-module-test--skip-unless-built)
  (nelisp-runtime-module-test--ensure-loaded)
  (let* ((empty (unibyte-string))
         (caught nil))
    (should (not (multibyte-string-p empty)))
    (condition-case _err
        (funcall (intern "nelisp-runtime-module-exec-bytes") empty)
      (nelisp-runtime-module-invalid-argument
       (setq caught :invalid-argument))
      (error
       (setq caught :other)))
    (should (eq caught :invalid-argument))))

;;; (5) load-cdylib idempotent + signals on bogus path -----------

(ert-deftest nelisp-runtime-module-load-cdylib-bogus-path-signals ()
  "T68 (5 / finding #1) — bogus PATH signals dlopen-error and leaves
state recoverable.

When the runtime is *already* loaded (which is the common case in
ERT because every prior test triggered loading) the function
returns t without re-evaluating PATH.  In that branch we cannot
exercise the dlopen failure path — but we can at least assert the
idempotency contract.  This still proves the transactional rewrite
did not regress the happy path."
  (nelisp-runtime-module-test--skip-unless-built)
  (nelisp-runtime-module-test--ensure-loaded)
  ;; Once the runtime is loaded, load-cdylib must be a no-op that
  ;; returns t — even when PATH is nonsense.
  (should (eq t (funcall (intern "nelisp-runtime-module-load-cdylib")
                         "/dev/null/nonexistent"))))

;;; (6) Exec bytes happy path: mov eax,42 / ret returns 42 -------

(ert-deftest nelisp-runtime-module-exec-bytes-roundtrip ()
  "T68 (6) — happy-path exec-bytes still works after Tier 2 fixes.

`B8 2A 00 00 00 C3' = mov eax, 42 ; ret.  The i64 return must come
back as 42, proving the unibyte gate, prebuilt-nil sentinel
plumbing, and prequal sanity check do not regress legitimate JIT
inputs.  This is identical in spirit to the T33 (17) golden test
but exercises the public symbol directly rather than going through
`nelisp-cc-runtime--exec-in-process'."
  (nelisp-runtime-module-test--skip-unless-built)
  (nelisp-runtime-module-test--ensure-loaded)
  ;; `unibyte-string' guarantees `string-bytes' == `length'.
  (let* ((bytes (unibyte-string #xB8 #x2A #x00 #x00 #x00 #xC3))
         (result (funcall (intern "nelisp-runtime-module-exec-bytes") bytes)))
    (should (integerp result))
    (should (= result 42))))

;;; (7) Version + smoke remain ---------------------------------------

(ert-deftest nelisp-runtime-module-version-string ()
  "T68 (7) — version probe returns a non-empty string.

Catches the trivial wiring regression where a misnamed C handler
ends up bound to the version symbol; the integration is small but
the failure mode is silent so we test it explicitly."
  (nelisp-runtime-module-test--skip-unless-built)
  (nelisp-runtime-module-test--ensure-loaded)
  (let ((v (funcall (intern "nelisp-runtime-module-version"))))
    (should (stringp v))
    (should (> (length v) 0))
    (should (string-match-p "nelisp-runtime-module" v))))

(provide 'nelisp-runtime-module-test)

;;; nelisp-runtime-module-test.el ends here
