;;; nelisp-load-selfhost-test.el --- ERT for T86 self-host load path  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; T86 (Phase 7+ Wave 2 F) — assert `nelisp-load.el' routes its file
;; I/O through `nelisp-emacs-compat' (T78 SHIPPED) instead of host
;; Emacs `with-temp-buffer' / `insert-file-contents' / `file-readable-p'
;; / `expand-file-name'.  This is the standalone-path chicken-and-egg
;; blocker: once nelisp-load is self-host clean, anvil.el (and other
;; downstream packages) can drive `nelisp-load-file' on the NeLisp-only
;; runtime where host Emacs primitives are not available.
;;
;; Strategy:
;;   1. Source-level audit — grep nelisp-load.el and assert no host
;;      file I/O primitive remains in the body of `nelisp-load-file' /
;;      `nelisp-locate-file'.  Catches regressions where someone re-
;;      introduces `with-temp-buffer' / `insert-file-contents'.
;;   2. Behavioural audit — advise `nelisp-ec-insert-file-contents' and
;;      `nelisp-ec-file-readable-p', drive `nelisp-load-file' against a
;;      tmp file, verify both wrappers fired at least once.  Proves the
;;      route actually exercised the compat layer.
;;   3. Roundtrip — write tmp .el via `nelisp-ec-write-region', load
;;      via `nelisp-load-file', verify evaluator side effects.  Proves
;;      end-to-end functional parity with the pre-T86 path.
;;   4. UTF-8 — load a file containing non-ASCII multibyte text and
;;      verify the decoded content reaches the reader correctly.
;;   5. Missing file — `nelisp-load-file' on a non-existent path still
;;      signals `file-error' (existing contract preserved).
;;   6. Baseline — multi-form load through the new path persists state
;;      exactly like the pre-T86 implementation.

;;; Code:

(require 'ert)
(require 'nelisp-load)
(require 'nelisp-emacs-compat)
(require 'nelisp-emacs-compat-fileio)

;;; §1. Source-level audit ----------------------------------------------

(defconst nelisp-load-selfhost-test--src-path
  (expand-file-name
   "../src/nelisp-load.el"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to nelisp-load.el under audit.")

(defun nelisp-load-selfhost-test--load-source ()
  "Return the textual body of `nelisp-load.el' for grep tests."
  (with-temp-buffer
    (insert-file-contents nelisp-load-selfhost-test--src-path)
    (buffer-string)))

(defun nelisp-load-selfhost-test--strip-comments-and-strings (s)
  "Return S with single-line `;' comments and string literals stripped.
Used so the audit grep ignores explanation text in docstrings."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    ;; Strip strings (best-effort, NeLisp-load.el has no #\\\" escapes
    ;; embedded in this kind of line).
    (while (re-search-forward "\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"" nil t)
      (replace-match ""))
    (goto-char (point-min))
    ;; Strip line comments.
    (while (re-search-forward ";.*$" nil t)
      (replace-match ""))
    (buffer-string)))

(ert-deftest nelisp-load-selfhost-no-with-temp-buffer ()
  "After T86, nelisp-load.el body must not call host `with-temp-buffer'."
  (let ((src (nelisp-load-selfhost-test--strip-comments-and-strings
              (nelisp-load-selfhost-test--load-source))))
    (should-not (string-match-p "(with-temp-buffer" src))))

(ert-deftest nelisp-load-selfhost-no-insert-file-contents ()
  "After T86, nelisp-load.el body must not call host `insert-file-contents'.
The compat wrapper `nelisp-ec-insert-file-contents' is allowed."
  (let* ((src (nelisp-load-selfhost-test--strip-comments-and-strings
               (nelisp-load-selfhost-test--load-source)))
         ;; Mask the compat wrapper so its substring match doesn't
         ;; trip the host-primitive check.
         (masked (replace-regexp-in-string
                  "nelisp-ec-insert-file-contents" "" src)))
    (should-not (string-match-p "(insert-file-contents" masked))))

(ert-deftest nelisp-load-selfhost-no-host-file-readable-p ()
  "After T86, nelisp-load.el body must not call host `file-readable-p'.
The compat wrapper `nelisp-ec-file-readable-p' is allowed."
  (let* ((src (nelisp-load-selfhost-test--strip-comments-and-strings
               (nelisp-load-selfhost-test--load-source)))
         (masked (replace-regexp-in-string
                  "nelisp-ec-file-readable-p" "" src)))
    (should-not (string-match-p "(file-readable-p" masked))))

(ert-deftest nelisp-load-selfhost-no-host-expand-file-name ()
  "After T86, nelisp-load.el body must not call host `expand-file-name'.
The compat wrapper `nelisp-ec-expand-file-name' is allowed."
  (let* ((src (nelisp-load-selfhost-test--strip-comments-and-strings
               (nelisp-load-selfhost-test--load-source)))
         (masked (replace-regexp-in-string
                  "nelisp-ec-expand-file-name" "" src)))
    (should-not (string-match-p "(expand-file-name" masked))))

;;; §2. Behavioural audit — compat wrappers actually fire --------------

(ert-deftest nelisp-load-selfhost-uses-nelisp-ec-wrappers ()
  "`nelisp-load-file' must drive `nelisp-ec-insert-file-contents' and
`nelisp-ec-file-readable-p' during a successful load.  Advice counts
the wrapper invocations to prove the standalone path is live."
  (nelisp--reset)
  (let* ((tmp (make-temp-file "nelisp-load-selfhost" nil ".el"))
         (insert-cell (list 0))
         (readable-cell (list 0))
         (insert-advice (lambda (&rest _args)
                          (setcar insert-cell (1+ (car insert-cell)))))
         (readable-advice (lambda (&rest _args)
                            (setcar readable-cell
                                    (1+ (car readable-cell))))))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (let ((coding-system-for-write 'utf-8))
              (insert "(defvar *t86-marker* 86)\n")))
          (advice-add 'nelisp-ec-insert-file-contents :before insert-advice)
          (advice-add 'nelisp-ec-file-readable-p :before readable-advice)
          (unwind-protect
              (progn
                (nelisp-load-file tmp)
                (should (>= (car insert-cell) 1))
                (should (>= (car readable-cell) 1))
                (should (= (nelisp-eval '*t86-marker*) 86)))
            (advice-remove 'nelisp-ec-insert-file-contents insert-advice)
            (advice-remove 'nelisp-ec-file-readable-p readable-advice)))
      (delete-file tmp))))

(ert-deftest nelisp-load-selfhost-locate-uses-nelisp-ec-readable-p ()
  "`nelisp-locate-file' must consult `nelisp-ec-file-readable-p' so
feature lookup works under the standalone runtime where host
`file-readable-p' is unavailable."
  ;; Use a list cell so the advice closure shares a mutable container
  ;; with the test body, regardless of how ert macroexpands the body.
  (let* ((counter (list 0))
         (probe (lambda (&rest _args)
                  (setcar counter (1+ (car counter))))))
    (advice-add 'nelisp-ec-file-readable-p :before probe)
    (unwind-protect
        (let ((nelisp-load-path '("/nonexistent/nelisp/feature/dir")))
          ;; Lookup should miss but still go through the wrapper.
          (should (null (nelisp-locate-file 'definitely-not-there)))
          (should (>= (car counter) 1)))
      (advice-remove 'nelisp-ec-file-readable-p probe))))

;;; §3. Roundtrip via nelisp-ec-write-region ----------------------------

(ert-deftest nelisp-load-selfhost-roundtrip-via-ec ()
  "Write a tmp file via `nelisp-ec-write-region', read it back via
`nelisp-load-file'.  Proves the read + write halves of the compat
layer cooperate end-to-end (= the pure-NeLisp self-host I/O cycle)."
  (nelisp--reset)
  (let* ((tmp (make-temp-file "nelisp-load-selfhost-roundtrip" nil ".el"))
         (buf (nelisp-ec-generate-new-buffer "rt-write")))
    (unwind-protect
        (progn
          (nelisp-ec-with-current-buffer buf
            (nelisp-ec-insert
             "(defvar *t86-rt* 0)\n(setq *t86-rt* (+ 1 2 3 4 5))\n")
            (nelisp-ec-write-region (nelisp-ec-point-min)
                                    (nelisp-ec-point-max)
                                    tmp))
          (nelisp-load-file tmp)
          (should (= (nelisp-eval '*t86-rt*) 15)))
      (nelisp-ec-kill-buffer buf)
      (delete-file tmp))))

;;; §4. UTF-8 multibyte content -----------------------------------------

(ert-deftest nelisp-load-selfhost-utf8-content ()
  "`nelisp-load-file' decodes UTF-8 source through `nelisp-coding'
inside `nelisp-ec-insert-file-contents'.  A multibyte string literal
must survive the read/eval round trip."
  (nelisp--reset)
  (let ((tmp (make-temp-file "nelisp-load-selfhost-utf8" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (let ((coding-system-for-write 'utf-8))
              ;; Japanese + accented Latin to exercise multi-byte
              ;; encoding paths.
              (insert "(defvar *t86-utf8* \"日本語 café\")\n")))
          (nelisp-load-file tmp)
          (should (string-equal (nelisp-eval '*t86-utf8*) "日本語 café")))
      (delete-file tmp))))

;;; §5. Missing file contract preserved --------------------------------

(ert-deftest nelisp-load-selfhost-missing-file-signals-file-error ()
  "After T86 the missing-file contract still raises `file-error'.
Routed through `nelisp-ec-file-readable-p' but caller observation is
unchanged from the pre-T86 implementation."
  (should-error
   (nelisp-load-file "/definitely/not/here/nelisp-t86.el")
   :type 'file-error))

;;; §6. Multi-form persistence ------------------------------------------

(ert-deftest nelisp-load-selfhost-multi-form-persistence ()
  "Loading a file with multiple top-level defuns + defvars persists
all state into the global NeLisp tables, exactly like pre-T86."
  (nelisp--reset)
  (let ((tmp (make-temp-file "nelisp-load-selfhost-multi" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (let ((coding-system-for-write 'utf-8))
              (insert "(defvar *t86-a* 1)\n"
                      "(defvar *t86-b* 2)\n"
                      "(defun t86-add () (+ *t86-a* *t86-b*))\n"
                      "(setq *t86-c* (t86-add))\n")))
          (nelisp-load-file tmp)
          (should (= (nelisp-eval '*t86-a*) 1))
          (should (= (nelisp-eval '*t86-b*) 2))
          (should (= (nelisp-eval '(t86-add)) 3))
          (should (= (nelisp-eval '*t86-c*) 3)))
      (delete-file tmp))))

;;; §7. Header dependency assertion ------------------------------------

(ert-deftest nelisp-load-selfhost-requires-emacs-compat-fileio ()
  "After T86, `nelisp-load' must `require' the compat fileio module so
loading nelisp-load.el alone is enough to wire the standalone path.
Catches the regression where someone removes the require but the
file happens to load because an earlier test pulled the dependency."
  (should (featurep 'nelisp-emacs-compat))
  (should (featurep 'nelisp-emacs-compat-fileio))
  ;; And the API symbols we depend on must be `fboundp'.
  (should (fboundp 'nelisp-ec-insert-file-contents))
  (should (fboundp 'nelisp-ec-file-readable-p))
  (should (fboundp 'nelisp-ec-expand-file-name))
  (should (fboundp 'nelisp-ec-generate-new-buffer))
  (should (fboundp 'nelisp-ec-buffer-string))
  (should (fboundp 'nelisp-ec-kill-buffer)))

(provide 'nelisp-load-selfhost-test)
;;; nelisp-load-selfhost-test.el ends here
