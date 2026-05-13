;;; nelisp-bake-fixtures.el --- Doc 95 §95.e fixture generator  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 95 §95.e CI integration: a hand-picked corpus of NELIMG v3
;; envelope fixtures produced by the elisp serializer
;; (`nelisp-sexp-bake-dump-fixture' in `nelisp-sexp-dsl.el').  The
;; companion `make verify-elisp-fixtures' target runs each fixture
;; through `nelisp-baker --verify-elisp-fixtures' so the Rust
;; encoder's bytes for the same fallback-form payload are confirmed
;; byte-identical at every CI build.
;;
;; Scope: this module only emits fixtures the elisp serializer can
;; produce today (= envelope subset with 0 nodes / 0 globals / N
;; fallback strings, per §95.e SHIPPED).  Fixture shapes that would
;; trip `nelisp-sexp-bake-unsupported' on the read side (= N_NODES>0
;; or N_GLOBALS>0) are deferred to Doc 95.f / Phase 48.

;;; Code:

(require 'nelisp-sexp-dsl)

(defconst nelisp-bake-fixtures-corpus
  '((empty             . ())
    (one-form          . ("(defun foo () 1)"))
    (multibyte-utf8    . ("(message \"日本語\")"))
    (two-forms         . ("(defun foo () 1)"
                          "(defun bar () 2)"))
    (multi-multibyte   . ("(message \"漢字 αβγ 🚀\")"
                          ";; 日本語のコメント"))
    (header-comment    . (";;; example.el --- demo  -*- lexical-binding: t; -*-"))
    (defvar-form       . ("(defvar nelisp-bake-fixtures-x 42)"))
    (progn-multiline   . ("(progn\n  (message \"line1\")\n  (message \"line2\"))"))
    (mixed-ascii-utf8  . ("(defconst greeting \"hello / こんにちは\")"))
    (long-fallback     . ("(progn\n  ;; one\n  ;; two\n  ;; three\n  (list 1 2 3 4 5))")))
  "Hand-picked corpus of NELIMG v3 envelope fixture payloads.
Each entry is `(NAME . FALLBACK-FORMS)' where FALLBACK-FORMS is a
list of strings (= envelope subset emitted by §95.e today).
Covers: empty envelope, single form, multibyte UTF-8, multi-form,
multi-form multibyte, comment-only payload, defvar / defconst,
progn body with embedded newlines, mixed ASCII + UTF-8, longer
multi-line block.  Total 10 fixtures.")

(defun nelisp-bake-fixtures-fixture-path (dir name)
  "Return absolute fixture path under DIR for fixture NAME (symbol)."
  (expand-file-name (format "%s.image" name)
                    (file-name-as-directory dir)))

(defun nelisp-bake-fixtures-emit-all (dir)
  "Emit every fixture in `nelisp-bake-fixtures-corpus' under DIR.
Creates DIR if missing.  Returns a list of (NAME . PATH) pairs in
the same order as the corpus.  Each fixture is written via
`nelisp-sexp-bake-dump-fixture', so the bytes are an authoritative
elisp-side NELIMG v3 envelope encoding that the Rust baker can
verify with `--verify-elisp-fixtures'."
  (unless (file-directory-p dir)
    (make-directory dir t))
  (let (result)
    (dolist (entry nelisp-bake-fixtures-corpus)
      (let* ((name (car entry))
             (forms (cdr entry))
             (path (nelisp-bake-fixtures-fixture-path dir name)))
        (nelisp-sexp-bake-dump-fixture forms path)
        (push (cons name path) result)))
    (nreverse result)))

(defun nelisp-bake-fixtures-emit-all-batch ()
  "Batch entry point for `make verify-elisp-fixtures'.
Reads target directory from command-line argument and emits the
full corpus there.  Prints one fixture path per line to stdout so
the Make recipe can xargs them into `nelisp-baker
--verify-elisp-fixtures'.  Exits non-zero on emit failure."
  (let ((dir (or (car command-line-args-left)
                 (error "nelisp-bake-fixtures-emit-all-batch: pass target dir"))))
    (setq command-line-args-left (cdr command-line-args-left))
    (let ((emitted (nelisp-bake-fixtures-emit-all dir)))
      (dolist (entry emitted)
        (princ (format "%s\n" (cdr entry)))))))

(provide 'nelisp-bake-fixtures)

;;; nelisp-bake-fixtures.el ends here
