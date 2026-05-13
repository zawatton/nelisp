;;; nelisp-bake-fixtures-test.el --- ERT for Doc 95 §95.e CI integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT coverage for `nelisp-bake-fixtures' (the §95.e CI corpus
;; emitter) + the round-trip through the Rust `nelisp-baker
;; --verify-elisp-fixtures' binary.  Each fixture is emitted to a
;; temp dir, then re-encoded by the elisp reader to confirm shape
;; matches.  When the release binary exists under `target/release/'
;; the tests additionally call out to `nelisp-baker' to gate the
;; cross-impl byte-identity contract.  Tests that need the binary
;; auto-skip when it is absent so a bare `make test' from a fresh
;; checkout stays green.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nelisp-bake-fixtures-test--this-file
  (or load-file-name buffer-file-name
      (locate-library "nelisp-bake-fixtures-test")
      (expand-file-name "test/nelisp-bake-fixtures-test.el"
                        default-directory)))

(add-to-list 'load-path
             (expand-file-name
              "../lisp"
              (file-name-directory nelisp-bake-fixtures-test--this-file)))

(require 'nelisp-sexp-dsl)
(require 'nelisp-bake-fixtures)

;;; --- helpers ------------------------------------------------------

(defun nelisp-bake-fixtures-test--repo-root ()
  "Return the repo root containing target/release/nelisp-baker."
  (expand-file-name
   ".."
   (file-name-directory nelisp-bake-fixtures-test--this-file)))

(defun nelisp-bake-fixtures-test--baker-bin ()
  "Return absolute path to `nelisp-baker' release binary (may not exist)."
  (let ((bin (expand-file-name
              "target/release/nelisp-baker"
              (nelisp-bake-fixtures-test--repo-root))))
    (if (and (eq system-type 'windows-nt) (not (file-exists-p bin)))
        (concat bin ".exe")
      bin)))

(defun nelisp-bake-fixtures-test--baker-available-p ()
  "Non-nil when the Rust baker binary is built and executable."
  (let ((bin (nelisp-bake-fixtures-test--baker-bin)))
    (and (file-exists-p bin) (file-executable-p bin))))

(defun nelisp-bake-fixtures-test--mktmpdir ()
  "Create + return a fresh empty temp dir for fixtures."
  (let ((d (make-temp-file "nelisp-bake-fixtures-test-" t)))
    d))

(defun nelisp-bake-fixtures-test--rm-rf (dir)
  "Recursively delete DIR if it exists."
  (when (and dir (file-directory-p dir))
    (delete-directory dir t)))

(defun nelisp-bake-fixtures-test--call-baker (fixtures)
  "Invoke `nelisp-baker --verify-elisp-fixtures FIXTURES'.
Returns the integer exit code.  Stdout / stderr is captured into
the current buffer for diagnostic snippet inclusion on failure."
  (let ((bin (nelisp-bake-fixtures-test--baker-bin)))
    (apply #'call-process bin nil t nil
           "--verify-elisp-fixtures" fixtures)))

;;; --- corpus shape ---------------------------------------------------

(ert-deftest nelisp-bake-fixtures-corpus-non-empty ()
  "The hand-picked corpus has at least 6 distinct fixture shapes."
  (should (>= (length nelisp-bake-fixtures-corpus) 6))
  ;; Every entry is (SYMBOL . LIST-OF-STRINGS).
  (dolist (entry nelisp-bake-fixtures-corpus)
    (should (symbolp (car entry)))
    (should (listp (cdr entry)))
    (dolist (form (cdr entry))
      (should (stringp form)))))

(ert-deftest nelisp-bake-fixtures-corpus-names-unique ()
  "Fixture names form a unique symbol set (= no shadowed paths)."
  (let ((names (mapcar #'car nelisp-bake-fixtures-corpus)))
    (should (= (length names) (length (cl-remove-duplicates names))))))

;;; --- elisp emit + self-read round-trip ----------------------------

(ert-deftest nelisp-bake-fixtures-emit-all-writes-files ()
  "`nelisp-bake-fixtures-emit-all' writes one file per corpus entry."
  (let ((dir (nelisp-bake-fixtures-test--mktmpdir)))
    (unwind-protect
        (let ((emitted (nelisp-bake-fixtures-emit-all dir)))
          (should (= (length emitted)
                     (length nelisp-bake-fixtures-corpus)))
          (dolist (entry emitted)
            (should (file-exists-p (cdr entry)))
            (should (> (nth 7 (file-attributes (cdr entry))) 20))))
      (nelisp-bake-fixtures-test--rm-rf dir))))

(ert-deftest nelisp-bake-fixtures-each-shape-self-round-trips ()
  "Every fixture re-decodes losslessly + byte-identical via elisp.
This is the inner gate: any corpus payload the elisp dumper writes
must come back through `nelisp-sexp-bake-read-image' with matching
fallback strings AND survive `nelisp-sexp-bake-verify-image' (=
elisp read + re-encode = on-disk)."
  (let ((dir (nelisp-bake-fixtures-test--mktmpdir)))
    (unwind-protect
        (let ((emitted (nelisp-bake-fixtures-emit-all dir)))
          (dolist (entry emitted)
            (let* ((name (car entry))
                   (path (cdr entry))
                   (expected (cdr (assoc name
                                         nelisp-bake-fixtures-corpus)))
                   (parsed (nelisp-sexp-bake-read-image path)))
              (should (equal expected
                             (plist-get parsed :fallback-forms)))
              (should (eq t (nelisp-sexp-bake-verify-image path))))))
      (nelisp-bake-fixtures-test--rm-rf dir))))

;;; --- cross-impl gate (Rust baker) ---------------------------------

(ert-deftest nelisp-bake-fixtures-rust-verifies-single ()
  "Rust baker accepts at least one elisp-produced fixture.
Auto-skip when `target/release/nelisp-baker' is absent so a bare
`make test' from a fresh tree stays green; CI's
`make verify-elisp-fixtures' is the canonical gate."
  (skip-unless (nelisp-bake-fixtures-test--baker-available-p))
  (let ((dir (nelisp-bake-fixtures-test--mktmpdir)))
    (unwind-protect
        (let* ((emitted (nelisp-bake-fixtures-emit-all dir))
               (one (cdar emitted)))
          (with-temp-buffer
            (let ((rc (nelisp-bake-fixtures-test--call-baker
                       (list one))))
              (should (= 0 rc)))))
      (nelisp-bake-fixtures-test--rm-rf dir))))

(ert-deftest nelisp-bake-fixtures-rust-verifies-whole-corpus ()
  "Rust baker accepts every fixture in the corpus (= the CI gate).
Auto-skip when baker binary absent."
  (skip-unless (nelisp-bake-fixtures-test--baker-available-p))
  (let ((dir (nelisp-bake-fixtures-test--mktmpdir)))
    (unwind-protect
        (let* ((emitted (nelisp-bake-fixtures-emit-all dir))
               (paths (mapcar #'cdr emitted)))
          (with-temp-buffer
            (let ((rc (nelisp-bake-fixtures-test--call-baker paths)))
              (unless (= 0 rc)
                (message "baker output:\n%s" (buffer-string)))
              (should (= 0 rc)))))
      (nelisp-bake-fixtures-test--rm-rf dir))))

(ert-deftest nelisp-bake-fixtures-rust-rejects-corrupted ()
  "Rust baker exits non-zero on a corrupted fixture (= failure path
exercised — the gate must fire when bytes diverge).  Auto-skip
when baker binary absent."
  (skip-unless (nelisp-bake-fixtures-test--baker-available-p))
  (let ((dir (nelisp-bake-fixtures-test--mktmpdir)))
    (unwind-protect
        (let* ((emitted (nelisp-bake-fixtures-emit-all dir))
               (one (cdar emitted)))
          ;; Truncate by 1 byte to corrupt the trailing UTF-8 string.
          (let ((sz (nth 7 (file-attributes one))))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally one)
              (delete-region (1- (point-max)) (point-max))
              (let ((coding-system-for-write 'no-conversion))
                (write-region (point-min) (point-max) one nil 'quiet)))
            (should (= (1- sz)
                       (nth 7 (file-attributes one)))))
          (with-temp-buffer
            (let ((rc (nelisp-bake-fixtures-test--call-baker
                       (list one))))
              (should-not (= 0 rc)))))
      (nelisp-bake-fixtures-test--rm-rf dir))))

;;; --- batch entry point --------------------------------------------

(ert-deftest nelisp-bake-fixtures-batch-emit-prints-paths ()
  "`nelisp-bake-fixtures-emit-all-batch' prints one path per line."
  (let ((dir (nelisp-bake-fixtures-test--mktmpdir)))
    (unwind-protect
        (let* ((command-line-args-left (list dir))
               (out (with-output-to-string
                      (nelisp-bake-fixtures-emit-all-batch)))
               (lines (split-string out "\n" t)))
          (should (= (length lines)
                     (length nelisp-bake-fixtures-corpus)))
          (dolist (line lines)
            (should (file-exists-p line))))
      (nelisp-bake-fixtures-test--rm-rf dir))))

(provide 'nelisp-bake-fixtures-test)

;;; nelisp-bake-fixtures-test.el ends here
