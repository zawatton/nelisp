;;; nelisp-sexp-bake-test.el --- ERT tests for Doc 95 §95.e bake integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 95 §95.e acceptance: read NELIMG v3 images produced by
;; `build-tool/src/bin/nelisp-baker.rs', re-emit via elisp, assert byte-
;; identity.  Also covers the dump-fixture cross-impl path and §95.c /
;; §95.d non-interference (= §95.e helpers consume a third wire format
;; orthogonal to the variable-length + 32-byte fixed serializers).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nelisp-sexp-bake-test--this-file
  (or load-file-name buffer-file-name
      (locate-library "nelisp-sexp-bake-test")
      ;; Final fallback for `emacs --batch -l test/...' where neither
      ;; of the above is set: assume CWD is the project root.
      (expand-file-name "test/nelisp-sexp-bake-test.el"
                        default-directory)))

(add-to-list 'load-path
             (expand-file-name
              "../lisp"
              (file-name-directory nelisp-sexp-bake-test--this-file)))
(require 'nelisp-sexp-dsl)

;;; --- helpers ------------------------------------------------------

(defun nelisp-sexp-bake-test--lisp-dir ()
  "Return the absolute path to lisp/ containing baked .image files."
  (expand-file-name
   "../lisp"
   (file-name-directory nelisp-sexp-bake-test--this-file)))

(defun nelisp-sexp-bake-test--tmp ()
  "Return a fresh temp file path under `temporary-file-directory'."
  (make-temp-file "nelisp-sexp-bake-fixture-" nil ".image"))

;;; --- envelope header parsing --------------------------------------

(ert-deftest nelisp-sexp-bake-magic-version-kind ()
  "Reader pulls magic / version / kind from a real baked image."
  (let* ((path (expand-file-name
                "nelisp-stdlib-hof.el.image"
                (nelisp-sexp-bake-test--lisp-dir)))
         (parsed (nelisp-sexp-bake-read-image path)))
    (should (plist-get parsed :magic-ok))
    (should (= (plist-get parsed :version) 3))
    (should (= (plist-get parsed :kind) 1))
    (should (= (plist-get parsed :n-nodes) 0))
    (should (= (plist-get parsed :n-globals) 0))
    (should (= (plist-get parsed :n-fallback-forms) 1))))

(ert-deftest nelisp-sexp-bake-fallback-form-extracted ()
  "Reader returns the embedded elisp source from fallback forms."
  (let* ((path (expand-file-name
                "nelisp-stdlib-hof.el.image"
                (nelisp-sexp-bake-test--lisp-dir)))
         (parsed (nelisp-sexp-bake-read-image path))
         (forms (plist-get parsed :fallback-forms)))
    (should (= (length forms) 1))
    (should (stringp (car forms)))
    ;; The hof source starts with its header comment.
    (should (string-prefix-p ";;; nelisp-stdlib-hof.el" (car forms)))))

;;; --- byte-identity on real baked output ---------------------------

(ert-deftest nelisp-sexp-bake-verify-single-real-image ()
  "One concrete real baked image round-trips byte-identical."
  (let ((path (expand-file-name
               "nelisp-stdlib-hof.el.image"
               (nelisp-sexp-bake-test--lisp-dir))))
    (should (eq t (nelisp-sexp-bake-verify-image path)))))

(ert-deftest nelisp-sexp-bake-verify-all-real-images ()
  "Every baked .image under lisp/ round-trips byte-identical.
This is the central acceptance test for Doc 95 §95.e (= the elisp
encoder mirrors the Rust `encode_v3_with_fallback' envelope path
exactly for the empty-env / single-fallback subset the baker emits
today)."
  (let* ((dir (nelisp-sexp-bake-test--lisp-dir))
         (images (directory-files dir t "\\.image\\'"))
         (failures nil))
    (should (> (length images) 0))
    (dolist (img images)
      (condition-case e
          (progn
            (nelisp-sexp-bake-verify-image img)
            t)
        (error (push (cons img e) failures))))
    (should (null failures))))

;;; --- dump-fixture round-trip --------------------------------------

(ert-deftest nelisp-sexp-bake-dump-fixture-round-trips ()
  "Encoded fixture survives read + re-encode."
  (let ((tmp (nelisp-sexp-bake-test--tmp))
        (forms '("(defun foo () 1)" "(defun bar () 2)")))
    (unwind-protect
        (progn
          (nelisp-sexp-bake-dump-fixture forms tmp)
          (let ((parsed (nelisp-sexp-bake-read-image tmp)))
            (should (equal forms (plist-get parsed :fallback-forms)))
            (should (eq t (nelisp-sexp-bake-verify-image tmp)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest nelisp-sexp-bake-dump-fixture-multibyte ()
  "UTF-8 multibyte fallback forms round-trip losslessly."
  (let ((tmp (nelisp-sexp-bake-test--tmp))
        (forms '("(message \"日本語テスト\")"
                 ";; 漢字 αβγ 🚀")))
    (unwind-protect
        (progn
          (nelisp-sexp-bake-dump-fixture forms tmp)
          (let ((parsed (nelisp-sexp-bake-read-image tmp)))
            (should (equal forms (plist-get parsed :fallback-forms)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest nelisp-sexp-bake-dump-fixture-empty ()
  "Empty fallback-forms list still produces a valid v3 envelope."
  (let ((tmp (nelisp-sexp-bake-test--tmp)))
    (unwind-protect
        (progn
          (nelisp-sexp-bake-dump-fixture nil tmp)
          (let ((parsed (nelisp-sexp-bake-read-image tmp)))
            (should (= (plist-get parsed :n-fallback-forms) 0))
            (should (null (plist-get parsed :fallback-forms)))
            (should (eq t (nelisp-sexp-bake-verify-image tmp)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

;;; --- error path coverage ------------------------------------------

(ert-deftest nelisp-sexp-bake-bad-magic-rejected ()
  "Garbage magic header signals `nelisp-sexp-bake-bad-magic'."
  (let ((tmp (nelisp-sexp-bake-test--tmp)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file tmp
              (set-buffer-multibyte nil)
              (insert (make-string 32 ?X))))
          (should-error (nelisp-sexp-bake-read-image tmp)
                        :type 'nelisp-sexp-bake-bad-magic))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest nelisp-sexp-bake-truncated-rejected ()
  "Image shorter than the 21-byte header signals truncation."
  (let ((tmp (nelisp-sexp-bake-test--tmp)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file tmp
              (set-buffer-multibyte nil)
              (insert (unibyte-string ?N ?E ?L))))
          (should-error (nelisp-sexp-bake-read-image tmp)
                        :type 'nelisp-sexp-bake-truncated))
      (when (file-exists-p tmp) (delete-file tmp)))))

;;; --- cross-format non-interference (§95.c / §95.d / §95.e) --------

(ert-deftest nelisp-sexp-bake-format-distinct-from-95c ()
  "§95.c per-Sexp wire bytes do NOT carry the NELIMG v3 header."
  ;; Take an §95.c-encoded Int and confirm it has no NELIMG magic.
  (let ((bytes (nelisp-sexp-write-bytes (nelisp-sexp-make-int 42))))
    (should (stringp bytes))
    (should-not (string-prefix-p nelisp-sexp-bake-v3-magic bytes))))

(ert-deftest nelisp-sexp-bake-format-distinct-from-95d ()
  "§95.d 32-byte JIT record does NOT carry the NELIMG v3 header."
  (let* ((store (nelisp-sexp-jit-make-heap-store))
         (rec (nelisp-sexp-jit-write-bytes
               (nelisp-sexp-make-int 7) store)))
    (should (= (length rec) 32))
    (should-not (string-prefix-p nelisp-sexp-bake-v3-magic rec))))

(ert-deftest nelisp-sexp-bake-95c-95d-still-work-after-95e ()
  "Loading §95.e helpers does not regress §95.c / §95.d round-trip."
  ;; §95.c
  (let ((s (nelisp-sexp-make-cons (nelisp-sexp-make-int 1)
                                  (nelisp-sexp-make-int 2))))
    (should (nelisp-sexp-bytes-round-trip-p s)))
  ;; §95.d
  (should (nelisp-sexp-jit-round-trip-p (nelisp-sexp-make-int 99)))
  (should (nelisp-sexp-jit-round-trip-p (nelisp-sexp-make-float 1.5))))

;;; --- envelope subset boundary -------------------------------------

(ert-deftest nelisp-sexp-bake-unsupported-on-nonzero-nodes ()
  "Reader bails with `nelisp-sexp-bake-unsupported' on N_NODES > 0.
This guards the envelope-only assumption explicitly so future
multi-node baker output is caught at read time instead of silently
producing nonsense fallback strings."
  (let ((tmp (nelisp-sexp-bake-test--tmp))
        (coding-system-for-write 'no-conversion))
    (unwind-protect
        (progn
          ;; Hand-craft a header claiming N_NODES = 1 but with a single
          ;; truncated node tag = TAG_V3_NIL (0x00) so Pass 2 starts
          ;; reading nodes (= a real baker would never emit this, but
          ;; the gate fires before we even reach the malformed body).
          (with-temp-file tmp
            (set-buffer-multibyte nil)
            (insert nelisp-sexp-bake-v3-magic)         ; 8 bytes
            (insert (unibyte-string 3 0 0 0))          ; version = 3
            (insert (unibyte-string 1))                ; kind = 1
            (insert (unibyte-string 1 0 0 0))          ; N_NODES = 1
            (insert (unibyte-string 0))                ; tag = TAG_V3_NIL
            (insert (unibyte-string 0 0 0 0))          ; N_GLOBALS = 0
            (insert (unibyte-string 0 0 0 0)))         ; N_FALLBACK = 0
          (should-error (nelisp-sexp-bake-read-image tmp)
                        :type 'nelisp-sexp-bake-unsupported))
      (when (file-exists-p tmp) (delete-file tmp)))))

(provide 'nelisp-sexp-bake-test)

;;; nelisp-sexp-bake-test.el ends here
