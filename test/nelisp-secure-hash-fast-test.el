;;; nelisp-secure-hash-fast-test.el --- parity for the fast prelude secure-hash -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; `secure-hash' in `scripts/nelisp-stdlib-prelude.el' shells out to
;; coreutils.  Doc 205 P3 follow-up (segment C2, 2026-09-28) rewrote it to
;; drop a `split-string' call that cost 100-450 ms per call regardless of
;; the hashed input's size -- see the file header comment beside
;; `nelisp--secure-hash-helper' for the full breakdown -- and widened it
;; from `sha1'/`sha256' only to every algorithm host Emacs's
;; `secure-hash-algorithms' lists, plus a real BINARY implementation.
;;
;; This suite is the correctness half of that change.  It reads the
;; prelude's own `secure-hash' (and its three private helpers) out of the
;; source with the real reader and evaluates them under a synthetic name,
;; the way `test/nelisp-prelude-executable-find-test.el' and
;; `test/nelisp-prelude-random-fixnum-test.el' do: the prelude is 10000+
;; lines of strings and comments no regexp survives unscathed, and host
;; Emacs's own `secure-hash' must never be touched.  Every case is checked
;; against that same host `secure-hash', on GNU Emacs -- never against a
;; value this suite computes independently, since that would just be
;; re-deriving the implementation under test.
;;
;; The suite runs entirely on host Emacs: the fast path under test calls
;; `call-process' on `md5sum'/`sha1sum'/.../`shasum', which exist on host
;; Emacs's `PATH' precisely because they are what host Emacs's own
;; `secure-hash' does NOT need but this tree's standalone runtime does.
;; No standalone binary is required to run this file.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar nelisp-sh-fast-test--root
  (let ((here (or load-file-name buffer-file-name)))
    (if here
        (file-name-directory (directory-file-name (file-name-directory here)))
      default-directory))
  "Repository root, derived from this file rather than assumed.")

(defvar nelisp-sh-fast-test--prelude-file
  (expand-file-name "scripts/nelisp-stdlib-prelude.el" nelisp-sh-fast-test--root)
  "Prelude source parsed by this suite.")

(defun nelisp-sh-fast-test--forms ()
  "Return every top-level form in the prelude, read with the real reader."
  (let* ((src (with-temp-buffer
                (insert-file-contents nelisp-sh-fast-test--prelude-file)
                (buffer-string)))
         (pos 0) (len (length src)) (forms nil))
    (while (< pos len)
      (let ((r (condition-case nil (read-from-string src pos len)
                 (end-of-file nil))))
        (if (null r)
            (setq pos len)
          (setq pos (cdr r))
          (push (car r) forms))))
    (nreverse forms)))

(defun nelisp-sh-fast-test--find-guarded-form (forms predicate guard-name head name)
  "Return the (HEAD NAME ...) form guarded by `(unless (PREDICATE 'GUARD-NAME))'.
PREDICATE is `fboundp' for a `defun' or `boundp' for a `defvar'/`defconst'.
Returns nil, rather than signalling, when no such form is found."
  (cl-some
   (lambda (form)
     (and (consp form) (eq (car form) 'unless)
          (equal (nth 1 form) (list predicate (list 'quote guard-name)))
          (cl-some (lambda (inner)
                     (and (consp inner) (eq (car inner) head)
                          (eq (nth 1 inner) name)
                          inner))
                   (cddr form))))
   forms))

(defun nelisp-sh-fast-test--guarded-defun (forms name)
  "Return NAME's `(unless (fboundp NAME) (defun NAME ...))' form.
Signals an error naming NAME when no such form is found, so a rename or
restructure in the prelude fails this suite loudly instead of silently
testing nothing."
  (or (nelisp-sh-fast-test--find-guarded-form forms 'fboundp name 'defun name)
      (error "nelisp-sh-fast-test: no (unless (fboundp '%S) (defun %S ...)) in %s"
             name name nelisp-sh-fast-test--prelude-file)))

(defun nelisp-sh-fast-test--guarded-defvar (forms name)
  "Return NAME's `(unless (boundp NAME) (defvar-or-defconst NAME ...))' form.
Signals an error naming NAME when no such form is found (as either a
`defvar' or a `defconst'), so a rename or restructure in the prelude
fails this suite loudly instead of silently testing nothing."
  (or (nelisp-sh-fast-test--find-guarded-form forms 'boundp name 'defvar name)
      (nelisp-sh-fast-test--find-guarded-form forms 'boundp name 'defconst name)
      (error "nelisp-sh-fast-test: no (unless (boundp '%S) (defvar-or-defconst %S ...)) in %s"
             name name nelisp-sh-fast-test--prelude-file)))

(defun nelisp-sh-fast-test--install ()
  "Evaluate the prelude's fast `secure-hash' under a synthetic name.
Returns the function value of the renamed `secure-hash', with its three
private helpers (`nelisp--secure-hash-helper',
`nelisp--secure-hash-hex-digit', `nelisp--secure-hash-hex-to-bytes')
installed under their real names -- those never collide with anything
host Emacs defines, so only the public name is renamed."
  (let ((forms (nelisp-sh-fast-test--forms)))
    (eval (nelisp-sh-fast-test--guarded-defvar forms 'nelisp--secure-hash-widths)
          t)
    (eval (nelisp-sh-fast-test--guarded-defvar
           forms 'nelisp--secure-hash-helper-cache)
          t)
    (eval (nelisp-sh-fast-test--guarded-defun
           forms 'nelisp--secure-hash-helper)
          t)
    (eval (nelisp-sh-fast-test--guarded-defun forms 'nelisp--secure-hash-hex-digit)
          t)
    (eval (nelisp-sh-fast-test--guarded-defun
           forms 'nelisp--secure-hash-hex-to-bytes)
          t)
    (let ((secure-hash-form (nelisp-sh-fast-test--guarded-defun forms 'secure-hash)))
      (eval (cons 'defun (cons 'nelisp-sh-fast-test--secure-hash
                                (cddr secure-hash-form)))
            t))
    ;; Every call resolves the helper program fresh under the synthetic
    ;; name too: clear any cache a previous suite run in this same Emacs
    ;; process left behind.
    (setq nelisp--secure-hash-helper-cache nil)
    (symbol-function 'nelisp-sh-fast-test--secure-hash)))

(defvar nelisp-sh-fast-test--fn nil
  "The installed fast `secure-hash', set once per test run by `should-parity'.")

(defmacro nelisp-sh-fast-test--should-parity (algorithm object &rest start-end-binary)
  "Assert the fast `secure-hash' agrees with host Emacs's on the same call."
  `(should (equal (apply #'secure-hash ,algorithm ,object (list ,@start-end-binary))
                  (apply nelisp-sh-fast-test--fn ,algorithm ,object
                         (list ,@start-end-binary)))))

(ert-deftest nelisp-secure-hash-fast/algorithms-match-host-on-ascii ()
  "Every one of host Emacs's `secure-hash-algorithms' matches on ASCII."
  (let ((nelisp-sh-fast-test--fn (nelisp-sh-fast-test--install)))
    (dolist (algorithm '(md5 sha1 sha224 sha256 sha384 sha512))
      (nelisp-sh-fast-test--should-parity algorithm "abc")
      (nelisp-sh-fast-test--should-parity algorithm ""))))

(ert-deftest nelisp-secure-hash-fast/sizes-up-to-64kib-match-host ()
  "Sizes spanning the perf table's range (16 B - 64 KiB) match host Emacs."
  (let ((nelisp-sh-fast-test--fn (nelisp-sh-fast-test--install)))
    (dolist (size '(16 1024 16384 65536))
      (let ((text (make-string size ?a)))
        (nelisp-sh-fast-test--should-parity 'sha256 text)
        (nelisp-sh-fast-test--should-parity 'sha1 text)))))

(ert-deftest nelisp-secure-hash-fast/multibyte-string-matches-host ()
  "A multibyte OBJECT is hashed as its UTF-8 bytes, matching host Emacs.
`nelisp-mach-o-write.el' and every artifact hash in `lisp/nelisp-artifact.el'
depend on this: their inputs are ordinary multibyte Lisp strings, not
pre-encoded byte strings."
  (let ((nelisp-sh-fast-test--fn (nelisp-sh-fast-test--install)))
    (nelisp-sh-fast-test--should-parity 'sha256 "日本語")
    (nelisp-sh-fast-test--should-parity 'sha1 "café éèê")
    (should (equal (funcall nelisp-sh-fast-test--fn 'sha256 "日本語")
                   (funcall nelisp-sh-fast-test--fn 'sha256
                            (encode-coding-string "日本語" 'utf-8))))))

(ert-deftest nelisp-secure-hash-fast/unibyte-string-matches-host ()
  "A unibyte OBJECT (raw bytes, no re-encoding) matches host Emacs."
  (let ((nelisp-sh-fast-test--fn (nelisp-sh-fast-test--install))
        (bytes (apply #'unibyte-string (number-sequence 0 255))))
    (nelisp-sh-fast-test--should-parity 'sha256 bytes)
    (nelisp-sh-fast-test--should-parity 'md5 bytes)))

(ert-deftest nelisp-secure-hash-fast/string-start-end-matches-host ()
  "START/END narrow a string the way host Emacs does."
  (let ((nelisp-sh-fast-test--fn (nelisp-sh-fast-test--install)))
    (nelisp-sh-fast-test--should-parity 'sha256 "xxabcxx" 2 5)
    (nelisp-sh-fast-test--should-parity 'sha256 "xxabc" 2)))

(ert-deftest nelisp-secure-hash-fast/buffer-object-matches-host ()
  "A buffer OBJECT, with and without START/END, matches host Emacs."
  (let ((nelisp-sh-fast-test--fn (nelisp-sh-fast-test--install)))
    (with-temp-buffer
      (insert "hello world")
      (should (equal (secure-hash 'sha256 (current-buffer))
                     (funcall nelisp-sh-fast-test--fn 'sha256 (current-buffer)))))
    (with-temp-buffer
      (insert "xxhelloxx")
      (should (equal (secure-hash 'sha256 (current-buffer) 3 8)
                     (funcall nelisp-sh-fast-test--fn 'sha256
                              (current-buffer) 3 8))))))

(ert-deftest nelisp-secure-hash-fast/binary-argument-matches-host ()
  "BINARY non-nil returns the raw digest bytes, matching host Emacs.
Regression coverage for the bug the rewrite fixed: the pre-fix prelude
ignored BINARY and always returned the hex string, so
`nelisp-mach-o--exe-uuid' (BINARY = t) was reading hex characters as if
they were already raw bytes."
  (let ((nelisp-sh-fast-test--fn (nelisp-sh-fast-test--install)))
    (dolist (algorithm '(md5 sha1 sha224 sha256 sha384 sha512))
      (let ((host (secure-hash algorithm "abc" nil nil t))
            (fast (funcall nelisp-sh-fast-test--fn algorithm "abc" nil nil t)))
        (should (equal host fast))
        (should-not (multibyte-string-p fast))
        (should (= (length fast) (/ (length (secure-hash algorithm "abc")) 2)))))))

(ert-deftest nelisp-secure-hash-fast/helper-cache-does-not-change-the-answer ()
  "A second call for the same ALGORITHM (hitting the executable-find cache)
still answers the same digest as the first, uncached call."
  (let ((nelisp-sh-fast-test--fn (nelisp-sh-fast-test--install)))
    (should (null nelisp--secure-hash-helper-cache))
    (let ((first (funcall nelisp-sh-fast-test--fn 'sha256 "abc")))
      (should nelisp--secure-hash-helper-cache)
      (should (equal first (funcall nelisp-sh-fast-test--fn 'sha256 "abc")))
      (should (equal first (secure-hash 'sha256 "abc"))))))

(ert-deftest nelisp-secure-hash-fast/unsupported-algorithm-signals ()
  "An ALGORITHM outside `secure-hash-algorithms' signals, not silently nil."
  (let ((nelisp-sh-fast-test--fn (nelisp-sh-fast-test--install)))
    (should-error (funcall nelisp-sh-fast-test--fn 'crc32 "abc"))))

(provide 'nelisp-secure-hash-fast-test)
;;; nelisp-secure-hash-fast-test.el ends here
