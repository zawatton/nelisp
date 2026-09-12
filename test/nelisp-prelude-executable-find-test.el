;;; nelisp-prelude-executable-find-test.el --- the prelude finds .exe on Windows -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; One defect, one property: `executable-find' in
;; `scripts/nelisp-stdlib-prelude.el' must find a program on windows-nt,
;; where the file on disk carries an executable suffix the caller does not
;; name.
;;
;; It could not.  The PATH sweep probed exactly `DIR/COMMAND' and nothing
;; else, so on windows-nt -- where the installed file is `sha256sum.exe',
;; never `sha256sum' -- every lookup answered nil for programs that were
;; installed and on PATH.  Measured 2026-09-12 on a Windows MSYS2 host at
;; 771e17a29: the standalone runtime's `secure-hash' fallback reported
;; "no sha256 helper ... this runtime does not search PATH" while
;; `sha256sum.exe' sat in a PATH directory.  The message was accurate about
;; `nelisp-call-process' and misleading about this function, which does
;; search PATH -- and had been blind on that platform all along.  Every
;; caller was affected; `secure-hash' is only where it surfaced.
;;
;; Testing it on the host is possible because nothing in the probe is
;; platform-specific except the suffix list: `file-exists-p' answers about
;; a file named `foo.exe' on GNU/Linux exactly as it does on Windows.  So
;; the suite plants a file that carries the suffix and nothing that does
;; not, then drives the real prelude function with `system-type' bound each
;; way.  The gnu/linux case is the control: it must still answer nil there,
;; because a POSIX host really has no such program, and it must still find
;; a suffix-less one.
;;
;; The function is read out of the prelude with the real reader and
;; evaluated under a synthetic name, the way
;; `test/nelisp-prelude-random-fixnum-test.el' does: the prelude is 10000+
;; lines of strings and comments that no pattern match survives, and the
;; host's own `executable-find' must never be touched.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar nelisp-xfnd-test--root
  (let ((here (or load-file-name buffer-file-name)))
    (if here
        (file-name-directory (directory-file-name (file-name-directory here)))
      default-directory))
  "Repository root, derived from this file rather than assumed.")

(defvar nelisp-xfnd-test--prelude-file
  (expand-file-name "scripts/nelisp-stdlib-prelude.el" nelisp-xfnd-test--root)
  "Prelude source parsed by this suite.
A variable, not a constant, so a bisect can point the same harness at
another revision of the file.")

(defvar nelisp--path-entries-key nil)
(defvar nelisp--path-entries-value nil)

(defun nelisp-xfnd-test--forms ()
  "Return every top-level form in the prelude, read with the real reader."
  (let* ((src (with-temp-buffer
                (insert-file-contents nelisp-xfnd-test--prelude-file)
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

(defun nelisp-xfnd-test--install ()
  "Evaluate the prelude's PATH search under synthetic names.
Returns the function value of its `executable-find'.  The two helpers it
calls are taken from the same file rather than reimplemented, so a change
to either is visible here."
  (let ((forms (nelisp-xfnd-test--forms))
        (guard nil) (splitter nil))
    (dolist (form forms)
      (cond ((and (consp form) (eq (car form) 'unless)
                  (equal (nth 1 form) '(fboundp 'executable-find)))
             (setq guard form))
            ((and (consp form) (eq (car form) 'defun)
                  (eq (nth 1 form) 'nelisp--split-on-char))
             (setq splitter form))))
    (unless guard
      (error "nelisp-xfnd-test: no (unless (fboundp 'executable-find) ...) in %s"
             nelisp-xfnd-test--prelude-file))
    (unless splitter
      (error "nelisp-xfnd-test: no (defun nelisp--split-on-char ...) in %s"
             nelisp-xfnd-test--prelude-file))
    (eval splitter t)
    ;; The prelude's own argument check lives behind an unrelated guard; the
    ;; property under test is the probe, so stand in for it exactly.
    (eval '(defun nelisp--check-string (x)
             (unless (stringp x) (signal 'wrong-type-argument (list 'stringp x))))
          t)
    (dolist (form (cddr guard))
      (eval (if (and (consp form) (eq (car form) 'defun)
                     (eq (nth 1 form) 'executable-find))
                (cons 'defun (cons 'nelisp-xfnd-test--find (cddr form)))
              form)
            t))
    (symbol-function 'nelisp-xfnd-test--find)))

(defmacro nelisp-xfnd-test--with-path (directory system separator &rest body)
  "Run BODY with DIRECTORY as the whole PATH, under SYSTEM and SEPARATOR."
  (declare (indent 3))
  `(let ((process-environment (cons (concat "PATH=" ,directory)
                                    process-environment))
         (system-type ,system)
         (path-separator ,separator)
         ;; The split is memoised on the PATH string; two cases in one test
         ;; share it, so clear it rather than depend on the order.
         (nelisp--path-entries-key nil)
         (nelisp--path-entries-value nil))
     ,@body))

(ert-deftest nelisp-prelude-executable-find/windows-tries-the-exe-suffix ()
  "A program installed as `NAME.exe' is found on windows-nt and only there."
  (let ((find (nelisp-xfnd-test--install))
        (directory (make-temp-file "nelisp-xfnd-" t)))
    (unwind-protect
        (let ((suffixed (expand-file-name "hasher.exe" directory)))
          (with-temp-file suffixed (insert "not really a program"))
          ;; The defect: the suffix-less probe misses the only file there.
          (nelisp-xfnd-test--with-path directory 'windows-nt ";"
            (should (equal suffixed (funcall find "hasher"))))
          ;; The control: a POSIX host really has no `hasher', and saying so
          ;; is the correct answer, not the defect.
          (nelisp-xfnd-test--with-path directory 'gnu/linux ":"
            (should (null (funcall find "hasher"))))
          ;; Naming the file outright still works on either host.
          (nelisp-xfnd-test--with-path directory 'windows-nt ";"
            (should (equal suffixed (funcall find "hasher.exe")))))
      (delete-directory directory t))))

(ert-deftest nelisp-prelude-executable-find/posix-probe-is-unchanged ()
  "A suffix-less program is still found, and the POSIX sweep adds no suffix."
  (let ((find (nelisp-xfnd-test--install))
        (directory (make-temp-file "nelisp-xfnd-" t)))
    (unwind-protect
        (let ((plain (expand-file-name "hasher" directory)))
          (with-temp-file plain (insert "not really a program"))
          (nelisp-xfnd-test--with-path directory 'gnu/linux ":"
            (should (equal plain (funcall find "hasher")))
            (should (null (funcall find "absent"))))
          ;; windows-nt ends its suffix list with "", so the same plain file
          ;; is still reachable there.
          (nelisp-xfnd-test--with-path directory 'windows-nt ";"
            (should (equal plain (funcall find "hasher")))))
      (delete-directory directory t))))

(ert-deftest nelisp-prelude-executable-find/suffix-list-is-host-shaped ()
  "The POSIX suffix list stays a single empty string.
The probe loop's cost is one `file-exists-p' per PATH entry on hosts that
never needed a suffix, and `docs/design/201' spent real measurement getting
it there."
  (nelisp-xfnd-test--install)
  (let ((system-type 'gnu/linux))
    (should (equal '("") (nelisp--exec-suffixes))))
  (let ((system-type 'windows-nt))
    (should (member ".exe" (nelisp--exec-suffixes)))
    ;; A suffix-less name must remain reachable on Windows too.
    (should (member "" (nelisp--exec-suffixes)))))

(provide 'nelisp-prelude-executable-find-test)
;;; nelisp-prelude-executable-find-test.el ends here
