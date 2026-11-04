;;; nelisp-prelude-bytecode-compile-context-test.el --- GNU file context -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-prelude-bytecode)

(defvar nelisp-prelude-bytecode-context-parent-state)
(defvar nelisp-prelude-bytecode-context-runtime-state)

(defconst nelisp-prelude-bytecode-context--source
  (concat "(eval-when-compile\n"
          "  (setq nelisp-prelude-bytecode-context-parent-state 'child-only)\n"
          "  (defun nelisp-context-compute () '((car cdr) (cons)))\n"
          "  (defconst nelisp-context-ops\n"
          "    (append (car (nelisp-context-compute)) '(nth)))\n"
          "  (defmacro nelisp-context-wrap (form)\n"
          "    (list 'list (list 'quote nelisp-context-ops) form)))\n"
          "(setq nelisp-prelude-bytecode-context-runtime-state 'must-not-run)\n"
          "(defun nelisp-context-result (x)\n"
          "  (nelisp-context-wrap (car x)))\n")
  "Source-ordered EWC fixture with computed constants, helper and macro.")

(defun nelisp-prelude-bytecode-context--generated-function (source name)
  "Find NAME's baked function object in transformed SOURCE without loading it."
  (let ((position 0) (result nil))
    (while (and (< position (length source)) (null result))
      (let* ((read-result (read-from-string source position))
             (form (car read-result)))
        (when (and (eq (car-safe form) 'prog1)
                   (equal (cadr form) (list 'quote name))
                   (eq (car-safe (nth 2 form)) 'fset))
          (setq result (nth 2 (nth 2 form))))
        (setq position (cdr read-result))))
    result))

(ert-deftest nelisp-prelude-bytecode-gnu-file-context-round-trips-and-isolates-state ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((entries (nelisp-prelude-bytecode-vendor-compile-functions
                   nelisp-prelude-bytecode-context--source
                   "compile-context-fixture.el" nil))
         (function (nth 1 (assq 'nelisp-context-result entries))))
    (should (byte-code-function-p function))
    (should (equal (funcall function '(value)) '((car cdr nth) value)))
    (should-not (boundp 'nelisp-prelude-bytecode-context-parent-state))
    (should-not (boundp 'nelisp-prelude-bytecode-context-runtime-state))
    (should-not (fboundp 'nelisp-context-compute))
    (should-not (fboundp 'nelisp-context-wrap))))

(ert-deftest nelisp-prelude-bytecode-gnu-file-context-keeps-runtime-forms-unexecuted ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((entries (nelisp-prelude-bytecode-vendor-compile-functions
                   nelisp-prelude-bytecode-context--source
                   "compile-context-runtime-fixture.el" nil)))
    (should (= (length entries) 1))
    (should (eq (caar entries) 'nelisp-context-result))
    (should-not (bound-and-true-p
                 nelisp-prelude-bytecode-context-runtime-state))))

(ert-deftest nelisp-prelude-bytecode-gnu-file-context-matches-duplicate-definitions ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((source (concat "(when t (defun nelisp-context-conditional (x) (car x)))\n"
                         "(defalias 'nelisp-context-generated\n"
                         "  (lambda (x) (car x)))\n"
                         "(defun nelisp-context-duplicate (x) (car x))\n"
                         "(defun nelisp-context-unrelated (x) (cdr x))\n"
                         "(defun nelisp-context-duplicate (x) (cdr x))\n"))
         (entries (nelisp-prelude-bytecode-vendor-compile-functions
                   source "compile-context-duplicates.el" nil))
         (duplicates (mapcar (lambda (entry) (nth 1 entry))
                             (cl-remove-if-not
                              (lambda (entry)
                                (eq (car entry) 'nelisp-context-duplicate))
                              entries)))
         (conditional (nth 1 (assq 'nelisp-context-conditional entries))))
    (should (= (length duplicates) 2))
    (should (byte-code-function-p conditional))
    (should (eq (funcall (nth 0 duplicates) '(left . right)) 'left))
    (should (eq (funcall (nth 1 duplicates) '(left . right)) 'right))))

(ert-deftest nelisp-prelude-bytecode-gnu-file-context-resolves-documentation ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((source (concat ";;; -*- lexical-binding: t -*-\n"
                         "(defun nelisp-context-documented (x) \"Fixture docs.\" (car x))\n"
                         "(defun nelisp-context-usage (x) (cdr x))\n"))
         (entries (nelisp-prelude-bytecode-vendor-compile-functions
                   source "compile-context-docs.el" nil)))
    (should (equal (nth 2 (assq 'nelisp-context-documented entries))
                   "Fixture docs.\n\n(fn X)"))
    (should (equal (nth 2 (assq 'nelisp-context-usage entries))
                   "\n\n(fn X)"))))

(ert-deftest nelisp-prelude-bytecode-gnu-file-context-never-falls-back-on-missing-entry ()
  (let* ((result (nelisp-prelude-bytecode-transform
                  "(defun nelisp-context-unmapped (x) (car x))\n"
                  "compile-context-empty.el" nil 'vendor nil t))
         (row (car (nth 2 result))))
    (should (= (nth 1 result) 0))
    (should (string-prefix-p "compile-error:" (nth 4 row)))
    (should (string-match-p "whole-file compiler has no definition"
                            (nth 4 row)))))

(ert-deftest nelisp-prelude-bytecode-gnu-file-context-times-out-and-reaps-child ()
  (let ((source "(eval-when-compile (sleep-for 10))\n(defun slow () t)\n"))
    (should-error
     (nelisp-prelude-bytecode-vendor-compile-functions
      source "compile-context-timeout.el" nil 0.1))
    (should-not (and (get-process "nelisp-prelude-gnu-compile")
                     (process-live-p
                      (get-process "nelisp-prelude-gnu-compile"))))))

(ert-deftest nelisp-prelude-bytecode-gnu-file-context-rejects-failed-ewc ()
  (let ((source (concat "(eval-when-compile (error \"fixture failure\"))\n"
                        "(defun nelisp-context-failed () (car x))\n")))
    (should-error
     (nelisp-prelude-bytecode-vendor-compile-functions
      source "compile-context-failure.el" nil))))

(ert-deftest nelisp-prelude-bytecode-vendor-byte-opt-uses-whole-file-ewc-context ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((root (expand-file-name ".."
                                 (file-name-directory
                                  (locate-library "nelisp-prelude-bytecode"))))
         (path (expand-file-name "vendor/emacs-lisp/emacs-lisp/byte-opt.el"
                                 root))
         (source (with-temp-buffer
                   (insert-file-contents path)
                   (buffer-string)))
         (dirs (list (expand-file-name "vendor/emacs-lisp/emacs-lisp"
                                       root)))
         (oracle-functions
          (nelisp-prelude-bytecode-vendor-compile-functions
           source "vendor/emacs-lisp/emacs-lisp/byte-opt.el" dirs))
         (result (nelisp-prelude-bytecode-vendor-transform
                  source "vendor/emacs-lisp/emacs-lisp/byte-opt.el"
                  dirs))
         (row (cl-find-if (lambda (candidate)
                            (eq (nth 2 candidate) 'byte-optimize-lapcode))
                          (nth 2 result)))
         (adopted-row (and row (equal (nth 3 row) "adopt") row))
         (oracle-entry (assq (nth 2 adopted-row) oracle-functions))
         (oracle-function (nth 1 oracle-entry))
         (oracle-doc (nth 2 oracle-entry))
         (constructor (nelisp-prelude-bytecode-context--generated-function
                       (car result) (nth 2 adopted-row)))
         (baked-function (and constructor (eval constructor)))
         (baked-code (and baked-function (aref baked-function 1)))
         (baked-constants (and baked-function (aref baked-function 2)))
         (baked-interactive (and baked-function
                                 (> (length baked-function) 5)
                                 (aref baked-function 5))))
    (should row)
    (should (equal (nth 3 row) "adopt"))
    (should (equal (nth 4 row)
                   "core-module-source-hash-and-opcode-allowlist"))
    (should (member 164 (read (nth 6 row))))
    (should-not (string-match-p "nelisp-prelude-gnu-file-"
                                (car result)))
    (should adopted-row)
    (should (byte-code-function-p baked-function))
    (should (byte-code-function-p oracle-function))
    (should (equal (list (aref baked-function 0) (aref baked-function 1)
                         (aref baked-function 2) (aref baked-function 3)
                         (aref baked-function 4) baked-interactive)
                   (list (aref oracle-function 0) (aref oracle-function 1)
                         (aref oracle-function 2) (aref oracle-function 3)
                         oracle-doc
                         (and (> (length oracle-function) 5)
                              (aref oracle-function 5)))))
    (should (equal (nth 7 adopted-row)
                   (format "args=%S;constants=%d;stack=%S;interactive=%S"
                           (aref baked-function 0)
                           (length baked-constants)
                           (aref baked-function 3) baked-interactive)))
    (should (eq (car-safe constructor) 'make-byte-code))
    (should (equal (cadr constructor) (list 'quote (aref baked-function 0))))
    (should (equal (aref baked-function 1) baked-code))
    (should (equal (cadr (nth 3 constructor)) baked-constants))
    (should (equal (nth 4 constructor) (aref baked-function 3)))
    (should (equal (nth 5 constructor) (aref baked-function 4)))
    (should (equal baked-interactive
                   (and (> (length baked-function) 5)
                        (aref baked-function 5))))))

(provide 'nelisp-prelude-bytecode-compile-context-test)
;;; nelisp-prelude-bytecode-compile-context-test.el ends here
