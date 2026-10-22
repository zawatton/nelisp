;;; nelisp-staged-handler-guard-test.el --- tests for the staged handler guard -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Host-Emacs ERT tests of scripts/nelisp-staged-handler-guard.el:
;;   emacs --batch -Q -L src -L scripts -L test -l ert \
;;     -l test/nelisp-staged-handler-guard-test.el -f ert-run-tests-batch-and-exit
;; The real build is covered by `tools/ai/batch-f-check.sh guard'; these tests
;; use synthetic trees, and a plain `emacs --batch' standing in for the binary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-staged-handler-guard)

(defconst nsgt--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Root of the repository these tests belong to.")

(defun nsgt--with-tree (build-script vendor-files fn)
  "Call FN with the root of a temporary tree holding BUILD-SCRIPT and VENDOR-FILES.
BUILD-SCRIPT is the text of scripts/nelisp-standalone-build.el, VENDOR-FILES an
alist of (RELATIVE-FILE . TEXT)."
  (let ((root (make-temp-file "staged-guard-tree-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "scripts" root) t)
          (with-temp-file (expand-file-name "scripts/nelisp-standalone-build.el" root)
            (insert build-script))
          (dolist (file vendor-files)
            (let ((path (expand-file-name (car file) root)))
              (make-directory (file-name-directory path) t)
              (with-temp-file path (insert (cdr file)))))
          (clrhash nelisp-staged-handler-guard--forms-cache)
          (funcall fn root))
      (clrhash nelisp-staged-handler-guard--forms-cache)
      (delete-directory root t))))

(defconst nsgt--vendor
  ";; -*- lexical-binding: t; -*-
(defun a (x) (declare (compiler-macro (lambda (form) (helper-one x) form))) x)
(defun b (x) \"doc\" (declare (gv-expander (lambda (do) (helper-two do)))) x)
(defun c (x) (declare (gv-setter helper-three)) x)
(defun d (x) (declare (compiler-macro d--cmacro) (pure t)) x)
(defun d--cmacro (form x) (helper-four form x))
(defun e (x) (declare (indent 1)) x)
(defun f2 (x) (declare (compiler-macro
                       (lambda (form)
                         (when (stringp x) (dolist (k (list x)) (helper-five k)))
                         form)))
  x)
(defvar v1 1)
"
  "A vendored file with one definition per handler shape.")

(defconst nsgt--build
  "(defun stage-it ()
  (concat (nelisp-vendor-source-forms \"vendor/g.el\" '(a b c d))
          (nelisp-standalone--vendor-function-source \"vendor/g.el\" 'e)
          (nelisp-vendor-source-form \"vendor/g.el\" 'f2)
          (nelisp-vendor-source-form \"vendor/g.el\" 'v1)))
(defun nelisp-standalone--vendor-function-source (relative-file function-name)
  (nelisp-vendor-source-form relative-file function-name))
"
  "A build script that stages the definitions of `nsgt--vendor'.")

(defun nsgt--spec (specs owner)
  (cl-find owner specs :key (lambda (s) (plist-get s :owner))))

(ert-deftest nelisp-staged-handler-guard-test-synthetic-declarations ()
  (nsgt--with-tree
   nsgt--build `(("vendor/g.el" . ,nsgt--vendor))
   (lambda (root)
     (let* ((scan (nelisp-staged-handler-guard-specs root))
            (specs (plist-get scan :specs)))
       (should (equal (plist-get scan :unresolved) nil))
       (should (equal (plist-get scan :missing) nil))
       (should (= (plist-get scan :sites) 4))
       (should (= (plist-get scan :names) 7))
       (should (= (length specs) 5))
       (let ((a (nsgt--spec specs 'a)) (b (nsgt--spec specs 'b))
             (c (nsgt--spec specs 'c)) (d (nsgt--spec specs 'd))
             (f2 (nsgt--spec specs 'f2)))
         (should (eq (plist-get a :kind) 'compiler-macro))
         (should (memq 'helper-one (plist-get a :callees)))
         (should (eq (plist-get b :kind) 'gv-expander))
         (should (memq 'helper-two (plist-get b :callees)))
         (should (eq (plist-get c :kind) 'gv-setter))
         (should (equal (plist-get c :callees) '(helper-three)))
         ;; A handler named by a symbol needs that symbol; its own callees are not followed.
         (should (equal (plist-get d :callees) '(d--cmacro)))
         ;; Macros the body uses, functions it calls after expansion, not variables.
         (dolist (needed '(helper-five dolist when stringp list))
           (should (memq needed (plist-get f2 :callees))))
         (dolist (variable '(k x form))
           (should-not (memq variable (plist-get f2 :callees)))))
       (should-not (nsgt--spec specs 'e))))))

(ert-deftest nelisp-staged-handler-guard-test-unresolved-and-missing ()
  (nsgt--with-tree
   "(defun stage-it (file names) (nelisp-vendor-source-forms file names))
(defun nelisp-standalone--vendor-pass-through (file) (nelisp-vendor-source-form file 'a))
(defun stage-more () (nelisp-vendor-source-forms \"vendor/g.el\" '(a nowhere)))
"
   `(("vendor/g.el" . ,nsgt--vendor))
   (lambda (root)
     (let ((scan (nelisp-staged-handler-guard-specs root)))
       ;; The call with variable arguments counts, the wrapper's pass-through does not.
       (should (= (length (plist-get scan :unresolved)) 1))
       (should (equal (car (car (plist-get scan :unresolved))) "stage-it"))
       (should (equal (plist-get scan :missing) '(("vendor/g.el" . nowhere))))))))

(ert-deftest nelisp-staged-handler-guard-test-whole-file ()
  (nsgt--with-tree
   "(defun stage-it () (nelisp-vendor-source-whole-file \"vendor/g.el\"))"
   `(("vendor/g.el" . ,nsgt--vendor))
   (lambda (root)
     (let ((scan (nelisp-staged-handler-guard-specs root)))
       (should (= (length (plist-get scan :specs)) 5))))))

(ert-deftest nelisp-staged-handler-guard-test-call-walker ()
  (let ((calls (nelisp-staged-handler-guard--calls
                '(let ((a (f1 x)) b)
                   (cond ((f2 a) (f3)) (t 'not-a-call))
                   (condition-case err (f6) (error (f7 err)))
                   (mapcar #'f4 (list (function (lambda (y) (f5 y)))))
                   ((lambda (z) (f8 z)) 1)
                   (if (f9) (progn (setq b (f10))) (quote (f11))))
                nil)))
    (dolist (called '(f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 mapcar list))
      (should (memq called calls)))
    (dolist (not-called '(let cond condition-case if progn setq quote function a b x y z
                          not-a-call f11 err))
      (should-not (memq not-called calls)))))

(ert-deftest nelisp-staged-handler-guard-test-real-tree ()
  (clrhash nelisp-staged-handler-guard--forms-cache)
  (let* ((scan (nelisp-staged-handler-guard-specs nsgt--root))
         (specs (plist-get scan :specs)))
    (should (equal (plist-get scan :unresolved) nil))
    (should (equal (plist-get scan :missing) nil))
    ;; The 28 c[ad]+r functions, zerop, eval-after-load, keymap-set, define-keymap.
    (should (>= (length specs) 32))
    (should (= 28 (cl-count 'internal--compiler-macro-cXXr specs
                            :key (lambda (s) (plist-get s :handler)))))
    (should (equal (plist-get (nsgt--spec specs 'keymap-set) :callees)
                   '(keymap--compile-check)))
    (should (equal (plist-get (nsgt--spec specs 'define-keymap) :callees)
                   '(define-keymap--compile)))
    (dolist (owner '(zerop eval-after-load))
      (should (nsgt--spec specs owner)))))

;;;; Against a binary: a plain `emacs --batch' with chosen functions removed stands in.

(defun nsgt--stand-in (removed)
  "Return the file of a script that runs GNU Emacs with the functions REMOVED unbound."
  (let ((file (make-temp-file "staged-guard-binary-" nil ".sh"))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-file file
      (insert (format "#!/bin/sh\nexec %s --batch -Q --eval %s \"$@\"\n"
                      (shell-quote-argument emacs)
                      (shell-quote-argument
                       (format "(mapc #'fmakunbound '%S)" removed)))))
    (set-file-modes file #o755)
    file))

(ert-deftest nelisp-staged-handler-guard-test-binary-with-and-without-the-helpers ()
  (clrhash nelisp-staged-handler-guard--forms-cache)
  (let ((whole (nsgt--stand-in nil))
        (cut (nsgt--stand-in '(keymap--compile-check define-keymap--compile))))
    (unwind-protect
        (progn
          (let ((result (nelisp-staged-handler-guard-check nsgt--root whole)))
            (should (plist-get result :ok))
            (should (equal (plist-get result :failures) nil))
            (should (string-prefix-p "staged-handler-guard: PASS" (plist-get result :report))))
          (let ((result (nelisp-staged-handler-guard-check nsgt--root cut)))
            (should-not (plist-get result :ok))
            (should (equal (sort (mapcar (lambda (row) (list (nth 0 row) (nth 2 row)))
                                         (plist-get result :failures))
                                 (lambda (x y) (string< (symbol-name (car x))
                                                        (symbol-name (car y)))))
                           '((define-keymap define-keymap--compile)
                             (keymap-set keymap--compile-check))))
            (should (string-match-p "FAIL keymap-set compiler-macro needs keymap--compile-check"
                                    (plist-get result :report)))
            (should-error (nelisp-staged-handler-guard-run nsgt--root cut))))
      (delete-file whole)
      (delete-file cut))))

(ert-deftest nelisp-staged-handler-guard-test-callee-void-in-gnu-is-allowed ()
  (nsgt--with-tree
   "(defun stage-it () (nelisp-vendor-source-form \"vendor/g.el\" 'a))"
   '(("vendor/g.el" . "(defun a (x) (declare (compiler-macro (lambda (form) (nsgt-no-such-function x) form))) x)"))
   (lambda (root)
     (let* ((binary (nsgt--stand-in nil))
            (result (unwind-protect (nelisp-staged-handler-guard-check root binary)
                      (delete-file binary))))
       ;; Void in the stand-in and in stock GNU Emacs: allowed, with GNU's answer recorded.
       (should (plist-get result :ok))
       (should (equal (plist-get result :failures) nil))
       (should (equal (mapcar (lambda (row) (nth 2 row)) (plist-get result :allowed))
                      '(nsgt-no-such-function)))
       (should (string-match-p "ALLOWED a compiler-macro calls nsgt-no-such-function: void in the binary and in GNU Emacs"
                               (plist-get result :report)))))))

(provide 'nelisp-staged-handler-guard-test)

;;; nelisp-staged-handler-guard-test.el ends here
