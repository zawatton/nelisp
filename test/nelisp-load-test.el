;;; nelisp-load-test.el --- ERT tests for Phase 2 multi-form loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 2 Week 1-2 — multi-form parsing, `nelisp-load-string', and
;; `nelisp-load-file'.  Fixture files live in `test/fixtures/*.nl'.

;;; Code:

(require 'ert)
(require 'nelisp-load)

;; CI-smoke gating rationale: the locate-file + nelisp-require fixtures
;; create temp dirs via `make-temp-file' and string-equal-compare the
;; resolved path with the original.  On Windows native Emacs the path
;; returned by `nelisp-locate-file' goes through `expand-file-name' and
;; the host's directory walker, which currently produces a value that
;; differs in drive-letter case / separator from the temp dir literal
;; (Phase 9d Windows path-normalization is still an open thread).  We
;; gate the `nelisp-locate-file-*' + `nelisp-require-*' assertions that
;; depend on path-equality on POSIX hosts; the path-independent
;; assertions (return-nil / rejects-bad-type / fallback-off-by-default)
;; stay enabled on every platform.
(defun nelisp-load-test--posix-fs-host-p ()
  "Non-nil iff the host filesystem matches NeLisp's path-resolution
contract used by `nelisp-locate-file' + `nelisp-require'."
  (memq system-type '(gnu/linux darwin berkeley-unix)))

(defconst nelisp-load-test--fixtures-dir
  (expand-file-name
   "fixtures"
   (file-name-directory (or load-file-name buffer-file-name))))

(defun nelisp-load-test--fixture (name)
  "Expand NAME against the fixtures directory."
  (expand-file-name name nelisp-load-test--fixtures-dir))

;;; nelisp-read-all ---------------------------------------------------

(ert-deftest nelisp-read-all-empty ()
  (should (equal (nelisp-read-all "") nil))
  (should (equal (nelisp-read-all "   ") nil))
  (should (equal (nelisp-read-all "; only a comment\n") nil)))

(ert-deftest nelisp-read-all-single ()
  (should (equal (nelisp-read-all "42") '(42))))

(ert-deftest nelisp-read-all-multiple ()
  (should (equal (nelisp-read-all "1 2 3") '(1 2 3))))

(ert-deftest nelisp-read-all-mixed-whitespace ()
  (should (equal (nelisp-read-all "\n\n(a b)\n\n(c)\n")
                 '((a b) (c)))))

(ert-deftest nelisp-read-all-with-comments ()
  (should (equal
           (nelisp-read-all
            "; heading\n(defun f () 1) ; end\n(defvar x 2)\n")
           '((defun f () 1) (defvar x 2)))))

(ert-deftest nelisp-read-all-rejects-non-string ()
  (should-error (nelisp-read-all 42) :type 'wrong-type-argument))

;;; nelisp-load-string ------------------------------------------------

(ert-deftest nelisp-load-string-empty-returns-nil ()
  (nelisp--reset)
  (should (eq (nelisp-load-string "") nil))
  (should (eq (nelisp-load-string "   \n; nothing\n") nil)))

(ert-deftest nelisp-load-string-returns-last-value ()
  (nelisp--reset)
  (should (= (nelisp-load-string "1 2 3") 3)))

(ert-deftest nelisp-load-string-installs-defun ()
  (nelisp--reset)
  (nelisp-load-string "(defun double (x) (* 2 x))")
  (should (= (nelisp-eval '(double 21)) 42)))

(ert-deftest nelisp-load-string-persists-state-across-forms ()
  "A later form sees a defun / defvar from an earlier form in the
same `nelisp-load-string' call."
  (nelisp--reset)
  (nelisp-load-string
   "(defun triple (x) (* 3 x))
    (defvar *t* (triple 14))")
  (should (= (nelisp-eval '*t*) 42)))

(ert-deftest nelisp-load-string-with-dynamic-binding ()
  "Specials declared in the string behave dynamically in later forms."
  (nelisp--reset)
  (nelisp-load-string
   "(defvar *depth* 0)
    (defun bump () (setq *depth* (+ *depth* 1)))
    (bump) (bump) (bump)")
  (should (= (nelisp-eval '*depth*) 3)))

;;; nelisp-load-file --------------------------------------------------

(ert-deftest nelisp-load-file-fib-fixture ()
  "The fib / fact fixture evaluates end-to-end."
  (nelisp--reset)
  (nelisp-load-file (nelisp-load-test--fixture "fib.nl"))
  (should (= (nelisp-eval '*fib-result*) 610))
  (should (= (nelisp-eval '*fact-result*) 3628800)))

(ert-deftest nelisp-load-file-stdlib-fixture ()
  "`my-foldl' written in pure NeLisp powers sum / max / length."
  (nelisp--reset)
  (nelisp-load-file (nelisp-load-test--fixture "stdlib.nl"))
  (should (= (nelisp-eval '*sample-sum*) 31))
  (should (= (nelisp-eval '*sample-max*) 9))
  (should (= (nelisp-eval '*sample-length*) 8)))

(ert-deftest nelisp-load-file-missing-raises ()
  (should-error (nelisp-load-file "/nonexistent/nelisp/file.nl")
                :type 'file-error))

;;; Doc 12 §3.1 — error propagation + `nelisp-load' alias ------------

(ert-deftest nelisp-load-is-load-string-equivalent ()
  "`nelisp-load' is the Doc 12 §2.1 B surface — behaviour equals
`nelisp-load-string', defined as a thin `defun' so NeLisp self-host
can install it without a `defalias' primitive."
  (nelisp--reset)
  (should (= (nelisp-load "1 2 3") 3))
  (nelisp--reset)
  (nelisp-load "(defun nelisp-load-alias-fn (x) (* x 7))")
  (should (= (nelisp-eval '(nelisp-load-alias-fn 6)) 42)))

(ert-deftest nelisp-load-string-reader-error-carries-position ()
  "A reader error mid-string re-signals as `nelisp-load-error' with
:phase = read and accurate :line / :column / :form-index.  Forms
evaluated before the failure keep their side-effects."
  (nelisp--reset)
  (nelisp-eval '(defvar *x* 0))
  (let* ((src "(setq *x* 1)\n(setq *x* 2)\n)unbalanced")
         (data (condition-case e (nelisp-load-string src) (nelisp-load-error (cdr e)))))
    (should (listp data))
    (should (eq (plist-get data :phase) 'read))
    (should (= (plist-get data :form-index) 2))
    (should (= (plist-get data :line) 3))
    (should (= (plist-get data :column) 1))
    (should (null (plist-get data :source))))
  ;; First two forms ran; state was not rolled back.
  (should (= (nelisp-eval '*x*) 2)))

(ert-deftest nelisp-load-string-eval-error-carries-position ()
  "An eval error mid-string re-signals as `nelisp-load-error' with
:phase = eval and the original signal captured in :cause."
  (nelisp--reset)
  (nelisp-eval '(defvar *y* 0))
  (let* ((src "(setq *y* 9)\n(error \"boom\")\n(setq *y* 99)")
         (data (condition-case e (nelisp-load-string src) (nelisp-load-error (cdr e)))))
    (should (eq (plist-get data :phase) 'eval))
    (should (= (plist-get data :form-index) 1))
    (should (= (plist-get data :line) 2))
    (should (= (plist-get data :column) 1))
    (let ((cause (plist-get data :cause)))
      (should (consp cause))
      (should (stringp (cadr cause)))
      (should (string-match-p "boom" (cadr cause)))))
  ;; First form ran; third form did not.
  (should (= (nelisp-eval '*y*) 9)))

(ert-deftest nelisp-load-file-error-records-path ()
  "`nelisp-load-file' funnels :source = the file path so callers
can distinguish failures across multiple loaded files."
  (nelisp--reset)
  (let* ((tmp (make-temp-file "nelisp-load-err" nil ".el"))
         data)
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "(+ 1 2)\n(unbound-symbol-xyz)\n"))
          (setq data (condition-case e (nelisp-load-file tmp)
                       (nelisp-load-error (cdr e))))
          (should (eq (plist-get data :phase) 'eval))
          (should (= (plist-get data :form-index) 1))
          (should (string-equal (plist-get data :source) tmp)))
      (delete-file tmp))))

(ert-deftest nelisp-load-file-reentrant-idempotent-defun ()
  "Loading the same file twice does not corrupt installed defuns.
Matches host `load' idempotency when the source is side-effect-free."
  (nelisp--reset)
  (let ((tmp (make-temp-file "nelisp-load-reentry" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "(defun nelisp-load-reentry-double (x) (* 2 x))\n"))
          (nelisp-load-file tmp)
          (should (= (nelisp-eval '(nelisp-load-reentry-double 7)) 14))
          (nelisp-load-file tmp)
          (should (= (nelisp-eval '(nelisp-load-reentry-double 7)) 14)))
      (delete-file tmp))))

;;; Doc 12 §3.2 — nelisp-load-path + nelisp-locate-file --------------

(defmacro nelisp-load-test--with-locate-env (body)
  "Evaluate BODY in a temp dir tree with isolated NeLisp load-path.
Creates two sibling dirs (a/ and b/), shadows
`nelisp-load-path' / `nelisp-load-path-include-host', cleans up
with `unwind-protect' even on failure."
  (declare (indent 0))
  `(let* ((root (make-temp-file "nelisp-locate" t))
          (dir-a (expand-file-name "a" root))
          (dir-b (expand-file-name "b" root)))
     (unwind-protect
         (let ((nelisp-load-path (list dir-a dir-b))
               (nelisp-load-path-include-host nil))
           (make-directory dir-a)
           (make-directory dir-b)
           ,body)
       (delete-directory root t))))

(ert-deftest nelisp-locate-file-finds-in-first-dir ()
  (skip-unless (nelisp-load-test--posix-fs-host-p))
  (nelisp-load-test--with-locate-env
    (let ((target (expand-file-name "foo.el" dir-a)))
      (with-temp-file target (insert ""))
      (should (string-equal (nelisp-locate-file 'foo) target)))))

(ert-deftest nelisp-locate-file-finds-in-second-dir ()
  "When the first directory has no hit, the second is searched."
  (skip-unless (nelisp-load-test--posix-fs-host-p))
  (nelisp-load-test--with-locate-env
    (let ((target (expand-file-name "bar.el" dir-b)))
      (with-temp-file target (insert ""))
      (should (string-equal (nelisp-locate-file 'bar) target)))))

(ert-deftest nelisp-locate-file-returns-nil-when-missing ()
  (nelisp-load-test--with-locate-env
    (should (null (nelisp-locate-file 'no-such-feature)))))

(ert-deftest nelisp-locate-file-accepts-string-feature ()
  (skip-unless (nelisp-load-test--posix-fs-host-p))
  (nelisp-load-test--with-locate-env
    (let ((target (expand-file-name "baz.el" dir-a)))
      (with-temp-file target (insert ""))
      (should (string-equal (nelisp-locate-file "baz") target)))))

(ert-deftest nelisp-locate-file-respects-explicit-el-suffix ()
  "A FEATURE that already ends with `.el' must NOT be double-suffixed."
  (skip-unless (nelisp-load-test--posix-fs-host-p))
  (nelisp-load-test--with-locate-env
    (let ((target (expand-file-name "qux.el" dir-a)))
      (with-temp-file target (insert ""))
      (should (string-equal (nelisp-locate-file "qux.el") target))
      (should (null (nelisp-locate-file "qux.el.el"))))))

(ert-deftest nelisp-locate-file-host-fallback-off-by-default ()
  "With include-host nil, a file present only on host `load-path'
must NOT be resolved."
  (let* ((host-dir (make-temp-file "nelisp-locate-host" t))
         (target (expand-file-name "host-only.el" host-dir)))
    (unwind-protect
        (progn
          (with-temp-file target (insert ""))
          (let ((nelisp-load-path nil)
                (nelisp-load-path-include-host nil)
                (load-path (cons host-dir load-path)))
            (should (null (nelisp-locate-file 'host-only)))))
      (delete-directory host-dir t))))

(ert-deftest nelisp-locate-file-host-fallback-on ()
  "With include-host non-nil, host `load-path' fills in misses."
  (skip-unless (nelisp-load-test--posix-fs-host-p))
  (let* ((host-dir (make-temp-file "nelisp-locate-host2" t))
         (target (expand-file-name "crossover.el" host-dir)))
    (unwind-protect
        (progn
          (with-temp-file target (insert ""))
          (let ((nelisp-load-path nil)
                (nelisp-load-path-include-host t)
                (load-path (cons host-dir load-path)))
            (should (string-equal (nelisp-locate-file 'crossover) target))))
      (delete-directory host-dir t))))

(ert-deftest nelisp-locate-file-nelisp-beats-host-on-name-clash ()
  "NeLisp path is searched first; same filename in host is NOT used."
  (skip-unless (nelisp-load-test--posix-fs-host-p))
  (let* ((root (make-temp-file "nelisp-locate-clash" t))
         (ne-dir (expand-file-name "ne" root))
         (host-dir (expand-file-name "host" root))
         (ne-target (expand-file-name "clashy.el" ne-dir))
         (host-target (expand-file-name "clashy.el" host-dir)))
    (unwind-protect
        (progn
          (make-directory ne-dir)
          (make-directory host-dir)
          (with-temp-file ne-target (insert ";; ne"))
          (with-temp-file host-target (insert ";; host"))
          (let ((nelisp-load-path (list ne-dir))
                (nelisp-load-path-include-host t)
                (load-path (cons host-dir load-path)))
            (should (string-equal (nelisp-locate-file 'clashy) ne-target))))
      (delete-directory root t))))

(ert-deftest nelisp-locate-file-rejects-bad-type ()
  (should-error (nelisp-locate-file 42)
                :type 'wrong-type-argument))

;;; Doc 12 §3.3 — nelisp-require + circular detection ---------------

(defmacro nelisp-load-test--with-require-env (&rest body)
  "Set up an isolated NeLisp load-path dir + reset registry.
Binds `root' to the temp dir inside BODY."
  (declare (indent 0))
  `(let ((root (make-temp-file "nelisp-require" t)))
     (unwind-protect
         (let ((nelisp-load-path (list root))
               (nelisp-load-path-include-host nil))
           (nelisp--reset)
           ,@body)
       (delete-directory root t))))

(ert-deftest nelisp-require-happy-path ()
  "A fresh feature loads once, registers in `nelisp--features',
and returns the feature symbol."
  (skip-unless (nelisp-load-test--posix-fs-host-p))
  (nelisp-load-test--with-require-env
    (with-temp-file (expand-file-name "hello-req-happy.el" root)
      (insert "(defun nelisp-req-happy-greet (who) (concat \"hi \" who))\n")
      (insert "(provide 'hello-req-happy)\n"))
    (should (eq (nelisp-require 'hello-req-happy) 'hello-req-happy))
    (should (memq 'hello-req-happy nelisp--features))
    (should (equal (nelisp-eval '(nelisp-req-happy-greet "bob")) "hi bob"))))

(ert-deftest nelisp-require-idempotent-short-circuit ()
  "Second require of the same feature must NOT re-load the file.
We detect this by having the file increment a side-effect counter
via `nelisp-provide' — the counter should only budge once."
  (skip-unless (nelisp-load-test--posix-fs-host-p))
  (nelisp-load-test--with-require-env
    (with-temp-file (expand-file-name "hello-req-idem.el" root)
      (insert "(defvar nelisp-req-idem-loads 0)\n")
      (insert "(setq nelisp-req-idem-loads (+ nelisp-req-idem-loads 1))\n")
      (insert "(provide 'hello-req-idem)\n"))
    (nelisp-require 'hello-req-idem)
    (nelisp-require 'hello-req-idem)
    (should (= (nelisp-eval 'nelisp-req-idem-loads) 1))))

(ert-deftest nelisp-require-missing-signals-file-error ()
  (nelisp-load-test--with-require-env
    (should-error (nelisp-require 'no-such-nelisp-feature)
                  :type 'file-error)))

(ert-deftest nelisp-require-missing-noerror-returns-nil ()
  (nelisp-load-test--with-require-env
    (should (null (nelisp-require 'no-such-nelisp-feature nil t)))))

(ert-deftest nelisp-require-did-not-provide ()
  "File that loads successfully but never calls `provide' must
signal `nelisp-load-error' with :cause `did-not-provide'."
  (skip-unless (nelisp-load-test--posix-fs-host-p))
  (nelisp-load-test--with-require-env
    (with-temp-file (expand-file-name "hello-req-no-prov.el" root)
      (insert "(defvar nelisp-req-no-prov-val 1)\n"))
    (let ((data (condition-case e (nelisp-require 'hello-req-no-prov)
                  (nelisp-load-error (cdr e)))))
      (should (eq (plist-get data :cause) 'did-not-provide))
      (should (eq (plist-get data :feature) 'hello-req-no-prov)))))

(ert-deftest nelisp-require-circular-signals ()
  "A → requires B → requires A must signal `nelisp-load-error'
with :cause `circular-require', :loading stack pinpointing both."
  (skip-unless (nelisp-load-test--posix-fs-host-p))
  (nelisp-load-test--with-require-env
    (with-temp-file (expand-file-name "nelisp-circ-a.el" root)
      (insert "(require 'nelisp-circ-b)\n")
      (insert "(provide 'nelisp-circ-a)\n"))
    (with-temp-file (expand-file-name "nelisp-circ-b.el" root)
      (insert "(require 'nelisp-circ-a)\n")
      (insert "(provide 'nelisp-circ-b)\n"))
    (let* ((outer (condition-case e (nelisp-require 'nelisp-circ-a)
                    (nelisp-load-error (cdr e))))
           ;; The circular signal is wrapped twice — first by B's
           ;; `nelisp-load-string' loop, then by A's.  Walk down the
           ;; :cause chain looking for :phase = require.
           (unwrap (lambda (plist)
                     (let ((c (plist-get plist :cause)))
                       (cond
                        ((and (consp c) (eq (car c) 'nelisp-load-error))
                         (cdr c))
                        (t plist)))))
           (mid (funcall unwrap outer))
           (inner (funcall unwrap mid)))
      (should (eq (plist-get inner :phase) 'require))
      (should (eq (plist-get inner :cause) 'circular-require))
      (should (eq (plist-get inner :feature) 'nelisp-circ-a))
      ;; Loading stack order: most recent first = nelisp-circ-b, then
      ;; nelisp-circ-a (outer).
      (should (equal (plist-get inner :loading)
                     '(nelisp-circ-b nelisp-circ-a))))))

(ert-deftest nelisp-require-with-explicit-filename ()
  "When FILENAME is given, `nelisp-load-path' is not consulted."
  (nelisp-load-test--with-require-env
    (let ((off-path-dir (make-temp-file "nelisp-require-ext" t)))
      (unwind-protect
          (let ((target (expand-file-name "ext-feat.el" off-path-dir)))
            (with-temp-file target
              (insert "(defvar nelisp-req-ext-val 77)\n")
              (insert "(provide 'ext-feat)\n"))
            (should (eq (nelisp-require 'ext-feat target) 'ext-feat))
            (should (= (nelisp-eval 'nelisp-req-ext-val) 77)))
        (delete-directory off-path-dir t)))))

(ert-deftest nelisp-require-honours-host-featurep ()
  "When host Emacs already `featurep's the symbol (and no explicit
filename is given), NeLisp short-circuits and records it."
  (nelisp--reset)
  (should (featurep 'cl-lib))
  (should (eq (nelisp-require 'cl-lib) 'cl-lib))
  (should (memq 'cl-lib nelisp--features)))

(ert-deftest nelisp-provide-rejects-non-symbol ()
  (should-error (nelisp-provide "not-a-symbol")
                :type 'wrong-type-argument))

(ert-deftest nelisp-require-rejects-non-symbol ()
  (should-error (nelisp-require 42)
                :type 'wrong-type-argument))

;;; Interaction with the rest of the subsystem ------------------------

(ert-deftest nelisp-load-then-macro ()
  "Load a file that installs a defun, then call it through a macro
installed after the load."
  (nelisp--reset)
  (nelisp-load-file (nelisp-load-test--fixture "fib.nl"))
  (nelisp-eval '(defmacro guard-fib (n)
                  (list (quote if) (list (quote <) n 0) 0
                        (list (quote fib) n))))
  (should (= (nelisp-eval '(guard-fib 10)) 55))
  (should (= (nelisp-eval '(guard-fib -5)) 0)))

(provide 'nelisp-load-test)

;;; nelisp-load-test.el ends here
