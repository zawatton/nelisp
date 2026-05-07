;;; nelisp-stdlib-eval-special-test.el --- ERT for Tier 2 elisp macros  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7 Stage 7.3.a (Doc 67) — verify the 18 Tier 2 macros installed
;; by `lisp/nelisp-stdlib-eval-special.el' expand to the expected shape.
;;
;; Stage 7.3.a is *parallel install only*: the Rust `apply_special'
;; match arms still preempt these macros at runtime (see
;; `build-tool/src/eval/mod.rs:191'), so we cannot verify behaviour
;; through `(when X Y)' direct evaluation.  Instead we drive
;; `(macroexpand-1 '(when X Y))' (= a Rust builtin added in the same
;; commit) and string-compare the printed expansion.
;;
;; Subprocess pattern mirrors `nelisp-stdlib-prn-test.el'.

;;; Code:

(require 'ert)

(defconst nelisp-stdlib-eval-special-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir (expand-file-name ".." test-dir))))

(defconst nelisp-stdlib-eval-special-test--bin
  (and nelisp-stdlib-eval-special-test--repo-root
       (expand-file-name "target/release/nelisp"
                         nelisp-stdlib-eval-special-test--repo-root)))

(defun nelisp-stdlib-eval-special-test--skip-unless-built ()
  (unless (and nelisp-stdlib-eval-special-test--bin
               (file-executable-p nelisp-stdlib-eval-special-test--bin))
    (ert-skip
     (format "nelisp binary missing — run `cargo build --release' (looked at %s)"
             nelisp-stdlib-eval-special-test--bin))))

(defun nelisp-stdlib-eval-special-test--eval (expr-string)
  "Run `nelisp eval EXPR-STRING' and return (EXIT-CODE . STDOUT)."
  (with-temp-buffer
    (let ((code (call-process nelisp-stdlib-eval-special-test--bin nil t nil
                              "eval" expr-string)))
      (cons code (buffer-substring-no-properties (point-min) (point-max))))))

(defun nelisp-stdlib-eval-special-test--expand (form-string)
  "Return printed `(macroexpand-1 'FORM)' for FORM-STRING."
  (let* ((wrapper (format "(progn (princ (prin1-to-string (macroexpand-1 (quote %s)))) nil)"
                          form-string))
         (r (nelisp-stdlib-eval-special-test--eval wrapper)))
    (should (eq (car r) 0))
    (let ((out (string-trim-right (cdr r))))
      ;; (progn ... nil) → trailing `nil\n' from auto-print.
      (if (string-suffix-p "nil" out)
          (string-trim-right (substring out 0 (- (length out) 3)))
        out))))

;;; ---- single-form macros ----------------------------------------------

(ert-deftest nelisp-eval-special/when-shape ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(when c a b)")
                   "(if c (progn a b) nil)")))

(ert-deftest nelisp-eval-special/unless-shape ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(unless c a b)")
                   "(if c nil (progn a b))")))

(ert-deftest nelisp-eval-special/cond-empty ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(cond)")
                   "nil")))

(ert-deftest nelisp-eval-special/cond-single-clause-with-body ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(cond (t 1 2))")
                   "(if t (progn 1 2) (cond))")))

(ert-deftest nelisp-eval-special/and-empty-yields-t ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(and)")
                   "t")))

(ert-deftest nelisp-eval-special/and-single-form-pass-through ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(and x)")
                   "x")))

(ert-deftest nelisp-eval-special/and-pair ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(and x y)")
                   "(if x (and y) nil)")))

(ert-deftest nelisp-eval-special/or-empty-yields-nil ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(or)")
                   "nil")))

(ert-deftest nelisp-eval-special/or-single-form-pass-through ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(or x)")
                   "x")))

(ert-deftest nelisp-eval-special/or-pair-uses-tmp ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(or x y)")
                   "(let ((--nl-or-tmp x)) (if --nl-or-tmp --nl-or-tmp (or y)))")))

(ert-deftest nelisp-eval-special/prog1-shape ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(prog1 a b c)")
                   "(let ((--nl-prog1-tmp a)) b c --nl-prog1-tmp)")))

(ert-deftest nelisp-eval-special/prog2-shape ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(prog2 a b c)")
                   "(progn a (prog1 b c))")))

(ert-deftest nelisp-eval-special/save-excursion-passthrough ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(save-excursion a b)")
                   "(progn a b)")))

(ert-deftest nelisp-eval-special/save-restriction-passthrough ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(save-restriction a)")
                   "(progn a)")))

(ert-deftest nelisp-eval-special/setq-default-aliases-setq ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(setq-default x 1 y 2)")
                   "(setq x 1 y 2)")))

(ert-deftest nelisp-eval-special/defvar-shape ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  ;; (defvar foo 5) → (progn (if (boundp 'foo) nil (set 'foo 5)) 'foo)
  ;; The printer abbreviates `(quote X)' as `'X' in the output string.
  (should (string= (nelisp-stdlib-eval-special-test--expand "(defvar foo 5)")
                   "(progn (if (boundp 'foo) nil (set 'foo 5)) 'foo)")))

(ert-deftest nelisp-eval-special/defvar-local-aliases-defvar ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(defvar-local foo 5)")
                   "(defvar foo 5 nil)")))

(ert-deftest nelisp-eval-special/defconst-shape ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(defconst foo 5)")
                   "(progn (set 'foo 5) 'foo)")))

(ert-deftest nelisp-eval-special/defcustom-aliases-defvar ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(defcustom foo 5 \"doc\")")
                   "(defvar foo 5 \"doc\")")))

(ert-deftest nelisp-eval-special/defgroup-noop ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(defgroup grp nil \"doc\")")
                   "nil")))

(ert-deftest nelisp-eval-special/dolist-shape ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  ;; (dolist (x lst) body) → let* + while
  (let ((expanded (nelisp-stdlib-eval-special-test--expand
                   "(dolist (x lst) body)")))
    (should (string-match-p "let\\*" expanded))
    (should (string-match-p "--nl-dolist-list" expanded))
    (should (string-match-p "while" expanded))))

(ert-deftest nelisp-eval-special/dotimes-shape ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (let ((expanded (nelisp-stdlib-eval-special-test--expand
                   "(dotimes (i 3) body)")))
    (should (string-match-p "let\\*" expanded))
    (should (string-match-p "--nl-dotimes-counter" expanded))
    (should (string-match-p "--nl-dotimes-limit" expanded))
    (should (string-match-p "nelisp--add2" expanded))))

(ert-deftest nelisp-eval-special/push-shape ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(push x lst)")
                   "(setq lst (cons x lst))")))

(ert-deftest nelisp-eval-special/pop-shape ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand "(pop lst)")
                   "(prog1 (car lst) (setq lst (cdr lst)))")))

;;; ---- function / macro definition (Stage 7.3.b) -----------------------

(ert-deftest nelisp-eval-special/defun-shape ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  ;; (defun foo (x) (+ x 1)) →
  ;;   (progn (fset 'foo (lambda (x) (+ x 1))) 'foo)
  (should (string= (nelisp-stdlib-eval-special-test--expand
                    "(defun foo (x) (+ x 1))")
                   "(progn (fset 'foo (lambda (x) (+ x 1))) 'foo)")))

(ert-deftest nelisp-eval-special/defun-no-body ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand
                    "(defun foo (x))")
                   "(progn (fset 'foo (lambda (x))) 'foo)")))

(ert-deftest nelisp-eval-special/defun-multiple-body-forms ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand
                    "(defun foo (x y) a b c)")
                   "(progn (fset 'foo (lambda (x y) a b c)) 'foo)")))

(ert-deftest nelisp-eval-special/defmacro-shape ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  ;; (defmacro foo (x) (+ x 1)) →
  ;;   (progn (fset 'foo (cons 'macro (cons (lambda (x) (+ x 1)) nil))) 'foo)
  (should (string= (nelisp-stdlib-eval-special-test--expand
                    "(defmacro foo (x) (+ x 1))")
                   "(progn (fset 'foo (cons 'macro (cons (lambda (x) (+ x 1)) nil))) 'foo)")))

(ert-deftest nelisp-eval-special/defmacro-no-body ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-special-test--expand
                    "(defmacro foo (x))")
                   "(progn (fset 'foo (cons 'macro (cons (lambda (x)) nil))) 'foo)")))

;;; ---- atom passthrough ------------------------------------------------

(ert-deftest nelisp-eval-special/macroexpand-1-non-macro-passthrough ()
  (nelisp-stdlib-eval-special-test--skip-unless-built)
  ;; Atoms / non-macro heads are returned as-is.
  (should (string= (nelisp-stdlib-eval-special-test--expand "42") "42"))
  (should (string= (nelisp-stdlib-eval-special-test--expand "(+ 1 2)") "(+ 1 2)")))

(provide 'nelisp-stdlib-eval-special-test)

;;; nelisp-stdlib-eval-special-test.el ends here
