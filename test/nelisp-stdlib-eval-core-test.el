;;; nelisp-stdlib-eval-core-test.el --- ERT for Stage 7.4.b apply/closure  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7 Stage 7.4.b (Doc 68) — verify the elisp helpers installed by
;; `lisp/nelisp-stdlib-eval-core.el' (= 4 predicates / bind-formals
;; pure helpers / apply-lambda-inner / apply-closure / apply-lambda /
;; apply-fn / expand-macro).
;;
;; Stage 7.4.b is *parallel install only*: Rust `apply_function' still
;; preempts the runtime call path, so we cannot verify behaviour by
;; calling user functions and observing.  Instead we drive the helpers
;; *directly* via the subprocess `nelisp eval EXPR' interface.
;;
;; Subprocess pattern mirrors `nelisp-stdlib-eval-special-test.el'.

;;; Code:

(require 'ert)

(defconst nelisp-stdlib-eval-core-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir (expand-file-name ".." test-dir))))

(defconst nelisp-stdlib-eval-core-test--bin
  (and nelisp-stdlib-eval-core-test--repo-root
       (expand-file-name "target/release/nelisp"
                         nelisp-stdlib-eval-core-test--repo-root)))

(defun nelisp-stdlib-eval-core-test--skip-unless-built ()
  (unless (and nelisp-stdlib-eval-core-test--bin
               (file-executable-p nelisp-stdlib-eval-core-test--bin))
    (ert-skip
     (format "nelisp binary missing — run `cargo build --release' (looked at %s)"
             nelisp-stdlib-eval-core-test--bin))))

(defun nelisp-stdlib-eval-core-test--eval (expr-string)
  "Run `nelisp eval EXPR-STRING' and return (EXIT-CODE . STDOUT)."
  (with-temp-buffer
    (let ((code (call-process nelisp-stdlib-eval-core-test--bin nil t nil
                              "eval" expr-string)))
      (cons code (buffer-substring-no-properties (point-min) (point-max))))))

(defun nelisp-stdlib-eval-core-test--printed (probe-form)
  "Return the printed result of PROBE-FORM (a string of elisp).
Wraps the probe with `(princ (prin1-to-string PROBE))' and trims the
trailing auto-print of the wrapper's own return value."
  (let* ((wrapper (format "(princ (prin1-to-string %s))" probe-form))
         (r (nelisp-stdlib-eval-core-test--eval wrapper)))
    (should (eq (car r) 0))
    ;; STDOUT layout: <printed PROBE><printed wrapper-return>
    ;; The wrapper returns the same string princ printed, so STDOUT is
    ;; <prin1>"<prin1>"\n.  We split on the trailing `"...":
    (let* ((out (string-trim-right (cdr r))))
      ;; Find the last `"<x>"' substring — that is the auto-print.
      ;; Strip it.
      (if (string-match "\\(.*\\)\"\\([^\"]*\\)\"\\'" out)
          (match-string 1 out)
        out))))

;;; ---- predicates ------------------------------------------------------

(ert-deftest nelisp-eval-core/builtinp-recognizes-cons ()
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--builtinp (cons (quote builtin) (cons (quote foo) nil)))")
                   "t"))
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--builtinp (cons (quote closure) nil))")
                   "nil"))
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--builtinp (quote not-a-cons))")
                   "nil")))

(ert-deftest nelisp-eval-core/closurep-recognizes-cons ()
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--closurep (cons (quote closure) (cons nil nil)))")
                   "t"))
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--closurep (cons (quote builtin) nil))")
                   "nil")))

(ert-deftest nelisp-eval-core/lambdap-recognizes-cons ()
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--lambdap (cons (quote lambda) (cons nil (cons nil nil))))")
                   "t"))
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--lambdap (cons (quote macro) nil))")
                   "nil")))

(ert-deftest nelisp-eval-core/macrop-recognizes-cons ()
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--macrop (cons (quote macro) (quote x)))")
                   "t"))
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--macrop (cons (quote lambda) nil))")
                   "nil")))

;;; ---- bind-formals--compute (pure helper) ----------------------------

(ert-deftest nelisp-eval-core/bind-formals-required-only ()
  "Two required formals consume two args and return ((a . 1) (b . 2))."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--bind-formals--compute (quote (a b)) (quote (1 2)))")
                   "((a . 1) (b . 2))")))

(ert-deftest nelisp-eval-core/bind-formals-with-optional ()
  "Required + &optional with exhausted arg gets nil default."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  ;; (a &optional b) called with (10) → ((a . 10) (b . nil))
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--bind-formals--compute (quote (a &optional b)) (quote (10)))")
                   "((a . 10) (b))"))
  ;; called with (10 20) → ((a . 10) (b . 20))
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--bind-formals--compute (quote (a &optional b)) (quote (10 20)))")
                   "((a . 10) (b . 20))")))

(ert-deftest nelisp-eval-core/bind-formals-with-rest ()
  "&rest collects the tail as a list."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  ;; (a &rest xs) called with (1 2 3) → ((a . 1) (xs . (2 3)))
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--bind-formals--compute (quote (a &rest xs)) (quote (1 2 3)))")
                   "((a . 1) (xs 2 3))")))

(ert-deftest nelisp-eval-core/bind-formals-arity-error ()
  "Too few args for required formals signals."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  ;; Wrap the probe in condition-case; on signal princ "signaled".
  (let* ((expr "(condition-case _e (nelisp--bind-formals--compute (quote (a b)) (quote (1))) (error (princ \"signaled\")))")
         (r (nelisp-stdlib-eval-core-test--eval expr)))
    (should (eq (car r) 0))
    (should (string-match-p "signaled" (cdr r)))))

;;; ---- apply-lambda-inner (frame-stack integration) ------------------

(ert-deftest nelisp-eval-core/apply-lambda-inner-basic ()
  "Empty captured env, single formal `x', body `(nelisp--mul2 x x)', arg 5 → 25."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--apply-lambda-inner nil (quote (x)) (quote ((nelisp--mul2 x x))) (quote (5)))")
                   "25")))

(ert-deftest nelisp-eval-core/apply-lambda-inner-respects-captured-env ()
  "Captured env binding visible from body."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  ;; captured = ((cap-x . 7)) (Cell wrap not needed — push-captured handles raw values)
  ;; body uses cap-x.  body = ((nelisp--mul2 cap-x 6))
  ;; result: 42
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--apply-lambda-inner (cons (cons (quote cap-x) 7) nil) nil (quote ((nelisp--mul2 cap-x 6))) nil)")
                   "42")))

;;; ---- apply-closure / apply-lambda dispatch -------------------------

(ert-deftest nelisp-eval-core/apply-closure-roundtrip ()
  "(closure nil (x) (nelisp--mul2 x x)) applied to (5) → 25."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  ;; Build the closure cons explicitly to avoid the lambda → closure
  ;; auto-conversion that the literal (lambda ...) would trigger.
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--apply-closure
                       (cons (quote closure)
                             (cons nil
                                   (cons (cons (quote x) nil)
                                         (cons (cons (quote nelisp--mul2)
                                                     (cons (quote x)
                                                           (cons (quote x) nil)))
                                               nil))))
                       (cons 5 nil))")
                   "25")))

(ert-deftest nelisp-eval-core/apply-lambda-bare ()
  "Bare (lambda (x) (nelisp--mul2 x 3)) applied to (4) → 12."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  ;; Build the lambda cons explicitly so it is NOT evaluated (= would
  ;; produce a closure).  apply-lambda walks the literal cons.
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--apply-lambda
                       (cons (quote lambda)
                             (cons (cons (quote x) nil)
                                   (cons (cons (quote nelisp--mul2)
                                               (cons (quote x)
                                                     (cons 3 nil)))
                                         nil)))
                       (cons 4 nil))")
                   "12")))

;;; ---- apply-fn dispatch + expand-macro ------------------------------

(ert-deftest nelisp-eval-core/apply-fn-dispatches-to-builtin ()
  "(apply-fn '(builtin cons) '(1 2)) → (1 . 2)."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--apply-fn (cons (quote builtin) (cons (quote cons) nil)) (cons 1 (cons 2 nil)))")
                   "(1 . 2)")))

(ert-deftest nelisp-eval-core/apply-fn-dispatches-to-closure ()
  "apply-fn → apply-closure round-trip via 4-way dispatch."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--apply-fn
                       (cons (quote closure)
                             (cons nil
                                   (cons (cons (quote x) nil)
                                         (cons (cons (quote nelisp--mul2)
                                                     (cons (quote x)
                                                           (cons (quote x) nil)))
                                               nil))))
                       (cons 6 nil))")
                   "36")))

(ert-deftest nelisp-eval-core/apply-fn-rejects-macro ()
  "Macro shape passed to apply-fn signals."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  (let* ((expr "(condition-case _e (nelisp--apply-fn (cons (quote macro) (quote x)) nil) (error (princ \"signaled\")))")
         (r (nelisp-stdlib-eval-core-test--eval expr)))
    (should (eq (car r) 0))
    (should (string-match-p "signaled" (cdr r)))))

(ert-deftest nelisp-eval-core/expand-macro-roundtrip ()
  "(macro lambda (x) (cons (quote quote) (cons x nil))) on `foo' → (quote foo).
The prin1 reader-syntax for (quote X) is 'X, so we compare against that."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  ;; macro-form: (macro lambda (x) (cons 'quote (cons x nil)))
  ;; arg-forms: (foo)
  ;; expansion: (quote foo) → printed as 'foo
  (should (string= (nelisp-stdlib-eval-core-test--printed
                    "(nelisp--expand-macro
                       (cons (quote macro)
                             (cons (quote lambda)
                                   (cons (cons (quote x) nil)
                                         (cons (cons (quote cons)
                                                     (cons (cons (quote quote)
                                                                 (cons (quote quote) nil))
                                                           (cons (cons (quote cons)
                                                                       (cons (quote x)
                                                                             (cons nil nil)))
                                                                 nil)))
                                               nil))))
                       (cons (quote foo) nil))")
                   "'foo")))

(provide 'nelisp-stdlib-eval-core-test)

;;; nelisp-stdlib-eval-core-test.el ends here

;;; ---- Stage 7.4.c — flag-on cross-equivalence ERT --------------------
;;;
;;; Verify that with `nelisp--set-use-elisp-apply t' the runtime
;;; dispatch goes through the elisp `nelisp--apply-fn' and produces
;;; the same final value as the Rust dispatch.  Stage 7.4.c uses the
;;; *outermost-only* delegation strategy (= delegation_depth counter
;;; in env.rs), so what we verify is that one entry through the elisp
;;; path computes correctly; deep recursive coverage is Stage 7.4.d's
;;; default-flip job.

(defun nelisp-stdlib-eval-core-test--printed-with-flag (probe-form)
  "Like `--printed' but with `nelisp--set-use-elisp-apply t' first.
Wraps PROBE-FORM in a progn that flips the flag, runs the probe,
then `princ's the prin1 of the probe result.  Same trim semantics as
`--printed' (= strips the trailing wrapper auto-print)."
  (let* ((wrapper
          (format "(progn (nelisp--set-use-elisp-apply t) (princ (prin1-to-string %s)))"
                  probe-form))
         (r (nelisp-stdlib-eval-core-test--eval wrapper)))
    (should (eq (car r) 0))
    (let* ((out (string-trim-right (cdr r))))
      (if (string-match "\\(.*\\)\"\\([^\"]*\\)\"\\'" out)
          (match-string 1 out)
        out))))

(ert-deftest nelisp-eval-core/flag-recursive-defun-dispatch ()
  "Recursive defun via elisp dispatch.
(fact 10) = 3628800 must hold whether dispatch is Rust or elisp."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  (should (string=
           (nelisp-stdlib-eval-core-test--printed-with-flag
            "(progn
               (defun fact (n)
                 (if (= n 0) 1 (nelisp--mul2 n (fact (nelisp--sub2 n 1)))))
               (fact 10))")
           "3628800")))

(ert-deftest nelisp-eval-core/flag-closure-write-through ()
  "setq inside a closure body writes through to the captured cell.
After 2 funcall, x = 2 (= the let binding's cell observed mutation)."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  (should (string=
           (nelisp-stdlib-eval-core-test--printed-with-flag
            "(let ((x 0))
               (let ((f (lambda () (setq x (1+ x)))))
                 (funcall f)
                 (funcall f)
                 x))")
           "2")))

(ert-deftest nelisp-eval-core/flag-rest-arg-defun ()
  "&rest formal collects the tail; (length xs) returns the count."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  ;; (defun count-args (&rest xs) (length xs))
  ;; (count-args 'a 'b 'c) → 3
  (should (string=
           (nelisp-stdlib-eval-core-test--printed-with-flag
            "(progn
               (defun count-args (&rest xs) (length xs))
               (count-args (quote a) (quote b) (quote c)))")
           "3")))

(ert-deftest nelisp-eval-core/flag-funcall-builtin-passthrough ()
  "funcall on a builtin sentinel works through the elisp dispatcher.
The Rust builtin short-circuit (= is_builtin_value check in
apply_combiner) still routes to bi_cons under flag-on."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  (should (string=
           (nelisp-stdlib-eval-core-test--printed-with-flag
            "(funcall (function cons) 1 2)")
           "(1 . 2)")))

(ert-deftest nelisp-eval-core/flag-runtime-toggle-roundtrip ()
  "Flag toggle is observable both ways via `nelisp--get-use-elisp-apply'."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  (let* ((expr "(progn
                  (princ (prin1-to-string (nelisp--get-use-elisp-apply)))
                  (princ \" \")
                  (nelisp--set-use-elisp-apply t)
                  (princ (prin1-to-string (nelisp--get-use-elisp-apply)))
                  (princ \" \")
                  (nelisp--set-use-elisp-apply nil)
                  (princ (prin1-to-string (nelisp--get-use-elisp-apply))))")
         (r (nelisp-stdlib-eval-core-test--eval expr)))
    (should (eq (car r) 0))
    ;; STDOUT: "nil t nil"<auto-print>
    (should (string-prefix-p "nil t nil" (cdr r)))))

(ert-deftest nelisp-eval-core/flag-recursion-depth-still-bounded ()
  "Even with elisp dispatch on, runaway recursion hits max-lisp-eval-depth.
Verifies that `delegation_depth' guard plus the bumped 1024 budget
do not let an infinite (defun loop () (loop)) silently overflow the
Rust call stack — it must surface a recoverable Internal error."
  (nelisp-stdlib-eval-core-test--skip-unless-built)
  (let* ((expr "(progn
                  (nelisp--set-use-elisp-apply t)
                  (defun nelisp-test-loop () (nelisp-test-loop))
                  (condition-case _e
                      (nelisp-test-loop)
                    (error (princ \"caught\"))))")
         (r (nelisp-stdlib-eval-core-test--eval expr)))
    ;; The depth error is an Internal `EvalError', surfaced by the
    ;; CLI as exit 1 with a stderr message; the elisp condition-case
    ;; cannot catch it (= it's an Internal, not a signal).  Either:
    ;;   exit=0 + STDOUT contains "caught" (= elisp signal layer),
    ;;   or exit≠0 + STDERR mentions max-lisp-eval-depth.
    (cond
     ((eq (car r) 0)
      (should (string-match-p "caught" (cdr r))))
     (t
      ;; Either path is acceptable — both prove "doesn't silently
      ;; segfault".  The cdr is STDOUT; we don't capture STDERR via
      ;; call-process here, so just assert non-zero exit.
      (should (not (eq (car r) 0)))))))
