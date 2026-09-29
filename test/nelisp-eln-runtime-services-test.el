;;; nelisp-eln-runtime-services-test.el --- tests for GNU .eln runtime services  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Runs under host GNU Emacs 31.1 ERT.  Each `supported' service is checked
;; against the same semantics as the genuine GNU C function it stands in
;; for (see nelisp-eln-runtime-services.el's docstrings for the exact
;; source citations).  A couple of tests are standalone-only and use an
;; inline `skip-unless' so they no-op here and activate automatically once
;; NeLisp exposes the hook they probe for; per project convention the
;; `skip-unless' call is written directly in the `ert-deftest' body, not
;; behind a helper function.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-runtime-services)

(defconst nelisp-eln-runtime-services-test--tmp-dir
  (expand-file-name "eln-services/" (expand-file-name "tmp" "~/.cache"))
  "Scratch directory for this test file's temporary artifacts.
Never /tmp, per project convention.")

(defun nelisp-eln-runtime-services-test--temp-file (prefix)
  "Return a fresh temp file path under the project scratch directory."
  (make-directory nelisp-eln-runtime-services-test--tmp-dir t)
  (make-temp-file (expand-file-name prefix
                                    nelisp-eln-runtime-services-test--tmp-dir)))

(defmacro nelisp-eln-runtime-services-test--with-clean-stack (&rest body)
  "Run BODY with a fresh, isolated unwind stack for this module."
  `(let ((nelisp-eln-runtime-services--specpdl nil))
     ,@body))

;;; maybe_quit

;; NOTE on structure: `quit' does not inherit from `error' (its condition
;; list is just (quit)), and ERT's `should-error' hardcodes an `error'
;; handler internally (see ert.el `should-error'), so it structurally
;; cannot catch a `quit' signal -- these tests use plain `condition-case'
;; instead.  That `condition-case' must be established BEFORE `quit-flag'
;; is bound to a truthy value: GNU's own evaluator polls `quit-flag' at
;; each form it evaluates (see `maybe_quit' in the module's Commentary),
;; so a `let' that binds `quit-flag' non-nil and then evaluates further
;; forms risks the *interpreter itself* raising a real quit before an
;; inner `condition-case' inside that same `let' has been reached.  Each
;; test below therefore keeps the truthy `quit-flag' binding as the
;; innermost form, directly inside an outer `condition-case'.

(ert-deftest nelisp-eln-runtime-services-test-maybe-quit-signals-when-pending ()
  (let ((caught nil))
    (condition-case _
        (let ((quit-flag t) (inhibit-quit nil))
          (nelisp-eln-runtime-services-maybe-quit))
      (quit (setq caught t)))
    (should caught)))

(ert-deftest nelisp-eln-runtime-services-test-maybe-quit-noop-when-clear ()
  (let ((quit-flag nil) (inhibit-quit nil))
    (should (null (nelisp-eln-runtime-services-maybe-quit)))
    (should (null quit-flag))))

;; No ERT test sets the real `quit-flag' and `inhibit-quit' both true at
;; once: empirically (verified in isolation, outside this module, with a
;; plain `-Q --batch' script and no `condition-case' involved at all --
;; e.g. `(let ((quit-flag nil) (inhibit-quit t)) (setq quit-flag t)
;; quit-flag)') this host's Emacs forces an immediate, uncatchable
;; process termination at the very next form evaluated once both are
;; simultaneously true, independent of any code in this module and
;; regardless of whether a `condition-case' for `quit' was already
;; established beforehand.  That is a property of this host binary's
;; own safepoint handling, not of `nelisp-eln-runtime-services-maybe-quit'
;; (whose own logic, per its docstring, checks `inhibit-quit' before ever
;; calling `signal' and is therefore never even reached in that state).
;; This sub-behaviour is verified by the source citation in the
;; docstring (GNU src/eval.c `probably_quit') rather than by an ERT test,
;; the same way the `kill-emacs' branch above is.

;; No ERT test for the `kill-emacs' branch: on host GNU Emacs, `Ffuncall'
;; itself calls the real, C-level `maybe_quit' as its first action (see
;; src/eval.c `Ffuncall'), so merely *calling* this module's
;; -maybe-quit function while the real `quit-flag' variable holds the
;; symbol `kill-emacs' lets the host interpreter's own safepoint act on
;; it -- invoking the genuine `kill-emacs' -- before this function's own
;; Lisp body ever runs, and before any `cl-letf' mock of `kill-emacs' has
;; been installed.  There is no way to observe this branch from outside
;; without actually terminating the test process, so it is exercised by
;; source-level review against GNU src/eval.c `process_quit_flag'
;; instead of by an ERT test.  The branch would only become reachable by
;; direct invocation in a runtime whose own function-call fast path does
;; not itself poll `quit-flag' the way GNU's `Ffuncall' does.

;;; maybe_gc

(ert-deftest nelisp-eln-runtime-services-test-maybe-gc-documented-noop-on-host ()
  (should (null (nelisp-eln-runtime-services-maybe-gc))))

(ert-deftest nelisp-eln-runtime-services-test-maybe-gc-invokes-standalone-safepoint-when-present ()
  (skip-unless (fboundp 'nelisp-runtime-gc-safepoint))
  ;; Standalone-only: activates once NeLisp defines a real safepoint hook.
  (let ((calls 0))
    (cl-letf (((symbol-function 'nelisp-runtime-gc-safepoint)
               (lambda () (setq calls (1+ calls)))))
      (nelisp-eln-runtime-services-maybe-gc))
    (should (= calls 1))))

;;; specbind / helper_unbind_n

(ert-deftest nelisp-eln-runtime-services-test-specbind-unbind-restores-value ()
  (nelisp-eln-runtime-services-test--with-clean-stack
   (defvar nelisp-eln-runtime-services-test--var1 10)
   (setq nelisp-eln-runtime-services-test--var1 10)
   (nelisp-eln-runtime-services-specbind 'nelisp-eln-runtime-services-test--var1 20)
   (should (= nelisp-eln-runtime-services-test--var1 20))
   (should (= (nelisp-eln-runtime-services-specpdl-depth) 1))
   (nelisp-eln-runtime-services-helper-unbind-n 1)
   (should (= nelisp-eln-runtime-services-test--var1 10))
   (should (= (nelisp-eln-runtime-services-specpdl-depth) 0))))

(ert-deftest nelisp-eln-runtime-services-test-specbind-unbind-restores-unbound ()
  (nelisp-eln-runtime-services-test--with-clean-stack
   (let ((sym (make-symbol "nelisp-eln-runtime-services-test-unbound")))
     (should (not (boundp sym)))
     (nelisp-eln-runtime-services-specbind sym 1)
     (should (= (symbol-value sym) 1))
     (nelisp-eln-runtime-services-helper-unbind-n 1)
     (should (not (boundp sym))))))

(ert-deftest nelisp-eln-runtime-services-test-specbind-nested-lifo-order ()
  (nelisp-eln-runtime-services-test--with-clean-stack
   (defvar nelisp-eln-runtime-services-test--var2 'a)
   (setq nelisp-eln-runtime-services-test--var2 'a)
   (nelisp-eln-runtime-services-specbind 'nelisp-eln-runtime-services-test--var2 'b)
   (nelisp-eln-runtime-services-specbind 'nelisp-eln-runtime-services-test--var2 'c)
   (should (eq nelisp-eln-runtime-services-test--var2 'c))
   (should (= (nelisp-eln-runtime-services-specpdl-depth) 2))
   (nelisp-eln-runtime-services-helper-unbind-n 1)
   (should (eq nelisp-eln-runtime-services-test--var2 'b))
   (nelisp-eln-runtime-services-helper-unbind-n 1)
   (should (eq nelisp-eln-runtime-services-test--var2 'a))))

(ert-deftest nelisp-eln-runtime-services-test-helper-unbind-n-zero-is-noop ()
  (nelisp-eln-runtime-services-test--with-clean-stack
   (should (null (nelisp-eln-runtime-services-helper-unbind-n 0)))
   (should (= (nelisp-eln-runtime-services-specpdl-depth) 0))))

(ert-deftest nelisp-eln-runtime-services-test-helper-unbind-n-underflow-signals ()
  (nelisp-eln-runtime-services-test--with-clean-stack
   (should-error (nelisp-eln-runtime-services-helper-unbind-n 1)
                 :type 'nelisp-eln-runtime-services-error)))

;;; helper_unwind_protect / interleaved cleanup order

(ert-deftest nelisp-eln-runtime-services-test-unwind-protect-cleanup-runs-once ()
  (nelisp-eln-runtime-services-test--with-clean-stack
   (let ((ran 0))
     (nelisp-eln-runtime-services-helper-unwind-protect (lambda () (setq ran (1+ ran))))
     (should (= (nelisp-eln-runtime-services-specpdl-depth) 1))
     (nelisp-eln-runtime-services-helper-unbind-n 1)
     (should (= ran 1))
     (should (= (nelisp-eln-runtime-services-specpdl-depth) 0)))))

(ert-deftest nelisp-eln-runtime-services-test-unwind-protect-non-function-handler-is-noop ()
  (nelisp-eln-runtime-services-test--with-clean-stack
   (nelisp-eln-runtime-services-helper-unwind-protect 'not-a-function)
   (should (null (nelisp-eln-runtime-services-helper-unbind-n 1)))))

(ert-deftest nelisp-eln-runtime-services-test-interleaved-specbind-and-unwind-protect-order ()
  (nelisp-eln-runtime-services-test--with-clean-stack
   (defvar nelisp-eln-runtime-services-test--var3 'x)
   (setq nelisp-eln-runtime-services-test--var3 'x)
   (let ((log nil))
     (nelisp-eln-runtime-services-specbind 'nelisp-eln-runtime-services-test--var3 'y)
     (nelisp-eln-runtime-services-helper-unwind-protect
      (lambda () (push 'cleanup-1 log)))
     (nelisp-eln-runtime-services-specbind 'nelisp-eln-runtime-services-test--var3 'z)
     (nelisp-eln-runtime-services-helper-unwind-protect
      (lambda () (push 'cleanup-2 log)))
     (should (= (nelisp-eln-runtime-services-specpdl-depth) 4))
     (nelisp-eln-runtime-services-helper-unbind-n 4)
     ;; LIFO: cleanup-2 runs first (most recently pushed), then var3->y,
     ;; then cleanup-1, then var3->x.  `push' prepends, so cleanup-2's
     ;; earlier run ends up as the tail of `log'.
     (should (equal log '(cleanup-1 cleanup-2)))
     (should (eq nelisp-eln-runtime-services-test--var3 'x)))))

(ert-deftest nelisp-eln-runtime-services-test-unbind-error-stops-mid-unwind ()
  (nelisp-eln-runtime-services-test--with-clean-stack
   (defvar nelisp-eln-runtime-services-test--var4 'x)
   (setq nelisp-eln-runtime-services-test--var4 'x)
   (nelisp-eln-runtime-services-specbind 'nelisp-eln-runtime-services-test--var4 'y)
   (nelisp-eln-runtime-services-helper-unwind-protect
    (lambda () (error "boom")))
   (should (= (nelisp-eln-runtime-services-specpdl-depth) 2))
   (should-error (nelisp-eln-runtime-services-helper-unbind-n 2) :type 'error)
   ;; The failing entry is popped before it runs (GNU unbind_to semantics),
   ;; so only the remaining :let entry is left on the stack.
   (should (= (nelisp-eln-runtime-services-specpdl-depth) 1))
   (nelisp-eln-runtime-services-helper-unbind-n 1)
   (should (eq nelisp-eln-runtime-services-test--var4 'x))))

;;; record_unwind_protect_excursion / record_unwind_current_buffer

(ert-deftest nelisp-eln-runtime-services-test-record-unwind-protect-excursion-restores ()
  (nelisp-eln-runtime-services-test--with-clean-stack
   (let ((buf-a (generate-new-buffer "eln-services-test-a"))
         (buf-b (generate-new-buffer "eln-services-test-b"))
         (orig (current-buffer)))
     (unwind-protect
         (progn
           ;; `with-current-buffer' restores the buffer that was current
           ;; when its body was entered, as soon as its body finishes; a
           ;; `set-buffer' meant to persist past this point must not be
           ;; nested inside one, so `set-buffer' is used directly here.
           (set-buffer buf-a)
           (insert "0123456789")
           (goto-char 3)
           (nelisp-eln-runtime-services-record-unwind-protect-excursion)
           (goto-char 7)
           (set-buffer buf-b)
           (should (eq (current-buffer) buf-b))
           (nelisp-eln-runtime-services-helper-unbind-n 1)
           (should (eq (current-buffer) buf-a))
           (should (= (point) 3)))
       (set-buffer orig)
       (kill-buffer buf-a)
       (kill-buffer buf-b)))))

(ert-deftest nelisp-eln-runtime-services-test-record-unwind-current-buffer-restores ()
  (nelisp-eln-runtime-services-test--with-clean-stack
   (let ((buf-a (generate-new-buffer "eln-services-test-cb-a"))
         (buf-b (generate-new-buffer "eln-services-test-cb-b")))
     (unwind-protect
         (with-current-buffer buf-a
           (nelisp-eln-runtime-services-record-unwind-current-buffer)
           (set-buffer buf-b)
           (should (eq (current-buffer) buf-b))
           (nelisp-eln-runtime-services-helper-unbind-n 1)
           (should (eq (current-buffer) buf-a)))
       (kill-buffer buf-a)
       (kill-buffer buf-b)))))

(ert-deftest nelisp-eln-runtime-services-test-record-unwind-current-buffer-skips-dead-buffer ()
  (nelisp-eln-runtime-services-test--with-clean-stack
   (let ((buf-a (generate-new-buffer "eln-services-test-cb-dead"))
         (buf-b (generate-new-buffer "eln-services-test-cb-alive")))
     (unwind-protect
         (progn
           (with-current-buffer buf-a
             (nelisp-eln-runtime-services-record-unwind-current-buffer))
           (kill-buffer buf-a)
           (set-buffer buf-b)
           (nelisp-eln-runtime-services-helper-unbind-n 1)
           ;; buf-a is dead, so the cleanup is a no-op: still on buf-b.
           (should (eq (current-buffer) buf-b)))
       (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

;;; push_handler -- unsupported

(ert-deftest nelisp-eln-runtime-services-test-push-handler-is-fail-closed ()
  (should-error (nelisp-eln-runtime-services-push-handler 'condition-case 0)
                :type 'nelisp-eln-runtime-services-unsupported))

;;; helper_PSEUDOVECTOR_TYPEP_XUNTAG

(ert-deftest nelisp-eln-runtime-services-test-pvec-typep-matches-host-predicates ()
  (should (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag
           (make-vector 3 nil) 0))
  (should (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag
           (make-marker) 3))
  (should (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag
           (symbol-function 'car) 18))
  (should (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag
           (current-buffer) 13))
  (should (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag
           (make-hash-table) 14))
  (should (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag
           (record 'nelisp-eln-runtime-services-test-tag 1) 34))
  (let ((ov (make-overlay (point-min) (point-min))))
    (unwind-protect
        (should (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag ov 4))
      (delete-overlay ov)))
  (should (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag
           (selected-window) 11))
  ;; Cross-check: a marker is not classified as a buffer, and vice versa.
  (should (null (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag
                 (make-marker) 13)))
  (should (null (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag
                 (current-buffer) 3))))

(ert-deftest nelisp-eln-runtime-services-test-pvec-typep-bignum ()
  (should (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag
           (* most-positive-fixnum most-positive-fixnum 4) 2))
  (should (null (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag 5 2))))

(ert-deftest nelisp-eln-runtime-services-test-pvec-typep-closure ()
  (skip-unless (fboundp 'closurep))
  (should (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag
           (lambda () 1) 31)))

(ert-deftest nelisp-eln-runtime-services-test-pvec-typep-unknown-code-errors ()
  (should-error (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag nil 19)
                :type 'nelisp-eln-runtime-services-error)
  (should-error (nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag nil 987654)
                :type 'nelisp-eln-runtime-services-error))

;;; wrong_type_argument

(ert-deftest nelisp-eln-runtime-services-test-wrong-type-argument-signals-exact-data ()
  (let ((err (should-error (nelisp-eln-runtime-services-wrong-type-argument
                             'integerp "not-an-int")
                            :type 'wrong-type-argument)))
    (should (equal (cdr err) '(integerp "not-an-int")))))

;;; set_internal

(ert-deftest nelisp-eln-runtime-services-test-set-internal-plain-set ()
  (defvar nelisp-eln-runtime-services-test--var5 nil)
  (nelisp-eln-runtime-services-set-internal
   'nelisp-eln-runtime-services-test--var5 42 nil 0)
  (should (= nelisp-eln-runtime-services-test--var5 42)))

(ert-deftest nelisp-eln-runtime-services-test-set-internal-rejects-setting-constant ()
  (should-error (nelisp-eln-runtime-services-set-internal :some-keyword 1 nil 0)
                :type 'setting-constant)
  ;; Setting a keyword to its own value is explicitly allowed.
  (should (null (nelisp-eln-runtime-services-set-internal
                 :some-keyword :some-keyword nil 0))))

(ert-deftest nelisp-eln-runtime-services-test-set-internal-bind-pushes-and-restores ()
  (nelisp-eln-runtime-services-test--with-clean-stack
   (defvar nelisp-eln-runtime-services-test--var6 'orig)
   (setq nelisp-eln-runtime-services-test--var6 'orig)
   (nelisp-eln-runtime-services-set-internal
    'nelisp-eln-runtime-services-test--var6 'bound nil 1)
   (should (eq nelisp-eln-runtime-services-test--var6 'bound))
   (should (= (nelisp-eln-runtime-services-specpdl-depth) 1))
   (nelisp-eln-runtime-services-helper-unbind-n 1)
   (should (eq nelisp-eln-runtime-services-test--var6 'orig))))

;;; slow_eq

(ert-deftest nelisp-eln-runtime-services-test-slow-eq-matches-eq ()
  (should (nelisp-eln-runtime-services-slow-eq 'foo 'foo))
  (should (not (nelisp-eln-runtime-services-slow-eq 'foo 'bar)))
  (let ((cons1 (cons 1 2)))
    (should (nelisp-eln-runtime-services-slow-eq cons1 cons1))
    (should (not (nelisp-eln-runtime-services-slow-eq cons1 (cons 1 2))))))

(ert-deftest nelisp-eln-runtime-services-test-slow-eq-unwraps-symbol-with-pos ()
  (skip-unless (and (fboundp 'position-symbol) (fboundp 'bare-symbol)))
  (let ((symbols-with-pos-enabled t))
    (should (nelisp-eln-runtime-services-slow-eq
             (position-symbol 'foo 1) (position-symbol 'foo 99)))
    (should (nelisp-eln-runtime-services-slow-eq
             (position-symbol 'foo 1) 'foo))))

;;; Lisp primitive redirects

(ert-deftest nelisp-eln-runtime-services-test-fcons-matches-cons ()
  (should (equal (nelisp-eln-runtime-services-fcons 1 2) (cons 1 2))))

(ert-deftest nelisp-eln-runtime-services-test-fmemq-matches-memq ()
  (let ((list (list 'a 'b 'c)))
    (should (eq (nelisp-eln-runtime-services-fmemq 'b list) (cdr list)))
    (should-not (nelisp-eln-runtime-services-fmemq 'z list))
    (should-not (nelisp-eln-runtime-services-fmemq 'a nil))
    (should (equal (should-error (nelisp-eln-runtime-services-fmemq 'z '(a . 5))
                                 :type 'wrong-type-argument)
                   '(wrong-type-argument listp (a . 5))))))

(ert-deftest nelisp-eln-runtime-services-test-fnreverse-matches-nreverse ()
  (should (equal (nelisp-eln-runtime-services-fnreverse (list 1 2 3)) '(3 2 1)))
  (should-not (nelisp-eln-runtime-services-fnreverse nil))
  (let ((d (cl-find-if (lambda (x) (eql (plist-get x :index) 1209))
                       nelisp-eln-runtime-services-descriptors)))
    (should (equal (plist-get d :symbol) "Fnreverse"))
    (should (equal (plist-get d :arity) 1)))
  (let ((d (cl-find-if (lambda (x) (eql (plist-get x :index) 1217))
                       nelisp-eln-runtime-services-descriptors)))
    (should (equal (plist-get d :symbol) "Fmemq"))
    (should (equal (plist-get d :arity) 2))))

(ert-deftest nelisp-eln-runtime-services-test-flength-fnth-match-builtins ()
  (should (= (nelisp-eln-runtime-services-flength '(setq foo 1)) 3))
  (should (= (nelisp-eln-runtime-services-flength "abc") 3))
  (should (= (nelisp-eln-runtime-services-flength nil) 0))
  (should (equal (should-error (nelisp-eln-runtime-services-flength '(a . b))
                               :type 'wrong-type-argument)
                 '(wrong-type-argument listp b)))
  (should-error (nelisp-eln-runtime-services-flength 5)
                :type 'wrong-type-argument)
  (should (= (nelisp-eln-runtime-services-fnth 2 '(setq foo 1)) 1))
  (should-not (nelisp-eln-runtime-services-fnth 5 '(a b)))
  (should-error (nelisp-eln-runtime-services-fnth 'x '(a b))
                :type 'wrong-type-argument)
  (dolist (row '((1250 "Flength" 1) (1220 "Fnth" 2)))
    (let ((d (cl-find-if (lambda (x) (eql (plist-get x :index) (car row)))
                         nelisp-eln-runtime-services-descriptors)))
      (should (equal (plist-get d :symbol) (nth 1 row)))
      (should (eq (plist-get d :convention) 'fixed))
      (should (equal (plist-get d :arity) (nth 2 row)))
      (should (eq (plist-get d :status) 'supported)))))

(ert-deftest nelisp-eln-runtime-services-test-fstringp-fcar-safe-match-builtins ()
  (should (eq (nelisp-eln-runtime-services-fstringp "a") t))
  (should-not (nelisp-eln-runtime-services-fstringp 'a))
  (should-not (nelisp-eln-runtime-services-fstringp nil))
  (should (eq (nelisp-eln-runtime-services-fcar-safe '(a . b)) 'a))
  (should-not (nelisp-eln-runtime-services-fcar-safe 5))
  (should-not (nelisp-eln-runtime-services-fcar-safe "s"))
  (should-not (nelisp-eln-runtime-services-fcar-safe nil))
  (dolist (row '((1376 "Fstringp") (1354 "Fcar_safe")))
    (let ((d (cl-find-if (lambda (x) (eql (plist-get x :index) (car row)))
                         nelisp-eln-runtime-services-descriptors)))
      (should (equal (plist-get d :symbol) (nth 1 row)))
      (should (eq (plist-get d :convention) 'fixed))
      (should (equal (plist-get d :arity) 1))
      (should (eq (plist-get d :status) 'supported)))))

(ert-deftest nelisp-eln-runtime-services-test-fassq-matches-assq ()
  (let ((alist '((a . 1) not-a-cons (b . 2))))
    (should (equal (nelisp-eln-runtime-services-fassq 'b alist) (assq 'b alist)))
    (should (equal (nelisp-eln-runtime-services-fassq 'missing alist)
                    (assq 'missing alist)))))

(ert-deftest nelisp-eln-runtime-services-test-fsymbol-value-matches-symbol-value ()
  (defvar nelisp-eln-runtime-services-test--var7 99)
  (should (= (nelisp-eln-runtime-services-fsymbol-value
              'nelisp-eln-runtime-services-test--var7)
             99))
  (let ((sym (make-symbol "unbound-sym")))
    (should-error (nelisp-eln-runtime-services-fsymbol-value sym)
                  :type 'void-variable)))

(ert-deftest nelisp-eln-runtime-services-test-fadd1-fsub1-match-and-coerce-markers ()
  (should (= (nelisp-eln-runtime-services-fadd1 5) (1+ 5)))
  (should (= (nelisp-eln-runtime-services-fsub1 5) (1- 5)))
  (with-temp-buffer
    (insert "0123456789")
    (let ((m (copy-marker 4)))
      (should (= (nelisp-eln-runtime-services-fadd1 m) 5))
      (should (= (nelisp-eln-runtime-services-fsub1 m) 3))
      (set-marker m nil))))

(ert-deftest nelisp-eln-runtime-services-test-ffuncall-matches-funcall-and-polls ()
  (should (= (nelisp-eln-runtime-services-ffuncall (list #'+ 1 2 3)) (funcall #'+ 1 2 3)))
  ;; See the maybe_quit tests above for why the `condition-case' must
  ;; wrap the `let' that binds `quit-flag', not the other way around.
  (let ((caught nil))
    (condition-case _
        (let ((quit-flag t) (inhibit-quit nil))
          (nelisp-eln-runtime-services-ffuncall (list #'identity 1)))
      (quit (setq caught t)))
    (should caught)))

(ert-deftest nelisp-eln-runtime-services-test-fapply-matches-apply ()
  (should (equal (nelisp-eln-runtime-services-fapply (list #'list 1 2 '(3 4)))
                 (apply #'list 1 2 '(3 4)))))

(ert-deftest nelisp-eln-runtime-services-test-feqlsign-fleq-match-arith ()
  (should (eq (nelisp-eln-runtime-services-feqlsign '(1 1 1)) (= 1 1 1)))
  (should (eq (nelisp-eln-runtime-services-feqlsign '(1 2)) (= 1 2)))
  (should (eq (nelisp-eln-runtime-services-fleq '(1 2 2 3)) (<= 1 2 2 3)))
  (should (eq (nelisp-eln-runtime-services-fleq '(3 2)) (<= 3 2))))

;;; Descriptor <-> freloc table validation, plus negative controls

(ert-deftest nelisp-eln-runtime-services-test-validate-descriptors-passes-on-real-tsv ()
  (should (file-exists-p nelisp-eln-runtime-services-freloc-tsv-file))
  (should (null (nelisp-eln-runtime-services-validate-descriptors))))

(ert-deftest nelisp-eln-runtime-services-test-validate-rejects-sha256-mismatch ()
  (let ((corrupt (nelisp-eln-runtime-services-test--temp-file "corrupt-tsv-")))
    (unwind-protect
        (progn
          (copy-file nelisp-eln-runtime-services-freloc-tsv-file corrupt t)
          (with-temp-buffer
            (insert-file-contents-literally corrupt)
            (goto-char (point-min))
            ;; Flip one character in the header so the byte-for-byte hash
            ;; changes without touching row shape.
            (delete-char 1)
            (insert "X")
            (write-region (point-min) (point-max) corrupt nil 'silent))
          (let ((problems (nelisp-eln-runtime-services-validate-descriptors corrupt)))
            (should (= (length problems) 1))
            (should (eq (plist-get (car problems) :reason) 'tsv-sha256-mismatch))))
      (delete-file corrupt))))

(ert-deftest nelisp-eln-runtime-services-test-validate-rejects-index-mismatch ()
  ;; Negative control: point a descriptor at an index the tsv does not
  ;; authenticate for that symbol, and confirm the validator rejects it
  ;; rather than silently accepting it.
  (let* ((original (car nelisp-eln-runtime-services-descriptors))
         (corrupted (plist-put (copy-sequence original) :index 999999))
         (nelisp-eln-runtime-services-descriptors
          (cons corrupted (cdr nelisp-eln-runtime-services-descriptors))))
    (let ((problems (nelisp-eln-runtime-services-validate-descriptors)))
      (should (= (length problems) 1))
      (should (eq (plist-get (car problems) :reason) 'index-not-in-tsv))
      (should (= (plist-get (car problems) :index) 999999)))))

(ert-deftest nelisp-eln-runtime-services-test-validate-rejects-symbol-mismatch ()
  (let* ((original (car nelisp-eln-runtime-services-descriptors))
         (corrupted (plist-put (copy-sequence original) :symbol "not_the_real_symbol"))
         (nelisp-eln-runtime-services-descriptors
          (cons corrupted (cdr nelisp-eln-runtime-services-descriptors))))
    (let ((problems (nelisp-eln-runtime-services-validate-descriptors)))
      (should (= (length problems) 1))
      (should (eq (plist-get (car problems) :reason) 'symbol-mismatch))
      (should (equal (plist-get (car problems) :found)
                      (plist-get original :symbol))))))

(ert-deftest nelisp-eln-runtime-services-test-validate-rejects-abi-mismatch ()
  (let* ((original (car nelisp-eln-runtime-services-descriptors))
         (corrupted (plist-put (copy-sequence original) :abi "wrongabi00"))
         (nelisp-eln-runtime-services-descriptors
          (cons corrupted (cdr nelisp-eln-runtime-services-descriptors))))
    (let ((problems (nelisp-eln-runtime-services-validate-descriptors)))
      (should (= (length problems) 1))
      (should (eq (plist-get (car problems) :reason) 'abi-mismatch)))))

(ert-deftest nelisp-eln-runtime-services-test-descriptor-table-internally-consistent ()
  ;; Every descriptor declares the pinned ABI and a unique (index . symbol).
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (descriptor nelisp-eln-runtime-services-descriptors)
      (should (equal (plist-get descriptor :abi) nelisp-eln-runtime-services-abi))
      (should (memq (plist-get descriptor :status) '(supported unsupported)))
      (let ((key (cons (plist-get descriptor :index) (plist-get descriptor :symbol))))
        (should (not (gethash key seen)))
        (puthash key t seen)))))

(provide 'nelisp-eln-runtime-services-test)

;;; nelisp-eln-runtime-services-test.el ends here
