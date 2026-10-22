;;; standalone-gnu-call-entry-check.el --- GNU entry/alias invariants -*- lexical-binding: t; -*-

(defvar gce-checks 0)
(defvar gce-failures 0)
(defun gce-check (name got expected)
  (setq gce-checks (1+ gce-checks))
  (unless (equal got expected)
    (setq gce-failures (1+ gce-failures))
    (princ (format "GNU-ENTRY FAIL %S got=%S expected=%S\n" name got expected))))
(defun gce-error (function)
  (condition-case err (funcall function) (error err)))
(defvar quit-flag nil)
(defvar inhibit-quit nil)
(defvar throw-on-input nil)
(defvar debug-on-next-call nil)
(defvar debugger nil)
(defvar inhibit-debugger nil)
(defvar debugger-may-continue nil)
(defvar inhibit-redisplay nil)
(defvar inhibit-changing-match-data nil)
(defvar gce-debug-events nil)
(defvar gce-autoload-loaded nil)


(fset 'gce-leaf (lambda () 17))
(fset 'gce-middle 'gce-leaf)
(fset 'gce-alias 'gce-middle)
(gce-check 'alias-calls (list (gce-alias) (funcall 'gce-alias) (apply 'gce-alias nil))
           '(17 17 17))
(gce-check 'raw-cell (symbol-function 'gce-alias) 'gce-middle)
(fmakunbound 'gce-leaf)
(gce-check 'void-funcall (gce-error (lambda () (funcall 'gce-alias)))
           '(void-function gce-alias))
(gce-check 'void-apply (gce-error (lambda () (apply 'gce-alias nil)))
           '(void-function gce-alias))
(gce-check 'void-inline (gce-error (lambda () (gce-alias)))
           '(void-function gce-alias))
(fset 'gce-leaf 41)
(gce-check 'invalid-funcall (gce-error (lambda () (funcall 'gce-alias)))
           '(invalid-function gce-alias))
(gce-check 'invalid-apply (gce-error (lambda () (apply 'gce-alias nil)))
           '(invalid-function gce-alias))
(gce-check 'spread-before-nil-function (gce-error (lambda () (apply nil 1)))
           '(wrong-type-argument listp 1))
(let ((tail (list 1)))
  (setcdr tail tail)
  (gce-check 'circular-last-spread
             (condition-case err (apply nil 3 tail) (circular-list (car err)))
             'circular-list))
(fset 'gce-leaf (lambda () 17))
(gce-check 'fset-cycle (gce-error (lambda () (fset 'gce-leaf 'gce-alias)))
           '(cyclic-function-indirection gce-leaf))
(gce-check 'rejected-cycle-preserves-cell (gce-alias) 17)
(fset 'gce-leaf (make-byte-code 0 (unibyte-string 192 135) [37] 1))
(gce-check 'alias-bytecode (funcall 'gce-alias) 37)
(fset 'gce-leaf (symbol-function 'car))
(gce-check 'alias-builtin (apply 'gce-alias '((39))) 39)
(let ((first (make-symbol "gce-private")) (second (make-symbol "gce-private")))
  (fset first second)
  (gce-check 'private-cycle
             (condition-case err (fset second first)
               (cyclic-function-indirection (list (car err) (eq (cadr err) second))))
             '(cyclic-function-indirection t)))

(fset 'gce-leaf (lambda (value) (+ value 10)))
(gce-check 'inline-resolves-before-arguments
           (gce-alias (progn (fset 'gce-leaf (lambda (value) (+ value 20))) 1)) 11)
(gce-check 'funcall-resolves-after-arguments
           (funcall 'gce-alias (progn (fset 'gce-leaf (lambda (value) (+ value 30))) 1)) 31)

(make-directory "target/tmp" t)
(let ((file (make-temp-file (expand-file-name "target/tmp/nelisp-gnu-entry-") nil ".el")))
  (unwind-protect
      (progn
        (write-region ";;; -*- lexical-binding: t; -*-\n(fset 'gce-autoload-target (lambda () 29))\n" nil file nil 'silent)
        (autoload 'gce-autoload-target file)
        (fset 'gce-autoload-alias 'gce-autoload-target)
        (gce-check 'autoload-retry-original (funcall 'gce-autoload-alias) 29)
        (write-region ";;; -*- lexical-binding: t; -*-\n(setq gce-autoload-loaded t)\n(fset 'gce-apply-autoload (lambda (&rest args) args))\n" nil file nil 'silent)
        (autoload 'gce-apply-autoload file)
        (gce-check 'spread-before-autoload
                   (gce-error (lambda () (apply 'gce-apply-autoload 1)))
                   '(wrong-type-argument listp 1))
        (gce-check 'invalid-spread-did-not-load gce-autoload-loaded nil)
        (fmakunbound 'gce-autoload-target)
        (write-region ";;; -*- lexical-binding: t; -*-\n(signal 'arith-error '(43))\n" nil file nil 'silent)
        (autoload 'gce-autoload-target file)
        (gce-check 'autoload-preserves-signal
                   (condition-case err (funcall 'gce-autoload-alias)
                     (arith-error err)) '(arith-error 43)))
    (delete-file file)))

;; Debugger and quit integration (GNU eval.c:3191-3215 maybe_quit,
;; do_debug_on_call and backtrace_debug_on_exit) are probed separately in
;; target/cpo/wip-tests/standalone-gnu-call-entry-debug-probe.el; they are
;; recorded as remaining GNU parity work in tools/ai/callpath-perf-progress.org.

;; An internal writer can bypass GNU fset's invariant. Such a chain must
;; terminate at the resolver and retain the original caller in its error.
(when (fboundp 'nelisp--env-globals-op)
  (nelisp--env-globals-op 'set-function 'gce-internal-a 'gce-internal-b)
  (nelisp--env-globals-op 'set-function 'gce-internal-b 'gce-internal-a)
  (gce-check 'internal-cycle
             (gce-error (lambda () (funcall 'gce-internal-a)))
             '(cyclic-function-indirection gce-internal-a))
  (fmakunbound 'gce-internal-a)
  (fmakunbound 'gce-internal-b))

(princ (format "GNU-ENTRY checks=%d failures=%d %s\n"
               gce-checks gce-failures (if (= gce-failures 0) "PASS" "FAIL")))
(kill-emacs (if (= gce-failures 0) 0 1))
