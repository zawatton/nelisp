;;; standalone-shallow-binding-check.el --- dynamic binding semantics -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-shallow-binding-check.el
;;
;; GNU Emacs answers below are the reference (run the same file under
;; `emacs --batch -l').  These pin what a `let' of a special variable must
;; do however bindings are stored: what callees see, what is restored on
;; every exit path (normal, `signal', `throw', `unwind-protect'), and what
;; `setq', `set-default', `makunbound', `boundp' and variable watchers
;; observe inside the binding.  Prints one SHALLOW-BINDING line and exits
;; non-zero on failure.

(defvar sb-x 0)
(defvar sb-y 'global-y)
(defvar sb-unbound)
(defvar sb-log nil)
(defun sb-read-x () sb-x)
(defun sb-read (sym) (if (boundp sym) (symbol-value sym) 'void))
(defun sb-set-x (v) (setq sb-x v))
(defun sb-fact (n) (if (< n 1) (list sb-x) (let ((sb-x n)) (sb-fact (1- n)))))
(defun sb-param (sb-x) (sb-read-x))
(defun sb-throw-deep (n) (if (< n 1) (throw 'sb-tag sb-x) (let ((sb-x n)) (sb-throw-deep (1- n)))))

(let* ((fails 0)
       (check (lambda (name got want)
                (unless (equal got want)
                  (setq fails (1+ fails))
                  (princ (format "SHALLOW-BINDING FAIL %s got %S want %S\n" name got want))))))
  ;; Callees see the binding; it is undone afterwards.
  (funcall check 'callee (let ((sb-x 1)) (sb-read-x)) 1)
  (funcall check 'restored sb-x 0)
  (funcall check 'nested (let ((sb-x 1)) (list (let ((sb-x 2)) (sb-read-x)) (sb-read-x))) '(2 1))
  (funcall check 'let*-same-name (let* ((sb-x 1) (sb-x (+ sb-x 10))) (sb-read-x)) 11)
  (funcall check 'restored-2 sb-x 0)
  ;; A variable with no value before the binding has none after it.
  (funcall check 'unbound-inside (let ((sb-unbound 5)) (sb-read 'sb-unbound)) 5)
  (funcall check 'unbound-after (sb-read 'sb-unbound) 'void)
  ;; Writes inside go to the binding, not the global value.
  (funcall check 'setq-inside (let ((sb-x 1)) (sb-set-x 7) (sb-read-x)) 7)
  (funcall check 'setq-restored sb-x 0)
  (funcall check 'set-inside (let ((sb-x 1)) (set 'sb-x 8) sb-x) 8)
  (funcall check 'default-value-inside (let ((sb-x 1)) (default-value 'sb-x)) 1)
  (funcall check 'set-default-inside (let ((sb-x 1)) (set-default 'sb-x 9) (sb-read-x)) 9)
  (funcall check 'set-default-restored sb-x 0)
  (funcall check 'makunbound-inside (let ((sb-x 1)) (makunbound 'sb-x) (sb-read 'sb-x)) 'void)
  (funcall check 'makunbound-restored sb-x 0)
  ;; Every exit path restores.
  (funcall check 'signal-exit
           (condition-case nil (let ((sb-x 1)) (error "boom")) (error (sb-read-x))) 0)
  (funcall check 'let*-init-signals
           (condition-case nil (let* ((sb-x 1) (sb-y (error "init"))) sb-y)
             (error (list sb-x sb-y)))
           '(0 global-y))
  (funcall check 'throw-exit (catch 'sb-tag (let ((sb-x 1)) (throw 'sb-tag (sb-read-x)))) 1)
  (funcall check 'throw-restored sb-x 0)
  (funcall check 'throw-deep (catch 'sb-tag (sb-throw-deep 50)) 1)
  (funcall check 'throw-deep-restored sb-x 0)
  (funcall check 'unwind-protect
           (let ((seen nil))
             (catch 'sb-tag
               (let ((sb-x 1))
                 (unwind-protect (throw 'sb-tag nil)
                   (setq seen (sb-read-x)))))
             (list seen sb-x))
           '(1 0))
  ;; Under lexical-binding a parameter named after a special variable is
  ;; bound lexically (GNU 31.1); the callee sees the global value.
  (funcall check 'param (sb-param 4) 0)
  (funcall check 'param-restored sb-x 0)
  ;; Recursion: each level sees its own binding.
  (funcall check 'recursion (sb-fact 30) '(1))
  (funcall check 'recursion-restored sb-x 0)
  ;; No binding leaks across 10000 signalled exits.
  (dotimes (_ 10000) (condition-case nil (let ((sb-x 1)) (error "x")) (error nil)))
  (funcall check 'leak-stress sb-x 0)
  ;; Byte-code (the regexp matcher) binds specials too.
  (funcall check 'case-fold-let (let ((case-fold-search t)) (string-match "A" "xa")) 1)
  (funcall check 'case-fold-let-nil (let ((case-fold-search nil)) (string-match "A" "xa")) nil)
  ;; Watchers see let and unlet.
  (when (fboundp 'add-variable-watcher)
    (add-variable-watcher 'sb-y (lambda (sym new op _where) (push (list sym new op) sb-log)))
    (let ((sb-y 'bound)) nil)
    (funcall check 'watchers (reverse sb-log)
             '((sb-y bound let) (sb-y global-y unlet))))
  (princ (format "SHALLOW-BINDING failures=%d %s\n" fails (if (= fails 0) "PASS" "FAIL")))
  (kill-emacs (if (= fails 0) 0 1)))
