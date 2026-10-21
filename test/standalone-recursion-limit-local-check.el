;;; standalone-recursion-limit-local-check.el --- Effective local recursion limit -*- lexical-binding: t; -*-
;; Run unchanged on GNU Emacs and the standalone reader. Use primitive local
;; creation explicitly so this check does not depend on the setq-local macro.
(defvar rll-checks 0)
(defvar rll-failures 0)
(defun rll-recurse (n)
  (if (= n 0) 0 (+ 1 (rll-recurse (- n 1)))))
(defun rll-observe ()
  (condition-case _err (rll-recurse 200)
    (excessive-lisp-nesting 'limited)))
(defun rll-snapshot ()
  (list (symbol-value 'max-lisp-eval-depth)
        (default-value 'max-lisp-eval-depth)
        (and (local-variable-p 'max-lisp-eval-depth) t)
        (rll-observe)))
(defun rll-check (label expected actual)
  (setq rll-checks (1+ rll-checks))
  (unless (equal expected actual)
    (setq rll-failures (1+ rll-failures))
    (princ (format "RECURSION-LIMIT-LOCAL %S actual=%S expected=%S\n"
                   label actual expected))))
(let* ((original (current-buffer))
       (saved-default (default-value 'max-lisp-eval-depth))
       (saved-local (local-variable-p 'max-lisp-eval-depth))
       (saved-value (symbol-value 'max-lisp-eval-depth))
       (first (generate-new-buffer " *rll-first*"))
       (second (generate-new-buffer " *rll-second*")))
  (unwind-protect
      (progn
        (kill-local-variable 'max-lisp-eval-depth)
        (set-default 'max-lisp-eval-depth 2000)
        (rll-check 'global '(2000 2000 nil 200) (rll-snapshot))
        (with-current-buffer first
          (make-local-variable 'max-lisp-eval-depth)
          (set 'max-lisp-eval-depth 100)
          (rll-check 'first-local '(100 2000 t limited) (rll-snapshot)))
        (with-current-buffer second
          (rll-check 'second-default '(2000 2000 nil 200) (rll-snapshot))
          (make-local-variable 'max-lisp-eval-depth)
          (set 'max-lisp-eval-depth 4000)
          (rll-check 'second-local '(4000 2000 t 200) (rll-snapshot)))
        (with-current-buffer first
          (rll-check 'switch-first '(100 2000 t limited) (rll-snapshot))
          (set-default 'max-lisp-eval-depth 3000)
          (rll-check 'default-change '(100 3000 t limited) (rll-snapshot))
          (kill-local-variable 'max-lisp-eval-depth)
          (rll-check 'kill-local '(3000 3000 nil 200) (rll-snapshot)))
        (with-current-buffer second
          (rll-check 'other-local-survives '(4000 3000 t 200) (rll-snapshot))
          (set 'max-lisp-eval-depth 1200)
          (rll-check 'local-set '(1200 3000 t 200) (rll-snapshot)))
        (set 'max-lisp-eval-depth 3500)
        (rll-check 'global-set '(3500 3500 nil 200) (rll-snapshot))
        (with-current-buffer first
          (rll-check 'global-visible '(3500 3500 nil 200) (rll-snapshot)))
        (let ((max-lisp-eval-depth 100))
          (rll-check 'default-dynamic '(100 100 nil limited) (rll-snapshot))
          (with-current-buffer second
            (rll-check 'local-over-default-dynamic '(1200 100 t 200) (rll-snapshot))
            (rll-check 'callback-local-over-default 200 (funcall (lambda () (rll-observe))))))
        (rll-check 'default-dynamic-restored '(3500 3500 nil 200) (rll-snapshot))
        (with-current-buffer second
          (let ((max-lisp-eval-depth 90))
            (rll-check 'selected-local-dynamic '(90 3500 t limited) (rll-snapshot)))
          (rll-check 'selected-local-restored '(1200 3500 t 200) (rll-snapshot))))
    (set-buffer original)
    (set-default 'max-lisp-eval-depth saved-default)
    (if saved-local
        (progn (make-local-variable 'max-lisp-eval-depth)
               (set 'max-lisp-eval-depth saved-value))
      (kill-local-variable 'max-lisp-eval-depth))
    (kill-buffer first)
    (kill-buffer second)))
(princ (format "RECURSION-LIMIT-LOCAL checks=%d failures=%d %s\n"
               rll-checks rll-failures (if (= rll-failures 0) "PASS" "FAIL")))
(princ "RECURSION-LIMIT-LOCAL-DONE 1\n")
(kill-emacs (if (= rll-failures 0) 0 1))
