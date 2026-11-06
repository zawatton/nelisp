;;; nelisp-bytecode-noop-unwind-test.el --- Empty unwind boundary -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(defvar nelisp-bytecode-noop-unwind-test--function nil)

(defun nelisp-bytecode-noop-unwind-test--host (form)
  (if (not (consp form)) form
    (cons (if (eq (car form) 'seq) 'progn (car form))
          (mapcar #'nelisp-bytecode-noop-unwind-test--host (cdr form)))))

(with-temp-buffer
  (insert-file-contents
   (or (getenv "NELISP_UNWIND_TEST_SOURCE")
       (expand-file-name "../scripts/nelisp-standalone-build.el"
                         (file-name-directory (or load-file-name buffer-file-name)))))
  (search-forward "(defun wf_bytecode_unwind_step")
  (goto-char (match-beginning 0))
  (setq nelisp-bytecode-noop-unwind-test--function
        (eval (cons 'lambda
                    (cddr (nelisp-bytecode-noop-unwind-test--host
                           (read (current-buffer))))) t)))

(defun nelisp-bytecode-noop-unwind-test--call (target stack-head remaining)
  "Run the step on AUX=100 with the unwinds cell holding STACK-HEAD.
Return (RC . WRITES) where WRITES lists (ADDRESS . VALUE) stores in order."
  (let (writes rc)
    (cl-letf (((symbol-function 'sar) (lambda (x n) (ash x (- n))))
              ((symbol-function 'shl) (lambda (x n) (ash x n)))
              ((symbol-function 'nl_root_mark)
               (lambda (&rest _) (error "Unwind step requested scratch roots")))
              ((symbol-function 'wf_bytecode_w_consp) (lambda (_) 0))
              ((symbol-function 'ptr-read-u64)
               (lambda (addr off)
                 ;; aux+96 holds fixnum 0 (no in-place records); aux+8 the unwinds cell.
                 (cond ((and (= addr 100) (= off 96)) 1)
                       ((and (= addr 108) (= off 0)) stack-head)
                       (t (error "Unexpected read %S %S" addr off)))))
              ((symbol-function 'ptr-write-u64)
               (lambda (addr off value) (push (cons (+ addr off) value) writes))))
      (setq rc (funcall nelisp-bytecode-noop-unwind-test--function 1 100 target remaining)))
    (cons rc (nreverse writes))))

;; wf_bytecode_unwind_step is now (env aux target remaining) and walks AUX's
;; event stack (0ebb84eb0); the earlier seven-argument scratch-root form is gone.
(ert-deftest nelisp-bytecode-noop-unwind-needs-no-scratch-roots ()
  (let ((r (nelisp-bytecode-noop-unwind-test--call 3 3 7)))
    (should (= (car r) 0))
    ;; Event cell cleared, remaining count (7 -> fixnum word 29) stored back.
    (should (equal (cdr r) '((172 . 3) (124 . 29))))))

(ert-deftest nelisp-bytecode-unmatched-unwind-retains-rooted-error-path ()
  (let ((r (nelisp-bytecode-noop-unwind-test--call 3 0 7)))
    ;; The stack ends before TARGET: it is left at TARGET and the step fails.
    (should (= (car r) 1))
    (should (equal (cdr r) '((108 . 3) (172 . 3) (124 . 29))))))
