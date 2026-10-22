;;; standalone-save-current-buffer-cell-check.el --- the runtime's save-current-buffer and library-owned function cells -*- lexical-binding: t; -*-

;; The buffer-bridge lane's standalone-buffer-function-cell-check.el simulates
;; the library's interpreted save-current-buffer with a macro of its own, so
;; its interpreted side never reaches the runtime's save-current-buffer
;; special form (GNU editfns.c:843-853 Fsave_current_buffer, ported by
;; eval/gnu-semantics-20261022).  This check keeps that special form and
;; fsets only set-buffer and current-buffer to recording closures, the way a
;; library that owns them does.  The interpreter's restore on normal exit, on
;; throw and on signal must call the recording closures exactly as the byte
;; code path does for the same GNU-compiled functions (fixtures/
;; buffer-function-cell.eld, compiled by GNU 31.1 from fixtures/
;; buffer-function-cell.el).
;;
;; `with-current-buffer' is the library's: GNU subr.el expands it into
;; save-current-buffer and set-buffer, and the bare runtime's own macro does
;; not use either.
;;
;; Under GNU 31.1 nothing is recorded (its special forms bypass the Lisp
;; function cells), so both sides print the same results and that output is
;; the pin.  On NeLisp each interpreted case must also have recorded its
;; calls, so the comparison cannot pass with both sides empty.

(defvar fbb-local nil)
(defmacro with-current-buffer (buffer &rest body)
  `(save-current-buffer (set-buffer ,buffer) ,@body))
(let* ((root (file-name-directory load-file-name))
       (source (expand-file-name "fixtures/buffer-function-cell.el" root))
       (runtime (fboundp 'nelisp--eval-source-string))
       (compiled (or (getenv "FBB_COMPILED") (expand-file-name "fixtures/buffer-function-cell.eld" root)))
       (forms nil) (failures 0) (cases 0)
       (original-set (symbol-function 'set-buffer))
       (original-current (symbol-function 'current-buffer)))
  (with-temp-buffer
    (insert-file-contents source)
    (goto-char (point-min))
    (condition-case nil
        (while t (let ((form (read (current-buffer))))
                   (when (eq (car-safe form) 'defun) (push form forms))))
      (end-of-file nil)))
  (load compiled nil t t)
  (dolist (name '(fbb-with fbb-normal fbb-throw fbb-signal fbb-nested fbb-killed))
    (let* ((form (assq name (mapcar (lambda (form) (cons (cadr form) form)) forms)))
           (interpreted (eval (cons 'lambda (cddr (cdr form))) t))
           (answers nil))
      (dolist (function (list interpreted (symbol-function name)))
        (let ((calls nil) (fbb-local nil)
              (home (get-buffer-create "fbb-home"))
              (b (get-buffer-create "fbb-b"))
              (c (get-buffer-create "fbb-c")))
          (funcall original-set home)
          (when runtime
            (fset 'set-buffer
                  (lambda (buffer)
                    (push (list 'set (if (stringp buffer) buffer (buffer-name buffer))) calls)
                    (funcall original-set buffer)))
            (fset 'current-buffer
                  (lambda () (push 'current calls) (funcall original-current))))
          (unwind-protect
              (let ((answer (funcall function b c)))
                (push (list answer fbb-local
                            (buffer-name (funcall original-current))
                            (reverse calls)) answers))
            (fset 'set-buffer original-set)
            (fset 'current-buffer original-current))))
      ;; The dolist pushed the interpreter's answer first, so ANSWERS is
      ;; (BYTECODE INTERPRETER).
      (let ((bytecode (car answers)) (interpreter (cadr answers)))
        (princ (format "SCB-RESULT %S %S\n" name (butlast bytecode)))
        (setq cases (1+ cases))
        (unless (equal bytecode interpreter)
          (setq failures (1+ failures))
          (princ (format "SCB-CELL FAIL %S interpreter=%S bytecode=%S\n" name interpreter bytecode)))
        (when (and runtime (null (nth 3 interpreter)))
          (setq failures (1+ failures))
          (princ (format "SCB-CELL FAIL %S: the interpreter reached no function cell\n" name))))))
  (princ (format "SCB-CELL %d cases, %d failures\n" cases failures))
  (kill-emacs (if (= failures 0) 0 1)))
