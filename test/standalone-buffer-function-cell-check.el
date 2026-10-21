;;; standalone-buffer-function-cell-check.el --- Buffer owner bridge -*- lexical-binding: t; -*-
;; Compile the fixture with GNU 31.1 first. Keep the driver interpreted so
;; the comparison does not accidentally exercise bytecode on both sides.
(defvar fbb-local nil)
;; Simulate the library's interpreted forms as well as its public function
;; cells. The bare bootstrap macros do not implement the library swap engine.
(defmacro save-current-buffer (&rest body)
  (let ((saved (make-symbol "saved")))
    `(let ((,saved (current-buffer)))
       (unwind-protect (progn ,@body)
         (when (buffer-live-p ,saved) (set-buffer ,saved))))))
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
  (dolist (name '(fbb-with fbb-normal fbb-throw fbb-signal fbb-nested
                 fbb-killed))
    (let* ((form (assq name (mapcar (lambda (form) (cons (cadr form) form)) forms)))
           (interpreted (eval (cons 'lambda (cddr (cdr form))) t))
           (answers nil))
      (dolist (function (list interpreted (symbol-function name)))
        (let ((calls nil) (fbb-local nil)
              (home (get-buffer-create "fbb-home"))
              (b (get-buffer-create "fbb-b"))
              (c (get-buffer-create "fbb-c")))
          (funcall original-set home)
          (when (fboundp 'nelisp--buffer-runtime-owned-p)
            (unless (and (nelisp--buffer-runtime-owned-p 'set-buffer)
                         (nelisp--buffer-runtime-owned-p 'current-buffer))
              (error "Runtime buffer ownership was not restored")))
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
      (princ (format "BUFFER-RESULT %S %S\n" name (butlast (car answers))))
      (setq cases (1+ cases))
      (unless (equal (car answers) (cadr answers))
        (setq failures (1+ failures))
        (princ (format "BUFFER-CELL FAIL %S interpreted=%S bytecode=%S\n"
                       name (cadr answers) (car answers))))))
  (princ (format "BUFFER-CELL %d cases, %d failures\n" cases failures))
  (kill-emacs (if (= failures 0) 0 1)))
