;;; -*- lexical-binding: t -*-
;; Doc 243 S5: print what `nelisp--fx-compile' makes of every named function
;; this image compiled, sorted by name.  The special-generation cell in code
;; slot 1 is dropped (its count differs between builds that declare a
;; different number of variables).
(let ((names nil))
  (maphash (lambda (_b r) (when (and (vectorp r) (aref r 4) (symbolp (aref r 4))) (push (aref r 4) names)))
           nelisp--fx-table)
  (setq names (sort names (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
  (dolist (name names)
    (let ((fn (symbol-function name)))
      (when (and (eq (car-safe fn) 'closure) (nelisp--fx-env-ok-p (nth 1 fn))
                 (not (string-prefix-p "nelisp--fxc-" (symbol-name name))))
        (let* ((body (nthcdr 3 fn)) (shape (nelisp--fx-arglist-shape (nth 2 fn)))
               (nelisp--fx-declared nil) (inhibit-message t)
               (nelisp--fx-expand (and (nelisp--fx-macros-used body) t))
               (nelisp--fx-outer-env (nth 1 fn)) (nelisp--fx-cap-names nil)
               (nelisp--fx-missing nil) (nelisp--fx-called nil)
               (c (condition-case e (nelisp--fx-compile (cons 'progn body) (car shape))
                    (error (list 'err (car e))))))
          (when (and (consp c) (vectorp (car c)) (vectorp (aref (car c) 1)))
            (aset (car c) 1 (cdr (cdr (append (aref (car c) 1) nil)))))
          ;; NAME, its body as read (so a check compares only functions
          ;; whose source is the same in both builds), then the output.
          (let ((print-escape-newlines t))
            (princ (format "%s\t%S\t%S %S\n" name body c (reverse nelisp--fx-called)))))))))
