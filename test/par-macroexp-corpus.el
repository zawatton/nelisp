;;; par-macroexp-corpus.el --- GNU macroexpansion structure -*- lexical-binding: t; -*-

(require 'macroexp)
(require 'cl-lib)
(require 'gv)
(require 'pcase)

(defvar par-macroexp-corpus
  '((pcase
     (pcase input
       (`(,name . ,value) (list name value))
       ((pred stringp) (length input))
       (_ nil)))
    (cl-loop
     (cl-loop for item in input
              when (numberp item) collect (1+ item)))
    (setf
     (setf (car (aref input (cl-incf i))) (list i (aref input i))))
    (backquote
     `(prefix ,(when x (+ x 1)) ,@items (sub ,(unless x (car items))) suffix))
    (push
     (push (list i) (aref input (cl-incf i))))
    (pop
     (list (pop items) (pop (aref input (cl-incf i)))))
    (defun
     (defun par-macroexp-corpus-defined (x)
       "Macro expansion fixture."
       (declare (pure t) (indent 1))
       (when x (or (car x) (cdr x)))))
    (plain
     (let* ((x (aref input i)) (y (car x))) (if y (+ 1 y) 0)))
    (environment
     (list (par-macroexp-local 4) (when ignored (par-macroexp-local 6))))))

(defvar par-macroexp-corpus-environment
  (list (cons 'par-macroexp-local (lambda (x) (list '1+ x))) '(when)))

(defun par-macroexp-corpus--call (value) value)

(defun par-macroexp-corpus--compiler-macro (form value)
  (if (integerp value) (list 'quote (1+ value)) form))

(defun par-macroexp-corpus--gensyms (form seen)
  "Record the names and identity of FORM's gensyms in traversal order.
The standalone printer omits the `#:' and shared-symbol labels.  An
explicit ID keeps the comparison sensitive to binding identity."
  (cond
   ((consp form)
    (let ((head (par-macroexp-corpus--gensyms (car form) seen)))
      (cons head (par-macroexp-corpus--gensyms (cdr form) seen))))
   ((and (symbolp form) (not (eq form (intern-soft (symbol-name form)))))
    (let ((id (or (gethash form seen)
                  (puthash form (1+ (hash-table-count seen)) seen))))
      (list :gensym id (symbol-name form))))
   (t form)))

(unless (bound-and-true-p par-macroexp-corpus-quiet)
  (dolist (entry par-macroexp-corpus)
    (let* ((gensym-counter 0)
           (expanded (macroexpand-all
                      (cadr entry)
                      (and (eq (car entry) 'environment)
                           par-macroexp-corpus-environment)))
           (raw (bound-and-true-p par-macroexp-corpus-raw))
           (print-gensym t)
           (print-circle raw))
      (prin1 (list (car entry)
                   (if raw expanded
                     (par-macroexp-corpus--gensyms expanded (make-hash-table :test 'eq)))))
      (terpri)))
  (let ((original (symbol-function 'macroexpand-1)))
    (unwind-protect
        (progn
          (fset 'macroexpand-1
                (lambda (form &optional environment)
                  (if (eq (car-safe form) 'identity)
                      '(quote remapped)
                    (funcall original form environment))))
          (prin1 (list 'redefined-expander (macroexpand-all '(list (identity 7)))))
          (terpri))
      (fset 'macroexpand-1 original)))
  (function-put 'par-macroexp-corpus--call 'compiler-macro
                #'par-macroexp-corpus--compiler-macro)
  (prin1 (list 'compiler-macro (macroexpand-all '(list (par-macroexp-corpus--call 7)))))
  (terpri)
  (let ((macroexp--dynvars nil))
    (macroexpand-all '(defvar par-macroexp-inner))
    (macroexpand--all-toplevel '(defvar par-macroexp-top))
    (prin1 (list 'dynamic-declarations macroexp--dynvars)))
  (terpri)
  (prin1 (list 'outer-plain
               (mapcar (lambda (form) (eq (macroexpand form) form))
                       '((par-macroexp-unknown 1) (nil 1) (t 1) (aref input 1)))))
  (terpri)
  (prin1 (list 'push-evaluation
               (eval '(let ((i -1) (input (vector '(a) '(b) '(c))) events)
                        (list (push (progn (push 'value events) 7)
                                    (aref input (progn (push 'place events) (cl-incf i))))
                              i (append input nil) (nreverse events)))
                     t)))
  (terpri)
  (prin1 (list 'pop-evaluation
               (eval '(let ((i -1) (input (vector '(7 8) '(9) '(10))))
                        (list (pop (aref input (cl-incf i))) i (append input nil)))
                     t)))
  (terpri))
