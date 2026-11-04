;;; emacs-cl-struct-setf.el --- setf support for GNU cl-defstruct accessors -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; On the standalone runtime the genuine cl-macs.el `cl-defstruct' is the
;; active macro once the bootstrap tail has loaded.  It defines accessors with
;; `define-inline', which the runtime lowers to plain functions without the
;; compiler-macro metadata GNU gv.el uses to turn `(setf (ACC OBJ) V)' into an
;; `aset'.  gv therefore falls back to calling a function named `(setf ACC)',
;; which does not exist.
;;
;; This file wraps the active `cl-defstruct' and, for every slot accessor in
;; its expansion, appends a `(setf ACC)' function that stores through the
;; same index the accessor reads.  It must load after cl-macs.el, so it is
;; listed in `nelisp-bootstrap-post-vendor-tail-files'.  It is inert on host
;; GNU Emacs, where the real compiler macros already exist.

;;; Code:

(defun emacs-cl-struct-setf--find-index (form)
  "Return (KIND . INDEX) for the first `aref'/`nth' with a literal index in FORM."
  (cond
   ((not (consp form)) nil)
   ((and (eq (car form) 'aref) (integerp (nth 2 form)))
    (cons 'aref (nth 2 form)))
   ((and (eq (car form) 'nth) (integerp (nth 1 form)))
    (cons 'nth (nth 1 form)))
   (t (let ((found nil) (tail form))
        (while (and (consp tail) (not found))
          (setq found (emacs-cl-struct-setf--find-index (car tail))
                tail (cdr tail)))
        found))))

(defun emacs-cl-struct-setf--forms (expansion)
  "Return `(setf ACC)' definition forms for the accessors in EXPANSION."
  (let ((accessors nil) (defs nil) (out nil))
    (when (eq (car-safe expansion) 'progn)
      (dolist (f (cdr expansion))
        (cond
         ((and (eq (car-safe f) 'function-put)
               (eq (car-safe (nth 1 f)) 'quote)
               (equal (nth 2 f) ''document-generalized-variable))
          (push (cadr (nth 1 f)) accessors))
         ((eq (car-safe f) 'define-inline)
          (push (cons (nth 1 f) (emacs-cl-struct-setf--find-index (cddr f)))
                defs)))))
    (dolist (acc (nreverse accessors))
      (let ((idx (cdr (assq acc defs))))
        (when idx
          (push (list 'defalias
                      (list 'quote (intern (format "(setf %s)" acc)))
                      (list 'lambda '(val obj)
                            (if (eq (car idx) 'aref)
                                (list 'aset 'obj (cdr idx) 'val)
                              (list 'setcar (list 'nthcdr (cdr idx) 'obj)
                                    'val))))
                out))))
    (nreverse out)))

(defun emacs-cl-struct-setf--augment (expansion)
  "Return EXPANSION with `(setf ACC)' definitions added before its value."
  (let ((extra (emacs-cl-struct-setf--forms expansion)))
    (if (and extra (eq (car-safe expansion) 'progn) (cdr expansion))
        (let ((body (cdr expansion)))
          (cons 'progn
                (append (butlast body) extra (last body))))
      expansion)))

(when (and (fboundp 'nelisp--repr)
           (fboundp 'cl-defstruct)
           (not (get 'cl-defstruct 'emacs-cl-struct-setf)))
  (let ((emacs-cl-struct-setf--expander (cdr (symbol-function 'cl-defstruct))))
    (defmacro cl-defstruct (name &rest slots)
      (emacs-cl-struct-setf--augment
       (apply emacs-cl-struct-setf--expander name slots)))
    (put 'cl-defstruct 'emacs-cl-struct-setf t)))

(provide 'emacs-cl-struct-setf)

;;; emacs-cl-struct-setf.el ends here
