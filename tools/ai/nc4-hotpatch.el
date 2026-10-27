;;; nc4-hotpatch.el --- load the nc4 fx compiler into an older image  -*- lexical-binding: t; -*-
;; Defines `nc4--fx-compile-new' from this tree's `nelisp--fx-compile'
;; (scripts/nelisp-stdlib-prelude.el) and routes `nelisp--fx-compile' to it
;; only for the bodies of the functions named in `nc4--hot', so a binary built before nc4 compiles
;; the handler forms for a harvest or an fxir reference transcript.  Its word
;; VM has no handler opcodes: code compiled for those names must not run on
;; it, and everything else keeps the image's own compiler.
(defvar nc4--hot nil)
(let ((want '(nelisp--fx-compile)))
  (with-temp-buffer
    (insert-file-contents (expand-file-name "scripts/nelisp-stdlib-prelude.el"))
    (goto-char (point-min))
    (let ((form t))
      (while (and want (setq form (condition-case nil (read (current-buffer)) (end-of-file nil))))
        (when (and (eq (car-safe form) 'defun) (memq (nth 1 form) want))
          (eval (cons 'defun (cons 'nc4--fx-compile-new (nthcdr 2 form))) t)
          (setq want (delq (nth 1 form) want))))))
  (when want (error "nc4 hot patch: not found %S" want)))
(defun nc4--hot-body-p (body)
  (let ((ns nc4--hot) (hit nil))
    (while (and ns (not hit))
      (let ((f (and (fboundp (car ns)) (symbol-function (car ns)))))
        (when (and (eq (car-safe f) 'closure) (eq body (nthcdr 3 f))) (setq hit t)))
      (setq ns (cdr ns)))
    hit))
(let ((old (symbol-function 'nelisp--fx-compile)))
  (fset 'nelisp--fx-compile
        (lambda (form &optional params)
          (if (nc4--hot-body-p (cdr-safe form)) (nc4--fx-compile-new form params) (funcall old form params))))
  (fset 'nc4--fx-hopeless-old (symbol-function 'nelisp--fx-hopeless-p))
  (fset 'nelisp--fx-hopeless-p
        (lambda (body) (if (nc4--hot-body-p body) nil (nc4--fx-hopeless-old body)))))
