;;; native-cfg-grammar-fixtures.el --- Raw CFG qualification helpers -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'cl-lib)
(require 'nelisp-native-cfg-grammar)

(defun native-cfg-grammar-fixtures-lower (form)
  "Translate a qualified structured raw DEFUN to CFG for backend testing.
Contracts continue to validate the original structured program.  This helper
runs only at the backend boundary, after validation; it neither changes the
planner nor admits new bytecodes.  Each conditional has one physical join."
  (let ((serial 0) (blocks nil) (locals nil))
    (cl-labels
        ((fresh () (setq serial (1+ serial)) (intern (format "cfg_fixture_%d" serial)))
         (local () (let ((name (fresh))) (push (list name 0) locals) name))
         (block (forms term) (let ((id (fresh))) (push (list 'block id forms term) blocks) id))
         (rename (node scope)
           (cond ((symbolp node) (or (cdr (assq node scope)) node))
                 ((atom node) node)
                 ((eq (car node) 'quote) node)
                 (t (mapcar (lambda (child) (rename child scope)) node))))
         (sequence (forms scope out next)
           (let ((entry next))
             (dolist (node (reverse forms)) (setq entry (lower node scope out entry)))
             entry))
         (lower (node scope out next)
           (cond
            ((and (consp node) (eq (car node) 'if))
             (let ((yes (lower (nth 2 node) scope out next))
                   (no (lower (nth 3 node) scope out next)))
               (block nil `(branch ,(rename (cadr node) scope) ,yes ,no))))
            ((and (consp node) (memq (car node) '(progn seq)))
             (sequence (cdr node) scope out next))
            ((and (consp node) (memq (car node) '(let let*)))
             (let ((inner scope) (initializers nil))
               (dolist (binding (cadr node))
                 (let ((name (local)))
                   (push (list name (rename (cadr binding)
                                           (if (eq (car node) 'let*) inner scope))) initializers)
                   (push (cons (car binding) name) inner)))
               (let ((entry (sequence (cddr node) inner out next)))
                 (dolist (binding initializers)
                   (setq entry (block (list `(setq ,(car binding) ,(cadr binding))) `(jump ,entry))))
                 entry)))
            (t (block (list `(setq ,out ,(rename node scope))) `(jump ,next))))))
      (let* ((out (local)) (finish (block nil `(return ,out)))
             (entry (sequence (cdddr form) nil out finish))
             (cfg `(cfg 1 cfg_dispatch_entry
                        (block cfg_dispatch_entry nil
                               (dispatch argument-count
                                         ((99 cfg_bad_arity) (98 cfg_bad_status)) cfg_body_entry))
                        (block cfg_bad_arity nil (status 2))
                        (block cfg_bad_status nil (status 3))
                        (block cfg_body_entry nil (jump ,entry))
                        ,@(nreverse blocks))))
        (nelisp-native-cfg-grammar-validate cfg)
        `(defun ,(cadr form) ,(nth 2 form) (let ,(nreverse locals) ,cfg))))))

(provide 'native-cfg-grammar-fixtures)
