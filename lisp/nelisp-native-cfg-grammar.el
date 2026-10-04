;;; nelisp-native-cfg-grammar.el --- Versioned raw CFG grammar -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; A CFG is an integer expression: (cfg 1 ENTRY (block ID FORMS TERM)...).
;; TERM is (jump ID), (branch TEST YES NO), (dispatch VALUE ((I ID)...) DEFAULT),
;; (return VALUE), or (status VALUE).  Boxed objects stay in authenticated roots;
;; VALUE is the raw status/return-root selector, never a literal Lisp object.
;; Locals shared across blocks are bound by the enclosing ordinary let.
;;; Code:
(require 'cl-lib)

(defconst nelisp-native-cfg-grammar-version 1)

(defun nelisp-native-cfg-grammar-validate-tree (form)
  "Validate every unquoted CFG in FORM before backend allocation."
  (let ((pending (list form)))
    (while pending
      (let ((node (pop pending)))
        (when (consp node)
          (unless (proper-list-p node) (error "raw CFG: improper source form"))
          (pcase (car node)
            ((or 'quote 'function) nil)
            ((or 'let 'let*)
             ;; Binding names are metadata, even when a local is named cfg.
             (dolist (binding (cadr node))
               (when (consp binding) (push (cadr binding) pending)))
             (setq pending (append (cddr node) pending)))
            ('defun (setq pending (append (cdddr node) pending)))
            ('cfg
             (dolist (block (nelisp-native-cfg-grammar-validate node))
               (setq pending (append (nth 2 block) pending))
               (let ((term (nth 3 block)))
                 (unless (eq (car term) 'jump) (push (cadr term) pending)))))
            (_ (dolist (child node) (when (consp child) (push child pending)))))))))
  form)

(defun nelisp-native-cfg-grammar-validate (form)
  "Validate FORM before either backend emits effects; return its blocks.
This slice admits reachable acyclic graphs only.  Labels are non-nil symbols
or nonnegative integers.  Bounds limit compilation resources, not execution."
  (unless (and (proper-list-p form) (>= (length form) 4)
               (eq (car form) 'cfg) (eql (cadr form) 1)
               (<= (length (cdddr form)) 4096))
    (error "raw CFG: unsupported version or shape"))
  (let ((blocks (cdddr form)) (labels nil) (edges nil))
    (cl-labels ((label-p (id) (or (and (symbolp id) id (not (memq id '(t nil))))
                                (and (integerp id) (>= id 0)))))
      (dolist (block blocks)
        (unless (and (proper-list-p block) (= (length block) 4)
                     (eq (car block) 'block) (label-p (cadr block))
                     (not (member (cadr block) labels))
                     (proper-list-p (nth 2 block)))
          (error "raw CFG: malformed or duplicate block %S" block))
        (push (cadr block) labels)
        (let ((term (nth 3 block)) (targets nil))
          (unless (proper-list-p term) (error "raw CFG: malformed terminator"))
          (pcase (car term)
            ('jump
             (unless (= (length term) 2) (error "raw CFG: jump arity"))
             (setq targets (cdr term)))
            ('branch
             (unless (= (length term) 4) (error "raw CFG: branch arity"))
             (setq targets (cddr term)))
            ('dispatch
             (unless (and (= (length term) 4) (proper-list-p (nth 2 term)))
               (error "raw CFG: dispatch shape"))
             (let ((keys nil))
               (dolist (case (nth 2 term))
                 (unless (and (proper-list-p case) (= (length case) 2)
                              (integerp (car case)) (memq (ash (car case) -63) '(0 -1))
                              (not (member (car case) keys)))
                   (error "raw CFG: malformed or duplicate dispatch key"))
                 (push (car case) keys) (push (cadr case) targets)))
             (push (nth 3 term) targets))
            ((or 'return 'status)
             (unless (= (length term) 2) (error "raw CFG: return/status arity")))
            (_ (error "raw CFG: unknown terminator %S" term)))
          (unless (cl-every #'label-p targets) (error "raw CFG: invalid target"))
          (push (cons (cadr block) (delete-dups targets)) edges)))
      (unless (member (nth 2 form) labels) (error "raw CFG: missing entry"))
      (dolist (edge edges)
        (unless (cl-every (lambda (id) (member id labels)) (cdr edge))
          (error "raw CFG: missing target label")))
      ;; Iterative DFS avoids recursive block expansion, including on bad input.
      (let ((pending (list (cons (nth 2 form) nil))) (active nil) (done nil))
        (while pending
          (let* ((event (pop pending)) (id (car event)))
            (cond
             ((cdr event) (setq active (delete id active)) (push id done))
             ((member id active) (error "raw CFG: cycles require U1b"))
             ((member id done) nil)
             (t (push id active) (push (cons id t) pending)
                (dolist (target (cdr (assoc id edges)))
                  (push (cons target nil) pending))))))
        (unless (= (length done) (length labels)) (error "raw CFG: unreachable block"))))
    blocks))

(defun nelisp-native-cfg-grammar-aot-form (form)
  "Lower validated FORM to AOT labels and relocatable jumps, each block once.
Reuse the assembler's existing landing-label IR, without adding runtime natives."
  (let* ((blocks (nelisp-native-cfg-grammar-validate form))
         (prefix (symbol-name (gensym "raw_cfg_")))
         (result (gensym "raw_cfg_result_"))
         (finish (intern (concat prefix "finish")))
         (index 0)
         (labels (mapcar (lambda (b)
                           (setq index (1+ index))
                           (cons (cadr b) (intern (format "%s_%d" prefix index)))) blocks)))
    (cl-labels
        ((jump (id) `(aot-machine-landing-jump (aot-current-sp) ,(cdr (assoc id labels))))
         (term (node)
           (pcase (car node)
             ('jump (jump (cadr node)))
             ('branch `(if ,(cadr node) ,(jump (nth 2 node)) ,(jump (nth 3 node))))
             ('dispatch
              (let ((selector (gensym "raw_cfg_selector_"))
                    (body (jump (nth 3 node))))
                (dolist (case (reverse (nth 2 node)))
                  (setq body `(if (= ,selector ,(car case)) ,(jump (cadr case)) ,body)))
                `(let ((,selector ,(cadr node))) ,body)))
             (_ `(progn (setq ,result ,(cadr node))
                        (aot-machine-landing-jump (aot-current-sp) ,finish))))))
      `(let ((,result 0))
         (progn ,(jump (nth 2 form))
                ,@(mapcar (lambda (b)
                            `(aot-landing-label ,(cdr (assoc (cadr b) labels))
                               (progn ,@(nth 2 b) ,(term (nth 3 b))))) blocks)
                (aot-landing-label ,finish ,result))))))

(provide 'nelisp-native-cfg-grammar)
;;; nelisp-native-cfg-grammar.el ends here
