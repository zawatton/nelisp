;;; nelisp-bytecode-native-rooted-cfg-postdom.el --- bounded CFG postdominators -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Compute postdominators and nearest common joins for freshly verified,
;; acyclic rooted-CFG plans.  This is analysis only; it does not lower code.

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-native-rooted-cfg)
(require 'nelisp-bytecode-native-rooted-cfg-plan)

(defun nelisp-bytecode-native-rooted-cfg-postdom--nearest-common (common postdom)
  "Return the unique nearest postdominator in COMMON, or nil if ambiguous."
  (let ((nearest
         (cl-remove-if
          (lambda (candidate)
            (cl-some (lambda (other)
                       (and (/= other candidate)
                            (memq candidate (gethash other postdom))))
                     common))
          common)))
    (and (= (length nearest) 1) (car nearest))))

(defun nelisp-bytecode-native-rooted-cfg-postdom--sets (order by-id)
  "Compute bounded fixed-point postdominators with a synthetic exit.
A closed SCC has no usable exit postdominator. Paths into such a component
also prevent fabricating a join on a different terminating arm."
  (let ((postdom (make-hash-table :test 'eql)) (exit -1)
        (can-exit nil) (changed t) (steps 0))
    (while changed
      (setq changed nil)
      (dolist (id order)
        (let ((successors (append (plist-get (gethash id by-id) :successors) nil)))
          (when (and (not (memq id can-exit))
                     (or (null successors)
                         (cl-some (lambda (e) (memq (plist-get e :target) can-exit)) successors)))
            (push id can-exit) (setq changed t)))))
    (puthash exit (list exit) postdom)
    (dolist (id order)
      (puthash id (if (memq id can-exit) (cons exit (copy-sequence can-exit)) (list id)) postdom))
    (setq changed t)
    (while changed
      (setq changed nil)
      (dolist (id order)
        (setq steps (1+ steps))
        (when (> steps nelisp-bytecode-native-rooted-cfg-max-analysis-steps)
          (error "postdominator analysis budget exceeded"))
        (when (memq id can-exit)
          (let* ((successors (or (mapcar (lambda (e) (plist-get e :target))
                                        (append (plist-get (gethash id by-id) :successors) nil))
                                 (list exit)))
                 (common (copy-sequence (gethash (car successors) postdom))))
            (dolist (other (cdr successors))
              (setq common (cl-intersection common (gethash other postdom))))
            (let ((new (sort (delete-dups (cons id common)) #'<)))
              (unless (equal new (gethash id postdom))
                (puthash id new postdom) (setq changed t)))))))
    postdom))

(defun nelisp-bytecode-native-rooted-cfg-postdom--joins (order by-id postdom)
  "Return nearest common postdominators for conditional blocks."
  (let ((joins nil) (failure nil))
    (dolist (id order)
      (let* ((block (gethash id by-id))
             (successors (mapcar (lambda (edge) (plist-get edge :target))
                                 (append (plist-get block :successors) nil))))
        (when (= (length successors) 2)
          (let* ((common (cl-intersection (gethash (car successors) postdom)
                                          (gethash (cadr successors) postdom)
                                          :test #'eql))
                 (nearest (nelisp-bytecode-native-rooted-cfg-postdom--nearest-common
                           common postdom)))
            (cond ((not (memq -1 (gethash id postdom))) (push (cons id nil) joins))
                  (nearest (push (cons id (and (not (= nearest -1)) nearest)) joins))
                  ((null common) (push (cons id nil) joins))
                  (t (setq failure "conditional has ambiguous common postdominators")))))))
    (if failure
        (list :status 'unsupported :reason failure)
      (list :status 'complete :joins (nreverse joins)))))

(defun nelisp-bytecode-native-rooted-cfg-postdom--compute (blocks order)
  "Compute postdominators and branch joins for BLOCKS in topological ORDER."
  (let ((by-id (make-hash-table :test #'eql)))
    (dolist (block blocks)
      (let ((id (plist-get block :start)))
        (puthash id block by-id)))
    (let* ((postdom (nelisp-bytecode-native-rooted-cfg-postdom--sets order by-id))
           (joins (nelisp-bytecode-native-rooted-cfg-postdom--joins order by-id postdom)))
      (if (eq (plist-get joins :status) 'unsupported)
          joins
        (list :status 'complete :scope 'acyclic-multi-exit
              :block-order (copy-sequence order)
              :postdominators
              (mapcar (lambda (id) (cons id (delq -1 (copy-sequence (gethash id postdom))))) order)
              :nearest-joins (plist-get joins :joins))))))

(defun nelisp-bytecode-native-rooted-cfg-postdom--canonical-input-p (input)
  "Return non-nil when INPUT metadata matches a fresh frame decode."
  (let* ((function (plist-get input :function))
         (fresh (and (byte-code-function-p function)
                     (nelisp-bytecode-compiler-input-build function)))
         (keys '(:status :argument-descriptor :argument-count :argument-min
                 :argument-max :rest-argument-p :initial-stack-depth :code
                 :constants :frame-result)))
    (and fresh
         ;; A fresh input can retain non-fixnum/primitive diagnostics while
         ;; its canonical rooted plan proves those operations through F1.
         ;; Analyze still requires that independently admitted complete plan.
         (memq (plist-get fresh :status) '(complete unsupported))
         (cl-every (lambda (key) (equal (plist-get input key)
                                        (plist-get fresh key)))
                   keys))))

(defun nelisp-bytecode-native-rooted-cfg-postdom-analyze (input)
  "Return verified postdominators and nearest joins for compiler INPUT."
  (let* ((canonical (nelisp-bytecode-native-rooted-cfg-postdom--canonical-input-p input))
         (plan (and canonical (nelisp-bytecode-native-rooted-cfg-plan input)))
         (frame (plist-get input :frame-result))
         (topology (and (eq (plist-get plan :status) 'complete)
                        (nelisp-bytecode-native-rooted-cfg-topology-check frame))))
    (if (not (and canonical
                  (eq (plist-get plan :status) 'complete)
                  (eq (plist-get topology :status) 'complete)))
        (list :status 'unsupported :reason "input lacks a fresh complete rooted CFG plan")
      (nelisp-bytecode-native-rooted-cfg-postdom--compute
       (append (plist-get frame :blocks) nil)
       (plist-get topology :block-order)))))

(provide 'nelisp-bytecode-native-rooted-cfg-postdom)
;;; nelisp-bytecode-native-rooted-cfg-postdom.el ends here
