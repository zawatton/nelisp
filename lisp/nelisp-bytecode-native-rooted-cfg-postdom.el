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
  "Return postdominator sets for verified blocks BY-ID in reverse ORDER."
  (let ((postdom (make-hash-table :test #'eql)))
    (dolist (id (reverse order))
      (let* ((block (gethash id by-id))
             (successors (mapcar (lambda (edge) (plist-get edge :target))
                                 (append (plist-get block :successors) nil)))
             (common (if successors
                         (copy-sequence (gethash (car successors) postdom))
                       (list id))))
        (dolist (successor (cdr successors))
          (setq common (cl-intersection common (gethash successor postdom)
                                        :test #'eql)))
        (puthash id (if successors (cons id common) (list id)) postdom)))
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
            (cond (nearest (push (cons id nearest) joins))
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
              (mapcar (lambda (id) (cons id (gethash id postdom))) order)
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
         (eq (plist-get fresh :status) 'complete)
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
                  (eq (plist-get input :status) 'complete)
                  (eq (plist-get plan :status) 'complete)
                  (eq (plist-get topology :status) 'complete)))
        (list :status 'unsupported :reason "input lacks a fresh complete rooted CFG plan")
      (nelisp-bytecode-native-rooted-cfg-postdom--compute
       (append (plist-get frame :blocks) nil)
       (plist-get topology :block-order)))))

(provide 'nelisp-bytecode-native-rooted-cfg-postdom)
;;; nelisp-bytecode-native-rooted-cfg-postdom.el ends here
