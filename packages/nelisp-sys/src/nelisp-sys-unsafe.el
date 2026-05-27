;;; nelisp-sys-unsafe.el --- Unsafe boundary + allocation-effect checker for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 130.S1 / 131.5: unsafe boundary and allocation-effect checker.
;; Walks every defun body in a module and enforces:
;;
;;   E-SYS-UNSAFE-001  ptr-load / ptr-store! / ptr-add outside an unsafe block.
;;   E-SYS-UNSAFE-002  call to an :unsafe extern outside an unsafe block.
;;   E-SYS-UNSAFE-003  slice-ref with :raw t outside an unsafe block.
;;   E-SYS-UNSAFE-004  resource-op with :op sys:forget outside an unsafe block.
;;   E-SYS-ALLOC-001   call (to a heap-alloc callee or an unknown callee) from a
;;                      function declared :alloc none.
;;
;; The `unsafe' node sets the unsafe-p flag for its :body only; the flag
;; does NOT propagate out.

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)
(require 'nelisp-sys-types)
(require 'nelisp-sys-ast)
(require 'nelisp-sys-check)

;;;; Error setup.

(define-error 'nelisp-sys-unsafe-error
  "nelisp-sys unsafe/effect error" 'nelisp-sys-error)

(defun nelisp-sys-unsafe--fail (code form fmt &rest args)
  "Signal a `nelisp-sys-unsafe-error' with stable CODE for FORM."
  (signal 'nelisp-sys-unsafe-error
          (list code (apply #'format fmt args) :form form)))

(defun nelisp-sys-unsafe-error-code (err)
  "Return the stable diagnostic code from a `nelisp-sys-unsafe-error' ERR."
  (nth 1 err))

(defun nelisp-sys-unsafe-error-message (err)
  "Return the message string from a `nelisp-sys-unsafe-error' ERR."
  (nth 2 err))

;;;; Walker.

(defun nelisp-sys-unsafe--walk (ctx node unsafe-p alloc-none-p)
  "Recursively walk NODE enforcing unsafe-boundary and alloc-effect rules.
CTX is the check context (provides the funcs table).
UNSAFE-P is non-nil when NODE is lexically inside a (sys:unsafe ...) block.
ALLOC-NONE-P is non-nil when the enclosing defun declares :alloc none."
  (let ((kind (nelisp-sys-ast-kind node))
        (form (nelisp-sys-ast-prop node :form)))
    (cond
     ;;; Rule 1: ptr ops outside unsafe.
     ((memq kind '(ptr-load ptr-store! ptr-add))
      (unless unsafe-p
        (nelisp-sys-unsafe--fail 'E-SYS-UNSAFE-001 form
                                 "pointer operation %S outside unsafe block" kind))
      ;; Recurse children with same unsafe-p (already inside, but flag is t here).
      (dolist (child (nelisp-sys-ast-children node))
        (nelisp-sys-unsafe--walk ctx child unsafe-p alloc-none-p)))

     ;;; Rule 3: raw slice-ref outside unsafe.
     ((eq kind 'slice-ref)
      (when (and (nelisp-sys-ast-prop node :raw nil) (not unsafe-p))
        (nelisp-sys-unsafe--fail 'E-SYS-UNSAFE-003 form
                                 "raw slice-ref outside unsafe block"))
      (dolist (child (nelisp-sys-ast-children node))
        (nelisp-sys-unsafe--walk ctx child unsafe-p alloc-none-p)))

     ;;; Rule 4: sys:forget outside unsafe.
     ((eq kind 'resource-op)
      (when (and (eq (nelisp-sys-ast-prop node :op) 'sys:forget) (not unsafe-p))
        (nelisp-sys-unsafe--fail 'E-SYS-UNSAFE-004 form
                                 "sys:forget outside unsafe block"))
      (dolist (child (nelisp-sys-ast-children node))
        (nelisp-sys-unsafe--walk ctx child unsafe-p alloc-none-p)))

     ;;; Call: rules 2 and 5, then recurse args.
     ((eq kind 'call)
      (let* ((fn (nelisp-sys-ast-prop node :fn))
             (sig (gethash fn (nelisp-sys-check-ctx-funcs ctx))))
        ;; Rule 2: unsafe extern call outside unsafe.
        (when (and sig
                   (eq (plist-get sig :kind) 'extern)
                   (plist-get sig :unsafe)
                   (not unsafe-p))
          (nelisp-sys-unsafe--fail 'E-SYS-UNSAFE-002 form
                                   "call to unsafe extern %S outside unsafe block"
                                   fn))
        ;; Rule 5: alloc-none function calling a heap-alloc or unknown callee.
        (when alloc-none-p
          (let ((callee-alloc (and sig (plist-get (plist-get sig :effects) :alloc))))
            (when (or (null sig)              ; unknown callee
                      (eq callee-alloc 'heap))
              (nelisp-sys-unsafe--fail 'E-SYS-ALLOC-001 form
                                       "call to %S (alloc=%S) from :alloc none function"
                                       fn (or callee-alloc 'unknown))))))
      (dolist (child (nelisp-sys-ast-children node))
        (nelisp-sys-unsafe--walk ctx child unsafe-p alloc-none-p)))

     ;;; unsafe node: recurse body with unsafe-p=t.
     ((eq kind 'unsafe)
      (dolist (child (nelisp-sys-ast-prop node :body))
        (nelisp-sys-unsafe--walk ctx child t alloc-none-p)))

     ;;; All other nodes: recurse children unchanged.
     (t
      (dolist (child (nelisp-sys-ast-children node))
        (nelisp-sys-unsafe--walk ctx child unsafe-p alloc-none-p))))))

;;;; Defun entry point.

(defun nelisp-sys-unsafe--check-defun (ctx item)
  "Check one defun ITEM for unsafe-boundary and alloc-effect violations."
  (let* ((effects (nelisp-sys-ast-prop item :effects))
         (alloc-none-p (eq (plist-get effects :alloc) 'none)))
    (dolist (node (nelisp-sys-ast-prop item :body))
      (nelisp-sys-unsafe--walk ctx node nil alloc-none-p))))

;;;; Public API.

(defun nelisp-sys-unsafe-check-module (module)
  "Check MODULE for unsafe-boundary and allocation-effect violations.
Signals `nelisp-sys-unsafe-error' with a stable code on the first
violation.  Returns t when the module is clean."
  (let ((ctx (nelisp-sys-check-build-ctx module)))
    (dolist (item (nelisp-sys-ast-prop module :items))
      (when (eq (nelisp-sys-ast-kind item) 'defun)
        (nelisp-sys-unsafe--check-defun ctx item)))
    t))

(defun nelisp-sys-unsafe-check-collect (module)
  "Check MODULE, returning a list of (CODE MESSAGE FORM) diagnostics.
Returns nil when the module is clean.  Stops at the first error."
  (condition-case err
      (progn (nelisp-sys-unsafe-check-module module) nil)
    (nelisp-sys-unsafe-error
     (list (list (nelisp-sys-unsafe-error-code err)
                 (nelisp-sys-unsafe-error-message err)
                 (plist-get (nthcdr 3 err) :form))))))

(provide 'nelisp-sys-unsafe)

;;; nelisp-sys-unsafe.el ends here
