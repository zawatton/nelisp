;;; nelisp-sys-ast.el --- Typed AST node ADT for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 131.1: the typed AST.  Nodes are plists `(:kind K ...props)'.  The
;; frontend builds them; the checkers (type/ownership/borrow/effect) and the
;; backend consume them.  Keeping nodes as plain plists keeps the IR
;; inspectable in tests and trivially serializable for golden snapshots.
;;
;; Every node may carry a `:form' prop holding the original source form,
;; used by the diagnostics layer.  Real text spans are deferred (the
;; frontend currently parses s-expressions, not characters); `:form' is the
;; MVP stand-in.
;;
;; The module deliberately uses only `cl-lib' and core special forms (no
;; `pcase') so it stays portable and extractable, independent of any host
;; `pcase' shadowing on the load path.

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)

(defun nelisp-sys-ast-make (kind &rest props)
  "Construct an AST node of KIND with PROPS (a plist)."
  (cons :kind (cons kind props)))

(defun nelisp-sys-ast-node-p (x)
  "Return non-nil if X is an AST node plist."
  (and (consp x) (eq (car x) :kind) (symbolp (cadr x))))

(defun nelisp-sys-ast-kind (node)
  "Return the kind symbol of NODE."
  (cadr node))

(defun nelisp-sys-ast-prop (node key &optional default)
  "Return the value of KEY in NODE, or DEFAULT when absent."
  (if (plist-member node key)
      (plist-get node key)
    default))

(defun nelisp-sys-ast-set-prop (node key value)
  "Return a copy of NODE with KEY set to VALUE (non-destructive)."
  (plist-put (copy-sequence node) key value))

(defun nelisp-sys-ast-describe (node)
  "Return a short human string describing NODE for diagnostics."
  (if (not (nelisp-sys-ast-node-p node))
      (format "%S" node)
    (let ((k (nelisp-sys-ast-kind node)))
      (cl-case k
        (int (format "int %S" (nelisp-sys-ast-prop node :value)))
        (bool (format "bool %S" (nelisp-sys-ast-prop node :value)))
        (var (format "var %S" (nelisp-sys-ast-prop node :name)))
        (binop (format "(%S ...)" (nelisp-sys-ast-prop node :op)))
        (call (format "call %S" (nelisp-sys-ast-prop node :fn)))
        (t (format "%S" k))))))

(defun nelisp-sys-ast-children (node)
  "Return the immediate child expression nodes of NODE, in evaluation order.
Used by the analysis passes (unsafe/effect/ownership/borrow) for generic
traversal.  Type/field/struct slots that are not expression nodes are
excluded."
  (cl-case (nelisp-sys-ast-kind node)
    ((int bool var sizeof alignof offsetof) nil)
    (binop (nelisp-sys-ast-prop node :args))
    (not (list (nelisp-sys-ast-prop node :arg)))
    (cast (list (nelisp-sys-ast-prop node :arg)))
    (let (append
          (mapcar (lambda (b) (nth 2 b)) (nelisp-sys-ast-prop node :bindings))
          (nelisp-sys-ast-prop node :body)))
    (seq (nelisp-sys-ast-prop node :body))
    (if (delq nil (list (nelisp-sys-ast-prop node :cond)
                        (nelisp-sys-ast-prop node :then)
                        (nelisp-sys-ast-prop node :else))))
    (cond (apply #'append
                 (mapcar (lambda (cl)
                           (append (and (nelisp-sys-ast-node-p (car cl))
                                        (list (car cl)))
                                   (cdr cl)))
                         (nelisp-sys-ast-prop node :clauses))))
    (while (cons (nelisp-sys-ast-prop node :cond)
                 (nelisp-sys-ast-prop node :body)))
    (set! (list (nelisp-sys-ast-prop node :value)))
    (load-field (list (nelisp-sys-ast-prop node :place)))
    (store-field! (list (nelisp-sys-ast-prop node :place)
                        (nelisp-sys-ast-prop node :value)))
    (slice-len (list (nelisp-sys-ast-prop node :slice)))
    (slice-ref (list (nelisp-sys-ast-prop node :slice)
                     (nelisp-sys-ast-prop node :index)))
    (slice-set! (list (nelisp-sys-ast-prop node :slice)
                      (nelisp-sys-ast-prop node :index)
                      (nelisp-sys-ast-prop node :value)))
    (ptr-load (list (nelisp-sys-ast-prop node :ptr)))
    (ptr-store! (list (nelisp-sys-ast-prop node :ptr)
                      (nelisp-sys-ast-prop node :value)))
    (ptr-add (list (nelisp-sys-ast-prop node :ptr)
                   (nelisp-sys-ast-prop node :offset)))
    (exit (list (nelisp-sys-ast-prop node :arg)))
    (unsafe (nelisp-sys-ast-prop node :body))
    (with-borrow (cons (nelisp-sys-ast-prop node :place)
                       (nelisp-sys-ast-prop node :body)))
    (resource-op (list (nelisp-sys-ast-prop node :arg)))
    (call (nelisp-sys-ast-prop node :args))
    (call-ptr (cons (nelisp-sys-ast-prop node :fn-expr)
                    (nelisp-sys-ast-prop node :args)))
    (addr-of nil)
    (atomic-add (list (nelisp-sys-ast-prop node :ptr)
                      (nelisp-sys-ast-prop node :delta)))
    (atomic-cas (list (nelisp-sys-ast-prop node :ptr)
                      (nelisp-sys-ast-prop node :expected)
                      (nelisp-sys-ast-prop node :new)))
    (alloc (list (nelisp-sys-ast-prop node :size)
                 (nelisp-sys-ast-prop node :align)))
    (dealloc (list (nelisp-sys-ast-prop node :ptr)
                   (nelisp-sys-ast-prop node :size)
                   (nelisp-sys-ast-prop node :align)))
    (t nil)))

(provide 'nelisp-sys-ast)

;;; nelisp-sys-ast.el ends here
