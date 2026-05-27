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

(provide 'nelisp-sys-ast)

;;; nelisp-sys-ast.el ends here
