;;; nelisp-sys-backend.el --- Native codegen backend for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 130.3 / 132.3: the MVP native code generator.  It lowers the
;; validated typed AST into the plain integer-subset s-expression form the
;; existing Phase 47 compiler accepts, then drives object emission through
;; the adapter (the only module allowed to touch the private backend).
;;
;; Scope of the MVP codegen: C-ABI integer/word functions — fixed-width
;; integer arithmetic, comparisons, logical ops, let/if/cond/while/seq,
;; set!, calls, and compile-time sizeof/alignof/offsetof folding.  Memory
;; operations (field/slice/raw-pointer load-store) are deferred (Stage
;; 130.4) and raise a clear `nelisp-sys-backend-error' so the boundary is
;; explicit rather than silently miscompiled.
;;
;; All integer values are 64-bit words in registers (SysV AMD64 / AAPCS64);
;; width casts are identity at this level for the MVP.

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)
(require 'nelisp-sys-ast)
(require 'nelisp-sys-types)
(require 'nelisp-sys-target)
(require 'nelisp-sys-abi-layout)
(require 'nelisp-sys-abi-meta)
(require 'nelisp-sys-check)
(require 'nelisp-sys-adapter-nelisp)

(define-error 'nelisp-sys-backend-error
  "nelisp-sys backend/codegen error" 'nelisp-sys-error)

(cl-defstruct (nelisp-sys-backend-ctx (:constructor nelisp-sys-backend--ctx-make))
  structs   ; struct env (for sizeof/offsetof folding)
  target    ; resolved target descriptor
  names)    ; hash: source fn name (symbol) -> emitted symbol

(defun nelisp-sys-backend--emit-symbol (item)
  "Return the emitted native symbol for defun ITEM.
Exported functions use their :export-name; others use their name."
  (if (eq (nelisp-sys-ast-prop item :linkage) :export)
      (intern (or (nelisp-sys-ast-prop item :export-name)
                  (symbol-name (nelisp-sys-ast-prop item :name))))
    (nelisp-sys-ast-prop item :name)))

(defun nelisp-sys-backend--unsupported (node what)
  "Signal that NODE's WHAT construct is outside the MVP integer codegen."
  (signal 'nelisp-sys-backend-error
          (list (format "%s is not supported by the MVP integer codegen" what)
                :form (nelisp-sys-ast-prop node :form))))

(defun nelisp-sys-backend--lower-body (ctx body)
  "Lower a BODY (list of nodes) to a single Phase 47 expression.
A single node lowers directly; multiple are wrapped in `seq'."
  (let ((forms (mapcar (lambda (n) (nelisp-sys-backend--lower ctx n)) body)))
    (cond ((null forms) 0)
          ((null (cdr forms)) (car forms))
          (t (cons 'seq forms)))))

(defun nelisp-sys-backend--lower (ctx node)
  "Lower a typed AST NODE to a plain Phase 47 integer-subset form."
  (cl-case (nelisp-sys-ast-kind node)
    (int (nelisp-sys-ast-prop node :value))
    (bool (if (nelisp-sys-ast-prop node :value) 1 0))
    (var (nelisp-sys-ast-prop node :name))
    (cast (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :arg)))
    (binop (nelisp-sys-backend--lower-binop ctx node))
    (not (list 'not (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :arg))))
    (let (nelisp-sys-backend--lower-let ctx node))
    (seq (cons 'seq (mapcar (lambda (n) (nelisp-sys-backend--lower ctx n))
                            (nelisp-sys-ast-prop node :body))))
    (if (list 'if
              (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :cond))
              (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :then))
              (if (nelisp-sys-ast-prop node :else)
                  (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :else))
                0)))
    (cond (nelisp-sys-backend--lower-cond ctx node))
    (while (list 'while
                 (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :cond))
                 (nelisp-sys-backend--lower-body
                  ctx (nelisp-sys-ast-prop node :body))))
    (set! (list 'setq (nelisp-sys-ast-prop node :name)
                (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :value))))
    (unsafe (nelisp-sys-backend--lower-body ctx (nelisp-sys-ast-prop node :body)))
    (sizeof (nelisp-sys-layout-sizeof (nelisp-sys-ast-prop node :type)
                                      (nelisp-sys-backend-ctx-target ctx)
                                      (nelisp-sys-backend-ctx-structs ctx)))
    (alignof (nelisp-sys-layout-alignof (nelisp-sys-ast-prop node :type)
                                        (nelisp-sys-backend-ctx-target ctx)
                                        (nelisp-sys-backend-ctx-structs ctx)))
    (offsetof (nelisp-sys-layout-offsetof
               (nelisp-sys-type-struct-name (nelisp-sys-ast-prop node :struct))
               (nelisp-sys-ast-prop node :field)
               (nelisp-sys-backend-ctx-target ctx)
               (nelisp-sys-backend-ctx-structs ctx)))
    (call (nelisp-sys-backend--lower-call ctx node))
    (exit (nelisp-sys-backend--unsupported node "sys:exit (freestanding)"))
    (load-field (nelisp-sys-backend--unsupported node "struct field load"))
    (store-field! (nelisp-sys-backend--unsupported node "struct field store"))
    ((slice-len slice-ref slice-set!)
     (nelisp-sys-backend--unsupported node "slice access"))
    ((ptr-load ptr-store! ptr-add)
     (nelisp-sys-backend--unsupported node "raw pointer access"))
    (with-borrow (nelisp-sys-backend--unsupported node "borrow"))
    (resource-op (nelisp-sys-backend--unsupported node "resource op"))
    (t (nelisp-sys-backend--unsupported node
                                        (format "node kind %S"
                                                (nelisp-sys-ast-kind node))))))

(defun nelisp-sys-backend--lower-binop (ctx node)
  "Lower a binop NODE."
  (let ((op (nelisp-sys-ast-prop node :op))
        (args (mapcar (lambda (a) (nelisp-sys-backend--lower ctx a))
                      (nelisp-sys-ast-prop node :args))))
    (cond
     ((eq op '/=)
      (unless (= (length args) 2)
        (nelisp-sys-backend--unsupported node "n-ary /="))
      (list 'not (cons '= args)))
     (t (cons op args)))))

(defun nelisp-sys-backend--lower-let (ctx node)
  "Lower a let NODE to (let ((NAME INIT)...) BODY)."
  (list 'let
        (mapcar (lambda (b)
                  (list (nth 0 b) (nelisp-sys-backend--lower ctx (nth 2 b))))
                (nelisp-sys-ast-prop node :bindings))
        (nelisp-sys-backend--lower-body ctx (nelisp-sys-ast-prop node :body))))

(defun nelisp-sys-backend--lower-cond (ctx node)
  "Lower a cond NODE to (cond (TEST BODY)...)."
  (cons 'cond
        (mapcar
         (lambda (clause)
           (cons (if (eq (car clause) 'else)
                     t
                   (nelisp-sys-backend--lower ctx (car clause)))
                 (list (nelisp-sys-backend--lower-body ctx (cdr clause)))))
         (nelisp-sys-ast-prop node :clauses))))

(defun nelisp-sys-backend--lower-call (ctx node)
  "Lower a call NODE, mapping the callee to its emitted symbol."
  (let* ((fn (nelisp-sys-ast-prop node :fn))
         (sym (or (gethash fn (nelisp-sys-backend-ctx-names ctx)) fn))
         (args (mapcar (lambda (a) (nelisp-sys-backend--lower ctx a))
                       (nelisp-sys-ast-prop node :args))))
    (cons sym args)))

(defun nelisp-sys-backend--lower-defun (ctx item)
  "Lower defun ITEM to a plain Phase 47 (defun SYM (PARAMS) BODY)."
  (when (> (length (nelisp-sys-ast-prop item :params)) 6)
    (signal 'nelisp-sys-backend-error
            (list (format "function %S has >6 params; MVP C-ABI codegen \
supports register args only" (nelisp-sys-ast-prop item :name))
                  :form (nelisp-sys-ast-prop item :form))))
  (list 'defun
        (nelisp-sys-backend--emit-symbol item)
        (mapcar #'car (nelisp-sys-ast-prop item :params))
        (nelisp-sys-backend--lower-body ctx (nelisp-sys-ast-prop item :body))))

(defun nelisp-sys-backend--build-ctx (module target)
  "Build a backend lowering context for MODULE targeting TARGET."
  (let ((cctx (nelisp-sys-check-build-ctx module))
        (names (make-hash-table :test 'eq)))
    (dolist (item (nelisp-sys-ast-prop module :items))
      (when (eq (nelisp-sys-ast-kind item) 'defun)
        (puthash (nelisp-sys-ast-prop item :name)
                 (nelisp-sys-backend--emit-symbol item) names)))
    (nelisp-sys-backend--ctx-make
     :structs (nelisp-sys-check-ctx-structs cctx)
     :target (nelisp-sys-target-get target)
     :names names)))

(defun nelisp-sys-backend-lower-module (module target)
  "Lower all defuns of MODULE to a Phase 47 program form for TARGET.
Returns a single `defun' form, or a `seq' of them for multiple defuns."
  (let* ((ctx (nelisp-sys-backend--build-ctx module target))
         (defuns (delq nil
                       (mapcar
                        (lambda (item)
                          (when (eq (nelisp-sys-ast-kind item) 'defun)
                            (nelisp-sys-backend--lower-defun ctx item)))
                        (nelisp-sys-ast-prop module :items)))))
    (when (null defuns)
      (signal 'nelisp-sys-backend-error (list "module has no functions to compile")))
    (if (cdr defuns) (cons 'seq defuns) (car defuns))))

(defun nelisp-sys-backend-emit-object (module output-path target)
  "Compile MODULE to a native object at OUTPUT-PATH for TARGET.
Lowers the typed AST to the Phase 47 integer subset, emits the object
through the adapter, and writes an ABI-summary sidecar at
OUTPUT-PATH.abi.  Returns OUTPUT-PATH."
  (let* ((tg (nelisp-sys-target-get target))
         (program (nelisp-sys-backend-lower-module module tg)))
    (nelisp-sys-adapter-compile-to-object program output-path tg)
    (with-temp-file (concat output-path ".abi")
      (insert (nelisp-sys-abi-meta-to-string
               (nelisp-sys-abi-meta-module module tg))
              "\n"))
    output-path))

(provide 'nelisp-sys-backend)

;;; nelisp-sys-backend.el ends here
