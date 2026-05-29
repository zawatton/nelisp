;;; nelisp-sys-backend.el --- Native codegen backend for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 130.3 / 132.3: the MVP native code generator.  It lowers the
;; validated typed AST into the plain integer-subset s-expression form the
;; existing Phase 47 compiler accepts, then drives object emission through
;; the adapter (the only module allowed to touch the private backend).
;;
;; Scope of the codegen: C-ABI integer/word functions — fixed-width
;; integer arithmetic, comparisons, logical ops, let/if/cond/while/seq,
;; set!, calls, compile-time sizeof/alignof/offsetof folding, (Stage
;; 130.4) struct-field + raw-pointer load/store through a pointer/ref
;; place, and slice-len/ref/set! over a {data,len} header -- all lowered to
;; the Phase 47 `ptr-read-uN'/`ptr-write-uN' primitives.
;; Still deferred (clear `nelisp-sys-backend-error', never a silent
;; miscompile): borrows, field/slice access through a non-var place, and
;; `sys:exit' (freestanding).  Signed scalar field/slice reads sign-extend
;; via the Phase 47 `ptr-read-sN' primitive; slice access has no runtime
;; bounds check yet.
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

(defvar nelisp-sys-backend--locals nil
  "Dynamic alist NAME -> TYPE of in-scope locals during lowering.
Bound per function from its params and extended inside `let'/`with-borrow'
so memory-op lowering can resolve a place's struct/pointee type.")

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
    (call-ptr (nelisp-sys-backend--lower-call-ptr ctx node))
    (addr-of
     (let ((nm (nelisp-sys-ast-prop node :name)))
       (list 'addr-of (or (gethash nm (nelisp-sys-backend-ctx-names ctx)) nm))))
    (atomic-add
     (let ((p (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :ptr)))
           (d (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :delta))))
       ;; fetch-sub = fetch-add of the negated delta.
       (list 'atomic-fetch-add p
             (if (eq (nelisp-sys-ast-prop node :op) 'sub) (list '- 0 d) d))))
    (atomic-cas
     (list 'atomic-compare-exchange
           (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :ptr))
           (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :expected))
           (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :new))))
    (exit (list 'exit (nelisp-sys-backend--lower
                       ctx (nelisp-sys-ast-prop node :arg))))
    (load-field (nelisp-sys-backend--lower-load-field ctx node))
    (store-field! (nelisp-sys-backend--lower-store-field ctx node))
    (ptr-load (nelisp-sys-backend--lower-ptr-load ctx node))
    (ptr-store! (nelisp-sys-backend--lower-ptr-store ctx node))
    (ptr-add (list '+
                   (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :ptr))
                   (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :offset))))
    (slice-len (nelisp-sys-backend--lower-slice-len ctx node))
    (slice-ref (nelisp-sys-backend--lower-slice-ref ctx node))
    (slice-set! (nelisp-sys-backend--lower-slice-set ctx node))
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
  "Lower a let NODE to (let ((NAME INIT)...) BODY).
Binding inits are lowered in the enclosing scope; the body is lowered
with locals extended by the new bindings so memory-op lowering can
resolve their types."
  (let* ((binds (nelisp-sys-ast-prop node :bindings))
         (emitted (mapcar (lambda (b)
                            (list (nth 0 b)
                                  (nelisp-sys-backend--lower ctx (nth 2 b))))
                          binds))
         (nelisp-sys-backend--locals
          (append (mapcar (lambda (b) (cons (nth 0 b) (nth 1 b))) binds)
                  nelisp-sys-backend--locals)))
    (list 'let emitted
          (nelisp-sys-backend--lower-body ctx (nelisp-sys-ast-prop node :body)))))

(defun nelisp-sys-backend--place-type (place-node)
  "Return the declared type of PLACE-NODE, or nil if not resolvable.
MVP: only a bare `var' place is resolved (against the dynamic locals)."
  (when (eq (nelisp-sys-ast-kind place-node) 'var)
    (cdr (assq (nelisp-sys-ast-prop place-node :name)
               nelisp-sys-backend--locals))))

(defun nelisp-sys-backend--pointee-struct (type)
  "Return struct name S when TYPE is (ptr S)/(& S)/(&mut S) with S named."
  (when (and type (or (nelisp-sys-type-pointer-p type)
                      (nelisp-sys-type-ref-p type)))
    (let ((el (nelisp-sys-type-element type)))
      (and (nelisp-sys-type-struct-ref-p el)
           (nelisp-sys-type-struct-name el)))))

(defun nelisp-sys-backend--mem-op (kind type ctx node)
  "Return the Phase 47 `ptr-KIND-{u,s}N' symbol for value TYPE.
N follows sizeof(TYPE) in {1,2,4,8}; other sizes are unsupported.  A read of
a signed integer scalar uses the sign-extending `ptr-read-sN'; writes and
unsigned/pointer/float reads use the `uN' form."
  (let* ((bytes (nelisp-sys-layout-sizeof
                 type (nelisp-sys-backend-ctx-target ctx)
                 (nelisp-sys-backend-ctx-structs ctx)))
         (bits (cl-case bytes (1 8) (2 16) (4 32) (8 64) (t nil)))
         (meta (nelisp-sys-type-scalar-meta type))
         (signed (and (string= kind "read")
                      (eq (plist-get meta :class) 'int)
                      (plist-get meta :signed))))
    (unless bits
      (nelisp-sys-backend--unsupported node "non-word-size memory access"))
    (intern (format "ptr-%s-%s%d" kind (if signed "s" "u") bits))))

(defun nelisp-sys-backend--lower-load-field (ctx node)
  "Lower (sys:load-field PLACE FIELD) to a ptr-read of the field width."
  (let* ((place (nelisp-sys-ast-prop node :place))
         (field (nelisp-sys-ast-prop node :field))
         (sname (nelisp-sys-backend--pointee-struct
                 (nelisp-sys-backend--place-type place))))
    (unless sname
      (nelisp-sys-backend--unsupported
       node "field load through a non-pointer-to-struct place"))
    (let ((ftype (nelisp-sys-types-struct-field-type
                  (nelisp-sys-backend-ctx-structs ctx) sname field)))
      (unless ftype
        (nelisp-sys-backend--unsupported node "load of unknown struct field"))
      (list (nelisp-sys-backend--mem-op "read" ftype ctx node)
            (nelisp-sys-backend--lower ctx place)
            (nelisp-sys-layout-offsetof sname field
                                        (nelisp-sys-backend-ctx-target ctx)
                                        (nelisp-sys-backend-ctx-structs ctx))))))

(defun nelisp-sys-backend--lower-store-field (ctx node)
  "Lower (sys:store-field! PLACE FIELD VAL) to a ptr-write of the field width."
  (let* ((place (nelisp-sys-ast-prop node :place))
         (field (nelisp-sys-ast-prop node :field))
         (sname (nelisp-sys-backend--pointee-struct
                 (nelisp-sys-backend--place-type place))))
    (unless sname
      (nelisp-sys-backend--unsupported
       node "field store through a non-pointer-to-struct place"))
    (let ((ftype (nelisp-sys-types-struct-field-type
                  (nelisp-sys-backend-ctx-structs ctx) sname field)))
      (unless ftype
        (nelisp-sys-backend--unsupported node "store to unknown struct field"))
      (list (nelisp-sys-backend--mem-op "write" ftype ctx node)
            (nelisp-sys-backend--lower ctx place)
            (nelisp-sys-layout-offsetof sname field
                                        (nelisp-sys-backend-ctx-target ctx)
                                        (nelisp-sys-backend-ctx-structs ctx))
            (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :value))))))

(defun nelisp-sys-backend--lower-ptr-load (ctx node)
  "Lower (sys:load PTR) to a ptr-read of the pointee width at offset 0."
  (let* ((ptr (nelisp-sys-ast-prop node :ptr))
         (pt (nelisp-sys-backend--place-type ptr)))
    (unless (and pt (nelisp-sys-type-pointer-p pt))
      (nelisp-sys-backend--unsupported node "raw load through a non-pointer var"))
    (list (nelisp-sys-backend--mem-op "read" (nelisp-sys-type-element pt) ctx node)
          (nelisp-sys-backend--lower ctx ptr)
          0)))

(defun nelisp-sys-backend--lower-ptr-store (ctx node)
  "Lower (sys:store! PTR VAL) to a ptr-write of the pointee width at offset 0."
  (let* ((ptr (nelisp-sys-ast-prop node :ptr))
         (pt (nelisp-sys-backend--place-type ptr)))
    (unless (and pt (nelisp-sys-type-pointer-p pt))
      (nelisp-sys-backend--unsupported node "raw store through a non-pointer var"))
    (list (nelisp-sys-backend--mem-op "write" (nelisp-sys-type-element pt) ctx node)
          (nelisp-sys-backend--lower ctx ptr)
          0
          (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :value)))))

(defun nelisp-sys-backend--slice-elem-type (node)
  "Resolve NODE's :slice operand to its element type T.
The slice place must be a bare slice-typed `var' (MVP); otherwise signal
`nelisp-sys-backend-error' rather than risk a silent miscompile."
  (let* ((slice (nelisp-sys-ast-prop node :slice))
         (sty (nelisp-sys-backend--place-type slice)))
    (unless (and sty (nelisp-sys-type-slice-p sty))
      (nelisp-sys-backend--unsupported node "slice op through a non-slice var place"))
    (nelisp-sys-type-element sty)))

(defun nelisp-sys-backend--slice-data-ptr (ctx node elem)
  "Lower a read of the data pointer (header offset 0) of NODE's slice.
A slice value is a pointer to a 2-word {data:(ptr ELEM), len:usize} header;
the data field is word-sized at offset 0."
  (list (nelisp-sys-backend--mem-op "read" (list 'ptr elem) ctx node)
        (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :slice))
        0))

(defun nelisp-sys-backend--lower-slice-len (ctx node)
  "Lower (sys:slice-len S) to a read of the len word from the slice header.
The len field lives at offset = pointer size in the {data,len} header."
  (nelisp-sys-backend--slice-elem-type node) ; validate S is a slice var
  (list (nelisp-sys-backend--mem-op "read" 'usize ctx node)
        (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :slice))
        (/ (nelisp-sys-target-pointer-width
            (nelisp-sys-backend-ctx-target ctx))
           8)))

(defun nelisp-sys-backend--lower-slice-ref (ctx node)
  "Lower (sys:slice-ref S I) to a read of element I from slice S.
Loads the data pointer (header offset 0), adds I*sizeof(T), then reads the
element width.  MVP: no runtime bounds check (documented gap); the checked
and `-raw' forms lower identically.  Signed elements sign-extend through the
element read op (`ptr-read-sN')."
  (let* ((elem (nelisp-sys-backend--slice-elem-type node))
         (esize (nelisp-sys-layout-sizeof
                 elem (nelisp-sys-backend-ctx-target ctx)
                 (nelisp-sys-backend-ctx-structs ctx)))
         (idx (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :index))))
    (list (nelisp-sys-backend--mem-op "read" elem ctx node)
          (list '+ (nelisp-sys-backend--slice-data-ptr ctx node elem)
                (list '* idx esize))
          0)))

(defun nelisp-sys-backend--lower-slice-set (ctx node)
  "Lower (sys:slice-set! S I V) to a write of V into element I of slice S.
Same addressing as `nelisp-sys-backend--lower-slice-ref'."
  (let* ((elem (nelisp-sys-backend--slice-elem-type node))
         (esize (nelisp-sys-layout-sizeof
                 elem (nelisp-sys-backend-ctx-target ctx)
                 (nelisp-sys-backend-ctx-structs ctx)))
         (idx (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :index)))
         (val (nelisp-sys-backend--lower ctx (nelisp-sys-ast-prop node :value))))
    (list (nelisp-sys-backend--mem-op "write" elem ctx node)
          (list '+ (nelisp-sys-backend--slice-data-ptr ctx node elem)
                (list '* idx esize))
          0
          val)))

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

(defun nelisp-sys-backend--lower-call-ptr (ctx node)
  "Lower (sys:call-ptr FN ARG...) to the Phase 47 (call-ptr FN ARG...)
form (Doc 133 Phase 0).  FN lowers to the code-address expression; the
Phase 47 backend emits the indirect CALL through a scratch register."
  (cons 'call-ptr
        (cons (nelisp-sys-backend--lower
               ctx (nelisp-sys-ast-prop node :fn-expr))
              (mapcar (lambda (a) (nelisp-sys-backend--lower ctx a))
                      (nelisp-sys-ast-prop node :args)))))

(defun nelisp-sys-backend--lower-defun (ctx item)
  "Lower defun ITEM to a plain Phase 47 (defun SYM (PARAMS) BODY)."
  (when (> (length (nelisp-sys-ast-prop item :params)) 6)
    (signal 'nelisp-sys-backend-error
            (list (format "function %S has >6 params; MVP C-ABI codegen \
supports register args only" (nelisp-sys-ast-prop item :name))
                  :form (nelisp-sys-ast-prop item :form))))
  (let ((nelisp-sys-backend--locals
         (mapcar (lambda (p) (cons (car p) (cdr p)))
                 (nelisp-sys-ast-prop item :params))))
    (list 'defun
          (nelisp-sys-backend--emit-symbol item)
          (mapcar #'car (nelisp-sys-ast-prop item :params))
          (nelisp-sys-backend--lower-body ctx (nelisp-sys-ast-prop item :body)))))

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
