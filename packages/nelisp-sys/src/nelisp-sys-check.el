;;; nelisp-sys-check.el --- Type checker for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 131.2: the type checker MVP.  Walks the typed AST produced by the
;; frontend and checks scalar/pointer/slice/struct/function-call typing
;; with stable diagnostic codes (E-SYS-TYPE-NNN).  Types are explicit
;; (Doc 131): no width inference beyond integer literals adopting the type
;; demanded by their context (defaulting to i32 when unconstrained).
;;
;; Ownership/borrow/effect checking lives in later stages; this stage only
;; establishes that values have consistent, resolvable types.
;;
;; Errors carry a stable CODE so callers and tests can assert on them:
;;   (nelisp-sys-check-error CODE MESSAGE :form FORM)

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)
(require 'nelisp-sys-types)
(require 'nelisp-sys-ast)

(define-error 'nelisp-sys-check-error
  "nelisp-sys type check error" 'nelisp-sys-error)

(defun nelisp-sys-check--fail (code form fmt &rest args)
  "Signal a check error with stable CODE for FORM and message FMT/ARGS."
  (signal 'nelisp-sys-check-error
          (list code (apply #'format fmt args) :form form)))

(defun nelisp-sys-check-error-code (err)
  "Return the stable diagnostic code from a `nelisp-sys-check-error' ERR."
  (nth 1 err))

(defun nelisp-sys-check-error-message (err)
  "Return the message string from a `nelisp-sys-check-error' ERR."
  (nth 2 err))

;;; Context.

(cl-defstruct (nelisp-sys-check-ctx (:constructor nelisp-sys-check--ctx-make))
  structs   ; struct env (hash)
  funcs)    ; hash NAME -> (:params (TYPE...) :ret TYPE :kind defun|extern)

(defun nelisp-sys-check--lookup-local (locals name)
  "Return the type bound to NAME in LOCALS alist, or nil."
  (cdr (assq name locals)))

;;; Type resolution.

(defun nelisp-sys-check--resolve-type (ctx type form)
  "Ensure every named/struct reference in TYPE exists in CTX's struct env.
Signal E-SYS-TYPE-003 otherwise.  Returns TYPE."
  (cond
   ((nelisp-sys-type-scalar-p type) type)
   ((nelisp-sys-type-struct-ref-p type)
    (let ((name (nelisp-sys-type-struct-name type)))
      (unless (nelisp-sys-types-env-get (nelisp-sys-check-ctx-structs ctx) name)
        (nelisp-sys-check--fail 'E-SYS-TYPE-003 form
                                "unknown type: %S" name))
      type))
   ((or (nelisp-sys-type-pointer-p type)
        (nelisp-sys-type-slice-p type)
        (nelisp-sys-type-owned-p type)
        (nelisp-sys-type-ref-p type)
        (nelisp-sys-type-array-p type))
    (let ((el (nelisp-sys-type-element type)))
      (unless (nelisp-sys-type-void-p el)
        (nelisp-sys-check--resolve-type ctx el form)))
    type)
   (t type)))

(defun nelisp-sys-check--struct-of-place (type)
  "Return the struct name for a field-access place TYPE, or nil.
Auto-derefs one level of (ptr S)/(& S)/(&mut S); also accepts bare S."
  (cond
   ((nelisp-sys-type-struct-ref-p type) (nelisp-sys-type-struct-name type))
   ((or (nelisp-sys-type-pointer-p type) (nelisp-sys-type-ref-p type))
    (let ((el (nelisp-sys-type-element type)))
      (and (nelisp-sys-type-struct-ref-p el)
           (nelisp-sys-type-struct-name el))))
   (t nil)))

;;; Expression checking.

(defun nelisp-sys-check--expr (ctx locals node expected)
  "Check NODE under CTX/LOCALS and return its type.
EXPECTED, when non-nil, is the type demanded by context; a mismatch
signals E-SYS-TYPE-002 (integer literals adopt EXPECTED first)."
  (let ((ty (nelisp-sys-check--infer ctx locals node expected)))
    (when (and expected
               (not (nelisp-sys-type-void-p expected))
               (not (nelisp-sys-type-equal ty expected)))
      (nelisp-sys-check--fail 'E-SYS-TYPE-002 (nelisp-sys-ast-prop node :form)
                              "type mismatch: expected %S, got %S" expected ty))
    ty))

(defun nelisp-sys-check--operand-type (ctx locals args expected)
  "Pick the integer/scalar operand type for a binop over ARGS.
Prefer EXPECTED when it is a scalar; else the type of the first
non-literal argument; else i32."
  (cond
   ((and expected (nelisp-sys-type-scalar-p expected)
         (not (nelisp-sys-type-bool-p expected))) expected)
   (t (let ((nonlit (cl-find-if
                     (lambda (a) (not (eq (nelisp-sys-ast-kind a) 'int)))
                     args)))
        (if nonlit (nelisp-sys-check--infer ctx locals nonlit nil) 'i32)))))

(defun nelisp-sys-check--infer (ctx locals node expected)
  "Infer and return the type of NODE under CTX/LOCALS (EXPECTED is a hint)."
  (let ((kind (nelisp-sys-ast-kind node))
        (form (nelisp-sys-ast-prop node :form)))
    (cl-case kind
      (int (if (and expected (nelisp-sys-type-integer-p expected))
               expected 'i32))
      (bool 'bool)
      (var
       (let* ((name (nelisp-sys-ast-prop node :name))
              (ty (nelisp-sys-check--lookup-local locals name)))
         (unless ty
           (nelisp-sys-check--fail 'E-SYS-TYPE-001 form
                                   "unknown variable: %S" name))
         ty))
      (binop (nelisp-sys-check--infer-binop ctx locals node expected))
      (not
       (nelisp-sys-check--expr ctx locals (nelisp-sys-ast-prop node :arg) 'bool)
       'bool)
      (cast
       (let ((target (nelisp-sys-ast-prop node :type)))
         (nelisp-sys-check--resolve-type ctx target form)
         (nelisp-sys-check--expr ctx locals (nelisp-sys-ast-prop node :arg) nil)
         target))
      (let (nelisp-sys-check--infer-let ctx locals node))
      (seq (nelisp-sys-check--infer-body
            ctx locals (nelisp-sys-ast-prop node :body)))
      (if (nelisp-sys-check--infer-if ctx locals node expected))
      (cond (nelisp-sys-check--infer-cond ctx locals node expected))
      (while
       (nelisp-sys-check--expr ctx locals (nelisp-sys-ast-prop node :cond) 'bool)
       (nelisp-sys-check--infer-body ctx locals (nelisp-sys-ast-prop node :body))
       'void)
      (set!
       (let* ((name (nelisp-sys-ast-prop node :name))
              (ty (nelisp-sys-check--lookup-local locals name)))
         (unless ty
           (nelisp-sys-check--fail 'E-SYS-TYPE-001 form
                                   "assignment to unknown variable: %S" name))
         (nelisp-sys-check--expr ctx locals (nelisp-sys-ast-prop node :value) ty)
         'void))
      (load-field (nelisp-sys-check--infer-field ctx locals node nil))
      (store-field! (nelisp-sys-check--infer-field ctx locals node t))
      (slice-len
       (nelisp-sys-check--slice-elem ctx locals
                                     (nelisp-sys-ast-prop node :slice) form)
       'usize)
      (slice-ref
       (let ((el (nelisp-sys-check--slice-elem
                  ctx locals (nelisp-sys-ast-prop node :slice) form)))
         (nelisp-sys-check--expect-int ctx locals
                                       (nelisp-sys-ast-prop node :index) form)
         el))
      (slice-set!
       (let ((el (nelisp-sys-check--slice-elem
                  ctx locals (nelisp-sys-ast-prop node :slice) form t)))
         (nelisp-sys-check--expect-int ctx locals
                                       (nelisp-sys-ast-prop node :index) form)
         (nelisp-sys-check--expr ctx locals (nelisp-sys-ast-prop node :value) el)
         'void))
      (ptr-load
       (nelisp-sys-type-element
        (nelisp-sys-check--expect-ptr ctx locals
                                      (nelisp-sys-ast-prop node :ptr) form)))
      (ptr-store!
       (let ((pt (nelisp-sys-check--expect-ptr
                  ctx locals (nelisp-sys-ast-prop node :ptr) form)))
         (nelisp-sys-check--expr ctx locals (nelisp-sys-ast-prop node :value)
                                 (nelisp-sys-type-element pt))
         'void))
      (ptr-add
       (let ((pt (nelisp-sys-check--expect-ptr
                  ctx locals (nelisp-sys-ast-prop node :ptr) form)))
         (nelisp-sys-check--expect-int ctx locals
                                       (nelisp-sys-ast-prop node :offset) form)
         pt))
      (sizeof
       (nelisp-sys-check--resolve-type ctx (nelisp-sys-ast-prop node :type) form)
       'usize)
      (alignof
       (nelisp-sys-check--resolve-type ctx (nelisp-sys-ast-prop node :type) form)
       'usize)
      (offsetof
       (nelisp-sys-check--check-field-exists
        ctx (nelisp-sys-ast-prop node :struct) (nelisp-sys-ast-prop node :field)
        form)
       'usize)
      (exit
       (nelisp-sys-check--expect-int ctx locals
                                     (nelisp-sys-ast-prop node :arg) form)
       'void)
      (unsafe (nelisp-sys-check--infer-body
               ctx locals (nelisp-sys-ast-prop node :body)))
      (with-borrow (nelisp-sys-check--infer-with-borrow ctx locals node))
      (resource-op
       (let ((at (nelisp-sys-check--expr ctx locals
                                         (nelisp-sys-ast-prop node :arg) nil)))
         (if (eq (nelisp-sys-ast-prop node :op) 'sys:dup) at 'void)))
      (call (nelisp-sys-check--infer-call ctx locals node))
      (t (nelisp-sys-check--fail 'E-SYS-TYPE-099 form
                                 "cannot type-check node kind: %S" kind)))))

(defun nelisp-sys-check--expect-int (ctx locals node form)
  "Check NODE is an integer-typed expression; signal E-SYS-TYPE-013 if not."
  (let ((ty (nelisp-sys-check--expr ctx locals node nil)))
    (unless (nelisp-sys-type-integer-p ty)
      (nelisp-sys-check--fail 'E-SYS-TYPE-013 form
                              "expected integer, got %S" ty))
    ty))

(defun nelisp-sys-check--expect-ptr (ctx locals node form)
  "Check NODE is pointer-typed; signal E-SYS-TYPE-014 if not.  Return its type."
  (let ((ty (nelisp-sys-check--expr ctx locals node nil)))
    (unless (nelisp-sys-type-pointer-p ty)
      (nelisp-sys-check--fail 'E-SYS-TYPE-014 form
                              "expected pointer, got %S" ty))
    ty))

(defun nelisp-sys-check--slice-elem (ctx locals node form &optional need-mut)
  "Check NODE is a slice; return its element type.  E-SYS-TYPE-012 if not.
With NEED-MUT, require a (slice-mut T)."
  (let ((ty (nelisp-sys-check--expr ctx locals node nil)))
    (unless (nelisp-sys-type-slice-p ty)
      (nelisp-sys-check--fail 'E-SYS-TYPE-012 form
                              "expected slice, got %S" ty))
    (when (and need-mut (not (nelisp-sys-type-slice-mut-p ty)))
      (nelisp-sys-check--fail 'E-SYS-TYPE-012 form
                              "expected mutable slice, got %S" ty))
    (nelisp-sys-type-element ty)))

(defun nelisp-sys-check--infer-binop (ctx locals node expected)
  "Infer the type of a binop NODE."
  (let ((class (nelisp-sys-ast-prop node :class))
        (args (nelisp-sys-ast-prop node :args))
        (form (nelisp-sys-ast-prop node :form)))
    (cl-case class
      (logic
       (dolist (a args) (nelisp-sys-check--expr ctx locals a 'bool))
       'bool)
      (cmp
       (let ((opnd (nelisp-sys-check--operand-type ctx locals args nil)))
         (dolist (a args) (nelisp-sys-check--expr ctx locals a opnd))
         'bool))
      (arith
       (let ((opnd (nelisp-sys-check--operand-type ctx locals args expected)))
         (unless (or (nelisp-sys-type-integer-p opnd)
                     (nelisp-sys-type-float-p opnd))
           (nelisp-sys-check--fail 'E-SYS-TYPE-008 form
                                   "arithmetic on non-numeric type %S" opnd))
         (dolist (a args) (nelisp-sys-check--expr ctx locals a opnd))
         opnd))
      (t (nelisp-sys-check--fail 'E-SYS-TYPE-099 form
                                 "unknown binop class %S" class)))))

(defun nelisp-sys-check--infer-let (ctx locals node)
  "Infer the type of a let NODE, extending LOCALS for the body."
  (let ((locals* locals))
    (dolist (b (nelisp-sys-ast-prop node :bindings))
      (let ((name (nth 0 b)) (ty (nth 1 b)) (init (nth 2 b)))
        (nelisp-sys-check--resolve-type ctx ty (nelisp-sys-ast-prop node :form))
        (nelisp-sys-check--expr ctx locals* init ty)
        (setq locals* (cons (cons name ty) locals*))))
    (nelisp-sys-check--infer-body ctx locals* (nelisp-sys-ast-prop node :body))))

(defun nelisp-sys-check--infer-body (ctx locals body)
  "Check each node in BODY; return the type of the last (or void if empty)."
  (let ((ty 'void))
    (dolist (n body) (setq ty (nelisp-sys-check--expr ctx locals n nil)))
    ty))

(defun nelisp-sys-check--infer-if (ctx locals node expected)
  "Infer the type of an if NODE."
  (nelisp-sys-check--expr ctx locals (nelisp-sys-ast-prop node :cond) 'bool)
  (let* ((then-ty (nelisp-sys-check--expr ctx locals
                                          (nelisp-sys-ast-prop node :then)
                                          expected))
         (else (nelisp-sys-ast-prop node :else)))
    (if else
        (nelisp-sys-check--expr ctx locals else then-ty)
      'void)))

(defun nelisp-sys-check--infer-cond (ctx locals node expected)
  "Infer the type of a cond NODE (clause bodies must agree, else void)."
  (let ((result nil))
    (dolist (clause (nelisp-sys-ast-prop node :clauses))
      (unless (eq (car clause) 'else)
        (nelisp-sys-check--expr ctx locals (car clause) 'bool))
      (let ((bt (nelisp-sys-check--infer-body ctx locals (cdr clause))))
        (setq result (or result bt))))
    (or result expected 'void)))

(defun nelisp-sys-check--infer-field (ctx locals node store)
  "Infer a load/store-field NODE.  With STORE, also check the value type."
  (let* ((form (nelisp-sys-ast-prop node :form))
         (place-ty (nelisp-sys-check--expr ctx locals
                                           (nelisp-sys-ast-prop node :place) nil))
         (sname (nelisp-sys-check--struct-of-place place-ty))
         (field (nelisp-sys-ast-prop node :field)))
    (unless sname
      (nelisp-sys-check--fail 'E-SYS-TYPE-007 form
                              "field access on non-struct type %S" place-ty))
    (let ((ftype (nelisp-sys-types-struct-field-type
                  (nelisp-sys-check-ctx-structs ctx) sname field)))
      (unless ftype
        (nelisp-sys-check--fail 'E-SYS-TYPE-004 form
                                "struct %S has no field %S" sname field))
      (when store
        (nelisp-sys-check--expr ctx locals (nelisp-sys-ast-prop node :value) ftype))
      (if store 'void ftype))))

(defun nelisp-sys-check--check-field-exists (ctx struct field form)
  "Signal unless STRUCT exists in CTX and has FIELD."
  (let ((sname (nelisp-sys-type-struct-name struct)))
    (setq sname (or sname struct))
    (unless (nelisp-sys-types-env-get (nelisp-sys-check-ctx-structs ctx) sname)
      (nelisp-sys-check--fail 'E-SYS-TYPE-003 form "unknown struct: %S" sname))
    (unless (nelisp-sys-types-struct-field-type
             (nelisp-sys-check-ctx-structs ctx) sname field)
      (nelisp-sys-check--fail 'E-SYS-TYPE-004 form
                              "struct %S has no field %S" sname field))))

(defun nelisp-sys-check--infer-with-borrow (ctx locals node)
  "Infer a with-borrow NODE: bind the borrow var, then check the body."
  (let* ((form (nelisp-sys-ast-prop node :form))
         (ref-type (nelisp-sys-ast-prop node :ref-type))
         (var (nelisp-sys-ast-prop node :var)))
    (nelisp-sys-check--resolve-type ctx ref-type form)
    (nelisp-sys-check--expr ctx locals (nelisp-sys-ast-prop node :place) nil)
    (nelisp-sys-check--infer-body
     ctx (cons (cons var ref-type) locals) (nelisp-sys-ast-prop node :body))))

(defun nelisp-sys-check--infer-call (ctx locals node)
  "Infer a call NODE against the function signature table."
  (let* ((form (nelisp-sys-ast-prop node :form))
         (fn (nelisp-sys-ast-prop node :fn))
         (args (nelisp-sys-ast-prop node :args))
         (sig (gethash fn (nelisp-sys-check-ctx-funcs ctx))))
    (unless sig
      (nelisp-sys-check--fail 'E-SYS-TYPE-006 form "unknown function: %S" fn))
    (let ((ptypes (plist-get sig :params)))
      (unless (= (length args) (length ptypes))
        (nelisp-sys-check--fail 'E-SYS-TYPE-005 form
                                "call to %S expects %d args, got %d"
                                fn (length ptypes) (length args)))
      (cl-loop for a in args for pt in ptypes
               do (nelisp-sys-check--expr ctx locals a pt))
      (plist-get sig :ret))))

;;; Module checking.

(defun nelisp-sys-check--build-ctx (module)
  "Build a check context (struct env + function table) from MODULE node."
  (let ((structs (nelisp-sys-types-env-make))
        (funcs (make-hash-table :test 'eq)))
    (dolist (item (nelisp-sys-ast-prop module :items))
      (cl-case (nelisp-sys-ast-kind item)
        (defstruct
         (nelisp-sys-types-env-add structs
                                   (nelisp-sys-ast-prop item :name)
                                   (nelisp-sys-ast-prop item :repr)
                                   (nelisp-sys-ast-prop item :fields)))
        ((defun extern)
         (let ((name (nelisp-sys-ast-prop item :name)))
           (when (gethash name funcs)
             (nelisp-sys-check--fail 'E-SYS-TYPE-015
                                     (nelisp-sys-ast-prop item :form)
                                     "duplicate function definition: %S" name))
           (puthash name
                    (list :params (mapcar #'cdr (nelisp-sys-ast-prop item :params))
                          :ret (nelisp-sys-ast-prop item :ret)
                          :effects (nelisp-sys-ast-prop item :effects)
                          :unsafe (nelisp-sys-ast-prop item :unsafe)
                          :kind (nelisp-sys-ast-kind item))
                    funcs)))))
    (nelisp-sys-check--ctx-make :structs structs :funcs funcs)))

(defun nelisp-sys-check-build-ctx (module)
  "Public: build the check context (struct env + function table) from MODULE.
Used by the safety passes (ownership/borrow/unsafe/effect) so they share
one resolved view of structs and function signatures."
  (nelisp-sys-check--build-ctx module))

(defun nelisp-sys-check--check-defun (ctx item)
  "Type-check one defun ITEM under CTX."
  (let ((form (nelisp-sys-ast-prop item :form))
        (params (nelisp-sys-ast-prop item :params))
        (ret (nelisp-sys-ast-prop item :ret))
        (locals '()))
    (dolist (p params)
      (nelisp-sys-check--resolve-type ctx (cdr p) form)
      (setq locals (cons (cons (car p) (cdr p)) locals)))
    (nelisp-sys-check--resolve-type ctx ret form)
    (let ((body-ty (nelisp-sys-check--infer-body
                    ctx locals (nelisp-sys-ast-prop item :body))))
      (unless (or (nelisp-sys-type-void-p ret)
                  (nelisp-sys-type-equal body-ty ret))
        (nelisp-sys-check--fail 'E-SYS-TYPE-011 form
                                "function %S returns %S but body has type %S"
                                (nelisp-sys-ast-prop item :name) ret body-ty)))))

(defun nelisp-sys-check-module (module)
  "Type-check MODULE (an AST module node).
Return the check context on success; signal `nelisp-sys-check-error'
with a stable code on the first type error."
  (let ((ctx (nelisp-sys-check--build-ctx module)))
    ;; Validate struct field types resolve.
    (maphash
     (lambda (_name def)
       (dolist (f (plist-get def :fields))
         (nelisp-sys-check--resolve-type ctx (cdr f) nil)))
     (nelisp-sys-check-ctx-structs ctx))
    (dolist (item (nelisp-sys-ast-prop module :items))
      (when (eq (nelisp-sys-ast-kind item) 'defun)
        (nelisp-sys-check--check-defun ctx item)))
    ctx))

(defun nelisp-sys-check-collect (module)
  "Type-check MODULE, returning a list of diagnostics instead of signaling.
Each diagnostic is (CODE MESSAGE FORM).  Returns nil when MODULE checks
clean.  Stops at the first error (MVP: no error recovery)."
  (condition-case err
      (progn (nelisp-sys-check-module module) nil)
    (nelisp-sys-check-error
     (list (list (nelisp-sys-check-error-code err)
                 (nelisp-sys-check-error-message err)
                 (plist-get (nthcdr 3 err) :form))))))

(provide 'nelisp-sys-check)

;;; nelisp-sys-check.el ends here
