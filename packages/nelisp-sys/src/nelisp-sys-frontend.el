;;; nelisp-sys-frontend.el --- Source frontend/parser for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 131.1: the source frontend.  Parses `sys:' source forms into the
;; typed AST defined by `nelisp-sys-ast'.  This stage is structural only:
;; it builds nodes, validates the shape of each form, normalizes function
;; effects, and validates type expressions structurally.  Type/ownership/
;; borrow/effect *checking* happens in later stages over this AST.
;;
;; Top-level forms:
;;   (sys:defstruct NAME (:repr c|sys) (FIELD TYPE)...)
;;   (sys:defun NAME ((ARG TYPE)...) RET (ATTRS...) BODY...)
;;   (sys:extern NAME (:symbol S :abi c :unsafe t) ((ARG TYPE)...) RET (ATTRS...))
;;   (sys:const NAME TYPE EXPR)
;;   (sys:static-assert EXPR "message")
;;
;; Uses only `cl-lib' and core special forms (no `pcase') so the package
;; stays portable regardless of host `pcase' shadowing on the load path.

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)
(require 'nelisp-sys-types)
(require 'nelisp-sys-ast)

(define-error 'nelisp-sys-parse-error
  "nelisp-sys parse error" 'nelisp-sys-error)

(defun nelisp-sys-frontend--err (form fmt &rest args)
  "Signal a parse error for FORM with message FMT/ARGS."
  (signal 'nelisp-sys-parse-error
          (list (apply #'format fmt args) :form form)))

;;; Effects.

(defconst nelisp-sys-frontend--effect-defaults
  '(:alloc heap :panic abort :unsafe none :syscall none :ffi none)
  "Default function effects when unspecified (Doc 131).")

(defun nelisp-sys-frontend--parse-effects (attrs form)
  "Parse function ATTRS plist into (EFFECTS ABI LINKAGE EXPORT-NAME).
ATTRS is the flat plist following RET in a defun/extern form."
  (let ((effects (copy-sequence nelisp-sys-frontend--effect-defaults))
        (abi 'c) (linkage :private) (export-name nil)
        (rest attrs))
    (while rest
      (let ((k (car rest)) (v (cadr rest)))
        (unless (keywordp k)
          (nelisp-sys-frontend--err form "expected attribute keyword, got %S" k))
        (cond
         ((eq k :abi) (setq abi v))
         ((eq k :export) (setq linkage :export export-name v))
         ((eq k :import) (setq linkage :import export-name v))
         ((eq k :private) (setq linkage :private))
         ((memq k '(:alloc :panic :unsafe :syscall :ffi))
          (setq effects (plist-put effects k v)))
         (t (nelisp-sys-frontend--err form "unknown attribute %S" k)))
        (setq rest (cddr rest))))
    (list effects abi linkage export-name)))

;;; Types.

(defun nelisp-sys-frontend--parse-type (texpr form)
  "Validate TEXPR structurally as a type and return it canonical.
FORM is the enclosing source form for diagnostics."
  (unless (nelisp-sys-type-valid-p texpr)
    (nelisp-sys-frontend--err form "invalid type: %S" texpr))
  texpr)

;;; Top-level dispatch.

(defun nelisp-sys-frontend-parse-module (forms)
  "Parse a list of top-level FORMS into a module AST node."
  (nelisp-sys-ast-make
   'module
   :items (mapcar #'nelisp-sys-frontend-parse-toplevel forms)))

(defun nelisp-sys-frontend-parse-toplevel (form)
  "Parse a single top-level FORM into an AST node."
  (unless (consp form)
    (nelisp-sys-frontend--err form "expected a top-level form, got %S" form))
  (cl-case (car form)
    (sys:defstruct (nelisp-sys-frontend--parse-defstruct form))
    (sys:defun     (nelisp-sys-frontend--parse-defun form))
    (sys:extern    (nelisp-sys-frontend--parse-extern form))
    (sys:const     (nelisp-sys-frontend--parse-const form))
    (sys:static-assert (nelisp-sys-frontend--parse-static-assert form))
    (t (nelisp-sys-frontend--err form "unknown top-level form: %S" (car form)))))

(defun nelisp-sys-frontend--parse-defstruct (form)
  "Parse (sys:defstruct NAME (:repr R) (FIELD TYPE)...) into a node."
  (let ((name (nth 1 form))
        (rest (nthcdr 2 form))
        (repr 'c) (fields '()))
    (unless (symbolp name)
      (nelisp-sys-frontend--err form "struct name must be a symbol: %S" name))
    (dolist (clause rest)
      (cond
       ((and (consp clause) (eq (car clause) :repr))
        (setq repr (cadr clause)))
       ((and (consp clause) (symbolp (car clause)))
        (push (cons (car clause)
                    (nelisp-sys-frontend--parse-type (cadr clause) form))
              fields))
       (t (nelisp-sys-frontend--err form "bad struct clause: %S" clause))))
    (unless (memq repr '(c sys))
      (nelisp-sys-frontend--err form "unsupported :repr %S (MVP: c|sys)" repr))
    (nelisp-sys-ast-make 'defstruct
                         :name name :repr repr
                         :fields (nreverse fields)
                         :form form)))

(defun nelisp-sys-frontend--parse-params (params form)
  "Parse a defun PARAMS list ((ARG TYPE)...) into ((NAME . TYPE)...)."
  (mapcar
   (lambda (p)
     (unless (and (consp p) (symbolp (car p)))
       (nelisp-sys-frontend--err form "bad parameter: %S" p))
     (cons (car p) (nelisp-sys-frontend--parse-type (cadr p) form)))
   params))

(defun nelisp-sys-frontend--parse-defun (form)
  "Parse (sys:defun NAME (PARAMS) RET (ATTRS...) BODY...) into a node."
  (let ((name (nth 1 form))
        (params (nth 2 form))
        (ret (nth 3 form))
        (attrs (nth 4 form))
        (body (nthcdr 5 form)))
    (unless (symbolp name)
      (nelisp-sys-frontend--err form "function name must be a symbol: %S" name))
    (unless (listp params)
      (nelisp-sys-frontend--err form "parameter list must be a list: %S" params))
    (unless (listp attrs)
      (nelisp-sys-frontend--err form "attribute list must be a list: %S" attrs))
    (let* ((parsed (nelisp-sys-frontend--parse-effects attrs form))
           (effects (nth 0 parsed))
           (abi (nth 1 parsed))
           (linkage (nth 2 parsed))
           (export-name (nth 3 parsed)))
      (nelisp-sys-ast-make
       'defun
       :name name
       :params (nelisp-sys-frontend--parse-params params form)
       :ret (nelisp-sys-frontend--parse-type ret form)
       :effects effects :abi abi :linkage linkage :export-name export-name
       :body (nelisp-sys-frontend--parse-body body form)
       :form form))))

(defun nelisp-sys-frontend--parse-extern (form)
  "Parse (sys:extern NAME (:symbol S ...) (PARAMS) RET (ATTRS...))."
  (let* ((name (nth 1 form))
         (decl (nth 2 form))
         (params (nth 3 form))
         (ret (nth 4 form))
         (attrs (nth 5 form))
         (symbol (plist-get decl :symbol))
         (abi (or (plist-get decl :abi) 'c))
         (unsafe (and (plist-get decl :unsafe) t))
         (effects (nth 0 (nelisp-sys-frontend--parse-effects (or attrs '()) form))))
    (unless (symbolp name)
      (nelisp-sys-frontend--err form "extern name must be a symbol: %S" name))
    (nelisp-sys-ast-make
     'extern
     :name name :symbol symbol :abi abi :unsafe unsafe
     :params (nelisp-sys-frontend--parse-params params form)
     :ret (nelisp-sys-frontend--parse-type ret form)
     :effects effects
     :form form)))

(defun nelisp-sys-frontend--parse-const (form)
  "Parse (sys:const NAME TYPE EXPR) into a node."
  (let ((name (nth 1 form))
        (type (nth 2 form))
        (expr (nth 3 form)))
    (unless (symbolp name)
      (nelisp-sys-frontend--err form "const name must be a symbol: %S" name))
    (nelisp-sys-ast-make 'const
                         :name name
                         :type (nelisp-sys-frontend--parse-type type form)
                         :value (nelisp-sys-frontend--parse-expr expr)
                         :form form)))

(defun nelisp-sys-frontend--parse-static-assert (form)
  "Parse (sys:static-assert EXPR \"msg\") into a node."
  (nelisp-sys-ast-make 'static-assert
                       :expr (nelisp-sys-frontend--parse-expr (nth 1 form))
                       :message (nth 2 form)
                       :form form))

;;; Body / expressions.

(defconst nelisp-sys-frontend--arith-ops '(+ - * / mod)
  "Arithmetic operator heads.")
(defconst nelisp-sys-frontend--cmp-ops '(< <= > >= = /=)
  "Comparison operator heads.")
(defconst nelisp-sys-frontend--logic-ops '(and or)
  "Short-circuit logical operator heads.")

(defun nelisp-sys-frontend--parse-body (forms _form)
  "Parse a BODY (list of FORMS) into a list of expression nodes."
  (mapcar #'nelisp-sys-frontend--parse-expr forms))

(defun nelisp-sys-frontend--parse-expr (form)
  "Parse a single expression FORM into an AST node."
  (cond
   ((integerp form) (nelisp-sys-ast-make 'int :value form))
   ((eq form 'true) (nelisp-sys-ast-make 'bool :value t))
   ((eq form 'false) (nelisp-sys-ast-make 'bool :value nil))
   ((symbolp form) (nelisp-sys-ast-make 'var :name form))
   ((stringp form)
    (signal 'nelisp-sys-parse-error
            (list "string literals are not supported in MVP" :form form)))
   ((consp form) (nelisp-sys-frontend--parse-compound form))
   (t (nelisp-sys-frontend--err form "cannot parse expression: %S" form))))

(defun nelisp-sys-frontend--parse-compound (form)
  "Parse a compound (list) expression FORM."
  (let ((head (car form)) (args (cdr form)))
    (cond
     ((memq head nelisp-sys-frontend--arith-ops)
      (nelisp-sys-ast-make 'binop :op head :class 'arith
                           :args (mapcar #'nelisp-sys-frontend--parse-expr args)
                           :form form))
     ((memq head nelisp-sys-frontend--cmp-ops)
      (nelisp-sys-ast-make 'binop :op head :class 'cmp
                           :args (mapcar #'nelisp-sys-frontend--parse-expr args)
                           :form form))
     ((memq head nelisp-sys-frontend--logic-ops)
      (nelisp-sys-ast-make 'binop :op head :class 'logic
                           :args (mapcar #'nelisp-sys-frontend--parse-expr args)
                           :form form))
     ((eq head 'not)
      (nelisp-sys-ast-make 'not :arg (nelisp-sys-frontend--parse-expr (car args))
                           :form form))
     ((eq head 'let) (nelisp-sys-frontend--parse-let form))
     ((eq head 'if)
      (nelisp-sys-ast-make 'if
                           :cond (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :then (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :else (and (> (length args) 2)
                                      (if (= (length args) 3)
                                          (nelisp-sys-frontend--parse-expr
                                           (nth 2 args))
                                        (nelisp-sys-ast-make
                                         'seq
                                         :body (nelisp-sys-frontend--parse-body
                                                (nthcdr 2 args) form)
                                         :form form)))
                           :form form))
     ((eq head 'cond) (nelisp-sys-frontend--parse-cond form))
     ((eq head 'while)
      (nelisp-sys-ast-make 'while
                           :cond (nelisp-sys-frontend--parse-expr (car args))
                           :body (nelisp-sys-frontend--parse-body (cdr args) form)
                           :form form))
     ((eq head 'seq)
      (nelisp-sys-ast-make 'seq
                           :body (nelisp-sys-frontend--parse-body args form)
                           :form form))
     ((eq head 'set!)
      (nelisp-sys-ast-make 'set!
                           :name (car args)
                           :value (nelisp-sys-frontend--parse-expr (cadr args))
                           :form form))
     ((eq head 'sys:cast)
      (nelisp-sys-ast-make 'cast
                           :type (nelisp-sys-frontend--parse-type (car args) form)
                           :arg (nelisp-sys-frontend--parse-expr (cadr args))
                           :form form))
     ((eq head 'sys:load-field)
      (nelisp-sys-ast-make 'load-field
                           :place (nelisp-sys-frontend--parse-expr (car args))
                           :field (cadr args) :form form))
     ((eq head 'sys:store-field!)
      (nelisp-sys-ast-make 'store-field!
                           :place (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :field (nth 1 args)
                           :value (nelisp-sys-frontend--parse-expr (nth 2 args))
                           :form form))
     ((eq head 'sys:slice-len)
      (nelisp-sys-ast-make 'slice-len
                           :slice (nelisp-sys-frontend--parse-expr (car args))
                           :form form))
     ((memq head '(sys:slice-ref sys:slice-ref-raw))
      (nelisp-sys-ast-make 'slice-ref
                           :raw (eq head 'sys:slice-ref-raw)
                           :slice (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :index (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :form form))
     ((eq head 'sys:slice-set!)
      (nelisp-sys-ast-make 'slice-set!
                           :slice (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :index (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :value (nelisp-sys-frontend--parse-expr (nth 2 args))
                           :form form))
     ((eq head 'sys:unsafe) (nelisp-sys-frontend--parse-unsafe form))
     ((memq head '(sys:with-borrow sys:with-borrow-mut))
      (nelisp-sys-frontend--parse-with-borrow form))
     ((eq head 'sys:load)
      (nelisp-sys-ast-make 'ptr-load
                           :ptr (nelisp-sys-frontend--parse-expr (car args))
                           :form form))
     ((eq head 'sys:store!)
      (nelisp-sys-ast-make 'ptr-store!
                           :ptr (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :value (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :form form))
     ((eq head 'sys:ptr-add)
      (nelisp-sys-ast-make 'ptr-add
                           :ptr (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :offset (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :form form))
     ((memq head '(sys:atomic-add! sys:atomic-sub!))
      ;; (sys:atomic-add!/sub! PTR DELTA) — SeqCst fetch-add/sub on the
      ;; *mut i64 at PTR; returns the previous value (Doc 133 P0, feeds
      ;; the Phase 2 refcount inc/dec).
      (nelisp-sys-ast-make 'atomic-add
                           :op (if (eq head 'sys:atomic-sub!) 'sub 'add)
                           :ptr (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :delta (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :form form))
     ((eq head 'sys:cas)
      ;; (sys:cas PTR EXPECTED NEW) — SeqCst compare-exchange; returns
      ;; 1 on success, 0 on mismatch.
      (nelisp-sys-ast-make 'atomic-cas
                           :ptr (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :expected (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :new (nelisp-sys-frontend--parse-expr (nth 2 args))
                           :form form))
     ((eq head 'sys:alloc)
      ;; (sys:alloc SIZE ALIGN) — raw heap allocation, returns a usize
      ;; address (or 0 on failure).  Doc 133 Phase 2/3 allocator surface
      ;; over the Phase 47 nl_alloc_bytes primitive.
      (nelisp-sys-ast-make 'alloc
                           :size (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :align (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :form form))
     ((eq head 'sys:dealloc)
      ;; (sys:dealloc PTR SIZE ALIGN) — free a sys:alloc block.
      (nelisp-sys-ast-make 'dealloc
                           :ptr (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :size (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :align (nelisp-sys-frontend--parse-expr (nth 2 args))
                           :form form))
     ((eq head 'sys:peek-u64)
      ;; (sys:peek-u64 ADDR) — raw u64 read at a usize address (offset 0).
      ;; Same usize-address convention as the atomics; callers compose
      ;; field offsets via sys:ptr-add (Doc 133 P2 box field access).
      (nelisp-sys-ast-make 'peek-u64
                           :ptr (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :form form))
     ((eq head 'sys:poke-u64)
      ;; (sys:poke-u64 ADDR VAL) — raw u64 store at a usize address.
      (nelisp-sys-ast-make 'poke-u64
                           :ptr (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :val (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :form form))
     ((eq head 'sys:syscall)
      ;; (sys:syscall NR A0 A1 A2 A3 A4 A5) — Linux x86_64 raw SYSCALL,
      ;; returns the kernel result as usize.  Used e.g. for a freestanding
      ;; mmap allocator (Doc 133 P2/P3).  Requires :syscall may.
      (unless (= (length args) 7)
        (nelisp-sys-frontend--err form "sys:syscall needs NR + 6 args"))
      (nelisp-sys-ast-make 'syscall
                           :args (mapcar #'nelisp-sys-frontend--parse-expr args)
                           :form form))
     ((eq head 'sys:sizeof)
      (nelisp-sys-ast-make 'sizeof
                           :type (nelisp-sys-frontend--parse-type (car args) form)
                           :form form))
     ((eq head 'sys:alignof)
      (nelisp-sys-ast-make 'alignof
                           :type (nelisp-sys-frontend--parse-type (car args) form)
                           :form form))
     ((eq head 'sys:offsetof)
      (nelisp-sys-ast-make 'offsetof :struct (nth 0 args) :field (nth 1 args)
                           :form form))
     ((eq head 'sys:exit)
      (nelisp-sys-ast-make 'exit
                           :arg (nelisp-sys-frontend--parse-expr (car args))
                           :form form))
     ((memq head '(sys:forget sys:dup))
      (nelisp-sys-ast-make 'resource-op :op head
                           :arg (nelisp-sys-frontend--parse-expr (car args))
                           :form form))
     ((eq head 'sys:addr-of)
      ;; (sys:addr-of NAME) — the runtime address of function NAME as a
      ;; raw code pointer (i64); companion of sys:call-ptr (Doc 133 P0).
      (unless (and (= (length args) 1) (symbolp (car args)))
        (nelisp-sys-frontend--err form "sys:addr-of needs a function name"))
      (nelisp-sys-ast-make 'addr-of :name (car args) :form form))
     ((eq head 'sys:call-ptr)
      ;; (sys:call-ptr FN-EXPR ARG...) — indirect call through a code
      ;; address (Doc 133 Phase 0).  FN-EXPR is a raw i64/usize address
      ;; (e.g. from a dispatch table); ARGs follow the C ABI.
      (unless args
        (nelisp-sys-frontend--err form "sys:call-ptr needs a function value"))
      (nelisp-sys-ast-make 'call-ptr
                           :fn-expr (nelisp-sys-frontend--parse-expr (car args))
                           :args (mapcar #'nelisp-sys-frontend--parse-expr
                                         (cdr args))
                           :form form))
     ;; --- char-table get/set (Doc 120.B residual surface) -------------
     ((eq head 'sys:char-table-get)
      ;; (sys:char-table-get TBL IDX OUT) -> i64 (0 OK / 1 ERR).  Delegates
      ;; to the `nl_char_table_get_raw' runtime extern (parent-table
      ;; recursion + default_val), the same primitive the Phase 47
      ;; `nl_jit_char_table_aref' swap calls.  TBL/OUT are raw Sexp
      ;; addresses (usize), IDX an i64 char code.
      (unless (= (length args) 3)
        (nelisp-sys-frontend--err form "sys:char-table-get needs TBL IDX OUT"))
      (nelisp-sys-ast-make 'char-table-get
                           :table (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :index (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :out   (nelisp-sys-frontend--parse-expr (nth 2 args))
                           :form form))
     ((eq head 'sys:char-table-set!)
      ;; (sys:char-table-set! TBL IDX VAL OUT) -> i64 (0 OK / 1 ERR).
      ;; Delegates to `nl_char_table_set_raw' (entries update + val echo).
      (unless (= (length args) 4)
        (nelisp-sys-frontend--err form
                                  "sys:char-table-set! needs TBL IDX VAL OUT"))
      (nelisp-sys-ast-make 'char-table-set!
                           :table (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :index (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :value (nelisp-sys-frontend--parse-expr (nth 2 args))
                           :out   (nelisp-sys-frontend--parse-expr (nth 3 args))
                           :form form))
     ;; --- heap-growing mutable strings (Doc 122 §122.B grammar) -------
     ((eq head 'sys:mut-str-make-empty)
      ;; (sys:mut-str-make-empty SLOT CAP) -> usize (SLOT addr).  Writes a
      ;; fresh Sexp::MutStr(String::with_capacity CAP) into SLOT.
      (unless (= (length args) 2)
        (nelisp-sys-frontend--err form "sys:mut-str-make-empty needs SLOT CAP"))
      (nelisp-sys-ast-make 'mut-str-make-empty
                           :slot (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :cap  (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :form form))
     ((eq head 'sys:mut-str-push-byte)
      ;; (sys:mut-str-push-byte PTR BYTE) -> i64 sentinel.  Appends BYTE's
      ;; low 8 bits to PTR's MutStr in place.
      (unless (= (length args) 2)
        (nelisp-sys-frontend--err form "sys:mut-str-push-byte needs PTR BYTE"))
      (nelisp-sys-ast-make 'mut-str-push-byte
                           :ptr  (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :byte (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :form form))
     ((eq head 'sys:mut-str-push-codepoint)
      ;; (sys:mut-str-push-codepoint PTR CP) -> i64 sentinel.  UTF-8 encodes
      ;; CP (1-4 bytes; surrogate/out-of-range clamp to U+FFFD) and appends.
      (unless (= (length args) 2)
        (nelisp-sys-frontend--err form
                                  "sys:mut-str-push-codepoint needs PTR CP"))
      (nelisp-sys-ast-make 'mut-str-push-codepoint
                           :ptr       (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :codepoint (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :form form))
     ((eq head 'sys:mut-str-len)
      ;; (sys:mut-str-len PTR) -> i64 byte length of PTR's MutStr.
      (unless (= (length args) 1)
        (nelisp-sys-frontend--err form "sys:mut-str-len needs PTR"))
      (nelisp-sys-ast-make 'mut-str-len
                           :ptr (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :form form))
     ((eq head 'sys:mut-str-finalize)
      ;; (sys:mut-str-finalize PTR SLOT) -> usize (SLOT addr).  Clones PTR's
      ;; MutStr inner String into a fresh Sexp::Str written to SLOT; the
      ;; source MutStr stays live and push-able (clone, not move).
      (unless (= (length args) 2)
        (nelisp-sys-frontend--err form "sys:mut-str-finalize needs PTR SLOT"))
      (nelisp-sys-ast-make 'mut-str-finalize
                           :ptr  (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :slot (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :form form))
     ;; --- floating-point arithmetic (Phase 47 f64 grammar) ------------
     ((memq head '(sys:f64+ sys:f64- sys:f64* sys:f64/))
      ;; binary f64 arithmetic, f64 x f64 -> f64.  Both operands must be
      ;; f64-typed (use sys:i64->f64 / sys:bits->f64 / an f64 param to make
      ;; one); lowers to the Phase 47 f64-add/sub/mul/div SSE ops.
      (unless (= (length args) 2)
        (nelisp-sys-frontend--err form "%S needs exactly two operands" head))
      (nelisp-sys-ast-make 'f64-arith :op head
                           :args (mapcar #'nelisp-sys-frontend--parse-expr args)
                           :form form))
     ((memq head '(sys:f64< sys:f64<= sys:f64> sys:f64>= sys:f64=))
      ;; binary f64 comparison, f64 x f64 -> bool.
      (unless (= (length args) 2)
        (nelisp-sys-frontend--err form "%S needs exactly two operands" head))
      (nelisp-sys-ast-make 'f64-cmp :op head
                           :args (mapcar #'nelisp-sys-frontend--parse-expr args)
                           :form form))
     ((eq head 'sys:i64->f64)
      ;; (sys:i64->f64 X) — signed integer -> f64 (CVTSI2SD).
      (unless (= (length args) 1)
        (nelisp-sys-frontend--err form "sys:i64->f64 needs one operand"))
      (nelisp-sys-ast-make 'i64->f64
                           :arg (nelisp-sys-frontend--parse-expr (car args))
                           :form form))
     ((eq head 'sys:f64->i64)
      ;; (sys:f64->i64 X) — f64 -> i64 truncating toward zero.
      (unless (= (length args) 1)
        (nelisp-sys-frontend--err form "sys:f64->i64 needs one operand"))
      (nelisp-sys-ast-make 'f64->i64
                           :arg (nelisp-sys-frontend--parse-expr (car args))
                           :form form))
     ((eq head 'sys:bits->f64)
      ;; (sys:bits->f64 X) — reinterpret an i64 bit pattern as f64 (no
      ;; numeric conversion); the bridge for a raw IEEE-754 word.
      (unless (= (length args) 1)
        (nelisp-sys-frontend--err form "sys:bits->f64 needs one operand"))
      (nelisp-sys-ast-make 'bits->f64
                           :arg (nelisp-sys-frontend--parse-expr (car args))
                           :form form))
     ((eq head 'sys:str-to-float)
      ;; (sys:str-to-float BYTES LEN OUT) -> i64 (1 success / 0 fail).
      ;; Delegates to the `nl_str_to_float' extern (str::parse::<f64>),
      ;; which writes Sexp::Float(parsed) into the OUT slot.  BYTES is a
      ;; usize byte-pointer, LEN the byte length, OUT a Sexp result slot.
      (unless (= (length args) 3)
        (nelisp-sys-frontend--err form "sys:str-to-float needs BYTES LEN OUT"))
      (nelisp-sys-ast-make 'str-to-float
                           :bytes (nelisp-sys-frontend--parse-expr (nth 0 args))
                           :len   (nelisp-sys-frontend--parse-expr (nth 1 args))
                           :out   (nelisp-sys-frontend--parse-expr (nth 2 args))
                           :form form))
     ((symbolp head)
      ;; Generic function/intrinsic call.
      (nelisp-sys-ast-make 'call :fn head
                           :args (mapcar #'nelisp-sys-frontend--parse-expr args)
                           :form form))
     (t (nelisp-sys-frontend--err form "cannot parse call head: %S" head)))))

(defun nelisp-sys-frontend--parse-let (form)
  "Parse (let ((NAME TYPE EXPR)...) BODY...) into a node."
  (let ((bindings (nth 1 form))
        (body (nthcdr 2 form)))
    (nelisp-sys-ast-make
     'let
     :bindings
     (mapcar
      (lambda (b)
        (unless (and (consp b) (= (length b) 3) (symbolp (car b)))
          (nelisp-sys-frontend--err form "bad let binding: %S" b))
        (list (nth 0 b)
              (nelisp-sys-frontend--parse-type (nth 1 b) form)
              (nelisp-sys-frontend--parse-expr (nth 2 b))))
      bindings)
     :body (nelisp-sys-frontend--parse-body body form)
     :form form)))

(defun nelisp-sys-frontend--parse-cond (form)
  "Parse (cond (TEST BODY...)...) into a node."
  (nelisp-sys-ast-make
   'cond
   :clauses
   (mapcar
    (lambda (clause)
      (cons (if (eq (car clause) 'else)
                'else
              (nelisp-sys-frontend--parse-expr (car clause)))
            (nelisp-sys-frontend--parse-body (cdr clause) form)))
    (cdr form))
   :form form))

(defun nelisp-sys-frontend--parse-unsafe (form)
  "Parse (sys:unsafe [(:reason STR)] BODY...) into a node."
  (let* ((args (cdr form))
         (reason nil)
         (body args))
    (when (and (consp (car args)) (eq (car (car args)) :reason))
      (setq reason (cadr (car args)))
      (setq body (cdr args)))
    (nelisp-sys-ast-make 'unsafe :reason reason
                         :body (nelisp-sys-frontend--parse-body body form)
                         :form form)))

(defun nelisp-sys-frontend--parse-with-borrow (form)
  "Parse (sys:with-borrow[-mut] ((VAR REFTYPE PLACE)) BODY...)."
  (let* ((mut (eq (car form) 'sys:with-borrow-mut))
         (binding (car (nth 1 form)))
         (body (nthcdr 2 form)))
    (unless (and (consp binding) (= (length binding) 3))
      (nelisp-sys-frontend--err form "bad with-borrow binding: %S" binding))
    (nelisp-sys-ast-make
     'with-borrow
     :mut mut
     :var (nth 0 binding)
     :ref-type (nelisp-sys-frontend--parse-type (nth 1 binding) form)
     :place (nelisp-sys-frontend--parse-expr (nth 2 binding))
     :body (nelisp-sys-frontend--parse-body body form)
     :form form)))

(provide 'nelisp-sys-frontend)

;;; nelisp-sys-frontend.el ends here
