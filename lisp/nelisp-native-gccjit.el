;;; nelisp-native-gccjit.el --- Raw-v2 libgccjit backend -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Lower the existing emitter AST, without changing its four-int64 frame ABI.
;; Imports are exported address cells, rebound by the trusted runtime resolver
;; after loading, so cached shared objects survive process ASLR.
;;; Code:
(require 'cl-lib)
(require 'nelisp-native-funcall-v2)
(require 'nelisp-native-load)
(declare-function alloc-bytes "ext:runtime" (size kind))
(declare-function ptr-write-u8 "ext:runtime" (address offset value))
(declare-function ptr-write-u64 "ext:runtime" (address offset value))
(declare-function ptr-read-u64 "ext:runtime" (address offset))
(declare-function ptr-call "ext:runtime" (address &rest arguments))
(declare-function nl-ffi--dlopen "nl-ffi" (soname))
(declare-function nl-ffi--dlsym "nl-ffi" (handle name))
;; libgccjit.h orders assembler=0, object=1, dynamic-library=2, executable=3.
;; The brief's value 3 requests an executable and fails without main.
(defconst nelisp-native-gccjit--dynamic-library 2)
(defvar nelisp-native-gccjit--jit nil)
(defvar nelisp-native-gccjit--ffi nil)
(defvar nelisp-native-gccjit--contexts nil)
(defvar nelisp-native-gccjit--results nil
  "Retain gcc_jit_result owners for published in-memory entry addresses.")
(defconst nelisp-native-gccjit--binary-ops
  '((+ . 0) (- . 1) (* . 2) (/ . 3) (% . 4)
    (logand . 5) (logxor . 6) (logior . 7)))
(defconst nelisp-native-gccjit--comparisons
  '((= . 0) (/= . 1) (< . 2) (<= . 3) (> . 4) (>= . 5)))

(defun nelisp-native-gccjit--cstring (string)
  "Copy STRING to a native NUL-terminated UTF-8 buffer."
  (when (string-match-p "\0" string) (error "gccjit: NUL in C string"))
  (let* ((bytes (if (fboundp 'string-byte) string (encode-coding-string string 'utf-8-unix)))
         (size (string-bytes bytes)) (buffer (alloc-bytes (1+ size) 1)))
    (unless (and (integerp buffer) (> buffer 0)) (error "gccjit: string allocation failed"))
    (dotimes (i size) (ptr-write-u8 buffer i (nelisp-native-load--byte bytes i)))
    (ptr-write-u8 buffer size 0) buffer))

(defun nelisp-native-gccjit--array (values)
  "Return a native pointer array containing VALUES."
  (let ((buffer (alloc-bytes (* 8 (max 1 (length values))) 1)) (index 0))
    (unless (and (integerp buffer) (> buffer 0)) (error "gccjit: array allocation failed"))
    (dolist (value values)
      (ptr-write-u64 buffer (* 8 index) value)
      (setq index (1+ index)))
    buffer))

(defun nelisp-native-gccjit--initialize ()
  (require 'nl-ffi)
  (unless nelisp-native-gccjit--jit
    (setq nelisp-native-gccjit--jit (nl-ffi--dlopen "libgccjit.so.0")))
  (unless nelisp-native-gccjit--ffi
    (setq nelisp-native-gccjit--ffi (nl-ffi--dlopen "libffi.so.8"))))

(defun nelisp-native-gccjit--symbol (handle name)
  (let ((address (nl-ffi--dlsym handle name)))
    (unless (and (integerp address) (> address 0))
      (error "gccjit: unresolved C API %s" name))
    address))

(defun nelisp-native-gccjit--direct (handle name args)
  (unless (<= (length args) 6) (error "gccjit: direct call exceeds six arguments"))
  (apply #'ptr-call (nelisp-native-gccjit--symbol handle name)
         (append args (make-list (- 6 (length args)) 0))))

(defun nelisp-native-gccjit--call (name &rest args)
  "Call libgccjit NAME; use libffi for APIs wider than six arguments."
  (nelisp-native-gccjit--initialize)
  (if (<= (length args) 6)
      (nelisp-native-gccjit--direct nelisp-native-gccjit--jit name args)
    ;; All wide APIs used here return pointers, and take pointer/int arguments.
    ;; On Linux x86_64 SysV, FFI_UNIX64=2 and each argument occupies eight bytes.
    (let* ((n (length args))
           (ptype (nelisp-native-gccjit--symbol nelisp-native-gccjit--ffi "ffi_type_pointer"))
           (cif (alloc-bytes 64 1))
           (types (nelisp-native-gccjit--array (make-list n ptype)))
           (values (nelisp-native-gccjit--array args))
           (avals (nelisp-native-gccjit--array
                   (cl-loop for i below n collect (+ values (* 8 i)))))
           (result (alloc-bytes 16 1)))
      (unless (= (logand (nelisp-native-gccjit--direct
                         nelisp-native-gccjit--ffi "ffi_prep_cif"
                         (list cif 2 n ptype types)) #xffffffff) 0)
        (error "gccjit: ffi_prep_cif failed"))
      (nelisp-native-gccjit--direct
       nelisp-native-gccjit--ffi "ffi_call"
       (list cif (nelisp-native-gccjit--symbol nelisp-native-gccjit--jit name) result avals))
      (ptr-read-u64 result 0))))

(defun nelisp-native-gccjit-import-cell-name (name)
  "Return the exported address-cell name for trusted import NAME."
  (unless (and (stringp name) (string-match-p "\\`[a-zA-Z_][a-zA-Z_0-9]*\\'" name))
    (error "gccjit: invalid import name %S" name))
  (concat "nl_gccjit_import_" name))

(defun nelisp-native-gccjit--lower (form imports)
  "Lower FORM with resolved (NAME . ADDRESS) IMPORTS, returning a C context."
  (unless (and (proper-list-p form) (= (length form) 4) (eq (car form) 'defun)
               (symbolp (cadr form))
               (equal (nth 2 form) '(env ticket argument-count root-count)))
    (error "gccjit: unsupported raw-v2 function %S" form))
  (let* ((ctx (nelisp-native-gccjit--call "gcc_jit_context_acquire"))
         (int (nelisp-native-gccjit--call "gcc_jit_context_get_int_type" ctx 8 1))
         (uint (nelisp-native-gccjit--call "gcc_jit_context_get_int_type" ctx 8 0))
         (params (mapcar (lambda (name)
                           (nelisp-native-gccjit--call "gcc_jit_context_new_param" ctx 0 int
                                                      (nelisp-native-gccjit--cstring (symbol-name name))))
                         (nth 2 form)))
         (fn (nelisp-native-gccjit--call "gcc_jit_context_new_function"
                                        ctx 0 0 int (nelisp-native-gccjit--cstring (symbol-name (cadr form)))
                                        4 (nelisp-native-gccjit--array params) 0))
         (block (nelisp-native-gccjit--call "gcc_jit_function_new_block"
                                           fn (nelisp-native-gccjit--cstring "entry")))
         (env (cl-mapcar (lambda (name param)
                          (cons name (nelisp-native-gccjit--call "gcc_jit_param_as_lvalue" param)))
                        (nth 2 form) params))
         (cells nil) (serial 0) (success nil))
    (unwind-protect
        (cl-labels
            ((call (name &rest args) (apply #'nelisp-native-gccjit--call name args))
             (constant (n) (call "gcc_jit_context_new_rvalue_from_long" ctx int n))
             (rv (lv) (call "gcc_jit_lvalue_as_rvalue" lv))
             (cast (value type) (call "gcc_jit_context_new_cast" ctx 0 value type))
             (local ()
               (setq serial (1+ serial))
               (call "gcc_jit_function_new_local" fn 0 int
                     (nelisp-native-gccjit--cstring (format "tmp_%d" serial))))
             (assign (lv value) (call "gcc_jit_block_add_assignment" block 0 lv value))
             (freeze (value) (let ((lv (local))) (assign lv value) (rv lv)))
             (new-block ()
               (setq serial (1+ serial))
               (call "gcc_jit_function_new_block" fn
                     (nelisp-native-gccjit--cstring (format "block_%d" serial))))
             (truth (value) (call "gcc_jit_context_new_comparison" ctx 0 1 value (constant 0)))
             (branch (condition yes no scope)
               (let* ((condition-value (expr condition scope))
                      (yes-block (new-block)) (no-block (new-block))
                      (join (new-block)) (out (local)))
                 (call "gcc_jit_block_end_with_conditional" block 0 (truth condition-value) yes-block no-block)
                 (setq block yes-block) (assign out (expr yes scope))
                 (call "gcc_jit_block_end_with_jump" block 0 join)
                 (setq block no-block) (assign out (expr no scope))
                 (call "gcc_jit_block_end_with_jump" block 0 join)
                 (setq block join) (rv out)))
             (sequence (forms scope)
               (let ((value (constant 0)))
                 (dolist (item forms) (setq value (freeze (expr item scope)))) value))
             (memory-slot (address offset scope)
               (let* ((base (freeze (expr address scope))) (off (expr offset scope))
                      (sum (call "gcc_jit_context_new_binary_op" ctx 0 0 int base off))
                      (pointer (call "gcc_jit_context_new_bitcast" ctx 0 sum
                                     (call "gcc_jit_type_get_pointer" uint))))
                 (call "gcc_jit_rvalue_dereference" pointer 0)))
             (expr (node scope)
               (cond
                ((integerp node)
                 (unless (memq (ash node -63) '(0 -1))
                   (error "gccjit: integer out of int64 range"))
                 (constant node))
                ((symbolp node)
                 (let ((binding (assq node scope)))
                   (unless binding (error "gccjit: unbound raw variable %S" node)) (rv (cdr binding))))
                ((not (proper-list-p node)) (error "gccjit: unsupported form %S" node))
                ((eq (car node) 'if)
                 (unless (= (length node) 4) (error "gccjit: if requires two arms"))
                 (branch (nth 1 node) (nth 2 node) (nth 3 node) scope))
                ((memq (car node) '(let let*))
                 (unless (and (>= (length node) 3) (proper-list-p (nth 1 node)))
                   (error "gccjit: malformed binding form"))
                 (let ((inner scope) (pending nil))
                   (dolist (binding (nth 1 node))
                     (unless (and (proper-list-p binding) (= (length binding) 2)
                                  (symbolp (car binding)) (car binding))
                       (error "gccjit: unsupported binding %S" binding))
                     (let ((lv (local)))
                       (assign lv (expr (cadr binding) (if (eq (car node) 'let*) inner scope)))
                       (push (cons (car binding) lv) pending)
                       (when (eq (car node) 'let*) (push (car pending) inner))))
                   (sequence (cddr node) (if (eq (car node) 'let*) inner (append pending scope)))))
                ((eq (car node) 'progn) (sequence (cdr node) scope))
                ((eq (car node) 'setq)
                 (unless (and (> (length node) 2) (= (% (length (cdr node)) 2) 0))
                   (error "gccjit: malformed setq"))
                 (let ((pairs (cdr node)) (value nil))
                   (while pairs
                     (let ((binding (assq (car pairs) scope)))
                       (unless binding (error "gccjit: setq of unbound variable"))
                       (setq value (freeze (expr (cadr pairs) scope)))
                       (assign (cdr binding) value))
                     (setq pairs (cddr pairs))) value))
                ((memq (car node) '(or and))
                 (unless (= (length node) 3) (error "gccjit: boolean form requires two operands"))
                 ;; Emitter predicates have integer 0/1 truth, not boxed Lisp truth.
                 (branch (nth 1 node)
                         (if (eq (car node) 'or) 1 `(if ,(nth 2 node) 1 0))
                         (if (eq (car node) 'or) `(if ,(nth 2 node) 1 0) 0) scope))
                ((or (assq (car node) nelisp-native-gccjit--binary-ops)
                     (assq (car node) nelisp-native-gccjit--comparisons))
                 (unless (= (length node) 3) (error "gccjit: operator requires two operands"))
                 (let* ((a (freeze (expr (nth 1 node) scope))) (b (expr (nth 2 node) scope))
                        (comparison (assq (car node) nelisp-native-gccjit--comparisons)))
                   (if comparison
                       (cast (call "gcc_jit_context_new_comparison" ctx 0 (cdr comparison) a b) int)
                     (call "gcc_jit_context_new_binary_op" ctx 0
                           (cdr (assq (car node) nelisp-native-gccjit--binary-ops)) int a b))))
                ((eq (car node) 'extern-call)
                 (unless (and (symbolp (cadr node)) (<= 0 (length (cddr node)) 6))
                   (error "gccjit: malformed extern-call"))
                 (let* ((name (symbol-name (cadr node))) (cell (assoc name cells))
                        (values (mapcar (lambda (arg) (freeze (expr arg scope))) (cddr node))))
                   (unless cell (error "gccjit: unresolved import %s" name))
                   (when (equal name "nl_native_funcall_v2")
                     (unless (and (= (length values) 6)
                                  (= (plist-get (nelisp-native-funcall-v2-descriptor) :arity) 6)
                                  (equal (plist-get (nelisp-native-funcall-v2-descriptor) :params)
                                         '(u64 u64 u64 u64 u64 u64)))
                       (error "gccjit: funcall descriptor mismatch")))
                   (setq values (append values (make-list (- 6 (length values)) (constant 0))))
                   (let ((type (call "gcc_jit_context_new_function_ptr_type" ctx 0 int 6
                                     (nelisp-native-gccjit--array (make-list 6 int)) 0)))
                     (freeze (call "gcc_jit_context_new_call_through_ptr" ctx 0
                                   (call "gcc_jit_context_new_bitcast" ctx 0 (rv (cdr cell)) type) 6 (nelisp-native-gccjit--array values))))))
                ((eq (car node) 'ptr-read-u64)
                 (unless (= (length node) 3) (error "gccjit: malformed ptr-read-u64"))
                 (cast (rv (memory-slot (nth 1 node) (nth 2 node) scope)) int))
                ((eq (car node) 'ptr-write-u64)
                 (unless (= (length node) 4) (error "gccjit: malformed ptr-write-u64"))
                 (let* ((slot (memory-slot (nth 1 node) (nth 2 node) scope))
                        (value (freeze (expr (nth 3 node) scope))))
                   (assign slot (cast value uint)) value))
                (t (error "gccjit: unsupported form %S" node)))))
          (dolist (import imports)
            (unless (and (consp import) (integerp (cdr import)) (> (cdr import) 0)
                         (not (assoc (car import) cells)))
              (error "gccjit: invalid or duplicate import %S" import))
            (push (cons (car import)
                        (call "gcc_jit_context_new_global" ctx 0 0 int
                              (nelisp-native-gccjit--cstring
                               (nelisp-native-gccjit-import-cell-name (car import))))) cells))
          (let ((value (expr (nth 3 form) env)))
            (call "gcc_jit_block_end_with_return" block 0 value))
          (push (cons ctx (list :entry (symbol-name (cadr form)) :imports imports))
                nelisp-native-gccjit--contexts)
          (setq success t) ctx)
      (unless success (nelisp-native-gccjit--call "gcc_jit_context_release" ctx)))))

;;;###autoload
(defun nelisp-native-gccjit-lower (form imports)
  "Lower raw-v2 FORM with resolved (NAME . ADDRESS) IMPORTS to a gccjit context.
Caller owns the context and must release it after compilation."
  (nelisp-native-load--without-midform-collect
   (lambda () (nelisp-native-gccjit--lower form imports))))

(defun nelisp-native-gccjit--release-context (ctx)
  (setq nelisp-native-gccjit--contexts (assq-delete-all ctx nelisp-native-gccjit--contexts))
  (nelisp-native-gccjit--call "gcc_jit_context_release" ctx))

;;;###autoload
(defun nelisp-native-gccjit-compile-in-memory (form imports)
  "Compile FORM to an entry address, retaining its gcc_jit_result owner."
  (nelisp-native-load--without-midform-collect
   (lambda ()
     (let ((ctx (nelisp-native-gccjit--lower form imports)) (result nil) (success nil))
       (unwind-protect
           (progn
             (setq result (nelisp-native-gccjit--call "gcc_jit_context_compile" ctx))
             (unless (and (integerp result) (> result 0)) (error "gccjit: compilation failed"))
             (dolist (import imports)
               (let ((cell (nelisp-native-gccjit--call
                            "gcc_jit_result_get_global" result
                            (nelisp-native-gccjit--cstring
                             (nelisp-native-gccjit-import-cell-name (car import))))))
                 (unless (> cell 0) (error "gccjit: missing import cell"))
                 (ptr-write-u64 cell 0 (cdr import))))
             (let ((entry (nelisp-native-gccjit--call
                           "gcc_jit_result_get_code" result
                           (nelisp-native-gccjit--cstring (symbol-name (cadr form))))))
               (unless (> entry 0) (error "gccjit: entry unavailable"))
               (push (cons entry result) nelisp-native-gccjit--results)
               (setq success t) entry))
         (nelisp-native-gccjit--release-context ctx)
         (when (and result (> result 0) (not success))
           (nelisp-native-gccjit--call "gcc_jit_result_release" result)))))))

;;;###autoload
(defun nelisp-native-gccjit-compile-to-file (form imports path)
  "Compile FORM with IMPORTS to a dynamic library at PATH.
Use GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY (2 in libgccjit.h)."
  (nelisp-native-load--without-midform-collect
   (lambda ()
     (let ((ctx (nelisp-native-gccjit--lower form imports)))
       (unwind-protect
           (progn
             (nelisp-native-gccjit--call "gcc_jit_context_compile_to_file" ctx nelisp-native-gccjit--dynamic-library
                                        (nelisp-native-gccjit--cstring path))
             (let ((err (nelisp-native-gccjit--call "gcc_jit_context_get_first_error" ctx)))
               (unless (eql err 0) (error "gccjit: compile-to-file failed")))
             (unless (and (file-exists-p path) (> (file-attribute-size (file-attributes path)) 0))
               (error "gccjit: shared library was not written"))
             path)
         (nelisp-native-gccjit--release-context ctx))))))
(provide 'nelisp-native-gccjit)
;;; nelisp-native-gccjit.el ends here
