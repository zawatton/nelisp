;;; nonenv-aot-builtin-call1.nl --- native nelisp_aot_builtin_call1 provider  -*- lexical-binding: t; -*-
;;
;; Doc 135 Stage 135.E (M2) — native provider for the Doc 129.6 AOT one-arg
;; builtin dispatcher ABI:
;;
;;   nelisp_aot_builtin_call1(mirror, frames, name, arg, out, scratch) -> out
;;
;; The Phase 47 compiler lowers a 1-arg env-INDEPENDENT builtin call
;; (car/cdr/symbolp/length/symbol-name/... — the
;; `nelisp-phase47-compiler--aot-builtin1-delegation-symbols' set) that appears
;; inside the elisp combiner (nl_eval / nl_apply_do_fset) into an extern-call
;; against this symbol.  Doc 129 only ever shipped an in-Emacs-process bridge
;; (nelisp-cc-runtime-aot-builtin-call1, src/nelisp-cc-runtime.el); it never
;; provided a NATIVE symbol, so the combiner .o left nelisp_aot_builtin_call1
;; undefined at link.  This module supplies it natively.
;;
;; Strategy: synthesize the `(builtin NAME)' sentinel (the exact shape
;; `nl_install_builtins' registers in the mirror function-cell) plus a
;; one-element argument list `(ARG)', then forward to the kept Rust
;; `nelisp_apply_function', which re-dispatches through `builtins::dispatch'.
;; Every delegation-list builtin is env-independent by construction, so dispatch
;; never reads the env ptr, and `eval_stash_err' touches env only on the error
;; path (`write(out,v); 0' on success).  We therefore pass `frames' as the env
;; ptr — matching the combiner's existing B2 convention for env-independent
;; builtin forwarding (see eval-port/combiner-apply.nl).
;;
;; The ABI passes `mirror' and `scratch' too, but the sentinel/list path does
;; not need them; they are accepted positionally to satisfy the C-ABI shape.

(sys:extern nelisp_apply_function
  (:symbol "nelisp_apply_function" :abi c :unsafe t)
  ((func_ptr usize) (args_list_ptr usize) (env_ptr usize) (out_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may))

(sys:extern nelisp_cons_construct
  (:symbol "nelisp_cons_construct" :abi c :unsafe t)
  ((car_ptr usize) (cdr_ptr usize) (result_slot usize))
  i64
  (:alloc may :ffi may :unsafe may))

(sys:extern nl_alloc_symbol
  (:symbol "nl_alloc_symbol" :abi c :unsafe t)
  ((bytes_ptr usize) (len i64) (result_slot usize))
  usize
  (:alloc may :ffi may :unsafe may))

;; nelisp_aot_builtin_call1(mirror, frames, name, arg, out, scratch) -> out
;; Returns OUT (the result has been written there by nelisp_apply_function).
(sys:defun nelisp_aot_builtin_call1
    ((mirror usize) (frames usize) (name usize)
     (arg usize) (out usize) (scratch usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((nil_slot usize (sys:alloc 32 8))
        (builtin_sym usize (sys:alloc 32 8))
        (name_buf usize (sys:alloc 8 1))
        (args_list usize (sys:alloc 32 8))
        (inner usize (sys:alloc 32 8))
        (func usize (sys:alloc 32 8)))
    (sys:unsafe
     ;; Nil terminator (tag 0, 32 bytes zeroed).
     (sys:poke-u64 nil_slot 0)
     (sys:poke-u64 (+ nil_slot 8) 0)
     (sys:poke-u64 (+ nil_slot 16) 0)
     (sys:poke-u64 (+ nil_slot 24) 0)
     ;; Symbol("builtin")  (7 bytes, little-endian packed = 31078196194145634).
     (sys:poke-u64 name_buf 31078196194145634)
     (nl_alloc_symbol name_buf 7 builtin_sym)
     ;; args_list = (ARG)
     (nelisp_cons_construct arg nil_slot args_list)
     ;; inner = (NAME)
     (nelisp_cons_construct name nil_slot inner)
     ;; func = (builtin NAME)   — head=="builtin", func.cdr.car == NAME
     (nelisp_cons_construct builtin_sym inner func)
     ;; Forward to Rust apply; `frames' stands in for the env ptr (env-free
     ;; builtins never dereference it; eval_stash_err writes OUT on success).
     (nelisp_apply_function func args_list frames out)
     out)))
