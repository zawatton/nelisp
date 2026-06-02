;;; nelisp-cc-evalport-aot-builtin-call1.el --- lowered -*- lexical-binding: t; -*-
;;; Code:
(defconst nelisp-cc-evalport-aot-builtin-call1--source
  (quote (defun nelisp_aot_builtin_call1 (mirror frames name arg out scratch) (let* ((nil_slot (alloc-bytes 32 8)) (builtin_sym (alloc-bytes 32 8)) (name_buf (alloc-bytes 8 1)) (args_list (alloc-bytes 32 8)) (inner (alloc-bytes 32 8)) (func (alloc-bytes 32 8))) (seq (ptr-write-u64 nil_slot 0 0) (ptr-write-u64 (+ nil_slot 8) 0 0) (ptr-write-u64 (+ nil_slot 16) 0 0) (ptr-write-u64 (+ nil_slot 24) 0 0) (ptr-write-u64 name_buf 0 31078196194145634) (nl_alloc_symbol name_buf 7 builtin_sym) (nelisp_cons_construct arg nil_slot args_list) (nelisp_cons_construct name nil_slot inner) (nelisp_cons_construct builtin_sym inner func) (nelisp_apply_function func args_list frames out) out)))))
(provide (quote nelisp-cc-evalport-aot-builtin-call1))
