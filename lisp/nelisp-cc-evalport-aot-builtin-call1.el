;;; nelisp-cc-evalport-aot-builtin-call1.el --- lowered -*- lexical-binding: t; -*-
;;; Code:
(defconst nelisp-cc-evalport-aot-builtin-call1--source
  (quote (seq (defun nelisp_aot_builtin_call1_w (mirror frames name arg _pad scratch) (let* ((builtin_sym (alloc-bytes 32 8)) (name_buf (alloc-bytes 8 1)) (args_list (alloc-bytes 32 8)) (inner (alloc-bytes 32 8)) (func (alloc-bytes 32 8))) (seq (ptr-write-u64 name_buf 0 31078196194145634) (nl_alloc_symbol name_buf 7 builtin_sym) (nelisp_cons_construct arg 3 args_list) (nelisp_cons_construct name 3 inner) (nelisp_cons_construct builtin_sym inner func) (nelisp_apply_function_w func args_list mirror 0)))) (defun nelisp_aot_builtin_call1 (mirror frames name arg out scratch) (seq (nl_abi_word_to_rc (nelisp_aot_builtin_call1_w mirror frames name arg 0 scratch) out) out)))))
(provide (quote nelisp-cc-evalport-aot-builtin-call1))
