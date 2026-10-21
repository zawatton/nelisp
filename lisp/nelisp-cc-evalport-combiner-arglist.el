;;; nelisp-cc-evalport-combiner-arglist.el --- argument WORDs -*- lexical-binding: t; -*-
;;; Code:

;; Evaluate left-to-right into one WORD frame on the root stack (the
;; values and the cursor, precisely rooted for parked Doc 199 workers too),
;; then build the argument list from those WORDs (word-heap S6.3).
(defconst nelisp-cc-evalport-combiner-arglist--source
  '(seq
    ;; GNU eval_sub evaluates the arguments into argvals (eval.c:2626,
    ;; 2685-2693); here into one WORD frame, which keeps each value and the
    ;; cursor rooted while the next argument is evaluated, and the list is
    ;; built from those WORDs afterwards: no Sexp root slots and no cloned
    ;; values.  A non-cons tail keeps the historical truncation.  No
    ;; safepoint intervenes between the release and the caller rooting W.
    (defun nl_eval_arg_list_w (args env _pad)
      (let* ((n (m5_fx_arg_count args 0)))
        (if (= n 0) 3
          (let* ((mark (nl_root_mark env)) (base (nl_root_frame env (+ n 1))))
            (if (= base 0)
                (seq (nl_root_release env mark) (wf_bytecode_root_exhausted) 0)
              (let* ((w (if (= (m5_fx_eval_forms args env base n) 0)
                            (m5_fx_slot_list base n) 0)))
                (seq (nl_root_release env mark) w)))))))
    (defun nl_eval_arg_list_walk_w (args env _pad)
      (nl_eval_arg_list_w args env 0))
    ;; Keep legacy callers at the outer boundary only.  The evaluator calls
    ;; nl_eval_arg_list_w and consumes its WORD directly.
    (defun nl_eval_arg_list_walk (args env out)
      (nl_abi_word_to_rc (nl_eval_arg_list_walk_w args env 0) out))
    (defun nl_eval_arg_list (args env out)
      (nl_abi_word_to_rc (nl_eval_arg_list_w args env 0) out))))

(provide 'nelisp-cc-evalport-combiner-arglist)
