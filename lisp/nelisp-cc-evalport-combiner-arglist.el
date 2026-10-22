;;; nelisp-cc-evalport-combiner-arglist.el --- argument WORDs -*- lexical-binding: t; -*-
;;; Code:

;; Evaluate left-to-right in a host loop.  The cursor, current value and
;; accumulated list and tail have precise roots for parked Doc 199 workers.
;; Root slots are views: their payloads survive collection, while a copied
;; header alone does not keep its original heap wrapper alive.  Return a fresh
;; durable header only after evaluation finishes (Doc 215 R2/R4).
(defconst nelisp-cc-evalport-combiner-arglist--source
  '(seq
    (defun nl_eval_arg_list_drive_w
        (cur env root-mark state-slot value-slot head-slot tail-slot)
      (let* ((cursor cur) (head 0) (status 0) (value 0) (argument 0) (node 0) (result 3))
        (seq
         (wf_word_store_out head-slot 3)
         (while (and (= status 0) (= (nl_val_tag cursor) 7))
           (seq
            ;; Preserve the current cons even if an argument mutates the
            ;; original input list or runs a collection during evaluation.
            (wf_word_store_out state-slot cursor)
            (let* ((form (nl_cons_car_word state-slot)))
              (setq value (if (< (nl_val_tag form) 4) form
                            (nelisp_eval_call_w form env 0))))
            (if (= value 0)
                (setq status 1)
              (seq
               ;; Publish the result root before the constructor allocates.
               (wf_word_store_out value-slot value)
               ;; Preserve original immediates across the WORD boundary.
               ;; Pointer wrappers still use the published view: a copied
               ;; header roots the payload, not its original heap wrapper.
               (setq argument (if (= (logand value 1) 1) value value-slot))
               (if (= head 0)
                   (seq (nelisp_cons_construct argument 3 head-slot)
                        (wf_copy32 tail-slot head-slot)
                        (setq head 1))
                 (seq (setq node (nelisp_cons_construct_w argument 3 0))
                      (ptr-write-u64 (heap-read-u64 tail-slot 8) 8 node)
                      (wf_copy32 tail-slot node)))
               (setq cursor (nl_cons_cdr_word state-slot))))))
         ;; A non-cons tail retains the historical truncation behavior.
         ;; No safepoint intervenes between release and the caller rooting W.
         (if (and (= status 0) (= head 1))
             (seq (setq result (alloc-bytes 32 8))
                  (wf_copy32 result head-slot)) 0)
         (nl_root_release env root-mark)
         (if (= status 0) result 0))))
    (defun nl_eval_arg_list_walk_w (cur env _pad)
      (let* ((mark (nl_root_mark env))
             (state-slot (nl_root_reserve_checked env))
             (value-slot (nl_root_reserve_checked env))
             (head-slot (nl_root_reserve_checked env))
             (tail-slot (nl_root_reserve_checked env)))
        ;; Reservation failure stashes its own signal; do not enter the loop.
        (if (= tail-slot 0)
            (seq (nl_root_release env mark) 0)
          (nl_eval_arg_list_drive_w cur env mark state-slot value-slot head-slot tail-slot))))
    (defun nl_eval_arg_list_w (args env _pad)
      (nl_eval_arg_list_walk_w args env 0))
    ;; Keep legacy callers at the outer boundary only.  The evaluator calls
    ;; nl_eval_arg_list_w and consumes its WORD directly.
    (defun nl_eval_arg_list_walk (args env out)
      (nl_abi_word_to_rc (nl_eval_arg_list_walk_w args env 0) out))
    (defun nl_eval_arg_list (args env out)
      (nl_abi_word_to_rc (nl_eval_arg_list_w args env 0) out))))

(provide 'nelisp-cc-evalport-combiner-arglist)
