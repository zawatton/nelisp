;;; nelisp-cc-evalport-combiner-arglist.el --- lowered -*- lexical-binding: t; -*-
;;; Code:

;; Evaluate arguments left-to-right with a rooted host loop.  Each evaluated
;; value is appended to a cons chain in O(1); the old reverse-and-clone pass
;; copied the complete accumulated list on every iteration.
(defconst nelisp-cc-evalport-combiner-arglist--source
  '(seq
    (defun nl_write_nil_slot (slot)
      (seq (ptr-write-u64 slot 0 0)
           (ptr-write-u64 (+ slot 8) 0 0)
           (ptr-write-u64 (+ slot 16) 0 0)
           (ptr-write-u64 (+ slot 24) 0 0)
           0))
    ;; Status is rooted because evaluator calls may re-enter this walker.
    (defun nl_eval_arg_list_status (slot value)
      (seq (ptr-write-u64 slot 0 2)
           (ptr-write-u64 (+ slot 8) 0 value)
           (ptr-write-u64 (+ slot 16) 0 0)
           (ptr-write-u64 (+ slot 24) 0 0)
           slot))
    ;; Copy a Sexp view into a root slot without allocating or changing object
    ;; identity.  In particular, unevaluated interned symbols must reach the
    ;; evaluator unchanged for local-frame lookup.
    (defun nl_eval_arg_list_copy32 (dst src)
      (seq (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
           (ptr-write-u64 (+ dst 8) 0 (ptr-read-u64 src 8))
           (ptr-write-u64 (+ dst 16) 0 (ptr-read-u64 src 16))
           (ptr-write-u64 (+ dst 24) 0 (ptr-read-u64 src 24))
           dst))
    ;; ROOT-MARK owns seven consecutive slots: state, eval, nil, node, head,
    ;; tail, and status.  Deriving addresses here keeps the helper within the
    ;; register ABI.
    (defun nl_eval_arg_list_drive (cur_ptr env_ptr acc_slot root_mark)
      (seq
       (nl_write_nil_slot (+ root_mark 0))
       (nl_write_nil_slot (+ root_mark 32))
       (nl_write_nil_slot (+ root_mark 64))
       (nl_write_nil_slot (+ root_mark 96))
       (nl_write_nil_slot (+ root_mark 128))
       (nl_write_nil_slot (+ root_mark 160))
       (nl_eval_arg_list_status (+ root_mark 192) 0)
       (nl_eval_arg_list_copy32 (+ root_mark 0) cur_ptr)
       (while (= (nl_val_store_word (+ root_mark 192)) 1)
         (if (= (sexp-tag (+ root_mark 0)) 7)
             (seq
              (if (< (nl_val_tag (nl_cons_car_ptr (+ root_mark 0))) 4)
                  ;; Self-evaluating immediate: materialise it in EVAL.
                  (nl_sexp_clone_into (nl_cons_car_ptr (+ root_mark 0))
                                      (+ root_mark 32))
                ;; Preserve symbol identity for variable lookup.  Cloning the
                ;; unevaluated form would create a fresh Symbol.
                (if (= (nelisp_eval_call
                        (nl_cons_car_ptr (+ root_mark 0))
                        env_ptr (+ root_mark 32)) 0)
                    0
                  (nl_eval_arg_list_status (+ root_mark 192) 2)))
              ;; Construct and append only when evaluation succeeded.  The
              ;; cons cdr is one tagged word, so direct linking is O(1).
              (if (= (nl_val_store_word (+ root_mark 192)) 1)
                  (let* ((durable-node (alloc-bytes 32 8)))
                    (seq
                     (nelisp_cons_construct (+ root_mark 32)
                                            (+ root_mark 64)
                                            durable-node)
                     (if (= (sexp-tag (+ root_mark 128)) 0)
                         (seq
                          (nl_eval_arg_list_copy32
                           (+ root_mark 128) durable-node)
                          (nl_eval_arg_list_copy32
                           (+ root_mark 160) durable-node))
                       (seq
                        ;; The cdr word points at the durable Sexp wrapper,
                        ;; not directly at its consbox payload.
                        (ptr-write-u64
                         (+ (ptr-read-u64 (+ root_mark 160) 8) 8)
                         0 durable-node)
                        (nl_eval_arg_list_copy32
                         (+ root_mark 160) durable-node)))
                     ;; Re-derive the cdr from rooted STATE after allocation.
                     (nl_eval_arg_list_copy32
                      (+ root_mark 0) (nl_cons_cdr_ptr (+ root_mark 0)))
                     (nl_eval_arg_list_status (+ root_mark 192) 0)))
                0))
           ;; Preserve the historical dotted-tail truncation behavior.
           (nl_eval_arg_list_status (+ root_mark 192) 1)))
       ;; Proper and historical dotted-tail termination both return the head;
       ;; an evaluator failure returns an error after releasing the roots.
       (if (= (nl_val_store_word (+ root_mark 192)) 5)
           (seq
            (nl_eval_arg_list_copy32 acc_slot (+ root_mark 128))
            (nl_root_release env_ptr root_mark)
            0)
         (seq (nl_root_release env_ptr root_mark) 1))))
    (defun nl_eval_arg_list_walk (cur_ptr env_ptr acc_slot)
      (let* ((root_mark (nl_root_mark env_ptr))
             (state_slot (nl_root_reserve env_ptr))
             (eval_slot (nl_root_reserve env_ptr))
             (nil_slot (nl_root_reserve env_ptr))
             (node_slot (nl_root_reserve env_ptr))
             (head_slot (nl_root_reserve env_ptr))
             (tail_slot (nl_root_reserve env_ptr))
             (status_slot (nl_root_reserve env_ptr)))
        (nl_eval_arg_list_drive
         cur_ptr env_ptr acc_slot root_mark)))
    (defun nl_eval_arg_list (args_ptr env out_list_slot)
      (let* ((env_ptr env))
        (nl_eval_arg_list_walk args_ptr env_ptr out_list_slot)))))

(provide (quote nelisp-cc-evalport-combiner-arglist))
