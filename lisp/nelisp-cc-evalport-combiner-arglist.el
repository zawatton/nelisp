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

    ;; perf/arglist-raw-word (segment G2): `nl_cons_car_ptr'/`nl_cons_
    ;; cdr_ptr' (lisp/nelisp-cc-jit-cons-car-ptr.el / -cdr-ptr.el) each
    ;; materialise an immediate WORD (Nil/T/Int) into a FRESH 32-byte
    ;; scratch box via `(alloc-bytes 32 8)' + `nl_val_load' so every
    ;; consumer can keep treating the result as a `*const Sexp' VIEW.
    ;; Every evaluated argument passes through this walker's
    ;; `(+ root_mark 0)' cons cursor, so on any call whose args include
    ;; a literal Int/Nil/T -- or simply reach the Nil list terminator,
    ;; which EVERY argument list does -- this walker was measured
    ;; (`tools/ai/nelisp-ai.sh alloc-sites') as the single largest
    ;; allocation source in the interpreter's hot path: ~13
    ;; allocations/iteration on `(dotimes (i N) (setq acc (cons (list i
    ;; (* 1.5 i)) acc)))', decomposed below into two independent sites.
    ;;
    ;; Both raw-word helpers below read the WORD directly (identical
    ;; box_ptr -> NlConsBox -> car/cdr-word addressing the two
    ;; accessors themselves use) instead of materialising.  CUR must
    ;; already be a validated Cons view (tag 7): both call sites in
    ;; `nl_eval_arg_list_drive' below sit directly inside the
    ;; `(sexp-tag (+ root_mark 0)) == 7' guard already established a
    ;; few lines above each use, with nothing in between that could
    ;; change `(+ root_mark 0)''s contents -- the same kind of
    ;; immediately-preceding precondition A1's `nelisp_frame_slot_raw_
    ;; word' (ab4a72484, lisp/nelisp-cc-frame-stack-find.el) relies on
    ;; via its callers' `record-slot-count' check.
    (defun nl_eval_arg_list_car_word (cur)
      (ptr-read-u64 (ptr-read-u64 cur 8) 0))
    (defun nl_eval_arg_list_cdr_word (cur)
      (ptr-read-u64 (ptr-read-u64 cur 8) 8))

    ;; Site 1 (classify + materialise the current arg): the ORIGINAL code
    ;; called `nl_cons_car_ptr (+ root_mark 0)' THREE times total across
    ;; this branch -- once to classify via `nl_val_tag', then AGAIN in
    ;; whichever arm ran, each call independently re-deriving (and, for
    ;; an immediate, independently RE-ALLOCATING) the same logical
    ;; value.  `nl_val_tag' (scripts/nelisp-standalone-build.el, Doc 146
    ;; §3.0) already classifies a raw WORD directly -- for a pointer
    ;; WORD (low bit 0) it dereferences the same tag byte `sexp-tag' on
    ;; a materialised view would read; for an immediate WORD it decodes
    ;; the tag from the bits -- so `(nl_val_tag car-word)' agrees with
    ;; `(sexp-tag (nl_cons_car_ptr cur))' for every value the slot can
    ;; hold, with no materialisation at all.  In the immediate branch,
    ;; `nl_sexp_clone_into' (lisp/nelisp-cc-sexp-clone-into.el) already
    ;; has its own "(logand src 1) == 1" immediate case that does
    ;; exactly what a discarded `nl_cons_car_ptr' scratch box existed to
    ;; feed it, so passing the raw WORD straight to `nl_sexp_clone_into'
    ;; skips that box entirely.  In the non-immediate branch, CAR-WORD
    ;; (low bit 0) IS ALREADY the exact `*const Sexp' `nl_cons_car_ptr'
    ;; would have returned unchanged for a pointer WORD, so passing it
    ;; to `nelisp_eval_call' is identical to re-deriving it.
    ;;
    ;; Site 2 (advance the cursor, `nl_eval_arg_list_advance_cdr'
    ;; below): the ORIGINAL code called the materialising `nl_cons_cdr_
    ;; ptr' and then IMMEDIATELY `nl_eval_arg_list_copy32'-ed its
    ;; result into `(+ root_mark 0)' and discarded it -- for an
    ;; immediate cdr (the Nil terminator on every list, or any literal
    ;; mid-list), that is a scratch box allocated, copied out of, and
    ;; thrown away.  `nl_sci_store_imm' (lisp/nelisp-cc-sexp-clone-
    ;; into.el) is the exact function `nl_val_load' itself calls to
    ;; materialise an immediate WORD; writing straight into CUR (=
    ;; `(+ root_mark 0)') skips the scratch box.  A pointer WORD is
    ;; copied via the pre-existing `nl_eval_arg_list_copy32' -- an
    ;; ordinary 4-word bit copy, NOT a refcount-aware clone, matching
    ;; this call site's pre-existing semantics exactly: CUR is a
    ;; borrowed walk cursor being re-pointed at the next list cell, not
    ;; a new owned reference, so no refcount bump belongs here (unlike
    ;; `nl_sexp_clone_into', which DOES bump refcounts -- deliberately
    ;; not used at this site for that reason).  Reading CUR's own
    ;; payload pointer and the box's cdr WORD through it completes
    ;; before any byte of CUR is written, so overwriting CUR in place
    ;; (CUR is also the read root) is safe.
    (defun nl_eval_arg_list_advance_cdr (cur)
      (let ((word (nl_eval_arg_list_cdr_word cur)))
        (if (= (logand word 1) 0)
            (nl_eval_arg_list_copy32 cur word)
          (nl_sci_store_imm word cur))))
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
             (let* ((car-word (nl_eval_arg_list_car_word (+ root_mark 0))))
               (seq
                (if (< (nl_val_tag car-word) 4)
                    ;; Self-evaluating immediate: materialise it directly
                    ;; from the raw WORD (perf/arglist-raw-word above).
                    (nl_sexp_clone_into car-word (+ root_mark 32))
                  ;; Preserve symbol identity for variable lookup.  Cloning
                  ;; the unevaluated form would create a fresh Symbol.
                  ;; CAR-WORD is already the pointer `nl_cons_car_ptr'
                  ;; would have returned for this branch (perf/arglist-
                  ;; raw-word above).
                  (if (= (nelisp_eval_call
                          car-word
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
                       ;; Re-derive the cdr from rooted STATE after
                       ;; allocation, directly in place (perf/arglist-raw-
                       ;; word above -- no scratch box for an immediate).
                       (nl_eval_arg_list_advance_cdr (+ root_mark 0))
                       (nl_eval_arg_list_status (+ root_mark 192) 0)))
                  0)))
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
