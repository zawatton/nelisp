;;; nelisp-cc-eln-callback7.el --- bounded seven-word callback entry -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; AOT-only callback probe. Its argument descriptor contains raw GNU words,
;; not NeLisp Sexps or decoded GNU Lisp_Objects.
(defconst nelisp-cc-eln-callback7-capacity 8)
(defconst nelisp-cc-eln-callback7-header-bytes 32)
(defconst nelisp-cc-eln-callback7-record-bytes 80)
(defconst nelisp-cc-eln-callback7-bss-bytes
  (+ nelisp-cc-eln-callback7-header-bytes
     (* nelisp-cc-eln-callback7-capacity
        nelisp-cc-eln-callback7-record-bytes)))

(defconst nelisp-cc-eln-callback7-port-count 32
  "Number of slot-identifying callback ports (see the port entries below).
This is the single table the AOT entries, the loader's symbol list, and
the dispatcher's range check are all generated from.")

(defun nelisp-cc-eln-callback7-port-symbol-names ()
  "Return the AOT entry symbol name of every callback port, in port order."
  (let ((names nil) (i 0))
    (while (< i nelisp-cc-eln-callback7-port-count)
      (push (format "nelisp_eln_callback_port%d_entry_word" i) names)
      (setq i (1+ i)))
    (nreverse names)))
(defconst nelisp-cc-eln-callback7-port-tag-base 1347375700
  "Descriptor word 6 of a port entry is this base plus the port number.
The base (ASCII \"PORT\") is an arbitrary nonzero fixnum; the ports only
exist so a native body importing several distinct freloc slots can have
each slot's table entry point at its own port, letting the Lisp dispatcher
recover which authenticated slot was called from the descriptor alone.")

(defconst nelisp-cc-eln-callback7--source
  `((defun nl_eln_callback7_record_at (control index)
      (+ control ,nelisp-cc-eln-callback7-header-bytes
         (* index ,nelisp-cc-eln-callback7-record-bytes)))
    (defun nl_eln_callback7_lock ()
      (while (= (atomic-compare-exchange
                 (data-addr nl_eln_callback7_context) 0 1) 0)
        0)
      0)
    (defun nl_eln_callback7_unlock ()
      (ptr-write-u64 (data-addr nl_eln_callback7_context) 0 0)
      0)
    (defun nl_eln_callback7_reserve ()
      (let* ((control (data-addr nl_eln_callback7_context))
             (tid (syscall-direct 186 0 0 0 0 0 0))
             (active (data-addr nl_eln_callback_context))
             (active-depth (ptr-read-u64 active 16))
             (active-record
              (if (= active-depth 0) 0
                (nl_eln_callback_record_at active (- active-depth 1))))
             (env (if (= active-record 0) 0 (ptr-read-u64 active-record 0)))
             (function-slot (if (= active-record 0) 0
                              (ptr-read-u64 active-record 8)))
             (args-slot (if (= active-record 0) 0
                          (ptr-read-u64 active-record 16)))
             (out-slot (if (= active-record 0) 0
                         (ptr-read-u64 active-record 24)))
             (argc (if (= active-record 0) 0
                     (ptr-read-u64 active-record 32))))
        (nl_eln_callback7_lock)
        (let* ((depth (ptr-read-u64 control 16))
               (owner (ptr-read-u64 control 8))
               (valid (if (and (> tid 0)
                               (> active-depth 0)
                               (<= active-depth 8)
                               (= (ptr-read-u64 active 8) tid)
                               (< depth ,nelisp-cc-eln-callback7-capacity)
                               (or (= depth 0) (= owner tid))
                               (= argc 1)
                               (= (ptr-read-u64 active-record 40) -1)
                               (= (nl_eln_callback_slot_ok env function-slot) 1)
                               (= (nl_eln_callback_slot_ok env args-slot) 1)
                               (= (nl_eln_callback_slot_ok env out-slot) 1))
                          1 0)))
          (if (= valid 0)
              (seq (nl_eln_callback7_unlock) 0)
            (let* ((record (nl_eln_callback7_record_at control depth)))
              (seq
               (if (= depth 0) (ptr-write-u64 control 8 tid) 0)
               (ptr-write-u64 record 56 -1)
               (ptr-write-u64 record 64 (+ depth 1))
               (ptr-write-u64 control 16 (+ depth 1))
               (ptr-write-u64 control 24 -1)
               (nl_eln_callback7_unlock)
               (+ depth 1)))))))
    (defun nl_eln_callback7_finish (token status)
      (let* ((token-value token) (status-value status)
             (control (data-addr nl_eln_callback7_context))
             (tid (syscall-direct 186 0 0 0 0 0 0)))
        (nl_eln_callback7_lock)
        (let* ((depth (ptr-read-u64 control 16)))
          (if (and (> depth 0) (= token-value depth)
                   (= (ptr-read-u64 control 8) tid))
              (let* ((record (nl_eln_callback7_record_at control (- depth 1))))
                (seq
                 (ptr-write-u64 record 56 status-value)
                 (ptr-write-u64 control 24 status-value)
                 (ptr-write-u64 record 0 0)
                 (ptr-write-u64 record 8 0)
                 (ptr-write-u64 record 16 0)
                 (ptr-write-u64 record 24 0)
                 (ptr-write-u64 record 32 0)
                 (ptr-write-u64 record 40 0)
                 (ptr-write-u64 record 48 0)
                 (ptr-write-u64 control 16 (- depth 1))
                 (if (= (- depth 1) 0) (ptr-write-u64 control 8 0) 0)
                 (nl_eln_callback7_unlock)
                 1))
            (seq (nl_eln_callback7_unlock) 0)))))
    (defun nl_eln_callback7_capture_exit (active-record)
      ;; Doc 207: the gateway reported a non-local exit.  Before any other
      ;; code can reuse the M6 stash, publish the first exit of this native
      ;; activation into the caller's pinned context slots: ARGS-SLOT gets
      ;; (TAG . VALUE) and OUT-SLOT the stash flag (1 signal, 2 throw).  The
      ;; stash itself is left intact.  A later exit of the same activation
      ;; never overwrites the first.
      (let* ((flag (ptr-read-u64 268435472 0))
             (env (ptr-read-u64 active-record 0))
             (args-slot (ptr-read-u64 active-record 16))
             (out-slot (ptr-read-u64 active-record 24)))
        (if (and (or (= flag 1) (= flag 2))
                 (= (nl_eln_callback_slot_ok env args-slot) 1)
                 (= (nl_eln_callback_slot_ok env out-slot) 1)
                 (= (ptr-read-u64 out-slot 0) 0))
            (seq (nelisp_cons_construct 268435480 268435512 args-slot)
                 (wf_write_int out-slot flag)
                 0)
          0)))
    (defun nelisp_eln_callback7_status ()
      (ptr-read-u64 (data-addr nl_eln_callback7_context) 24))
    (defun nl_eln_callback7_reject_if_idle ()
      ;; Do not overwrite an enclosing activation's status on nested or
      ;; foreign-owner rejection.  With no active record, publish failure so
      ;; an inactive caller cannot mistake the raw nil sentinel for success.
      (let ((control (data-addr nl_eln_callback7_context)))
        (nl_eln_callback7_lock)
        (if (= (ptr-read-u64 control 16) 0)
            (ptr-write-u64 control 24 1)
          0)
        (nl_eln_callback7_unlock)
        0))
    (defun nelisp_eln_callback7_root_mark (env out)
      (wf_write_int out (nl_root_mark env))
      0)
    (defun nelisp_eln_callback7_entry
        (raw0 raw1 raw2 raw3 raw4 raw5 raw6)
      ;; Capturing is separated from dispatch so all seven input words survive
      ;; calls to the lock and gateway. Descriptor lifetime ends at return.
      (let* ((word0 raw0) (word1 raw1) (word2 raw2) (word3 raw3)
             (word4 raw4) (word5 raw5) (word6 raw6)
             (token (nl_eln_callback7_reserve)))
        (if (= token 0)
            0
          (let* ((control (data-addr nl_eln_callback7_context))
                 (record (nl_eln_callback7_record_at control (- token 1)))
                 (active (data-addr nl_eln_callback_context))
                 (active-depth (ptr-read-u64 active 16))
                 (active-record
                  (nl_eln_callback_record_at active (- active-depth 1)))
                 (env (ptr-read-u64 active-record 0))
                 (function-slot (ptr-read-u64 active-record 8))
                 (descriptor record)
                 (mark (nl_root_mark env))
                 (args-slot (nl_root_reserve_checked env))
                 (out-slot (if (= args-slot 0) 0
                             (nl_root_reserve_checked env)))
                 (reserved (if (and (> args-slot 0)
                                    (= out-slot (+ args-slot 32)))
                               1 0)))
            (ptr-write-u64 record 0 word0)
            (ptr-write-u64 record 8 word1)
            (ptr-write-u64 record 16 word2)
            (ptr-write-u64 record 24 word3)
            (ptr-write-u64 record 32 word4)
            (ptr-write-u64 record 40 word5)
            (ptr-write-u64 record 48 word6)
            (if (or (= reserved 0)
                    (>= descriptor 2305843009213693952))
                (seq
                 (nl_root_release env mark)
                 (nl_eln_callback7_finish token 1)
                 0)
              (seq
               (wf_write_int args-slot descriptor)
               (wf_write_nil out-slot)
               (let* ((rc (wf_bytecode_call_gateway
                           env function-slot args-slot 0 1 out-slot))
                      (tag (ptr-read-u64 out-slot 0))
                      (value (ptr-read-u64 out-slot 8))
                      (status (if (/= rc 0) rc
                                (if (or (/= tag 2)
                                        (< value -2305843009213693952)
                                        (>= value 2305843009213693952))
                                    4 0)))
                      (word (if (= status 0) (+ (* value 4) 2) 0)))
                 (nl_root_release env mark)
                 (nl_eln_callback7_finish token status)
                 word)))))))
    (defun nelisp_eln_callback7_entry_word
        (raw0 raw1 raw2 raw3 raw4 raw5 raw6)
      ;; A dotted pair of u32 fixnums is the only accepted raw-word result.
      ;; The halves are copied into this activation's private record before
      ;; root release; the final u64 load returns machine bits without ever
      ;; asking Lisp's signed fixnum ABI to represent the whole word.
      (let* ((word0 raw0) (word1 raw1) (word2 raw2) (word3 raw3)
             (word4 raw4) (word5 raw5) (word6 raw6)
             (token (nl_eln_callback7_reserve)))
        (if (= token 0)
            (seq (nl_eln_callback7_reject_if_idle) 0)
          (let* ((control (data-addr nl_eln_callback7_context))
                 (record (nl_eln_callback7_record_at control (- token 1)))
                 (active (data-addr nl_eln_callback_context))
                 (active-depth (ptr-read-u64 active 16))
                 (active-record
                  (nl_eln_callback_record_at active (- active-depth 1)))
                 (env (ptr-read-u64 active-record 0))
                 (function-slot (ptr-read-u64 active-record 8))
                 (descriptor record)
                 (mark (nl_root_mark env))
                 (args-slot (nl_root_reserve_checked env))
                 (out-slot (if (= args-slot 0) 0
                             (nl_root_reserve_checked env)))
                 (reserved (if (and (> args-slot 0)
                                    (= out-slot (+ args-slot 32)))
                               1 0)))
            (ptr-write-u64 record 0 word0)
            (ptr-write-u64 record 8 word1)
            (ptr-write-u64 record 16 word2)
            (ptr-write-u64 record 24 word3)
            (ptr-write-u64 record 32 word4)
            (ptr-write-u64 record 40 word5)
            (ptr-write-u64 record 48 word6)
            (if (or (= reserved 0)
                    (>= descriptor 2305843009213693952))
                (seq
                 (nl_root_release env mark)
                 (nl_eln_callback7_finish token 1)
                 0)
              (seq
               (wf_write_int args-slot descriptor)
               (wf_write_nil out-slot)
               (let* ((rc (wf_bytecode_call_gateway
                           env function-slot args-slot 0 1 out-slot))
                      (result-tag (ptr-read-u64 out-slot 0)))
                 (if (/= rc 0)
                     (seq
                      (nl_eln_callback7_capture_exit active-record)
                      (nl_root_release env mark)
                      (nl_eln_callback7_finish token rc)
                      0)
                   (if (/= result-tag 7)
                       (seq
                        (nl_root_release env mark)
                        (nl_eln_callback7_finish token 4)
                        0)
                     (let* ((low-slot (nl_cons_car_ptr out-slot))
                            (high-slot (nl_cons_cdr_ptr out-slot))
                            (low-tag (ptr-read-u64 low-slot 0))
                            (low (ptr-read-u64 low-slot 8))
                            (high-tag (ptr-read-u64 high-slot 0))
                            (high (ptr-read-u64 high-slot 8))
                            (valid (if (and (= low-tag 2) (= high-tag 2)
                                            (>= low 0) (<= low 4294967295)
                                            (>= high 0) (<= high 4294967295))
                                       1 0)))
                       (if (= valid 0)
                           (seq
                            (nl_root_release env mark)
                            (nl_eln_callback7_finish token 4)
                            0)
                         (seq
                          (ptr-write-u32 record 72 low)
                          (ptr-write-u32 record 76 high)
                          (let ((raw-word (ptr-read-u64 record 72)))
                            (nl_root_release env mark)
                            (nl_eln_callback7_finish token 0)
                            raw-word)))))))))))))
    (defun nelisp_eln_callback1_entry_word (raw0)
      ;; Supply initialized values for the fixed seven-word backend entry.
      ;; Never read caller registers other than the one declared argument.
      (nelisp_eln_callback7_entry_word raw0 0 0 0 0 0 0))
    ;; Slot-identifying ports: each forwards the six argument registers
    ;; and replaces the seventh (stack) word with its own port tag, which
    ;; no admitted import (arity <= 6) ever reads as an argument.
    ,@(let ((forms nil) (i 0))
        (while (< i nelisp-cc-eln-callback7-port-count)
          (push `(defun ,(intern (format "nelisp_eln_callback_port%d_entry_word"
                                         i))
                     (raw0 raw1 raw2 raw3 raw4 raw5)
                   (nelisp_eln_callback7_entry_word
                    raw0 raw1 raw2 raw3 raw4 raw5
                    ,(+ nelisp-cc-eln-callback7-port-tag-base i)))
                forms)
          (setq i (1+ i)))
        (nreverse forms)))
  "AOT seven-word descriptor callback. Each activation owns a checked
root-stack argument/result pair so nested callbacks cannot overwrite an
outer call's gateway slots. The descriptor contains uninterpreted raw machine
words and is valid only until callback return. The `_entry_word' sibling
accepts only a dotted pair of two nonnegative u32 fixnums and returns their
combined raw machine word. The unary `_callback1_entry_word' wrapper supplies
six initialized zero words before delegating to that entry. These entries are
not GNU Lisp_Object decoders.  When the gateway reports a non-local exit,
the `_entry_word' path publishes it into the caller's pinned context slots
\(`nl_eln_callback7_capture_exit', Doc 207); resuming it is the Lisp
bridge's job.")

(provide 'nelisp-cc-eln-callback7)

;;; nelisp-cc-eln-callback7.el ends here
