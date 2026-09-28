;;; nelisp-cc-eln-callback.el --- scoped GNU .eln callback helpers -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; These forms are spliced into the standalone AOT unit.  The BSS records
;; retain addresses of caller-owned pinned Sexp slots, never copied objects.
(defconst nelisp-cc-eln-callback-context-capacity 8)
(defconst nelisp-cc-eln-callback-context-header-bytes 32)
(defconst nelisp-cc-eln-callback-context-record-bytes 56)
(defconst nelisp-cc-eln-callback-context-bss-bytes
  (* 32 (/ (+ nelisp-cc-eln-callback-context-header-bytes
              (* nelisp-cc-eln-callback-context-capacity
                 nelisp-cc-eln-callback-context-record-bytes)
              31)
           32)))

(defconst nelisp-cc-eln-callback--source
  `((defun nl_eln_callback_record_at (control index)
      (+ control ,nelisp-cc-eln-callback-context-header-bytes
         (* index ,nelisp-cc-eln-callback-context-record-bytes)))
    (defun nl_eln_callback_lock ()
      (while (= (atomic-compare-exchange
                 (data-addr nl_eln_callback_context) 0 1) 0)
        0)
      0)
    (defun nl_eln_callback_unlock ()
      (ptr-write-u64 (data-addr nl_eln_callback_context) 0 0)
      0)
    (defun nl_eln_callback_slot_ok (env slot)
      (if (and (> env 0)
               (= (ptr-read-u64 (data-addr nl_root_pin_control) 8) env)
               (= (nl_root_pin_slot_active slot) 1))
          1 0))
    (defun nelisp_eln_callback_context_push
        (gateway env function_slot args_slot out_slot argc)
      ;; Parameters begin in ABI argument registers. Spill all six before any
      ;; helper/syscall call so a caller-clobbered register cannot alter the
      ;; addresses used during validation or frame publication.
      (let* ((gateway-value gateway)
             (env-value env)
             (function-value function_slot)
             (args-value args_slot)
             (out-value out_slot)
             (argc-value argc))
        (nl_eln_callback_lock)
        (ptr-write-u64 (data-addr nl_eln_callback_context) 24
                       (syscall-direct 186 0 0 0 0 0 0))
        (if (and (> (ptr-read-u64 (data-addr nl_eln_callback_context) 24) 0)
                 (= gateway-value (data-addr wf_bytecode_call_gateway))
                 (= argc-value 1)
                 (< (ptr-read-u64 (data-addr nl_eln_callback_context) 16)
                    ,nelisp-cc-eln-callback-context-capacity)
                 (or (= (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 0)
                     (= (ptr-read-u64 (data-addr nl_eln_callback_context) 8)
                        (ptr-read-u64 (data-addr nl_eln_callback_context) 24)))
                 (= (nl_eln_callback_slot_ok env-value function-value) 1)
                 (= (nl_eln_callback_slot_ok env-value args-value) 1)
                 (= (nl_eln_callback_slot_ok env-value out-value) 1)
                 (/= function-value args-value)
                 (/= function-value out-value)
                 (/= args-value out-value))
            (let ((record
                   (nl_eln_callback_record_at
                    (data-addr nl_eln_callback_context)
                    (ptr-read-u64 (data-addr nl_eln_callback_context) 16))))
              (seq
               (if (= (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 0)
                   (ptr-write-u64 (data-addr nl_eln_callback_context) 8
                                  (ptr-read-u64
                                   (data-addr nl_eln_callback_context) 24))
                 0)
               (ptr-write-u64 record 0 env-value)
               (ptr-write-u64 record 8 function-value)
               (ptr-write-u64 record 16 args-value)
               (ptr-write-u64 record 24 out-value)
               (ptr-write-u64 record 32 argc-value)
               (ptr-write-u64 record 40 -1)
               (ptr-write-u64 record 48 0)
               (ptr-write-u64 (data-addr nl_eln_callback_context) 16
                              (+ (ptr-read-u64
                                  (data-addr nl_eln_callback_context) 16) 1))
               (let ((token (ptr-read-u64
                             (data-addr nl_eln_callback_context) 16)))
                 (ptr-write-u64 (data-addr nl_eln_callback_context) 24 token)
                 (nl_eln_callback_unlock)
                 token)))
          (seq (nl_eln_callback_unlock) 0))))
    (defun nelisp_eln_callback_context_status (token)
      (if (and (> (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 0)
               (<= (ptr-read-u64 (data-addr nl_eln_callback_context) 16)
                   ,nelisp-cc-eln-callback-context-capacity)
               (= token (ptr-read-u64 (data-addr nl_eln_callback_context) 16))
               (> (syscall-direct 186 0 0 0 0 0 0) 0)
               (= (syscall-direct 186 0 0 0 0 0 0)
                  (ptr-read-u64 (data-addr nl_eln_callback_context) 8)))
          (ptr-read-u64
           (nl_eln_callback_record_at
            (data-addr nl_eln_callback_context)
            (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
           40)
        -1))
    (defun nelisp_eln_callback_context_pop (token)
      (nl_eln_callback_lock)
      (ptr-write-u64 (data-addr nl_eln_callback_context) 24
                     (syscall-direct 186 0 0 0 0 0 0))
      (if (and (> (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 0)
               (<= (ptr-read-u64 (data-addr nl_eln_callback_context) 16)
                   ,nelisp-cc-eln-callback-context-capacity)
               (= token (ptr-read-u64 (data-addr nl_eln_callback_context) 16))
               (> (ptr-read-u64 (data-addr nl_eln_callback_context) 24) 0)
               (= (ptr-read-u64 (data-addr nl_eln_callback_context) 24)
                  (ptr-read-u64 (data-addr nl_eln_callback_context) 8)))
          (let ((record
                 (nl_eln_callback_record_at
                  (data-addr nl_eln_callback_context)
                  (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))))
            (seq
             (ptr-write-u64 record 0 0)
             (ptr-write-u64 record 8 0)
             (ptr-write-u64 record 16 0)
             (ptr-write-u64 record 24 0)
             (ptr-write-u64 record 32 0)
             (ptr-write-u64 record 40 -1)
             (ptr-write-u64 record 48 0)
             (ptr-write-u64 (data-addr nl_eln_callback_context) 16
                            (- (ptr-read-u64
                                (data-addr nl_eln_callback_context) 16) 1))
             (if (= (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 0)
                 (ptr-write-u64 (data-addr nl_eln_callback_context) 8 0)
               0)
             (ptr-write-u64 (data-addr nl_eln_callback_context) 24
                            (ptr-read-u64
                             (data-addr nl_eln_callback_context) 16))
             (nl_eln_callback_unlock)
             1))
        (seq (nl_eln_callback_unlock) 0)))
    (defun nelisp_eln_fixnum1_callback (raw_value)
      ;; GNU 31.1 fixnums have low tag bits 10 and a signed 62-bit payload.
      ;; Failure is side-channel state for this probe only; general GNU
      ;; nonlocal exits need a distinct handler/unwind compatibility bridge.
      (if (or (= (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 0)
              (> (ptr-read-u64 (data-addr nl_eln_callback_context) 16)
                 ,nelisp-cc-eln-callback-context-capacity)
              (= (syscall-direct 186 0 0 0 0 0 0) 0)
              (/= (syscall-direct 186 0 0 0 0 0 0)
                  (ptr-read-u64 (data-addr nl_eln_callback_context) 8)))
          raw_value
        (seq
         ;; Save the raw fallback before helper/gateway calls; only the
         ;; `status' result is live across the gateway call below.
         (ptr-write-u64
          (nl_eln_callback_record_at
           (data-addr nl_eln_callback_context)
           (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
          48 raw_value)
         (if (or (/= (ptr-read-u64
                      (nl_eln_callback_record_at
                       (data-addr nl_eln_callback_context)
                       (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                      32) 1)
                 (/= (nl_eln_callback_slot_ok
                      (ptr-read-u64
                       (nl_eln_callback_record_at
                        (data-addr nl_eln_callback_context)
                        (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                       0)
                      (ptr-read-u64
                       (nl_eln_callback_record_at
                        (data-addr nl_eln_callback_context)
                        (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                       8)) 1)
                 (/= (nl_eln_callback_slot_ok
                      (ptr-read-u64
                       (nl_eln_callback_record_at
                        (data-addr nl_eln_callback_context)
                        (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                       0)
                      (ptr-read-u64
                       (nl_eln_callback_record_at
                        (data-addr nl_eln_callback_context)
                        (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                       16)) 1)
                 (/= (nl_eln_callback_slot_ok
                      (ptr-read-u64
                       (nl_eln_callback_record_at
                        (data-addr nl_eln_callback_context)
                        (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                       0)
                      (ptr-read-u64
                       (nl_eln_callback_record_at
                        (data-addr nl_eln_callback_context)
                        (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                       24)) 1))
             (seq
              (ptr-write-u64
               (nl_eln_callback_record_at
                (data-addr nl_eln_callback_context)
                (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
               40 2)
              (ptr-read-u64
               (nl_eln_callback_record_at
                (data-addr nl_eln_callback_context)
                (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
               48))
           (if (/= (logand
                    (ptr-read-u64
                     (nl_eln_callback_record_at
                      (data-addr nl_eln_callback_context)
                      (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                     48) 3) 2)
               (seq
                (ptr-write-u64
                 (nl_eln_callback_record_at
                  (data-addr nl_eln_callback_context)
                  (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                 40 3)
                (ptr-read-u64
                 (nl_eln_callback_record_at
                  (data-addr nl_eln_callback_context)
                  (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                 48))
             (seq
              (ptr-write-u64
               (ptr-read-u64
                (nl_eln_callback_record_at
                 (data-addr nl_eln_callback_context)
                 (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                16) 0 2)
              (ptr-write-u64
               (ptr-read-u64
                (nl_eln_callback_record_at
                 (data-addr nl_eln_callback_context)
                 (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                16) 8
               (sar
                (ptr-read-u64
                 (nl_eln_callback_record_at
                  (data-addr nl_eln_callback_context)
                  (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                 48) 2))
              (ptr-write-u64
               (ptr-read-u64
                (nl_eln_callback_record_at
                 (data-addr nl_eln_callback_context)
                 (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                16) 16 0)
              (ptr-write-u64
               (ptr-read-u64
                (nl_eln_callback_record_at
                 (data-addr nl_eln_callback_context)
                 (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                16) 24 0)
              (let ((status
                     (wf_bytecode_call_gateway
                      (ptr-read-u64
                       (nl_eln_callback_record_at
                        (data-addr nl_eln_callback_context)
                        (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                       0)
                      (ptr-read-u64
                       (nl_eln_callback_record_at
                        (data-addr nl_eln_callback_context)
                        (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                       8)
                      (ptr-read-u64
                       (nl_eln_callback_record_at
                        (data-addr nl_eln_callback_context)
                        (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                       16)
                      0
                      (ptr-read-u64
                       (nl_eln_callback_record_at
                        (data-addr nl_eln_callback_context)
                        (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                       32)
                      (ptr-read-u64
                       (nl_eln_callback_record_at
                        (data-addr nl_eln_callback_context)
                        (- (ptr-read-u64 (data-addr nl_eln_callback_context) 16) 1))
                       24))))
                (let* ((control (data-addr nl_eln_callback_context))
                       (depth (ptr-read-u64 control 16))
                       (record (nl_eln_callback_record_at control (- depth 1)))
                       (fallback (ptr-read-u64 record 48)))
                  (if (/= status 0)
                      (seq (ptr-write-u64 record 40 status) fallback)
                    (let ((tag (ptr-read-u64 (ptr-read-u64 record 24) 0))
                          (result (ptr-read-u64 (ptr-read-u64 record 24) 8)))
                      (if (or (/= tag 2)
                              (< result -2305843009213693952)
                              (>= result 2305843009213693952))
                          (seq (ptr-write-u64 record 40 4) fallback)
                        (seq
                         (ptr-write-u64 record 40 0)
                         (+ (* result 4) 2))))))))))))))
  "AOT forms for a bounded, single-thread GNU fixnum callback probe.

The appended 480-byte BSS object is a lock, owner TID, depth, scratch word,
and eight 56-byte records. Each record holds env/function/args/out pointers,
argc, status, and a raw fallback word. This is not a general GNU callback or
nonlocal-exit bridge.")

(provide 'nelisp-cc-eln-callback)

;;; nelisp-cc-eln-callback.el ends here
