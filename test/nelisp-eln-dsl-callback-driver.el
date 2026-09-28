;;; nelisp-eln-dsl-callback-driver.el --- native DSL callback smoke -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'nelisp-native-load)
(require 'nl-ffi)

(defvar nelisp-eln-dsl-callback--env nil)
(defvar nelisp-eln-dsl-callback--marker nil)
(defvar nelisp-eln-dsl-callback--entry nil)
(defvar nelisp-eln-dsl-callback--gateway nil)
(defvar nelisp-eln-dsl-callback--function-slot nil)
(defvar nelisp-eln-dsl-callback--args nil)
(defvar nelisp-eln-dsl-callback--out nil)
(defvar nelisp-eln-dsl-callback--context nil)
(defvar nelisp-eln-dsl-callback--depth 0)
(defvar nelisp-eln-dsl-callback--calls 0)

(defun nelisp-eln-dsl-callback--assert (label condition)
  (unless condition
    (error "ELN DSL callback smoke failed: %s" label)))

(defun nelisp-eln-dsl-callback--call (name a b c d e f)
  (ptr-call (nelisp-native-load--symbol-addr name) a b c d e f))

(defun nelisp-eln-dsl-callback--slot (record offset)
  (ptr-read-u64 record offset))

(defun nelisp-eln-dsl-callback-random-impl (limit)
  "Force GC and enter the same .eln recursively once to test LIFO restore."
  (unless (= limit 1)
    (error "DSL callback fixture accepts only limit 1"))
  (if (= nelisp-eln-dsl-callback--depth 0)
      (progn
        (setq nelisp-eln-dsl-callback--depth 1)
        (let* ((args (nelisp-native-load--pin-reserve
                      nelisp-eln-dsl-callback--env
                      nelisp-eln-dsl-callback--marker))
               (out (nelisp-native-load--pin-reserve
                     nelisp-eln-dsl-callback--env
                     nelisp-eln-dsl-callback--marker))
               (outer-record (+ nelisp-eln-dsl-callback--context 32))
               (token (nelisp-eln-dsl-callback--call
                       "nelisp_eln_callback_context_push"
                       nelisp-eln-dsl-callback--gateway
                       nelisp-eln-dsl-callback--env
                       nelisp-eln-dsl-callback--function-slot args out 1))
               (inner-record (+ nelisp-eln-dsl-callback--context 88))
               (raw (ptr-call nelisp-eln-dsl-callback--entry
                              (+ (* limit 4) 2) 0 0 0 0 0))
               (status (nelisp-eln-dsl-callback--call
                        "nelisp_eln_callback_context_status"
                        token 0 0 0 0 0)))
          (nelisp-eln-dsl-callback--assert "nested token is depth two"
                                           (= token 2))
          (nelisp-eln-dsl-callback--assert
           "outer record retained while nested"
           (and (= (nelisp-eln-dsl-callback--slot outer-record 0)
                   nelisp-eln-dsl-callback--env)
                (= (nelisp-eln-dsl-callback--slot outer-record 8)
                   nelisp-eln-dsl-callback--function-slot)
                (= (nelisp-eln-dsl-callback--slot outer-record 16)
                   nelisp-eln-dsl-callback--args)
                (= (nelisp-eln-dsl-callback--slot outer-record 24)
                   nelisp-eln-dsl-callback--out)
                (= (nelisp-eln-dsl-callback--slot outer-record 32) 1)
                (= (nelisp-eln-dsl-callback--slot outer-record 40) -1)
                (= (nelisp-eln-dsl-callback--slot outer-record 48) 6)))
          (nelisp-eln-dsl-callback--assert "inner record starts at stride 56"
                                           (= inner-record
                                              (+ outer-record 56)))
          (nelisp-eln-dsl-callback--assert
           "inner record stores its own rooted frame"
           (and (= (nelisp-eln-dsl-callback--slot inner-record 0)
                   nelisp-eln-dsl-callback--env)
                (= (nelisp-eln-dsl-callback--slot inner-record 8)
                   nelisp-eln-dsl-callback--function-slot)
                (= (nelisp-eln-dsl-callback--slot inner-record 16) args)
                (= (nelisp-eln-dsl-callback--slot inner-record 24) out)
                (= (nelisp-eln-dsl-callback--slot inner-record 32) 1)
                (= (nelisp-eln-dsl-callback--slot inner-record 48) 6)))
          (nelisp-eln-dsl-callback--assert
           "inner gateway result" (and (= status 0)
                                        (= (logand raw 3) 2)
                                        (= (ash raw -2) 0)))
          (nelisp-eln-dsl-callback--assert
           "nested pop succeeds"
           (= (nelisp-eln-dsl-callback--call
               "nelisp_eln_callback_context_pop" token 0 0 0 0 0) 1))
          (nelisp-eln-dsl-callback--assert "depth restored to outer"
                                           (= (ptr-read-u64
                                               nelisp-eln-dsl-callback--context 16)
                                              1))
          (nelisp-eln-dsl-callback--assert
           "popped inner record cleared without touching outer"
           (and (= (nelisp-eln-dsl-callback--slot inner-record 0) 0)
                (= (nelisp-eln-dsl-callback--slot inner-record 8) 0)
                (= (nelisp-eln-dsl-callback--slot inner-record 16) 0)
                (= (nelisp-eln-dsl-callback--slot inner-record 24) 0)
                (= (nelisp-eln-dsl-callback--slot inner-record 32) 0)
                (= (nelisp-eln-dsl-callback--slot inner-record 40) -1)
                (= (nelisp-eln-dsl-callback--slot inner-record 48) 0)
                (= (nelisp-eln-dsl-callback--slot outer-record 0)
                   nelisp-eln-dsl-callback--env)))
          (setq nelisp-eln-dsl-callback--depth 0)
          (garbage-collect)
          (setq nelisp-eln-dsl-callback--calls
                (1+ nelisp-eln-dsl-callback--calls)))
        0)
    (progn
      (garbage-collect)
      (setq nelisp-eln-dsl-callback--calls
            (1+ nelisp-eln-dsl-callback--calls))
      0)))

(defun nelisp-eln-dsl-callback--read-abi-hash (handle expected)
  (let* ((addr (nl-ffi-loader-symbol handle "freloc_hash_blob"))
         (len (and (> addr 0) (ptr-read-u64 addr 0)))
         (bytes (and len (make-string len 0))))
    (nelisp-eln-dsl-callback--assert "GNU ABI hash blob"
                                     (and len (= len (+ (length expected) 3))))
    (dotimes (i len)
      (aset bytes i (ptr-read-u8 addr (+ 8 i))))
    (nelisp-eln-dsl-callback--assert
     "GNU ABI hash matches producer"
     (and (= (aref bytes 0) 34)
          (= (aref bytes (- len 2)) 34)
          (= (aref bytes (1- len)) 0)
          (string= (substring bytes 1 (- len 2)) expected)))))

(let* ((outdir (getenv "OUTDIR"))
       (eln (nl-ffi-loader-open (expand-file-name "callback-random.eln" outdir)))
       (slot (string-to-number (getenv "RANDOM_HELPER_SLOT")))
       (subr-index (string-to-number (getenv "RANDOM_SUBR_INDEX")))
       (entry (nl-ffi-loader-symbol eln (getenv "ELN_SYMBOL")))
       (link-slot (nl-ffi-loader-symbol eln "freloc_link_table"))
       (callback (nelisp-native-load--symbol-addr "nelisp_eln_fixnum1_callback"))
       (table (alloc-bytes (* 8 (1+ slot)) 8))
       (env (nelisp--native-env))
       (marker (nelisp-native-load--pin-begin env))
       (function-slot nil)
       (args nil)
       (out nil)
       (wrong-function-slot nil)
       (gateway (nelisp-native-load--symbol-addr "wf_bytecode_call_gateway"))
       (context (nelisp-native-load--symbol-addr "nl_eln_callback_context"))
       (token nil)
       (raw nil)
       (status nil))
  (unwind-protect
      (progn
        (nelisp-eln-dsl-callback--read-abi-hash eln (getenv "HOST_ABI_HASH"))
        (nelisp-eln-dsl-callback--assert
         "helper slot from producer metadata" (= slot (+ 15 subr-index)))
        (nelisp-eln-dsl-callback--assert
         "ELN/runtime symbols resolved"
         (and (> entry 0) (> link-slot 0) (> callback 0) (> context 0)))
        (let ((i 0))
          (while (< i (1+ slot))
            (ptr-write-u64 table (* i 8) 0)
            (setq i (1+ i))))
        (ptr-write-u64 table (* slot 8) callback)
        (ptr-write-u64 link-slot 0 table)
        (setq function-slot (nelisp--native-pin-copy env marker 'random)
              args (nelisp-native-load--pin-reserve env marker)
              out (nelisp-native-load--pin-reserve env marker))
        (nelisp-eln-dsl-callback--assert
         "pin API returned nonzero env and distinct slots"
         (and (> env 0) (> function-slot 0) (> args 0) (> out 0)
              (/= function-slot args) (/= args out) (/= function-slot out)))
        (nelisp-native-load-box out 777 env marker)
        (setq nelisp-eln-dsl-callback--env env
              nelisp-eln-dsl-callback--marker marker
              nelisp-eln-dsl-callback--entry entry
              nelisp-eln-dsl-callback--gateway gateway
              nelisp-eln-dsl-callback--function-slot function-slot
              nelisp-eln-dsl-callback--args args
              nelisp-eln-dsl-callback--out out
              nelisp-eln-dsl-callback--context context
              nelisp-eln-dsl-callback--depth 0
              nelisp-eln-dsl-callback--calls 0)
        (fset 'random (function nelisp-eln-dsl-callback-random-impl))
        (nelisp-native-load-box args 1 env marker)

        ;; The caller's outer record is visible at index 0 while the Lisp
        ;; wrapper recursively pushes and pops index 1 around the same .eln.
        (setq token (nelisp-eln-dsl-callback--call
                     "nelisp_eln_callback_context_push"
                     gateway env function-slot args out 1))
        (nelisp-eln-dsl-callback--assert
         "outer push released lock"
         (= (ptr-read-u64 context 0) 0))
        (nelisp-eln-dsl-callback--assert
         "outer push recorded caller thread" (> (ptr-read-u64 context 24) 0))
        (nelisp-eln-dsl-callback--assert
         "outer push published depth one" (= (ptr-read-u64 context 16) 1))
        (nelisp-eln-dsl-callback--assert
         "outer push published owner" (> (ptr-read-u64 context 8) 0))
        (nelisp-eln-dsl-callback--assert "outer context token is depth one"
                                         (= token 1))
        (setq raw (ptr-call entry 6 0 0 0 0 0))
        (setq status (nelisp-eln-dsl-callback--call
                      "nelisp_eln_callback_context_status"
                      token 0 0 0 0 0))
        (nelisp-eln-dsl-callback--assert
         "native .eln -> DSL gateway -> nested .eln"
         (and (= status 0) (= (logand raw 3) 2) (= (ash raw -2) 0)
              (= nelisp-eln-dsl-callback--calls 2)
              (= (nelisp-native-load-unbox out env marker) 0)
              (= (ptr-read-u64 context 16) 1)))
        (nelisp-eln-dsl-callback--assert
         "outer context pop succeeds"
         (= (nelisp-eln-dsl-callback--call
             "nelisp_eln_callback_context_pop" token 0 0 0 0 0) 1))

        ;; A non-fixnum raw callback argument fails closed before gateway call.
        (setq token (nelisp-eln-dsl-callback--call
                     "nelisp_eln_callback_context_push"
                     gateway env function-slot args out 1))
        (nelisp-native-load-box out 777 env marker)
        (setq raw (ptr-call callback 0 0 0 0 0 0)
              status (nelisp-eln-dsl-callback--call
                      "nelisp_eln_callback_context_status" token 0 0 0 0 0))
        (nelisp-eln-dsl-callback--assert
         "non-fixnum callback is rejected without changing result"
         (and (= raw 0) (= status 3)
              (= (nelisp-native-load-unbox out env marker) 777)))
        (nelisp-eln-dsl-callback--call
         "nelisp_eln_callback_context_pop" token 0 0 0 0 0)

        ;; Invalid slot and wrong gateway values are refused before install.
        (nelisp-eln-dsl-callback--assert
         "invalid output slot rejected"
         (= (nelisp-eln-dsl-callback--call
             "nelisp_eln_callback_context_push"
             gateway env function-slot args 0 1) 0))
        (nelisp-eln-dsl-callback--assert "invalid slot leaves stack empty"
                                         (= (ptr-read-u64 context 16) 0))
        (nelisp-eln-dsl-callback--assert
         "wrong gateway address rejected"
         (= (nelisp-eln-dsl-callback--call
             "nelisp_eln_callback_context_push"
             (+ gateway 1) env function-slot args out 1) 0))
        (nelisp-eln-dsl-callback--assert "wrong gateway leaves stack empty"
                                         (= (ptr-read-u64 context 16) 0))

        ;; Owner TID mismatch must refuse both observation and pop. Restoring
        ;; the original owner leaves the active record available for cleanup.
        (setq token (nelisp-eln-dsl-callback--call
                     "nelisp_eln_callback_context_push"
                     gateway env function-slot args out 1))
        (let ((owner (ptr-read-u64 context 8)))
          (ptr-write-u64 context 8 (+ owner 1))
          (nelisp-eln-dsl-callback--assert
           "foreign owner cannot read or pop"
           (and (= (nelisp-eln-dsl-callback--call
                    "nelisp_eln_callback_context_status"
                    token 0 0 0 0 0) -1)
                (= (nelisp-eln-dsl-callback--call
                    "nelisp_eln_callback_context_pop"
                    token 0 0 0 0 0) 0)))
          (ptr-write-u64 context 8 owner))
        (nelisp-eln-dsl-callback--assert
         "restored owner pops context"
         (= (nelisp-eln-dsl-callback--call
             "nelisp_eln_callback_context_pop" token 0 0 0 0 0) 1))

        ;; Overflow and non-LIFO cleanup leave every earlier record intact.
        (let ((tokens nil) (i 0))
          (while (< i 8)
            (setq tokens
                  (cons (nelisp-eln-dsl-callback--call
                         "nelisp_eln_callback_context_push"
                         gateway env function-slot args out 1)
                        tokens)
                  i (1+ i)))
          (nelisp-eln-dsl-callback--assert
           "eight contexts fill the bounded stack"
           (and (= (car tokens) 8) (= (ptr-read-u64 context 16) 8)))
          (nelisp-eln-dsl-callback--assert
           "ninth context rejected"
           (= (nelisp-eln-dsl-callback--call
               "nelisp_eln_callback_context_push"
               gateway env function-slot args out 1) 0))
          (nelisp-eln-dsl-callback--assert
           "non-LIFO pop rejected without mutation"
           (= (nelisp-eln-dsl-callback--call
               "nelisp_eln_callback_context_pop" 1 0 0 0 0 0) 0))
          (dolist (saved tokens)
            (nelisp-eln-dsl-callback--assert "LIFO pop succeeds"
                                             (= (nelisp-eln-dsl-callback--call
                                                 "nelisp_eln_callback_context_pop"
                                                 saved 0 0 0 0 0)
                                                1)))
          (nelisp-eln-dsl-callback--assert "overflow cleanup leaves depth zero"
                                           (= (ptr-read-u64 context 16) 0)))

        ;; Keep argc=1; the callee itself requires two arguments to trigger
        ;; the gateway's real wrong-number-of-arguments status.
        (fset 'nelisp-eln-dsl-callback-wrong-arity
              (lambda (x y) (+ x y)))
        (setq wrong-function-slot
              (nelisp--native-pin-copy
               env marker 'nelisp-eln-dsl-callback-wrong-arity))
        (nelisp-native-load-box out 777 env marker)
        (setq token (nelisp-eln-dsl-callback--call
                     "nelisp_eln_callback_context_push"
                     gateway env wrong-function-slot args out 1))
        (setq raw (ptr-call entry 6 0 0 0 0 0)
              status (nelisp-eln-dsl-callback--call
                      "nelisp_eln_callback_context_status"
                      token 0 0 0 0 0))
        (nelisp-eln-dsl-callback--assert
         "wrong-arity gateway status and output"
         (and (= token 1) (= raw 6) (= status 1)
              (= (nelisp-native-load-unbox out env marker) 777)))
        (nelisp-eln-dsl-callback--assert
         "wrong-arity record status and fallback"
         (and (= (nelisp-eln-dsl-callback--slot (+ context 32) 40) 1)
              (= (nelisp-eln-dsl-callback--slot (+ context 32) 48) 6)))
        (nelisp-eln-dsl-callback--assert
         "wrong-arity signal stash flag and symbol"
         (and (= (ptr-read-u64
                  (ptr-read-u64 (nelisp-native-load--symbol-addr
                                 "nl_arena_base") 0) 16) 1)
              (eq (nelisp-native-load-unbox
                   (+ (ptr-read-u64 (nelisp-native-load--symbol-addr
                                     "nl_arena_base") 0) 24)
                   env marker)
                  'wrong-number-of-arguments)))
        (nelisp-eln-dsl-callback--assert
         "error context pop succeeds"
         (= (nelisp-eln-dsl-callback--call
             "nelisp_eln_callback_context_pop" token 0 0 0 0 0) 1))
        (princ (format "ELN_DSL_CALLBACK_PASS slot=%d subr-index=%d success_status=0 negative_arity_status=%d calls=%d\n"
                       slot subr-index status nelisp-eln-dsl-callback--calls)))
    (when (and marker (> env 0))
      (nelisp-native-load--pin-end env marker))))

;;; nelisp-eln-dsl-callback-driver.el ends here
