;;; nelisp-eln-callback7-driver.el --- seven-word callback ABI smoke -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'nelisp-native-load)
(require 'nl-ffi)
(require 'nl-ffi-loader)
(require 'nl-ffi-memory)
(require 'nelisp-eln-abi)

(defvar nelisp-callback7--owner-outer nil)
(defvar nelisp-callback7--owner-inner nil)
(defvar nelisp-callback7--shim 0)
(defvar nelisp-callback7--state 0)
(defvar nelisp-callback7--nested-return 0)
(defvar nelisp-callback7--nested-status 0)
(defvar nelisp-callback7--nested-c-root-before 0)
(defvar nelisp-callback7--nested-c-root-after 0)
(defvar nelisp-callback7--expected-outer nil)
(defvar nelisp-callback7--expected-inner nil)
(defvar nelisp-callback7--handler-depth 0)
(defvar nelisp-callback7--handler-count 0)
(defvar nelisp-callback7--fail-handler nil)

(defun nelisp-callback7--assert (condition label)
  (unless condition (error "seven-word callback smoke failed: %s" label)))

(defun nelisp-callback7--call6 (address a b c d e f)
  (ptr-call address a b c d e f))

(defun nelisp-callback7--words (descriptor)
  (let ((i 0) (result nil))
    (while (< i 7)
      (push (nelisp-eln-abi-read-word descriptor (* i 8)) result)
      (setq i (1+ i)))
    (nreverse result)))

(defun nelisp-callback7--handler (descriptor)
  (setq nelisp-callback7--handler-count
        (1+ nelisp-callback7--handler-count))
  (garbage-collect)
  (let ((expected (if (= nelisp-callback7--handler-depth 0)
                      nelisp-callback7--expected-outer
                    nelisp-callback7--expected-inner)))
    (nelisp-callback7--assert
     (equal (nelisp-callback7--words descriptor) expected)
     "all seven words, including caller-stack word seven, survive GC"))
  (if nelisp-callback7--fail-handler
      (error "seven-word callback intentional failure")
    (if (= nelisp-callback7--handler-depth 0)
        (let* ((outer-string (copy-sequence "outer-live"))
               (outer-symbol 'callback7-live)
               (outer-cons (cons outer-string outer-symbol))
               (inner-address
                (nl-ffi-memory-address nelisp-callback7--owner-inner)))
          (setq nelisp-callback7--handler-depth 1)
          (let ((nested-raw (nelisp-callback7--call6
                             nelisp-callback7--shim inner-address 0 0 0 0 0)))
            (setq nelisp-callback7--handler-depth 0
                  nelisp-callback7--nested-return
                  (nelisp-eln-abi-read-word inner-address 64)
                  nelisp-callback7--nested-status
                  (ptr-read-u64 nelisp-callback7--state 24)
                  nelisp-callback7--nested-c-root-before
                  (nelisp-eln-abi-read-word inner-address 96)
                  nelisp-callback7--nested-c-root-after
                  (nelisp-eln-abi-read-word inner-address 104))
            (garbage-collect)
            (nelisp-callback7--assert (eq outer-string (car outer-cons))
                                     "outer string remains rooted across reentry")
            (nelisp-callback7--assert (eq outer-symbol (cdr outer-cons))
                                     "outer symbol remains rooted across reentry")
            (nelisp-callback7--assert (equal outer-string "outer-live")
                                     "outer string content survives nested GC")
            (nelisp-callback7--assert (or (null nested-raw)
                                          (integerp nested-raw))
                                     "native result wrapper returns a scalar"))
          most-positive-fixnum)
      most-negative-fixnum)))

(defun nelisp-callback7--word-handler (descriptor)
  "Return one raw 64-bit word as a dotted pair of u32 fixnums."
  (setq nelisp-callback7--handler-count
        (1+ nelisp-callback7--handler-count))
  (garbage-collect)
  (let ((expected (if (= nelisp-callback7--handler-depth 0)
                      nelisp-callback7--expected-outer
                    nelisp-callback7--expected-inner)))
    (nelisp-callback7--assert
     (equal (nelisp-callback7--words descriptor) expected)
     "raw-word callback preserves all descriptor words across GC"))
  (if nelisp-callback7--fail-handler
      (error "raw-word callback intentional failure")
    (if (= nelisp-callback7--handler-depth 0)
        (progn
          (setq nelisp-callback7--handler-depth 1)
          (let ((nested-raw
                 (nelisp-callback7--call6
                  nelisp-callback7--shim
                  (nl-ffi-memory-address nelisp-callback7--owner-inner)
                  0 0 0 0 0)))
            (setq nelisp-callback7--handler-depth 0)
            (nelisp-callback7--assert
             (= (nelisp-eln-abi-normalize-word nested-raw)
                #x0123456789abcdef)
             "nested raw-word return uses its own activation record")
            (nelisp-callback7--assert
             (= (ptr-read-u64 nelisp-callback7--state 24) 0)
             "nested raw-word status is success"))
          (cons #x44332211 #x88776655))
      (cons #x89abcdef #x01234567))))

(defun nelisp-callback7--malformed-word-handler (_descriptor)
  "Return a pair with a half outside the unsigned 32-bit range."
  (cons 0 4294967296))

(let* ((outdir (getenv "OUTDIR"))
       (adapter (nl-ffi-loader-open
                 (expand-file-name "callback7-caller.so" outdir)))
       (call-address (nl-ffi-loader-symbol adapter "nelisp_eln_callback7_call"))
       (call1-address (nl-ffi-loader-symbol adapter "nelisp_eln_callback1_call"))
       (entry (nelisp-native-load--symbol-addr "nelisp_eln_callback7_entry"))
       (entry1 (nelisp-native-load--symbol-addr
                "nelisp_eln_callback1_entry_word"))
       (root-helper (nelisp-native-load--symbol-addr
                     "nelisp_eln_callback7_root_mark"))
       (state (nelisp-native-load--symbol-addr "nl_eln_callback7_context"))
       (gateway (nelisp-native-load--symbol-addr "wf_bytecode_call_gateway"))
       (env (nelisp--native-env))
       (marker (nelisp-native-load--pin-begin env))
       (function-slot nil) (args-slot nil) (out-slot nil)
       (outer-token nil) (raw-outer nil) (raw-error nil)
       (outer-owner (nl-ffi-memory-allocate 112))
       (inner-owner (nl-ffi-memory-allocate 112))
       (outer-values '(81985529216486895 18364758544493064720
                       9223372036854775808 305419896 1234605616436508552
                       16045690984833335023 1311768467463790320))
       (inner-values '(270544960 9833440827789222417
                       1147797409030816545 4886718345 12379813738877118345
                       2465395958572223729 81985529216486895)))
  (unwind-protect
      (progn
        (nelisp-callback7--assert (and (> call-address 0) (> call1-address 0)
                                       (> entry 0) (> entry1 0)
                                       (> state 0) (> gateway 0))
                                 "native addresses resolve")
        (setq nelisp-callback7--owner-outer outer-owner
              nelisp-callback7--owner-inner inner-owner
              nelisp-callback7--state state
              nelisp-callback7--shim call-address
              nelisp-callback7--expected-outer outer-values
              nelisp-callback7--expected-inner inner-values)
        (let ((i 0))
          (dolist (pair (list (cons outer-owner outer-values)
                              (cons inner-owner inner-values)))
            (let ((address (nl-ffi-memory-address (car pair))))
              (nelisp-eln-abi-write-word address 0 entry)
              (nelisp-eln-abi-write-word address 72 root-helper)
              (nelisp-eln-abi-write-word address 80 env)
              (nelisp-eln-abi-write-word address 88 0)
              (dolist (word (cdr pair))
                (nelisp-eln-abi-write-word address (* 8 (1+ i)) word)
                (setq i (1+ i)))
              (setq i 0)))
        (setq function-slot (nelisp-native-load--pin-reserve env marker)
              args-slot (nelisp-native-load--pin-reserve env marker)
              out-slot (nelisp-native-load--pin-reserve env marker))
        (dolist (owner (list outer-owner inner-owner))
          (nelisp-eln-abi-write-word
           (nl-ffi-memory-address owner) 88 out-slot))
        (nelisp-native-load-box function-slot 'nelisp-callback7--handler)
        (setq outer-token
              (nelisp-callback7--call6
               (nelisp-native-load--symbol-addr
                "nelisp_eln_callback_context_push")
               gateway env function-slot args-slot out-slot 1))
        (nelisp-callback7--assert (> outer-token 0) "rooted context activates")
        (setq raw-outer
              (nelisp-callback7--call6 call-address
               (nl-ffi-memory-address outer-owner) 0 0 0 0 0))
        (nelisp-callback7--assert
         (= (nelisp-eln-abi-read-word (nl-ffi-memory-address outer-owner) 96)
            (nelisp-eln-abi-read-word (nl-ffi-memory-address outer-owner) 104))
         "C-observed successful callback restores root top")
        (unless (= nelisp-callback7--handler-count 2)
          (error "seven-word entry did not reach nested handlers: count=%S callback-depth=%S callback-owner=%S status=%S"
                 nelisp-callback7--handler-count
                 (ptr-read-u64 state 16) (ptr-read-u64 state 8)
                 (ptr-read-u64 state 24)))
        (unless (= (nelisp-eln-abi-read-word
                    (nl-ffi-memory-address outer-owner) 64)
                   (- (ash 1 63) 2))
          (error "outer GNU raw return mismatch: got=%S expected=%S status=%S out-tag=%S out-value=%S"
                 (nelisp-eln-abi-read-word
                  (nl-ffi-memory-address outer-owner) 64)
                 (- (ash 1 63) 2) (ptr-read-u64 state 24)
                 (ptr-read-u64 out-slot 0) (ptr-read-u64 out-slot 8)))
        (unless (= nelisp-callback7--nested-c-root-before
                   nelisp-callback7--nested-c-root-after)
          (error "C-observed nested callback root top changed: before=%S after=%S"
                 nelisp-callback7--nested-c-root-before
                 nelisp-callback7--nested-c-root-after))
        (unless (= nelisp-callback7--nested-return
                   (nelisp-eln-abi-normalize-word
                    (+ (* most-negative-fixnum 4) 2)))
          (error "nested GNU raw return mismatch: got=%S expected=%S status=%S C-roots=%S->%S"
                 nelisp-callback7--nested-return
                 (nelisp-eln-abi-normalize-word
                  (+ (* most-negative-fixnum 4) 2))
                 nelisp-callback7--nested-status
                 nelisp-callback7--nested-c-root-before
                 nelisp-callback7--nested-c-root-after))
        (nelisp-callback7--assert (= nelisp-callback7--nested-status 0)
                                 "nested callback status is zero")
        (nelisp-callback7--assert (= (ptr-read-u64 state 24) 0)
                                 "success status is zero")
        (nelisp-callback7--assert (= nelisp-callback7--handler-count 2)
                                 "nested handler entered exactly twice")
        (let ((word-entry
               (nelisp-native-load--symbol-addr
                "nelisp_eln_callback7_entry_word")))
          (nelisp-eln-abi-write-word
           (nl-ffi-memory-address outer-owner) 0 word-entry)
          (nelisp-eln-abi-write-word
           (nl-ffi-memory-address inner-owner) 0 word-entry)
          (nelisp-native-load-box function-slot
                                  'nelisp-callback7--word-handler)
          (setq nelisp-callback7--handler-count 0
                nelisp-callback7--handler-depth 0)
          (setq raw-outer
                (nelisp-callback7--call6 call-address
                 (nl-ffi-memory-address outer-owner) 0 0 0 0 0))
          (nelisp-callback7--assert
           (= (nelisp-eln-abi-normalize-word raw-outer)
              #x8877665544332211)
           "paired-u32 gateway result returns exact raw GNU word")
          (nelisp-callback7--assert (= (ptr-read-u64 state 24) 0)
                                   "raw-word success status is zero")
          (nelisp-callback7--assert (= nelisp-callback7--handler-count 2)
                                   "raw-word nested handler entered twice")
          (nelisp-callback7--assert
           (= (nelisp-eln-abi-read-word
               (nl-ffi-memory-address outer-owner) 96)
              (nelisp-eln-abi-read-word
               (nl-ffi-memory-address outer-owner) 104))
           "raw-word success releases callback roots")
          (let ((address (nl-ffi-memory-address inner-owner))
                (input #x1020304050607080))
            (nelisp-eln-abi-write-word address 0 entry1)
            (nelisp-eln-abi-write-word address 8 input)
            (setq nelisp-callback7--expected-inner
                  (list input 0 0 0 0 0 0)
                  nelisp-callback7--handler-depth 1
                  nelisp-callback7--handler-count 0
                  nelisp-callback7--fail-handler nil)
            (nelisp-native-load-box function-slot 'nelisp-callback7--word-handler)
            (let ((raw (nelisp-callback7--call6 call1-address address 0 0 0 0 0)))
              (nelisp-callback7--assert
               (= (nelisp-eln-abi-normalize-word raw) #x0123456789abcdef)
               "unary entry forwards one raw word and preserves paired result")
              (nelisp-callback7--assert
               (= (nelisp-eln-abi-read-word address 64) #x0123456789abcdef)
               "unary C caller observes full-width result")
              (nelisp-callback7--assert (= (ptr-read-u64 state 24) 0)
                                       "unary callback status is success")
              (nelisp-callback7--assert (= nelisp-callback7--handler-count 1)
                                       "unary callback enters once"))
            (setq nelisp-callback7--fail-handler t)
            (ptr-write-u64 state 24 0)
            (let ((raw (nelisp-callback7--call6 call1-address address 0 0 0 0 0)))
              (nelisp-callback7--assert (= (nelisp-eln-abi-normalize-word raw) 0)
                                       "unary callback error returns refusal sentinel")
              (nelisp-callback7--assert (= (ptr-read-u64 state 24) 1)
                                       "unary callback error status is preserved"))
            (setq nelisp-callback7--handler-depth 0
                  nelisp-callback7--handler-count 0
                  nelisp-callback7--expected-inner inner-values
                  nelisp-callback7--fail-handler nil)
            (princ "ELN_CALLBACK1_PASS argc=1 zero_tail=6 error_status=1\n"))
          (nelisp-native-load-box function-slot
                                  'nelisp-callback7--malformed-word-handler)
          (setq raw-error
                (nelisp-callback7--call6 call-address
                 (nl-ffi-memory-address outer-owner) 0 0 0 0 0))
          (nelisp-callback7--assert
           (= (nelisp-eln-abi-normalize-word raw-error) 0)
           "malformed paired-u32 result returns refusal sentinel")
          (nelisp-callback7--assert (= (ptr-read-u64 state 24) 4)
                                   "malformed raw word has nonzero status")
          (nelisp-native-load-box function-slot
                                  'nelisp-callback7--word-handler)
          (setq nelisp-callback7--fail-handler t)
          (setq raw-error
                (nelisp-callback7--call6 call-address
                 (nl-ffi-memory-address outer-owner) 0 0 0 0 0))
          (nelisp-callback7--assert
           (= (nelisp-eln-abi-normalize-word raw-error) 0)
           "callback signal is not disguised as raw GNU word")
          (nelisp-callback7--assert (= (ptr-read-u64 state 24) 1)
                                   "callback error status remains nonzero")
          (setq nelisp-callback7--fail-handler nil)
          (nelisp-eln-abi-write-word
           (nl-ffi-memory-address outer-owner) 0 entry)
          (nelisp-eln-abi-write-word
           (nl-ffi-memory-address inner-owner) 0 entry)
          (nelisp-native-load-box function-slot 'nelisp-callback7--handler))
        (setq nelisp-callback7--fail-handler t
              nelisp-callback7--handler-count 0)
        (setq raw-error
              (nelisp-callback7--call6 call-address
               (nl-ffi-memory-address outer-owner) 0 0 0 0 0))
        (nelisp-callback7--assert
         (= (nelisp-eln-abi-read-word (nl-ffi-memory-address outer-owner) 96)
            (nelisp-eln-abi-read-word (nl-ffi-memory-address outer-owner) 104))
         "C-observed error callback restores root top")
        (nelisp-callback7--assert (= (nelisp-eln-abi-normalize-word raw-error) 0)
                                 "error returns non-object nil sentinel")
        (nelisp-callback7--assert (= (ptr-read-u64 state 24) 1)
                                 "gateway error remains status 1")
        (nelisp-callback7--assert (= nelisp-callback7--handler-count 1)
                                 "error is not replayed")
        (nelisp-callback7--assert
         (= (nelisp-callback7--call6
             (nelisp-native-load--symbol-addr
              "nelisp_eln_callback_context_pop") outer-token 0 0 0 0 0)
            1)
         "rooted context releases")
        (setq outer-token nil)
        (nelisp-native-load--pin-end env marker)
        (setq marker nil)
        (setq nelisp-callback7--fail-handler nil
              nelisp-callback7--handler-count 0)
        (let ((inactive-raw
               (nelisp-callback7--call6 call-address
                (nl-ffi-memory-address outer-owner) 0 0 0 0 0)))
          (nelisp-callback7--assert
           (= (nelisp-eln-abi-normalize-word inactive-raw) 0)
           "inactive entry returns refusal sentinel")
          (nelisp-callback7--assert (= nelisp-callback7--handler-count 0)
                                   "inactive context has no Lisp effects"))
        (let ((word-entry
               (nelisp-native-load--symbol-addr
                "nelisp_eln_callback7_entry_word")))
          (nelisp-eln-abi-write-word
           (nl-ffi-memory-address outer-owner) 0 word-entry)
          (setq raw-error
                (nelisp-callback7--call6 call-address
                 (nl-ffi-memory-address outer-owner) 0 0 0 0 0))
          (nelisp-callback7--assert
           (= (nelisp-eln-abi-normalize-word raw-error) 0)
           "inactive raw-word callback returns refusal sentinel")
          (nelisp-callback7--assert (= (ptr-read-u64 state 24) 1)
                                   "inactive raw-word callback publishes failure")
          (nelisp-callback7--assert (= nelisp-callback7--handler-count 0)
                                   "inactive raw-word callback has no effects"))
        (princ "ELN_CALLBACK7_PASS full_width=2 nested=1 error_status=1 inactive=1\n"))
    (when outer-token
      (nelisp-callback7--call6
       (nelisp-native-load--symbol-addr
        "nelisp_eln_callback_context_pop") outer-token 0 0 0 0 0))
    (when marker (nelisp-native-load--pin-end env marker))
    (nl-ffi-memory-release inner-owner)
    (nl-ffi-memory-release outer-owner))))

;;; nelisp-eln-callback7-driver.el ends here
