;;; nelisp-eln-callback-driver.el --- incoming callback smoke -*- lexical-binding: t; -*-

(require 'nl-ffi)
(require 'nl-ffi-loader)
(require 'nelisp-native-load)

(defvar nelisp-p5-callback--env nil)
(defvar nelisp-p5-callback--marker nil)
(defvar nelisp-p5-callback--entry nil)
(defvar nelisp-p5-callback--adapter nil)
(defvar nelisp-p5-callback--gateway nil)
(defvar nelisp-p5-callback--function-slot nil)
(defvar nelisp-p5-callback--depth 0)
(defvar nelisp-p5-callback--calls 0)

(defun nelisp-p5-callback--adapter-call (name a b c d e f)
  (ptr-call (nl-ffi-loader-symbol nelisp-p5-callback--adapter name)
            a b c d e f))

(defun nelisp-p5-callback--assert (label value)
  (unless value
    (error "ELN callback smoke failed: %s" label)))

(defun nelisp-p5-callback-random-impl (limit)
  "Fixture-only NeLisp replacement: accept exactly one, force GC, return zero.
The first invocation performs a nested native call to verify LIFO context
restoration; the nested invocation only forces GC and returns zero."
  (unless (= limit 1)
    (error "callback fixture accepts only limit 1"))
  (if (= nelisp-p5-callback--depth 0)
      (progn
       (setq nelisp-p5-callback--depth 1)
       (let* ((args (nelisp-native-load--pin-reserve
                     nelisp-p5-callback--env nelisp-p5-callback--marker))
              (out (nelisp-native-load--pin-reserve
                    nelisp-p5-callback--env nelisp-p5-callback--marker))
              (token
               (nelisp-p5-callback--adapter-call
                "nelisp_p5_callback_context_push"
                nelisp-p5-callback--gateway nelisp-p5-callback--env
                nelisp-p5-callback--function-slot args out 1))
              (raw (ptr-call nelisp-p5-callback--entry
                             (+ (* limit 4) 2) 0 0 0 0 0))
              (status
               (nelisp-p5-callback--adapter-call
                "nelisp_p5_callback_context_status" token 0 0 0 0 0)))
         (nelisp-p5-callback--assert
          "nested callback status/result" (and (> token 0) (= status 0)
                                                (= (logand raw 3) 2)
                                                (= (ash raw -2) 0)))
         (nelisp-p5-callback--assert
          "nested callback pop"
          (= (nelisp-p5-callback--adapter-call
              "nelisp_p5_callback_context_pop" token 0 0 0 0 0)
             1)))
       (setq nelisp-p5-callback--depth 0)
       (garbage-collect)
       (setq nelisp-p5-callback--calls
             (1+ nelisp-p5-callback--calls))
       0)
    (progn
     (garbage-collect)
     (setq nelisp-p5-callback--calls
           (1+ nelisp-p5-callback--calls))
     0)))

(defun nelisp-p5-callback--read-eln-hash (handle expected)
  (let* ((addr (nl-ffi-loader-symbol handle "freloc_hash_blob"))
         (len (and (> addr 0) (ptr-read-u64 addr 0)))
         (bytes (and len (make-string len 0))))
    (nelisp-p5-callback--assert "GNU ABI hash blob"
                                (and len (= len (+ (length expected) 3))))
    (dotimes (i len)
      (aset bytes i (ptr-read-u8 addr (+ 8 i))))
    (nelisp-p5-callback--assert
     "GNU ABI hash match"
     (and (= (aref bytes 0) 34)
          (= (aref bytes (- len 2)) 34)
          (= (aref bytes (1- len)) 0)
          (string= (substring bytes 1 (- len 2)) expected)))))

(let* ((outdir (getenv "OUTDIR"))
       (eln-path (expand-file-name "callback-random.eln" outdir))
       (adapter-path (expand-file-name "callback-adapter.so" outdir))
       (eln (nl-ffi-loader-open eln-path))
       (adapter (nl-ffi-loader-open adapter-path))
       (slot (string-to-number (getenv "RANDOM_HELPER_SLOT")))
       (subr-index (string-to-number (getenv "RANDOM_SUBR_INDEX")))
       (function-name (getenv "ELN_SYMBOL"))
       (entry (nl-ffi-loader-symbol eln function-name))
       (link-slot (nl-ffi-loader-symbol eln "freloc_link_table"))
       (callback (nl-ffi-loader-symbol adapter "nelisp_eln_random_callback"))
       (table (alloc-bytes (* 8 (1+ slot)) 8))
       (env (nelisp--native-env))
       (marker (nelisp-native-load--pin-begin env))
       (function-slot nil)
       (args nil)
       (second-arg nil)
       (out nil)
       (gateway (nelisp-native-load--symbol-addr "wf_bytecode_call_gateway"))
       (token nil)
       (status nil)
       (raw nil)
       (arena-base nil))
  (unwind-protect
      (progn
        (nelisp-p5-callback--read-eln-hash
         eln (getenv "HOST_ABI_HASH"))
        (nelisp-p5-callback--assert
         "random helper slot from producer metadata"
         (= slot (+ 15 subr-index)))
        (nelisp-p5-callback--assert "ELN symbols resolved"
                                    (and (> entry 0) (> link-slot 0)
                                         (> callback 0)))
        (let ((i 0))
          (while (< i (1+ slot))
            (ptr-write-u64 table (* i 8) 0)
            (setq i (1+ i))))
        ;; This is the real artifact's per-unit freloc slot, set to the
        ;; fixture's native callback adapter for this one primitive.
        (ptr-write-u64 table (* slot 8) callback)
        (ptr-write-u64 link-slot 0 table)
        (setq function-slot (nelisp--native-pin-copy env marker 'random))
        (setq args (nelisp-native-load--pin-reserve env marker))
        (setq second-arg (nelisp-native-load--pin-reserve env marker))
        (setq out (nelisp-native-load--pin-reserve env marker))
        (nelisp-p5-callback--assert
         "pin slots are contiguous"
         (and (= args (+ function-slot 32))
              (= second-arg (+ args 32))
              (= out (+ second-arg 32))))
        (nelisp-native-load-box second-arg 99 env marker)
        (nelisp-native-load-box out 777 env marker)
        (setq nelisp-p5-callback--env env
              nelisp-p5-callback--marker marker
              nelisp-p5-callback--entry entry
              nelisp-p5-callback--adapter adapter
              nelisp-p5-callback--gateway gateway
              nelisp-p5-callback--function-slot function-slot)
        (fset 'random (function nelisp-p5-callback-random-impl))
        (setq nelisp-p5-callback--depth 0
              nelisp-p5-callback--calls 0)
        (nelisp-native-load-box args 1 env marker)
        (setq token
              (nelisp-p5-callback--adapter-call
               "nelisp_p5_callback_context_push"
               gateway env function-slot args out 1))
        (nelisp-p5-callback--assert "outer callback context pushed" (> token 0))
        (setq raw (ptr-call entry 6 0 0 0 0 0))
        (setq status
              (nelisp-p5-callback--adapter-call
               "nelisp_p5_callback_context_status" token 0 0 0 0 0))
        (nelisp-p5-callback--assert
         "native .eln -> NeLisp gateway result"
         (and (= status 0) (= (logand raw 3) 2) (= (ash raw -2) 0)
              (= nelisp-p5-callback--calls 2)
              (= (nelisp-native-load-unbox out env marker) 0)))
        (nelisp-p5-callback--assert
         "outer callback context popped"
         (= (nelisp-p5-callback--adapter-call
             "nelisp_p5_callback_context_pop" token 0 0 0 0 0)
            1))
        ;; A wrong-arity gateway result stays a nonzero side-channel status;
        ;; do not decode the native return word on this failure path.
        (setq token
              (nelisp-p5-callback--adapter-call
               "nelisp_p5_callback_context_push"
               gateway env function-slot args out 2))
        (nelisp-native-load-box out 777 env marker)
        (setq raw (ptr-call entry 6 0 0 0 0 0))
        (setq status
              (nelisp-p5-callback--adapter-call
               "nelisp_p5_callback_context_status" token 0 0 0 0 0))
        (setq arena-base (ptr-read-u64
                          (nelisp-native-load--symbol-addr "nl_arena_base") 0))
        (nelisp-p5-callback--assert
         "native callback exposes gateway arity error"
         (and (= status 1)
              (= (nelisp-native-load-unbox out env marker) 777)
              (= (ptr-read-u64 arena-base 16) 1)
              (eq (nelisp-native-load-unbox (+ arena-base 24) env marker)
                  'wrong-number-of-arguments)
              (= nelisp-p5-callback--calls 2)))
        (nelisp-p5-callback--assert
         "error context popped"
         (= (nelisp-p5-callback--adapter-call
             "nelisp_p5_callback_context_pop" token 0 0 0 0 0)
            1))
        (princ (format "ELN_CALLBACK_PASS slot=%d subr-index=%d success_status=0 negative_arity_status=%d calls=%d\n"
                       slot subr-index status nelisp-p5-callback--calls))
        nil)
    (when (and marker (> env 0))
      (nelisp-native-load--pin-end env marker))))
