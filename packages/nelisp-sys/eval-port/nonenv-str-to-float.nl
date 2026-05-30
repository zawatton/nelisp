;; Doc 136 nonenv-str-to-float.nl — nl_str_to_float standalone implementation
;; STAGED (unwired — do NOT touch manifest/build.rs)
;; Type-checked + object-compiled.  Not linked.
;;
;; Replaces the deleted Rust #[no_mangle] body:
;;   nl_str_to_float(bytes_ptr: usize, len: i64, slot: usize) -> i64
;;   On success: write Sexp::Float(parsed_f64) into *slot, return 1.
;;   On parse error: write Sexp::Nil into *slot, return 0.
;;
;; ---------------------------------------------------------------------------
;; Implementation strategy
;; ---------------------------------------------------------------------------
;;
;; Parse bytes[0..len] as decimal float:
;;   ['-'] digit+ ['.' digit*] [('e'|'E') ['+'|'-'] digit+]
;;
;; After parsing:
;;   mant_i64 = int_val * 10^frac_cnt + frac_val
;;   net_exp  = exp_signed - frac_cnt
;;
;; For positive net_exp: result = sign * mant * 10^net_exp (integer, then f64)
;; For negative net_exp: result = (sign * mant) / 10^|net_exp| (f64 division)
;;
;; Each sys:defun has at most 6 params (backend limit, nelisp-sys §MVP).
;; f64 arithmetic uses sys:i64->f64 as direct f64-binop operands only —
;; no f64 values in let-bindings, function args, or if-arm results.
;;
;; ---------------------------------------------------------------------------
;; APPROXIMATION FLAGS
;; ---------------------------------------------------------------------------
;;   [A] Mantissa overflow: mant_i64 overflows i64 for total digits > 18.
;;       Typical Elisp float literals (≤ 15 sig-digits) are fine.
;;
;;   [B] 10^n: computed as integer nl_stf_ipow10, then one i64->f64 cast.
;;       Not IEEE-correctly-rounded (task allowance: "approximate").
;;       Integer pow overflows i64 for n > 18.
;;
;;   [C] |net_exp| > 18 → overflow in nl_stf_ipow10 (i64 wraps).
;;       NaN/Inf strings not parsed.
;;
;;   [D] ABI for nl_sexp_write_float f64 arg: nelisp-sys backend lowers
;;       the call as a gp-class call.  Phase-47 emit-value puts the
;;       f64 result in xmm0 but the call spill pushes rax (stale) → wrong
;;       register at runtime.  The .o IS produced; symbol nl_str_to_float
;;       is defined.  Deferred: Phase-47 mixed-ABI extern-call wiring.
;;
;; ---------------------------------------------------------------------------
;; Compiler prerequisite (Doc 136 §fix):
;;   nelisp-phase47-compiler--emit-value gained a tag-28 (i64-to-f64) arm
;;   that emits CVTSI2SD xmm0, rax for the x86_64 path.  Without it, any
;;   sys:i64->f64 in call-arg or if-arm position signals :unknown-value-kind.
;;
;; nm verification (libnelisp_elisp_spike.a):
;;   nelisp_ptr_read_u8   T  — byte reader
;;   nl_sexp_write_float  T  — Sexp::Float slot writer (f64 ABI)
;;   nl_str_to_float      U  — symbol we define here
;;
;; NO *.rs files edited.  Not committed.  Not wired.

;; ---------------------------------------------------------------------------
;; sys:extern declarations
;; ---------------------------------------------------------------------------

;; nelisp_ptr_read_u8(base_ptr, offset) -> i64
;; Zero-extended byte: MOVZX RAX, BYTE [base+offset].
(sys:extern nelisp_ptr_read_u8
  (:symbol "nelisp_ptr_read_u8" :abi c :unsafe t)
  ((base_ptr usize) (offset i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; nl_sexp_write_float(slot, val) -> usize
;; C ABI: rdi=slot, xmm0=val.  Writes tag=3 and f64 bits.  T in archive.
;; APPROXIMATION FLAG [D]: f64 arg passed via gp-class path — see header.
(sys:extern nl_sexp_write_float
  (:symbol "nl_sexp_write_float" :abi c :unsafe t)
  ((slot usize) (val f64))
  usize
  (:alloc none :ffi may :unsafe may))

;; ---------------------------------------------------------------------------
;; nl_stf_write_nil(slot) -> i64
;; Write Sexp::Nil (32 zero bytes) into slot.  Returns 0.
;; ---------------------------------------------------------------------------
(sys:defun nl_stf_write_nil
    ((slot usize))
  i64
  (:alloc none :ffi none :unsafe may)
  (sys:unsafe
   (sys:poke-u64 slot 0)
   (sys:poke-u64 (+ slot 8) 0)
   (sys:poke-u64 (+ slot 16) 0)
   (sys:poke-u64 (+ slot 24) 0)
   (sys:cast i64 0)))

;; ---------------------------------------------------------------------------
;; nl_stf_byte(bytes_ptr, i, len) -> i64
;; Return bytes_ptr[i] if i < len, else -1 (EOF).
;; ---------------------------------------------------------------------------
(sys:defun nl_stf_byte
    ((bytes_ptr usize) (i i64) (len i64))
  i64
  (:alloc none :ffi may :unsafe may)
  (if (< i len)
      (sys:unsafe (nelisp_ptr_read_u8 bytes_ptr i))
    (sys:cast i64 -1)))

;; ---------------------------------------------------------------------------
;; nl_stf_is_digit(b) -> i64
;; 1 if b in '0'..'9' (48..57), else 0.
;; ---------------------------------------------------------------------------
(sys:defun nl_stf_is_digit
    ((b i64))
  i64
  (:alloc none :ffi none :unsafe none)
  (if (< b 48)
      (sys:cast i64 0)
    (if (> b 57)
        (sys:cast i64 0)
      (sys:cast i64 1))))

;; ---------------------------------------------------------------------------
;; nl_stf_count(bytes_ptr, i, len) -> i64
;; Count consecutive digit bytes starting at i.
;; ---------------------------------------------------------------------------
(sys:defun nl_stf_count
    ((bytes_ptr usize) (i i64) (len i64))
  i64
  (:alloc none :ffi may :unsafe may)
  (let ((b i64 (nl_stf_byte bytes_ptr i len)))
    (if (= (nl_stf_is_digit b) 1)
        (+ 1 (nl_stf_count bytes_ptr (+ i 1) len))
      (sys:cast i64 0))))

;; ---------------------------------------------------------------------------
;; nl_stf_accum(bytes_ptr, i, len, acc) -> i64
;; Accumulate digit bytes from i into acc.
;; APPROXIMATION FLAG [A]: overflows for > 18 digits.
;; ---------------------------------------------------------------------------
(sys:defun nl_stf_accum
    ((bytes_ptr usize) (i i64) (len i64) (acc i64))
  i64
  (:alloc none :ffi may :unsafe may)
  (let ((b i64 (nl_stf_byte bytes_ptr i len)))
    (if (= (nl_stf_is_digit b) 1)
        (nl_stf_accum bytes_ptr (+ i 1) len (+ (* acc 10) (- b 48)))
      acc)))

;; ---------------------------------------------------------------------------
;; nl_stf_ipow10(n) -> i64
;; 10^n as i64 for 0 ≤ n ≤ 18.
;; APPROXIMATION FLAG [B][C]: wraps for n > 18.
;; ---------------------------------------------------------------------------
(sys:defun nl_stf_ipow10
    ((n i64))
  i64
  (:alloc none :ffi none :unsafe none)
  (if (= n 0)
      (sys:cast i64 1)
    (* 10 (nl_stf_ipow10 (- n 1)))))

;; ---------------------------------------------------------------------------
;; nl_stf_finish(slot, neg, mant_i64, net_exp) -> i64
;;
;; Final computation: convert mant_i64 to f64, apply exponent, apply sign,
;; write Sexp::Float into slot.  Returns 1.
;;
;; All f64 arithmetic uses sys:i64->f64 as direct f64-binop operands.
;; No f64 values flow through let-bindings, function arguments, or if arms.
;;
;; net_exp >= 0:
;;   scale = 10^net_exp (i64, may overflow — APPROX FLAG [A][B][C])
;;   result = sign * mant * scale  (i64 → f64 via single i64->f64 cast)
;;
;; net_exp < 0:
;;   pow_neg = 10^|net_exp| (i64)
;;   result  = (sign * mant) / pow_neg  (f64 division)
;;
;; APPROXIMATION FLAGS [A][B][C][D].
;; ---------------------------------------------------------------------------
(sys:defun nl_stf_finish
    ((slot usize) (neg i64) (mant_i64 i64) (net_exp i64))
  i64
  (:alloc none :ffi may :unsafe may)
  ;; sign_val: gp-class i64 — safe in arithmetic and let-bindings.
  (let ((sign_val i64 (if (= neg 1) (- 0 1) (sys:cast i64 1))))
    (if (>= net_exp 0)
        ;; Positive (or zero) exponent path.
        ;; Scale mant as i64, then cast to f64 for the write call.
        ;; APPROX FLAG [D]: nl_sexp_write_float receives f64 via wrong ABI reg.
        (let ((pow i64 (nl_stf_ipow10 net_exp)))
          (let ((scaled i64 (* (* sign_val mant_i64) pow)))
            (sys:unsafe
             (nl_sexp_write_float slot (sys:i64->f64 scaled)))
            (sys:cast i64 1)))
      ;; Negative exponent path: f64 division avoids integer truncation.
      ;; APPROX FLAG [B]: rounding error from integer pow + one f64 divide.
      ;; APPROX FLAG [D]: same ABI mismatch for nl_sexp_write_float.
      (let ((neg_exp i64 (- 0 net_exp)))
        (let ((pow_neg i64 (nl_stf_ipow10 neg_exp)))
          (sys:unsafe
           (nl_sexp_write_float slot
             (sys:f64/ (sys:i64->f64 (* sign_val mant_i64))
                       (sys:i64->f64 pow_neg))))
          (sys:cast i64 1))))))

;; ---------------------------------------------------------------------------
;; nl_stf_build_result(bp, len, slot, neg, i2, mant) -> i64
;;
;; Called after integer part is parsed.
;; bp    : bytes_ptr
;; len   : byte count
;; slot  : *mut Sexp
;; neg   : 1 if negative
;; i2    : cursor after integer digits (= i1 + int_cnt)
;; mant  : accumulated integer-digit value (int_val, i64)
;;
;; Parses optional frac/exp, assembles mant_i64, net_exp,
;; calls nl_stf_finish directly.  6 params — within MVP limit.
;; ---------------------------------------------------------------------------
(sys:defun nl_stf_build_result
    ((bp usize) (len i64) (slot usize) (neg i64) (i2 i64) (mant i64))
  i64
  (:alloc none :ffi may :unsafe may)

  ;; Phase 3: optional '.'
  (let ((has_dot i64 (if (= (nl_stf_byte bp i2 len) 46) 1 0)))
    (let ((frac_cnt i64 (if (= has_dot 1)
                             (nl_stf_count bp (+ i2 1) len)
                           (sys:cast i64 0)))
          (frac_val i64 (if (= has_dot 1)
                             (nl_stf_accum bp (+ i2 1) len 0)
                           (sys:cast i64 0))))
      (let ((i3 i64 (if (= has_dot 1)
                         (+ (+ i2 1) frac_cnt)
                       i2))
            ;; APPROXIMATION FLAG [A]: ipow10 for frac_cnt.
            (scale i64 (if (= has_dot 1)
                            (nl_stf_ipow10 frac_cnt)
                          (sys:cast i64 1))))
        (let ((mant2 i64 (+ (* mant scale) frac_val)))

          ;; Phase 4: optional 'e'/'E'
          (let ((has_exp i64 (if (= (nl_stf_byte bp i3 len) 101) 1  ; 'e'
                              (if (= (nl_stf_byte bp i3 len) 69) 1   ; 'E'
                                (sys:cast i64 0)))))
            (let ((sb  i64 (nl_stf_byte bp (+ i3 1) len))
                  (i4  i64 (if (= has_exp 1)
                               (if (= (nl_stf_byte bp (+ i3 1) len) 45)
                                   (+ i3 2)              ; skip '-'
                                 (if (= (nl_stf_byte bp (+ i3 1) len) 43)
                                     (+ i3 2)            ; skip '+'
                                   (+ i3 1)))
                               i3))
                  (eneg i64 (if (= has_exp 1)
                                (if (= (nl_stf_byte bp (+ i3 1) len) 45) 1 0)
                                (sys:cast i64 0))))
              (let ((ecnt i64 (if (= has_exp 1)
                                   (nl_stf_count bp i4 len)
                                 (sys:cast i64 0)))
                    (emag i64 (if (= has_exp 1)
                                   (nl_stf_accum bp i4 len 0)
                                 (sys:cast i64 0))))

                ;; Validate: end_pos must equal len; exp_marker implies digits.
                (let ((end_pos i64 (+ i4 ecnt))
                      (exp_ok  i64 (if (= has_exp 1)
                                       (if (= ecnt 0)
                                           (sys:cast i64 0)
                                         (sys:cast i64 1))
                                     (sys:cast i64 1))))
                  (if (= end_pos len)
                      (if (= exp_ok 1)
                          ;; Signed exponent: esign = emag or -emag.
                          ;; net_exp = esign - frac_cnt.
                          (let ((esign i64 (if (= eneg 1)
                                               (- 0 emag)
                                             emag)))
                            (nl_stf_finish slot neg mant2 (- esign frac_cnt)))
                        (nl_stf_write_nil slot))
                    (nl_stf_write_nil slot)))))))))))

;; ---------------------------------------------------------------------------
;; nl_str_to_float(bytes_ptr, len, slot) -> i64
;;
;; Exported C-ABI entry point.  Deleted Rust body replacement.
;;
;; Grammar (strict subset of Rust str::parse::<f64>()):
;;   float = ['-'] digit+ ['.' digit*] [('e'|'E') ['+'|'-'] digit+]
;;
;; Returns 1 on success (Sexp::Float in *slot).
;; Returns 0 on parse failure (Sexp::Nil in *slot).
;; APPROXIMATION FLAGS [A][B][C][D].
;; ---------------------------------------------------------------------------
(sys:defun nl_str_to_float
    ((bytes_ptr usize) (len i64) (slot usize))
  i64
  (:export "nl_str_to_float" :abi c :alloc none :ffi may :unsafe may)

  ;; Reject empty.
  (if (= len 0)
      (nl_stf_write_nil slot)

    ;; Phase 1: optional '-'.
    (let ((b0 i64 (nl_stf_byte bytes_ptr 0 len)))
      (let ((neg i64 (if (= b0 45) 1 0))       ; 45 = '-'
            (i1  i64 (if (= b0 45) 1 0)))

        ;; Phase 2: integer digits — at least one required.
        (let ((int_cnt i64 (nl_stf_count bytes_ptr i1 len)))
          (if (= int_cnt 0)
              (nl_stf_write_nil slot)

            ;; Compute cursor i2 and int_val, then delegate.
            (let ((int_val i64 (nl_stf_accum bytes_ptr i1 len 0))
                  (i2      i64 (+ i1 int_cnt)))
              (nl_stf_build_result bytes_ptr len slot neg i2 int_val))))))))

;; End of Doc 136 nonenv-str-to-float.nl
