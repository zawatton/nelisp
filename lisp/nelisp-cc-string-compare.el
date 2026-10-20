;;; nelisp-cc-string-compare.el --- Word VM string lowering -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Compiler implementation only: this adds no reader builtin or :lit arm.
;; The boot compiler installs instruction 18 for the original Lisp functions,
;; keyed by closure identity in the ordinary traced fx table. Redefinitions,
;; advice, invalid operands and case-table folding retain normal execution.

(defconst nelisp-cc-string-compare--source
  '(seq
    (defun m5_string_order_words (a b)
      ;; Public native entry for sort: checked string WORDs, negative/zero/
      ;; positive order, GNU string-lessp semantics (no raw-byte promotion).
      (m5_string_compare_range a 0 (m5_length a) b 0 (m5_length b) 0))
    (defun m5_string_compare_range (a sa ea b sb eb flags)
      ;; Signed one-based mismatch index, zero for equal ranges. FLAGS bit 0
      ;; promotes mixed unibyte characters for compare-strings; bit 1 folds
      ;; ASCII case. The caller declines non-ASCII folding before any effect.
      (let* ((ua (m5_unibyte_tag_p (ptr-read-u64 a 0)))
             (ub (m5_unibyte_tag_p (ptr-read-u64 b 0)))
             (pa (nl_bi_strptr a)) (pb (nl_bi_strptr b))
             (ia (if (= ua 1) sa (nl_u8_cidx_byte a 0 (m5_strlen a) 0 sa)))
             (ib (if (= ub 1) sb (nl_u8_cidx_byte b 0 (m5_strlen b) 0 sb)))
             (na (- ea sa)) (nb (- eb sb)) (i 0) (r 0) (ca 0) (cb 0))
        (seq
         (while (and (= r 0) (and (< i na) (< i nb)))
           (seq
            (setq ca (ptr-read-u8 pa ia))
            (setq cb (ptr-read-u8 pb ib))
            (if (= ua 1) (setq ia (+ ia 1))
              (seq (setq ia (+ ia (nl_u8_clen_at ca)))
                   (if (>= ca 128) (setq ca (nl_u8_decode a (- ia (nl_u8_clen_at ca)))) 0)))
            (if (= ub 1) (setq ib (+ ib 1))
              (seq (setq ib (+ ib (nl_u8_clen_at cb)))
                   (if (>= cb 128) (setq cb (nl_u8_decode b (- ib (nl_u8_clen_at cb)))) 0)))
            (if (and (/= (logand flags 1) 0) (/= ua ub))
                (seq (if (and (= ua 1) (>= ca 128)) (setq ca (+ ca 4194048)) 0)
                     (if (and (= ub 1) (>= cb 128)) (setq cb (+ cb 4194048)) 0)) 0)
            (if (/= (logand flags 2) 0)
                (seq (if (and (>= ca 97) (<= ca 122)) (setq ca (- ca 32)) 0)
                     (if (and (>= cb 97) (<= cb 122)) (setq cb (- cb 32)) 0)) 0)
            (if (< ca cb) (setq r (- 0 (+ i 1)))
              (if (> ca cb) (setq r (+ i 1)) 0))
            (setq i (+ i 1))))
         (if (/= r 0) r
           (if (< na nb) (- 0 (+ i 1)) (if (> na nb) (+ i 1) 0))))))
    (defun m5_fx_string_arg (args argw argc i)
      (if (>= i argc) 3
        (if (/= argw 0) (ptr-read-u64 argw (* i 8))
          (let* ((p args) (k 0))
            (seq (while (< k i) (setq p (nl_cons_cdr_word p)) (setq k (+ k 1)))
                 (nl_cons_car_word p))))))
    (defun m5_fx_string_bound (w len endp)
      ;; -1 declines to the retained Lisp validation/error implementation.
      (if (= (nl_val_tag w) 0) (if (= endp 1) len 0)
        (if (/= (nl_val_tag w) 2) -1
          (let* ((n (nl_word_payload w)))
            (if (< n 0) (+ n len) (if (and (= endp 1) (> n len)) len n))))))
    (defun m5_fx_string_dependencies (code env)
      (let* ((i 3) (n (vector-len code)) (ok 1))
        (seq
         (while (and (= ok 1) (< i n))
           (let* ((entry (nelisp_mirror_lookup_entry env (nl_vector_slot_word code i))))
             (seq
              (if (= entry 0) (setq ok 0)
                (if (= (wf_bytecode_w_eqw (nl_record_slot_word entry 1)
                                          (nl_vector_slot_word code (+ i 1))) 0)
                    (setq ok 0) 0))
              (setq i (+ i 2)))))
         ok)))
    (defun m5_fx_string_intrinsic (code args argc argw env)
      ;; 2 means decline, with no Lisp effects. Results are canonical WORDs.
      (let* ((mode (wf_bytecode_w_int (nl_vector_slot_word code 1)))
             (a (m5_fx_string_arg args argw argc 0))
             (b (m5_fx_string_arg args argw argc (if (= mode 3) 3 1)))
             (ta (nl_val_tag a)) (tb (nl_val_tag b)))
        (if (and (= (m5_fx_string_dependencies code env) 1)
                 (and (= (m5_string_tag_p ta) 1) (= (m5_string_tag_p tb) 1)))
            (let* ((la (m5_length a)) (lb (m5_length b))
                   (sa 0) (ea la) (sb 0) (eb lb)
                   (fold (if (or (= mode 3) (= mode 4))
                             (m5_fx_string_arg args argw argc (if (= mode 3) 6 2)) 3)))
              (seq
               (if (= mode 3)
                   (seq
                    (setq sa (m5_fx_string_bound (m5_fx_string_arg args argw argc 1) la 0))
                    (setq ea (m5_fx_string_bound (m5_fx_string_arg args argw argc 2) la 1))
                    (setq sb (m5_fx_string_bound (m5_fx_string_arg args argw argc 4) lb 0))
                    (setq eb (m5_fx_string_bound (m5_fx_string_arg args argw argc 5) lb 1))) 0)
               (if (or (< sa 0) (or (< sb 0) (or (< ea sa) (or (< eb sb) (or (> ea la) (> eb lb)))))) 2
                 ;; Case folding belongs to the current Lisp case table,
                 ;; including user changes to ASCII mappings.
                 (if (/= (nl_val_tag fold) 0) 2
                   (if (= mode 2) (if (= (m5_streq a b) 1) 7 3)
                     (if (and (= mode 4) (> la lb)) 3
                       (let* ((r (if (= mode 0) (m5_string_order_words a b)
                                   (if (= mode 1) (m5_string_order_words b a)
                                     (m5_string_compare_range a sa ea b sb
                                       (if (= mode 4) la eb)
                                       (if (= (nl_val_tag fold) 0) 1 3))))))
                         (if (= mode 3) (if (= r 0) 7 (+ (shl r 2) 1))
                           (if (= mode 4) (if (= r 0) 7 3)
                             (if (< r 0) 7 3))))))))))
          2))))
  "WORD-only compiler lowering for canonical string comparison definitions.")

(provide 'nelisp-cc-string-compare)
;;; nelisp-cc-string-compare.el ends here
