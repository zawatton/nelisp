;;; nelisp-cc-fx-call-primitives.el --- VM primitive WORD calls -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Evaluator transport only (native-surface clauses 1/2/5). No new Lisp
;; primitive or native sorting implementation: sort remains ordinary Lisp.
;; Inspect the live resolved definition, retaining normal APPLY on decline.

(defconst nelisp-cc-fx-call-primitives--source
  '(seq
    (defun m5_fx_cached_function (cache env)
      (let* ((table (nl_record_slot_word env 0))
             (buckets (nl_record_slot_word table 1))
             (count (nl_record_slot_word table 2))
             (owner (nl_vector_slot_word cache 2))
             (old-buckets (nl_vector_slot_word cache 4))
             (entry (if (and (/= owner 3) (/= old-buckets 3)
                             (= (nl_word_payload owner) (nl_word_payload table))
                             (= (nl_word_payload old-buckets) (nl_word_payload buckets))
                             (= (nl_vector_slot_word cache 3) count))
                        (nl_vector_slot_word cache 1) 3)))
        (seq
         (if (= entry 3)
             (seq (setq entry (nelisp_mirror_lookup_entry env (nl_vector_slot_word cache 0)))
                  (if (/= entry 0)
                      (let* ((data (+ (nl_word_payload cache) 8)))
                        (seq (ptr-write-u64 data 8 entry)
                             (ptr-write-u64 data 16 table)
                             (ptr-write-u64 data 24 count)
                             (ptr-write-u64 data 32 buckets))) 0)) 0)
         ;; Alias symbols and malformed/immediate definitions retain the full
         ;; checked resolver. The cached record's live function cell is read
         ;; every time, including after FSET and FMAKUNBOUND.
         (if (or (= entry 0) (= entry 3)) 0
           (let* ((fn (nl_record_slot_word entry 1)))
             (if (= (wf_bytecode_w_unbound_p env fn) 1) 0
               (if (= (m5_fx_word_cons_p fn 0) 1) fn 0)))))))
    (defun m5_fx_primitive (fn stk sp argc resw env scratch)
      (if (= (m5_fx_word_cons_p fn 0) 0) -3
        (let* ((head (nl_cons_car_word fn)) (tail (nl_cons_cdr_word fn)))
          (if (= (logand head 1) 1) -3
            (if (= (sexp-name-eq head "builtin") 0) -3
              (if (= (m5_fx_word_cons_p tail 0) 0) -3
                (m5_fx_primitive_named (nl_cons_car_word tail)
                                       stk sp argc resw env scratch)))))))
    (defun m5_fx_primitive_named (name stk sp argc resw env scratch)
      (let* ((a (if (> argc 0) (heap-read-u64 (+ stk (* (- sp argc) 8)) 0) 3))
             (b (if (> argc 1) (heap-read-u64 (+ stk (* (+ (- sp argc) 1) 8)) 0) 3))
             (tag (nl_word_tag64 a)) (r -1))
        (seq
         (setq r
          (cond
           ((= argc 1)
            (cond
             ((= (sexp-name-eq name "car") 1)
              (if (= tag 7) (nl_cons_car_word a) (if (= tag 0) 3 -1)))
             ((= (sexp-name-eq name "cdr") 1)
              (if (= tag 7) (nl_cons_cdr_word a) (if (= tag 0) 3 -1)))
             ((or (= (sexp-name-eq name "null") 1) (= (sexp-name-eq name "not") 1))
              (if (= (wf_bytecode_w_nilp a) 1) 7 3))
             ((= (sexp-name-eq name "consp") 1) (if (= tag 7) 7 3))
             (t -1)))
           ((= argc 2)
            (cond
             ((and (= tag 7) (= (sexp-name-eq name "setcdr") 1))
              (seq (wf_dirty) (ptr-write-u64 (nl_word_payload a) 8 b) b))
             ((and (= tag 7) (= (sexp-name-eq name "setcar") 1))
              (seq (wf_dirty) (ptr-write-u64 (nl_word_payload a) 0 b) b))
             ((= (sexp-name-eq name "eq") 1)
              (if (= (wf_bytecode_w_eqw a b) 1) 7 3))
             ((and (= (sexp-name-eq name "<") 1)
                   (= (m5_fx_int_p a) 1) (= (m5_fx_int_p b) 1))
              (if (< (wf_bytecode_w_int a) (wf_bytecode_w_int b)) 7 3))
             ((and (= (sexp-name-eq name "string<") 1)
                   (= (m5_fx_string_argument_p tag) 1)
                   (= (m5_fx_string_argument_p (nl_word_tag64 b)) 1))
              (if (= (m5_fx_sort_string_lessp a b) 1) 7 3))
             ((and (or (= (sexp-name-eq name "nth") 1)
                       (= (sexp-name-eq name "nthcdr") 1))
                   (= (m5_fx_int_p a) 1) (>= (wf_bytecode_w_int a) 0))
              (m5_fx_primitive_nth b (wf_bytecode_w_int a) (sexp-name-eq name "nthcdr")))
             (t -1)))
           (t -1)))
         (if (and (> argc 0) (= (sexp-name-eq name "funcall") 1))
             (m5_fx_primitive_funcall a stk sp (- argc 1) resw env scratch)
           (if (= r -1) -3 (seq (ptr-write-u64 resw 0 r) 0))))))
    (defun m5_fx_string_argument_p (tag)
      (if (or (= tag 4) (= tag 16) (= tag 5) (= tag 6)
              (= tag 14) (= tag 15)) 1 0))
    ;; prim-strcmp integration changes this one expression. Both inputs and
    ;; the result stay WORD/raw values; no FUNCALL or transport box is needed.
    (defun m5_fx_sort_string_lessp (a b)
      (bf_str_lt (bf_str_or_name_w a 0) (bf_str_or_name_w b 0)))
    (defun m5_fx_primitive_nth (items index tail)
      (seq
       (while (and (> index 0) (= (m5_fx_word_cons_p items 0) 1))
         (setq items (nl_cons_cdr_word items))
         (setq index (- index 1)))
       (if (= (nl_word_tag64 items) 0) 3
         (if (/= index 0) -1
           (if (= tail 1) items
             (if (= (m5_fx_word_cons_p items 0) 1) (nl_cons_car_word items) -1))))))
    (defun m5_fx_primitive_funcall (fn stk sp argc resw env scratch)
      (let* ((tag (nl_word_tag64 fn)))
        (if (or (= tag 4) (= tag 16))
            (if (= (nelisp_env_lookup_function env (+ env 64) fn scratch) 0)
                (m5_fx_primitive scratch stk sp argc resw env scratch) -3)
          (m5_fx_primitive fn stk sp argc resw env scratch)))))
  "Direct WORD transport for resolved existing VM primitives.")

(provide 'nelisp-cc-fx-call-primitives)
;;; nelisp-cc-fx-call-primitives.el ends here
