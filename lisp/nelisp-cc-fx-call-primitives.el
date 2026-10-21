;;; nelisp-cc-fx-call-primitives.el --- VM primitive WORD calls -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Evaluator transport only (native-surface clauses 1/2/5). No new Lisp
;; primitive or native sorting implementation: sort remains ordinary Lisp.
;; Inspect the live resolved definition, retaining normal APPLY on decline.

(defconst nelisp-cc-fx-call-primitives--source
  '(seq
    ;; GNU bytecode.c:803 reads the called symbol's function cell directly.
    ;; The symbol cell (Doc 221 M02) replaces the per-site entry cache that
    ;; was validated against the table, its count and its buckets on every
    ;; call; CACHE's slot 0 is the call site's symbol, owned by CACHE.
    ;; Alias symbols and malformed/immediate definitions retain the full
    ;; checked resolver: this answers 0 for them.  The live function cell
    ;; is read every time, including after FSET and FMAKUNBOUND.
    (defun m5_fx_cached_function (cache env)
      (let* ((entry (nelisp_mirror_lookup_entry_owned
                     env (nl_vector_slot_word cache 0))))
        (if (or (= entry 0) (= entry 3)) 0
          (let* ((fn (nl_record_slot_word entry 1)))
            (if (= (wf_bytecode_w_unbound_p env fn) 1) 0
              (if (= (m5_fx_word_cons_p fn 0) 1) fn 0))))))
    ;; NAME's WORD when FN is (builtin NAME), else 0.
    (defun m5_fx_builtin_name (fn _pad)
      (if (= (m5_fx_word_cons_p fn 0) 0) 0
        (let* ((head (nl_cons_car_word fn)) (tail (nl_cons_cdr_word fn)))
          (if (= (logand head 1) 1) 0
            (if (= (sexp-name-eq head "builtin") 0) 0
              (if (= (m5_fx_word_cons_p tail 0) 0) 0
                (nl_cons_car_word tail)))))))
    (defun m5_fx_primitive (fn stk sp argc resw env scratch)
      (let* ((name (m5_fx_builtin_name fn 0)))
        (if (= name 0) -3
          (m5_fx_primitive_named name stk sp argc resw env scratch))))
    ;; The primitives VM code calls without an argument list, by id: the
    ;; names are compared once per call site and definition (the call-out's
    ;; inline cache, `m5_fx_callout'), not on every call.  0: no primitive
    ;; for NAME with ARGC arguments.
    (defun m5_fx_primitive_id (name argc)
      (if (= argc 1)
          (cond
           ((= (sexp-name-eq name "car") 1) 1)
           ((= (sexp-name-eq name "cdr") 1) 2)
           ((or (= (sexp-name-eq name "null") 1) (= (sexp-name-eq name "not") 1)) 3)
           ((= (sexp-name-eq name "consp") 1) 4)
           ((= (sexp-name-eq name "funcall") 1) 13)
           (t 0))
        (if (= argc 2)
            (cond
             ((= (sexp-name-eq name "setcdr") 1) 5)
             ((= (sexp-name-eq name "setcar") 1) 6)
             ((= (sexp-name-eq name "eq") 1) 7)
             ((= (sexp-name-eq name "<") 1) 8)
             ((= (sexp-name-eq name "string<") 1) 9)
             ((= (sexp-name-eq name "nth") 1) 10)
             ((= (sexp-name-eq name "nthcdr") 1) 11)
             ((= (sexp-name-eq name "funcall") 1) 13)
             (t 0))
          (if (if (> argc 0) (= (sexp-name-eq name "funcall") 1) 0) 13 0))))
    ;; Primitive ID on the ARGC words ending at STK[SP-1]: 0 with the value
    ;; in RESW, -3 when it declines (a type it leaves to the full builtin).
    (defun m5_fx_primitive_run (id stk sp argc resw env scratch)
      (let* ((a (if (> argc 0) (heap-read-u64 (+ stk (* (- sp argc) 8)) 0) 3))
             (b (if (> argc 1) (heap-read-u64 (+ stk (* (+ (- sp argc) 1) 8)) 0) 3))
             (tag (nl_word_tag64 a)) (r -1))
        (if (= id 13)
            (m5_fx_primitive_funcall a stk sp (- argc 1) resw env scratch)
          (seq
           (setq r
            (cond
             ((= id 1) (if (= tag 7) (nl_cons_car_word a) (if (= tag 0) 3 -1)))
             ((= id 2) (if (= tag 7) (nl_cons_cdr_word a) (if (= tag 0) 3 -1)))
             ((= id 3) (if (= (wf_bytecode_w_nilp a) 1) 7 3))
             ((= id 4) (if (= tag 7) 7 3))
             ((= id 5) (if (= tag 7) (seq (wf_dirty) (ptr-write-u64 (nl_word_payload a) 8 (nl_word_canon b)) b) -1))
             ((= id 6) (if (= tag 7) (seq (wf_dirty) (ptr-write-u64 (nl_word_payload a) 0 (nl_word_canon b)) b) -1))
             ((= id 7) (if (= (wf_bytecode_w_eqw a b) 1) 7 3))
             ((= id 8) (if (and (= (m5_fx_int_p a) 1) (= (m5_fx_int_p b) 1))
                           (if (< (wf_bytecode_w_int a) (wf_bytecode_w_int b)) 7 3) -1))
             ((= id 9) (if (and (= (m5_fx_string_argument_p tag) 1)
                                (= (m5_fx_string_argument_p (nl_word_tag64 b)) 1))
                           (if (= (m5_fx_sort_string_lessp a b) 1) 7 3) -1))
             ((or (= id 10) (= id 11))
              (if (and (= (m5_fx_int_p a) 1) (>= (wf_bytecode_w_int a) 0))
                  (m5_fx_primitive_nth b (wf_bytecode_w_int a) (if (= id 11) 1 0)) -1))
             (t -1)))
           (if (= r -1) -3 (seq (ptr-write-u64 resw 0 r) 0))))))
    (defun m5_fx_primitive_named (name stk sp argc resw env scratch)
      (let* ((id (m5_fx_primitive_id name argc)))
        (if (= id 0) -3 (m5_fx_primitive_run id stk sp argc resw env scratch))))
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
    ;; GNU eval.c:3185-3215 Ffuncall -> funcall_general -> funcall_lambda: a
    ;; compiled callee runs on the already evaluated argument words, with
    ;; its activation recorded, and no argument list.  A closure the VM
    ;; covers for ARGC arguments therefore runs through `m5_fx_call_words'
    ;; directly; anything else answers -3 and keeps the generic apply.
    ;; WS: a rooted WORD slot of the caller's frame, which holds a looked-up
    ;; definition while it runs (0: no symbol lookup, the call declines).
    ;; The VM's closure call needs no scratch (`m5_fx_call_words').
    (defun m5_fx_primitive_funcall (fn stk sp argc resw env ws)
      (m5_fx_funcall_site 0 fn stk sp argc resw env ws))
    ;; GNU's function object carries its own code (Ffuncall, funcall_lambda,
    ;; exec_byte_code on the object's bytecode, eval.c:3185-3215, 3350);
    ;; a closure's VM record lives in `nelisp--fx-table' under its body, a
    ;; hash lookup per call.  A `funcall' call site keeps the last covered
    ;; closure it called and that closure's record in its cache vector's
    ;; slots 3 and 4, so a site that keeps calling one closure skips the
    ;; lookup.  A record retired from the table is marked in its slot 7
    ;; (`nelisp--fx-retire'), which the cached copy checks (its code stays:
    ;; a call already running it must go on), and the arity is checked at
    ;; every call.  CACHE 0: no site cache (a worker, or a
    ;; call with no cache vector).
    (defun m5_fx_funcall_site (cache fn stk sp argc resw env ws)
      (if (if (= cache 0) 0 (= (nl_vector_slot_word cache 3) fn))
          (let* ((rec (nl_vector_slot_word cache 4)))
            (if (if (if (>= (nl_vector_length rec) 8)
                        (= (nl_vector_slot_word rec 7) 3) 0)
                    (>= (m5_fx_arity_code rec argc) 0) 0)
                (m5_fx_call_words rec fn (+ stk (* (- sp argc) 8)) argc
                                  env 0 resw 0)
              (m5_fx_funcall_site 0 fn stk sp argc resw env ws)))
        (let* ((tag (nl_word_tag64 fn)))
          (if (or (= tag 4) (= tag 16))
              (let* ((def (if (= ws 0) 2
                            (nelisp_env_lookup_function_w (+ env 0) (+ env 64) fn 0))))
                (if (= def 2) -3
                  (seq (ptr-write-u64 ws 0 def)
                       (m5_fx_primitive def stk sp argc resw env ws))))
            (let* ((st (m5_fx_primitive fn stk sp argc resw env ws)))
              (if (/= st -3) st
                (let* ((rec (m5_fx_covered fn argc env 0)))
                  (if (= rec 0) -3
                    (seq (if (= cache 0) 0 (m5_fx_funcall_site_fill cache fn rec))
                         (m5_fx_call_words rec fn (+ stk (* (- sp argc) 8)) argc
                                           env 0 resw 0))))))))))
    ;; The record before the closure, so a reader that sees the closure
    ;; sees its record (the site kind's order, `m5_fx_site_kind').
    (defun m5_fx_funcall_site_fill (cache fn rec)
      (if (if (< (nl_vector_length cache) 5) 1 (< (nl_vector_length rec) 8)) 0
        (seq (wf_dirty)
             (ptr-write-u64 (+ (nl_word_payload cache) 40) 0 rec)
             (ptr-write-u64 (+ (nl_word_payload cache) 32) 0 fn)))))
  "Direct WORD transport for resolved existing VM primitives.")

(provide 'nelisp-cc-fx-call-primitives)
;;; nelisp-cc-fx-call-primitives.el ends here
