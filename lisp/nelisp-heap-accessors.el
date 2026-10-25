;;; nelisp-heap-accessors.el --- Heap read boundary (Doc 216 S1) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; S1 established the load boundary without changing representation.
;; S4 lowers logical Sexp fields to WORD-aware helpers before optimization:
;; legacy storage views and direct vectorlike values share one semantic API.
;; These remain compiler forms, not reader builtins.
;;
;; Explicit storage APIs retain physical reads. Generic legacy consumers also
;; accept validated direct string WORDs (Doc 227), mapping their former inline
;; fields without allocating a view. Allocation/GC/value transport APIs decode
;; WORDs before storage reads; their physical mode never applies that mapping.
;; Native/extern raw pointer operations keep their physical contract.
;;
;; We deliberately centralize a superset of header reads: all storage loads,
;; including non-object memory. Thus the census needs no optimistic pointer
;; provenance inference. Zero raw storage reads outside this boundary proves
;; zero raw header reads there. Report the conservative scope, not an exact
;; count of object-header instructions or a predicted speedup.
;;
;; The assembler aliases cover host AOT/JIT emitters as well as the DSL. ISA
;; encoders remain in the architecture modules; the heap interpretation and
;; the choice to issue a storage load belong here. No new native is added.

;;; Code:

(require 'cl-lib)
(require 'nelisp-asm-x86_64)
(require 'nelisp-asm-arm64)
(require 'nelisp-sexp-layout)

(defconst nelisp-heap-accessors-sexp-fields
  '((heap-sexp-tag-byte ptr-read-u8 0)
    (heap-sexp-tag-word ptr-read-u64 0)
    (heap-sexp-payload ptr-read-u64 8)
    (heap-sexp-data ptr-read-u64 16)
    (heap-sexp-length ptr-read-u64 24))
  "Logical Sexp/view fields; offsets describe the legacy fallback only.
S4 resolves these fields through the WORD-aware semantic helpers. Physical
storage reads below retain their exact load widths and calling conventions.")

(defconst nelisp-heap-accessors-read-ops
  '((heap-read-u8 ptr-read-u8)
    (heap-read-u16 ptr-read-u16)
    (heap-read-u32 ptr-read-u32)
    (heap-read-u64 ptr-read-u64)
    (heap-read-s8 ptr-read-s8)
    (heap-read-s16 ptr-read-s16)
    (heap-read-s32 ptr-read-s32)
    (heap-read-s64 ptr-read-s64)
    (heap-native-read-u8 nelisp_ptr_read_u8)
    (heap-native-read-u64 nelisp_ptr_read_u64)
    (heap-extern-read-u8 extern-call nelisp_ptr_read_u8)
    (heap-extern-read-u64 extern-call nelisp_ptr_read_u64))
  "Storage accessor forms and their exact S1 lowering prefixes.
Keep direct/native/explicit-extern calls distinct: their old ABI is retained.")

(defun nelisp-heap-accessors--string-raw-base-p (form)
  "Whether FORM explicitly denotes storage, rather than a Lisp WORD."
  (or (numberp form)
      (and (consp form)
           (or (memq (car form) '(data-addr alloc-bytes heap-sexp-payload nl_word_payload))
               (and (memq (car form) '(+ -))
                    (cl-every #'nelisp-heap-accessors--string-raw-base-p (cdr form)))
               (equal form '(heap-read-u64 (data-addr nl_arena_base) 0))))))

(defun nelisp-heap-accessors--computed-address-p (form)
  "Whether FORM is a byte-offset sum with a variable term.
`(+ DATA INDEX)' addresses a position inside a buffer, never a Lisp WORD;
`(+ SLOT 24)' names a field of a value and keeps the String-aware read."
  (and (consp form) (memq (car form) '(+ -))
       (or (cl-some (lambda (term) (not (integerp term))) (cddr form))
           (nelisp-heap-accessors--computed-address-p (cadr form)))))

(defconst nelisp-heap-accessors-record-bases
  '(env state st fr frame frames-data chunk acc hbuf env_ptr hdr active-record active
    entry scratch resw link abase roots nlstr control head cin base resume node next
    cnblk cwf vptr mark_chunk addr address fw nl_sb nl_sw out-slot walk pending dst wtmp
    rootslot cout faddr reach total cmis cb cby cf cl cok out cursor-addr oldbase
    head_slot flag-value bytes pool free-node gbox crel old-root count_slot magptr_slot
    sum_sign_slot sign_slot r1 r2 qslot rbuf dbuf buf top binding-state record statusp
    box boxa boxb)
  "Variables that hold runtime records, stack cells or buffers, never a Lisp value.
A generic slot read through one of them addresses runtime memory, so it keeps
its physical load instead of the String-aware read. The list comes from the
Doc 227 audit of every String-aware read site in the reader units
(tools/ai/strings-check.py audit lists the bases that remain).")

(defconst nelisp-heap-accessors-record-accessors
  '(nl_eln_callback_record_at nl_thread_registry_entry nl_tty_slot_fd
    nl_tty_slot_saved_flag nl_chunk_cursor_addr nl_n4_state nl_ct_head_slot)
  "Calls that answer a runtime record pointer.")

(defun nelisp-heap-accessors--record-base-p (form)
  "Whether FORM names a runtime record: a record variable, a constant offset
from one, or a call to a record accessor."
  (cond ((symbolp form) (and (memq form nelisp-heap-accessors-record-bases) t))
        ((and (consp form) (memq (car form) '(+ -)))
         (and (nelisp-heap-accessors--record-base-p (cadr form))
              (cl-every #'integerp (cddr form))))
        ((consp form) (and (memq (car form) nelisp-heap-accessors-record-accessors) t))
        (t nil)))

(defconst nelisp-heap-accessors-guarded-functions
  '(nl_apply_sym_eq_w nl_apply_sym_eq_bytes nl_apply_name_classify nelisp_fnv1a
    nl_ht_test_mode m5_push_str_promoted m5_forced_multibyte_p nl_str_aref_byte_off
    nl_str_sub_byte_off bf_keyword_raw nl_positioned_flag m5_fmt_loop
    wf_bytecode_execute_frame wf_bytecode_w_function wf_bytecode_scratch
    wf_bytecode_call wf_bytecode_w_fast_run wf_bytecode_w_lean_bind
    nl_directcall_invoke nl_apply_builtin_w nl_sb_plain_entry_p
    ;; strguard: measured v13 callers (callgrind, 43 matrix rows, startup removed).
    nl_bind_frame_param nl_ali_bind_done nl_ali_body_tail_nil wf_bytecode_try_jit_frame_w
    wf_cb_prim_call wf_bytecode_call_named_value nl_mut_str_push_byte nl_mut_str_finalize
    nl_mut_str_len nelisp_reader_p_depth_ok_p nelisp_reader_p_label_proxy_number
    bf_mark_declared_special nl_n4_bind_declared bf_declared_special_p nl_sb_set
    wf_word_view nl_directcall_alloc_run m5_make_repeated_string)
  "Value readers on hot paths. Their String-aware slot reads test the WORD tag
inline and call nl_string_read64 only for a String WORD.")

;; strguard: the list above was hand-maintained and missed callers.  A defun that
;; is not a storage internal (the physical families above) is guarded by rule
;; when its String-aware slot-read site count is at most
;; `nelisp-heap-accessors-guard-max-sites'; larger bodies keep the out-of-line
;; call, so inlined text growth per function stays bounded.  Both forms are
;; semantically identical: nl_string_read64 starts with the same tag test.
(defconst nelisp-heap-accessors-guard-max-sites -1
  "Largest number of String-aware slot reads one defun may inline-guard.")

(defconst nelisp-heap-accessors-unguarded-functions
  '(driver nelisp_project_driver nelisp_reader_p_leaf_w nelisp_reader_p_seq_dispatch_w
    nl_bi_process_call_process_w nl_ct_coalesce nl_float_alloc nl_jit_sxhash_dispatch
    nl_sexp_eq nl_stackmap_shadow nl_str_to_float nl_strview_selftest_w
    nl_symcells_selftest_check nl_tls_encrypt_all wf_bytecode_handlers_push wf_bytecode_pending_exit
    wf_bytecode_rem wf_diff_w wf_prod_w wf_subtail_w
    nl_ent_w_slow wf_bytecode_unwind_step)
  "Defuns that keep the out-of-line nl_string_read64 call.
Guarding one of these would push its lowered body from at most 80 nodes to more
(`nelisp-standalone--inline-max-nodes'), so the unit inliner would stop copying
it into its callers and the F1 closure would gain a live function.  Measured on
v13 by the strguard census, none of them is a hot nl_string_read64 caller.")

(defun nelisp-heap-accessors--count-read-sites (form)
  "Count heap-read-u64/u8 slot reads of FORM that the String guard applies to."
  (cond ((not (consp form)) 0)
        ((memq (car form) '(quote function)) 0)
        (t (let ((n (if (and (memq (car form) '(heap-read-u64 heap-read-u8))
                             (consp (cdr form)) (consp (cddr form))
                             (if (eq (car form) 'heap-read-u8) (eql (nth 2 form) 0)
                               (memq (nth 2 form) '(0 8 16 24))))
                        1 0)))
             (while (consp form)
               (setq n (+ n (nelisp-heap-accessors--count-read-sites (car form))))
               (setq form (cdr form)))
             n))))

(defun nelisp-heap-accessors--rule-guarded-p (source)
  "Non-nil when the defun SOURCE is guarded by rule or by the explicit list."
  (and (not (memq (cadr source) nelisp-heap-accessors-unguarded-functions))
       (or (memq (cadr source) nelisp-heap-accessors-guarded-functions)
           (<= (nelisp-heap-accessors--count-read-sites (cdddr source))
               nelisp-heap-accessors-guard-max-sites))))

(defvar nelisp-heap-accessors--string-guarded nil
  "Non-nil inside a function listed in `nelisp-heap-accessors-guarded-functions'.")

(defvar nelisp-heap-accessors--string-physical nil
  "Non-nil inside a storage API whose caller has already decoded WORDs.")

;; Doc 226 D9: a symbol object's WORD (class 110, header byte 4 or 16) names itself
;; by its name cell at +8 -- a String WORD or a legacy view.  The grammar emitters
;; for the name ops redirect it the same way (`emit-symbol-object-redirect'); these
;; lowerings replace the grammar op before the emitter sees it, so they redirect too.
(defun nelisp-heap-accessors--symbol-name-operand (form)
  "FORM's value, or the name cell of FORM's value when that is a symbol object WORD."
  (let ((v (make-symbol "name-operand")) (k (make-symbol "name-operand-kind")))
    `(let* ((,v ,form))
       (if (= (logand ,v 7) 6)
           (let* ((,k (ptr-read-u8 (- ,v 6) 0)))
             (if (= ,k 4) (ptr-read-u64 (- ,v 6) 8)
               (if (= ,k 16) (ptr-read-u64 (- ,v 6) 8) ,v)))
         ,v))))

(defun nelisp-heap-accessors--lower-string-eq (left right)
  "Compare byte spans in GNU headers or legacy String/Symbol views.
The old grammar emitter reads physical fields implicitly. Decode here so
ordinary representation work does not require changing a machine emitter."
  (let ((a (make-symbol "string-eq-a")) (b (make-symbol "string-eq-b"))
        (ap (make-symbol "string-eq-ap")) (bp (make-symbol "string-eq-bp"))
        (an (make-symbol "string-eq-an")) (bn (make-symbol "string-eq-bn"))
        (i (make-symbol "string-eq-i")) (ok (make-symbol "string-eq-ok")))
    (cl-labels
        ((bytes (v)
           `(if (= (logand ,v 7) 4)
                (let* ((n (ptr-read-u64 (- ,v 4) 8)))
                  (if (< n 0)
                      (logand (ptr-read-u64 (- ,v 4) 0) 9223372036854775807) n))
              (ptr-read-u64 ,v 24)))
         (data (v)
           `(if (= (logand ,v 7) 4) (ptr-read-u64 (- ,v 4) 24)
              (ptr-read-u64 ,v 16))))
      `(let* ((,a ,(nelisp-heap-accessors--symbol-name-operand
                    (nelisp-heap-accessors-lower left)))
              (,b ,(nelisp-heap-accessors--symbol-name-operand
                    (nelisp-heap-accessors-lower right))))
         ;; Legacy Symbol/name slots retain the existing byte-comparison
         ;; primitive. Only GNU header WORDs need the representation bridge.
         (if (and (/= (logand ,a 7) 4) (/= (logand ,b 7) 4))
             (str-eq ,a ,b)
           (let* ((,an ,(bytes a)) (,bn ,(bytes b)) (,i 0) (,ok 1))
             (if (/= ,an ,bn) 0
               (let* ((,ap ,(data a)) (,bp ,(data b)))
                 (seq
                  (while (and (< ,i ,an) (= ,ok 1))
                    (if (/= (ptr-read-u8 ,ap ,i) (ptr-read-u8 ,bp ,i))
                        (setq ,ok 0) 0)
                    (setq ,i (+ ,i 1)))
                  ,ok)))))))))

(defun nelisp-heap-accessors--lower-string-tag (operand)
  "Use the canonical WORD tag accessor, including GNU strings."
  ;; Share the existing accessor instead of expanding its complete
  ;; immediate/vector/String classification at every compatibility read.
  (list 'extern-call 'nl_val_tag (nelisp-heap-accessors-lower operand)))

(defun nelisp-heap-accessors--lower-string-name-eq (operand literal)
  "Keep allocation-free literal comparisons for GNU and legacy strings."
  (let* ((v (make-symbol "string-name-value"))
         (tag (make-symbol "string-name-tag"))
         (n (make-symbol "string-name-length"))
         (p (make-symbol "string-name-data"))
         (nb (make-symbol "string-name-size-byte"))
         (bytes (encode-coding-string literal 'utf-8 t))
         (index 0) comparisons)
    ;; Compare complete native words without reading beyond the string.
    ;; All supported standalone targets are little endian. The remaining
    ;; tail bytes keep the original literal comparison's exact bounds.
    (while (<= (+ index 8) (length bytes))
      (let ((word 0))
        (dotimes (j 8)
          (setq word (logior word (ash (aref bytes (+ index j)) (* j 8)))))
        (when (>= word (expt 2 63)) (setq word (- word (expt 2 64))))
        (push `(= (ptr-read-u64 ,p ,index) ,word) comparisons))
      (setq index (+ index 8)))
    (while (< index (length bytes))
      (push `(= (ptr-read-u8 ,p ,index) ,(aref bytes index)) comparisons)
      (setq index (1+ index)))
    `(let* ((,v ,(nelisp-heap-accessors--symbol-name-operand
                  (nelisp-heap-accessors-lower operand)))
            (,tag (if (= (logand ,v 7) 4) 5 (ptr-read-u8 ,v 0))))
       (if (or (= ,tag 4) (= ,tag 5) (= ,tag 14))
           (let* ((,n (if (= (logand ,v 7) 4)
                          (let* ((,nb (ptr-read-u64 (- ,v 4) 8)))
                            (if (< ,nb 0)
                                (logand (ptr-read-u64 (- ,v 4) 0) 9223372036854775807)
                              ,nb))
                        (ptr-read-u64 ,v 24))))
             (if (/= ,n ,(length bytes)) 0
               (let* ((,p (if (= (logand ,v 7) 4)
                              (ptr-read-u64 (- ,v 4) 24)
                            (ptr-read-u64 ,v 16))))
                 ,(if comparisons (cons 'and (nreverse comparisons)) 1))))
         0))))

(defun nelisp-heap-accessors-lower (source)
  "Lower semantic fields and physical storage access in SOURCE.
Do not touch quoted Lisp data.
Do this after the census, before any source optimizer or native emitter.
Every operand is retained exactly once; dotted data is preserved."
  (if (not (consp source)) source
    (if (memq (car source) '(quote function)) source
      (let* ((entry (and (symbolp (car source))
                         (assq (car source) nelisp-heap-accessors-read-ops)))
             (field (and (symbolp (car source))
                         (assq (car source) nelisp-heap-accessors-sexp-fields))))
        (cond
         ;; Doc 227: the string kernel describes physical GNU structures.
         ;; Its ptr operations must not be interpreted as legacy Sexp fields.
         ((and (eq (car source) 'defun)
               (or (string-prefix-p "nl_string_" (symbol-name (cadr source)))
                   (memq (cadr source) '(nl_sweep_strings nl_compact_small_strings))))
          (let ((nelisp-heap-accessors--string-physical t))
            (append (list 'defun (cadr source) (caddr source))
                    (nelisp-heap-accessors--lower-tail (cdddr source)))))
         ;; Allocator, collector, image and symbol-cell internals (including the
         ;; GNU block families of Doc 223/226) address raw memory, never a
         ;; Lisp value that could be a String WORD: they keep physical reads.
         ((and (eq (car source) 'defun)
               (or (memq (cadr source) '(bf_special_registry bf_eq2 wf_key_eq_depth wf_copy32 wf_word_store_out nl_abi_word_to_rc nl_sexp_clone_into nl_cons_car_word nl_cons_cdr_word nelisp_symbol_key_equal nl_n3_cell_set nl_str_bytes_ptr nl_str_charlen nl_str_charlen_c nl_str_char_count nelisp_mirror_walk_bucket nelisp_frame_stack_find_walk_bucket nelisp_env_lookup_value nelisp_env_lookup_function))
                   (string-match-p
                "\\`\\(?:nl_gc_\\|nelisp_env_\\|nelisp_mirror_\\|nelisp_frame_stack_\\|nl_bignum_\\|nl_native_\\|nl_intern_\\|nl_cell_\\|nl_lookup_fn_autoload\\|nl_symname_\\|nl_symbol_name_\\|nl_record_\\|nl_vector_\\|nl_frame_stack_\\|nl_root_\\|nl_hdr_\\|nl_chunk_\\|nl_alloc_\\|nl_freelist_\\|nl_word_\\|nl_vci_\\|nl_vl_\\|nl_sci_\\|nl_val_\\|nl_ablock_\\|nl_boot_\\|nl_sweep_\\|nl_cons_alloc\\|nl_cons_mark\\|nl_cons_unmark\\|nl_cons_dead\\|nl_symbol_\\|nl_symcell_\\|nl_sym_\\|nl_typed_\\|nl_live_\\|nl_dump_\\|nl_fa_\\|nl_os_\\|nl_conserv_\\|nl_aref_cache_\\|nl_fvcache_\\|nl_mxcache_\\|nl_compact_\\|nl_tty_\\|nl_neln_\\|m5_fx_\\|nl_name_is_\\|bf_load_abs_p\\)"
                (symbol-name (cadr source)))))
          (let ((nelisp-heap-accessors--string-physical t))
            (append (list 'defun (cadr source) (caddr source))
                    (nelisp-heap-accessors--lower-tail (cdddr source)))))
         ((and (eq (car source) 'defun)
               (nelisp-heap-accessors--rule-guarded-p source))
          (let ((nelisp-heap-accessors--string-guarded t))
            (append (list 'defun (cadr source) (caddr source))
                    (nelisp-heap-accessors--lower-tail (cdddr source)))))
         ((eq (car source) 'sexp-tag)
          (nelisp-heap-accessors--lower-string-tag (cadr source)))
         ((memq (car source) '(nl_val_tag nl_word_tag64))
          (nelisp-heap-accessors--lower-string-tag (cadr source)))
         ((eq (car source) 'sexp-name-eq)
          (nelisp-heap-accessors--lower-string-name-eq (cadr source) (caddr source)))
         ((eq (car source) 'str-eq)
          (nelisp-heap-accessors--lower-string-eq (cadr source) (caddr source)))
         ((eq (car source) 'str-len)
          ;; This grammar op's contract is String/Symbol, while the
          ;; generic Sexp length accessor also handles vectorlike flags.
          (let ((p (make-symbol "string-length-value")))
            `(let* ((,p ,(nelisp-heap-accessors--symbol-name-operand
                          (nelisp-heap-accessors-lower (cadr source)))))
               (if (= (logand ,p 7) 4)
                   (let* ((n (ptr-read-u64 (- ,p 4) 8)))
                     (if (< n 0) (logand (ptr-read-u64 (- ,p 4) 0) 9223372036854775807) n))
                 (ptr-read-u64 ,p 24)))))
         ((eq (car source) 'str-bytes)
          (list 'nl_word_data (nelisp-heap-accessors-lower (cadr source))))
         ((eq (car source) 'str-byte-at)
          (list 'ptr-read-u8
                (list 'nl_word_data (nelisp-heap-accessors-lower (cadr source)))
                (nelisp-heap-accessors-lower (caddr source))))
         ((and (not nelisp-heap-accessors--string-physical)
               (memq (car source) '(heap-read-u64 heap-read-u8))
               (if (eq (car source) 'heap-read-u8) (eql (nth 2 source) 0)
                 (memq (nth 2 source) '(0 8 16 24)))
               (not (nelisp-heap-accessors--string-raw-base-p (cadr source)))
               (not (nelisp-heap-accessors--computed-address-p (cadr source)))
               (not (nelisp-heap-accessors--record-base-p (cadr source))))
          (let* ((u64 (eq (car source) 'heap-read-u64))
                 (off (if u64 (nth 2 source) -1)))
            (if nelisp-heap-accessors--string-guarded
                ;; Only a String WORD reaches the call; every other operand
                ;; (a slot pointer, a buffer address) is one load, as before.
                (let ((g (make-symbol "string-read-base")))
                  `(let* ((,g ,(nelisp-heap-accessors-lower (cadr source))))
                     (if (= (logand ,g 7) 4)
                         (nl_string_read64 ,g ,off)
                       ,(if u64 `(ptr-read-u64 ,g ,off) `(ptr-read-u8 ,g 0)))))
              (list 'nl_string_read64 (nelisp-heap-accessors-lower (cadr source)) off))))
         (field
          ;; These operands denote Lisp values, unlike the raw storage
          ;; accessors below. Preserve legacy views while accepting direct
          ;; vectorlike WORDs. The primitive helpers use storage loads, so
          ;; this lowering cannot recurse into itself.
          (if (memq (car field) '(heap-sexp-tag-byte heap-sexp-tag-word))
              (nelisp-heap-accessors--lower-string-tag (cadr source))
            (list (pcase (car field)
                  ('heap-sexp-tag-byte 'nl_val_tag)
                  ('heap-sexp-tag-word 'nl_word_tag64)
                  ('heap-sexp-payload 'nl_word_payload)
                  ('heap-sexp-data 'nl_word_data)
                  ('heap-sexp-length 'nl_word_length))
                (nelisp-heap-accessors-lower (cadr source)))))
         (entry
          (append (cdr entry) (nelisp-heap-accessors--lower-tail (cdr source))))
         (t
          (cons (nelisp-heap-accessors-lower (car source))
                (nelisp-heap-accessors--lower-tail (cdr source)))))))))

(defun nelisp-heap-accessors--lower-tail (tail)
  "Lower each expression in TAIL, preserving a dotted final atom."
  (let (items)
    (while (consp tail)
      (push (nelisp-heap-accessors-lower (car tail)) items)
      (setq tail (cdr tail)))
    (nconc (nreverse items) tail)))

;; Function aliases introduce no new emitted call or register clobber. Keep
;; the exact architecture operation (including widths and addressing mode).
(defalias 'nelisp-heap-accessors-x86_64-mov-reg-mem-disp8
  #'nelisp-asm-x86_64-mov-reg-mem-disp8)
(defalias 'nelisp-heap-accessors-x86_64-mov-reg-mem-rsp-disp
  #'nelisp-asm-x86_64-mov-reg-mem-rsp-disp)
(defalias 'nelisp-heap-accessors-x86_64-movdqu-xmm-mem-disp8
  #'nelisp-asm-x86_64-movdqu-xmm-mem-disp8)
(defalias 'nelisp-heap-accessors-x86_64-movsd-xmm-mem-disp8
  #'nelisp-asm-x86_64-movsd-xmm-mem-disp8)
(defalias 'nelisp-heap-accessors-x86_64-movzx-reg-byte-mem
  #'nelisp-asm-x86_64-movzx-reg-byte-mem)
(defalias 'nelisp-heap-accessors-arm64-ldr-imm #'nelisp-asm-arm64-ldr-imm)
(defalias 'nelisp-heap-accessors-arm64-ldr-post-sp-16
  #'nelisp-asm-arm64-ldr-post-sp-16)
(defalias 'nelisp-heap-accessors-arm64-ldr-reg-reg #'nelisp-asm-arm64-ldr-reg-reg)
(defalias 'nelisp-heap-accessors-arm64-ldrb-imm #'nelisp-asm-arm64-ldrb-imm)
(defalias 'nelisp-heap-accessors-arm64-ldrw-reg-reg #'nelisp-asm-arm64-ldrw-reg-reg)
(defalias 'nelisp-heap-accessors-arm64-ldur-d-base-disp
  #'nelisp-asm-arm64-ldur-d-base-disp)

(defun nelisp-heap-accessors-x86_64-sexp-payload (buf dst base)
  "Emit the legacy Sexp payload read from BASE into DST in BUF."
  (nelisp-asm-x86_64-mov-reg-mem-disp8
   buf dst base nelisp-sexp--offset-payload))

(defun nelisp-heap-accessors-arm64-sexp-payload (buf dst base)
  "Emit the legacy Sexp payload read from BASE into DST in BUF."
  (nelisp-asm-arm64-ldr-imm buf dst base nelisp-sexp--offset-payload))

(defun nelisp-heap-accessors-arm64-sexp-tag (buf dst base)
  "Emit the legacy Sexp tag byte read from BASE into DST in BUF."
  (nelisp-asm-arm64-ldrb-imm buf dst base nelisp-sexp--offset-tag))

(provide 'nelisp-heap-accessors)
;;; nelisp-heap-accessors.el ends here
