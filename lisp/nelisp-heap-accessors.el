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

(defvar nelisp-heap-accessors--string-physical nil
  "Non-nil inside a storage API whose caller has already decoded WORDs.")

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
      `(let* ((,a ,(nelisp-heap-accessors-lower left))
              (,b ,(nelisp-heap-accessors-lower right))
              (,an ,(bytes a)) (,bn ,(bytes b))
              (,i 0) (,ok 1))
         (if (/= ,an ,bn) 0
           (let* ((,ap ,(data a)) (,bp ,(data b)))
             (seq
              (while (and (< ,i ,an) (= ,ok 1))
                (if (/= (ptr-read-u8 ,ap ,i) (ptr-read-u8 ,bp ,i))
                    (setq ,ok 0) 0)
                (setq ,i (+ ,i 1)))
              ,ok)))))))

(defun nelisp-heap-accessors--lower-string-tag (operand)
  "Use the canonical WORD tag accessor, including GNU strings."
  (list 'nl_val_tag (nelisp-heap-accessors-lower operand)))

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
    `(let* ((,v ,(nelisp-heap-accessors-lower operand))
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
         ((and (eq (car source) 'defun)
               (or (memq (cadr source) '(bf_special_registry bf_eq2 wf_key_eq_depth wf_copy32 wf_word_store_out nl_abi_word_to_rc nelisp_symbol_key_equal nl_n3_cell_set nl_str_bytes_ptr nl_str_charlen nl_str_charlen_c nl_str_char_count nelisp_mirror_walk_bucket nelisp_frame_stack_find_walk_bucket nelisp_env_lookup_value nelisp_env_lookup_function))
                   (string-match-p
                "\\`\\(?:nl_gc_\\|nelisp_env_\\|nelisp_mirror_\\|nelisp_frame_stack_\\|nl_bignum_\\|nl_native_\\|nl_intern_\\|nl_cell_\\|nl_lookup_fn_autoload\\|nl_symname_\\|nl_symbol_name_\\|nl_record_\\|nl_vector_\\|nl_frame_stack_\\|nl_root_\\|nl_hdr_\\|nl_chunk_\\|nl_alloc_\\|nl_freelist_\\|nl_word_\\|nl_vci_\\|nl_vl_\\|nl_sci_\\|nl_val_\\)"
                (symbol-name (cadr source)))))
          (let ((nelisp-heap-accessors--string-physical t))
            (append (list 'defun (cadr source) (caddr source))
                    (nelisp-heap-accessors--lower-tail (cdddr source)))))
         ((eq (car source) 'sexp-tag)
          (nelisp-heap-accessors--lower-string-tag (cadr source)))
         ((eq (car source) 'sexp-name-eq)
          (nelisp-heap-accessors--lower-string-name-eq (cadr source) (caddr source)))
         ((eq (car source) 'str-eq)
          (nelisp-heap-accessors--lower-string-eq (cadr source) (caddr source)))
         ((eq (car source) 'str-len)
          (list 'nl_word_length (nelisp-heap-accessors-lower (cadr source))))
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
               (not (nelisp-heap-accessors--string-raw-base-p (cadr source))))
          (if (eq (car source) 'heap-read-u64)
              (cons 'nl_string_read64
                    (nelisp-heap-accessors--lower-tail (cdr source)))
            (list 'nl_string_read64 (nelisp-heap-accessors-lower (cadr source)) -1)))
         (field
          ;; These operands denote Lisp values, unlike the raw storage
          ;; accessors below. Preserve legacy views while accepting direct
          ;; vectorlike WORDs. The primitive helpers use storage loads, so
          ;; this lowering cannot recurse into itself.
          (list (pcase (car field)
                  ('heap-sexp-tag-byte 'nl_val_tag)
                  ('heap-sexp-tag-word 'nl_word_tag64)
                  ('heap-sexp-payload 'nl_word_payload)
                  ('heap-sexp-data 'nl_word_data)
                  ('heap-sexp-length 'nl_word_length))
                (nelisp-heap-accessors-lower (cadr source))))
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
