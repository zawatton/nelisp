;;; nelisp-heap-accessors.el --- Heap read boundary (Doc 216 S1) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; S1 changes no representation. These are compiler forms, not runtime
;; functions or reader builtins. Lower them before optimization/inlining to
;; the original operations, preserving evaluation order and instruction bytes.
;;
;; A storage operand is an explicit address: a legacy Sexp/view, backing
;; object, allocator metadata, or foreign buffer. It is NOT an unchecked
;; arbitrary tagged WORD. WORD consumers must retain their immediate checks
;; and semantic accessors (nl_val_tag, nl_word_payload, container accessors).
;; Later per-type stages must change those policies before producing a new
;; pointer class; changing this storage primitive alone is not sufficient.
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

(require 'nelisp-asm-x86_64)
(require 'nelisp-asm-arm64)
(require 'nelisp-sexp-layout)

(defconst nelisp-heap-accessors-sexp-fields
  '((heap-sexp-tag-byte ptr-read-u8 0)
    (heap-sexp-tag-word ptr-read-u64 0)
    (heap-sexp-payload ptr-read-u64 8)
    (heap-sexp-data ptr-read-u64 16)
    (heap-sexp-length ptr-read-u64 24))
  "Logical legacy Sexp/view fields; their physical offsets live only here.
Tag-word retains all eight bytes (some callers deliberately use a full-word
tag), while tag-byte retains zero-extension. No immediate check is added:
these callers have already selected a pointer/view. WORD-aware operations
retain their existing immediate branches outside the storage selection.")

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

(defun nelisp-heap-accessors-lower (source)
  "Lower accessor forms in SOURCE without touching quoted Lisp data.
Do this after the census, before any source optimizer or native emitter.
Every operand is retained exactly once; dotted data is preserved."
  (if (not (consp source)) source
    (if (memq (car source) '(quote function)) source
      (let* ((entry (and (symbolp (car source))
                         (assq (car source) nelisp-heap-accessors-read-ops)))
             (field (and (symbolp (car source))
                         (assq (car source) nelisp-heap-accessors-sexp-fields))))
        (cond
         (field
          (append (list (cadr field))
                  (nelisp-heap-accessors--lower-tail (cdr source))
                  (list (caddr field))))
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
