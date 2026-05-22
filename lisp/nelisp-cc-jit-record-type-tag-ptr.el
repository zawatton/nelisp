;;; nelisp-cc-jit-record-type-tag-ptr.el --- Phase 47 nl_record_type_tag_ptr swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for the `nl_record_type_tag_ptr' Rust extern in
;; `build-tool/src/jit/box_accessor.rs'.  The Rust body was:
;;
;;   match &*arg {
;;       Sexp::Record(rec) => &rec.type_tag as *const Sexp,
;;       _ => std::ptr::null(),
;;   }
;;
;; `NlRecordRef' is `#[repr(transparent)]' over `NonNull<NlRecord>',
;; and `Sexp::Record(r)' stores the `NlRecordRef' with its box pointer
;; at SEXP_PAYLOAD_OFFSET=8.  Since `NlRecord::type_tag' is at offset 0
;; of the `#[repr(C)]' `NlRecord' struct (asserted by `offset_of!' in
;; `nlrecord.rs'), the NlRecord* == &type_tag.  Therefore
;; `sexp-payload-ptr-record(arg)' returns the NlRecord box pointer which
;; IS `*const Sexp' for the inline `type_tag' field --- no arithmetic
;; needed.
;;
;; On non-Record variants `sexp-payload-ptr-record' returns 0.  The
;; explicit tag guard preserves the Rust body semantics and avoids
;; dereferencing non-Record boxed payloads.
;;
;; ABI constants used (S100.B frozen, `nelisp-sexp-layout.el'):
;;   SEXP_TAG_RECORD    = 12
;;   SEXP_PAYLOAD_OFFSET = 8  (= sexp-payload-ptr-record reads at this offset)
;;   NlRecord::type_tag @ offset 0 (= box ptr == &type_tag)
;;
;; ABI contract: `(*const Sexp) -> *const Sexp' encoded as i64.
;;   Returns: i64 = NlRecord* (= &type_tag as *const Sexp) when arg is
;;            Sexp::Record (tag=12), else 0 (null pointer).
;;
;; Linker wiring: `build-tool/src/jit/bridge.rs' declares
;; `fn nl_record_type_tag_ptr()' in the `extern "C"' block and adds it
;; to `_ELISP_ARCHIVE_ANCHOR'.

;;; Code:

(defconst nelisp-cc-jit-record-type-tag-ptr--source
  (quote (defun nl_record_type_tag_ptr (arg)
     ;; arg: *const Sexp.
     ;; Returns: i64 = NlRecord* (= &type_tag as *const Sexp) when
     ;;          tag == 12 (Sexp::Record), else 0 (null).
     ;;
     ;; sexp-payload-ptr-record reads the NonNull<NlRecord> box
     ;; pointer stored at SEXP_PAYLOAD_OFFSET=8 inside the Sexp
     ;; enum.
     ;; Since NlRecord::type_tag is at offset 0, the
     ;; box pointer IS *const Sexp for the type_tag field.
     (if (= (sexp-tag arg) 12)
         (sexp-payload-ptr-record arg)
       0)))
  "Phase 47 source for nl_record_type_tag_ptr (jit/box_accessor.rs to elisp).

Returns the NlRecord* (= &type_tag as *const Sexp, NlRecord offset 0)
when arg is Sexp::Record (tag 12), else 0 (null).")

(provide (quote nelisp-cc-jit-record-type-tag-ptr))

;;; nelisp-cc-jit-record-type-tag-ptr.el ends here
