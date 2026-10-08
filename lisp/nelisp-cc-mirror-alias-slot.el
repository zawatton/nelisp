;;; nelisp-cc-mirror-alias-slot.el --- private symbol-entry slot access -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Private tagged-record accessors for symbol-entry slot 4. This native
;; code exists because reading and mutating tagged record slots requires
;; object-aware primitives (AGENTS.md minimal-native clause 1). It is not
;; exposed through the reader builtin table and does not install aliases.
;; Status: 0 = stored/read, 1 = invalid record/tag/target, 2 = legacy
;; four-slot symbol-entry (getter writes nil; setter leaves it unchanged).

;;; Code:

(defconst nelisp-cc-mirror-alias-slot--source
  '(seq
    (defun nelisp_mirror_alias_slot_tag_matches
        (entry-ptr expected-tag-ptr tag-out _pad)
      ;; `record-type-tag' is a raw borrowed copy, not refcounted. The entry
      ;; remains live through this synchronous `symbol-eq' (no GC point); do
      ;; not drop the borrowed copy. Initialize and free only the raw slot.
      (let ((matches (alloc-bytes 32 8)))
        (sexp-write-nil matches)
        (if (= (sexp-tag entry-ptr) 12)
            (seq (record-type-tag entry-ptr matches)
                 (let ((same (= (symbol-eq matches expected-tag-ptr) 1)))
                   (dealloc-bytes matches 32 8)
                   (if same 1 0)))
          (seq (dealloc-bytes matches 32 8) 0))))
    (defun nelisp_mirror_alias_slot_get
        (entry-ptr expected-tag-ptr result-slot _pad)
      (if (= (nelisp_mirror_alias_slot_tag_matches
              entry-ptr expected-tag-ptr 0 0) 0)
          1
        (if (> (record-slot-count entry-ptr) 4)
            (seq (record-slot-ref entry-ptr 4 result-slot) 0)
          (seq (sexp-write-nil result-slot) 2))))
    (defun nelisp_mirror_alias_slot_set
        (entry-ptr expected-tag-ptr target-ptr _pad)
      (let ((target-tag (sexp-tag target-ptr)))
        (if (if (= target-tag 0) 1 (if (= target-tag 4) 1 0))
            (if (= (nelisp_mirror_alias_slot_tag_matches
                    entry-ptr expected-tag-ptr 0 0) 0)
                1
              (if (> (record-slot-count entry-ptr) 4)
                  (seq (record-slot-set entry-ptr 4 target-ptr) 0)
                2))
          1))))
  "Private AOT helpers for validated symbol-entry alias slot access.
Use existing record tag/count/ref/set grammar operations; no raw NlRecord
offsets are read or written.")

(defconst nelisp-cc-mirror-alias-slot-source
  nelisp-cc-mirror-alias-slot--source
  "Build-source facade for the private alias-slot AOT module.")

(provide 'nelisp-cc-mirror-alias-slot)

;;; nelisp-cc-mirror-alias-slot.el ends here
