;;; nelisp-cc-mirror-alias-stage.el --- rooted alias-entry publication -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Private staging primitives for a missing alias entry. New alias entries
;; have six slots: the existing four symbol cells, slot 4 target, and slot 5
;; presence flag. Slot 5 distinguishes an alias to nil from no alias. Legacy
;; four- and five-slot entries are not rewritten by this unit.
;;
;; PREPARE allocates the key/pair/bucket cell, parks every live Sexp in the
;; checked EvalCtx root stack, and forces a recorded-root collection. It
;; returns the root-slot base on success, and 0 for a malformed or duplicate
;; key, invalid entry, or root exhaustion. The caller must either PUBLISH
;; immediately or ABORT with the exact saved root marker. Publication performs
;; all validation before its first persistent store; after that store it executes
;; only an immediate integer record-slot write after the bucket setter has
;; completed its clone/allocation before mutating the vector head.

;;; Code:

(defconst nelisp-cc-mirror-alias-stage--source
  '(seq
    (defun nelisp_mirror_alias_stage_true_p (value-ptr)
      ;; Check the value, not merely a discriminant: flag storage is required
      ;; to contain canonical Sexp::T.
      (let ((true-slot (alloc-bytes 32 8)))
        (sexp-write-t true-slot)
        (let ((is-true (= (bf_eq2 value-ptr true-slot) 1)))
          (dealloc-bytes true-slot 32 8)
          is-true)))
    (defun nelisp_mirror_alias_stage_valid_role
        (mirror-ptr key-ptr entry-ptr env-tag-ptr table-tag-ptr entry-tag-ptr role)
      ;; Authenticate each record before reading its payload or reserving
      ;; roots. The supplied tags are fixed trusted symbols from the caller.
      (if (or (and (/= role 0) (/= role 1))
              (/= (nelisp_mirror_alias_slot_tag_matches
                    mirror-ptr env-tag-ptr 0 0) 1)
              (< (record-slot-count mirror-ptr) 1)
              (and (/= (sexp-tag key-ptr) 4) (/= (sexp-tag key-ptr) 16))
              (/= (nelisp_mirror_alias_slot_tag_matches
                    entry-ptr entry-tag-ptr 0 0) 1)
              (< (record-slot-count entry-ptr) 6))
          0
        (let ((table-ptr (record-slot-ref-ptr mirror-ptr 0)))
          (if (or (/= (nelisp_mirror_alias_slot_tag_matches
                        table-ptr table-tag-ptr 0 0) 1)
                  (< (record-slot-count table-ptr) 3)
                  (/= (sexp-tag (record-slot-ref-ptr table-ptr 0)) 2)
                  (/= (sexp-tag (record-slot-ref-ptr table-ptr 1)) 8)
                  (/= (sexp-tag (record-slot-ref-ptr table-ptr 2)) 2)
                  (< (sar (nelisp_mirror_slot_raw_word table-ptr 2) 2) 0)
                  (let ((bucket-count
                         (sar (nelisp_mirror_slot_raw_word table-ptr 0) 2)))
                    (or (<= bucket-count 0)
                        (/= (logand bucket-count (- bucket-count 1)) 0)
                        (/= bucket-count
                            (vector-len
                             (record-slot-ref-ptr table-ptr 1)))))
                  (if (= role 1)
                      (or (/= (sexp-tag (record-slot-ref-ptr entry-ptr 5)) 1)
                          (= (nelisp_mirror_alias_stage_true_p
                              (record-slot-ref-ptr entry-ptr 5)) 0)
                          (and (/= (sexp-tag (record-slot-ref-ptr entry-ptr 4)) 0)
                               (/= (sexp-tag (record-slot-ref-ptr entry-ptr 4)) 1)
                               (/= (sexp-tag (record-slot-ref-ptr entry-ptr 4)) 4)
                               (/= (sexp-tag (record-slot-ref-ptr entry-ptr 4)) 16)))
                    (or (/= (sexp-tag (record-slot-ref-ptr entry-ptr 5)) 0)
                        (/= (sexp-tag (record-slot-ref-ptr entry-ptr 4)) 0)))
                  (/= (extern-call nelisp_mirror_lookup_entry mirror-ptr key-ptr) 0))
              0
            1))))
    (defun nelisp_mirror_alias_stage_valid
        (mirror-ptr key-ptr entry-ptr env-tag-ptr table-tag-ptr entry-tag-ptr)
      ;; Keep the original single-alias validation contract.
      (nelisp_mirror_alias_stage_valid_role
       mirror-ptr key-ptr entry-ptr env-tag-ptr table-tag-ptr entry-tag-ptr 1))
    (defun nelisp_mirror_alias_stage_prepare_role
        (mirror-ptr key-ptr entry-ptr env-tag-ptr table-tag-ptr entry-tag-ptr role)
      (if (/= (nl_thread_mirror_mutation_guard mirror-ptr key-ptr) 0)
          0
        (if (= (nelisp_mirror_alias_stage_valid_role
                mirror-ptr key-ptr entry-ptr env-tag-ptr table-tag-ptr entry-tag-ptr role) 0)
            0
          (let* ((mark (nl_root_mark mirror-ptr))
             (base (nl_root_reserve_checked mirror-ptr))
             (key-root (nl_root_reserve_checked mirror-ptr))
             (entry-root (nl_root_reserve_checked mirror-ptr))
             (table-root (nl_root_reserve_checked mirror-ptr))
             (buckets-root (nl_root_reserve_checked mirror-ptr))
             (old-head-root (nl_root_reserve_checked mirror-ptr))
             (key-string-root (nl_root_reserve_checked mirror-ptr))
             (pair-root (nl_root_reserve_checked mirror-ptr))
             (head-root (nl_root_reserve_checked mirror-ptr))
             (count-root (nl_root_reserve_checked mirror-ptr))
             (idx 0)
             (valid (if (= base 0) 0
                      (if (= key-root (+ base 32))
                          (if (= entry-root (+ base 64))
                              (if (= table-root (+ base 96))
                                  (if (= buckets-root (+ base 128))
                                      (if (= old-head-root (+ base 160))
                                          (if (= key-string-root (+ base 192))
                                              (if (= pair-root (+ base 224))
                                                  (if (= head-root (+ base 256))
                                                      (if (= count-root (+ base 288)) 1 0)
                                                    0)
                                                0)
                                            0)
                                        0)
                                    0)
                                0)
                            0)
                        0))))
        (if (= valid 0)
            (seq (nl_root_release mirror-ptr mark) 0)
          (seq
           ;; `wf_copy32' is a raw Sexp copy, not an owning clone. These slots
           ;; are traced GC roots and do not change source refcounts.
           (wf_copy32 base mirror-ptr)
           (wf_copy32 key-root key-ptr)
           (wf_copy32 entry-root entry-ptr)
           (wf_copy32 table-root (record-slot-ref-ptr base 0))
           (wf_copy32 buckets-root (record-slot-ref-ptr table-root 1))
           (setq idx
                 (logand (extern-call nelisp_fnv1a key-root)
                         (- (sar (nelisp_mirror_slot_raw_word table-root 0) 2) 1)))
           (wf_copy32 old-head-root (vector-ref-ptr buckets-root idx))
           ;; Mirror keys for uninterned symbols retain the actual identity;
           ;; the established prepend path clones tag16 symbols instead of
           ;; flattening their names to Str. Interned symbols remain name-keyed.
           (if (= (sexp-tag key-root) 16)
               (extern-call nl_sexp_clone_into key-root key-string-root)
             (sexp-write-str key-string-root
                             (str-bytes-ptr key-root) (str-len key-root)))
           (let ((nil-slot (alloc-bytes 32 8)))
             (sexp-write-nil nil-slot)
             (cons-make nil-slot nil-slot pair-root)
             (cons-set-car pair-root key-string-root)
             (cons-set-cdr pair-root entry-root)
             (cons-make nil-slot nil-slot head-root)
             (cons-set-car head-root pair-root)
             (cons-set-cdr head-root old-head-root)
             (dealloc-bytes nil-slot 32 8)
             (sexp-int-make count-root
                            (+ (sar (nelisp_mirror_slot_raw_word table-root 2) 2) 1)))
           ;; The checked root stack is scanned by this recorded-root
           ;; collector. No source argument is used after this safepoint.
           (extern-call nl_gc_collect_from_recorded_roots 0)
           base))))))
    (defun nelisp_mirror_alias_stage_prepare
        (mirror-ptr key-ptr entry-ptr env-tag-ptr table-tag-ptr entry-tag-ptr)
      ;; Preserve the existing single-alias six-argument interface.
      (nelisp_mirror_alias_stage_prepare_role
       mirror-ptr key-ptr entry-ptr env-tag-ptr table-tag-ptr entry-tag-ptr 1))
    (defun nelisp_mirror_alias_stage_publish (mirror-ptr base)
      ;; Root layout is fixed by PREPARE. Guard refusal leaves the staged
      ;; objects rooted for the caller to ABORT; no persistent write occurred.
      (if (or (and (/= (sexp-tag (+ base 32)) 4)
                   (/= (sexp-tag (+ base 32)) 16))
              (/= (sexp-tag (+ base 64)) 12)
              (/= (sexp-tag (+ base 96)) 12)
              (/= (sexp-tag (+ base 128)) 8)
              (and (/= (sexp-tag (+ base 160)) 0)
                   (/= (sexp-tag (+ base 160)) 7))
              (if (= (sexp-tag (+ base 32)) 16)
                  (or (/= (sexp-tag (+ base 192)) 16)
                      (/= (extern-call nelisp_symbol_key_equal
                                       (+ base 192) (+ base 32)) 1))
                (or (/= (sexp-tag (+ base 192)) 5)
                    (/= (extern-call nelisp_symbol_key_equal
                                     (+ base 192) (+ base 32)) 1)))
              (/= (sexp-tag (+ base 224)) 7)
              (/= (sexp-tag (+ base 256)) 7)
              (/= (sexp-tag (+ base 288)) 2))
          10
        (let* ((key-root (+ base 32))
             (table-root (+ base 96))
             (buckets-root (+ base 128))
             (head-root (+ base 256))
             (count-root (+ base 288))
             (idx (logand (extern-call nelisp_fnv1a key-root)
                          (- (sar (nelisp_mirror_slot_raw_word table-root 0) 2) 1))))
          (if (/= (nl_thread_mirror_mutation_guard mirror-ptr key-root) 0)
              5
            (seq
             (wf_dirty)
             ;; `vector-slot-set' emits a guaranteed truthy 1 sentinel after
             ;; cloning; after its first persistent write only the immediate
             ;; count store runs.
             (vector-slot-set buckets-root idx head-root)
             (record-slot-set table-root 2 count-root)
             0)))))
    (defun nelisp_mirror_alias_stage_insert
        (mirror-ptr key-ptr entry-ptr env-tag-ptr table-tag-ptr entry-tag-ptr)
      ;; Keep PREPARE, its forced collection, and PUBLISH in one native call
      ;; so no evaluator boundary can rewind the checked root stack.
      (let* ((mark (nl_root_mark mirror-ptr))
             (base (nelisp_mirror_alias_stage_prepare
                    mirror-ptr key-ptr entry-ptr env-tag-ptr
                    table-tag-ptr entry-tag-ptr)))
        (if (<= base 0)
            1
          (let ((status (nelisp_mirror_alias_stage_publish mirror-ptr base)))
            (nelisp_mirror_alias_stage_abort mirror-ptr mark)
            status))))
    (defun nelisp_mirror_alias_stage_prepare_pair
        (mirror-ptr key-a entry-a key-b entry-b env-tag-ptr table-tag-ptr entry-tag-ptr)
      ;; Stage two absent records and return the first stage-root base. The
      ;; caller must commit or release the exact mark in the same native call.
      (let* ((mirror-root (nl_root_reserve_checked mirror-ptr))
             (key-a-root (nl_root_reserve_checked mirror-ptr))
             (entry-a-root (nl_root_reserve_checked mirror-ptr))
             (key-b-root (nl_root_reserve_checked mirror-ptr))
             (entry-b-root (nl_root_reserve_checked mirror-ptr))
             (env-tag-root (nl_root_reserve_checked mirror-ptr))
             (table-tag-root (nl_root_reserve_checked mirror-ptr))
             (entry-tag-root (nl_root_reserve_checked mirror-ptr))
             (epoch (atomic-fetch-add 268435544 0))
             (roots-valid
              (if (or (= mirror-root 0) (= key-a-root 0) (= entry-a-root 0)
                      (= key-b-root 0) (= entry-b-root 0) (= env-tag-root 0)
                      (= table-tag-root 0) (= entry-tag-root 0)
                      (/= key-a-root (+ mirror-root 32))
                      (/= entry-a-root (+ mirror-root 64))
                      (/= key-b-root (+ mirror-root 96))
                      (/= entry-b-root (+ mirror-root 128))
                      (/= env-tag-root (+ mirror-root 160))
                      (/= table-tag-root (+ mirror-root 192))
                      (/= entry-tag-root (+ mirror-root 224)))
                  0 1))
             (same-key 0)
             (base-a 0)
             (base-b 0)
             (copy-buckets 0)
             (copy-table 0)
             (new-count 0)
             (status 0)
             (size 0)
             (i 0))
        (if (= roots-valid 0)
            (setq status 2)
          (seq
           ;; Root every argument that can be used after a collection before
           ;; either prepare call. The prepare calls may collect independently.
           (wf_copy32 mirror-root mirror-ptr)
           (wf_copy32 key-a-root key-a)
           (wf_copy32 entry-a-root entry-a)
           (wf_copy32 key-b-root key-b)
           (wf_copy32 entry-b-root entry-b)
           (wf_copy32 env-tag-root env-tag-ptr)
           (wf_copy32 table-tag-root table-tag-ptr)
           (wf_copy32 entry-tag-root entry-tag-ptr)
           ;; Validate both roles before identity comparison or any allocation.
           (if (or (/= (nelisp_mirror_alias_stage_valid_role
                        mirror-root key-a-root entry-a-root env-tag-root
                        table-tag-root entry-tag-root 1) 1)
                   (/= (nelisp_mirror_alias_stage_valid_role
                        mirror-root key-b-root entry-b-root env-tag-root
                        table-tag-root entry-tag-root 0) 1)
                   (or (and (/= (sexp-tag (record-slot-ref-ptr entry-a-root 4)) 4)
                            (/= (sexp-tag (record-slot-ref-ptr entry-a-root 4)) 16))
                       (and (/= (sexp-tag key-b-root) 4)
                            (/= (sexp-tag key-b-root) 16))
                       (/= (extern-call nelisp_symbol_key_equal
                                        (record-slot-ref-ptr entry-a-root 4)
                                        key-b-root) 1)))
               (setq status 1)
             (seq
              (setq same-key
                    (= (extern-call nelisp_symbol_key_equal
                                    key-a-root key-b-root) 1))
              (if same-key (setq status 1))
              (if (= status 0)
                  (setq base-a
                        (nelisp_mirror_alias_stage_prepare_role
                         mirror-root key-a-root entry-a-root env-tag-root
                         table-tag-root entry-tag-root 1))))))
        (if (and (= status 0) (<= base-a 0)) (setq status 1))
        (if (= status 0)
            (setq base-b
                  (nelisp_mirror_alias_stage_prepare_role
                   mirror-root key-b-root entry-b-root env-tag-root
                   table-tag-root entry-tag-root 0)))
        (if (and (= status 0) (<= base-b 0)) (setq status 1))
        (if (= status 0)
            (let* ((copy-buckets (nl_root_reserve_checked mirror-root))
                   (copy-table (nl_root_reserve_checked mirror-root))
                   (new-count (nl_root_reserve_checked mirror-root)))
              (if (or (= copy-buckets 0) (= copy-table 0) (= new-count 0)
                      (/= copy-table (+ copy-buckets 32))
                      (/= new-count (+ copy-buckets 64))
                      (/= copy-buckets (+ base-b 320)))
                  (setq status 2)
                (seq
                 (setq size (vector-len (+ base-a 128)))
                 (vector-make size copy-buckets)
                 (setq i 0)
                 (while (< i size)
                   (vector-slot-set copy-buckets i
                                    (vector-ref-ptr (+ base-a 128) i))
                   (setq i (+ i 1)))
                 (let ((idx-a
                        (logand (extern-call nelisp_fnv1a (+ base-a 32))
                                (- (sar (nelisp_mirror_slot_raw_word
                                         (+ base-a 96) 0) 2) 1)))
                       (idx-b
                        (logand (extern-call nelisp_fnv1a (+ base-b 32))
                                (- (sar (nelisp_mirror_slot_raw_word
                                         (+ base-b 96) 0) 2) 1))))
                 (if (= idx-a idx-b)
                     (seq
                      (cons-set-cdr (+ base-b 256) (+ base-a 256))
                      (vector-slot-set copy-buckets idx-b (+ base-b 256)))
                   (seq
                    (vector-slot-set copy-buckets idx-a (+ base-a 256))
                    (vector-slot-set copy-buckets idx-b (+ base-b 256))))))
                 (record-make table-tag-root 3 copy-table)
                 (record-slot-set copy-table 0 (record-slot-ref-ptr (+ base-a 96) 0))
                 (record-slot-set copy-table 1 copy-buckets)
                 (sexp-int-make new-count
                                (+ (sexp-int-unwrap
                                    (record-slot-ref-ptr (+ base-a 96) 2)) 2))
                 (record-slot-set copy-table 2 new-count)
                 ;; The table has copied its tag by now. Preserve the epoch
                 ;; in the trusted-tag root slot for the paired commit call.
                 (sexp-int-make entry-tag-root epoch)
                 (extern-call nl_gc_collect_from_recorded_roots 0))))
        (if (= status 0) base-a
          (if (= roots-valid 0) 0 -1)))))
    (defun nelisp_mirror_alias_stage_commit_pair (mirror-ptr base-a)
      ;; BASE-A and the saved epoch are valid only while the caller retains the
      ;; exact prepare mark. No allocation or callback occurs before checks.
      (let* ((mirror-root (- base-a 256))
             (key-a-root (- base-a 224))
             (key-b-root (- base-a 160))
             (entry-tag-root (- base-a 32))
             (base-b (+ base-a 320))
             (copy-buckets (+ base-b 320))
             (copy-table (+ copy-buckets 32))
             (new-count (+ copy-buckets 64))
             (idx-a 0)
             (idx-b 0)
             (expected-epoch 0))
        (if (or (<= base-a 0)
                (/= (sexp-tag (+ base-a 96)) 12)
                (/= (sexp-tag (+ base-a 128)) 8)
                (and (/= (sexp-tag (+ base-a 160)) 0)
                     (/= (sexp-tag (+ base-a 160)) 7))
                (/= (sexp-tag (+ base-a 256)) 7)
                (/= (sexp-tag (+ base-a 288)) 2)
                (/= (sexp-tag (+ base-b 96)) 12)
                (/= (sexp-tag (+ base-b 128)) 8)
                (and (/= (sexp-tag (+ base-b 160)) 0)
                     (/= (sexp-tag (+ base-b 160)) 7))
                (/= (sexp-tag (+ base-b 256)) 7)
                (/= (sexp-tag (+ base-b 288)) 2)
                (/= (sexp-tag copy-buckets) 8)
                (/= (sexp-tag copy-table) 12)
                (/= (sexp-tag new-count) 2)
                (/= (sexp-tag entry-tag-root) 2))
            2
          (seq
           ;; The bucket indices are computed only after every staged record
           ;; and table payload has passed its tag checks.
           (setq idx-a
                 (logand (extern-call nelisp_fnv1a (+ base-a 32))
                         (- (sar (nelisp_mirror_slot_raw_word
                                  (+ base-a 96) 0) 2) 1)))
           (setq idx-b
                 (logand (extern-call nelisp_fnv1a (+ base-b 32))
                         (- (sar (nelisp_mirror_slot_raw_word
                                  (+ base-b 96) 0) 2) 1)))
           (setq expected-epoch (sexp-int-unwrap entry-tag-root))
           (if (or (/= expected-epoch (atomic-fetch-add 268435544 0))
                   (/= (wf_raw_eq (record-slot-ref-ptr mirror-root 0)
                                  (+ base-a 96)) 1)
                   (/= (wf_raw_eq (vector-ref-ptr (+ base-a 128) idx-a)
                                  (+ base-a 160)) 1)
                   (/= (wf_raw_eq (vector-ref-ptr (+ base-a 128) idx-b)
                                  (+ base-b 160)) 1))
               6
             (if (or (/= (nl_thread_mirror_mutation_guard
                           mirror-ptr key-a-root) 0)
                     (/= (nl_thread_mirror_mutation_guard
                           mirror-ptr key-b-root) 0))
                 5
               (seq
                (wf_dirty)
                (record-slot-set mirror-root 0 copy-table)
                0)))))))
    (defun nelisp_mirror_alias_stage_insert_pair
        (mirror-ptr key-a entry-a key-b entry-b env-tag-ptr table-tag-ptr entry-tag-ptr)
      ;; Ordinary path: prepare, collect, validate, and publish in one native
      ;; invocation. The staged roots never cross an evaluator boundary.
      (let* ((mark (nl_root_mark mirror-ptr))
             (base-a (nelisp_mirror_alias_stage_prepare_pair
                      mirror-ptr key-a entry-a key-b entry-b
                      env-tag-ptr table-tag-ptr entry-tag-ptr))
             (status (if (<= base-a 0) 1
                       (nelisp_mirror_alias_stage_commit_pair mirror-ptr base-a))))
        (nl_root_release mirror-ptr mark)
        status))
    ;; Diagnostic-only entry point: it shares the exact prepare/GC/release
    ;; path and deliberately skips publication. Do not register it as a Lisp
    ;; builtin or in the production alias frontend.
    (defun nelisp_mirror_alias_stage_abort_probe
        (mirror-ptr key-ptr entry-ptr env-tag-ptr table-tag-ptr entry-tag-ptr)
      (let* ((mark (nl_root_mark mirror-ptr))
             (base (nelisp_mirror_alias_stage_prepare
                    mirror-ptr key-ptr entry-ptr env-tag-ptr
                    table-tag-ptr entry-tag-ptr)))
        (if (<= base 0)
            1
          (seq (nelisp_mirror_alias_stage_abort mirror-ptr mark) 3))))
    ;; Deliberately simulate omitting the stage roots from the recorded set:
    ;; clear the private staging slots, release the exact mark, collect, then
    ;; require PUBLISH to reject the invalid root layout before any dereference
    ;; or mutation. The fresh PAIR/HEAD are not arguments to this function.
    (defun nelisp_mirror_alias_stage_root_disabled_probe
        (mirror-ptr key-ptr entry-ptr env-tag-ptr table-tag-ptr entry-tag-ptr)
      (let* ((mark (nl_root_mark mirror-ptr))
             (base (nelisp_mirror_alias_stage_prepare
                    mirror-ptr key-ptr entry-ptr env-tag-ptr
                    table-tag-ptr entry-tag-ptr)))
        (if (<= base 0)
            1
          (seq
           (sexp-write-nil base)
           (sexp-write-nil (+ base 32))
           (sexp-write-nil (+ base 64))
           (sexp-write-nil (+ base 96))
           (sexp-write-nil (+ base 128))
           (sexp-write-nil (+ base 160))
           (sexp-write-nil (+ base 192))
           (sexp-write-nil (+ base 224))
           (sexp-write-nil (+ base 256))
           (sexp-write-nil (+ base 288))
           (nl_root_release mirror-ptr mark)
           (extern-call nl_gc_collect_from_recorded_roots 0)
           (nelisp_mirror_alias_stage_publish mirror-ptr base)))))
    (defun nelisp_mirror_alias_stage_abort (mirror-ptr marker)
      (nl_root_release mirror-ptr marker)))
  "Private rooted prepare/publish/abort primitives for a missing six-slot alias entry.")

(defconst nelisp-cc-mirror-alias-stage-source
  nelisp-cc-mirror-alias-stage--source
  "Build-source facade for the private mirror alias staging unit.")

(provide 'nelisp-cc-mirror-alias-stage)

;;; nelisp-cc-mirror-alias-stage.el ends here
