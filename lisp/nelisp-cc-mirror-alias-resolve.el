;;; nelisp-cc-mirror-alias-resolve.el --- bounded private alias resolver -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Resolve variable-alias chains in the active symbol-entry mirror. This
;; helper is private and does not install or expose `defvaralias'.

;;; Code:

(defconst nelisp-cc-mirror-alias-resolve--source
  '(seq
    (defun nelisp_mirror_alias_resolve_borrowed
       (mirror-ptr start-ptr expected-env-tag-ptr expected-table-tag-ptr
                   expected-entry-tag-ptr result-address-ptr)
     ;; Visited contains borrowed Sexp* addresses. No operation in the walk
     ;; allocates a Lisp object or reaches a GC point; mirror owns each entry.
     ;; RESULT-ADDRESS-PTR is raw u64 storage, not a Sexp output slot. On
     ;; success it receives the borrowed terminal symbol address, valid only
     ;; while MIRROR-PTR remains rooted and the caller stays in this synchronous
     ;; native path without allocation, reentry, or GC.
     (let ((visited (alloc-bytes 512 8))
           (current start-ptr)
           (entry 0)
           (target 0)
           (count 0)
           (scan 0)
           (seen 0)
           (status 0)
           (done 0))
       (ptr-write-u64 result-address-ptr 0 0)
       (if (or (/= (sexp-tag mirror-ptr) 12)
               (= (nelisp_mirror_alias_slot_tag_matches
                   mirror-ptr expected-env-tag-ptr 0 0) 0)
               (<= (record-slot-count mirror-ptr) 0))
           (setq status 1)
         (let ((table (record-slot-ref-ptr mirror-ptr 0)))
           (if (or (/= (sexp-tag table) 12)
                   (= (nelisp_mirror_alias_slot_tag_matches
                       table expected-table-tag-ptr 0 0) 0)
                   (<= (record-slot-count table) 1))
               (setq status 1)
             (let ((bucket-count (record-slot-ref-ptr table 0))
                   (buckets (record-slot-ref-ptr table 1)))
               (if (or (/= (sexp-tag bucket-count) 2)
                       (/= (sexp-tag buckets) 8)
                       (<= (sexp-int-unwrap bucket-count) 0)
                       (/= (logand (sexp-int-unwrap bucket-count)
                                   (- (sexp-int-unwrap bucket-count) 1)) 0)
                       (> (sexp-int-unwrap bucket-count) (vector-len buckets)))
                   (setq status 1)
                 (if (/= (sexp-tag start-ptr) 4)
                     (setq status 1)
                   (while (and (= done 0) (= status 0) (< count 64))
                     (setq entry (extern-call nelisp_mirror_lookup_entry mirror-ptr current))
                     (if (= entry 0)
                         (setq done 1)
                       (if (= (nelisp_mirror_alias_slot_tag_matches
                               entry expected-entry-tag-ptr 0 0) 0)
                           (setq status 1)
                         (if (<= (record-slot-count entry) 4)
                             (setq done 1)
                           (setq target (record-slot-ref-ptr entry 4))
                           (if (= (sexp-tag target) 0)
                               (setq done 1)
                             (if (/= (sexp-tag target) 4)
                                 (setq status 1)
                               (setq scan 0)
                               (setq seen 0)
                               (while (and (< scan count) (= seen 0))
                                 (if (= (symbol-eq current (ptr-read-u64 visited (* scan 8))) 1)
                                     (setq seen 1)
                                   (setq scan (+ scan 1))))
                               (if (= seen 1)
                                   (setq status 2)
                                 (ptr-write-u64 visited (* count 8) current)
                                 (setq count (+ count 1))
                                 (setq current target))))))))))))))
       (if (and (= status 0) (= done 0))
           (setq status 2))
       (if (= status 0)
           (ptr-write-u64 result-address-ptr 0 current))
       (dealloc-bytes visited 512 8)
       status))
    (defun nelisp_mirror_alias_resolve
        (mirror-ptr start-ptr expected-env-tag-ptr expected-table-tag-ptr
                    expected-entry-tag-ptr result-slot)
      ;; Preserve the established Sexp-cloning ABI. The inner walk returns a
      ;; borrowed pointer; clone it immediately while the mirror is rooted.
      (let ((result-address (alloc-bytes 8 8))
            (status 0))
        (sexp-write-nil result-slot)
        (setq status
              (nelisp_mirror_alias_resolve_borrowed
               mirror-ptr start-ptr expected-env-tag-ptr expected-table-tag-ptr
               expected-entry-tag-ptr result-address))
        (if (= status 0)
            (extern-call nl_sexp_clone_into
                         (ptr-read-u64 result-address 0) result-slot)
          0)
        (dealloc-bytes result-address 8 8)
        status)))
  "AOT bounded alias-chain resolver; status 0=resolved, 1=invalid, 2=cycle/hop limit.")

(defconst nelisp-cc-mirror-alias-resolve-source
  nelisp-cc-mirror-alias-resolve--source
  "Build-source facade for the private alias resolver.")

(provide 'nelisp-cc-mirror-alias-resolve)

;;; nelisp-cc-mirror-alias-resolve.el ends here
