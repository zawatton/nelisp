;;; nelisp-cc-nlvector-alloc.el --- Compact vector allocation -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Doc 216 S4.1: one allocation contains a packed kind/length word followed
;; by eight-byte element WORDs. The allocator returns the untagged backing
;; address to native constructors; retained Lisp values use address|6.
;; The tracing collector owns lifetime; there is no Vec header or refcount.
;;; Code:
(defconst nelisp-cc-nlvector-alloc--source
  '(seq
    (defun nl_alloc_vector_fill (data-ptr i cap)
      (let ((k i))
        (while (< k cap)
          (seq (ptr-write-u64 (+ data-ptr (* k 8)) 0 3)
               (setq k (+ k 1)))) data-ptr))
    ;; Callers that overwrite every slot can skip the initial nil fill.
    (defun nl_alloc_vector_uninit (n)
      (let* ((bytes (+ 8 (* n 8)))
             (box (alloc-bytes bytes 8))
             (policy (data-addr nl_gc_policy)))
        (seq (ptr-write-u64 policy 40 (+ (heap-read-u64 policy 40) bytes))
             (ptr-write-u64 box 0 (+ 8 (* n 256)))
             box)))
    (defun nl_alloc_vector (capacity)
      (let* ((n (if (< capacity 0) 0 capacity)))
        (if (= n 0)
            ;; GNU's zero_vector: no slots, never mutable or collectible.
            ;; Driver BSS is outside the arena on every target; repeated
            ;; initialization writes the same header, with no heap pointer.
            (let* ((box (data-addr nl_empty_vector)))
              (seq (ptr-write-u64 box 0 8) box))
          (let* ((box (nl_alloc_vector_uninit n)))
            (seq (nl_alloc_vector_fill (+ box 8) 0 n) box))))))
  "Native compiler source for compact vector backing allocation.")
(provide 'nelisp-cc-nlvector-alloc)
;;; nelisp-cc-nlvector-alloc.el ends here
