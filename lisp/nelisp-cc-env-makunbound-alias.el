;;; nelisp-cc-env-makunbound-alias.el --- alias detach for makunbound -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Private AOT helper for the reader's `makunbound' path. It detaches a
;; genuine slot-4 alias and unbinds only that alias's own value entry.

;;; Code:

(defconst nelisp-cc-env-makunbound-alias--source
  '(seq
    (defun nelisp_env_makunbound_alias (mirror-ptr name-ptr unbound-ptr _pad)
      ;; Status: 0=detached, 1=malformed, 2=cycle, 3=nonalias,
      ;; 4=constant, 5=mutation guard refused (signal already stashed).
      (let ((entry (extern-call nelisp_mirror_lookup_entry mirror-ptr name-ptr)))
        (if (= entry 0)
            3
          (let ((candidate (nelisp_env_alias_slot_candidate entry 0)))
            (if (= candidate 3)
                3
            (if (/= candidate 0)
                  1
                ;; Authenticate and resolve the edge before any mutation.
                ;; These two raw buffers never hold owned Lisp references.
                (let ((terminal-address (alloc-bytes 8 8))
                      (nil-scratch (alloc-bytes 32 8))
                      (status 1))
                  (sexp-write-nil nil-scratch)
                  (setq status
                        (nelisp_env_alias_canonicalize
                         mirror-ptr entry name-ptr terminal-address))
                  (if (= status 0)
                      (let ((terminal (ptr-read-u64 terminal-address 0)))
                        (if (= (extern-call nelisp_mirror_is_constant
                                            mirror-ptr name-ptr) 1)
                            (setq status 4)
                          (if (= (extern-call nelisp_mirror_is_constant
                                              mirror-ptr terminal) 1)
                              (setq status 4)
                            ;; Reject a concurrent/invalid mirror mutation
                            ;; before the first store. A refusal has already
                            ;; placed its signal in the runtime stash.
                            (if (= (extern-call nl_thread_mirror_mutation_guard
                                                mirror-ptr name-ptr) 1)
                                (setq status 5)
                              ;; The value clone escapes this evaluation arena.
                              ;; Mark the persistent mutation before installing it.
                              (wf_dirty)
                              ;; Both slot writes follow all read-only checks.
                              (record-slot-set entry 0 unbound-ptr)
                              (record-slot-set entry 4 nil-scratch)
                              (setq status 0)))))
                    (if (= status 2)
                        (setq status 2)
                      (setq status 1)))
                  (dealloc-bytes nil-scratch 32 8)
                  (dealloc-bytes terminal-address 8 8)
                  status)))))))
    (defun nelisp_env_makunbound_alias_signal_cycle (offender _pad)
      ;; Match the established signal stash layout used by bf_setting_constant.
      (let ((name-bytes (alloc-bytes 32 8))
            (nil-slot (alloc-bytes 32 8)))
        (ptr-write-u64 name-bytes 0 8515571774868650339)
        (ptr-write-u64 name-bytes 8 3271139874151428705)
        (ptr-write-u64 name-bytes 16 8386658473162862185)
        (ptr-write-u64 name-bytes 24 7237481)
        (nl_alloc_symbol name-bytes 27 268435480)
        (sexp-write-nil nil-slot)
        (nelisp_cons_construct offender nil-slot 268435512)
        (ptr-write-u64 268435472 0 1)
        (atomic-fetch-add 268435544 1)
        (dealloc-bytes name-bytes 32 8)
        (dealloc-bytes nil-slot 32 8)
        1))
    (defun nelisp_env_makunbound_alias_signal_malformed (_offender _pad)
      ;; Malformed internal metadata is an infrastructure error, never a
      ;; fabricated setting-constant condition.
      (let ((name-bytes (alloc-bytes 8 8)))
        (ptr-write-u64 name-bytes 0 491496043109)
        (nl_alloc_symbol name-bytes 5 268435480)
        (sexp-write-nil 268435512)
        (ptr-write-u64 268435472 0 1)
        (atomic-fetch-add 268435544 1)
        (dealloc-bytes name-bytes 8 8)
        1)))
  "Private alias detach. Status 0=detached, 1=malformed, 2=cycle, 3=nonalias, 4=constant, 5=guard-refused.")

(defconst nelisp-cc-env-makunbound-alias-source
  nelisp-cc-env-makunbound-alias--source
  "Build-source facade for private alias-aware `makunbound'.")

(provide 'nelisp-cc-env-makunbound-alias)

;;; nelisp-cc-env-makunbound-alias.el ends here
