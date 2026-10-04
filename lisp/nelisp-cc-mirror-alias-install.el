;;; nelisp-cc-mirror-alias-install.el --- private variable alias installation -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Install a variable alias in a six-slot symbol-entry. Existing entries use
;; the checked slot store; the narrow absent-both branch stages two entries.

;;; Code:

(defconst nelisp-cc-mirror-alias-install--source
  '(seq
    (defun nelisp_mirror_alias_install_missing_pair
        (mirror-ptr alias-ptr target-ptr env-tag-ptr table-tag-ptr entry-tag-ptr)
      ;; Construct both fresh six-slot entries under checked EvalCtx roots.
      ;; The alias and target remain absent from the mirror until the pair
      ;; transaction validates and publishes one composed table.
      (let* ((mark (nl_root_mark mirror-ptr))
             (mirror-root (nl_root_reserve_checked mirror-ptr))
             (alias-root (nl_root_reserve_checked mirror-ptr))
             (target-root (nl_root_reserve_checked mirror-ptr))
             (unbound-root (nl_root_reserve_checked mirror-ptr))
             (env-tag-root (nl_root_reserve_checked mirror-ptr))
             (table-tag-root (nl_root_reserve_checked mirror-ptr))
             (entry-tag-root (nl_root_reserve_checked mirror-ptr))
             (alias-entry-root (nl_root_reserve_checked mirror-ptr))
             (target-entry-root (nl_root_reserve_checked mirror-ptr))
             (presence-root (nl_root_reserve_checked mirror-ptr))
             (roots-valid
              (if (or (= mirror-root 0)
                      (/= alias-root (+ mirror-root 32))
                      (/= target-root (+ mirror-root 64))
                      (/= unbound-root (+ mirror-root 96))
                      (/= env-tag-root (+ mirror-root 128))
                      (/= table-tag-root (+ mirror-root 160))
                      (/= entry-tag-root (+ mirror-root 192))
                      (/= alias-entry-root (+ mirror-root 224))
                      (/= target-entry-root (+ mirror-root 256))
                      (/= presence-root (+ mirror-root 288)))
                  0 1))
             (status 1))
        (if (= roots-valid 0)
            (seq (nl_root_release mirror-ptr mark) status)
          (seq
           ;; Raw slot copies are roots only; no refcount ownership is implied.
           (wf_copy32 mirror-root mirror-ptr)
           (wf_copy32 alias-root alias-ptr)
           (wf_copy32 target-root target-ptr)
           (wf_copy32 unbound-root (+ mirror-ptr 64))
           (wf_copy32 env-tag-root env-tag-ptr)
           (wf_copy32 table-tag-root table-tag-ptr)
           (wf_copy32 entry-tag-root entry-tag-ptr)
           (sexp-write-nil alias-entry-root)
           (sexp-write-nil target-entry-root)
           (sexp-write-nil presence-root)
           (sexp-write-t presence-root)
           (if (and (record-make entry-tag-root 6 alias-entry-root)
                    (record-slot-set alias-entry-root 0 unbound-root)
                    (record-slot-set alias-entry-root 1 unbound-root)
                    (record-slot-set alias-entry-root 4 target-root)
                    (record-slot-set alias-entry-root 5 presence-root)
                    (record-make entry-tag-root 6 target-entry-root)
                    (record-slot-set target-entry-root 0 unbound-root)
                    (record-slot-set target-entry-root 1 unbound-root))
               (setq status
                     (extern-call nelisp_mirror_alias_stage_insert_pair
                                  mirror-ptr alias-root alias-entry-root
                                  target-root target-entry-root env-tag-root
                                  table-tag-root entry-tag-root))
             0)
           (nl_root_release mirror-ptr mark)
           status))))
    (defun nelisp_mirror_alias_install
       (mirror-ptr alias-ptr target-ptr expected-env-tag-ptr
                   expected-table-tag-ptr expected-entry-tag-ptr)
     ;; This raw scratch holds only the resolver's immutable Symbol result.
     ;; The target walk below is read-only; free scratch before the slot store.
     (let ((resolved (alloc-bytes 32 8))
           (alias-entry 0)
           (target-entry 0)
           (current target-ptr)
           (entry 0)
           (next 0)
           (steps 0)
           (done 0)
           (status 0))
       (sexp-write-nil resolved)
       (if (or (/= (sexp-tag alias-ptr) 4)
               (/= (sexp-tag target-ptr) 4))
           (setq status 1)
         (setq status
               (extern-call nelisp_mirror_alias_resolve
                            mirror-ptr target-ptr expected-env-tag-ptr
                            expected-table-tag-ptr expected-entry-tag-ptr
                            resolved)))
       (if (= status 0)
           (setq alias-entry
                 (extern-call nelisp_mirror_lookup_entry mirror-ptr alias-ptr))
         0)
       (if (= status 0)
           (setq target-entry
                 (extern-call nelisp_mirror_lookup_entry mirror-ptr target-ptr))
         0)
       (if (and (= status 0) (/= alias-entry 0)
                (or (= (nelisp_mirror_alias_slot_tag_matches
                        alias-entry expected-entry-tag-ptr 0 0) 0)
                    (<= (record-slot-count alias-entry) 4)))
           (setq status 1)
         0)
       ;; The resolver proves this chain is well-formed, acyclic, and within
       ;; the limit. Walk it once more to detect ALIAS-PTR before following
       ;; ALIAS-PTR's existing slot, which canonical-terminal comparison alone
       ;; cannot detect when an alias is being retargeted.
       (while (and (= status 0) (= done 0) (< steps 64))
         (if (= (symbol-eq current alias-ptr) 1)
             (setq status 2)
           (setq entry
                 (extern-call nelisp_mirror_lookup_entry mirror-ptr current))
           (if (= entry 0)
               (setq done 1)
             (if (= (nelisp_mirror_alias_slot_tag_matches
                     entry expected-entry-tag-ptr 0 0) 0)
                 (setq status 1)
               (if (<= (record-slot-count entry) 4)
                   (setq done 1)
                 (setq next (record-slot-ref-ptr entry 4))
                 (if (= (sexp-tag next) 0)
                     (setq done 1)
                   (if (/= (sexp-tag next) 4)
                       (setq status 1)
                     (setq current next)
                     (setq steps (+ steps 1)))))))))
       (if (and (= status 0) (= done 0))
           (setq status 2))
       (dealloc-bytes resolved 32 8)
       (if (= status 0)
           (if (= alias-entry 0)
               (if (= target-entry 0)
                   (setq status
                         (nelisp_mirror_alias_install_missing_pair
                          mirror-ptr alias-ptr target-ptr
                          expected-env-tag-ptr expected-table-tag-ptr
                          expected-entry-tag-ptr))
                 (setq status 1))
             (setq status
                   (extern-call nelisp_mirror_alias_slot_set
                                alias-entry expected-entry-tag-ptr
                                target-ptr 0)))
         0)
       status)))
  "Private alias installer; status 0=installed, 1=invalid/unsupported, 2=cycle or hop limit, 5=mutation guard refused.")

(defconst nelisp-cc-mirror-alias-install-source
  nelisp-cc-mirror-alias-install--source
  "Build-source facade for the private alias installer.")

(provide 'nelisp-cc-mirror-alias-install)

;;; nelisp-cc-mirror-alias-install.el ends here
