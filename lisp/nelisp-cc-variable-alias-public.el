;;; nelisp-cc-variable-alias-public.el --- Runtime variable redirects -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Symbol-entry access qualifies under minimal-native clause 1. Public
;; policy and docstrings remain Lisp definitions. prim-globals owns watchers.

(defconst nelisp-cc-variable-alias-public--source
  '(seq
    (defun nl_pa_terminal (env entry name forbidden)
      ;; Brent's cycle detector: one mirror lookup per edge, no transport
      ;; allocation and no finite chain-length limit. FORBIDDEN is the name
      ;; about to acquire an edge during installation (0 on ordinary reads).
      (let ((current name) (checkpoint name) (power 1) (distance 0)
            (done 0) (result 0))
        (while (= done 0)
          (if (and (/= forbidden 0) (= (nelisp_symbol_key_equal current forbidden) 1))
              (seq (nelisp_env_makunbound_alias_signal_cycle name 0)
                   (setq done 1))
            (if (= entry 0)
                (seq (setq result current) (setq done 1))
              (if (or (/= (nl_val_tag entry) 12)
                      (/= (nl_sp_eq_lit (nl_record_type_word entry) 12
                                       7290602597431212403 2037544046) 1))
                  (seq (nelisp_env_makunbound_alias_signal_malformed name 0) (setq done 1))
              (if (<= (record-slot-count entry) 4)
                  (seq (setq result current) (setq done 1))
              (let ((target (nl_record_slot_word entry 4)))
                (if (= (nl_val_tag target) 0)
                    (seq (setq result current) (setq done 1))
                  (if (and (/= (nl_val_tag target) 4) (/= (nl_val_tag target) 16))
                      (seq (nelisp_env_makunbound_alias_signal_malformed name 0)
                           (setq done 1))
                    (setq current target)
                    (setq distance (+ distance 1))
                    (if (= (nelisp_symbol_key_equal current checkpoint) 1)
                        (seq (nelisp_env_makunbound_alias_signal_cycle name 0)
                             (setq done 1))
                      (seq
                       (if (= distance power)
                           (seq (setq checkpoint current) (setq power (* power 2))
                                (setq distance 0)) 0)
                       (setq entry (nelisp_mirror_lookup_entry env current))))))))))))
        result))
    (defun nl_pa_canonical (env name)
      (nl_pa_terminal env (nelisp_mirror_lookup_entry env name) name 0))
    (defun nl_pa_public_name (name _pad)
      (if (and (= (nl_val_tag name) 4) (= (sexp-name-eq name "nil") 1)) 3
        (if (and (= (nl_val_tag name) 4) (= (sexp-name-eq name "t") 1)) 7 name)))
    (defun nl_pa_alias_read_w (mirror frames entry validated)
      ;; The usual single redirect needs one additional hash, not a full
      ;; canonicalization followed by two more terminal lookups. 6 requests
      ;; the general chain/local resolver; it is not a valid Lisp WORD.
      ;; Use the same mirror entry-identity trust as the plain path; keep
      ;; full record/type-name authentication in the general resolver.
      ;; VALIDATED is set only by the shared slot-4 checkpoint. Both read
      ;; paths retain full validation when that checkpoint rejects ENTRY.
      (if (or (= validated 1)
              (and (= (nl_val_tag entry) 12) (> (record-slot-count entry) 4)))
          (let ((target (nl_record_slot_word entry 4)))
            (if (or (= (nl_val_tag target) 4) (= (nl_val_tag target) 16))
                (let ((terminal (nelisp_mirror_lookup_entry mirror target)))
                  (if (if (= terminal 0) 1
                        ;; The shared predicate already rejects non-records.
                        (= (nl_sb_plain_entry_p terminal) 1))
                      ;; Alias bases are declared special. Their dynamic
                      ;; bindings are shallow outside workers; pre-existing
                      ;; lexical bindings of the base do not bind the alias.
                      (let ((cell (if (= (nl_sb_worker_p mirror) 0) 0
                                    (if (= frames 0) 0
                                      (nelisp_frame_stack_find_kind frames target 1 0)))))
                        (if (= cell 0)
                            (if (= terminal 0) 2 (nl_record_slot_word terminal 0))
                          (nl_cell_get_value_w cell 0)))
                    6))
              6))
        6))
    (defun nl_pa_binding_name (env frames name)
      (let ((entry (nelisp_mirror_lookup_entry env name)))
        (if (= entry 0) name
          (let* ((address (alloc-bytes 8 8))
                 (status (nelisp_env_variable_canonicalize env frames entry name address 0))
                 (canonical (if (or (= status 0) (= status 3))
                                (ptr-read-u64 address 0) 0)))
            (seq (dealloc-bytes address 8 8)
                 (if (= canonical 0) 0
                   (if (= (nelisp_mirror_is_constant env canonical) 1)
                       (seq (bf_setting_constant (nl_pa_public_name canonical 0)) 0) canonical)))))))
    (defun nl_pa_source_kind (env name)
      ;; 1=let-bound source, 2=buffer-local source, 0=ordinary variable.
      (let* ((frames (+ env 32)) (entry (nelisp_mirror_lookup_entry env name))
             (depth (nl_word_payload (nl_record_slot_word frames 1))) (kind 0))
        (if (and (/= entry 0) (> (record-slot-count entry) 5)
                 (= (nelisp_env_entry_slot_tag entry 5) 8))
            (setq kind 2)
          (if (/= (nelisp_frame_stack_find_kind frames name 1 0) 0) (setq kind 1)
            (while (and (> depth 0) (= kind 0))
              (let* ((frame (nl_vector_slot_word (nl_record_slot_word frames 0) (- depth 1)))
                     (names (nl_record_slot_word frame 1)))
                (while (and (= (nl_val_tag names) 7) (= kind 0))
                  (let ((item (nl_cons_car_word names)))
                    (if (and (= (nl_val_tag item) 8)
                             (= (nelisp_symbol_key_equal (nl_vector_slot_word item 0) name) 1))
                        (setq kind 1) 0))
                  (setq names (nl_cons_cdr_word names))))
              (setq depth (- depth 1)))))
        (if (and (= kind 0) (/= (nl_ps_outer_saved env name) 0))
            (setq kind 1) 0)
        ;; GNU considers the target let-bound while its pre-write watcher
        ;; runs. Each raw link points to a rooted canonical symbol view.
        (let ((pending (ptr-read-u64 (nl_n4_state env) 24)))
          (while (and (= kind 0) (/= pending 0))
            (if (= (nelisp_symbol_key_equal (ptr-read-u64 pending 8) name) 1)
                (setq kind 1) 0)
            (setq pending (ptr-read-u64 pending 0))))
        (+ (* kind 4) 1)))
    (defun nl_pa_entry (env name)
      (let ((entry (nelisp_mirror_lookup_entry env name)))
        (if (/= entry 0) entry
          (let ((scratch (alloc-bytes 32 8)))
            (seq (nl_env_build_scratch (+ env 64) (+ env 64) scratch)
                 (nelisp_mirror_set_value_or_insert env name scratch 0)
                 (nelisp_mirror_lookup_entry env name))))))
    (defun nl_pa_constant_name (value)
      ;; A private Symbol key for the constant's mirror entry. The ordinary
      ;; symbol allocator canonicalizes these names to nil/t, which are
      ;; immediate WORDs and cannot be used as symbol-entry addresses.
      (let ((bytes (alloc-bytes 8 1)) (symbol (alloc-bytes 32 8)))
        (seq (ptr-write-u64 bytes 0 (if (= (nl_val_tag value) 0) 7104878 116))
             (nl_alloc_symbol_plain bytes (if (= (nl_val_tag value) 0) 3 1) symbol)
             symbol)))
    (defun nl_pa_local_entry (env name)
      ;; Ordinary entries have five slots. Explicit locals need the sixth;
      ;; publish a complete shadow entry rather than reading the type trailer
      ;; as slot 5. Redirects follow names, preserving existing aliases.
      (if (or (= (nl_val_tag name) 0) (= (nl_val_tag name) 1)
              (= (bf_keyword_raw (wf_word_view name)) 1))
          (seq (bf_setting_constant (wf_word_view name)) 0)
      (if (and (/= (nl_val_tag name) 4) (/= (nl_val_tag name) 16))
          (seq (bf_wrong_type_symbolp (wf_word_view name)) 0)
      (if (= (nl_thread_mirror_mutation_guard env name) 1) 0
        (let ((old (nl_pa_entry env name)))
          (if (> (record-slot-count old) 5) old
            (let ((entry (alloc-bytes 32 8)) (scratch (alloc-bytes 32 8)) (i 0))
              (seq
               (record-make (nl_record_type_word old) 6 entry)
               (while (< i (record-slot-count old))
                 (record-slot-set entry i (nl_record_slot_word old i))
                 (setq i (+ i 1)))
               (nl_env_build_scratch (+ env 64) (+ env 64) scratch)
               (nelisp_mirror_bucket_prepend env name entry scratch)
               entry))))))))
    (defun nl_n3_defvaralias_raw_w (alias base env _pad)
      ;; Callbacks can replace either cell and collect. Root all borrowed
      ;; operands, including private nil/t keys and the migrating old value.
      (let* ((mark (nl_root_mark env))
             (roots (wf_bytecode_w_roots env 5)) (result 0))
        (seq
         (if (= roots 0) (wf_bytecode_root_exhausted)
           (seq (wf_word_store_out roots alias)
                (wf_word_store_out (+ roots 32) base)
                (setq result (nl_pa_install_w roots (+ roots 32) env roots))))
         (nl_root_release env mark)
         result)))
    (defun nl_pa_install_w (alias base env roots)
      (if (and (/= (nl_val_tag alias) 4) (/= (nl_val_tag alias) 16))
          (seq (bf_wrong_type_symbolp alias) 0)
        (if (= (nl_thread_mirror_mutation_guard env alias) 1) 0
          (let* ((base-name
                  (seq (wf_word_store_out (+ roots 64)
                         (if (or (= (nl_val_tag base) 0) (= (nl_val_tag base) 1))
                             (nl_pa_constant_name base) base))
                       (+ roots 64)))
                 (terminal (nl_pa_terminal env (nelisp_mirror_lookup_entry env base-name)
                                           base-name alias)))
            (if (= terminal 0) 0
              (let* ((alias-entry (nl_pa_entry env alias))
                     (base-entry (nl_pa_entry env terminal))
                     (saved (bf_dynamic_lookup_w env alias 0))
                     (migration 0))
                (seq
                 (wf_word_store_out (+ roots 96) terminal)
                 (setq terminal (+ roots 96))
                 (if (and (/= saved 0) (/= saved 2))
                     (seq (wf_word_store_out (+ roots 128) saved)
                          (setq saved (+ roots 128))) 0)
                 (if (or (= (nl_val_tag base) 0) (= (nl_val_tag base) 1)
                         (= (bf_keyword_raw base-name) 1))
                     (seq (record-slot-set base-entry 0 base)
                          (record-slot-set base-entry 3 7)) 0)
                 (if (and (/= saved 0) (/= saved 2)
                          (= (bf_dynamic_lookup_w env terminal 0) 2))
                     (seq
                      ;; GNU calls this initialization a let notification.
                      (if (/= (logand (ptr-read-u64 env 112) 1048576) 0)
                          ;; GNU calls this `let', but it installs a value,
                          ;; not a dynamic binding. Its watcher may alias BASE.
                          (setq migration (nl_n3_notify env terminal saved 7)) 0)
                      (if (= migration 0)
                          (setq migration (nl_n3_set_dynamic_raw env terminal saved)) 0)) 0)
                 (if (and (= migration 0)
                          (/= (logand (ptr-read-u64 env 112) 1048576) 0))
                     (setq migration (nl_n3_notify env alias base 5)) 0)
                 (if (/= migration 0) 0
                   ;; A watcher can reenter, resize the source entry or alter
                   ;; the proposed chain. Never publish through a cached record.
                   (if (= (nl_pa_terminal env (nelisp_mirror_lookup_entry env base-name)
                                           base-name alias) 0) 0
                   (seq (setq alias-entry (nl_pa_entry env alias))
                        (wf_dirty)
                        (record-slot-set alias-entry 4 base-name)
                        (if (> (record-slot-count alias-entry) 5)
                            (record-slot-set alias-entry 5 3) 0)
                        (bf_declare_special env alias)
                        (bf_declare_special env base-name)
                        ;; The result outlives the wrapper's temporary roots.
                        (if (= (nl_val_tag base) 0) 3
                          (if (= (nl_val_tag base) 1) 7
                            (nl_word_clone base)))))))))))))
    (defun nl_pa_alias_pairs_w (env _pad)
      ;; Read native routing, including uninterned names. Shadowed entries
      ;; from a five-to-six-slot upgrade must not appear twice.
      (let* ((buckets (nl_record_slot_word (nl_record_slot_word env 0) 1))
             (i 0) (result 3))
        (while (< i (vector-len buckets))
          (let ((chain (nl_vector_slot_word buckets i)))
            (while (= (nl_val_tag chain) 7)
              (let* ((pair (nl_cons_car_word chain)) (key (nl_cons_car_word pair))
                     (entry (nl_cons_cdr_word pair)))
                (if (and (> (record-slot-count entry) 4)
                         (/= (nl_val_tag (nl_record_slot_word entry 4)) 0))
                    (let ((name (if (= (nl_val_tag key) 16) key
                                  (bf_intern_w key 0))))
                      (if (= entry (nelisp_mirror_lookup_entry env name))
                          (setq result
                                (nelisp_cons_construct_w
                                 (nelisp_cons_construct_w name
                                  (nl_pa_public_name (nl_record_slot_word entry 4) 0) 0)
                                 result 0)) 0)) 0))
              (setq chain (nl_cons_cdr_word chain))))
          (setq i (+ i 1)))
        result))
    (defun nl_pa_default_w (op name value env)
      (let ((canonical (nl_pa_canonical env name)))
        (if (= canonical 0) 0
          (let* ((entry (nelisp_mirror_lookup_entry env canonical))
                 (cell (nelisp_frame_stack_find_kind (+ env 32) canonical 1 0))
                 (old (if (/= cell 0) (nl_cell_get_value_w cell 0)
                        (if (= entry 0) 2 (nl_record_slot_word entry 0))))
                 (void (if (= old 2) 1 (wf_bytecode_w_unbound_p env old))))
            (if (= (sexp-name-eq op "variable-default-boundp") 1)
                (if (= void 1) 3 7)
              (if (= (sexp-name-eq op "variable-set-default") 1)
                  (if (= (nelisp_mirror_is_constant env canonical) 1)
                      (seq (bf_setting_constant name) 0)
                    (if (= (nl_thread_mirror_mutation_guard env canonical) 1) 0
                      (if (/= (logand (ptr-read-u64 env 112) 1048576) 0)
                          ;; Preserve the source across callbacks: GNU set
                          ;; follows a watcher-retargeted alias afterwards.
                          (nl_pa_watched_default_w env name value 0)
                        (seq (wf_dirty)
                             (if (/= cell 0) (cell-set-value cell value)
                               (nelisp_env_setv_mirror_lazy env canonical value (+ env 64)))
                             value))))
                (if (= void 1)
                    (seq (nl_stash_void_variable env name) 0) old)))))))
    (defun nl_pa_watched_default_w (env name value _pad)
      (let* ((mark (nl_root_mark env)) (roots (wf_bytecode_w_roots env 2))
             (result (alloc-bytes 8 8)))
        (seq
         (ptr-write-u64 result 0 0)
         (if (= roots 0) (wf_bytecode_root_exhausted)
           (seq
            (wf_word_store_out roots name)
            (wf_word_store_out (+ roots 32) value)
            (if (= (nl_n3_notify env roots (+ roots 32) 6) 0)
                (let* ((canonical (nl_pa_canonical env roots))
                       (cell (if (= canonical 0) 0
                               (nelisp_frame_stack_find_kind (+ env 32) canonical 1 0))))
                  (if (= canonical 0) 0
                    (if (= (nelisp_mirror_is_constant env canonical) 1)
                        (bf_setting_constant roots)
                      (if (= (nl_thread_mirror_mutation_guard env canonical) 1) 0
                        (seq (wf_dirty)
                             (if (= (if (/= cell 0)
                                        (seq (cell-set-value cell (+ roots 32)) 0)
                                      (nelisp_env_setv_mirror_lazy env canonical (+ roots 32) (+ env 64))) 0)
                                 (nl_val_clone_into (+ roots 32) result) 0)))))) 0)))
         (nl_root_release env mark)
         (ptr-read-u64 result 0))))
    (defun nl_pa_operation_w (args env _pad)
      (let* ((op (nl_apply_list_nth args 0)) (name (nl_apply_list_nth args 1)))
        (if (= (sexp-name-eq op "variable-aliases") 1) (nl_pa_alias_pairs_w env 0)
        (if (or (= (sexp-name-eq op "variable-default-value") 1)
                (= (sexp-name-eq op "variable-default-boundp") 1)
                (= (sexp-name-eq op "variable-set-default") 1))
            (nl_pa_default_w op name (nl_apply_list_nth args 2) env)
        (if (= (sexp-name-eq op "variable-entry") 1)
            (let ((canonical (nl_pa_canonical env name)))
              (if (= canonical 0) 0
                (if (= (nl_val_tag (nl_apply_list_nth args 2)) 0)
                    (let ((entry (nelisp_mirror_lookup_entry env canonical)))
                      (if (= entry 0) 3 entry))
                  (nl_pa_local_entry env canonical))))
        (if (= (sexp-name-eq op "variable-alias-source-kind") 1)
            (nl_pa_source_kind env name)
        (if (= (sexp-name-eq op "variable-alias-set") 1)
            (nl_sf_defvaralias_apply_w name (nl_apply_list_nth args 2) env 0)
          (if (= (sexp-name-eq op "variable-alias-delete") 1)
              (let* ((entry (nelisp_mirror_lookup_entry env name))
                     (was-alias (if (= entry 0) 0
                                  (= (nelisp_env_alias_slot_candidate entry 0) 0))))
                (if (= (nl_thread_mirror_mutation_guard env name) 1) 0
                  (seq (if (/= entry 0)
                           (seq (wf_dirty) (record-slot-set entry 4 3)
                                (record-slot-set entry 0 (+ env 64))
                                (if (= was-alias 1) (bf_fx_special_bump env) 0)) 0)
                       name)))
            (let ((canonical (if (or (= (nl_val_tag name) 4) (= (nl_val_tag name) 16))
                                 (nl_pa_canonical env name) name)))
              (if (= canonical 0) 0
                (if (= (sexp-name-eq op "constant-variable-p") 1)
                    (if (= (nelisp_mirror_is_constant env canonical) 1) 7 3)
                  (nl_pa_public_name canonical 0)))))))))))))
  "WORD-only variable alias operations; no additional public native entry.")

(provide 'nelisp-cc-variable-alias-public)
;;; nelisp-cc-variable-alias-public.el ends here
