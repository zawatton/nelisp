;;; nelisp-runtime-reload-abi.el --- Native development ABI -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Shared by the opt-in native builder and its in-process loader.
;; Table order is an ABI: rebuild the development binary after changing it.
;;; Code:

(defconst nelisp-runtime-reload-gc-contract
  (quote (("nl_gc_chunk_end" . 1) ("nl_gc_in_arena" . 1) ("nl_gc_is_boot" . 1) ("nl_gc_mark_block" . 1) ("nl_gc_mark_slot" . 1) ("nl_gc_free_block" . 1) ("nl_gc_bt_ok" . 3) ("nl_gc_debt_refresh_limit" . 0) ("nl_gc_boundary_due" . 0) ("nl_mxcache_lookup" . 1) ("nl_mxcache_store" . 2) ("nl_mxcache_evict" . 1) ("nl_fvcache_disabled" . 0) ("nl_fvcache_lookup" . 1) ("nl_fvcache_store" . 2) ("nl_gc_mark_recorded_env" . 1) ("nl_gc_pool_cap_of" . 1) ("nl_gc_mark_recorded_contexts" . 0) ("nl_gc_mark_symentry" . 0) ("nl_gc_collect_recorded_mark_sweep_body" . 1) ("nl_gc_collect_from_recorded_roots" . 1) ("nl_gc_midform_collect" . 0) ("nl_gc_collect" . 7) ("nl_gc_collect_form_boundary" . 7))))

(defconst nelisp-runtime-reload-symbols
  (quote ("nl_runtime_reload_state" "nl_runtime_reload_install" "nl_runtime_reload_alloc_original" "nl_runtime_reload_gc_original" "nl_alloc_check" "nl_alloc_diag" "nl_aref_cache_clear" "nl_arena_base" "nl_bind_clone_force" "nl_frame_push_sym0" "nl_frame_push_sym1" "nl_freelist_large_bins" "nl_freelist_small_mask" "nl_fvcache_disable_lookup" "nl_fvcache_table_base" "nl_gc_diag" "nl_gc_loop_ctx" "nl_gc_mark_rootstack" "nl_gc_mark_thread_roots" "nl_gc_reclaim_scratch" "nl_gc_start_index" "nl_gc_stats" "nl_mxcache_disable_lookup" "nl_mxcache_epoch" "nl_mxcache_table_base" "nl_record_slot_ptr" "nl_thread_parallel_ctx" "nl_thread_park_collect_succeeded" "nl_thread_park_request_begin" "nl_thread_park_request_end" "nl_gc_conserv_state")))

(defun nelisp-runtime-reload-contract-hash ()
  "Return the contract digest embedded in a development executable.
Changing public entries, resolver order, or the layout version requires a
new process.  Editing private function bodies does not change this digest."
  (let* ((print-length nil) (print-level nil)
         (bytes (prin1-to-string
                 (list 'nelisp-runtime-contract-v2
                       '(:sexp-bytes 32 :block-header-bytes 8
                         :reload-state-bytes 96 :gc-table-header-bytes 16
                         :gc-conservative-state-bytes 64)
                       nelisp-runtime-reload-gc-contract
                       nelisp-runtime-reload-symbols))))
    (if (fboundp 'nelisp-native-load--sha256)
        (nelisp-native-load--sha256 bytes)
      (secure-hash 'sha256 bytes))))

(defun nelisp-runtime-reload-contract-matches-p ()
  "Return non-nil when this source contract matches the running binary."
  (when (fboundp 'nelisp--native-runtime-contract-word)
    (let ((digest (nelisp-runtime-reload-contract-hash)) (index 0) (ok t))
      (while (and ok (< index 8))
        (unless (= (nelisp--native-runtime-contract-word index)
                   (string-to-number (substring digest (* index 8)
                                                (* (1+ index) 8)) 16))
          (setq ok nil))
        (setq index (1+ index)))
      ok)))

(provide (quote nelisp-runtime-reload-abi))
;;; nelisp-runtime-reload-abi.el ends here
