;;; nelisp-runtime-reload-abi.el --- Native development ABI -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Shared by the opt-in native builder and its in-process loader.
;; Table order is an ABI: rebuild the development binary after changing it.
;;; Code:

(require 'cl-lib)

(defconst nelisp-runtime-reload-gc-contract
  (quote (("nl_gc_chunk_end" . 1) ("nl_gc_in_arena" . 1) ("nl_gc_is_boot" . 1) ("nl_gc_mark_block" . 1) ("nl_gc_mark_slot" . 1) ("nl_gc_free_block" . 1) ("nl_gc_bt_ok" . 3) ("nl_gc_debt_refresh_limit" . 0) ("nl_gc_boundary_due" . 0) ("nl_mxcache_lookup" . 1) ("nl_mxcache_store" . 2) ("nl_mxcache_evict" . 1) ("nl_fvcache_disabled" . 0) ("nl_fvcache_lookup" . 1) ("nl_fvcache_store" . 2) ("nl_gc_mark_recorded_env" . 1) ("nl_gc_pool_cap_of" . 1) ("nl_gc_mark_recorded_contexts" . 0) ("nl_gc_mark_symentry" . 0) ("nl_gc_collect_recorded_mark_sweep_body" . 1) ("nl_gc_collect_from_recorded_roots" . 1) ("nl_gc_midform_collect" . 0) ("nl_gc_collect" . 7) ("nl_gc_collect_form_boundary" . 7))))

(defconst nelisp-runtime-reload-symbols
  (quote ("nl_runtime_reload_state" "nl_runtime_reload_install" "nl_runtime_reload_alloc_original" "nl_runtime_reload_gc_original" "nl_alloc_check" "nl_alloc_diag" "nl_aref_cache_clear" "nl_arena_base" "nl_bind_clone_force" "nl_frame_push_sym0" "nl_frame_push_sym1" "nl_freelist_large_bins" "nl_freelist_small_mask" "nl_fvcache_disable_lookup" "nl_fvcache_table_base" "nl_gc_diag" "nl_gc_loop_ctx" "nl_gc_mark_rootstack" "nl_gc_mark_thread_roots" "nl_gc_reclaim_scratch" "nl_gc_start_index" "nl_gc_stats" "nl_mxcache_disable_lookup" "nl_mxcache_epoch" "nl_mxcache_table_base" "nl_record_slot_ptr" "nl_thread_parallel_ctx" "nl_thread_park_collect_succeeded" "nl_thread_park_request_begin" "nl_thread_park_request_end" "nl_gc_conserv_state" "nl_freelist_large_mask")))

(defvar nelisp-runtime-reload--digest-cache nil
  "One bounded, private ABI snapshot and its digest.")
(defvar nelisp-runtime-reload--snapshot-plain-p t
  "Dynamically scoped cache eligibility for the current snapshot.")

(defun nelisp-runtime-reload--snapshot ()
  "Copy bounded ABI data, refusing cycles before printing or hashing."
  (let ((nodes 0) (bytes 0))
    (catch 'invalid
      (cl-labels
          ((walk (value depth active)
             (setq nodes (1+ nodes))
             (when (or (> nodes 512) (> depth 16)) (throw 'invalid :uncacheable))
             (cond
              ((consp value)
               (when (memq value active) (throw 'invalid :cycle))
               (let ((parents (cons value active)))
                 (cons (walk (car value) (1+ depth) parents)
                       (walk (cdr value) depth parents))))
              ((stringp value)
               (setq bytes (+ bytes (length value)))
               (when (> bytes 8192) (throw 'invalid :uncacheable))
               (unless (and (fboundp 'text-properties-at)
                            (fboundp 'next-property-change)
                            (or (= (length value) 0)
                                (and (null (text-properties-at 0 value))
                                     (let ((next (next-property-change 0 value)))
                                       (or (null next) (= next (length value)))))))
                 (setq nelisp-runtime-reload--snapshot-plain-p nil))
               (copy-sequence value))
              ((null value) nil)
              ((integerp value)
               ;; ABI arities are small integers; preserve slow serialization
               ;; of other integer inputs without admitting them to the memo.
               (unless (and (>= value -65536) (<= value 65536))
                 (setq nelisp-runtime-reload--snapshot-plain-p nil))
               value)
              ((symbolp value)
               (setq nelisp-runtime-reload--snapshot-plain-p nil)
               value)
              (t (throw 'invalid :uncacheable)))))
        (walk (append (when (and (fboundp 'nelisp-native-load--windows-p)
                                 (nelisp-native-load--windows-p))
                        '("win64-v1"))
                      (list nelisp-runtime-reload-gc-contract nelisp-runtime-reload-symbols)) 0 nil)))))

(defun nelisp-runtime-reload--digest-context ()
  "Return identities of every helper used to snapshot and hash ABI data."
  (let ((loader-context
         (when (fboundp 'nelisp-native-load-sha256-dependency-context)
           (nelisp-native-load-sha256-dependency-context))))
   (when (or (not (fboundp 'nelisp-native-load-sha256-dependency-context))
             (and (vectorp loader-context) (<= (length loader-context) 48)))
    (vconcat
   (mapcar (lambda (symbol)
            (and (fboundp symbol) (symbol-function symbol)))
          '(nelisp-runtime-reload--snapshot
            nelisp-runtime-reload--digest-context
            nelisp-runtime-reload--same-context-p
            nelisp-runtime-reload--canonical-printer-p
            nelisp-runtime-reload-contract-hash
            nelisp-runtime-reload--contract-hash-slow
            nelisp-native-load-sha256
            nelisp-native-load-sha256-dependency-context secure-hash
            prin1-to-string equal copy-sequence make-hash-table gethash puthash
            stringp consp car cdr cons length integerp symbolp null fboundp
            symbol-function mapcar vconcat vector vectorp aref memq + > =
            < <= >= 1+ nth error boundp symbol-value
            text-properties-at next-property-change))
     loader-context))))

(defun nelisp-runtime-reload--same-context-p (left right)
  "Compare helper identities without traversing function bodies."
  (when (and (vectorp left) (vectorp right)
             (<= (length left) 96) (= (length left) (length right)))
    (let ((ok t) (index 0) (count (length left)))
      (while (and ok (< index count))
        (unless (eq (aref left index) (aref right index)) (setq ok nil))
        (setq index (1+ index)))
      ok)))

(defun nelisp-runtime-reload--canonical-printer-p ()
  "Permit memoization only under the ordinary ABI printer settings.
The slow hash locally binds print-length and print-level.  Other printer
settings retain their original dynamic semantics, including shared graphs."
  (let ((settings '((print-circle . nil) (print-continuous-numbering . nil)
                    (print-number-table . nil) (print-gensym . nil)
                    (print-escape-newlines . nil)
                    (print-escape-control-characters . nil)
                    (print-escape-nonascii . nil) (print-escape-multibyte . nil)
                    (print-integers-as-characters . nil) (print-quoted . t)
                    (print-symbols-bare . nil) (print-unreadable-function . nil)
                    (print-charset-text-property . default)))
        (ok t))
    (while (and ok settings)
      (let ((setting (car settings)))
        (when (and (boundp (car setting))
                   (not (eq (symbol-value (car setting)) (cdr setting))))
          (setq ok nil)))
      (setq settings (cdr settings)))
    ok))

(defun nelisp-runtime-reload-contract-hash ()
  "Hash bounded ABI data, reusing a digest only for an unchanged snapshot."
  (let* ((nelisp-runtime-reload--snapshot-plain-p t)
         (snapshot (nelisp-runtime-reload--snapshot))
        (context (nelisp-runtime-reload--digest-context)))
    (when (eq snapshot :cycle)
      (setq nelisp-runtime-reload--digest-cache nil)
      (error "nelisp-runtime-reload: cyclic ABI contract"))
    (if (eq snapshot :uncacheable)
        (progn (setq nelisp-runtime-reload--digest-cache nil)
               (nelisp-runtime-reload--contract-hash-slow))
     (if (and nelisp-runtime-reload--snapshot-plain-p
             (nelisp-runtime-reload--canonical-printer-p)
             (or (not (fboundp 'nelisp-native-load-sha256))
                 (fboundp 'nelisp-native-load-sha256-dependency-context))
             nelisp-runtime-reload--digest-cache
             (equal snapshot (car nelisp-runtime-reload--digest-cache))
             (nelisp-runtime-reload--same-context-p
              context (nth 1 nelisp-runtime-reload--digest-cache)))
        (copy-sequence (nth 2 nelisp-runtime-reload--digest-cache))
      (let ((digest (nelisp-runtime-reload--contract-hash-slow)))
        (setq nelisp-runtime-reload--digest-cache nil)
        (when (and nelisp-runtime-reload--snapshot-plain-p
                   (nelisp-runtime-reload--canonical-printer-p)
                   (or (not (fboundp 'nelisp-native-load-sha256))
                       (fboundp 'nelisp-native-load-sha256-dependency-context))
                   (stringp digest) (= (length digest) 64)
                   (equal snapshot (nelisp-runtime-reload--snapshot))
                   nelisp-runtime-reload--snapshot-plain-p
                   (nelisp-runtime-reload--same-context-p
                    context (nelisp-runtime-reload--digest-context)))
          (setq nelisp-runtime-reload--digest-cache
                (list snapshot context (copy-sequence digest))))
        digest)))))

(defun nelisp-runtime-reload--contract-hash-slow ()
  "Return the contract digest embedded in a development executable.
Changing public entries, resolver order, or the layout version requires a
new process.  Editing private function bodies does not change this digest."
  (let* ((print-length nil) (print-level nil)
         (bytes (prin1-to-string
                 (append (when (and (fboundp 'nelisp-native-load--windows-p)
                                    (nelisp-native-load--windows-p)) '("win64-v1"))
                  (list 'nelisp-runtime-contract-v2
                       '(:sexp-bytes 32 :block-header-bytes 8
                         :reload-state-bytes 96 :gc-table-header-bytes 16
                         :gc-conservative-state-bytes 64)
                       nelisp-runtime-reload-gc-contract
                       nelisp-runtime-reload-symbols)))))
    (if (fboundp 'nelisp-native-load-sha256)
        (nelisp-native-load-sha256 bytes)
      (secure-hash 'sha256 bytes))))

(defun nelisp-runtime-reload-contract-matches-p ()
  "Return non-nil when this source contract matches the running binary."
  (when (fboundp 'nelisp--native-runtime-contract-word)
    (let ((digest (nelisp-runtime-reload-contract-hash)) (index 0) (ok t))
      (while (< index 8)
        (unless (= (nelisp--native-runtime-contract-word index)
                   (string-to-number (substring digest (* index 8)
                                                (* (1+ index) 8)) 16))
          (setq ok nil))
        (setq index (1+ index)))
      ok)))

(provide (quote nelisp-runtime-reload-abi))
;;; nelisp-runtime-reload-abi.el ends here
