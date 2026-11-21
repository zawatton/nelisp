;;; nelisp-alloc-fastpath-test.el --- structural pins for the allocation fast path  -*- lexical-binding: t; -*-

;;; Commentary:

;; Pins the shape of the large-request allocation lowering: checked size ->
;; reserve (exact-fit pop or one CAS bump with a single limit check) ->
;; initialize -> publish, with the slow path as the fallback and GC debt
;; billed exactly once on success.  Override the build script with
;; NELISP_ALLOC_BUILD to run a negative control on a pristine copy.

;;; Code:

(require 'ert)

(defconst nelisp-alloc-fastpath-test--build
  (or (getenv "NELISP_ALLOC_BUILD")
      (expand-file-name "../scripts/nelisp-standalone-build.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
  "Build script under test, captured at load time.")

(defun nelisp-alloc-fastpath-test--defun (name)
  "Return the text of the first DSL defun NAME in the build script."
  (with-temp-buffer
    (insert-file-contents nelisp-alloc-fastpath-test--build)
    (let* ((text (buffer-string))
           (start (string-match (format "^ +(defun %s " (regexp-quote name)) text))
           (end (and start (string-match "^ +(defun " text (1+ start)))))
      (should start)
      (substring text start end))))

(defun nelisp-alloc-fastpath-test--before (a b text)
  "Non-nil when literal A occurs in TEXT before literal B."
  (let ((pa (string-match (regexp-quote a) text)) (pb (string-match (regexp-quote b) text)))
    (and pa pb (< pa pb))))

(ert-deftest nelisp-alloc-fastpath-large-fast-shape ()
  "Reserve is guarded by the gate, has one limit check, and falls back."
  (let ((b (nelisp-alloc-fastpath-test--defun "nl_alloc_large_fast")))
    ;; gate: checker and diagnostics off, reuse enabled
    (should (string-match-p "(data-addr nl_alloc_check) 0) 0)" b))
    (should (string-match-p "(data-addr nl_alloc_diag) 0) 0)" b))
    ;; bump reserve goes through the CAS chunk reservation (single limit check)
    (should (string-match-p "(nl_chunk_try_alloc chunk want)" b))
    ;; exact-fit validation: arena, mark FREE, BT == want
    (should (string-match-p "nl_alloc_pop_in_arena" b))
    (should (string-match-p "(= (logand hw 7) 2)" b))
    (should (string-match-p "(= (logand hw 4294967288) want)" b))
    ;; bump only when no large bin can serve the request
    (should (string-match-p "(< (nl_freelist_large_first want) 0)" b))
    ;; fallback keeps every existing check
    (should (string-match-p "(nl_alloc_bytes_slow size align)" b))
    ;; the size class bound keeps huge requests on the slow path
    (should (string-match-p "(> want 32768)" b))))

(ert-deftest nelisp-alloc-fastpath-debt-billed-once-after-reserve ()
  "GC debt is billed only on the success branch, after the reserve."
  (let ((b (nelisp-alloc-fastpath-test--defun "nl_alloc_large_fast")))
    (should (= 1 (1- (length (split-string b (regexp-quote "(ptr-write-u64 (data-addr nl_gc_until)") nil)))))
    (should (nelisp-alloc-fastpath-test--before
             "(nl_chunk_try_alloc chunk want)" "(data-addr nl_gc_until)" b))
    (should (nelisp-alloc-fastpath-test--before
             "(nl_alloc_zero_fill cur 0 (- want 8))" "(data-addr nl_gc_until)" b))
    ;; the fallback is taken before debt is billed (slow path bills itself)
    (should (nelisp-alloc-fastpath-test--before
             "(= got 0) (nl_alloc_bytes_slow" "(data-addr nl_gc_until)" b))))

(ert-deftest nelisp-alloc-fastpath-reused-block-zeroed-and-header-live ()
  "An exact-fit block is unlinked, marked live, and zeroed before return."
  (let ((b (nelisp-alloc-fastpath-test--defun "nl_alloc_large_fast")))
    (should (nelisp-alloc-fastpath-test--before
             "(ptr-write-u64 head 0 (heap-read-u64 cur 0))"
             "(ptr-write-u64 (- cur 8) 0 want)" b))
    (should (nelisp-alloc-fastpath-test--before
             "(ptr-write-u64 (- cur 8) 0 want)"
             "(nl_alloc_zero_fill cur 0 (- want 8))" b))
    (should (string-match-p "nl_freelist_large_mask_sync bin head" b))))

(ert-deftest nelisp-alloc-fastpath-dispatch-from-nl-alloc-bytes ()
  "`nl_alloc_bytes' routes large requests to the fast function."
  (let ((b (nelisp-alloc-fastpath-test--defun "nl_alloc_bytes")))
    (should (string-search "(> want 472) (nl_alloc_large_fast size align want)" b))))

(ert-deftest nelisp-alloc-fastpath-make-vector-size-check-before-reserve ()
  "Both make-vector entries check the size before reserving, fill word-wise."
  (dolist (spec '(("bf_make_vector_w" . "(if (> n ") ("nl_eic_prim_make_vector" . "(< n 1048576)")))
    (let ((b (nelisp-alloc-fastpath-test--defun (car spec))))
      (should (nelisp-alloc-fastpath-test--before (cdr spec) "(alloc-bytes (+ 8" b))
      ;; header written before the slots, slots two per iteration
      (should (nelisp-alloc-fastpath-test--before
               "(ptr-write-u64 box 0 (+ 8 (* n 256)))" "(ptr-write-u64 box (+ 16 (* k 8))" b))
      (should (string-search "(setq k (+ k 2))" b))
      (should (string-search "(lim (- n 1))" b)))))

(ert-deftest nelisp-alloc-fastpath-oversize-error-before-reserve ()
  "GNU's Memory exhausted signal precedes the vector reservation."
  (let ((b (nelisp-alloc-fastpath-test--defun "bf_make_vector_w")))
    (should (nelisp-alloc-fastpath-test--before
             "bf_signal_alloc_exhausted" "alloc-bytes" b))))

;; Lane C1: bump shortcut for small requests when every free structure is empty.
(ert-deftest nelisp-alloc-fastpath-bump-try-reads-before-reserve ()
  "`nl_alloc_bump_try' consults every free structure, reserves, then bills once."
  (let ((b (nelisp-alloc-fastpath-test--defun "nl_alloc_bump_try")))
    ;; exact-fit bit, no-collection/run, buckets above, large bin 0 and masks
    (should (string-search "(shl 1 (/ (- want 16) 8))" b))
    (should (string-search "268435632" b))
    (should (string-search "(if (= rbt want) 0 1)" b))
    (should (string-search "(sar mask (/ want 8))" b))
    (should (string-search "(data-addr nl_freelist_large_bins)" b))
    (dolist (off '(0 8 16 24 32 40 48))
      (should (string-search (format "(data-addr nl_freelist_large_mask) %d)" off) b)))
    ;; reserve by the CAS bump, debt billed once, after the reserve
    (should (string-search "(nl_chunk_try_alloc chunk want)" b))
    (should (= 1 (1- (length (split-string b (regexp-quote "(ptr-write-u64 (data-addr nl_gc_until)") nil)))))
    (should (nelisp-alloc-fastpath-test--before
             "(nl_chunk_try_alloc chunk want)" "(data-addr nl_gc_until)" b))
    ;; any failure keeps the full path
    (should (nelisp-alloc-fastpath-test--before
             "(= got 0) (nl_alloc_bytes_slow" "(data-addr nl_gc_until)" b))))

(ert-deftest nelisp-alloc-fastpath-run-falls-to-bump-try ()
  "`nl_alloc_bytes_run' hands a run miss to the bump shortcut, not the slow path."
  (let ((b (nelisp-alloc-fastpath-test--defun "nl_alloc_bytes_run")))
    (should (string-search "(nl_alloc_bump_try size align want r)" b))
    (should-not (string-search "(nl_alloc_bytes_slow size align)" b))))

;; Lane C2: read-only list walks take the cdr as a WORD (no box for the nil terminator).
(ert-deftest nelisp-alloc-fastpath-readonly-walks-use-cdr-word ()
  "Formals scan, proper-list check, memq and assq walks do not box the cdr."
  (dolist (spec '(("nl_clx_formals_contain_symbol" . "(nl_cons_cdr_word node-ptr)")
                  ("bf_proper_list_raw" . "(nl_cons_cdr_word p)")
                  ("wf_assq_walk_w" . "(nl_cons_cdr_word alist_ptr)")))
    (let ((b (nelisp-alloc-fastpath-test--defun (car spec))))
      (should (string-search (cdr spec) b))
      (should-not (string-search "nl_cons_cdr_ptr" b)))))

;; `wf_memq_walk_w' opens a quoted list, so it is not at a line-start defun.
(ert-deftest nelisp-alloc-fastpath-memq-walk-uses-cdr-word ()
  "The memq walk recurses on the cdr WORD."
  (with-temp-buffer
    (insert-file-contents nelisp-alloc-fastpath-test--build)
    (should (search-forward "(wf_memq_walk_w elt_ptr (nl_cons_cdr_word list_ptr) 0)" nil t))
    (goto-char (point-min))
    (should-not (search-forward "(wf_memq_walk_w elt_ptr (nl_cons_cdr_ptr list_ptr) 0)" nil t))))

(provide 'nelisp-alloc-fastpath-test)
;;; nelisp-alloc-fastpath-test.el ends here
