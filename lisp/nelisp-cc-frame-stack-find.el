;;; nelisp-cc-frame-stack-find.el --- Doc 111 §111.E #24 frame_stack_find  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #24 — `frame_stack_find_rust_direct'
;; with the §111.E #25 private helpers `frame_lookup_rust_direct' +
;; `frame_lookup_in' folded in.  Doc 115 §115.6 — full pure-elisp
;; implementation of the innermost-first stack walk.  Replaces the
;; ~78 LOC Rust shim `nl_frame_stack_find' (+ private
;; `lookup_in_frame' helper) that previously lived in
;; `build-tool/src/eval/env_lexframe_aot_shims.rs'.
;;
;; Algorithm (= literal transcription of the Rust impl):
;;
;;   1. Read depth via `sexp-int-unwrap' on slots[1] of the
;;      frames-record.
;;   2. Read backing-vector pointer via slots[0].
;;   3. Recursive descend `i' from depth-1 down to 0:
;;        a. Fetch frame = backing[i] via `vector-ref-ptr'.
;;        b. Look up NAME in frame's hash table (= slot 0 of frame).
;;        c. If hit (= non-zero ptr), return the cell pointer.
;;        d. Else recurse with i-1.
;;   4. Return 0 when i < 0 (= no frame matched).
;;
;; Per-frame lookup is structurally identical to
;; `mirror_lookup_entry' (= §111.E #1) — same `(KEY . VALUE)' bucket
;; cons-pair shape — except one extra record hop through the outer
;; `nelisp-lexframe' record to reach the inner `fast-hash-table'.  The
;; hot private path reads the known 8-byte tagged words in those records,
;; vectors, and cons boxes directly.  It returns the value word itself on a
;; hit, which is the live Sexp::Cell box required by the frame lookup ABI.
;; It never materialises an immediate Nil/count view and never allocates or
;; runs GC while borrowing the owner-held Cell pointer.
;;
;; ABI deps satisfied by the complete object (the outer stack walk still
;; uses the generic pointer accessors; the private frame lookup uses the
;; direct word reads below):
;;   §111.B  `record-slot-ref-ptr'  — outer frames-record / backing walk.
;;   §111.C  `vector-ref-ptr'       — outer backing walk.
;;   §101.B  `cons-walk' primitives — `sexp-payload-ptr' + `cons-cdr-raw-from-box'.
;;   §101.C  `str-eq'               — byte-payload equality on bucket KEY.
;;   §100.A  `extern-call'          — `nelisp_fnv1a' (Doc 115 §115.7
;;                                    pure-elisp 32-bit FNV-1a).
;;   §100    `sexp-int-unwrap'      — depth payload + bucket-count payload.
;;   §111.E  `logand'               — `(h & (count - 1))' fast-path mask.

;;; Code:

(defconst nelisp-cc-frame-stack-find--source
  '(seq
    (defun nelisp_frame_stack_find_word_tag_p (word tag)
      ;; WORD is a tagged container slot value.  Immediate words cannot be
      ;; heap objects; only a pointer with the expected Sexp tag is read.
      ;; The caller has obtained WORD from a live frame/table/cons owner, so
      ;; the tag check is enough here.  In particular, do not call
      ;; `nl_gc_in_arena' for every edge: that predicate walks the chunk list
      ;; and would put the old allocator cost back into this hot path.
      (if (= word 0)
          0
        (if (= (logand word 1) 1)
          0
          (if (= (sexp-tag word) tag) 1 0))))

    (defun nelisp_frame_stack_find_word_name_p (word)
      ;; `str-eq' reads a string-shaped Sexp inline.  Reject malformed or
      ;; immediate bucket keys before it can read their payload.
      (if (= word 0)
          0
        (if (= (logand word 1) 1)
          0
          (if (or (= (sexp-tag word) 4) (= (sexp-tag word) 16))
              1
            (if (= (sexp-tag word) 5)
                1
              (if (= (sexp-tag word) 14) 1 0))))))

    ;; A hybrid binding keeps its value in the pair's cdr, as GNU's
    ;; (SYMBOL . VALUE) conses do (eval.c:3350 funcall_lambda: Fcons (Fcons (sym,
    ;; arg), lexenv)); a Cell appears only when something needs one to share:
    ;; a closure's capture, a setq, a dynamic lookup.  PAIR is a binding pair:
    ;; answer its Cell, making one first (stored in the pair, where the frame,
    ;; the closures and the setters find it from then on) when its cdr is a
    ;; plain value.  Allocation only: nothing collects.
    (defun nl_frame_pair_cell (pair _pad)
      (let* ((x (nl_cons_cdr_word pair)))
        (if (= (nl_val_tag x) 11)
            x
          (let* ((cell (+ (nl_alloc_cell_init (alloc-bytes 16 8) x) 6)))
            (seq (ptr-write-u64 (+ (nl_word_payload pair) 8) 0 cell)
                 cell)))))
    ;; RAW 0 answers a binding's Cell (a hybrid binding gets one, see above);
    ;; RAW 1 answers the pair's cdr as it is, the Cell or the plain value, for a
    ;; reader that decodes it with `nl_cell_get_value_w'.
    (defun nelisp_frame_stack_find_walk_bucket_m (cell-ptr name-ptr raw _pad)
      ;; Walk bucket and pair NlConsBox words directly.  No accessor
      ;; materialises a temporary Sexp view, and the owner of the frame/table
      ;; keeps the returned Cell slot live for this borrow-only call.
      (if (= cell-ptr 0)
          0
        (if (= (nelisp_frame_stack_find_word_tag_p cell-ptr 7) 0)
            0
          (let* ((cell-box (heap-sexp-payload cell-ptr)))
            (let* ((pair-word (heap-read-u64 cell-box 0))
                   (next-word (heap-read-u64 cell-box 8)))
              ;; A shared closure environment (`nelisp_frame_try_share_chain')
              ;; also holds bare declarations: skip any non-pair element.
              (if (= (nelisp_frame_stack_find_word_tag_p pair-word 7) 0)
                  (if (= (logand next-word 1) 1)
                      0
                    (nelisp_frame_stack_find_walk_bucket_m next-word name-ptr raw 0))
                (let* ((pair-box (heap-sexp-payload pair-word)))
                  (let* ((key-word (heap-read-u64 pair-box 0))
                         (value-word (heap-read-u64 pair-box 8)))
                    ;; The same WORD is the same symbol, and two different symbol
                    ;; objects are different symbols (an interned symbol has one
                    ;; object): only a legacy symbol view needs the polymorphic
                    ;; compare (GNU compares the symbols with eq, eval.c Fassq).
                    (let* ((hit (if (= key-word name-ptr) 1
                                  (if (= (nelisp_frame_stack_find_word_name_p key-word) 0) -1
                                    (if (if (= (logand key-word 7) 6)
                                            (if (= (logand name-ptr 7) 6)
                                                (if (or (= (sexp-tag key-word) 4) (= (sexp-tag key-word) 16))
                                                    (if (or (= (sexp-tag name-ptr) 4) (= (sexp-tag name-ptr) 16)) 1 0)
                                                  0)
                                              0)
                                          0)
                                        0
                                      (extern-call nelisp_symbol_key_equal key-word name-ptr))))))
                      (if (= hit -1)
                          0
                        (if (= hit 1)
                            (if (= (nelisp_frame_stack_find_word_tag_p value-word 11) 1)
                                value-word
                              (if (= raw 1) value-word (nl_frame_pair_cell pair-word 0)))
                          (if (= (logand next-word 1) 1)
                              0
                            (nelisp_frame_stack_find_walk_bucket_m
                             next-word name-ptr raw 0)))))))))))))
    (defun nelisp_frame_stack_find_walk_bucket (cell-ptr name-ptr)
      (nelisp_frame_stack_find_walk_bucket_m cell-ptr name-ptr 0 0))

    (defun nelisp_frame_stack_find_in_frame_m (frame-ptr name-ptr raw _pad)
      ;; Read one validated lexframe/table through semantic WORD accessors.
      ;; The frame owns the table and bucket chain throughout this
      ;; allocation-free borrowed lookup.
      (if (= (nelisp_frame_stack_find_word_name_p name-ptr) 0)
          0
        (if (= (nelisp_frame_stack_find_word_tag_p frame-ptr 12) 0)
            0
          (if (/= (nl_val_tag (nl_record_type_word frame-ptr)) 4)
              0
            (let* ((ht-word (nl_record_slot_word frame-ptr 0)))
              ;; Small frame: slot 0 is nil or an alist, i.e. one bucket.
              (if (= (nelisp_frame_stack_find_word_tag_p ht-word 12) 0)
                  (if (= (nelisp_frame_stack_find_word_tag_p ht-word 7) 1)
                      (nelisp_frame_stack_find_walk_bucket_m ht-word name-ptr raw 0)
                    0)
                (if (/= (nl_val_tag (nl_record_type_word ht-word)) 4)
                    0
                  (let* ((count-word (nl_record_slot_word ht-word 0))
                         (buckets-word (nl_record_slot_word ht-word 1)))
                    (if (/= (logand count-word 3) 1)
                        0
                      (let* ((bucket-count (sar count-word 2)))
                        (if (<= bucket-count 0)
                            0
                          ;; The mask is sound only for power-of-two tables.
                          (if (/= (logand bucket-count (- bucket-count 1)) 0)
                              0
                            (if (= (nelisp_frame_stack_find_word_tag_p buckets-word 8) 0)
                                0
                              (if (> bucket-count (nl_vector_length buckets-word))
                                  0
                                (let* ((idx (logand (extern-call nelisp_fnv1a name-ptr)
                                                    (- bucket-count 1)))
                                       (head-word (nl_vector_slot_word buckets-word idx)))
                                  (if (= (logand head-word 1) 1)
                                      0
                                    (nelisp_frame_stack_find_walk_bucket_m
                                     head-word name-ptr raw 0)))))))))))))))))
    (defun nelisp_frame_stack_find_in_frame (frame-ptr name-ptr)
      (nelisp_frame_stack_find_in_frame_m frame-ptr name-ptr 0 0))
    (defun nelisp_frame_stack_find_descend (backing-ptr i name-ptr)
      ;; Innermost-first walk: descend from i = depth-1 down to 0.
      ;; Returns the first non-zero hit's cell pointer, or 0 if every
      ;; frame missed (= the i < 0 base case).
      ;;
      ;; backing-ptr: `*const Sexp' = frames-record.slots[0] (= the
      ;;              backing `Sexp::Vector').
      ;; i:           i64 — current index being probed; recursion
      ;;              starts at depth-1.
      ;; name-ptr:    `*const Sexp' for the lookup key.
      ;;
      ;; R11b Wave 9 CSE-hoist: the pre-R11b version re-evaluated
      ;; `nelisp_frame_stack_find_in_frame' on the hit branch (= second
      ;; call site) because the original AOT source elided the
      ;; let-binding.  This paid a redundant FNV-1a hash + bucket walk
      ;; per hit — non-negligible since lookup hit rate dominates.
      ;; We now hoist the first call's result into `found' via let-rt
      ;; and return it directly on the non-zero branch, saving one
      ;; full hash+walk per successful frame lookup.
      (if (< i 0)
          0
        (let* ((found (nelisp_frame_stack_find_in_frame
                       (vector-ref-ptr backing-ptr i)
                       name-ptr)))
          (if (= found 0)
              (nelisp_frame_stack_find_descend backing-ptr (- i 1) name-ptr)
            found))))
    (defun nelisp_frame_stack_find_valid_key (name-ptr)
      ;; Same cold-load hardening as `nelisp_mirror_lookup_entry_valid_key'
      ;; (`lisp/nelisp-cc-mirror-lookup-entry.el') — NAME-PTR must be a
      ;; live Sexp::Symbol / Sexp::Str before any hash/compare touches
      ;; it.  Reproduced crash: once the mirror lookup path is guarded, a
      ;; malformed `nl_env_set_value' NAME-PTR (observed tag 7 / Cons)
      ;; reaches this SEPARATE lexical-frame lookup next and crashes
      ;; `nelisp_fnv1a_step4' the same way.  A sound "not found" here is
      ;; safe: a genuinely malformed key was never a valid lexical
      ;; binding to begin with.
      ;; Doc 226 D9: a symbol object's WORD (class 110, kind 4 or 16) lives
      ;; in a symbol_block cell outside the chunks; its header is the check.
      (if (= (logand name-ptr 7) 6)
          (if (or (= (logand (heap-read-u64 (- name-ptr 6) 0) 63) 4)
                  (= (logand (heap-read-u64 (- name-ptr 6) 0) 63) 16)) 1 0)
      (if (and (= (extern-call nl_gc_in_arena name-ptr) 0)
               (= (extern-call nl_root_pin_slot_active name-ptr) 0)
               (= (extern-call nl_rootstack_slot_active name-ptr) 0))
          0
        (if (or (= (sexp-tag name-ptr) 4) (= (sexp-tag name-ptr) 16))
            1
          (if (or (= (sexp-tag name-ptr) 5)
                  (= (sexp-tag name-ptr) 14)) 1 0)))))
    (defun nelisp_frame_stack_find (frames-ptr name-ptr)
      ;; frames-ptr: *const Sexp pointing at Env::frames_record (=
      ;;             Sexp::Record(`nelisp-lexframe-stack')).
      ;; name-ptr:   *const Sexp pointing at Sexp::Str / Sexp::Symbol.
      ;;
      ;; Returns: i64 — the `*const Sexp' of the matching (NAME . CELL)
      ;; pair's CDR slot (= the cell), or 0 on miss / empty stack /
      ;; malformed NAME-PTR.
      ;; The returned pointer borrows the bucket-pair's slot owned by
      ;; `*frames-ptr'; callers must not outlive that ownership (= same
      ;; contract as `nelisp_mirror_lookup_entry').
      ;;
      ;; Pre-condition (= matches the Rust impl's early-return arms):
      ;;   - frames-ptr.tag = Sexp::Record.
      ;;   - slots[0] = Sexp::Vector(BACKING).
      ;;   - slots[1] = Sexp::Int(DEPTH).
      ;; The `Sexp::Nil' pre-bootstrap case routes through Rust until
      ;; the dispatcher rewires, so no tag-check here.  Empty stack
      ;; (= depth 0) yields i = -1 which hits the `(< i 0)' base case
      ;; immediately and returns 0.
      (if (= (nelisp_frame_stack_find_valid_key name-ptr) 0)
          0
        (nelisp_frame_stack_find_descend
         (nl_record_slot_word frames-ptr 0)
         (- (sar (nelisp_frame_slot_raw_word frames-ptr 1) 2) 1)
         name-ptr)))

    ;; ============================================================
    ;; Doc 49 Wave 10.1d-retry — capture-to-depth AOT native
    ;; ============================================================
    ;;
    ;; `nelisp-lexframe-stack-capture-to-depth' (= R9 95% cumulative
    ;; chain driver, invoked from sf-lambda closure capture) — Phase
    ;; 47 fast path co-located in this .o so the static archive linker
    ;; pulls it in via the same chain that already keeps
    ;; `nelisp_frame_stack_find' alive (= `extern-call' from
    ;; `nelisp-cc-env-lookup-value.el').  No Rust LOC delta required.
    ;;
    ;; Doc 215: capture walks return WORD accumulators. ROOTS is a precise
    ;; vector root holding [accumulator pair filter backing]. The accumulator
    ;; and inner pair are published before the next allocation. Frame/input
    ;; trees retain the traversal cursors; no helper evaluates Lisp.

    ;; ROOTS contains canonical WORDs, not cloned storage views. In particular,
    ;; a nil-initialized object must remain the same header while it is filled.
    (defun nl_capture_root_set_word (roots index word)
      (ptr-write-u64 (+ (nl_word_payload roots) 8) (* index 8) word))

    (defun nl_capture_emit_one_w (name cell acc roots)
      (let* ((pair (nelisp_cons_construct_w name cell 0)))
        (nl_capture_root_set_word roots 1 pair)
        (let* ((word (nelisp_cons_construct_w pair acc 0)))
          (nl_capture_root_set_word roots 0 word)
          word)))

    (defun nl_capture_filter_contains (filter-ptr name-ptr)
      ;; Doc 215: FILTER-PTR may be a view or a WORD; walk by WORD.
      (if (= (nl_val_tag filter-ptr) 7)
          (let* ((sym-ptr (nl_cons_car_word filter-ptr)))
            (if (if (= (logand sym-ptr 1) 0)
                    (= (extern-call nelisp_symbol_key_equal sym-ptr name-ptr) 1)
                  0)
                1
              (nl_capture_filter_contains
               (nl_cons_cdr_word filter-ptr)
               name-ptr)))
        0))

    ;; perf/frame-declaration-check: a record's per-slot storage is one
    ;; 8-byte tagged WORD (Doc 147 Phase 2): low bit 0 = pointer to a
    ;; 32-byte Sexp box, low bit 1 = immediate (Nil = 3, T = 7,
    ;; Int(n) = (n<<2)|1 -- `nl_val_load''s own encoding,
    ;; lisp/nelisp-cc-val-load.el).  This mirrors `nl_record_slot_ptr'
    ;; (lisp/nelisp-cc-nlrecord-slot-ptr.el) up to the point where THAT
    ;; helper would materialise an immediate WORD into a fresh 32-byte
    ;; box for the general `*const Sexp' contract; the two callers below
    ;; only need to classify the WORD, never read a Sexp through it, so
    ;; no allocation is needed at all.  REC-PTR must already be a
    ;; validated `*const Sexp' view of a Record (same precondition its
    ;; callers already establish for `record-slot-count').
    (defun nelisp_frame_slot_raw_word (rec-ptr idx)
      (nl_record_slot_word rec-ptr idx))

    (defun nelisp_frame_binding_dynamic_p (frame-ptr name-ptr)
      ;; Slot 1 is binding-time metadata, independent of later declarations.
      ;; Older native frames have only slot 0 and remain lexical-only.
      ;; f25ec9af1 (2026-09-16) made every frame visited during a lexical
      ;; descent pay this check -- one call per variable reference per
      ;; frame walked -- and slot 1 is Nil (no declared-dynamic names) in
      ;; the overwhelming majority of frames.  `record-slot-ref-ptr' would
      ;; materialise a fresh 32-byte box for that Nil on every single
      ;; call even though an immediate WORD can never be the tag-7 Cons
      ;; this predicate looks for.  Rejecting any immediate WORD directly
      ;; is exactly equivalent: `nl_capture_filter_contains' given a
      ;; materialised non-Cons immediate would also return 0, just after
      ;; paying for the allocation first.
      (if (= (sexp-tag frame-ptr) 12)
          (if (> (record-slot-count frame-ptr) 1)
              (if (= (logand (nelisp_frame_slot_raw_word frame-ptr 1) 1) 1)
                  0
                (nl_capture_filter_contains
                 (record-slot-ref-ptr frame-ptr 1) name-ptr))
            0)
        0))

    (defun nelisp_frame_scope_boundary_p (frame-ptr)
      ;; Older frames have no boundary metadata.  A marked frame belongs
      ;; to the callee, but lexical lookup must not descend below it.
      ;; Same fix as `nelisp_frame_binding_dynamic_p' above: slot 2 is
      ;; always an immediate here (Nil when unset, Int(1) once
      ;; `nelisp_frame_scope_mark' has set it), so a materialising
      ;; `record-slot-ref-ptr' call is pure overhead on every frame a
      ;; lexical descent visits, whether or not it finds a match.  Nil's
      ;; raw WORD is the literal 3 (lisp/nelisp-cc-val-load.el); any
      ;; other WORD's materialised Sexp would not have tag 0 (Nil) either
      ;; -- immediate Int(1) has tag 2, a pointer has its own tag -- so
      ;; comparing the raw WORD to 3 matches the original
      ;; `(sexp-tag ...) == 0' test exactly, for every value this slot
      ;; can actually hold.
      (if (> (record-slot-count frame-ptr) 2)
          (if (= (nelisp_frame_slot_raw_word frame-ptr 2) 3) 0 1)
        0))

    (defun nelisp_frame_local_names (frame)
      (if (> (record-slot-count frame) 3) (nl_record_slot_word frame 3) 0))

    (defun nelisp_frame_name_count (names)
      (if (= (sexp-tag names) 7)
          (+ 1 (nelisp_frame_name_count (nl_cons_cdr_word names))) 0))

    (defun nelisp_frame_declaration_scope_p (frame)
      ;; Empty and dynamic-only lets do not establish a lexical environment.
      (if (= (nelisp_frame_scope_boundary_p frame) 1) 1
        (> (nelisp_frame_cell_count frame)
           (if (> (record-slot-count frame) 1)
               (nelisp_frame_name_count (nl_record_slot_word frame 1)) 0))))

    ;; Bindings in FRAME's cell table, either shape.
    (defun nelisp_frame_cell_count (frame)
      (let* ((w (nelisp_frame_slot_raw_word frame 0)))
        (if (= (nelisp_frame_stack_find_word_tag_p w 12) 1)
            (nl_word_payload (nl_record_slot_word (nl_record_slot_word frame 0) 2))
          (nelisp_frame_chain_length w 0))))

    (defun nelisp_frame_chain_length (w n)
      (if (= (nelisp_frame_stack_find_word_tag_p w 7) 1)
          (nelisp_frame_chain_length
           (nl_cons_cdr_word w)
           (+ n (nelisp_frame_stack_find_word_tag_p (nl_cons_car_word w) 7)))
        n))

    ;; Closure calls (perf/prim phase 2).  A closure environment whose
    ;; entries are at most 8 (NAME . CELL) pairs with distinct names, plus
    ;; any bare declarations, already is a frame chain in GNU order: install
    ;; it as the captured frame's cell table instead of rebinding every
    ;; pair, and mark the frame header as shared so that a later bind into
    ;; this frame first copies the chain into a hash table
    ;; (`nelisp_frame_bind_in_frame') and never writes the closure's pairs.
    ;; Answers 1 when installed, 0 to take `nl_push_captured_walk'.
    (defun nl_frame_set_flags (value flag)
      ;; Private frame flags occupy kind bits 6/7, below the length field.
      ;; The semantic kind accessor masks those bits off.
      (let* ((base (nl_word_payload value))
             (header (heap-read-u64 base 0)))
        (ptr-write-u64 base 0
                       (logior (logand header -193)
                               (if (= flag 13110) 128
                                 (if (= flag 13109) 64 0))))))

    (defun nelisp_frame_try_share_chain (frames alist)
      (if (= (nelisp_frame_stack_find_word_tag_p alist 7) 0)
          0
        (if (= (nelisp_frame_chain_shareable alist 0) 0)
            0
          (let* ((frame (vector-ref-ptr
                         (nl_record_slot_word frames 0)
                         (- (sar (nelisp_frame_slot_raw_word frames 1) 2) 1))))
            (seq (record-slot-set frame 0 alist)
                 (nl_frame_set_flags frame 13110)
                 1)))))

    (defun nelisp_frame_chain_shareable (al n)
      (if (= (nelisp_frame_stack_find_word_tag_p al 7) 0)
          1
        (let* ((p (nl_cons_car_word al)))
          (if (= (nelisp_frame_stack_find_word_tag_p p 7) 0)
              (nelisp_frame_chain_shareable (nl_cons_cdr_word al) n)
            (if (>= n 8)
                0
              (if (= (nelisp_frame_stack_find_word_tag_p (nl_cons_cdr_word p) 11) 0)
                  0
                (if (= (nelisp_frame_stack_find_word_name_p (nl_cons_car_word p)) 0)
                    0
                  (if (= (nelisp_frame_chain_has_name
                          (nl_cons_cdr_word al) (nl_cons_car_word p))
                         1)
                      0
                    (nelisp_frame_chain_shareable (nl_cons_cdr_word al) (+ n 1))))))))))

    (defun nelisp_frame_chain_has_name (al name)
      (if (= (nelisp_frame_stack_find_word_tag_p al 7) 0)
          0
        (let* ((p (nl_cons_car_word al)))
          (if (if (= (nelisp_frame_stack_find_word_tag_p p 7) 1)
                  (= (extern-call nelisp_symbol_key_equal (nl_cons_car_word p) name) 1)
                0)
              1
            (nelisp_frame_chain_has_name (nl_cons_cdr_word al) name)))))

    ;; The bare declarations of a shared environment, recorded on the top
    ;; frame as `nl_push_captured_walk' does.
    (defun nelisp_frame_captured_decls (al frames)
      (if (= (nelisp_frame_stack_find_word_tag_p al 7) 0)
          0
        (let* ((p (nl_cons_car_word al)))
          (seq (if (= (logand p 1) 1)
                   0
                 (if (or (= (nelisp_frame_stack_find_word_name_p p) 1)
                         (= (sexp-tag p) 6)
                         (= (sexp-tag p) 15))
                     (nelisp_frame_local_declare_one
                      (vector-ref-ptr
                       (nl_record_slot_word frames 0)
                       (- (sar (nelisp_frame_slot_raw_word frames 1) 2) 1))
                      p)
                   0))
               (nelisp_frame_captured_decls (nl_cons_cdr_word al) frames)))))

    (defun nelisp_frame_local_declare_one (frame name)
      (if (> (record-slot-count frame) 3)
          (let* ((names (record-slot-ref-ptr frame 3)))
            (if (= (nl_capture_filter_contains names name) 1) 0
              (let* ((new-names (alloc-bytes 32 8)))
                (seq (cons-make-with-clone name names new-names)
                     (record-slot-set frame 3 new-names) 0))))
        1))

    (defun nelisp_frame_local_declare_walk (backing i name)
      (if (< i 0) 0
        (let* ((frame (vector-ref-ptr backing i)))
          ;; The outermost frame is the evaluator's root lexical context,
          ;; even when no user lexical variable has been installed there.
          (if (or (= i 0) (= (nelisp_frame_declaration_scope_p frame) 1))
              (nelisp_frame_local_declare_one frame name)
            (nelisp_frame_local_declare_walk backing (- i 1) name)))))

    (defun nelisp_frame_local_declare (frames name)
      (nelisp_frame_local_declare_walk (nl_record_slot_word frames 0)
       (- (sar (nelisp_frame_slot_raw_word frames 1) 2) 1) name))

    (defun nelisp_frame_local_special_walk (backing i name)
      (if (< i 0) 0
        (let* ((frame (vector-ref-ptr backing i))
               (names (nelisp_frame_local_names frame)))
          (if (if (= names 0) 0 (nl_capture_filter_contains names name)) 1
            (if (= (nelisp_frame_scope_boundary_p frame) 1) 0
              (nelisp_frame_local_special_walk backing (- i 1) name))))))

    (defun nelisp_frame_local_special_p (frames name)
      (nelisp_frame_local_special_walk (nl_record_slot_word frames 0)
       (- (sar (nelisp_frame_slot_raw_word frames 1) 2) 1) name))

    (defun nl_capture_local_names_w (names acc roots)
      (if (= (nl_val_tag names) 7)
          (let* ((word (nelisp_cons_construct_w (nl_cons_car_word names) acc 0)))
            (nl_capture_root_set_word roots 0 word)
            (nl_capture_local_names_w (nl_cons_cdr_word names) word roots))
        acc))

    (defun nl_capture_local_declarations_w (frame acc roots)
      (let* ((names (nelisp_frame_local_names frame)))
        (if (= names 0) acc (nl_capture_local_names_w names acc roots))))

    (defun nelisp_frame_scope_mark (frames-ptr)
      (let* ((depth (sar (nelisp_frame_slot_raw_word frames-ptr 1) 2)))
        (if (> depth 0)
            (let* ((frame (vector-ref-ptr (nl_record_slot_word frames-ptr 0)
                                         (- depth 1))))
              (if (> (record-slot-count frame) 2)
                  ;; Publish the fixnum WORD through the ordinary setter.
                  ;; The scope marker needs no materialized Sexp box.
                  (seq (record-slot-set frame 2 5) 0)
                1))
          1)))

    (defun nelisp_frame_scope_push (frames)
      (let* ((scratch (alloc-bytes 32 8)))
        (seq (nelisp_frame_push_direct frames (nl_frame_push_sym0_ptr)
                                       (nl_frame_push_sym1_ptr) scratch)
             (nelisp_frame_scope_mark frames))))

    (defun nelisp_frame_stack_find_kind_descend_m (backing-ptr i name-ptr dynamic raw _pad)
      (if (< i 0)
          0
        (let* ((frame-ptr (vector-ref-ptr backing-ptr i)))
          (let* ((found (if (= (nelisp_frame_binding_dynamic_p frame-ptr name-ptr)
                               dynamic)
                            (nelisp_frame_stack_find_in_frame_m frame-ptr name-ptr raw 0)
                          0)))
            (if (= found 0)
                (if (and (= dynamic 0) (= (nelisp_frame_scope_boundary_p frame-ptr) 1))
                    0
                  (nelisp_frame_stack_find_kind_descend_m
                   backing-ptr (- i 1) name-ptr dynamic raw 0))
              found)))))
    (defun nelisp_frame_stack_find_kind_descend (backing-ptr i name-ptr dynamic)
      (nelisp_frame_stack_find_kind_descend_m backing-ptr i name-ptr dynamic 0 0))

    (defun nelisp_frame_stack_find_kind (frames-ptr name-ptr dynamic raw)
      ;; Return the borrowed Cell pointer, like frame_stack_find.  DYNAMIC
      ;; is 0 for lexical and 1 for dynamic.  Leave the old mixed lookup's
      ;; ABI and allocation-free inner walk intact during evaluator migration.
      ;; Slot 0 (the backing Vector) is always a pointer WORD, so reading
      ;; it via `record-slot-ref-ptr' was already allocation-free.  Slot 1
      ;; (the depth) is always an Int, i.e. always an immediate WORD
      ;; ((n<<2)|1, `nl_val_load''s encoding); decode it straight from the
      ;; raw WORD instead of materialising a fresh 32-byte box through
      ;; `record-slot-ref-ptr' on every call just to unwrap it right back
      ;; out with `sexp-int-unwrap' -- this call happens once per lexical
      ;; variable reference.
      ;; RAW 1 answers the pair's cdr as it is (a Cell or a hybrid binding's plain
      ;; value, see `nl_frame_pair_cell'); the callers pass 0 and get a Cell.
      ;; Shallow binding: a dynamic binding of a plain variable lives in the
      ;; mirror, never in a frame cell, so a dynamic lookup of one has
      ;; nothing to find here (FRAMES-PTR is env+32; see `nl_sb_bind').
      (if (= (nelisp_frame_stack_find_valid_key name-ptr) 0)
          0
        (if (if (= dynamic 1) (nl_sb_mirror_only_p (- frames-ptr 32) name-ptr) 0)
            0
          (let* ((regular
                  (nelisp_frame_stack_find_kind_descend_m
                   (nl_record_slot_word frames-ptr 0)
                   (- (sar (nelisp_frame_slot_raw_word frames-ptr 1) 2) 1)
                   name-ptr dynamic raw 0)))
            (if (and (= dynamic 1) (/= (heap-read-u64 (- frames-ptr 32) 168) 0))
                (nl_thread_dynamic_cell (- frames-ptr 32) name-ptr regular)
              regular)))))

    (defun nelisp_frame_stack_find_value (frames-ptr name-ptr)
      ;; A lexical binding remains authoritative even if a later declaration
      ;; causes an inner binding of the same name to be dynamic.
      (let* ((lexical (nelisp_frame_stack_find_kind frames-ptr name-ptr 0 0)))
        (if (= lexical 0)
            (nelisp_frame_stack_find_kind frames-ptr name-ptr 1 0)
          lexical)))
    ;; The lexical reader's finder: the binding's cdr as it is (decode it with
    ;; `nl_cell_get_value_w'), a hybrid binding is not given a Cell.
    (defun nelisp_frame_stack_find_value_raw (frames-ptr name-ptr)
      (let* ((lexical (nelisp_frame_stack_find_kind frames-ptr name-ptr 0 1)))
        (if (= lexical 0)
            (nelisp_frame_stack_find_kind frames-ptr name-ptr 1 1)
          lexical)))

    (defun nl_capture_name_allowed (filtered filter-ptr name-ptr)
      (if (= filtered 0)
          1
        (nl_capture_filter_contains filter-ptr name-ptr)))

    (defun nl_capture_walk_filter_symbols_w (frame filter acc roots)
      (if (= (nl_val_tag filter) 7)
          (let* ((name (nl_cons_car_word filter))
                 (cell (if (= (nelisp_frame_binding_dynamic_p frame name) 1) 0
                         (nelisp_frame_stack_find_in_frame frame name))))
            (nl_capture_walk_filter_symbols_w frame (nl_cons_cdr_word filter)
              (if (= cell 0) acc (nl_capture_emit_one_w name cell acc roots)) roots))
        acc))

    (defun nl_capture_walk_bucket_w (chain acc filtered filter frame roots)
      (if (= (nl_val_tag chain) 7)
          (let* ((pair (nl_cons_car_word chain)))
            (if (= (nl_val_tag pair) 7)
                (let* ((name (nl_cons_car_word pair))
                       (cell (nl_cons_cdr_word pair)))
                  (nl_capture_walk_bucket_w (nl_cons_cdr_word chain)
                    (if (if (= (nelisp_frame_binding_dynamic_p frame name) 1) 0
                          (if (= filtered 2)
                              (if (= (nl_val_tag cell) 11)
                                  (= (nl_cell_pair_p cell) 1) 0)
                            (nl_capture_name_allowed filtered filter name)))
                        ;; The closure shares the binding's Cell; a hybrid
                        ;; binding gets one now (`nl_frame_pair_cell').
                        (nl_capture_emit_one_w name (nl_frame_pair_cell pair 0) acc roots) acc)
                    filtered filter frame roots))
              (nl_capture_walk_bucket_w (nl_cons_cdr_word chain) acc filtered filter frame roots)))
        acc))

    (defun nl_capture_walk_buckets_w (buckets j count acc filtered filter frame roots)
      (if (>= j count) acc
        (nl_capture_walk_buckets_w buckets (+ j 1) count
          (nl_capture_walk_bucket_w (nl_vector_slot_word buckets j) acc filtered filter frame roots)
          filtered filter frame roots)))

    (defun nl_capture_frame_chain (w)
      (if (= (nelisp_frame_stack_find_word_tag_p w 7) 1) w 0))

    (defun nl_capture_walk_frame_w (frame acc filtered filter roots)
      (let* ((captured
      (if (= filtered 1)
          ;; Explicit eval-alist cells remain visible independently of the filter.
          (nl_capture_walk_frame_w frame
            (nl_capture_walk_filter_symbols_w frame filter acc roots) 2 filter roots)
        (let* ((bindings (nelisp_frame_slot_raw_word frame 0)))
          (if (= (nl_val_tag bindings) 12)
              (nl_capture_walk_buckets_w (nl_record_slot_word bindings 1) 0
                (nl_word_payload (nl_record_slot_word bindings 0)) acc filtered filter frame roots)
            (nl_capture_walk_bucket_w bindings acc filtered filter frame roots))))))
        ;; Capture pairs retain the existing lexical cells. Promote only a
        ;; frame that actually contributed bindings to this environment.
        (seq (if (/= captured acc) (nl_frame_stack_promote frame 0) 0)
             captured)))

    (defun nl_capture_walk_frames_w (backing i acc filtered filter roots)
      (if (< i 0) acc
        (let* ((frame (nl_vector_slot_word backing i))
               (captured (nl_capture_walk_frame_w frame acc filtered filter roots))
               (with-names (nl_capture_local_declarations_w frame captured roots)))
          (if (= (nelisp_frame_scope_boundary_p frame) 1) with-names
            (nl_capture_walk_frames_w backing (- i 1) with-names filtered filter roots)))))

    (defun nl_capture_walk_frames_rooted_w (backing i filtered filter env)
      ;; Doc 216 S6.2: ROOTS is a native 4-slot vector whose WORD sits in one
      ;; WORD root.
      (let* ((mark (nl_root_mark env)) (root (nl_root_word env))
             (roots (+ (nl_alloc_vector 4) 6)))
        (seq
         (ptr-write-u64 root 0 roots)
         (nl_capture_root_set_word roots 2 filter)
         (nl_capture_root_set_word roots 3 backing)
         (let* ((word (nl_capture_walk_frames_w backing i 3 filtered filter roots)))
           (nl_root_release env mark)
           word))))
    (defun nl_capture_descend_native_w (in-vec)
      (let* ((depth (nl_word_payload (nl_vector_slot_word in-vec 1))))
        (if (= depth 0) 3
          (let* ((stack (nl_vector_slot_word in-vec 0))
                 (roots (alloc-bytes 32 8)))
            ;; The host bridge owns and roots IN-VEC for the whole native call.
            (nl_capture_root_set_word in-vec 2 roots)
            (vector-make 4 roots)
            (nl_capture_root_set_word roots 2 (nl_vector_slot_word in-vec 3))
            (nl_capture_walk_frames_w (nl_record_slot_word stack 0) (- depth 1) 3
              (nl_word_payload (nl_vector_slot_word in-vec 4))
              (nl_vector_slot_word in-vec 3) roots)))))
    (defun nl_capture_descend_native (in-vec out)
      (nl_abi_word_to_rc (nl_capture_descend_native_w in-vec) out)))
  "AOT source for Doc 111 §111.E #24 / Doc 115 §115.6
`frame_stack_find_rust_direct' + folded `lookup_in_frame'.

Pure-elisp innermost-first stack walk.  Composes `record-slot-ref-
ptr' (§111.B), `vector-ref-ptr' (§111.C), `sexp-payload-ptr' /
`cons-cdr-raw-from-box' (§101.B), `str-eq' (§101.C), `logand'
(§111.E), `sexp-int-unwrap' (§100), and `extern-call' into
`nelisp_fnv1a' (Doc 115 §115.7 pure-elisp 32-bit FNV-1a)
+ three recursive helper-function calls for the inner loops (=
bucket walk + per-frame lookup + stack descend).

Replaces the ~78 LOC Rust shim `nl_frame_stack_find' (+ private
`lookup_in_frame' helper) which has been removed from
`env_lexframe_aot_shims.rs'.")

(provide 'nelisp-cc-frame-stack-find)

;;; nelisp-cc-frame-stack-find.el ends here
