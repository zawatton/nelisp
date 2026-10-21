;;; nelisp-cc-mirror-lookup-entry.el --- Doc 111 §111.E #1 mirror_lookup_entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group A helper #1 — `mirror_lookup_entry'.  AOT
;; reimplementation of the env-mirror hash-bucket walk that currently
;; lives in Rust at `build-tool/src/eval/env_mirror.rs::mirror_lookup_entry'.
;; This object replaces the inner walk; the Rust impl is kept alive
;; alongside until all Group A/B helpers ship (= the extern wrapper
;; that dispatches into the AOT `.o' replaces the Rust function
;; body in a follow-up commit after the full Group lands).
;;
;; Layout (= `lisp/nelisp-env.el' + `nelisp-stdlib-fast-hash.el'):
;;   globals_record = Sexp::Record(`nelisp-env')
;;     slots[0] = Sexp::Record(`fast-hash-table')
;;       slots[0] = Sexp::Int (= bucket count, power-of-2)
;;       slots[1] = Sexp::Vector (= buckets, each elt an alist)
;;       slots[2] = Sexp::Int (= entry count)
;;   Bucket alist cell: (Sexp::Cons (Sexp::Cons (Sexp::Str . Sexp::Record(`symbol-entry'))) . NEXT)
;;
;; ABI deps satisfied:
;;   §111.B  `record-slot-ref-ptr'  — slot pointer through env-record → ht-record.
;;   §111.C  `vector-ref-ptr'       — bucket pointer at FNV1a-hashed index.
;;   §101.B  `cons-walk' primitives — `sexp-payload-ptr' (= bucket / pair box-ptr extraction)
;;                                  + `cons-cdr-raw-from-box' (= next-bucket walk).
;;   §101.C  `str-eq'               — byte-payload equality between bucket KEY (Sexp::Str)
;;                                    and the lookup symbol (Sexp::Symbol).
;;   §100.A  `extern-call'          — `nelisp_fnv1a' for the hash (Doc 115
;;                                    §115.7 pure-elisp 32-bit FNV-1a;
;;                                    the previous `nl_mirror_fnv1a_sexp'
;;                                    Rust extern + `mirror_fnv1a' free fn
;;                                    have been deleted).
;;
;; Power-of-2 bucket-count assumption: the bootstrap mirror created by
;; `install_empty_mirror_rust_direct' always allocates 65536 buckets and
;; `mirror_prepend_to_bucket' never resizes, so the elisp body uses the
;; cheap `(h & (count-1))' mask matching the Rust fast path.

;;; Code:

(defun nelisp-cc-mirror-lookup-entry--symcell-mode ()
  "Build-time symbol-cell check mode (Doc 221 M02 negative controls).
\"1\": every cell answer is compared with the hashed walk, and a mismatch
exits with status 97.  \"fault\": the same, plus cells are deliberately
written wrong, so the check must fire.  Anything else: no check."
  (cdr (assoc (getenv "NELISP_SYMCELL_VERIFY")
              '(("1" . verify) ("fault" . fault)))))

(defun nelisp-cc-mirror-lookup-entry--cell-use (key)
  "The form answering the symbol cell `cell' found for KEY."
  (if (nelisp-cc-mirror-lookup-entry--symcell-mode)
      `(nl_symcell_checked mirror-ptr ,key cell)
    'cell))

(defun nelisp-cc-mirror-lookup-entry--cell-store ()
  "The entry WORD form `nl_symcell_refresh' stores at `idx'."
  (if (eq (nelisp-cc-mirror-lookup-entry--symcell-mode) 'fault)
      '(nl_symcell_fault_entry mirror-ptr idx (if (= entry 0) 3 entry))
    '(if (= entry 0) 3 entry)))

(defconst nelisp-cc-mirror-lookup-entry--source
  `(seq
    (defun nelisp_mirror_walk_bucket (cell-ptr sym-ptr)
      ;; Tail-recursive walk over a bucket's cons chain.
      ;;
      ;; Doc 147 Phase 3 — the NlConsBox car / cdr are now 8-byte tagged
      ;; WORDS (not 32B inline Sexps), so the bucket cell can no longer be
      ;; read as a raw box with car @ box+0 / cdr @ box+32.  The walk now
      ;; carries the bucket cell's 32B-slot Sexp VIEW and uses the Phase 3
      ;; materialising accessors `nl_cons_car_ptr' / `nl_cons_cdr_ptr':
      ;;
      ;;   cell-ptr: `*const Sexp' — a Cons (tag 7) bucket cell whose car
      ;;             is the (KEY . ENTRY) PAIR, cdr is the next bucket
      ;;             cell (or Nil at end-of-bucket).  0 / non-Cons = end.
      ;;   sym-ptr:  `*const Sexp' for the Sexp::Symbol / Sexp::Str key.
      ;;
      ;; Returns: i64.  On hit, the `*const Sexp' 32B-slot view of the
      ;; matching ENTRY (= the PAIR's cdr, `Sexp::Record(NlRecordRef)').
      ;; On miss / end-of-bucket, 0.
      ;;
      ;;   pair-view = nl_cons_car_ptr(cell)   // the (KEY . ENTRY) PAIR
      ;;   key-view  = nl_cons_car_ptr(pair)   // KEY -> str-eq vs sym-ptr
      ;;   hit       -> nl_cons_cdr_ptr(pair)  // ENTRY view (live box)
      ;;   miss      -> recurse nl_cons_cdr_ptr(cell)  // next bucket cell
      ;;
      ;; The materialising accessors return the live 32B child box for a
      ;; pointer slot (bucket cells / pairs / entries are all boxed), and
      ;; a fresh-per-call 32B box only for the rare immediate slot, so two
      ;; concurrently-live views never alias (no shared-scratch clobber
      ;; under the recursive descent).
      (if (= cell-ptr 0)
          0
        (if (= (nl_val_tag cell-ptr) 7)
            ;; The validated Cons payload is physical storage. Read each
            ;; edge directly instead of repeating its WORD tag/payload decode.
            (let* ((cell-box (nl_word_payload cell-ptr))
                   (pair (heap-read-u64 cell-box 0)))
              (if (= (nl_val_tag pair) 7)
                  (let* ((pair-box (nl_word_payload pair))
                         (key (heap-read-u64 pair-box 0)))
                    (if (if (= (logand key 1) 0)
                            (= (extern-call nelisp_symbol_key_equal key sym-ptr) 1)
                          0)
                        (let ((entry (heap-read-u64 pair-box 8)))
                          (if (= (logand entry 1) 0) entry 0))
                      (nelisp_mirror_walk_bucket
                       (heap-read-u64 cell-box 8) sym-ptr)))
                (nelisp_mirror_walk_bucket
                 (heap-read-u64 cell-box 8) sym-ptr)))
          0)))
    (defun nelisp_mirror_lookup_entry_valid_key (sym-ptr)
      ;; Cold-load hardening (segfault fix, 2026-07): a caller-supplied
      ;; SYM-PTR that is not actually a live Sexp::Symbol / Sexp::Str
      ;; must never be hashed/compared as one.  Reproduced crash:
      ;; `nl_sf_setq' evaluating a `setq' deep inside a cold-loaded
      ;; `org-mode' activation body passes a SYM-PTR whose 32-byte view
      ;; reads tag 7 (Cons) instead of 4/5 (Symbol/Str) -- the pointer
      ;; is in-arena and mapped (so a bare `nl_gc_in_arena' guard does
      ;; NOT catch it), but `nelisp_fnv1a'/`str-eq' blindly reading its
      ;; would-be char* field at +16 lands on an unrelated live object's
      ;; bytes and eventually dereferences a value that is not a valid
      ;; pointer in THIS process, causing a SIGSEGV inside
      ;; `nelisp_fnv1a_step4'.  Root cause is upstream (almost certainly
      ;; the dump-time relocation walk in
      ;; `scripts/nelisp-standalone-build.el' -- `nl_fa_roots' and
      ;; friends -- missing or mis-targeting some occurrence reachable
      ;; only through a persisted function body); this guard does not
      ;; fix that, it prevents the guaranteed-wrong dereference from
      ;; crashing the process.  A sound "not found" on a malformed key
      ;; matches the existing `wf_key_eq_depth' precedent (commit
      ;; 7aa3ed8b): a miss is always safe, a crash is not.
      ;; Doc 226 D9: a symbol object's WORD (class 110, kind 4 or 16) lives
      ;; in a symbol_block cell outside the chunks; its header is the check.
      (if (= (logand sym-ptr 7) 6)
          (if (or (= (logand (heap-read-u64 (- sym-ptr 6) 0) 63) 4)
                  (= (logand (heap-read-u64 (- sym-ptr 6) 0) 63) 16)) 1 0)
      (if (and (= (extern-call nl_gc_in_arena sym-ptr) 0)
               (= (extern-call nl_root_pin_slot_active sym-ptr) 0)
               (= (extern-call nl_rootstack_slot_active sym-ptr) 0))
          0
        (if (or (= (sexp-tag sym-ptr) 4) (= (sexp-tag sym-ptr) 16))
            1
          (if (or (= (sexp-tag sym-ptr) 5)
                  (= (sexp-tag sym-ptr) 14)) 1 0)))))
    ;; perf/mirror-bucket-count-raw: `ht_rec.slots[0]' (the bucket count)
    ;; is `Sexp::Int' by this function's own precondition list below --
    ;; it is NEVER a pointer WORD -- so every call to
    ;; `nelisp_mirror_lookup_entry' (every `mirror_is_constant' /
    ;; `mirror_is_bound' / `mirror_is_fbound' / `mirror_lookup_value' /
    ;; `mirror_lookup_function' / `mirror_set_*_or_insert' -- i.e. every
    ;; `set'/`setq', constant or not, hit or miss) paid
    ;; `nl_record_slot_ptr''s `(alloc-bytes 32 8)' + `nl_val_load' to
    ;; materialise a fresh 32-byte box for that Int just to unwrap the
    ;; same integer back out with `sexp-int-unwrap' and discard the box.
    ;; This mirrors A1's `nelisp_frame_slot_raw_word' fix
    ;; (lisp/nelisp-cc-frame-stack-find.el, commit ab4a72484): read the
    ;; slot's raw 8-byte tagged WORD directly (identical box_ptr ->
    ;; slots.data_ptr -> word addressing to `nl_record_slot_ptr',
    ;; lisp/nelisp-cc-nlrecord-slot-ptr.el) and decode the Int in place
    ;; with `(sar word 2)' -- `nl_val_load' encodes `Int(n)' as
    ;; `(n<<2)|1' (lisp/nelisp-cc-val-load.el), so an arithmetic
    ;; right-shift by 2 recovers the exact same `n' `sexp-int-unwrap'
    ;; would have, discarding the tag bit the shift drops regardless of
    ;; its value.  REC-PTR here is always the fast-hash-table Record
    ;; (`ht_rec' below), whose slot 0 is provably never a pointer WORD,
    ;; so no behavior changes for any value this slot can hold.
    (defun nelisp_mirror_slot_raw_word (rec-ptr idx)
      (nl_record_slot_word rec-ptr idx))

    ;; Doc 221 M02/M23 and Doc 226 D2, the symbol's own object.  GNU reads
    ;; the cells in the symbol itself, without a name hash:
    ;; XSYMBOL (fun)->u.s.function (eval.c:3153, bytecode.c:803) and the
    ;; SYMBOL_PLAINVAL value (bytecode.c:640, data.c:1630); lisp.h:786.
    ;; Adapted: a NeLisp symbol box (tag 4) is a view of its canonical
    ;; interned name buffer (+16), whose header holds the symbol's index
    ;; (`nl_intern_bump'); the mirror table's slot 3 vector maps the index
    ;; to the symbol's one object (a nine-slot symbol-entry record).
    ;; Answers:
    ;;   the object WORD -- NAME's symbol has an object;
    ;;   0 -- NAME is an interned symbol without an object, i.e. without
    ;;        state (void value and function, empty plist).  Every writer
    ;;        that creates an object stores it here (`nl_symcell_refresh'),
    ;;        so this is answered without the hash;
    ;;   1 -- NAME cannot have an object slot (its name buffer is outside
    ;;        the intern arena, or the table has no slot 3); the caller
    ;;        walks the hashed chain.
    ;; Every hop is an inline load (no accessor calls): a WORD's payload
    ;; is W-6 for a compact vectorlike and [W+8] for a box; a record keeps
    ;; its slot count in header>>8 and slot I at payload+16+8I, a vector
    ;; its length in header>>8 and slot I at payload+8+8I.
    (defun nl_symcell_entry (mirror-ptr name)
      (let* ((buf (heap-read-u64 name 16))
             (base (heap-read-u64 268436288 0)))
        (if (and (> buf (+ base 16777224))
                 (< buf (heap-read-u64 268436296 0)))
            (let* ((mp (if (= (logand mirror-ptr 7) 6) (- mirror-ptr 6)
                         (heap-read-u64 mirror-ptr 8)))
                   (hw (heap-read-u64 mp 16))
                   (ht (if (= (logand hw 7) 6) (- hw 6) (heap-read-u64 hw 8))))
              (if (< (sar (heap-read-u64 ht 0) 8) 4) 1
                (let* ((idx (logand (heap-read-u64 (- buf 8) 0) 4294967295)))
                  (if (= idx 0) 0
                    (let* ((cw (heap-read-u64 ht 40))
                           (cells (if (= (logand cw 7) 6) (- cw 6)
                                    (heap-read-u64 cw 8))))
                      (if (>= idx (sar (heap-read-u64 cells 0) 8)) 0
                        (let* ((entry (heap-read-u64 (+ cells (* idx 8)) 8)))
                          (if (= (logand entry 1) 1) 0 entry))))))))
          1)))

    ;; The hashed bucket walk, kept for keys without a cell and as the
    ;; authority `nl_symcell_refresh' copies into a cell.  KEY is a valid
    ;; symbol or string key.
    (defun nelisp_mirror_lookup_hashed (mirror-ptr key)
      (let* ((ht (nl_record_slot_word mirror-ptr 0))
             (head (heap-read-u64
                    (+ (+ (nl_word_payload (nl_record_slot_word ht 1)) 8)
                       (* (logand (extern-call nelisp_fnv1a key)
                                  (- (sar (nelisp_mirror_slot_raw_word ht 0) 2) 1)) 8)) 0)))
        (if (= (logand head 1) 1) 0
          (nelisp_mirror_walk_bucket head key))))

    ;; Cell maintenance.  Every writer that prepends a mirror entry calls
    ;; `nl_symcell_refresh' for its key after the bucket store, so a
    ;; non-empty cell always names the first entry of the key's chain,
    ;; which is what the hashed walk answers.  There is no unlink path.
    (defun nl_symcell_capacity (m idx)
      (if (> m idx) m (nl_symcell_capacity (* m 2) idx)))
    ;; 1 when the table has a cell vector holding IDX, growing it first;
    ;; 0 when the table has no cells.  Allocation may move the table, so
    ;; the old vector is reread after it.
    (defun nl_symcell_reserve (mirror-ptr idx)
      (let* ((ht (nl_record_slot_word mirror-ptr 0)))
        (if (< (nl_record_length ht) 4) 0
          (let* ((n (nl_vector_length (nl_record_slot_word ht 3))))
            (if (< idx n) 1
              (let* ((fresh (extern-call nl_alloc_vector
                                         (nl_symcell_capacity
                                          (if (< n 8) 16 (* n 2)) idx)))
                     (old (nl_word_payload
                           (nl_record_slot_word
                            (nl_record_slot_word mirror-ptr 0) 3)))
                     (i 0))
                (seq
                 (while (< i n)
                   (seq (ptr-write-u64 (+ (+ fresh 8) (* i 8)) 0
                                       (heap-read-u64 (+ (+ old 8) (* i 8)) 0))
                        (setq i (+ i 1))))
                 (extern-call nl_record_set_slot
                              (nl_word_payload (nl_record_slot_word mirror-ptr 0))
                              3 (+ fresh 6))
                 1)))))))
    (defun nl_symcell_refresh (mirror-ptr key)
      (if (if (= key 0) 1 (= (logand key 1) 1)) 0
        (if (or (= (sexp-tag key) 4) (= (sexp-tag key) 5))
            (let* ((idx (seq (extern-call nl_symcells_attach_lazy mirror-ptr)
                             (extern-call nl_symcell_assign
                                          (str-bytes-ptr key) (str-len key)))))
              (if (= idx 0) 0
                (if (= (nl_symcell_reserve mirror-ptr idx) 0) 0
                  (let* ((entry (nelisp_mirror_lookup_hashed mirror-ptr key)))
                    (seq
                     ;; Doc 226 D1 (verify builds): a symbol's object is never
                     ;; replaced, so a writer that publishes a different first
                     ;; entry for a key whose slot holds an object has made a
                     ;; second object.  Inline: the F1 prelink closure is capped.
                     ,@(when (nelisp-cc-mirror-lookup-entry--symcell-mode)
                         '((let* ((cells (nl_record_slot_word
                                          (nl_record_slot_word mirror-ptr 0) 3))
                                  (old (if (< idx (nl_vector_length cells))
                                           (nl_vector_slot_word cells idx) 3)))
                             (if (or (= (logand old 1) 1) (= entry 0)
                                     (= (nl_word_payload old) (nl_word_payload entry)))
                                 0
                               (syscall-direct 231 97 0 0 0 0 0)))))
                     (extern-call nl_vector_set_slot
                                  (nl_word_payload
                                   (nl_record_slot_word
                                    (nl_record_slot_word mirror-ptr 0) 3))
                                  idx ,(nelisp-cc-mirror-lookup-entry--cell-store))
                     1)))))
          0)))

    (defun nelisp_mirror_lookup_entry_owned (mirror-ptr name)
      ;; Clause 1: NAME is a live value from an owning/rooted container,
      ;; not an arbitrary borrowed address. As in frame-stack-find's WORD
      ;; walk, its owner makes a tag check sufficient. No allocation or
      ;; safepoint occurs here. Reject non-key values before hashing; the
      ;; generic entry below retains full address authentication for callers
      ;; without this ownership proof (including its cold-load hardening).
      (if (if (= name 0) 1 (= (logand name 1) 1)) 0
        (if (= (sexp-tag name) 4)
            ;; Doc 226 D9 stage 3: an interned symbol's object WORD is its own entry.
            (if (= (logand name 7) 6) name
            (let* ((cell (nl_symcell_entry mirror-ptr name)))
              (if (= cell 1)
                  (nelisp_mirror_lookup_hashed mirror-ptr name)
                ,(nelisp-cc-mirror-lookup-entry--cell-use 'name))))
          (if (= (sexp-tag name) 16)
              ;; Doc 226 D9: an uninterned symbol is its own object.
              (if (= (logand name 7) 6) name
                (if (> (heap-read-u64 name 8) 65535) (+ (heap-read-u64 name 8) 6)
                  (nelisp_mirror_lookup_hashed mirror-ptr name)))
            (if (or (= (sexp-tag name) 5) (= (sexp-tag name) 14))
                (nelisp_mirror_lookup_hashed mirror-ptr name)
              0)))))

    (defun nelisp_mirror_lookup_entry (mirror-ptr sym-ptr)
      ;; mirror-ptr: *const Sexp pointing at the env-mirror Record
      ;;             (= `globals_record', tag `nelisp-env').
      ;; sym-ptr:    *const Sexp pointing at the Sexp::Symbol /
      ;;             Sexp::Str to look up.
      ;;
      ;; Returns: i64.  On hit, the `*const Sexp' of the symbol-entry
      ;; Record (slot 1 of the bucket's inner (KEY . ENTRY) cons).
      ;; On miss / empty mirror / malformed SYM-PTR, 0.
      ;;
      ;; Pre-conditions (= caller / dispatcher responsibility, mirrors
      ;; the Rust impl's early-`return None' arms):
      ;;   - mirror-ptr.tag = Sexp::Record.
      ;;   - mirror-ptr.slots[0].tag = Sexp::Record (= fast-hash-table).
      ;;   - ht_rec.slots[0].tag = Sexp::Int (= bucket-count, power of 2).
      ;;   - ht_rec.slots[1].tag = Sexp::Vector (= buckets).
      ;; Doc 147 Phase 3: seed the walk with the bucket-head Sexp VIEW
      ;; (`vector-ref-ptr' = the 32B-slot view of the bucket slot — a
      ;; Cons for a non-empty bucket, Nil for an empty one), NOT the raw
      ;; `sexp-payload-ptr' box (the walker now reads car/cdr WORDS via
      ;; the materialising accessors, so it needs the Sexp VIEW).
      ;;
      ;; perf/mirror-bucket-count-raw: `ht-rec-ptr' is bound once (was
      ;; re-derived via a second `record-slot-ref-ptr mirror-ptr 0' call
      ;; below) and its slot 0 (bucket count) is read via
      ;; `nelisp_mirror_slot_raw_word' instead of the materialising
      ;; `record-slot-ref-ptr' + `sexp-int-unwrap' -- see that helper's
      ;; commentary above for the invariant.
      (if (= (nelisp_mirror_lookup_entry_valid_key sym-ptr) 0)
          0
        ;; A symbol with a cell is answered from it (Doc 221 M02, see
        ;; `nl_symcell_entry'); otherwise the bucket head is read as a raw
        ;; word (Vector box at payload+8, data at box+8): `vector-ref-ptr'
        ;; boxed the Nil of an empty bucket into a fresh heap slot, and
        ;; with a table sized for a GUI image most lookups of names that
        ;; have no global entry (every lexical variable reference and
        ;; `setq' checks one) land on an empty bucket.  A pointer head is
        ;; the bucket's first Cons view.
        (if (if (= (logand sym-ptr 7) 6) 1
              (if (= (sexp-tag sym-ptr) 16) (> (heap-read-u64 sym-ptr 8) 65535) 0))
            ;; Doc 226 D9: a symbol object WORD (kind 4 or 16) is its own entry,
            ;; and the view of an uninterned symbol carries its object.
            (if (= (logand sym-ptr 7) 6) sym-ptr (+ (heap-read-u64 sym-ptr 8) 6))
        (let ((cell (if (= (sexp-tag sym-ptr) 4)
                        (nl_symcell_entry mirror-ptr sym-ptr) 1)))
          (if (= cell 1)
              (nelisp_mirror_lookup_hashed mirror-ptr sym-ptr)
            ,(nelisp-cc-mirror-lookup-entry--cell-use 'sym-ptr))))))
    ,@(when (nelisp-cc-mirror-lookup-entry--symcell-mode)
        '((defun nl_symcell_checked (mirror-ptr key cell)
            ;; CELL is the object answer, or 0 for "no object" (Doc 226
            ;; D2); the hashed walk must agree in both cases.
            (let* ((hashed (nelisp_mirror_lookup_hashed mirror-ptr key)))
              (if (if (= cell 0) (= hashed 0)
                    (and (/= hashed 0)
                         (= (nl_word_payload hashed) (nl_word_payload cell))))
                  cell
                (syscall-direct 231 97 0 0 0 0 0))))
          ;; A wrong entry: the previous index's cell when it has one.
          (defun nl_symcell_fault_entry (mirror-ptr idx entry)
            (if (< idx 2) entry
              (let* ((prev (nl_vector_slot_word
                            (nl_record_slot_word
                             (nl_record_slot_word mirror-ptr 0) 3)
                            (- idx 1))))
                (if (= (logand prev 1) 1) entry prev)))))))
  "AOT source for Doc 111 §111.E #1 `mirror_lookup_entry'.

Composes record-slot-ref-ptr (§111.B) + vector-ref-ptr (§111.C) +
cons-walk primitives (§101.B) + str-eq (§101.C) + extern-call into
`nelisp_fnv1a' (Doc 115 §115.7) to walk the env-mirror fast-hash-
table without materialising any intermediate Sexp slot (= every
hop is a raw `*const Sexp' / NlConsBox* pointer in rax).

2026-07 cold-load segfault hardening: `nelisp_mirror_lookup_entry_valid_key'
gates SYM-PTR on `nl_gc_in_arena' + `sexp-tag' (4 Symbol / 5 Str) before
any hash/compare touches it, so a malformed key (observed: tag 7 Cons)
reaching this single choke-point for every mirror lookup/insert path
(`mirror_is_constant', `mirror_lookup_value', `mirror_is_bound',
`mirror_is_fbound', `mirror_lookup_function', `mirror_install_entry*',
`mirror_set_*_or_insert') returns a sound miss instead of dereferencing
garbage inside `nelisp_fnv1a'/`nelisp_mirror_walk_bucket'.")

(provide 'nelisp-cc-mirror-lookup-entry)

;;; nelisp-cc-mirror-lookup-entry.el ends here
