;;; nelisp-cc-mirror-bucket-prepend.el --- Doc 119 §119.A mirror_bucket_prepend  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 119 §119.A — pure-elisp port of the Rust
;; `mirror_prepend_to_bucket' helper.  Hashes NAME via the §115.7
;; `nelisp_fnv1a' AOT `.o', locates the destination bucket in
;; the env-mirror fast-hash-table, builds a fresh
;; `(KEY-STR . ENTRY-RECORD)' Cons pair, and prepends it onto the
;; bucket head via a refcount-safe `vector-slot-set'.  Bumps the
;; HT's entry-count slot.
;;
;; Mirrors `Env::mirror_prepend_to_bucket' (~45 LOC) in
;; `build-tool/src/eval/env_helpers.rs'.  Used by Doc 119 §119.A's
;; four `_or_insert' wrappers (= the miss-path of `mirror_set_value'
;; / `mirror_set_function' / `mirror_install_entry' /
;; `mirror_set_constant').
;;
;; Layout (= `lisp/nelisp-env.el' + `nelisp-stdlib-fast-hash.el'):
;;   mirror = Sexp::Record(`nelisp-env')
;;     slots[0] = Sexp::Record(`fast-hash-table')
;;       slots[0] = Sexp::Int (= bucket count, power-of-2)
;;       slots[1] = Sexp::Vector (= buckets, each elt either Sexp::Nil
;;                                  or Sexp::Cons of bucket chain)
;;       slots[2] = Sexp::Int (= entry count)
;;   Bucket head cell:  Sexp::Cons(NlConsBox)
;;     box.car = Sexp::Cons(INNER-PAIR)  (= (KEY-STR . ENTRY-RECORD))
;;     box.cdr = next bucket cell        (= Sexp::Cons / Sexp::Nil)
;;   Inner pair:  NlConsBox
;;     car = Sexp::Str(NAME)        (Str — NOT Symbol; see below)
;;     cdr = Sexp::Record(symbol-entry, [value, function, plist, constant])
;;
;; NOTE: the bucket KEY must be a `Sexp::Str' (not `Sexp::Symbol').
;; The baker-only walker `Env::mirror_iter_entries' tag-matches on
;; `Sexp::Str(k)' to enumerate entries; storing a Symbol would make
;; the entry invisible to image bake.  The AOT walker
;; `nelisp_mirror_walk_bucket' uses `str-eq' which is byte-payload-
;; only and works for both tags, so the lookup path doesn't care.
;; Per Rust impl: `Sexp::cons(Sexp::Str(name.to_string()), entry)'.
;;
;; Algorithm (= literal transcription of the Rust impl):
;;
;;   1. Read HT-PTR = record-slot-ref-ptr mirror 0.
;;   2. Read BUCKET-COUNT = sexp-int-unwrap (record-slot-ref-ptr HT 0).
;;      BUCKETS-PTR = record-slot-ref-ptr HT 1.
;;      Bootstrap-installed mirror always has BUCKET-COUNT = 1024 (=
;;      power of 2), so idx = h & (count - 1) is correct without the
;;      `% count' fallback.
;;   3. Materialise fresh `Sexp::Str(NAME)' KEY into scratch-key-slot
;;      via `sexp-write-str (str-bytes-ptr sym-ptr) (str-len sym-ptr)'.
;;      The sym-ptr can be either `Sexp::Symbol' or `Sexp::Str' — both
;;      have the same `String' payload layout per Doc 122 §122.H, so
;;      `str-bytes-ptr' + `str-len' work uniformly.
;;   4. Build INNER-PAIR `(KEY . ENTRY)' via `cons-make Nil Nil'
;;      followed by refcount-safe `cons-set-car KEY' / `cons-set-cdr
;;      ENTRY'.  (Same refcount-safety pattern as
;;      `nelisp-cc-frame-bind.el' — see that file's commentary for
;;      the Nil-seed + refcount-bumping `cons-set-*' rationale.)
;;   5. Build OUTER-CELL `(INNER . OLD-HEAD)' via the same pattern,
;;      reading OLD-HEAD via `vector-ref-ptr buckets idx'.
;;   6. Install OUTER-CELL at buckets[idx] via
;;      `vector-slot-set' (= `nl_vector_set_slot' clones the value
;;      into the slot then drops the old one, refcount-safe).
;;   7. Bump HT.slots[2] entry-count via `sexp-int-make' +
;;      `record-slot-set'.
;;
;; Caller-owned scratch vector layout (= `scratch-vec-ptr' is a
;; `Sexp::Vector' with 5 slots, all pre-initialised by the Rust safe
;; wrapper to `Sexp::Nil').  Doc 147 P1.5: only slot 0 is still consumed
;; here (as a READ source); slots 1-4 are no longer written through the
;; Vector interior — their freshly-built Sexps now live in fresh stack
;; slots (see the defun body) so a Phase-2 buffer stride shrink cannot
;; stomp neighbours.
;;
;;   slot 0  Sexp::Nil source        — read as `cons-make' Nil/Nil seed.
;;   slot 1  (was inner pair)        — now a fresh stack slot.
;;   slot 2  (was outer cell)        — now a fresh stack slot.
;;   slot 3  (was count int)         — now a fresh stack slot.
;;   slot 4  (was KEY Str)           — now a fresh stack slot.
;;
;; Outer arity is 4 (even ✓) so body-entry rsp ≡ 0 mod 16, matching
;; the static rsp-alignment of `cons-make' / `vector-slot-set' /
;; `record-make' etc.  (Doc 124.F-blocker even-arity fix.)
;;
;; ABI deps satisfied:
;;   §111.B  `record-slot-ref-ptr'  — env/HT slot pointer.
;;   §111.B  `record-slot-set'      — HT count slot install.
;;   §111.C  `vector-ref-ptr'       — bucket-head Sexp slot ptr.
;;   §111.E  `vector-slot-set'      — refcount-safe bucket-head install.
;;   §100    `sexp-int-unwrap'      — i64 payload read for count / cap.
;;   §100    `sexp-int-make'        — Sexp::Int writer for count bump.
;;   §101.D  `cons-make'            — fresh `NlConsBox' allocator.
;;   §101.D  `cons-set-car'         — refcount-safe pair-car install.
;;   §101.D  `cons-set-cdr'         — refcount-safe pair-cdr install.
;;   §101.C  `str-len' / `str-byte-at' (via §122.H `str-bytes-ptr')
;;                                   — sym-ptr byte payload extraction.
;;   §122.A  `sexp-write-str'       — fresh `Sexp::Str' allocator.
;;   §122.H  `str-bytes-ptr'        — `*const u8' for `Sexp::Symbol'.
;;   §100.A  `extern-call'          — `nelisp_fnv1a' for the hash.

;;; Code:

(defconst nelisp-cc-mirror-bucket-prepend--source
  '(seq
    (defun nelisp_mirror_prepend_install
        (buckets-ptr idx scratch-outer-slot scratch-pair-slot)
      ;; Tail of the miss path: populate the empty outer bucket cell in
      ;; SCRATCH-OUTER-SLOT (= `(Nil . Nil)' from the caller's `cons-make
      ;; Nil-src Nil-src') with car = SCRATCH-PAIR-SLOT (= the freshly
      ;; built (KEY . ENTRY) inner pair) and cdr = OLD bucket head, then
      ;; refcount-safely install at BUCKETS-PTR[idx] via `vector-slot-set'
      ;; (= `nl_vector_set_slot' clones the value into the slot then
      ;; drops the old one).  Returns 1 (= `vector-slot-set' sentinel).
      ;;
      ;; This sub-defun exists to keep the outer prepend-defun's
      ;; GP-register parameter count at the SysV AMD64 limit of 6.  The
      ;; 4-arg shape (= even) preserves the body-entry alignment
      ;; invariant.  Mirrors `nelisp_frame_bind_install' in
      ;; `nelisp-cc-frame-bind.el'.
      (and (cons-set-car scratch-outer-slot scratch-pair-slot)
           (cons-set-cdr scratch-outer-slot
                         (vector-ref-ptr buckets-ptr idx))
           (vector-slot-set buckets-ptr idx scratch-outer-slot)))
    (defun nelisp_mirror_bucket_prepend
        (mirror-ptr sym-ptr entry-ptr scratch-vec-ptr)
      ;; mirror-ptr:      *const Sexp pointing at Env::globals_record (=
      ;;                  Sexp::Record(`nelisp-env')).
      ;; sym-ptr:         *const Sexp pointing at Sexp::Symbol / Sexp::Str
      ;;                  whose byte payload is the entry NAME.
      ;; entry-ptr:       *const Sexp pointing at the freshly-allocated
      ;;                  `Sexp::Record(symbol-entry)' to install.  The
      ;;                  caller (= `_or_insert' wrapper) uses
      ;;                  `nelisp_mirror_alloc_entry' to materialise this
      ;;                  Sexp into its own scratch slot before this call.
      ;; scratch-vec-ptr: *const Sexp pointing at a Sexp::Vector with 5
      ;;                  slots (= layout in file commentary above).
      ;;
      ;; Returns: i64 — 1 on success.  The `and' chain threads all
      ;; sub-op rax sentinels through to the final `record-slot-set'
      ;; which materialises rax = 1.
      ;;
      ;; Refcount discipline (= the critical invariant — see
      ;; `nelisp-cc-frame-bind.el' for the full analysis):
      ;;   `cons-make' uses MVP byte-copy semantics (= no refcount bump
      ;;   on box-tagged car/cdr inputs).  Building the inner / outer
      ;;   pair directly with `cons-make' would share refcount-1
      ;;   handles between the caller's stack and the new boxes,
      ;;   causing SIGSEGV / double-free when the caller's frame
      ;;   unwinds.  Fix: seed with `(Nil . Nil)' then `cons-set-car' /
      ;;   `cons-set-cdr' which call refcount-aware
      ;;   `nl_consbox_set_car' / `nl_consbox_set_cdr' (= clone before
      ;;   write).  Same pattern as `nelisp_frame_bind_prepend'.
      ;; Doc 147 Phase 1.5 Group S — the four scratch slots that received
      ;; a freshly-built 32B Sexp via a write-through-interior-pointer
      ;; (slot 4 = KEY Str, slot 1 = inner pair, slot 2 = outer cell,
      ;; slot 3 = count Int) now land in FRESH 32B stack slots
      ;; (`alloc-bytes 32 8' on the per-eval arena, OUTSIDE the scratch
      ;; Vector buffer).  `sexp-write-str' / `cons-make' / `sexp-int-make'
      ;; wrote a full 32B Sexp at `data_ptr + N*32'; once the Phase-2
      ;; Vector buffer stride shrinks to 8B, those 32B writes would
      ;; address the wrong slot AND overrun three neighbours.  Routing the
      ;; allocators to stack slots removes the interior-pointer write
      ;; entirely; every later READ of the value uses the same stack slot.
      ;; Only scratch slot 0 (the `Sexp::Nil' cons-make seed source) is
      ;; still read via `(vector-ref-ptr scratch-vec-ptr 0)' — a READ
      ;; pointer, handled stride-correctly when Phase 2 updates
      ;; `vector-ref-ptr'; it is never a WRITE destination.
      ;;
      ;; Refcount re-derivation: UNCHANGED.  `cons-make' MVP byte-copy +
      ;; the `(Nil . Nil)' seed / `cons-set-car' / `cons-set-cdr'
      ;; refcount-bumping pattern is untouched — only the box that holds
      ;; the in-progress pair/cell moved from a Vector interior slot to a
      ;; stack slot.  Both are arena-reclaimed (never refcount-dropped),
      ;; so neither was a counted owner; the new bucket head's sole
      ;; counted owner remains buckets[idx] after `vector-slot-set'
      ;; clones it in.  `sexp-write-str' KEY and `sexp-int-make' count are
      ;; consumed (cloned) by `cons-set-car' / `record-slot-set'
      ;; respectively, identical to before.  No new owner, no leak.
      (let ((key-slot (alloc-bytes 32 8))
            (pair-slot (alloc-bytes 32 8))
            (outer-slot (alloc-bytes 32 8))
            (count-slot (alloc-bytes 32 8)))
        (and
         ;; Step 1: materialise fresh `Sexp::Str(NAME)' KEY into key-slot.
         (sexp-write-str key-slot
                         (str-bytes-ptr sym-ptr)
                         (str-len sym-ptr))
         ;; Step 2: alloc inner pair `(Nil . Nil)' into pair-slot.
         (cons-make (vector-ref-ptr scratch-vec-ptr 0)
                    (vector-ref-ptr scratch-vec-ptr 0)
                    pair-slot)
         ;; Step 3: refcount-safe inner-pair car = KEY, cdr = ENTRY.
         (cons-set-car pair-slot key-slot)
         (cons-set-cdr pair-slot entry-ptr)
         ;; Step 4: alloc outer cell `(Nil . Nil)' into outer-slot.
         (cons-make (vector-ref-ptr scratch-vec-ptr 0)
                    (vector-ref-ptr scratch-vec-ptr 0)
                    outer-slot)
         ;; Step 5: outer.car = INNER, outer.cdr = OLD-HEAD, install at
         ;; buckets[idx], all via `nelisp_mirror_prepend_install' sub-defun
         ;; (= keeps outer arity at 4).  Idx is computed from
         ;; FNV-1a(NAME) & (BUCKET-COUNT - 1).
         (nelisp_mirror_prepend_install
          (record-slot-ref-ptr
           (record-slot-ref-ptr mirror-ptr 0) ; HT-PTR
           1)                                  ; HT.slots[1] = buckets vec
          (logand
           (extern-call nelisp_fnv1a sym-ptr)
           (- (sexp-int-unwrap
               (record-slot-ref-ptr
                (record-slot-ref-ptr mirror-ptr 0)
                0))                            ; HT.slots[0] = bucket count
              1))
          outer-slot   ; outer scratch (stack)
          pair-slot)   ; inner pair scratch (stack)
         ;; Step 6: bump HT.slots[2] entry count.
         (sexp-int-make count-slot
                        (+ (sexp-int-unwrap
                            (record-slot-ref-ptr
                             (record-slot-ref-ptr mirror-ptr 0)
                             2))
                           1))
         (record-slot-set (record-slot-ref-ptr mirror-ptr 0)
                          2
                          count-slot)))))
  "AOT source for Doc 119 §119.A `mirror_bucket_prepend'.

Pure-elisp port of `Env::mirror_prepend_to_bucket' (~45 LOC).
Hashes NAME via the pure-elisp `nelisp_fnv1a' (§115.7), locates
the destination bucket, builds a fresh `(KEY-STR . ENTRY-RECORD)'
inner pair + outer bucket-cell `Sexp::Cons', installs at
buckets[idx] via refcount-safe `vector-slot-set' (§111.E), bumps
the HT entry-count slot.  Mirrors the algorithm + refcount
discipline of `nelisp-cc-frame-bind.el' (= same shape, different
KEY tag: bucket KEY is `Sexp::Str' here vs. `Sexp::Str' there).

Caller-owned scratch vector layout — see file commentary above.")

(provide 'nelisp-cc-mirror-bucket-prepend)

;;; nelisp-cc-mirror-bucket-prepend.el ends here
