;;; nelisp-flat-arena-probe.el --- read-only flat-arena snapshot probe  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Spike step 1 (READ-ONLY) for the flat-arena cold-start snapshot.
;;
;; GOAL of the larger spike: replace the per-object heap-image codec
;; (`nelisp-heap-image.el', ~45 obj/sec, OOM > 1000 objects) and the
;; source-replay runtime-image (`nelisp-runtime-image.el', re-evals the
;; whole boot) with a flat-arena image: bulk-copy the live heap bytes +
;; relocate pointers on load (O(n) memcpy + a single swizzle pass).
;;
;; This step makes NO mutation: it verifies the load-bearing INVARIANT
;; that the runtime heap is a contiguous, linearly-walkable, self-describing
;; `[header][object]' block sequence -- which is exactly what a flat-arena
;; dump bulk-copies.  Running it never loads or writes a snapshot, so it
;; cannot corrupt the heap.
;;
;; VERIFIED RUNTIME MODEL (probed on target/nelisp, Doc 140 Stage 8
;; chunk-arena; corrects an earlier reconnaissance that mis-read the
;; aspirational `nelisp-allocator.el' single-nursery module):
;;
;;   * The heap is a bump arena over `mmap(NULL)' chunks.  The chunk-0
;;     base is KERNEL-CHOSEN at run time (NO fixed address, NO MAP_FIXED)
;;     and stored in the `nl_arena_base' bss slot.  Every former
;;     fixed-base (0x10000000-relative) metadata access is rewritten to
;;     `nl_arena_base + offset' at build time.  => absolute inter-object
;;     pointers differ every run, so a flat-arena image MUST swizzle
;;     pointers to arena-relative offsets (a fixed-base relocation-free
;;     image is NOT possible here).
;;
;;   * `(nelisp--arena-stats)' is eval-callable and returns a list whose
;;     field 0 is the REAL runtime arena base (the mmap address).  From
;;     it, base-relative metadata reads work via `ptr-read-u64':
;;
;;       base+0x000  bump cursor (offset; live data ends at base+cursor)
;;       base+0x2c0  chunk-head descriptor pointer
;;       base+0x2c8  chunk-current descriptor pointer
;;       base+0x2d0  chunk count
;;       base+0x300  chunk-0 descriptor:
;;                     +0x00 base   +0x08 size   +0x10 cursor
;;                     +0x18 data-start (absolute = base+0x400)
;;                     +0x20 limit   +0x28 flags  +0x30 next
;;       base+0x400  first object block (data-start)
;;
;;   * Each allocation carries an 8-byte block header at `obj-8'
;;     (Doc 08 sec.8.18): a u64 whose high bits are BLOCK_TOTAL (bytes
;;     from this header to the next, 8-aligned) and whose low 3 bits are
;;     the GC mark (0 live / 1 marked / 2 free).  The object pointer is
;;     `header + 8'; the next header is `header + BLOCK_TOTAL'.  This is
;;     the same walk the GC sweep uses.
;;
;; STEP 2 (DONE): a baked, native-speed `nelisp--arena-walk-verify' op now
;; walks the FULL multi-chunk arena (the interpreted walk below times out on
;; the ~28 MB eval-interpreter arena).  It follows the chunk-descriptor
;; chain and, within each chunk, the [header][object] sequence to the
;; chunk's live end (`bf_arena_chunk_cursor', the GC's own cursor),
;; returning (BLOCKS LIVE FREE BYTES WELLFORMED).  On a normal heap every
;; block is well-formed (BLOCKS = LIVE + FREE); it also correctly flags a
;; malformed/gappy state (e.g. after `nelisp--arena-force-grow-smoke', which
;; artificially inflates the cursor past the live data).  See
;; `nelisp-flat-arena-probe-verify-full'.  Implemented in
;; `scripts/nelisp-standalone-build.el' (`bf_arena_walk_verify').
;;
;; STEP 3a (DONE): the bulk-copy "memcpy" half.  Baked
;; `nelisp--arena-dump-copy-verify' copies chunk-0's live region into a
;; fresh large-object buffer (own mmap; the source heap is never mutated)
;; and verifies byte-identity, returning (USED-BYTES MISMATCH-WORDS
;; DEST-PTR).  On target/nelisp: ~28.7 MB copied, MISMATCH-WORDS = 0 (the
;; copy is byte-faithful) at native speed.  See
;; `nelisp-flat-arena-probe-copy-verify'; implemented in
;; `scripts/nelisp-standalone-build.el' (`bf_arena_dump_copy_verify').
;;
;; STEP 3b-i (DONE): root-coverage precondition for the pointer swizzle.
;; Baked `nelisp--arena-mark-reach-verify' reuses the GC mark-from-roots
;; (`nl_gc_mark_published_contexts' / `-rootstack' / `-symentry') to mark the
;; reachable graph, linearly counts the marked blocks, then CLEARS the marks
;; (no sweep -> nothing freed; the heap is left exactly as before).  Returns
;; (REACHABLE TOTAL).  On target/nelisp the reachable set is ~76 k objects;
;; the linear "live" count is much larger only because the bump allocator
;; retains unreachable-not-yet-swept garbage -- after `(garbage-collect)' the
;; live count drops to ~83 k and REACHABLE ~= LIVE (~92%; the remainder are
;; transient C-stack roots, which a dump intentionally omits).  This proves
;; the published roots reach the whole persistent live set the dump must
;; capture.  See `nelisp-flat-arena-probe-mark-reach'.
;;
;; NEXT STEPS (not in this file):
;;   step 3b-ii  the swizzle proper: mirror the GC per-type walk
;;               (`nl_gc_mark_slot') but RECORD each pointer field's
;;               arena-offset + target, then rewrite each absolute pointer in
;;               the copied buffer to (chunk, offset).  Extend the copy to
;;               every chunk.
;;   step 4      load: alloc fresh chunks, memcpy, unswizzle to the new base,
;;               install the roots into a fresh EvalCtx; boot hook before the
;;               source-replay fallback.

;;; Code:

;; Runtime primitives, supplied by the baked standalone reader; absent at
;; host byte-compile time.  Declared so `make compile' (error-on-warn)
;; stays clean.
(declare-function ptr-read-u64 "ext" (addr off))
(declare-function nelisp--arena-stats "ext" ())
(declare-function nelisp--arena-walk-verify "ext" ())
(declare-function nelisp--arena-dump-copy-verify "ext" ())
(declare-function nelisp--arena-mark-reach-verify "ext" ())
(declare-function garbage-collect "ext" ())

(defconst nelisp-flat-arena-probe--chunk-head-offset  #x2c0)
(defconst nelisp-flat-arena-probe--chunk-count-offset #x2d0)
(defconst nelisp-flat-arena-probe--desc-base-offset       #x00)
(defconst nelisp-flat-arena-probe--desc-cursor-offset     #x10)
(defconst nelisp-flat-arena-probe--desc-data-start-offset #x18)
(defconst nelisp-flat-arena-probe--desc-next-offset       #x30)

(defun nelisp-flat-arena-probe-base ()
  "Return the real runtime arena base (mmap address) via `nelisp--arena-stats'."
  (nth 0 (nelisp--arena-stats)))

(defun nelisp-flat-arena-probe-walk (&optional limit)
  "Walk the chunk-0 `[header][object]' block sequence (READ-ONLY).
Start at chunk-0's descriptor data-start (absolute) and follow BLOCK_TOTAL
toward chunk-0's live end (descriptor base + cursor).  Stop after LIMIT
blocks (default 8000): this walker is INTERPRETED and the eval-interpreter
arena is large, so a full ~28 MB walk times out -- the full, multi-chunk,
reach-end-exactly walk is step 2 (a baked op, native speed).  Following the
chunk-descriptor chain (`:desc-next') to later chunks is also step 2; the
chunks are SEPARATE mmaps, so a single linear walk past chunk-0's end would
read unmapped/foreign memory.  This proves the block FORMAT is sound; it
never mutates the heap.
Return a plist of the verification result."
  (let* ((base (nelisp-flat-arena-probe-base))
         (desc (ptr-read-u64 base nelisp-flat-arena-probe--chunk-head-offset))
         (cbase (ptr-read-u64 desc nelisp-flat-arena-probe--desc-base-offset))
         (cursor (ptr-read-u64 desc nelisp-flat-arena-probe--desc-cursor-offset))
         (cend (+ cbase cursor))
         (hdr (ptr-read-u64 desc nelisp-flat-arena-probe--desc-data-start-offset))
         (cap (or limit 8000))
         (n 0) (live 0) (free 0) (bytes 0) (wellformed 1))
    (while (and (= wellformed 1) (< hdr cend) (< n cap))
      (let* ((h (ptr-read-u64 hdr 0))
             (bt (- h (logand h 7)))
             (mark (logand h 7)))
        (if (or (< bt 8) (> (+ hdr bt) cend))
            (setq wellformed 0)
          (setq n (+ n 1)
                bytes (+ bytes bt)
                live (if (= mark 2) live (+ live 1))
                free (if (= mark 2) (+ free 1) free)
                hdr (+ hdr bt)))))
    (list :base base
          :chunk-count (ptr-read-u64 base nelisp-flat-arena-probe--chunk-count-offset)
          :chunk0-base cbase :chunk0-live-end cend
          :blocks n :live live :free free :walked-bytes bytes
          :wellformed (= wellformed 1)
          :hit-cap (= n cap))))

(defun nelisp-flat-arena-probe-verify-full ()
  "Full multi-chunk arena walk via the baked `nelisp--arena-walk-verify' op.
Native speed (the interpreted `nelisp-flat-arena-probe-walk' only checks a
bounded prefix).  Return a labelled plist; READ-ONLY, never mutates."
  (let ((wv (nelisp--arena-walk-verify)))
    (list :blocks (nth 0 wv)
          :live (nth 1 wv)
          :free (nth 2 wv)
          :bytes (nth 3 wv)
          :wellformed (= (nth 4 wv) 1)
          :blocks-eq-live+free (= (nth 0 wv) (+ (nth 1 wv) (nth 2 wv))))))

(defun nelisp-flat-arena-probe-copy-verify ()
  "Bulk-copy chunk-0's live region + byte-identity check via the baked
`nelisp--arena-dump-copy-verify' op (the dump \"memcpy\" half).  The source
heap is never mutated.  Return a labelled plist."
  (let ((r (nelisp--arena-dump-copy-verify)))
    (list :used-bytes (nth 0 r)
          :mismatch-words (nth 1 r)
          :byte-faithful (= (nth 1 r) 0)
          :dest-ptr (nth 2 r))))

(defun nelisp-flat-arena-probe-mark-reach (&optional gc-first)
  "Root-coverage check via the baked `nelisp--arena-mark-reach-verify' op.
Marks the reachable graph from the published roots, counts it, then clears
the marks (no sweep).  With GC-FIRST non-nil, run `(garbage-collect)' first
so the linear live count drops to the reachable set for a tight comparison.
Return a labelled plist."
  (when gc-first (garbage-collect))
  (let* ((mr (nelisp--arena-mark-reach-verify))
         (reach (nth 0 mr))
         (live (nth 1 (nelisp--arena-walk-verify))))
    (list :reachable reach
          :linear-live live
          :reach-le-live (<= reach live)
          :reach-pct-of-live (if (> live 0) (/ (* reach 100) live) 0))))

(defun nelisp-flat-arena-probe-report (&optional limit)
  "Print the walk, full walk, bulk-copy, and root-coverage (post-GC) checks."
  (princ (nelisp-flat-arena-probe-walk limit))
  (princ "\n")
  (princ (nelisp-flat-arena-probe-verify-full))
  (princ "\n")
  (princ (nelisp-flat-arena-probe-copy-verify))
  (princ "\n")
  (princ (nelisp-flat-arena-probe-mark-reach t)))

(provide 'nelisp-flat-arena-probe)

;;; nelisp-flat-arena-probe.el ends here
