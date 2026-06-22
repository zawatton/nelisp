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
;; STEP 3b-ii (DONE) + 3c all-roots: the pointer SWIZZLE proper.  Baked
;; `nelisp--arena-swizzle-verify' bulk-copies chunk-0, then mirrors the GC
;; per-type walk (`nl_gc_mark_slot' / `-cons' / `-vec_slots') from ALL roots
;; (`nl_fa_roots' = every published frame's globals/frames/unbound + the
;; reader transients result/out/src/cursor/bsym + the shared symentry,
;; mirroring `nl_gc_mark_published_frame') and, at every pointer FIELD, rewrites the matching
;; word IN THE COPIED buffer from an absolute address to an arena-relative
;; offset (in-place; no separate pointer list).  The SOURCE heap is never
;; written.  Verified by a round trip: swizzle then unswizzle restores the
;; copy byte-for-byte.  On target/nelisp: ~75.7 k in-region pointers
;; swizzled, ~13.8 k out-of-region (interned / large-object / growth-chunk
;; targets, for the multi-region dump), ROUNDTRIP-MISMATCH = 0 (perfectly
;; reversible, with and without a prior GC); source uncorrupted (A12 +
;; eval still work).  See `nelisp-flat-arena-probe-swizzle'.
;;
;; STEP 4 (core LOAD mechanic DONE): baked
;; `nelisp--arena-load-relocate-verify' copies chunk-0 into a fresh buffer
;; (its address is the new base), rebases every in-region pointer to that
;; base via the per-type walk (dir 2: field = target + (newbase - oldbase)),
;; then verifies the loaded image is a STRUCTURALLY VALID arena -- a linear
;; [header][object] walk of the relocated buffer reaches the end exactly
;; (relocate touched only pointer fields, never the block headers).  Returns
;; (RELOCATED-PTRS BLOCKS WELLFORMED).  On target/nelisp: ~75.7 k pointers
;; relocated (== the swizzle's in-region count, a cross-check), ~464 k blocks,
;; WELLFORMED = 1; source uncorrupted.  This proves an image relocates to an
;; arbitrary (kernel-chosen) base as a sound arena -- the load half of the
;; memcpy+relocate round trip.  See `nelisp-flat-arena-probe-load-relocate'.
;;
;; STEP 3c (DONE): the image is now COMPLETE.
;;   - all roots: swizzle/relocate walk from every published frame + reader
;;     transients + symentry (`nl_fa_roots'), not just frame[0] globals.
;;   - all chunks: measured chunk-count = 1 (the live heap is entirely in
;;     chunk-0; growth chunks were transient GC garbage), so the single-chunk
;;     copy already covers every live chunk.
;;   - external regions: the image carries TWO regions -- chunk-0 at offset
;;     [0,span) and the interned symbol-name region at [span, span+isize) --
;;     and a pointer target is swizzled into whichever region holds it.
;;   Result on target/nelisp: ~90 k pointers swizzle into the image,
;;   OUT-OF-REGION = 0 (the former ~13.8k out-of-region were all interned
;;   symbol-name targets; no large objects in this heap's live graph),
;;   ROUNDTRIP-MISMATCH = 0, and the load relocates all ~90 k into a fresh
;;   base with both regions copied -> WELLFORMED = 1.  Every live inter-object
;;   pointer is now relocatable: the dump<->load mechanism is complete.
;;
;; STEP 4-boot precondition (DONE): `nelisp--arena-image-root-verify' reports
;; the IMAGE OFFSETS of the 3 EvalCtx roots so a boot loader can reinstall
;; them at newbase+offset.  On target/nelisp: globals_record and frames_record
;; are in-image (offsets >= 0); unbound_marker is -1 (an immediate, rebuilt
;; inline, no relocation).  So every datum the boot must reconstruct is either
;; in the relocatable image or a trivial immediate.  See
;; `nelisp-flat-arena-probe-roots'.
;;
;; BOOT-WIRING started: the boot loader relocates with a flat RELOCATION TABLE
;; (no graph walk on load).  `nelisp--arena-dump-table-verify' demonstrates it
;; end-to-end in-process: the dump's swizzle (dir 3) records every pointer
;; field's offset into a table, and the LOAD does exactly what boot would --
;; for each table entry F: image[F] += newbase (the image offset already
;; encodes the region, so one `+ newbase' lands chunk-0 or interned).  On
;; target/nelisp: ~90 k-entry table, OUT-OF-REGION = 0, and the table-driven
;; load rebuilds a WELLFORMED arena.  See `nelisp-flat-arena-probe-dump-table'.
;;
;; FILE persistence (DONE): `nelisp--arena-dump-image-to-file' writes the image
;; ({64B header | table | regions}) to a path, and
;; `nelisp--arena-load-image-from-file' reads it back, applies the relocation
;; table (image[F] += newbase), and verifies the result.  On target/nelisp a
;; ~46 MB image round-trips: magic OK, ~90 k-entry table, loaded image
;; WELLFORMED.  The cold-start image now persists across processes.  See
;; `nelisp-flat-arena-probe-file-roundtrip'.
;;
;; BOOT-LOAD INSTALL (DONE, in-process): `nelisp--arena-boot-load-verify' runs
;; exactly what a boot hook runs -- read the image, relocate it, RECONSTRUCT
;; the EvalCtx globals root (a tag-12 Record slot whose box = imgbase +
;; globals_off), and confirm it resolves to a valid Record in the loaded image.
;; On target/nelisp: (1 12 1 72) = magic OK, root tag 12 (Record), box inside
;; chunk-0, sane 72-byte block header (8 + a 64-byte NlRecord).  So the whole
;; cold-loader pipeline is proven end to end: dump -> file -> load -> relocate
;; -> reconstruct a sound EvalCtx root.  See `nelisp-flat-arena-probe-boot-load'.
;;
;; REMAINING (wire it into the actual boot -- a focused integration, ideally on
;; the L2 `nemacs' boot where the win is, since the L1 reader already boots
;; fast): load into the mmap'd ARENA (chunk-0 + interned at their real, separate
;; bases -- split the `+ newbase' by region) instead of a buffer, install the
;; LIVE EvalCtx roots, and take this path BEFORE the 352-module source replay.
;;   (Large objects + multi-live-chunk are not exercised by this heap; add if a
;;   future heap needs them.)

;;; Code:

;; Runtime primitives, supplied by the baked standalone reader; absent at
;; host byte-compile time.  Declared so `make compile' (error-on-warn)
;; stays clean.
(declare-function ptr-read-u64 "ext" (addr off))
(declare-function nelisp--arena-stats "ext" ())
(declare-function nelisp--arena-walk-verify "ext" ())
(declare-function nelisp--arena-dump-copy-verify "ext" ())
(declare-function nelisp--arena-mark-reach-verify "ext" ())
(declare-function nelisp--arena-swizzle-verify "ext" ())
(declare-function nelisp--arena-load-relocate-verify "ext" ())
(declare-function nelisp--arena-image-root-verify "ext" ())
(declare-function nelisp--arena-dump-table-verify "ext" ())
(declare-function nelisp--arena-dump-image-to-file "ext" (path))
(declare-function nelisp--arena-load-image-from-file "ext" (path))
(declare-function nelisp--arena-boot-load-verify "ext" (path))
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

(defun nelisp-flat-arena-probe-swizzle ()
  "Pointer-swizzle round trip via the baked `nelisp--arena-swizzle-verify' op.
Bulk-copies chunk-0, swizzles its pointers (absolute -> arena offset) by a
per-type walk from the globals root, then unswizzles; the copy must return
byte-identical.  The source heap is never written.  Return a labelled plist;
:roundtrip-mismatch 0 means the swizzle is exactly reversible."
  (let ((r (nelisp--arena-swizzle-verify)))
    (list :in-region-ptrs (nth 0 r)
          :out-region-ptrs (nth 1 r)
          :roundtrip-mismatch (nth 2 r)
          :reversible (= (nth 2 r) 0))))

(defun nelisp-flat-arena-probe-load-relocate ()
  "Load round trip via the baked `nelisp--arena-load-relocate-verify' op.
Copies chunk-0 into a fresh buffer, rebases its pointers to that buffer's
base, and confirms the relocated image is a structurally valid arena (linear
block walk reaches the end exactly).  Return a labelled plist;
:wellformed t means the image loads to a fresh base as a sound arena."
  (let ((r (nelisp--arena-load-relocate-verify)))
    (list :relocated-ptrs (nth 0 r)
          :blocks (nth 1 r)
          :wellformed (= (nth 2 r) 1))))

(defun nelisp-flat-arena-probe-boot-load (path)
  "Run the boot-load INSTALL mechanism on the image at PATH (dump it first).
Reads the image, relocates it, reconstructs the EvalCtx globals root, and
checks the root resolves to a valid Record in the loaded image -- exactly what
a boot hook does, but on a fresh buffer so the live runtime is untouched.
Return a labelled plist; :magic-ok t with :root-tag 12 and :root-in-chunk0 t
means a boot would install a sound globals root from the persisted image."
  (nelisp--arena-dump-image-to-file path)
  (let ((r (nelisp--arena-boot-load-verify path)))
    (list :magic-ok (= (nth 0 r) 1)
          :root-tag (nth 1 r)
          :root-in-chunk0 (= (nth 2 r) 1)
          :gbox-block-total (nth 3 r))))

(defun nelisp-flat-arena-probe-file-roundtrip (path)
  "Dump the cold-start image to PATH, load it back, and verify it.
Writes {header | relocation table | regions} to PATH, then reads it,
applies the table (image[F] += newbase), and walks the result.  Return a
labelled plist; :magic-ok and :loaded-wellformed t means the persisted image
round-trips into a sound arena."
  (let* ((written (nelisp--arena-dump-image-to-file path))
         (r (nelisp--arena-load-image-from-file path)))
    (list :bytes-written written
          :magic-ok (= (nth 0 r) 1)
          :table-len (nth 1 r)
          :blocks (nth 2 r)
          :loaded-wellformed (= (nth 3 r) 1))))

(defun nelisp-flat-arena-probe-dump-table ()
  "Table-driven boot-load round trip via `nelisp--arena-dump-table-verify'.
Builds the offset image + its relocation table, then relocates exactly as the
boot loader would (per table entry F: image[F] += newbase) and checks the
loaded image is well-formed.  Return a labelled plist; :loaded-wellformed t
with :out-of-region 0 means the table fully drives the load."
  (let ((r (nelisp--arena-dump-table-verify)))
    (list :table-len (nth 0 r)
          :out-of-region (nth 1 r)
          :blocks (nth 2 r)
          :loaded-wellformed (= (nth 3 r) 1))))

(defun nelisp-flat-arena-probe-roots ()
  "Image offsets of the 3 EvalCtx roots via `nelisp--arena-image-root-verify'.
A non-negative offset means the boot can relocate + reinstall that root at
newbase+offset; -1 means an immediate (rebuilt inline)."
  (let ((r (nelisp--arena-image-root-verify)))
    (list :globals-offset (nth 0 r)
          :frames-offset (nth 1 r)
          :unbound-offset (nth 2 r)
          :globals-in-image (>= (nth 0 r) 0)
          :frames-in-image (>= (nth 1 r) 0))))

(defun nelisp-flat-arena-probe-report (&optional limit)
  "Print walk, full walk, bulk-copy, root-coverage, swizzle, load, and roots."
  (princ (nelisp-flat-arena-probe-walk limit))
  (princ "\n")
  (princ (nelisp-flat-arena-probe-verify-full))
  (princ "\n")
  (princ (nelisp-flat-arena-probe-copy-verify))
  (princ "\n")
  (princ (nelisp-flat-arena-probe-mark-reach t))
  (princ "\n")
  (princ (nelisp-flat-arena-probe-swizzle))
  (princ "\n")
  (princ (nelisp-flat-arena-probe-load-relocate))
  (princ "\n")
  (princ (nelisp-flat-arena-probe-roots)))

(provide 'nelisp-flat-arena-probe)

;;; nelisp-flat-arena-probe.el ends here
