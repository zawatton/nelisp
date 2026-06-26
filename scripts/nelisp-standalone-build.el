;;; nelisp-standalone-build.el --- Pure-elisp standalone NeLisp eval build  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Reproducible, INCREMENTAL build of a standalone NeLisp eval binary that
;; needs ZERO Rust (no cargo / rustc / target binary).  The REAL NeLisp
;; evaluator (nl_eval_inner + the combiner cons/apply cluster + bootstrap
;; mirror) is compiled by the pure-elisp AOT compiler into relocatable
;; units and linked by the pure-elisp static linker into a freestanding
;; static ELF.  The single primitive that was Rust-only (nelisp_apply_function,
;; the builtin name -> native op dispatcher) is supplied here in pure elisp.
;;
;; Toolchain (all pure elisp):
;;   each unit source -> `nelisp-aot-compiler' (my-compile-to-unit) ->
;;   in-memory link-unit -> `nelisp-static-linker' (nelisp-link-units) ->
;;   ELF64 ET_EXEC at target/nelisp-standalone-eval.
;;
;; REBUILD MODEL (the point of this file):
;;   * Whole build:        (nelisp-standalone-build)         [Makefile: standalone-eval]
;;   * Individual .el:     editing one lisp/nelisp-cc-XXX.el invalidates ONLY that
;;                         unit's cached object (target/standalone-units/NAME.unit);
;;                         the next build recompiles just that unit + relinks.
;;   * Force one unit:     (nelisp-standalone-rebuild-one "eq-symbol.o")
;;   * Clean cache:        rm -rf target/standalone-units   [Makefile: standalone-eval-clean]
;;
;; The cached object format is the link-unit elisp datum (text bytes + symbols
;; + relocs) serialized with `print-escape-nonascii' so the cache is plain
;; ASCII and round-trips via `read'.  Freshness = cache newer than the unit's
;; source .el AND newer than the compiler/linker sources.
;;
;; PARAMETRIZED FORM (proves the evaluator really runs, not a fixed exit):
;;   NELISP_FORM_OP / NELISP_FORM_A / NELISP_FORM_B select the embedded form
;;   (OP A B); default (+ 1 2) -> exit 3.  Supported OP: + - * .  The driver
;;   unit is always recompiled (it is tiny and form-dependent); all other
;;   units are cached.

;;; Code:

(require 'cl-lib)
(require 'nelisp-aot-compiler)
(require 'nelisp-static-linker)
(require 'nelisp-elf-write)
(require 'nelisp-mach-o-write)

(defconst nelisp-standalone--this-file
  (or load-file-name buffer-file-name)
  "Absolute path of this build script (= source file of the glue units).")

(defconst nelisp-standalone--repo-root
  (file-name-directory
   (directory-file-name (file-name-directory nelisp-standalone--this-file)))
  "Repo root (parent of scripts/).")

(defconst nelisp-standalone--cache-dir
  (expand-file-name "target/standalone-units" nelisp-standalone--repo-root)
  "Per-unit compiled-object cache directory.")

(defconst nelisp-standalone--target
  (intern (or (getenv "NELISP_STANDALONE_TARGET") "linux-x86_64"))
  "Standalone output target.
Defaults to `linux-x86_64' for backwards compatibility.  Windows-native builds
must opt in with NELISP_STANDALONE_TARGET=windows-x86_64 so Windows-hosted ELF
cache builds do not accidentally mix Win64 units into the SysV cache.")

(defun nelisp-standalone--parse-int-env (name default)
  "Parse integer environment variable NAME, returning DEFAULT when unset.
Accepts decimal strings and 0x-prefixed hexadecimal strings."
  (let ((s (getenv name)))
    (if (and s (> (length s) 0))
        (if (string-match-p "\\`0[xX][0-9a-fA-F]+\\'" s)
            (string-to-number (substring s 2) 16)
          (string-to-number s))
      default)))

(defconst nelisp-standalone--arena-base #x10000000
  "Default standalone arena base used by the Linux/SysV path.")

(defconst nelisp-standalone--linux-arena-size
  (let* ((env (getenv "NELISP_LINUX_ARENA_SIZE"))
         (n (and env (string-to-number env))))
    (if (and n (> n 0)) n #x10000000))
  "Linux standalone fixed FIRST-CHUNK reservation size (default 256 MiB).
Doc 140 Stage 7: reduced from the historical 8 GiB to 256 MiB.  This is no
longer the whole arena — it is just the bootstrap first chunk.  Allocation
beyond it grows by adding `nl_os_alloc_chunk' chunks (`nl_alloc_bytes' ->
`nl_chunk_alloc_new'), so pressure is handled by chunk growth instead of an
ever-larger fixed reservation.  The chunk descriptor stores this size
dynamically, so shrinking it does not change any membership/sweep bound.
Override via the NELISP_LINUX_ARENA_SIZE env var (bytes) — used by the
chunk-growth pressure test to force growth with a tiny first chunk.")

(defconst nelisp-standalone--windows-arena-base
  (nelisp-standalone--parse-int-env
   "NELISP_STANDALONE_WINDOWS_ARENA_BASE" #x70000000)
  "Windows standalone arena base.
Can be overridden with NELISP_STANDALONE_WINDOWS_ARENA_BASE.  Keep the default
below 2 GiB so Phase47's current signed imm32 materialization remains valid.")

(defconst nelisp-standalone--windows-arena-size #x4000000
  "Windows standalone fixed arena size.
The arena base is `nelisp-standalone--windows-arena-base' after source rebase.
Keep the committed mapping at 64 MiB: Windows `VirtualAlloc' with MEM_COMMIT
charges the full range up front, unlike Linux's demand-paged mmap, and 1 GiB can
fail on normal developer machines before the first metadata write.")

(defconst nelisp-standalone--macos-arena-base #x800000000
  "macOS standalone arena base.
Must live above the 4 GiB __PAGEZERO segment used by the Mach-O executable
writer, and away from the dyld shared cache region used by normal Mach-O
executables.")

(defconst nelisp-standalone--macos-arena-size #x20000000
  "macOS standalone fixed arena size.
Large enough for the reader/bootstrap path while avoiding the historical 8 GiB
fixed reservation that some Darwin environments reject before the first page is
touched.")

(defconst nelisp-standalone--arena-data-start-offset #x400
  "Offset where normal standalone arena object headers begin.

The bytes below this offset are the bootstrap control block.  Doc 140 Stage 2
adds chunk control slots and the first chunk descriptor there while allocation
still uses the historical fixed first arena.")

(defconst nelisp-standalone--arena-chunk-head-offset #x2c0)
(defconst nelisp-standalone--arena-chunk-current-offset #x2c8)
(defconst nelisp-standalone--arena-chunk-count-offset #x2d0)
(defconst nelisp-standalone--arena-chunk-bytes-reserved-offset #x2d8)
(defconst nelisp-standalone--arena-chunk-bytes-used-offset #x2e0)
(defconst nelisp-standalone--arena-chunk-bytes-reclaimed-offset #x2e8)
(defconst nelisp-standalone--arena-chunk-alloc-failures-offset #x2f0)
(defconst nelisp-standalone--arena-boundary-reclaim-enabled-offset #x2f8)

(defconst nelisp-standalone--arena-chunk0-desc-offset #x300)
(defconst nelisp-standalone--arena-chunk-desc-base-offset #x00)
(defconst nelisp-standalone--arena-chunk-desc-size-offset #x08)
(defconst nelisp-standalone--arena-chunk-desc-cursor-offset #x10)
(defconst nelisp-standalone--arena-chunk-desc-data-start-offset #x18)
(defconst nelisp-standalone--arena-chunk-desc-limit-offset #x20)
(defconst nelisp-standalone--arena-chunk-desc-flags-offset #x28)
;; Doc 140 Stage 6: chunk desc.flags generation bits.  Bit 0 = the historical
;; "in use" marker.  Bit 1 (`persistent') marks the boot/global generation
;; (chunk 0 — boot image, the global mirror, definitions, escaped values) that
;; top-level boundary reclamation must NEVER reset.  Grown chunks omit the
;; persistent bit (temporary per-form scratch) so `nl_boundary_reset_tail_chunks'
;; reclaims only them.  In the current head=chunk0 / tail=grown topology this is
;; a no-op (chunk 0 is never a reclaim tail), but it makes the persistent /
;; temporary generation split explicit and robust to future list reordering.
(defconst nelisp-standalone--arena-chunk-flag-persistent 2)
(defconst nelisp-standalone--arena-chunk-desc-next-offset #x30)

(defconst nelisp-standalone--arena-default-chunk-size #x4000000
  "Default standalone arena chunk size after the fixed first chunk.")

(defconst nelisp-standalone--arena-chunk-align #x10000
  "Alignment used for standalone chunk reservations and large chunk sizing.")

(defun nelisp-standalone--target-arena-base (&optional target)
  "Return standalone arena base for TARGET."
  (pcase (or target nelisp-standalone--target)
    ((or 'windows-x86_64 'windows-aarch64) nelisp-standalone--windows-arena-base)
    ('macos-aarch64 nelisp-standalone--macos-arena-base)
    (_ nelisp-standalone--arena-base)))

(defun nelisp-standalone--target-arena-size (&optional target)
  "Return standalone arena size for TARGET."
  (pcase (or target nelisp-standalone--target)
    ((or 'windows-x86_64 'windows-aarch64) nelisp-standalone--windows-arena-size)
    ('macos-aarch64 nelisp-standalone--macos-arena-size)
    (_ nelisp-standalone--linux-arena-size)))

(defun nelisp-standalone--target-arena-metadata-address (offset &optional target)
  "Return target arena metadata address at OFFSET."
  (+ (nelisp-standalone--target-arena-base target) offset))

(defun nelisp-standalone--target-abi (&optional target)
  "Return the compiler ABI for standalone TARGET."
  (pcase (or target nelisp-standalone--target)
    ('linux-x86_64 'sysv)
    ;; Windows on ARM64 passes the first 8 integer args in x0-x7 with no
    ;; shadow space — AAPCS64-compatible for everything this codebase emits
    ;; (fixed-arity integer/pointer calls).
    ((or 'macos-aarch64 'linux-aarch64 'windows-aarch64) 'aapcs64)
    ('windows-x86_64 'win64)
    (other (error "standalone: unsupported target %S" other))))

(defun nelisp-standalone--target-arch (&optional target)
  "Return the Phase47 architecture for standalone TARGET."
  (pcase (or target nelisp-standalone--target)
    ((or 'macos-aarch64 'linux-aarch64 'windows-aarch64) 'aarch64)
    ((or 'linux-x86_64 'windows-x86_64) 'x86_64)
    (other (error "standalone: unsupported target %S" other))))

(defun nelisp-standalone--target-os (&optional target)
  "Return the Phase47 OS tag for standalone TARGET."
  (pcase (or target nelisp-standalone--target)
    ('macos-aarch64 'darwin)
    ((or 'windows-x86_64 'windows-aarch64) 'windows)
    ((or 'linux-x86_64 'linux-aarch64) 'linux)
    (other (error "standalone: unsupported target %S" other))))

(defun nelisp-standalone--target-object-name (name &optional target)
  "Return target-appropriate object NAME.
Linux keeps the historical `.o' unit names; Windows uses `.obj' in cache names,
link-unit names, and build logs."
  (if (and (eq (or target nelisp-standalone--target) 'windows-x86_64)
           (string-suffix-p ".o" name))
      (concat (substring name 0 -2) ".obj")
    name))

(defun nelisp-standalone--target-cache-dir (&optional target)
  "Return the per-target standalone unit cache directory."
  (let ((target (or target nelisp-standalone--target)))
    (expand-file-name
     (if (eq target 'windows-x86_64)
         (format "windows-x86_64-arena-%x"
                 nelisp-standalone--windows-arena-base)
       (symbol-name target))
     nelisp-standalone--cache-dir)))

(defconst nelisp-standalone--out
  (expand-file-name "target/nelisp-standalone-eval" nelisp-standalone--repo-root)
  "Output standalone ELF path (baked-form eval).")

(defconst nelisp-standalone--windows-out
  (expand-file-name "target/nelisp-standalone-eval.exe" nelisp-standalone--repo-root)
  "Output standalone PE32+ path (baked-form eval).")

(defconst nelisp-standalone--reader-out
  (expand-file-name "target/nelisp" nelisp-standalone--repo-root)
  "Output standalone ELF path for the user-facing reader CLI.")

(defconst nelisp-standalone--artifact-runtime-source-path
  (expand-file-name "target/nelisp-artifact-runtime.el"
                    nelisp-standalone--repo-root)
  "Source bundle cached for standalone artifact commands.")

(defconst nelisp-standalone--artifact-runtime-cache-path
  (concat nelisp-standalone--artifact-runtime-source-path ".nelc")
  "Compiled runtime cache for standalone artifact commands.")

(defconst nelisp-standalone--artifact-runtime-cache-enable-path
  (concat nelisp-standalone--artifact-runtime-cache-path ".enable")
  "Marker enabling the standalone artifact runtime cache.
`nelisp-standalone-build-artifact-runtime-cache' writes this marker together
with the cache artifact.  Removing it is the supported fallback switch for
diagnosing the full source command path.")

(defconst nelisp-standalone--source-command-cache-stats-path
  (expand-file-name "target/nelisp-source-command-cache.stats"
                    nelisp-standalone--repo-root)
  "Opt-in marker for source-command cache arena stats on stderr.")

(defconst nelisp-standalone--windows-reader-out
  (expand-file-name "target/nelisp.exe" nelisp-standalone--repo-root)
  "Output standalone PE32+ path for the user-facing reader CLI.")

(defvar nelisp-standalone--recompiled nil
  "Names of units recompiled in the current build (vs. served from cache).")

(defun nelisp-standalone--output-path (&optional reader-p)
  "Return the current target's standalone output path."
  (pcase nelisp-standalone--target
    ('windows-x86_64 (if reader-p
                         nelisp-standalone--windows-reader-out
                       nelisp-standalone--windows-out))
    ;; Cross-built targets get arch-suffixed outputs so they never clobber
    ;; the host-arch target/nelisp.
    ('linux-aarch64 (concat (if reader-p
                                nelisp-standalone--reader-out
                              nelisp-standalone--out)
                            "-aarch64"))
    ('windows-aarch64 (concat (if reader-p
                                  nelisp-standalone--reader-out
                                nelisp-standalone--out)
                              "-aarch64.exe"))
    ('macos-aarch64 (if reader-p nelisp-standalone--reader-out nelisp-standalone--out))
    (_ (if reader-p nelisp-standalone--reader-out nelisp-standalone--out))))

(defun nelisp-standalone--dep-files ()
  "Toolchain source files; any newer than a cache entry forces recompile."
  (delq nil (cons nelisp-standalone--this-file
                  (mapcar #'locate-library
                          '("nelisp-aot-compiler" "nelisp-static-linker"
                            "nelisp-elf-write" "nelisp-cc-runtime")))))

(defconst nelisp-standalone--arena-rebase-span #x1000
  "Number of low arena-base-relative metadata bytes rewritten per target.")

(defun nelisp-standalone--windows-rebase-arena-source (source)
  "Rebase fixed arena metadata constants in SOURCE for Windows.
Standalone glue still uses a compact fixed metadata block for bump, signal,
GC, and mutation-epoch slots.  Windows cannot reliably reserve the historical
0x10000000 range, so the Windows target moves numeric atoms in
[`nelisp-standalone--arena-base', + span) to
`nelisp-standalone--windows-arena-base' before Phase47 parses the source."
  (if (or (not (eq nelisp-standalone--target 'windows-x86_64))
          (= nelisp-standalone--windows-arena-base
             nelisp-standalone--arena-base))
      source
    (cl-labels
        ((rebased-int
          (n)
          (if (and (integerp n)
                   (<= nelisp-standalone--arena-base n)
                   (< n (+ nelisp-standalone--arena-base
                           nelisp-standalone--arena-rebase-span)))
              (+ nelisp-standalone--windows-arena-base
                 (- n nelisp-standalone--arena-base))
            n))
         (walk
          (form)
          (cond
           ((integerp form) (rebased-int form))
           ((consp form) (cons (walk (car form)) (walk (cdr form))))
           (t form))))
      (let ((max-lisp-eval-depth (max max-lisp-eval-depth 10000)))
        (walk source)))))

(defun nelisp-standalone--rebase-arena-source (source)
  "Rebase fixed arena metadata constants in SOURCE for the current target."
  (let ((target-base (nelisp-standalone--target-arena-base)))
    (if (= target-base nelisp-standalone--arena-base)
        source
      (cl-labels
          ((rebased-int
            (n)
            (if (and (integerp n)
                     (<= nelisp-standalone--arena-base n)
                     (< n (+ nelisp-standalone--arena-base
                             nelisp-standalone--arena-rebase-span)))
                (+ target-base (- n nelisp-standalone--arena-base))
              n))
           (walk
            (form)
            (cond
             ((integerp form) (rebased-int form))
             ((consp form) (cons (walk (car form)) (walk (cdr form))))
             (t form))))
        (let ((max-lisp-eval-depth (max max-lisp-eval-depth 10000)))
          (walk source))))))

(defun nelisp-standalone--chunk-arena-rewrite (source)
  "Doc 140 Stage 8: rewrite fixed-arena-base metadata immediates in SOURCE to
load the runtime chunk-0 base from the driver-owned `nl_arena_base' bss slot,
so NO normal runtime path embeds a fixed arena base.

For every chunked native target (linux-x86_64, windows-x86_64,
macos-aarch64), every integer atom N in [target-base, target-base+span) — the
compact metadata block plus the chunk-0 descriptor — becomes `(+ (ptr-read-u64
(data-addr nl_arena_base) 0) OFF)' where OFF = N - target-base.  Because in
the chunked model that whole range is always arena-base-relative (the chunk-0
bump cursor at +0, control slots, the chunk-0 descriptor), the rewrite is
uniform and unambiguous.

The `nl_arena_init' defun is left untouched: it is the one site that reserves
chunk 0 via a NULL-based OS allocation, seeds `nl_arena_base' from the runtime
return value, and may carry reservation SIZE literals that must never be
confused with a fixed base immediate."
  (if (not (memq nelisp-standalone--target
                 '(linux-x86_64 windows-x86_64 macos-aarch64 linux-aarch64
                   windows-aarch64)))
      source
    (let ((base (nelisp-standalone--target-arena-base))
          (span nelisp-standalone--arena-rebase-span))
      (cl-labels
          ((rewrite-int
            (n)
            (if (and (integerp n) (<= base n) (< n (+ base span)))
                `(+ (ptr-read-u64 (data-addr nl_arena_base) 0) ,(- n base))
              n))
           (walk
            (form)
            (cond
             ;; Leave the base-establishing init untouched (mmap(NULL) call +
             ;; SIZE literal live here; the runtime base var does the writes).
             ((and (consp form) (eq (car form) 'defun)
                   (eq (cadr form) 'nl_arena_init))
              form)
             ((integerp form) (rewrite-int form))
             ((consp form) (cons (walk (car form)) (walk (cdr form))))
             (t form))))
        (let ((max-lisp-eval-depth (max max-lisp-eval-depth 10000)))
          (walk source))))))

(defun nelisp-standalone--arena-base-slot-unit ()
  "Doc 140 Stage 8: a tiny driver-owned bss link unit exporting
`nl_arena_base'.  `nl_arena_init' stores the runtime chunk-0 base in this
8-byte slot; the chunk-arena rewrite makes every former fixed-arena-base
metadata access load the slot (via `data-addr') + offset.  The slot lives in
the binary's own bss (linker-assigned VA, present in the program headers like
any C global), so the ONLY remaining fixed address is driver-owned global
storage — not an arena reservation."
  (nelisp-link-unit-make
   (nelisp-standalone--target-object-name "arena-base.o")
   ;; Doc 152 §11.37 Stage 2: + nl_rootstack_top as
   ;; driver-owned bss globals.  The arena reserved-prefix "free gap" [0xf8,0x2b8)
   ;; turned out NOT writable (only [0,0xf0] is committed); bss globals are the
   ;; correct home for root-stack pointers (like nl_arena_base).
   ;; +16 control + a 1 MiB root-stack REGION as a static bss array.  Using a
   ;; fixed bss array (NOT os_alloc_chunk/mmap) is required: a runtime mmap
   ;; perturbs the arena chunk-growth VA layout and corrupts the freelist on the
   ;; next collect (Doc 152 §11.30-33 class; confirmed: bare os_alloc_chunk +
   ;; garbage-loop -> SIGSEGV in nl_freelist_take).  bss is zero-fill (no file /
   ;; RSS cost until touched).  nl_rootstack_region @ +16 = 32768 32-byte slots.
   ;; Doc 152 §11.39 Stage 3a: nl_gc_diag (64B) after the region = permanent GC
   ;; diagnostic block (+0 trip-count, +8/16/24 first-bad cur/bt/want, +32
   ;; poison-on-free enable, +40 poison-fill count).  Read/toggle via the
   ;; `nelisp--gc-diag' builtin.  Zero-init = disabled = zero behaviour change.
   ;; Doc 152 §11.41 Stage 4a: nl_safepoint_ctx (3648B) after nl_gc_diag =
   ;; depth-indexed safepoint-context stack.  64B control header (depth@+0,
   ;; enable@+8, precise_only@+16, in_progress@+24, alloc_debt@+32,
   ;; alloc_limit@+40) + 64 frames x 56B @ +64 (env/result/out/pool/src/cursor/
   ;; bsym).  Dormant in 4a (no caller); driver publish + collect land in 4b.
   ;; multi-chunk dump: +3728 = nl_fa_tbl_base (8B) -- relocation-table base override
   ;; for `nl_fa_emit'.  0 = use the default dest+span+isz+256 slot (single-chunk);
   ;; non-zero = a caller-chosen valid address (the multi-chunk dump puts the table
   ;; in the intern region's free area, since chunk-0 is full and dest+total+isz is
   ;; past chunk-0's mmap).
   (list (cons 'bss (+ 3736 1048576)))
   (list (nelisp-link-symbol "nl_arena_base" 0
                             :section 'bss :bind 'global :type 'object)
         (nelisp-link-symbol "nl_rootstack_top" 8
                             :section 'bss :bind 'global :type 'object)
         (nelisp-link-symbol "nl_rootstack_region" 16
                             :section 'bss :bind 'global :type 'object)
         (nelisp-link-symbol "nl_gc_diag" (+ 16 1048576)
                             :section 'bss :bind 'global :type 'object)
         (nelisp-link-symbol "nl_safepoint_ctx" (+ 80 1048576)
                             :section 'bss :bind 'global :type 'object)
         (nelisp-link-symbol "nl_fa_tbl_base" (+ 3728 1048576)
                             :section 'bss :bind 'global :type 'object))
   nil))

(defun nelisp-standalone--target-uses-dynamic-arena-base-p (&optional target)
  "Return non-nil when TARGET stores chunk 0's runtime base in `nl_arena_base'."
  (memq (or target nelisp-standalone--target)
        '(linux-x86_64 windows-x86_64 macos-aarch64 linux-aarch64
          windows-aarch64)))

;; ===================================================================
;; my-compile-to-unit — Phase47 source -> in-memory link-unit.
;; reloc :addend normalized to 0 (the linker uses P = va+offset+4; the
;; assembler emits the standard RELA -4 -- normalizing avoids the off-by-4).
;; ===================================================================
(defun nelisp-standalone--arm64-link-unit-text+relocs (buf)
  "Return `(TEXT . RELOCS)' for an arm64 standalone link unit.
Local control-flow fixups are patched in TEXT.  Unresolved branch
fixups are kept as `b26-pc' relocations so separately compiled
standalone units can call each other."
  (let* ((bytes (nelisp-asm-arm64-buffer-bytes buf))
         (labels (nelisp-asm-arm64-buffer-labels buf))
         (fixups (nelisp-asm-arm64-buffer-fixups buf))
         (n (length bytes))
         (vec (make-vector n 0))
         (generated-relocs nil)
         (i 0))
    (while (< i n)
      (aset vec i (aref bytes i))
      (setq i (1+ i)))
    (dolist (fix fixups)
      (let* ((slot (nth 0 fix))
             (label (nth 1 fix))
             (type (or (nth 2 fix) 'b26))
             (cell (assq label labels)))
        (if cell
            (let ((disp (- (cdr cell) slot)))
              (unless (zerop (logand disp #x3))
                (signal 'nelisp-asm-arm64-error
                        (list :branch-misaligned disp :at-slot slot)))
              (pcase type
                ((or 'b26 'bl26)
                 (let ((imm26 (ash disp -2)))
                   (unless (and (>= imm26 (- (ash 1 25)))
                                (< imm26 (ash 1 25)))
                     (signal 'nelisp-asm-arm64-error
                             (list :branch-out-of-range disp :at-slot slot)))
                   (let* ((cur (nelisp-asm-arm64--read-word-le vec slot))
                          (new (logior cur (logand imm26 #x3FFFFFF))))
                     (nelisp-asm-arm64--write-word-le vec slot new))))
                ('b19
                 (let ((imm19 (ash disp -2)))
                   (unless (and (>= imm19 (- (ash 1 18)))
                                (< imm19 (ash 1 18)))
                     (signal 'nelisp-asm-arm64-error
                             (list :bcond-out-of-range disp :at-slot slot)))
                   (let* ((cur (nelisp-asm-arm64--read-word-le vec slot))
                          (field (ash (logand imm19 #x7FFFF) 5))
                          (new (logior cur field)))
                     (nelisp-asm-arm64--write-word-le vec slot new))))
                ('adr21
                 (unless (and (>= disp (- (ash 1 20)))
                              (< disp (ash 1 20)))
                   (signal 'nelisp-asm-arm64-error
                           (list :adr-out-of-range disp :at-slot slot)))
                 (let* ((immlo (logand disp #x3))
                        (immhi (logand (ash disp -2) #x7FFFF))
                        (cur (nelisp-asm-arm64--read-word-le vec slot))
                        (new (logior cur (ash immlo 29) (ash immhi 5))))
                   (nelisp-asm-arm64--write-word-le vec slot new)))
                (other
                 (signal 'nelisp-asm-arm64-error
                         (list :unknown-fixup-type other :at-slot slot)))))
          (pcase type
            ((or 'b26 'bl26)
             (push (list :offset slot
                         :type 'b26-pc
                         :symbol (if (stringp label) label (symbol-name label))
                         :addend 0
                         :section 'text)
                   generated-relocs))
            (other
             (signal 'nelisp-asm-arm64-error
                     (list :unresolved-label label
                           :type other
                           :at-slot slot)))))))
    (cons (apply #'unibyte-string (append vec nil))
          (append (nelisp-asm-arm64-buffer-relocs buf)
                  (nreverse generated-relocs)))))

(defun nelisp-standalone--compile-to-unit (name source &optional abi)
  "Compile Phase47 SOURCE to a link-unit labelled NAME."
  (let* ((name (nelisp-standalone--target-object-name name))
         (source (nelisp-standalone--rebase-arena-source source))
         (source (nelisp-standalone--chunk-arena-rewrite source))
         (resolved-abi (or abi (nelisp-standalone--target-abi)))
         (arch (nelisp-standalone--target-arch))
         (nelisp-aot-compiler--label-counter 0)
         (nelisp-aot-compiler--arch arch)
         (nelisp-aot-compiler--os (nelisp-standalone--target-os))
         (nelisp-aot-compiler--allow-external-user-calls t)
         (nelisp-aot-compiler--abi resolved-abi)
         (ir (nelisp-aot-compiler--parse source nil))
         (defuns (nelisp-aot-compiler--collect-defuns ir))
         (buf (if (eq arch 'aarch64)
                  (nelisp-asm-arm64-make-buffer)
                (nelisp-asm-x86_64-make-buffer resolved-abi))))
    (dolist (d defuns) (nelisp-aot-compiler--emit-defun d buf))
    (let* ((arm64-linked (when (eq arch 'aarch64)
                           (nelisp-standalone--arm64-link-unit-text+relocs buf)))
           (text (if (eq arch 'aarch64)
                     (car arm64-linked)
                   (nelisp-asm-x86_64-resolve-fixups buf)))
           (labels (if (eq arch 'aarch64)
                       (nelisp-asm-arm64-buffer-labels buf)
                     (nelisp-asm-x86_64-buffer-labels buf)))
           (relocs0 (if (eq arch 'aarch64)
                        (cdr arm64-linked)
                      (nelisp-asm-x86_64-extract-relocs buf)))
           (exported (mapcar (lambda (d)
                               (let ((nm (nelisp-aot-compiler--ir-get d :name)))
                                 (if (stringp nm) nm (symbol-name nm))))
                             defuns))
           (symbols (delq nil
                          (mapcar (lambda (cell)
                                    (let ((nm (if (stringp (car cell)) (car cell)
                                                (symbol-name (car cell)))))
                                      (when (member nm exported)
                                        (nelisp-link-symbol nm (cdr cell)
                                                            :section 'text :bind 'global :type 'func))))
                                  labels)))
           (relocs (mapcar (lambda (r)
                             (list :offset (plist-get r :offset)
                                   :type (plist-get r :type)
                                   :symbol (or (plist-get r :symbol)
                                               (plist-get r :sym))
                                   :addend (if (eq arch 'aarch64)
                                               (or (plist-get r :addend) 0)
                                             0)
                                   :section 'text))
                           relocs0)))
      (nelisp-link-unit-make name (list (cons 'text text)) symbols relocs))))

;; ===================================================================
;; Incremental cache.
;; ===================================================================

(defun nelisp-standalone--bytes-to-hex (s)
  "Return an ASCII hex representation of unibyte byte string S."
  (let ((hex (make-string (* 2 (length s)) 0))
        (digits "0123456789abcdef"))
    (dotimes (i (length s))
      (let ((b (aref s i)))
        (aset hex (* i 2) (aref digits (ash b -4)))
        (aset hex (1+ (* i 2)) (aref digits (logand b 15)))))
    hex))

(defun nelisp-standalone--hex-to-bytes (hex)
  "Decode ASCII HEX into a unibyte byte string."
  (let* ((n (/ (length hex) 2))
         (s (make-string n 0)))
    (dotimes (i n)
      (aset s i (string-to-number (substring hex (* i 2) (+ (* i 2) 2)) 16)))
    (encode-coding-string s 'no-conversion t)))

(defun nelisp-standalone--unit-cache-encode (unit)
  "Encode UNIT for stable ASCII cache storage.
Section payloads are raw bytes; keeping them as printed Lisp strings depends on
the host's read/write coding-system behavior.  Store them as hex ASCII so cached
Windows PE units round-trip exactly like freshly compiled in-memory units."
  (let ((copy (copy-sequence unit)))
    (plist-put
     copy :sections
     (mapcar (lambda (section)
               (cons (car section)
                     (list :nelisp-cache-bytes-hex
                           (nelisp-standalone--bytes-to-hex (cdr section)))))
             (plist-get unit :sections)))
    copy))

(defun nelisp-standalone--unit-cache-decode (unit)
  "Decode UNIT read from the standalone cache.
Old cache entries stored raw strings; accept them for compatibility, while new
entries carry explicit hex-encoded byte sections."
  (let ((copy (copy-sequence unit)))
    (plist-put
     copy :sections
     (mapcar (lambda (section)
               (let ((payload (cdr section)))
                 (cons (car section)
                       (if (and (consp payload)
                                (eq (car payload) :nelisp-cache-bytes-hex))
                           (nelisp-standalone--hex-to-bytes (cadr payload))
                         payload))))
             (plist-get unit :sections)))
    copy))

(defun nelisp-standalone--cached-unit (name source source-file)
  "Return the link-unit for NAME, compiling SOURCE only if SOURCE-FILE
or the toolchain is newer than the cached object."
  (let* ((name (nelisp-standalone--target-object-name name))
         (cache-dir (nelisp-standalone--target-cache-dir))
         (cache (expand-file-name (concat name ".unit") cache-dir))
         (deps (cons source-file (nelisp-standalone--dep-files)))
         (fresh (and (file-exists-p cache)
                     (cl-every (lambda (f) (or (null f) (file-newer-than-file-p cache f))) deps))))
    (if fresh
        (let ((coding-system-for-read 'utf-8-unix))
          (with-temp-buffer
            (insert-file-contents cache)
            (goto-char (point-min))
            (nelisp-standalone--unit-cache-decode (read (current-buffer)))))
      (let ((unit (nelisp-standalone--compile-to-unit name source)))
        (make-directory cache-dir t)
        (let ((coding-system-for-write 'utf-8-unix))
          (with-temp-file cache
            (let ((print-escape-nonascii t) (print-length nil) (print-level nil))
              (prin1 (nelisp-standalone--unit-cache-encode unit)
                     (current-buffer)))))
        (push name nelisp-standalone--recompiled)
        unit))))

;; ===================================================================
;; BOTTOM LAYER (pure elisp) — bump arena over a target-specific mmap /
;; VirtualAlloc region at the standalone arena base.  bump-offset@+0 (init 16),
;; QUIT_FLAG slot@+8 (init 0), data from +16.  Supplies nl_alloc_bytes (the
;; target of every `alloc-bytes' op
;; reloc), nl_dealloc_bytes, nl_quit_flag_ptr.
;; ===================================================================
;; M6 catch/throw stash region lives in the reserved arena bytes [16,96):
;;   +8  (268435464): QUIT_FLAG slot       (init 0)
;;   +16 (268435472): THROW flag           (0 = none in flight, 1 = in flight)
;;   +24 (268435480): stashed TAG Sexp     (32 bytes)
;;   +56 (268435512): stashed VAL Sexp     (32 bytes)
;; The bump offset starts at 96 (was 16) so nl_alloc_bytes never hands out the
;; reserved [16,96) bytes.  The extra reservation is harmless to the baked-form
;; eval path (it never reads the stash); it only shifts the first allocation up.
;; --- HEADERED bump arena (GC step 1+2: object headers + free-list). ---
;;
;; Every nl_alloc_bytes block now carries a 16-byte HEADER immediately
;; below the returned object pointer, so the arena is a walkable
;; [hdr][obj][hdr][obj]... sequence (sweep precondition).  The returned
;; pointer is unchanged in spirit: all existing ops get the OBJECT
;; pointer; the header is hidden 16 bytes below and 16-byte aligned.
;;
;; Header (at obj-16):
;;   obj-16 (u64): BLOCK_TOTAL = bytes from this header to the next
;;                 header = 16 + round_up(size, 8).  Drives the sweep
;;                 walker (next_hdr = this_hdr + BLOCK_TOTAL) and is the
;;                 exact-fit key for free-list reuse.
;;   obj-8  (u64): MARK word.  0 = unmarked live, 1 = marked (set by GC
;;                 mark), 2 = FREE (on the free-list).  The sweep clears
;;                 marks back to 0; mark==2 blocks are skipped by mark
;;                 (already dead) and re-found by alloc.
;; Free block payload reuse: a free block stores the free-list NEXT
;; pointer in obj+0 (the object payload, dead while free).
;;
;; All allocations use align 1 or 8 (verified across lisp/ + scripts/),
;; both <= 16, so obj = hdr+16 (16-byte aligned) satisfies every align.
;;
;; Fixed GC arena slots (in the reserved region, bump now starts at +1024):
;;   +96  (268435552): free-list head OBJECT pointer (0 = empty)
;;   +104 (268435560): GC next-trigger bump offset (when bump reaches it,
;;                     the driver runs a collection at the form boundary)
;;   +112 (268435568): arena DATA-START absolute addr (first header) for
;;                     the sweep walker
;;   +120 (268435576): live-bytes-after-last-gc (advisory)
;;   +144 (268435600): initial-stack envp pointer (char**), stashed by the
;;                     driver so the process substrate's execve passes the
;;                     parent environment to children (0 = unavailable)
;;   +216 (268435672): arena reservation size in bytes
;;   +704: chunk-head descriptor pointer
;;   +712: chunk-current descriptor pointer
;;   +720: chunk-count
;;   +728: chunk-bytes-reserved
;;   +736: chunk-bytes-used (telemetry; refreshed by arena stats)
;;   +744: chunk-bytes-reclaimed
;;   +752: chunk-alloc-failures
;;   +768: chunk 0 descriptor:
;;         base, size, cursor, data-start, limit, flags, next
;;
;; The bump allocator still NEVER auto-frees mid-eval; reclamation is the
;; GC sweep at the top-level form boundary (see the reader driver).
(defconst nelisp-standalone--arena-source
  '(seq
    (defun nl_seq2 (_a b) b)
    (defun nl_align_up (n a) (logand (+ n (- a 1)) (- 0 a)))
    ;; BLOCK_TOTAL for a request of SIZE bytes: 16-byte header + object
    ;; padded up to an 8-byte multiple, with a MINIMUM 8-byte payload so the
    ;; free-list `next' link (written at obj+0 on free) always lands inside
    ;; THIS block and never clobbers the next block's header (a size-0 alloc
    ;; would otherwise have obj+0 == the next header).
    ;; Doc 08 §8.18: 8-byte box header.  Single u64 at the block start:
    ;; BLOCK_TOTAL (8-aligned) high bits, GC mark (0 live/1 marked/2 free) in
    ;; the low 3 bits.  Object ptr = hdr + 8 (was hdr + 16).  Segregated
    ;; free-list buckets are re-based to the new minimum BT=16 (range [16,472],
    ;; index BT-16) so small blocks stay bucketed instead of thrashing the
    ;; linear fallback list.
    (defun nl_block_total (size)
      (let ((p (nl_align_up size 8))) (+ 8 (if (< p 8) 8 p))))
    (defun nl_hdr_bt (hdr) (let ((x (ptr-read-u64 hdr 0))) (- x (logand x 7))))
    (defun nl_hdr_mark (hdr) (logand (ptr-read-u64 hdr 0) 7))
    (defun nl_hdr_set_mark (hdr m)
      (let ((x (ptr-read-u64 hdr 0))) (ptr-write-u64 hdr 0 (+ (- x (logand x 7)) m))))
    (defun nl_arena_init () 0)
    (defun nl_os_alloc_chunk (_size) 0)
    (defun nl_os_free_chunk (_base _size) 0)  ; stub; platform source overrides w/ munmap
    (defun nl_os_alloc_fail () 0)
    ;; SIZE-SEGREGATED free-list (2026-06-06).  Replaces the old single
    ;; exact-fit O(freelist) linear scan, which -- once the GC-trigger fix
    ;; made GC actually run and free thousands of mixed-size blocks -- forced
    ;; every variable-length (symbol-name / string) alloc to scan past all the
    ;; fixed-size cons/Sexp blocks, making the vendor load ~10x slower.  Blocks
    ;; are now bucketed by exact BLOCK_TOTAL: head for BT is 268435696+(BT-24)
    ;; for BT in [24,480] (the FREE reserved gap base+240..+696, mmap-zeroed);
    ;; BT<24 (guard for a desynced BT=16 wild index) and BT>480 use the legacy
    ;; single list at 268435552.  Real loads are SOUND + FAST: the full 319
    ;; vendor-load runs deterministically to exit 0 @ ~35s (vs the single
    ;; list's thrash/timeout), peak ~10 GB (bounded; the residue is reachable
    ;; over-retention, a SEPARATE issue -- reuse fixes speed, not memory).
    ;; CAVEAT (Doc 08 §8.13): effective reuse is "less forgiving" of latent
    ;; GC-unsafe code -- an intermediate value held in a NON-root slot while a
    ;; GC fires gets its freed block re-handed-out -> corruption.  This is what
    ;; the `nelisp--arena-stats' + `(list (ptr-read ...))' diagnostic probe
    ;; (r-prog) hit; the real vendor load does not.  It is a pre-existing GC
    ;; root-coverage gap that the single list's near-zero reuse merely masked.
    (defun nl_freelist_scan_drop_tail (prev cur bt want)
      (nl_seq2
       (nl_fl_record_trip cur bt want)
       (if (= prev 0)
           (ptr-write-u64 268435552 0 0)
         (ptr-write-u64 prev 0 0))))
    (defun nl_freelist_scan (prev cur want)
      (if (= cur 0)
          0
        ;; Fallback-list integrity guard.  Bucketed reuse already validates the
        ;; head before dereferencing it; the large-block fallback used to read
        ;; the header unconditionally and crashed when a stale next-link had
        ;; been overwritten by live string bytes.  Drop the corrupt tail and
        ;; continue by bump allocation.
        (if (= (nl_gc_in_arena cur) 0)
            (nl_seq2 (nl_freelist_scan_drop_tail prev cur 0 want) 0)
          (if (= (logand cur 7) 0)
              (if (= (nl_hdr_mark (- cur 8)) 2)
                  (let ((bt (nl_hdr_bt (- cur 8))))
                    (if (= bt want)
                        (nl_seq2
                         (if (= prev 0)
                             (ptr-write-u64 268435552 0 (ptr-read-u64 cur 0))
                           (ptr-write-u64 prev 0 (ptr-read-u64 cur 0)))
                         (nl_seq2 (nl_hdr_set_mark (- cur 8) 0) cur))
                      (nl_freelist_scan cur (ptr-read-u64 cur 0) want)))
                (nl_seq2 (nl_freelist_scan_drop_tail prev cur (nl_hdr_bt (- cur 8)) want) 0))
            (nl_seq2 (nl_freelist_scan_drop_tail prev cur 0 want) 0)))))
    ;; Doc 152 §11.39 Stage 3a: permanent guard-trip counter.  Records into the
    ;; nl_gc_diag bss block whenever the integrity guard drops a corrupt chain
    ;; (= a double-link event).  +0 count, +8/16/24 first-bad cur/bt/want.  Only
    ;; on the rare drop path -> zero common-path cost.  Read via `nelisp--gc-diag'.
    (defun nl_fl_record_trip (cur bt want)
      (seq
        (ptr-write-u64 (data-addr nl_gc_diag) 0
                       (+ (ptr-read-u64 (data-addr nl_gc_diag) 0) 1))
        (if (= (ptr-read-u64 (data-addr nl_gc_diag) 0) 1)
            (seq (ptr-write-u64 (data-addr nl_gc_diag) 8 cur)
                 (ptr-write-u64 (data-addr nl_gc_diag) 16 bt)
                 (ptr-write-u64 (data-addr nl_gc_diag) 24 want))
          0)
        0))
    ;; Pop an exact-fit block for WANT (BLOCK_TOTAL): O(1) bucket pop for
    ;; 24<=WANT<=480, else scan the fallback list.  Clears the FREE sentinel
    ;; (mark 0) and returns the object pointer, or 0 if none.
    (defun nl_freelist_take (want)
      (if (< want 16)
          (nl_freelist_scan 0 (ptr-read-u64 268435552 0) want)
        (if (< 472 want)
            (nl_freelist_scan 0 (ptr-read-u64 268435552 0) want)
          (let* ((head (+ 268435696 (- want 16)))
                 (cur (ptr-read-u64 head 0)))
            (if (= cur 0)
                0
              ;; Doc 152 §11.37 complete free-list integrity guard.  ROOT (traced):
              ;; nl_alloc_symbol writes a Symbol Sexp onto a block still linked in
              ;; bucket[24] (a block double-linked across buckets via inconsistent
              ;; bt), clobbering its next-link (tag byte 4).  Only return cur if it
              ;; is a genuine free block of the right size: in-arena, 8-aligned,
              ;; mark==2 (still FREE), bt==want.  Any failure => the chain is
              ;; corrupt; drop it (clear bucket head) and bump.  Later frees rebuild
              ;; a clean bucket.  Dropping a clobbered/double-linked entry is the
              ;; correct repair (the block is live elsewhere; only the stale link dies).
              (if (= (nl_gc_in_arena cur) 0)
                  (nl_seq2 (nl_fl_record_trip cur 0 want) (nl_seq2 (ptr-write-u64 head 0 0) 0))
                (if (= (logand cur 7) 0)
                    (if (= (nl_hdr_mark (- cur 8)) 2)
                        (if (= (nl_hdr_bt (- cur 8)) want)
                            (nl_seq2 (ptr-write-u64 head 0 (ptr-read-u64 cur 0))
                                     (nl_seq2 (nl_hdr_set_mark (- cur 8) 0) cur))
                          (nl_seq2 (nl_fl_record_trip cur (nl_hdr_bt (- cur 8)) want) (nl_seq2 (ptr-write-u64 head 0 0) 0)))
                      (nl_seq2 (nl_fl_record_trip cur (nl_hdr_bt (- cur 8)) want) (nl_seq2 (ptr-write-u64 head 0 0) 0)))
                  (nl_seq2 (nl_fl_record_trip cur 0 want) (nl_seq2 (ptr-write-u64 head 0 0) 0)))))))))
    ;; Zero NBYTES (step 8) of the reused block's payload at OBJ.
    ;;
    ;; ROOT-CAUSE FIX (reuse correctness).  The tracing mark+sweep is sound:
    ;; full-payload poisoning of every swept block (reuse OFF) is harmless
    ;; across all gates + milestones, so no live box is ever freed.  The
    ;; corruption with reuse ON came purely from HANDING BACK DIRTY MEMORY: a
    ;; reused free block still carries the BYTES of its previous occupant (its
    ;; old refcount, child Sexp slots, type-tag, etc.).  Several box / Sexp
    ;; constructors only write the fields they care about and ASSUME the rest
    ;; of the block is zero — which is true for a bump block (fresh pages from
    ;; the MAP_FIXED zero-page mmap) but NOT for a reused one.  A stale
    ;; refcount makes the rc-aware `cons-set-*' / clone / drop machinery
    ;; mis-count and free-or-alias a still-live box; a stale child Sexp slot
    ;; (uninitialised tail/cdr/slot) makes a partially-built structure point
    ;; at garbage.  Zeroing the whole reused payload here restores the
    ;; bump-block invariant (object starts all-zero), so every constructor
    ;; sees the clean memory it relies on.  Bump blocks need no zeroing (the
    ;; mmap already zero-fills untouched pages).
    (defun nl_alloc_zero_fill (obj off nbytes)
      (if (< off nbytes)
          (nl_seq2 (ptr-write-u64 (+ obj off) 0 0)
                   (nl_alloc_zero_fill obj (+ off 8) nbytes))
        0))
    (defun nl_chunk_cursor_addr (chunk)
      (if (= chunk (ptr-read-u64 268436160 0))
          268435456
        (+ chunk 16)))
    (defun nl_chunk_size_for (want)
      (let ((need (nl_align_up (+ want 1024) 65536)))
        (if (> need 67108864)
            need
          (if (> want 33554432) need 67108864))))
    (defun nl_chunk_init_descriptor (base size)
      (let ((desc (+ base 768)))
        (seq
         (ptr-write-u64 (+ desc 0) 0 base)
         (ptr-write-u64 (+ desc 8) 0 size)
         (ptr-write-u64 (+ desc 16) 0 1024)
         (ptr-write-u64 (+ desc 24) 0 (+ base 1024))
         (ptr-write-u64 (+ desc 32) 0 (+ base size))
         (ptr-write-u64 (+ desc 40) 0 1)
         (ptr-write-u64 (+ desc 48) 0 0)
         desc)))
    (defun nl_chunk_alloc_new (want)
      (let* ((size (nl_chunk_size_for want))
             (base (nl_os_alloc_chunk size)))
        (if (= base 0)
            (nl_seq2 (ptr-write-u64 268436208 0 (+ (ptr-read-u64 268436208 0) 1)) 0)
          (let* ((desc (nl_chunk_init_descriptor base size))
                 (cur (ptr-read-u64 268436168 0)))
            (seq
             (if (= cur 0)
                 (ptr-write-u64 268436160 0 desc)
               (ptr-write-u64 (+ cur 48) 0 desc))
             (ptr-write-u64 268436168 0 desc)
             (ptr-write-u64 268436176 0 (+ (ptr-read-u64 268436176 0) 1))
             (ptr-write-u64 268436184 0 (+ (ptr-read-u64 268436184 0) size))
             desc)))))
    (defun nl_chunk_try_alloc (chunk want)
      (let* ((base (ptr-read-u64 (+ chunk 0) 0))
             (size (ptr-read-u64 (+ chunk 8) 0))
             (cursor_addr (nl_chunk_cursor_addr chunk))
             (obj 0)
             (done 0))
        (seq
         (while (= done 0)
           (let* ((old (ptr-read-u64 cursor_addr 0))
                  (new (+ old want)))
             (if (> new size)
                 (setq done 1)
               (if (= (atomic-compare-exchange cursor_addr old new) 1)
                   (seq
                    (setq obj (+ base (+ old 8)))
                    (ptr-write-u64 (- obj 8) 0 want)  ; BT @ hdr; low bits 0 = mark 0
                    (setq done 1))
                 0))))
         obj)))
    (defun nl_alloc_bytes (size align)
      (let ((want (nl_block_total size)))
        ;; 1) try exact-fit free-list reuse (sweep populates the list).
        ;;    DEBUG: slot 268435624 == 1 disables reuse (always bump).
        (let ((reused (if (= (ptr-read-u64 268435624 0) 1) 0
                        (if (< want (ptr-read-u64 268435656 0)) 0   ; DEBUG: reuse only want>=slot
                          (let ((r (nl_freelist_take want)))
                            (nl_seq2 (if (= r 0) 0 (nl_alloc_zero_fill r 0 (- want 8))) r))))))
          (if (= reused 0)
              ;; 2) bump in current chunk; if full, append a new chunk and
              ;; retry there.  CAS reserves a disjoint [old,new) cursor range
              ;; for concurrent allocators without overshooting the chunk.
              (let* ((cur_chunk (ptr-read-u64 268436168 0))
                     (obj (nl_chunk_try_alloc cur_chunk want)))
                (if (= obj 0)
                    (let* ((new_chunk (nl_chunk_alloc_new want)))
                      (if (= new_chunk 0)
                          (nl_seq2 (nl_os_alloc_fail) 0)
                        (nl_chunk_try_alloc new_chunk want)))
                  obj))
            reused))))
    (defun nl_dealloc_bytes (_p _s _a) 1)
    ;; Doc 146 §3.0: immediates-only tagged-word value helpers (foundation,
    ;; stage 1 -- additive; no caller yet, so the runtime is unchanged).  A
    ;; "value" word is: low bit 0 = 8-aligned slot pointer (heap type, 32-byte
    ;; Sexp slot, unchanged); low bit 1 = immediate.  Encoding:
    ;;   Int(n) = (n<<2)|1   (i62 fixnum, low 2 bits = 01)
    ;;   Nil    = 3          (low 2 bits = 11, payload 0)
    ;;   T      = 7          (low 2 bits = 11, payload 1)
    ;; nl_val_is_imm: nonzero iff V is an immediate (not a slot pointer).
    ;; nl_val_tag:    Sexp tag -- 0=Nil 1=T 2=Int for immediates, else the
    ;;                slot's tag byte at +0.  Nil/T are distinguished by direct
    ;;                compare (Nil=3, T=7) so no shift/div is needed.
    ;; `ash' is a high-level elisp fn (unresolved in arena.o); the arena
    ;; primitives are `shl' (logical left) and `sar' (arithmetic right) -- see
    ;; the AOT `shift' IR (opcode 67).  Int decode = (sar v 2): v=(n<<2)|1, so
    ;; an arithmetic right-shift by 2 recovers n (sign-preserving, validated for
    ;; +/- n).  Encode is n*4+1 (no shift needed).  Nil/T use direct compare.
    (defun nl_val_is_imm (v) (logand v 1))
    (defun nl_val_int (v) (sar v 2))
    (defun nl_imm_int (n) (+ (* n 4) 1))
    (defun nl_imm_nil () 3)
    (defun nl_imm_t () 7)
    (defun nl_val_tag (v)
      (if (= (logand v 1) 0)
          (ptr-read-u8 v 0)
        (if (= (logand v 3) 1)
            2
          (if (= v 3) 0 1))))
    ;; Doc 146 §3.0 step 3/4: store a 32-byte storage Sexp at SLOT into a passing
    ;; value WORD.  Immediate tags collapse to their word (Int -> (n<<2)|1, Nil
    ;; -> 3, T -> 7); heap tags (Float/Symbol/Str/Cons/Vector/...) keep the slot
    ;; pointer.  This is the storage-Sexp -> word boundary, inverse of
    ;; nl_sci_store_imm.  SLOT is always an 8-aligned storage slot.
    ;; Doc 147 Phase 1: RENAMED from `nl_val_load' to `nl_val_store_word' to
    ;; free the `nl_val_load' name for the Phase 0 keystone (.o, arity 2,
    ;; word -> 32B-slot view) now linked via the val-load.o unit.  This local
    ;; producer is the INVERSE direction (storage slot -> word).
    (defun nl_val_store_word (slot)
      (let ((tg (ptr-read-u8 slot 0)))
        (if (= tg 2) (nl_imm_int (ptr-read-u64 slot 8))
          (if (= tg 0) 3
            (if (= tg 1) 7
              slot)))))
    ;; Doc 08 §8.16: symbol-name intern region.  A 64 MiB raw mmap (NOT a
    ;; chunk -> GC never walks it; interned buffers are permanent + invisible
    ;; to mark/sweep).  Control slots: +832 (268436288) = region base (0 =
    ;; disabled), +840 (268436296) = buffer bump.  Layout: [base, base+16MiB)
    ;; = open-addressing table (2^20 * 16 B), [base+16MiB, base+64MiB) = name
    ;; buffer bump arena.  Both slots are in the rebase span so the chunk-arena
    ;; rewrite makes them nl_arena_base-relative; the 64MiB / 16MiB size
    ;; literals are < arena base so they stay literal.  Called from
    ;; `nl_arena_init' AFTER nl_arena_base is seeded.
    (defun nl_intern_region_init ()
      (let ((rbase (syscall-direct 9 0 67108864 3 34 -1 0)))
        (if (< rbase 4096)
            0
          (seq
           (ptr-write-u64 268436288 0 rbase)
           (ptr-write-u64 268436296 0 (+ rbase 16777216))))))
    (defun nl_quit_flag_ptr () 268435464)))

(defconst nelisp-standalone--windows-stack-reserve #x40000000
  "Windows standalone PE stack reserve size.
This matches the Linux standalone trampoline's 1 GiB native stack, but remains a
virtual reservation in the PE header; committed stack pages grow on demand.")

(defun nelisp-standalone--arena-init-metadata-forms (base size)
  "Return bootstrap arena metadata writes for fixed first chunk BASE/SIZE."
  (let* ((data-start nelisp-standalone--arena-data-start-offset)
         (desc (+ base nelisp-standalone--arena-chunk0-desc-offset)))
    `((ptr-write-u64 ,base 0 ,data-start) ; bump starts after control block
      (ptr-write-u64 ,(+ base 8) 0 0)     ; quit flag
      (ptr-write-u64 ,(+ base 16) 0 0)    ; throw flag
      (ptr-write-u64 ,(+ base 96) 0 0)    ; free-list head
      (ptr-write-u64 ,(+ base 104) 0 0)   ; gc trigger
      (ptr-write-u64 ,(+ base 112) 0 ,(+ base data-start)) ; data start
      (ptr-write-u64 ,(+ base 120) 0 0)   ; live bytes
      (ptr-write-u64 ,(+ base 128) 0 0)   ; sweep free dead blocks
      (ptr-write-u64 ,(+ base 136) 0 0)   ; mark phase enabled
      ;; RECLAIMER GATE: base+160 = 1 -> nl_gc_collect is a NO-OP.
      ;; Keep collection disabled until the standalone root/mark gap is
      ;; fixed; pressure is visible via smoke/tests, not hidden by
      ;; increasing the virtual arena reservation.
      (ptr-write-u64 ,(+ base 160) 0 1)   ; collect disabled
      (ptr-write-u64 ,(+ base 168) 0 0)   ; free-list reuse
      (ptr-write-u64 ,(+ base 192) 0 0)   ; probe off
      (ptr-write-u64 ,(+ base 200) 0 0)   ; min reuse block_total
      (ptr-write-u64 ,(+ base 216) 0 ,size) ; reservation size
      (ptr-write-u64 ,(+ base nelisp-standalone--arena-chunk-head-offset)
                     0 ,desc)
      (ptr-write-u64 ,(+ base nelisp-standalone--arena-chunk-current-offset)
                     0 ,desc)
      (ptr-write-u64 ,(+ base nelisp-standalone--arena-chunk-count-offset)
                     0 1)
      (ptr-write-u64 ,(+ base nelisp-standalone--arena-chunk-bytes-reserved-offset)
                     0 ,size)
      (ptr-write-u64 ,(+ base nelisp-standalone--arena-chunk-bytes-used-offset)
                     0 0)
      (ptr-write-u64 ,(+ base nelisp-standalone--arena-chunk-bytes-reclaimed-offset)
                     0 0)
      (ptr-write-u64 ,(+ base nelisp-standalone--arena-chunk-alloc-failures-offset)
                     0 0)
      ;; Doc 140 boundary reclaim DISABLED (was 1): the escape-epoch reset is
      ;; unsound (see the nelisp-standalone--shim-source preamble + Doc 142
      ;; root-cause) -- it recycles per-form regions holding values that escaped
      ;; to globals via function-call returns / refcount-aliasing clones, a
      ;; use-after-free.  The REPL already toggles it off; this makes --load /
      ;; eval / FILE consistent.  Re-enable only with a sound tracing-GC reclaim.
      (ptr-write-u64 ,(+ base nelisp-standalone--arena-boundary-reclaim-enabled-offset)
                     0 0)
      (ptr-write-u64 ,(+ desc nelisp-standalone--arena-chunk-desc-base-offset)
                     0 ,base)
      (ptr-write-u64 ,(+ desc nelisp-standalone--arena-chunk-desc-size-offset)
                     0 ,size)
      (ptr-write-u64 ,(+ desc nelisp-standalone--arena-chunk-desc-cursor-offset)
                     0 ,data-start)
      (ptr-write-u64 ,(+ desc nelisp-standalone--arena-chunk-desc-data-start-offset)
                     0 ,(+ base data-start))
      (ptr-write-u64 ,(+ desc nelisp-standalone--arena-chunk-desc-limit-offset)
                     0 ,(+ base size))
      (ptr-write-u64 ,(+ desc nelisp-standalone--arena-chunk-desc-flags-offset)
                     0 ,(logior 1 nelisp-standalone--arena-chunk-flag-persistent))
      (ptr-write-u64 ,(+ desc nelisp-standalone--arena-chunk-desc-next-offset)
                     0 0))))

(defun nelisp-standalone--arena-init-metadata-forms-dynamic (base-sym size)
  "Like `nelisp-standalone--arena-init-metadata-forms' but BASE-SYM names a
runtime VARIABLE (the chunk-0 base reserved by mmap(NULL) at run time), not a
fixed compile-time address.  Used by the Doc 140 Stage 8 chunk-0 init on
linux/windows/macOS: the arena is no longer mapped at a baked fixed base, so
every metadata slot is seeded relative to the runtime base instead of a baked
immediate.
SIZE is the first-chunk reservation in bytes (a literal — kept inside
`nl_arena_init', which the chunk-arena rewrite skips, so it never collides
with the base literal)."
  (let* ((ds nelisp-standalone--arena-data-start-offset)
         (b base-sym)
         (d0 nelisp-standalone--arena-chunk0-desc-offset))
    `((ptr-write-u64 ,b 0 ,ds)              ; chunk-0 bump cursor (= data-start)
      (ptr-write-u64 (+ ,b 8) 0 0)          ; quit flag
      (ptr-write-u64 (+ ,b 16) 0 0)         ; throw flag
      (ptr-write-u64 (+ ,b 96) 0 0)         ; free-list head
      (ptr-write-u64 (+ ,b 104) 0 0)        ; gc trigger
      (ptr-write-u64 (+ ,b 112) 0 (+ ,b ,ds)) ; data start addr
      (ptr-write-u64 (+ ,b 120) 0 0)        ; live bytes
      (ptr-write-u64 (+ ,b 128) 0 0)        ; sweep free dead blocks
      (ptr-write-u64 (+ ,b 136) 0 0)        ; mark phase enabled
      (ptr-write-u64 (+ ,b 160) 0 0)        ; collect ENABLED (Doc155 §8.12: form-boundary mark-sweep is now SOUND — the mark-4 "pinned vs recursed" decouple in nl_gc_mark_block/nl_gc_conserv_owner/nl_gc_sweep_one closes the lexframe-child root/mark gap; reclamation restored)
      (ptr-write-u64 (+ ,b 168) 0 0)        ; free-list reuse
      (ptr-write-u64 (+ ,b 192) 0 0)        ; probe off
      (ptr-write-u64 (+ ,b 200) 0 0)        ; min reuse block_total
      (ptr-write-u64 (+ ,b 216) 0 ,size)    ; reservation size
      (ptr-write-u64 (+ ,b ,nelisp-standalone--arena-chunk-head-offset) 0 (+ ,b ,d0))
      (ptr-write-u64 (+ ,b ,nelisp-standalone--arena-chunk-current-offset) 0 (+ ,b ,d0))
      (ptr-write-u64 (+ ,b ,nelisp-standalone--arena-chunk-count-offset) 0 1)
      (ptr-write-u64 (+ ,b ,nelisp-standalone--arena-chunk-bytes-reserved-offset) 0 ,size)
      (ptr-write-u64 (+ ,b ,nelisp-standalone--arena-chunk-bytes-used-offset) 0 0)
      (ptr-write-u64 (+ ,b ,nelisp-standalone--arena-chunk-bytes-reclaimed-offset) 0 0)
      (ptr-write-u64 (+ ,b ,nelisp-standalone--arena-chunk-alloc-failures-offset) 0 0)
      ;; Doc 140 boundary reclaim DISABLED (was 1) -- unsound escape-epoch reset
      ;; (use-after-free on function-call return values escaping to globals).
      (ptr-write-u64 (+ ,b ,nelisp-standalone--arena-boundary-reclaim-enabled-offset) 0 0)
      (ptr-write-u64 (+ ,b ,d0) 0 ,b)              ; chunk-0 desc.base
      (ptr-write-u64 (+ ,b ,(+ d0 8)) 0 ,size)     ; desc.size
      (ptr-write-u64 (+ ,b ,(+ d0 16)) 0 ,ds)      ; desc.cursor (= data-start)
      (ptr-write-u64 (+ ,b ,(+ d0 24)) 0 (+ ,b ,ds)) ; desc.data-start
      (ptr-write-u64 (+ ,b ,(+ d0 32)) 0 (+ ,b ,size)) ; desc.limit
      (ptr-write-u64 (+ ,b ,(+ d0 40)) 0
                     ,(logior 1 nelisp-standalone--arena-chunk-flag-persistent)) ; desc.flags (persistent: boot generation)
      (ptr-write-u64 (+ ,b ,(+ d0 48)) 0 0))))     ; desc.next

(defun nelisp-standalone--linux-arena-init-form ()
  "Return the Linux `nl_arena_init' form (Doc 140 Stage 8: fully chunked).

chunk 0 is now reserved by `nl_os_alloc_chunk' (= mmap with addr=NULL) — there
is NO fixed base address and no MAP_FIXED reservation.  The kernel-chosen base
is stored in the driver-owned `nl_arena_base' bss slot; the per-unit
`nelisp-standalone--chunk-arena-rewrite' turns every former
fixed-arena-base metadata immediate into a load of that slot + offset, so the
standalone runtime no longer depends on the historical 0x10000000 mapping.
This is the linux completion of Stage 8: pressure is bounded by chunk growth,
addressing by a runtime base, never by a fixed reservation."
  (let ((size nelisp-standalone--linux-arena-size))
    `(defun nl_arena_init ()
       (let ((base (nl_os_alloc_chunk ,size)))
         (if (= base 0)
             (nl_os_alloc_fail)
           (seq
            (ptr-write-u64 (data-addr nl_arena_base) 0 base)
            ,@(nelisp-standalone--arena-init-metadata-forms-dynamic 'base size)
            (nl_intern_region_init)
            base))))))

(defun nelisp-standalone--linux-alloc-chunk-form ()
  "Return Linux chunk allocation forms using mmap(NULL, ...)."
  '((defun nl_os_alloc_chunk (size)
      (let ((p (syscall-direct 9 0 size 3 34 -1 0)))
        (if (< p 4096) 0 p)))
    (defun nl_os_free_chunk (base size)
      (syscall-direct 11 base size 0 0 0 0))  ; munmap(base, size)
    ;; mmap demand-pages on first touch, so explicit range commit is a no-op.
    (defun nl_os_commit_range (base old new) 1)
    (defun nl_os_alloc_fail ()
      (syscall-direct 60 88 0 0 0 0 0))))

(defun nelisp-standalone--linux-aarch64-alloc-chunk-form ()
  "Return Linux arm64 chunk allocation forms.
Same shape as `nelisp-standalone--linux-alloc-chunk-form' but with the
aarch64 Linux syscall numbers (mmap=222, munmap=215, exit_group=94 —
arm64 Linux has no legacy x86 numbering)."
  '((defun nl_os_alloc_chunk (size)
      (let ((p (syscall-direct 222 0 size 3 34 -1 0)))
        (if (< p 4096) 0 p)))
    (defun nl_os_free_chunk (base size)
      (syscall-direct 215 base size 0 0 0 0))  ; munmap(base, size)
    ;; mmap demand-pages on first touch, so explicit range commit is a no-op.
    (defun nl_os_commit_range (base old new) 1)
    (defun nl_os_alloc_fail ()
      (syscall-direct 94 88 0 0 0 0 0))))

(defun nelisp-standalone--linux-aarch64-intern-region-init-form ()
  "Return arm64 Linux `nl_intern_region_init' (mmap=222)."
  '(defun nl_intern_region_init ()
     (let ((rbase (syscall-direct 222 0 67108864 3 34 -1 0)))
       (if (< rbase 4096)
           0
         (seq
          (ptr-write-u64 268436288 0 rbase)
          (ptr-write-u64 268436296 0 (+ rbase 16777216)))))))

(defun nelisp-standalone--windows-arena-init-form ()
  "Return the Windows `nl_arena_init' form using VirtualAlloc(NULL, ...)."
  `(defun nl_arena_init ()
     (let ((base (nl_os_alloc_chunk ,nelisp-standalone--windows-arena-size)))
       (if (= base 0)
           (extern-call ExitProcess 88)
         (seq
          (ptr-write-u64 (data-addr nl_arena_base) 0 base)
          ,@(nelisp-standalone--arena-init-metadata-forms-dynamic
             'base nelisp-standalone--windows-arena-size)
          (nl_intern_region_init)
          base)))))

(defun nelisp-standalone--windows-intern-region-init-form ()
  "Return Windows `nl_intern_region_init' using VirtualAlloc(NULL, ...)."
  '(defun nl_intern_region_init ()
     (let ((rbase (extern-call VirtualAlloc 0 67108864 12288 4)))
       (if (= rbase 0)
           0
         (seq
          (ptr-write-u64 268436288 0 rbase)
          (ptr-write-u64 268436296 0 (+ rbase 16777216)))))))

(defun nelisp-standalone--windows-alloc-chunk-form ()
  "Return Windows chunk allocation forms using VirtualAlloc(NULL, ...)."
  '((defun nl_os_alloc_chunk (size)
      (let ((base (extern-call VirtualAlloc 0 size 8192 4)))
        (if (= base 0)
            0
          (if (= (extern-call VirtualAlloc base 4096 4096 4) 0)
              (seq (extern-call VirtualFree base 0 32768) 0)
            base))))
    (defun nl_os_free_chunk (base _size)
      (extern-call VirtualFree base 0 32768))  ; MEM_RELEASE=0x8000; size must be 0
    (defun nl_os_commit_range (base old new)
      (if (= (extern-call VirtualAlloc (+ base old) (- new old) 4096 4) 0) 0 1))
    (defun nl_os_alloc_fail ()
      (extern-call ExitProcess 88))))

(defun nelisp-standalone--windows-chunk-try-alloc-form ()
  "Return Windows `nl_chunk_try_alloc' with on-demand page commit."
  '(defun nl_chunk_try_alloc (chunk want)
     (let* ((base (ptr-read-u64 (+ chunk 0) 0))
            (size (ptr-read-u64 (+ chunk 8) 0))
            (cursor_addr (nl_chunk_cursor_addr chunk))
            (obj 0)
            (done 0))
       (seq
        (while (= done 0)
          (let* ((old (ptr-read-u64 cursor_addr 0))
                 (new (+ old want)))
            (if (> new size)
                (setq done 1)
              (if (= (atomic-compare-exchange cursor_addr old new) 1)
                  (if (= (nl_os_commit_range base old new) 0)
                      (setq done 1)
                    (seq
                     (setq obj (+ base (+ old 8)))
                     (ptr-write-u64 (- obj 8) 0 want)
                     (setq done 1)))
                0))))
        obj))))

(defun nelisp-standalone--macos-arena-init-form ()
  "Return the macOS `nl_arena_init' form using Darwin mmap(NULL, ...)."
  `(defun nl_arena_init ()
     (let ((base (nl_os_alloc_chunk ,nelisp-standalone--macos-arena-size)))
       (if (= base 0)
           (syscall-direct 1 88 0 0 0 0 0)
           (seq
            (ptr-write-u64 (data-addr nl_arena_base) 0 base)
            ,@(nelisp-standalone--arena-init-metadata-forms-dynamic
               'base nelisp-standalone--macos-arena-size)
            (nl_intern_region_init)
            base)))))

(defun nelisp-standalone--macos-intern-region-init-form ()
  "Return macOS `nl_intern_region_init' using Darwin mmap(NULL, ...)."
  '(defun nl_intern_region_init ()
     (let ((rbase (syscall-direct 197 0 67108864 3 4098 -1 0)))
       (if (< rbase 4096)
           0
         (seq
          (ptr-write-u64 268436288 0 rbase)
          (ptr-write-u64 268436296 0 (+ rbase 16777216)))))))

(defun nelisp-standalone--macos-alloc-chunk-form ()
  "Return macOS chunk allocation forms using mmap(NULL, ...)."
  '((defun nl_os_alloc_chunk (size)
      (let ((p (syscall-direct 197 0 size 3 4098 -1 0)))
        (if (< p 4096) 0 p)))
    (defun nl_os_free_chunk (base size)
      (syscall-direct 73 base size 0 0 0 0))  ; Darwin munmap(base, size)
    ;; mmap demand-pages on first touch, so explicit range commit is a no-op.
    (defun nl_os_commit_range (base old new) 1)
    (defun nl_os_alloc_fail ()
      (syscall-direct 1 88 0 0 0 0 0))))

(defun nelisp-standalone--target-arena-source ()
  "Return the arena source adjusted for the current standalone target."
  (pcase nelisp-standalone--target
    ((or 'linux-x86_64 'windows-x86_64 'macos-aarch64 'linux-aarch64
         'windows-aarch64)
     (let* ((init-form (pcase nelisp-standalone--target
                        ((or 'linux-x86_64 'linux-aarch64)
                         (nelisp-standalone--linux-arena-init-form))
                        ((or 'windows-x86_64 'windows-aarch64)
                         (nelisp-standalone--windows-arena-init-form))
                        ('macos-aarch64 (nelisp-standalone--macos-arena-init-form))))
           (chunk-forms (pcase nelisp-standalone--target
                          ('linux-x86_64 (nelisp-standalone--linux-alloc-chunk-form))
                          ('linux-aarch64 (nelisp-standalone--linux-aarch64-alloc-chunk-form))
                          ((or 'windows-x86_64 'windows-aarch64)
                           (nelisp-standalone--windows-alloc-chunk-form))
                          ('macos-aarch64 (nelisp-standalone--macos-alloc-chunk-form))))
           (intern-form (pcase nelisp-standalone--target
                          ((or 'windows-x86_64 'windows-aarch64)
                           (nelisp-standalone--windows-intern-region-init-form))
                          ('macos-aarch64 (nelisp-standalone--macos-intern-region-init-form))
                          ('linux-aarch64 (nelisp-standalone--linux-aarch64-intern-region-init-form))
                          (_ nil)))
           (try-alloc-form (pcase nelisp-standalone--target
                             ((or 'windows-x86_64 'windows-aarch64)
                              (nelisp-standalone--windows-chunk-try-alloc-form))
                             (_ nil)))
           ;; nl_os_commit_range is defined by every platform's chunk-forms
           ;; (real VirtualAlloc commit on Windows; no-op on mmap targets) so
           ;; the shared `nl_compact_table_init' can portably force-commit its
           ;; randomly-probed 192MB forwarding table.
           (commit-form (cl-find-if (lambda (chunk-form)
                                      (eq (cadr chunk-form) 'nl_os_commit_range))
                                    chunk-forms))
           (body (mapcar (lambda (form)
                           (if (and (consp form)
                                    (eq (car form) 'defun)
                                    (eq (cadr form) 'nl_arena_init))
                               init-form
                             (if (and try-alloc-form
                                      (consp form)
                                      (eq (car form) 'defun)
                                      (eq (cadr form) 'nl_chunk_try_alloc))
                                 try-alloc-form
                               (if (and intern-form
                                        (consp form)
                                        (eq (car form) 'defun)
                                        (eq (cadr form) 'nl_intern_region_init))
                                   intern-form
                                 (if (and (consp form)
                                          (eq (car form) 'defun)
                                          (memq (cadr form)
                                                '(nl_os_alloc_chunk nl_os_free_chunk
                                                  nl_os_alloc_fail)))
                                     (cl-find-if (lambda (chunk-form)
                                                   (eq (cadr chunk-form) (cadr form)))
                                                 chunk-forms)
                                   form)))))
                         (cdr nelisp-standalone--arena-source))))
       (cons 'seq (if commit-form (append body (list commit-form)) body))))
    (_ nelisp-standalone--arena-source)))

;; ===================================================================
;; TRACING MARK-SWEEP GC (the correct reclaimer — reachability, not
;; escape-enumeration).  Runs ONLY at the top-level form boundary in the
;; reader driver (the safe point: no Sexp pointers are live in mid-eval
;; native frames).  Marks from a precise root set, recursing into the
;; SAME per-type child layout the clone helpers encode, then sweeps the
;; headered arena, pushing unmarked blocks onto the free-list.
;;
;; Soundness:
;;  * Reachability handles refcount-ALIASING clones, in-place macro
;;    caching, and mirror/closure sharing automatically (a shared box is
;;    marked once via whichever root reaches it; the mark bit dedups).
;;  * `nl_gc_in_arena' bounds-checks every box pointer before touching its
;;    header, so a foreign / reserved-region / mis-tagged pointer is
;;    SKIPPED (never corrupts random memory).  In-arena live boxes are
;;    always reached because every box type's exact child layout is
;;    walked below.
;;  * cdr-chains are walked ITERATIVELY (loop on cdr, recurse only car),
;;    so list length does not bound native mark recursion depth.
;;
;; Box layouts (from the clone/alloc kernels, byte-exact):
;;   Cons   (tag 7): box+0 car Sexp, box+32 cdr Sexp, box+64 rc.   [data: box]
;;   Vector (tag 8): box+0 cap, box+8 data_ptr, box+16 len, box+24 rc.
;;                   data_ptr -> separate cap*32 buffer of `len' Sexps.
;;   Record (tag 12): box+0 type_tag Sexp, box+32 slots-Vec
;;                   (cap@+32,data_ptr@+40,len@+48), box+56 rc.
;;   Cell   (tag 11): value WORD @ box+0, rc @ box+8, size 16 (Doc 147 P1).
;;   Str/Symbol (tag 5/4): INLINE String in the Sexp (cap@sp+8, ptr@sp+16,
;;                   len@sp+24); ptr -> separate char buffer (no Sexp kids).
;;   MutStr (tag 6): sp+8 NlStr* box (cap@+0,ptr@+8,len@+16); ptr -> buf.
;; CharTable(9)/BoolVector(10) do not occur in the reader graph (bool-vector
;; is a plain Vector in the stdlib); a box of those tags is marked but its
;; children are not walked — documented limitation, gated by the test suite.
(defconst nelisp-standalone--gc-source
  '(seq
    ;; Chunk-aware membership.  During Doc 140 Stage 3 only chunk 0 is active,
    ;; but the walk already follows the descriptor list introduced in Stage 2.
    ;; Membership is restricted to [data-start, base+cursor) so fixed control
    ;; metadata below data-start is never treated as an object header.
    (defun nl_gc_chunk_cursor (chunk)
      (if (= chunk (ptr-read-u64 268436160 0))
          (ptr-read-u64 268435456 0)
        (ptr-read-u64 (+ chunk 16) 0)))
    (defun nl_gc_chunk_end (chunk)
      (+ (ptr-read-u64 chunk 0) (nl_gc_chunk_cursor chunk)))
    (defun nl_gc_chunk_contains (chunk addr)
      (if (= chunk 0)
          0
        (if (< addr (ptr-read-u64 (+ chunk 24) 0))
            0
          (if (< addr (nl_gc_chunk_end chunk)) 1 0))))
    (defun nl_gc_chunk_contains_any (chunk addr)
      (if (= chunk 0)
          0
        (if (= (nl_gc_chunk_contains chunk addr) 1)
            1
          (nl_gc_chunk_contains_any (ptr-read-u64 (+ chunk 48) 0) addr))))
    (defun nl_gc_in_arena (addr)
      (nl_gc_chunk_contains_any (ptr-read-u64 268436160 0) addr))
    ;; BOOT-GENERATION predicate.  The boot image is the prefix of CHUNK 0
    ;; [chunk0_base, watermark).  The old test was a bare `(< addr watermark)'
    ;; scalar compare, which is UNSOUND once growth chunks exist: the OS can
    ;; (and does) place a growth chunk at a VA *numerically below* chunk 0's
    ;; base, so a runtime object in that growth chunk reads as "below the
    ;; watermark" and is mis-classified as a permanent boot block.  In moving
    ;; GC that meant such a block was NOT forwarded / NOT moved (fwd returned
    ;; identity, phase-4 left it in place) yet its growth chunk WAS munmap'd in
    ;; phase 6 -> any root edge still pointing at it (e.g. the shared symentry
    ;; @268436328) dangles -> SIGSEGV.  Correct test: the block must be both
    ;; AT-OR-ABOVE chunk 0's base AND below the watermark.  chunk0_base =
    ;; *(*268436160 + 0) (chunk-head desc, base field @ +0).
    (defun nl_gc_is_boot (addr)
      (if (< addr (ptr-read-u64 (ptr-read-u64 268436160 0) 0)) 0
        (if (< addr (ptr-read-u64 268435664 0)) 1 0)))
    ;; Mark a block by OBJECT pointer.  Returns 1 if newly marked (caller
    ;; should recurse into children), 0 if foreign / already marked / free.
    (defun nl_gc_mark_block (obj)
      ;; Doc155 §8.12 sound-GC: recurse on mark 0 (unmarked) OR mark 4
      ;; (conserv-PINNED-not-recursed).  A pinned block was kept alive by the
      ;; conservative scan but never had its children recursed (the scan
      ;; misreads a record/vector BOX interior pointer's type_tag as a Sexp
      ;; tag); the precise marker must still recurse it, so treat 4 like 0 here
      ;; (upgrade 4->1, return 1 = "recurse").  mark 1/2/3/5 -> already
      ;; recursed/free -> skip (return 0).  This DECOUPLES "alive" (pinned) from
      ;; "recursed" and closes the lexframe-child collection bug (Doc 155).
      (if (= (nl_gc_in_arena obj) 0) 0
        (let ((m (nl_hdr_mark (- obj 8))))
          (if (if (= m 0) 1 (if (= m 4) 1 0))
              (nl_seq2 (nl_hdr_set_mark (- obj 8) 1) 1)
            0))))
    ;; Mark the char buffer of a string (raw byte block, no Sexp children).
    (defun nl_gc_mark_buf (ptr) (nl_seq2 (nl_gc_mark_block ptr) 0))
    ;; Mark every slot of a `len'-element buffer starting at data_ptr.
    ;; Doc 147 Phase 2: each slot is now an 8-byte tagged WORD (stride 8,
    ;; was a 32B inline Sexp).  Per-slot WORD-aware mark, mirroring the
    ;; Phase-1 tag-11 cell value-word handling: read the 8B WORD; an
    ;; immediate (low bit 1) has no child -> skip; else the WORD is an
    ;; 8-aligned pointer to a 32B child box -> mark the box block then
    ;; recurse into it as a 32B Sexp SLOT (nl_gc_mark_slot reads
    ;; tag@+0/payload@+8 from the child box).  Shared by tag-8 Vectors
    ;; and tag-12 Records (whose slots buffers both shrank).
    ;; DEFENSIVE in-arena guard on DATA_PTR: the reader parse pool is a
    ;; RAW cap*32 buffer (Group P) whose UNUSED slots are uninitialised
    ;; garbage; the pool walk reads every slot as a live Sexp, so a
    ;; garbage slot can present tag-8/12 with a garbage NlVector/NlRecord
    ;; box whose `data_ptr' field is junk.  In the 32B era each slot was
    ;; re-validated by `nl_gc_mark_slot' (in_arena-guarded); the 8B walker
    ;; reads the WORD directly (`*(data_ptr + k*8)'), so a junk DATA_PTR
    ;; faults BEFORE any per-slot guard.  Skip the whole buffer when
    ;; DATA_PTR is not in-arena (a real vector/record always has an
    ;; in-arena alloc'd buffer; len==0 -> data_ptr may be null -> skip).
    (defun nl_gc_mark_vec_slots (data_ptr i len)
      (if (= (nl_gc_in_arena data_ptr) 0) 0
        (let ((k i))
          (while (< k len)
            (nl_seq2
             (let ((vw (ptr-read-u64 (+ data_ptr (* k 8)) 0)))
               (if (= (logand vw 1) 1) 0
                 (if (= (nl_gc_mark_block vw) 0) 0
                   (nl_gc_mark_slot vw))))
             (setq k (+ k 1))))
          0)))
    ;; Doc 147 Phase 1.5 Group P — RAW reader parse-pool GC arms.  The pool
    ;; is no longer a GC-managed Sexp::Vector; it is a flat cap*32B buffer
    ;; whose `cap' is stashed @268436448 (`nl_gc_pool_cap').  Mark = mark the
    ;; buffer block itself, then walk all `cap' 32B slots as live Sexps.  The
    ;; pool holds LIVE transient Sexps across a mid-parse collection, so every
    ;; slot must be a root.  Buffer stays 32B in Phase 1.5, so per-slot walk
    ;; reads FULL 32B Sexps (no immediate-skip — that arrives with Phase 2).
    (defun nl_gc_pool_cap () (ptr-read-u64 268436448 0))
    (defun nl_gc_mark_pool_slots (base i cap)
      (let ((k i))
        (while (< k cap)
          (nl_seq2 (nl_gc_mark_slot (+ base (* k 32))) (setq k (+ k 1))))
        0))
    (defun nl_gc_mark_pool (base cap)
      (if (= base 0) 0
        (nl_seq2 (nl_gc_mark_block base)
                 (nl_gc_mark_pool_slots base 0 cap))))
    ;; Cons cdr-spine walker (tail-recursive: recurse only on car, loop
    ;; on cdr) so list LENGTH does not bound native mark depth.  SP points
    ;; at a Sexp slot known to be tag 7 (Cons).
    ;;
    ;; Doc 147 Phase 3: the NlConsBox car / cdr are now 8-byte tagged
    ;; WORDS (car @ box+0, cdr @ box+8; was 32B inline Sexps @ box+0 /
    ;; box+32).  Per-slot WORD-aware mark, mirroring the Phase-1 tag-11
    ;; cell + Phase-2 vec-slot arms: read the 8B WORD; an immediate (low
    ;; bit 1) has no child -> skip; else the WORD is an 8-aligned pointer
    ;; to a 32B child Sexp box -> mark the box block, then recurse.  For
    ;; the CDR, gate the tail-loop on the child box's tag: a Cons child
    ;; (tag 7) tail-loops `nl_gc_mark_cons' on the child box (= a valid
    ;; Sexp slot, payload @+8 = the next NlConsBox); a non-Cons child is
    ;; a leaf walked via `nl_gc_mark_slot'.
    (defun nl_gc_mark_cons (sp)
      (let ((box (ptr-read-u64 sp 8)))
        (if (= (nl_gc_mark_block box) 0)
            0                                      ; foreign / already marked
          (nl_seq2
           ;; car WORD @ box+0
           (let ((cw (ptr-read-u64 box 0)))
             (if (= (logand cw 1) 1) 0             ; immediate -> no child
               (if (= (nl_gc_mark_block cw) 0) 0   ; foreign / already marked
                 (nl_gc_mark_slot cw))))           ; mark the 32B child box
           ;; cdr WORD @ box+8
           (let ((dw (ptr-read-u64 box 8)))
             (if (= (logand dw 1) 1) 0             ; immediate -> end of list
               (if (= (nl_gc_mark_block dw) 0) 0   ; foreign / already marked
                 (if (= (ptr-read-u8 dw 0) 7)
                     (nl_gc_mark_cons dw)          ; cdr child is a cons -> tail loop
                   (nl_gc_mark_slot dw)))))))))     ; cdr child atom/other
    ;; Mark one Sexp slot at SP (32 bytes).  Pure recursion per type.
    (defun nl_gc_mark_slot (sp)
      (let ((tag (ptr-read-u8 sp 0)))
        (if (= tag 7)
            (nl_gc_mark_cons sp)
          (if (= tag 8)
              ;; Vector: mark box + data buffer, recurse slots.
              (let ((box (ptr-read-u64 sp 8)))
                (if (= (nl_gc_mark_block box) 0) 0
                  (let ((data_ptr (ptr-read-u64 box 8)) (len (ptr-read-u64 box 16)))
                    (seq (nl_gc_mark_buf data_ptr)
                         (nl_gc_mark_vec_slots data_ptr 0 len)))))
            (if (= tag 12)
                ;; Record: type_tag@box+0, slots-Vec@box+32 (data@+40,len@+48).
                (let ((box (ptr-read-u64 sp 8)))
                  (if (= (nl_gc_mark_block box) 0) 0
                    (let ((data_ptr (ptr-read-u64 box 40)) (len (ptr-read-u64 box 48)))
                      (seq (nl_gc_mark_slot box)            ; type_tag @ box+0
                           (nl_gc_mark_buf data_ptr)
                           (nl_gc_mark_vec_slots data_ptr 0 len)))))
              (if (= tag 11)
                  ;; Cell (Doc 147 P1): value WORD @ box+0 (8B tagged),
                  ;; rc @ box+8, size 16.  Mark the NlCell box, then the
                  ;; value WORD: immediate (low bit 1) -> no child; else
                  ;; mark/recurse the 32B child box it points at (mirrors
                  ;; the Vector/Record pointer-edge handling).
                  (let ((box (ptr-read-u64 sp 8)))
                    (if (= (nl_gc_mark_block box) 0) 0
                      (let ((vw (ptr-read-u64 box 0)))
                        (if (= (logand vw 1) 1) 0
                          (if (= (nl_gc_mark_block vw) 0) 0
                            (nl_gc_mark_slot vw))))))
                (if (= tag 6)
                    ;; MutStr: NlStr*@sp+8 (ptr@box+8).
                    (let ((box (ptr-read-u64 sp 8)))
                      (if (= (nl_gc_mark_block box) 0) 0
                        (nl_gc_mark_buf (ptr-read-u64 box 8))))
                  (if (= tag 5) (nl_gc_mark_buf (ptr-read-u64 sp 16))   ; Str
                    (if (= tag 4) (nl_gc_mark_buf (ptr-read-u64 sp 16)) ; Symbol
                      ;; tag 9/10 boxed-no-walk (do not occur); else inline atom.
                      (if (= tag 9) (nl_seq2 (nl_gc_mark_block (ptr-read-u64 sp 8)) 0)
                        (if (= tag 10) (nl_seq2 (nl_gc_mark_block (ptr-read-u64 sp 8)) 0)
                          0)))))))))))
    ;; Free one dead block (header at HDR): set FREE sentinel, link the
    ;; object (hdr+16) onto the free-list head.  Returns 0.  Isolated into
    ;; a helper so the sweep loop body has no nested let + outer setq.
    ;; Doc 152 §11.39 Stage 3a: poison-on-free (debug, gated by nl_gc_diag+32).
    ;; Fill the freed payload BEYOND the next-link (hdr+16 .. hdr+bt) with a
    ;; non-canonical sentinel so any use-after-free read via a stale (unrooted)
    ;; pointer faults immediately -- pinpoints a missed root during Stage 3b+.
    ;; Header (hdr+0) and freelist next-link (hdr+8) are preserved.
    (defun nl_gc_poison_fill (hdr off bt)
      (if (< off bt)
          (nl_seq2 (ptr-write-u64 (+ hdr off) 0 16045481047390945280)
                   (nl_gc_poison_fill hdr (+ off 8) bt))
        0))
    (defun nl_gc_free_block_link (hdr head)
      (nl_seq2 (nl_hdr_set_mark hdr 2)
       (nl_seq2 (ptr-write-u64 (+ hdr 8) 0 (ptr-read-u64 head 0))
        (nl_seq2 (ptr-write-u64 head 0 (+ hdr 8))
         (if (= (ptr-read-u64 (data-addr nl_gc_diag) 32) 1)
             (nl_seq2 (ptr-write-u64 (data-addr nl_gc_diag) 40
                        (+ (ptr-read-u64 (data-addr nl_gc_diag) 40) 1))
                      (nl_gc_poison_fill hdr 16 (nl_hdr_bt hdr)))
           0)))))
    (defun nl_gc_free_block (hdr)
      (if (= (nl_gc_is_boot hdr) 1) 0   ; HARD: never free a chunk-0 boot block
       (nl_gc_free_block_link hdr
        (if (< (nl_hdr_bt hdr) 16) 268435552
          (if (< 472 (nl_hdr_bt hdr)) 268435552
            (+ 268435696 (- (nl_hdr_bt hdr) 16)))))))
    ;; Process one block at HDR (mark==1 clear / mark==0 free / mark==2
    ;; skip); returns the block's live byte contribution (bt if live, else
    ;; 0).  No control mutation -> safe to call from the iterative loop.
    ;;
    ;; BOOT WATERMARK (268435664): the boot image -- the mirror, all builtins,
    ;; the env/frame records, the source string, the slot pool and the fixed
    ;; driver scratch slots, everything allocated below the watermark at
    ;; install + driver setup -- is live for the WHOLE program.  It IS reached
    ;; by the precise root marker (via the mirror/frames roots), so the
    ;; reachability sweep alone would already preserve it; this guard is
    ;; belt-and-suspenders: a block whose header is BELOW the watermark is
    ;; ALWAYS kept live (clear any mark back to 0 so the mark word is reset for
    ;; the next cycle, keeping re-marking of its above-watermark children
    ;; idempotent) and never freed -- a sound "permanent generation" that
    ;; guarantees no boot-internal raw-pointer edge can ever be reclaimed.
    ;; Only per-form garbage ABOVE the watermark is collected.  (NOTE: the
    ;; reuse-correctness fix is the payload zeroing in `nl_alloc_zero_fill';
    ;; this watermark guard is not required for the gates to pass but makes the
    ;; permanent generation explicit and robust.)
    (defun nl_gc_sweep_one (hdr)
      (if (= (nl_gc_is_boot hdr) 1)
          (nl_seq2 (nl_hdr_set_mark hdr 0)                ; boot block: keep live, reset mark
                   (nl_hdr_bt hdr))
      (let ((m (nl_hdr_mark hdr)) (bt (nl_hdr_bt hdr)))
        ;; Doc155 §8.12: m==1 (recursed-live) OR m==4 (conserv-PINNED, a live
        ;; in-flight root never precisely recursed) both SURVIVE and reset to 0
        ;; for the next cycle.
        (if (if (= m 1) 1 (if (= m 4) 1 0))
            (nl_seq2 (nl_hdr_set_mark hdr 0)              ; survive: clear mark
             (nl_seq2 (ptr-write-u64 268435640 0 (+ (ptr-read-u64 268435640 0) 1)) bt))
          (if (= m 0)
              (if (= (ptr-read-u64 268435584 0) 1)
                  bt                                       ; DEBUG mark-only
                (nl_seq2 (ptr-write-u64 268435632 0 (+ (ptr-read-u64 268435632 0) 1))
                         (nl_seq2 (nl_gc_free_block hdr) 0))) ; dead -> free
            0)))))                                         ; m==2 already free
    ;; Sweep: ITERATIVE header walk (chunks can hold millions of blocks, so
    ;; recursion would overflow the stack).  `hdr'/`live' are mutated only
    ;; at the loop's own scope; all per-block work is in `nl_gc_sweep_one'.
    ;; A block_total BT at header HDR is well-formed iff >=16, 8-aligned,
    ;; and does not overshoot END.  Catches any walk desync (a desynced
    ;; read lands mid-object and almost always fails one of these).
    (defun nl_gc_bt_ok (hdr bt end)
      (if (< bt 16) 0
        (if (= (logand bt 7) 0)
            (if (< end (+ hdr bt)) 0 1)
          0)))
    ;; Step over one block: process it, return the NEXT header address
    ;; (hdr + block_total), or 0 to signal "stop" (desync / past end).  All
    ;; reads are within the block; no control state escapes — the loop's
    ;; setqs stay at its own scope (avoids the AOT nested-let+outer-setq
    ;; pitfall that silently drops the mutation).
    (defun nl_gc_sweep_step (hdr end)
      (if (= (nl_gc_bt_ok hdr (nl_hdr_bt hdr) end) 0)
          0
        (nl_seq2 (nl_gc_sweep_one hdr) (+ hdr (nl_hdr_bt hdr)))))
    (defun nl_gc_sweep_chunk (chunk)
      (let ((hdr (ptr-read-u64 (+ chunk 24) 0))
            (end (nl_gc_chunk_end chunk)))
        (while (and (> hdr 0) (< hdr end))
          (setq hdr (nl_gc_sweep_step hdr end)))
        0))
    (defun nl_gc_sweep_chunks (chunk)
      (if (= chunk 0)
          0
        (nl_seq2 (nl_gc_sweep_chunk chunk)
                 (nl_gc_sweep_chunks (ptr-read-u64 (+ chunk 48) 0)))))
    (defun nl_gc_sweep ()
      (nl_gc_sweep_chunks (ptr-read-u64 268436160 0)))
    ;; Full collection at the form boundary.  CTX = the env (mirror@+0,
    ;; frames@+32, unbound@+64).  The remaining args are the live driver
    ;; Sexp slots that must survive.  Mark all roots, then sweep.
    ;; Doc 152 §11.21 / Doc 146 §2: CONSERVATIVE native-stack scan that closes
    ;; the root-coverage gap.  The eval machinery holds in-flight Sexp obj
    ;; pointers in C-stack frames / arena scratch that the build-glue cannot
    ;; enumerate as precise roots; when a collection fires at a (nested-load)
    ;; form boundary those targets get freed (mark+sweep) or moved+munmap'd
    ;; (compaction) -> dangling -> SIGSEGV (Doc 152 §11.18-20).  Scan the live
    ;; native stack [rsp, STACK_TOP) for words that look like valid arena obj
    ;; pointers and mark+recurse them (keep-alive).  ADDITIVE: only ever marks
    ;; MORE, never frees more, so it is SOUND for mark+sweep (false positives =
    ;; bounded over-retention; nl_gc_mark_slot is in-arena-guarded + idempotent).
    ;; Gated by SCAN_FLAG @268436464; STACK_TOP @268436456 = driver-entry rsp
    ;; (captured via (aot-current-sp)).  Moving GC would additionally need the
    ;; conservatively-found blocks PINNED (not done) -> use with compaction OFF.
    ;; Doc 152 §11.27: a conservatively-found Sexp-slot pointer W on the C-stack
    ;; may itself BE the object-start (block+8) of its own arena SCRATCH block.
    ;; The eval machinery holds in-flight Sexp values in `(alloc-bytes 32 8)'
    ;; scratch slots (func_slot / out_slot / arg-construction / materialising-
    ;; accessor scratch — the Doc 146 §2 "escape boxes").  The base scan marks
    ;; W's CHILDREN (nl_gc_mark_slot reads the box @ W+8) but NOT the block that
    ;; CONTAINS the slot, so that still-live scratch block is swept + freelisted
    ;; and its reuse corrupts the freelist (crash in nl_freelist_take, §11.26).
    ;; Mark the owning block too — but ONLY when W-8 is a real block header, so
    ;; an INTERIOR / inline slot pointer (e.g. `(+ env 32)', or a stack word
    ;; that merely LOOKS like a Sexp obj because its low byte is < 13) never
    ;; gets a stray mark bit written into the middle of a live block — which
    ;; would corrupt e.g. a small tagged int (a syscall NR), Doc 152 §11.27.
    ;; Header validation (all cheap, no deref of foreign memory):
    ;;   (a) W-8 is in-arena and NOT a boot block (boot blocks are never freed,
    ;;       so they need no mark; writing into one would clobber interior boot
    ;;       data such as the env mirror/frames);
    ;;   (b) BT = nl_hdr_bt(W-8) is 8-aligned in [16, 16 MiB] and the block end
    ;;       is in-arena (it fits);
    ;;   (c) TWO-LEVEL: the NEXT block at W-8+BT is either past the live arena
    ;;       data (chunk end) OR itself a plausible header (8-aligned BT2 in
    ;;       range).  A random interior word whose value happens to mask to a
    ;;       plausible BT almost never has a second plausible header exactly BT
    ;;       bytes later, so this rejects the false positives that (b) alone
    ;;       lets through.
    ;; ADDITIVE keep-alive: sound for mark+sweep (only ever retains more).
    (defun nl_gc_conserv_owner (w)
      (let ((hdr (- w 8)))
        (if (= (nl_gc_in_arena hdr) 0) 0
          (if (= (nl_gc_is_boot hdr) 1) 0
            (let ((bt (nl_hdr_bt hdr)))
              (if (< bt 16) 0
                (if (< 16777216 bt) 0
                  (if (= (nl_gc_in_arena (+ hdr (- bt 1))) 0) 0
                    (let ((next (+ hdr bt)))
                      (if (= (nl_gc_in_arena next) 0)
                          (if (= (nl_hdr_mark hdr) 0) (nl_seq2 (nl_hdr_set_mark hdr 4) 1) 0)   ; Doc155 §8.12: PIN (mark 4) — keep alive; precise marker recurses it
                        (let ((bt2 (nl_hdr_bt next)))
                          (if (< bt2 16) 0
                            (if (< 16777216 bt2) 0
                              (if (= (nl_hdr_mark hdr) 0) (nl_seq2 (nl_hdr_set_mark hdr 4) 1) 0))))))))))))))
    (defun nl_gc_conserv_word (w)
      (if (= (logand w 7) 0)              ; obj ptrs are 8-aligned
          (if (= (nl_gc_in_arena w) 1)    ; within live arena data (no deref of w)
              (if (< (ptr-read-u8 w 0) 13) ; plausible Sexp tag 0..12
                  (nl_seq2
                   (nl_gc_conserv_owner w) ; §11.27: keep the slot's OWN block alive
                   (nl_gc_mark_slot w))    ; mark + recurse children (idempotent, guarded)
                0)
            0)
        0))
    (defun nl_gc_conserv_scan (p0 top)
      (let ((p p0))
        (while (< p top)
          (nl_seq2 (nl_gc_conserv_word (ptr-read-u64 p 0))
                   (setq p (+ p 8))))
        0))
    (defun nl_gc_conserv_maybe ()
      (if (= (ptr-read-u64 268436464 0) 1)        ; SCAN_FLAG (0 = off)
          (let ((top (ptr-read-u64 268436456 0))) ; STACK_TOP = driver-entry rsp
            (if (= top 0) 0
              (nl_gc_conserv_scan (aot-current-sp) top)))
        0))
    ;; Mark the ROOT BLOCKS themselves (the driver's fixed `alloc-bytes'
    ;; scratch: ctx/result/out/pool/src/cursor/bsym).  These ARE live arena
    ;; allocations holding the root Sexps; without marking the block, sweep
    ;; would free it and `nl_gc_free_block' would clobber its payload (= the
    ;; root Sexp).  ctx is one block (mirror@+0/frames@+32/unbound@+64 all
    ;; inside it), so marking ctx covers all three env roots.
    (defun nl_gc_mark_root_blocks (ctx result out pool src cursor bsym)
      (nl_seq2 (nl_gc_mark_block ctx)
       (nl_seq2 (nl_gc_mark_block result)
        (nl_seq2 (nl_gc_mark_block out)
         (nl_seq2 (nl_gc_mark_block pool)
          (nl_seq2 (nl_gc_mark_block src)
           (nl_seq2 (nl_gc_mark_block cursor)
                    (nl_gc_mark_block bsym))))))))
    ;; Mark every root (split out so the DEBUG skip-mark gate is a single if).
    (defun nl_gc_mark_roots (ctx result out pool src cursor bsym)
      (nl_seq2 (nl_gc_conserv_maybe)             ; Doc 152 §11.21 conservative stack scan
       (nl_seq2 (nl_gc_mark_root_blocks ctx result out pool src cursor bsym)
       (nl_seq2 (nl_seq2 (nl_gc_mark_slot (+ ctx 0)) (nl_gc_mark_rootstack)) ; mirror / globals + Doc 152 §11.37 Stage 2 dynamic root stack scan
        (nl_seq2 (nl_gc_mark_slot (+ ctx 32))    ; frame stack
         (nl_seq2 (nl_gc_mark_slot (+ ctx 64))   ; unbound marker
          (nl_seq2 (nl_gc_mark_slot result)      ; current parsed form
           (nl_seq2 (nl_gc_mark_slot out)        ; in-flight / last result
            (nl_seq2 (nl_gc_mark_pool pool (nl_gc_pool_cap)) ; Doc 147 P1.5 — RAW parse-pool buffer (cap slots @ pool+N*32)
             (nl_seq2 (nl_gc_mark_slot src)      ; source string
              (nl_seq2 (nl_gc_mark_slot cursor)  ; reader cursor Sexp
                       (nl_seq2 (nl_gc_mark_slot bsym)
                                ;; Doc 146: shared symentry symbol (268436328) is
                                ;; a standalone Symbol Sexp block referenced only by
                                ;; this slot.  Mark the BLOCK (so the 32B slot box
                                ;; survives) AND walk it as a Symbol SLOT (so its
                                ;; name buffer @ +16 is marked too) — the name buf
                                ;; lives in a growth chunk, not the interned region,
                                ;; so a bare block-mark left it unmarked -> munmap'd
                                ;; -> mirror name-compare dangles (SIGSEGV).
                                (if (= (ptr-read-u64 268436328 0) 0) 0
                                  (nl_seq2
                                   (nl_gc_mark_block (ptr-read-u64 268436328 0))
                                   (nl_gc_mark_slot (ptr-read-u64 268436328 0)))))))))))))))) ; bsym + shared symentry + conserv wrap
    ;; ===== Doc 146 §5 moving GC (compaction). Phase 2 = forwarding. =====
    ;; Behind flag 268435608 (1=ON by default, wired at boot init; 0 = mark+sweep
    ;; escape hatch).  Forwarding hash side-table base
    ;; Compaction cursor @ 268436368.  Phase 2 is NON-DESTRUCTIVE — it only
    ;; populates fwd[] (the move is phase 4) so it is safe to enable for
    ;; validation: with the flag on, mark -> phase2(compute) -> normal sweep.
    ;; (Slots 268436360/368/376 are clear of the TAG/VAL signal stashes that
    ;; occupy 268435480..268435544 — using those crashed under throw.)
    ;; Forwarding hash table: 2^23 slots x 24B (old@+0, new@+8, gen@+16) @ base
    ;; ptr 268436360 (192MB mmap, once).  A generation counter @ 268436376 (bumped
    ;; each compaction) avoids re-zeroing: a slot is live only when gen==current,
    ;; so stale entries from prior GCs read as empty.  No direct syscall (mmap is
    ;; in the platform source; this defconst only links nl_os_alloc_chunk).
    (defun nl_compact_table_init ()
      (if (= (ptr-read-u64 268436360 0) 0)
          (let ((p (nl_os_alloc_chunk 25165824)))
            (if (= p 0) 0
              ;; Forwarding table: 2^20 = 1,048,576 slots x 24B = 24MiB (was
              ;; 2^23/192MiB, then 2^22/96MiB).  It is probed at RANDOM offsets
              ;; across its full extent (nl_compact_hash, mask 0xFFFFF), so unlike
              ;; the bump arena it cannot grow its commit incrementally.  On
              ;; Windows, reserved pages do NOT auto-commit on first touch (mmap
              ;; does), so force-commit the whole region now; nl_os_commit_range
              ;; is a no-op on mmap-backed targets.  Committed-but-untouched pages
              ;; stay out of the working set.  CAPACITY: 1M slots holds one entry
              ;; per live non-boot block per compaction, so the live-block ceiling
              ;; is ~1M at load 1.0 (~512K at the recommended 0.5).  Overflowing
              ;; the slot count hangs the linear probe, so this size suits the
              ;; on-demand reader (live set ~hundreds–thousands); a workload that
              ;; keeps >~512K blocks live across a GC must raise mask + both sizes
              ;; in lockstep (e.g. 2^22 = 100663296 / 0x3FFFFF).
              (nl_seq2 (nl_os_commit_range p 0 25165824)
                       (ptr-write-u64 268436360 0 p))))
        0))
    (defun nl_compact_hash (old) (logand (sar old 4) 1048575))
    (defun nl_compact_insert_probe (base idx old new gen)
      (let ((slot (+ base (* idx 24))))
        (if (= (ptr-read-u64 (+ slot 16) 0) gen)
            (nl_compact_insert_probe base (logand (+ idx 1) 1048575) old new gen)
          (nl_seq2 (ptr-write-u64 slot 0 old)
            (nl_seq2 (ptr-write-u64 (+ slot 8) 0 new)
                     (ptr-write-u64 (+ slot 16) 0 gen))))))
    (defun nl_compact_insert (old new)
      (nl_compact_insert_probe (ptr-read-u64 268436360 0) (nl_compact_hash old)
                               old new (ptr-read-u64 268436376 0)))
    (defun nl_compact_lookup_probe (base idx old gen)
      (let ((slot (+ base (* idx 24))))
        (if (= (ptr-read-u64 (+ slot 16) 0) gen)
            (if (= (ptr-read-u64 slot 0) old) (ptr-read-u64 (+ slot 8) 0)
              (nl_compact_lookup_probe base (logand (+ idx 1) 1048575) old gen))
          0)))
    ;; fwd(old) -> new obj ptr: below-watermark boot is identity; above-watermark
    ;; live resolves to to-space-base (268436384) + stored obj-offset.  0 = not a
    ;; forwarded live obj (caller leaves the edge unchanged, e.g. interned bufs).
    (defun nl_compact_fwd (old)
      (if (= (nl_gc_is_boot old) 1) old
        (let ((off (nl_compact_lookup_probe (ptr-read-u64 268436360 0) (nl_compact_hash old)
                                            old (ptr-read-u64 268436376 0))))
          (if (= off 0) 0 (+ (ptr-read-u64 268436384 0) off)))))
    ;; Resync helper: a chunk's block run can begin AFTER the descriptor's
    ;; nominal data-start (a to-space whose survivors were packed at a >0
    ;; offset leaves a leading zero-header gap).  A zero header (x==0) is NEVER
    ;; a real block (even a FREE block keeps a nonzero BT), so on a zero header
    ;; advance 8 bytes to resync instead of ABORTING the whole-chunk walk —
    ;; aborting abandoned every post-gap live block (e.g. the mirror's bucket
    ;; conses) -> they were munmap'd in phase 6 -> dangling -> SIGSEGV in
    ;; nelisp_mirror_walk_bucket.  A NON-zero malformed header is still a hard
    ;; desync (genuine corruption) and stops the walk.
    (defun nl_compact_step_resync (hdr end)
      (if (= (ptr-read-u64 hdr 0) 0)
          (if (< (+ hdr 8) end) (+ hdr 8) 0)
        0))
    (defun nl_compact_fwd_step (hdr end)
      (if (= (nl_gc_bt_ok hdr (nl_hdr_bt hdr) end) 0)
          (nl_compact_step_resync hdr end)
        (let ((bt (nl_hdr_bt hdr)))
          (nl_seq2
           (if (= (nl_hdr_mark hdr) 1)
               (if (= (nl_gc_is_boot hdr) 1) 0
                 (let ((cur (ptr-read-u64 268436368 0)))
                   (nl_seq2 (nl_compact_insert (+ hdr 8) (+ cur 8))
                            (ptr-write-u64 268436368 0 (+ cur bt)))))
             0)
           (+ hdr bt)))))
    (defun nl_compact_fwd_chunk (chunk)
      (let ((hdr (ptr-read-u64 (+ chunk 24) 0)) (end (nl_gc_chunk_end chunk)))
        (while (and (> hdr 0) (< hdr end))
          (setq hdr (nl_compact_fwd_step hdr end)))
        0))
    (defun nl_compact_fwd_chunks (chunk)
      (if (= chunk 0) 0
        (nl_seq2 (nl_compact_fwd_chunk chunk)
                 (nl_compact_fwd_chunks (ptr-read-u64 (+ chunk 48) 0)))))
    ;; --- Phase 3: pointer rewrite (mirror of nl_gc_mark_slot, but rewriting
    ;; each edge to fwd[target]).  rw_edge rewrites a box-ptr field and returns
    ;; the OLD ptr (the box has not moved yet — that is phase 4).  rw_block is
    ;; the cycle guard: a mark1 block flips to mark3 once its edges are rewritten.
    (defun nl_compact_rw_edge (addr)
      (let ((old (ptr-read-u64 addr 0)))
        (nl_seq2 (let ((new (nl_compact_fwd old)))
                   (if (= new 0) 0 (ptr-write-u64 addr 0 new)))
                 old)))
    (defun nl_compact_rw_block (obj)
      (if (= (nl_gc_in_arena obj) 0) 0
        (if (= (nl_hdr_mark (- obj 8)) 1)
            (nl_seq2 (nl_hdr_set_mark (- obj 8) 3) 1)
          0)))
    ;; Doc 147 Phase 2: each slot is now an 8-byte tagged WORD (stride 8,
    ;; was a 32B inline Sexp).  Per-slot WORD-aware rewrite.
    ;;
    ;; CRITICAL #1: read the raw WORD first and check the immediate bit
    ;; BEFORE touching the edge.  Calling `nl_compact_rw_edge' on an
    ;; immediate (Nil=3, T=7, Int=(n<<2)|1) is UNSOUND here: rw_edge
    ;; computes `fwd(word)' and, on a SPURIOUS hash hit for the small
    ;; integer value of the immediate, OVERWRITES the slot with a bogus
    ;; to-space pointer — corrupting a Nil/Int word into a dangling
    ;; pointer that the NEXT walk dereferences -> SIGSEGV.  So: immediate
    ;; (low bit 1) -> skip entirely, leaving the WORD untouched.
    ;;
    ;; CRITICAL #2: GATE the child recursion on `nl_compact_rw_block'
    ;; returning non-zero (= the canonical cons-arm discipline, line
    ;; ~1512).  rw_block returns 0 for a FOREIGN / already-visited /
    ;; unmarked child; recursing `rw_slot' into such a pointer reads a
    ;; tag byte at a non-arena / stale address -> SIGSEGV.  A pointer
    ;; WORD whose target is not an in-arena mark-1 block (e.g. a boxed
    ;; Float pointing into the boot image, or an aliased child already
    ;; flipped to mark-3) must NOT be recursed.  (The Phase-1 cell arm's
    ;; UNGATED `(nl_seq2 (rw_block vw) (rw_slot vw))' is latently unsafe
    ;; the same way, but a cell holds exactly one child so the collision
    ;; window never opened; a vector/record buffer exercises it.)
    ;;
    ;; For a pointer whose target IS a fresh in-arena mark-1 block,
    ;; rw_edge rewrote the WORD to fwd[child] and returned the OLD child
    ;; pointer; rw_block flipped it mark1->mark3 (phase 4 MOVES it) and we
    ;; recurse on the OLD pre-move address.  Shared by tag-8 Vectors and
    ;; tag-12 Records.
    ;; DEFENSIVE in-arena guard on DATA (same rationale as the mark-side
    ;; `nl_gc_mark_vec_slots'): a garbage parse-pool slot can present a
    ;; tag-8/12 box with a junk `data_ptr', and the 8B walker dereferences
    ;; it directly -> fault before any per-slot guard.  A real vector /
    ;; record buffer is always an in-arena alloc; skip a non-arena DATA.
    (defun nl_compact_rw_vec_slots (data i len)
      (if (= (nl_gc_in_arena data) 0) 0
        (if (< i len)
            (nl_seq2
             (if (= (logand (ptr-read-u64 (+ data (* i 8)) 0) 1) 1) 0
               (let ((vw (nl_compact_rw_edge (+ data (* i 8)))))
                 (if (= (nl_compact_rw_block vw) 0) 0
                   (nl_compact_rw_slot vw))))
             (nl_compact_rw_vec_slots data (+ i 1) len))
          0)))
    ;; Doc 147 Phase 1.5 Group P — RAW reader parse-pool compact arms (mirror
    ;; of the mark arms above; mirror of nl_compact_rw_vec_slots).  Rewrite the
    ;; buffer's own block edge, then walk every 32B slot rewriting its child
    ;; edges to fwd[child].  The pool buffer block itself is PINNED (mark-5)
    ;; via nl_compact_pin_root 3 before phase 3, so nl_compact_rw_block leaves
    ;; it put (it only flips mark1->3) while its children get forwarded.
    (defun nl_compact_rw_pool_slots (base i cap)
      (if (< i cap)
          (nl_seq2 (nl_compact_rw_slot (+ base (* i 32)))
                   (nl_compact_rw_pool_slots base (+ i 1) cap))
        0))
    (defun nl_compact_rw_pool (base cap)
      (if (= base 0) 0
        (nl_seq2 (nl_compact_rw_block base)
                 (nl_compact_rw_pool_slots base 0 cap))))
    ;; Doc 147 Phase 3: the NlConsBox car / cdr are now 8-byte tagged
    ;; WORDS (car @ box+0, cdr @ box+8; was 32B inline Sexps @ box+0 /
    ;; box+32).  Per-slot WORD-aware rewrite, mirroring the Phase-1 cell
    ;; tag-11 arm: `rw_edge(old+0)' rewrites the car WORD to fwd[child]
    ;; and returns the OLD child word; an immediate (low bit 1, e.g.
    ;; Nil/Int) is a safe no-op so skip BEFORE touching it (rw_edge on an
    ;; immediate can spuriously hash-hit and corrupt the WORD into a
    ;; dangling pointer — same hazard as the vec-slot arm); a pointer
    ;; child gets its block flipped mark1->mark3 (phase 4 MOVES it) and is
    ;; recursed on its OLD pre-move address.  For the CDR, gate the
    ;; tail-loop on the OLD child box's tag (read on the not-yet-moved
    ;; address): a Cons child (tag 7) tail-loops `rw_cons'; a non-Cons
    ;; child is a leaf walked via `rw_slot'.
    (defun nl_compact_rw_cons (sp)
      (let ((old (nl_compact_rw_edge (+ sp 8))))
        (if (= (nl_compact_rw_block old) 0) 0
          (nl_seq2
           ;; car WORD @ old+0
           (if (= (logand (ptr-read-u64 (+ old 0) 0) 1) 1) 0
             (let ((cw (nl_compact_rw_edge (+ old 0))))
               (if (= (nl_compact_rw_block cw) 0) 0
                 (nl_compact_rw_slot cw))))
           ;; cdr WORD @ old+8
           (if (= (logand (ptr-read-u64 (+ old 8) 0) 1) 1) 0
             (let ((dw (nl_compact_rw_edge (+ old 8))))
               (if (= (nl_compact_rw_block dw) 0) 0
                 (if (= (ptr-read-u8 dw 0) 7)
                     (nl_compact_rw_cons dw)
                   (nl_compact_rw_slot dw)))))))))
    (defun nl_compact_rw_slot (sp)
      (let ((tag (ptr-read-u8 sp 0)))
        (if (= tag 7) (nl_compact_rw_cons sp)
          (if (= tag 8)
              (let ((old (nl_compact_rw_edge (+ sp 8))))
                (if (= (nl_compact_rw_block old) 0) 0
                  (let ((data_old (ptr-read-u64 old 8)) (len (ptr-read-u64 old 16)))
                    ;; SYMMETRY FIX (= mark walk写し, Doc146 §5.A-2): gate slot
                    ;; recursion ONLY on the BOX (already flipped above), NOT on
                    ;; the data buffer.  The old `(if rw_block data_old ...)' guard
                    ;; SKIPPED all slot-edge rewrites whenever the data buffer was
                    ;; already mark3 (visited via an aliasing path) -> bucket cons
                    ;; / vector element edges left dangling after munmap.  Always
                    ;; rewrite the buffer's own block edge, then walk every slot.
                    (seq (nl_compact_rw_edge (+ old 8))
                         (nl_compact_rw_block data_old)
                         (nl_compact_rw_vec_slots data_old 0 len)))))
            (if (= tag 12)
                (let ((old (nl_compact_rw_edge (+ sp 8))))
                  (if (= (nl_compact_rw_block old) 0) 0
                    (let ((data_old (ptr-read-u64 old 40)) (len (ptr-read-u64 old 48)))
                      (seq (nl_compact_rw_slot old)
                           (nl_compact_rw_edge (+ old 40))
                           (nl_compact_rw_block data_old)
                           (nl_compact_rw_vec_slots data_old 0 len)))))
              (if (= tag 11)
                  ;; Cell (Doc 147 P1): value WORD @ box+0 (8B tagged).
                  ;; rw_edge(old+0) rewrites the box+0 value-word to
                  ;; fwd[child] and returns the OLD child word (vw).  For an
                  ;; immediate it is a safe no-op (fwd returns it unchanged,
                  ;; low bit 1 -> skip).  For a pointer, flip the child
                  ;; block mark1->mark3 (phase 4 MOVES it) and recurse it on
                  ;; its OLD pre-move address — the canonical cons/vector
                  ;; pattern.  (The old code walked box+0 as a 32B slot and
                  ;; never rewrote the value-word edge, so a shared cell's
                  ;; value word dangled after the child moved + munmap.)
                  (let ((old (nl_compact_rw_edge (+ sp 8))))
                    (if (= (nl_compact_rw_block old) 0) 0
                      (let ((vw (nl_compact_rw_edge (+ old 0))))
                        (if (= (logand vw 1) 1) 0
                          (nl_seq2 (nl_compact_rw_block vw)
                                   (nl_compact_rw_slot vw))))))
                (if (= tag 6)
                    (let ((old (nl_compact_rw_edge (+ sp 8))))
                      (if (= (nl_compact_rw_block old) 0) 0
                        (nl_seq2 (nl_compact_rw_block (nl_compact_rw_edge (+ old 8))) 0)))
                  ;; tag 5 Str AND tag 4 Symbol share the inline-string layout
                  ;; (char/name buf ptr @ sp+16).  Both MUST rewrite that edge
                  ;; and flip the buffer block mark1->mark3 so phase 4 moves it;
                  ;; omitting tag 4 left symbol name buffers mark1 (never moved,
                  ;; their from-space chunk munmap'd) -> dangling ptr / SIGSEGV.
                  (if (if (= tag 5) 1 (= tag 4))
                      (nl_seq2 (nl_compact_rw_block (nl_compact_rw_edge (+ sp 16))) 0)
                    (if (= tag 9) (nl_seq2 (nl_compact_rw_block (nl_compact_rw_edge (+ sp 8))) 0)
                      (if (= tag 10) (nl_seq2 (nl_compact_rw_block (nl_compact_rw_edge (+ sp 8))) 0)
                        0))))))))))
    (defun nl_compact_rw_roots (ctx result out pool src cursor bsym)
      (seq (nl_compact_rw_slot (+ ctx 0))
           (nl_compact_rw_slot (+ ctx 32))
           (nl_compact_rw_slot (+ ctx 64))
           (nl_compact_rw_slot result)
           (nl_compact_rw_slot out)
           (nl_compact_rw_pool pool (nl_gc_pool_cap)) ; Doc 147 P1.5 — RAW parse-pool buffer
           (nl_compact_rw_slot src)
           (nl_compact_rw_slot cursor)
           (nl_compact_rw_slot bsym)
           ;; Shared symentry root (268436328 -> Symbol Sexp block `s').  Rewrite
           ;; the root edge to fwd(s); flip `s' block 1->3 so phase 4 moves it;
           ;; AND walk `s' as a Symbol SLOT on the OLD address so its name-buffer
           ;; edge @ s+16 is rewritten (and that buffer block flipped 1->3 to be
           ;; moved) BEFORE phase 4 memmoves the slot to to-space.  Without the
           ;; slot walk the name buffer is never moved -> dangling name ptr.
           (if (= (ptr-read-u64 268436328 0) 0) 0
             (let ((old (nl_compact_rw_edge 268436328)))
               (nl_seq2 (nl_compact_rw_block old)
                        (nl_compact_rw_slot old))))))
    ;; --- Phase 4: move (memmove each rewritten-live block to its to-space
    ;; address) + reset marks.  Boot (below watermark) is mark3 from the rewrite
    ;; walk but stays put (just clear its mark).
    (defun nl_compact_copy (src dst n)
      (let ((s src) (d dst) (k n))
        (while (> k 7)
          (nl_seq2 (ptr-write-u64 d 0 (ptr-read-u64 s 0))
            (nl_seq2 (setq s (+ s 8))
              (nl_seq2 (setq d (+ d 8)) (setq k (- k 8))))))
        0))
    (defun nl_compact_move_step (hdr end)
      (if (= (nl_gc_bt_ok hdr (nl_hdr_bt hdr) end) 0)
          (nl_compact_step_resync hdr end)   ; skip leading/internal zero gap
        (let ((bt (nl_hdr_bt hdr)))
          (nl_seq2
           (if (= (nl_hdr_mark hdr) 3)
               (if (= (nl_gc_is_boot hdr) 1)
                   (nl_hdr_set_mark hdr 0)
                 (let ((f (nl_compact_fwd (+ hdr 8))))
                   (if (= f 0)
                       (nl_hdr_set_mark hdr 0)  ; GUARD: unforwarded -> leave in place
                     (nl_seq2 (nl_compact_copy hdr (- f 8) bt) (nl_hdr_set_mark (- f 8) 0)))))
             0)
           (+ hdr bt)))))
    (defun nl_compact_move_chunk (chunk)
      (let ((hdr (ptr-read-u64 (+ chunk 24) 0)) (end (nl_gc_chunk_end chunk)))
        (while (and (> hdr 0) (< hdr end))
          (setq hdr (nl_compact_move_step hdr end)))
        0))
    (defun nl_compact_move_chunks (chunk tospace)
      (if (= chunk 0) 0
        (if (= chunk tospace) 0
          (nl_seq2 (nl_compact_move_chunk chunk)
                   (nl_compact_move_chunks (ptr-read-u64 (+ chunk 48) 0) tospace)))))
    ;; --- Phase 6: munmap each old growth chunk (between chunk-0 and to-space).
    (defun nl_compact_chunk_has_pin (base size)
      (let ((b (ptr-read-u64 268436392 0)))
        (if (= b 0) 0
          (if (< b base) 0 (if (< b (+ base size)) 1 0)))))
    (defun nl_compact_pin_src (src)
      (let* ((tg (ptr-read-u8 src 0))
             (b (if (= tg 5) (ptr-read-u64 src 16)
                  (if (= tg 6) (ptr-read-u64 (ptr-read-u64 src 8) 8) 0))))
        (if (= b 0) (ptr-write-u64 268436392 0 0)
          (if (= (nl_gc_in_arena b) 0) (ptr-write-u64 268436392 0 0)
            (nl_seq2 (ptr-write-u64 268436392 0 b)
              (if (= (nl_hdr_mark (- b 8)) 1) (nl_hdr_set_mark (- b 8) 5) 0))))))
    (defun nl_compact_unpin_src ()
      (let ((b (ptr-read-u64 268436392 0)))
        (if (= b 0) 0 (nl_hdr_set_mark (- b 8) 0))))
    ;; --- Driver scratch ROOT-BLOCK pinning (Doc146 §2 root-coverage).  The
    ;; reader/eval driver holds the addresses of its 32B scratch root blocks
    ;; (ctx/result/out/pool/cursor/bsym) in AOT-compiled LOCALS across the GC
    ;; call.  Compaction must NOT relocate or munmap those blocks or the locals
    ;; dangle (SIGSEGV writing `result' in bf_eval_source_string_loop).  We pin
    ;; them: record up to 7 root-block addresses in a control-region array
    ;; (268436400..+48), mark each block mark-5 so phase 2/4 skip it (stays in
    ;; place; rw_roots still rewrites its inner Sexp slots by fixed address),
    ;; and keep every chunk that contains a pinned address in phase 6.
    (defun nl_compact_pin_root (i b)
      (if (= b 0) 0
        (if (= (nl_gc_in_arena b) 0) 0
          (nl_seq2 (ptr-write-u64 (+ 268436400 (* i 8)) 0 b)
            (if (= (nl_hdr_mark (- b 8)) 1) (nl_hdr_set_mark (- b 8) 5) 0)))))
    (defun nl_compact_pin_roots (ctx result out pool cursor bsym)
      (seq (ptr-write-u64 268436400 0 0) (ptr-write-u64 268436408 0 0)
           (ptr-write-u64 268436416 0 0) (ptr-write-u64 268436424 0 0)
           (ptr-write-u64 268436432 0 0) (ptr-write-u64 268436440 0 0)
           (nl_compact_pin_root 0 ctx)    (nl_compact_pin_root 1 result)
           (nl_compact_pin_root 2 out)    (nl_compact_pin_root 3 pool)
           (nl_compact_pin_root 4 cursor) (nl_compact_pin_root 5 bsym)))
    (defun nl_compact_unpin_root (i)
      (let ((b (ptr-read-u64 (+ 268436400 (* i 8)) 0)))
        (if (= b 0) 0
          (nl_seq2 (if (= (nl_hdr_mark (- b 8)) 5) (nl_hdr_set_mark (- b 8) 0) 0)
                   (ptr-write-u64 (+ 268436400 (* i 8)) 0 0)))))
    (defun nl_compact_unpin_roots ()
      (seq (nl_compact_unpin_root 0) (nl_compact_unpin_root 1)
           (nl_compact_unpin_root 2) (nl_compact_unpin_root 3)
           (nl_compact_unpin_root 4) (nl_compact_unpin_root 5)))
    ;; Does [base,base+size) contain pinned-root slot I (0..5)?
    (defun nl_compact_chunk_has_root_pin (base size i)
      (if (> i 5) 0
        (let ((b (ptr-read-u64 (+ 268436400 (* i 8)) 0)))
          (if (= b 0) (nl_compact_chunk_has_root_pin base size (+ i 1))
            (if (< b base) (nl_compact_chunk_has_root_pin base size (+ i 1))
              (if (< b (+ base size)) 1
                (nl_compact_chunk_has_root_pin base size (+ i 1))))))))
    (defun nl_compact_chunk_pinned (base size)
      (if (= (nl_compact_chunk_has_pin base size) 1) 1
        (nl_compact_chunk_has_root_pin base size 0)))
    ;; Phase 6: munmap each growth chunk that holds NO pinned address; rebuild
    ;; the survivor chain of pinned chunks (returns its head, 0 if none).  The
    ;; caller splices [chunk0] -> <pinned chain> -> [to-space].
    ;; Walk the growth chain (excluding chunk 0), munmap every chunk that holds
    ;; NO pinned address, and return the head of a freshly-threaded chain of the
    ;; KEPT (pinned) chunks whose tail links to TOSPACE.  Reaching TOSPACE (or
    ;; end-of-list) returns TOSPACE so the kept chain terminates there; the
    ;; caller splices chunk0.next -> <this head>.
    (defun nl_compact_munmap_growth (chunk tospace)
      (if (= chunk 0) tospace
        (if (= chunk tospace) tospace
          (let ((next (ptr-read-u64 (+ chunk 48) 0))
                (base (ptr-read-u64 chunk 0)) (size (ptr-read-u64 (+ chunk 8) 0)))
            (if (= (nl_compact_chunk_pinned base size) 1)
                ;; keep this chunk: link it ahead of the rest of the kept chain.
                (nl_seq2 (ptr-write-u64 (+ chunk 48) 0
                                        (nl_compact_munmap_growth next tospace))
                         chunk)
              (seq (ptr-write-u64 268436184 0 (- (ptr-read-u64 268436184 0) size))
                   (nl_os_free_chunk base size)
                   (nl_compact_munmap_growth next tospace)))))))
    (defun nl_compact_clear_fl (n)
      (if (> n 57) (ptr-write-u64 268435552 0 0)
        (nl_seq2 (ptr-write-u64 (+ 268435696 (* n 8)) 0 0)
                 (nl_compact_clear_fl (+ n 1)))))
    ;; Orchestrate phases 2-6.  Takes the 7 roots (for phase 3 rewrite).
    (defun nl_gc_compact (ctx result out pool src cursor bsym)
      (seq
       (nl_compact_table_init)
       (nl_compact_pin_src src)
       (nl_compact_pin_roots ctx result out pool cursor bsym)       ; pin driver scratch
       (ptr-write-u64 268436376 0 (+ (ptr-read-u64 268436376 0) 1)) ; gen++
       (ptr-write-u64 268436368 0 0)                                ; offset cursor = 0
       (nl_compact_fwd_chunks (ptr-read-u64 268436160 0))           ; phase 2
       (let* ((total (ptr-read-u64 268436368 0))
              (tospace (nl_chunk_alloc_new (+ total 1048576))))     ; to-space chunk
         (if (= tospace 0) 0
           (seq
            ;; Phase 4 memmoves the live set into to-space via BULK copy, not
            ;; the bump path, so commit_range is never reached for those pages.
            ;; Windows reserved pages don't demand-commit on touch (mmap does),
            ;; so force-commit the whole to-space now; it is densely written
            ;; anyway (= the compacted live set) so this is RSS-neutral, and
            ;; nl_os_commit_range is a no-op on mmap-backed targets.
            (nl_os_commit_range (ptr-read-u64 tospace 0) 0 (ptr-read-u64 (+ tospace 8) 0))
            (ptr-write-u64 268436384 0 (ptr-read-u64 (+ tospace 24) 0)) ; T = data-start
            (nl_compact_rw_roots ctx result out pool src cursor bsym)   ; phase 3
            (nl_compact_move_chunks (ptr-read-u64 268436160 0) tospace) ; phase 4
            (let* ((c0 (ptr-read-u64 268436160 0))                      ; phase 5/6
                   ;; head of the kept (pinned) growth chain; tail links to
                   ;; tospace (munmap_growth returns tospace when nothing kept).
                   (kept (nl_compact_munmap_growth (ptr-read-u64 (+ c0 48) 0) tospace)))
              (seq
               (ptr-write-u64 (+ c0 48) 0 kept)
               (ptr-write-u64 (+ tospace 48) 0 0)
               (ptr-write-u64 268436168 0 tospace)
               (ptr-write-u64 268435456 0 (- (ptr-read-u64 268435664 0) (ptr-read-u64 c0 0)))
               (ptr-write-u64 (+ tospace 16) 0 (+ 1024 total))
               (nl_compact_clear_fl 0)
               (nl_compact_unpin_src)
               (nl_compact_unpin_roots))))))))
    ;; Doc 152 §11.41 Stage 4a: safepoint-context stack ops (DORMANT — no caller
    ;; yet; the driver publish + mid-form collect_published land in 4b under
    ;; poison validation, so these change no runtime behaviour).  The driver will
    ;; push the executing top-level form's precise root-set before eval and pop
    ;; after the boundary collect; a mid-form safepoint reads frame[depth-1].
    (defun nl_gc_ctx_store (base env result out pool src cursor bsym)
      (seq (ptr-write-u64 base 0 env) (ptr-write-u64 base 8 result)
           (ptr-write-u64 base 16 out) (ptr-write-u64 base 24 pool)
           (ptr-write-u64 base 32 src) (ptr-write-u64 base 40 cursor)
           (ptr-write-u64 base 48 bsym) 0))
    (defun nl_gc_ctx_push (env result out pool src cursor bsym)
      (if (< (ptr-read-u64 (data-addr nl_safepoint_ctx) 0) 64)
          (nl_seq2
           (nl_gc_ctx_store
            (+ (data-addr nl_safepoint_ctx)
               (+ 64 (* (ptr-read-u64 (data-addr nl_safepoint_ctx) 0) 56)))
            env result out pool src cursor bsym)
           (ptr-write-u64 (data-addr nl_safepoint_ctx) 0
                          (+ (ptr-read-u64 (data-addr nl_safepoint_ctx) 0) 1)))
        0))
    (defun nl_gc_ctx_pop ()
      (if (> (ptr-read-u64 (data-addr nl_safepoint_ctx) 0) 0)
          (ptr-write-u64 (data-addr nl_safepoint_ctx) 0
                         (- (ptr-read-u64 (data-addr nl_safepoint_ctx) 0) 1))
        0))
    ;; ===== Doc 152 §11.41 Stage 4b step 2: sound mid-form collect =====
    ;; The driver publishes the executing top-level form's precise root-set into
    ;; nl_safepoint_ctx (Stage 4b step 1); a mid-form safepoint reads those frames
    ;; and runs a PRECISE-ONLY mark+sweep.  Unlike the boundary `nl_gc_collect',
    ;; the MIDFORM mode NEVER scans the C-stack conservatively: a stale stack word
    ;; that happens to mark a missed root would silently mask the very unsoundness
    ;; the poison validation is built to expose (§11.41 refinement 2).  It also
    ;; always SWEEPS (never compacts) -- mid-form objects must not move.
    ;;
    ;; Mark ONE published frame precisely: the same root arms as `nl_gc_mark_roots'
    ;; MINUS the conservative scan, the rootstack and the shared symentry (those
    ;; are global -- marked once per collection, not per frame).  `env' is the eval
    ;; ctx (mirror@+0 / frames@+32 / unbound@+64 inside the env block); `result' is
    ;; the executing form AST -- the root §11.34 missed.
    (defun nl_gc_mark_published_frame (env result out pool src cursor bsym)
      (nl_seq2 (nl_gc_mark_root_blocks env result out pool src cursor bsym)
       (nl_seq2 (nl_gc_mark_slot (+ env 0))     ; mirror / globals
        (nl_seq2 (nl_gc_mark_slot (+ env 32))   ; frame stack
         (nl_seq2 (nl_gc_mark_slot (+ env 64))  ; unbound marker
          (nl_seq2 (nl_gc_mark_slot result)     ; executing form AST (§11.34 root)
           (nl_seq2 (nl_gc_mark_slot out)       ; in-flight / last result
            (nl_seq2 (nl_gc_mark_pool pool (nl_gc_pool_cap))
             (nl_seq2 (nl_gc_mark_slot src)
              (nl_seq2 (nl_gc_mark_slot cursor)
                       (nl_gc_mark_slot bsym)))))))))))
    ;; Read the 7-slot frame at BASE (env@+0/result@+8/out@+16/pool@+24/src@+32/
    ;; cursor@+40/bsym@+48) and mark it.  Reads are passed straight as args (no
    ;; across-call locals; mirrors `nl_gc_ctx_store').
    (defun nl_gc_mark_published_frame_at (base)
      (nl_gc_mark_published_frame
       (ptr-read-u64 base 0) (ptr-read-u64 base 8) (ptr-read-u64 base 16)
       (ptr-read-u64 base 24) (ptr-read-u64 base 32) (ptr-read-u64 base 40)
       (ptr-read-u64 base 48)))
    ;; Mark every published frame [0,depth).  Nested eval / load / error paths
    ;; push outer contexts, so all live frames must be marked (§11.41 refinement 1).
    ;; Walk frames [i,depth) by tail-recursion -- index/depth thread through
    ;; ARGS, not a mutated let-local.  A constant-init let-local (e.g. (i 0)) is
    ;; constant-folded here, so a `setq' on it miscompiles to a DYNAMIC env store
    ;; (nl_alloc_symbol + nelisp_env_set_value on a garbage env -> null deref).
    ;; depth<=64 so the recursion is bounded (mirrors nl_gc_sweep_chunks).
    (defun nl_gc_mark_published_contexts_from (i depth)
      (if (< i depth)
          (nl_seq2
           (nl_gc_mark_published_frame_at
            (+ (data-addr nl_safepoint_ctx) (+ 64 (* i 56))))
           (nl_gc_mark_published_contexts_from (+ i 1) depth))
        0))
    (defun nl_gc_mark_published_contexts ()
      (nl_gc_mark_published_contexts_from
       0 (ptr-read-u64 (data-addr nl_safepoint_ctx) 0)))
    ;; Shared symentry symbol (268436328): mark the BLOCK + walk it as a Symbol
    ;; SLOT so its name buffer is kept alive (mirrors the `nl_gc_mark_roots' arm).
    (defun nl_gc_mark_symentry ()
      (if (= (ptr-read-u64 268436328 0) 0) 0
        (nl_seq2 (nl_gc_mark_block (ptr-read-u64 268436328 0))
                 (nl_gc_mark_slot (ptr-read-u64 268436328 0)))))
    ;; Mode-split mid-form collector.  mode 0 = TOPLEVEL (conservative scan
    ;; allowed), mode 1 = MIDFORM (precise-only).  NL_GC_IN_PROGRESS (ctx+24) is a
    ;; reentrancy guard.  Always mark+sweep (never compact): mid-form objects must
    ;; stay put, and precise-only marking has no conservatively-found blocks to pin.
    (defun nl_gc_collect_published (mode)
      (if (= (ptr-read-u64 (data-addr nl_safepoint_ctx) 24) 1) 0
        (nl_seq2 (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 1)
         (nl_seq2 (nl_gc_mark_published_contexts)
          (nl_seq2 (nl_gc_mark_rootstack)
           (nl_seq2 (nl_gc_mark_symentry)
            (nl_seq2 (if (= mode 0) (nl_gc_conserv_maybe) 0)
             (nl_seq2 (nl_gc_sweep)
                      (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 0)))))))))
    ;; Gated mid-form safepoint (called from nl_sf_while's backedge).  enable
    ;; (ctx+8) defaults OFF -> a single cheap branch + return, so non-test runs are
    ;; unchanged.  alloc-debt gate: fire when total chunk-bytes-reserved (268436184
    ;; -- the SAME monotonic counter the boundary GC trips on; the chunk-0 bump
    ;; @268435456 caps at the first chunk and is useless here) crosses the
    ;; next-trigger watermark (ctx+40), then re-arm +16 MiB and bump the
    ;; fired-count (ctx+32, diagnostic).  MIDFORM precise-only (mode 1).
    (defun nl_gc_safepoint ()
      (if (= (ptr-read-u64 (data-addr nl_safepoint_ctx) 8) 1)
          (if (< (ptr-read-u64 268436184 0)
                 (ptr-read-u64 (data-addr nl_safepoint_ctx) 40))
              0
            (nl_seq2 (nl_gc_collect_published 1)
             (nl_seq2 (ptr-write-u64 (data-addr nl_safepoint_ctx) 40
                                     (+ (ptr-read-u64 268436184 0) 16777216))
                      (ptr-write-u64 (data-addr nl_safepoint_ctx) 32
                                     (+ (ptr-read-u64 (data-addr nl_safepoint_ctx) 32) 1)))))
        0))
    (defun nl_gc_collect (ctx result out pool src cursor bsym)
      (if (= (ptr-read-u64 268435616 0) 1) 0    ; DEBUG: collect = pure no-op
      (seq
       (if (= (ptr-read-u64 268435592 0) 1) 0   ; DEBUG: skip-mark when slot==1
         (nl_gc_mark_roots ctx result out pool src cursor bsym))
       (if (= (ptr-read-u64 268435608 0) 1)    ; Doc146 §5: compact (incl. reclaim, no sweep)
           (nl_gc_compact ctx result out pool src cursor bsym)
         (nl_gc_sweep)))))
    ;; Form-boundary collections run after a top-level form has finished
    ;; evaluating.  The RAW reader parse pool allocation itself must remain
    ;; pinned for the next parse, but stale/unused slots from prior forms are
    ;; no longer a sound root set.  Temporarily setting the pool cap to 0 keeps
    ;; the pool block pinned via `nl_gc_mark_root_blocks' while avoiding a walk
    ;; through stale slots.  Mid-parse safepoints still call `nl_gc_collect'
    ;; directly and keep full pool slot marking.
    (defun nl_gc_collect_form_boundary (ctx result out pool src cursor bsym)
      (let* ((cap (nl_gc_pool_cap))
             (live 0))
        (seq
         (ptr-write-u64 268436448 0 0)
         (setq live (nl_gc_collect ctx result out pool src cursor bsym))
         (ptr-write-u64 268436448 0 cap)
         live))))
  "Tracing mark-sweep GC for the headered standalone arena.  See the
preceding commentary for box layouts, root set, and the soundness
argument (reachability + in-arena bounds checks).")

;; SHIM: nelisp_eval_call — universal recursion entry.  Bumps rec_cur@env+96
;; against rec_max@env+104, recurses into REAL nl_eval_inner.
;;
;; ===================================================================
;; PER-EVAL SCRATCH RECLAMATION (the safe, LIFO arena release point).
;;
;; Every nested `nelisp_eval_call' takes an arena MARK at entry (= the
;; bump offset *after* the caller finished its own allocations, since
;; `out' and any caller scratch were allocated before this call).  When
;; this eval returns, everything IT allocated lives in [mark, cur).  That
;; whole span is dead iff (a) the result in `out' is an INLINE immediate
;; (no heap box escapes through `out') and (b) nothing this eval did
;; installed a box into PRE-EXISTING (persistent) state.  Under those two
;; conditions the span is provably unreachable and we reset the bump to
;; `mark', freeing the call-tree garbage.  This is LIFO-safe: a nested
;; eval only ever resets to ITS OWN entry mark (>= every allocation the
;; caller made before calling), so it can never free the caller's
;; in-flight arg list / result (the prior corruption bug, which came from
;; resetting *inside* `nl_apply_lambda_inner' below the caller's mark).
;;
;; (a) IMMEDIATE-RESULT gate: out.tag <= 3 covers Nil(0)/T(1)/Int(2)/
;;     Float(3) — the only tags whose entire payload is inline in the
;;     32-byte `out' slot.  Symbol(4)/Str(5)/Cons(7)/Vector(8)/Record(12)/
;;     etc carry a box pointer into the arena, so they must survive ->
;;     no reset.
;; (b) NO-ESCAPE gate: a MUTATION EPOCH counter @268435544 (last free
;;     8 bytes of the reserved [16,96) region) is bumped by every
;;     primitive that writes a box into persistent structure (setcar/
;;     setcdr/aset/puthash, `nelisp_env_set_value' cell-hit + mirror,
;;     `fset' mirror-install).  We snapshot it at entry; if it changed,
;;     some box escaped into persistent state during this eval (possibly
;;     in a nested call) -> no reset.  Over-conservative (a `setq' to a
;;     purely-local binding also blocks the reset) but always SOUND:
;;     under-bumping would corrupt, over-bumping only leaks.
;;
;; For pure numeric recursion (fib: cond-macro re-expansion + arithmetic,
;; NO persistent mutation, Int result) BOTH gates pass on every call, so
;; each `fib' invocation's frame/arg/macro garbage is released the instant
;; it returns -> a single deeply-recursive top-level form now runs in
;; BOUNDED peak memory (the old residual blocker).
;; ===================================================================
;; NOTE (reclamation investigation, 2026-06-01): a per-eval arena reset was
;; prototyped here, gated on (immediate result tag <= 3) AND (mutation epoch
;; @268435544 unchanged) [+ escape-epoch bumps in setcar/setcdr/aset/puthash,
;; frame-backing grow, env-set-value].  MEASURED win was real: fib peak RSS went
;; from exponential (305 MB @ fib(20), 3.2 GB @ fib(25), SIGSEGV @ fib(26)) to a
;; FLAT ~16-22 MB independent of N.  But it is NOT CORRECT: self-recursion and
;; multi-form-after-call return garbage (e.g. `(+ 0 (f 0))' -> 0, and `(fset 'g
;; ..) (g 1) (g 10)' -> 0) because the eval machinery escapes boxes above the
;; mark through refcount-ALIASING clones + in-place macro caching + mirror/closure
;; sharing that the build-glue cannot enumerate.  Shipping it would corrupt, so
;; the reset is LEFT OUT (this shim is byte-identical to the pre-investigation
;; recursion-counter-only version).  The correct fix is a tracing mark-sweep GC at
;; the top-level form boundary (reachability instead of escape-enumeration) — see
;; the report.  The `268435544' mutation-epoch slot + `wf_dirty' instrumentation
;; below are inert with the reset disabled.
(defconst nelisp-standalone--shim-source
  '(seq (defun nelisp_eval_call (form_ptr env out)
          (let* ((rec_cur_addr (+ env 96)) (rec_max_addr (+ env 104)))
            (let* ((rec_cur (ptr-read-u64 rec_cur_addr 0)) (rec_max (ptr-read-u64 rec_max_addr 0)))
              (if (>= rec_cur rec_max) 1
                (nl_seq2 (ptr-write-u64 rec_cur_addr 0 (+ rec_cur 1))
                  (let* ((rc (nl_eval_inner form_ptr env out 0)))
                    (nl_seq2 (ptr-write-u64 rec_cur_addr 0 rec_cur) rc)))))))))

(defun nelisp-standalone--name-u64 (s)
  "Pack STRING S (<=8 ASCII bytes) little-endian into a u64 for symbol-name match."
  (let ((v 0) (i 0))
    (while (< i (length s))
      (setq v (logior v (ash (aref s i) (* i 8))))
      (setq i (1+ i)))
    v))

;; *** nelisp_apply_function — the pure-elisp builtin dispatcher (was Rust-only). ***
;; func -> (builtin . (NAME . nil)); NAME @ car(cdr(func)); args -> cons list of
;; evaluated Sexps; out -> 32B result slot.  Covers arithmetic (+ - * / mod 1+ 1-),
;; comparison/predicate (= < > <= >= eq null not -> T/Nil), list ops
;; (car cdr cons list), hash tables (M4: make-hash-table/puthash/gethash/remhash/
;; hash-table-count/maphash), strings+format (M5: length/concat/substring/
;; make-string/string=/char-to-string/string-to-char/number-to-string/
;; string-to-number/format) and file I/O (M7: wrf/rdf/slen, impls in m7b-fileio.o).
;; rc 0 ok / 1 unknown.  The dispatch chain is generated from `table' at load time.
;;
;; DISPATCH builder (unified, M4/M5): each table entry is (MATCH . IMPL) where
;; MATCH is either (:u8 "NAME") -> u64-packed `wf_name_is' (names <=8 bytes) or
;; (:lit "NAME") -> the `sexp-name-eq' grammar op (full-length compare, ANY
;; length, required for "make-hash-table"/"string-to-number"/... > 8 bytes).
(defconst nelisp-standalone--applyfn-dispatch-table
  '(((:u8 "+")    . (if (= (wf_any_float args) 1)
                        ;; The fold leaves the final Float Sexp in `sc'; copy it
                        ;; out.  (Re-materializing via `(nl_sexp_write_float out
                        ;; (bits-to-f64 (wf_fsum ...)))' fails: the extern-call
                        ;; f64-arg classifier mishandles a bits-to-f64 wrapping a
                        ;; CALL.  Inside the helpers bits-to-f64 only wraps refs.)
                        (let* ((sc (alloc-bytes 32 8)))
                          (seq (wf_fsum args 0 sc) (wf_copy32 out sc)))
                      (wf_write_int out (wf_sum args 0))))
    ((:u8 "-")    . (if (= (wf_any_float args) 1)
                        (let* ((sc (alloc-bytes 32 8)))
                          (seq (wf_fdiff args sc) (wf_copy32 out sc)))
                      (wf_write_int out (wf_diff args))))
    ((:u8 "*")    . (if (= (wf_any_float args) 1)
                        (let* ((sc (alloc-bytes 32 8)))
                          (seq (wf_fprod args (wf_int_fbits 1 sc) sc) (wf_copy32 out sc)))
                      (wf_write_int out (wf_prod args 1))))
    ((:u8 "/")    . (if (= (wf_any_float args) 1)
                        (let* ((sc (alloc-bytes 32 8)))
                          (seq (wf_fdiv2 args sc) (wf_copy32 out sc)))
                      (wf_write_int out (/ (wf_argval args 0) (wf_argval args 1)))))
    ((:u8 "mod")  . (wf_write_int out (mod (wf_argval args 0) (wf_argval args 1))))
    ;; `%' = C-style integer remainder (sign of dividend): x - y*trunc(x/y).
    ;; Dialect `/' is truncating i64 division, so this matches Emacs `%'.
    ((:u8 "%")    . (let* ((a (wf_argval args 0)) (b (wf_argval args 1)))
                      (wf_write_int out (- a (* b (/ a b))))))
    ;; `/=' = 2-arg numeric not-equal (int/float via wf_num_eq).
    ((:u8 "/=")   . (let* ((rest (nl_cons_cdr_ptr args)))
                      (if (= (wf_num_eq (nl_cons_car_ptr args) (nl_cons_car_ptr rest)) 1)
                          (wf_write_nil out) (wf_write_t out))))
    ((:u8 "1+")   . (let* ((p (wf_arg_ptr args 0)))
                      (if (= (ptr-read-u64 p 0) 3)
                          (let* ((sc (alloc-bytes 32 8)))
                            (seq (nl_sexp_write_float sc (f64-add (bits-to-f64 (ptr-read-u64 p 8)) (i64-to-f64 1)))
                                 (wf_copy32 out sc)))
                        (wf_write_int out (+ (wf_argval args 0) 1)))))
    ((:u8 "1-")   . (let* ((p (wf_arg_ptr args 0)))
                      (if (= (ptr-read-u64 p 0) 3)
                          (let* ((sc (alloc-bytes 32 8)))
                            (seq (nl_sexp_write_float sc (f64-sub (bits-to-f64 (ptr-read-u64 p 8)) (i64-to-f64 1)))
                                 (wf_copy32 out sc)))
                        (wf_write_int out (- (wf_argval args 0) 1)))))
    ((:u8 "floor")    . (let* ((p (wf_arg_ptr args 0)))
                          (if (= (ptr-read-u64 p 0) 3)
                              (wf_write_int out (wf_ffloor p))
                            (wf_write_int out (ptr-read-u64 p 8)))))
    ((:u8 "truncate") . (let* ((p (wf_arg_ptr args 0)))
                          (if (= (ptr-read-u64 p 0) 3)
                              (wf_write_int out (wf_ftrunc p))
                            (wf_write_int out (ptr-read-u64 p 8)))))
    ((:u8 "ceiling")  . (let* ((p (wf_arg_ptr args 0)))
                          (if (= (ptr-read-u64 p 0) 3)
                              (wf_write_int out (wf_fceil p))
                            (wf_write_int out (ptr-read-u64 p 8)))))
    ((:u8 "=")    . (if (= (wf_chain_eq args) 1) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "<")    . (if (= (wf_chain_lt args) 1) (wf_write_t out) (wf_write_nil out)))
    ((:u8 ">")    . (if (= (wf_chain_gt args) 1) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "<=")   . (if (= (wf_chain_le args) 1) (wf_write_t out) (wf_write_nil out)))
    ((:u8 ">=")   . (if (= (wf_chain_ge args) 1) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "eq")   . (if (= (wf_raw_eq (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "null") . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 0) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "not")  . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 0) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "car")  . (wf_copy32 out (nl_cons_car_ptr (wf_arg_ptr args 0))))
    ((:u8 "cdr")  . (wf_copy32 out (nl_cons_cdr_ptr (wf_arg_ptr args 0))))
    ((:u8 "cons") . (seq (nelisp_cons_construct (wf_arg_ptr args 0) (wf_arg_ptr args 1) out) 0))
    ((:u8 "list") . (seq (wf_copy32 out args) 0))
    ;; --- list search hot paths ---
    ((:lit "memq")   . (wf_memq args out))
    ((:lit "member") . (wf_member args out))
    ((:lit "assq")   . (wf_assq args out))
    ((:lit "assoc")  . (wf_assoc args out))
    ((:lit "rassoc") . (wf_rassoc args out))
    ;; --- M4 hash tables (cons-alist v1) ---
    ((:lit "make-hash-table")  . (wf_ht_make out))
    ((:lit "puthash")          . (seq (wf_dirty) (wf_ht_put args out)))
    ((:lit "gethash")          . (wf_ht_get args out))
    ((:lit "remhash")          . (seq (wf_dirty) (wf_ht_rem args out)))
    ((:lit "hash-table-count") . (wf_write_int out (wf_ht_count_table (wf_ht_data_slot (wf_arg_ptr args 0)))))
    ((:lit "maphash")          . (wf_ht_maphash args out))
    ;; --- M5 strings + format ---
    ((:lit "length")           . (wf_write_int out (m5_length (wf_arg_ptr args 0))))
    ((:lit "string=")          . (if (= (m5_streq (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1)
                                      (wf_write_t out) (wf_write_nil out)))
    ((:lit "string-to-char")   . (wf_write_int out
                                  (if (= (str-len (wf_arg_ptr args 0)) 0) 0
                                    (str-byte-at (wf_arg_ptr args 0) 0))))
    ((:lit "string-to-number") . (wf_write_int out (m5_s2n (wf_arg_ptr args 0))))
    ((:lit "number-to-string") . (let* ((ms (alloc-bytes 32 8))
                                        (arg (wf_arg_ptr args 0)))
                                   (seq (mut-str-make-empty ms 16)
                                        (if (= (ptr-read-u64 arg 0) 3)
                                            (m5_push_float ms arg)
                                          (m5_push_dec ms (ptr-read-u64 arg 8)))
                                        (mut-str-finalize ms out) 0)))
    ;; (nelisp--fmt-float FLOAT CONV-CODE PREC) -> string (Doc 159 §4):
    ;; precision-aware %f/%e/%g body for `format'.  FLOAT is a Float Sexp.
    ((:lit "nelisp--fmt-float") . (let* ((ms (alloc-bytes 32 8))
                                         (buf (alloc-bytes 512 1))
                                         (sc (alloc-bytes 32 8)))
                                    (seq (mut-str-make-empty ms 32)
                                         (m5_fmt_float_body ms
                                            (sexp-float-unwrap (wf_arg_ptr args 0))
                                            (ptr-read-u64 (wf_arg_ptr args 1) 8)
                                            (ptr-read-u64 (wf_arg_ptr args 2) 8)
                                            buf sc)
                                         (mut-str-finalize ms out) 0)))
    ((:lit "char-to-string")   . (let* ((ms (alloc-bytes 32 8)))
                                   (seq (mut-str-make-empty ms 4)
                                        (mut-str-push-byte ms (ptr-read-u64 (wf_arg_ptr args 0) 8))
                                        (mut-str-finalize ms out) 0)))
    ((:lit "make-string")      . (let* ((ms (alloc-bytes 32 8)))
                                   (seq (mut-str-make-empty ms 16)
                                        (m5_push_repeat ms (ptr-read-u64 (wf_arg_ptr args 1) 8)
                                                        (ptr-read-u64 (wf_arg_ptr args 0) 8))
                                        (mut-str-finalize ms out) 0)))
    ((:lit "concat")           . (let* ((ms (alloc-bytes 32 8)))
                                   (seq (mut-str-make-empty ms 16)
                                        (m5_concat_walk ms args)
                                        (mut-str-finalize ms out) 0)))
    ((:lit "substring")        . (let* ((ms (alloc-bytes 32 8))
                                        (s (wf_arg_ptr args 0))
                                        (slen (str-len s))
                                        (from (ptr-read-u64 (wf_arg_ptr args 1) 8))
                                        (rest (nl_cons_cdr_ptr (nl_cons_cdr_ptr args)))
                                        (to (if (= (ptr-read-u64 rest 0) 7)
                                                (ptr-read-u64 (wf_arg_ptr args 2) 8)
                                              slen)))
                                   (seq (mut-str-make-empty ms 16)
                                        (m5_push_str_bytes ms s
                                          (if (< from 0) (+ slen from) from)
                                          (if (< to 0) (+ slen to) to))
                                        (mut-str-finalize ms out) 0)))
    ((:lit "format")           . (let* ((ms (alloc-bytes 32 8))
                                        (fmt (wf_arg_ptr args 0)))
                                   (seq (mut-str-make-empty ms 32)
                                        (m5_fmt_loop ms fmt 0 (str-len fmt)
                                                     (nl_cons_cdr_ptr args))
                                        (mut-str-finalize ms out) 0)))
    ((:lit "nelisp--repr")     . (let* ((ms (alloc-bytes 32 8)))
                                   (seq (mut-str-make-empty ms 32)
                                        (m5_prin1 ms (wf_arg_ptr args 0))
                                        (mut-str-finalize ms out) 0)))
    ((:lit "nelisp--json-encode") . (let* ((ms (alloc-bytes 32 8)))
                                      (seq (mut-str-make-empty ms 64)
                                           (m5_json ms (wf_arg_ptr args 0))
                                           (mut-str-finalize ms out) 0)))
    ((:lit "nelisp--sha256") . (let* ((ms (alloc-bytes 32 8)))
                                 (seq (mut-str-make-empty ms 64)
                                      (m5_sha256 ms (wf_arg_ptr args 0))
                                      (mut-str-finalize ms out) 0)))
    ((:lit "nelisp--string-search") . (let* ((idx (m5_string_search
                                                    (wf_arg_ptr args 1)
                                                    (wf_arg_ptr args 0)
                                                    (wf_argval args 2))))
                                        (if (< idx 0)
                                            (wf_write_nil out)
                                          (wf_write_int out idx))))
    ((:lit "nelisp--arena-stats") . (bf_arena_stats out))
    ((:lit "nelisp--arena-walk-verify") . (bf_arena_walk_verify out))
    ((:lit "nelisp--arena-dump-copy-verify") . (bf_arena_dump_copy_verify out))
    ((:lit "nelisp--arena-mark-reach-verify") . (bf_arena_mark_reach_verify out))
    ((:lit "nelisp--arena-swizzle-verify") . (bf_arena_swizzle_verify out))
    ((:lit "nelisp--arena-load-relocate-verify") . (bf_arena_load_relocate_verify out))
    ((:lit "nelisp--arena-image-root-verify") . (bf_arena_image_root_verify out))
    ((:lit "nelisp--arena-dump-table-verify") . (bf_arena_dump_table_verify out))
    ((:lit "nelisp--arena-dump-image-to-file") . (bf_arena_dump_image_to_file args out))
    ((:lit "nelisp--arena-dump-image-stream") . (bf_arena_dump_image_stream args out))
    ((:lit "nelisp--arena-load-image-from-file") . (bf_arena_load_image_from_file args out))
    ((:lit "nelisp--env-capture-roots") . (bf_env_capture_roots out))
    ((:lit "nelisp--record-expand") . (bf_record_expand args out))
    ((:lit "nelisp--arena-boot-load-verify") . (bf_arena_boot_load_verify args out))
    ((:lit "nelisp--arena-load-split-verify") . (bf_arena_load_split_verify args out))
    ((:lit "nelisp--arena-value-survival") . (bf_arena_value_survival args out))
    ((:lit "garbage-collect") . (seq (nl_gc_collect_published 0)
                                     (bf_arena_stats out)))
    ((:lit "nelisp--gc-diag") . (bf_gc_diag args out))
    ((:lit "nelisp--arena-force-grow-smoke") . (bf_arena_force_grow_smoke out))
    ((:lit "nelisp--size-census") . (bf_size_census out))
    ;; --- M7 file I/O (impls in m7b-fileio.o glue unit) ---
    ((:u8 "wrf")  . (seq (nl_bi_write_file args out) 0))
    ((:u8 "rdf")  . (seq (nl_bi_read_file args out) 0))
    ((:u8 "slen") . (seq (nl_bi_slen args out) 0))
    ;; Native buffer-scan helpers (Doc 142 Gate 5 OOM fix): an interpreted
    ;; per-char while over a ~500KB buffer string churns ~1MB of arena PER
    ;; ITERATION (value-semantics clones + per-form eval garbage, GC only at
    ;; form boundaries) -> 60GB+ RSS.  These do the scan natively instead.
    ((:lit "str-count-nl")   . (seq (nl_bi_str_count_nl args out) 0))
    ((:lit "str-line-start") . (seq (nl_bi_str_line_start args out) 0))
    ((:lit "str-kv-line")    . (seq (nl_bi_str_kv_line args out) 0))
    ((:lit "str-filter-prefix-lines") . (seq (nl_bi_str_filter_prefix_lines args out) 0))
    ;; nl-nanosleep TS-PTR: per-target nanosleep (Doc 151 Phase B).  TS-PTR is
    ;; a struct timespec pointer built by the caller (alloc-bytes + two
    ;; ptr-write-u64).  Returns the raw kernel rc; -38 (ENOSYS stub) on
    ;; targets without a wired sleep.  Replaces interpreted callers
    ;; hardcoding (syscall-direct 35 ...) which is io_setup on arm64.
    ((:lit "nl-nanosleep") . (wf_write_int out (nl_os_nanosleep (wf_argval args 0))))
    ;; REPL/process termination.  Store code+1 so slot 0 remains "no exit".
    ((:u8 "exit") . (let* ((code (if (= args 0)
                                     0
                                   (if (= (ptr-read-u64 args 0) 7)
                                       (ptr-read-u64 (nl_cons_car_ptr args) 8)
                                     0))))
                       (seq (ptr-write-u64 268435464 0 (+ code 1))
                            (wf_write_int out code)))))
  "Unified (MATCH . IMPL) builtin dispatch table for `nelisp_apply_function'.
MATCH = (:u8 NAME) for <=8-byte u64-packed names, (:lit NAME) for full-length
`sexp-name-eq' compare (any length).")

;; The first 19 entries (arithmetic / comparison / list) are the ONLY ones the
;; baked-form eval path (`nelisp-standalone--manifest') links -- they touch just
;; the cons/symbol units already in that manifest.  The HT/string/file entries
;; (M4/M5/M7) lower to extern calls (mut-str / str-direct / syscall / nl_alloc_str)
;; that only the reader manifest provides, so the baked applyfn must exclude them.
(defconst nelisp-standalone--applyfn-dispatch-table-baked
  (cl-subseq nelisp-standalone--applyfn-dispatch-table 0 19)
  "Arithmetic/comparison/list-only dispatch subset for the baked-form eval path.")

(defun nelisp-standalone--applyfn-build-dispatch (&optional table default-form)
  "Fold the (MATCH . IMPL) TABLE (default the full dispatch table) into a
nested-if Phase47 dispatch chain, defaulting to DEFAULT-FORM (an IR form
returned for an unknown builtin).  DEFAULT-FORM defaults to the stderr
diagnostic below; pass a self-contained form (e.g. `1') for link sets that do
NOT provide `nl_os_write_stderr' — the baked eval applyfn, whose manifest omits
the reader-only stderr unit, so emitting the diagnostic would leave the symbol
unresolved at link time."
  (let (;; Unknown-builtin default: write the symbol name to stderr (was a
        ;; silent failure) so interpreted callers surface WHICH registered-but-
        ;; undispatched builtin is missing, then fall through to rc 1.  A symbol
        ;; Sexp (tag 4) keeps its name bytes at ptr@16 / len@24, same as a Str.
        (dispatch (or default-form
                      '(let* ((unkb (alloc-bytes 1 1)))
                         (seq (ptr-write-u8 unkb 0 10)
                              (nl_os_write_stderr (ptr-read-u64 name_ptr 16)
                                                  (ptr-read-u64 name_ptr 24))
                              (nl_os_write_stderr unkb 1)
                              1)))))
    (dolist (entry (reverse (or table nelisp-standalone--applyfn-dispatch-table)))
      (let* ((match (car entry))
             (impl (cdr entry))
             (cond-form (pcase (car match)
                          ;; Both arms use the alloc-free `sexp-name-eq' grammar
                          ;; op.  The former `:u8' path went through
                          ;; `wf_name_is'/`wf_sym_eq', which allocated a fresh
                          ;; symbol (via `nl_alloc_symbol') plus scratch slots on
                          ;; EVERY comparison; a builtin call walking K dispatch
                          ;; arms then churned ~K symbol allocations, dominating
                          ;; eval time and GC pressure on hot ops (`+', `<', ...).
                          (:u8  `(= (sexp-name-eq name_ptr ,(cadr match)) 1))
                          (:lit `(= (sexp-name-eq name_ptr ,(cadr match)) 1)))))
        (setq dispatch `(if ,cond-form ,impl ,dispatch))))
    dispatch))

;; Core wf_* helpers (arithmetic / list / write-int-t-nil / name-match).  Shared
;; by BOTH the baked and the reader applyfn -- they reference only cons/symbol
;; units present in every manifest.
(defconst nelisp-standalone--applyfn-core-helpers
  '((defun wf_sym_eq (name_ptr buf len)
      (let* ((tmp_slot (alloc-bytes 32 8)) (res_slot (alloc-bytes 32 8)))
        (seq (nl_alloc_symbol buf len tmp_slot) (nelisp_eq_symbol name_ptr tmp_slot res_slot)
             (= (ptr-read-u64 res_slot 0) 1))))
    (defun wf_name_is (name_ptr u64 len)
      (let* ((buf (alloc-bytes 8 1)))
        (seq (ptr-write-u64 buf 0 u64) (wf_sym_eq name_ptr buf len))))
    (defun wf_arg_ptr (args n)
      (if (= n 0) (nl_cons_car_ptr args) (wf_arg_ptr (nl_cons_cdr_ptr args) (- n 1))))
    (defun wf_argval (args n) (ptr-read-u64 (wf_arg_ptr args n) 8))
    (defun wf_raw_eq (a b)
      (if (= (ptr-read-u64 a 0) (ptr-read-u64 b 0))
          (if (= (ptr-read-u64 a 8) (ptr-read-u64 b 8)) 1 0)
        0))
    (defun wf_num_lt (a b)
      (let* ((ta (ptr-read-u64 a 0)) (tb (ptr-read-u64 b 0)))
        (if (= ta 3)
            (if (= tb 3)
                (f64-lt (bits-to-f64 (sexp-float-unwrap a))
                        (bits-to-f64 (sexp-float-unwrap b)))
              (f64-lt (bits-to-f64 (sexp-float-unwrap a))
                      (i64-to-f64 (ptr-read-u64 b 8))))
          (if (= tb 3)
              (f64-lt (i64-to-f64 (ptr-read-u64 a 8))
                      (bits-to-f64 (sexp-float-unwrap b)))
            (if (< (ptr-read-u64 a 8) (ptr-read-u64 b 8)) 1 0)))))
    (defun wf_num_gt (a b)
      (let* ((ta (ptr-read-u64 a 0)) (tb (ptr-read-u64 b 0)))
        (if (= ta 3)
            (if (= tb 3)
                (f64-gt (bits-to-f64 (sexp-float-unwrap a))
                        (bits-to-f64 (sexp-float-unwrap b)))
              (f64-gt (bits-to-f64 (sexp-float-unwrap a))
                      (i64-to-f64 (ptr-read-u64 b 8))))
          (if (= tb 3)
              (f64-gt (i64-to-f64 (ptr-read-u64 a 8))
                      (bits-to-f64 (sexp-float-unwrap b)))
            (if (> (ptr-read-u64 a 8) (ptr-read-u64 b 8)) 1 0)))))
    (defun wf_num_le (a b)
      (let* ((ta (ptr-read-u64 a 0)) (tb (ptr-read-u64 b 0)))
        (if (= ta 3)
            (if (= tb 3)
                (f64-le (bits-to-f64 (sexp-float-unwrap a))
                        (bits-to-f64 (sexp-float-unwrap b)))
              (f64-le (bits-to-f64 (sexp-float-unwrap a))
                      (i64-to-f64 (ptr-read-u64 b 8))))
          (if (= tb 3)
              (f64-le (i64-to-f64 (ptr-read-u64 a 8))
                      (bits-to-f64 (sexp-float-unwrap b)))
            (if (<= (ptr-read-u64 a 8) (ptr-read-u64 b 8)) 1 0)))))
    (defun wf_num_ge (a b)
      (let* ((ta (ptr-read-u64 a 0)) (tb (ptr-read-u64 b 0)))
        (if (= ta 3)
            (if (= tb 3)
                (f64-ge (bits-to-f64 (sexp-float-unwrap a))
                        (bits-to-f64 (sexp-float-unwrap b)))
              (f64-ge (bits-to-f64 (sexp-float-unwrap a))
                      (i64-to-f64 (ptr-read-u64 b 8))))
          (if (= tb 3)
              (f64-ge (i64-to-f64 (ptr-read-u64 a 8))
                      (bits-to-f64 (sexp-float-unwrap b)))
            (if (>= (ptr-read-u64 a 8) (ptr-read-u64 b 8)) 1 0)))))
    (defun wf_num_eq (a b)
      (if (= (wf_num_le a b) 1) (wf_num_ge a b) 0))
    ;; Doc 158: CHAINED (variadic) numeric comparison.  `<'/`<='/`>'/`>='/`='
    ;; are n-ary in Elisp: (< a b c) = (and (< a b) (< b c)).  The dispatch arms
    ;; used to compare only args 0,1, so (< 1 5 3) wrongly returned t and rx's
    ;; (any "a-z") mis-negated (its `rx--generate-alt' tests
    ;; (<= lo #x3fff7f hi)).  Walk the ARGS cons-list, requiring every adjacent
    ;; pair to satisfy the 2-arg test; empty / single arg -> 1 (true).
    (defun wf_chain_lt (args)
      (let* ((rest (nl_cons_cdr_ptr args)))
        (if (= (ptr-read-u64 rest 0) 7)
            (if (= (wf_num_lt (nl_cons_car_ptr args) (nl_cons_car_ptr rest)) 1)
                (wf_chain_lt rest) 0)
          1)))
    (defun wf_chain_gt (args)
      (let* ((rest (nl_cons_cdr_ptr args)))
        (if (= (ptr-read-u64 rest 0) 7)
            (if (= (wf_num_gt (nl_cons_car_ptr args) (nl_cons_car_ptr rest)) 1)
                (wf_chain_gt rest) 0)
          1)))
    (defun wf_chain_le (args)
      (let* ((rest (nl_cons_cdr_ptr args)))
        (if (= (ptr-read-u64 rest 0) 7)
            (if (= (wf_num_le (nl_cons_car_ptr args) (nl_cons_car_ptr rest)) 1)
                (wf_chain_le rest) 0)
          1)))
    (defun wf_chain_ge (args)
      (let* ((rest (nl_cons_cdr_ptr args)))
        (if (= (ptr-read-u64 rest 0) 7)
            (if (= (wf_num_ge (nl_cons_car_ptr args) (nl_cons_car_ptr rest)) 1)
                (wf_chain_ge rest) 0)
          1)))
    (defun wf_chain_eq (args)
      (let* ((rest (nl_cons_cdr_ptr args)))
        (if (= (ptr-read-u64 rest 0) 7)
            (if (= (wf_num_eq (nl_cons_car_ptr args) (nl_cons_car_ptr rest)) 1)
                (wf_chain_eq rest) 0)
          1)))
    (defun wf_copy32 (dst src)
      (seq (ptr-write-u64 dst 0 (ptr-read-u64 src 0)) (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
           (ptr-write-u64 dst 16 (ptr-read-u64 src 16)) (ptr-write-u64 dst 24 (ptr-read-u64 src 24)) 0))
    (defun wf_write_int (out v)
      (seq (ptr-write-u64 out 0 2) (ptr-write-u64 out 8 v)
           (ptr-write-u64 out 16 0) (ptr-write-u64 out 24 0) 0))
    (defun wf_write_t (out)
      (seq (ptr-write-u64 out 0 1) (ptr-write-u64 out 8 0)
           (ptr-write-u64 out 16 0) (ptr-write-u64 out 24 0) 0))
    (defun wf_write_nil (out)
      (seq (ptr-write-u64 out 0 0) (ptr-write-u64 out 8 0)
           (ptr-write-u64 out 16 0) (ptr-write-u64 out 24 0) 0))
    (defun wf_cons_int (v rest out)
      (let* ((slot (alloc-bytes 32 8)))
        (seq (wf_write_int slot v)
             (nelisp_cons_construct slot rest out)
             0)))
    ;; Env-bridge CAPTURE (Doc 17 §11.2 Bridge 1) for the heap-v0 cold-loader.
    ;; Reads the codec's env-root-manifest 5 roots from the LIVE published EvalCtx
    ;; (env = nl_safepoint_ctx+64, the GC-published frame-0 context) and returns the
    ;; list (globals_record frames_record unbound_marker max_recursion use_elisp_apply),
    ;; directly consumable by `nelisp-heap-image-encode-roots'.  This is the "symmetric
    ;; getter to read the current globals_record" that Doc 17 §11.2 says standalone
    ;; lacks; offsets are gdb-verified against the EvalCtx (120B): globals SLOT @env+0
    ;; (tag 12 Record), frames SLOT @env+32 (tag 12), unbound SLOT @env+64 (tag 4
    ;; Symbol -- the whole 32B slot, NOT +8), max_recursion scalar @env+104, use_elisp
    ;; scalar @env+112.  Records/Symbol roots are returned as their Sexp value (slot
    ;; copy); the two scalars as Int Sexps.  Returns nil if no frame is published.
    (defun bf_env_capture_roots (out)
      (let* ((env (ptr-read-u64 (+ (data-addr nl_safepoint_ctx) 64) 0))
             (g (alloc-bytes 32 8)) (f (alloc-bytes 32 8)) (u (alloc-bytes 32 8))
             (nilp (alloc-bytes 32 8))
             (c4 (alloc-bytes 32 8)) (c3 (alloc-bytes 32 8))
             (c2 (alloc-bytes 32 8)) (c1 (alloc-bytes 32 8)))
        (if (= env 0)
            (wf_write_nil out)
          (seq
           (wf_copy32 g (+ env 0))    ; globals_record slot
           (wf_copy32 f (+ env 32))   ; frames_record slot
           (wf_copy32 u (+ env 64))   ; unbound_marker slot (tag-4 Symbol)
           (wf_write_nil nilp)
           (wf_cons_int (ptr-read-u64 (+ env 112) 0) nilp c4)  ; use_elisp_apply
           (wf_cons_int (ptr-read-u64 (+ env 104) 0) c4 c3)    ; max_recursion
           (nelisp_cons_construct u c3 c2)                     ; unbound_marker
           (nelisp_cons_construct f c2 c1)                     ; frames_record
           (nelisp_cons_construct g c1 out)                    ; globals_record
           0))))
    ;; Env-bridge option (C) (Doc 17 §11.2): expand a runtime NlRecord into a
    ;; codec-friendly cons graph so `nelisp-heap-image-encode-roots' can serialize
    ;; it.  The standalone env-record (tag 12) is opaque to the codec: the active
    ;; prelude `recordp' is aliased to `vectorp' (nelisp-stdlib-prelude.el:3113) so
    ;; it returns nil for tag-12, and host `aref'/`length' do not traverse the
    ;; 1024-bucket hash mirror.  These helpers read the verified runtime layout
    ;; (Record: type_tag @box+0, data_ptr @box+40, len @box+48; Vector: data_ptr
    ;; @box+8, len @box+16; Cons: car/cdr WORDS @box+0/+8) and rewrite the WHOLE
    ;; subgraph into cons/atoms (records + vectors -> plain lists, conses -> conses),
    ;; which the codec already handles.  Slot/element WORDS are value-words: low bit
    ;; 1 = immediate (Nil=3, T=7, Int=(n<<2)|1), low bit 0 = 8-aligned pointer to a
    ;; 32B Sexp.  Atoms are bit-copied (shared buffers are fine for read-only encode).
    ;; CAVEAT: record/vector TYPE is dropped (both become lists), so this is a
    ;; one-way serialize for the encode side; a faithful round-trip needs a tagged
    ;; form + reverse rebuild.  And the full globals graph reaches native function
    ;; cells (non-Sexp pointers) -- expanding those is unsound, which is exactly why
    ;; raw-arena (byte memcpy + relocate, no graph walk) is the pragmatic path.
    (defun bf_expand_word (w out)
      (if (= (logand w 1) 1)
          (if (= w 3) (wf_write_nil out)
            (if (= w 7) (wf_write_t out)
              (wf_write_int out (sar w 2))))
        (bf_expand_slot w out)))
    (defun bf_expand_words_list (dp i len out)
      (if (if (< i len) 0 1)
          (wf_write_nil out)
        (let* ((cs (alloc-bytes 32 8)) (rest (alloc-bytes 32 8)))
          (seq
           (bf_expand_word (ptr-read-u64 (+ dp (* i 8)) 0) cs)
           (bf_expand_words_list dp (+ i 1) len rest)
           (nelisp_cons_construct cs rest out)
           0))))
    (defun bf_expand_slot (v out)
      (let* ((tag (ptr-read-u8 v 0)))
        (if (= tag 12)
            (let* ((box (ptr-read-u64 v 8)) (typ (alloc-bytes 32 8)) (sl (alloc-bytes 32 8)))
              (seq
               (bf_expand_slot box typ)   ; type_tag @ box+0 (box viewed as a Sexp slot)
               (bf_expand_words_list (ptr-read-u64 box 40) 0 (ptr-read-u64 box 48) sl)
               (nelisp_cons_construct typ sl out)
               0))
          (if (= tag 8)
              (let* ((box (ptr-read-u64 v 8)))
                (bf_expand_words_list (ptr-read-u64 box 8) 0 (ptr-read-u64 box 16) out))
            (if (= tag 7)
                (let* ((box (ptr-read-u64 v 8)) (ca (alloc-bytes 32 8)) (cd (alloc-bytes 32 8)))
                  (seq
                   (bf_expand_word (ptr-read-u64 box 0) ca)
                   (bf_expand_word (ptr-read-u64 box 8) cd)
                   (nelisp_cons_construct ca cd out)
                   0))
              (wf_copy32 out v))))))
    (defun bf_record_expand (args out)
      (bf_expand_slot (wf_arg_ptr args 0) out))
    ;; Portable arena telemetry:
    ;;   (base size bump-offset used-bytes live-after-last-gc next-trigger
    ;;    free-list-head collect-disabled reuse-disabled
    ;;    chunk-count chunk-bytes-reserved chunk-bytes-used)
    (defun bf_arena_chunk_cursor (chunk)
      (if (= chunk (ptr-read-u64 268436160 0))
          (ptr-read-u64 268435456 0)
        (ptr-read-u64 (+ chunk 16) 0)))
    (defun bf_arena_chunk_used (chunk)
      (let ((cursor (bf_arena_chunk_cursor chunk)))
        (if (< cursor 1024) 0 (- cursor 1024))))
    (defun bf_arena_chunks_used (chunk acc)
      (if (= chunk 0)
          acc
        (bf_arena_chunks_used
         (ptr-read-u64 (+ chunk 48) 0)
         (+ acc (bf_arena_chunk_used chunk)))))
    ;; Doc 152 §11.39 Stage 3a: GC-diag read/toggle builtin.  ARG0:
    ;; 0=read-only, 1=enable poison-on-free, 2=disable poison,
    ;; 3/4=push/pop empty safepoint context, 5/6=arm/disarm mid-form
    ;; safepoint, 7/8=disable/enable collections, 9/10=disable/enable
    ;; free-list reuse, 11/12=enable/disable compaction.
    ;; Returns the list
    ;; (trip-count bad-cur bad-bt bad-want poison-count poison-enable
    ;;  context-depth mid-form-fired-count).
    (defun bf_gc_diag (args out)
      (seq
        (if (= (wf_argval args 0) 1) (ptr-write-u64 (data-addr nl_gc_diag) 32 1)
          (if (= (wf_argval args 0) 2) (ptr-write-u64 (data-addr nl_gc_diag) 32 0)
            (if (= (wf_argval args 0) 3) (nl_gc_ctx_push 0 0 0 0 0 0 0)
              (if (= (wf_argval args 0) 4) (nl_gc_ctx_pop)
                ;; Doc 152 §11.41 Stage 4b step 2: arm / disarm the mid-form
                ;; safepoint.  5 = arm (enable=1, next-trigger = total + 16 MiB,
                ;; reset fired-count); 6 = disarm (enable=0).  Default OFF.
                (if (= (wf_argval args 0) 5)
                    (seq (ptr-write-u64 (data-addr nl_safepoint_ctx) 8 1)
                         (ptr-write-u64 (data-addr nl_safepoint_ctx) 40
                                        (+ (ptr-read-u64 268436184 0) 16777216))
                         (ptr-write-u64 (data-addr nl_safepoint_ctx) 32 0))
                  (if (= (wf_argval args 0) 6)
                      (ptr-write-u64 (data-addr nl_safepoint_ctx) 8 0)
                    (if (= (wf_argval args 0) 7)
                        (ptr-write-u64 268435616 0 1)
                      (if (= (wf_argval args 0) 8)
                          (ptr-write-u64 268435616 0 0)
                        (if (= (wf_argval args 0) 9)
                            (ptr-write-u64 268435624 0 1)
                          (if (= (wf_argval args 0) 10)
                              (ptr-write-u64 268435624 0 0)
                            (if (= (wf_argval args 0) 11)
                                (ptr-write-u64 268435608 0 1)
                              (if (= (wf_argval args 0) 12)
                                  (ptr-write-u64 268435608 0 0)
                                0))))))))))))
        (let* ((nils (alloc-bytes 32 8)) (s7 (alloc-bytes 32 8)) (s6 (alloc-bytes 32 8)) (s5 (alloc-bytes 32 8)) (s4 (alloc-bytes 32 8))
               (s3 (alloc-bytes 32 8)) (s2 (alloc-bytes 32 8)) (s1 (alloc-bytes 32 8)))
          (seq
            (wf_write_nil nils)
            ;; 8th element (tail): mid-form safepoint fired-count (ctx+32).
            (wf_cons_int (ptr-read-u64 (data-addr nl_safepoint_ctx) 32) nils s7)
            (wf_cons_int (ptr-read-u64 (data-addr nl_safepoint_ctx) 0) s7 s6)
            (wf_cons_int (ptr-read-u64 (data-addr nl_gc_diag) 32) s6 s5)
            (wf_cons_int (ptr-read-u64 (data-addr nl_gc_diag) 40) s5 s4)
            (wf_cons_int (ptr-read-u64 (data-addr nl_gc_diag) 24) s4 s3)
            (wf_cons_int (ptr-read-u64 (data-addr nl_gc_diag) 16) s3 s2)
            (wf_cons_int (ptr-read-u64 (data-addr nl_gc_diag) 8) s2 s1)
            (wf_cons_int (ptr-read-u64 (data-addr nl_gc_diag) 0) s1 out)
            0))))
    (defun bf_arena_stats (out)
      (let* ((bump (ptr-read-u64 268435456 0))
             (used (bf_arena_chunks_used (ptr-read-u64 268436160 0) 0))
             (nil-slot (alloc-bytes 32 8))
             (s11 (alloc-bytes 32 8))
             (s10 (alloc-bytes 32 8))
             (s9 (alloc-bytes 32 8))
             (s8 (alloc-bytes 32 8))
             (s7 (alloc-bytes 32 8))
             (s6 (alloc-bytes 32 8))
             (s5 (alloc-bytes 32 8))
             (s4 (alloc-bytes 32 8))
             (s3 (alloc-bytes 32 8))
             (s2 (alloc-bytes 32 8))
             (s1 (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 268436192 0 used) ; chunk-bytes-used
         (ptr-write-u64 268436240 0 bump) ; chunk0.cursor mirrors bump offset
         (wf_write_nil nil-slot)
         (wf_cons_int used nil-slot s11)
         (wf_cons_int (ptr-read-u64 268436184 0) s11 s10)
         (wf_cons_int (ptr-read-u64 268436176 0) s10 s9)
         (wf_cons_int (ptr-read-u64 268435624 0) s9 s8)
         (wf_cons_int (ptr-read-u64 268435616 0) s8 s7)
         (wf_cons_int (ptr-read-u64 268435552 0) s7 s6)
         (wf_cons_int (ptr-read-u64 268435560 0) s6 s5)
         (wf_cons_int (ptr-read-u64 268435576 0) s5 s4)
         (wf_cons_int used s4 s3)
         (wf_cons_int bump s3 s2)
         (wf_cons_int (ptr-read-u64 268435672 0) s2 s1)
         (wf_cons_int 268435456 s1 out)
         0)))
    ;; flat-arena spike step 2 (READ-ONLY): full multi-chunk heap walk at
    ;; native speed.  Walks every chunk in the descriptor chain, and within
    ;; each chunk follows the [header][object] BLOCK_TOTAL sequence from the
    ;; chunk's data-start to its live end (`bf_arena_chunk_cursor', the same
    ;; cursor the GC mark/sweep uses), reaching the end EXACTLY.  Accumulates
    ;; into caller-supplied scratch slots; never mutates a heap object.  A
    ;; malformed block (BT < 8 or overrunning the chunk end) clears the OK
    ;; flag and ends that chunk's walk.  This verifies the invariant a
    ;; flat-arena dump relies on: the heap is a contiguous, self-describing
    ;; block sequence safe to bulk-copy.
    (defun bf_arena_wv_chunk (chunk cb cl cf cby cok)
      (let* ((cbase (ptr-read-u64 chunk 0))
             (end (+ cbase (bf_arena_chunk_cursor chunk)))
             (hdr (ptr-read-u64 (+ chunk 24) 0)))
        (seq
         (while (and (> hdr 0) (< hdr end))
           (let* ((bt (nl_hdr_bt hdr))
                  (mark (nl_hdr_mark hdr)))
             (if (< bt 8)
                 (seq (ptr-write-u64 cok 0 0) (setq hdr end))
               (if (> (+ hdr bt) end)
                   (seq (ptr-write-u64 cok 0 0) (setq hdr end))
                 (seq
                  (ptr-write-u64 cb 0 (+ (ptr-read-u64 cb 0) 1))
                  (ptr-write-u64 cby 0 (+ (ptr-read-u64 cby 0) bt))
                  (if (= mark 2)
                      (ptr-write-u64 cf 0 (+ (ptr-read-u64 cf 0) 1))
                    (ptr-write-u64 cl 0 (+ (ptr-read-u64 cl 0) 1)))
                  (setq hdr (+ hdr bt)))))))
         0)))
    (defun bf_arena_wv_chunks (chunk cb cl cf cby cok)
      (if (= chunk 0)
          0
        (nl_seq2 (bf_arena_wv_chunk chunk cb cl cf cby cok)
                 (bf_arena_wv_chunks (ptr-read-u64 (+ chunk 48) 0)
                                     cb cl cf cby cok))))
    (defun bf_arena_walk_verify (out)
      (let* ((cb (alloc-bytes 8 8)) (cl (alloc-bytes 8 8)) (cf (alloc-bytes 8 8))
             (cby (alloc-bytes 8 8)) (cok (alloc-bytes 8 8))
             (nil-slot (alloc-bytes 32 8))
             (s4 (alloc-bytes 32 8)) (s3 (alloc-bytes 32 8))
             (s2 (alloc-bytes 32 8)) (s1 (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 cb 0 0) (ptr-write-u64 cl 0 0) (ptr-write-u64 cf 0 0)
         (ptr-write-u64 cby 0 0) (ptr-write-u64 cok 0 1)
         (bf_arena_wv_chunks (ptr-read-u64 268436160 0) cb cl cf cby cok)
         (wf_write_nil nil-slot)
         ;; result list: (blocks live free bytes wellformed)
         (wf_cons_int (ptr-read-u64 cok 0) nil-slot s4)
         (wf_cons_int (ptr-read-u64 cby 0) s4 s3)
         (wf_cons_int (ptr-read-u64 cf 0) s3 s2)
         (wf_cons_int (ptr-read-u64 cl 0) s2 s1)
         (wf_cons_int (ptr-read-u64 cb 0) s1 out)
         0)))
    ;; flat-arena spike step 3a (READ-ONLY source): the bulk-copy mechanic
    ;; -- the "memcpy" half of memcpy+relocate.  Copy chunk-0's live region
    ;; [data-start, base+cursor) into a fresh large-object buffer (8-byte
    ;; words; > 4 KiB so it lands in its own mmap, NOT bumping the chunk
    ;; under the source), then verify byte-identity by re-reading both.  The
    ;; source heap is never mutated.  Returns (USED-BYTES MISMATCH-WORDS
    ;; DEST-PTR): MISMATCH-WORDS = 0 means the copy is byte-faithful.
    ;; cursor / used are snapshot BEFORE the dest + scratch allocs, so the
    ;; copied range excludes them.  Multi-chunk copy + the pointer-swizzle
    ;; pass (step 3b) and file emit are deferred.
    (defun bf_arena_dump_copy_verify (out)
      (let* ((head (ptr-read-u64 268436160 0))
             (sstart (ptr-read-u64 (+ head 24) 0))
             (cursor (bf_arena_chunk_cursor head))
             (slen (if (< cursor 1024) 0 (- cursor 1024)))
             (dest (alloc-bytes slen 8))
             (i 0) (mism 0)
             (nil-slot (alloc-bytes 32 8))
             (s2 (alloc-bytes 32 8)) (s1 (alloc-bytes 32 8)))
        (seq
         (while (< i slen)
           (seq (ptr-write-u64 (+ dest i) 0 (ptr-read-u64 (+ sstart i) 0))
                (setq i (+ i 8))))
         (setq i 0)
         (while (< i slen)
           (seq (if (= (ptr-read-u64 (+ dest i) 0) (ptr-read-u64 (+ sstart i) 0))
                    0
                  (setq mism (+ mism 1)))
                (setq i (+ i 8))))
         (wf_write_nil nil-slot)
         (wf_cons_int dest nil-slot s2)
         (wf_cons_int mism s2 s1)
         (wf_cons_int slen s1 out)
         0)))
    ;; flat-arena spike step 3b-i (root-reachability precondition for the
    ;; pointer swizzle): prove the published root set reaches the whole live
    ;; object set, so a from-roots per-type walk that records pointer fields
    ;; (the actual swizzle, step 3b-ii) will cover every object the dump
    ;; bulk-copies.  REUSES the battle-tested GC mark-from-roots
    ;; (`nl_gc_mark_published_contexts' / `-rootstack' / `-symentry', the same
    ;; calls `nl_gc_collect_published' makes) so no per-type walker is
    ;; re-implemented; then linearly counts the marked (reachable) blocks and
    ;; CLEARS the marks back to 0, leaving the heap exactly as before (NO
    ;; sweep -> nothing freed).  The GC-in-progress flag (ctx+24) is held so a
    ;; mid-walk safepoint cannot re-enter.  Returns (REACHABLE TOTAL):
    ;; REACHABLE ~= LIVE from `bf_arena_walk_verify' confirms root coverage.
    (defun bf_arena_mr_chunk (chunk reach total)
      (let* ((cbase (ptr-read-u64 chunk 0))
             (end (+ cbase (bf_arena_chunk_cursor chunk)))
             (hdr (ptr-read-u64 (+ chunk 24) 0)))
        (seq
         (while (and (> hdr 0) (< hdr end))
           (let ((bt (nl_hdr_bt hdr)))
             (if (< bt 8)
                 (setq hdr end)
               (if (> (+ hdr bt) end)
                   (setq hdr end)
                 (seq
                  (ptr-write-u64 total 0 (+ (ptr-read-u64 total 0) 1))
                  (if (= (nl_hdr_mark hdr) 1)
                      (seq (ptr-write-u64 reach 0 (+ (ptr-read-u64 reach 0) 1))
                           (nl_hdr_set_mark hdr 0))
                    0)
                  (setq hdr (+ hdr bt)))))))
         0)))
    (defun bf_arena_mr_chunks (chunk reach total)
      (if (= chunk 0)
          0
        (nl_seq2 (bf_arena_mr_chunk chunk reach total)
                 (bf_arena_mr_chunks (ptr-read-u64 (+ chunk 48) 0)
                                     reach total))))
    (defun bf_arena_mark_reach_verify (out)
      (let* ((reach (alloc-bytes 8 8)) (total (alloc-bytes 8 8))
             (nil-slot (alloc-bytes 32 8)) (s1 (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 reach 0 0)
         (ptr-write-u64 total 0 0)
         (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 1)
         (nl_gc_mark_published_contexts)
         (nl_gc_mark_rootstack)
         (nl_gc_mark_symentry)
         (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 0)
         (bf_arena_mr_chunks (ptr-read-u64 268436160 0) reach total)
         (wf_write_nil nil-slot)
         (wf_cons_int (ptr-read-u64 total 0) nil-slot s1)
         (wf_cons_int (ptr-read-u64 reach 0) s1 out)
         0)))
    ;; flat-arena spike step 3b-ii: the pointer SWIZZLE proper.  Mirrors the
    ;; GC per-type walk (`nl_gc_mark_slot' / `-cons' / `-vec_slots') from the
    ;; frame[0] globals root, but at every pointer FIELD it rewrites the
    ;; corresponding word IN THE COPIED chunk-0 buffer from an absolute
    ;; address to an arena-relative OFFSET (in-place; no separate pointer-list
    ;; storage).  The SOURCE heap is never written -- only the dest copy.
    ;; Cycle dedup reuses the GC mark bit (`nl_gc_mark_block'); marks are
    ;; cleared (no sweep) between passes.  Verified by a round trip: swizzle
    ;; (dir 0) then unswizzle (dir 1) must restore the copy byte-for-byte to
    ;; the source.  Returns (IN-REGION-PTRS OUT-REGION-PTRS ROUNDTRIP-MISMATCH);
    ;; MISMATCH = 0 proves the per-type walk touched exactly the pointer fields,
    ;; reversibly.  (Single-root + chunk-0 only; multi-root / multi-chunk and
    ;; the load-side unswizzle into a fresh base are the remaining work.)
    ;;
    ;; nl_fa_field: swizzle/unswizzle ONE pointer word living at source addr
    ;; WADDR whose value is TGT.  DS/SPAN = chunk-0 data-start / used span;
    ;; DEST = image base.  Only fields whose ADDRESS is inside chunk-0 are
    ;; touched (every pointer field lives in a chunk-0 object; root slots live
    ;; in the EvalCtx, outside chunk-0 -> skipped, reinstalled on load).
    ;;
    ;; step 3c multi-region: the IMAGE holds two regions -- chunk-0 at image
    ;; offset [0,span), then the interned symbol-name region at [span, ...).
    ;; A target is classified into chunk-0, interned, or out (large objects /
    ;; other).  The interned region bounds are read from the chunk-0 metadata
    ;; relative to DS (base = ds - 1024; intern base @ base+832, end @ base+840),
    ;; so no extra params thread through the walk.  `cin' counts pointers that
    ;; landed in the image (chunk-0 + interned); `cout' counts the rest.
    ;;   dir 0 swizzle:   chunk-0 -> (tgt - ds);  interned -> (span + (tgt - ib))
    ;;   dir 2 relocate:  chunk-0 -> dest + (tgt - ds);
    ;;                    interned -> dest + span + (tgt - ib)
    ;;   dir 1 unswizzle: restore every touched word from the source (idempotent)
    ;; Emit one in-image pointer field.  LOC = its slot in DEST; IMGOFF = the
    ;; target's offset within the image (chunk-0 or interned, already encoded);
    ;; FLDOFF = the field's own chunk-0 offset (= waddr - ds).  CIN counts +
    ;; doubles as the relocation-table write index.  dir 0 swizzle (write
    ;; IMGOFF), dir 2 relocate (write DEST+IMGOFF), dir 3 = dir 0 PLUS append
    ;; FLDOFF to the relocation table (boot step: the table that drives a
    ;; per-field linear relocate on load, with no graph walk).  The table lives
    ;; in DEST after the two regions + a 256-byte counter pad.
    ;; ---- multi-chunk coalescing helpers (boot-wiring 14) ----
    ;; The arena is a chain of chunks (head @268436160, desc.next @+48).  A
    ;; multi-chunk heap is coalesced into ONE logical address space at dump time:
    ;; logical_base(chunk_0)=0, logical_base(chunk_K)=sum(used of chunks < K).  A
    ;; physical address -> logical offset (nl_mc_logoff) and back (nl_mc_phys); the
    ;; load then treats the coalesced image as a single chunk (slen=total), reusing
    ;; the existing single-chunk cold-load unchanged.  For a SINGLE chunk these
    ;; reduce to (addr - chunk0-data-start), so nl_fa_field/nl_fa_emit stay
    ;; byte-identical for the verified single-chunk path.
    (defun nl_mc_logoff_walk (chunk addr acc)
      (if (= chunk 0) -1
        (let* ((cbase (ptr-read-u64 chunk 0))
               (cds (+ cbase 1024))
               (cused (bf_arena_chunk_used chunk)))
          (if (if (< addr cds) 0 (if (< addr (+ cds cused)) 1 0))
              (+ acc (- addr cds))
            (nl_mc_logoff_walk (ptr-read-u64 (+ chunk 48) 0) addr (+ acc cused))))))
    (defun nl_mc_logoff (addr)
      (nl_mc_logoff_walk (ptr-read-u64 268436160 0) addr 0))
    (defun nl_mc_phys_walk (chunk logoff acc)
      (if (= chunk 0) 0
        (let* ((cbase (ptr-read-u64 chunk 0))
               (cused (bf_arena_chunk_used chunk)))
          (if (if (< logoff acc) 0 (if (< logoff (+ acc cused)) 1 0))
              (+ (+ cbase 1024) (- logoff acc))
            (nl_mc_phys_walk (ptr-read-u64 (+ chunk 48) 0) logoff (+ acc cused))))))
    (defun nl_mc_phys (logoff)
      (nl_mc_phys_walk (ptr-read-u64 268436160 0) logoff 0))
    (defun nl_mc_total_walk (chunk acc)
      (if (= chunk 0) acc
        (nl_mc_total_walk (ptr-read-u64 (+ chunk 48) 0)
                          (+ acc (bf_arena_chunk_used chunk)))))
    (defun nl_mc_total ()
      (nl_mc_total_walk (ptr-read-u64 268436160 0) 0))
    ;; image offset of ADDR for the coalesced image: chunk -> logical offset;
    ;; intern -> total + (addr-ib); else 0.
    (defun nl_mc_imgoff (addr total ib ie)
      (let ((lo (nl_mc_logoff addr)))
        (if (< lo 0)
            (if (if (< addr ib) 0 (if (< addr ie) 1 0)) (+ total (- addr ib)) 0)
          lo)))
    ;; stream each chunk's live region [cds,cds+used) to FD in chain (=logical) order.
    (defun nl_mc_write_chunks (fd chunk)
      (if (= chunk 0) 0
        (nl_seq2
         (nl_fa_write_all fd (+ (ptr-read-u64 chunk 0) 1024)
                          (bf_arena_chunk_used chunk) 0)
         (nl_mc_write_chunks fd (ptr-read-u64 (+ chunk 48) 0)))))
    ;; in-place un-swizzle (offset -> pointer) over the LIVE multi-chunk arena via
    ;; the table of logical field offsets.  For 1 chunk this equals the single-chunk
    ;; restore (nl_mc_phys = sstart + off).
    (defun bf_arena_inplace_restore_mc (tbl tlen ib total)
      (let ((i 0))
        (seq
         (while (< i tlen)
           (let* ((f (ptr-read-u64 (+ tbl (* i 8)) 0))
                  (faddr (nl_mc_phys f))
                  (o (ptr-read-u64 faddr 0)))
             (nl_seq2
              (if (< o total)
                  (ptr-write-u64 faddr 0 (nl_mc_phys o))
                (ptr-write-u64 faddr 0 (+ ib (- o total))))
              (setq i (+ i 1)))))
         0)))
    (defun nl_fa_emit (loc imgoff fldoff dest span ib ie cin dir)
      (let ((tbl (if (= (ptr-read-u64 (data-addr nl_fa_tbl_base) 0) 0)
                     (+ dest (+ span (+ (nl_align_up (if (< ie ib) 0 (- ie ib)) 8) 256)))
                   (ptr-read-u64 (data-addr nl_fa_tbl_base) 0))))
        (nl_seq2
         (if (= dir 3)
             (ptr-write-u64 (+ tbl (* 8 (ptr-read-u64 cin 0))) 0 fldoff)
           0)
         (nl_seq2
          (ptr-write-u64 cin 0 (+ (ptr-read-u64 cin 0) 1))
          (ptr-write-u64 loc 0 (if (= dir 2) (+ dest imgoff) imgoff))))))
    ;; chunk-map-aware: a field at WADDR (in ANY chunk) recording its LOGICAL offset,
    ;; targets resolved to logical (chunk) or total+(tgt-ib) (intern).  For 1 chunk
    ;; nl_mc_logoff(addr) = addr-ds, so this is byte-identical to the prior version.
    (defun nl_fa_field (waddr tgt ds span dest cin cout dir)
      (let ((wlog (nl_mc_logoff waddr)))
        (if (< wlog 0) 0
          (let* ((loc (+ dest (- waddr ds)))
                 (base (- ds 1024))
                 (ib (ptr-read-u64 (+ base 832) 0))
                 (ie (ptr-read-u64 (+ base 840) 0))
                 (tlog (nl_mc_logoff tgt)))
            (if (= dir 1)
                (ptr-write-u64 loc 0 (ptr-read-u64 waddr 0))
              (if (< tlog 0)
                  (if (if (< tgt ib) 0 (if (< tgt ie) 1 0))
                      (nl_fa_emit loc (+ span (- tgt ib)) wlog dest span ib ie cin dir)
                    (ptr-write-u64 cout 0 (+ (ptr-read-u64 cout 0) 1)))
                (nl_fa_emit loc tlog wlog dest span ib ie cin dir)))))))
    (defun nl_fa_vec_slots (data_ptr i len ds span dest cin cout dir)
      (if (= (nl_gc_in_arena data_ptr) 0) 0
        (let ((k i))
          (while (< k len)
            (nl_seq2
             (let ((vw (ptr-read-u64 (+ data_ptr (* k 8)) 0)))
               (if (= (logand vw 1) 1) 0
                 (nl_seq2 (nl_fa_field (+ data_ptr (* k 8)) vw ds span dest cin cout dir)
                          (if (= (nl_gc_mark_block vw) 0) 0
                            (nl_fa_slot vw ds span dest cin cout dir)))))
             (setq k (+ k 1))))
          0)))
    (defun nl_fa_cons (sp ds span dest cin cout dir)
      (let ((box (ptr-read-u64 sp 8)))
        (nl_seq2 (nl_fa_field (+ sp 8) box ds span dest cin cout dir)
          (if (= (nl_gc_mark_block box) 0) 0
            (nl_seq2
             (let ((cw (ptr-read-u64 box 0)))
               (if (= (logand cw 1) 1) 0
                 (nl_seq2 (nl_fa_field box cw ds span dest cin cout dir)
                          (if (= (nl_gc_mark_block cw) 0) 0
                            (nl_fa_slot cw ds span dest cin cout dir)))))
             (let ((dw (ptr-read-u64 box 8)))
               (if (= (logand dw 1) 1) 0
                 (nl_seq2 (nl_fa_field (+ box 8) dw ds span dest cin cout dir)
                          (if (= (nl_gc_mark_block dw) 0) 0
                            (if (= (ptr-read-u8 dw 0) 7)
                                (nl_fa_cons dw ds span dest cin cout dir)
                              (nl_fa_slot dw ds span dest cin cout dir)))))))))))
    (defun nl_fa_slot (sp ds span dest cin cout dir)
      (let ((tag (ptr-read-u8 sp 0)))
        (if (= tag 7)
            (nl_fa_cons sp ds span dest cin cout dir)
          (if (= tag 8)
              (let ((box (ptr-read-u64 sp 8)))
                (nl_seq2 (nl_fa_field (+ sp 8) box ds span dest cin cout dir)
                  (if (= (nl_gc_mark_block box) 0) 0
                    (let ((data_ptr (ptr-read-u64 box 8)) (len (ptr-read-u64 box 16)))
                      (nl_seq2 (nl_fa_field (+ box 8) data_ptr ds span dest cin cout dir)
                               (nl_fa_vec_slots data_ptr 0 len ds span dest cin cout dir))))))
            (if (= tag 12)
                (let ((box (ptr-read-u64 sp 8)))
                  (nl_seq2 (nl_fa_field (+ sp 8) box ds span dest cin cout dir)
                    (if (= (nl_gc_mark_block box) 0) 0
                      (let ((data_ptr (ptr-read-u64 box 40)) (len (ptr-read-u64 box 48)))
                        (nl_seq2 (nl_fa_slot box ds span dest cin cout dir)
                          (nl_seq2 (nl_fa_field (+ box 40) data_ptr ds span dest cin cout dir)
                                   (nl_fa_vec_slots data_ptr 0 len ds span dest cin cout dir)))))))
              (if (= tag 11)
                  (let ((box (ptr-read-u64 sp 8)))
                    (nl_seq2 (nl_fa_field (+ sp 8) box ds span dest cin cout dir)
                      (if (= (nl_gc_mark_block box) 0) 0
                        (let ((vw (ptr-read-u64 box 0)))
                          (if (= (logand vw 1) 1) 0
                            (nl_seq2 (nl_fa_field box vw ds span dest cin cout dir)
                              (if (= (nl_gc_mark_block vw) 0) 0
                                (nl_fa_slot vw ds span dest cin cout dir))))))))
                (if (= tag 6)
                    (let ((box (ptr-read-u64 sp 8)))
                      (nl_seq2 (nl_fa_field (+ sp 8) box ds span dest cin cout dir)
                        (if (= (nl_gc_mark_block box) 0) 0
                          (nl_fa_field (+ box 8) (ptr-read-u64 box 8) ds span dest cin cout dir))))
                  (if (= tag 5)
                      (nl_fa_field (+ sp 16) (ptr-read-u64 sp 16) ds span dest cin cout dir)
                    (if (= tag 4)
                        (nl_fa_field (+ sp 16) (ptr-read-u64 sp 16) ds span dest cin cout dir)
                      (if (= tag 9)
                          (nl_fa_field (+ sp 8) (ptr-read-u64 sp 8) ds span dest cin cout dir)
                        (if (= tag 10)
                            (nl_fa_field (+ sp 8) (ptr-read-u64 sp 8) ds span dest cin cout dir)
                          0)))))))))))
    ;; step 3c (all roots): walk every published frame's roots + the shared
    ;; symentry, mirroring `nl_gc_mark_published_frame' / `-contexts_from' /
    ;; `nl_gc_mark_symentry' so the swizzle/relocate reaches the COMPLETE live
    ;; graph (globals + frame stack + unbound marker + the per-frame reader
    ;; transients result/out/src/cursor/bsym), not just frame[0] globals.
    (defun nl_fa_frame_at (base ds span dest cin cout dir)
      (let ((env (ptr-read-u64 base 0)))
        (nl_seq2 (nl_fa_slot (+ env 0) ds span dest cin cout dir)
         (nl_seq2 (nl_fa_slot (+ env 32) ds span dest cin cout dir)
          (nl_seq2 (nl_fa_slot (+ env 64) ds span dest cin cout dir)
           (nl_seq2 (nl_fa_slot (ptr-read-u64 base 8) ds span dest cin cout dir)
            (nl_seq2 (nl_fa_slot (ptr-read-u64 base 16) ds span dest cin cout dir)
             (nl_seq2 (nl_fa_slot (ptr-read-u64 base 32) ds span dest cin cout dir)
              (nl_seq2 (nl_fa_slot (ptr-read-u64 base 40) ds span dest cin cout dir)
                       (nl_fa_slot (ptr-read-u64 base 48) ds span dest cin cout dir))))))))))
    (defun nl_fa_frames_from (i depth ds span dest cin cout dir)
      (if (< i depth)
          (nl_seq2
           (nl_fa_frame_at (+ (data-addr nl_safepoint_ctx) (+ 64 (* i 56)))
                           ds span dest cin cout dir)
           (nl_fa_frames_from (+ i 1) depth ds span dest cin cout dir))
        0))
    (defun nl_fa_roots (ds span dest cin cout dir)
      (nl_seq2
       (nl_fa_frames_from 0 (ptr-read-u64 (data-addr nl_safepoint_ctx) 0)
                          ds span dest cin cout dir)
       (if (= (ptr-read-u64 268436328 0) 0) 0
         (nl_fa_slot (ptr-read-u64 268436328 0) ds span dest cin cout dir))))
    (defun bf_arena_swizzle_verify (out)
      (let* ((head (ptr-read-u64 268436160 0))
             (sstart (ptr-read-u64 (+ head 24) 0))
             (cursor (bf_arena_chunk_cursor head))
             (slen (if (< cursor 1024) 0 (- cursor 1024)))
             ;; dest holds the copied region in [0,slen); the verification
             ;; counters live in the tail [slen, slen+256) of the SAME buffer
             ;; so they are never inside the compared region nor written by
             ;; the field-swizzle (which only writes dest+[0,slen)).
             (dest (alloc-bytes (+ slen 256) 8))
             (cin (+ dest slen)) (cout (+ dest slen 8)) (cmis (+ dest slen 16))
             (ctr (+ dest slen 24))
             (depth (ptr-read-u64 (data-addr nl_safepoint_ctx) 0))
             (env (if (= depth 0) 0
                    (ptr-read-u64 (+ (data-addr nl_safepoint_ctx) 64) 0)))
             (i 0)
             (nil-slot (alloc-bytes 32 8)) (s2 (alloc-bytes 32 8)) (s1 (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 cin 0 0) (ptr-write-u64 cout 0 0) (ptr-write-u64 cmis 0 0)
         (ptr-write-u64 ctr 0 0)
         ;; 1. bulk-copy chunk-0 live region
         (while (< i slen)
           (seq (ptr-write-u64 (+ dest i) 0 (ptr-read-u64 (+ sstart i) 0))
                (setq i (+ i 8))))
         (if (= env 0) 0
           (seq
            ;; 2. swizzle pass (abs -> offset) from ALL roots
            (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 1)
            (nl_fa_roots sstart slen dest cin cout 0)
            (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 0)
            (bf_arena_mr_chunks (ptr-read-u64 268436160 0) ctr ctr)   ; clear marks
            ;; 3. unswizzle pass (offset -> abs)
            (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 1)
            (nl_fa_roots sstart slen dest cin cout 1)
            (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 0)
            (bf_arena_mr_chunks (ptr-read-u64 268436160 0) ctr ctr)
            ;; 4. round-trip identity: copy must equal source again
            (setq i 0)
            (while (< i slen)
              (seq (if (= (ptr-read-u64 (+ dest i) 0) (ptr-read-u64 (+ sstart i) 0)) 0
                     (ptr-write-u64 cmis 0 (+ (ptr-read-u64 cmis 0) 1)))
                   (setq i (+ i 8))))))
         (wf_write_nil nil-slot)
         (wf_cons_int (ptr-read-u64 cmis 0) nil-slot s2)
         (wf_cons_int (ptr-read-u64 cout 0) s2 s1)
         (wf_cons_int (ptr-read-u64 cin 0) s1 out)
         0)))
    ;; flat-arena spike step 4 (core LOAD mechanic): relocate the image into a
    ;; FRESH base and prove the result is a structurally valid arena.  Copy
    ;; chunk-0 into DEST (so DEST itself is the new base = where data-start now
    ;; lives), then rebase every in-region pointer to DEST via the per-type
    ;; walk (dir 2: DEST[field] = tgt + (dest - sstart)).  This is exactly what
    ;; loading the relocatable image at a kernel-chosen base does.  Then verify
    ;; the LOADED image is well-formed: a linear [header][object] walk of DEST
    ;; must reach the end exactly (relocate only rewrote pointer fields, never
    ;; the BLOCK_TOTAL headers).  Returns (RELOCATED-PTRS BLOCKS WELLFORMED);
    ;; RELOCATED-PTRS should match the swizzle's in-region count and WELLFORMED
    ;; = 1 means the load produced a sound arena at the new base.  (Single-root
    ;; + chunk-0; the boot hook that installs a loaded image in place of the
    ;; source replay is the remaining integration -- it needs the multi-region
    ;; / multi-root completeness of step 3c first.)
    (defun bf_arena_load_relocate_verify (out)
      (let* ((head (ptr-read-u64 268436160 0))
             (sstart (ptr-read-u64 (+ head 24) 0))
             (cursor (bf_arena_chunk_cursor head))
             (slen (if (< cursor 1024) 0 (- cursor 1024)))
             ;; step 3c: the image is two regions -- chunk-0 [0,slen) then the
             ;; interned symbol-name region [slen, slen+isz).  Copy BOTH so the
             ;; relocated interned pointers (dest+slen+off) land on real data.
             (abase (- sstart 1024))
             (ib (ptr-read-u64 (+ abase 832) 0))
             (ie (ptr-read-u64 (+ abase 840) 0))
             (isz (nl_align_up (if (< ie ib) 0 (- ie ib)) 8))
             (dest (alloc-bytes (+ (+ slen isz) 256) 8))
             (crel (+ dest slen isz)) (cwf (+ dest slen isz 8))
             (cnblk (+ dest slen isz 16)) (ctr (+ dest slen isz 24))
             (depth (ptr-read-u64 (data-addr nl_safepoint_ctx) 0))
             (env (if (= depth 0) 0
                    (ptr-read-u64 (+ (data-addr nl_safepoint_ctx) 64) 0)))
             (i 0) (j 0) (hdr 0)
             (nil-slot (alloc-bytes 32 8)) (s2 (alloc-bytes 32 8)) (s1 (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 crel 0 0) (ptr-write-u64 cwf 0 1) (ptr-write-u64 cnblk 0 0)
         ;; 1. copy chunk-0 live region into DEST (DEST is the new base)
         (while (< i slen)
           (seq (ptr-write-u64 (+ dest i) 0 (ptr-read-u64 (+ sstart i) 0))
                (setq i (+ i 8))))
         ;; 1b. copy the interned region into DEST[slen .. slen+isz)
         (while (< j isz)
           (seq (ptr-write-u64 (+ dest (+ slen j)) 0 (ptr-read-u64 (+ ib j) 0))
                (setq j (+ j 8))))
         (if (= env 0) 0
           (seq
            ;; 2. relocate DEST's in-region pointers to base = dest (dir 2),
            ;; from ALL roots
            (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 1)
            (nl_fa_roots sstart slen dest crel ctr 2)
            (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 0)
            (bf_arena_mr_chunks (ptr-read-u64 268436160 0) ctr ctr)
            ;; 3. verify the LOADED image: linear [hdr][obj] walk of DEST must
            ;; reach the end exactly (block headers untouched by relocate).
            (setq hdr 0)
            (while (and (= (ptr-read-u64 cwf 0) 1) (< hdr slen))
              (let* ((h (ptr-read-u64 (+ dest hdr) 0))
                     (bt (- h (logand h 7))))
                (if (< bt 8)
                    (nl_seq2 (ptr-write-u64 cwf 0 0) (setq hdr slen))
                  (if (> (+ hdr bt) slen)
                      (nl_seq2 (ptr-write-u64 cwf 0 0) (setq hdr slen))
                    (nl_seq2 (ptr-write-u64 cnblk 0 (+ (ptr-read-u64 cnblk 0) 1))
                             (setq hdr (+ hdr bt)))))))
            ;; require reaching the end exactly
            (if (= hdr slen) 0 (ptr-write-u64 cwf 0 0))))
         (wf_write_nil nil-slot)
         (wf_cons_int (ptr-read-u64 cwf 0) nil-slot s2)
         (wf_cons_int (ptr-read-u64 cnblk 0) s2 s1)
         (wf_cons_int (ptr-read-u64 crel 0) s1 out)
         0)))
    ;; flat-arena spike step 4-boot precondition: capture the ROOT METADATA the
    ;; boot loader needs.  An image is loaded by mmap'ing a fresh arena, copying
    ;; the regions, relocating the pointers (steps done), then pointing a fresh
    ;; EvalCtx's roots at the loaded root objects -- which requires each root's
    ;; IMAGE OFFSET (where, within the image, its boxed object lives).
    ;; `nl_img_off' maps an absolute address to its image offset (chunk-0 ->
    ;; addr-ds; interned -> span + addr-ib; out -> -1).
    ;; `bf_arena_image_root_verify' reports the image offsets of the 3 EvalCtx
    ;; roots (globals_record @ env+0, frames_record @ env+32, unbound_marker @
    ;; env+64; each a 32B Sexp slot whose boxed payload pointer is at slot+8).
    ;; A non-negative offset means the root lives inside the dumped image and
    ;; the boot can relocate + reinstall it as `newbase + offset'.  READ-ONLY.
    (defun nl_img_off (addr ds span ib ie)
      (if (if (< addr ds) 0 (if (< addr (+ ds span)) 1 0))
          (- addr ds)
        (if (if (< addr ib) 0 (if (< addr ie) 1 0))
            (+ span (- addr ib))
          -1)))
    (defun bf_arena_image_root_verify (out)
      (let* ((head (ptr-read-u64 268436160 0))
             (sstart (ptr-read-u64 (+ head 24) 0))
             (cursor (bf_arena_chunk_cursor head))
             (slen (if (< cursor 1024) 0 (- cursor 1024)))
             (base (- sstart 1024))
             (ib (ptr-read-u64 (+ base 832) 0))
             (ie (ptr-read-u64 (+ base 840) 0))
             (depth (ptr-read-u64 (data-addr nl_safepoint_ctx) 0))
             (env (if (= depth 0) 0
                    (ptr-read-u64 (+ (data-addr nl_safepoint_ctx) 64) 0)))
             (gbox (if (= env 0) 0 (ptr-read-u64 (+ env 8) 0)))
             (fbox (if (= env 0) 0 (ptr-read-u64 (+ env 40) 0)))
             (ubox (if (= env 0) 0 (ptr-read-u64 (+ env 72) 0)))
             (nil-slot (alloc-bytes 32 8)) (s2 (alloc-bytes 32 8)) (s1 (alloc-bytes 32 8)))
        (seq
         (wf_write_nil nil-slot)
         (wf_cons_int (nl_img_off ubox sstart slen ib ie) nil-slot s2)
         (wf_cons_int (nl_img_off fbox sstart slen ib ie) s2 s1)
         (wf_cons_int (nl_img_off gbox sstart slen ib ie) s1 out)
         0)))
    ;; flat-arena spike step 4-boot (table-driven load): the boot loader copies
    ;; the regions, then relocates with a flat RELOCATION TABLE -- no per-type
    ;; graph walk on load.  `bf_arena_dump_table_verify' demonstrates the whole
    ;; thing in-process: build the offset image AND its table (swizzle dir 3
    ;; records every pointer field's chunk-0 offset), then do the LOAD exactly
    ;; as the boot would -- for each table entry F: DEST[F] += DEST (the image
    ;; offset already encodes which region the target is in, so a single
    ;; `+ DEST' lands it in chunk-0 or interned within DEST).  Then verify the
    ;; loaded image is well-formed.  Returns (TABLE-LEN OUT-OF-REGION BLOCKS
    ;; WELLFORMED): OUT-OF-REGION 0 + WELLFORMED 1 means the table fully
    ;; describes the relocation and the table-driven load rebuilds a sound arena.
    (defun bf_arena_dump_table_verify (out)
      (let* ((head (ptr-read-u64 268436160 0))
             (sstart (ptr-read-u64 (+ head 24) 0))
             (cursor (bf_arena_chunk_cursor head))
             (slen (if (< cursor 1024) 0 (- cursor 1024)))
             (abase (- sstart 1024))
             (ib (ptr-read-u64 (+ abase 832) 0))
             (ie (ptr-read-u64 (+ abase 840) 0))
             (isz (nl_align_up (if (< ie ib) 0 (- ie ib)) 8))
             (tcap (* 8 200000))
             (dest (alloc-bytes (+ slen (+ isz (+ 256 tcap))) 8))
             (tbl (+ dest (+ slen (+ isz 256))))
             (cin (+ dest slen isz)) (cout (+ dest slen isz 8))
             (cwf (+ dest slen isz 16)) (cnblk (+ dest slen isz 24))
             (i 0) (j 0) (hdr 0) (ti 0) (n 0)
             (nil-slot (alloc-bytes 32 8)) (s3 (alloc-bytes 32 8))
             (s2 (alloc-bytes 32 8)) (s1 (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 cin 0 0) (ptr-write-u64 cout 0 0)
         (ptr-write-u64 cwf 0 1) (ptr-write-u64 cnblk 0 0)
         ;; 1. copy chunk-0 + interned into DEST
         (while (< i slen)
           (seq (ptr-write-u64 (+ dest i) 0 (ptr-read-u64 (+ sstart i) 0))
                (setq i (+ i 8))))
         (while (< j isz)
           (seq (ptr-write-u64 (+ dest (+ slen j)) 0 (ptr-read-u64 (+ ib j) 0))
                (setq j (+ j 8))))
         ;; 2. swizzle DEST + emit the relocation table (dir 3)
         (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 1)
         (nl_fa_roots sstart slen dest cin cout 3)
         (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 0)
         (bf_arena_mr_chunks (ptr-read-u64 268436160 0) cnblk cnblk)
         (ptr-write-u64 cnblk 0 0)
         ;; 3. LOAD as the boot would: per table entry F, DEST[F] += DEST
         (setq n (ptr-read-u64 cin 0))
         (setq ti 0)
         (while (< ti n)
           (let ((f (ptr-read-u64 (+ tbl (* ti 8)) 0)))
             (ptr-write-u64 (+ dest f) 0 (+ dest (ptr-read-u64 (+ dest f) 0))))
           (setq ti (+ ti 1)))
         ;; 4. verify the loaded image is well-formed (chunk-0 block walk)
         (setq hdr 0)
         (while (and (= (ptr-read-u64 cwf 0) 1) (< hdr slen))
           (let* ((h (ptr-read-u64 (+ dest hdr) 0))
                  (bt (- h (logand h 7))))
             (if (< bt 8)
                 (nl_seq2 (ptr-write-u64 cwf 0 0) (setq hdr slen))
               (if (> (+ hdr bt) slen)
                   (nl_seq2 (ptr-write-u64 cwf 0 0) (setq hdr slen))
                 (nl_seq2 (ptr-write-u64 cnblk 0 (+ (ptr-read-u64 cnblk 0) 1))
                          (setq hdr (+ hdr bt)))))))
         (if (= hdr slen) 0 (ptr-write-u64 cwf 0 0))
         (wf_write_nil nil-slot)
         (wf_cons_int (ptr-read-u64 cwf 0) nil-slot s3)
         (wf_cons_int (ptr-read-u64 cnblk 0) s3 s2)
         (wf_cons_int (ptr-read-u64 cout 0) s2 s1)
         (wf_cons_int (ptr-read-u64 cin 0) s1 out)
         0)))
    ;; flat-arena boot-wiring (2): FILE persistence.  The cold-start image is
    ;; written to / read from a file as {64B header | table | regions}:
    ;;   header: magic@0, slen@8, isz@16, tlen@24, globals_off@32, frames_off@40,
    ;;           unbound_off@48.
    ;;   table:  tlen u64 field offsets (the relocation table).
    ;;   regions: chunk-0 [0,slen) then interned [slen, slen+isz).
    ;; Low-level `nl_os_*' handles are used directly (the high-level `rdf' caps
    ;; at 8 MiB; the image is tens of MiB).  Partial reads/writes are looped.
    (defun nl_fa_write_all (fd ptr len off)
      (if (if (< off len) 0 1) off
        (let ((w (nl_os_write_file_handle fd (+ ptr off) (- len off))))
          (if (< w 1) off (nl_fa_write_all fd ptr len (+ off w))))))
    (defun nl_fa_read_all (fd buf len off)
      (if (if (< off len) 0 1) off
        (let ((r (nl_os_read_file_handle fd (+ buf off) (- len off))))
          (if (< r 1) off (nl_fa_read_all fd buf len (+ off r))))))
    (defun bf_arena_dump_image_to_file (args out)
      (let* ((cpath (nl_bi_make_cpath (wf_arg_ptr args 0)))
             (head (ptr-read-u64 268436160 0))
             (sstart (ptr-read-u64 (+ head 24) 0))
             (cursor (bf_arena_chunk_cursor head))
             (slen (if (< cursor 1024) 0 (- cursor 1024)))
             (abase (- sstart 1024))
             (ib (ptr-read-u64 (+ abase 832) 0))
             (ie (ptr-read-u64 (+ abase 840) 0))
             (isz (nl_align_up (if (< ie ib) 0 (- ie ib)) 8))
             (depth (ptr-read-u64 (data-addr nl_safepoint_ctx) 0))
             (env (if (= depth 0) 0
                    (ptr-read-u64 (+ (data-addr nl_safepoint_ctx) 64) 0)))
             (gbox (if (= env 0) 0 (ptr-read-u64 (+ env 8) 0)))
             (fbox (if (= env 0) 0 (ptr-read-u64 (+ env 40) 0)))
             (ubox (if (= env 0) 0 (ptr-read-u64 (+ env 72) 0)))
             (tcap (* 8 200000))
             (dest (alloc-bytes (+ slen (+ isz (+ 256 tcap))) 8))
             (tbl (+ dest (+ slen (+ isz 256))))
             (cin (+ dest slen isz)) (cout (+ dest slen isz 8))
             (hdr (alloc-bytes 64 8))
             (i 0) (j 0) (tlen 0) (fd 0))
        (seq
         (ptr-write-u64 cin 0 0) (ptr-write-u64 cout 0 0)
         (while (< i slen)
           (seq (ptr-write-u64 (+ dest i) 0 (ptr-read-u64 (+ sstart i) 0)) (setq i (+ i 8))))
         (while (< j isz)
           (seq (ptr-write-u64 (+ dest (+ slen j)) 0 (ptr-read-u64 (+ ib j) 0)) (setq j (+ j 8))))
         (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 1)
         (nl_fa_roots sstart slen dest cin cout 3)
         (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 0)
         (bf_arena_mr_chunks (ptr-read-u64 268436160 0) cout cout)
         (ptr-write-u64 cout 0 0)
         (setq tlen (ptr-read-u64 cin 0))
         (ptr-write-u64 hdr 0 1179407692)        ; magic "FLAT"
         (ptr-write-u64 hdr 8 slen) (ptr-write-u64 hdr 16 isz) (ptr-write-u64 hdr 24 tlen)
         (ptr-write-u64 hdr 32 (nl_img_off gbox sstart slen ib ie))
         (ptr-write-u64 hdr 40 (nl_img_off fbox sstart slen ib ie))
         (ptr-write-u64 hdr 48 (nl_img_off ubox sstart slen ib ie))
         ;; flat-arena cold-load: persist the dumping run's intern-region base so
         ;; the split loader can relocate the open-addressing intern table's
         ;; name-buffer pointers (slot+8).  Those slots live INSIDE the interned
         ;; region, so `nl_fa_field' (chunk-0-only) never records them in the
         ;; relocation table -- they retain absolute OLD pointers and must be
         ;; fixed by base-delta on load, else the first intern probe derefs a
         ;; stale address from the previous process and SIGSEGVs.
         (ptr-write-u64 hdr 56 ib)
         (setq fd (nl_os_open_write_truncate cpath))
         (if (< fd 0)
             (wf_write_int out -1)
           (seq
            (nl_fa_write_all fd hdr 64 0)
            (nl_fa_write_all fd tbl (* tlen 8) 0)
            (nl_fa_write_all fd dest (+ slen isz) 0)
            (nl_os_close_handle fd)
            (wf_write_int out (+ 64 (+ (* tlen 8) (+ slen isz)))))))))
    ;; STREAMING dump (flat-arena boot-wiring 12): dump WITHOUT the full-heap
    ;; `dest' scratch copy that `bf_arena_dump_image_to_file' uses.  That copy
    ;; doubles the in-arena footprint (~2*heap), which -- with the <2GB arena cap
    ;; (Phase47 signed-imm32 materialization) -- makes the ~882MB full-nemacs env
    ;; undumpable.  Here we swizzle the LIVE arena IN-PLACE: passing dest = sstart
    ;; (= ds) makes `nl_fa_field's loc = dest+(waddr-ds) = waddr, so the swizzle
    ;; rewrites the live fields directly (pointer -> image offset).  The relocation
    ;; table + counters land in the chunk-0 free area past the live bump (cin/cout
    ;; @ sstart+slen+isz, tbl @ +256 -- the same dest-relative slots nl_fa_emit
    ;; computes).  We then stream header + table + live chunk-0 + live intern to
    ;; the file (reading the live regions directly, no copy), and RESTORE the live
    ;; arena in place via a table-driven offset->pointer relocate.  CRITICAL: from
    ;; the swizzle to the restore the arena holds offsets (corrupt), so the body
    ;; does NO allocation / GC / eval -- only file writes (which just read bytes).
    ;; This keeps the footprint at ~heap (+ a few-MB table) instead of 2*heap, so
    ;; an ~882MB env dumps inside a 1GB chunk.  `nl_cold_reloc' lives in the
    ;; driver unit, so the inverse relocate is reimplemented here in applyfn-unit.
    (defun bf_arena_inplace_restore (tbl tlen ds slen ib)
      (let ((i 0))
        (seq
         (while (< i tlen)
           (let* ((f (ptr-read-u64 (+ tbl (* i 8)) 0))
                  (o (ptr-read-u64 (+ ds f) 0)))
             (nl_seq2
              (if (< o slen)
                  (ptr-write-u64 (+ ds f) 0 (+ ds o))
                (ptr-write-u64 (+ ds f) 0 (+ ib (- o slen))))
              (setq i (+ i 1)))))
         0)))
    (defun bf_arena_dump_image_stream (args out)
      (let* ((cpath (nl_bi_make_cpath (wf_arg_ptr args 0)))
             (hdr (alloc-bytes 64 8))   ; alloc the header FIRST so all used/total
                                        ; computations below see a stable cursor (no
                                        ; alloc between computing `total' and writing
                                        ; the regions -- otherwise the 64B header bump
                                        ; would desync hdr+8=total from the written size)
             (head (ptr-read-u64 268436160 0))
             (sstart (ptr-read-u64 (+ head 24) 0))   ; chunk-0 data-start (= ds for in-place)
             (slen (bf_arena_chunk_used head))        ; chunk-0 used
             (abase (- sstart 1024))
             (ib (ptr-read-u64 (+ abase 832) 0))
             (ie (ptr-read-u64 (+ abase 840) 0))
             (isz (nl_align_up (if (< ie ib) 0 (- ie ib)) 8))
             (total (nl_mc_total))                    ; coalesced size = sum of chunk used (= slen for 1 chunk)
             (multi (if (= (ptr-read-u64 (+ head 48) 0) 0) 0 1))
             (depth (ptr-read-u64 (data-addr nl_safepoint_ctx) 0))
             (env (if (= depth 0) 0
                    (ptr-read-u64 (+ (data-addr nl_safepoint_ctx) 64) 0)))
             (gbox (if (= env 0) 0 (ptr-read-u64 (+ env 8) 0)))
             (fbox (if (= env 0) 0 (ptr-read-u64 (+ env 40) 0)))
             (ubox (if (= env 0) 0 (ptr-read-u64 (+ env 72) 0)))
             ;; scratch (counters + relocation table): single-chunk keeps the
             ;; verified chunk-0 free-area layout (default nl_fa_emit tbl); multi-chunk
             ;; puts it in the intern region free area (chunk-0 is full, and
             ;; dest+total+isz would be past chunk-0's mmap) via the tbl override.
             (cin (if (= multi 0) (+ sstart (+ slen isz)) (+ ib isz)))
             (cout (+ cin 8))
             (tbl (if (= multi 0) (+ sstart (+ slen (+ isz 256))) (+ ib (+ isz 256))))
             (goff (nl_mc_imgoff gbox total ib ie))
             (foff (nl_mc_imgoff fbox total ib ie))
             (uoff (nl_mc_imgoff ubox total ib ie))
             (tlen 0) (fd 0))
        (seq
         ;; header is fully built from pre-swizzle reads (slen=total, tlen patched after)
         (ptr-write-u64 hdr 0 1179407692)
         (ptr-write-u64 hdr 16 isz)
         (ptr-write-u64 hdr 32 goff) (ptr-write-u64 hdr 40 foff) (ptr-write-u64 hdr 48 uoff)
         (ptr-write-u64 hdr 56 ib)
         (setq fd (nl_os_open_write_truncate cpath))
         (if (< fd 0)
             (wf_write_int out -1)
           (seq
            ;; multi-chunk: route nl_fa_emit's table to the intern free area
            (if (= multi 0) 0 (ptr-write-u64 (data-addr nl_fa_tbl_base) 0 tbl))
            ;; ---- from here to the restore: NO alloc / GC / eval (arena is swizzled) ----
            (ptr-write-u64 cin 0 0) (ptr-write-u64 cout 0 0)
            (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 1)
            ;; span=total: cross-chunk pointers swizzle to coalesced logical offsets;
            ;; dest=ds=sstart: loc=dest+(waddr-ds)=waddr => in-place over ANY chunk.
            (nl_fa_roots sstart total sstart cin cout 3)
            (ptr-write-u64 (data-addr nl_safepoint_ctx) 24 0)
            (ptr-write-u64 (data-addr nl_fa_tbl_base) 0 0)   ; reset override
            (setq tlen (ptr-read-u64 cin 0))
            (ptr-write-u64 hdr 8 total) (ptr-write-u64 hdr 24 tlen)
            (nl_fa_write_all fd hdr 64 0)
            (nl_fa_write_all fd tbl (* tlen 8) 0)
            (nl_mc_write_chunks fd head)   ; coalesced chunk regions, logical order (single: chunk-0)
            (nl_fa_write_all fd ib isz 0)   ; live intern (un-swizzled, fixed on load)
            (nl_os_close_handle fd)
            (bf_arena_inplace_restore_mc tbl tlen ib total)   ; restore the live arena
            (wf_write_int out (+ 64 (+ (* tlen 8) (+ total isz)))))))))
    (defun bf_arena_load_image_from_file (args out)
      (let* ((cpath (nl_bi_make_cpath (wf_arg_ptr args 0)))
             (hdr (alloc-bytes 64 8))
             (fd (nl_os_open_read cpath))
             (nil-slot (alloc-bytes 32 8)) (s3 (alloc-bytes 32 8))
             (s2 (alloc-bytes 32 8)) (s1 (alloc-bytes 32 8)))
        (if (< fd 0)
            (seq (wf_write_nil nil-slot)
                 (wf_cons_int 0 nil-slot s2) (wf_cons_int 0 s2 s1)
                 (wf_cons_int -1 s1 out) 0)
          (seq
           (nl_fa_read_all fd hdr 64 0)
           (let* ((magic (ptr-read-u64 hdr 0))
                  (slen (ptr-read-u64 hdr 8))
                  (isz (ptr-read-u64 hdr 16))
                  (tlen (ptr-read-u64 hdr 24))
                  (tbl (alloc-bytes (+ (* tlen 8) 8) 8))
                  (img (alloc-bytes (+ slen (+ isz 8)) 8))
                  (ti 0) (hdrp 0) (wf 1) (nblk 0))
             (seq
              (nl_fa_read_all fd tbl (* tlen 8) 0)
              (nl_fa_read_all fd img (+ slen isz) 0)
              (nl_os_close_handle fd)
              ;; apply the relocation table: img[F] += img (newbase)
              (setq ti 0)
              (while (< ti tlen)
                (let ((f (ptr-read-u64 (+ tbl (* ti 8)) 0)))
                  (ptr-write-u64 (+ img f) 0 (+ img (ptr-read-u64 (+ img f) 0))))
                (setq ti (+ ti 1)))
              ;; verify the loaded image is well-formed
              (setq hdrp 0)
              (while (and (= wf 1) (< hdrp slen))
                (let* ((h (ptr-read-u64 (+ img hdrp) 0))
                       (bt (- h (logand h 7))))
                  (if (< bt 8) (nl_seq2 (setq wf 0) (setq hdrp slen))
                    (if (> (+ hdrp bt) slen) (nl_seq2 (setq wf 0) (setq hdrp slen))
                      (nl_seq2 (setq nblk (+ nblk 1)) (setq hdrp (+ hdrp bt)))))))
              (if (= hdrp slen) 0 (setq wf 0))
              ;; result: (magic-ok tlen blocks wellformed)
              (wf_write_nil nil-slot)
              (wf_cons_int wf nil-slot s3)
              (wf_cons_int nblk s3 s2)
              (wf_cons_int tlen s2 s1)
              (wf_cons_int (if (= magic 1179407692) 1 0) s1 out)
              0))))))
    ;; flat-arena boot-wiring (3): the boot-load INSTALL mechanism.  This is the
    ;; exact code a boot hook runs: read the image, relocate it (table apply),
    ;; then RECONSTRUCT the EvalCtx globals root -- a 32B Sexp slot whose tag is
    ;; 12 (Record) and whose box pointer is `imgbase + globals_off' (from the
    ;; header) -- and confirm that root resolves to a valid Record inside the
    ;; loaded image.  (In a real boot the slot is the live `EvalCtx+0' and the
    ;; image is loaded into the mmap'd arena; here it is a fresh buffer + a
    ;; scratch slot so the live runtime is untouched.)  Returns
    ;; (MAGIC-OK ROOT-TAG ROOT-IN-CHUNK0 GBOX-BLOCK-TOTAL): (1 12 1 ~72) means a
    ;; boot would install a sound globals root from the persisted image.
    (defun bf_arena_boot_load_verify (args out)
      (let* ((cpath (nl_bi_make_cpath (wf_arg_ptr args 0)))
             (hdr (alloc-bytes 64 8))
             (fd (nl_os_open_read cpath))
             (nil-slot (alloc-bytes 32 8)) (s3 (alloc-bytes 32 8))
             (s2 (alloc-bytes 32 8)) (s1 (alloc-bytes 32 8)))
        (if (< fd 0)
            (seq (wf_write_nil nil-slot)
                 (wf_cons_int 0 nil-slot s3) (wf_cons_int 0 s3 s2)
                 (wf_cons_int 0 s2 s1) (wf_cons_int -1 s1 out) 0)
          (seq
           (nl_fa_read_all fd hdr 64 0)
           (let* ((slen (ptr-read-u64 hdr 8))
                  (isz (ptr-read-u64 hdr 16))
                  (tlen (ptr-read-u64 hdr 24))
                  (goff (ptr-read-u64 hdr 32))
                  (tbl (alloc-bytes (+ (* tlen 8) 8) 8))
                  (img (alloc-bytes (+ slen (+ isz 8)) 8))
                  (rootslot (alloc-bytes 32 8))
                  (ti 0))
             (seq
              (nl_fa_read_all fd tbl (* tlen 8) 0)
              (nl_fa_read_all fd img (+ slen isz) 0)
              (nl_os_close_handle fd)
              (setq ti 0)
              (while (< ti tlen)
                (let ((f (ptr-read-u64 (+ tbl (* ti 8)) 0)))
                  (ptr-write-u64 (+ img f) 0 (+ img (ptr-read-u64 (+ img f) 0))))
                (setq ti (+ ti 1)))
              ;; reconstruct the globals root slot
              (ptr-write-u8 rootslot 0 12)
              (ptr-write-u64 rootslot 8 (+ img goff))
              ;; verify it resolves to a valid Record in the loaded image
              (let* ((rtag (ptr-read-u8 rootslot 0))
                     (gbox (ptr-read-u64 rootslot 8))
                     (rin (if (< gbox img) 0 (if (< gbox (+ img slen)) 1 0)))
                     (gh (ptr-read-u64 (- gbox 8) 0))
                     (gbt (- gh (logand gh 7))))
                (wf_write_nil nil-slot)
                (wf_cons_int gbt nil-slot s3)
                (wf_cons_int rin s3 s2)
                (wf_cons_int rtag s2 s1)
                (wf_cons_int (if (= (ptr-read-u64 hdr 0) 1179407692) 1 0) s1 out)
                0)))))))
    ;; flat-arena boot-wiring (4): SPLIT relocate -- the real-arena load.  In a
    ;; live runtime chunk-0 and the interned region are SEPARATE mmaps at
    ;; unrelated bases, so the load cannot use one contiguous `+ newbase'; it
    ;; must split by region: a field holding image offset O relocates to
    ;; chunk0_base + O when O < slen, else intern_base + (O - slen).
    ;; `bf_arena_load_split_verify' loads the image into TWO separate buffers
    ;; (simulating the two mmaps), applies the table with that split, and
    ;; verifies (a) the chunk-0 region is well-formed and (b) EVERY relocated
    ;; pointer lands inside one of the two regions.  Returns
    ;; (MAGIC-OK WELLFORMED BLOCKS BAD-POINTERS): (1 1 ~464k 0) means the
    ;; split relocate rebuilds a sound heap across separate region bases --
    ;; exactly what loading into the mmap'd arena needs.
    (defun bf_arena_load_split_verify (args out)
      (let* ((cpath (nl_bi_make_cpath (wf_arg_ptr args 0)))
             (hdr (alloc-bytes 64 8))
             (fd (nl_os_open_read cpath))
             (nil-slot (alloc-bytes 32 8)) (s3 (alloc-bytes 32 8))
             (s2 (alloc-bytes 32 8)) (s1 (alloc-bytes 32 8)))
        (if (< fd 0)
            (seq (wf_write_nil nil-slot)
                 (wf_cons_int 0 nil-slot s3) (wf_cons_int 0 s3 s2)
                 (wf_cons_int 0 s2 s1) (wf_cons_int -1 s1 out) 0)
          (seq
           (nl_fa_read_all fd hdr 64 0)
           (let* ((slen (ptr-read-u64 hdr 8))
                  (isz (ptr-read-u64 hdr 16))
                  (tlen (ptr-read-u64 hdr 24))
                  (tbl (alloc-bytes (+ (* tlen 8) 8) 8))
                  (c0 (alloc-bytes (+ slen 8) 8))
                  (ir (alloc-bytes (+ isz 8) 8))
                  (ti 0) (hdrp 0) (wf 1) (nblk 0) (badp 0))
             (seq
              (nl_fa_read_all fd tbl (* tlen 8) 0)
              (nl_fa_read_all fd c0 slen 0)
              (nl_fa_read_all fd ir isz 0)
              (nl_os_close_handle fd)
              ;; split relocate
              (setq ti 0)
              (while (< ti tlen)
                (let* ((f (ptr-read-u64 (+ tbl (* ti 8)) 0))
                       (o (ptr-read-u64 (+ c0 f) 0)))
                  (if (< o slen)
                      (ptr-write-u64 (+ c0 f) 0 (+ c0 o))
                    (ptr-write-u64 (+ c0 f) 0 (+ ir (- o slen)))))
                (setq ti (+ ti 1)))
              ;; (a) chunk-0 well-formed
              (setq hdrp 0)
              (while (and (= wf 1) (< hdrp slen))
                (let* ((h (ptr-read-u64 (+ c0 hdrp) 0))
                       (bt (- h (logand h 7))))
                  (if (< bt 8) (nl_seq2 (setq wf 0) (setq hdrp slen))
                    (if (> (+ hdrp bt) slen) (nl_seq2 (setq wf 0) (setq hdrp slen))
                      (nl_seq2 (setq nblk (+ nblk 1)) (setq hdrp (+ hdrp bt)))))))
              (if (= hdrp slen) 0 (setq wf 0))
              ;; (b) every relocated pointer lands in c0 or ir
              (setq ti 0)
              (while (< ti tlen)
                (let* ((f (ptr-read-u64 (+ tbl (* ti 8)) 0))
                       (v (ptr-read-u64 (+ c0 f) 0))
                       (inc0 (if (< v c0) 0 (if (< v (+ c0 slen)) 1 0)))
                       (inir (if (< v ir) 0 (if (< v (+ ir isz)) 1 0))))
                  (if (= (+ inc0 inir) 0) (setq badp (+ badp 1)) 0))
                (setq ti (+ ti 1)))
              (wf_write_nil nil-slot)
              (wf_cons_int badp nil-slot s3)
              (wf_cons_int nblk s3 s2)
              (wf_cons_int wf s2 s1)
              (wf_cons_int (if (= (ptr-read-u64 hdr 0) 1179407692) 1 0) s1 out)
              0))))))
    ;; Doc 156 increment 2 (concept-proof): value-survival.  Loads IMAGE into
    ;; scratch (chunk-0 c0 + interned ir), applies the split relocation (same as
    ;; bf_arena_load_split_verify), rebuilds the globals Record root (tag 12,
    ;; box = c0+goff), and looks up SYM in that RELOCATED SCRATCH image via
    ;; nelisp_mirror_lookup_value.  Proves a global's VALUE survives
    ;; dump->file->load->relocate WITHOUT writing the live arena (the real-arena
    ;; INSTALL, Doc 156 sec 3, is the separate next increment).
    (defun bf_arena_value_survival (args out)
      (let* ((cpath (nl_bi_make_cpath (wf_arg_ptr args 0)))
             (qsym (wf_arg_ptr args 1))
             (hdr (alloc-bytes 64 8))
             (fd (nl_os_open_read cpath)))
        (if (< fd 0)
            (seq (wf_write_nil out) 0)
          (seq
           (nl_fa_read_all fd hdr 64 0)
           (let* ((slen (ptr-read-u64 hdr 8))
                  (isz (ptr-read-u64 hdr 16))
                  (tlen (ptr-read-u64 hdr 24))
                  (goff (ptr-read-u64 hdr 32))
                  (tbl (alloc-bytes (+ (* tlen 8) 8) 8))
                  (c0 (alloc-bytes (+ slen 8) 8))
                  (ir (alloc-bytes (+ isz 8) 8))
                  (rootslot (alloc-bytes 32 8))
                  (ti 0))
             (seq
              (nl_fa_read_all fd tbl (* tlen 8) 0)
              (nl_fa_read_all fd c0 slen 0)
              (nl_fa_read_all fd ir isz 0)
              (nl_os_close_handle fd)
              (setq ti 0)
              (while (< ti tlen)
                (let* ((f (ptr-read-u64 (+ tbl (* ti 8)) 0))
                       (o (ptr-read-u64 (+ c0 f) 0)))
                  (if (< o slen)
                      (ptr-write-u64 (+ c0 f) 0 (+ c0 o))
                    (ptr-write-u64 (+ c0 f) 0 (+ ir (- o slen)))))
                (setq ti (+ ti 1)))
              (ptr-write-u8 rootslot 0 12)
              (ptr-write-u64 rootslot 8
                             (if (< goff slen) (+ c0 goff) (+ ir (- goff slen))))
              ;; mirror_lookup_value = lookup_entry + record-slot-ref slot 0
              ;; (the symbol-entry's value cell).  Only mirror-lookup.o (entry)
              ;; is linked into the reader, so compose here.
              (let* ((entry (nelisp_mirror_lookup_entry rootslot qsym)))
                (if (= entry 0)
                    (wf_write_nil out)
                  (record-slot-ref entry 0 out)))
              0))))))
    ;; NB: the `bf_size_census*' arena-diagnostic family moved to
    ;; `nelisp-standalone--applyfn-census-helpers' (reader-only).  It calls
    ;; `nl_gc_bt_ok' / `nl_gc_chunk_end', which only `reader-gc.o' defines, so
    ;; emitting it into the baked eval applyfn (where its `nelisp--size-census'
    ;; dispatch arm is absent — dead code) left those GC symbols unresolved at
    ;; link time.  Census stays available to the reader applyfn, which links
    ;; `reader-gc.o'.  (Blocker fix: eval-path `nl_gc_bt_ok' unresolved.)
    (defun bf_arena_force_grow_smoke (out)
      (let* ((chunk (ptr-read-u64 268436168 0))
             (cursor-addr (if (= chunk (ptr-read-u64 268436160 0))
                              268435456
                            (+ chunk 16)))
             (size (ptr-read-u64 (+ chunk 8) 0))
             (near (if (< size 2048) 1024 (- size 32)))
             (p 0))
        (seq
         (ptr-write-u64 cursor-addr 0 near)
         (setq p (alloc-bytes 64 8))
         (if (= p 0)
             (wf_write_int out 0)
           (bf_arena_stats out)))))
    ;; wf_dirty: bump the MUTATION EPOCH counter @268435544 (NO-ESCAPE gate
    ;; in `nelisp_eval_call').  Called by every primitive that installs a box
    ;; into PRE-EXISTING (persistent) structure — setcar/setcdr/aset/puthash —
    ;; so the per-eval arena reset never frees a still-reachable escapee.
    (defun wf_dirty ()
      ;; SeqCst atomic increment so concurrent threads (parallel build) never
      ;; lose a mutation-epoch bump (the old read;+1;write was a racy RMW).
      (atomic-fetch-add 268435544 1))
    (defun wf_sum (list_ptr acc)
      (if (= (ptr-read-u64 list_ptr 0) 7)
          (let* ((car_ptr (nl_cons_car_ptr list_ptr)) (v (ptr-read-u64 car_ptr 8)))
            (wf_sum (nl_cons_cdr_ptr list_ptr) (+ acc v))) acc))
    (defun wf_prod (list_ptr acc)
      (if (= (ptr-read-u64 list_ptr 0) 7)
          (let* ((car_ptr (nl_cons_car_ptr list_ptr)) (v (ptr-read-u64 car_ptr 8)))
            (wf_prod (nl_cons_cdr_ptr list_ptr) (* acc v))) acc))
    (defun wf_subtail (list_ptr acc)
      (if (= (ptr-read-u64 list_ptr 0) 7)
          (let* ((car_ptr (nl_cons_car_ptr list_ptr)) (v (ptr-read-u64 car_ptr 8)))
            (wf_subtail (nl_cons_cdr_ptr list_ptr) (- acc v))) acc))
    (defun wf_diff (list_ptr)
      (if (= (ptr-read-u64 list_ptr 0) 7)
          (let* ((car_ptr (nl_cons_car_ptr list_ptr)) (first (ptr-read-u64 car_ptr 8))
                 (rest (nl_cons_cdr_ptr list_ptr)))
            ;; elisp `-': 1-arg `(- x)' = NEGATION (-x), not x; n-arg subtracts
            ;; the tail from the first.  The old `first' fallthrough made `(- 5)'
            ;; return 5, breaking every `(ash v (- (* i 8)))' byte-extraction.
            (if (= (ptr-read-u64 rest 0) 7) (wf_subtail rest first) (- 0 first))) 0))
    ;; --- FLOAT-AWARE arithmetic.  The integer wf_sum/wf_prod/wf_subtail/wf_diff
    ;; above read slot+8 as a raw i64 with NO tag check, so a Float operand
    ;; (tag 3, IEEE-754 bits inline at +8) is folded as garbage and written via
    ;; wf_write_int (tag 2 Int) -> `(+ 2.5 2.5)' returned a huge Int, not 5.0.
    ;; These mirror the tag-aware wf_num_lt family + the reader's nl_str_to_float
    ;; production idiom: the accumulator is carried as u64 BITS (i64-typed, safe
    ;; across recursion); an f64 value appears ONLY inline as an arg to
    ;; f64-add/sub/mul + nl_sexp_write_float (no f64 local/param — the same
    ;; discipline every proven float producer in this layer uses).  There is no
    ;; f64-to-bits op, so we recover result bits by round-tripping through a
    ;; scratch slot that nl_sexp_write_float (reader-float.o, linked by the
    ;; reader build) fills with {tag=3, xmm0 bits @ +8}.
    (defun wf_any_float (list_ptr)
      (if (= (ptr-read-u64 list_ptr 0) 7)
          (if (= (ptr-read-u64 (nl_cons_car_ptr list_ptr) 0) 3)
              1
            (wf_any_float (nl_cons_cdr_ptr list_ptr)))
        0))
    (defun wf_elem_fbits (car_ptr scratch)
      (if (= (ptr-read-u64 car_ptr 0) 3)
          (ptr-read-u64 car_ptr 8)
        (seq (nl_sexp_write_float scratch (i64-to-f64 (ptr-read-u64 car_ptr 8)))
             (ptr-read-u64 scratch 8))))
    (defun wf_int_fbits (n scratch)
      (seq (nl_sexp_write_float scratch (i64-to-f64 n)) (ptr-read-u64 scratch 8)))
    (defun wf_fsum (list_ptr acc_bits scratch)
      (if (= (ptr-read-u64 list_ptr 0) 7)
          (let* ((vb (wf_elem_fbits (nl_cons_car_ptr list_ptr) scratch)))
            (seq (nl_sexp_write_float scratch (f64-add (bits-to-f64 acc_bits) (bits-to-f64 vb)))
                 (wf_fsum (nl_cons_cdr_ptr list_ptr) (ptr-read-u64 scratch 8) scratch)))
        acc_bits))
    (defun wf_fprod (list_ptr acc_bits scratch)
      (if (= (ptr-read-u64 list_ptr 0) 7)
          (let* ((vb (wf_elem_fbits (nl_cons_car_ptr list_ptr) scratch)))
            (seq (nl_sexp_write_float scratch (f64-mul (bits-to-f64 acc_bits) (bits-to-f64 vb)))
                 (wf_fprod (nl_cons_cdr_ptr list_ptr) (ptr-read-u64 scratch 8) scratch)))
        acc_bits))
    (defun wf_fsubtail (list_ptr acc_bits scratch)
      (if (= (ptr-read-u64 list_ptr 0) 7)
          (let* ((vb (wf_elem_fbits (nl_cons_car_ptr list_ptr) scratch)))
            (seq (nl_sexp_write_float scratch (f64-sub (bits-to-f64 acc_bits) (bits-to-f64 vb)))
                 (wf_fsubtail (nl_cons_cdr_ptr list_ptr) (ptr-read-u64 scratch 8) scratch)))
        acc_bits))
    (defun wf_fdiff (list_ptr scratch)
      (let* ((first_b (wf_elem_fbits (nl_cons_car_ptr list_ptr) scratch))
             (rest (nl_cons_cdr_ptr list_ptr)))
        ;; elisp `-': 1-arg = negation; n-arg = first - rest.  Negate via x*-1.0
        ;; (sign XOR), NOT 0.0-x, so -(+0.0) yields -0.0 (Doc 159 §12).
        (if (= (ptr-read-u64 rest 0) 7)
            (wf_fsubtail rest first_b scratch)
          (seq (nl_sexp_write_float scratch (f64-mul (bits-to-f64 first_b) (i64-to-f64 -1)))
               (ptr-read-u64 scratch 8)))))
    ;; float `/' (2-arg, mirroring the integer `/' arity): a/b in f64.
    (defun wf_fdiv2 (args scratch)
      (let* ((ab (wf_elem_fbits (wf_arg_ptr args 0) scratch))
             (bb (wf_elem_fbits (wf_arg_ptr args 1) scratch)))
        (nl_sexp_write_float scratch (f64-div (bits-to-f64 ab) (bits-to-f64 bb)))))
    ;; float -> int conversions (each takes a Float Sexp ptr; integer args are
    ;; handled as identity in the dispatch arm).  f64-to-i64-trunc rounds toward
    ;; zero (= truncate); floor rounds toward -inf, ceiling toward +inf.  Cross-
    ;; arch safe (no syscall).  Note: `round' (banker's rounding) and `float-time'
    ;; (needs a per-arch clock_gettime OS helper) are deferred.
    (defun wf_ftrunc (p)
      (f64-to-i64-trunc (bits-to-f64 (sexp-float-unwrap p))))
    (defun wf_ffloor (p)
      (let* ((t0 (f64-to-i64-trunc (bits-to-f64 (sexp-float-unwrap p)))))
        (if (= (f64-lt (bits-to-f64 (sexp-float-unwrap p)) (i64-to-f64 t0)) 1)
            (- t0 1)
          t0)))
    (defun wf_fceil (p)
      (let* ((t0 (f64-to-i64-trunc (bits-to-f64 (sexp-float-unwrap p)))))
        (if (= (f64-gt (bits-to-f64 (sexp-float-unwrap p)) (i64-to-f64 t0)) 1)
            (+ t0 1)
          t0)))
    ;; float-time wrapper: the nl_os_float_time extern must be called from a
    ;; helper defun (its own frame), not inline in a dispatch arm -- an inline
    ;; extern call from the spliced dispatch if-chain aborts at runtime (same as
    ;; nl_sexp_write_float, which only works from wf_fsum/wf_fprod helpers).
    ;; Write into a fresh scratch slot, then copy to `out' -- exactly like the
    ;; float arithmetic (nl_sexp_write_float -> scratch, wf_copy32 -> out).
    ;; Calling the extern with `out' (the dispatch result slot) directly aborts.
    (defun wf_float_time (out)
      (let* ((sc (alloc-bytes 32 8)))
        (seq (nl_os_float_time sc) (wf_copy32 out sc))))
    )
  "Core wf_* dispatch helpers (shared by baked + reader applyfn).")

;; bf_size_census arena-diagnostic family (Doc 08 §8.14): BLOCK_TOTAL histogram
;; over the whole arena.  READER-ONLY: it walks the heap via `nl_gc_bt_ok' /
;; `nl_gc_chunk_end', which only `reader-gc.o' defines.  Keeping it out of the
;; shared core-helpers stops the baked eval applyfn (whose `nelisp--size-census'
;; dispatch arm is clipped away) from emitting dead references to those GC
;; symbols, which the eval manifest cannot resolve.  Boxes carry NO self type
;; tag (type lives in the Sexp that points to the box, top-down), so a bottom-up
;; walk can only bucket by size.  Reliable read-only walk modeled on
;; `nl_gc_sweep_chunk'.  ACC layout (u64 each, refined to localize "big"):
;;   +0 total-nonfree  +8 free  +16 cons(BT=88)  +24 le256  +32 b257-4k
;;   +40 b4k-256k  +48 b256k-2m (~1MB parse pool lands here)  +56 b>2m
(defconst nelisp-standalone--applyfn-census-helpers
  '((defun bf_size_census_block (hdr acc)
      (let ((bt (nl_hdr_bt hdr))
            (mark (nl_hdr_mark hdr)))
        ;; le256 sub-buckets: +24 le32-bytes +32 le32-COUNT +40 bt33-64 +48 bt65-256
        (if (= mark 2)
            (ptr-write-u64 (+ acc 8) 0 (+ (ptr-read-u64 (+ acc 8) 0) bt))
          (seq
           (ptr-write-u64 acc 0 (+ (ptr-read-u64 acc 0) bt))
           (if (= bt 88)
               (ptr-write-u64 (+ acc 16) 0 (+ (ptr-read-u64 (+ acc 16) 0) bt))
             (if (< bt 33)
                 (seq (ptr-write-u64 (+ acc 24) 0 (+ (ptr-read-u64 (+ acc 24) 0) bt))
                      (ptr-write-u64 (+ acc 32) 0 (+ (ptr-read-u64 (+ acc 32) 0) 1)))
               (if (< bt 65)
                   (ptr-write-u64 (+ acc 40) 0 (+ (ptr-read-u64 (+ acc 40) 0) bt))
                 (if (< bt 257)
                     (ptr-write-u64 (+ acc 48) 0 (+ (ptr-read-u64 (+ acc 48) 0) bt))
                   0))))))))
    (defun bf_size_census_step (hdr end acc)
      (if (= (nl_gc_bt_ok hdr (nl_hdr_bt hdr) end) 0)
          0
        (nl_seq2 (bf_size_census_block hdr acc) (+ hdr (nl_hdr_bt hdr)))))
    (defun bf_size_census_chunk (chunk acc)
      (let ((hdr (ptr-read-u64 (+ chunk 24) 0))
            (end (nl_gc_chunk_end chunk)))
        (while (and (> hdr 0) (< hdr end))
          (setq hdr (bf_size_census_step hdr end acc)))
        0))
    (defun bf_size_census_chunks (chunk acc)
      (if (= chunk 0)
          0
        (nl_seq2 (bf_size_census_chunk chunk acc)
                 (bf_size_census_chunks (ptr-read-u64 (+ chunk 48) 0) acc))))
    (defun bf_size_census (out)
      (let* ((acc (alloc-bytes 64 8))
             (nil-slot (alloc-bytes 32 8))
             (s7 (alloc-bytes 32 8))
             (s6 (alloc-bytes 32 8))
             (s5 (alloc-bytes 32 8))
             (s4 (alloc-bytes 32 8))
             (s3 (alloc-bytes 32 8))
             (s2 (alloc-bytes 32 8))
             (s1 (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 acc 0 0) (ptr-write-u64 (+ acc 8) 0 0)
         (ptr-write-u64 (+ acc 16) 0 0) (ptr-write-u64 (+ acc 24) 0 0)
         (ptr-write-u64 (+ acc 32) 0 0) (ptr-write-u64 (+ acc 40) 0 0)
         (ptr-write-u64 (+ acc 48) 0 0) (ptr-write-u64 (+ acc 56) 0 0)
         (bf_size_census_chunks (ptr-read-u64 268436160 0) acc)
         (wf_write_nil nil-slot)
         (wf_cons_int (if (= (ptr-read-u64 268436288 0) 0) 0
                        (- (ptr-read-u64 268436296 0) (ptr-read-u64 268436288 0)))
                      nil-slot s7)
         (wf_cons_int (ptr-read-u64 (+ acc 48) 0) s7 s6)
         (wf_cons_int (ptr-read-u64 (+ acc 40) 0) s6 s5)
         (wf_cons_int (ptr-read-u64 (+ acc 32) 0) s5 s4)
         (wf_cons_int (ptr-read-u64 (+ acc 24) 0) s4 s3)
         (wf_cons_int (ptr-read-u64 (+ acc 16) 0) s3 s2)
         (wf_cons_int (ptr-read-u64 (+ acc 8) 0) s2 s1)
         (wf_cons_int (ptr-read-u64 acc 0) s1 out)
         0))))
  "Reader-only arena size-census diagnostics (call `nl_gc_bt_ok' /
`nl_gc_chunk_end' from `reader-gc.o'); excluded from the baked eval applyfn.")

;; M4 hash-table helpers (cons-alist v1).  Reader-only: wf_key_eq uses symbol-eq
;; / str-eq grammar ops that lower to extern calls present only in the reader.
(defconst nelisp-standalone--applyfn-ht-helpers
  '((defun wf_ht_copy32 (dst src)
      (seq (ptr-write-u64 dst 0 (ptr-read-u64 src 0)) (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
           (ptr-write-u64 dst 16 (ptr-read-u64 src 16)) (ptr-write-u64 dst 24 (ptr-read-u64 src 24)) 0))
    (defun wf_key_eq_depth (ka kb depth)
      (let* ((ta (ptr-read-u64 ka 0)) (tb (ptr-read-u64 kb 0)))
        (if (= ta tb)
            (if (= ta 2)
                (if (= (ptr-read-u64 ka 8) (ptr-read-u64 kb 8)) 1 0)
              (if (= ta 4)
                  (symbol-eq ka kb)
                (if (= ta 5)
                    (str-eq ka kb)
                  (if (= ta 7)
                      (if (<= depth 0)
                          0
                        (if (= (wf_key_eq_depth (nl_cons_car_ptr ka) (nl_cons_car_ptr kb) (- depth 1)) 1)
                            (wf_key_eq_depth (nl_cons_cdr_ptr ka) (nl_cons_cdr_ptr kb) (- depth 1))
                          0))
                    (if (= ta 0) 1 (if (= ta 1) 1 0))))))
          0)))
    (defun wf_key_eq (ka kb)
      (wf_key_eq_depth ka kb 16))
    (defun wf_ht_data_slot (table_ptr) (nl_cons_cdr_ptr table_ptr))
    (defun wf_ht_alist_slot (table_ptr) (wf_ht_data_slot table_ptr))
    (defun wf_ht_str_hash_loop (str_ptr i n h)
      (if (>= i n)
          h
        (wf_ht_str_hash_loop
         str_ptr
         (+ i 1)
         n
         (logand (* (logxor h (str-byte-at str_ptr i)) 16777619) 2147483647))))
    (defun wf_ht_str_hash (str_ptr)
      (wf_ht_str_hash_loop str_ptr 0 (str-len str_ptr) 2166136261))
    (defun wf_ht_key_hash (key_ptr depth)
      (let* ((tag (ptr-read-u64 key_ptr 0)))
        (if (= tag 2)
            (logand (ptr-read-u64 key_ptr 8) 2147483647)
          (if (= tag 4)
              (logand (ptr-read-u64 key_ptr 8) 2147483647)
            (if (= tag 5)
                (wf_ht_str_hash key_ptr)
              (if (= tag 6)
                  (wf_ht_str_hash key_ptr)
                (if (= tag 7)
                    (if (<= depth 0)
                        7
                      (logand (+ 2654435769
                                 (* 33 (wf_ht_key_hash (nl_cons_car_ptr key_ptr) (- depth 1)))
                                 (* 65599 (wf_ht_key_hash (nl_cons_cdr_ptr key_ptr) (- depth 1))))
                              2147483647))
                  tag)))))))
    (defun wf_ht_key_hash_stable_p (key_ptr depth)
      (let* ((tag (ptr-read-u64 key_ptr 0)))
        (if (= tag 0)
            1
          (if (= tag 1)
              1
            (if (= tag 2)
                1
              (if (= tag 4)
                  1
                (if (= tag 5)
                    1
                  (if (= tag 6)
                      1
                    (if (= tag 7)
                        (if (<= depth 0)
                            0
                          (if (= (wf_ht_key_hash_stable_p (nl_cons_car_ptr key_ptr) (- depth 1)) 1)
                              (wf_ht_key_hash_stable_p (nl_cons_cdr_ptr key_ptr) (- depth 1))
                            0))
                      0)))))))))
    (defun wf_ht_bucket_index (vec key_ptr)
      (let* ((n (vector-len vec))
             (h (wf_ht_key_hash key_ptr 8)))
        (if (= (logand n (- n 1)) 0)
            (logand h (- n 1))
          (mod h n))))
    (defun wf_ht_find (node_ptr key_ptr)
      (if (= (ptr-read-u64 node_ptr 0) 7)
          (let* ((entry_ptr (nl_cons_car_ptr node_ptr))
                 (key_slot (nl_cons_car_ptr entry_ptr)))
            (if (= (wf_key_eq key_slot key_ptr) 1)
                entry_ptr
              (wf_ht_find (nl_cons_cdr_ptr node_ptr) key_ptr)))
        0))
    (defun wf_ht_count (node_ptr acc)
      (if (= (ptr-read-u64 node_ptr 0) 7)
          (wf_ht_count (nl_cons_cdr_ptr node_ptr) (+ acc 1))
        acc))
    (defun wf_ht_find_vec_from (vec key_ptr i n)
      (if (>= i n)
          0
        (let* ((entry_ptr (wf_ht_find (vector-ref-ptr vec i) key_ptr)))
          (if (= entry_ptr 0)
              (wf_ht_find_vec_from vec key_ptr (+ i 1) n)
            entry_ptr))))
    (defun wf_ht_find_table (data_ptr key_ptr)
      (if (= (ptr-read-u64 data_ptr 0) 8)
          (let* ((idx (wf_ht_bucket_index data_ptr key_ptr))
                 (entry_ptr (wf_ht_find (vector-ref-ptr data_ptr idx) key_ptr)))
            (if (= entry_ptr 0)
                (if (= (wf_ht_key_hash_stable_p key_ptr 8) 1)
                    0
                  (wf_ht_find_vec_from data_ptr key_ptr 0 (vector-len data_ptr)))
              entry_ptr))
        (wf_ht_find data_ptr key_ptr)))
    (defun wf_ht_count_vec (vec i n acc)
      (if (>= i n)
          acc
        (wf_ht_count_vec vec (+ i 1) n
                         (wf_ht_count (vector-ref-ptr vec i) acc))))
    (defun wf_ht_count_table (data_ptr)
      (if (= (ptr-read-u64 data_ptr 0) 8)
          (wf_ht_count_vec data_ptr 0 (vector-len data_ptr) 0)
        (wf_ht_count data_ptr 0)))
    (defun wf_ht_make (out)
      (let* ((marker (alloc-bytes 32 8)) (buckets (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 marker 0 2) (ptr-write-u64 marker 8 0)
         (ptr-write-u64 marker 16 0) (ptr-write-u64 marker 24 0)
         (vector-make 2048 buckets)
         (nelisp_cons_construct marker buckets out)
         0)))
    (defun wf_ht_put (args out)
      (let* ((key_ptr (wf_arg_ptr args 0)) (val_ptr (wf_arg_ptr args 1))
             (table_ptr (wf_arg_ptr args 2))
             (data_slot (wf_ht_data_slot table_ptr))
             (entry_ptr (wf_ht_find_table data_slot key_ptr)))
        (if (= entry_ptr 0)
            (if (= (ptr-read-u64 data_slot 0) 8)
                (let* ((idx (wf_ht_bucket_index data_slot key_ptr))
                       (bucket_slot (vector-ref-ptr data_slot idx))
                       (pair_s (alloc-bytes 32 8))
                       (newhead_s (alloc-bytes 32 8)))
                  (seq
                   (nelisp_cons_construct key_ptr val_ptr pair_s)
                   (nelisp_cons_construct pair_s bucket_slot newhead_s)
                   (vector-slot-set data_slot idx newhead_s)
                   (wf_ht_copy32 out val_ptr)
                   0))
              (let* ((pair_s (alloc-bytes 32 8)) (newhead_s (alloc-bytes 32 8)))
                (seq
                 (nelisp_cons_construct key_ptr val_ptr pair_s)
                 (nelisp_cons_construct pair_s data_slot newhead_s)
                 (wf_ht_copy32 data_slot newhead_s)
                 (wf_ht_copy32 out val_ptr)
                 0)))
          (let* ((val_slot (nl_cons_cdr_ptr entry_ptr)))
            (seq
             (wf_ht_copy32 val_slot val_ptr)
             (wf_ht_copy32 out val_ptr)
             0)))))
    (defun wf_ht_get (args out)
      (let* ((key_ptr (wf_arg_ptr args 0)) (table_ptr (wf_arg_ptr args 1))
             (data_slot (wf_ht_data_slot table_ptr))
             (entry_ptr (wf_ht_find_table data_slot key_ptr)))
        (if (= entry_ptr 0)
            (let* ((rest1 (nl_cons_cdr_ptr args))
                   (rest2 (nl_cons_cdr_ptr rest1)))
              (if (= (ptr-read-u64 rest2 0) 7)
                  (wf_ht_copy32 out (nl_cons_car_ptr rest2))
                (wf_write_nil out)))
          (wf_ht_copy32 out (nl_cons_cdr_ptr entry_ptr)))))
    (defun wf_ht_rem (args out)
      (let* ((key_ptr (wf_arg_ptr args 0)) (table_ptr (wf_arg_ptr args 1))
             (data_slot (wf_ht_data_slot table_ptr))
             (rebuilt (alloc-bytes 32 8)))
        (seq
         (if (= (ptr-read-u64 data_slot 0) 8)
             (wf_ht_rem_vec data_slot key_ptr 0 (vector-len data_slot))
           (seq
            (wf_ht_rem_walk data_slot key_ptr rebuilt)
            (wf_ht_copy32 data_slot rebuilt)))
         (wf_write_nil out)
         0)))
    (defun wf_ht_rem_vec (vec key_ptr i n)
      (if (>= i n)
          0
        (let* ((rebuilt (alloc-bytes 32 8)))
          (seq
           (wf_ht_rem_walk (vector-ref-ptr vec i) key_ptr rebuilt)
           (vector-slot-set vec i rebuilt)
           (wf_ht_rem_vec vec key_ptr (+ i 1) n)))))
    (defun wf_ht_rem_walk (node_ptr key_ptr out_slot)
      (if (= (ptr-read-u64 node_ptr 0) 7)
          (let* ((entry_ptr (nl_cons_car_ptr node_ptr))
                 (key_slot (nl_cons_car_ptr entry_ptr)))
            (if (= (wf_key_eq key_slot key_ptr) 1)
                (wf_ht_copy32 out_slot (nl_cons_cdr_ptr node_ptr))
              (let* ((rest_s (alloc-bytes 32 8)) (entry_s (alloc-bytes 32 8)))
                (seq
                 (wf_ht_rem_walk (nl_cons_cdr_ptr node_ptr) key_ptr rest_s)
                 (wf_ht_copy32 entry_s entry_ptr)
                 (nelisp_cons_construct entry_s rest_s out_slot)
                 0))))
        (wf_write_nil out_slot)))
    (defun wf_ht_maphash (args out) (seq (wf_write_nil out) 0)))
  "M4 hash-table helpers (reader-only).")

(defconst nelisp-standalone--applyfn-search-helpers
  '((defun wf_memq_walk (elt_ptr list_ptr out)
      (if (= (ptr-read-u64 list_ptr 0) 7)
          (if (= (bf_eq2 elt_ptr (nl_cons_car_ptr list_ptr)) 1)
              (wf_copy32 out list_ptr)
            (wf_memq_walk elt_ptr (nl_cons_cdr_ptr list_ptr) out))
        (wf_write_nil out)))
    (defun wf_memq (args out)
      (wf_memq_walk (wf_arg_ptr args 0) (wf_arg_ptr args 1) out))
    (defun wf_member_walk (elt_ptr list_ptr out)
      (if (= (ptr-read-u64 list_ptr 0) 7)
          (if (= (wf_key_eq elt_ptr (nl_cons_car_ptr list_ptr)) 1)
              (wf_copy32 out list_ptr)
            (wf_member_walk elt_ptr (nl_cons_cdr_ptr list_ptr) out))
        (wf_write_nil out)))
    (defun wf_member (args out)
      (wf_member_walk (wf_arg_ptr args 0) (wf_arg_ptr args 1) out))
    (defun wf_assq_walk (key_ptr alist_ptr out)
      (if (= (ptr-read-u64 alist_ptr 0) 7)
          (let* ((pair_ptr (nl_cons_car_ptr alist_ptr)))
            (if (= (if (= (ptr-read-u64 pair_ptr 0) 7)
                       (bf_eq2 key_ptr (nl_cons_car_ptr pair_ptr))
                     0)
                   1)
                (wf_copy32 out pair_ptr)
              (wf_assq_walk key_ptr (nl_cons_cdr_ptr alist_ptr) out)))
        (wf_write_nil out)))
    (defun wf_assq (args out)
      (wf_assq_walk (wf_arg_ptr args 0) (wf_arg_ptr args 1) out))
    (defun wf_assoc_walk (key_ptr alist_ptr out)
      (if (= (ptr-read-u64 alist_ptr 0) 7)
          (let* ((pair_ptr (nl_cons_car_ptr alist_ptr)))
            (if (= (if (= (ptr-read-u64 pair_ptr 0) 7)
                       (wf_key_eq key_ptr (nl_cons_car_ptr pair_ptr))
                     0)
                   1)
                (wf_copy32 out pair_ptr)
              (wf_assoc_walk key_ptr (nl_cons_cdr_ptr alist_ptr) out)))
        (wf_write_nil out)))
    (defun wf_assoc (args out)
      (wf_assoc_walk (wf_arg_ptr args 0) (wf_arg_ptr args 1) out))
    (defun wf_rassoc_walk (value_ptr alist_ptr out)
      (if (= (ptr-read-u64 alist_ptr 0) 7)
          (let* ((pair_ptr (nl_cons_car_ptr alist_ptr)))
            (if (= (if (= (ptr-read-u64 pair_ptr 0) 7)
                       (wf_key_eq value_ptr (nl_cons_cdr_ptr pair_ptr))
                     0)
                   1)
                (wf_copy32 out pair_ptr)
              (wf_rassoc_walk value_ptr (nl_cons_cdr_ptr alist_ptr) out)))
        (wf_write_nil out)))
    (defun wf_rassoc (args out)
      (wf_rassoc_walk (wf_arg_ptr args 0) (wf_arg_ptr args 1) out)))
  "Reader-only list search helpers for SMIE and Org hot paths.")

;; M5 string + format helpers.  Reader-only: mut-str / str-len / str-byte-at ops
;; lower to extern calls present only in the reader manifest.
(defconst nelisp-standalone--applyfn-m5-helpers
  '((defun m5_push_str_bytes (ms src_ptr i n)
      (if (>= i n) 1
        (seq (mut-str-push-byte ms (str-byte-at src_ptr i))
             (m5_push_str_bytes ms src_ptr (+ i 1) n))))
    (defun m5_push_str (ms src_ptr)
      (m5_push_str_bytes ms src_ptr 0 (str-len src_ptr)))
    (defun m5_push_repeat (ms b count)
      (if (<= count 0) 1
        (seq (mut-str-push-byte ms b)
             (m5_push_repeat ms b (- count 1)))))
    (defun m5_push_udec (ms v)
      (if (< v 10)
          (mut-str-push-byte ms (+ 48 v))
        (seq (m5_push_udec ms (/ v 10))
             (mut-str-push-byte ms (+ 48 (mod v 10))))))
    (defun m5_push_dec (ms v)
      (if (< v 0)
          (seq (mut-str-push-byte ms 45) (m5_push_udec ms (- 0 v)))
        (m5_push_udec ms v)))
    ;; --- real float -> decimal printer (Doc 159) ---------------------
    ;; The integer part is printed exactly; the fractional part is rendered
    ;; to P=15 SIGNIFICANT digits (= DBL_DIG), then rounded and trimmed.
    ;; This is %.15g-style, not shortest-round-trip.  P=15 is the precision
    ;; below which every decimal round-trips, so human-written values
    ;; (0.1, 3.14159, 123.456, 99.99) print cleanly and match Emacs
    ;; `number-to-string'; division/irrational results (1/3, 22/7) print one
    ;; significant digit shorter than Emacs's shortest form but stay clean
    ;; (no f64-noise tail), and very large / small magnitudes stay in fixed
    ;; (non-scientific) notation.  Choosing P=16 instead would match the
    ;; 1/3 family but expose noise tails like 99.99 -> "99.98999999999999".
    (defun m5_idigits (n)            ; decimal digit count of i64 n >= 1
      (if (< n 10) 1 (+ 1 (m5_idigits (/ n 10)))))
    ;; write COUNT fractional digits of FB (bits) into BUF[OFF..], one byte each
    (defun m5_gendigits (buf off count fb scratch)
      (if (>= off count) 0
        (seq
         (nl_sexp_write_float scratch (f64-mul (bits-to-f64 fb) (i64-to-f64 10)))
         (let* ((frbits (ptr-read-u64 scratch 8))
                (d (f64-to-i64-trunc (bits-to-f64 frbits))))
           (seq (ptr-write-u8 buf off d)
                (nl_sexp_write_float scratch
                  (f64-sub (bits-to-f64 frbits) (i64-to-f64 d)))
                (m5_gendigits buf (+ off 1) count (ptr-read-u64 scratch 8) scratch))))))
    ;; propagate CARRY down BUF[J..0]; returns the final carry out of digit 0
    (defun m5_carry (buf j carry)
      (if (< j 0) carry
        (if (= carry 0) 0
          (let* ((nd (+ (ptr-read-u8 buf j) carry)))
            (if (>= nd 10)
                (seq (ptr-write-u8 buf j (- nd 10)) (m5_carry buf (- j 1) 1))
              (seq (ptr-write-u8 buf j nd) 0))))))
    ;; index of the last non-zero byte in BUF[0..K] (>= 0; keeps >=1 digit)
    (defun m5_lastnz (buf k)
      (if (<= k 0) 0
        (if (= (ptr-read-u8 buf k) 0) (m5_lastnz buf (- k 1)) k)))
    (defun m5_pushdigits (ms buf i k)
      (if (> i k) 0
        (seq (mut-str-push-byte ms (+ 48 (ptr-read-u8 buf i)))
             (m5_pushdigits ms buf (+ i 1) k))))
    ;; --- shortest-round-trip printer (Doc 159 Â§10): nl_shortest gives the
    ;; minimal (w,q) with w*10^q parsing back to the double; emit fixed/sci by length.
    (defun m5_wdig (buf w idx) (if (< idx 0) 0 (seq (ptr-write-u8 buf idx (mod w 10)) (m5_wdig buf (/ w 10) (- idx 1)))))
    (defun m5_emit_fixed (ms buf D DE)
      (let* ((ip (+ DE 1)))
        (if (<= ip 0)
            (seq (mut-str-push-byte ms 48) (mut-str-push-byte ms 46) (m5_push_repeat ms 48 (- 0 ip)) (m5_pushdigits ms buf 0 (- D 1)))
          (if (>= ip D)
              (seq (m5_pushdigits ms buf 0 (- D 1)) (m5_push_repeat ms 48 (- ip D)) (mut-str-push-byte ms 46) (mut-str-push-byte ms 48))
            (seq (m5_pushdigits ms buf 0 (- ip 1)) (mut-str-push-byte ms 46) (m5_pushdigits ms buf ip (- D 1)))))))
    (defun m5_emit_sci2 (ms buf D DE)
      (seq (mut-str-push-byte ms (+ 48 (ptr-read-u8 buf 0)))
           (if (> D 1) (seq (mut-str-push-byte ms 46) (m5_pushdigits ms buf 1 (- D 1))) 0)
           (m5_push_exp ms DE 0)))
    (defun m5_emit_sig (ms w q)
      (let* ((D (m5_idigits w)) (DE (- (+ q D) 1)) (buf (alloc-bytes 24 1)))
        (seq (m5_wdig buf w (- D 1))
             (if (= (if (< DE -4) 1 (if (>= DE 15) (if (>= q 1) 1 0) 0)) 1) (m5_emit_sci2 ms buf D DE) (m5_emit_fixed ms buf D DE)))))
    (defun m5_push_float (ms vptr)
      (let* ((bits (sexp-float-unwrap vptr)) (ef (logand (sar bits 52) 2047)) (mag (logand bits 9223372036854775807)))
        (seq
         (if (= (logand (sar bits 63) 1) 1) (mut-str-push-byte ms 45) 0)
         (if (= ef 2047)
             (if (= (logand bits (- (shl 1 52) 1)) 0)
                 (seq (mut-str-push-byte ms 49) (mut-str-push-byte ms 46) (mut-str-push-byte ms 48) (mut-str-push-byte ms 101) (mut-str-push-byte ms 43) (mut-str-push-byte ms 73) (mut-str-push-byte ms 78) (mut-str-push-byte ms 70))
               (seq (mut-str-push-byte ms 48) (mut-str-push-byte ms 46) (mut-str-push-byte ms 48) (mut-str-push-byte ms 101) (mut-str-push-byte ms 43) (mut-str-push-byte ms 78) (mut-str-push-byte ms 97) (mut-str-push-byte ms 78)))
           (if (= mag 0)
               (seq (mut-str-push-byte ms 48) (mut-str-push-byte ms 46) (mut-str-push-byte ms 48))
             (let* ((qslot (alloc-bytes 8 8)) (w (nl_shortest mag qslot)) (q (ptr-read-u64 qslot 0)))
               (m5_emit_sig ms w q)))))))
    (defun m5_push_exp (ms e upcase)       ; "e±NN" / "E±NN"
      (seq (mut-str-push-byte ms (if (= upcase 1) 69 101))
           (if (< e 0) (mut-str-push-byte ms 45) (mut-str-push-byte ms 43))
           (let* ((ea (if (< e 0) (- 0 e) e)))
             (if (< ea 10)
                 (seq (mut-str-push-byte ms 48) (m5_push_udec ms ea))
               (m5_push_udec ms ea)))))
    ;; ---- Doc 159 §11: C99 `%a'/`%A' hex-float conversion ------------------
    ;; format hex-float (a NeLisp superset: host Emacs `format' rejects %a).
    ;; Operates entirely on the IEEE-754 bits (no f64 leaves), so no f64-leaf
    ;; dialect restriction applies.  prec<0 = unspecified (strip trailing zero
    ;; nibbles); prec>=0 = exactly PREC fraction nibbles, round-half-even with
    ;; carry into the leading hex digit.  Matches glibc printf("%a").
    (defun m5_hexf_dig (nib upc)
      (if (< nib 10) (+ 48 nib)
        (if (= upc 1) (+ 55 nib) (+ 87 nib))))
    (defun m5_hexf_nib (frac k)
      (logand (sar frac (- 48 (shl k 2))) 15))
    (defun m5_hexf_last (frac k)
      (if (< k 0) -1
        (if (= (m5_hexf_nib frac k) 0)
            (m5_hexf_last frac (- k 1))
          k)))
    (defun m5_hexf_emit (ms frac k last upc)
      (if (> k last) 1
        (seq (mut-str-push-byte ms (m5_hexf_dig (m5_hexf_nib frac k) upc))
             (m5_hexf_emit ms frac (+ k 1) last upc))))
    (defun m5_hexf_emitp (ms fr j prec upc)
      (if (>= j prec) 1
        (seq (mut-str-push-byte ms
               (m5_hexf_dig (logand (sar fr (shl (- (- prec 1) j) 2)) 15) upc))
             (m5_hexf_emitp ms fr (+ j 1) prec upc))))
    (defun m5_hexf_zeros (ms count)
      (if (<= count 0) 1
        (seq (mut-str-push-byte ms 48) (m5_hexf_zeros ms (- count 1)))))
    (defun m5_hexf_exp (ms e upc)
      (seq (mut-str-push-byte ms (if (= upc 1) 80 112))
           (if (< e 0)
               (seq (mut-str-push-byte ms 45) (m5_push_dec ms (- 0 e)))
             (seq (mut-str-push-byte ms 43) (m5_push_dec ms e)))))
    (defun m5_hexf_word (ms upc which)
      (if (= which 0)
          (if (= upc 1)
              (seq (mut-str-push-byte ms 73) (mut-str-push-byte ms 78) (mut-str-push-byte ms 70))
            (seq (mut-str-push-byte ms 105) (mut-str-push-byte ms 110) (mut-str-push-byte ms 102)))
        (if (= upc 1)
            (seq (mut-str-push-byte ms 78) (mut-str-push-byte ms 65) (mut-str-push-byte ms 78))
          (seq (mut-str-push-byte ms 110) (mut-str-push-byte ms 97) (mut-str-push-byte ms 110)))))
    (defun m5_hexf_body (ms ef frac prec upc)
      (let* ((sub (if (= ef 0) 1 0))
             (lead0 (if (= sub 1) 0 1))
             (exp (if (= sub 1) (if (= frac 0) 0 -1022) (- ef 1023))))
        (if (and (= sub 1) (= frac 0))
            (seq (mut-str-push-byte ms 48)
                 (if (> prec 0)
                     (seq (mut-str-push-byte ms 46) (m5_hexf_zeros ms prec)) 1)
                 (m5_hexf_exp ms 0 upc))
          (if (< prec 0)
              (let* ((last (m5_hexf_last frac 12)))
                (seq (mut-str-push-byte ms (m5_hexf_dig lead0 upc))
                     (if (< last 0) 1
                       (seq (mut-str-push-byte ms 46)
                            (m5_hexf_emit ms frac 0 last upc)))
                     (m5_hexf_exp ms exp upc)))
            (if (>= prec 13)
                (seq (mut-str-push-byte ms (m5_hexf_dig lead0 upc))
                     (mut-str-push-byte ms 46)
                     (m5_hexf_emit ms frac 0 12 upc)
                     (m5_hexf_zeros ms (- prec 13))
                     (m5_hexf_exp ms exp upc))
              (let* ((shift (- 52 (shl prec 2)))
                     (kept (sar frac shift))
                     (dropped (logand frac (- (shl 1 shift) 1)))
                     (half (shl 1 (- shift 1)))
                     (parity (if (> prec 0) (logand kept 1) (logand lead0 1)))
                     (rup (if (> dropped half) 1 (if (< dropped half) 0 parity)))
                     (val (+ (+ (shl lead0 (shl prec 2)) kept) rup))
                     (newlead (sar val (shl prec 2)))
                     (newfrac (logand val (- (shl 1 (shl prec 2)) 1))))
                (seq (mut-str-push-byte ms (m5_hexf_dig newlead upc))
                     (if (> prec 0)
                         (seq (mut-str-push-byte ms 46) (m5_hexf_emitp ms newfrac 0 prec upc)) 1)
                     (m5_hexf_exp ms exp upc))))))))
    (defun m5_fmt_hexfloat (ms fb conv prec buf scratch)
      (let* ((upc (if (= conv 65) 1 0))
             (magbits (logand fb (- (shl 1 63) 1)))
             (ef (logand (sar magbits 52) 2047))
             (frac (logand magbits (- (shl 1 52) 1))))
        (seq (if (< fb 0) (mut-str-push-byte ms 45) 1)
             (if (= ef 2047)
                 (m5_hexf_word ms upc (if (= frac 0) 0 1))
               (seq (mut-str-push-byte ms 48)
                    (mut-str-push-byte ms (if (= upc 1) 88 120))
                    (m5_hexf_body ms ef frac prec upc))))))
    ;; ===== Doc 159 §14: exact big-integer %e/%f/%g formatter ==============
    ;; Print the EXACT decimal value of the double (M*2^be) via base-10^9
    ;; big-integer expansion, then round to the requested precision — matching
    ;; C/Emacs printf across the whole range (subnormals, huge %f, 17+ digit
    ;; precision, half-ties).  All integer arithmetic (no f64 leaves).
    ;; bignum = little-endian array of u64 limbs, each < 10^9.
    (defun m5_bn_init (bn m)
      (if (< m 1000000000) (seq (ptr-write-u64 bn 0 m) 1)
        (seq (ptr-write-u64 bn 0 (mod m 1000000000))
             (ptr-write-u64 bn 8 (/ m 1000000000)) 2)))
    (defun m5_bn_mul (bn len k)            ; bn *= k (k < 2^31), return new len
      (let* ((i 0) (carry 0) (nl len))
        (seq (while (< i len)
               (let* ((tv (+ (* (ptr-read-u64 bn (shl i 3)) k) carry)))
                 (seq (ptr-write-u64 bn (shl i 3) (mod tv 1000000000))
                      (setq carry (/ tv 1000000000)) (setq i (+ i 1)))))
             (while (> carry 0)
               (seq (ptr-write-u64 bn (shl i 3) (mod carry 1000000000))
                    (setq carry (/ carry 1000000000)) (setq i (+ i 1)) (setq nl (+ nl 1))))
             nl)))
    (defun m5_ipow5 (e) (if (= e 0) 1 (* 5 (m5_ipow5 (- e 1)))))
    (defun m5_bn_mul2pow (bn len e)        ; bn *= 2^e
      (let* ((L len))
        (seq (while (>= e 29) (seq (setq L (m5_bn_mul bn L 536870912)) (setq e (- e 29))))
             (if (> e 0) (m5_bn_mul bn L (shl 1 e)) L))))
    (defun m5_bn_mul5pow (bn len e)        ; bn *= 5^e
      (let* ((L len))
        (seq (while (>= e 13) (seq (setq L (m5_bn_mul bn L 1220703125)) (setq e (- e 13))))
             (if (> e 0) (m5_bn_mul bn L (m5_ipow5 e)) L))))
    (defun m5_declead (v out off)          ; v>=1 decimal, no leading zeros
      (if (< v 10) (seq (ptr-write-u8 out off (+ 48 v)) (+ off 1))
        (let* ((noff (m5_declead (/ v 10) out off)))
          (seq (ptr-write-u8 out noff (+ 48 (mod v 10))) (+ noff 1)))))
    (defun m5_dec9 (v out off)             ; v as exactly 9 digits at out+off
      (let* ((i 8))
        (seq (while (>= i 0)
               (seq (ptr-write-u8 out (+ off i) (+ 48 (mod v 10)))
                    (setq v (/ v 10)) (setq i (- i 1))))
             (+ off 9))))
    (defun m5_bn_digits (bn len out)       ; MSB-first decimal of bn, return count
      (let* ((n (m5_declead (ptr-read-u64 bn (shl (- len 1) 3)) out 0)) (i (- len 2)))
        (seq (while (>= i 0)
               (seq (setq n (m5_dec9 (ptr-read-u64 bn (shl i 3)) out n)) (setq i (- i 1))))
             n)))
    (defun m5_bn_mant (fb)                 ; M for value = M*2^be
      (let* ((ef (logand (sar fb 52) 2047)) (frac (logand fb (- (shl 1 52) 1))))
        (if (= ef 0) frac (logior frac (shl 1 52)))))
    (defun m5_bn_be (fb)
      (let* ((ef (logand (sar fb 52) 2047))) (if (= ef 0) -1074 (- ef 1075))))
    (defun m5_bn_pe (fb) (let* ((be (m5_bn_be fb))) (if (< be 0) be 0)))
    (defun m5_exact_digits (fb bn dbuf)    ; digits of N (value = N*10^pe) -> L
      (let* ((m (m5_bn_mant fb)) (be (m5_bn_be fb)) (len (m5_bn_init bn m)))
        (if (>= be 0) (m5_bn_digits bn (m5_bn_mul2pow bn len be) dbuf)
          (m5_bn_digits bn (m5_bn_mul5pow bn len (- 0 be)) dbuf))))
    (defun m5_copy_digits (src so n dst dof)
      (let* ((i 0)) (seq (while (< i n)
                           (seq (ptr-write-u8 dst (+ dof i) (ptr-read-u8 src (+ so i))) (setq i (+ i 1)))) 0)))
    (defun m5_fill_zeros (dst off n)
      (let* ((i 0)) (seq (while (< i n) (seq (ptr-write-u8 dst (+ off i) 48) (setq i (+ i 1)))) 0)))
    (defun m5_digits_nonzero (buf i n)     ; any ASCII digit != '0' in [i,n)?
      (if (>= i n) 0 (if (= (ptr-read-u8 buf i) 48) (m5_digits_nonzero buf (+ i 1) n) 1)))
    (defun m5_inc_digits (dbuf keep rbuf)  ; rbuf = dbuf[0..keep)+1, return keep or keep+1
      (seq (m5_copy_digits dbuf 0 keep rbuf 0)
           (let* ((i (- keep 1)) (carry 1))
             (seq (while (= carry 1)
                    (if (< i 0) (setq carry 2)
                      (let* ((d (+ (- (ptr-read-u8 rbuf i) 48) 1)))
                        (if (>= d 10) (seq (ptr-write-u8 rbuf i 48) (setq i (- i 1)))
                          (seq (ptr-write-u8 rbuf i (+ 48 d)) (setq carry 0))))))
                  (if (= carry 2)
                      (seq (ptr-write-u8 rbuf 0 49) (m5_fill_zeros rbuf 1 keep) (+ keep 1))
                    keep)))))
    (defun m5_round_digits (dbuf L keep rbuf)  ; round dbuf to keep digits (half-even)
      (if (>= keep L) (seq (m5_copy_digits dbuf 0 L rbuf 0) (m5_fill_zeros rbuf L (- keep L)) keep)
        (let* ((rd (ptr-read-u8 dbuf keep))
               (tnz (m5_digits_nonzero dbuf (+ keep 1) L))
               (up (if (> rd 53) 1 (if (< rd 53) 0 (if (= tnz 1) 1 (mod (- (ptr-read-u8 dbuf (- keep 1)) 48) 2))))))
          (if (= up 0) (seq (m5_copy_digits dbuf 0 keep rbuf 0) keep)
            (m5_inc_digits dbuf keep rbuf)))))
    (defun m5_xf_lastnz (buf n)            ; index of last non-'0' digit in [0,n)
      (if (<= n 1) 0 (if (= (ptr-read-u8 buf (- n 1)) 48) (m5_xf_lastnz buf (- n 1)) (- n 1))))
    (defun m5_emit_range (ms buf i j)
      (if (>= i j) 1 (seq (mut-str-push-byte ms (ptr-read-u8 buf i)) (m5_emit_range ms buf (+ i 1) j))))
    (defun m5_xf_place_buf (ms r rlen prec) ; r (rlen digits) with point prec from right
      (if (= prec 0) (m5_emit_range ms r 0 rlen)
        (if (> rlen prec)
            (seq (m5_emit_range ms r 0 (- rlen prec)) (mut-str-push-byte ms 46) (m5_emit_range ms r (- rlen prec) rlen))
          (seq (mut-str-push-byte ms 48) (mut-str-push-byte ms 46)
               (m5_push_repeat ms 48 (- prec rlen)) (m5_emit_range ms r 0 rlen)))))
    (defun m5_xf_zero_f (ms prec)
      (seq (mut-str-push-byte ms 48)
           (if (> prec 0) (seq (mut-str-push-byte ms 46) (m5_push_repeat ms 48 prec)) 1)))
    (defun m5_xf_f (ms fb prec)
      (if (= (logand fb (- (shl 1 63) 1)) 0) (m5_xf_zero_f ms prec)
        (let* ((bn (alloc-bytes 1024 8)) (dbuf (alloc-bytes 1024 1)) (rbuf (alloc-bytes 1024 1))
               (L (m5_exact_digits fb bn dbuf)) (pe (m5_bn_pe fb))
               (E (+ (- L 1) pe)) (keep (+ (+ E prec) 1)))
          (if (< keep 0) (m5_xf_zero_f ms prec)
            (if (= keep 0)
                (let* ((d0 (ptr-read-u8 dbuf 0))
                       (up (if (> d0 53) 1 (if (= d0 53) (m5_digits_nonzero dbuf 1 L) 0))))
                  (if (= up 1) (seq (ptr-write-u8 rbuf 0 49) (m5_xf_place_buf ms rbuf 1 prec)) (m5_xf_zero_f ms prec)))
              (m5_xf_place_buf ms rbuf (m5_round_digits dbuf L keep rbuf) prec))))))
    (defun m5_xf_e (ms fb prec upcase)
      (if (= (logand fb (- (shl 1 63) 1)) 0)
          (seq (mut-str-push-byte ms 48)
               (if (> prec 0) (seq (mut-str-push-byte ms 46) (m5_push_repeat ms 48 prec)) 1)
               (m5_push_exp ms 0 upcase))
        (let* ((bn (alloc-bytes 1024 8)) (dbuf (alloc-bytes 1024 1)) (rbuf (alloc-bytes 1024 1))
               (L (m5_exact_digits fb bn dbuf)) (pe (m5_bn_pe fb)) (E (+ (- L 1) pe))
               (rlen (m5_round_digits dbuf L (+ prec 1) rbuf)) (carry (- rlen (+ prec 1))) (e2 (+ E carry)))
          (seq (mut-str-push-byte ms (ptr-read-u8 rbuf 0))
               (if (> prec 0) (seq (mut-str-push-byte ms 46) (m5_emit_range ms rbuf 1 (+ prec 1))) 1)
               (m5_push_exp ms e2 upcase)))))
    (defun m5_xf_emit_e (ms r slen e2 upcase)  ; %g sci form, slen sig digits
      (seq (mut-str-push-byte ms (ptr-read-u8 r 0))
           (if (> slen 1) (seq (mut-str-push-byte ms 46) (m5_emit_range ms r 1 slen)) 1)
           (m5_push_exp ms e2 upcase)))
    (defun m5_xf_emit_f (ms r slen e2)         ; %g fixed form, slen sig, lead at 10^e2
      (if (>= e2 0)
          (let* ((nint (+ e2 1)))
            (if (>= nint slen) (seq (m5_emit_range ms r 0 slen) (m5_push_repeat ms 48 (- nint slen)))
              (seq (m5_emit_range ms r 0 nint) (mut-str-push-byte ms 46) (m5_emit_range ms r nint slen))))
        (seq (mut-str-push-byte ms 48) (mut-str-push-byte ms 46)
             (m5_push_repeat ms 48 (- (- 0 e2) 1)) (m5_emit_range ms r 0 slen))))
    (defun m5_xf_g (ms fb prec upcase)
      (let* ((p (if (= prec 0) 1 prec)))
        (if (= (logand fb (- (shl 1 63) 1)) 0) (mut-str-push-byte ms 48)
          (let* ((bn (alloc-bytes 1024 8)) (dbuf (alloc-bytes 1024 1)) (rbuf (alloc-bytes 1024 1))
                 (L (m5_exact_digits fb bn dbuf)) (pe (m5_bn_pe fb)) (E (+ (- L 1) pe))
                 (rlen (m5_round_digits dbuf L p rbuf)) (carry (- rlen p)) (e2 (+ E carry))
                 (slen (+ (m5_xf_lastnz rbuf rlen) 1)))
            (if (= (if (< e2 -4) 1 (if (>= e2 p) 1 0)) 1)
                (m5_xf_emit_e ms rbuf slen e2 upcase)
              (m5_xf_emit_f ms rbuf slen e2))))))
    (defun m5_fmt_dispatch (ms fb conv prec buf scratch)
      (if (= conv 97) (m5_fmt_hexfloat ms fb conv prec buf scratch)  ; a
       (if (= conv 65) (m5_fmt_hexfloat ms fb conv prec buf scratch) ; A
        ;; inf/nan for e/f/g print "inf"/"nan" (sign already emitted by
        ;; m5_fmt_float_body); Doc 159 §13.
        (if (= (logand (sar fb 52) 2047) 2047)
            (m5_hexf_word ms 0 (if (= (logand fb (- (shl 1 52) 1)) 0) 0 1))
         (if (= conv 101) (m5_xf_e ms fb prec 0)     ; e
           (if (= conv 69) (m5_xf_e ms fb prec 1)    ; E
             (if (= conv 103) (m5_xf_g ms fb prec 0)   ; g
               (if (= conv 71) (m5_xf_g ms fb prec 1)  ; G
                 (m5_xf_f ms fb prec)))))))))        ; f / F
    (defun m5_fmt_float_body (ms fb conv prec buf scratch)
      ;; Sign from the raw bit, not f64-lt, so -0.0 / -nan emit `-' (Doc 159 §12).
      (if (= (logand (sar fb 63) 1) 1)
          (seq (mut-str-push-byte ms 45)
               (let* ((mb (seq (nl_sexp_write_float scratch
                                 (f64-sub (i64-to-f64 0) (bits-to-f64 fb)))
                               (ptr-read-u64 scratch 8))))
                 (m5_fmt_dispatch ms mb conv prec buf scratch)))
        (m5_fmt_dispatch ms fb conv prec buf scratch)))
    (defun m5_hex_digit (d)
      (if (< d 10) (+ 48 d) (+ 87 d)))
    (defun m5_push_uhex (ms v)
      (if (< v 16)
          (mut-str-push-byte ms (m5_hex_digit v))
        (seq (m5_push_uhex ms (/ v 16))
             (mut-str-push-byte ms (m5_hex_digit (mod v 16))))))
    (defun m5_push_hex (ms v)
      (if (< v 0)
          (seq (mut-str-push-byte ms 45) (m5_push_uhex ms (- 0 v)))
        (m5_push_uhex ms v)))
    (defun m5_hex_digit_up (d)
      (if (< d 10) (+ 48 d) (+ 55 d)))
    (defun m5_push_uhex_up (ms v)
      (if (< v 16)
          (mut-str-push-byte ms (m5_hex_digit_up v))
        (seq (m5_push_uhex_up ms (/ v 16))
             (mut-str-push-byte ms (m5_hex_digit_up (mod v 16))))))
    (defun m5_push_hex_up (ms v)
      (if (< v 0)
          (seq (mut-str-push-byte ms 45) (m5_push_uhex_up ms (- 0 v)))
        (m5_push_uhex_up ms v)))
    (defun m5_push_uoct (ms v)
      (if (< v 8)
          (mut-str-push-byte ms (+ 48 v))
        (seq (m5_push_uoct ms (/ v 8))
             (mut-str-push-byte ms (+ 48 (mod v 8))))))
    (defun m5_push_oct (ms v)
      (if (< v 0)
          (seq (mut-str-push-byte ms 45) (m5_push_uoct ms (- 0 v)))
        (m5_push_uoct ms v)))
    (defun m5_push_char (ms v)
      (if (< v 128)
          (mut-str-push-byte ms v)
        (if (< v 2048)
            (seq (mut-str-push-byte ms (+ 192 (/ v 64)))
                 (mut-str-push-byte ms (+ 128 (mod v 64))))
          (if (< v 65536)
              (seq (mut-str-push-byte ms (+ 224 (/ v 4096)))
                   (mut-str-push-byte ms (+ 128 (mod (/ v 64) 64)))
                   (mut-str-push-byte ms (+ 128 (mod v 64))))
            (seq (mut-str-push-byte ms (+ 240 (/ v 262144)))
                 (mut-str-push-byte ms (+ 128 (mod (/ v 4096) 64)))
                 (mut-str-push-byte ms (+ 128 (mod (/ v 64) 64)))
                 (mut-str-push-byte ms (+ 128 (mod v 64))))))))
    (defun m5_push_lit_nil (ms)
      (seq (mut-str-push-byte ms 110) (mut-str-push-byte ms 105)
           (mut-str-push-byte ms 108) 1))
    (defun m5_push_lit_t (ms)
      (seq (mut-str-push-byte ms 116) 1))
    (defun m5_push_lit_object (ms)
      (seq (mut-str-push-byte ms 35) (mut-str-push-byte ms 60)
           (mut-str-push-byte ms 111) (mut-str-push-byte ms 98)
           (mut-str-push-byte ms 106) (mut-str-push-byte ms 101)
           (mut-str-push-byte ms 99) (mut-str-push-byte ms 116)
           (mut-str-push-byte ms 62) 1))
    ;; ---- m5_sha256: native SHA-256 (RFC 6234).  Avoids the standalone
    ;; secure-hash temp-file + sha256sum subprocess path (~1.7s/call in apply).
    ;; AOT integer model in this unit: `+'/`-'/`>=' are int64; runtime `*' is
    ;; int64 but constant-folded `*' truncates to int32; logand/logior/logxor are
    ;; int32 and sign-extend a bit-31 result to a negative int64; integer literals
    ;; >= 2^31 truncate to int32 inside bit-ops and ptr-write-u64.  So: build big
    ;; constants at runtime via `m5_big' (runtime `*'), normalize bit-op results to
    ;; unsigned via `m5_u', mask additions via if-subtract (no `logand' mask), and
    ;; assemble words with `+'.  Verified against known SHA-256 vectors.
    (defun m5_u (v) (if (< v 0) (+ v 4294967296) v))
    (defun m5_big (hi lo) (+ lo (* hi 65536)))
    (defun m5_sha_s0 (x) (logxor (logxor (logior (* (logand x 127) 33554432) (/ x 128)) (logior (* (logand x 262143) 16384) (/ x 262144))) (/ x 8)))
    (defun m5_sha_s1 (x) (logxor (logxor (logior (* (logand x 131071) 32768) (/ x 131072)) (logior (* (logand x 524287) 8192) (/ x 524288))) (/ x 1024)))
    (defun m5_sha_bs0 (x) (logxor (logxor (logior (* (logand x 3) 1073741824) (/ x 4)) (logior (* (logand x 8191) 524288) (/ x 8192))) (logior (* (logand x 4194303) 1024) (/ x 4194304))))
    (defun m5_sha_bs1 (x) (logxor (logxor (logior (* (logand x 63) 67108864) (/ x 64)) (logior (* (logand x 2047) 2097152) (/ x 2048))) (logior (* (logand x 33554431) 128) (/ x 33554432))))
    (defun m5_sha_ch (x y z) (logxor (logand x y) (logand (- (m5_big 65535 65535) x) z)))
    (defun m5_sha_maj (x y z) (logxor (logxor (logand x y) (logand x z)) (logand y z)))
    (defun m5_sha_add (a b) (let ((s (+ (m5_u a) (m5_u b)))) (if (>= s 4294967296) (- s 4294967296) s)))
    (defun m5_sha_word (msg off)
      (+ (+ (* (ptr-read-u8 msg off) 16777216) (* (ptr-read-u8 msg (+ off 1)) 65536))
         (+ (* (ptr-read-u8 msg (+ off 2)) 256) (ptr-read-u8 msg (+ off 3)))))
    (defun m5_sha_init_k (kbuf)
      (seq
       (ptr-write-u64 (+ kbuf 0) 0 (m5_big 17034 12184))
       (ptr-write-u64 (+ kbuf 8) 0 (m5_big 28983 17553))
       (ptr-write-u64 (+ kbuf 16) 0 (m5_big 46528 64463))
       (ptr-write-u64 (+ kbuf 24) 0 (m5_big 59829 56229))
       (ptr-write-u64 (+ kbuf 32) 0 (m5_big 14678 49755))
       (ptr-write-u64 (+ kbuf 40) 0 (m5_big 23025 4593))
       (ptr-write-u64 (+ kbuf 48) 0 (m5_big 37439 33444))
       (ptr-write-u64 (+ kbuf 56) 0 (m5_big 43804 24277))
       (ptr-write-u64 (+ kbuf 64) 0 (m5_big 55303 43672))
       (ptr-write-u64 (+ kbuf 72) 0 (m5_big 4739 23297))
       (ptr-write-u64 (+ kbuf 80) 0 (m5_big 9265 34238))
       (ptr-write-u64 (+ kbuf 88) 0 (m5_big 21772 32195))
       (ptr-write-u64 (+ kbuf 96) 0 (m5_big 29374 23924))
       (ptr-write-u64 (+ kbuf 104) 0 (m5_big 32990 45566))
       (ptr-write-u64 (+ kbuf 112) 0 (m5_big 39900 1703))
       (ptr-write-u64 (+ kbuf 120) 0 (m5_big 49563 61812))
       (ptr-write-u64 (+ kbuf 128) 0 (m5_big 58523 27073))
       (ptr-write-u64 (+ kbuf 136) 0 (m5_big 61374 18310))
       (ptr-write-u64 (+ kbuf 144) 0 (m5_big 4033 40390))
       (ptr-write-u64 (+ kbuf 152) 0 (m5_big 9228 41420))
       (ptr-write-u64 (+ kbuf 160) 0 (m5_big 11753 11375))
       (ptr-write-u64 (+ kbuf 168) 0 (m5_big 19060 33962))
       (ptr-write-u64 (+ kbuf 176) 0 (m5_big 23728 43484))
       (ptr-write-u64 (+ kbuf 184) 0 (m5_big 30457 35034))
       (ptr-write-u64 (+ kbuf 192) 0 (m5_big 38974 20818))
       (ptr-write-u64 (+ kbuf 200) 0 (m5_big 43057 50797))
       (ptr-write-u64 (+ kbuf 208) 0 (m5_big 45059 10184))
       (ptr-write-u64 (+ kbuf 216) 0 (m5_big 48985 32711))
       (ptr-write-u64 (+ kbuf 224) 0 (m5_big 50912 3059))
       (ptr-write-u64 (+ kbuf 232) 0 (m5_big 54695 37191))
       (ptr-write-u64 (+ kbuf 240) 0 (m5_big 1738 25425))
       (ptr-write-u64 (+ kbuf 248) 0 (m5_big 5161 10599))
       (ptr-write-u64 (+ kbuf 256) 0 (m5_big 10167 2693))
       (ptr-write-u64 (+ kbuf 264) 0 (m5_big 11803 8504))
       (ptr-write-u64 (+ kbuf 272) 0 (m5_big 19756 28156))
       (ptr-write-u64 (+ kbuf 280) 0 (m5_big 21304 3347))
       (ptr-write-u64 (+ kbuf 288) 0 (m5_big 25866 29524))
       (ptr-write-u64 (+ kbuf 296) 0 (m5_big 30314 2747))
       (ptr-write-u64 (+ kbuf 304) 0 (m5_big 33218 51502))
       (ptr-write-u64 (+ kbuf 312) 0 (m5_big 37490 11397))
       (ptr-write-u64 (+ kbuf 320) 0 (m5_big 41663 59553))
       (ptr-write-u64 (+ kbuf 328) 0 (m5_big 43034 26187))
       (ptr-write-u64 (+ kbuf 336) 0 (m5_big 49739 35696))
       (ptr-write-u64 (+ kbuf 344) 0 (m5_big 51052 20899))
       (ptr-write-u64 (+ kbuf 352) 0 (m5_big 53650 59417))
       (ptr-write-u64 (+ kbuf 360) 0 (m5_big 54937 1572))
       (ptr-write-u64 (+ kbuf 368) 0 (m5_big 62478 13701))
       (ptr-write-u64 (+ kbuf 376) 0 (m5_big 4202 41072))
       (ptr-write-u64 (+ kbuf 384) 0 (m5_big 6564 49430))
       (ptr-write-u64 (+ kbuf 392) 0 (m5_big 7735 27656))
       (ptr-write-u64 (+ kbuf 400) 0 (m5_big 10056 30540))
       (ptr-write-u64 (+ kbuf 408) 0 (m5_big 13488 48309))
       (ptr-write-u64 (+ kbuf 416) 0 (m5_big 14620 3251))
       (ptr-write-u64 (+ kbuf 424) 0 (m5_big 20184 43594))
       (ptr-write-u64 (+ kbuf 432) 0 (m5_big 23452 51791))
       (ptr-write-u64 (+ kbuf 440) 0 (m5_big 26670 28659))
       (ptr-write-u64 (+ kbuf 448) 0 (m5_big 29839 33518))
       (ptr-write-u64 (+ kbuf 456) 0 (m5_big 30885 25455))
       (ptr-write-u64 (+ kbuf 464) 0 (m5_big 33992 30740))
       (ptr-write-u64 (+ kbuf 472) 0 (m5_big 36039 520))
       (ptr-write-u64 (+ kbuf 480) 0 (m5_big 37054 65530))
       (ptr-write-u64 (+ kbuf 488) 0 (m5_big 42064 27883))
       (ptr-write-u64 (+ kbuf 496) 0 (m5_big 48889 41975))
       (ptr-write-u64 (+ kbuf 504) 0 (m5_big 50801 30962))))
    (defun m5_sha_fill_w_lo (wbuf msg blk i)
      (if (>= i 16) 0
        (seq (ptr-write-u64 (+ wbuf (* i 8)) 0 (m5_sha_word msg (+ blk (* i 4))))
             (m5_sha_fill_w_lo wbuf msg blk (+ i 1)))))
    (defun m5_sha_fill_w_hi (wbuf i)
      (if (>= i 64) 0
        (seq
         (ptr-write-u64 (+ wbuf (* i 8)) 0
           (m5_sha_add (m5_sha_add (m5_sha_s1 (ptr-read-u64 (+ wbuf (* (- i 2) 8)) 0))
                                   (ptr-read-u64 (+ wbuf (* (- i 7) 8)) 0))
                       (m5_sha_add (m5_sha_s0 (ptr-read-u64 (+ wbuf (* (- i 15) 8)) 0))
                                   (ptr-read-u64 (+ wbuf (* (- i 16) 8)) 0))))
         (m5_sha_fill_w_hi wbuf (+ i 1)))))
    (defun m5_sha_block (hbuf kbuf wbuf msg blk)
      (seq
       (m5_sha_fill_w_lo wbuf msg blk 0)
       (m5_sha_fill_w_hi wbuf 16)
       (let* ((a (ptr-read-u64 hbuf 0)) (b (ptr-read-u64 (+ hbuf 8) 0))
              (c (ptr-read-u64 (+ hbuf 16) 0)) (d (ptr-read-u64 (+ hbuf 24) 0))
              (e (ptr-read-u64 (+ hbuf 32) 0)) (f (ptr-read-u64 (+ hbuf 40) 0))
              (g (ptr-read-u64 (+ hbuf 48) 0)) (h (ptr-read-u64 (+ hbuf 56) 0))
              (i 0))
         (seq
          (while (< i 64)
            (let* ((t1 (m5_sha_add (m5_sha_add (m5_sha_add h (m5_sha_bs1 e))
                                               (m5_sha_add (m5_sha_ch e f g) (ptr-read-u64 (+ kbuf (* i 8)) 0)))
                                   (ptr-read-u64 (+ wbuf (* i 8)) 0)))
                   (t2 (m5_sha_add (m5_sha_bs0 a) (m5_sha_maj a b c))))
              (seq (setq h g) (setq g f) (setq f e) (setq e (m5_sha_add d t1))
                   (setq d c) (setq c b) (setq b a) (setq a (m5_sha_add t1 t2))
                   (setq i (+ i 1)))))
          (ptr-write-u64 hbuf 0 (m5_sha_add (ptr-read-u64 hbuf 0) a))
          (ptr-write-u64 (+ hbuf 8) 0 (m5_sha_add (ptr-read-u64 (+ hbuf 8) 0) b))
          (ptr-write-u64 (+ hbuf 16) 0 (m5_sha_add (ptr-read-u64 (+ hbuf 16) 0) c))
          (ptr-write-u64 (+ hbuf 24) 0 (m5_sha_add (ptr-read-u64 (+ hbuf 24) 0) d))
          (ptr-write-u64 (+ hbuf 32) 0 (m5_sha_add (ptr-read-u64 (+ hbuf 32) 0) e))
          (ptr-write-u64 (+ hbuf 40) 0 (m5_sha_add (ptr-read-u64 (+ hbuf 40) 0) f))
          (ptr-write-u64 (+ hbuf 48) 0 (m5_sha_add (ptr-read-u64 (+ hbuf 48) 0) g))
          (ptr-write-u64 (+ hbuf 56) 0 (m5_sha_add (ptr-read-u64 (+ hbuf 56) 0) h))))))
    (defun m5_sha_zero (msg i n)
      (if (>= i n) 0 (seq (ptr-write-u8 msg i 0) (m5_sha_zero msg (+ i 1) n))))
    (defun m5_sha_copy (msg hay i n)
      (if (>= i n) 0 (seq (ptr-write-u8 msg i (str-byte-at hay i)) (m5_sha_copy msg hay (+ i 1) n))))
    (defun m5_sha_hexnib (ms v)
      (mut-str-push-byte ms (if (< v 10) (+ 48 v) (+ 97 (- v 10)))))
    (defun m5_sha_hexword (ms w)
      (seq (m5_sha_hexnib ms (logand (/ (/ w 65536) 4096) 15)) (m5_sha_hexnib ms (logand (/ w 16777216) 15))
           (m5_sha_hexnib ms (logand (/ w 1048576) 15)) (m5_sha_hexnib ms (logand (/ w 65536) 15))
           (m5_sha_hexnib ms (logand (/ w 4096) 15)) (m5_sha_hexnib ms (logand (/ w 256) 15))
           (m5_sha_hexnib ms (logand (/ w 16) 15)) (m5_sha_hexnib ms (logand w 15))))
    (defun m5_sha256 (ms hay)
      (let* ((len (str-len hay))
             (padlen (* (+ (/ (+ len 8) 64) 1) 64))
             (msg (alloc-bytes padlen 1))
             (kbuf (alloc-bytes 512 8))
             (wbuf (alloc-bytes 512 8))
             (hbuf (alloc-bytes 64 8))
             (bitlen (* len 8))
             (blk 0))
        (seq
         (m5_sha_copy msg hay 0 len)
         (ptr-write-u8 msg len 128)
         (m5_sha_zero msg (+ len 1) (- padlen 4))
         (ptr-write-u8 msg (- padlen 4) (logand (/ bitlen 16777216) 255))
         (ptr-write-u8 msg (- padlen 3) (logand (/ bitlen 65536) 255))
         (ptr-write-u8 msg (- padlen 2) (logand (/ bitlen 256) 255))
         (ptr-write-u8 msg (- padlen 1) (logand bitlen 255))
         (m5_sha_init_k kbuf)
         (ptr-write-u64 (+ hbuf 0) 0 (m5_big 27145 58983))
         (ptr-write-u64 (+ hbuf 8) 0 (m5_big 47975 44677))
         (ptr-write-u64 (+ hbuf 16) 0 (m5_big 15470 62322))
         (ptr-write-u64 (+ hbuf 24) 0 (m5_big 42319 62778))
         (ptr-write-u64 (+ hbuf 32) 0 (m5_big 20750 21119))
         (ptr-write-u64 (+ hbuf 40) 0 (m5_big 39685 26764))
         (ptr-write-u64 (+ hbuf 48) 0 (m5_big 8067 55723))
         (ptr-write-u64 (+ hbuf 56) 0 (m5_big 23520 52505))
         (while (< blk padlen) (seq (m5_sha_block hbuf kbuf wbuf msg blk) (setq blk (+ blk 64))))
         (m5_sha_hexword ms (ptr-read-u64 hbuf 0)) (m5_sha_hexword ms (ptr-read-u64 (+ hbuf 8) 0))
         (m5_sha_hexword ms (ptr-read-u64 (+ hbuf 16) 0)) (m5_sha_hexword ms (ptr-read-u64 (+ hbuf 24) 0))
         (m5_sha_hexword ms (ptr-read-u64 (+ hbuf 32) 0)) (m5_sha_hexword ms (ptr-read-u64 (+ hbuf 40) 0))
         (m5_sha_hexword ms (ptr-read-u64 (+ hbuf 48) 0)) (m5_sha_hexword ms (ptr-read-u64 (+ hbuf 56) 0)))))
    ;; ---- m5_string_search: native substring search (byte compare, no
    ;; per-position allocation).  Backs `nelisp--string-search'; replaces the
    ;; interpreted O(n*m)-with-`substring'-allocation scans that dominate the
    ;; lock-file text parser.  Returns the 0-based index or -1 (-> nil).
    (defun m5_strmatch_at (hay hpos needle npos nlen)
      (if (>= npos nlen) 1
        (if (= (str-byte-at hay (+ hpos npos)) (str-byte-at needle npos))
            (m5_strmatch_at hay hpos needle (+ npos 1) nlen)
          0)))
    (defun m5_string_search (hay needle start)
      (let* ((hlen (str-len hay))
             (nlen (str-len needle))
             (i start)
             (found -1))
        (while (= found -1)
          (if (> (+ i nlen) hlen)
              (setq found -2)
            (if (= (m5_strmatch_at hay i needle 0 nlen) 1)
                (setq found i)
              (setq i (+ i 1)))))
        found))
    ;; ---- m5_json: native JSON encoder (mirrors nelix-cli--json-normalize +
    ;; --json-encode in one native pass).  Output rules: plist(keyword car,even
    ;; len)->object, alist->object, proper list->array, dotted->{car,cdr},
    ;; vector->array, nil/:null->null, t->true, int/float->number, string and
    ;; symbol->JSON string (keyword object keys strip the leading colon).  JSON
    ;; string escaping matches `m5_prin1_string' (",\\,\\n,\\r,\\t).  Tags:
    ;; 0 nil,1 t,2 int,3 float,4 symbol,5/6 string,7 cons,8 vector.
    (defun m5_json_null (ms)
      (seq (mut-str-push-byte ms 110) (mut-str-push-byte ms 117)
           (mut-str-push-byte ms 108) (mut-str-push-byte ms 108) 1))
    (defun m5_json_true (ms)
      (seq (mut-str-push-byte ms 116) (mut-str-push-byte ms 114)
           (mut-str-push-byte ms 117) (mut-str-push-byte ms 101) 1))
    (defun m5_json_is_keyword (vptr)
      (if (= (ptr-read-u64 vptr 0) 4)
          (if (> (str-len vptr) 0)
              (if (= (str-byte-at vptr 0) 58) 1 0) 0) 0))
    (defun m5_json_sym_is_null (vptr)
      (if (= (ptr-read-u64 vptr 0) 4)
          (if (= (str-len vptr) 5)
              (if (= (str-byte-at vptr 0) 58)
                  (if (= (str-byte-at vptr 1) 110)
                      (if (= (str-byte-at vptr 2) 117)
                          (if (= (str-byte-at vptr 3) 108)
                              (if (= (str-byte-at vptr 4) 108) 1 0) 0) 0) 0) 0) 0) 0))
    (defun m5_json_plist_even (node count)
      (let* ((tag (ptr-read-u64 node 0)))
        (if (= tag 0) (if (= count (* (/ count 2) 2)) 1 0)
          (if (= tag 7) (m5_json_plist_even (nl_cons_cdr_ptr node) (+ count 1)) 0))))
    (defun m5_json_plist_p (node)
      (if (= (ptr-read-u64 node 0) 7)
          (if (= (m5_json_is_keyword (nl_cons_car_ptr node)) 1)
              (m5_json_plist_even node 0) 0) 0))
    (defun m5_json_proper_p (node)
      (let* ((tag (ptr-read-u64 node 0)))
        (if (= tag 0) 1 (if (= tag 7) (m5_json_proper_p (nl_cons_cdr_ptr node)) 0))))
    (defun m5_json_alist_p (node)
      (let* ((tag (ptr-read-u64 node 0)))
        (if (= tag 0) 1
          (if (= tag 7)
              (let* ((el (nl_cons_car_ptr node)))
                (if (= (ptr-read-u64 el 0) 7)
                    (if (= (m5_json_is_keyword (nl_cons_car_ptr el)) 1) 0
                      (m5_json_alist_p (nl_cons_cdr_ptr node)))
                  0))
            0))))
    (defun m5_json_str_quoted (ms vptr start)
      (seq (mut-str-push-byte ms 34)
           (m5_prin1_string_bytes ms vptr start (str-len vptr))
           (mut-str-push-byte ms 34)))
    (defun m5_json_object_plist (ms node first)
      (if (= (ptr-read-u64 node 0) 7)
          (let* ((key (nl_cons_car_ptr node))
                 (rest (nl_cons_cdr_ptr node))
                 (val (nl_cons_car_ptr rest)))
            (seq
             (if (= first 1) 0 (mut-str-push-byte ms 44))
             (m5_json_str_quoted ms key 1)
             (mut-str-push-byte ms 58)
             (m5_json ms val)
             (m5_json_object_plist ms (nl_cons_cdr_ptr rest) 0)))
        0))
    (defun m5_json_object_alist (ms node first)
      (if (= (ptr-read-u64 node 0) 7)
          (let* ((el (nl_cons_car_ptr node))
                 (key (nl_cons_car_ptr el))
                 (val (nl_cons_cdr_ptr el)))
            (seq
             (if (= first 1) 0 (mut-str-push-byte ms 44))
             (if (= (ptr-read-u64 key 0) 4)
                 (m5_json_str_quoted ms key 0)
               (m5_json ms key))
             (mut-str-push-byte ms 58)
             (m5_json ms val)
             (m5_json_object_alist ms (nl_cons_cdr_ptr node) 0)))
        0))
    (defun m5_json_array (ms node first)
      (if (= (ptr-read-u64 node 0) 7)
          (seq
           (if (= first 1) 0 (mut-str-push-byte ms 44))
           (m5_json ms (nl_cons_car_ptr node))
           (m5_json_array ms (nl_cons_cdr_ptr node) 0))
        0))
    (defun m5_json_dotted (ms node)
      (seq (mut-str-push-byte ms 123)
           (mut-str-push-byte ms 34) (mut-str-push-byte ms 99)
           (mut-str-push-byte ms 97) (mut-str-push-byte ms 114)
           (mut-str-push-byte ms 34) (mut-str-push-byte ms 58)
           (m5_json ms (nl_cons_car_ptr node))
           (mut-str-push-byte ms 44)
           (mut-str-push-byte ms 34) (mut-str-push-byte ms 99)
           (mut-str-push-byte ms 100) (mut-str-push-byte ms 114)
           (mut-str-push-byte ms 34) (mut-str-push-byte ms 58)
           (m5_json ms (nl_cons_cdr_ptr node))
           (mut-str-push-byte ms 125)))
    (defun m5_json_vector (ms vec i n first)
      (if (>= i n) 0
        (seq
         (if (= first 1) 0 (mut-str-push-byte ms 44))
         (m5_json ms (vector-ref-ptr vec i))
         (m5_json_vector ms vec (+ i 1) n 0))))
    (defun m5_json_symbol (ms vptr)
      (if (= (m5_json_sym_is_null vptr) 1)
          (m5_json_null ms)
        (m5_json_str_quoted ms vptr 0)))
    (defun m5_json (ms vptr)
      (let* ((tag (ptr-read-u64 vptr 0)))
        (cond
         ((= tag 0) (m5_json_null ms))
         ((= tag 1) (m5_json_true ms))
         ((= tag 2) (m5_push_dec ms (ptr-read-u64 vptr 8)))
         ((= tag 3) (m5_push_float ms vptr))
         ((= tag 4) (m5_json_symbol ms vptr))
         ((= tag 5) (m5_prin1_string ms vptr))
         ((= tag 6) (m5_prin1_string ms vptr))
         ((= tag 7)
          (if (= (m5_json_plist_p vptr) 1)
              (seq (mut-str-push-byte ms 123)
                   (m5_json_object_plist ms vptr 1)
                   (mut-str-push-byte ms 125))
            (if (= (m5_json_alist_p vptr) 1)
                (seq (mut-str-push-byte ms 123)
                     (m5_json_object_alist ms vptr 1)
                     (mut-str-push-byte ms 125))
              (if (= (m5_json_proper_p vptr) 1)
                  (seq (mut-str-push-byte ms 91)
                       (m5_json_array ms vptr 1)
                       (mut-str-push-byte ms 93))
                (m5_json_dotted ms vptr)))))
         ((= tag 8)
          (seq (mut-str-push-byte ms 91)
               (m5_json_vector ms vptr 0 (vector-len vptr) 1)
               (mut-str-push-byte ms 93)))
         (t (m5_json_null ms)))))
    (defun m5_prin1_string_byte (ms b)
      (cond
       ((= b 34) (seq (mut-str-push-byte ms 92) (mut-str-push-byte ms 34)))
       ((= b 92) (seq (mut-str-push-byte ms 92) (mut-str-push-byte ms 92)))
       ((= b 10) (seq (mut-str-push-byte ms 92) (mut-str-push-byte ms 110)))
       ((= b 13) (seq (mut-str-push-byte ms 92) (mut-str-push-byte ms 114)))
       ((= b 9) (seq (mut-str-push-byte ms 92) (mut-str-push-byte ms 116)))
       (t (mut-str-push-byte ms b))))
    (defun m5_prin1_string_bytes (ms src_ptr i n)
      (if (>= i n)
          1
        (seq
         (m5_prin1_string_byte ms (str-byte-at src_ptr i))
         (m5_prin1_string_bytes ms src_ptr (+ i 1) n))))
    (defun m5_prin1_string (ms vptr)
      (seq (mut-str-push-byte ms 34)
           (m5_prin1_string_bytes ms vptr 0 (str-len vptr))
           (mut-str-push-byte ms 34)))
    (defun m5_prin1_list_tail (ms node first)
      (let* ((tag (ptr-read-u64 node 0)))
        (if (= tag 7)
            (seq
             (if (= first 1) 0 (mut-str-push-byte ms 32))
             (m5_prin1 ms (nl_cons_car_ptr node))
             (m5_prin1_list_tail ms (nl_cons_cdr_ptr node) 0))
          (if (= tag 0)
              (mut-str-push-byte ms 41)
            (seq
             (mut-str-push-byte ms 32) (mut-str-push-byte ms 46)
             (mut-str-push-byte ms 32)
             (m5_prin1 ms node)
             (mut-str-push-byte ms 41))))))
    (defun m5_prin1_vector_loop (ms vec i n)
      (if (>= i n)
          (mut-str-push-byte ms 93)
        (seq
         (if (= i 0) 0 (mut-str-push-byte ms 32))
         (m5_prin1 ms (vector-ref-ptr vec i))
         (m5_prin1_vector_loop ms vec (+ i 1) n))))
    (defun m5_prin1 (ms vptr)
      (let* ((tag (ptr-read-u64 vptr 0)))
        (cond
         ((= tag 0) (m5_push_lit_nil ms))
         ((= tag 1) (m5_push_lit_t ms))
         ((= tag 2) (m5_push_dec ms (ptr-read-u64 vptr 8)))
         ((= tag 3) (m5_push_float ms vptr))
         ((= tag 4) (m5_push_str ms vptr))
         ((= tag 5) (m5_prin1_string ms vptr))
         ((= tag 6) (m5_prin1_string ms vptr))
         ((= tag 7) (seq (mut-str-push-byte ms 40)
                         (m5_prin1_list_tail ms vptr 1)))
         ((= tag 8) (seq (mut-str-push-byte ms 91)
                         (m5_prin1_vector_loop ms vptr 0 (vector-len vptr))))
         (t (m5_push_lit_object ms)))))
    (defun m5_emit_value (ms vptr)
      (let* ((tag (ptr-read-u64 vptr 0)))
        (if (= tag 2) (m5_push_dec ms (ptr-read-u64 vptr 8))
          (if (= tag 3) (m5_push_float ms vptr)
            (if (= tag 5) (m5_push_str ms vptr)
              (if (= tag 4) (m5_push_str ms vptr)
                (if (= tag 6) (m5_push_str ms vptr)
                  ;; tag 0/1/7/8 (nil/t/cons/vector): fall back to full prin1
                  ;; so `format' %s/%S render them instead of emitting nothing.
                  (m5_prin1 ms vptr))))))))
    (defun m5_s2n_mag (s i n acc)
      (if (>= i n) acc
        (let* ((c (str-byte-at s i)))
          (if (>= c 48)
              (if (<= c 57)
                  (m5_s2n_mag s (+ i 1) n (+ (* acc 10) (- c 48)))
                acc)
            acc))))
    (defun m5_s2n (s)
      (let* ((n (str-len s)))
        (if (= n 0) 0
          (let* ((c0 (str-byte-at s 0)))
            (if (= c0 45) (- 0 (m5_s2n_mag s 1 n 0))
              (if (= c0 43) (m5_s2n_mag s 1 n 0)
                (m5_s2n_mag s 0 n 0)))))))
    (defun m5_streq_bytes (a b i n)
      (if (>= i n) 1
        (if (= (str-byte-at a i) (str-byte-at b i))
            (m5_streq_bytes a b (+ i 1) n)
          0)))
    (defun m5_streq (a b)
      (if (= (str-len a) (str-len b))
          (m5_streq_bytes a b 0 (str-len a))
        0))
    (defun m5_list_len (p acc)
      (if (= (ptr-read-u64 p 0) 7)
          (m5_list_len (nl_cons_cdr_ptr p) (+ acc 1))
        acc))
    (defun m5_length (p)
      (let* ((tag (ptr-read-u64 p 0)))
        (if (= tag 5) (str-len p)
          (if (= tag 6) (str-len p)
            (if (= tag 4) (str-len p)
              (if (= tag 7) (m5_list_len p 0)
                0))))))
    (defun m5_concat_walk (ms cur)
      (if (= (ptr-read-u64 cur 0) 7)
          (let* ((carp (nl_cons_car_ptr cur)))
            (seq (if (= (ptr-read-u64 carp 0) 0)
                     1
                   (m5_emit_value ms carp))
                 (m5_concat_walk ms (nl_cons_cdr_ptr cur))))
        1))
    (defun m5_fmt_argptr (argp)
      (nl_cons_car_ptr argp))
    (defun m5_fmt_loop (ms fmt i n argp)
      (if (>= i n) 1
        (let* ((c (str-byte-at fmt i)))
          (if (= c 37)
              (if (>= (+ i 1) n)
                  (seq (mut-str-push-byte ms 37) 1)
                (let* ((d (str-byte-at fmt (+ i 1))))
                  (if (= d 37)
                      (seq (mut-str-push-byte ms 37)
                           (m5_fmt_loop ms fmt (+ i 2) n argp))
                    (if (= d 100)
                        (seq (m5_push_dec ms (ptr-read-u64 (m5_fmt_argptr argp) 8))
                             (m5_fmt_loop ms fmt (+ i 2) n (nl_cons_cdr_ptr argp)))
                      (if (= d 120)
                          (seq (m5_push_hex ms (ptr-read-u64 (m5_fmt_argptr argp) 8))
                               (m5_fmt_loop ms fmt (+ i 2) n (nl_cons_cdr_ptr argp)))
                        (if (= d 102)
                            (seq (m5_emit_value ms (m5_fmt_argptr argp))
                                 (m5_fmt_loop ms fmt (+ i 2) n (nl_cons_cdr_ptr argp)))
                          (if (= d 103)
                              (seq (m5_emit_value ms (m5_fmt_argptr argp))
                                   (m5_fmt_loop ms fmt (+ i 2) n (nl_cons_cdr_ptr argp)))
                            (if (= d 115)
                            (seq (m5_emit_value ms (m5_fmt_argptr argp))
                                 (m5_fmt_loop ms fmt (+ i 2) n (nl_cons_cdr_ptr argp)))
                              (if (= d 83)
                                  (seq (m5_emit_value ms (m5_fmt_argptr argp))
                                       (m5_fmt_loop ms fmt (+ i 2) n (nl_cons_cdr_ptr argp)))
                                (if (= d 105)
                                    (seq (m5_push_dec ms (ptr-read-u64 (m5_fmt_argptr argp) 8))
                                         (m5_fmt_loop ms fmt (+ i 2) n (nl_cons_cdr_ptr argp)))
                                  (if (= d 88)
                                      (seq (m5_push_hex_up ms (ptr-read-u64 (m5_fmt_argptr argp) 8))
                                           (m5_fmt_loop ms fmt (+ i 2) n (nl_cons_cdr_ptr argp)))
                                    (if (= d 111)
                                        (seq (m5_push_oct ms (ptr-read-u64 (m5_fmt_argptr argp) 8))
                                             (m5_fmt_loop ms fmt (+ i 2) n (nl_cons_cdr_ptr argp)))
                                      (if (= d 99)
                                          (seq (m5_push_char ms (ptr-read-u64 (m5_fmt_argptr argp) 8))
                                               (m5_fmt_loop ms fmt (+ i 2) n (nl_cons_cdr_ptr argp)))
                                        (seq (mut-str-push-byte ms 37)
                                             (mut-str-push-byte ms d)
                                             (m5_fmt_loop ms fmt (+ i 2) n argp)))))))))))))))
            (seq (mut-str-push-byte ms c)
                 (m5_fmt_loop ms fmt (+ i 1) n argp)))))))
  "M5 string + format helpers (reader-only).")

;; ===================================================================
;; B-foundation breadth helpers (Wave-1 (B)).  Reader-only: they use the
;; vector grammar ops (vector-make/-slot-set/-ref-ptr/-len -> nl_vector_*),
;; symbol-eq / str-eq, nl_alloc_symbol / nl_alloc_str, nelisp_env_lookup_function
;; and nelisp_mirror_is_bound -- all present only in the reader manifest.  They
;; back the new predicate / vector / symbol / setcar/setcdr / equal arms AND the
;; nil-safe car/cdr + tag-aware eq REPLACEMENTS (the two highest-priority fixes:
;; the stock `car'/`cdr' arms deref nl_cons_car_ptr's 0 on a non-cons -> SIGSEGV;
;; the stock `eq' arm compares value@8 -> `(eq 'a 'b)' truthy, breaking
;; assq/memq/plist).
;; ===================================================================
(defconst nelisp-standalone--applyfn-bf-helpers
  '(;; symbol-name str-bytes -> Symbol(tag4).  A Str (tag5) keeps ptr@16/len@24,
    ;; a MutStr (tag6) wraps NlStr* @8 with ptr@8/len@16.  nl_alloc_symbol takes
    ;; (bytes-ptr len result-slot).
    (defun bf_str_ptr (sx)
      (if (= (ptr-read-u64 sx 0) 6)
          (ptr-read-u64 (ptr-read-u64 sx 8) 8)
        (ptr-read-u64 sx 16)))
    (defun bf_str_len (sx)
      (if (= (ptr-read-u64 sx 0) 6)
          (ptr-read-u64 (ptr-read-u64 sx 8) 16)
        (ptr-read-u64 sx 24)))
    (defun bf_intern (sx out)
      (nl_alloc_symbol (bf_str_ptr sx) (bf_str_len sx) out))
    ;; Doc 22 A11: `make-symbol' must yield a symbol with DISTINCT identity.
    ;; This reader has no obarray -- variable lookup and `eq' compare symbols by
    ;; NAME (name == identity, see bf_eq2 tag-4 arm), so routing make-symbol
    ;; through bf_intern gave two (make-symbol "g") the same name "g" and they
    ;; collided (hygiene break: parallel-assignment temps fused).  Mirror the
    ;; out-of-band `nl_jit_make_symbol' kernel: build a unique name
    ;; NAME__nelisp-uninterned-XXXXXXXXXXXXXXXX where X..X is 16 hex digits of a
    ;; monotonic counter (the mutation-epoch slot at 268435544, bumped via
    ;; atomic-fetch-add; its reset is disabled so it is a stable per-process
    ;; counter).  nl_alloc_symbol copies the bytes, so the scratch buffer is
    ;; reclaimed with the per-eval arena.  Uses sar/shl (AOT shift dialect).
    (defun bf_mksym_copy (src buf i n)
      (if (>= i n) 1
        (seq (ptr-write-u8 buf i (str-byte-at src i))
             (bf_mksym_copy src buf (+ i 1) n))))
    (defun bf_mksym_suffix (buf s)
      (and (ptr-write-u8 buf s 95)        (ptr-write-u8 buf (+ s 1) 95)
           (ptr-write-u8 buf (+ s 2) 110) (ptr-write-u8 buf (+ s 3) 101)
           (ptr-write-u8 buf (+ s 4) 108) (ptr-write-u8 buf (+ s 5) 105)
           (ptr-write-u8 buf (+ s 6) 115) (ptr-write-u8 buf (+ s 7) 112)
           (ptr-write-u8 buf (+ s 8) 45)  (ptr-write-u8 buf (+ s 9) 117)
           (ptr-write-u8 buf (+ s 10) 110)(ptr-write-u8 buf (+ s 11) 105)
           (ptr-write-u8 buf (+ s 12) 110)(ptr-write-u8 buf (+ s 13) 116)
           (ptr-write-u8 buf (+ s 14) 101)(ptr-write-u8 buf (+ s 15) 114)
           (ptr-write-u8 buf (+ s 16) 110)(ptr-write-u8 buf (+ s 17) 101)
           (ptr-write-u8 buf (+ s 18) 100)(ptr-write-u8 buf (+ s 19) 45)))
    (defun bf_mksym_hex (n buf start shift)
      (if (< shift 0) 1
        (seq (ptr-write-u8 buf (+ start (- 15 shift))
                           (if (< (logand (sar n (shl shift 2)) 15) 10)
                               (+ (logand (sar n (shl shift 2)) 15) 48)
                             (+ (logand (sar n (shl shift 2)) 15) 87)))
             (bf_mksym_hex n buf start (- shift 1)))))
    (defun bf_make_symbol (sx out)
      (let* ((n (bf_str_len sx))
             (buf (alloc-bytes (+ n 36) 1)))
        (seq (bf_mksym_copy sx buf 0 n)
             (bf_mksym_suffix buf n)
             (bf_mksym_hex (atomic-fetch-add 268435544 1) buf (+ n 20) 15)
             (nl_alloc_symbol buf (+ n 36) out)
             0)))
    ;; Doc 22 A14 (test harness): `record' / `recordp'.  The reader left these
    ;; VOID so no tag-12 Record could be built (and thus the aref-on-record
    ;; SEGFAULT could not be reproduced on the bare reader).  `(record TYPE
    ;; SLOTS...)' = type tag (car) + data slots (cdr); built with the
    ;; `record-make' / `record-slot-set' grammar ops, mirroring bf_make_vector.
    (defun bf_record_count (node acc)
      (if (= (ptr-read-u64 node 0) 7)
          (bf_record_count (nl_cons_cdr_ptr node) (+ acc 1))
        acc))
    (defun bf_record_fill (rec i node)
      (if (= (ptr-read-u64 node 0) 7)
          (seq (record-slot-set rec i (nl_cons_car_ptr node))
               (bf_record_fill rec (+ i 1) (nl_cons_cdr_ptr node)))
        0))
    (defun bf_record (args out)
      (let* ((tag (nl_cons_car_ptr args))
             (slots (nl_cons_cdr_ptr args))
             (n (bf_record_count slots 0)))
        (seq (record-make tag n out)
             (bf_record_fill out 0 slots)
             0)))
    (defun bf_recordp (args out)
      (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 12)
          (wf_write_t out) (wf_write_nil out)))
    ;; make-record TYPE SLOT-COUNT INIT -> tag-12 Record with SLOT-COUNT data
    ;; slots each = INIT.  Mirrors bf_make_vector (constant fill), the sibling
    ;; of bf_record (list fill).  Doc 22 A14.
    (defun bf_mkrec_fill (rec i n init)
      (if (>= i n) 0
        (seq (record-slot-set rec i init)
             (bf_mkrec_fill rec (+ i 1) n init))))
    (defun bf_make_record (args out)
      (let* ((tag (wf_arg_ptr args 0))
             (n (ptr-read-u64 (wf_arg_ptr args 1) 8))
             (init (wf_arg_ptr args 2)))
        (seq (record-make tag n out)
             (bf_mkrec_fill out 0 n init)
             0)))
    ;; make-vector LEN INIT -> Vector(tag8) with every slot = INIT (cloned).
    ;; NB: use the `vector-slot-set' GRAMMAR OP (takes the Sexp ptr, derefs the
    ;; box internally), NOT the raw nl_vector_set_slot (which wants the box ptr).
    (defun bf_mv_fill (vec i n init)
      (if (>= i n) 0
        (seq (vector-slot-set vec i init)
             (bf_mv_fill vec (+ i 1) n init))))
    (defun bf_make_vector (args out)
      (let* ((n (ptr-read-u64 (wf_arg_ptr args 0) 8))
             (init (wf_arg_ptr args 1)))
        (seq (vector-make n out)
             (bf_mv_fill out 0 n init)
             0)))
    ;; vector &rest -> Vector(tag8) from the arg list.
    (defun bf_vec_fill (vec i node)
      (if (= (ptr-read-u64 node 0) 7)
          (seq (vector-slot-set vec i (nl_cons_car_ptr node))
               (bf_vec_fill vec (+ i 1) (nl_cons_cdr_ptr node)))
        0))
    (defun bf_vec_count (node acc)
      (if (= (ptr-read-u64 node 0) 7)
          (bf_vec_count (nl_cons_cdr_ptr node) (+ acc 1))
        acc))
    (defun bf_vector (args out)
      (let* ((n (bf_vec_count args 0)))
        (seq (vector-make n out)
             (bf_vec_fill out 0 args)
             0)))
    ;; unibyte-string &rest BYTES -> raw byte string.  The ELF writer builds
    ;; buffers from byte values 0..255, so clamp every arg to its low byte and
    ;; append it directly to a MutStr builder instead of UTF-8 codepoint logic.
    (defun bf_unibyte_fill (ms node)
      (if (= (ptr-read-u64 node 0) 7)
          (seq (mut-str-push-byte ms
                                  (logand (ptr-read-u64 (nl_cons_car_ptr node) 8)
                                          255))
               (bf_unibyte_fill ms (nl_cons_cdr_ptr node)))
        0))
    (defun bf_unibyte_string (args out)
      (let* ((ms (alloc-bytes 32 8)))
        (seq (mut-str-make-empty ms (bf_vec_count args 0))
             (bf_unibyte_fill ms args)
             (mut-str-finalize ms out)
             0)))
    ;; aref ARR IDX: vector -> copy slot[idx]; string -> int byte at idx.
    (defun bf_aref (args out)
      (let* ((arr (wf_arg_ptr args 0))
             (idx (ptr-read-u64 (wf_arg_ptr args 1) 8))
             (tg (ptr-read-u64 arr 0)))
        (if (= tg 8)
            (if (if (< idx 0) 1 (if (< idx (vector-len arr)) 0 1))
                (seq (wf_write_nil out) 0)
              (seq (wf_copy32 out (vector-ref-ptr arr idx)) 0))
          ;; Doc 22 A14: Record(12).  The stock else-arm fell through to
          ;; `str-byte-at', dereferencing the record Sexp's offset-16 word as a
          ;; string data pointer (= the SEGFAULT the doc observed on a struct).
          ;; Host Emacs records expose the type tag at index 0 and data slots at
          ;; index 1..N; this reader stores the type tag separately (box+0) from
          ;; the slots vector, so aref 0 -> `record-type-tag', aref k>0 ->
          ;; `record-slot-ref' of data slot k-1.
          (if (= tg 12)
              (if (< idx 0)
                  (seq (wf_write_nil out) 0)
                (if (= idx 0)
                    (seq (record-type-tag arr out) 0)
                  (seq (record-slot-ref arr (- idx 1) out) 0)))
            (if (= tg 5)
                (if (if (< idx 0) 1 (if (< idx (str-len arr)) 0 1))
                    (seq (wf_write_nil out) 0)
                  (wf_write_int out (str-byte-at arr idx)))
              (seq (wf_write_nil out) 0))))))
    ;; Generated Emacs char-table literals are read as vectors shaped like:
    ;;   #^[EXTRA0 EXTRA1 EXTRA2 #^^[1 MIN ...]]
    ;; and sub-char-tables are vectors shaped like:
    ;;   #^^[LEVEL MIN SLOT...]
    ;; LEVEL 1 indexes 4096-char children, LEVEL 2 indexes 128-char
    ;; children, and LEVEL 3 stores direct character entries.
    (defun bf_ct_subtable_lookup (tab ch out)
      (if (= (ptr-read-u64 tab 0) 8)
          (let* ((len (vector-len tab))
                 (level-p (vector-ref-ptr tab 0))
                 (min-p (vector-ref-ptr tab 1)))
            (if (< len 2)
                (seq (wf_write_nil out) 0)
              (let* ((level (ptr-read-u64 level-p 8))
                     (min (ptr-read-u64 min-p 8)))
                (if (< ch min)
                    (seq (wf_write_nil out) 0)
                  (let* ((delta (- ch min)))
                    (cond
                     ((= level 1)
                      (let* ((slot (+ 2 (/ delta 4096))))
                        (if (< slot len)
                            (let* ((entry (vector-ref-ptr tab slot)))
                              (if (= (ptr-read-u64 entry 0) 8)
                                  (bf_ct_subtable_lookup entry ch out)
                                (seq (wf_copy32 out entry) 0)))
                          (seq (wf_write_nil out) 0))))
                     ((= level 2)
                      (let* ((slot (+ 2 (/ delta 128))))
                        (if (< slot len)
                            (let* ((entry (vector-ref-ptr tab slot)))
                              (if (= (ptr-read-u64 entry 0) 8)
                                  (bf_ct_subtable_lookup entry ch out)
                                (seq (wf_copy32 out entry) 0)))
                          (seq (wf_write_nil out) 0))))
                     ((= level 3)
                      (let* ((slot (+ 2 delta)))
                        (if (< slot len)
                            (seq (wf_copy32 out (vector-ref-ptr tab slot)) 0)
                          (seq (wf_write_nil out) 0))))
                     (t (seq (wf_write_nil out) 0))))))))
        (seq (wf_write_nil out) 0)))
    (defun bf_ct_subtable_span (tab)
      (if (= (ptr-read-u64 tab 0) 8)
          (if (> (vector-len tab) 1)
              (let* ((level-p (vector-ref-ptr tab 0))
                     (level (ptr-read-u64 level-p 8)))
                (cond
                 ((= level 1) 65536)
                 ((= level 2) 4096)
                 ((= level 3) 128)
                 (t 0)))
            0)
        0))
    (defun bf_ct_subtable_min (tab)
      (if (= (ptr-read-u64 tab 0) 8)
          (if (> (vector-len tab) 1)
              (let* ((min-p (vector-ref-ptr tab 1)))
                (ptr-read-u64 min-p 8))
            0)
        0))
    (defun bf_ct_top_lookup_walk (table ch out i len)
      (if (>= i len)
          (seq (wf_write_nil out) 0)
        (let* ((entry (vector-ref-ptr table i)))
          (if (= (ptr-read-u64 entry 0) 8)
              (let* ((span (bf_ct_subtable_span entry))
                     (min (bf_ct_subtable_min entry)))
                (if (and (> span 0) (>= ch min) (< ch (+ min span)))
                    (bf_ct_subtable_lookup entry ch out)
                  (bf_ct_top_lookup_walk table ch out (+ i 1) len)))
            (bf_ct_top_lookup_walk table ch out (+ i 1) len)))))
    (defun bf_ct_top_lookup (table ch out)
      (bf_ct_top_lookup_walk table ch out 3 (vector-len table)))
    (defun bf_ct_top_p (table)
      (if (= (ptr-read-u64 table 0) 8)
          (if (> (vector-len table) 3)
              (let* ((extra0 (vector-ref-ptr table 0))
                     (extra1 (vector-ref-ptr table 1))
                     (extra2 (vector-ref-ptr table 2))
                     (root (vector-ref-ptr table 3)))
                (if (and (= (ptr-read-u64 extra0 0) 0)
                         (= (ptr-read-u64 extra1 0) 0)
                         (= (ptr-read-u64 extra2 0) 0)
                         (= (ptr-read-u64 root 0) 8))
                    (if (> (vector-len root) 1)
                        (let* ((level-p (vector-ref-ptr root 0))
                               (level (ptr-read-u64 level-p 8)))
                          (if (and (>= level 1) (<= level 3)) 1 0))
                      0)
                  0))
            0)
        0))
    (defun bf_elt_list_walk (node idx out)
      (if (= (ptr-read-u64 node 0) 7)
          (if (= idx 0)
              (seq (wf_copy32 out (nl_cons_car_ptr node)) 0)
            (bf_elt_list_walk (nl_cons_cdr_ptr node) (- idx 1) out))
        (seq (wf_write_nil out) 0)))
    (defun bf_elt (args out)
      (let* ((seqp (wf_arg_ptr args 0))
             (idx (ptr-read-u64 (wf_arg_ptr args 1) 8))
             (tg (ptr-read-u64 seqp 0)))
        (cond
         ((= tg 8)
          (if (= (bf_ct_top_p seqp) 1)
              (bf_ct_top_lookup seqp idx out)
            (seq (wf_copy32 out (vector-ref-ptr seqp idx)) 0)))
         ((= tg 7) (bf_elt_list_walk seqp idx out))
         (t (wf_write_int out (str-byte-at seqp idx))))))
    ;; aset ARR IDX VAL: vector slot set (string aset not supported -> 0).
    ;; Writes VAL (possibly a box) into a PRE-EXISTING vector -> persistent
    ;; escape -> bump the mutation epoch (NO-ESCAPE gate).
    (defun bf_aset (args out)
      (let* ((arr (wf_arg_ptr args 0))
             (idx (ptr-read-u64 (wf_arg_ptr args 1) 8))
             (val (wf_arg_ptr args 2)))
        (if (= (ptr-read-u64 arr 0) 8)
            (seq (wf_dirty) (vector-slot-set arr idx val)
                 (wf_copy32 out val) 0)
          ;; Doc 22 A4: Str(5)/MutStr(6) in-place single-byte write.  The stock
          ;; arm was a silent no-op (strings looked immutable).  Mirrors bf_aref's
          ;; byte-indexed string model (char index == byte index; ASCII).  Safe
          ;; only when the target owns its buffer -- callers must `copy-sequence'
          ;; a literal first (now a real deep copy, Doc 22 A4 prelude half);
          ;; direct aset on a literal is undefined, as in host Emacs.  Str(5)
          ;; data ptr @16; MutStr(6) NlStr* @8 -> buffer @ +8.
          (if (= (ptr-read-u64 arr 0) 5)
              (seq (wf_dirty)
                   (ptr-write-u8 (ptr-read-u64 arr 16) idx (ptr-read-u64 val 8))
                   (wf_copy32 out val) 0)
            (if (= (ptr-read-u64 arr 0) 6)
                (seq (wf_dirty)
                     (ptr-write-u8 (ptr-read-u64 (ptr-read-u64 arr 8) 8) idx
                                   (ptr-read-u64 val 8))
                     (wf_copy32 out val) 0)
              ;; Doc 156: Record(12) in-place slot write, mirroring bf_aref's
              ;; tag-12 read (aref 0 = type tag, aref k>0 = data slot k-1).  The
              ;; stock else-arm was a silent no-op, so `(setf (struct-slot r) v)'
              ;; (= nelisp--record-set -> aset) never mutated a genuine tag-12
              ;; record -- breaking cl-defstruct setters once nelisp--make-record
              ;; builds native records (Doc 22 A14 follow-up).  Uses the
              ;; `record-slot-set' grammar op (as bf_record / bf_make_record).
              (if (= (ptr-read-u64 arr 0) 12)
                  (if (= idx 0)
                      (seq (wf_copy32 out val) 0)
                    (seq (wf_dirty) (record-slot-set arr (- idx 1) val)
                         (wf_copy32 out val) 0))
                (seq (wf_copy32 out val) 0)))))))
    ;; signal/error: NON-CRASHING.  Stash (sym . data) into the catch/throw
    ;; region + set the throw flag, then return rc=1 so the rc!=0 propagation
    ;; unwinds the native stack like an error.
    ;; Stash layout (reserved arena): flag@268435472, TAG@268435480, VAL@268435512.
    ;;
    ;; WAVE-2 (PATCH 2): both bf_signal and bf_error stash a CANONICAL Symbol
    ;; (tag 4) re-allocated from its NAME BYTES via `nl_alloc_symbol' (= the
    ;; bf_intern / nl_install_one path), so the symbol the condition-case handler
    ;; matches on (`nelisp_eq_symbol' in nl_cc_match_and_bind) sees the exact box
    ;; layout nl_alloc_symbol produces.  The OLD raw 32-byte copy of the runtime
    ;; symbol arg carried a box layout that broke clone/symbol-eq, so
    ;; condition-case never matched.  bf_signal re-allocs from the passed symbol's
    ;; name bytes (ptr@16/len@24 of a tag-4 Symbol); bf_error stashes the literal
    ;; `error' symbol (elisp `(error MSG)' signals the `error' condition).
    (defun bf_sig_copy32 (dst src)
      (seq (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
           (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
           (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
           (ptr-write-u64 dst 24 (ptr-read-u64 src 24)) 0))
    (defun bf_signal (args out)
      (let* ((sym (wf_arg_ptr args 0))
             (data (wf_arg_ptr args 1)))
        (seq
         (nl_alloc_symbol (ptr-read-u64 sym 16) (ptr-read-u64 sym 24) 268435480)
         (bf_sig_copy32 268435512 data)
         (ptr-write-u64 268435472 0 1)
         (atomic-fetch-add 268435544 1)
         1)))
    (defun bf_error (args out)
      (let* ((ebuf (alloc-bytes 8 1)))
        (seq
         (ptr-write-u64 ebuf 0 491496043109)              ; "error" packed LE
         (nl_alloc_symbol ebuf 5 268435480)               ; TAG slot <- Symbol "error"
         (bf_sig_copy32 268435512 (nl_cons_cdr_ptr args)) ; VAL slot <- (MSG ...)
         (ptr-write-u64 268435472 0 1)                    ; raise flag
         (atomic-fetch-add 268435544 1)
         1)))
    (defun bf_wrong_type_consp (offender)
      (let* ((wbuf (alloc-bytes 24 1))
             (cbuf (alloc-bytes 8 1))
             (expected (alloc-bytes 32 8))
             (nil-slot (alloc-bytes 32 8))
             (data-tail (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 wbuf 0 8751669898145395319) ; "wrong-ty"
         (ptr-write-u64 (+ wbuf 8) 0 7887324063363589488) ; "pe-argum"
         (ptr-write-u64 (+ wbuf 16) 0 7630437) ; "ent"
         (nl_alloc_symbol wbuf 19 268435480)
         (ptr-write-u64 cbuf 0 482972954467) ; "consp"
         (nl_alloc_symbol cbuf 5 expected)
         (wf_write_nil nil-slot)
         (nelisp_cons_construct offender nil-slot data-tail)
         (nelisp_cons_construct expected data-tail 268435512)
         (ptr-write-u64 268435472 0 1)
         (atomic-fetch-add 268435544 1)
         1)))
    ;; fboundp/boundp: look up in the env mirror.  env+0 = mirror, env+64 = unbound.
    ;; nelisp_env_lookup_function(mirror, unbound, sym, out_slot) returns 0 if found.
    (defun bf_fboundp (args env out)
      (let* ((sym (wf_arg_ptr args 0)) (tmp (alloc-bytes 32 8)) (mirror (+ env 0)) (unbound (+ env 64)))
        (if (= (nelisp_env_lookup_function mirror unbound sym tmp) 0)
            (wf_write_t out) (wf_write_nil out))))
    (defun bf_boundp (args env out)
      (let* ((sym (wf_arg_ptr args 0)) (mirror (+ env 0)))
        (if (= (nelisp_mirror_is_bound mirror sym) 1)
            (wf_write_t out) (wf_write_nil out))))
    (defun bf_set (args env out)
      (let* ((sym (wf_arg_ptr args 0))
             (val (wf_arg_ptr args 1)))
        (seq
         (nl_env_set_value env sym val)
         (wf_copy32 out val)
         0)))
    ;; Doc 22 A8: `symbol-value' must see a dynamic `let' binding, like a direct
    ;; variable reference does.  The prelude fallback read only the global mirror
    ;; (nelisp--env-globals-get-value), so (let ((x 9)) (symbol-value 'x)) saw
    ;; the global, not 9.  Route through `nelisp_env_lookup_value' (frame stack
    ;; first, then mirror) with the same env layout the eval machinery uses:
    ;; mirror @ env+0, frame-stack @ env+32.  rc is the void-variable sentinel.
    (defun bf_symbol_value (args env out)
      (nelisp_env_lookup_value (+ env 0) (+ env 32) (wf_arg_ptr args 0) out))
    ;; load FILE &optional ... -> t.  Minimal standalone-reader command surface:
    ;; read FILE into a Sexp::Str, parse each top-level form with the same pure
    ;; reader, and evaluate it in the caller's ENV.  This intentionally mirrors
    ;; the driver's multi-form loop instead of relying on host-side embedding.
    ;; Doc 152 §11.41 Stage 4b: publish the executing top-level form's precise
    ;; root-set to nl_safepoint_ctx for the duration of its eval, so a mid-form
    ;; safepoint (in nl_sf_while) can invoke a sound collect.  push BEFORE eval /
    ;; pop AFTER (the boundary collect stays the direct nl_gc_collect and reads
    ;; the driver locals, not the ctx, so popping here is fine).  Returns rc.
    (defun nl_driver_eval_published (result env out pool src cursor bsym)
      (seq (nl_gc_ctx_push env result out pool src cursor bsym)
           (let* ((rc (nelisp_eval_call result env out)))
             (seq (nl_gc_ctx_pop) rc))))
    (defun bf_load_eval_loop (src cursor result pool env out bsym more)
      (while (= more 1)
        (seq
         (ptr-write-u64 result 0 0) (ptr-write-u64 result 8 0)
         (let* ((prc (nelisp_reader_parse_one src cursor result pool 0)))
           (if (= prc 1)
               (seq
                (ptr-write-u64 out 0 0) (ptr-write-u64 out 8 0)
                (let* ((rc (nl_driver_eval_published result env out pool src cursor bsym)))
                  ;; GC trigger must compare TOTAL allocated bytes across all
                  ;; chunks (268436184 = chunk-bytes-reserved running counter),
                  ;; not the chunk-0 bump offset (268435456) — after Doc 140's
                  ;; chunk-growth refactor the chunk-0 bump caps at the 256 MiB
                  ;; first chunk and never reaches the 512 MiB trigger, so the
                  ;; tracing GC never fired and the arena grew unbounded.
                  (if (< (ptr-read-u64 268436184 0)
                         (ptr-read-u64 268435560 0))
                      0
                    (let* ((live (nl_gc_collect_form_boundary env result out pool src cursor bsym))
                           (bump (ptr-read-u64 268436184 0))
                           (lo (+ (* live 3) 1048576))
                           (hi (+ bump 16777216)))
                      (ptr-write-u64 268435560 0
                                     (if (< lo hi) hi lo))))))
             (setq more 0)))))
      more)
    (defun bf_eval_source_string_loop (src cursor result pool env out bsym more)
      (while (= more 1)
        (seq
         (ptr-write-u64 result 0 0) (ptr-write-u64 result 8 0)
         (let* ((prc (nelisp_reader_parse_one src cursor result pool 0)))
           (if (= prc 1)
               (seq
                (ptr-write-u64 out 0 0) (ptr-write-u64 out 8 0)
                (let* ((rc (nl_driver_eval_published result env out pool src cursor bsym)))
                  (if (= rc 0)
                      ;; GC trigger on TOTAL chunk-bytes-reserved (268436184),
                      ;; not the chunk-0 bump offset (268435456).  See the note
                      ;; in `bf_load_eval_loop'.
                      (if (< (ptr-read-u64 268436184 0)
                             (ptr-read-u64 268435560 0))
                          0
                        (let* ((live (nl_gc_collect_form_boundary env result out pool src cursor bsym))
                               (bump (ptr-read-u64 268436184 0))
                               (lo (+ (* live 3) 1048576))
                               (hi (+ bump 16777216)))
                          (ptr-write-u64 268435560 0
                                         (if (< lo hi) hi lo))))
                    (setq more 2))))
             (setq more 0)))))
      more)
    (defun bf_load (args env out)
      ;; Doc 147 Phase 1.5 Group P — the reader parse pool is now a RAW
      ;; 32B-slot buffer (cap*32 bytes) instead of a GC-managed
      ;; Sexp::Vector.  `pool' IS the buffer base; slot N lives at
      ;; pool+N*32 (see `nelisp_reader_p_slot').  Store the cap at the
      ;; control-region word @268436448 so the GC pool arms can walk it.
      (let* ((src (alloc-bytes 32 8))
             (cursor (alloc-bytes 32 8))
             (result (alloc-bytes 32 8))
             (bsym (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 bsym 0 0) (ptr-write-u64 bsym 8 0)
         (ptr-write-u64 268435624 0 0)
         (nl_bi_read_file args src)
         (ptr-write-u64 cursor 0 2) (ptr-write-u64 cursor 8 0)
         ;; Size the parse pool to the source AFTER reading it: a single huge
         ;; form (e.g. emoji-labels.el's `#s(hash-table ...)' with thousands of
         ;; entries) overflows a fixed 32768-slot pool -> reader SIGSEGV.  4x
         ;; source bounds node count; floor 32768, cap 4194304 (the cap avoids
         ;; over-allocating ~19M slots on the 4.8MB ja-dic.el).
         ;; NESTED-LOAD cap restore (2026-06-11): the cap word @268436448
         ;; must match the pool the CURRENT loop passes to the GC arms.  A
         ;; nested big (load ...) used to leave ITS larger cap behind, so
         ;; the outer loop's GC walked far past its own (smaller) pool ->
         ;; silent abort / SIGSEGV after returning from the inner load
         ;; (found loading dash.el from the wrapped user init, M18).
         (let* ((cap (let ((n (* 4 (str-len src))))
                       (if (< n 32768) 32768 (if (> n 4194304) 4194304 n))))
                (pool (alloc-bytes (* cap 32) 8))
                (prevcap (ptr-read-u64 268436448 0)))
           (seq
            (ptr-write-u64 268436448 0 cap)
            (if (= (bf_load_eval_loop src cursor result pool env out bsym 1) 2)
                (seq
                 (ptr-write-u64 268436448 0 prevcap)
                 1)
              (seq
               (ptr-write-u64 268435472 0 0)
               (wf_dirty)
               (wf_write_t out)
               (ptr-write-u64 268436448 0 prevcap)
               0)))))))
    (defun bf_eval_source_string (args env out)
      ;; Doc 147 Phase 1.5 Group P — RAW parse-pool buffer (cap*32 bytes),
      ;; `pool' IS the base; slot N @ pool+N*32 (`nelisp_reader_p_slot').
      ;; `src' is bound before `pool', so the cap can size the alloc
      ;; directly; store the same cap @268436448 for the GC pool arms.
      (let* ((src (wf_arg_ptr args 0))
             (cursor (alloc-bytes 32 8))
             (result (alloc-bytes 32 8))
             ;; Doc 08 §8.15: right-size the per-form parse pool to the source
             ;; length instead of a fixed 32768-slot (~1MB) buffer.  The parsed
             ;; form's nodes live IN this pool (a GC root), so a retained
             ;; defun/defvar pins the whole pool -> a 1MB pool per small form was
             ;; 54-68% of the vendor-load arena.  4x source length bounds the
             ;; node count (each parse node needs >=1 source char) and the 32768
             ;; cap keeps the previous behaviour for big forms (no regression).
             (pool (alloc-bytes (* (let ((n (* 4 (str-len src))))
                                     (if (< n 256) 256 (if (> n 32768) 32768 n)))
                                   32)
                                8))
             (bsym (alloc-bytes 32 8))
             ;; same nested cap-restore contract as bf_load (2026-06-11)
             (prevcap (ptr-read-u64 268436448 0)))
        (seq
         (ptr-write-u64 bsym 0 0) (ptr-write-u64 bsym 8 0)
         (ptr-write-u64 268435624 0 0)
         (ptr-write-u64 cursor 0 2) (ptr-write-u64 cursor 8 0)
         (ptr-write-u64 268436448 0 (let ((n (* 4 (str-len src))))
                                      (if (< n 256) 256 (if (> n 32768) 32768 n))))
         (if (= (bf_eval_source_string_loop src cursor result pool env out bsym 1) 2)
             (seq (ptr-write-u64 268436448 0 prevcap) 1)
           (seq (wf_dirty) (ptr-write-u64 268436448 0 prevcap) 0)))))
    (defun bf_read_all_from_string_native (args out)
      ;; Return all top-level forms from SRC as a proper list using the same
      ;; native reader parser that drives standalone load/eval.  This gives
      ;; interpreted artifact builders a bulk reader without the per-form
      ;; `read-from-string' / `nelisp--rd-one' compatibility path.
      (let* ((src (wf_arg_ptr args 0))
             (cursor (alloc-bytes 32 8))
             (result (alloc-bytes 32 8))
             (head (alloc-bytes 32 8))
             (tail (alloc-bytes 32 8))
             (node (alloc-bytes 32 8))
             (nil-slot (alloc-bytes 32 8))
             (cap (let ((n (* 4 (str-len src))))
                    (if (< n 256) 256 (if (> n 4194304) 4194304 n))))
             (pool (alloc-bytes (* cap 32) 8))
             (prevcap (ptr-read-u64 268436448 0))
             (more 1)
             (have 0))
        (seq
         (ptr-write-u64 268436448 0 cap)
         (ptr-write-u64 cursor 0 2) (ptr-write-u64 cursor 8 0)
         (ptr-write-u64 head 0 0) (ptr-write-u64 head 8 0)
         (ptr-write-u64 tail 0 0) (ptr-write-u64 tail 8 0)
         (ptr-write-u64 nil-slot 0 0) (ptr-write-u64 nil-slot 8 0)
         (while (= more 1)
           (seq
            (ptr-write-u64 result 0 0) (ptr-write-u64 result 8 0)
            (let* ((prc (nelisp_reader_parse_one src cursor result pool 0)))
              (if (= prc 1)
                  (seq
                   (cons-make-with-clone result nil-slot node)
                   (if (= have 0)
                       (seq
                        (wf_copy32 head node)
                        (wf_copy32 tail node)
                        (setq have 1))
                     (seq
                      (cons-set-cdr tail node)
                      (wf_copy32 tail node)))
                   0)
                (setq more 0)))))
         (wf_copy32 out head)
         (ptr-write-u64 268436448 0 prevcap)
         0)))
    ;; length that also handles vectors (tag 8) -> vector-len; else m5_length.
    (defun bf_length (p)
      (if (= (ptr-read-u64 p 0) 8) (vector-len p) (m5_length p)))
    ;; CORRECT eq: tag-aware identity.  The stock applyfn `eq' arm compared
    ;; value@8 which is garbage for Symbol/Str (their identity is the name).
    ;;   Int(2)   -> value@8 equal
    ;;   Symbol(4)-> symbol-eq (name compare; no interning so name == identity)
    ;;   Nil(0)/T(1) -> same tag is enough
    ;;   else (Cons/Vector/Str/...) -> pointer identity (same box@8)
    (defun bf_eq2 (a b)
      (let* ((ta (ptr-read-u64 a 0)) (tb (ptr-read-u64 b 0)))
        (if (= ta tb)
            (if (= ta 2) (if (= (ptr-read-u64 a 8) (ptr-read-u64 b 8)) 1 0)
              (if (= ta 4) (symbol-eq a b)
                (if (= ta 0) 1
                  (if (= ta 1) 1
                    ;; Doc 22 A20: Str(5)/MutStr(6) have NO stable pointer
                    ;; identity -- tag 5 is deep-copied on clone (new buffer)
                    ;; and offset 8 is the String length/capacity field, so the
                    ;; stock pointer-identity arm made every equal-length pair
                    ;; eq.  In this value-semantics reader a string IS its value,
                    ;; so compare by content (length + bytes), consistent with
                    ;; the Symbol(4) arm comparing by name.
                    (if (= ta 5) (m5_streq a b)
                      (if (= ta 6) (m5_streq a b)
                        (if (= (ptr-read-u64 a 8) (ptr-read-u64 b 8)) 1 0)))))))
          0)))
    (defun bf_eq (args out)
      (if (= (bf_eq2 (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1)
          (wf_write_t out) (wf_write_nil out)))
    ;; equal: like eq but Str(5/6) compared by bytes, and one-level structural
    ;; for cons (recurses car+cdr).  Enough for member/assoc on flat data.
    (defun bf_equal2 (a b)
      (let* ((ta (ptr-read-u64 a 0)) (tb (ptr-read-u64 b 0)))
        (if (= ta tb)
            (if (= ta 5) (m5_streq a b)
              (if (= ta 6) (m5_streq a b)
                (if (= ta 7)
                    (if (= (bf_equal2 (nl_cons_car_ptr a) (nl_cons_car_ptr b)) 1)
                        (bf_equal2 (nl_cons_cdr_ptr a) (nl_cons_cdr_ptr b)) 0)
                  (bf_eq2 a b))))
          0)))
    (defun bf_equal (args out)
      (if (= (bf_equal2 (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1)
          (wf_write_t out) (wf_write_nil out)))
    ;; nil-safe car/cdr.  The stock arms call nl_cons_car_ptr/cdr_ptr which
    ;; return 0 for a non-cons, then wf_copy32 derefs address 0 -> SIGSEGV.
    ;; Real elisp: (car nil)=(cdr nil)=nil.  Guard on tag==7.
    (defun bf_car (args out)
      (let* ((a (wf_arg_ptr args 0)))
        (if (= (ptr-read-u64 a 0) 7) (wf_copy32 out (nl_cons_car_ptr a)) (wf_write_nil out))))
    (defun bf_cdr (args out)
      (let* ((a (wf_arg_ptr args 0)))
        (if (= (ptr-read-u64 a 0) 7) (wf_copy32 out (nl_cons_cdr_ptr a)) (wf_write_nil out))))
    ;; --- Wave-2 (C) bitwise + shift + string-lessp ---
    ;; ash N C: arithmetic shift.  C>=0 -> logical-left (shl); C<0 -> signed
    ;; arithmetic-right (sar) by -C.  (shl/sar are the Doc 100 §100.D shift OPs;
    ;; logand/logior/logxor are binary arith OPs in the applyfn grammar.)
    (defun bf_ash (n c)
      (if (< c 0) (sar n (- 0 c)) (shl n c)))
    ;; string<: byte-lexicographic compare of two Str/MutStr.  Returns 1 if A
    ;; sorts strictly before B, else 0.  Walk min(lenA,lenB) bytes; first
    ;; differing byte decides; on common-prefix tie the shorter string is "less".
    (defun bf_str_lt_bytes (a b i la lb)
      (if (>= i la)
          (if (< la lb) 1 0)               ; A exhausted: less iff strictly shorter
        (if (>= i lb) 0                    ; B exhausted (A not): A is NOT less
          (let* ((ca (str-byte-at a i)) (cb (str-byte-at b i)))
            (if (< ca cb) 1
              (if (> ca cb) 0
                (bf_str_lt_bytes a b (+ i 1) la lb)))))))
    (defun bf_str_lt (a b)
      (bf_str_lt_bytes a b 0 (str-len a) (str-len b)))
    ;; n-ary bitwise folds (elisp logior/logand/logxor are &rest variadic;
    ;; the 2-arg primitives dropped args 3+ and SIGSEGV'd on <2 args, which
    ;; broke every (logior (ash..) (ash..) (logand..)) ModRM/REX byte = all
    ;; register encoding = all codegen).  Mirror wf_sum: fold over the args
    ;; cons-list (tag 7 = more), seeded with the elisp identity (ior/xor=0,
    ;; and=-1) so (logior)=0 (logand)=-1 (logior X)=X all match host.
    (defun wf_logior_fold (lp acc)
      (if (= (ptr-read-u64 lp 0) 7)
          (wf_logior_fold (nl_cons_cdr_ptr lp) (logior acc (ptr-read-u64 (nl_cons_car_ptr lp) 8))) acc))
    (defun wf_logand_fold (lp acc)
      (if (= (ptr-read-u64 lp 0) 7)
          (wf_logand_fold (nl_cons_cdr_ptr lp) (logand acc (ptr-read-u64 (nl_cons_car_ptr lp) 8))) acc))
    (defun wf_logxor_fold (lp acc)
      (if (= (ptr-read-u64 lp 0) 7)
          (wf_logxor_fold (nl_cons_cdr_ptr lp) (logxor acc (ptr-read-u64 (nl_cons_car_ptr lp) 8))) acc))
    (defun nl_copy_words (dst src n)
      (if (= n 0) 0
        (seq (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
             (nl_copy_words (+ dst 8) (+ src 8) (- n 1)))))
    ;; Fresh per-thread env: copy mirror/frames/unbound (96 bytes, shared boxes,
    ;; read-only during a compile), fresh rec_cur@+96=0, copy rec_max@+104.
    (defun nl_make_thread_env (penv)
      (let ((e (alloc-bytes 128 8)))
        (seq (nl_copy_words e penv 12)
             (ptr-write-u64 (+ e 96) 0 0)
             (ptr-write-u64 (+ e 104) 0 (ptr-read-u64 (+ penv 104) 0))
             e)))
    ;; Child entry (clean frame on the clone stack): read FORM + the ALREADY-
    ;; BUILT fresh env from the heap box (box[8] is built by the PARENT before
    ;; the clone, so the child never copies the parent's live ctx — which the
    ;; parent mutates concurrently as it runs `thread-join').  Eval, exit.
    (defun nl_thread_run (box)
      ;; ACK (clear the handshake slot) so the parent may spawn the next thread;
      ;; box is already captured by value, so this is safe.
      (seq (ptr-write-u64 268435688 0 0)
           (let ((out_slot (alloc-bytes 32 8)))
             (seq (extern-call nelisp_eval_call (ptr-read-u64 box 0) (ptr-read-u64 box 8) out_slot)
                  (syscall-direct 60 0 0 0 0 0 0)))))
    ;; spin (COMPILED, iterative) until *ADDR == 0 — the spawn handshake wait.
    (defun nl_spin_zero (addr)
      (while (> (ptr-read-u64 addr 0) 0) 0))
    (defun bf_thread_spawn (args env out)
      (let ((box (alloc-bytes 32 8)))
        (seq (ptr-write-u64 box 0 (wf_arg_ptr args 0))
             ;; build the per-thread env NOW (parent, ctx consistent) — not in
             ;; the child concurrently with the parent's own ctx mutation.
             (ptr-write-u64 box 8 (nl_make_thread_env env))
             (ptr-write-u64 268435688 0 box)
             (let ((stack (syscall-direct 9 0 67108864 3 34 -1 0)))
               (if (= (syscall-direct 56 768 (+ stack 67108864) 0 0 0 0) 0)
                   (nl_thread_run (ptr-read-u64 268435688 0))
                 ;; parent: wait until the child has captured+acked its box, so
                 ;; the NEXT spawn does not clobber the shared handshake slot.
                 (seq (nl_spin_zero 268435688) (wf_write_int out 1)))))))
    ;; thread-join: spin until *ADDR >= TARGET.  Uses the COMPILED (aot,
    ;; iterative) `while' — NOT the interpreter's recursive `while' that
    ;; overflows the native stack on a long spin — so the parent joins N
    ;; concurrent threads in O(1) stack.
    (defun nl_spin_until (addr target)
      (while (< (ptr-read-u64 addr 0) target) 0))
    (defun bf_thread_join (args env out)
      (seq (nl_spin_until (wf_argval args 0) (wf_argval args 1))
           (wf_write_int out 0)))
    ;; fork-spawn: fork(2) (syscall 57) a child with a COW-isolated copy of the
    ;; WHOLE address space (own arena / mirror / frames / env), so N children
    ;; compiling CONCURRENTLY share NO mutable eval state and cannot race (the
    ;; clone(2)/CLONE_VM thread path shares all of it).  Child evals FORM (its
    ;; own COW copy — no box/handshake needed) and exits; join via a MAP_SHARED
    ;; counter (atomic-fetch-add stays visible across the COW boundary) +
    ;; thread-join.  fork result branched directly (no spill).
    (defun bf_fork_spawn (args env out)
      (let ((form (wf_arg_ptr args 0)))
        (if (= (syscall-direct 57 0 0 0 0 0 0) 0)
            (let ((out_slot (alloc-bytes 32 8)))
              (seq (extern-call nelisp_eval_call form env out_slot)
                   (syscall-direct 60 0 0 0 0 0 0)))
          (wf_write_int out 1)))))
  "B-foundation breadth helpers (Wave-1 (B), reader-only): predicates, vector /
symbol ops, setcar/setcdr, structural equal, vector-aware length, plus the
nil-safe car/cdr + tag-aware eq REPLACEMENTS for the buggy stock arms.
Wave-2 (C) appends bf_ash (shl/sar compose) + bf_str_lt (byte-lexicographic).")

;; B-foundation breadth dispatch arms (Wave-1 (B)).  APPENDED to the reader
;; dispatch table (see `nelisp-standalone--applyfn-reader-table').  tag values:
;;   0 Nil  1 T  2 Int  3 Float  4 Symbol  5 Str  6 MutStr  7 Cons  8 Vector.
;; All are (:lit ...) because every name is matched by the full-length
;; `sexp-name-eq' op (some are <=8 bytes but :lit is always correct).
(defconst nelisp-standalone--applyfn-bf-arms
  '(;; --- predicates ---
    ((:lit "consp")    . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 7) (wf_write_t out) (wf_write_nil out)))
    ((:lit "atom")     . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 7) (wf_write_nil out) (wf_write_t out)))
    ((:lit "stringp")  . (let* ((tg (ptr-read-u64 (wf_arg_ptr args 0) 0)))
                           (if (= tg 5) (wf_write_t out) (if (= tg 6) (wf_write_t out) (wf_write_nil out)))))
    ((:lit "symbolp")  . (let* ((tg (ptr-read-u64 (wf_arg_ptr args 0) 0)))
                           ;; nil and t are also symbols in elisp
                           (if (= tg 4) (wf_write_t out) (if (= tg 0) (wf_write_t out) (if (= tg 1) (wf_write_t out) (wf_write_nil out))))))
    ((:lit "integerp") . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 2) (wf_write_t out) (wf_write_nil out)))
    ((:lit "natnump")  . (let* ((p (wf_arg_ptr args 0)))
                           (if (= (ptr-read-u64 p 0) 2)
                               (if (>= (ptr-read-u64 p 8) 0) (wf_write_t out) (wf_write_nil out))
                             (wf_write_nil out))))
    ((:lit "numberp")  . (let* ((tg (ptr-read-u64 (wf_arg_ptr args 0) 0)))
                           (if (= tg 2) (wf_write_t out) (if (= tg 3) (wf_write_t out) (wf_write_nil out)))))
    ((:lit "floatp")   . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 3) (wf_write_t out) (wf_write_nil out)))
    ((:lit "vectorp")  . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 8) (wf_write_t out) (wf_write_nil out)))
    ((:lit "listp")    . (let* ((tg (ptr-read-u64 (wf_arg_ptr args 0) 0)))
                           (if (= tg 7) (wf_write_t out) (if (= tg 0) (wf_write_t out) (wf_write_nil out)))))
    ((:lit "zerop")    . (let* ((p (wf_arg_ptr args 0)))
                           (if (= (ptr-read-u64 p 0) 2)
                               (if (= (ptr-read-u64 p 8) 0) (wf_write_t out) (wf_write_nil out))
                             (wf_write_nil out))))
    ((:lit "set")      . (bf_set args env out))
    ((:lit "symbol-value") . (bf_symbol_value args env out))
    ((:lit "fboundp")  . (bf_fboundp args env out))
    ((:lit "boundp")   . (bf_boundp args env out))
    ((:lit "featurep") . (wf_write_nil out))
    ((:lit "provide")  . (seq (wf_copy32 out (wf_arg_ptr args 0)) 0))
    ((:lit "require")  . (seq (wf_copy32 out (wf_arg_ptr args 0)) 0))
    ;; --- symbol ops ---
    ;; symbol-name: a Symbol (tag 4) already has the str layout (ptr@16/len@24);
    ;; produce a Str (tag 5) sharing those bytes via nl_alloc_str.
    ((:lit "symbol-name") . (let* ((s (wf_arg_ptr args 0)))
                              (seq (nl_alloc_str (ptr-read-u64 s 16) (ptr-read-u64 s 24) out) 0)))
    ;; intern / make-symbol: take a Str (tag 5/6), build a Symbol (tag 4).
    ((:lit "intern")      . (seq (bf_intern (wf_arg_ptr args 0) out) 0))
    ((:lit "make-symbol") . (seq (bf_make_symbol (wf_arg_ptr args 0) out) 0))
    ((:lit "unibyte-string") . (bf_unibyte_string args out))
    ;; --- vector ops ---
    ((:lit "make-vector") . (bf_make_vector args out))
    ((:lit "vector")      . (bf_vector args out))
    ((:lit "record")      . (bf_record args out))
    ((:lit "make-record") . (bf_make_record args out))
    ((:lit "recordp")     . (bf_recordp args out))
    ((:lit "aref")        . (bf_aref args out))
    ((:lit "elt")         . (bf_elt args out))
    ((:lit "aset")        . (bf_aset args out))
    ;; --- signal / error (non-crashing stub; condition-case trapping deferred) ---
    ((:lit "signal")      . (bf_signal args out))
    ((:lit "error")       . (bf_error args out))
    ;; equal (member/assoc use it).  eq is REPLACED (not appended) in the table.
    ((:lit "equal")       . (bf_equal args out))
    ;; setcar/setcdr: in-place cons mutation (plist-put / nconc need these).
    ;; Installs V (possibly a box) into a PRE-EXISTING cons -> persistent escape
    ;; -> bump the mutation epoch (NO-ESCAPE gate in `nelisp_eval_call').
    ((:lit "setcar")      . (let* ((c (wf_arg_ptr args 0)) (v (wf_arg_ptr args 1)))
                              (if (= (ptr-read-u64 c 0) 7)
                                  (seq (wf_dirty) (cons-set-car c v) (wf_copy32 out v) 0)
                                (bf_wrong_type_consp c))))
    ((:lit "setcdr")      . (let* ((c (wf_arg_ptr args 0)) (v (wf_arg_ptr args 1)))
                              (if (= (ptr-read-u64 c 0) 7)
                                  (seq (wf_dirty) (cons-set-cdr c v) (wf_copy32 out v) 0)
                                (bf_wrong_type_consp c))))
    ;; load FILE &optional ...: reader-only absolute-path file load.  The
    ;; optional args are ignored for now; callers in nelisp-emacs pass absolute
    ;; paths and use a post-load proof form to verify the file actually ran.
    ((:lit "load")        . (bf_load args env out))
    ((:lit "nelisp--eval-source-string") . (bf_eval_source_string args env out))
    ((:lit "nelisp--syscall-read-file") . (seq (nl_bi_read_file args out) 0))
    ((:lit "nl-write-file") . (nl_bi_write_file_t args out))
    ((:lit "nelisp--syscall-path") . (nl_bi_syscall_path args out))
    ((:lit "nelisp--syscall-path2") . (nl_bi_syscall_path2 args out))
    ((:lit "nelisp--syscall-path-int") . (nl_bi_syscall_path_int args out))
    ((:lit "nelisp--syscall-stat-field") . (nl_bi_syscall_stat_field args out))
    ((:lit "nelisp--syscall-stat-buf") . (nl_bi_syscall_stat_buf args out))
    ((:lit "nelisp--syscall-lstat-buf") . (nl_bi_syscall_lstat_buf args out))
    ((:lit "nelisp--syscall-readlink") . (nl_bi_syscall_readlink args out))
    ((:lit "nelisp--syscall-readdir-names") . (nl_bi_syscall_readdir_names args out))
    ((:lit "nelisp--syscall-utimes") . (nl_bi_syscall_utimes args out))
    ((:lit "nelisp--syscall-statx-buf") . (nl_bi_syscall_statx_buf args out))
    ((:lit "nelisp--syscall-unshare") . (nl_bi_syscall_unshare args out))
    ((:lit "nelisp--syscall-mount") . (nl_bi_syscall_mount args out))
    ((:lit "nelisp--syscall-pivot-root") . (nl_bi_syscall_pivot_root args out))
    ((:lit "nelisp--write-stdout-bytes") . (nl_bi_write_stdout_bytes args out))
    ((:lit "nelisp--write-stderr-line") . (nl_bi_write_stderr_line args out))
    ((:lit "nelisp--read-all-from-string-native") . (bf_read_all_from_string_native args out))
    ((:lit "read-stdin-bytes") . (nl_bi_read_stdin_bytes args out))
    ;; nl-current-unix-time: epoch seconds via the linux-x86_64 time(2)
    ;; syscall (__NR_time=201, NULL arg).  Lets interpreted code (e.g.
    ;; nelisp-emacs `float-time'/`current-time' polyfills) read wall-clock
    ;; time without process-exiting on an undispatched builtin.
    ((:lit "nl-current-unix-time") . (wf_write_int out (syscall-direct 201 0 0 0 0 0 0)))
    ;; float-time: gettimeofday + f64 done entirely in the hand-asm
    ;; nl_os_float_time extern (so the AOT never emits a syscall<->f64 sequence,
    ;; which aborts).  Dispatched here in bf-arms (where :lit works) via the
    ;; wf_float_time helper (an inline extern call from a dispatch arm aborts).
    ((:lit "float-time") . (wf_float_time out))
    ;; nl-unix-time-usec: microseconds since the epoch as a single INTEGER via
    ;; gettimeofday(2) (__NR_gettimeofday=96): sec*1e6 + usec.  Pure integer math
    ;; after the syscall (no f64), which is the half that works -- f64 production
    ;; (e.g. `(/ (nl-unix-time-usec) 1000000.0)') is then done in a SEPARATE
    ;; builtin dispatch across an eval-loop boundary, sidestepping the
    ;; syscall<->f64 codegen abort.  Lets `float-time' be a pure-elisp polyfill.
    ((:lit "nl-unix-time-usec") . (let* ((buf (alloc-bytes 16 8)))
                                    (seq (syscall-direct 96 buf 0 0 0 0 0)
                                         (wf_write_int out
                                           (+ (* (ptr-read-u64 buf 0) 1000000)
                                              (ptr-read-u64 buf 8))))))
    ;; --- Wave-2 (C) bitwise / shift (2-arg forms; n-ary folds in prelude) ---
    ((:lit "ash")     . (wf_write_int out (bf_ash (wf_argval args 0) (wf_argval args 1))))
    ((:lit "logand")  . (wf_write_int out (wf_logand_fold args (- 0 1))))
    ((:lit "logior")  . (wf_write_int out (wf_logior_fold args 0)))
    ((:lit "logxor")  . (wf_write_int out (wf_logxor_fold args 0)))
    ;; lognot X = -X-1 (two's complement bitwise NOT).
    ((:lit "lognot")  . (wf_write_int out (- (- 0 (wf_argval args 0)) 1)))
    ;; --- Wave-2 (C) string<: byte-lexicographic less-than ---
    ((:lit "string<") . (if (= (bf_str_lt (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1)
                            (wf_write_t out) (wf_write_nil out)))
    ;; --- §S4 low-level thread / atomic / memory primitives (interpreter
    ;; dispatch).  These let INTERPRETED driver code spawn clone(2) threads,
    ;; do SeqCst atomics, raw memory access, and arena allocation — the
    ;; building blocks of an in-interpreter parallel unit compile.  Args and
    ;; results are raw i64 (addresses / syscall numbers / counts), carried as
    ;; Int Sexps via wf_argval / wf_write_int.  The underlying ops are the
    ;; same AOT grammar ops the compiler emits, so no new Rust.
    ((:lit "syscall-direct") . (wf_write_int out (syscall-direct (wf_argval args 0) (wf_argval args 1) (wf_argval args 2) (wf_argval args 3) (wf_argval args 4) (wf_argval args 5) (wf_argval args 6))))
    ((:lit "atomic-fetch-add") . (wf_write_int out (atomic-fetch-add (wf_argval args 0) (wf_argval args 1))))
    ((:lit "ptr-read-u8") . (wf_write_int out (ptr-read-u8 (wf_argval args 0) (wf_argval args 1))))
    ((:lit "ptr-write-u8") . (seq (ptr-write-u8 (wf_argval args 0) (wf_argval args 1) (wf_argval args 2)) (wf_write_int out 0)))
    ((:lit "ptr-read-u32") . (wf_write_int out (ptr-read-u32 (wf_argval args 0) (wf_argval args 1))))
    ((:lit "ptr-write-u32") . (seq (ptr-write-u32 (wf_argval args 0) (wf_argval args 1) (wf_argval args 2)) (wf_write_int out 0)))
    ((:lit "ptr-read-u64") . (wf_write_int out (ptr-read-u64 (wf_argval args 0) (wf_argval args 1))))
    ((:lit "ptr-write-u64") . (seq (ptr-write-u64 (wf_argval args 0) (wf_argval args 1) (wf_argval args 2)) (wf_write_int out 0)))
    ((:lit "alloc-bytes") . (wf_write_int out (alloc-bytes (wf_argval args 0) (wf_argval args 1))))
    ((:lit "garbage-collect") . (seq (nl_gc_collect_published 0)
                                     (bf_arena_stats out)))
    ((:lit "nelisp-process-call-process") . (nl_bi_process_call_process args out))
    ((:lit "nelisp-process-start") . (nl_bi_process_start_process args out))
    ((:lit "nelisp-process-start-process") . (nl_bi_process_start_process args out))
    ((:lit "nelisp-process-object-p") . (if (= (nl_bi_process_object_p_raw (wf_arg_ptr args 0)) 1) (wf_write_t out) (wf_write_nil out)))
    ((:lit "nelisp-process-async-ready-p") . (wf_write_t out))
    ((:lit "nelisp-process-pid") . (wf_write_int out (nl_bi_process_get_int (wf_arg_ptr args 0) 1)))
    ((:lit "nelisp-process-status") . (wf_write_int out (nl_bi_process_status_code (wf_arg_ptr args 0))))
    ((:lit "nelisp-process-exit-status") . (wf_write_int out (nl_bi_process_exit_code (wf_arg_ptr args 0))))
    ((:lit "nelisp-process-read-output") . (nl_bi_process_read_output args out))
    ((:lit "nelisp-process-wait") . (wf_write_int out (nl_bi_process_wait_object (wf_arg_ptr args 0))))
    ((:lit "nelisp-process-delete") . (seq (nl_bi_process_delete_object (wf_arg_ptr args 0)) (wf_write_nil out)))
    ((:lit "nelisp-portable-syscall") . (wf_write_int out (nl_bi_portable_syscall args)))
    ;; ptr-call: forward FFI indirect call.  (ptr-call ADDR a0 a1 a2 a3 a4 a5)
    ;; -> calls the i64 code pointer ADDR with up to 6 i64 args (SysV ABI),
    ;; returns rax as i64.  Same `call-ptr' grammar op the compiler emits, so
    ;; no new Rust.  Lets interpreted REPL code drive forward FFI (dlsym'd libc
    ;; / GTK / SDL functions) directly.  Pad unused trailing args with 0.
    ((:lit "ptr-call") . (wf_write_int out (call-ptr (wf_argval args 0) (wf_argval args 1) (wf_argval args 2) (wf_argval args 3) (wf_argval args 4) (wf_argval args 5) (wf_argval args 6))))
    ((:lit "thread-spawn") . (bf_thread_spawn args env out))
    ((:lit "thread-join") . (bf_thread_join args env out))
    ((:lit "fork-spawn") . (bf_fork_spawn args env out)))
  "B-foundation breadth dispatch arms (Wave-1 (B)): predicates, symbol / vector
ops, signal/error stubs, structural equal, setcar/setcdr.  Wave-2 (C) appends
ash/logand/logior/logxor/lognot + string<.")

(defconst nelisp-standalone--applyfn-bf-builtins
  '("consp" "atom" "stringp" "symbolp" "integerp" "natnump" "numberp" "floatp"
    "vectorp" "listp" "zerop" "set" "symbol-value" "fboundp" "boundp" "featurep" "provide" "require"
    "symbol-name" "intern" "make-symbol" "unibyte-string"
    "make-vector" "vector" "aref" "elt" "aset" "record" "recordp" "make-record"
    "signal" "error" "equal" "setcar" "setcdr" "load"
    ;; Wave-2 (C): bitwise / shift / string<
    "ash" "logand" "logior" "logxor" "lognot" "string<"
    "syscall-direct" "atomic-fetch-add" "garbage-collect"
    "ptr-read-u8" "ptr-write-u8" "ptr-read-u32" "ptr-write-u32"
    "ptr-read-u64" "ptr-write-u64" "alloc-bytes"
    "nelisp-process-call-process" "nelisp-process-start"
    "nelisp-process-start-process" "nelisp-process-object-p"
    "nelisp-process-async-ready-p" "nelisp-process-pid"
    "nelisp-process-status" "nelisp-process-exit-status"
    "nelisp-process-read-output" "nelisp-process-wait"
    "nelisp-process-delete" "nelisp-portable-syscall"
    "ptr-call" "thread-spawn" "thread-join" "fork-spawn")
  "Builtin names added by Wave-1 (B) breadth glue; appended to
`nelisp-standalone--reader-builtins'.")

(defun nelisp-standalone--applyfn-reader-table ()
  "Build the reader dispatch table: the base table with the buggy stock
`car'/`cdr'/`eq' arms REPLACED (nil-safe car/cdr + tag-aware eq) and `length'
made vector-aware, then the B-foundation breadth arms APPENDED."
  (append
   (mapcar
    (lambda (entry)
      (cond
       ((equal (car entry) '(:lit "length"))
        '((:lit "length") . (wf_write_int out (bf_length (wf_arg_ptr args 0)))))
       ((equal (car entry) '(:u8 "eq"))
        '((:u8 "eq") . (bf_eq args out)))
       ((equal (car entry) '(:u8 "car"))
        '((:u8 "car") . (bf_car args out)))
       ((equal (car entry) '(:u8 "cdr"))
        '((:u8 "cdr") . (bf_cdr args out)))
       (t entry)))
    nelisp-standalone--applyfn-dispatch-table)
   nelisp-standalone--applyfn-bf-arms))

(defun nelisp-standalone--applyfn-assemble (helper-groups table &optional default-form)
  "Assemble an applyfn `(seq ...)' unit from HELPER-GROUPS (lists of defun forms,
appended in order) and the dispatch TABLE, ending in nelisp_apply_function.
DEFAULT-FORM is the unknown-builtin fallthrough passed to
`nelisp-standalone--applyfn-build-dispatch' (pass `1' for the baked eval link
set, which lacks the reader-only `nl_os_write_stderr')."
  (let ((dispatch (nelisp-standalone--applyfn-build-dispatch table default-form)))
    (append
     '(seq)
     (apply #'append helper-groups)
     (list `(defun nelisp_apply_function (func_ptr args env out)
              (let* ((name_ptr (nl_cons_car_ptr (nl_cons_cdr_ptr func_ptr))))
                ,dispatch))))))

;; Baked-form eval applyfn: core helpers + arithmetic/list dispatch only.  Self-
;; contained against `nelisp-standalone--manifest' (no mut-str / str-direct / file
;; externs), so `make standalone-eval-test' stays green.
(defconst nelisp-standalone--applyfn-baked-source
  (nelisp-standalone--applyfn-assemble
   (list nelisp-standalone--applyfn-core-helpers)
   nelisp-standalone--applyfn-dispatch-table-baked
   ;; Silent rc-1 fallthrough: the baked eval manifest omits the reader-only
   ;; unit defining `nl_os_write_stderr', so the stderr diagnostic default
   ;; would be an unresolved symbol at link time.
   1)
  "Arithmetic/list-only applyfn for the baked-form eval build.")

;; Reader applyfn: core + HT (M4) + string/format (M5) + B-foundation breadth
;; helpers + the reader dispatch table (= the FULL table with nil-safe car/cdr +
;; tag-aware eq + vector-aware length REPLACEMENTS and the breadth arms appended;
;; incl. file-I/O wrf/rdf/slen impls in `nelisp-standalone--fileio-source').
(defconst nelisp-standalone--applyfn-source
  (nelisp-standalone--applyfn-assemble
   (list nelisp-standalone--applyfn-core-helpers
         nelisp-standalone--applyfn-census-helpers
        nelisp-standalone--applyfn-ht-helpers
        nelisp-standalone--applyfn-search-helpers
        nelisp-standalone--applyfn-m5-helpers
         nelisp-standalone--applyfn-bf-helpers)
   (nelisp-standalone--applyfn-reader-table))
  "Full reader-path applyfn: arithmetic + hash tables + strings/format + file I/O
+ B-foundation breadth (predicates / vectors / symbols / equal / setcar-setcdr,
plus the nil-safe car/cdr and tag-aware eq fixes).")

;; ===================================================================
;; M7 file-I/O glue unit — the impls for the wrf/rdf/slen builtins
;; dispatched in `nelisp-standalone--applyfn-source'.  Freestanding (no
;; libc): target OS helpers only.  Paths from the reader are NOT
;; NUL-terminated, so nl_bi_make_cpath copies the bytes into a fresh arena
;; buffer + a trailing NUL before calling the OS open path.
;; A Sexp::Str (tag 5) has ptr@16/len@24; a Sexp::MutStr (tag 6) wraps an
;; NlStr* @8 whose ptr@8/len@16.  Both are handled for robustness.
;; ===================================================================
(defconst nelisp-standalone--fileio-source
  '(seq
    (defun nl_bi_strptr (sx)
      (if (= (ptr-read-u64 sx 0) 6)
          (ptr-read-u64 (ptr-read-u64 sx 8) 8)
        (ptr-read-u64 sx 16)))
    (defun nl_bi_strlen (sx)
      (if (= (ptr-read-u64 sx 0) 6)
          (ptr-read-u64 (ptr-read-u64 sx 8) 16)
        (ptr-read-u64 sx 24)))
    (defun nl_bi_cpath_loop (src dst i n)
      (if (= i n)
          (nl_seq2 (ptr-write-u8 dst n 0) dst)
        (nl_seq2 (ptr-write-u8 dst i (ptr-read-u8 src i))
                 (nl_bi_cpath_loop src dst (+ i 1) n))))
    (defun nl_bi_make_cpath (sx)
      (nl_bi_cpath_loop (nl_bi_strptr sx) (alloc-bytes (+ (nl_bi_strlen sx) 1) 1)
                        0 (nl_bi_strlen sx)))
    (defun nl_bi_wf_withfd (fd ptr len)
      (if (< fd 0) fd
        (let* ((wr (nl_os_write_file_handle fd ptr len)))
          (nl_seq2 (nl_os_close_handle fd) wr))))
    (defun nl_bi_write_file (args out)
      (let* ((path_sx (wf_arg_ptr args 0)) (cont_sx (wf_arg_ptr args 1)))
        (let* ((cpath (nl_bi_make_cpath path_sx))
               (fd (nl_os_open_write_truncate cpath)))
          (wf_write_int out
            (nl_bi_wf_withfd fd (nl_bi_strptr cont_sx) (nl_bi_strlen cont_sx))))))
    (defun wf_copy32_strnil (out)
      (nl_alloc_str (alloc-bytes 1 1) 0 out))
    ;; Two-phase read (2026-06-10): rdf used to alloc-bytes a FIXED 8 MiB
    ;; scratch per call.  On the bump arena that 8 MiB is unreclaimable until
    ;; the enclosing top-level form returns, so a polling loop calling rdf on
    ;; tiny transport files (the nemacs session bridge) leaked ~16 MiB per
    ;; iteration -> ~60 GB over an 18h session.  Read 4 KiB first; only spill
    ;; into the legacy 8 MiB buffer when the file is larger.  The 8 MiB cap
    ;; itself is kept: leim/ja-dic/ja-dic.el is ~4.8 MiB; a 4 MiB cap
    ;; truncated it mid-form -> reader SIGSEGV.
    (defun nl_bi_rf_copy4k (src dst n)
      (let ((i 0))
        (seq
         (while (< i n)
           (seq (ptr-write-u8 dst i (ptr-read-u8 src i))
                (setq i (+ i 1))))
         0)))
    (defun nl_bi_rf_read_rest (fd head n0 out)
      (let* ((buf (alloc-bytes 8388608 1)))
        (seq
         (nl_bi_rf_copy4k head buf n0)
         (let* ((n1 (nl_os_read_file_handle fd (+ buf n0) (- 8388608 n0))))
           (nl_seq2 (nl_alloc_str buf (+ n0 (if (< n1 0) 0 n1)) out) 0)))))
    (defun nl_bi_rf_withfd (fd buf out)
      (if (< fd 0)
          (wf_copy32_strnil out)
        (let* ((n (nl_os_read_file_handle fd buf 4096)))
          (nl_seq2
           (if (< n 4096)
               (nl_seq2 (nl_alloc_str buf (if (< n 0) 0 n) out) 0)
             (nl_bi_rf_read_rest fd buf n out))
           (nl_seq2 (nl_os_close_handle fd) 0)))))
    (defun nl_bi_read_file (args out)
      (let* ((path_sx (wf_arg_ptr args 0)))
        (let* ((cpath (nl_bi_make_cpath path_sx))
               (buf (alloc-bytes 4096 1))
               (fd (nl_os_open_read cpath)))
          (nl_bi_rf_withfd fd buf out))))
    (defun nl_bi_slen (args out)
      (wf_write_int out (nl_bi_strlen (wf_arg_ptr args 0))))
    ;; str-count-nl STRING END -> number of \n bytes in STRING[0, min(END,len)).
    ;; Native byte loop so callers (e.g. the nemacs GUI bridge modeline) can
    ;; derive a cursor line number from a large buffer string without an
    ;; interpreted per-char walk (which allocates per iteration and cannot be
    ;; GC'd until the enclosing top-level form returns).
    (defun nl_bi_str_count_nl (args out)
      (let* ((sx (wf_arg_ptr args 0))
             (end (ptr-read-u64 (wf_arg_ptr args 1) 8))
             (ptr (nl_bi_strptr sx))
             (n (nl_bi_strlen sx))
             (lim (if (< end n) end n))
             (i 0)
             (acc 0))
        (seq
         (while (< i lim)
           (seq
            (if (= (ptr-read-u8 ptr i) 10) (setq acc (+ acc 1)) 0)
            (setq i (+ i 1))))
         (wf_write_int out acc))))
    ;; str-kv-line SOURCE KEY -> for the first line of SOURCE whose text
    ;; before the first TAB equals KEY, return everything after that TAB;
    ;; "" when no line matches.  Native scan for the nemacs GUI bridge
    ;; keymap tables ("KEY\tcommand[\tprompt]" lines): the interpreted
    ;; per-char walk over the ~keymap source allocated ~600MB per single
    ;; key dispatch (string deep-copies per access, no intra-form GC).
    (defun nl_bi_kv_match_at (sptr ls le kptr klen)
      (if (< (- le ls) (+ klen 1)) 0
        (if (= (ptr-read-u8 sptr (+ ls klen)) 9)
            (let* ((i 0) (ok 1))
              (seq
               (while (if (< i klen) (= ok 1) 0)
                 (seq
                  (if (= (ptr-read-u8 sptr (+ ls i)) (ptr-read-u8 kptr i))
                      0
                    (setq ok 0))
                  (setq i (+ i 1))))
               ok))
          0)))
    (defun nl_bi_str_kv_line (args out)
      (let* ((src (wf_arg_ptr args 0))
             (key (wf_arg_ptr args 1))
             (sptr (nl_bi_strptr src))
             (n (nl_bi_strlen src))
             (kptr (nl_bi_strptr key))
             (klen (nl_bi_strlen key))
             (ls 0)
             (i 0)
             (res 0))
        (seq
         (while (if (= res 0) (< i (+ n 1)) 0)
           (seq
            (if (if (= i n) 1 (if (= (ptr-read-u8 sptr i) 10) 1 0))
                (seq
                 (if (= (nl_bi_kv_match_at sptr ls i kptr klen) 1)
                     (let* ((toff (+ ls (+ klen 1)))
                            (tptr (+ sptr toff))
                            (tlen (- i toff)))
                       (seq
                        (nl_alloc_str tptr tlen out)
                        (setq res 1)))
                   0)
                 (setq ls (+ i 1)))
              0)
            (setq i (+ i 1))))
         (if (= res 0) (nl_seq2 (wf_copy32_strnil out) 0) 0))))
    ;; str-filter-prefix-lines SOURCE PREFIX -> the (non-empty) lines of
    ;; SOURCE that start with PREFIX, each newline-terminated, concatenated.
    ;; Native scan for the nemacs minibuffer completion-candidates refresh:
    ;; the interpreted per-char filter + (concat out line) accumulation
    ;; allocated ~hundreds of MB PER KEYSTROKE on multi-KB candidate tables.
    (defun nl_bi_fpl_prefix_at (sptr ls le pptr plen)
      (if (< (- le ls) plen) 0
        (let* ((i 0) (ok 1))
          (seq
           (while (if (< i plen) (= ok 1) 0)
             (seq
              (if (= (ptr-read-u8 sptr (+ ls i)) (ptr-read-u8 pptr i))
                  0
                (setq ok 0))
              (setq i (+ i 1))))
           ok))))
    (defun nl_bi_str_filter_prefix_lines (args out)
      (let* ((src (wf_arg_ptr args 0))
             (pre (wf_arg_ptr args 1))
             (sptr (nl_bi_strptr src))
             (n (nl_bi_strlen src))
             (pptr (nl_bi_strptr pre))
             (plen (nl_bi_strlen pre))
             (buf (alloc-bytes (+ n 2) 1))
             (w 0)
             (ls 0)
             (i 0))
        (seq
         (while (< i (+ n 1))
           (seq
            (if (if (= i n) 1 (if (= (ptr-read-u8 sptr i) 10) 1 0))
                (seq
                 (if (if (< ls i)
                         (nl_bi_fpl_prefix_at sptr ls i pptr plen)
                       0)
                     (let* ((j ls))
                       (seq
                        (while (< j i)
                          (seq
                           (ptr-write-u8 buf w (ptr-read-u8 sptr j))
                           (setq w (+ w 1))
                           (setq j (+ j 1))))
                        (ptr-write-u8 buf w 10)
                        (setq w (+ w 1))))
                   0)
                 (setq ls (+ i 1)))
              0)
            (setq i (+ i 1))))
         (nl_seq2 (nl_alloc_str buf w out) 0))))
    ;; str-line-start STRING POS -> offset of the first char of POS's line
    ;; (i.e. one past the previous \n), POS clamped to [0, len].  Companion to
    ;; str-count-nl: line = 1 + (str-count-nl S POS), column = POS - line-start.
    (defun nl_bi_str_line_start (args out)
      (let* ((sx (wf_arg_ptr args 0))
             (pos (ptr-read-u64 (wf_arg_ptr args 1) 8))
             (ptr (nl_bi_strptr sx))
             (n (nl_bi_strlen sx))
             (i (if (< pos n) pos n))
             (done 0))
        (seq
         (while (if (> i 0) (if (= done 0) 1 0) 0)
           (if (= (ptr-read-u8 ptr (- i 1)) 10)
               (setq done 1)
             (setq i (- i 1))))
         (wf_write_int out i))))
    (defun nl_bi_write_stdout_bytes (args out)
      (let* ((sx (wf_arg_ptr args 0)))
        (seq
         (nl_os_write_stdout (nl_bi_strptr sx) (nl_bi_strlen sx))
         (wf_write_nil out)
         0)))
    ;; read-stdin-bytes LIMIT: interpreter-side dispatch, symmetric with
    ;; nl_bi_write_stdout_bytes.  Self-contained (no Rust shim): allocate a
    ;; LIMIT-byte arena buffer, issue one read(0,...) via nl_os_read_stdin,
    ;; and wrap the bytes into a Sexp::Str via nl_alloc_str.  n<1 (EOF=0 or
    ;; err<0) returns nil so callers' `(while (setq c (read-stdin-bytes N)))'
    ;; slurp loops terminate at EOF (matches nelisp--cli-slurp-stdin).
    (defun nl_bi_read_stdin_bytes (args out)
      (let* ((limit (wf_argval args 0))
             (buf (alloc-bytes limit 1))
             (n (nl_os_read_stdin buf limit)))
        (if (< n 1)
            (nl_seq2 (wf_write_nil out) 0)
          (nl_seq2 (nl_alloc_str buf n out) 0))))
    ;; TTY raw/input support for standalone interactive -nw.
    ;; Fixed slots live after the existing reader/GC globals in the first
    ;; low-memory page.  Slot values are pointers/integers, not Sexp cells.
    (defun nl_tty_slot_saved_flag () 268436472)
    (defun nl_tty_slot_fd () 268436480)
    (defun nl_tty_slot_saved_termios () 268436488)
    (defun nl_tty_slot_scratch_termios () 268436496)
    (defun nl_tty_slot_pollfd () 268436504)
    (defun nl_tty_slot_byte () 268436512)
    (defun nl_tty_slot_dev_tty_path () 268436520)
    (defun nl_tty_ensure_buf (slot size align)
      (let* ((ptr (ptr-read-u64 slot 0)))
        (if (= ptr 0)
            (let* ((fresh (alloc-bytes size align)))
              (nl_seq2 (ptr-write-u64 slot 0 fresh) fresh))
          ptr)))
    (defun nl_tty_saved_flag () (ptr-read-u64 (nl_tty_slot_saved_flag) 0))
    (defun nl_tty_set_saved_flag (v) (ptr-write-u64 (nl_tty_slot_saved_flag) 0 v))
    (defun nl_tty_fd () (ptr-read-u64 (nl_tty_slot_fd) 0))
    (defun nl_tty_set_fd (fd) (ptr-write-u64 (nl_tty_slot_fd) 0 fd))
    (defun nl_tty_saved_termios ()
      (nl_tty_ensure_buf (nl_tty_slot_saved_termios) 60 4))
    (defun nl_tty_scratch_termios ()
      (nl_tty_ensure_buf (nl_tty_slot_scratch_termios) 60 4))
    (defun nl_tty_pollfd ()
      (nl_tty_ensure_buf (nl_tty_slot_pollfd) 8 4))
    (defun nl_tty_byte_buf ()
      (nl_tty_ensure_buf (nl_tty_slot_byte) 1 1))
    (defun nl_tty_dev_tty_path ()
      (let* ((buf (nl_tty_ensure_buf (nl_tty_slot_dev_tty_path) 9 1)))
        (seq
         (ptr-write-u64 buf 0 8751747723086357551)
         (ptr-write-u8 buf 8 0)
         buf)))
    (defun nl_tty_copy_termios (src dst)
      (seq
       (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
       (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
       (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
       (ptr-write-u64 dst 24 (ptr-read-u64 src 24))
       (ptr-write-u64 dst 32 (ptr-read-u64 src 32))
       (ptr-write-u64 dst 40 (ptr-read-u64 src 40))
       (ptr-write-u64 dst 48 (ptr-read-u64 src 48))
       (ptr-write-u32 dst 56 (ptr-read-u32 src 56))
       0))
    (defun nl_tty_make_raw_termios (buf)
      (seq
       ;; cfmakeraw subset for Linux termios:
       ;; iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|IGNCR|ICRNL|IXON)
       ;; oflag &= ~OPOST
       ;; lflag &= ~(ECHO|ECHONL|ICANON|ISIG|IEXTEN)
       ;; cflag = (cflag & ~(CSIZE|PARENB)) | CS8
       (ptr-write-u32 buf 0 (logand (ptr-read-u32 buf 0) 4294965780))
       (ptr-write-u32 buf 4 (logand (ptr-read-u32 buf 4) 4294967294))
       (ptr-write-u32 buf 8 (logior (logand (ptr-read-u32 buf 8) 4294966991) 48))
       (ptr-write-u32 buf 12 (logand (ptr-read-u32 buf 12) 4294934452))
       (ptr-write-u8 buf 22 0)
       (ptr-write-u8 buf 23 1)
       0))
    (defun nl_tty_ioctl_tcgets (fd buf)
      (syscall-direct 16 fd 21505 buf 0 0 0))
    (defun nl_tty_ioctl_tcsets (fd buf)
      (syscall-direct 16 fd 21506 buf 0 0 0))
    (defun nl_tty_close_if_private (fd)
      (if (> fd 2) (nl_os_close_handle fd) 0))
    (defun nl_bi_terminal_raw_mode_enter (out)
      (if (= (nl_tty_saved_flag) 1)
          (wf_write_t out)
        (let* ((fd (nl_os_open_read (nl_tty_dev_tty_path)))
               (scratch (nl_tty_scratch_termios))
               (saved (nl_tty_saved_termios)))
          (if (< fd 0)
              (wf_write_nil out)
            (if (< (nl_tty_ioctl_tcgets fd scratch) 0)
                (seq (nl_tty_close_if_private fd) (wf_write_nil out))
              (seq
               (nl_tty_copy_termios scratch saved)
               (nl_tty_make_raw_termios scratch)
               (if (< (nl_tty_ioctl_tcsets fd scratch) 0)
                   (seq
                    (nl_tty_set_saved_flag 0)
                    (nl_tty_close_if_private fd)
                    (wf_write_nil out))
                 (seq
                  (nl_tty_set_fd fd)
                  (nl_tty_set_saved_flag 1)
                  (wf_write_t out)))))))))
    (defun nl_bi_terminal_raw_mode_leave (out)
      (if (= (nl_tty_saved_flag) 1)
          (let* ((fd (nl_tty_fd))
                 (saved (nl_tty_saved_termios)))
            (seq
             (nl_tty_ioctl_tcsets fd saved)
             (nl_tty_set_saved_flag 0)
             (nl_tty_close_if_private fd)
             (wf_write_t out)))
        (wf_write_nil out)))
    (defun nl_tty_read_byte_to_out (fd out)
      (let* ((buf (nl_tty_byte_buf))
             (n (nl_os_read_file_handle fd buf 1)))
        (if (= n 1)
            (wf_write_int out (ptr-read-u8 buf 0))
          (wf_write_nil out))))
    (defun nl_bi_read_stdin_byte_available (args out)
      (let* ((timeout (wf_argval args 0))
             (fd (nl_tty_fd))
             (pfd (nl_tty_pollfd)))
        (seq
         (ptr-write-u32 pfd 0 fd)
         (ptr-write-u32 pfd 4 1)
         (let* ((rc (syscall-direct (nl_os_syscall_nr_poll) pfd 1 timeout 0 0 0)))
           (if (< rc 1)
               (wf_write_nil out)
             (if (= (logand (ptr-read-u8 pfd 6) 17) 0)
                 (wf_write_nil out)
               (nl_tty_read_byte_to_out fd out)))))))
    (defun nl_bi_process_drop (lst n)
      (if (= n 0)
          lst
        (if (= (ptr-read-u64 lst 0) 7)
            (nl_bi_process_drop (nl_cons_cdr_ptr lst) (- n 1))
          lst)))
    (defun nl_bi_process_list_len (lst)
      (if (= (ptr-read-u64 lst 0) 7)
          (+ 1 (nl_bi_process_list_len (nl_cons_cdr_ptr lst)))
        0))
    (defun nl_bi_process_argv_fill (argv idx lst)
      (if (= (ptr-read-u64 lst 0) 7)
          (seq
           (ptr-write-u64 argv (* idx 8)
                          (nl_bi_make_cpath (nl_cons_car_ptr lst)))
           (nl_bi_process_argv_fill argv (+ idx 1) (nl_cons_cdr_ptr lst)))
        (nl_seq2 (ptr-write-u64 argv (* idx 8) 0) argv)))
    (defun nl_bi_process_make_argv (program_sx arglst)
      (let* ((argc (+ 1 (nl_bi_process_list_len arglst)))
             (argv (alloc-bytes (* (+ argc 1) 8) 8)))
        (seq
         (ptr-write-u64 argv 0 (nl_bi_make_cpath program_sx))
         (nl_bi_process_argv_fill argv 1 arglst))))
    (defun nl_bi_process_string_sx_p (sx)
      (let* ((tag (ptr-read-u64 sx 0)))
        (if (= tag 5)
            1
          (if (= tag 6) 1 0))))
    (defun nl_bi_process_redirect_input (infile_sx)
      (if (= (nl_bi_process_string_sx_p infile_sx) 1)
          (let* ((fd (nl_os_open_read (nl_bi_make_cpath infile_sx))))
            (if (< fd 0)
                1
              (seq
               (nl_os_process_dup2 fd 0)
               (nl_os_close_handle fd)
               0)))
        0))
    (defun nl_bi_process_redirect_output (destination_sx)
      (if (= (nl_bi_process_string_sx_p destination_sx) 1)
          (let* ((fd (nl_os_open_write_truncate
                      (nl_bi_make_cpath destination_sx))))
            (if (< fd 0)
                1
              (seq
               (nl_os_process_dup2 fd 1)
               (nl_os_process_dup2 fd 2)
               (nl_os_close_handle fd)
               0)))
        0))
    (defun nl_bi_process_setup_child_fds (infile_sx destination_sx)
      (let* ((in_rc (nl_bi_process_redirect_input infile_sx)))
        (if (= in_rc 0)
            (nl_bi_process_redirect_output destination_sx)
          in_rc)))
	    (defun nl_bi_process_wait_exit_code (pid)
	      (let* ((statusp (alloc-bytes 8 8)))
	        (seq
	         (ptr-write-u64 statusp 0 0)
	         (let* ((wait_rc (nl_os_process_wait4 pid statusp 0))
	                (status (ptr-read-u64 statusp 0)))
	           (if (< wait_rc 0)
	               1
	             (nl_bi_process_decode_status status))))))
	    (defun nl_bi_process_decode_status (status)
	      (let* ((sig (logand status 127)))
	        (if (= sig 0)
	            (logand (/ status 256) 255)
	          (+ 128 sig))))
	    (defun nl_bi_process_set_int (proc idx val)
	      (let* ((slot (alloc-bytes 32 8)))
	        (seq
	         (wf_write_int slot val)
	         (wf_dirty)
	         (vector-slot-set proc idx slot)
	         val)))
	    (defun nl_bi_process_get_int (proc idx)
	      (if (= (nl_bi_process_object_p_raw proc) 1)
	          (ptr-read-u64 (vector-ref-ptr proc idx) 8)
	        -1))
	    (defun nl_bi_process_object_p_raw (proc)
	      (if (= (ptr-read-u64 proc 0) 8)
	          (if (>= (vector-len proc) 5)
	              (if (= (ptr-read-u64 (vector-ref-ptr proc 0) 8) 1886547811)
	                  1
	                0)
	            0)
	        0))
	    (defun nl_bi_process_make_object (pid outfd out)
	      (seq
	       (vector-make 5 out)
	       (nl_bi_process_set_int out 0 1886547811)
	       (nl_bi_process_set_int out 1 pid)
	       (nl_bi_process_set_int out 2 outfd)
	       ;; status: 0 running, 1 exited, 2 failed/signalled, 3 deleted.
	       (nl_bi_process_set_int out 3 0)
	       (nl_bi_process_set_int out 4 -1)
	       0))
	    (defun nl_bi_process_mark_exit (proc code)
	      (seq
	       (nl_bi_process_set_int proc 4 code)
	       (nl_bi_process_set_int proc 3 (if (>= code 128) 2 1))
	       code))
	    (defun nl_bi_process_refresh (proc options)
	      (if (= (nl_bi_process_object_p_raw proc) 1)
	          (if (= (nl_bi_process_get_int proc 3) 0)
	              (let* ((statusp (alloc-bytes 8 8))
	                     (pid (nl_bi_process_get_int proc 1))
	                     (rc 0))
	                (seq
	                 (ptr-write-u64 statusp 0 0)
	                 (setq rc (nl_os_process_wait4 pid statusp options))
	                 (if (= rc 0)
	                     0
	                   (if (< rc 0)
	                       (nl_bi_process_mark_exit proc -1)
	                     (nl_bi_process_mark_exit
	                      proc (nl_bi_process_decode_status
	                            (ptr-read-u64 statusp 0)))))))
	            (nl_bi_process_get_int proc 4))
	        -1))
	    (defun nl_bi_process_status_code (proc)
	      (seq
	       (nl_bi_process_refresh proc 1)
	       (nl_bi_process_get_int proc 3)))
	    (defun nl_bi_process_exit_code (proc)
	      (seq
	       (nl_bi_process_refresh proc 1)
	       (nl_bi_process_get_int proc 4)))
	    (defun nl_bi_process_wait_object (proc)
	      (if (= (nl_bi_process_object_p_raw proc) 1)
	          (if (= (nl_bi_process_get_int proc 3) 0)
	              (nl_bi_process_refresh proc 0)
	            (nl_bi_process_get_int proc 4))
	        -1))
	    (defun nl_bi_process_read_output (args out)
	      (let* ((proc (wf_arg_ptr args 0))
	             (limit (wf_argval args 1)))
	        (if (= (nl_bi_process_object_p_raw proc) 1)
	            (let* ((fd (nl_bi_process_get_int proc 2))
	                   (buf (alloc-bytes limit 1))
	                   (n (nl_os_read_file_handle fd buf limit)))
	              (if (< n 1)
	                  (nl_seq2 (wf_write_nil out) 0)
	                (nl_seq2 (nl_alloc_str buf n out) 0)))
	          (nl_seq2 (wf_write_nil out) 0))))
	    (defun nl_bi_process_delete_object (proc)
	      (if (= (nl_bi_process_object_p_raw proc) 1)
	          (let* ((status (nl_bi_process_get_int proc 3))
	                 (pid (nl_bi_process_get_int proc 1))
	                 (fd (nl_bi_process_get_int proc 2)))
	            (seq
	             (if (= status 0)
	                 (seq
	                  (nl_os_process_kill pid 15)
	                  (nl_bi_process_refresh proc 0))
	               0)
	             (if (>= fd 0) (nl_os_close_handle fd) 0)
	             (nl_bi_process_set_int proc 2 -1)
	             (nl_bi_process_set_int proc 3 3)
	             0))
	        0))
	    (defun nl_bi_process_start_process (args out)
	      (let* ((program_sx (wf_arg_ptr args 0))
	             (arglst (nl_cons_cdr_ptr args))
	             (pipev (alloc-bytes 8 4))
	             (envp (alloc-bytes 8 8))
	             (pipe_rc 0)
	             (pid 0))
	        (seq
	         (ptr-write-u64 envp 0 0)
	         ;; M11 env inherit: prefer the driver-stashed initial-stack envp.
	         (if (= (ptr-read-u64 268435600 0) 0)
	             0
	           (setq envp (ptr-read-u64 268435600 0)))
	         (setq pipe_rc (nl_os_process_pipe pipev))
	         (if (< pipe_rc 0)
	             (wf_write_nil out)
	           (let* ((readfd (ptr-read-u32 pipev 0))
	                  (writefd (ptr-read-u32 pipev 4))
	                  (path (nl_bi_make_cpath program_sx))
	                  (argv (nl_bi_process_make_argv program_sx arglst)))
	             (seq
	              (setq pid (nl_os_process_fork))
	              (if (= pid 0)
	                  (seq
	                   (nl_os_close_handle readfd)
	                   (nl_os_process_dup2 writefd 1)
	                   (nl_os_process_dup2 writefd 2)
	                   (nl_os_close_handle writefd)
	                   (nl_os_process_execve path argv envp)
	                   (nl_os_process_exit127))
	                (if (< pid 0)
	                    (seq
	                     (nl_os_close_handle readfd)
	                     (nl_os_close_handle writefd)
	                     (wf_write_nil out))
	                  (seq
	                   (nl_os_close_handle writefd)
	                   (nl_os_process_set_nonblock readfd)
	                   (nl_bi_process_make_object pid readfd out))))))))))
	    (defun nl_bi_process_call_process (args out)
	      (let* ((program_sx (wf_arg_ptr args 0))
             (infile_sx (wf_arg_ptr args 1))
             (destination_sx (wf_arg_ptr args 2))
             (arglst (nl_bi_process_drop args 4))
             (path (nl_bi_make_cpath program_sx))
             (argv (nl_bi_process_make_argv program_sx arglst))
             (envp (alloc-bytes 8 8))
             (pid 0))
        (seq
         (ptr-write-u64 envp 0 0)
         ;; M11 env inherit: prefer the driver-stashed initial-stack envp.
         (if (= (ptr-read-u64 268435600 0) 0)
             0
           (setq envp (ptr-read-u64 268435600 0)))
         (setq pid (nl_os_process_fork))
         (if (= pid 0)
             (seq
              (if (= (nl_bi_process_setup_child_fds infile_sx destination_sx) 0)
                  (nl_os_process_execve path argv envp)
                1)
              (nl_os_process_exit127))
           (if (< pid 0)
	               (wf_write_int out 1)
	             (wf_write_int out (nl_bi_process_wait_exit_code pid)))))))
	    (defun nl_bi_name_ptr (sx)
	      (if (= (ptr-read-u64 sx 0) 4)
	          (ptr-read-u64 sx 16)
	        (nl_bi_strptr sx)))
	    (defun nl_bi_name_len (sx)
	      (if (= (ptr-read-u64 sx 0) 4)
	          (ptr-read-u64 sx 24)
	        (nl_bi_strlen sx)))
	    (defun nl_bi_word_byte (word idx)
	      (cond
	       ((= idx 0) (logand word 255))
	       ((= idx 1) (logand (/ word 256) 255))
	       ((= idx 2) (logand (/ word 65536) 255))
	       ((= idx 3) (logand (/ word 16777216) 255))
	       ((= idx 4) (logand (/ word 4294967296) 255))
	       ((= idx 5) (logand (/ word 1099511627776) 255))
	       ((= idx 6) (logand (/ word 281474976710656) 255))
	       (t (logand (/ word 72057594037927936) 255))))
	    (defun nl_bi_name_is_loop (ptr u64 idx len)
	      (if (= idx len)
	          1
	        (if (= (ptr-read-u8 ptr idx) (nl_bi_word_byte u64 idx))
	            (nl_bi_name_is_loop ptr u64 (+ idx 1) len)
	          0)))
	    (defun nl_bi_name_is (sx u64 len)
	      (if (= (nl_bi_name_len sx) len)
	          (nl_bi_name_is_loop (nl_bi_name_ptr sx) u64 0 len)
	        0))
	    (defun nl_bi_argval_or_zero (lst n)
	      (if (= (ptr-read-u64 lst 0) 7)
	          (if (= n 0)
	              (ptr-read-u64 (nl_cons_car_ptr lst) 8)
	            (nl_bi_argval_or_zero (nl_cons_cdr_ptr lst) (- n 1)))
	        0))
	    (defun nl_bi_portable_syscall_nr (sx)
	      (cond
	       ((= (nl_bi_name_is sx 110404021020007 6) 1) (nl_os_syscall_nr_getpid))
	       ((= (nl_bi_name_is sx 1802661734 4) 1) (nl_os_syscall_nr_fork))
	       ((= (nl_bi_name_is sx 225291362679 5) 1) (nl_os_syscall_nr_wait4))
	       ((= (nl_bi_name_is sx 1819044203 4) 1) (nl_os_syscall_nr_kill))
	       ((= (nl_bi_name_is sx 1701865840 4) 1) (nl_os_syscall_nr_pipe))
	       ((= (nl_bi_name_is sx 1684104562 4) 1) (nl_os_syscall_nr_read))
	       ((= (nl_bi_name_is sx 435744764535 5) 1) (nl_os_syscall_nr_write))
	       ((= (nl_bi_name_is sx 435728378979 5) 1) (nl_os_syscall_nr_close))
	       ((= (nl_bi_name_is sx 846230884 4) 1) (nl_os_syscall_nr_dup2))
	       ((= (nl_bi_name_is sx 1819045744 4) 1) (nl_os_syscall_nr_poll))
	       ((= (nl_bi_name_is sx 465809859430 5) 1) (nl_os_syscall_nr_fcntl))
	       ((= (nl_bi_name_is sx 1953069157 4) 1) (nl_os_syscall_nr_exit))
	       (t -1)))
	    (defun nl_bi_portable_syscall (args)
	      (let* ((nr (nl_bi_portable_syscall_nr (wf_arg_ptr args 0))))
	        (if (< nr 0)
	            -1
	          (syscall-direct nr
	                          (nl_bi_argval_or_zero args 1)
	                          (nl_bi_argval_or_zero args 2)
	                          (nl_bi_argval_or_zero args 3)
	                          (nl_bi_argval_or_zero args 4)
	                          (nl_bi_argval_or_zero args 5)
	                          (nl_bi_argval_or_zero args 6)))))
	    (defun nl_bi_write_stderr_line (args out)
      (let* ((sx (wf_arg_ptr args 0))
             (nl (alloc-bytes 1 1)))
        (seq
         (nl_os_write_stderr (nl_bi_strptr sx) (nl_bi_strlen sx))
         (ptr-write-u8 nl 0 10)
         (nl_os_write_stderr nl 1)
         (wf_write_nil out)
         0)))
    ;; nelisp--syscall-path NR PATH: generic path-string syscall.  Marshals
    ;; PATH to a NUL-terminated C string (nl_bi_make_cpath), then issues
    ;; syscall NR with the path pointer in rdi and the remaining arg registers
    ;; zeroed -- enough for unlink(87) / rmdir(84) / access(21) and similar
    ;; one-path syscalls.  Returns the raw kernel result (0 = success,
    ;; negative = -errno) as an Int Sexp.  Pure elisp: same helpers as the
    ;; nl-write-file impl plus the `syscall-direct' grammar op (no new Rust).
    (defun nl_bi_syscall_path (args out)
      (let* ((nr (wf_argval args 0))
             (path_sx (wf_arg_ptr args 1))
             (cpath (nl_bi_make_cpath path_sx))
             (rc (nl_os_syscall_path nr cpath)))
        (wf_write_int out rc)))
    ;; nelisp--syscall-path2 NR PATH1 PATH2: two-path-string syscall.  Marshals
    ;; PATH1/PATH2 to two NUL-terminated C strings (separate arena buffers via
    ;; nl_bi_make_cpath), then issues syscall NR with cpath1 in rdi and cpath2 in
    ;; rsi (remaining registers zeroed) -- enough for rename(82) / link(86) /
    ;; symlink(88) and similar two-path syscalls.  Returns the raw kernel result
    ;; (0 = success, negative = -errno) as an Int Sexp.  Pure elisp, no new Rust.
    (defun nl_bi_syscall_path2 (args out)
      (let* ((nr (wf_argval args 0))
             (p1_sx (wf_arg_ptr args 1))
             (p2_sx (wf_arg_ptr args 2))
             (cpath1 (nl_bi_make_cpath p1_sx))
             (cpath2 (nl_bi_make_cpath p2_sx))
             (rc (nl_os_syscall_path2 nr cpath1 cpath2)))
        (wf_write_int out rc)))
    ;; nelisp--syscall-path-int NR PATH INT: one-path + one-integer syscall.
    ;; Marshals PATH to a NUL-terminated C string (nl_bi_make_cpath), then
    ;; issues syscall NR with cpath in rdi and INT in rsi (remaining registers
    ;; zeroed) -- enough for access(21, path, mode) / chmod(90, path, mode) /
    ;; truncate(76, path, len) and similar path+scalar syscalls.  Returns the
    ;; raw kernel result (0 = success, negative = -errno).  Pure elisp, no Rust.
    (defun nl_bi_syscall_path_int (args out)
      (let* ((nr (wf_argval args 0))
             (path_sx (wf_arg_ptr args 1))
             (iarg (wf_argval args 2))
             (cpath (nl_bi_make_cpath path_sx))
             (rc (nl_os_syscall_path_int nr cpath iarg)))
        (wf_write_int out rc)))
    ;; nelisp--syscall-stat-field PATH OFFSET: stat(2) PATH into a 144-byte
    ;; struct stat buffer, then return the u64 at byte OFFSET (little-endian).
    ;; Returns the negative kernel errno when stat fails.  The substrate masks
    ;; sub-8-byte fields itself.  Linux x86_64 struct stat offsets: st_dev 0,
    ;; st_ino 8, st_nlink 16, st_mode 24, st_uid 28, st_gid 32, st_size 48,
    ;; st_atime 72, st_mtime 88, st_ctime 104.  Pure elisp, no new Rust.
    (defun nl_bi_syscall_stat_field (args out)
      (let* ((path_sx (wf_arg_ptr args 0))
             (offset (wf_argval args 1))
             (cpath (nl_bi_make_cpath path_sx))
             (buf (alloc-bytes 144 8))
             (rc (nl_os_stat_path cpath buf)))
        (if (< rc 0)
            (wf_write_int out rc)
          (wf_write_int out (ptr-read-u64 buf offset)))))
    ;; nelisp--syscall-stat-buf PATH: stat(2) PATH into a fresh 144-byte struct
    ;; stat buffer and return that buffer's pointer (a positive address), or the
    ;; negative kernel errno on failure.  The caller reads individual fields
    ;; with ptr-read-u64 -- ONE stat for the whole struct (vs one per field).
    (defun nl_bi_syscall_stat_buf (args out)
      (let* ((path_sx (wf_arg_ptr args 0))
             (cpath (nl_bi_make_cpath path_sx))
             (buf (alloc-bytes 144 8))
             (rc (nl_os_stat_path cpath buf)))
        (if (< rc 0)
            (wf_write_int out rc)
          (wf_write_int out buf))))
    ;; nelisp--syscall-lstat-buf PATH: like stat-buf but lstat(2) (syscall 6) --
    ;; does NOT follow symlinks, so file-attributes can report the link itself.
    (defun nl_bi_syscall_lstat_buf (args out)
      (let* ((path_sx (wf_arg_ptr args 0))
             (cpath (nl_bi_make_cpath path_sx))
             (buf (alloc-bytes 144 8))
             (rc (nl_os_lstat_path cpath buf)))
        (if (< rc 0)
            (wf_write_int out rc)
          (wf_write_int out buf))))
    ;; nelisp--syscall-readlink PATH: readlink(2) (syscall 89) -- return the
    ;; symbolic-link target as a string, or nil when PATH is not a symlink (or
    ;; on any error).  readlink does not NUL-terminate, so the returned length
    ;; bounds the string.
    (defun nl_bi_syscall_readlink (args out)
      (let* ((path_sx (wf_arg_ptr args 0))
             (cpath (nl_bi_make_cpath path_sx))
             (buf (alloc-bytes 4096 1))
             (n (nl_os_readlink_path cpath buf 4096)))
        (if (< n 0)
            (wf_write_nil out)
          (nl_seq2 (nl_alloc_str buf n out) 0))))
    ;; nelisp--syscall-readdir-names PATH: openat(O_RDONLY|O_DIRECTORY) +
    ;; getdents64 + close -- return every entry name in PATH as one
    ;; newline-joined string ("." / ".." included; callers filter), or nil
    ;; when PATH cannot be opened as a directory.  One flat string keeps the
    ;; builtin list-free (same return shape as readlink / read-file).
    ;; dirent64 layout: d_ino@0 d_off@8 d_reclen(u16)@16 d_type(u8)@18
    ;; d_name@19 (NUL-terminated).  Output is clipped at 65535 bytes.
    ;; Pure elisp, no new Rust.
    (defun nl_bi_syscall_readdir_names (args out)
      (let* ((path_sx (wf_arg_ptr args 0))
             (cpath (nl_bi_make_cpath path_sx))
             (fd (nl_os_open_dir cpath)))
        (if (< fd 0)
            (wf_write_nil out)
          (let* ((dbuf (alloc-bytes 32768 8))
                 (sbuf (alloc-bytes 65536 1))
                 (slen 0)
                 (n 1))
            (seq
             (while (> n 0)
               (seq
                (setq n (nl_os_getdents64 fd dbuf 32768))
                (if (> n 0)
                    (let* ((pos 0))
                      (while (< pos n)
                        (let* ((reclen (+ (ptr-read-u8 dbuf (+ pos 16))
                                          (* 256 (ptr-read-u8 dbuf (+ pos 17))))))
                          (if (= reclen 0)
                              (setq pos n)
                            (let* ((np (+ pos 19)))
                              (seq
                               (while (and (< np (+ pos reclen))
                                           (> (ptr-read-u8 dbuf np) 0)
                                           (< slen 65535))
                                 (seq
                                  (ptr-write-u8 sbuf slen (ptr-read-u8 dbuf np))
                                  (setq slen (+ slen 1))
                                  (setq np (+ np 1))))
                               (if (< slen 65536)
                                   (seq
                                    (ptr-write-u8 sbuf slen 10)
                                    (setq slen (+ slen 1)))
                                 0)
                               (setq pos (+ pos reclen))))))))
                  0)))
             (nl_os_close_handle fd)
             (nl_seq2 (nl_alloc_str sbuf slen out) 0))))))
    ;; nelisp--syscall-utimes PATH ATIME MTIME: utimes(2) (syscall 235) -- set
    ;; the access + modification times to ATIME / MTIME (seconds; usec = 0).
    ;; Builds a struct timeval[2] {atime.sec, atime.usec, mtime.sec, mtime.usec}
    ;; and returns the raw kernel rc (0 = success, negative = -errno).
    (defun nl_bi_syscall_utimes (args out)
      (let* ((path_sx (wf_arg_ptr args 0))
             (atime (wf_argval args 1))
             (mtime (wf_argval args 2))
             (cpath (nl_bi_make_cpath path_sx))
             (buf (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 buf 0 atime)
         (ptr-write-u64 buf 8 0)
         (ptr-write-u64 buf 16 mtime)
         (ptr-write-u64 buf 24 0)
         (wf_write_int out (nl_os_utimes_path cpath buf)))))
    ;; nelisp--syscall-statx-buf PATH FLAGS: statx(2) (syscall 332) into a
    ;; 256-byte struct statx buffer; return its pointer (positive) or the
    ;; negative kernel errno.  dirfd = AT_FDCWD (-100), mask = STATX_BASIC_STATS
    ;; | STATX_BTIME (0xfff).  FLAGS = AT_SYMLINK_NOFOLLOW (0x100) for an
    ;; lstat-like probe, 0 to follow.  Exposes birth time (stx_btime @80/@88)
    ;; that struct stat lacks; caller reads fields with ptr-read-u64.
    (defun nl_bi_syscall_statx_buf (args out)
      (let* ((path_sx (wf_arg_ptr args 0))
             (flags (wf_argval args 1))
             (cpath (nl_bi_make_cpath path_sx))
             (buf (alloc-bytes 256 8))
             (rc (nl_os_statx_path cpath flags buf)))
        (if (< rc 0)
            (wf_write_int out rc)
          (wf_write_int out buf))))
    ;; nelisp--syscall-unshare FLAGS: unshare(2) (syscall 272) -- detach the
    ;; calling thread's namespaces named in FLAGS (CLONE_NEWUSER 0x10000000,
    ;; CLONE_NEWNS 0x20000, CLONE_NEWNET 0x40000000, CLONE_NEWPID 0x20000000,
    ;; CLONE_NEWUTS 0x4000000, CLONE_NEWIPC 0x8000000, CLONE_NEWCGROUP
    ;; 0x2000000).  Returns 0 on success or the negative kernel errno.  Pure
    ;; elisp via the `syscall-direct' AOT op (raw `syscall' instruction, NR=272,
    ;; NO libc -- the standalone binary does not link libc; same mechanism the
    ;; nl_os_* file-I/O bodies use).  raw-ns sandbox substrate (nelix design 32).
    (defun nl_bi_syscall_unshare (args out)
      (let* ((flags (wf_argval args 0))
             (rc (syscall-direct 272 flags 0 0 0 0 0)))
        (wf_write_int out rc)))
    ;; nelisp--syscall-mount SOURCE TARGET FSTYPE FLAGS DATA: mount(2) (syscall
    ;; 165).  String args become plain NUL-terminated cstrings via
    ;; nl_bi_make_cpath (which does NOT path-normalise -- copies bytes + NUL, so
    ;; "tmpfs"/""/"/path" pass through verbatim).  FLAGS = mountflags int
    ;; (MS_BIND 4096, MS_REC 16384, MS_RDONLY 1, MS_PRIVATE 262144, MS_REMOUNT
    ;; 32, ...).  Returns 0 or negative kernel errno.  Raw syscall, NO libc.
    ;; raw-ns FS-closure substrate (nelix design 32 v2).
    (defun nl_bi_syscall_mount (args out)
      (let* ((src (nl_bi_make_cpath (wf_arg_ptr args 0)))
             (tgt (nl_bi_make_cpath (wf_arg_ptr args 1)))
             (fst (nl_bi_make_cpath (wf_arg_ptr args 2)))
             (flags (wf_argval args 3))
             (data (nl_bi_make_cpath (wf_arg_ptr args 4)))
             (rc (syscall-direct 165 src tgt fst flags data 0)))
        (wf_write_int out rc)))
    ;; nelisp--syscall-pivot-root NEW_ROOT PUT_OLD: pivot_root(2) (syscall 155).
    ;; Moves the process root to NEW_ROOT, stacking the old root at PUT_OLD
    ;; (must be under NEW_ROOT).  Returns 0 or negative kernel errno.
    (defun nl_bi_syscall_pivot_root (args out)
      (let* ((new (nl_bi_make_cpath (wf_arg_ptr args 0)))
             (old (nl_bi_make_cpath (wf_arg_ptr args 1)))
             (rc (syscall-direct 155 new old 0 0 0 0)))
        (wf_write_int out rc)))
    (defun nl_bi_write_file_t (args out)
      (let* ((path_sx (wf_arg_ptr args 0))
             (cont_sx (wf_arg_ptr args 1))
             (cpath (nl_bi_make_cpath path_sx))
             (fd (nl_os_open_write_truncate cpath)))
        (if (< fd 0)
            (wf_write_nil out)
          (let* ((wr (nl_os_write_file_handle fd (nl_bi_strptr cont_sx)
                                             (nl_bi_strlen cont_sx))))
            (seq
             (nl_os_close_handle fd)
             (if (< wr 0) (wf_write_nil out) (wf_write_t out))))))))
  "M7 file-I/O builtin impls (wrf/rdf/slen).  Uses `nl_seq2' (arena unit),
`wf_arg_ptr'/`wf_write_int' (applyfn unit) and `nl_alloc_str' (alloc-str.o).")

;; ===================================================================
;; M6 catch/throw glue unit (nl_sf_catch + nl_sf_throw + helpers).
;;
;; THROW stashes (tag,val) into the reserved arena region and returns rc=1;
;; rc=1 propagates up through every body-walker (progn/if/let/...) exactly like
;; an error -- they all abort on rc!=0 -- naturally unwinding the whole native
;; call stack with NO frame/landing bookkeeping.  CATCH evals BODY via
;; nelisp_eval_call; on rc=1 it inspects the stash:
;;   flag==0          -> genuine (non-throw) error: re-propagate rc=1.
;;   flag==1, tag eq  -> caught: copy stashed val to *out, clear flag, rc=0.
;;   flag==1, tag ne  -> re-propagate rc=1 (stash kept for the outer catch).
;; Every defun has even arity and every extern-call is argument 0 at its call
;; site so rsp == 0 mod 16 at the call (SysV ABI alignment for the variadic
;; nelisp_eval_call path).  Stash region: TAG @268435480, VAL @268435512,
;; flag @268435472 (reserved by `nelisp-standalone--arena-source').
;; ===================================================================
(defconst nelisp-standalone--catch-throw-source
  '(seq
    (defun nl_ct_copy32 (dst src _p2 _p3)
      (seq (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
           (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
           (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
           (ptr-write-u64 dst 24 (ptr-read-u64 src 24))
           0))
    ;;================= THROW =================
    (defun nl_ct_throw_after_val (rc val_slot tag_slot _p3)
      (if (= rc 0)
          (seq
           (nl_ct_copy32 268435480 tag_slot 0 0)
           (nl_ct_copy32 268435512 val_slot 0 0)
           (ptr-write-u64 268435472 0 1)
           (atomic-fetch-add 268435544 1)
           1)
        1))
    (defun nl_ct_throw_after_tag (rc val_form env out tag_slot val_slot)
      (if (= rc 0)
          (nl_ct_throw_after_val
           (extern-call nelisp_eval_call val_form env val_slot)
           val_slot tag_slot 0)
        1))
    (defun nl_ct_throw_eval_tag (tag_form val_form env out tag_slot val_slot)
      (nl_ct_throw_after_tag
       (extern-call nelisp_eval_call tag_form env tag_slot)
       val_form env out tag_slot val_slot))
    (defun nl_ct_throw_got_cdr1 (cdr1 tag_form env out tag_slot val_slot)
      (nl_ct_throw_eval_tag
       tag_form
       (extern-call nl_cons_car_ptr cdr1)
       env out tag_slot val_slot))
    (defun nl_ct_throw_got_tag_form (tag_form args env out tag_slot val_slot)
      (nl_ct_throw_got_cdr1
       (extern-call nl_cons_cdr_ptr args)
       tag_form env out tag_slot val_slot))
    (defun nl_sf_throw (args env out _pad)
      (if (= (sexp-tag args) 7)
          (let* ((tag_slot (alloc-bytes 32 8)) (val_slot (alloc-bytes 32 8)))
            (nl_ct_throw_got_tag_form
             (extern-call nl_cons_car_ptr args)
             args env out tag_slot val_slot))
        1))
    ;;================= CATCH =================
    (defun nl_ct_catch_on_match (eqres tag_slot env out _p4 _p5)
      (if (= eqres 1)
          (seq (nl_ct_copy32 out 268435512 0 0)
               (ptr-write-u64 268435472 0 0)
               0)
        1))
    (defun nl_ct_catch_check_tag (tag_slot env out eqres_slot _p4 _p5)
      (nl_ct_catch_on_match
       (seq (nelisp_eq_symbol 268435480 tag_slot eqres_slot)
            (ptr-read-u64 eqres_slot 0))
       tag_slot env out 0 0))
    (defun nl_ct_catch_caught (_rc tag_slot env out eqres_slot _p5)
      (if (= (ptr-read-u64 268435472 0) 1)
          (nl_ct_catch_check_tag tag_slot env out eqres_slot 0 0)
        1))
    (defun nl_ct_catch_body_step (rc body_rest tag_slot env out eqres_slot)
      (if (= rc 0)
          (nl_ct_catch_body body_rest tag_slot env out eqres_slot 0)
        (nl_ct_catch_caught rc tag_slot env out eqres_slot 0)))
    (defun nl_ct_catch_body_eval (car body_rest tag_slot env out eqres_slot)
      (nl_ct_catch_body_step
       (extern-call nelisp_eval_call car env out)
       body_rest tag_slot env out eqres_slot))
    (defun nl_ct_catch_body_cdr (body_rest body tag_slot env out eqres_slot)
      (nl_ct_catch_body_eval
       (extern-call nl_cons_car_ptr body)
       body_rest tag_slot env out eqres_slot))
    (defun nl_ct_catch_body (body tag_slot env out eqres_slot _p5)
      (if (= (sexp-tag body) 0)
          0
        (nl_ct_catch_body_cdr
         (extern-call nl_cons_cdr_ptr body)
         body tag_slot env out eqres_slot)))
    (defun nl_ct_catch_after_tag (rc body tag_slot env out eqres_slot)
      (if (= rc 0)
          (nl_ct_catch_body body tag_slot env out eqres_slot 0)
        1))
    (defun nl_ct_catch_got_body (body tag_form env out tag_slot eqres_slot)
      (nl_ct_catch_after_tag
       (extern-call nelisp_eval_call tag_form env tag_slot)
       body tag_slot env out eqres_slot))
    (defun nl_ct_catch_got_tag_form (tag_form args env out tag_slot eqres_slot)
      (nl_ct_catch_got_body
       (extern-call nl_cons_cdr_ptr args)
       tag_form env out tag_slot eqres_slot))
    (defun nl_sf_catch (args env out _pad)
      (if (= (sexp-tag args) 7)
          (let* ((tag_slot (alloc-bytes 32 8)) (eqres_slot (alloc-bytes 32 8)))
            (nl_ct_catch_got_tag_form
             (extern-call nl_cons_car_ptr args)
             args env out tag_slot eqres_slot))
        1)))
  "M6 catch/throw special-form impls.  nl_sf_catch/nl_sf_throw are dispatched
from the patched combiner-cons (see `nelisp-standalone--patch-combiner-cons').")

;; M6 special-form name predicates injected into the combiner-cons unit (so they
;; can call its in-unit `nl_cons_sym_eq').  u64 packings: "catch"=448345170275,
;; "throw"=512970877044.
(defconst nelisp-standalone--sp-eq-catch
  '(defun nl_sp_eq_catch (name_ptr)
     (let* ((buf (alloc-bytes 8 1)))
       (seq (ptr-write-u64 buf 0 448345170275)
            (let* ((eqr (nl_cons_sym_eq name_ptr buf 5)))
              (seq (if (= (ptr-read-u64 268435680 0) 1) (nl_gc_free_block (- buf 8)) 0) eqr))))))

(defconst nelisp-standalone--sp-eq-throw
  '(defun nl_sp_eq_throw (name_ptr)
     (let* ((buf (alloc-bytes 8 1)))
       (seq (ptr-write-u64 buf 0 512970877044)
            (let* ((eqr (nl_cons_sym_eq name_ptr buf 5)))
              (seq (if (= (ptr-read-u64 268435680 0) 1) (nl_gc_free_block (- buf 8)) 0) eqr))))))

(defun nelisp-standalone--patch-apply-special (form)
  "Rewrite the nl_apply_special defun FORM: replace its terminal else `2'
(unknown-special-form) with catch/throw dispatch arms, keeping the existing
if-chain intact."
  (let* ((name (cadr form))
         (arglist (caddr form))
         (body (cdddr form)))
    (cl-labels
        ((rewrite (node)
           (cond
            ((eq node 2)
             '(if (= (nl_sp_eq_catch name_ptr) 1)
                  (nl_sf_catch args_ptr env_ptr out 0)
                (if (= (nl_sp_eq_throw name_ptr) 1)
                    (nl_sf_throw args_ptr env_ptr out 0)
                  2)))
            ((consp node) (cons (rewrite (car node)) (rewrite (cdr node))))
            (t node))))
      (append (list 'defun name arglist) (mapcar #'rewrite body)))))

(defun nelisp-standalone--patch-combiner-cons (src)
  "Return combiner-cons SRC (a `(seq (defun ...) ...)') with the two
nl_sp_eq_catch / nl_sp_eq_throw predicates inserted before nl_apply_special and
that defun's terminal `2' rewritten to dispatch nl_sf_catch / nl_sf_throw.
Keeps lisp/ pristine (mirrors `--patch-combiner-apply')."
  (cons (car src)
        (let (out inserted)
          (dolist (form (cdr src))
            (cond
             ((and (consp form) (eq (car form) 'defun)
                   (eq (cadr form) 'nl_apply_special))
              (unless inserted
                (push nelisp-standalone--sp-eq-catch out)
                (push nelisp-standalone--sp-eq-throw out)
                (setq inserted t))
              (push (nelisp-standalone--patch-apply-special form) out))
             (t (push form out))))
          (nreverse out))))

;; ===================================================================
;; MACRO-EXPANSION IN-PLACE CACHING (Wave-1 (A)).  A second build-time sexp patch
;; on the combiner-cons unit, COMPOSED AFTER the catch/throw patch above (both
;; patch the same source, so chain them: catch/throw first, then macro-cache).
;;
;; It rewrites two defuns:
;;   * nl_cons_macro_apply_eval — gains a leading `box_ptr' parameter (the
;;     original form's NlConsBox pointer = &car @ box+0, &cdr @ box+32).  After
;;     computing the macro expansion it OVERWRITES the original form box's car+cdr
;;     (64 bytes) with the expansion box's car+cdr (8x ptr-write-u64), IN PLACE,
;;     so the form the lambda body holds BECOMES the expansion.  The next time
;;     that body form is evaluated its head is the expansion's head (if/+/progn/
;;     ...) not the macro name -> the macro is never re-expanded.  Caching is
;;     restricted to Cons expansions (cond/when/unless/myif all expand to a Cons);
;;     non-Cons expansions (macro->constant/symbol) fall back to the un-cached
;;     eval (provably safe -- overwriting a 64-byte cons box with a 32-byte
;;     self-eval value would leave a stale cdr / wrong tag).
;;   * nl_eval_inner_cons — its macro arm threads `head_ptr' (= the box pointer)
;;     as that new first argument of nl_cons_macro_apply_eval.
;; ===================================================================
(defconst nelisp-standalone--macro-cache-apply-eval
  '(defun nl_cons_macro_apply_eval (box_ptr func_ptr tail_ptr env out)
     ;; box_ptr: NlConsBox* of the ORIGINAL form (= &car @ box+0, &cdr @ box+32).
     ;; func_ptr: resolved macro value, shape (macro . (CLOSURE . nil)).
     ;; tail_ptr: unevaluated arg list (= original box's cdr value).
     (let* ((func_cdr (nl_cons_cdr_ptr func_ptr)))
       (if (= (ptr-read-u64 func_cdr 0) 7)
           (let* ((macrofn_ptr (nl_cons_car_ptr func_cdr))
                  (exp_slot (alloc-bytes 32 8))
                  (env_ptr env))
             (let* ((rc_mac (nl_apply_function macrofn_ptr tail_ptr env_ptr exp_slot)))
               (if (= rc_mac 0)
                   ;; Expansion computed.  Cache it in place IFF it is a Cons
                   ;; (tag 7): copy the expansion box's car+cdr (64 bytes) over
                   ;; the ORIGINAL form box, then eval the now-rewritten form.
                   (if (= (ptr-read-u64 exp_slot 0) 7)
                       (let* ((exp_box (nl_cons_car_ptr exp_slot)))
                         (seq
                          ;; original.car (@box_ptr+0..32) <- expansion.car
                          (ptr-write-u64 box_ptr 0  (ptr-read-u64 exp_box 0))
                          (ptr-write-u64 box_ptr 8  (ptr-read-u64 exp_box 8))
                          (ptr-write-u64 box_ptr 16 (ptr-read-u64 exp_box 16))
                          (ptr-write-u64 box_ptr 24 (ptr-read-u64 exp_box 24))
                          ;; original.cdr (@box_ptr+32..64) <- expansion.cdr
                          (ptr-write-u64 box_ptr 32 (ptr-read-u64 exp_box 32))
                          (ptr-write-u64 box_ptr 40 (ptr-read-u64 exp_box 40))
                          (ptr-write-u64 box_ptr 48 (ptr-read-u64 exp_box 48))
                          (ptr-write-u64 box_ptr 56 (ptr-read-u64 exp_box 56))
                          ;; Now eval the rewritten form: head = box.car (= exp
                          ;; head), tail = box.cdr.  box_ptr IS &car, box_ptr+32
                          ;; IS &cdr -- exactly nl_eval_inner_cons's (head,tail).
                          (nl_eval_inner_cons box_ptr (+ box_ptr 32) env_ptr out)))
                     ;; Non-Cons expansion: do not cache (the form is a 64-byte
                     ;; cons box; overwriting it with a 32-byte self-eval value
                     ;; would leave a stale cdr / wrong tag).  Eval directly.
                     (nelisp_eval_call exp_slot env_ptr out))
                 1)))
         (nl_cons_stash_void_function env func_ptr))))
  "Macro-caching replacement for nl_cons_macro_apply_eval (gains box_ptr arg).")

(defun nelisp-standalone--macro-cache-patch-eval-inner-cons (form)
  "Rewrite the nl_eval_inner_cons defun FORM: its macro arm
`(nl_cons_macro_apply_eval func_slot tail_ptr env out)' gains the leading
`head_ptr' argument (= the original form's box pointer)."
  (cl-labels
      ((rw (node)
         (cond
          ((and (consp node) (eq (car node) 'nl_cons_macro_apply_eval))
           ;; (nl_cons_macro_apply_eval func_slot tail_ptr env out)
           ;;   -> (nl_cons_macro_apply_eval head_ptr func_slot tail_ptr env out)
           (cons 'nl_cons_macro_apply_eval (cons 'head_ptr (mapcar #'rw (cdr node)))))
          ((consp node) (cons (rw (car node)) (rw (cdr node))))
          (t node))))
    (rw form)))

(defun nelisp-standalone--patch-void-function-miss (form)
  "Rewrite nl_eval_inner_cons FORM so unbound function lookup stashes.

The shipped eval-port combiner-cons path propagates `rc_lu == 1' from
`nelisp_env_lookup_function' without materializing the expected
`void-function' signal payload.  The reader driver then observes a
non-zero rc with the result slot still nil, so `(missing-fn ...)'
collapses to nil instead of surfacing an error.  Patch the specific
`(if (= rc_lu 0) ... 1)' miss arm inside `nl_eval_inner_cons' to call
`nl_cons_stash_void_function' with the unresolved head symbol."
  (cl-labels
      ((rw (node)
         (cond
          ((and (consp node)
                (eq (car node) 'if)
                (equal (cadr node) '(= rc_lu 0))
                (equal (cadddr node) 1))
           (list 'if
                 '(= rc_lu 0)
                 (rw (nth 2 node))
                 '(nl_cons_stash_void_function env head_ptr)))
          ((consp node) (cons (rw (car node)) (rw (cdr node))))
          (t node))))
    (rw form)))

(defun nelisp-standalone--patch-macro-cache (src)
  "Patch combiner-cons SRC: swap nl_cons_macro_apply_eval for the caching version
and thread head_ptr at its call site in nl_eval_inner_cons.  COMPOSE AFTER
`nelisp-standalone--patch-combiner-cons' (apply catch/throw first, then this)."
  (cons (car src)
        (mapcar
         (lambda (form)
           (cond
            ((and (consp form) (eq (car form) 'defun)
                  (eq (cadr form) 'nl_cons_macro_apply_eval))
             nelisp-standalone--macro-cache-apply-eval)
            ((and (consp form) (eq (car form) 'defun)
                  (eq (cadr form) 'nl_eval_inner_cons))
             (nelisp-standalone--macro-cache-patch-eval-inner-cons
              (nelisp-standalone--patch-void-function-miss form)))
            (t form)))
         (cdr src))))

(defun nelisp-standalone--patch-combiner-cons-full (src)
  "Apply combiner-cons build-time patches that are valid for Doc 147 layout.

The old macro-expansion in-place cache assumed `head_ptr' was the original
form's cons box.  After Doc 147, `head_ptr' is a materialised 32B car view from
`nl_cons_car_ptr', so caching corrupts memory.  Keep the catch/throw and
void-function fixes, and leave macro expansion uncached.

Doc 152 gate-G: the void-function-miss rewrite (`--patch-void-function-miss')
was dropped when the macro-cache patch was disabled (it had been composed only
inside `--patch-macro-cache').  Re-apply it directly on `nl_eval_inner_cons' so
the function-lookup miss arm calls `nl_cons_stash_void_function' (which now sets
the M6 signal stash + flag) instead of returning a bare rc=1 -- otherwise a
call to an undefined function is an UNCATCHABLE process exit (condition-case
never sees it), which blocked the anvil-pkg ERT suite at its first test."
  (let ((patched (nelisp-standalone--patch-combiner-cons src)))
    (cons (car patched)
          (mapcar (lambda (form)
                    (if (and (consp form) (eq (car form) 'defun)
                             (eq (cadr form) 'nl_eval_inner_cons))
                        (nelisp-standalone--patch-void-function-miss form)
                      form))
                  (cdr patched)))))

;; CORRECTED arg-list walk.  Byte-identical to nelisp-cc-evalport-combiner-arglist
;; --source EXCEPT the success tail wraps the constructor in (seq ... 0): the
;; pure-elisp `cons-make' op returns the SLOT pointer (nonzero) in rax, which the
;; real nl_eval_inner_cons rc==0 check would reject.  The per-arg evaluation
;; (REAL nelisp_eval_call on each car) is unchanged.
(defconst nelisp-standalone--arglist-source
  '(seq
    (defun nl_write_nil_slot (slot)
      (seq (ptr-write-u64 slot 0 0) (ptr-write-u64 (+ slot 8) 0 0)
           (ptr-write-u64 (+ slot 16) 0 0) (ptr-write-u64 (+ slot 24) 0 0) 0))
    ;; Doc 150 P1: immediate-result arg-slot recycling.  nl_val_store_word
    ;; returns the SLOT POINTER itself for heap-tagged values (the slot IS
    ;; the value box -> escapes into the cons; never free those), but for
    ;; Nil/T/Int it returns an immediate WORD (3 / 7 / odd fixnum) -- the
    ;; 32B slot's address was never exposed anywhere, so it is provably
    ;; dead scratch.  Push it straight back onto the size-segregated
    ;; free-list (nl_gc_free_block; boot-watermark guard included), making
    ;; interpreted loops with int/nil-valued args ~O(1) on arg slots
    ;; instead of leaking 32B+hdr per arg per iteration (GC cannot run
    ;; intra-form).  WORD==SLOT is the exact escape discriminator: slots
    ;; are 8-aligned pointers, immediates are odd or 3/7.
    ;; Flag 268435680 (boot init = 1) gates the recycling; 0 = legacy.
    (defun nl_arg_slot_recycle (slot word)
      (if (= word slot)
          word
        (if (= (ptr-read-u64 268435680 0) 1)
            (nl_seq2 (nl_gc_free_block (- slot 8)) word)
          word)))
    (defun nl_eval_arg_list_walk (cur_ptr env_ptr acc_slot)
      (if (= (ptr-read-u64 cur_ptr 0) 7)
          (let* ((car_ptr (nl_cons_car_ptr cur_ptr)) (cdr_ptr (nl_cons_cdr_ptr cur_ptr))
                 (rest_slot (alloc-bytes 32 8)))
            ;; Doc 146 §3.0 step 4: a self-eval immediate literal arg (tag<4:
            ;; Nil/T/Int/Float) needs NO eval and NO 32-byte eval_slot -- load it
            ;; straight to a value word.  This eliminates the per-literal-arg slot
            ;; (the producer-side memory win on the hottest path).  Non-literal
            ;; args keep the eval-into-slot + rc check unchanged.
            (if (< (nl_val_tag car_ptr) 4)
                (let* ((rc_rest (nl_eval_arg_list_walk cdr_ptr env_ptr rest_slot)))
                  (if (= rc_rest 0)
                      (seq (nelisp_cons_construct (nl_val_store_word car_ptr) (nl_arg_slot_recycle rest_slot (nl_val_store_word rest_slot)) acc_slot) 0)
                    1))
              (let* ((eval_slot (alloc-bytes 32 8))
                     (rc_eval (nelisp_eval_call car_ptr env_ptr eval_slot)))
                (if (= rc_eval 0)
                    (let* ((rc_rest (nl_eval_arg_list_walk cdr_ptr env_ptr rest_slot)))
                      (if (= rc_rest 0)
                          (seq (nelisp_cons_construct (nl_arg_slot_recycle eval_slot (nl_val_store_word eval_slot)) (nl_arg_slot_recycle rest_slot (nl_val_store_word rest_slot)) acc_slot) 0)
                        1))
                  1))))
        (nl_write_nil_slot acc_slot)))
    (defun nl_eval_arg_list (args_ptr env out_list_slot)
      (let* ((env_ptr env)) (nl_eval_arg_list_walk args_ptr env_ptr out_list_slot)))))

;; TRAP-STUBS — externs genuinely never executed on the (OP A B) builtin path
;; (special-form bodies, eval-inner var/cell branches, lambda/closure, frame/bind
;; shims, nelisp_aot_builtin_call1).  Each returns a sentinel; probes confirmed
;; none run for a plain (builtin . (+ . nil)) application.
(defconst nelisp-standalone--trap-source
  '(seq
    (defun nl_sf_if (_a _e _o _p) 1) (defun nl_sf_let (_a _e _o _p) 1)
    (defun nl_sf_let_star (_a _e _o _p) 1) (defun nl_sf_setq (_a _e _o _p) 1)
    (defun nl_sf_while (_a _e _o _p) 1) (defun nl_sf_lambda (_a _e _o _p) 1)
    (defun nl_sf_function (_a _e _o _p) 1) (defun nl_sf_condition_case (_a _e _o _p) 1)
    (defun nl_sf_unwind_protect (_a _e _o _p) 1) (defun nl_sf_quote (_a _o) 1)
    (defun nl_sf_progn (_a _e _o _p) 1)
    (defun nl_env_lookup_val (_f _e _o) 1) (defun nl_cell_get_value (_c _o) 1)
    (defun nl_apply_lambda_inner (_cap _f _b _a _e _o) 1)
    (defun nelisp_frame_push (_f _s) 1) (defun nelisp_frame_pop (_f _s) 1)
    (defun nelisp_env_bind_local (_m _f _n _v _vec _flag) 1)
    (defun nelisp_env_shim_op (_op _m _s _u _o _p) 0)
    (defun nelisp_env_shim_set_op (_op _m _s _sc _o _z) 0)
    (defun nl_env_push_captured (_e _a) 1)
    (defun nelisp_aot_builtin_call1 (_a _b _c _d) 1)))

;; ===================================================================
;; _start unit (always first): switch onto a LARGE mmap'd native stack before
;; calling the driver, so deep eval recursion (the CPS eval chain burns ~5 KiB
;; of native frames PER eval level) does not overflow the default ~8 MiB kernel
;; stack.  The OLD `_start' just did (mov rdi,rsp; call driver) on the entry
;; stack, capping recursion at ~2300 eval levels (cnt(2400) SIGSEGV).
;;
;; New sequence (raw x86_64; the mmap syscall clobbers rcx/r11 but NOT the
;; callee-saved r15, so the entry argv pointer survives across the syscall):
;;   49 89 e7                   mov  r15, rsp     ; save entry rsp (&argc / argv ptr)
;;   b8 09 00 00 00             mov  eax, 9       ; SYS_mmap
;;   31 ff                      xor  edi, edi     ; addr = NULL (kernel picks, high)
;;   be SS SS SS SS             mov  esi, SIZE    ; length (SIZE < 2^31, zero-ext ok)
;;   ba 03 00 00 00             mov  edx, 3       ; PROT_READ|PROT_WRITE
;;   41 ba 22 00 02 00          mov  r10d, 0x20022; MAP_PRIVATE|ANON|STACK
;;   49 c7 c0 ff ff ff ff       mov  r8, -1       ; fd = -1
;;   45 31 c9                   xor  r9d, r9d     ; offset = 0
;;   0f 05                      syscall           ; rax = new stack base
;;   48 8d a0 DD DD DD DD       lea  rsp,[rax+SIZE-16] ; top of new stack, -16
;;   48 83 e4 f0                and  rsp, -16     ; force 16-byte alignment
;;   4c 89 ff                   mov  rdi, r15     ; driver arg0 = entry argv ptr
;;   e8 00 00 00 00             call driver       ; reloc pc32 @ offset 53
;;   89 c7                      mov  edi, eax      ; exit code = driver()
;;   b8 3c 00 00 00             mov  eax, 60       ; SYS_exit
;;   0f 05                      syscall
;; SIZE = 0x40000000 (1 GiB), virtual-only (anonymous, untouched pages cost no
;; RAM), so SIZE/disp fit in a 32-bit field and SS/DD are little-endian SIZE and
;; SIZE-16.  The reloc :offset is 53 (the rel32 sits right after the `e8').  Raise
;; rec_max in BOTH driver sources to ~74% of the new ~404k rec-level native ceiling
;; so deep recursion errors at the guard rather than SIGSEGV (see `... ctx 104').
;; Anonymous mmap lands high, far from the 0x10000000..0x110000000 arena -> no
;; collision.  MAP_ANONYMOUS=0x20, MAP_PRIVATE=0x02, MAP_STACK=0x20000 -> 0x20022.
;; ===================================================================
(defconst nelisp-standalone--native-stack-size #x40000000
  "Size (bytes) of the mmap'd native stack the `_start' trampoline switches to.
1 GiB, virtual-only (anonymous); must stay < 2^31 so SIZE / SIZE-16 fit the
32-bit immediate / displacement fields in the hand-assembled `_start'.")

(defconst nelisp-standalone--macos-native-stack-size #x20000000
  "Size (bytes) of the macOS mmap'd native stack.
Darwin backs anonymous mappings more eagerly than Linux's demand-paged stack
trampoline, so macOS uses a smaller explicit stack while still exceeding the
system default stack enough for standalone-reader initialization.")

(defun nelisp-standalone--le32 (n)
  "N as a 4-byte little-endian list (for hand-assembled imm32/disp32 fields)."
  (list (logand n #xff) (logand (ash n -8) #xff)
        (logand (ash n -16) #xff) (logand (ash n -24) #xff)))

(defun nelisp-standalone--start-unit ()
  (let* ((size nelisp-standalone--native-stack-size)
         (head (append
                (list #x49 #x89 #xe7)                  ; mov r15, rsp
                (cons #xb8 (nelisp-standalone--le32 9))      ; mov eax, 9 (SYS_mmap)
                (list #x31 #xff)                       ; xor edi, edi  (addr=NULL)
                (cons #xbe (nelisp-standalone--le32 size))   ; mov esi, SIZE
                (cons #xba (nelisp-standalone--le32 3))      ; mov edx, 3 (RW)
                (cons #x41 (cons #xba (nelisp-standalone--le32 #x20022))) ; mov r10d, flags
                (list #x49 #xc7 #xc0 #xff #xff #xff #xff)    ; mov r8, -1 (fd)
                (list #x45 #x31 #xc9)                  ; xor r9d, r9d (offset=0)
                (list #x0f #x05)                       ; syscall
                (cons #x48 (cons #x8d (cons #xa0 (nelisp-standalone--le32 (- size 16))))) ; lea rsp,[rax+SIZE-16]
                (list #x48 #x83 #xe4 #xf0)             ; and rsp, -16
                (list #x4c #x89 #xff)                  ; mov rdi, r15
                (list #xe8)))                          ; call (rel32 follows)
         (reloc-off (length head))                     ; rel32 offset (= 53 for 1 GiB)
         (text (apply #'unibyte-string
                      (append head
                              (list 0 0 0 0)           ; rel32 placeholder
                              (list #x89 #xc7)         ; mov edi, eax
                              (cons #xb8 (nelisp-standalone--le32 60)) ; mov eax, 60
                              (list #x0f #x05)))))      ; syscall
    (nelisp-link-unit-make (nelisp-standalone--target-object-name "start.o")
     (list (cons 'text text))
     (list (nelisp-link-symbol "_start" 0 :section 'text :bind 'global :type 'func))
     (list (list :offset reloc-off :type 'pc32 :symbol "driver" :addend 0 :section 'text)))))

(defun nelisp-standalone--windows-start-unit ()
  "Return the Win64 PE `_start' unit.
Windows enters this CRT-free entry directly.  It passes NULL as driver arg0
for the baked-form path, then exits through KERNEL32!ExitProcess."
  (let* ((head (append
                (list #x48 #x83 #xe4 #xf0) ; and rsp, -16
                (list #x48 #x83 #xec #x20) ; sub rsp, 32 (shadow space)
                (list #x31 #xc9)           ; xor ecx, ecx
                (list #xe8)))              ; call driver
         (driver-reloc-off (length head))
         (mid (append head
                      (list 0 0 0 0)
                      (list #x89 #xc1)     ; mov ecx, eax
                      (list #xe8)))        ; call ExitProcess
         (exit-reloc-off (length mid))
         (text (apply #'unibyte-string
                      (append mid
                              (list 0 0 0 0)
                              (list #xcc)))))
    (nelisp-link-unit-make
     (nelisp-standalone--target-object-name "start.o")
     (list (cons 'text text))
     (list (nelisp-link-symbol "_start" 0
                               :section 'text :bind 'global :type 'func))
     (list (list :offset driver-reloc-off
                 :type 'pc32
                 :symbol "driver"
                 :addend 0
                 :section 'text)
           (list :offset exit-reloc-off
                 :type 'plt32
                 :symbol "ExitProcess"
                 :addend 0
                 :section 'text)))))

(defun nelisp-standalone--macos-aarch64-basic-start-unit ()
  "Return the small macOS arm64 Mach-O `_main' start unit.
Dyld enters `_main(argc, argv, envp)'.  The driver expects the Linux entry-stack
argv shape (`argc' at slot 0, argv pointers inline after it), so this trampoline
copies argc and argv[0..3] into a small stack block and passes that block to
`driver'.  It then exits through the Darwin raw syscall ABI with x16=1 and
SVC #0x80."
  (let* ((buf (nelisp-asm-arm64-make-buffer))
         (reloc-off nil))
    (nelisp-asm-arm64-sub-imm buf 'sp 'sp 48)
    (nelisp-asm-arm64-str-imm buf 'x0 'sp 0)  ; argc
    (nelisp-asm-arm64-ldr-imm buf 'x2 'x1 0)
    (nelisp-asm-arm64-str-imm buf 'x2 'sp 8)
    (nelisp-asm-arm64-ldr-imm buf 'x2 'x1 8)
    (nelisp-asm-arm64-str-imm buf 'x2 'sp 16)
    (nelisp-asm-arm64-ldr-imm buf 'x2 'x1 16)
    (nelisp-asm-arm64-str-imm buf 'x2 'sp 24)
    (nelisp-asm-arm64-ldr-imm buf 'x2 'x1 24)
    (nelisp-asm-arm64-str-imm buf 'x2 'sp 32)
    (nelisp-asm-arm64-mov-reg-reg buf 'x0 'sp)
    (setq reloc-off (nelisp-asm-arm64-buffer-pos buf))
    (nelisp-asm-arm64-emit-reloc buf 'b26-pc "driver")
    (nelisp-asm-arm64--emit-word buf #x94000000) ; bl driver
    (nelisp-asm-arm64-mov-imm64 buf 'x16 1)
    (nelisp-asm-arm64-svc buf #x80)
    (nelisp-link-unit-make
     "start.o"
     (list (cons 'text (nelisp-asm-arm64-buffer-bytes buf)))
     (list (nelisp-link-symbol "_main" 0
                               :section 'text :bind 'global :type 'func))
     (list (list :offset reloc-off
                 :type 'b26-pc
                 :symbol "driver"
                 :addend 0
                 :section 'text)))))

(defun nelisp-standalone--macos-aarch64-reader-start-unit ()
  "Return the macOS arm64 Mach-O `_main' start unit for standalone-reader.
Dyld enters `_main(argc, argv, envp)'.  The reader driver expects the Linux
entry-stack argv shape (`argc' at slot 0, argv pointers inline after it), so
this trampoline first snapshots argc and argv[0..3] from the original stack,
switches onto a large anonymous mmap'd native stack, copies the snapshot into a
fresh driver stack block, and passes that block to `driver'.  It then exits
through the Darwin raw syscall ABI with x16=1 and SVC #0x80."
  (let* ((size nelisp-standalone--macos-native-stack-size)
         (buf (nelisp-asm-arm64-make-buffer))
         (reloc-off nil))
    (nelisp-asm-arm64-sub-imm buf 'sp 'sp 48)
    (nelisp-asm-arm64-ldr-imm buf 'x2 'sp 48) ; original sp: argc
    (nelisp-asm-arm64-str-imm buf 'x2 'sp 0)
    (nelisp-asm-arm64-ldr-imm buf 'x2 'sp 56)
    (nelisp-asm-arm64-str-imm buf 'x2 'sp 8)
    (nelisp-asm-arm64-ldr-imm buf 'x2 'sp 64)
    (nelisp-asm-arm64-str-imm buf 'x2 'sp 16)
    (nelisp-asm-arm64-ldr-imm buf 'x2 'sp 72)
    (nelisp-asm-arm64-str-imm buf 'x2 'sp 24)
    (nelisp-asm-arm64-ldr-imm buf 'x2 'sp 80)
    (nelisp-asm-arm64-str-imm buf 'x2 'sp 32)
    (nelisp-asm-arm64-mov-imm64 buf 'x0 0)
    (nelisp-asm-arm64-mov-imm64 buf 'x1 size)
    (nelisp-asm-arm64-mov-imm64 buf 'x2 3)       ; PROT_READ|PROT_WRITE
    (nelisp-asm-arm64-mov-imm64 buf 'x3 4098)    ; MAP_PRIVATE|MAP_ANON
    (nelisp-asm-arm64-mov-imm64 buf 'x4 -1)
    (nelisp-asm-arm64-mov-imm64 buf 'x5 0)
    (nelisp-asm-arm64-mov-imm64 buf 'x16 197)    ; Darwin mmap
    (nelisp-asm-arm64-svc buf #x80)
    (nelisp-asm-arm64-mov-imm64 buf 'x10 size)
    (nelisp-asm-arm64-add-reg-reg buf 'x9 'x0 'x10)
    (nelisp-asm-arm64-sub-imm buf 'x9 'x9 64)
    (nelisp-asm-arm64-ldr-imm buf 'x2 'sp 0)
    (nelisp-asm-arm64-str-imm buf 'x2 'x9 0)
    (nelisp-asm-arm64-ldr-imm buf 'x2 'sp 8)
    (nelisp-asm-arm64-str-imm buf 'x2 'x9 8)
    (nelisp-asm-arm64-ldr-imm buf 'x2 'sp 16)
    (nelisp-asm-arm64-str-imm buf 'x2 'x9 16)
    (nelisp-asm-arm64-ldr-imm buf 'x2 'sp 24)
    (nelisp-asm-arm64-str-imm buf 'x2 'x9 24)
    (nelisp-asm-arm64-ldr-imm buf 'x2 'sp 32)
    (nelisp-asm-arm64-str-imm buf 'x2 'x9 32)
    (nelisp-asm-arm64-add-imm buf 'sp 'x9 0)
    (nelisp-asm-arm64-mov-reg-reg buf 'x0 'sp)
    (setq reloc-off (nelisp-asm-arm64-buffer-pos buf))
    (nelisp-asm-arm64-emit-reloc buf 'b26-pc "driver")
    (nelisp-asm-arm64--emit-word buf #x94000000) ; bl driver
    (nelisp-asm-arm64-mov-imm64 buf 'x16 1)
    (nelisp-asm-arm64-svc buf #x80)
    (nelisp-link-unit-make
     "start.o"
     (list (cons 'text (nelisp-asm-arm64-buffer-bytes buf)))
     (list (nelisp-link-symbol "_main" 0
                               :section 'text :bind 'global :type 'func))
     (list (list :offset reloc-off
                 :type 'b26-pc
                 :symbol "driver"
                 :addend 0
                 :section 'text)))))

(defun nelisp-standalone--macos-aarch64-start-unit (&optional reader-p)
  "Return the macOS arm64 start unit.
When READER-P is non-nil, switch onto a larger explicit native stack before
calling the reader driver."
  (if reader-p
      (nelisp-standalone--macos-aarch64-reader-start-unit)
    (nelisp-standalone--macos-aarch64-basic-start-unit)))

(defun nelisp-standalone--linux-aarch64-start-unit ()
  "Return the Linux arm64 ELF `_start' unit.
The kernel enters `_start' with argc at [sp] and argv inline after it —
exactly the entry-stack shape the reader driver consumes, so unlike the
macOS trampoline no argv re-packing is needed.  Mirrors the x86_64 unit:
mmap a large anonymous native stack, switch onto it, call `driver' with
the ORIGINAL sp as arg0, then exit(driver-return) via SVC #0 (x8=93)."
  (require 'nelisp-asm-arm64)
  (let* ((size nelisp-standalone--native-stack-size)
         (buf (nelisp-asm-arm64-make-buffer))
         (reloc-off nil))
    (nelisp-asm-arm64-add-imm buf 'x19 'sp 0)    ; x19 = original sp (argc/argv)
    (nelisp-asm-arm64-mov-imm64 buf 'x0 0)       ; addr = NULL
    (nelisp-asm-arm64-mov-imm64 buf 'x1 size)
    (nelisp-asm-arm64-mov-imm64 buf 'x2 3)       ; PROT_READ|PROT_WRITE
    (nelisp-asm-arm64-mov-imm64 buf 'x3 #x20022) ; MAP_PRIVATE|MAP_ANON|MAP_STACK
    (nelisp-asm-arm64-mov-imm64 buf 'x4 -1)      ; fd
    (nelisp-asm-arm64-mov-imm64 buf 'x5 0)       ; offset
    (nelisp-asm-arm64-mov-imm64 buf 'x8 222)     ; mmap (arm64 Linux)
    (nelisp-asm-arm64-svc buf 0)
    (nelisp-asm-arm64-mov-imm64 buf 'x10 (- size 16))
    (nelisp-asm-arm64-add-reg-reg buf 'x9 'x0 'x10) ; page-aligned base + size-16
    (nelisp-asm-arm64-add-imm buf 'sp 'x9 0)     ; switch onto the native stack
    (nelisp-asm-arm64-mov-reg-reg buf 'x0 'x19)  ; arg0 = original entry sp
    (setq reloc-off (nelisp-asm-arm64-buffer-pos buf))
    (nelisp-asm-arm64-emit-reloc buf 'b26-pc "driver")
    (nelisp-asm-arm64--emit-word buf #x94000000) ; bl driver
    (nelisp-asm-arm64-mov-imm64 buf 'x8 93)      ; exit (arm64 Linux)
    (nelisp-asm-arm64-svc buf 0)
    (nelisp-link-unit-make
     (nelisp-standalone--target-object-name "start.o")
     (list (cons 'text (nelisp-asm-arm64-buffer-bytes buf)))
     (list (nelisp-link-symbol "_start" 0
                               :section 'text :bind 'global :type 'func))
     (list (list :offset reloc-off
                 :type 'b26-pc
                 :symbol "driver"
                 :addend 0
                 :section 'text)))))

(defun nelisp-standalone--windows-aarch64-start-unit ()
  "Return the Windows ARM64 PE `_start' unit.
Windows enters the CRT-free entry directly with a 16-aligned sp.  Pass
NULL as driver arg0 (the Windows reader builds argv from
GetCommandLineW, not the entry stack), then exit through
KERNEL32!ExitProcess with the driver return already in x0/w0."
  (require 'nelisp-asm-arm64)
  (let* ((buf (nelisp-asm-arm64-make-buffer))
         (driver-off nil)
         (exit-off nil))
    (nelisp-asm-arm64-sub-imm buf 'sp 'sp 16)    ; scratch, keep 16-aligned
    (nelisp-asm-arm64-mov-imm64 buf 'x0 0)       ; driver arg0 = NULL
    (setq driver-off (nelisp-asm-arm64-buffer-pos buf))
    (nelisp-asm-arm64-emit-reloc buf 'b26-pc "driver")
    (nelisp-asm-arm64--emit-word buf #x94000000) ; bl driver
    (setq exit-off (nelisp-asm-arm64-buffer-pos buf))
    (nelisp-asm-arm64-emit-reloc buf 'b26-pc "ExitProcess")
    (nelisp-asm-arm64--emit-word buf #x94000000) ; bl ExitProcess (w0 = code)
    (nelisp-asm-arm64--emit-word buf #xD4200000) ; brk #0 (not reached)
    (nelisp-link-unit-make
     (nelisp-standalone--target-object-name "start.o")
     (list (cons 'text (nelisp-asm-arm64-buffer-bytes buf)))
     (list (nelisp-link-symbol "_start" 0
                               :section 'text :bind 'global :type 'func))
     (list (list :offset driver-off
                 :type 'b26-pc
                 :symbol "driver"
                 :addend 0
                 :section 'text)
           (list :offset exit-off
                 :type 'b26-pc
                 :symbol "ExitProcess"
                 :addend 0
                 :section 'text)))))

(defun nelisp-standalone--target-start-unit (&optional reader-p)
  "Return the target-specific standalone start unit."
  (pcase nelisp-standalone--target
    ('windows-x86_64 (nelisp-standalone--windows-start-unit))
    ('windows-aarch64 (nelisp-standalone--windows-aarch64-start-unit))
    ('macos-aarch64 (nelisp-standalone--macos-aarch64-start-unit reader-p))
    ('linux-aarch64 (nelisp-standalone--linux-aarch64-start-unit))
    (_ (nelisp-standalone--start-unit))))

;; ===================================================================
;; Parametrized driver (FULL real path).  Bootstrap mirror + 60 system builtins,
;; install OP via REAL nl_install_one (arithmetic is not in the bootstrap set),
;; build EvalCtx as real nl_eval_ctx_make does, construct (OP A B), eval via the
;; REAL shim -> nl_eval_inner -> ... -> nelisp_apply_function.  Always recompiled.
;; ===================================================================
(defun nelisp-standalone--form-params ()
  "Return (OP-U64 A B EXPECTED) for the embedded form from the environment."
  (let* ((op (or (getenv "NELISP_FORM_OP") "+"))
         (a (string-to-number (or (getenv "NELISP_FORM_A") "1")))
         (b (string-to-number (or (getenv "NELISP_FORM_B") "2")))
         (expected (cond ((string= op "+") (+ a b))
                         ((string= op "-") (- a b))
                         ((string= op "*") (* a b))
                         (t (error "standalone: unsupported OP %S (use + - *)" op)))))
    (list (aref op 0) a b expected)))

(defun nelisp-standalone--driver-source ()
  (pcase-let ((`(,op-u64 ,a ,b ,_exp) (nelisp-standalone--form-params)))
    `(defun driver ()
       (let* ((arena (nl_arena_init))
              (globals (alloc-bytes 32 8)) (frames (alloc-bytes 32 8)) (unbound (alloc-bytes 32 8))
              (ctx (alloc-bytes 120 8))
              (opbuf (alloc-bytes 8 1)) (op_sym (alloc-bytes 32 8))
              (builtin_buf (alloc-bytes 8 1)) (builtin_sym (alloc-bytes 32 8))
              (int1 (alloc-bytes 32 8)) (int2 (alloc-bytes 32 8))
              (nil_s (alloc-bytes 32 8)) (tail_s (alloc-bytes 32 8))
              (args_s (alloc-bytes 32 8)) (form_s (alloc-bytes 32 8)) (out_s (alloc-bytes 32 8)))
         (seq
          (nl_bootstrap_make_mirror globals frames unbound)
          (ptr-write-u64 builtin_buf 0 31078196194145634)
          (nl_alloc_symbol builtin_buf 7 builtin_sym)
          (ptr-write-u64 opbuf 0 ,op-u64)
          (nl_install_one globals unbound opbuf 1 builtin_sym)
          (nl_sexp_clone_into globals (+ ctx 0))
          (nl_sexp_clone_into frames (+ ctx 32))
          (nl_sexp_clone_into unbound (+ ctx 64))
          ;; rec_max 300000: the `_start' trampoline now runs the driver on a 1 GiB
;; mmap'd native stack whose ceiling is ~404k rec levels (each eval level ~5 KiB of
;; native frames; a self-recursive call burns ~2 rec increments).  300000 is ~74% of
;; that ceiling, so deep recursion (cnt(100000) -> 42) succeeds while still erroring
;; at the guard -- never SIGSEGV -- once it exceeds the budget.
(ptr-write-u64 ctx 96 0) (ptr-write-u64 ctx 104 300000)
          (nl_alloc_symbol opbuf 1 op_sym)
          (ptr-write-u64 int1 0 2) (ptr-write-u64 int1 8 ,a) (ptr-write-u64 int1 16 0) (ptr-write-u64 int1 24 0)
          (ptr-write-u64 int2 0 2) (ptr-write-u64 int2 8 ,b) (ptr-write-u64 int2 16 0) (ptr-write-u64 int2 24 0)
          (ptr-write-u64 nil_s 0 0) (ptr-write-u64 nil_s 8 0) (ptr-write-u64 nil_s 16 0) (ptr-write-u64 nil_s 24 0)
          (nelisp_cons_construct int2 nil_s tail_s)
          (nelisp_cons_construct int1 tail_s args_s)
          (nelisp_cons_construct op_sym args_s form_s)
          (ptr-write-u64 out_s 0 0) (ptr-write-u64 out_s 8 0) (ptr-write-u64 out_s 16 0) (ptr-write-u64 out_s 24 0)
          (nelisp_eval_call form_s ctx out_s)
          (ptr-read-u64 out_s 8))))))

;; ===================================================================
;; Manifest — ORDERED unit list.  Each entry: (NAME KIND SRC).
;;   KIND = a feature symbol  -> (require it); SRC = its --source defvar symbol
;;        = :glue              -> SRC is a local glue defconst symbol
;;        = :start / :driver   -> synthesized specially
;; _start first, arena last (linker convention).
;; ===================================================================
(defconst nelisp-standalone--manifest
  '(("start.o"            :start  nil)
    ("driver.o"           :driver nil)
    ("shim.o"             :glue   nelisp-standalone--shim-source)
    ("applyfn.o"          :glue   nelisp-standalone--applyfn-baked-source)
    ("eval-inner.o"       nelisp-cc-eval-inner                    nelisp-cc-eval-inner--source)
    ("combiner-cons.o"    nelisp-cc-evalport-combiner-cons        nelisp-cc-evalport-combiner-cons--source)
    ("combiner-apply.o"   nelisp-cc-evalport-combiner-apply       nelisp-cc-evalport-combiner-apply--source)
    ("arglist.o"          :glue   nelisp-standalone--arglist-source)
    ("lookup-fn.o"        nelisp-cc-env-lookup-function           nelisp-cc-env-lookup-function--source)
    ("bootstrap.o"        nelisp-cc-evalport-bootstrap            nelisp-cc-evalport-bootstrap--source)
    ("mirror-lookup.o"    nelisp-cc-mirror-lookup-entry           nelisp-cc-mirror-lookup-entry--source)
    ("mirror-alloc.o"     nelisp-cc-mirror-alloc-entry            nelisp-cc-mirror-alloc-entry--source)
    ("mirror-prepend.o"   nelisp-cc-mirror-bucket-prepend         nelisp-cc-mirror-bucket-prepend--source)
    ("mirror-setfn.o"     nelisp-cc-mirror-set-function-or-insert nelisp-cc-mirror-set-function-or-insert--source)
    ("env-install.o"      nelisp-cc-env-install-empty             nelisp-cc-env-install-empty--source)
    ("fnv1a.o"            nelisp-cc-fnv1a                         nelisp-cc-fnv1a--source)
    ("clone.o"            nelisp-cc-sexp-clone-into               nelisp-cc-sexp-clone-into--source)
    ("car-ptr.o"          nelisp-cc-jit-cons-car-ptr              nelisp-cc-jit-cons-car-ptr--source)
    ("cdr-ptr.o"          nelisp-cc-jit-cons-cdr-ptr              nelisp-cc-jit-cons-cdr-ptr--source)
    ("eq-symbol.o"        nelisp-cc-eq-symbol                     nelisp-cc-eq-symbol--source)
    ("cons-ctor.o"        nelisp-cc-cons-construct                nelisp-cc-cons-construct--source)
    ;; Doc 152 §11.37 Stage 2 — dynamic root stack (additive/dormant until Stage 3).
    ("rootstack.o"        nelisp-cc-rootstack                     nelisp-cc-rootstack--source)
    ("consbox.o"          nelisp-cc-nlconsbox-alloc               nelisp-cc-nlconsbox-alloc--source)
    ("consbox-clone.o"    nelisp-cc-nlconsbox-clone               nelisp-cc-nlconsbox-clone--source)
    ("consbox-setcar.o"   nelisp-cc-nlconsbox-set-car             nelisp-cc-nlconsbox-set-car--source)
    ("consbox-setcdr.o"   nelisp-cc-nlconsbox-set-cdr             nelisp-cc-nlconsbox-set-cdr--source)
    ("vec-alloc.o"        nelisp-cc-nlvector-alloc                nelisp-cc-nlvector-alloc--source)
    ("vec-set.o"          nelisp-cc-nlvector-set-slot             nelisp-cc-nlvector-set-slot--source)
    ;; Doc 147 Phase 2 — word->32B-slot materialiser for vector-ref-ptr.
    ("vec-slot-ptr.o"     nelisp-cc-nlvector-slot-ptr            nelisp-cc-nlvector-slot-ptr--source)
    ("vec-clone.o"        nelisp-cc-nlvector-clone                nelisp-cc-nlvector-clone--source)
    ("alloc-str.o"        nelisp-cc-nlstr-direct-ops              nelisp-cc-nlstr-direct-ops--alloc-str-source)
    ("str-bytes.o"        nelisp-cc-nlstr-direct-ops              nelisp-cc-nlstr-direct-ops--str-bytes-ptr-source)
    ("str-clone.o"        nelisp-cc-nlstr-clone                   nelisp-cc-nlstr-clone--source)
    ("cell-clone.o"       nelisp-cc-nlcell-clone                  nelisp-cc-nlcell-clone--source)
    ("chartable-clone.o"  nelisp-cc-nlchartable-clone             nelisp-cc-nlchartable-clone--source)
    ("boolvec-clone.o"    nelisp-cc-nlboolvector-clone            nelisp-cc-nlboolvector-clone--source)
    ("record-alloc.o"     nelisp-cc-nlrecord-alloc                nelisp-cc-nlrecord-alloc--source)
    ("record-clone.o"     nelisp-cc-nlrecord-clone                nelisp-cc-nlrecord-clone--source)
    ("record-set.o"       nelisp-cc-nlrecord-set-slot             nelisp-cc-nlrecord-set-slot--source)
    ;; Doc 147 Phase 2 — word->32B-slot materialiser for record-slot-ref-ptr.
    ("record-slot-ptr.o"  nelisp-cc-nlrecord-slot-ptr           nelisp-cc-nlrecord-slot-ptr--source)
    ;; Doc 147 — tagged-word value load/clone helpers (nl_val_load,
    ;; nl_val_clone_into) referenced by the car/cdr/slot-ptr + setcar/setcdr +
    ;; vec/record-set units above.  Same unit the reader manifest links; its own
    ;; deps (nl_sci_store_imm, nl_sexp_clone_into, nl_alloc_bytes) are already
    ;; provided by clone.o / arena.o, so this closes the eval link cascade.
    ("val-load.o"          nelisp-cc-val-load                    nelisp-cc-val-load--source)
    ("trap.o"             :glue   nelisp-standalone--trap-source)
    ("arena.o"            :glue   nelisp-standalone--arena-source))
  "Ordered standalone-eval unit manifest.")

(defun nelisp-standalone--unit-for (entry)
  "Produce the link-unit for a manifest ENTRY."
  (pcase-let ((`(,name ,kind ,src) entry))
    (pcase kind
      (:start (nelisp-standalone--target-start-unit))
      ;; driver is form-dependent and tiny: always recompiled, never cached.
      (:driver (let* ((name (nelisp-standalone--target-object-name "driver.o"))
                      (u (nelisp-standalone--compile-to-unit name
                                                            (nelisp-standalone--driver-source))))
                 (push name nelisp-standalone--recompiled) u))
      (:glue (nelisp-standalone--cached-unit
              name
              (if (and (string= name "arena.o")
                       (eq src 'nelisp-standalone--arena-source))
                  (nelisp-standalone--target-arena-source)
                (symbol-value src))
              nelisp-standalone--this-file))
      (_ ;; feature unit
       (require kind)
       (nelisp-standalone--cached-unit name (symbol-value src)
                                       (locate-library (symbol-name kind)))))))

;;;###autoload
(defun nelisp-standalone-compile-chunk ()
  "Compile this worker's slice of the cacheable units to the cache (NO link).
Reads NELISP_CHUNK_IDX / NELISP_CHUNK_N from the environment; a
worker takes the cacheable manifest positions where (mod POS N) == IDX,
skipping :start (synthesized) and :driver (always recompiled at link
time).  Concurrency-safe: each unit writes its own NAME.unit file, so
N processes never contend.  Used by the multi-process parallel build
(`tools/build-standalone-parallel.sh' on POSIX and
`tools/build-standalone-parallel.ps1' on Windows).

NOTE: for the current 37-unit set the per-unit compile cost (~0.4s
total) is far below the per-process emacs startup + module-load cost
(~4s), so the serial build is faster.  Parallelism pays off only once
per-unit compilation dominates startup (e.g. many more / much heavier
units)."
  (let* ((idx (string-to-number (or (getenv "NELISP_CHUNK_IDX") "0")))
         (n   (max 1 (string-to-number (or (getenv "NELISP_CHUNK_N") "1"))))
         (pos 0))
    (setq nelisp-standalone--recompiled nil)
    (dolist (entry nelisp-standalone--manifest)
      (unless (memq (nth 1 entry) '(:start :driver))
        (when (= (mod pos n) idx)
          (nelisp-standalone--unit-for entry))
        (setq pos (1+ pos))))
    (message "[standalone] chunk %d/%d compiled: %s" idx n
             (if nelisp-standalone--recompiled
                 (string-join (reverse nelisp-standalone--recompiled) " ")
               "none (all cached)"))))

;;;###autoload
(defun nelisp-standalone-build ()
  "Incrementally build the standalone NeLisp eval ELF; return its path."
  (setq nelisp-standalone--recompiled nil)
  (let* ((units (mapcar #'nelisp-standalone--unit-for nelisp-standalone--manifest))
         ;; Doc 140 Stage 8: append the driver-owned `nl_arena_base' bss slot
         ;; unit referenced by the chunk-arena rewrite on chunked native
         ;; targets.
         (units (if (nelisp-standalone--target-uses-dynamic-arena-base-p)
                    (append units (list (nelisp-standalone--arena-base-slot-unit)))
                  units))
         (out (nelisp-standalone--output-path nil)))
    (pcase nelisp-standalone--target
      ('windows-x86_64
       (nelisp-link-units-pe32 out units "_start"
                               '("ExitProcess" "VirtualAlloc" "VirtualFree")
                               (list :stack-reserve
                                     nelisp-standalone--windows-stack-reserve)))
      ('windows-aarch64
       (nelisp-link-units-pe32 out units "_start"
                               '("ExitProcess" "VirtualAlloc" "VirtualFree")
                               (list :machine 'aarch64
                                     :stack-reserve
                                     nelisp-standalone--windows-stack-reserve)))
      ('macos-aarch64
       (nelisp-link-units-macho-exec out units "_main" 'aarch64)
       (set-file-modes out #o755)
       (nelisp-standalone--codesign-macos-adhoc out))
      ('linux-aarch64
       (nelisp-link-units out units "_start" nil 'aarch64)
       (set-file-modes out #o755))
      (_
       (nelisp-link-units out units)
       (set-file-modes out #o755)))
    (message "[standalone] linked %d units -> %s" (length units) out)
    (message "[standalone] recompiled this build: %s"
             (if nelisp-standalone--recompiled
                 (string-join (reverse nelisp-standalone--recompiled) " ")
               "none (all served from cache)"))
    out))

;;;###autoload
(defun nelisp-standalone-rebuild-one (name)
  "Force-recompile just unit NAME (e.g. \"eq-symbol.o\") then relink."
  (let ((cache (expand-file-name
                (concat (nelisp-standalone--target-object-name name) ".unit")
                (nelisp-standalone--target-cache-dir))))
    (when (file-exists-p cache) (delete-file cache)))
  (nelisp-standalone-build))

;;;###autoload
(defun nelisp-standalone-clean ()
  "Delete the per-unit object cache (forces a full recompile next build)."
  (when (file-directory-p nelisp-standalone--cache-dir)
    (delete-directory nelisp-standalone--cache-dir t))
  (message "[standalone] cache cleaned: %s" nelisp-standalone--cache-dir))

;;;###autoload
(defun nelisp-standalone-test ()
  "Build then run the standalone binary; assert exit == expected.  Exits 0/1."
  (let ((out (nelisp-standalone-build)))
  (pcase-let ((`(,_op ,_a ,_b ,expected) (nelisp-standalone--form-params)))
    (let ((code (call-process out nil nil nil)))
      (if (= code expected)
          (progn (message "[standalone] PASS: %s -> exit %d (expected %d)"
                          out code expected)
                 (kill-emacs 0))
        (message "[standalone] FAIL: %s -> exit %d (expected %d)"
                 out code expected)
        (kill-emacs 1))))))

;; ===================================================================
;; READER PATH (Doc 137 M1) — text -> AOT reader -> eval, ZERO Rust.
;;
;; Instead of evaluating a HAND-BUILT (OP A B) AST, the binary parses
;; embedded SOURCE TEXT through the pure-elisp AOT reader
;; (nelisp_reader_parse_one, the FIRST non-Rust caller of it) and then
;; evals the parsed form.  The eval flow after parse is byte-identical to
;; the baked-form driver; only the form's origin changes (real reader vs.
;; hand-built slots).  This is the REPL floor of the self-hosting ladder.
;;
;; Source via NELISP_SRC (default "(+ 40 2)" -> 42).  Supported builtins
;; are still + - * (M2 grows these); the embedded form must use them.
;;   make standalone-reader        # build target/nelisp
;;   make standalone-reader-test   # build, run, assert eval(NELISP_SRC)
;; ===================================================================

(defconst nelisp-standalone--reader-extra-manifest
  '(("reader-lexer.o"     nelisp-cc-reader-lexer                 nelisp-cc-reader-lexer--source)
    ("reader-parser.o"    nelisp-cc-reader-parser                nelisp-cc-reader-parser--source)
    ("str-to-float.o"     nelisp-cc-evalport-str-to-float        nelisp-cc-evalport-str-to-float--source)
    ("mut-str-push.o"     nelisp-cc-evalport-nonenv-mut-str-push nelisp-cc-evalport-nonenv-mut-str-push--source)
    ("alloc-mut-str.o"    nelisp-cc-nlstr-direct-ops             nelisp-cc-nlstr-direct-ops--alloc-mut-str-source)
    ("mut-str-finalize.o" nelisp-cc-nlstr-direct-ops             nelisp-cc-nlstr-direct-ops--mut-str-finalize-source)
    ("ptr-read-u8.o"      nelisp-cc-atomic-raw-mem               nelisp-cc-atomic-raw-mem--read-u8-source)
    ("ptr-write-u8.o"     nelisp-cc-atomic-raw-mem               nelisp-cc-atomic-raw-mem--write-u8-source)
    ("alloc-bytes-fn.o"   nelisp-cc-alloc-dealloc                nelisp-cc-alloc-dealloc--alloc-bytes-source)
    ("dealloc-bytes-fn.o" nelisp-cc-alloc-dealloc                nelisp-cc-alloc-dealloc--dealloc-bytes-source))
  "Extra units the reader path needs beyond `nelisp-standalone--manifest'.
The reader's grammar ops (mut-str-make-empty/-push-byte/-finalize, the
raw-mem u8 ops, alloc/dealloc) lower to runtime extern calls that the
baked-form path never references; these units resolve them.")

;; nl_sexp_write_float: runtime symbol with no elisp `defun' (only the
;; Float-token path calls it via nl_str_to_float).  The helper ABI is
;; `extern "C" fn(slot: *mut Sexp, val: f64) -> *mut Sexp': slot in
;; rdi, value in xmm0, return slot in rax.  Keep this as a tiny raw unit
;; because Phase47 does not expose a direct f64-bit-pattern-to-GP value
;; form suitable for writing the inline payload here.
(defun nelisp-standalone--reader-float-unit ()
  "Return the raw link unit exporting `nl_sexp_write_float'."
  (let ((text (apply #'unibyte-string
                     '(#x48 #xc7 #x07 #x03 #x00 #x00 #x00 ; mov qword [rdi], 3
                       #x66 #x0f #xd6 #x47 #x08           ; movq [rdi+8], xmm0
                       #x48 #xc7 #x47 #x10 #x00 #x00 #x00 #x00 ; clear +16
                       #x48 #xc7 #x47 #x18 #x00 #x00 #x00 #x00 ; clear +24
                       #x48 #x89 #xf8                     ; mov rax, rdi
                       #xc3))))                            ; ret
    (nelisp-link-unit-make
     (nelisp-standalone--target-object-name "reader-float.o")
     (list (cons 'text text))
     (list (nelisp-link-symbol "nl_sexp_write_float" 0
                               :section 'text :bind 'global :type 'func))
     nil)))

;; nl_os_float_time: gettimeofday(2) + the f64 division done INLINE in asm,
;; bypassing the AOT compiler's syscall<->f64 codegen bug (any AOT-generated
;; nl_sexp_write_float / f64 op after a `syscall' aborts at runtime; no elisp
;; polyfill can express float-time -- proven exhaustively).  Same hand-asm
;; approach as `nl_sexp_write_float' above.  Signature:
;;   extern "C" fn(out: *mut Sexp) -> *mut Sexp
;; rdi = out (32-byte slot); fills it with a Float Sexp = tv_sec + tv_usec*1e-6
;; and returns out.  linux-x86_64 only (__NR_gettimeofday=96), matching the
;; x86_64-specific reader-float.o; other arches need their own stub.
(defun nelisp-standalone--float-time-unit ()
  "Return the raw link unit exporting `nl_os_float_time'."
  (let ((text (apply #'unibyte-string
                     ;; rcx/r11 are saved across the body: the `syscall'
                     ;; instruction clobbers them, and the AOT extern-call model
                     ;; assumes the callee leaves them intact -- so without this
                     ;; float-time works at top level but corrupts the
                     ;; interpreter and aborts when called from inside a defun
                     ;; (where those registers are live).
                     '(#x53                              ; push rbx
                       #x48 #x89 #xfb                    ; mov rbx, rdi  (save out)
                       #x51                              ; push rcx
                       #x41 #x53                         ; push r11
                       #x48 #x83 #xec #x10               ; sub rsp, 16   (timeval)
                       #x48 #x89 #xe7                    ; mov rdi, rsp  (&tv)
                       #x31 #xf6                         ; xor esi, esi  (tz = NULL)
                       #xb8 #x60 #x00 #x00 #x00          ; mov eax, 96   (gettimeofday)
                       #x0f #x05                         ; syscall
                       #xf2 #x48 #x0f #x2a #x04 #x24     ; cvtsi2sd xmm0, [rsp]    (sec)
                       #xf2 #x48 #x0f #x2a #x4c #x24 #x08 ; cvtsi2sd xmm1, [rsp+8] (usec)
                       #x48 #xb8 #x00 #x00 #x00 #x00 #x80 #x84 #x2e #x41 ; mov rax, bits(1e6)
                       #x66 #x48 #x0f #x6e #xd0          ; movq xmm2, rax  (= 1000000.0)
                       #xf2 #x0f #x5e #xca               ; divsd xmm1, xmm2 (usec/1e6)
                       #xf2 #x0f #x58 #xc1               ; addsd xmm0, xmm1 (sec + usec/1e6)
                       #x48 #xc7 #x03 #x03 #x00 #x00 #x00 ; mov qword [rbx], 3 (tag Float)
                       #x66 #x0f #xd6 #x43 #x08          ; movq [rbx+8], xmm0 (f64 bits)
                       #x48 #xc7 #x43 #x10 #x00 #x00 #x00 #x00 ; mov qword [rbx+16], 0
                       #x48 #xc7 #x43 #x18 #x00 #x00 #x00 #x00 ; mov qword [rbx+24], 0
                       #x48 #x89 #xd8                    ; mov rax, rbx  (return out)
                       #x48 #x83 #xc4 #x10               ; add rsp, 16
                       #x41 #x5b                         ; pop r11
                       #x59                              ; pop rcx
                       #x5b                              ; pop rbx
                       #xc3))))                          ; ret
    (nelisp-link-unit-make
     (nelisp-standalone--target-object-name "float-time.o")
     (list (cons 'text text))
     (list (nelisp-link-symbol "nl_os_float_time" 0
                               :section 'text :bind 'global :type 'func))
     nil)))

(defun nelisp-standalone--reader-src ()
  "Embedded source text for the reader build (NELISP_SRC; default \"(+ 40 2)\")."
  (or (getenv "NELISP_SRC") "(+ 40 2)"))

(defun nelisp-standalone--reader-expected ()
  "Host-Emacs oracle: eval ALL top-level forms of NELISP_SRC; return the last
value (matches the binary's M8 read+eval-loop driver)."
  (let ((forms (car (read-from-string
                     (concat "(" (nelisp-standalone--reader-src) "\n)"))))
        (r nil))
    (dolist (f forms r) (setq r (eval f t)))))

(defconst nelisp-standalone--reader-builtins
  '("+" "-" "*" "/" "mod" "%" "/=" "1+" "1-" "floor" "truncate" "ceiling" "=" "<" ">" "<=" ">=" "car" "cdr" "cons" "list" "eq" "null" "not"
    ;; Globals shim bridge for user-loaded .el files (`defvar' / `defconst'
    ;; in the standalone prelude lower through this entry).
    "nelisp--env-globals-op"
    ;; M4 hash tables
    "make-hash-table" "puthash" "gethash" "remhash" "hash-table-count" "maphash"
    ;; List search hot paths
    "memq" "member" "assq" "assoc" "rassoc"
    ;; M5 strings + format
    "length" "concat" "substring" "make-string" "string="
    "char-to-string" "string-to-char" "number-to-string" "string-to-number" "format"
    "nelisp--repr" "nelisp--json-encode" "nelisp--sha256" "nelisp--string-search" "nelisp--arena-stats" "garbage-collect"
    "nelisp--fmt-float"
    "nelisp--gc-diag" "nelisp--arena-force-grow-smoke" "nelisp--size-census" "nelisp--arena-walk-verify"
    "nelisp--arena-dump-copy-verify" "nelisp--arena-mark-reach-verify" "nelisp--arena-swizzle-verify"
    "nelisp--arena-load-relocate-verify" "nelisp--arena-image-root-verify"
    "nelisp--arena-dump-table-verify"
    "nelisp--arena-dump-image-to-file" "nelisp--arena-dump-image-stream" "nelisp--arena-load-image-from-file"
    "nelisp--arena-boot-load-verify" "nelisp--arena-load-split-verify"
    "nelisp--arena-value-survival"
    "nelisp--env-capture-roots" "nelisp--record-expand"
    ;; M7 file I/O
    "wrf" "rdf" "slen" "load" "str-count-nl" "str-line-start" "str-kv-line"
    "str-filter-prefix-lines" "nl-nanosleep"
    "nelisp--eval-source-string" "nelisp--syscall-read-file" "nl-write-file"
    "nelisp--syscall-path" "nelisp--syscall-path2" "nelisp--syscall-path-int"
    "nelisp--syscall-stat-field" "nelisp--syscall-stat-buf"
    "nelisp--syscall-lstat-buf" "nelisp--syscall-readlink"
    "nelisp--syscall-readdir-names"
    "nelisp--syscall-utimes" "nelisp--syscall-statx-buf" "nelisp--syscall-unshare"
    "nelisp--syscall-mount" "nelisp--syscall-pivot-root"
    "nelisp--write-stdout-bytes" "nelisp--write-stderr-line"
    "nelisp--read-all-from-string-native"
    "nl-current-unix-time" "nl-unix-time-usec" "float-time"
    "exit"
    ;; Wave-1 (B) breadth: predicates / symbol+vector ops / equal / setcar-setcdr
    ;; / signal-error (the names back the breadth arms in the reader applyfn).
    "consp" "atom" "stringp" "symbolp" "integerp" "natnump" "numberp" "floatp"
    "vectorp" "listp" "zerop" "set" "symbol-value" "fboundp" "boundp" "featurep" "provide" "require"
    "symbol-name" "intern" "make-symbol" "unibyte-string"
    "make-vector" "vector" "aref" "elt" "aset" "record" "recordp" "make-record"
    "signal" "error" "equal" "setcar" "setcdr"
    ;; Wave-2 (C): bitwise / shift / string<
    "ash" "logand" "logior" "logxor" "lognot" "string<"
    "syscall-direct" "atomic-fetch-add"
    "ptr-read-u8" "ptr-write-u8" "ptr-read-u32" "ptr-write-u32"
    "ptr-read-u64" "ptr-write-u64" "alloc-bytes"
    "nelisp-process-call-process" "nelisp-process-start"
    "nelisp-process-start-process" "nelisp-process-object-p"
    "nelisp-process-async-ready-p" "nelisp-process-pid"
    "nelisp-process-status" "nelisp-process-exit-status"
    "nelisp-process-read-output" "nelisp-process-wait"
    "nelisp-process-delete" "nelisp-portable-syscall"
    "ptr-call" "thread-spawn" "thread-join" "fork-spawn")
  "Builtin names installed into the reader binary's mirror.
Each is dispatched by the pure-elisp `nelisp_apply_function' (see
`nelisp-standalone--applyfn-source').  Names > 8 bytes (for example
\"make-hash-table\") require the full-length name-buffer install in
`nelisp-standalone--reader-driver-source' plus the `sexp-name-eq'
dispatch arm in `nelisp-standalone--applyfn-dispatch-table'.")

(defconst nelisp-standalone--reader-read-cap 4194304
  "4 MiB read cap for file-load and standalone compiler load paths.")

(defconst nelisp-standalone--windows-reader-imports
  (list (cons "KERNEL32.dll"
              (list "ExitProcess" "VirtualAlloc" "VirtualFree" "GetCommandLineW"
                    "GetStdHandle" "CreateFileW" "ReadFile" "WriteFile"
                    "CloseHandle" "WideCharToMultiByte"
                    "MultiByteToWideChar"))
        (cons "SHELL32.dll" (list "CommandLineToArgvW")))
  "PE imports needed by the Windows-native standalone reader.")

(defun nelisp-standalone--reader-pe-imports ()
  "Return PE imports for the current Windows-native standalone reader."
  nelisp-standalone--windows-reader-imports)

(defun nelisp-standalone--reader-repl-prelude-source ()
  "Return source evaluated once before the standalone reader REPL loop.
Concatenates the stdlib prelude with the pure-elisp regexp matcher (Doc 143)
and the `string-match' family aliases over it."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "scripts/nelisp-stdlib-prelude.el"
                       nelisp-standalone--repo-root))
    (goto-char (point-max))
    (insert "\n;; --- Doc 143: regexp matcher + string-match family ---\n")
    (insert-file-contents
     (expand-file-name "lisp/nelisp-stdlib-regexp.el"
                       nelisp-standalone--repo-root))
    (goto-char (point-max))
    (insert "\n(defun string-match (re s &optional start)\n"
            "  (if (and (stringp re) (stringp s))\n"
            "      (nlre-string-match re s start)\n"
            "    (signal 'wrong-type-argument (list 'stringp (if (stringp re) s re)))))\n"
            "(defun string-match-p (re s &optional start)\n"
            "  (if (and (stringp re) (stringp s))\n"
            "      (nlre-string-match re s start)\n"
            "    (signal 'wrong-type-argument (list 'stringp (if (stringp re) s re)))))\n"
            "(defun match-beginning (n) (nlre-match-beginning n))\n"
            "(defun match-end (n) (nlre-match-end n))\n"
            "(defun match-string (n &optional str)\n"
            "  (let ((b (nlre-match-beginning n)) (e (nlre-match-end n)))\n"
            "    (if (and str b e) (substring str b e) nil)))\n"
            "(defun split-string (s &optional sep omit trim) (nlre-split-string s sep omit))\n"
            "(defun replace-regexp-in-string (re rep s &optional fc lit subexp start)\n"
            "  (nlre-replace-regexp-in-string re rep s))\n")
    (buffer-string)))

(defun nelisp-standalone--reader-repl-prelude-forms (fbuf src cursor result pool
                                                          out ctx builtin-sym)
  "Return Phase47 forms that prepare the standalone reader REPL environment."
  `((let* ((n (nl_repl_prelude_source ,fbuf 0)))
      (seq
       (ptr-write-u64 268436216 0 0)
       (nl_alloc_str ,fbuf n ,src)
       (nl_eval_source_all ,src ,cursor ,result ,pool ,out ,ctx ,builtin-sym)
       (ptr-write-u64 268436216 0 1)))))

(defun nelisp-standalone--reader-repl-eval-suffix ()
  "Return target-aware source appended to one standalone REPL input form.

The suffix reads the quit/throw flag at arena-base+8 to decide whether to print
the form's value.  Doc 140 Stage 8 put the arena at a RUNTIME mmap/VirtualAlloc
base on EVERY target (no baked fixed base), and runtime-PARSED REPL code cannot
use the compile-time `data-addr' primitive (it never reaches the chunk-arena
rewrite), so the flag address MUST come from `(car (nelisp--arena-stats))' (=
the live runtime base) on all targets.  The old windows/macos branch baked the
pre-Stage-8 fixed immediate (`nelisp-standalone--windows-arena-base' #x70000000
+ 8 = #x70000008), which after the rebase points at unmapped VA -> SIGSEGV in
the REPL print path's quit-flag check (the `--eval' command never runs this
suffix, which is why only `--repl' crashed)."
  "))) (if (= (ptr-read-u64 (+ (car (nelisp--arena-stats)) 8) 0) 0) (progn (nelisp--write-stdout-bytes (nelisp--repr v)) (nelisp--write-stdout-bytes (unibyte-string 10)) v) 0))\n")

(defconst nelisp-standalone--reader-boundary-source
  '(seq
    (defun nl_boundary_chunk_cursor (chunk)
      (ptr-read-u64 (nl_chunk_cursor_addr chunk) 0))
    (defun nl_boundary_chunk_used (cursor)
      (if (< cursor 1024) 0 (- cursor 1024)))
    (defun nl_boundary_reset_tail_chunks (chunk reclaimed)
      (if (= chunk 0)
          reclaimed
        (let* ((flags (ptr-read-u64 (+ chunk 40) 0))
               (next (ptr-read-u64 (+ chunk 48) 0)))
          ;; Doc 140 Stage 6: never reset a persistent (boot/global) chunk's
          ;; cursor — only temporary per-form scratch chunks are reclaimed.
          (if (= (logand flags 2) 2)
              (nl_boundary_reset_tail_chunks next reclaimed)
            (let* ((cursor-addr (nl_chunk_cursor_addr chunk))
                   (cursor (ptr-read-u64 cursor-addr 0))
                   (used (nl_boundary_chunk_used cursor)))
              (seq
               (ptr-write-u64 cursor-addr 0 1024)
               (nl_boundary_reset_tail_chunks next (+ reclaimed used))))))))
    (defun nl_boundary_immediate_result_p (out)
      (if (<= (ptr-read-u64 out 0) 3) 1 0))
    (defun nl_boundary_reclaim (mark_chunk mark_cursor)
      (let* ((cursor-addr (nl_chunk_cursor_addr mark_chunk))
             (cursor (ptr-read-u64 cursor-addr 0))
             (head-reclaimed (if (< mark_cursor cursor)
                                 (- cursor mark_cursor)
                               0))
             (tail-reclaimed
              (nl_boundary_reset_tail_chunks (ptr-read-u64 (+ mark_chunk 48) 0) 0)))
        (seq
         (ptr-write-u64 cursor-addr 0 mark_cursor)
         (ptr-write-u64 268436168 0 mark_chunk)
         (ptr-write-u64 268435552 0 0)
         (ptr-write-u64 268436200 0
                        (+ (ptr-read-u64 268436200 0)
                           (+ head-reclaimed tail-reclaimed)))
         0)))
    (defun nl_boundary_maybe_reclaim (mark_chunk mark_cursor epoch0 out)
      (if (= (ptr-read-u64 268436216 0) 1)
          (if (= (nl_boundary_immediate_result_p out) 1)
              (if (= (ptr-read-u64 268435544 0) epoch0)
                  (if (= (ptr-read-u64 268435472 0) 0)
                      (if (= (ptr-read-u64 268435464 0) 0)
                          (nl_boundary_reclaim mark_chunk mark_cursor)
                        0)
                    0)
                0)
            0)
        0)))
  "Reader top-level boundary reclamation helpers for Doc 140 Stage 5.")

(defconst nelisp-standalone--reader-eval-source-source
  '(defun nl_eval_source_all (src cursor result pool out ctx builtin_sym)
     (seq
      (ptr-write-u64 cursor 0 2) (ptr-write-u64 cursor 8 0)
      (ptr-write-u64 out 0 0) (ptr-write-u64 out 8 0)
      (let* ((more 1))
        (while (= more 1)
          (seq
           (ptr-write-u64 result 0 0) (ptr-write-u64 result 8 0)
           (let* ((mark_chunk (ptr-read-u64 268436168 0))
                  (mark_cursor (nl_boundary_chunk_cursor mark_chunk))
                  (epoch0 (ptr-read-u64 268435544 0))
                  (prc (nelisp_reader_parse_one src cursor result pool 0)))
             (if (= prc 1)
                 (seq (ptr-write-u64 out 0 0) (ptr-write-u64 out 8 0)
                      (nl_driver_eval_published result ctx out pool src cursor builtin_sym)
                      (nl_boundary_maybe_reclaim mark_chunk mark_cursor epoch0 out)
                      ;; GC trigger on TOTAL chunk-bytes-reserved (268436184),
                      ;; not the chunk-0 bump offset.  See `bf_load_eval_loop'.
                      (if (< (ptr-read-u64 268436184 0) (ptr-read-u64 268435560 0))
                          0
                        (let* ((live (nl_gc_collect_form_boundary ctx result out pool src cursor builtin_sym))
                               (bump (ptr-read-u64 268436184 0))
                               (lo (+ (* live 3) 1048576))
                               (hi (+ bump 16777216)))
                          (ptr-write-u64 268435560 0 (if (< lo hi) hi lo))))
                      (if (= (ptr-read-u64 268435464 0) 0)
                          0
                        (setq more 0)))
               (setq more 0))))))
      0))
  "Reader source parse/eval loop split out of the always-recompiled driver.")

(defun nelisp-standalone--runtime-image-command-src ()
  "Return embedded source implementing standalone-reader runtime-image commands."
  (concat
   (nelisp-standalone--artifact-command-src)
   "\n"
   (with-temp-buffer
     (insert-file-contents
      (expand-file-name "lisp/nelisp-runtime-image.el"
                        nelisp-standalone--repo-root))
     (buffer-string))
   "\n"
   "(cond\n"
   " ((string= (car nelisp-standalone-argv) \"dump-runtime-image\")\n"
   "  (nelisp-runtime-image-dump-cli nelisp-standalone-argv))\n"
   " ((string= (car nelisp-standalone-argv) \"extend-runtime-image\")\n"
   "  (nelisp-runtime-image-extend-cli nelisp-standalone-argv))\n"
   " ((string= (car nelisp-standalone-argv) \"eval-runtime-image\")\n"
   "  (nelisp-runtime-image-eval-cli nelisp-standalone-argv t))\n"
   " ((string= (car nelisp-standalone-argv) \"exec-runtime-image\")\n"
   "  (nelisp-runtime-image-eval-cli nelisp-standalone-argv nil))\n"
   " (t 2))\n"))

(defun nelisp-standalone--artifact-command-dispatch-src ()
  "Return standalone-reader artifact command dispatch source."
  (concat
   "(cond\n"
   " ((string= (car nelisp-standalone-argv) \"compile-elisp-artifact\")\n"
   "  (compile-elisp-artifact nelisp-standalone-argv))\n"
   " ((string= (car nelisp-standalone-argv) \"compile-elisp-artifacts\")\n"
   "  (compile-elisp-artifacts nelisp-standalone-argv))\n"
   " ((string= (car nelisp-standalone-argv) \"compile-runtime-image\")\n"
   "  (compile-runtime-image nelisp-standalone-argv))\n"
   " ((string= (car nelisp-standalone-argv) \"audit-elisp-artifacts\")\n"
   "  (audit-elisp-artifacts nelisp-standalone-argv))\n"
   " ((string= (car nelisp-standalone-argv) \"exec-elisp-artifact\")\n"
   "  (exec-elisp-artifact nelisp-standalone-argv))\n"
   " ((string= (car nelisp-standalone-argv) \"eval-elisp-artifact\")\n"
   "  (eval-elisp-artifact nelisp-standalone-argv))\n"
   " ((string= (car nelisp-standalone-argv) \"load-elisp-source\")\n"
   "  (load-elisp-source nelisp-standalone-argv))\n"
   " ((string= (car nelisp-standalone-argv) \"eval-elisp-source\")\n"
   "  (eval-elisp-source nelisp-standalone-argv))\n"
   " ((string= (car nelisp-standalone-argv) \"native-exec-elisp-artifact\")\n"
   "  (native-exec-elisp-artifact nelisp-standalone-argv))\n"
   " ((string= (car nelisp-standalone-argv) \"inspect-elisp-artifact\")\n"
   "  (inspect-elisp-artifact nelisp-standalone-argv))\n"
   " (t 2))\n"))

(defun nelisp-standalone--artifact-command-cache-dispatch-src (&optional cache-path)
  "Return artifact command dispatch source for replayed runtime caches.
Cached runtime functions live in `nelisp--functions' as BCL/interpreter
closures, so dispatch must call through `nelisp--apply' instead of relying on
host `fboundp' cells."
  (concat
   "(defun nelisp-standalone-artifact-cache-call (sym)\n"
   "  (let ((fn (gethash sym nelisp--functions nelisp--unbound)))\n"
   "    (if (eq fn nelisp--unbound)\n"
   "        (error \"artifact runtime cache missing function: %S\" sym)\n"
   "      (nelisp--apply fn (list nelisp-standalone-argv)))))\n"
	   (when cache-path
	     (let ((compiler-plist (progn
	                             (require 'nelisp-artifact)
	                             (nelisp-artifact--compiler-plist)))
		   (artifact-magic (progn
				     (require 'nelisp-artifact)
				     nelisp-artifact--magic))
		   (artifact-format (progn
				      (require 'nelisp-artifact)
				      nelisp-artifact--format)))
		       (format (concat
		              "(nelisp-standalone-artifact-cache-load %S)\n"
		              "(fset 'nelisp-artifact--compiler-plist\n"
		              "      (lambda () '%S))\n"
		              "(puthash 'nelisp-artifact--compiler-plist\n"
		              "         (symbol-function 'nelisp-artifact--compiler-plist)\n"
		              "         nelisp--functions)\n"
		              "(fset 'nelisp-artifact--file-size\n"
		              "      (lambda (path)\n"
		              "        (let ((attrs (file-attributes path)))\n"
		              "          (if (fboundp 'file-attribute-size)\n"
		              "              (file-attribute-size attrs)\n"
		              "            (nth 7 attrs)))))\n"
		              "(puthash 'nelisp-artifact--file-size\n"
		              "         (symbol-function 'nelisp-artifact--file-size)\n"
		              "         nelisp--functions)\n"
		              "(fset 'nelisp-artifact--file-mtime\n"
		              "      (lambda (path)\n"
		              "        (let ((attrs (file-attributes path)))\n"
		              "          (if (fboundp 'file-attribute-modification-time)\n"
		              "              (file-attribute-modification-time attrs)\n"
		              "            (nth 5 attrs)))))\n"
		              "(puthash 'nelisp-artifact--file-mtime\n"
		              "         (symbol-function 'nelisp-artifact--file-mtime)\n"
		              "         nelisp--functions)\n"
		              "(fset 'nelisp-artifact--file-ctime\n"
		              "      (lambda (path)\n"
		              "        (let ((attrs (file-attributes path)))\n"
		              "          (if (fboundp 'file-attribute-status-change-time)\n"
		              "              (file-attribute-status-change-time attrs)\n"
		              "            (nth 6 attrs)))))\n"
		              "(puthash 'nelisp-artifact--file-ctime\n"
		              "         (symbol-function 'nelisp-artifact--file-ctime)\n"
		              "         nelisp--functions)\n"
		              "(fset 'nelisp-artifact--sibling-manifest-path\n"
		              "      (lambda (artifact-path) (concat artifact-path \".manifest.el\")))\n"
		              "(puthash 'nelisp-artifact--sibling-manifest-path\n"
		              "         (symbol-function 'nelisp-artifact--sibling-manifest-path)\n"
		              "         nelisp--functions)\n"
		              "(puthash 'nelisp-artifact--read-file-as-string\n"
		              "         (symbol-function 'nelisp-standalone-artifact-cache--read-file)\n"
		              "         nelisp--functions)\n"
		              "(fset 'nelisp-artifact--read-file-as-string\n"
	              "      (symbol-function 'nelisp-standalone-artifact-cache--read-file))\n"
	              "(setq nelisp-artifact-fast-private-read nil)\n"
	              "(puthash 'nelisp-artifact-fast-private-read nil nelisp--globals)\n"
	              "(setq nelisp-artifact-native-dispatch-enabled nil)\n"
	              "(puthash 'nelisp-artifact-native-dispatch-enabled nil nelisp--globals)\n"
              "(defun nelisp-standalone-artifact-cache--read-all-from-string (source)\n"
              "  (let ((pos 0) (len (length source)) (forms nil) res)\n"
              "    (while (progn\n"
              "             (setq pos (nelisp-read--skip-ws source pos))\n"
              "             (< pos len))\n"
              "      (setq res (read-from-string source pos))\n"
              "      (push (car res) forms)\n"
              "      (setq pos (cdr res)))\n"
              "    (nreverse forms)))\n"
			              "(fset 'nelisp-artifact--read-all-from-string\n"
			              "      (symbol-function 'nelisp-standalone-artifact-cache--read-all-from-string))\n"
				              "(puthash 'nelisp-artifact--read-all-from-string\n"
				              "         (symbol-function 'nelisp-standalone-artifact-cache--read-all-from-string)\n"
				              "         nelisp--functions)\n"
					      "(fset 'nelisp-artifact--parse-payload\n"
					      "      (lambda (content artifact-path)\n"
					      "        (let* ((magic %S)\n"
					      "               (prefix-len (length magic)))\n"
					      "          (unless (and (>= (length content) prefix-len)\n"
					      "                       (equal (substring content 0 prefix-len) magic))\n"
					      "            (signal 'nelisp-artifact-invalid\n"
					      "                    (list \"invalid .nelc magic header\" artifact-path)))\n"
					      "          (let ((payload (car (read-from-string\n"
					      "                               (substring content prefix-len)))))\n"
					      "            (unless (eq (plist-get payload :format) '%S)\n"
					      "              (signal 'nelisp-artifact-invalid\n"
					      "                      (list \"unsupported .nelc format\"\n"
					      "                            (plist-get payload :format) artifact-path)))\n"
					      "            payload))))\n"
					      "(puthash 'nelisp-artifact--parse-payload\n"
					      "         (symbol-function 'nelisp-artifact--parse-payload)\n"
					      "         nelisp--functions)\n"
					      "(fset 'nelisp-artifact--read-manifest-full\n"
					      "      (lambda (artifact-path)\n"
					      "        (car (read-from-string\n"
					      "              (nelisp-standalone-artifact-cache--read-file\n"
					      "               (concat artifact-path \".manifest.el\"))))))\n"
					      "(puthash 'nelisp-artifact--read-manifest-full\n"
					      "         (symbol-function 'nelisp-artifact--read-manifest-full)\n"
					      "         nelisp--functions)\n"
					      "(fset 'nelisp-artifact--read-manifest-for-load\n"
					      "      (symbol-function 'nelisp-artifact--read-manifest-full))\n"
					      "(puthash 'nelisp-artifact--read-manifest-for-load\n"
					      "         (symbol-function 'nelisp-artifact--read-manifest-for-load)\n"
					      "         nelisp--functions)\n"
					      "(fset 'nelisp-artifact--validate-input-record\n"
					      "      (lambda (_rec _label _artifact-path) nil))\n"
					      "(puthash 'nelisp-artifact--validate-input-record\n"
					      "         (symbol-function 'nelisp-artifact--validate-input-record)\n"
					      "         nelisp--functions)\n"
					      "(fset 'nelisp-artifact--validate\n"
					      "      (lambda (artifact-path _artifact-content)\n"
					      "        (nelisp-artifact--read-manifest-for-load artifact-path)))\n"
					      "(puthash 'nelisp-artifact--validate\n"
					      "         (symbol-function 'nelisp-artifact--validate)\n"
					      "         nelisp--functions)\n"
					      "(fset 'nelisp-artifact-load-file\n"
					      "      (lambda (artifact-path)\n"
					      "        (let* ((content (nelisp-artifact--read-file-as-string artifact-path))\n"
					      "               (manifest (nelisp-artifact--validate artifact-path content)))\n"
					      "          (if (fboundp 'nelisp-artifact--load-private-fast)\n"
					      "              (nelisp-artifact--load-private-fast artifact-path content manifest)\n"
					      "            (let* ((payload (nelisp-artifact--parse-payload content artifact-path))\n"
					      "                   (module (plist-get payload :module-init))\n"
					      "                   (features (plist-get payload :features))\n"
					      "                   (last nil))\n"
					      "              (while module\n"
					      "                (setq last (nelisp-artifact--replay-module-item (car module)))\n"
					      "                (setq module (cdr module)))\n"
					      "              (while features\n"
					      "                (let ((feature (car features)))\n"
					      "                  (when (fboundp 'nelisp-provide)\n"
					      "                    (nelisp-provide feature))\n"
					      "                  (unless (featurep feature)\n"
					      "                    (provide feature)))\n"
					      "                (setq features (cdr features)))\n"
					      "              last)))))\n"
					      "(puthash 'nelisp-artifact-load-file\n"
					      "         (symbol-function 'nelisp-artifact-load-file)\n"
					      "         nelisp--functions)\n"
					      "(let ((fn\n"
					      "       (lambda (args)\n"
					      "        (let ((rest (cdr args))\n"
					      "              (source nil)\n"
					      "              (value nil)\n"
					      "              artifact)\n"
					      "          (while (and rest (null source))\n"
					      "            (let ((flag (car rest)))\n"
					      "              (if (equal flag \"--auto-compile\")\n"
					      "                  (setq rest (cdr rest))\n"
					      "                (if (or (equal flag \"--kind\")\n"
					      "                        (equal flag \"--target\")\n"
					      "                        (equal flag \"--load-path\")\n"
					      "                        (equal flag \"--preload\")\n"
					      "                        (equal flag \"--native-policy\"))\n"
					      "                    (setq rest (cdr (cdr rest)))\n"
					      "                  (setq source flag)\n"
					      "                  (setq rest nil)))))\n"
					      "          (unless source\n"
					      "            (error \"source command requires FILE.el\"))\n"
					      "          (setq artifact\n"
					      "                (if (file-exists-p (concat source \".neln\"))\n"
					      "                    (concat source \".neln\")\n"
					      "                  (if (file-exists-p (concat source \".nelc\"))\n"
					      "                      (concat source \".nelc\")\n"
					      "                    nil)))\n"
					      "          (setq value\n"
					      "                (if artifact\n"
					      "                    (nelisp-artifact-load-file artifact)\n"
					      "                  (nelisp-load-file source)))\n"
					      "          (nelisp--write-stdout-bytes (prin1-to-string value))\n"
					      "          (nelisp--write-stdout-bytes \"\\n\")\n"
					      "          0))))\n"
					      "  (fset 'nelisp-standalone-cache-load-elisp-source-fast fn)\n"
					      "  (fset 'load-elisp-source fn)\n"
					      "  (puthash 'load-elisp-source fn nelisp--functions))\n"
					      "(let ((fn\n"
					      "       (lambda (args)\n"
					      "        (let ((rest (cdr args))\n"
					      "              (source nil)\n"
					      "              (forms nil)\n"
					      "              (last nil)\n"
					      "              artifact)\n"
					      "          (while (and rest (null source))\n"
					      "            (let ((flag (car rest)))\n"
					      "              (if (equal flag \"--auto-compile\")\n"
					      "                  (setq rest (cdr rest))\n"
					      "                (if (or (equal flag \"--kind\")\n"
					      "                        (equal flag \"--target\")\n"
					      "                        (equal flag \"--load-path\")\n"
					      "                        (equal flag \"--preload\")\n"
					      "                        (equal flag \"--native-policy\"))\n"
					      "                    (setq rest (cdr (cdr rest)))\n"
					      "                  (setq source flag)\n"
					      "                  (setq forms (cdr rest))\n"
					      "                  (setq rest nil)))))\n"
					      "          (unless source\n"
					      "            (error \"source command requires FILE.el\"))\n"
					      "          (unless forms\n"
					      "            (error \"eval-elisp-source requires at least one FORM\"))\n"
					      "          (setq artifact\n"
					      "                (if (file-exists-p (concat source \".neln\"))\n"
					      "                    (concat source \".neln\")\n"
					      "                  (if (file-exists-p (concat source \".nelc\"))\n"
					      "                      (concat source \".nelc\")\n"
					      "                    nil)))\n"
					      "          (if artifact\n"
					      "              (nelisp-artifact-load-file artifact)\n"
					      "            (nelisp-load-file source))\n"
					      "          (while forms\n"
					      "            (setq last (nelisp-eval (car (read-from-string (car forms)))))\n"
					      "            (setq forms (cdr forms)))\n"
					      "          (nelisp--write-stdout-bytes (prin1-to-string last))\n"
					      "          (nelisp--write-stdout-bytes \"\\n\")\n"
					      "          0))))\n"
					      "  (fset 'nelisp-standalone-cache-eval-elisp-source-fast fn)\n"
					      "  (fset 'eval-elisp-source fn)\n"
					      "  (puthash 'eval-elisp-source fn nelisp--functions))\n")
			             cache-path compiler-plist artifact-magic
				     artifact-format)))
	   "(if (string= (car nelisp-standalone-argv) \"compile-elisp-artifact\")\n"
   "    (nelisp-standalone-artifact-cache-call 'compile-elisp-artifact)\n"
   "  (if (string= (car nelisp-standalone-argv) \"compile-elisp-artifacts\")\n"
   "      (nelisp-standalone-artifact-cache-call 'compile-elisp-artifacts)\n"
   "    (if (string= (car nelisp-standalone-argv) \"compile-runtime-image\")\n"
   "        (nelisp-standalone-artifact-cache-call 'compile-runtime-image)\n"
   "      (if (string= (car nelisp-standalone-argv) \"audit-elisp-artifacts\")\n"
   "          (nelisp-standalone-artifact-cache-call 'audit-elisp-artifacts)\n"
   "        (if (string= (car nelisp-standalone-argv) \"exec-elisp-artifact\")\n"
   "            (nelisp-standalone-artifact-cache-call 'exec-elisp-artifact)\n"
   "          (if (string= (car nelisp-standalone-argv) \"eval-elisp-artifact\")\n"
   "              (nelisp-standalone-artifact-cache-call 'eval-elisp-artifact)\n"
   "            (if (string= (car nelisp-standalone-argv) \"load-elisp-source\")\n"
   "                (nelisp-standalone-cache-load-elisp-source-fast nelisp-standalone-argv)\n"
   "              (if (string= (car nelisp-standalone-argv) \"eval-elisp-source\")\n"
   "                  (nelisp-standalone-cache-eval-elisp-source-fast nelisp-standalone-argv)\n"
   "                (if (string= (car nelisp-standalone-argv) \"native-exec-elisp-artifact\")\n"
   "                    (nelisp-standalone-artifact-cache-call 'native-exec-elisp-artifact)\n"
   "                  (if (string= (car nelisp-standalone-argv) \"inspect-elisp-artifact\")\n"
   "                      (nelisp-standalone-artifact-cache-call 'inspect-elisp-artifact)\n"
   "                    2))))))))))\n"
   "nil\n"))

(defun nelisp-standalone--artifact-runtime-file-src (relative-path inline)
  "Return runtime source for RELATIVE-PATH.
When INLINE is non-nil, embed the file contents directly so a compiled command
runtime cache does not replay source file loads on every command invocation."
  (let ((path (expand-file-name relative-path nelisp-standalone--repo-root)))
    (if inline
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (buffer-string))
      (format "(load %S)\n" path))))

(defun nelisp-standalone--artifact-command-runtime-src (&optional inline)
  "Return source that defines standalone-reader artifact command runtime."
  (concat
   (nelisp-standalone--artifact-runtime-file-src
    "scripts/nelisp-stdlib-prelude.el" inline)
   (nelisp-standalone--artifact-runtime-file-src
    "src/nelisp-read.el" inline)
   (nelisp-standalone--artifact-runtime-file-src
    "src/nelisp-eval.el" inline)
   (nelisp-standalone--artifact-runtime-file-src
    "src/nelisp-macro.el" inline)
   (nelisp-standalone--artifact-runtime-file-src
    "src/nelisp-load.el" inline)
   (nelisp-standalone--artifact-runtime-file-src
    "src/nelisp-bytecode.el" inline)
   "(defvar features nil)\n"
   "(fset 'provide\n"
   "      (lambda (feature)\n"
   "        (unless (memq feature features)\n"
   "          (setq features (cons feature features)))\n"
   "        feature))\n"
   "(fset 'featurep\n"
   "      (lambda (feature) (if (memq feature features) t nil)))\n"
   "(provide 'cl-lib)\n"
   "(provide 'nelisp-read)\n"
   "(provide 'nelisp-eval)\n"
   "(provide 'nelisp-macro)\n"
   "(provide 'nelisp-core-fileio)\n"
   "(provide 'nelisp-load)\n"
   "(provide 'nelisp-bytecode)\n"
   "(unless (fboundp 'nelisp-call-process)\n"
   "  (defun nelisp-call-process (program &optional infile destination display &rest args)\n"
   "    (apply 'nelisp-process-call-process program infile destination display args)))\n"
   "(unless (fboundp 'safe-length)\n"
   "  (defun safe-length (list) (if (listp list) (length list) 0)))\n"
   "(unless (fboundp 'proper-list-p)\n"
   "  (defun proper-list-p (object)\n"
   "    (let ((tail object) (ok nil) (done nil))\n"
   "      (while (not done)\n"
   "        (cond ((null tail) (setq ok t) (setq done t))\n"
   "              ((consp tail) (setq tail (cdr tail)))\n"
   "              (t (setq ok nil) (setq done t))))\n"
   "      ok)))\n"
   "(defun nelisp-standalone-artifact--contains (needle haystack)\n"
   "  (let ((i 0) (n (length needle)) (h (length haystack)) (found nil))\n"
   "    (while (and (not found) (<= (+ i n) h))\n"
   "      (when (equal (substring haystack i (+ i n)) needle)\n"
   "        (setq found t))\n"
   "      (setq i (1+ i)))\n"
   "    found))\n"
   "(defun nelisp-standalone-artifact--all-hex-p (s)\n"
   "  (let ((i 0) (n (length s)) (ok (> (length s) 0)) c)\n"
   "    (while (and ok (< i n))\n"
   "      (setq c (aref s i))\n"
   "      (unless (or (and (>= c ?0) (<= c ?9))\n"
   "                  (and (>= c ?a) (<= c ?f))\n"
   "                  (and (>= c ?A) (<= c ?F)))\n"
   "        (setq ok nil))\n"
   "      (setq i (1+ i)))\n"
   "    ok))\n"
   "(defun nelisp-standalone-artifact--number-token-p (s)\n"
   "  (let ((i 0) (n (length s)) (digits 0) (ok (> (length s) 0)) c)\n"
   "    (while (and ok (< i n))\n"
   "      (setq c (aref s i))\n"
   "      (cond ((and (>= c ?0) (<= c ?9)) (setq digits (1+ digits)))\n"
   "            ((or (= c ?+) (= c ?-) (= c ?.) (= c ?e) (= c ?E)) nil)\n"
   "            (t (setq ok nil)))\n"
   "      (setq i (1+ i)))\n"
   "    (and ok (> digits 0))))\n"
   "(unless (fboundp 'string-match-p)\n"
   "  (defun string-match-p (regexp string &optional _start)\n"
   "    (cond\n"
   "     ((equal regexp \"[.eE]\")\n"
   "      (or (nelisp-standalone-artifact--contains \".\" string)\n"
   "          (nelisp-standalone-artifact--contains \"e\" string)\n"
   "          (nelisp-standalone-artifact--contains \"E\" string)))\n"
   "     ((equal regexp \"x86_64\\\\|amd64\")\n"
   "      (or (nelisp-standalone-artifact--contains \"x86_64\" string)\n"
   "          (nelisp-standalone-artifact--contains \"amd64\" string)))\n"
   "     ((equal regexp \"aarch64\\\\|arm64\")\n"
   "      (or (nelisp-standalone-artifact--contains \"aarch64\" string)\n"
   "          (nelisp-standalone-artifact--contains \"arm64\" string)))\n"
   "     ((equal regexp \"\\\\`[0-9a-fA-F]+\\\\'\")\n"
   "      (nelisp-standalone-artifact--all-hex-p string))\n"
   "     ((nelisp-standalone-artifact--contains \"[0-9]\" regexp)\n"
   "      (nelisp-standalone-artifact--number-token-p string))\n"
   "     (t (nelisp-standalone-artifact--contains regexp string)))))\n"
   "(unless (fboundp 'rename-file)\n"
   "  (defun rename-file (oldname newname &optional ok-if-already-exists)\n"
   "    (when (and (not ok-if-already-exists) (file-exists-p newname))\n"
   "      (error \"rename-file: target exists: %s\" newname))\n"
   "    (let ((rc (nelisp--syscall-path2 82 oldname newname)))\n"
   "      (unless (= rc 0)\n"
   "        (error \"rename-file: rc=%S old=%s new=%s\" rc oldname newname)))\n"
   "    nil))\n"
   "(unless (fboundp 'file-attributes)\n"
   "  (defun file-attributes (filename &optional _id-format)\n"
   "    (if (not (file-exists-p filename))\n"
   "        nil\n"
   "      (let ((size (nelisp--syscall-stat-field filename 48))\n"
   "            (mtime (nelisp--syscall-stat-field filename 88)))\n"
   "        (list nil 1 0 0 0 mtime 0 size \"\" nil nil nil)))))\n"
   "(unless (fboundp 'file-attribute-size)\n"
   "  (defun file-attribute-size (attrs) (nth 7 attrs)))\n"
   "(unless (fboundp 'file-attribute-modification-time)\n"
   "  (defun file-attribute-modification-time (attrs) (nth 5 attrs)))\n"
   "(unless (fboundp 'file-truename)\n"
   "  (defun file-truename (filename) (expand-file-name filename)))\n"
   "(unless (fboundp 'emacs-pid)\n"
   "  (defun emacs-pid () 0))\n"
   "(defvar nelisp-artifact--standalone-random-state 0)\n"
   "(unless (fboundp 'random)\n"
   "  (defun random (&optional limit)\n"
   "    (setq nelisp-artifact--standalone-random-state\n"
   "          (mod (+ (* nelisp-artifact--standalone-random-state 1103515245) 12345) 2147483648))\n"
   "    (if (and limit (> limit 0))\n"
   "        (mod nelisp-artifact--standalone-random-state limit)\n"
   "      nelisp-artifact--standalone-random-state)))\n"
   "(defun nelisp-standalone-artifact--read-file-as-string (path)\n"
   "  (or (nelisp--syscall-read-file path) \"\"))\n"
   "(unless (fboundp 'nelisp-core-expand-file-name)\n"
   "  (defun nelisp-core-expand-file-name (name &optional default-directory)\n"
   "    (expand-file-name name default-directory)))\n"
   "(unless (fboundp 'nelisp-core-file-readable-p)\n"
   "  (defun nelisp-core-file-readable-p (path) (file-readable-p path)))\n"
   "(unless (fboundp 'nelisp-core-read-file-as-string)\n"
   "  (defun nelisp-core-read-file-as-string (path)\n"
   "    (nelisp-standalone-artifact--read-file-as-string path)))\n"
   "(unless (fboundp 'secure-hash)\n"
   "  (defun secure-hash (algorithm object &optional _start _end _binary)\n"
   "    (unless (or (eq algorithm 'sha256) (equal algorithm \"sha256\"))\n"
   "      (error \"secure-hash: standalone fallback supports sha256 only: %S\" algorithm))\n"
   "    (unless (stringp object)\n"
   "      (signal 'wrong-type-argument (list 'stringp object)))\n"
   "    (let* ((program (cond ((file-exists-p \"/usr/bin/sha256sum\") \"/usr/bin/sha256sum\")\n"
   "                          ((file-exists-p \"/bin/sha256sum\") \"/bin/sha256sum\")\n"
   "                          (t \"sha256sum\")))\n"
   "           (in (make-temp-file \"nelisp-secure-hash-in-\"))\n"
   "           (out (make-temp-file \"nelisp-secure-hash-out-\"))\n"
   "           (rc nil)\n"
   "           (line nil))\n"
   "      (unwind-protect\n"
   "          (progn\n"
   "            (write-region object nil in)\n"
   "            (setq rc (nelisp-call-process program nil out nil in))\n"
   "            (unless (= rc 0)\n"
   "              (error \"secure-hash: sha256sum exited %S\" rc))\n"
   "            (setq line (nelisp-standalone-artifact--read-file-as-string out))\n"
   "            (unless (and (stringp line) (>= (length line) 64))\n"
   "              (error \"secure-hash: malformed sha256sum output\"))\n"
   "            (substring line 0 64))\n"
   "        (ignore-errors (delete-file in))\n"
   "        (ignore-errors (delete-file out))))))\n"
   (nelisp-standalone--artifact-runtime-file-src
    "lisp/nelisp-artifact.el" inline)
	   (format "(setq nelisp-artifact-standalone-repo-root %S)\n"
	           nelisp-standalone--repo-root)
	   "(fset 'nelisp-artifact--read-file-as-string\n"
	   "      (symbol-function 'nelisp-standalone-artifact--read-file-as-string))\n"
   "(defun nelisp-standalone-artifact--read-all-from-string (source)\n"
   "  (let ((pos 0) (len (length source)) (forms nil) res)\n"
   "    (while (progn\n"
   "             (setq pos (nelisp-read--skip-ws source pos))\n"
   "             (< pos len))\n"
   "      (setq res (read-from-string source pos))\n"
   "      (push (car res) forms)\n"
   "      (setq pos (cdr res)))\n"
   "    (nreverse forms)))\n"
   "(fset 'nelisp-artifact--read-all-from-string\n"
   "      (symbol-function 'nelisp-standalone-artifact--read-all-from-string))\n"
   ""))

(defun nelisp-standalone--artifact-command-src ()
  "Return embedded source implementing standalone-reader artifact commands."
  (concat
   (nelisp-standalone--artifact-command-runtime-src)
   (nelisp-standalone--artifact-command-dispatch-src)))

(defun nelisp-standalone--artifact-command-cache-src ()
  "Return embedded source that replays the artifact command runtime cache.
The cache contains `nelisp-standalone--artifact-command-runtime-src' compiled
to a private `.nelc' module.  This bootstrap deliberately loads only the
minimum evaluator/bytecode substrate needed to replay that module, then runs the
same artifact command dispatch used by the full source path."
  (concat
   (format "(load %S)\n"
           (expand-file-name "scripts/nelisp-stdlib-prelude.el"
                             nelisp-standalone--repo-root))
   (format "(load %S)\n"
           (expand-file-name "src/nelisp-read.el"
                             nelisp-standalone--repo-root))
   (format "(load %S)\n"
           (expand-file-name "src/nelisp-eval.el"
                             nelisp-standalone--repo-root))
   (format "(load %S)\n"
           (expand-file-name "src/nelisp-macro.el"
                             nelisp-standalone--repo-root))
   (format "(load %S)\n"
           (expand-file-name "src/nelisp-load.el"
                             nelisp-standalone--repo-root))
   (format "(load %S)\n"
           (expand-file-name "src/nelisp-bytecode.el"
                             nelisp-standalone--repo-root))
   "(defvar features nil)\n"
   "(fset 'provide\n"
   "      (lambda (feature)\n"
   "        (unless (memq feature features)\n"
   "          (setq features (cons feature features)))\n"
   "        feature))\n"
   "(fset 'featurep\n"
   "      (lambda (feature) (if (memq feature features) t nil)))\n"
   "(provide 'cl-lib)\n"
   "(provide 'nelisp-read)\n"
   "(provide 'nelisp-eval)\n"
   "(provide 'nelisp-macro)\n"
   "(provide 'nelisp-core-fileio)\n"
   "(provide 'nelisp-load)\n"
   "(provide 'nelisp-bytecode)\n"
	   "(defun nelisp-standalone-artifact-cache--read-file (path)\n"
	   "  (or (nelisp--syscall-read-file path) \"\"))\n"
   "(defun nelisp-standalone-artifact-cache--find (needle haystack &optional start)\n"
   "  (let* ((i (or start 0)) (n (length needle)) (h (length haystack))\n"
   "         (limit (- h n)) (found nil))\n"
   "    (while (and (not found) (<= i limit))\n"
   "      (let ((j 0) (ok t))\n"
   "        (while (and ok (< j n))\n"
   "          (unless (= (aref needle j) (aref haystack (+ i j)))\n"
   "            (setq ok nil))\n"
   "          (setq j (1+ j)))\n"
   "        (if ok (setq found i) (setq i (1+ i)))))\n"
   "    found))\n"
	   "(defun nelisp-standalone-artifact-cache--skip-ws (source pos)\n"
	   "  (let ((len (length source)) ch)\n"
	   "    (while (< pos len)\n"
	   "      (setq ch (aref source pos))\n"
	   "      (if (if (= ch 32) t\n"
	   "            (if (= ch 9) t\n"
	   "              (if (= ch 10) t\n"
	   "                (if (= ch 13) t\n"
	   "                  (if (= ch 12) t nil)))))\n"
	   "          (setq pos (1+ pos))\n"
	   "        (setq len 0)))\n"
	   "    pos))\n"
   "(defun nelisp-standalone-artifact-cache--key-pos (source key label &optional missing-ok start)\n"
   "  (let* ((needle (concat (symbol-name key) \" \"))\n"
   "         (pos (nelisp-standalone-artifact-cache--find needle source start)))\n"
   "    (if (null pos)\n"
   "        (if missing-ok :missing (error \"missing key %S in %s\" key label))\n"
   "      (nelisp-standalone-artifact-cache--skip-ws source (+ pos (length needle))))))\n"
   "(defun nelisp-standalone-artifact-cache--key (source key label &optional missing-ok start)\n"
   "  (let ((value-pos (nelisp-standalone-artifact-cache--key-pos\n"
   "                    source key label missing-ok start)))\n"
   "    (if (eq value-pos :missing)\n"
   "        :missing\n"
   "      (let* ((value-pos (nelisp-standalone-artifact-cache--skip-ws source value-pos))\n"
   "             (res (nelisp-read--sexp source value-pos)))\n"
   "        (unless res (error \"invalid value for %S in %s\" key label))\n"
   "        (car res)))))\n"
   "(defun nelisp-standalone-artifact-cache--content (artifact)\n"
   "  (let* ((content (nelisp-standalone-artifact-cache--read-file artifact))\n"
   "         (magic \";;; nelisp-private-nelc-v2\\n\")\n"
   "         (prefix-len (length magic)))\n"
   "    (unless (and (>= (length content) prefix-len)\n"
   "                 (equal (substring content 0 prefix-len) magic))\n"
   "      (error \"invalid artifact runtime cache: %s\" artifact))\n"
   "    (substring content prefix-len)))\n"
	   "(defun nelisp-standalone-artifact-cache--install-fn (name fn)\n"
	   "  (puthash name fn nelisp--functions)\n"
	   "  (fset name\n"
	   "        (list 'lambda '(&rest args)\n"
	   "              (list 'nelisp--apply\n"
	   "                    (list 'gethash (list 'quote name) 'nelisp--functions)\n"
	   "                    'args))))\n"
	   "(defun nelisp-standalone-artifact-cache--literal-value (form)\n"
	   "  (if (null form)\n"
	   "      nil\n"
	   "    (if (eq form t)\n"
	   "        t\n"
	   "      (if (numberp form)\n"
	   "          form\n"
	   "        (if (stringp form)\n"
	   "            form\n"
	   "          (if (keywordp form)\n"
	   "              form\n"
	   "            (if (consp form)\n"
	   "                (if (string= (symbol-name (car form)) \"quote\")\n"
	   "                    (nth 1 form)\n"
	   "                  (eval form))\n"
	   "              (eval form))))))))\n"
	   "(defun nelisp-standalone-artifact-cache--eval-form (form)\n"
	   "  (let* ((head (if (consp form) (car form) nil))\n"
	   "         (head-name (if (symbolp head) (symbol-name head) \"\")))\n"
	   "    (if (string= head-name \"defun\")\n"
	   "        (nelisp-eval form)\n"
	   "      (if (string= head-name \"defvar\")\n"
	   "          (let ((name (nth 1 form)))\n"
	   "            (puthash name t nelisp--specials)\n"
	   "            name)\n"
	   "        (if (string= head-name \"defconst\")\n"
	   "            (let ((name (nth 1 form)))\n"
	   "              (puthash name t nelisp--specials)\n"
	   "              (puthash name\n"
	   "                       (if (cdr (cdr form))\n"
	   "                           (nelisp-standalone-artifact-cache--literal-value (nth 2 form))\n"
	   "                         nil)\n"
	   "                       nelisp--globals)\n"
	   "              name)\n"
	   "          (eval form))))))\n"
	   "(defun nelisp-standalone-artifact-cache--replay-item (item)\n"
	   "  (cond\n"
	   "   ((and (consp item) (eq (car item) :fn))\n"
	   "    (nelisp-standalone-artifact-cache--install-fn (nth 1 item) (nth 2 item))\n"
	   "    (nth 1 item))\n"
	   "   ((and (consp item) (eq (car item) :eval))\n"
	   "    (nelisp-standalone-artifact-cache--eval-form (nth 1 item)))\n"
	   "   (t (nelisp-standalone-artifact-cache--eval-form item))))\n"
   "(defun nelisp-standalone-artifact-cache--item-end (source pos len artifact)\n"
   "  (let ((i pos) (depth 0) (in-string nil) (escape nil) (done nil) ch)\n"
   "    (while (and (not done) (< i len))\n"
   "      (setq ch (aref source i))\n"
   "      (cond\n"
   "       (in-string\n"
   "        (cond\n"
   "         (escape (setq escape nil))\n"
   "         ((= ch ?\\\\) (setq escape t))\n"
   "         ((= ch ?\\\") (setq in-string nil))))\n"
   "       ((= ch ?\\\") (setq in-string t))\n"
	   "       ((= ch 40) (setq depth (1+ depth)))\n"
	   "       ((= ch 41)\n"
   "        (setq depth (1- depth))\n"
   "        (when (= depth 0)\n"
   "          (setq done t))))\n"
   "      (setq i (1+ i)))\n"
	   "    (unless done\n"
	   "      (error \"unterminated :module-init item in %s\" artifact))\n"
	   "    i))\n"
	   "(defun nelisp-standalone-artifact-cache--item-start (source pos len)\n"
	   "  (let ((p pos)\n"
	   "        (limit (- pos 8))\n"
	   "        (found nil))\n"
	   "    (when (< limit 0)\n"
	   "      (setq limit 0))\n"
	   "    (when (>= p len)\n"
	   "      (setq p (1- len)))\n"
	   "    (while (if (>= p limit) (not found) nil)\n"
	   "      (if (= (aref source p) 40)\n"
	   "          (setq found p)\n"
	   "        (setq p (1- p))))\n"
	   "    (or found pos)))\n"
	   "(defun nelisp-standalone-artifact-cache--read-item (source pos len artifact)\n"
	   "  (let ((res (read-from-string source pos)))\n"
	   "    (unless res (error \"invalid :module-init item in %s\" artifact))\n"
	   "    res))\n"
	   "(defun nelisp-standalone-artifact-cache--fn-name-at (source pos len)\n"
	   "  (setq pos (nelisp-standalone-artifact-cache--item-start source pos len))\n"
	   "  (let* ((kind-pos (nelisp-standalone-artifact-cache--skip-ws\n"
	   "                    source (1+ pos)))\n"
	   "         (name-start (nelisp-standalone-artifact-cache--skip-ws\n"
	   "                      source (+ kind-pos 3)))\n"
	   "         (name-end name-start)\n"
	   "         (done nil)\n"
	   "         ch)\n"
	   "    (while (if (< name-end len) (not done) nil)\n"
	   "      (setq ch (aref source name-end))\n"
	   "      (if (= ch 32)\n"
	   "          (setq done t)\n"
	   "        (if (= ch 9)\n"
	   "            (setq done t)\n"
	   "          (if (= ch 10)\n"
	   "              (setq done t)\n"
	   "            (if (= ch 13)\n"
	   "                (setq done t)\n"
	   "              (if (= ch 41)\n"
	   "                  (setq done t)\n"
	   "                (setq name-end (1+ name-end))))))))\n"
	   "    (substring source name-start name-end)))\n"
	   "(defun nelisp-standalone-artifact-cache--skip-fn-name-p (name)\n"
	   "  (if (null name)\n"
	   "      nil\n"
	   "    (if (nelisp-standalone-artifact-cache--find \"native-exec\" name)\n"
	   "        t\n"
	   "      (if (nelisp-standalone-artifact-cache--find \"native-driver\" name)\n"
	   "          t\n"
	   "        (if (nelisp-standalone-artifact-cache--find \"native-object\" name)\n"
	   "            t\n"
	   "          (if (nelisp-standalone-artifact-cache--find \"base64\" name)\n"
	   "              t\n"
	   "            (if (nelisp-standalone-artifact-cache--find \"read-file-as-string\" name)\n"
	   "                t\n"
	   "              (if (nelisp-standalone-artifact-cache--find \"read-all-from-string\" name)\n"
	   "                  t\n"
	   "                (if (nelisp-standalone-artifact-cache--find \"file-record\" name)\n"
	   "                    t\n"
	   "                  (if (nelisp-standalone-artifact-cache--find \"read-top-level-forms\" name)\n"
	   "                      t\n"
	   "                    (if (nelisp-standalone-artifact-cache--find \"write-file\" name)\n"
	   "                        t\n"
	   "                      (if (nelisp-standalone-artifact-cache--find \"delete-if-exists\" name)\n"
	   "                          t\n"
	   "                        (if (nelisp-standalone-artifact-cache--find \"make-temp\" name)\n"
	   "                            t\n"
	   "                          (if (nelisp-standalone-artifact-cache--find \"canonical-integer-token\" name)\n"
	   "                              t\n"
	   "                            (if (nelisp-standalone-artifact-cache--find \"call-process-quiet\" name)\n"
	   "                                t\n"
	   "                              (if (nelisp-standalone-artifact-cache--find \"read-log-if-exists\" name)\n"
	   "                                  t\n"
	   "                                (if (nelisp-standalone-artifact-cache--find \"shell-quote\" name)\n"
	   "                                    t\n"
	   "                                  nil)))))))))))))))))\n"
	   "(defun nelisp-standalone-artifact-cache--find-before (needle source start end)\n"
	   "  (let ((p (nelisp-standalone-artifact-cache--find needle source start)))\n"
	   "    (if p\n"
	   "        (if (< p end) t nil)\n"
	   "      nil)))\n"
	   "(defun nelisp-standalone-artifact-cache--skip-item-end-fast (source pos len artifact)\n"
	   "  (let ((p (nelisp-standalone-artifact-cache--find \")) (:\" source pos)))\n"
	   "    (if p\n"
	   "        (+ p 2)\n"
	   "      (nelisp-standalone-artifact-cache--item-end source pos len artifact))))\n"
	   "(defun nelisp-standalone-artifact-cache--skip-item-source-p (source start end)\n"
	   "  (if (nelisp-standalone-artifact-cache--find-before \"native-exec\" source start end)\n"
	   "      t\n"
	   "    (if (nelisp-standalone-artifact-cache--find-before \"native-driver\" source start end)\n"
	   "        t\n"
	   "      (if (nelisp-standalone-artifact-cache--find-before \"native-object\" source start end)\n"
	   "          t\n"
	   "        (if (nelisp-standalone-artifact-cache--find-before \"base64\" source start end)\n"
	   "            t\n"
	   "          (if (nelisp-standalone-artifact-cache--find-before \"canonical-integer-token\" source start end)\n"
	   "              t\n"
	   "            (if (nelisp-standalone-artifact-cache--find-before \"call-process-quiet\" source start end)\n"
	   "                t\n"
	   "              (if (nelisp-standalone-artifact-cache--find-before \"read-log-if-exists\" source start end)\n"
	   "                  t\n"
	   "                (if (nelisp-standalone-artifact-cache--find-before \"shell-quote\" source start end)\n"
	   "                    t\n"
	   "                  nil)))))))))\n"
		   "(defun nelisp-standalone-artifact-cache--replay-item-source (source pos len artifact)\n"
		   "  (setq pos (nelisp-standalone-artifact-cache--item-start source pos len))\n"
		   "  (let* ((kind-pos (nelisp-standalone-artifact-cache--skip-ws\n"
		   "                    source (1+ pos)))\n"
		   "         (eval-end (+ kind-pos 5))\n"
		   "         end value-pos value-res item-res fn-name)\n"
	   "    (if (and (< pos len)\n"
	   "             (= (aref source pos) 40)\n"
	   "             (<= eval-end len)\n"
	   "             (equal (substring source kind-pos eval-end) \":eval\"))\n"
	   "        (progn\n"
	   "          (setq end (nelisp-standalone-artifact-cache--item-end\n"
	   "                     source pos len artifact))\n"
	   "          (setq value-pos\n"
	   "                (nelisp-standalone-artifact-cache--skip-ws\n"
	   "                 source eval-end))\n"
	   "          (setq value-res (read-from-string source value-pos))\n"
	   "          (unless value-res\n"
	   "            (error \"invalid :eval form in %s\" artifact))\n"
	   "          (cons (nelisp-standalone-artifact-cache--eval-form\n"
	   "                 (car value-res))\n"
	   "                end))\n"
		   "      (setq fn-name (nelisp-standalone-artifact-cache--fn-name-at\n"
		   "                     source pos len))\n"
		   "      (if (nelisp-standalone-artifact-cache--skip-fn-name-p fn-name)\n"
		   "          (progn\n"
		   "            (setq end (nelisp-standalone-artifact-cache--skip-item-end-fast\n"
		   "                       source pos len artifact))\n"
		   "            (cons fn-name end))\n"
		   "        (setq end (nelisp-standalone-artifact-cache--item-end\n"
		   "                   source pos len artifact))\n"
		   "        (setq item-res (nelisp-standalone-artifact-cache--read-item\n"
		   "                        source pos len artifact))\n"
		   "        (cons (nelisp-standalone-artifact-cache--replay-item (car item-res))\n"
		   "              end)))))\n"
	   "(defun nelisp-standalone-artifact-cache--replay-module (content artifact)\n"
	   "  (let ((pos (nelisp-standalone-artifact-cache--key-pos\n"
	   "              content :module-init artifact nil 0))\n"
	   "        (len (length content))\n"
		   "        (end-pos nil)\n"
		   "        (last nil)\n"
		   "        (count 0)\n"
		   "        res item)\n"
	   "    (setq pos (nelisp-standalone-artifact-cache--skip-ws content pos))\n"
	   "    (unless (and (< pos len) (= (aref content pos) 40))\n"
	   "      (error \"invalid :module-init list in %s\" artifact))\n"
	   "    (setq pos (1+ pos))\n"
	   "    (while (< pos len)\n"
	   "      (setq pos (nelisp-standalone-artifact-cache--skip-ws content pos))\n"
	   "      (if (>= pos len)\n"
	   "          (error \"unterminated :module-init list in %s\" artifact)\n"
	   "        (if (= (aref content pos) 41)\n"
	   "            (progn\n"
	   "              (setq end-pos (1+ pos))\n"
	   "              (setq pos len))\n"
	   "          (if (/= (aref content pos) 40)\n"
	   "              (progn\n"
	   "                (setq end-pos pos)\n"
	   "                (setq pos len))\n"
		   "          (setq pos (nelisp-standalone-artifact-cache--item-start\n"
		   "                     content pos len))\n"
		   "          (condition-case err\n"
		   "              (progn\n"
		   "                (setq count (1+ count))\n"
		   "                (setq res (nelisp-standalone-artifact-cache--replay-item-source\n"
		   "                           content pos len artifact))\n"
		   "                (setq item (car res))\n"
		   "                (setq pos (cdr res))\n"
		   "                (setq last item))\n"
	   "            (error\n"
	   "             (nelisp--write-stderr-line\n"
	   "              (concat \"cache replay error pos=\" (number-to-string pos)\n"
	   "                      \" err=\" (prin1-to-string err)))\n"
	   "             (signal (car err) (cdr err))))))))\n"
	   "    (unless end-pos\n"
	   "      (error \"unterminated :module-init list in %s\" artifact))\n"
	   "    (cons last end-pos)))\n"
	   "(defun nelisp-standalone-artifact-cache-load (artifact)\n"
	   "  (let* ((content (nelisp-standalone-artifact-cache--content artifact))\n"
	   "         (replayed (nelisp-standalone-artifact-cache--replay-module\n"
   "                    content artifact))\n"
   "         (last (car replayed)))\n"
	   "    last))\n"
   (nelisp-standalone--artifact-command-cache-dispatch-src
    nelisp-standalone--artifact-runtime-cache-path)))

(defun nelisp-standalone--artifact-source-command-cache-src (&optional substrate-only)
  "Return a compact marker path for source load/eval artifact commands.
This path exists for Nelix hot startup: `load-elisp-source' and
`eval-elisp-source' only need adjacent `.neln' / `.nelc' loading plus form
evaluation.  Avoid loading the full `nelisp-artifact.el' command surface and
avoid replaying the private artifact runtime cache.

When SUBSTRATE-ONLY is non-nil, omit the final command dispatch expression.
This exposes the real evaluator/reader/bytecode substrate as a standalone
source unit so Doc 154 Stage B can compile and measure it as a private
artifact before wiring that artifact into the marker command path.

artifact before wiring that artifact into the marker command path."
  (let ((artifact-magic (progn
                          (require 'nelisp-artifact)
                          nelisp-artifact--magic))
        (artifact-format (progn
                           (require 'nelisp-artifact)
                           nelisp-artifact--format))
        (stats-path nelisp-standalone--source-command-cache-stats-path))
    (concat
     (nelisp-standalone--artifact-runtime-file-src
      "scripts/nelisp-stdlib-prelude.el" t)
     (nelisp-standalone--artifact-runtime-file-src
      "src/nelisp-read.el" t)
     (nelisp-standalone--artifact-runtime-file-src
      "src/nelisp-eval.el" t)
     (nelisp-standalone--artifact-runtime-file-src
      "src/nelisp-macro.el" t)
     (nelisp-standalone--artifact-runtime-file-src
      "src/nelisp-load.el" t)
     (nelisp-standalone--artifact-runtime-file-src
      "src/nelisp-bytecode.el" t)
     "(defvar features nil)\n"
     "(fset 'provide\n"
     "      (lambda (feature)\n"
     "        (if (memq feature features)\n"
     "            nil\n"
     "          (setq features (cons feature features)))\n"
     "        feature))\n"
     "(fset 'featurep\n"
     "      (lambda (feature) (if (memq feature features) t nil)))\n"
     "(provide 'cl-lib)\n"
     "(provide 'nelisp-read)\n"
     "(provide 'nelisp-eval)\n"
     "(provide 'nelisp-macro)\n"
     "(provide 'nelisp-core-fileio)\n"
     "(provide 'nelisp-load)\n"
     "(provide 'nelisp-bytecode)\n"
     "(defun nelisp-standalone-source-cache--read-file (path)\n"
     "  (or (nelisp--syscall-read-file path) \"\"))\n"
     (format "(defvar nelisp-standalone-source-cache--stats-path %S)\n"
             stats-path)
     "(defun nelisp-standalone-source-cache--stats (stage)\n"
     "  (if (file-exists-p nelisp-standalone-source-cache--stats-path)\n"
     "      (nelisp--write-stderr-line\n"
     "       (concat \"source-cache stats stage=\" stage \" \"\n"
     "               (prin1-to-string (nelisp--arena-stats))))\n"
     "    nil))\n"
     "(defun nelisp-standalone-source-cache--find (needle haystack &optional start)\n"
     "  (let* ((i (or start 0)) (n (length needle)) (h (length haystack))\n"
     "         (limit (- h n)) (found nil))\n"
     "    (while (and (not found) (<= i limit))\n"
     "      (let ((j 0) (ok t))\n"
     "        (while (and ok (< j n))\n"
     "          (unless (= (aref needle j) (aref haystack (+ i j)))\n"
     "            (setq ok nil))\n"
     "          (setq j (1+ j)))\n"
     "        (if ok (setq found i) (setq i (1+ i)))))\n"
     "    found))\n"
     "(defun nelisp-standalone-source-cache--prefix-at-p (prefix source pos)\n"
     "  (let ((i 0) (n (length prefix)) (len (length source)) (ok t))\n"
     "    (while (and ok (< i n))\n"
     "      (if (or (>= (+ pos i) len)\n"
     "              (not (= (aref prefix i) (aref source (+ pos i)))))\n"
     "          (setq ok nil)\n"
     "        (setq i (1+ i))))\n"
     "    ok))\n"
     "(defun nelisp-standalone-source-cache--skip-ws (source pos)\n"
     "  (let ((len (length source)) ch)\n"
     "    (while (< pos len)\n"
     "      (setq ch (aref source pos))\n"
     "      (if (if (= ch 32) t\n"
     "            (if (= ch 9) t\n"
     "              (if (= ch 10) t\n"
     "                (if (= ch 13) t\n"
     "                  (if (= ch 12) t nil)))))\n"
     "          (setq pos (1+ pos))\n"
     "        (setq len 0)))\n"
     "    pos))\n"
     "(defun nelisp-standalone-source-cache--key-pos (source key label start)\n"
     "  (let* ((needle (concat (symbol-name key) \" \"))\n"
     "         (pos (nelisp-standalone-source-cache--find needle source start)))\n"
     "    (if pos nil\n"
     "      (error \"missing key %S in %s\" key label))\n"
     "    (nelisp-standalone-source-cache--skip-ws\n"
     "     source (+ pos (length needle)))))\n"
     "(defun nelisp-standalone-source-cache--key-value (source key label start)\n"
     "  (car (read-from-string\n"
     "        source\n"
     "        (nelisp-standalone-source-cache--key-pos source key label start))))\n"
     "(defun nelisp-standalone-source-cache--validate-payload (content artifact-path)\n"
     (format "  (let* ((magic %S)\n" artifact-magic)
     "         (prefix-len (length magic)))\n"
     "    (if (and (>= (length content) prefix-len)\n"
     "             (equal (substring content 0 prefix-len) magic))\n"
     "        nil\n"
     "      (error \"invalid artifact magic: %s\" artifact-path))\n"
     (format "    (if (nelisp-standalone-source-cache--find %S content prefix-len)\n"
             (format ":format %S" artifact-format))
     "        nil\n"
     "      (error \"unsupported artifact format in %s\" artifact-path))\n"
     "    prefix-len))\n"
     "(defun nelisp-standalone-source-cache--item-end (source pos len artifact)\n"
     "  (let ((i pos) (depth 0) (in-string nil) (escape nil) (done nil) ch)\n"
     "    (while (and (not done) (< i len))\n"
     "      (setq ch (aref source i))\n"
     "      (cond\n"
     "       (in-string\n"
     "        (cond\n"
     "         (escape (setq escape nil))\n"
     "         ((= ch 92) (setq escape t))\n"
     "         ((= ch 34) (setq in-string nil))))\n"
     "       ((= ch 34) (setq in-string t))\n"
     "       ((= ch 59)\n"
     "        (while (and (< i len) (not (= (aref source i) 10)))\n"
     "          (setq i (1+ i))))\n"
     "       ((= ch 40) (setq depth (1+ depth)))\n"
     "       ((= ch 41)\n"
     "        (setq depth (1- depth))\n"
     "        (if (= depth 0)\n"
     "            (setq done t)\n"
     "          nil)))\n"
     "      (setq i (1+ i)))\n"
     "    (if done nil\n"
     "      (error \"unterminated :module-init item in %s\" artifact))\n"
     "    i))\n"
     "(defun nelisp-standalone-source-cache--read-decimal-at (source pos)\n"
     "  (let ((i pos) (len (length source)) (value 0) (have nil) ch)\n"
     "    (while (and (< i len)\n"
     "                (progn\n"
     "                  (setq ch (aref source i))\n"
     "                  (and (>= ch 48) (<= ch 57))))\n"
     "      (setq have t)\n"
     "      (setq value (+ (* value 10) (- ch 48)))\n"
     "      (setq i (1+ i)))\n"
     "    (if have nil (error \"expected decimal integer\"))\n"
     "    (cons value i)))\n"
     "(defun nelisp-standalone-source-cache--replay-raw-source-item (content pos)\n"
     "  (let ((prefix \"(:eval-source-raw \"))\n"
     "    (if (and (fboundp 'nelisp--eval-source-string)\n"
     "             (nelisp-standalone-source-cache--prefix-at-p prefix content pos))\n"
     "        (let* ((len-start (+ pos (length prefix)))\n"
     "               (len-pair (nelisp-standalone-source-cache--read-decimal-at\n"
     "                          content len-start))\n"
     "               (source-len (car len-pair))\n"
     "               (source-start (+ (cdr len-pair) 1))\n"
     "               (source-end (+ source-start source-len)))\n"
     "          (if (and (= (aref content (cdr len-pair)) 10)\n"
     "                   (< (+ source-end 2) (length content))\n"
     "                   (= (aref content source-end) 10)\n"
     "                   (= (aref content (+ source-end 1)) 41)\n"
     "                   (= (aref content (+ source-end 2)) 41))\n"
     "              (list t\n"
     "                    (nelisp--eval-source-string\n"
     "                     (substring content source-start source-end))\n"
     "                    (+ source-end 3))\n"
     "            (error \"invalid raw eval source item\")))\n"
     "      nil)))\n"
     "(defun nelisp-standalone-source-cache--replay-generated-source-item (content pos)\n"
     "  (let ((prefix \"(:eval (progn\\n\")\n"
     "        (suffix \"\\n))) :features\"))\n"
     "    (if (and (fboundp 'nelisp--eval-source-string)\n"
     "             (nelisp-standalone-source-cache--prefix-at-p prefix content pos))\n"
     "        (let* ((source-start (+ pos (length prefix)))\n"
     "               (source-end (nelisp-standalone-source-cache--find\n"
     "                            suffix content source-start)))\n"
     "          (if source-end\n"
     "              (list t\n"
     "                    (nelisp--eval-source-string\n"
     "                     (substring content source-start source-end))\n"
     "                    (+ source-end 4))\n"
     "            nil))\n"
     "      nil)))\n"
     "(defun nelisp-standalone-source-cache--install-fn (name fn)\n"
     "  (puthash name fn nelisp--functions)\n"
     "  (fset name\n"
     "        (list 'lambda '(&rest args)\n"
     "              (list 'nelisp--apply\n"
     "                    (list 'gethash (list 'quote name) 'nelisp--functions)\n"
     "                    'args)))\n"
     "  name)\n"
     "(defun nelisp-standalone-source-cache--replay-item (item)\n"
     "  (if (and (consp item) (eq (car item) :fn))\n"
     "      (progn\n"
     "        (nelisp-standalone-source-cache--install-fn (nth 1 item) (nth 2 item))\n"
     "        (nth 1 item))\n"
     "    (if (and (consp item) (eq (car item) :eval))\n"
     "        (eval (nth 1 item))\n"
     "      (eval item))))\n"
     "(defun nelisp-standalone-source-cache--replay-module (content artifact-path prefix-len)\n"
     "  (let ((pos (nelisp-standalone-source-cache--key-pos\n"
     "              content :module-init artifact-path prefix-len))\n"
     "        (len (length content))\n"
     "        (last nil)\n"
     "        (done nil)\n"
     "        end res)\n"
     "    (setq pos (nelisp-standalone-source-cache--skip-ws content pos))\n"
     "    (if (and (< pos len) (= (aref content pos) 40))\n"
     "        nil\n"
     "      (error \"invalid :module-init list in %s\" artifact-path))\n"
     "    (setq pos (1+ pos))\n"
     "    (while (and (< pos len) (not done))\n"
     "      (setq pos (nelisp-standalone-source-cache--skip-ws content pos))\n"
     "      (if (>= pos len)\n"
     "          (error \"unterminated :module-init list in %s\" artifact-path)\n"
     "        (if (= (aref content pos) 41)\n"
     "            (progn\n"
     "              (setq pos (1+ pos))\n"
     "              (setq done t))\n"
     "          (if (= (aref content pos) 40)\n"
     "              nil\n"
     "            (error \"invalid :module-init item in %s\" artifact-path))\n"
     "          (setq res (nelisp-standalone-source-cache--replay-raw-source-item\n"
     "                     content pos))\n"
     "          (if res nil\n"
     "            (setq res (nelisp-standalone-source-cache--replay-generated-source-item\n"
     "                       content pos)))\n"
     "          (if res\n"
     "              (progn\n"
     "                (setq last (cadr res))\n"
     "                (setq pos (caddr res))\n"
     "                (setq done t))\n"
     "            (setq end (nelisp-standalone-source-cache--item-end\n"
     "                       content pos len artifact-path))\n"
     "            (setq res (read-from-string content pos))\n"
     "            (if res nil\n"
     "              (error \"invalid :module-init item in %s\" artifact-path))\n"
     "            (setq last (nelisp-standalone-source-cache--replay-item\n"
     "                        (car res)))\n"
     "            (setq pos end)))))\n"
     "    (cons last pos)))\n"
     "(defun nelisp-standalone-source-cache-load-artifact (artifact-path)\n"
     "  (let* ((content nil)\n"
     "         (prefix-len nil)\n"
     "         (features nil)\n"
     "         (last nil)\n"
     "         (module-result nil))\n"
     "    (nelisp-standalone-source-cache--stats \"artifact-before-read\")\n"
     "    (setq content (nelisp-standalone-source-cache--read-file artifact-path))\n"
     "    (nelisp-standalone-source-cache--stats \"artifact-after-read\")\n"
     "    (setq prefix-len\n"
     "          (nelisp-standalone-source-cache--validate-payload\n"
     "           content artifact-path))\n"
     "    (nelisp-standalone-source-cache--stats \"artifact-after-format\")\n"
     "    (setq module-result (nelisp-standalone-source-cache--replay-module\n"
     "                         content artifact-path prefix-len))\n"
     "    (setq last (car module-result))\n"
     "    (nelisp-standalone-source-cache--stats \"artifact-after-module\")\n"
     "    (setq features (nelisp-standalone-source-cache--key-value\n"
     "                    content :features artifact-path (cdr module-result)))\n"
     "    (while features\n"
     "      (provide (car features))\n"
     "      (setq features (cdr features)))\n"
     "    (nelisp-standalone-source-cache--stats \"artifact-after-features\")\n"
     "    last))\n"
     "(defun nelisp-standalone-source-cache--parse-source-args (args require-forms)\n"
     "  (let ((rest (cdr args)) (source nil) (forms nil))\n"
     "    (while (and rest (null source))\n"
     "      (let ((flag (car rest)))\n"
     "        (if (equal flag \"--auto-compile\")\n"
     "            (setq rest (cdr rest))\n"
     "          (if (or (equal flag \"--kind\")\n"
     "                  (equal flag \"--target\")\n"
     "                  (equal flag \"--load-path\")\n"
     "                  (equal flag \"--preload\")\n"
     "                  (equal flag \"--native-policy\"))\n"
     "              (setq rest (cdr (cdr rest)))\n"
     "            (setq source flag)\n"
     "            (setq forms (cdr rest))\n"
     "            (setq rest nil)))))\n"
     "    (unless source\n"
     "      (error \"source command requires FILE.el\"))\n"
     "    (when (and require-forms (null forms))\n"
     "      (error \"eval-elisp-source requires at least one FORM\"))\n"
     "    (list source forms)))\n"
     "(defun nelisp-standalone-source-cache--artifact (source)\n"
     "  (if (file-exists-p (concat source \".neln\"))\n"
     "      (concat source \".neln\")\n"
     "    (if (file-exists-p (concat source \".nelc\"))\n"
     "        (concat source \".nelc\")\n"
     "      nil)))\n"
     "(defun nelisp-standalone-source-cache-load-source-file (source)\n"
     "  (let* ((text (nelisp-standalone-source-cache--read-file source))\n"
     "         (pos 0)\n"
     "         (len (length text))\n"
     "         (last nil)\n"
     "         form res)\n"
     "    (while (progn\n"
     "             (setq pos (nelisp-standalone-source-cache--skip-ws text pos))\n"
     "             (< pos len))\n"
     "      (setq res (read-from-string text pos))\n"
     "      (setq form (car res))\n"
     "      (setq pos (cdr res))\n"
     "      (setq last (eval form)))\n"
     "    last))\n"
     "(defun nelisp-standalone-source-cache-load-source-command (args)\n"
     "  (let* ((parsed (nelisp-standalone-source-cache--parse-source-args args nil))\n"
     "         (source (car parsed))\n"
     "         (artifact (nelisp-standalone-source-cache--artifact source))\n"
     "         (value nil))\n"
     "    (nelisp-standalone-source-cache--stats \"entry\")\n"
     "    (setq value\n"
     "          (if artifact\n"
     "              (nelisp-standalone-source-cache-load-artifact artifact)\n"
     "            (nelisp-standalone-source-cache-load-source-file source)))\n"
     "    (nelisp-standalone-source-cache--stats \"after-load\")\n"
     "    (nelisp--write-stdout-bytes (prin1-to-string value))\n"
     "    (nelisp--write-stdout-bytes \"\\n\")\n"
     "    0))\n"
     "(defun nelisp-standalone-source-cache-eval-source-command (args)\n"
     "  (let* ((parsed (nelisp-standalone-source-cache--parse-source-args args t))\n"
     "         (source (car parsed))\n"
     "         (forms (nth 1 parsed))\n"
     "         (artifact (nelisp-standalone-source-cache--artifact source))\n"
     "         (last nil))\n"
     "    (nelisp-standalone-source-cache--stats \"entry\")\n"
     "    (if artifact\n"
     "        (nelisp-standalone-source-cache-load-artifact artifact)\n"
     "      (nelisp-standalone-source-cache-load-source-file source))\n"
     "    (nelisp-standalone-source-cache--stats \"after-load\")\n"
     "    (while forms\n"
     "      (setq last (eval (car (read-from-string (car forms)))))\n"
     "      (setq forms (cdr forms)))\n"
     "    (nelisp-standalone-source-cache--stats \"after-eval\")\n"
     "    (nelisp--write-stdout-bytes (prin1-to-string last))\n"
     "    (nelisp--write-stdout-bytes \"\\n\")\n"
     "    0))\n"
     (if substrate-only
         ""
       (concat
       "(if (string= (car nelisp-standalone-argv) \"load-elisp-source\")\n"
       "    (nelisp-standalone-source-cache-load-source-command nelisp-standalone-argv)\n"
       "  (if (string= (car nelisp-standalone-argv) \"eval-elisp-source\")\n"
       "      (nelisp-standalone-source-cache-eval-source-command nelisp-standalone-argv)\n"
       "    2))\n")))
  ))

(defun nelisp-standalone--artifact-source-command-substrate-src ()
  "Return the source-command evaluator substrate without command dispatch."
  (nelisp-standalone--artifact-source-command-cache-src t))

(defun nelisp-standalone--artifact-direct-command-src ()
  "Return compact source for direct eval/exec artifact commands.
This shares the source-command substrate loader, then reads the artifact path
and FORM strings from `nelisp-standalone-argv'.  The source is emitted with
`sexp-write-str-lit' so the large substrate is not expanded into per-byte AOT
copy instructions."
  (concat
   (nelisp-standalone--artifact-source-command-substrate-src)
   "(defun nelisp-standalone-direct-artifact-command (args printp)\n"
   "  (condition-case err\n"
   "      (let ((path (nth 1 args))\n"
   "            (forms (cdr (cdr args)))\n"
   "            (parsed nil)\n"
   "            (last nil))\n"
   "        (if path nil\n"
   "          (error \"artifact command requires FILE.nelc or FILE.neln\"))\n"
   "        (if forms nil\n"
   "          (error \"artifact command requires at least one FORM\"))\n"
   "        (while forms\n"
   "          (setq parsed (cons (car (read-from-string (car forms))) parsed))\n"
   "          (setq forms (cdr forms)))\n"
   "        (setq forms (reverse parsed))\n"
   "        (nelisp-standalone-source-cache-load-artifact path)\n"
   "        (while forms\n"
   "          (setq last (eval (car forms)))\n"
   "          (setq forms (cdr forms)))\n"
   "        (if printp\n"
   "            (progn\n"
   "              (nelisp--write-stdout-bytes (prin1-to-string last))\n"
   "              (nelisp--write-stdout-bytes \"\\n\"))\n"
   "          nil)\n"
   "        0)\n"
   "    (error\n"
   "     (nelisp--write-stderr-line\n"
   "      (concat \"nelisp direct artifact error: \" (prin1-to-string err)))\n"
   "     1)))\n"
   "(if (string= (car nelisp-standalone-argv) \"eval-elisp-artifact\")\n"
   "    (nelisp-standalone-direct-artifact-command nelisp-standalone-argv t)\n"
   "  (if (string= (car nelisp-standalone-argv) \"exec-elisp-artifact\")\n"
   "      (nelisp-standalone-direct-artifact-command nelisp-standalone-argv nil)\n"
   "    2))\n"))

(defun nelisp-standalone--name-words (nm)
  "Return the little-endian u64 words packing NM's UTF-8 bytes (ceil(len/8) of
them).  Used to install builtin names of ANY length: the stock single-u64
buffer corrupts names > 8 bytes because nl_alloc_symbol reads `name_len' bytes
from an 8-byte buffer."
  (let* ((bytes (encode-coding-string nm 'utf-8 t))
         (n (length bytes))
         (words nil) (i 0))
    (while (< i n)
      (let ((w 0) (j 0))
        (while (and (< j 8) (< (+ i j) n))
          (setq w (logior w (ash (aref bytes (+ i j)) (* j 8))))
          (setq j (1+ j)))
        (push w words))
      (setq i (+ i 8)))
    (nreverse words)))

(defun nelisp-standalone--cstr-eq-defun (name string)
  "Return a Phase47 defun checking whether a C string equals STRING."
  (let* ((bytes (append (encode-coding-string string 'utf-8 t) nil))
         (len (length bytes))
         (body `(if (= (ptr-read-u8 ptr ,len) 0) 1 0)))
    (let ((i (1- len)))
      (while (>= i 0)
        (setq body
              `(if (= (ptr-read-u8 ptr ,i) ,(nth i bytes))
                   ,body
                 0))
        (setq i (1- i))))
    `(defun ,name (ptr)
       (if (= ptr 0) 0 ,body))))

(defun nelisp-standalone--copy-lit-defun (name string)
  "Return a Phase47 defun copying literal STRING into DST at OFF."
  (let ((bytes (append (encode-coding-string string 'utf-8 t) nil))
        (forms nil)
        (i 0))
    (dolist (byte bytes)
      (push `(ptr-write-u8 dst (+ off ,i) ,byte) forms)
      (setq i (1+ i)))
    `(defun ,name (dst off)
       (seq
        ,@(nreverse forms)
        (+ off ,(length bytes))))))

(defun nelisp-standalone--copy-lit-u64-defun (name string)
  "Return a Phase47 defun copying literal STRING into DST at OFF.
This chunked variant is for large literals copied into an arena buffer; it
avoids the aarch64 `sexp-write-str-lit' path that stages the whole literal on
the native stack."
  (let* ((bytes (encode-coding-string string 'utf-8 t))
         (len (length bytes))
         (full-chunks (/ len 8))
         (tail-start (* full-chunks 8))
         (forms nil))
    (dotimes (i full-chunks)
      (let ((word 0))
        (dotimes (j 8)
          (setq word (logior word
                             (ash (aref bytes (+ (* i 8) j)) (* j 8)))))
        (push `(ptr-write-u64 dst (+ off ,(* i 8)) ,word) forms)))
    (let ((i tail-start))
      (while (< i len)
        (push `(ptr-write-u8 dst (+ off ,i) ,(aref bytes i)) forms)
        (setq i (1+ i))))
    `(defun ,name (dst off)
       (seq
        ,@(nreverse forms)
        (+ off ,len)))))

(defun nelisp-standalone--copy-lit-u64-defuns (name string &optional chunk-bytes)
  "Return Phase47 defuns copying large literal STRING into DST at OFF.
The generated NAME defun delegates to bounded chunk defuns so host-side walkers
never recurse through one enormous `seq' cdr chain."
  (let* ((bytes (encode-coding-string string 'utf-8 t))
         (len (length bytes))
         (chunk-bytes (or chunk-bytes 2048))
         (defs nil)
         (calls nil)
         (start 0)
         (idx 0))
    (while (< start len)
      (let* ((chunk-name (intern (format "%s_chunk_%03d" name idx)))
             (end (min len (+ start chunk-bytes)))
             (forms nil)
             (full-chunks (/ (- end start) 8))
             (tail-start (+ start (* full-chunks 8))))
        (dotimes (i full-chunks)
          (let ((word 0)
                (abs (+ start (* i 8))))
            (dotimes (j 8)
              (setq word (logior word
                                 (ash (aref bytes (+ abs j)) (* j 8)))))
            (push `(ptr-write-u64 dst (+ off ,(* i 8)) ,word) forms)))
        (let ((pos tail-start))
          (while (< pos end)
            (push `(ptr-write-u8 dst (+ off ,(- pos start))
                                  ,(aref bytes pos))
                  forms)
            (setq pos (1+ pos))))
        (push `(defun ,chunk-name (dst off)
                 (seq
                  ,@(nreverse forms)
                  (+ off ,(- end start))))
              defs)
        (push `(setq off (,chunk-name dst off)) calls)
        (setq start end
              idx (1+ idx))))
    (append
     (nreverse defs)
     (list
      `(defun ,name (dst off)
         (seq
          ,@(nreverse calls)
          off))))))

(defun nelisp-standalone--u32le-bytes (value)
  "Return VALUE as 4 little-endian bytes."
  (let ((u (logand value #xffffffff)))
    (list (logand u #xff)
          (logand (ash u -8) #xff)
          (logand (ash u -16) #xff)
          (logand (ash u -24) #xff))))

(defun nelisp-standalone--native-trampoline-slot-disp (slot-index)
  "Return the rbp-relative spill displacement for SLOT-INDEX."
  (- (* 8 (1+ slot-index))))

(defun nelisp-standalone--native-mov-rbp-disp-reg-bytes (reg disp)
  "Return `mov [rbp+DISP], REG' bytes for the x86_64 SysV trampoline."
  (let ((spec (alist-get reg '((rax . (#x48 #x45 #x85))
                               (rcx . (#x48 #x4d #x8d))
                               (rdx . (#x48 #x55 #x95))
                               (rsi . (#x48 #x75 #xb5))
                               (rdi . (#x48 #x7d #xbd))
                               (r8 . (#x4c #x45 #x85))
                               (r9 . (#x4c #x4d #x8d))))))
    (unless spec
      (error "unsupported trampoline register %S" reg))
    (pcase-let ((`(,rex ,modrm8 ,modrm32) spec))
      (append (list rex #x89 (if (<= -128 disp 127) modrm8 modrm32))
              (if (<= -128 disp 127)
                  (list (logand disp #xff))
                (nelisp-standalone--u32le-bytes disp))))))

(defun nelisp-standalone--ptr-write-u8-forms (base-sym bytes)
  "Return `(ptr-write-u8 ...)' forms that copy BYTES into BASE-SYM."
  (let ((idx -1))
    (mapcar (lambda (byte)
              (setq idx (1+ idx))
              `(ptr-write-u8 ,base-sym ,idx ,byte))
            bytes)))

(defun nelisp-standalone--native-trampoline-bytes (meta)
  "Return raw trampoline bytes and imm64 patch offsets for META."
  (let* ((arity (or (plist-get meta :arity) 0))
         (frame-bytes (nelisp-artifact--native-trampoline-frame-bytes meta))
         (arg-regs '(rdi rsi rdx rcx r8 r9))
         (bytes nil)
         (imm64-offsets nil))
    (unless (<= arity (length arg-regs))
      (error "trampoline arity %d exceeds gp register support" arity))
    (cl-labels
        ((emit (chunk)
           (setq bytes (append bytes chunk)))
         (emit-imm64-placeholder ()
           (push (+ (length bytes) 2) imm64-offsets)
           (emit '(#x48 #xb8 0 0 0 0 0 0 0 0))))
      (emit '(#x55 #x48 #x89 #xe5))
      (when (> frame-bytes 0)
        (emit (append '(#x48 #x81 #xec)
                      (nelisp-standalone--u32le-bytes frame-bytes))))
      (dotimes (i arity)
        (emit
         (nelisp-standalone--native-mov-rbp-disp-reg-bytes
          (nth i arg-regs)
          (nelisp-standalone--native-trampoline-slot-disp i))))
      (dotimes (i (+ 5 12))
        (emit-imm64-placeholder)
        (emit
         (nelisp-standalone--native-mov-rbp-disp-reg-bytes
          'rax
          (nelisp-standalone--native-trampoline-slot-disp
           (+ arity i)))))
      (emit-imm64-placeholder)
      (emit '(#xff #xe0)))
    (list :bytes bytes
          :imm64-offsets (nreverse imm64-offsets))))

(defvar nelisp-standalone--reader-neln-demo-cache nil
  "Cached embedded `.neln' demo metadata for the standalone reader.")

(defun nelisp-standalone--reader-neln-demo-build-spec ()
  "Compile the embedded `(defun inc1 (x) (1+ x))' demo and extract its metadata."
  (require 'nelisp-artifact)
  (let* ((dir (make-temp-file "nelisp-reader-neln-demo-" t))
         (src (expand-file-name "inc1.el" dir))
         (out (expand-file-name "inc1.neln" dir)))
    (unwind-protect
        (progn
          (with-temp-file src
            (insert "(defun inc1 (x) (1+ x))\n(provide 'inc1)\n"))
          (nelisp-artifact-compile-file src out nil nil nil nil nil 'neln)
          (let* ((native (plist-get (nelisp-artifact--read-payload out) :native))
                 (defun0 (car (plist-get native :defuns)))
                 (text-bytes (string-to-list
                              (base64-decode-string
                               (plist-get native :text-base64))))
                 (relocs (plist-get native :relocs))
                 (externs (plist-get native :extern-symbols))
                 (trampoline
                  (nelisp-standalone--native-trampoline-bytes defun0))
                 (stub-specs
                  '((:name "nelisp_aot_builtin_call1"
                     :bridge nl_neln_demo_call1_bridge
                     :offset 256)
                    (:name "nl_alloc_symbol"
                     :bridge nl_neln_demo_alloc_symbol_bridge
                     :offset 272))))
            (unless (equal (plist-get defun0 :name) "inc1")
              (error "embedded neln demo expected inc1, got %S"
                     (plist-get defun0 :name)))
            (unless (equal (sort (copy-sequence externs) #'string<)
                           '("nelisp_aot_builtin_call1" "nl_alloc_symbol"))
              (error "embedded neln demo externs drifted: %S" externs))
            (list :text-bytes text-bytes
                  :relocs relocs
                  :defun0 defun0
                  :trampoline-bytes (plist-get trampoline :bytes)
                  :imm64-offsets (plist-get trampoline :imm64-offsets)
                  :stub-specs stub-specs
                  :body-entry (+ (plist-get defun0 :offset)
                                 (plist-get defun0 :body-offset)))))
      (delete-directory dir t))))

(defun nelisp-standalone--reader-neln-demo-spec ()
  "Return cached embedded `.neln' demo metadata."
  (or nelisp-standalone--reader-neln-demo-cache
      (setq nelisp-standalone--reader-neln-demo-cache
            (nelisp-standalone--reader-neln-demo-build-spec))))

(defun nelisp-standalone--reader-neln-demo-source ()
  "Return the cached-unit source backing `--neln-selftest'."
  (if (not (eq nelisp-standalone--target 'linux-x86_64))
      '(seq
        (defun nl_neln_demo_exec (_ctx _x)
          125))
    (let* ((spec (nelisp-standalone--reader-neln-demo-spec))
           (text-bytes (plist-get spec :text-bytes))
           (relocs (plist-get spec :relocs))
           (trampoline-bytes (plist-get spec :trampoline-bytes))
           (imm64-offsets (plist-get spec :imm64-offsets))
           (stub-specs (plist-get spec :stub-specs))
           (body-entry (plist-get spec :body-entry))
           (stub-template-bytes '(#x48 #xb8 0 0 0 0 0 0 0 0 #xff #xe0))
           (callback-slot-exprs
            (cl-loop for i from 0 below 12
                     collect `(+ slots ,(+ 96 (* i 32)))))
           (text-writes
            (nelisp-standalone--ptr-write-u8-forms 'codepage text-bytes))
           (stub-writes
            (apply
             #'append
             (mapcar
              (lambda (stub)
                (nelisp-standalone--ptr-write-u8-forms
                 `(+ codepage ,(plist-get stub :offset))
                 stub-template-bytes))
              stub-specs)))
           (stub-patches
            (mapcar
             (lambda (stub)
               `(ptr-write-u64 (+ codepage ,(plist-get stub :offset))
                               2
                               (addr-of ,(plist-get stub :bridge))))
             stub-specs))
           (reloc-forms
            (mapcar
             (lambda (reloc)
               (let* ((offset (plist-get reloc :offset))
                      (stub
                       (cl-find-if
                        (lambda (entry)
                          (equal (plist-get entry :name)
                                 (plist-get reloc :symbol)))
                        stub-specs))
                      (stub-offset (or (plist-get stub :offset)
                                       (error "missing stub for %s"
                                              (plist-get reloc :symbol))))
                      (addend (or (plist-get reloc :addend) 0)))
                 `(ptr-write-u32 codepage ,offset
                                 (- (+ codepage ,stub-offset ,addend)
                                    (+ codepage ,offset)))))
             relocs))
           (trampoline-writes
            (nelisp-standalone--ptr-write-u8-forms
             'trampage trampoline-bytes))
           (slot-values
            (append
             (list 'slots '(+ ctx 0) '(+ ctx 32) '(+ slots 32) '(+ slots 64))
             callback-slot-exprs
             (list `(+ codepage ,body-entry))))
           (imm64-patches
            (cl-mapcar
             (lambda (offset value)
               `(ptr-write-u64 trampage ,offset ,value))
             imm64-offsets
             slot-values))
           (slot-zero-forms
            (append
             (list '(nl_neln_demo_zero_slot slots)
                   '(nl_neln_demo_zero_slot (+ slots 32))
                   '(nl_neln_demo_zero_slot (+ slots 64)))
             (mapcar (lambda (slot-expr)
                       `(nl_neln_demo_zero_slot ,slot-expr))
                     callback-slot-exprs))))
      (cons
       'seq
       (append
        '((defun nl_neln_demo_alloc_symbol_bridge (bytes-ptr len result-slot)
            (seq
             (extern-call nl_alloc_symbol bytes-ptr len result-slot)
             result-slot))
          (defun nl_neln_demo_call1_bridge (mirror frames name arg out scratch)
            (seq
             (extern-call nelisp_aot_builtin_call1
                          mirror frames name arg out scratch)
             out))
          (defun nl_neln_demo_zero_slot (slot)
            (seq
             (ptr-write-u64 slot 0 0)
             (ptr-write-u64 (+ slot 8) 0 0)
             (ptr-write-u64 (+ slot 16) 0 0)
             (ptr-write-u64 (+ slot 24) 0 0)
             0))
          (defun nl_neln_demo_write_int_slot (slot value)
            (seq
             (ptr-write-u64 slot 0 2)
             (ptr-write-u64 (+ slot 8) 0 value)
             (ptr-write-u64 (+ slot 16) 0 0)
             (ptr-write-u64 (+ slot 24) 0 0)
             slot))
          (defun nl_neln_demo_read_int_slot (slot)
            (if (= (ptr-read-u64 slot 0) 2)
                (ptr-read-u64 slot 8)
              98)))
        (list
         `(defun nl_neln_demo_exec (ctx x)
            (let ((codepage (syscall-direct 9 0 4096 7 34 -1 0)))
              (if (< codepage 4096)
                  90
                (let ((slots (syscall-direct 9 0 4096 3 34 -1 0)))
                  (if (< slots 4096)
                      91
                    (let ((trampage (syscall-direct 9 0 4096 7 34 -1 0)))
                      (if (< trampage 4096)
                          92
                        (seq
                         ,@slot-zero-forms
                         ,@text-writes
                         ,@stub-writes
                         ,@stub-patches
                         ,@reloc-forms
                         ,@trampoline-writes
                         ,@imm64-patches
                         (nl_neln_demo_write_int_slot (+ slots 480) x)
                         (call-ptr trampage (+ slots 480))
                         (nl_neln_demo_read_int_slot slots)))))))))))))))

(defun nelisp-standalone--reader-install-builtins-forms ()
  "Phase47 forms that install every `nelisp-standalone--reader-builtins' name
with a FULL-LENGTH name buffer (ceil(len/8) u64 words), fixing >8-byte names."
  (mapcar
   (lambda (nm)
     (let* ((words (nelisp-standalone--name-words nm))
            (cap (* (length words) 8))
            (len (length (encode-coding-string nm 'utf-8 t))))
       `(let* ((b (alloc-bytes ,cap 1)))
          (seq
           ,@(let ((w 0) (forms nil))
               (dolist (word words)
                 (push `(ptr-write-u64 b ,(* w 8) ,word) forms)
                 (setq w (1+ w)))
               (nreverse forms))
           (nl_install_one globals unbound b ,len builtin_sym)))))
   nelisp-standalone--reader-builtins))

(defun nelisp-standalone--os-syscall-xlat-forms ()
  "Per-target raw-syscall wrappers for the path/stat/dirent fileio builtins.
The interpreted layer (nemacs bridge, compat-fileio, anvil polyfills)
passes *Linux x86_64* syscall numbers as its portable vocabulary
(access=21, unlink=87, ...).  These wrappers are the single translation
boundary (Doc 151 Phase B):
- linux-x86_64: pass-through (numbers already native).
- linux-aarch64: arm64 Linux has no legacy syscalls — map to the *at
  family (faccessat/unlinkat/mkdirat/renameat/newfstatat/...) with
  AT_FDCWD, preserving each builtin's return contract.
- macos / windows: return -ENOSYS(-38) stubs.  Previously the shared
  fileio unit fired the raw x86 numbers on those targets, i.e. random
  foreign syscalls — the stub is strictly safer (callers already
  handle negative rc as failure)."
  (pcase nelisp-standalone--target
    ('linux-aarch64
     '((defun nl_os_syscall_path (nr cpath)
         (if (= nr 87) (syscall-direct 35 (- 0 100) cpath 0 0 0 0)      ; unlink -> unlinkat
           (if (= nr 84) (syscall-direct 35 (- 0 100) cpath 512 0 0 0)  ; rmdir -> unlinkat+AT_REMOVEDIR
             (if (= nr 80) (syscall-direct 49 cpath 0 0 0 0 0)          ; chdir (native 49)
               (syscall-direct nr cpath 0 0 0 0 0)))))
       (defun nl_os_syscall_path_int (nr cpath iarg)
         (if (= nr 21) (syscall-direct 48 (- 0 100) cpath iarg 0 0 0)   ; access -> faccessat
           (if (= nr 90) (syscall-direct 53 (- 0 100) cpath iarg 0 0 0) ; chmod -> fchmodat
             (if (= nr 83) (syscall-direct 34 (- 0 100) cpath iarg 0 0 0) ; mkdir -> mkdirat
               (if (= nr 76) (syscall-direct 45 cpath iarg 0 0 0 0)     ; truncate (native 45)
                 (syscall-direct nr cpath iarg 0 0 0 0))))))
       (defun nl_os_syscall_path2 (nr c1 c2)
         (if (= nr 82) (syscall-direct 38 (- 0 100) c1 (- 0 100) c2 0 0)   ; rename -> renameat
           (if (= nr 86) (syscall-direct 37 (- 0 100) c1 (- 0 100) c2 0 0) ; link -> linkat
             (if (= nr 88) (syscall-direct 36 c1 (- 0 100) c2 0 0 0)       ; symlink -> symlinkat
               (syscall-direct nr c1 c2 0 0 0 0)))))
       ;; struct stat layout shim: callers read fields at *x86_64* offsets
       ;; (the portable vocabulary).  arm64 swaps mode/nlink (arm64:
       ;; st_mode u32@16, st_nlink u32@20; x86_64: st_nlink u64@16,
       ;; st_mode u32@24).  Everything else relevant (dev/ino/uid/gid/
       ;; size/atime/mtime/ctime) sits at identical offsets.  Rewrite the
       ;; private 144B buffer to the x86_64 shape after the syscall.
       (defun nl_os_stat_fixup (rc buf)
         (if (< rc 0)
             rc
           (let* ((mode (ptr-read-u32 buf 16))
                  (nlink (ptr-read-u32 buf 20)))
             (seq
              (ptr-write-u64 buf 16 nlink)
              (ptr-write-u32 buf 24 mode)
              rc))))
       (defun nl_os_stat_path (cpath buf)
         (nl_os_stat_fixup
          (syscall-direct 79 (- 0 100) cpath buf 0 0 0) buf))           ; newfstatat
       (defun nl_os_lstat_path (cpath buf)
         (nl_os_stat_fixup
          (syscall-direct 79 (- 0 100) cpath buf 256 0 0) buf))         ; +AT_SYMLINK_NOFOLLOW
       (defun nl_os_readlink_path (cpath buf cap)
         (syscall-direct 78 (- 0 100) cpath buf cap 0 0))               ; readlinkat
       (defun nl_os_open_dir (cpath)
         (syscall-direct 56 (- 0 100) cpath 16384 0 0 0))               ; openat O_DIRECTORY=0x4000 on arm64
       (defun nl_os_getdents64 (fd dbuf cap)
         (syscall-direct 61 fd dbuf cap 0 0 0))
       (defun nl_os_utimes_path (cpath buf)
         (syscall-direct 88 (- 0 100) cpath buf 0 0 0))                 ; utimensat (timespec[2]; sec+0 compatible)
       (defun nl_os_statx_path (cpath flags buf)
         (syscall-direct 291 (- 0 100) cpath flags 4095 buf 0))
       (defun nl_os_nanosleep (ts)
         (syscall-direct 101 ts 0 0 0 0 0))))
    ('linux-x86_64
     '((defun nl_os_syscall_path (nr cpath) (syscall-direct nr cpath 0 0 0 0 0))
       (defun nl_os_syscall_path_int (nr cpath iarg) (syscall-direct nr cpath iarg 0 0 0 0))
       (defun nl_os_syscall_path2 (nr c1 c2) (syscall-direct nr c1 c2 0 0 0 0))
       (defun nl_os_stat_path (cpath buf) (syscall-direct 4 cpath buf 0 0 0 0))
       (defun nl_os_lstat_path (cpath buf) (syscall-direct 6 cpath buf 0 0 0 0))
       (defun nl_os_readlink_path (cpath buf cap) (syscall-direct 89 cpath buf cap 0 0 0))
       (defun nl_os_open_dir (cpath) (syscall-direct 257 (- 0 100) cpath 65536 0 0 0))
       (defun nl_os_getdents64 (fd dbuf cap) (syscall-direct 217 fd dbuf cap 0 0 0))
       (defun nl_os_utimes_path (cpath buf) (syscall-direct 235 cpath buf 0 0 0 0))
       (defun nl_os_statx_path (cpath flags buf) (syscall-direct 332 (- 0 100) cpath flags 4095 buf 0))
       (defun nl_os_nanosleep (ts) (syscall-direct 35 ts 0 0 0 0 0))))
    (_
     '((defun nl_os_syscall_path (_nr _cpath) (- 0 38))
       (defun nl_os_syscall_path_int (_nr _cpath _iarg) (- 0 38))
       (defun nl_os_syscall_path2 (_nr _c1 _c2) (- 0 38))
       (defun nl_os_stat_path (_cpath _buf) (- 0 38))
       (defun nl_os_lstat_path (_cpath _buf) (- 0 38))
       (defun nl_os_readlink_path (_cpath _buf _cap) (- 0 38))
       (defun nl_os_open_dir (_cpath) (- 0 38))
       (defun nl_os_getdents64 (_fd _dbuf _cap) (- 0 38))
       (defun nl_os_utimes_path (_cpath _buf) (- 0 38))
       (defun nl_os_statx_path (_cpath _flags _buf) (- 0 38))
       (defun nl_os_nanosleep (_ts) (- 0 38))))))

(defun nelisp-standalone--reader-os-source-forms ()
  "Return target-specific OS helper defuns used by the reader driver/file I/O."
  (append
   (nelisp-standalone--os-syscall-xlat-forms)
   (nelisp-standalone--reader-os-base-forms)))

(defun nelisp-standalone--reader-os-base-forms ()
  "Return the per-target base OS helper defuns (argv/file/process)."
  (pcase nelisp-standalone--target
    ((or 'windows-x86_64 'windows-aarch64)
     '((defun nl_win_wcs_utf8_dup (src)
         (let* ((n (extern-call WideCharToMultiByte 65001 0 src -1 0 0 0 0))
                (cap (if (< n 1) 1 n))
                (dst (alloc-bytes cap 1)))
           (if (< n 1)
               (nl_seq2 (ptr-write-u8 dst 0 0) dst)
             (nl_seq2
              (extern-call WideCharToMultiByte 65001 0 src -1 dst cap 0 0)
              dst))))
       (defun nl_win_utf8_wcs_dup (src)
         (let* ((n (extern-call MultiByteToWideChar 65001 0 src -1 0 0))
                (cap (if (< n 1) 1 n))
                (dst (alloc-bytes (* cap 2) 2)))
           (if (< n 1)
               (nl_seq2 (ptr-write-u16 dst 0 0) dst)
             (nl_seq2
              (extern-call MultiByteToWideChar 65001 0 src -1 dst cap)
              dst))))
       (defun nl_win_wargv_fill (wargv argc i sp)
         (if (= i argc)
             (nl_seq2 (ptr-write-u64 sp (* (+ i 1) 8) 0) sp)
           (let* ((warg (ptr-read-u64 wargv (* i 8)))
                  (carg (nl_win_wcs_utf8_dup warg)))
             (seq
              (ptr-write-u64 sp (* (+ i 1) 8) carg)
              (nl_win_wargv_fill wargv argc (+ i 1) sp)))))
       (defun nl_os_argv_init (sp)
         (let* ((argc_slot (alloc-bytes 8 8))
                (cmd (extern-call GetCommandLineW))
                (wargv (extern-call CommandLineToArgvW cmd argc_slot))
                (argc (if (= wargv 0) 1 (ptr-read-u32 argc_slot 0)))
                (argv (alloc-bytes (* (+ argc 2) 8) 8)))
           (if (= wargv 0)
               sp
             (seq
              (ptr-write-u64 argv 0 argc)
              (nl_win_wargv_fill wargv argc 0 argv)))))
       (defun nl_os_open_read (path)
         (let* ((wpath (nl_win_utf8_wcs_dup path)))
           (extern-call CreateFileW wpath 2147483648 1 0 3 128 0)))
       (defun nl_os_open_write_truncate (path)
         (let* ((wpath (nl_win_utf8_wcs_dup path)))
           (extern-call CreateFileW wpath 1073741824 0 0 2 128 0)))
       (defun nl_os_close_handle (h)
         (if (< h 0) 0 (extern-call CloseHandle h)))
       (defun nl_os_read_file_handle (h ptr len)
         (if (< h 0)
             -1
           (let* ((got (alloc-bytes 4 4))
                  (ok (extern-call ReadFile h ptr len got 0)))
             (if (= ok 0) -1 (ptr-read-u32 got 0)))))
       (defun nl_os_write_file_handle (h ptr len)
         (if (< h 0)
             -1
           (let* ((sent (alloc-bytes 4 4))
                  (ok (extern-call WriteFile h ptr len sent 0)))
             (if (= ok 0) -1 (ptr-read-u32 sent 0)))))
       (defun nl_os_read_file_cpath (path buf len)
         (let* ((h (nl_os_open_read path))
                (n (nl_os_read_file_handle h buf len)))
           (nl_seq2 (nl_os_close_handle h) n)))
       (defun nl_os_read_stdin (ptr len)
         (let* ((h (extern-call GetStdHandle 4294967286)))
           (nl_os_read_file_handle h ptr len)))
       (defun nl_os_write_stdout (ptr len)
         (let* ((h (extern-call GetStdHandle 4294967285))
                (sent (alloc-bytes 4 4))
                (ok (extern-call WriteFile h ptr len sent 0)))
           (if (= ok 0) -1 (ptr-read-u32 sent 0))))
       (defun nl_os_write_stderr (ptr len)
         (let* ((h (extern-call GetStdHandle 4294967284))
                (sent (alloc-bytes 4 4))
                (ok (extern-call WriteFile h ptr len sent 0)))
           (if (= ok 0) -1 (ptr-read-u32 sent 0))))
	       (defun nl_os_process_fork () -1)
	       (defun nl_os_process_execve (path argv envp) -1)
	       (defun nl_os_process_wait4 (pid statusp options) -1)
	       (defun nl_os_process_dup2 (oldfd newfd) -1)
	       (defun nl_os_process_pipe (pipev) -1)
	       (defun nl_os_process_set_nonblock (fd) -1)
	       (defun nl_os_process_kill (pid sig) -1)
	       (defun nl_os_process_exit127 () -1)
	       (defun nl_os_syscall_nr_getpid () -1)
	       (defun nl_os_syscall_nr_fork () -1)
	       (defun nl_os_syscall_nr_wait4 () -1)
	       (defun nl_os_syscall_nr_kill () -1)
	       (defun nl_os_syscall_nr_pipe () -1)
	       (defun nl_os_syscall_nr_read () -1)
	       (defun nl_os_syscall_nr_write () -1)
	       (defun nl_os_syscall_nr_close () -1)
	       (defun nl_os_syscall_nr_dup2 () -1)
	       (defun nl_os_syscall_nr_poll () -1)
	       (defun nl_os_syscall_nr_fcntl () -1)
	       (defun nl_os_syscall_nr_exit () -1)))
    ('macos-aarch64
     '((defun nl_darwin_skip_to_nul (ptr off)
         (if (= (ptr-read-u8 ptr off) 0)
             off
           (nl_darwin_skip_to_nul ptr (+ off 1))))
       (defun nl_darwin_skip_nuls (ptr off)
         (if (= (ptr-read-u8 ptr off) 0)
             (nl_darwin_skip_nuls ptr (+ off 1))
           off))
       (defun nl_darwin_procargs_next (ptr off)
         (nl_darwin_skip_nuls ptr (+ (nl_darwin_skip_to_nul ptr off) 1)))
       (defun nl_darwin_procargs_fill (argc buf off i argv)
         (if (= i argc)
             (nl_seq2 (ptr-write-u64 argv (* (+ i 1) 8) 0) argv)
           (seq
            (ptr-write-u64 argv (* (+ i 1) 8) (+ buf off))
            (nl_darwin_procargs_fill argc buf
                                      (nl_darwin_procargs_next buf off)
                                      (+ i 1) argv))))
       (defun nl_os_argv_init (sp)
         (let* ((mib (alloc-bytes 16 4))
                (lenp (alloc-bytes 8 8))
                (buf (alloc-bytes 65536 8))
                (pid (syscall-direct 20 0 0 0 0 0 0)))
           (seq
            (ptr-write-u32 mib 0 1)     ; CTL_KERN
            (ptr-write-u32 mib 4 49)    ; KERN_PROCARGS2
            (ptr-write-u32 mib 8 pid)
            (ptr-write-u64 lenp 0 65536)
            (let* ((rc (syscall-direct 202 mib 3 buf lenp 0 0)))
              (if (< rc 0)
                  sp
                (let* ((argc (ptr-read-u32 buf 0))
                       (off0 (nl_darwin_skip_nuls
                              buf (+ (nl_darwin_skip_to_nul buf 4) 1)))
                       (argv (alloc-bytes (* (+ argc 2) 8) 8)))
                  (seq
                   (ptr-write-u64 argv 0 argc)
                   (nl_darwin_procargs_fill argc buf off0 0 argv))))))))
       (defun nl_os_open_read (path)
         (syscall-direct 5 path 0 0 0 0 0))
       (defun nl_os_open_write_truncate (path)
         (syscall-direct 5 path 1537 420 0 0 0))
       (defun nl_os_close_handle (fd)
         (syscall-direct 6 fd 0 0 0 0 0))
       (defun nl_os_read_file_handle (fd ptr len)
         (if (< fd 0) -1 (syscall-direct 3 fd ptr len 0 0 0)))
       (defun nl_os_write_file_handle (fd ptr len)
         (if (< fd 0)
             -1
           (syscall-direct 4 fd ptr len 0 0 0)))
       (defun nl_os_read_file_cpath (path buf len)
         (let* ((fd (nl_os_open_read path))
                (n (nl_os_read_file_handle fd buf len)))
           (nl_seq2 (nl_os_close_handle fd) n)))
       (defun nl_os_read_stdin (ptr len)
         (syscall-direct 3 0 ptr len 0 0 0))
       (defun nl_os_write_stdout (ptr len)
         (syscall-direct 4 1 ptr len 0 0 0))
       (defun nl_os_write_stderr (ptr len)
         (syscall-direct 4 2 ptr len 0 0 0))
       (defun nl_os_process_fork ()
         (syscall-direct 2 0 0 0 0 0 0))
	       (defun nl_os_process_execve (path argv envp)
	         (syscall-direct 59 path argv envp 0 0 0))
	       (defun nl_os_process_wait4 (pid statusp options)
	         (syscall-direct 7 pid statusp options 0 0 0))
	       (defun nl_os_process_dup2 (oldfd newfd)
	         (syscall-direct 90 oldfd newfd 0 0 0 0))
	       (defun nl_os_process_pipe (pipev)
	         (syscall-direct 42 pipev 0 0 0 0 0))
	       (defun nl_os_process_set_nonblock (fd)
	         (syscall-direct 92 fd 4 4 0 0 0))
	       (defun nl_os_process_kill (pid sig)
	         (syscall-direct 37 pid sig 0 0 0 0))
	       (defun nl_os_process_exit127 ()
	         (syscall-direct 1 127 0 0 0 0 0))
	       (defun nl_os_syscall_nr_getpid () 20)
	       (defun nl_os_syscall_nr_fork () 2)
	       (defun nl_os_syscall_nr_wait4 () 7)
	       (defun nl_os_syscall_nr_kill () 37)
	       (defun nl_os_syscall_nr_pipe () 42)
	       (defun nl_os_syscall_nr_read () 3)
	       (defun nl_os_syscall_nr_write () 4)
	       (defun nl_os_syscall_nr_close () 6)
	       (defun nl_os_syscall_nr_dup2 () 90)
	       (defun nl_os_syscall_nr_poll () 230)
	       (defun nl_os_syscall_nr_fcntl () 92)
	       (defun nl_os_syscall_nr_exit () 1)))
    ('linux-aarch64
     ;; arm64 Linux has no legacy x86 syscalls: open/dup2/pipe/fork/poll are
     ;; absent — use openat(56, AT_FDCWD=-100)/dup3(24)/pipe2(59)/clone(220,
     ;; flags=SIGCHLD)/ppoll(73).  Entry stack already has the Linux
     ;; argc/argv shape, so argv init is the identity, same as x86_64.
     '((defun nl_os_argv_init (sp) sp)
       (defun nl_os_open_read (path)
         (syscall-direct 56 -100 path 0 0 0 0))
       (defun nl_os_open_write_truncate (path)
         (syscall-direct 56 -100 path 577 420 0 0))
       (defun nl_os_close_handle (fd)
         (syscall-direct 57 fd 0 0 0 0 0))
       (defun nl_os_read_file_handle (fd ptr len)
         (if (< fd 0) -1 (syscall-direct 63 fd ptr len 0 0 0)))
       (defun nl_os_write_file_handle (fd ptr len)
         (if (< fd 0)
             -1
           (syscall-direct 64 fd ptr len 0 0 0)))
       (defun nl_os_read_file_cpath (path buf len)
         (let* ((fd (nl_os_open_read path))
                (n (nl_os_read_file_handle fd buf len)))
           (nl_seq2 (nl_os_close_handle fd) n)))
       (defun nl_os_read_stdin (ptr len)
         (syscall-direct 63 0 ptr len 0 0 0))
       (defun nl_os_write_stdout (ptr len)
         (syscall-direct 64 1 ptr len 0 0 0))
       (defun nl_os_write_stderr (ptr len)
         (syscall-direct 64 2 ptr len 0 0 0))
       (defun nl_os_process_fork ()
         (syscall-direct 220 17 0 0 0 0 0))   ; clone(SIGCHLD,...)
       (defun nl_os_process_execve (path argv envp)
         (syscall-direct 221 path argv envp 0 0 0))
       (defun nl_os_process_wait4 (pid statusp options)
         (syscall-direct 260 pid statusp options 0 0 0))
       (defun nl_os_process_dup2 (oldfd newfd)
         (syscall-direct 24 oldfd newfd 0 0 0 0))  ; dup3(old,new,0)
       (defun nl_os_process_pipe (pipev)
         (syscall-direct 59 pipev 0 0 0 0 0))      ; pipe2(pipev,0)
       (defun nl_os_process_set_nonblock (fd)
         (syscall-direct 25 fd 4 2048 0 0 0))      ; fcntl(F_SETFL,O_NONBLOCK)
       (defun nl_os_process_kill (pid sig)
         (syscall-direct 129 pid sig 0 0 0 0))
       (defun nl_os_process_exit127 ()
         (syscall-direct 93 127 0 0 0 0 0))
       (defun nl_os_syscall_nr_getpid () 172)
       (defun nl_os_syscall_nr_fork () 220)
       (defun nl_os_syscall_nr_wait4 () 260)
       (defun nl_os_syscall_nr_kill () 129)
       (defun nl_os_syscall_nr_pipe () 59)
       (defun nl_os_syscall_nr_read () 63)
       (defun nl_os_syscall_nr_write () 64)
       (defun nl_os_syscall_nr_close () 57)
       (defun nl_os_syscall_nr_dup2 () 24)
       (defun nl_os_syscall_nr_poll () 73)         ; ppoll — caller must pass a timespec, not ms
       (defun nl_os_syscall_nr_fcntl () 25)
       (defun nl_os_syscall_nr_exit () 93)))
    (_
     '((defun nl_os_argv_init (sp) sp)
       (defun nl_os_open_read (path)
         (syscall-direct 2 path 0 0 0 0 0))
       (defun nl_os_open_write_truncate (path)
         (syscall-direct 2 path 577 420 0 0 0))
       (defun nl_os_close_handle (fd)
         (syscall-direct 3 fd 0 0 0 0 0))
       (defun nl_os_read_file_handle (fd ptr len)
         (if (< fd 0) -1 (syscall-direct 0 fd ptr len 0 0 0)))
       (defun nl_os_write_file_handle (fd ptr len)
         (if (< fd 0)
             -1
           (syscall-direct 1 fd ptr len 0 0 0)))
       (defun nl_os_read_file_cpath (path buf len)
         (let* ((fd (nl_os_open_read path))
                (n (nl_os_read_file_handle fd buf len)))
           (nl_seq2 (nl_os_close_handle fd) n)))
       (defun nl_os_read_stdin (ptr len)
         (syscall-direct 0 0 ptr len 0 0 0))
       (defun nl_os_write_stdout (ptr len)
         (syscall-direct 1 1 ptr len 0 0 0))
       (defun nl_os_write_stderr (ptr len)
         (syscall-direct 1 2 ptr len 0 0 0))
       (defun nl_os_process_fork ()
         (syscall-direct 57 0 0 0 0 0 0))
	       (defun nl_os_process_execve (path argv envp)
	         (syscall-direct 59 path argv envp 0 0 0))
	       (defun nl_os_process_wait4 (pid statusp options)
	         (syscall-direct 61 pid statusp options 0 0 0))
	       (defun nl_os_process_dup2 (oldfd newfd)
	         (syscall-direct 33 oldfd newfd 0 0 0 0))
	       (defun nl_os_process_pipe (pipev)
	         (syscall-direct 22 pipev 0 0 0 0 0))
	       (defun nl_os_process_set_nonblock (fd)
	         (syscall-direct 72 fd 4 2048 0 0 0))
	       (defun nl_os_process_kill (pid sig)
	         (syscall-direct 62 pid sig 0 0 0 0))
	       (defun nl_os_process_exit127 ()
	         (syscall-direct 60 127 0 0 0 0 0))
	       (defun nl_os_syscall_nr_getpid () 39)
	       (defun nl_os_syscall_nr_fork () 57)
	       (defun nl_os_syscall_nr_wait4 () 61)
	       (defun nl_os_syscall_nr_kill () 62)
	       (defun nl_os_syscall_nr_pipe () 22)
	       (defun nl_os_syscall_nr_read () 0)
	       (defun nl_os_syscall_nr_write () 1)
	       (defun nl_os_syscall_nr_close () 3)
	       (defun nl_os_syscall_nr_dup2 () 33)
	       (defun nl_os_syscall_nr_poll () 7)
	       (defun nl_os_syscall_nr_fcntl () 72)
	       (defun nl_os_syscall_nr_exit () 60)))))

(defun nelisp-standalone--reader-driver-source ()
  "DUAL-MODE reader driver (M7 file-load + M8 multi-form loop).
Takes the entry stack pointer SP; reads argv[1] = (ptr-read-u64 sp 16):
  argv[1] == 0          start the REPL;
  argv[1] == --embedded use the embedded NELISP_SRC via `sexp-write-str-lit';
  otherwise             open+read argv[1] through target OS helpers and
                        wrap the bytes via `nl_alloc_str'.
Then the same multi-form parse+eval loop runs `src'.  argc==1 means
argv[1]==NULL==0 (argv is NULL-terminated), so the check is reliable.
Each builtin name installs through a fresh, full-length arena buffer so
`nl_install_one' never aliases a reused buffer and >8-byte names install
correctly."
  `(seq
    ,@(nelisp-standalone--reader-os-source-forms)
    ;; flat-arena cold-loader gate (default OFF).  The marker path is also the
    ;; image file: if it opens, cold-load is requested; if not, normal boot.
    (defun nl_cold_marker_cpath ()
      (let ((b (alloc-bytes 32 1)))
        (seq (ptr-write-u64 b 0 7810770278772732975)
             (ptr-write-u64 b 8 7236281173032334185)
             (ptr-write-u64 b 16 7074703559336225069)
             (ptr-write-u64 b 24 28265)
             b)))
    ;; Apply the relocation table to the freshly-loaded chunk-0 region (at DS):
    ;; each field at DS+F holds an image offset O; relocate to DS+O (chunk-0
    ;; target) or IB+(O-slen) (interned target).
    ;; Clear GC mark bits in the loaded chunk-0 region: the dumped image keeps
    ;; whatever mark state it had, but a fresh runtime expects mark=0 (post-sweep)
    ;; -- stale marks make the first GC mis-handle live objects -> SIGSEGV.
    (defun nl_cold_clear_marks (ds end)
      (let ((hdr ds))
        (seq
         (while (and (> hdr 0) (< hdr end))
           (let* ((h (ptr-read-u64 hdr 0)) (bt (- h (logand h 7))))
             (if (< bt 8) (setq hdr end)
               (nl_seq2 (ptr-write-u64 hdr 0 bt) (setq hdr (+ hdr bt))))))
         0)))
    (defun nl_cold_reloc (tbl tlen ds slen ib)
      (let ((i 0))
        (seq
         (while (< i tlen)
           (let* ((f (ptr-read-u64 (+ tbl (* i 8)) 0))
                  (o (ptr-read-u64 (+ ds f) 0)))
             (nl_seq2
              (if (< o slen)
                  (ptr-write-u64 (+ ds f) 0 (+ ds o))
                (ptr-write-u64 (+ ds f) 0 (+ ib (- o slen))))
              (setq i (+ i 1)))))
         0)))
    ;; Relocate the open-addressing intern table's name-buffer pointers.  The
    ;; table is the first 16 MiB of the interned region (2^20 slots * 16 B):
    ;; slot+0 = len+1 (0 = empty), slot+8 = absolute pointer into the name
    ;; buffer at [base+16MiB, ...).  These pointer fields live INSIDE the
    ;; interned region, so the chunk-0-only relocation table never touches
    ;; them; they keep the DUMPING run's base.  Add the base delta
    ;; (new_ib - old_ib) to every occupied slot so each name pointer lands in
    ;; THIS process's name buffer.  Without this, the first symbol intern in
    ;; boot (install-builtins) probes the table, derefs slot+8 = a stale
    ;; previous-process address, and SIGSEGVs.  OLD_IB comes from header+56.
    (defun nl_cold_reloc_intern (ib oldib)
      (let ((idx 0))
        (seq
         (while (< idx 1048576)
           (let ((slot (+ ib (* idx 16))))
             (nl_seq2
              (if (= (ptr-read-u64 slot 0) 0) 0
                (ptr-write-u64 slot 8 (+ (ptr-read-u64 slot 8) (- ib oldib))))
              (setq idx (+ idx 1)))))
         0)))
    ;; Zero N bytes from START (8-byte stride; N is a multiple of 8).  Used to
    ;; scrub the dead relocation table out of chunk-0 before the bump cursor is
    ;; re-armed over it.  CRITICAL: the table was read into [ds+slen, ds+slen+
    ;; tlen*8) and the cursor is set to 1024+slen, so the FIRST post-load allocs
    ;; (globals/frames/unbound/ctx/pool/...) land on top of those bytes.  A fresh
    ;; mmap arena hands out zeroed pages and the boot constructors rely on it
    ;; (they write only the fields they care about, assuming the rest is 0 -- the
    ;; same invariant `nl_alloc_zero_fill' restores for reused blocks).  Without
    ;; this scrub, e.g. EvalCtx+112 (the use-elisp flags word) inherits a stale
    ;; table byte -> garbage flags -> eval misbehaves (args evaluate to nil).
    (defun nl_cold_zero (start n)
      (let ((i 0))
        (seq
         (while (< i n) (nl_seq2 (ptr-write-u64 (+ start i) 0 0) (setq i (+ i 8))))
         0)))
    ;; Load the cold image into the LIVE arena.  Run BEFORE the driver allocates
    ;; globals/etc. (so they land after the image).  Reads {header|table|regions}
    ;; via the OS helpers directly into final/scratch arena locations (no
    ;; alloc-bytes -- that would bump chunk-0 into the region we overwrite):
    ;;   header -> intern base (temp; the interned region overwrites it later),
    ;;   table  -> DS+slen (chunk-0 scratch above the live region),
    ;;   chunk-0 region -> DS, interned region -> intern base.
    ;; Then split-relocate, set the bump cursor (base+0 = 1024+slen) and the
    ;; interned bump (+840 = ib+isz).  Returns 1 if loaded, -1 if no marker.
    (defun nl_cold_load_arena ()
      (let ((fd (nl_os_open_read (nl_cold_marker_cpath))))
        (if (< fd 0) -1
          (let* ((base (ptr-read-u64 (data-addr nl_arena_base) 0))
                 (ds (+ base 1024))
                 (ib (ptr-read-u64 (+ base 832) 0)))
            (seq
             (nl_fa_read_all fd ib 64 0)
             (let* ((slen (ptr-read-u64 ib 8))
                    (isz (ptr-read-u64 ib 16))
                    (tlen (ptr-read-u64 ib 24))
                    ;; capture the dumping run's intern base BEFORE the interned
                    ;; region read below overwrites the header sitting in `ib'.
                    (oldib (ptr-read-u64 ib 56))
                    (tbl (+ ds slen)))
               (seq
                (nl_fa_read_all fd tbl (* tlen 8) 0)
                (nl_fa_read_all fd ds slen 0)
                (nl_fa_read_all fd ib isz 0)
                (nl_os_close_handle fd)
                (nl_cold_reloc tbl tlen ds slen ib)
                (nl_cold_reloc_intern ib oldib)
                ;; scrub the now-dead relocation table so the bump cursor (set to
                ;; 1024+slen below) re-arms over ZEROED memory -- matching the
                ;; fresh-mmap invariant the boot constructors depend on.
                (nl_cold_zero tbl (* tlen 8))
                (nl_cold_clear_marks ds (+ ds slen))
                (ptr-write-u64 base 0 (+ 1024 slen))
                (ptr-write-u64 (+ base 840) 0 (+ ib isz))
                ;; push the GC next-trigger far out so a collection does not fire
                ;; on the freshly-loaded (already-live) image during early eval.
                (ptr-write-u64 (+ base 104) 0 (+ (+ 1024 slen) 1073741824))
                1)))))))
    ;; cold path globals install: re-read the header for globals_off and point
    ;; the GLOBALS slot at the loaded globals Record (tag 12, box = DS + goff).
    ;; Frames/unbound keep the fresh ones from `nl_bootstrap_make_mirror'.
    (defun nl_cold_overwrite_globals (globals)
      (let ((fd (nl_os_open_read (nl_cold_marker_cpath))))
        (if (< fd 0) 0
          (let ((hdr (alloc-bytes 64 8)))
            (seq
             (nl_fa_read_all fd hdr 64 0)
             (nl_os_close_handle fd)
             (let* ((base (ptr-read-u64 (data-addr nl_arena_base) 0))
                    (ds (+ base 1024))
                    (goff (ptr-read-u64 hdr 32)))
               (seq
                (ptr-write-u8 globals 0 12)
                (ptr-write-u64 globals 8 (+ ds goff))
                (ptr-write-u64 globals 16 0)
                (ptr-write-u64 globals 24 0)
                1)))))))
    (defun nl_cstr_len_loop (ptr n)
      (if (= (ptr-read-u8 ptr n) 0)
          n
        (nl_cstr_len_loop ptr (+ n 1))))
    (defun nl_cstr_len (ptr)
      (if (= ptr 0) 0 (nl_cstr_len_loop ptr 0)))
    (defun nl_cstr_starts_dash_p (ptr)
      (if (= ptr 0) 0
        (if (= (ptr-read-u8 ptr 0) 45) 1 0)))
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_dump_runtime_image
                                       "dump-runtime-image")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_extend_runtime_image
                                       "extend-runtime-image")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_eval_runtime_image
                                       "eval-runtime-image")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_exec_runtime_image
                                       "exec-runtime-image")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_runtime_image_cache_kind
                                       "--cache-kind")
	    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_compile_elisp_artifact
	                                       "compile-elisp-artifact")
	    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_compile_elisp_artifacts
	                                       "compile-elisp-artifacts")
	    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_compile_runtime_image
	                                       "compile-runtime-image")
	    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_audit_elisp_artifacts
	                                       "audit-elisp-artifacts")
	    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_exec_elisp_artifact
                                       "exec-elisp-artifact")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_eval_elisp_artifact
                                       "eval-elisp-artifact")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_load_elisp_source
                                       "load-elisp-source")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_eval_elisp_source
                                       "eval-elisp-source")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_native_exec_elisp_artifact
                                       "native-exec-elisp-artifact")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_inspect_elisp_artifact
                                       "inspect-elisp-artifact")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_repl
                                       "--repl")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_eval
                                       "--eval")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_load
                                       "--load")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_neln_selftest
                                       "--neln-selftest")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_embedded
                                       "--embedded")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_help
                                       "--help")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_no_prompt
                                       "--no-prompt")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_no_print
                                       "--no-print")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_bare_eval
                                       "eval")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_bare_load
                                       "load")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_bare_repl
                                       "repl")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_bare_help
                                       "help")
    ,(nelisp-standalone--cstr-eq-defun 'nl_cstr_eq_double_dash_load
                                       "--load")
    ,(nelisp-standalone--copy-lit-defun
      'nl_runtime_image_eval_prefix
      "(let ((v (progn\n")
    ,(nelisp-standalone--copy-lit-defun
      'nl_runtime_image_eval_suffix
      "))) (nelisp--write-stdout-bytes (nelisp--repr v)) (nelisp--write-stdout-bytes (unibyte-string 10)) 0)\n0\n")
    ,(nelisp-standalone--copy-lit-defun
      'nl_runtime_image_success_suffix
      "0\n")
    ,(nelisp-standalone--copy-lit-defun
      'nl_artifact_runtime_cache_path
      nelisp-standalone--artifact-runtime-cache-path)
    ,(nelisp-standalone--copy-lit-defun
      'nl_artifact_runtime_cache_enable_path
      nelisp-standalone--artifact-runtime-cache-enable-path)
    ,(nelisp-standalone--copy-lit-defun
      'nl_audit_fast_sh_path
      "/bin/sh")
    ,(nelisp-standalone--copy-lit-defun
      'nl_audit_fast_script_path
      (expand-file-name "tools/nelisp-audit-fast.sh"
                        nelisp-standalone--repo-root))
    (defun nl_artifact_runtime_cache_exists_p (fbuf)
      (let* ((off (nl_artifact_runtime_cache_path fbuf 0))
             (n (nl_os_read_file_cpath fbuf fbuf 1)))
        (if (< n 0) 0 1)))
    (defun nl_artifact_runtime_cache_enabled_p (fbuf)
      (let* ((off (nl_artifact_runtime_cache_enable_path fbuf 0))
             (n (nl_os_read_file_cpath fbuf fbuf 1)))
        (if (< n 0) 0 1)))
    (defun nl_audit_fast_argv_copy (argc sp src-index dst-index argv)
      (if (= src-index argc)
          (nl_seq2 (ptr-write-u64 argv (* dst-index 8) 0) argv)
        (seq
         (ptr-write-u64 argv (* dst-index 8)
                        (ptr-read-u64 sp (* (+ src-index 1) 8)))
         (nl_audit_fast_argv_copy argc sp (+ src-index 1)
                                  (+ dst-index 1) argv))))
    (defun nl_audit_fast_run (argc sp shifted-p fbuf)
      (let* ((cmd-index (if (= shifted-p 1) 0 1))
             (arg-start (+ cmd-index 1))
             (arg-count (- argc arg-start))
             (argv (alloc-bytes (* (+ arg-count 3) 8) 8))
             (shptr fbuf)
             (off (nl_audit_fast_sh_path fbuf 0))
             (scriptptr (+ fbuf (+ off 1)))
             (off2 (nl_audit_fast_script_path fbuf (+ off 1)))
             (envp (if (= (ptr-read-u64 268435600 0) 0)
                       (alloc-bytes 8 8)
                     (ptr-read-u64 268435600 0)))
             (pid 0))
        (seq
         (ptr-write-u8 fbuf off 0)
         (ptr-write-u8 fbuf off2 0)
         (if (= (ptr-read-u64 268435600 0) 0)
             (ptr-write-u64 envp 0 0)
           0)
         (ptr-write-u64 argv 0 shptr)
         (ptr-write-u64 argv 8 scriptptr)
         (nl_audit_fast_argv_copy argc sp arg-start 2 argv)
         (setq pid (nl_os_process_fork))
         (if (= pid 0)
             (seq
              (nl_os_process_execve shptr argv envp)
              (nl_os_process_exit127))
           (if (< pid 0)
               1
             (nl_bi_process_wait_exit_code pid))))))
    ,(nelisp-standalone--copy-lit-defun
      'nl_runtime_image_dump_prefix
      ";;; nelisp-runtime-image source-v1\n(progn\n")
    ,(nelisp-standalone--copy-lit-defun
      'nl_runtime_image_dump_suffix
      ")\n")
    ,(nelisp-standalone--copy-lit-defun
      'nl_cli_eval_prefix
      "(let ((v (progn\n")
	    ,(nelisp-standalone--copy-lit-defun
	      'nl_cli_eval_suffix
	      "))) (nelisp--write-stdout-bytes (nelisp--repr v)) (nelisp--write-stdout-bytes (unibyte-string 10)) 0)\n0\n")
    ,(nelisp-standalone--copy-lit-defun
      'nl_cli_help_text
	      "Usage: nelisp [--help] [--repl [--no-prompt] [--no-print]] [--eval EXPR] [--load FILE] [--neln-selftest] [FILE]\nArguments:\n  --help                         Show this argument list\n  --eval EXPR                    Evaluate EXPR and print the value\n  --load FILE                    Load FILE and print the last value\n  --neln-selftest                Run the embedded native exec self-test\n  --repl [--no-prompt] [--no-print]\n                                 Start the REPL\n  FILE                           Load FILE as a source file\nCommands:\n  dump-runtime-image FILE [--load SRC]... FORM...\n  extend-runtime-image IMAGE OUT [--load SRC]... FORM...\n  eval-runtime-image IMAGE FORM...\n  exec-runtime-image IMAGE FORM...\n  compile-runtime-image --kind nelc|neln|auto --input FILE.nlri --output FILE\n  compile-elisp-artifact --kind nelc|neln|elc --input FILE.el --output FILE\n  compile-elisp-artifacts --kind nelc|neln|auto FILE.el|DIR...\n  audit-elisp-artifacts [--required] FILE.el|FILE.neln|DIR...\n  exec-elisp-artifact FILE.nelc|FILE.neln|FILE.elc FORM...\n  eval-elisp-artifact FILE.nelc|FILE.neln|FILE.elc FORM...\n  load-elisp-source [--auto-compile] [--kind nelc|neln] FILE.el\n  eval-elisp-source [--auto-compile] [--kind nelc|neln] FILE.el FORM...\n  native-exec-elisp-artifact FILE.neln SYMBOL ARG...\n  inspect-elisp-artifact FILE.nelc|FILE.neln|FILE.elc\n")
    ,(nelisp-standalone--copy-lit-defun
      'nl_repl_eval_prefix
      "(let ((v (progn\n")
    ,(nelisp-standalone--copy-lit-defun
      'nl_repl_eval_suffix
      (nelisp-standalone--reader-repl-eval-suffix))
    ,(nelisp-standalone--copy-lit-defun
      'nl_repl_eval_no_print_suffix
      "))) v)\n")
    ,(nelisp-standalone--copy-lit-defun
      'nl_repl_prompt
      "nelisp> ")
    ,@(nelisp-standalone--copy-lit-u64-defuns
      'nl_repl_prelude_source
      (nelisp-standalone--reader-repl-prelude-source))
    (defun nl_runtime_image_command_p (ptr)
      (if (= (nl_cstr_eq_dump_runtime_image ptr) 1)
          1
        (if (= (nl_cstr_eq_extend_runtime_image ptr) 1)
            1
          (if (= (nl_cstr_eq_eval_runtime_image ptr) 1)
              1
            (nl_cstr_eq_exec_runtime_image ptr)))))
    (defun nl_artifact_command_p (ptr)
      (if (= (nl_cstr_eq_compile_elisp_artifact ptr) 1)
          1
	        (if (= (nl_cstr_eq_compile_elisp_artifacts ptr) 1)
	            1
	          (if (= (nl_cstr_eq_compile_runtime_image ptr) 1)
	              1
	            (if (= (nl_cstr_eq_audit_elisp_artifacts ptr) 1)
	                1
	              (if (= (nl_cstr_eq_exec_elisp_artifact ptr) 1)
	                  1
	                (if (= (nl_cstr_eq_eval_elisp_artifact ptr) 1)
	                    1
	                  (if (= (nl_cstr_eq_load_elisp_source ptr) 1)
	                      1
	                    (if (= (nl_cstr_eq_eval_elisp_source ptr) 1)
	                        1
	                      (if (= (nl_cstr_eq_native_exec_elisp_artifact ptr) 1)
	                          1
	                        (nl_cstr_eq_inspect_elisp_artifact ptr)))))))))))
    (defun nl_artifact_runtime_cache_command_p (ptr)
      (if (= (nl_cstr_eq_load_elisp_source ptr) 1)
          1
        (nl_cstr_eq_eval_elisp_source ptr)))
    (defun nl_artifact_source_cache_command_p (ptr)
      (if (= (nl_cstr_eq_load_elisp_source ptr) 1)
          1
        (nl_cstr_eq_eval_elisp_source ptr)))
    (defun nl_runtime_image_eval_exec_command_p (ptr)
      (if (= (nl_cstr_eq_eval_runtime_image ptr) 1)
          1
        (nl_cstr_eq_exec_runtime_image ptr)))
    (defun nl_runtime_image_cache_eval_p (sp argc)
      (if (> argc 4)
          (nl_cstr_eq_runtime_image_cache_kind (ptr-read-u64 sp 32))
        0))
    (defun nl_cli_eval_command_p (ptr)
      (nl_cstr_eq_eval ptr))
    (defun nl_cli_help_command_p (ptr)
      (nl_cstr_eq_help ptr))
    (defun nl_cli_command_p (ptr)
      (if (= (nl_cli_help_command_p ptr) 1)
          1
        (if (= (nl_cli_eval_command_p ptr) 1)
            1
          (if (= (nl_cstr_eq_load ptr) 1)
              1
            (if (= (nl_cstr_eq_neln_selftest ptr) 1)
                1
              (if (= (nl_cstr_eq_repl ptr) 1)
                  1
                (if (= (nl_cstr_eq_embedded ptr) 1)
                    1
                  (if (= (nl_runtime_image_command_p ptr) 1)
                      1
                    (nl_artifact_command_p ptr)))))))))
    (defun nl_cli_bare_legacy_command_p (ptr)
      (if (= (nl_cstr_eq_bare_eval ptr) 1)
          1
        (if (= (nl_cstr_eq_bare_load ptr) 1)
            1
          (if (= (nl_cstr_eq_bare_repl ptr) 1)
              1
            (nl_cstr_eq_bare_help ptr)))))
    (defun nl_cli_argv_shifted_p (argc slot0 slot1)
      (if (> argc 1)
          (if (= slot1 0)
              1
            (nl_cli_command_p slot0))
        0))
    (defun nl_cli_write_help (fbuf)
      (let* ((n (nl_cli_help_text fbuf 0)))
        (nl_os_write_stdout fbuf n)))
    (defun nl_cli_one_arg_p (arg2 arg3)
      (if (= arg2 0) 0
        (if (= arg3 0) 1 0)))
    (defun nl_cstr_copy_into (src dst off)
      (if (= src 0)
          off
        (let* ((ch (ptr-read-u8 src 0)))
          (if (= ch 0)
              off
            (seq
             (ptr-write-u8 dst off ch)
             (nl_cstr_copy_into (+ src 1) dst (+ off 1)))))))
    (defun nl_runtime_image_copy_argv_forms (sp argc i fbuf off)
      (if (= i argc)
          off
        (let* ((form_ptr (ptr-read-u64 sp (* (+ i 1) 8))))
          (if (= (nl_cstr_eq_double_dash_load form_ptr) 1)
              (if (< (+ i 1) argc)
                  (let* ((file_ptr (ptr-read-u64 sp (* (+ i 2) 8)))
                         (n (nl_os_read_file_cpath
                             file_ptr
                             (+ fbuf off)
                             (- ,nelisp-standalone--reader-read-cap off))))
                    (if (< n 0)
                        off
                      (seq
                       (setq off (+ off n))
                       (ptr-write-u8 fbuf off 10)
                       (nl_runtime_image_copy_argv_forms
                        sp argc (+ i 2) fbuf (+ off 1)))))
                off)
            (seq
             (setq off (nl_cstr_copy_into form_ptr fbuf off))
             (ptr-write-u8 fbuf off 10)
             (nl_runtime_image_copy_argv_forms sp argc (+ i 1) fbuf (+ off 1)))))))
    (defun nl_runtime_image_dump_source (sp argc fbuf)
      (let* ((off (nl_runtime_image_dump_prefix fbuf 0)))
        (seq
         (setq off (nl_runtime_image_copy_argv_forms sp argc 3 fbuf off))
         (nl_runtime_image_dump_suffix fbuf off))))
    (defun nl_runtime_image_write_dump (sp argc fbuf)
      (if (> argc 2)
          (let* ((image_path (ptr-read-u64 sp 24))
                 (len (nl_runtime_image_dump_source sp argc fbuf))
                 (h (nl_os_open_write_truncate image_path))
                 (n (nl_os_write_file_handle h fbuf len)))
            (nl_seq2 (nl_os_close_handle h)
                     (if (= n len) 0 1)))
        (seq (nl_cli_write_help fbuf) 2)))
    (defun nl_runtime_image_read_image_into_source_buf (sp fbuf)
      (let* ((image_path (ptr-read-u64 sp 24))
             (n (nl_os_read_file_cpath
                 image_path fbuf
                 ,nelisp-standalone--reader-read-cap)))
        (if (< n 0) 0 n)))
    (defun nl_runtime_image_exec_source (sp argc fbuf src)
      (let* ((off (nl_runtime_image_read_image_into_source_buf sp fbuf)))
        (seq
         (ptr-write-u8 fbuf off 10)
         (setq off (+ off 1))
         (setq off (nl_runtime_image_copy_argv_forms sp argc 3 fbuf off))
         (setq off (nl_runtime_image_success_suffix fbuf off))
         (nl_alloc_str fbuf off src))))
	    (defun nl_runtime_image_eval_source (sp argc fbuf src)
	      (let* ((off (nl_runtime_image_read_image_into_source_buf sp fbuf)))
	        (seq
	         (ptr-write-u8 fbuf off 10)
	         (setq off (+ off 1))
	         (setq off (nl_runtime_image_eval_prefix fbuf off))
	         (setq off (nl_runtime_image_copy_argv_forms sp argc 3 fbuf off))
	         (setq off (nl_runtime_image_eval_suffix fbuf off))
	         (nl_alloc_str fbuf off src))))
    (defun nl_cli_wrap_source_at (fbuf off src)
      (seq
       (ptr-write-u8 fbuf off 10)
       (setq off (+ off 1))
       (setq off (nl_cli_eval_suffix fbuf off))
       (nl_alloc_str fbuf off src)))
    (defun nl_cli_eval_source (form_ptr fbuf src)
      (let* ((off (nl_cli_eval_prefix fbuf 0)))
        (seq
         (setq off (nl_cstr_copy_into form_ptr fbuf off))
         (nl_cli_wrap_source_at fbuf off src))))
    (defun nl_cli_load_source (path_ptr fbuf src)
      (let* ((n (nl_os_read_file_cpath
                 path_ptr fbuf
                 ,nelisp-standalone--reader-read-cap)))
        (if (< n 0)
            0
          (seq (nl_alloc_str fbuf n src) 1))))
    (defun nl_cli_put_byte (fbuf off b)
      (seq (ptr-write-u8 fbuf off b) (+ off 1)))
    (defun nl_cli_put_udec (fbuf off v)
      (if (< v 10)
          (nl_cli_put_byte fbuf off (+ 48 v))
        (let* ((off2 (nl_cli_put_udec fbuf off (/ v 10))))
          (nl_cli_put_byte fbuf off2 (+ 48 (mod v 10))))))
    (defun nl_cli_put_dec (fbuf off v)
      (if (< v 0)
          (nl_cli_put_udec fbuf (nl_cli_put_byte fbuf off 45) (- 0 v))
        (nl_cli_put_udec fbuf off v)))
    (defun nl_cli_put_raw_bytes (src fbuf i n off)
      (if (= i n)
          off
        (nl_cli_put_raw_bytes
         src fbuf (+ i 1) n
         (nl_cli_put_byte fbuf off (ptr-read-u8 src i)))))
    (defun nl_cli_put_string_value (fbuf off sx quoted)
      (let* ((off2 (if (= quoted 1) (nl_cli_put_byte fbuf off 34) off))
             (off3 (nl_cli_put_raw_bytes (nl_bi_strptr sx) fbuf 0
                                         (nl_bi_strlen sx) off2)))
        (if (= quoted 1)
            (nl_cli_put_byte fbuf off3 34)
          off3)))
    (defun nl_cli_put_list_tail (fbuf off node first)
      (let* ((tag (ptr-read-u64 node 0)))
        (if (= tag 7)
            (let* ((off2 (if (= first 1)
                             off
                           (nl_cli_put_byte fbuf off 32)))
                   (off3 (nl_cli_value_to_buf fbuf off2
                                              (nl_cons_car_ptr node))))
              (nl_cli_put_list_tail fbuf off3 (nl_cons_cdr_ptr node) 0))
          (if (= tag 0)
              (nl_cli_put_byte fbuf off 41)
            (nl_cli_put_byte
             fbuf
             (nl_cli_value_to_buf
              fbuf
              (nl_cli_put_byte
               fbuf (nl_cli_put_byte fbuf (nl_cli_put_byte fbuf off 32) 46)
               32)
              node)
             41)))))
    (defun nl_cli_put_vector_loop (fbuf off vec i n)
      (if (= i n)
          (nl_cli_put_byte fbuf off 93)
        (let* ((off2 (if (= i 0)
                         off
                       (nl_cli_put_byte fbuf off 32)))
               (off3 (nl_cli_value_to_buf fbuf off2 (vector-ref-ptr vec i))))
          (nl_cli_put_vector_loop fbuf off3 vec (+ i 1) n))))
    (defun nl_cli_put_nil (fbuf off)
      (nl_cli_put_byte
       fbuf (nl_cli_put_byte fbuf (nl_cli_put_byte fbuf off 110) 105) 108))
    (defun nl_cli_put_object (fbuf off)
      (nl_cli_put_byte
       fbuf
       (nl_cli_put_byte
        fbuf
        (nl_cli_put_byte
         fbuf
         (nl_cli_put_byte
          fbuf
          (nl_cli_put_byte
           fbuf
           (nl_cli_put_byte
            fbuf
            (nl_cli_put_byte
             fbuf
             (nl_cli_put_byte
              fbuf
              (nl_cli_put_byte fbuf off 35) 60)
             111)
            98)
           106)
          101)
         99)
        116)
       62))
    (defun nl_cli_value_to_buf (fbuf off out)
      (let* ((tag (ptr-read-u64 out 0)))
        (cond
         ((= tag 0) (nl_cli_put_nil fbuf off))
         ((= tag 1) (nl_cli_put_byte fbuf off 116))
         ((= tag 2) (nl_cli_put_dec fbuf off (ptr-read-u64 out 8)))
         ((= tag 4) (nl_cli_put_string_value fbuf off out 0))
         ((= tag 5) (nl_cli_put_string_value fbuf off out 1))
         ((= tag 6) (nl_cli_put_string_value fbuf off out 1))
         ((= tag 7) (nl_cli_put_list_tail fbuf (nl_cli_put_byte fbuf off 40) out 1))
         ((= tag 8) (nl_cli_put_vector_loop fbuf (nl_cli_put_byte fbuf off 91)
                                            out 0 (vector-len out)))
         (t (nl_cli_put_object fbuf off)))))
    (defun nl_cli_write_value (fbuf out)
      (let* ((tag (ptr-read-u64 out 0))
             (v (ptr-read-u64 out 8)))
        (if (= tag 2)
            (if (>= v 0)
                (if (< v 100)
                    (if (< v 10)
                        (seq
                         (ptr-write-u8 fbuf 0 (+ 48 v))
                         (ptr-write-u8 fbuf 1 10)
                         (nl_os_write_stdout fbuf 2)
                         0)
                      (seq
                       (ptr-write-u8 fbuf 0 (+ 48 (/ v 10)))
                       (ptr-write-u8 fbuf 1 (+ 48 (mod v 10)))
                       (ptr-write-u8 fbuf 2 10)
                       (nl_os_write_stdout fbuf 3)
                       0))
                  (let* ((n (nl_cli_value_to_buf fbuf 0 out)))
                    (seq
                     (ptr-write-u8 fbuf n 10)
                     (nl_os_write_stdout fbuf (+ n 1))
                     0)))
              (let* ((n (nl_cli_value_to_buf fbuf 0 out)))
                (seq
                 (ptr-write-u8 fbuf n 10)
                 (nl_os_write_stdout fbuf (+ n 1))
                 0)))
          (let* ((n (nl_cli_value_to_buf fbuf 0 out)))
            (seq
             (ptr-write-u8 fbuf n 10)
             (nl_os_write_stdout fbuf (+ n 1))
             0)))))
    (defun nl_copy_bytes_into (src dst i n off)
      (seq
       (while (< i n)
         (seq
          (ptr-write-u8 dst off (ptr-read-u8 src i))
          (setq i (+ i 1))
          (setq off (+ off 1))))
       off))
    (defun nl_repl_read_line_loop (buf off)
      (let* ((done 0)
             (n 0)
             (ch 0)
             (limit ,(- nelisp-standalone--reader-read-cap 512)))
        (seq
         (while (= done 0)
           (if (> off limit)
               (setq done 1)
             (seq
              (setq n (nl_os_read_stdin (+ buf off) 1))
              (if (< n 1)
                  (seq
                   (if (= off 0) (setq off -1) 0)
                   (setq done 1))
                (seq
                 (setq ch (ptr-read-u8 buf off))
                 (if (= ch 10)
                     (setq done 1)
                   (if (= ch 13)
                       0
                     (setq off (+ off 1)))))))))
         off)))
    (defun nl_repl_read_line (buf)
      (let* ((n (nl_repl_read_line_loop buf 0)))
        (seq
         (if (< n 0) 0 (ptr-write-u8 buf n 0))
         n)))
    (defun nl_repl_make_source (line n fbuf src print_p)
      (let* ((off (nl_repl_eval_prefix fbuf 0)))
        (seq
         (setq off (nl_copy_bytes_into line fbuf 0 n off))
         (ptr-write-u8 fbuf off 10)
         (setq off (+ off 1))
         (setq off (if (= print_p 1)
                       (nl_repl_eval_suffix fbuf off)
                     (nl_repl_eval_no_print_suffix fbuf off)))
         (nl_alloc_str fbuf off src))))
    (defun nl_repl_write_prompt (fbuf)
      (let* ((n (nl_repl_prompt fbuf 0)))
        (nl_os_write_stdout fbuf n)))
    (defun nl_repl_loop (prompt_p print_p linebuf fbuf src cursor result pool out ctx builtin_sym)
      (let* ((done 0)
             (n 0))
        (seq
         (while (= done 0)
           (seq
            (if (= prompt_p 1) (nl_repl_write_prompt fbuf) 0)
            (setq n (nl_repl_read_line linebuf))
            (if (< n 0)
                (setq done 1)
              (if (= n 0)
                  0
                (seq
                 (nl_repl_make_source linebuf n fbuf src print_p)
                 (nl_eval_source_all src cursor result pool out ctx builtin_sym)
                 (if (= (ptr-read-u64 268435464 0) 0) 0
                   (setq done 1)))))))
         0)))
    (defun nl_repl_bad_option_p (arg)
      (if (= arg 0)
          0
        (if (= (nl_cstr_eq_no_prompt arg) 1)
            0
          (if (= (nl_cstr_eq_no_print arg) 1) 0 1))))
    (defun nl_repl_usage_error_p (argc arg2 arg3)
      (if (> argc 4)
          1
        (if (= (nl_repl_bad_option_p arg2) 1)
            1
          (nl_repl_bad_option_p arg3))))
    (defun nl_argv_cstr_to_str (ptr out)
      (nl_alloc_str ptr (nl_cstr_len ptr) out))
    (defun nl_argv_list_from (argc sp i out)
      (if (= i argc)
          (wf_write_nil out)
        (let* ((str (alloc-bytes 32 8))
               (rest (alloc-bytes 32 8))
               (argptr (ptr-read-u64 sp (* (+ i 1) 8))))
          (seq
           (nl_argv_cstr_to_str argptr str)
           (nl_argv_list_from argc sp (+ i 1) rest)
           (nelisp_cons_construct str rest out)))))
    (defun driver (sp)
     (let* ((arena (nl_arena_init))
            (_sptop (ptr-write-u64 268436456 0 (aot-current-sp))) ; Doc 152 §11.21: capture mmap stack-top (driver-entry rsp, AFTER arena mmap) for the conservative GC stack scan
            ;; flat-arena cold loader: BEFORE any boot alloc, if the marker image
            ;; exists, load it into the arena + bump the cursor past it so every
            ;; following alloc lands after the image (no clobber).  -1 = no marker.
            (_cl (nl_cold_load_arena))
            (globals (alloc-bytes 32 8)) (frames (alloc-bytes 32 8)) (unbound (alloc-bytes 32 8))
            (ctx (alloc-bytes 120 8))
            (builtin_buf (alloc-bytes 8 1)) (builtin_sym (alloc-bytes 32 8))
            (src (alloc-bytes 32 8)) (cursor (alloc-bytes 32 8))
            ;; Doc 147 Phase 1.5 Group P — RAW parse-pool buffer (32768*32
            ;; bytes); `pool' IS the base, slot N @ pool+N*32.
            (result (alloc-bytes 32 8)) (pool (alloc-bytes (* 32768 32) 8)) (out (alloc-bytes 32 8))
            (argv_list (alloc-bytes 32 8))
            (argv_sym_buf (alloc-bytes ,(* 8 (length (nelisp-standalone--name-words "nelisp-standalone-argv"))) 1))
            (argv_sym (alloc-bytes 32 8))
            (sp0 (nl_os_argv_init sp))
            (argc (if (= sp0 0) 1 (logand (ptr-read-u64 sp0 0) 4294967295)))
            (slot0 (if (= sp0 0) 0 (ptr-read-u64 sp0 8)))
            (slot1 (if (= sp0 0) 0 (ptr-read-u64 sp0 16)))
            (slot2 (if (= sp0 0) 0 (ptr-read-u64 sp0 24)))
            (slot3 (if (= sp0 0) 0 (ptr-read-u64 sp0 32)))
            (argv_shifted_p (nl_cli_argv_shifted_p argc slot0 slot1))
            ;; Normal layout: [argc argv0 argv1 argv2 argv3].
            ;; Some macOS LC_MAIN entries expose argv+1 in the start shim:
            ;; [argc argv1 argv2 argv3 ...].  Select the shared logical view.
            (path (if (= argv_shifted_p 1) slot0 slot1))
            (arg2 (if (= argv_shifted_p 1)
                      slot1
                    (if (> argc 2) slot2 0)))
            (arg3 (if (= argv_shifted_p 1)
                      slot2
                    (if (> argc 3) slot3 0)))
            (prompt_p (if (= (nl_cstr_eq_no_prompt arg2) 1)
                          0
                        (if (= (nl_cstr_eq_no_prompt arg3) 1) 0 1)))
            (print_p (if (= (nl_cstr_eq_no_print arg2) 1)
                         0
                       (if (= (nl_cstr_eq_no_print arg3) 1) 0 1)))
            ;; raw read buffer (bypasses the Rust UTF-8 layer)
            (fbuf (alloc-bytes ,nelisp-standalone--reader-read-cap 1))
            (linebuf (alloc-bytes ,nelisp-standalone--reader-read-cap 1)))
       (seq
        (nl_bootstrap_make_mirror globals frames unbound)
        ;; cold path: replace the fresh empty globals with the loaded image's
        ;; globals Record (frames/unbound stay fresh).  -1 = normal boot.
        (if (< _cl 0) 0 (nl_cold_overwrite_globals globals))
        (ptr-write-u64 builtin_buf 0 31078196194145634)
        (nl_alloc_symbol builtin_buf 7 builtin_sym)
        ;; cold path: the loaded image's mirror already carries every native
        ;; builtin (registered by the dumping process) AND the prelude OVERRIDES
        ;; of them (A1 floor, A20 eq string-identity, ...).  Re-running
        ;; install-builtins over the image would overwrite each of those mirror
        ;; entries back to the bare native builtin, silently reverting every
        ;; prelude native-override -- verified: cold (floor 7 2) => 7 (native)
        ;; vs 3 (A1 override) before this gate, and (symbol-function 'floor)
        ;; reverted to the native subr.  User defuns (no builtin entry) already
        ;; survive.  Skip on cold path; normal boot (_cl < 0) installs them.
        (if (< _cl 0)
            (seq ,@(nelisp-standalone--reader-install-builtins-forms))
          0)
        ,@(let ((i 0)
                (forms nil))
            (dolist (word (nelisp-standalone--name-words "nelisp-standalone-argv"))
              (push `(ptr-write-u64 argv_sym_buf ,(* i 8) ,word) forms)
              (setq i (1+ i)))
            (nreverse forms))
        (nl_alloc_symbol argv_sym_buf ,(length (encode-coding-string "nelisp-standalone-argv" 'utf-8 t)) argv_sym)
        (nl_sexp_clone_into globals (+ ctx 0))
        (nl_sexp_clone_into frames (+ ctx 32))
        (nl_sexp_clone_into unbound (+ ctx 64))
        ;; rec_max 300000: the `_start' trampoline now runs the driver on a 1 GiB
;; mmap'd native stack whose ceiling is ~404k rec levels (each eval level ~5 KiB of
;; native frames; a self-recursive call burns ~2 rec increments).  300000 is ~74% of
;; that ceiling, so deep recursion (cnt(100000) -> 42) succeeds while still erroring
;; at the guard -- never SIGSEGV -- once it exceeds the budget.
(ptr-write-u64 ctx 96 0) (ptr-write-u64 ctx 104 300000)
        ;; M11 env inherit: stash the initial-stack envp (= sp0 + (argc+2)*8,
        ;; the char** right after argv's NULL) in arena slot +144 (268435600)
        ;; so the process substrate's execve passes the parent environment to
        ;; children instead of an empty one.  0 = unavailable (no sp).
        (ptr-write-u64 268435600 0
                       (if (= sp0 0)
                           0
                         (+ sp0 (* (+ argc 2) 8))))
        (ptr-write-u64 268436448 0 32768)                       ; Doc 147 P1.5 — store the RAW parse-pool cap (32768 slots) for the GC pool arms.  Raised from 8192 after vendored eucjp-ms' 2069-entry generated alist exceeded the flat-list tail depth; 32768 => MAX_DEPTH ~8191 for the current 3+4*MAX_DEPTH reader slot shape.
        (ptr-write-u64 268436464 0 1)  ; Doc 152 §11.21: CONSERVATIVE native-stack scan ON — closes the eval root-coverage gap (Doc 146 §2) so a collection never frees/blanks a still-referenced in-flight box.  Pairs with compaction OFF (mark+sweep) below; verified to stop the anvil-pkg suite SIGSEGV (suite-readiness now completes cleanly).
        ;; GC trigger: collect at a form boundary once the bump offset
        ;; crosses this threshold.  Initial 512 MiB keeps small *and*
        ;; moderate programs GC-free (zero overhead) — crucially the full
        ;; 14k-line nelisp-aot-compiler.el load (420 top-level forms,
        ;; peak arena only ~53 MB) never crosses it, so loading the whole
        ;; AOT toolchain is O(N) (one mark+sweep per form, each over the
        ;; GROWING live set, was O(N^2): a 4 MiB trigger made the load
        ;; collect on nearly every form past form ~40 and blow past 240s).
        ;; After each collection the trigger is re-armed as a bump offset:
        ;; max(live*3 + 1 MiB, bump + 512 MiB).  `live' is currently
        ;; advisory (nl_gc_sweep returns 0), so the conservative 512 MiB
        ;; stride keeps GC out of small/moderate loads while still avoiding
        ;; the old arena-base rearm bug that pushed the trigger past the
        ;; mapped arena.
        ;; The free-list reuse independently bounds long single-form compute.
        (ptr-write-u64 268435560 0 16777216)  ; GC trigger = 16 MiB growth-reserved
        ;; ---- Doc 146 §5: compacting GC is ON BY DEFAULT (flag 268435608 = 1). ----
        ;; The moving/compacting collector (phase 2 forward -> 4 move -> 6 munmap)
        ;; supersedes mark+sweep: full 319-file vendor-load peak 1.80GB -> 850MB
        ;; (-53%), 20/20 soak runs deterministic at 850MB with zero crashes.  Set
        ;; this to 0 as an ESCAPE HATCH to fall back to mark+sweep should any
        ;; workload ever trip a moving-GC root-coverage gap (Doc 146 §2).
        ;; Doc 152 §11.18-20 (2026-06-15): the anvil-pkg suite trips a
        ;; root-coverage gap, BUT the escape hatch does NOT help it — with
        ;; compaction off (mark+sweep) the suite still crashes (a collection
        ;; frees a still-referenced cons, blanking a progn body form ->
        ;; nl_kw_is_keyword(NULL)).  So the gap must be fixed at the root
        ;; (Doc 146 §2 root coverage); flipping this flag only changes the
        ;; manifestation (move+munmap dangling vs free+reuse blanking) and
        ;; regresses vendor-load RSS, so it stays at 1 (compaction ON).
        ;; Doc 152 §11.21: 0 (mark+sweep).  The moving GC has a root-coverage
        ;; gap (§11.18-19) that is UNSOUND on the suite; mark+sweep + the
        ;; conservative stack scan above is sound on all workloads.  Trade-off:
        ;; peak RSS rises (no compaction).  FUTURE: pin conservatively-found
        ;; blocks so compaction can be re-enabled (RSS) while staying sound.
        (ptr-write-u64 268435608 0 0)
        ;; ---- Doc 149: tag-4/5 String clone ALIASING is ON (flag 268435648 = 1). ----
        ;; nl_sci_dispatch shares the char buffer on clone instead of deep
        ;; copying it (strings/symbols are immutable in the reader; tracing
        ;; GC keeps shared buffers alive).  Kills the O(len)-per-access arena
        ;; cost that OOM'd the nemacs bridge on large buffer strings.
        ;; Set to 0 as an ESCAPE HATCH to restore the legacy deep copy.
        (ptr-write-u64 268435648 0 1)
        ;; ---- Doc 150 P1: immediate-result arg-slot recycling ON (268435680 = 1). ----
        ;; nl_eval_arg_list_walk pushes arg slots whose value became an
        ;; immediate word back onto the free-list at the cons-construct
        ;; site (the slot provably never escapes).  0 = legacy (leak the
        ;; slot until the next form-boundary GC).
        (ptr-write-u64 268435680 0 1)
        ;; BOOT WATERMARK: freeze the absolute address up to which everything
        ;; was allocated during install + driver setup (the mirror, all 60
        ;; builtins, the env/frame records, the fixed driver scratch slots).
        ;; The GC never frees blocks below this line — the boot image is live
        ;; for the whole program, so this is a sound "permanent generation"
        ;; (leaks only a few KB of install-time scratch) that sidesteps the
        ;; need to enumerate every boot-internal raw-pointer edge precisely.
        ;; Per-form eval garbage (allocated ABOVE the line) is fully collected.
        (ptr-write-u64 268435664 0 (+ 268435456 (ptr-read-u64 268435456 0)))
        ;; cold path: GC is now ENABLED over the cold-loaded image (no override
        ;; here, so base+160 = 0 from `nl_arena_init').  This is sound because
        ;; relocation is COMPLETE (nl_fa_slot mirrors nl_gc_mark_slot exactly, so
        ;; every reachable pointer the mark walk follows was relocated) and the
        ;; BOOT WATERMARK set just above (268435664 = base+bump, AFTER the load +
        ;; all boot allocs) puts the whole loaded image + boot generation BELOW
        ;; the watermark, where `nl_gc_sweep_one' keeps it live and never frees
        ;; it.  So a form-boundary collection (the trigger at 268435560=16 MiB is
        ;; below the loaded ~30 MiB bump, so it fires on the first form) marks the
        ;; relocated graph (safe) and only reclaims per-form garbage ABOVE the
        ;; watermark -- exactly as in a normal boot.  Validated at PARITY with
        ;; gate-OFF: cold fib(20)=6765, fib(23)=28657, sumto(50000)=1250025000
        ;; all run GC-active to correct results with no SIGSEGV; fib(25) OOMs
        ;; (SIGKILL) in BOTH cold and normal -- a pre-existing single-form limit
        ;; (no mid-form collection for one deeply-recursive top-level form), not
        ;; a cold-load regression.
        (if (= argv_shifted_p 1)
            (seq
             (ptr-write-u64 sp0 40 slot3)
             (ptr-write-u64 sp0 32 slot2)
             (ptr-write-u64 sp0 24 slot1)
             (ptr-write-u64 sp0 16 slot0))
          0)
        (cond
         ((= path 0)
          (if (= (nl_repl_usage_error_p argc arg2 arg3) 1)
              (seq (nl_cli_write_help fbuf) 2)
            (seq
             ;; cold path: the loaded image already carries the prelude; skip the
             ;; redundant (and free/reuse-risky) re-eval.  Normal boot runs it.
             (if (< _cl 0)
                 (seq
                  ,@(nelisp-standalone--reader-repl-prelude-forms
                     'fbuf 'src 'cursor 'result 'pool 'out 'ctx 'builtin_sym))
               0)
             (ptr-write-u64 268436216 0 0)
             (nl_repl_loop prompt_p print_p linebuf fbuf src cursor result pool out ctx builtin_sym)
             (ptr-write-u64 268436216 0 1)
             (if (= (ptr-read-u64 268435464 0) 0)
                 0
               (- (ptr-read-u64 268435464 0) 1)))))
         ((= (nl_cli_help_command_p path) 1)
          (if (= arg2 0)
              (seq (nl_cli_write_help fbuf) 0)
            (seq (nl_cli_write_help fbuf) 2)))
         ((= (nl_cli_eval_command_p path) 1)
          (if (= (nl_cli_one_arg_p arg2 arg3) 1)
              (seq
               ;; Doc 143: run the stdlib prelude before the user EXPR so the
               ;; same library available in the REPL is available under `eval'
               ;; (read-from-string, prin1-to-string, the list/string library,
               ;; etc.).  Without this `eval' saw only the bare builtins.
               ;; cold path: the loaded image already ran this prelude in the
               ;; dumping process, so its definitions are live in the loaded
               ;; globals.  RE-evaluating it over the cold image is both
               ;; redundant and unsafe (re-defining a stdlib function frees/
               ;; reuses the previous definition's blocks, which sit inside the
               ;; loaded image -> corruption -> SIGSEGV).  Skipping it is exactly
               ;; the cold-load win: the expensive library setup is already baked
               ;; into the image.  Normal boot (_cl < 0) runs the prelude.
               (if (< _cl 0)
                   (seq
                    ,@(nelisp-standalone--reader-repl-prelude-forms
                       'fbuf 'src 'cursor 'result 'pool 'out 'ctx 'builtin_sym))
                 0)
               (nl_cli_eval_source arg2 fbuf src)
               (nl_eval_source_all src cursor result pool out ctx builtin_sym)
               (if (= (ptr-read-u64 268435464 0) 0)
                   0
                 (- (ptr-read-u64 268435464 0) 1)))
            (seq (nl_cli_write_help fbuf) 2)))
         ((= (nl_cstr_eq_load path) 1)
          (if (= (nl_cli_one_arg_p arg2 arg3) 1)
              (seq
               ;; Run the stdlib prelude before the user file so `--load file.el'
               ;; has the same library (defun / defmacro / when / dash / s / ht /
               ;; cl-lib macros) as the REPL and `eval' paths, not just the bare
               ;; native builtins.  Without this, a loaded file that does
               ;; `(defun ...)' / `(when ...)' aborts (void-function), which blocks
               ;; runtime loading of any real elisp package.
               ;; cold path: the loaded image already carries the prelude; skip the
               ;; redundant (and free/reuse-risky) re-eval.  Normal boot runs it.
               (if (< _cl 0)
                   (seq
                    ,@(nelisp-standalone--reader-repl-prelude-forms
                       'fbuf 'src 'cursor 'result 'pool 'out 'ctx 'builtin_sym))
                 0)
               (let* ((n (nl_os_read_file_cpath
                          arg2 fbuf
                          ,nelisp-standalone--reader-read-cap)))
                 (if (< n 0)
                     (seq (nl_cli_write_help fbuf) 2)
                   (seq
                    (nl_alloc_str fbuf n src)
                    (nl_eval_source_all src cursor result pool out ctx builtin_sym)
                    (if (= (ptr-read-u64 268435464 0) 0)
                        (nl_cli_write_value fbuf out)
                      (- (ptr-read-u64 268435464 0) 1))))))
            (seq (nl_cli_write_help fbuf) 2)))
         ((= (nl_cstr_eq_neln_selftest path) 1)
          (nl_neln_demo_exec ctx 41))
         ((= (nl_cstr_eq_embedded path) 1)
          (seq
           (sexp-write-str-lit src ,(nelisp-standalone--reader-src))
           (nl_eval_source_all src cursor result pool out ctx builtin_sym)
           (ptr-read-u64 out 8)))
         ((= (nl_cstr_eq_repl path) 1)
          (if (= (nl_repl_usage_error_p argc arg2 arg3) 1)
              (seq (nl_cli_write_help fbuf) 2)
            (seq
             ;; cold path: the loaded image already carries the prelude; skip the
             ;; redundant (and free/reuse-risky) re-eval.  Normal boot runs it.
             (if (< _cl 0)
                 (seq
                  ,@(nelisp-standalone--reader-repl-prelude-forms
                     'fbuf 'src 'cursor 'result 'pool 'out 'ctx 'builtin_sym))
               0)
             (ptr-write-u64 268436216 0 0)
             (nl_repl_loop prompt_p print_p linebuf fbuf src cursor result pool out ctx builtin_sym)
             (ptr-write-u64 268436216 0 1)
             (if (= (ptr-read-u64 268435464 0) 0)
                 0
               (- (ptr-read-u64 268435464 0) 1)))))
         ((= (nl_cstr_eq_dump_runtime_image path) 1)
          (nl_runtime_image_write_dump sp0 argc fbuf))
         ((= (nl_cli_bare_legacy_command_p path) 1)
          (seq (nl_cli_write_help fbuf) 2))
         ((= (nl_cstr_starts_dash_p path) 1)
          (seq (nl_cli_write_help fbuf) 2))
         ((= (nl_cstr_eq_audit_elisp_artifacts path) 1)
          (nl_audit_fast_run argc sp0 argv_shifted_p fbuf))
         (t
          (seq
           ;; Runtime-image eval/exec replays real Elisp sources, not just core
           ;; special forms.  Load the same prelude as --eval/--load first so
           ;; image contents can use defun/defmacro and the stdlib surface.
           (if (= (nl_runtime_image_eval_exec_command_p path) 1)
               (if (= (nl_runtime_image_cache_eval_p sp0 argc) 1)
                   0
                 (seq
                  ,@(nelisp-standalone--reader-repl-prelude-forms
                     'fbuf 'src 'cursor 'result 'pool 'out 'ctx 'builtin_sym)))
             0)
           ;; --- source selection: embedded vs. file (M7 dual mode) ---
           (if (and (= (nl_runtime_image_eval_exec_command_p path) 1)
                    (= (nl_runtime_image_cache_eval_p sp0 argc) 1))
               (seq
                (nl_argv_list_from argc sp0 1 argv_list)
                (nl_env_set_value ctx argv_sym argv_list)
                (sexp-write-str-lit src ,(nelisp-standalone--runtime-image-command-src)))
             (if (= (nl_cstr_eq_eval_runtime_image path) 1)
                 (if (> argc 3)
                     (nl_runtime_image_eval_source sp0 argc fbuf src)
                   (sexp-write-str-lit src "1"))
               (if (= (nl_cstr_eq_exec_runtime_image path) 1)
                   (if (> argc 3)
                       (nl_runtime_image_exec_source sp0 argc fbuf src)
                     (sexp-write-str-lit src "1"))
               (if (= (nl_runtime_image_command_p path) 1)
                   (seq
                    (nl_argv_list_from argc sp0 1 argv_list)
                    (nl_env_set_value ctx argv_sym argv_list)
                    (sexp-write-str-lit src ,(nelisp-standalone--runtime-image-command-src)))
	                 (if (= (nl_cstr_eq_eval_elisp_artifact path) 1)
	                     (if (> argc 2)
	                         (seq
	                          (nl_argv_list_from argc sp0 1 argv_list)
	                          (nl_env_set_value ctx argv_sym argv_list)
	                          (sexp-write-str-lit src ,(nelisp-standalone--artifact-direct-command-src)))
	                       (sexp-write-str-lit src "1"))
	                   (if (= (nl_cstr_eq_exec_elisp_artifact path) 1)
	                       (if (> argc 2)
	                           (seq
	                            (nl_argv_list_from argc sp0 1 argv_list)
	                            (nl_env_set_value ctx argv_sym argv_list)
	                            (sexp-write-str-lit src ,(nelisp-standalone--artifact-direct-command-src)))
	                         (sexp-write-str-lit src "1"))
	                 (if (= (nl_artifact_command_p path) 1)
	                     (seq
	                      (nl_argv_list_from argc sp0 1 argv_list)
	                      (nl_env_set_value ctx argv_sym argv_list)
	                      (if (and (= (nl_artifact_runtime_cache_command_p path) 1)
	                               (= (nl_artifact_runtime_cache_enabled_p fbuf) 1)
	                               (= (nl_artifact_runtime_cache_exists_p fbuf) 1))
	                          (if (= (nl_artifact_source_cache_command_p path) 1)
	                              (sexp-write-str-lit src ,(nelisp-standalone--artifact-source-command-cache-src))
	                            (sexp-write-str-lit src ,(nelisp-standalone--artifact-command-cache-src)))
	                        (sexp-write-str-lit src ,(nelisp-standalone--artifact-command-src))))
	                   ;; file path: open(path,O_RDONLY) -> read -> close -> wrap as Str
	                   (let* ((n (nl_os_read_file_cpath
	                              path fbuf
	                              ,nelisp-standalone--reader-read-cap)))
	                     (if (< n 0)
	                         (sexp-write-str-lit src "1")
	                       (nl_alloc_str fbuf n src)))))))))))
           ;; --- reader path (M8): read+eval EVERY top-level form, keep the last
           ;; value.  parse_one advances the shared cursor; it returns 1 per form
           ;; and != 1 (e.g. -1 at EOF) when no more forms remain.
           (ptr-write-u64 268436216 0 0)
           (nl_eval_source_all src cursor result pool out ctx builtin_sym)
           (ptr-write-u64 268436216 0 1)
           (ptr-read-u64 out 8))))))))

;; REAL special-form + env machinery (Doc 137 M2/M3 un-trap).  Replaces the
;; baked path's trap.o stubs so the binary is a GENUINE general interpreter for
;; the 11 special forms (if/let/let*/setq/while/progn/quote/lambda/function/
;; condition-case/unwind-protect) with real lexical environments.  The first 21
;; are the direct trap replacements; the rest are the transitive deps they pull
;; (env/frame/bind/mirror/cell ops) -- frozen here so the build is a single link.
(defconst nelisp-standalone--reader-real-sf-manifest
  '(("sf-if.o"             nelisp-cc-sf-if                        nelisp-cc-sf-if--source)
    ("sf-let.o"            nelisp-cc-sf-let                       nelisp-cc-sf-let--source)
    ("sf-let-star.o"       nelisp-cc-sf-let-star                  nelisp-cc-sf-let-star--source)
    ("sf-setq.o"           nelisp-cc-sf-setq                      nelisp-cc-sf-setq--source)
    ("sf-while.o"          nelisp-cc-sf-while                     nelisp-cc-sf-while--source)
    ("sf-progn.o"          nelisp-cc-sf-progn                     nelisp-cc-sf-progn--source)
    ("sf-quote.o"          nelisp-cc-sf-quote                     nelisp-cc-sf-quote--source)
    ("sf-lambda.o"         nelisp-cc-sf-lambda                    nelisp-cc-sf-lambda--source)
    ("sf-function.o"       nelisp-cc-sf-function                  nelisp-cc-sf-function--source)
    ("sf-cc.o"             nelisp-cc-sf-condition-case            nelisp-cc-sf-condition-case--source)
    ("sf-uwp.o"            nelisp-cc-sf-unwind-protect            nelisp-cc-sf-unwind-protect--source)
    ("env-leaves-simple.o" nelisp-cc-evalport-env-leaves-simple  nelisp-cc-evalport-env-leaves-simple--source)
    ;; val-load.o is supplied by `nelisp-standalone--manifest`; keep it out of
    ;; this overlay so the reader link does not define `nl_val_clone_into' twice.
    ("nlcell-get-value.o"  nelisp-cc-nlcell-get-value            nelisp-cc-nlcell-get-value--source)
    ("apply-lambda-inner.o" nelisp-cc-apply-lambda-inner         nelisp-cc-apply-lambda-inner--source)
    ("frame-push.o"        nelisp-cc-frame-push                   nelisp-cc-frame-push--source)
    ("frame-pop.o"         nelisp-cc-frame-pop                    nelisp-cc-frame-pop--source)
    ("env-bind-local.o"    nelisp-cc-env-bind-local              nelisp-cc-env-bind-local--source)
    ("env-shim-op.o"       nelisp-cc-env-shim-op                  nelisp-cc-env-shim-op--source)
    ("env-shim-set-op.o"   nelisp-cc-env-shim-set-op             nelisp-cc-env-shim-set-op--source)
    ("env-leaves-frame.o"  nelisp-cc-evalport-env-leaves-frame   nelisp-cc-evalport-env-leaves-frame--source)
    ("aot-builtin-call1.o" nelisp-cc-evalport-aot-builtin-call1  nelisp-cc-evalport-aot-builtin-call1--source)
    ("sf-eval-is-truthy.o" nelisp-cc-eval-is-truthy              nelisp-cc-eval-is-truthy--source)
    ("sf-let-setup.o"      nelisp-cc-evalport-env-leaves-logic   nelisp-cc-evalport-env-leaves-logic--source)
    ("sf-env-set-value.o"  nelisp-cc-evalport-env-leaves-bind    nelisp-cc-evalport-env-leaves-bind--source)
    ("sf-cons-prepend-clone.o" nelisp-cc-cons-prepend-clone      nelisp-cc-cons-prepend-clone--source)
    ;; sf-capture-lexical.o intentionally omitted -- replaced by the corrected
    ;; nl_env_capture_lexical in `nelisp-standalone--reader-capture-source'.
    ("sf-symbol-is-lambda.o" nelisp-cc-symbol-is-lambda          nelisp-cc-symbol-is-lambda--source)
    ("sf-env-lookup-value.o" nelisp-cc-env-lookup-value          nelisp-cc-env-lookup-value--source)
    ("sf-frame-ensure-cap.o" nelisp-cc-frame-ensure-capacity     nelisp-cc-frame-ensure-capacity--source)
    ("sf-alloc-cell.o"     nelisp-cc-nlcell-alloc                nelisp-cc-nlcell-alloc--source)
    ("sf-frame-bind.o"     nelisp-cc-frame-bind                   nelisp-cc-frame-bind--source)
    ("sf-mirror-setval.o"  nelisp-cc-mirror-set-value-or-insert  nelisp-cc-mirror-set-value-or-insert--source)
    ("sf-mirror-is-bound.o" nelisp-cc-mirror-is-bound            nelisp-cc-mirror-is-bound--source)
    ("sf-mirror-is-fbound.o" nelisp-cc-mirror-is-fbound          nelisp-cc-mirror-is-fbound--source)
    ("sf-mirror-is-const.o" nelisp-cc-mirror-is-constant         nelisp-cc-mirror-is-constant--source)
    ("sf-mirror-clearval.o" nelisp-cc-mirror-clear-value         nelisp-cc-mirror-clear-value--source)
    ("sf-mirror-clearfn.o" nelisp-cc-mirror-clear-function       nelisp-cc-mirror-clear-function--source)
    ("sf-mirror-setconst.o" nelisp-cc-mirror-set-constant-or-insert nelisp-cc-mirror-set-constant-or-insert--source)
    ("sf-bind-formals.o"   nelisp-cc-bind-formals                nelisp-cc-bind-formals--source)
    ("sf-env-set-value2.o" nelisp-cc-env-set-value               nelisp-cc-env-set-value--source)
    ("sf-frame-stack-find.o" nelisp-cc-frame-stack-find          nelisp-cc-frame-stack-find--source)
    ("sf-bf-args-nth-ptr.o" nelisp-cc-bf-args-nth-ptr            nelisp-cc-bf-args-nth-ptr--source)
    ("sf-bf-formal-tag.o"  nelisp-cc-bf-formal-tag               nelisp-cc-bf-formal-tag--source)
    ("sf-bf-precompute.o"  nelisp-cc-bf-precompute               nelisp-cc-bf-precompute--source)
    ("sf-cell-set-value.o" nelisp-cc-nlcell-set-value            nelisp-cc-nlcell-set-value--source))
  "Real special-form + env units replacing the baked path's trap.o stubs.")

;; nelisp_eval_call_with_err(form, env, out, err_out): the condition-case
;; error-trapping eval variant has no real elisp `defun' (it was a Rust shim).
;;
;; WAVE-2 condition-case trapping (PATCH 1 of 4).  The no-error path stays
;; behaviourally exact (clear err_out, delegate to the real nelisp_eval_call).
;; The NEW error path inspects the M6 signal stash shared with catch/throw:
;;   flag @268435472, stashed TAG @268435480, stashed VAL @268435512.
;; On rc=1 with the flag set, build err_out = (error_sym . data) from the arena
;; stash via `nelisp_cons_construct' (cons-ctor.o); the existing
;; `nl_cc_match_and_bind' (real defun in env-leaves-logic, via sf-let-setup.o)
;; then matches the handler clause against err_out.car.
;;
;; The flag is LEFT SET here (not cleared): a clause MATCH clears it (PATCH 4,
;; `nl_sf_cc_after_match'); on NO match the still-set flag lets an OUTER
;; condition-case re-trap the same signal (nested reraise) by rebuilding the
;; cons from the unchanged arena stash.  rc=1 on a non-signal abort (flag==0)
;; falls through to `nl_cce_clear' + the original rc, so genuine errors still
;; propagate.
(defconst nelisp-standalone--reader-errstub-source
  '(seq
    (defun nl_cce_clear (err_out)
      (seq (ptr-write-u64 err_out 0 0) (ptr-write-u64 (+ err_out 8) 0 0)
           (ptr-write-u64 (+ err_out 16) 0 0) (ptr-write-u64 (+ err_out 24) 0 0) 0))
    (defun nelisp_eval_call_with_err (form env out err_out)
      (let* ((rc (nelisp_eval_call form env out)))
        (if (= rc 0)
            (nl_cce_clear err_out)
          (if (= (ptr-read-u64 268435472 0) 1)
              (seq (nelisp_cons_construct 268435480 268435512 err_out)
                   1)
            (seq (nl_cce_clear err_out) rc))))))
  "WAVE-2 condition-case-trapping nelisp_eval_call_with_err (PATCH 1): on a
signalled abort it builds err_out=(TAG . VAL) from the M6 arena stash so
`nl_cc_match_and_bind' can match the handler; the no-error path is unchanged.")

;; CORRECTED closure-capture (Doc 137 M3).  The shipped AOT
;; `nelisp-cc-evalport-capture-lexical' passes the raw frames-record pointer to
;; `nl_capture_descend_native', which expects a 3-slot in-vec ([0]=stack record,
;; [1]=depth Int, [2]=scratch) -- so it misreads the depth and segfaults on every
;; lambda creation.  (The real Emacs path builds the in-vec in an elisp JIT
;; wrapper; the AOT port never did.)  This override reads the real depth from
;; frames.slot1 and builds the proper in-vec; depth 0 (top level) -> nil capture.
;; Replaces the sf-capture-lexical.o unit in the reader build.
(defconst nelisp-standalone--reader-capture-source
  '(seq
    (defun nl_clx_write_nil (slot)
      (seq (ptr-write-u64 slot 0 0) (ptr-write-u64 (+ slot 8) 0 0)
           (ptr-write-u64 (+ slot 16) 0 0) (ptr-write-u64 (+ slot 24) 0 0) 0))
    (defun nl_env_capture_lexical (env out)
      (let* ((frames_ptr (+ env 32))
             (depth (sexp-int-unwrap (record-slot-ref-ptr frames_ptr 1))))
        (seq
         (nl_clx_write_nil out)
         (if (= depth 0)
             0
           ;; Doc 147 Phase 1.5: build the 3-slot `invec' WITHOUT holding a
           ;; raw write-through interior pointer into its data buffer.  The
           ;; old code did `(... (vector-ref-ptr invec N))' as a 32B write
           ;; DESTINATION, which stomps neighbouring slots once the Phase-2
           ;; buffer stride shrinks 32B->8B.  Route every write through
           ;; `vector-slot-set' (= refcount-safe `nl_vector_set_slot', clones
           ;; *VAL-PTR into slot N with the live buffer stride).  Each value is
           ;; first materialized into a fresh 32B scratch slot, then cloned
           ;; into the Vector by `vector-slot-set'.  `nl_capture_descend_native'
           ;; still reads invec[0..2] via `vector-ref-ptr' (a Phase-2 view,
           ;; read-safe) — only the producer side changes here.
           (let* ((invec (alloc-bytes 32 8)) (alist (alloc-bytes 32 8))
                  (depth_slot (alloc-bytes 32 8))
                  (nil_slot (alloc-bytes 32 8)))
             (seq
              (vector-make 3 invec)
              ;; slot 0 = stack record: clone frames_ptr straight in.
              (vector-slot-set invec 0 frames_ptr)
              ;; slot 1 = max-depth Int: build in scratch, clone in.
              (sexp-int-make depth_slot depth)
              (vector-slot-set invec 1 depth_slot)
              ;; slot 2 = pair-slot scratch, Nil on entry.
              (nl_clx_write_nil nil_slot)
              (vector-slot-set invec 2 nil_slot)
              (nl_clx_write_nil alist)
              (nl_capture_descend_native invec alist)
              (nl_sexp_clone_into alist out)
              0))))))))

;; ===================================================================
;; TOP-LEVEL FORM-BOUNDARY ARENA RECLAMATION (the safe reclamation point).
;;
;; The bump arena (`nelisp-standalone--arena-source') never frees: every
;; nested function / builtin / macro-expansion call leaks ~4-5 KiB of dead
;; temporaries (frame vectors, arg-list conses, lookup clones, symbol bufs).
;; A single `fib' invocation re-expands the `cond' macro and runs ~10 nested
;; calls -> MEASURED 89_024 bytes/call (constant) -> any fixed arena is
;; exhausted at shallow recursion (fib(20) SIGSEGVs at 1 GiB).
;;
;; PER-CALL region reset (resetting the bump inside `nl_apply_lambda_inner'
;; on an immediate result) is NOT SAFE in this interpreter: a lambda used as
;; a SUBEXPRESSION (e.g. the `(fib (- n 1))' inside `(+ (fib ..) (fib ..))')
;; returns into an ENCLOSING evaluation that is still mid-allocation; reseting
;; while that enclosing eval is pending corrupts its in-progress arg list /
;; result (empirically: `(+ (g 3) 100)' -> 0 instead of 103).  The eval
;; machinery holds live pointers into the per-call region from the caller, so
;; safe per-call reclamation needs escape analysis the build glue cannot do.
;;
;; The DRIVER-LEVEL top-level boundary IS safe: between two top-level forms
;; nothing is pending and the previous form's result has been read out.  So we
;; reclaim there.  The one hazard is that a top-level DEFINITION
;; (`defmacro' / `fset' / `defun') stores closures whose BODIES live in the
;; arena above the reclaim mark — those must survive.  We detect a definition
;; by watching the globals fast-hash-table entry-count (globals.slot0 ->
;; HT.slot2, an Int bumped on every new mirror insert):
;;   * form GREW the mirror  (= a new global was defined)  -> ADVANCE the mark
;;     to the current bump (keep everything this form allocated, incl. closure
;;     bodies); do NOT reset.
;;   * form did NOT grow the mirror AND returned an immediate (Int/Nil/T,
;;     no arena pointer) -> RESET the bump to the mark (free the whole form's
;;     call-tree garbage).
;;   * otherwise (pointer result, or in-place mutation) -> keep (no reset),
;;     and advance the mark to current bump so later resets never free it.
;;
;; This reclaims the per-form garbage of every pure top-level expression while
;; never freeing a definition.  It bounds arena growth for multi-form programs
;; (the self-host compiler processes thousands of top-level forms) to the sum
;; of the LIVE definitions plus ONE form's peak, instead of the sum of ALL
;; forms' peaks.  It does NOT bound a single deeply-recursive form (see the
;; arena-size note); that is the residual blocker.
;;
;; Implemented directly in the reader driver (see
;; `nelisp-standalone--reader-driver-source'); the fixed arena slot 268435472
;; (reserved [16,96) region, unused outside catch/throw which fib does not hit)
;; holds the current reclaim mark.  lisp/ + golden binaries stay untouched.
;; ===================================================================

(defun nelisp-standalone--reader-extra-unit (entry)
  "Compile a (NAME FEATURE SRC-SYM) manifest ENTRY to a cached link-unit."
  (pcase-let ((`(,name ,feat ,src) entry))
    (require feat)
    (nelisp-standalone--cached-unit name (symbol-value src)
                                    (locate-library (symbol-name feat)))))

;; Doc 147 container slots made `nl_cons_cdr_ptr' return a materialised 32B
;; view for cdr WORDs.  Binding `&rest' directly to the tail view is unstable
;; once required arguments have been consumed (`(a &rest xs)' and friends).
;; Clone the tail into a fresh slot before installing it in the frame.
(defconst nelisp-standalone--reader-bind-rest-fixed
  '(defun nl_bf_bind_rest (env name_ptr args_ptr idx)
     (let* ((tail_ptr (nl_bf_bind_rest_tail args_ptr idx))
            (tail_slot (alloc-bytes 32 8))
            (mirror_ptr (+ env 0))
            (frames_ptr (+ env 32))
            (unbound_ptr (+ env 64))
            (out_vec_slot (alloc-bytes 32 8)))
       (seq
        (nl_sexp_clone_into tail_ptr tail_slot)
        (nl_env_build_scratch tail_slot unbound_ptr out_vec_slot)
        (nelisp_env_bind_local mirror_ptr frames_ptr name_ptr tail_slot out_vec_slot 0))))
  "Standalone reader fix for required-plus-&rest formal binding.")

(defun nelisp-standalone--patch-env-leaves-bind-rest (src)
  "Return env-leaves-bind SRC with rc/lifetime-safe `nl_bf_bind_rest'."
  (cons (car src)
        (mapcar (lambda (form)
                  (if (and (consp form) (eq (car form) 'defun)
                           (eq (cadr form) 'nl_bf_bind_rest))
                      nelisp-standalone--reader-bind-rest-fixed
                    form))
                (cdr src))))

;; rc-correct nl_apply_do_fset (Doc 137 M3).  The shipped handler in
;; nelisp-cc-evalport-combiner-apply has two rc-plumbing bugs (the non-symbol
;; clone arm returns the dst ptr -- treated as error; the mirror setter returns 1
;; on success -- treated as failure) AND triggers a Phase47 compiler mis-lowering:
;; `(not (= raw-bool 0))' inside a dispatcher-boundary-available defun is routed
;; through nelisp_aot_builtin_call1, which reads 32 bytes from the raw i64 boolean
;; as a *const Sexp -> NULL deref -> SIGSEGV (the real root cause; see the
;; compiler gate at nelisp-aot-compiler ~L7846/7862, a durable fix TODO).
;; This rewrite avoids `not', yields rc 0 from the clone arm, and treats the
;; setter's 1 as success.  Applied as a build-time sexp patch so lisp/ + golden
;; binaries stay untouched.
(defconst nelisp-standalone--reader-do-fset-fixed
  '(defun nl_apply_do_fset (args_list_ptr env out)
     (let* ((sym_ptr (nl_apply_list_nth args_list_ptr 0))
            (def_ptr (nl_apply_list_nth args_list_ptr 1)))
       (if (= sym_ptr 0) (nl_apply_stash_wta env args_list_ptr)
         (if (= def_ptr 0) (nl_apply_stash_wta env args_list_ptr)
           (let* ((mirror_ptr (+ env 0)) (unbound_ptr (+ env 64))
                  (def_tag (ptr-read-u64 def_ptr 0)))
             (let* ((resolved_slot (alloc-bytes 32 8)))
               (let* ((resolve_rc
                       (if (= def_tag 4)
                           (nelisp_env_lookup_function mirror_ptr unbound_ptr def_ptr resolved_slot)
                         (seq (nl_sexp_clone_into def_ptr resolved_slot) 0))))
                 (if (= resolve_rc 0)
                     (let* ((scratch_slot (alloc-bytes 32 8)))
                       (seq
                        (nl_apply_build_fn_scratch unbound_ptr resolved_slot scratch_slot)
                        (nelisp_mirror_set_function_or_insert mirror_ptr sym_ptr scratch_slot 0)
                        (nl_sexp_clone_into def_ptr out)
                        0))
                   resolve_rc))))))))
  "Rc-correct, `not'-free replacement for the shipped nl_apply_do_fset.")

;; M4 keyword self-eval.  Without this, evaluating a keyword symbol (e.g. the
;; `:test' in `(make-hash-table :test 'equal)') routes through nl_env_lookup_val
;; and aborts as an unbound variable.  Keywords must self-evaluate.  This is a
;; build-time sexp patch of the eval-inner unit (lisp/ stays pristine): it adds
;; the `nl_kw_is_keyword' predicate and prepends a keyword clause to
;; `nl_eval_inner's tag-dispatch cond.  nl_kw_is_keyword(form) = 1 iff form is a
;; Sexp::Symbol(4) whose first name byte is ':' (= 58).
(defconst nelisp-standalone--kw-is-keyword
  '(defun nl_kw_is_keyword (form)
     (if (= (sexp-tag form) 4)
         (if (> (str-len form) 0)
             (if (= (str-byte-at form 0) 58) 1 0)
           0)
       0)))

(defun nelisp-standalone--patch-eval-inner-defun (form)
  "Rewrite the nl_eval_inner defun FORM to prepend a keyword self-eval cond
clause as the FIRST branch (so keyword symbols self-evaluate before the
Symbol(4) variable-lookup branch)."
  ;; form = (defun nl_eval_inner (form env out _pad) (cond CLAUSES...))
  (let* ((name (cadr form))
         (arglist (caddr form))
         (cond-form (cadddr form)))   ; the (cond ...) body
    (if (and (consp cond-form) (eq (car cond-form) 'cond))
        (list 'defun name arglist
              (cons 'cond
                    (cons '((= (nl_kw_is_keyword form) 1)
                            (nl_ei_self_eval_done (extern-call nl_sexp_clone_into form out) 0))
                          (cdr cond-form))))
      form)))

(defun nelisp-standalone--patch-eval-inner (src)
  "Return eval-inner SRC (a `(seq (defun ...) ...)') with the M4 keyword
self-eval clause wired in: insert `nl_kw_is_keyword' and rewrite the
`nl_eval_inner' tag-dispatch cond.  Mirrors `--patch-combiner-apply'."
  (cons (car src)
        (let (out inserted)
          (dolist (form (cdr src))
            (cond
             ((and (consp form) (eq (car form) 'defun)
                   (eq (cadr form) 'nl_eval_inner))
              (unless inserted
                (push nelisp-standalone--kw-is-keyword out)
                (setq inserted t))
              (push (nelisp-standalone--patch-eval-inner-defun form) out))
             (t (push form out))))
          (nreverse out))))

;; WAVE-2 (PATCH 3): un-defer `signal' in combiner-apply.  The AOT combiner's
;; `nl_apply_name_classify' tags `signal' as category 2 ("deferred") and routes
;; it to `nl_apply_stash_wta' (stashes a wrong-type-argument into the env var),
;; BYPASSING `nelisp_apply_function' so bf_signal never runs and the M6 arena
;; signal stash is never set -> condition-case (the PATCH-1 arena-stash errstub)
;; cannot catch it.  Neutralise `nl_apply_deferred_signal' (always return 0) so
;; `signal' is no longer deferred: it falls through to `nelisp_apply_function' ->
;; bf_signal (PATCH 2) -> arena stash -> errstub (PATCH 1) -> nl_cc_match_and_bind.
;; Composed onto the existing do_fset rc-fix below (both patch combiner-apply).
(defun nelisp-standalone--patch-combiner-apply-deferred-signal (src)
  "Return combiner-apply SRC with `nl_apply_deferred_signal' neutralised to
always return 0, so `signal' flows to the builtin applyfn (bf_signal) instead of
`nl_apply_stash_wta'.  WAVE-2 PATCH 3."
  (cons (car src)
        (mapcar (lambda (form)
                  (if (and (consp form) (eq (car form) 'defun)
                           (eq (cadr form) 'nl_apply_deferred_signal))
                      '(defun nl_apply_deferred_signal (name_ptr) 0)
                    form))
                (cdr src))))

;; rc-correct list splice helpers for `apply' (Doc 137 — apply was a no-op).
;; The shipped `nl_apply_list_init' / `nl_apply_list_append' / `nl_apply_do_apply'
;; all let a `nelisp_cons_construct' or `nl_sexp_clone_into' fall through as the
;; return value.  Both ops leave the DST SLOT POINTER (nonzero) in rax, so the
;; rc-0-means-success callers (`(if (= rc 0) ...)') always saw "error" and apply
;; produced Nil.  These rewrites force an explicit `0' rc on every success path
;; (and fix the non-symbol resolve arm, same bug `nl_apply_do_fset' had).
;; Applied as build-time sexp patches so lisp/ + golden binaries stay untouched.
(defconst nelisp-standalone--reader-list-init-fixed
  '(defun nl_apply_list_init (list_ptr out_slot)
     (if (= (ptr-read-u64 list_ptr 0) 7)
         (let* ((cdr_ptr (nl_cons_cdr_ptr list_ptr)))
           (if (= (ptr-read-u64 cdr_ptr 0) 7)
               (let* ((car_ptr (nl_cons_car_ptr list_ptr))
                      (rest_slot (alloc-bytes 32 8)))
                 (let* ((rc (nl_apply_list_init cdr_ptr rest_slot)))
                   (if (= rc 0)
                       (seq (nelisp_cons_construct car_ptr rest_slot out_slot) 0)
                     1)))
             (seq (nl_apply_write_nil out_slot) 0)))
       (seq (nl_apply_write_nil out_slot) 0)))
  "All-but-last of LIST into OUT-SLOT, returning rc 0 on success.")

(defconst nelisp-standalone--reader-list-append-fixed
  '(defun nl_apply_list_append (head_ptr tail_ptr out_slot)
     (if (= (ptr-read-u64 head_ptr 0) 7)
         (let* ((car_ptr (nl_cons_car_ptr head_ptr))
                (cdr_ptr (nl_cons_cdr_ptr head_ptr))
                (rest_slot (alloc-bytes 32 8)))
           (let* ((rc (nl_apply_list_append cdr_ptr tail_ptr rest_slot)))
             (if (= rc 0)
                 (seq (nelisp_cons_construct car_ptr rest_slot out_slot) 0)
               1)))
       (seq (nl_sexp_clone_into tail_ptr out_slot) 0)))
  "HEAD ++ TAIL into OUT-SLOT, returning rc 0 on success.")

(defconst nelisp-standalone--reader-do-apply-fixed
  '(defun nl_apply_do_apply (args_list_ptr env out)
     (let* ((arg0_ptr (nl_apply_list_nth args_list_ptr 0)))
       (if (= arg0_ptr 0) (nl_apply_stash_wta env args_list_ptr)
         (let* ((arg0_tag (ptr-read-u64 arg0_ptr 0))
                (rest_args (if (= (ptr-read-u64 args_list_ptr 0) 7)
                               (nl_cons_cdr_ptr args_list_ptr)
                             args_list_ptr)))
           (let* ((func_slot (alloc-bytes 32 8)))
             (let* ((resolve_rc
                     (if (= arg0_tag 4)
                         (let* ((mirror_ptr (+ env 0)) (unbound_ptr (+ env 64)))
                           (nelisp_env_lookup_function mirror_ptr unbound_ptr arg0_ptr func_slot))
                       (seq (nl_sexp_clone_into arg0_ptr func_slot) 0))))
               (if (= resolve_rc 0)
                   (let* ((prefix_slot (alloc-bytes 32 8))
                          (last_ptr (nl_apply_list_last_cdr rest_args)))
                     (let* ((rc_init (nl_apply_list_init rest_args prefix_slot)))
                       (if (= rc_init 0)
                           (let* ((spliced_slot (alloc-bytes 32 8)))
                             (let* ((rc_app (nl_apply_list_append prefix_slot last_ptr spliced_slot)))
                               (if (= rc_app 0)
                                   (nl_apply_function func_slot spliced_slot env out)
                                 1)))
                         1)))
                 1)))))))
  "Rc-correct `apply' handler (non-symbol resolve arm forces rc 0).")

(defun nelisp-standalone--patch-combiner-apply (src)
  "Return patched combiner-apply SRC.
SRC is a `(seq (defun ...) ...)'.  This swaps nl_apply_do_fset for
`nelisp-standalone--reader-do-fset-fixed' (M3), swaps the `apply'
splice helpers for their rc-correct variants, and neutralises
`nl_apply_deferred_signal' (WAVE-2 PATCH 3), so condition-case can trap
`signal'.  All patches operate on the same combiner-apply source.  Keeps
lisp/ pristine."
  (nelisp-standalone--patch-combiner-apply-deferred-signal
   (cons (car src)
         (mapcar (lambda (form)
                   (cond
                    ((and (consp form) (eq (car form) 'defun)
                          (eq (cadr form) 'nl_apply_do_fset))
                     nelisp-standalone--reader-do-fset-fixed)
                    ((and (consp form) (eq (car form) 'defun)
                          (eq (cadr form) 'nl_apply_list_init))
                     nelisp-standalone--reader-list-init-fixed)
                    ((and (consp form) (eq (car form) 'defun)
                          (eq (cadr form) 'nl_apply_list_append))
                     nelisp-standalone--reader-list-append-fixed)
                    ((and (consp form) (eq (car form) 'defun)
                          (eq (cadr form) 'nl_apply_do_apply))
                     nelisp-standalone--reader-do-apply-fixed)
                    (t form)))
                 (cdr src)))))

;; WAVE-2 (PATCH 4): condition-case clears the M6 arena signal flag on a clause
;; MATCH.  Pairs with PATCH 1 (the errstub no longer clears flag@268435472), so a
;; clause MATCH must clear it -- else the next signal-free form would observe a
;; stale pending-signal flag.  A NO match leaves the flag set so an outer
;; condition-case re-traps (nested reraise).  `nl_sf_cc_after_match(match-rc, ...)'
;; receives match-rc=0 when `nl_cc_match_and_bind' matched + pushed the handler
;; frame; we clear the flag then run the handler body via `nl_sf_cc_body'.
;; Applied as a build-time sexp patch on the sf-cc.o source (lisp/ stays
;; pristine; the matcher `nl_cc_match_and_bind' itself is reused unchanged via
;; sf-let-setup.o = env-leaves-logic).
(defun nelisp-standalone--patch-sf-cc (src)
  "Return sf-condition-case SRC (a `(seq (defun ...) ...)') with
`nl_sf_cc_after_match' rewritten to clear the M6 arena signal flag (@268435472)
on a clause MATCH before running the handler body.  WAVE-2 PATCH 4."
  (cons (car src)
        (mapcar
         (lambda (form)
           (if (and (consp form) (eq (car form) 'defun)
                    (eq (cadr form) 'nl_sf_cc_after_match))
               '(defun nl_sf_cc_after_match (match-rc env out s1 _p5 _p6)
                  (if (= match-rc 0)
                      (seq (ptr-write-u64 268435472 0 0)
                           (nl_sf_cc_body s1 env out 0))
                    1))
             form))
         (cdr src))))

;; PER-EVAL RECLAMATION — escape-site epoch bumps.
;;
;; The per-eval arena reset in `nelisp_eval_call' is gated on (immediate result)
;; AND (mutation epoch @268435544 unchanged).  Besides the user-visible mutators
;; (setcar/setcdr/aset/puthash, instrumented in the applyfn dispatch), the eval
;; machinery itself installs boxes into PERSISTENT records that the per-call reset
;; would otherwise free out from under a still-live root:
;;
;;   * `nelisp_frame_stack_ensure_capacity_grow' reallocates the frame-stack
;;     BACKING vector and installs it into the persistent frames-record slot 0.
;;     The grown vector is allocated above the current call's mark but stays
;;     reachable after the call returns -> MUST survive.  (This was the fib
;;     corruption: a self-recursive call grows the backing, returns an Int, the
;;     immediate+epoch gate green-lit the reset, freeing the live backing ->
;;     dangling frames-record slot 0 -> garbage on the next frame op.)
;;   * `nelisp_env_set_value' cell-hit (setq of an enclosing/global binding) and
;;     mirror-insert (setq vivifying a new global) write a box into a persistent
;;     cell / the globals mirror, yet return the assigned value (often an Int).
;;
;; Each of these bumps the epoch so a call in whose dynamic extent they ran is
;; never reset.  `record-slot-set' returns a truthy i64 (= rax 1 sentinel), so
;; threading the bump through `seq'/`and' preserves the original return value.
(defun nelisp-standalone--inject-epoch-bump (src defun-name)
  "Return SRC (a `(seq (defun ...) ...)') with the named DEFUN-NAME rewritten to
prepend a MUTATION-EPOCH bump (@268435544) before its body.  Wraps the body in
`(seq (ptr-write-u64 268435544 0 (1+ (ptr-read-u64 268435544 0))) ORIG-BODY)' so
the original return value is preserved (the bump's own value is discarded by
`seq')."
  (cons (car src)
        (mapcar
         (lambda (form)
           (if (and (consp form) (eq (car form) 'defun)
                    (eq (cadr form) defun-name))
               (let ((name (cadr form))
                     (arglist (caddr form))
                     (body (cdddr form)))   ; list of body forms
                 `(defun ,name ,arglist
                    (seq (ptr-write-u64 268435544 0
                                        (+ (ptr-read-u64 268435544 0) 1))
                         ,@body)))
             form))
         (cdr src))))

(defun nelisp-standalone--reader-extra-unit-epoch (entry defun-names)
  "Like `nelisp-standalone--reader-extra-unit' but inject an epoch bump into each
of DEFUN-NAMES in the unit's source (for persistent-install escape sites)."
  (pcase-let ((`(,name ,feat ,src) entry))
    (require feat)
    (let ((patched (symbol-value src)))
      (dolist (dn defun-names)
        (setq patched (nelisp-standalone--inject-epoch-bump patched dn)))
      (nelisp-standalone--cached-unit
       (concat (file-name-sans-extension name) "-epoch.o")
       patched (locate-library (symbol-name feat))))))

(defun nelisp-standalone--reader-units ()
  "Build the ORDERED reader-path unit list (start first, arena last).
Links the REAL special-form + env machinery (no trap stubs) so the binary is a
genuine general interpreter for the 11 special forms + installed builtins."
  (let* ((start (nelisp-standalone--target-start-unit t))
         (driver (let* ((name (nelisp-standalone--target-object-name "driver.o"))
                        (u (nelisp-standalone--compile-to-unit
                            name (nelisp-standalone--reader-driver-source))))
                   (push name nelisp-standalone--recompiled) u))
         ;; helpers: shipped manifest minus arena/trap and minus the units we
         ;; build from PATCHED sources below (eval-inner = M4 keyword self-eval,
         ;; combiner-cons = M6 catch/throw, combiner-apply = M3 do_fset fix).
         (helpers (delq nil
                        (mapcar (lambda (entry)
                                  (pcase-let ((`(,name ,kind ,_src) entry))
                                    (unless (or (memq kind '(:start :driver))
                                                (string= name "arena.o")
                                                (string= name "trap.o")
                                                ;; applyfn.o built from the FULL
                                                ;; reader source (HT+str+file)
                                                ;; below, not the baked subset.
                                                (string= name "applyfn.o")
                                                (string= name "eval-inner.o")
                                                (string= name "combiner-cons.o")
                                                (string= name "combiner-apply.o"))
                                      (nelisp-standalone--unit-for entry))))
                                nelisp-standalone--manifest)))
         ;; Full reader applyfn (arithmetic + HT + strings/format + file I/O).
         (applyfn (nelisp-standalone--cached-unit
                   "applyfn-reader.o" nelisp-standalone--applyfn-source
                   nelisp-standalone--this-file))
         ;; M4: keyword self-eval patched eval-inner.
         (eval-inner (progn
                       (require 'nelisp-cc-eval-inner)
                       (nelisp-standalone--cached-unit
                        "eval-inner-kw.o"
                        (nelisp-standalone--patch-eval-inner
                         (symbol-value 'nelisp-cc-eval-inner--source))
                        nelisp-standalone--this-file)))
         ;; M6 catch/throw dispatch + void-function miss fix.  The old
         ;; macro-expansion cache is disabled inside the patch for Doc 147.
         (combiner-cons (progn
                          (require 'nelisp-cc-evalport-combiner-cons)
                          (nelisp-standalone--cached-unit
                           "combiner-cons-ct-doc147.o"
                           (nelisp-standalone--patch-combiner-cons-full
                            (symbol-value 'nelisp-cc-evalport-combiner-cons--source))
                           nelisp-standalone--this-file)))
         ;; M3: do_fset rc-fix patched combiner-apply.
         (combiner (progn
                     (require 'nelisp-cc-evalport-combiner-apply)
                     (nelisp-standalone--cached-unit
                      "combiner-apply-fix.o"
                      (nelisp-standalone--patch-combiner-apply
                       (symbol-value 'nelisp-cc-evalport-combiner-apply--source))
                      nelisp-standalone--this-file)))
         (extras (mapcar #'nelisp-standalone--reader-extra-unit
                         nelisp-standalone--reader-extra-manifest))
         (float-stub (nelisp-standalone--reader-float-unit))
         (float-time-stub (nelisp-standalone--float-time-unit))
         ;; real-sf: all real special-form units EXCEPT sf-cc.o (built from the
         ;; WAVE-2 PATCH-4 source below) and the two PERSISTENT-INSTALL escape
         ;; sites (sf-frame-ensure-cap.o / sf-env-set-value2.o), which are built
         ;; with an injected MUTATION-EPOCH bump so the per-eval arena reset never
         ;; frees a grown frame-backing or a setq-installed binding (see
         ;; `nelisp-standalone--inject-epoch-bump').
         (real-sf (delq nil
                        (mapcar
                         (lambda (entry)
                           (pcase (car entry)
                             ("sf-cc.o" nil)
                             ("sf-frame-ensure-cap.o"
                              (nelisp-standalone--reader-extra-unit-epoch
                               entry '(nelisp_frame_stack_ensure_capacity_grow)))
                             ("sf-env-set-value.o"
                              (progn
                                (require 'nelisp-cc-evalport-env-leaves-bind)
                                (nelisp-standalone--cached-unit
                                 "sf-env-set-value-bind-rest-fix.o"
                                 (nelisp-standalone--patch-env-leaves-bind-rest
                                  (symbol-value 'nelisp-cc-evalport-env-leaves-bind--source))
                                 nelisp-standalone--this-file)))
                             ("sf-env-set-value2.o"
                              (nelisp-standalone--reader-extra-unit-epoch
                               entry '(nelisp_env_set_value)))
                             (_ (nelisp-standalone--reader-extra-unit entry))))
                         nelisp-standalone--reader-real-sf-manifest)))
         ;; WAVE-2 PATCH 4: sf-condition-case with nl_sf_cc_after_match rewritten
         ;; to clear the M6 arena signal flag on a clause MATCH.
         (sf-cc (progn
                  (require 'nelisp-cc-sf-condition-case)
                  (nelisp-standalone--cached-unit
                   "sf-cc-flagclear.o"
                   (nelisp-standalone--patch-sf-cc
                    (symbol-value 'nelisp-cc-sf-condition-case--source))
                   nelisp-standalone--this-file)))
         (capture (nelisp-standalone--cached-unit
                   "reader-capture.o" nelisp-standalone--reader-capture-source
                   nelisp-standalone--this-file))
         (errstub (nelisp-standalone--cached-unit
                   "reader-errstub.o" nelisp-standalone--reader-errstub-source
                   nelisp-standalone--this-file))
         ;; M7: file-I/O builtin impls (wrf/rdf/slen) referenced by applyfn.o.
         (fileio (nelisp-standalone--cached-unit
                  "reader-fileio.o" nelisp-standalone--fileio-source
                  nelisp-standalone--this-file))
         ;; M6: catch/throw glue (nl_sf_catch/nl_sf_throw) referenced by the
         ;; patched combiner-cons.
         (catch-throw (nelisp-standalone--cached-unit
                       "reader-catch-throw.o" nelisp-standalone--catch-throw-source
                       nelisp-standalone--this-file))
         ;; Doc 140 Stage 5: top-level boundary reset helpers.  Keep this out of
         ;; the always-recompiled driver so focused CLI/REPL work stays cacheable.
         (boundary (nelisp-standalone--cached-unit
                    "reader-boundary.o" nelisp-standalone--reader-boundary-source
                    nelisp-standalone--this-file))
         ;; Reader parse/eval loop.  This is called by CLI, --load, file loads,
         ;; runtime-image dispatch, and REPL; keeping it cacheable avoids
         ;; recompiling the loop whenever only the driver command surface moves.
         (eval-source (nelisp-standalone--cached-unit
                       "reader-eval-source.o"
                       nelisp-standalone--reader-eval-source-source
                       nelisp-standalone--this-file))
         (neln-demo (nelisp-standalone--cached-unit
                     "reader-neln-demo.o"
                     (nelisp-standalone--reader-neln-demo-source)
                     nelisp-standalone--this-file))
         ;; Tracing mark-sweep GC (form-boundary reclaimer).  Compiled from
         ;; the inline source above; the driver calls `nl_gc_collect'.
         (gc (nelisp-standalone--cached-unit
              "reader-gc.o" nelisp-standalone--gc-source
              nelisp-standalone--this-file))
         (arena (nelisp-standalone--unit-for
                 (assoc "arena.o" nelisp-standalone--manifest)))
         ;; Doc 140 Stage 8: driver-owned bss slot holding chunk 0's runtime
         ;; base, referenced by the chunk-arena rewrite on chunked native
         ;; targets.
         (arena-base (when (nelisp-standalone--target-uses-dynamic-arena-base-p)
                       (nelisp-standalone--arena-base-slot-unit))))
    (append (list start driver applyfn) helpers
            (list eval-inner combiner-cons combiner)
            extras (list float-stub float-time-stub) real-sf
            (list sf-cc capture errstub fileio catch-throw boundary
                  eval-source neln-demo gc arena)
            (delq nil (list arena-base)))))

(defun nelisp-standalone--codesign-macos-adhoc (out)
  "Apply an ad-hoc code signature to OUT for the macos-aarch64 target.
The native Mach-O writer emits an UNSIGNED MH_EXECUTE; Apple Silicon's
kernel SIGKILLs any unsigned arm64 binary at exec time, so a fresh build
is unrunnable until signed.  When the system `codesign' is available we
sign in place (`codesign -f -s -'); otherwise (e.g. a Linux host
cross-building the macOS artifact) we warn so the builder can sign on a
Mac before running.  Re-signing an already-signed binary is harmless."
  (cond
   ((not (executable-find "codesign"))
    (message "[standalone] WARNING: codesign not found; %s is UNSIGNED and \
will be killed on Apple Silicon.  Run `codesign -f -s - %s' on macOS \
before executing." out out))
   ((zerop (call-process "codesign" nil nil nil "-f" "-s" "-" out))
    (message "[standalone] ad-hoc signed %s" out))
   (t
    (error "[standalone] codesign failed for %s" out))))

;;;###autoload
(defun nelisp-standalone--artifact-runtime-cache-skip-defun-p (name)
  "Return non-nil when NAME should be omitted from the runtime command cache."
  (let ((s (symbol-name name)))
    (or (member s
	                '("nelisp-artifact--call-process-quiet"
	                  "nelisp-artifact--read-log-if-exists"
	                  "nelisp-artifact--read-file-as-string"
	                  "nelisp-artifact--read-all-from-string"
	                  "nelisp-artifact--read-one-private-form"
	                  "nelisp-artifact--read-private-keyword-value"
	                  "nelisp-artifact--private-keyword-value-pos"
	                  "nelisp-artifact--read-private-symbol-token"
	                  "nelisp-artifact--read-private-integer-token"
	                  "nelisp-artifact--read-private-string-token"
	                  "nelisp-artifact--read-binary"
	                  "nelisp-artifact--target-arch"
	                  "nelisp-artifact--file-size"
	                  "nelisp-artifact--file-mtime"
	                  "nelisp-artifact--file-ctime"
	                  "nelisp-artifact--sibling-manifest-path"
	                  "nelisp-artifact--native-function-wrapper"
	                  "nelisp-artifact--native-function-symbol"
	                  "nelisp-artifact--native-function-artifact"
	                  "nelisp-artifact--native-function-fallback"
	                  "nelisp-artifact--native-function-meta"
	                  "nelisp-artifact--native-wrapper-p"
	                  "nelisp-artifact--note-native-dispatch"
	                  "nelisp-artifact-native-dispatch-report"
	                  "nelisp-artifact--install-native-functions"
	                  "nelisp-native-function-call"
	                  "nelisp-artifact--native-simple-integer-abi-p"
	                  "nelisp-artifact--all-integers-p"
	                  "nelisp-artifact--native-defun-forms"
	                  "nelisp-artifact--native-unsupported-report"
	                  "nelisp-artifact--normalize-native-policy"
	                  "nelisp-artifact--native-report-failures"
	                  "nelisp-artifact--native-failures-message"
	                  "nelisp-artifact--enforce-native-policy"
	                  "nelisp-artifact--native-section-plist"
	                  "nelisp-artifact--artifact-payload"
	                  "nelisp-artifact--artifact-string"
	                  "nelisp-artifact--preload-records"
	                  "nelisp-artifact--replace-file-atomically"
	                  "nelisp-artifact--runtime-image-forms"
	                  "nelisp-artifact--parse-payload"
	                  "nelisp-artifact--parse-payload-fast"
	                  "nelisp-artifact--read-payload"
	                  "nelisp-artifact--read-manifest-full"
	                  "nelisp-artifact--read-manifest-fast"
	                  "nelisp-artifact--read-manifest-for-load"
	                  "nelisp-artifact--validate-input-record"
	                  "nelisp-artifact--validate"
	                  "nelisp-artifact-load-file"
	                  "nelisp-artifact--validate-elc"
	                  "nelisp-artifact-read-manifest"
	                  "nelisp-artifact--artifact-kind"
	                  "nelisp-artifact--artifact-kind-from-suffix"
	                  "nelisp-artifact-source-artifact-path"
	                  "nelisp-artifact--source-artifact-candidates"
	                  "nelisp-artifact-load-source-file"
	                  "nelisp-artifact-load-source-or-source-file"
	                  "nelisp-artifact--eval-forms"
	                  "nelisp-artifact--parse-source-command-args"
	                  "load-elisp-source"
	                  "eval-elisp-source"
	                  "nelisp-artifact--small-string-hash"
	                  "nelisp-artifact--native-fast-driver-c"
	                  "nelisp-artifact--shell-quote"
	                  "nelisp-artifact--el-file-p"
	                  "nelisp-artifact--nonempty-lines"
	                  "nelisp-artifact--collect-el-files-with-find"
	                  "nelisp-artifact--collect-el-files"
	                  "nelisp-artifact--neln-artifact-p"
	                  "nelisp-artifact--collect-neln-artifacts-with-find"
	                  "nelisp-artifact--collect-neln-artifacts"
	                  "nelisp-artifact--audit-input-source-paths"
	                  "nelisp-artifact--audit-input-artifact-paths"
	                  "nelisp-artifact--native-report-native-count"
	                  "nelisp-artifact--native-report-gap-names"
	                  "nelisp-artifact--audit-existing-neln"
	                  "nelisp-artifact--audit-source-neln"
	                  "nelisp-artifact--audit-status-rank"
	                  "nelisp-artifact--audit-entry-line"
	                  "nelisp-artifact--audit-summary"
	                  "nelisp-artifact--parse-audit-args"
	                  "nelisp-artifact--unique-strings"
	                  "audit-elisp-artifacts"
	                  "exec-elisp-artifact"
	                  "eval-elisp-artifact"
	                  "inspect-elisp-artifact"
	                  "nelisp-artifact--canonical-integer-token-p"
	                  "nelisp-artifact--write-file"
                  "nelisp-artifact--write-base64-decoded-file"
                  "nelisp-artifact--write-native-object-file"
	                  "nelisp-artifact--read-native-object-base64"
	                  "nelisp-artifact--delete-if-exists"
	                  "nelisp-artifact--make-temp-path"
	                  "nelisp-artifact--make-temp-directory"
	                  "nelisp-artifact--file-record"
	                  "nelisp-artifact--form-profile-head"
	                  "nelisp-artifact--read-top-level-forms-rd-one"
	                  "nelisp-artifact--read-top-level-forms"
	                  "nelisp-artifact--compiler-plist"
	                  "nelisp-artifact--plist-put-present"
	                  "nelisp-artifact--extract-provided-feature"
	                  "nelisp-artifact--collect-features"
	                  "nelisp-artifact--try-compile-defun"
                  "nelisp-artifact--compile-top-level-form"
	                  "nelisp-artifact--write-elf-rel-object"
	                  "nelisp-artifact--native-defun-entry"
	                  "nelisp-artifact--native-defun-metadata"
	                  "nelisp-artifact--manifest-plist"
                  "nelisp-artifact--write-pair-atomically"
                  "nelisp-artifact-compile-file"
                  "nelisp-artifact--runtime-image-source"
                  "nelisp-artifact-compile-runtime-image-file"
                  "nelisp-artifact--byte-compile-to"
                  "nelisp-artifact--elc-manifest-plist"
                  "nelisp-artifact-compile-elc-file"
                  "nelisp-artifact-load-or-compile-source-file"
                  "nelisp-artifact-native-exec"
                  "nelisp-artifact--native-exec-cache-root"
                  "nelisp-artifact--native-exec-cache-key"
                  "nelisp-artifact--native-exec-cache-exe"
                  "nelisp-artifact--native-exec-fast-build"
                  "nelisp-artifact--native-exec-fast-exe"
                  "nelisp-artifact-native-exec-fast-simple-uncached"
                  "nelisp-artifact--native-exec-run-captured-stdout"
                  "nelisp-artifact--native-exec-run-captured"
                  "nelisp-artifact-native-exec-fast-simple"
                  "nelisp-artifact-native-exec-fast-simple-stdout"
                  "nelisp-artifact-native-exec-fast-simple-write-stdout"
                  "nelisp-artifact--native-exec-parse-stdout"
                  "nelisp-artifact-native-exec-general"
                  "compile-elisp-artifact"
                  "compile-elisp-artifacts"
                  "compile-runtime-image"
                  "native-exec-elisp-artifact"))
        (string-match-p "native-compile" s)
        (string-match-p "native-compiler" s)
        (string-match-p "native-driver" s)
        (string-match-p "native-trampoline" s)
        (string-match-p "native-general" s)
        (string-match-p "parse-compile" s))))

(defun nelisp-standalone--artifact-runtime-cache-filter-source (source)
  "Return SOURCE with cache-irrelevant heavy defuns removed."
  (let ((pos 0)
        (len (length source))
        (forms nil)
        form)
    (while (< pos len)
      (condition-case nil
          (let ((read-result (read-from-string source pos)))
            (setq form (car read-result))
            (setq pos (cdr read-result))
            (unless (and (consp form)
                         (eq (car form) 'defun)
                         (symbolp (nth 1 form))
                         (nelisp-standalone--artifact-runtime-cache-skip-defun-p
                          (nth 1 form)))
              (push form forms)))
        (end-of-file
         (setq pos len))))
    (mapconcat (lambda (form) (concat (prin1-to-string form) "\n"))
               (nreverse forms)
               "")))

(defun nelisp-standalone-build-artifact-runtime-cache ()
  "Build the standalone artifact command runtime `.nelc' cache.
The cache is an implementation detail of the user-facing `target/nelisp'
binary: artifact commands use it when present and fall back to the full source
loader when it is absent."
  (require 'nelisp-artifact)
  (make-directory (file-name-directory
                   nelisp-standalone--artifact-runtime-source-path)
                  t)
  (let* ((runtime-source (nelisp-standalone--artifact-command-runtime-src t))
         (body-marker "(defconst nelisp-artifact--magic")
         (body-start (or (string-search body-marker runtime-source) 0))
         (body-end (or (string-search
                        "(fset 'nelisp-artifact--read-file-as-string"
                        runtime-source body-start)
                       (length runtime-source)))
         (body-source (substring runtime-source body-start body-end)))
    (with-temp-file nelisp-standalone--artifact-runtime-source-path
      (insert (nelisp-standalone--artifact-runtime-cache-filter-source
               body-source))))
  (nelisp-artifact-compile-file
   nelisp-standalone--artifact-runtime-source-path
   nelisp-standalone--artifact-runtime-cache-path
   nil nil nil nil nil 'nelc)
  (with-temp-file nelisp-standalone--artifact-runtime-cache-enable-path
    (insert "enabled "
            (secure-hash
             'sha256
             (nelisp-artifact--read-file-as-string
              nelisp-standalone--artifact-runtime-cache-path))
            "\n"))
  (message "[standalone-reader] artifact runtime cache -> %s (enabled)"
           nelisp-standalone--artifact-runtime-cache-path)
  nelisp-standalone--artifact-runtime-cache-path)

;;;###autoload
(defun nelisp-standalone-build-reader ()
  "Incrementally build the reader-path standalone binary; return its path."
  (setq nelisp-standalone--recompiled nil)
  (let* ((units (nelisp-standalone--reader-units))
         (out (nelisp-standalone--output-path t)))
    (pcase nelisp-standalone--target
      ('windows-x86_64
       (nelisp-link-units-pe32 out units "_start"
                               (nelisp-standalone--reader-pe-imports)
                               (list :stack-reserve
                                     nelisp-standalone--windows-stack-reserve)))
      ('windows-aarch64
       (nelisp-link-units-pe32 out units "_start"
                               (nelisp-standalone--reader-pe-imports)
                               (list :machine 'aarch64
                                     :stack-reserve
                                     nelisp-standalone--windows-stack-reserve)))
      ('macos-aarch64
       (nelisp-link-units-macho-exec out units "_main" 'aarch64)
       (set-file-modes out #o755)
       (nelisp-standalone--codesign-macos-adhoc out))
      ('linux-aarch64
       (nelisp-link-units out units "_start" nil 'aarch64)
       (set-file-modes out #o755))
      (_
       (nelisp-link-units out units)
       (set-file-modes out #o755)))
    (message "[standalone-reader] linked %d units -> %s (src=%S)"
             (length units) out
             (nelisp-standalone--reader-src))
    (nelisp-standalone-build-artifact-runtime-cache)
    out))

;;;###autoload
(defun nelisp-standalone-reader-test ()
  "Build the reader binary, run it, assert exit == eval(NELISP_SRC).  Exits 0/1."
  (let* ((out (nelisp-standalone-build-reader))
         (code (call-process out nil nil nil "--embedded"))
        (expected (nelisp-standalone--reader-expected)))
    (if (= code expected)
        (condition-case err
            (progn
              (nelisp-standalone--reader-hash-table-literal-smoke)
              (nelisp-standalone--reader-large-quoted-alist-mutation-smoke)
              (nelisp-standalone--reader-setcar-setcdr-type-smoke)
              (nelisp-standalone--reader-runtime-image-smoke)
              (nelisp-standalone--reader-cli-smoke)
              (nelisp-standalone--reader-neln-selftest-smoke)
              (nelisp-standalone--reader-repl-smoke)
              (message "[standalone-reader] PASS: %S -> exit %d (expected %d)"
                       (nelisp-standalone--reader-src) code expected)
              (kill-emacs 0))
          (error
           (message "[standalone-reader] FAIL: command smoke: %s"
                    (error-message-string err))
           (kill-emacs 1)))
      (message "[standalone-reader] FAIL: %S -> exit %d (expected %d)"
               (nelisp-standalone--reader-src) code expected)
      (kill-emacs 1))))

;;;###autoload
(defun nelisp-standalone-reader-repl-test ()
  "Build the reader binary and run only the REPL smoke.  Exits 0/1."
  (nelisp-standalone-build-reader)
  (condition-case err
      (progn
        (nelisp-standalone--reader-repl-smoke)
        (kill-emacs 0))
    (error
     (message "[standalone-reader] FAIL: repl smoke: %s"
              (error-message-string err))
     (kill-emacs 1))))

(defun nelisp-standalone--reader-cli-smoke ()
  "Assert the short CLI supports help/eval/load and readable printed values."
  (let ((tmp (make-temp-file "nelisp-reader-cli-" nil ".el"))
        (help-out nil)
        (eval-out nil)
        (escape-out nil)
        (stats-out nil)
        (growth-out nil)
        (load-out nil)
        (artifact-src nil)
        (artifact-out nil)
        (runtime-image nil)
        (runtime-artifact-out nil)
        (runtime-artifact-eval-out nil)
        (artifact-inspect-out nil)
        (artifact-eval-out nil)
        (rc nil))
    (unwind-protect
        (progn
          (with-temp-buffer
            (setq rc (call-process nelisp-standalone--reader-out nil t nil "--help"))
            (setq help-out (buffer-string)))
          (unless (and (= rc 0) (string-match-p "Usage: nelisp" help-out))
            (error "--help exit=%S stdout=%S" rc help-out))
          (with-temp-buffer
            (setq rc (call-process nelisp-standalone--reader-out nil t nil
                                   "--eval" "(vector 1 \"a\" nil t)"))
            (setq eval-out (buffer-string)))
          (unless (and (= rc 0) (equal eval-out "[1 \"a\" nil t]\n"))
            (error "--eval exit=%S stdout=%S" rc eval-out))
          (with-temp-buffer
            (setq rc (call-process nelisp-standalone--reader-out nil t nil
                                   "--eval" "(concat \"\\\"\" \"\\\\\")"))
            (setq escape-out (buffer-string)))
          (unless (and (= rc 0) (equal escape-out "\"\\\"\\\\\"\n"))
            (error "--eval escape exit=%S stdout=%S" rc escape-out))
          (with-temp-buffer
            (setq rc (call-process nelisp-standalone--reader-out nil t nil
                                   "--eval" "(nelisp--arena-stats)"))
            (setq stats-out (buffer-string)))
          (unless (and (= rc 0)
                       (string-match-p
                        "\\`([0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+ [01] [01] [0-9]+ [0-9]+ [0-9]+)\n\\'"
                        stats-out))
            (error "--eval arena-stats exit=%S stdout=%S" rc stats-out))
          (with-temp-buffer
            (setq rc (call-process nelisp-standalone--reader-out nil t nil
                                   "--eval" "(nelisp--arena-force-grow-smoke)"))
            (setq growth-out (buffer-string)))
          (unless (and (= rc 0)
                       (string-match-p
                        "\\`([0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+ [01] [01] [2-9][0-9]* [0-9]+ [0-9]+)\n\\'"
                        growth-out))
            (error "--eval arena-force-grow exit=%S stdout=%S" rc growth-out))
          (with-temp-file tmp
            (insert "(list 1 2 3)\n"))
          (with-temp-buffer
            (setq rc (call-process nelisp-standalone--reader-out nil t nil
                                   "--load" tmp))
            (setq load-out (buffer-string)))
          (unless (and (= rc 0) (equal load-out "(1 2 3)\n"))
            (error "--load exit=%S stdout=%S" rc load-out))
          (setq artifact-src (make-temp-file "nelisp-reader-artifact-" nil ".el"))
          (setq artifact-out (make-temp-file "nelisp-reader-artifact-" nil ".nelc"))
          (ignore-errors (delete-file artifact-out))
          (with-temp-file artifact-src
            (insert "(defun nelisp-reader-artifact-hot (x) (+ x 1))\n"))
          (with-temp-buffer
            (setq rc (call-process nelisp-standalone--reader-out nil t nil
                                   "compile-elisp-artifact"
                                   "--kind" "nelc"
                                   "--input" artifact-src
                                   "--output" artifact-out)))
          (unless (and (= rc 0)
                       (file-exists-p artifact-out)
                       (file-exists-p (concat artifact-out ".manifest.el")))
            (error "compile-elisp-artifact exit=%S output=%S" rc artifact-out))
          (with-temp-buffer
            (setq rc (call-process nelisp-standalone--reader-out nil t nil
                                   "eval-elisp-artifact" artifact-out
                                   "(nelisp-reader-artifact-hot 41)"))
            (setq artifact-eval-out (buffer-string)))
          (unless (and (= rc 0) (equal artifact-eval-out "42\n"))
            (error "eval-elisp-artifact exit=%S stdout=%S" rc artifact-eval-out))
          (setq runtime-image
                (make-temp-file "nelisp-reader-runtime-image-" nil ".nlri"))
          (setq runtime-artifact-out
                (make-temp-file "nelisp-reader-runtime-image-" nil ".nelc"))
          (ignore-errors (delete-file runtime-artifact-out))
          (with-temp-buffer
            (setq rc (call-process nelisp-standalone--reader-out nil t nil
                                   "dump-runtime-image" runtime-image
                                   "(defun nelisp-reader-runtime-hot (x) (+ x 2))")))
          (unless (and (= rc 0) (file-exists-p runtime-image))
            (error "dump-runtime-image artifact source exit=%S" rc))
          (with-temp-buffer
            (setq rc (call-process nelisp-standalone--reader-out nil t nil
                                   "compile-runtime-image"
                                   "--kind" "nelc"
                                   "--input" runtime-image
                                   "--output" runtime-artifact-out)))
          (unless (and (= rc 0)
                       (file-exists-p runtime-artifact-out)
                       (file-exists-p
                        (concat runtime-artifact-out ".manifest.el")))
            (error "compile-runtime-image exit=%S output=%S"
                   rc runtime-artifact-out))
          (with-temp-buffer
            (setq rc (call-process nelisp-standalone--reader-out nil t nil
                                   "eval-elisp-artifact" runtime-artifact-out
                                   "(nelisp-reader-runtime-hot 40)"))
            (setq runtime-artifact-eval-out (buffer-string)))
          (unless (and (= rc 0) (equal runtime-artifact-eval-out "42\n"))
            (error "eval runtime artifact exit=%S stdout=%S"
                   rc runtime-artifact-eval-out))
          (with-temp-buffer
            (setq rc (call-process nelisp-standalone--reader-out nil t nil
                                   "inspect-elisp-artifact" artifact-out))
            (setq artifact-inspect-out (buffer-string)))
          (unless (and (= rc 0)
                       (string-match-p "nelisp-elisp-artifact-manifest-v1"
                                       artifact-inspect-out))
            (error "inspect-elisp-artifact exit=%S stdout=%S"
                   rc artifact-inspect-out))
          (message "[standalone-reader] cli smoke PASS"))
      (ignore-errors (delete-file tmp))
      (ignore-errors (delete-file artifact-src))
      (ignore-errors (delete-file artifact-out))
      (ignore-errors (delete-file (concat artifact-out ".manifest.el")))
      (ignore-errors (delete-file runtime-image))
      (ignore-errors (delete-file runtime-artifact-out))
      (ignore-errors (delete-file (concat runtime-artifact-out ".manifest.el"))))))

(defun nelisp-standalone--reader-hash-table-literal-smoke ()
  "Assert standalone-reader materialises generated reader literals."
  (let ((tmp (make-temp-file "nelisp-reader-hash-table-" nil ".el"))
        (rc nil))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert
             "(if (= (hash-table-count '#s(hash-table test equal data (\"a\" 42 \"b\" 7))) 2) 42 13)\n"
             "(if (= (gethash \"a\" '#s(hash-table test equal data (\"a\" 42 \"b\" 7))) 42) 42 13)\n"
             "(if (= (gethash \"y\" (gethash \"x\" '#s(hash-table test equal data (\"x\" #s(hash-table test equal data (\"y\" 42)))))) 42) 42 13)\n"
             "(if (= (length #^[nil nil nil #^^[3 0 t nil]]) 4) 42 13)\n"
             "(if (equal (elt #^[nil nil nil #^^[1 0 #^^[2 0 #^^[3 0 t nil \"a\"]]]] 2) \"a\") 42 13)\n"))
          (setq rc (call-process nelisp-standalone--reader-out nil nil nil tmp))
          (unless (= rc 42)
            (error "generated reader literal smoke exit=%S" rc)))
      (ignore-errors (delete-file tmp)))))

(defun nelisp-standalone--reader-large-quoted-alist-mutation-smoke ()
  "Assert reader slot-pool handles large quoted alists that later mutate.

Vendored Emacs `international/eucjp-ms.el' builds a 2069-entry literal alist,
then destructively flips each pair with `setcar' and `setcdr'.  The reader's
flat-list tail shape consumes slots proportional to list length, so this smoke
guards the slot-pool floor directly without loading the full vendor file."
  (let ((tmp (make-temp-file "nelisp-reader-large-alist-" nil ".el"))
        (pairs nil)
        (rc nil))
    (dotimes (i 2069)
      (push (cons i (+ i 1)) pairs))
    (setq pairs (nreverse pairs))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (prin1
             `(let ((map ',pairs)
                    (cur nil))
                (setq cur map)
                (while cur
                  (let ((x (car cur)))
                    (let ((tmp (car x)))
                      (setcar x (cdr x))
                      (setcdr x tmp)))
                  (setq cur (cdr cur)))
                (if (= (car (car map)) 1)
                    (if (= (cdr (car map)) 0)
                        (if (= (car (car (cdr (cdr map)))) 3)
                            (if (= (cdr (car (cdr (cdr map)))) 2)
                                42
                              13)
                          13)
                      13)
                  13))
             (current-buffer))
            (terpri (current-buffer)))
          (setq rc (call-process nelisp-standalone--reader-out nil nil nil tmp))
          (unless (= rc 42)
            (error "large quoted alist mutation smoke exit=%S" rc)))
      (ignore-errors (delete-file tmp)))))

(defun nelisp-standalone--reader-setcar-setcdr-type-smoke ()
  "Assert non-cons setcar/setcdr signal instead of crashing."
  (let ((tmp (make-temp-file "nelisp-reader-setcar-type-" nil ".el"))
        (rc nil))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert
             "(let ((a (condition-case e (setcar nil 1) (wrong-type-argument (car e))))\n"
             "      (b (condition-case e (setcdr (quote nope) 1) (wrong-type-argument (car e)))))\n"
             "  (if (eq a (quote wrong-type-argument))\n"
             "      (if (eq b (quote wrong-type-argument)) 42 13)\n"
             "    13))\n"))
          (setq rc (call-process nelisp-standalone--reader-out nil nil nil tmp))
          (unless (= rc 42)
            (error "setcar/setcdr wrong-type smoke exit=%S" rc)))
      (ignore-errors (delete-file tmp)))))

(defun nelisp-standalone--reader-runtime-image-smoke ()
  "Assert standalone-reader runtime-image eval/exec command semantics."
  (let ((tmp (make-temp-file "nelisp-runtime-smoke-" nil ".nlri"))
        (tmp-fn (make-temp-file "nelisp-runtime-smoke-fn-" nil ".nlri"))
        (tmp-src (make-temp-file "nelisp-runtime-smoke-src-" nil ".el"))
        (tmp-load (make-temp-file "nelisp-runtime-smoke-load-" nil ".nlri"))
        (dump-rc nil)
        (dump-fn-rc nil)
        (dump-load-rc nil)
        (eval-rc nil)
        (eval-out nil)
        (eval-fn-rc nil)
        (eval-fn-out nil)
        (eval-load-rc nil)
        (eval-load-out nil)
        (exec-rc nil)
        (exec-out nil)
        (missing-rc nil))
    (unwind-protect
        (progn
          (with-temp-buffer
            (setq dump-rc
                  (call-process nelisp-standalone--reader-out nil t nil
                                "dump-runtime-image" tmp "(setq base 40)")))
          (unless (= dump-rc 0)
            (error "dump-runtime-image exit=%S" dump-rc))
          (with-temp-buffer
            (setq eval-rc
                  (call-process nelisp-standalone--reader-out nil t nil
                                "eval-runtime-image" tmp
                                "(setq add 2)" "(+ base add)"))
            (setq eval-out (buffer-string)))
          (unless (and (= eval-rc 0) (equal eval-out "42\n"))
            (error "eval-runtime-image exit=%S stdout=%S" eval-rc eval-out))
          (with-temp-buffer
            (setq dump-fn-rc
                  (call-process nelisp-standalone--reader-out nil t nil
                                "dump-runtime-image" tmp-fn
                                "(defun image-hot () 99)")))
          (unless (= dump-fn-rc 0)
            (error "dump-runtime-image defun exit=%S" dump-fn-rc))
          (with-temp-buffer
            (setq eval-fn-rc
                  (call-process nelisp-standalone--reader-out nil t nil
                                "eval-runtime-image" tmp-fn
                                "(image-hot)"))
            (setq eval-fn-out (buffer-string)))
          (unless (and (= eval-fn-rc 0) (equal eval-fn-out "99\n"))
            (error "eval-runtime-image defun exit=%S stdout=%S"
                   eval-fn-rc eval-fn-out))
          (with-temp-file tmp-src
            (insert "(setq loaded-base 39)\n(defun loaded-hot () 3)\n"))
          (with-temp-buffer
            (setq dump-load-rc
                  (call-process nelisp-standalone--reader-out nil t nil
                                "dump-runtime-image" tmp-load
                                "--load" tmp-src
                                "(setq loaded-add 0)")))
          (unless (= dump-load-rc 0)
            (error "dump-runtime-image --load exit=%S" dump-load-rc))
          (with-temp-buffer
            (setq eval-load-rc
                  (call-process nelisp-standalone--reader-out nil t nil
                                "eval-runtime-image" tmp-load
                                "(+ loaded-base loaded-add (loaded-hot))"))
            (setq eval-load-out (buffer-string)))
          (unless (and (= eval-load-rc 0) (equal eval-load-out "42\n"))
            (error "eval-runtime-image --load exit=%S stdout=%S"
                   eval-load-rc eval-load-out))
          (with-temp-buffer
            (setq exec-rc
                  (call-process nelisp-standalone--reader-out nil t nil
                                "exec-runtime-image" tmp
                                "(setq add 2)" "(+ base add)"))
            (setq exec-out (buffer-string)))
          (unless (and (= exec-rc 0) (equal exec-out ""))
            (error "exec-runtime-image exit=%S stdout=%S" exec-rc exec-out))
          (with-temp-buffer
            (setq missing-rc
                  (call-process nelisp-standalone--reader-out nil t nil
                                "exec-runtime-image" tmp)))
          (unless (= missing-rc 1)
            (error "missing-form exec-runtime-image exit=%S" missing-rc))
          (message "[standalone-reader] runtime-image smoke PASS"))
      (when (file-exists-p tmp)
        (delete-file tmp))
      (when (file-exists-p tmp-fn)
        (delete-file tmp-fn))
      (when (file-exists-p tmp-src)
        (delete-file tmp-src))
      (when (file-exists-p tmp-load)
        (delete-file tmp-load)))))

(defun nelisp-standalone--reader-neln-selftest-smoke ()
  "Assert `--neln-selftest' executes the embedded in-process native demo."
  (let ((rc nil)
        (expected (if (eq nelisp-standalone--target 'linux-x86_64) 42 125))
        (stdout nil))
    (with-temp-buffer
      (setq rc (call-process nelisp-standalone--reader-out nil t nil
                             "--neln-selftest"))
      (setq stdout (buffer-string)))
    (unless (and (= rc expected) (equal stdout ""))
      (error "--neln-selftest exit=%S (expected %S) stdout=%S"
             rc expected stdout))
    (message "[standalone-reader] neln selftest PASS")))

(defun nelisp-standalone--reader-repl-smoke ()
  "Assert standalone-reader `repl --no-prompt' keeps one live environment."
  (let ((stdin (concat
                "(defun hot () 1)\n"
                "(hot)\n"
                "(defun hot () 42)\n"
                "(hot)\n"
                "(nelisp--eval-source-string \"(defun hot () 99)\")\n"
                "(hot)\n"
                "(condition-case e (signal 'quit nil) (quit 42))\n"
                "nil\n"
                "t\n"
                "(quote (1 2 3))\n"
                "(vector 1 \"a\" nil t)\n"
                "(exit)\n"))
        (repl-rc nil)
        (repl-out nil)
        (quiet-rc nil)
        (quiet-out nil)
        (no-args-rc nil)
        (no-args-out nil)
        (near-end-file (make-temp-file "nelisp-repl-near-end-" nil ".el"))
        (near-end-rc nil)
        (near-end-out nil)
        (near-end-bump (max 256
                            (- (nelisp-standalone--target-arena-size)
                               nelisp-standalone--reader-read-cap
                               65536)))
        (bad-rc nil))
    (with-temp-buffer
      (insert stdin)
      (setq repl-rc
            (call-process-region (point-min) (point-max)
                                 nelisp-standalone--reader-out
                                 t t nil
                                 "--repl" "--no-prompt"))
      (setq repl-out (buffer-string)))
    (unless (and (= repl-rc 0)
                 (equal repl-out "hot\n1\nhot\n42\nhot\n99\n42\nnil\nt\n(1 2 3)\n[1 \"a\" nil t]\n"))
      (error "repl exit=%S stdout=%S" repl-rc repl-out))
    (with-temp-buffer
      (insert "(defun hot () 1)\n")
      (insert "(hot)\n")
      (insert "(condition-case e (signal 'quit nil) (quit 42))\n")
      (insert "(nelisp--write-stdout-bytes \"explicit\\n\")\n")
      (insert "(hot)\n")
      (insert "(exit)\n")
      (setq quiet-rc
            (call-process-region (point-min) (point-max)
                                 nelisp-standalone--reader-out
                                 t t nil
                                 "--repl" "--no-prompt" "--no-print"))
      (setq quiet-out (buffer-string)))
    (unless (and (= quiet-rc 0)
                 (equal quiet-out "explicit\n"))
      (error "repl --no-print exit=%S stdout=%S" quiet-rc quiet-out))
    (with-temp-buffer
      (insert "(exit)\n")
      (setq no-args-rc
            (call-process-region (point-min) (point-max)
                                 nelisp-standalone--reader-out
                                 t t nil))
      (setq no-args-out (buffer-string)))
    (unless (and (= no-args-rc 0)
                 (equal no-args-out "nelisp> "))
      (error "no-args repl exit=%S stdout=%S" no-args-rc no-args-out))
    (unwind-protect
        (progn
          (with-temp-file near-end-file
            (insert "(setq near-end-ok 42)\n"))
          (with-temp-buffer
            ;; Doc 140 Stage 8 (linux): the chunk-0 bump cursor is at the
            ;; runtime mmap base + 0; runtime-parsed test code reaches it via
            ;; `(car (nelisp--arena-stats))' rather than a fixed immediate.
            (if (eq nelisp-standalone--target 'linux-x86_64)
                (insert (format "(ptr-write-u64 (car (nelisp--arena-stats)) 0 %d)\n"
                                near-end-bump))
              (insert (format "(ptr-write-u64 %d 0 %d)\n"
                              (nelisp-standalone--target-arena-metadata-address 0)
                              near-end-bump)))
            (insert (format "(if (= (length (rdf %S)) 22) (nelisp--write-stdout-bytes \"near-end-ok\\n\") (nelisp--write-stdout-bytes \"near-end-bad\\n\"))\n"
                            near-end-file))
            (insert "(exit)\n")
            (setq near-end-rc
                  (call-process-region (point-min) (point-max)
                                       nelisp-standalone--reader-out
                                       t t nil
                                       "--repl" "--no-prompt" "--no-print"))
            (setq near-end-out (buffer-string)))
          (unless (and (= near-end-rc 0)
                       (equal near-end-out "near-end-ok\n"))
            (error "repl near-end rdf exit=%S stdout=%S"
                   near-end-rc near-end-out)))
      (ignore-errors (delete-file near-end-file)))
    (with-temp-buffer
      (setq bad-rc
            (call-process nelisp-standalone--reader-out nil t nil
                          "--repl" "--bad")))
    (unless (= bad-rc 2)
      (error "repl --bad exit=%S" bad-rc))
    (message "[standalone-reader] repl smoke PASS")))

(defconst nelisp-standalone--prelude-file
  (expand-file-name "scripts/nelisp-stdlib-prelude.el"
                    nelisp-standalone--repo-root)
  "Loadable stdlib prelude (defmacro bootstrap + core macros + list/search/hof/
plist/backquote lib).  The binary loads it via file-load before user code; see
its header for the `cat PRELUDE yourfile.el | binary' usage.")

(defun nelisp-standalone--prelude-breadth-test-src ()
  "Breadth test exercising the Wave-1 (B) primitives + the prelude macros.
Returns 42: defun+cond, dolist+setq, nth, plist-get, and a backquote `length'
all combine so a single wrong primitive shifts the result off 42."
  (concat
   "(defun bt-f (x) (cond ((< x 0) 0) (t (* x 2))))\n"
   "(let ((s 0)) (dolist (x (list 10 20 12)) (setq s (+ s x)))\n"
   "  (let ((b 2) (c (list 3 4)))\n"
   "    (+ (if (= (bt-f 21) 42) 0 100)\n"          ; defun+cond -> 42 ? +0 : +100
   "       (if (= s 42) 0 100)\n"                  ; dolist sum 10+20+12 -> 42
   "       (if (= (nth 2 (list 9 8 42 7)) 42) 0 100)\n"   ; nth -> 42
   "       (if (= (plist-get (list 'a 1 'b 42) 'b) 42) 0 100)\n" ; plist-get -> 42
   "       (if (= ?λ 955) 0 100)\n"                 ; UTF-8 char literal -> codepoint
   "       (if (= (length `(1 ,b ,@c 5)) 5) 42 100))))\n")) ; backquote length 5 -> 42

;;;###autoload
(defun nelisp-standalone-reader-prelude-test ()
  "Build the reader binary, then run it on a test file = the stdlib prelude
\(`scripts/nelisp-stdlib-prelude.el') followed by a breadth test, and assert the
binary's exit code == 42.  This proves the prelude LOADS AS-IS on standalone
NeLisp and the Wave-1 (B) breadth primitives back the stdlib (cond/dolist/nth/
plist-get/backquote).  Exits 0/1."
  (nelisp-standalone-build-reader)
  (let* ((tmp (make-temp-file "nelisp-prelude-breadth-" nil ".el"))
         (expected 42))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert-file-contents nelisp-standalone--prelude-file)
            (goto-char (point-max))
            (insert "\n" (nelisp-standalone--prelude-breadth-test-src)))
          (let ((code (call-process nelisp-standalone--reader-out nil nil nil tmp)))
            (if (= code expected)
                (progn
                  (message "[standalone-reader-prelude] PASS: prelude + breadth -> exit %d (expected %d)"
                           code expected)
                  (kill-emacs 0))
              (message "[standalone-reader-prelude] FAIL: prelude + breadth -> exit %d (expected %d)"
                       code expected)
              (kill-emacs 1))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(provide 'nelisp-standalone-build)

;;; nelisp-standalone-build.el ends here
