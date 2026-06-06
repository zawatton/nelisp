;;; nelisp-standalone-build.el --- Pure-elisp standalone NeLisp eval build  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Reproducible, INCREMENTAL build of a standalone NeLisp eval binary that
;; needs ZERO Rust (no cargo / rustc / target binary).  The REAL NeLisp
;; evaluator (nl_eval_inner + the combiner cons/apply cluster + bootstrap
;; mirror) is compiled by the pure-elisp Phase 47 compiler into relocatable
;; units and linked by the pure-elisp static linker into a freestanding
;; static ELF.  The single primitive that was Rust-only (nelisp_apply_function,
;; the builtin name -> native op dispatcher) is supplied here in pure elisp.
;;
;; Toolchain (all pure elisp):
;;   each unit source -> `nelisp-phase47-compiler' (my-compile-to-unit) ->
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
(require 'nelisp-phase47-compiler)
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
    ('windows-x86_64 nelisp-standalone--windows-arena-base)
    ('macos-aarch64 nelisp-standalone--macos-arena-base)
    (_ nelisp-standalone--arena-base)))

(defun nelisp-standalone--target-arena-size (&optional target)
  "Return standalone arena size for TARGET."
  (pcase (or target nelisp-standalone--target)
    ('windows-x86_64 nelisp-standalone--windows-arena-size)
    ('macos-aarch64 nelisp-standalone--macos-arena-size)
    (_ nelisp-standalone--linux-arena-size)))

(defun nelisp-standalone--target-arena-metadata-address (offset &optional target)
  "Return target arena metadata address at OFFSET."
  (+ (nelisp-standalone--target-arena-base target) offset))

(defun nelisp-standalone--target-abi (&optional target)
  "Return the compiler ABI for standalone TARGET."
  (pcase (or target nelisp-standalone--target)
    ('linux-x86_64 'sysv)
    ('macos-aarch64 'aapcs64)
    ('windows-x86_64 'win64)
    (other (error "standalone: unsupported target %S" other))))

(defun nelisp-standalone--target-arch (&optional target)
  "Return the Phase47 architecture for standalone TARGET."
  (pcase (or target nelisp-standalone--target)
    ('macos-aarch64 'aarch64)
    ((or 'linux-x86_64 'windows-x86_64) 'x86_64)
    (other (error "standalone: unsupported target %S" other))))

(defun nelisp-standalone--target-os (&optional target)
  "Return the Phase47 OS tag for standalone TARGET."
  (pcase (or target nelisp-standalone--target)
    ('macos-aarch64 'darwin)
    ('windows-x86_64 'windows)
    ('linux-x86_64 'linux)
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
    (_ (if reader-p nelisp-standalone--reader-out nelisp-standalone--out))))

(defun nelisp-standalone--dep-files ()
  "Toolchain source files; any newer than a cache entry forces recompile."
  (delq nil (cons nelisp-standalone--this-file
                  (mapcar #'locate-library
                          '("nelisp-phase47-compiler" "nelisp-static-linker"
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
                 '(linux-x86_64 windows-x86_64 macos-aarch64)))
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
   (list (cons 'bss 8))
   (list (nelisp-link-symbol "nl_arena_base" 0
                             :section 'bss :bind 'global :type 'object))
   nil))

(defun nelisp-standalone--target-uses-dynamic-arena-base-p (&optional target)
  "Return non-nil when TARGET stores chunk 0's runtime base in `nl_arena_base'."
  (memq (or target nelisp-standalone--target)
        '(linux-x86_64 windows-x86_64 macos-aarch64)))

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
         (nelisp-phase47-compiler--label-counter 0)
         (nelisp-phase47-compiler--arch arch)
         (nelisp-phase47-compiler--os (nelisp-standalone--target-os))
         (nelisp-phase47-compiler--allow-external-user-calls t)
         (nelisp-phase47-compiler--abi resolved-abi)
         (ir (nelisp-phase47-compiler--parse source nil))
         (defuns (nelisp-phase47-compiler--collect-defuns ir))
         (buf (if (eq arch 'aarch64)
                  (nelisp-asm-arm64-make-buffer)
                (nelisp-asm-x86_64-make-buffer resolved-abi))))
    (dolist (d defuns) (nelisp-phase47-compiler--emit-defun d buf))
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
                               (let ((nm (nelisp-phase47-compiler--ir-get d :name)))
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
    (defun nl_block_total (size)
      (let ((p (nl_align_up size 8))) (+ 16 (if (< p 8) 8 p))))
    (defun nl_arena_init () 0)
    (defun nl_os_alloc_chunk (_size) 0)
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
    (defun nl_freelist_scan (prev cur want)
      (if (= cur 0)
          0
        (if (= (ptr-read-u64 (- cur 16) 0) want)
            (nl_seq2
             (if (= prev 0)
                 (ptr-write-u64 268435552 0 (ptr-read-u64 cur 0))
               (ptr-write-u64 prev 0 (ptr-read-u64 cur 0)))
             (nl_seq2 (ptr-write-u64 (- cur 8) 0 0) cur))
          (nl_freelist_scan cur (ptr-read-u64 cur 0) want))))
    ;; Pop an exact-fit block for WANT (BLOCK_TOTAL): O(1) bucket pop for
    ;; 24<=WANT<=480, else scan the fallback list.  Clears the FREE sentinel
    ;; (mark 0) and returns the object pointer, or 0 if none.
    (defun nl_freelist_take (want)
      (if (< want 24)
          (nl_freelist_scan 0 (ptr-read-u64 268435552 0) want)
        (if (< 480 want)
            (nl_freelist_scan 0 (ptr-read-u64 268435552 0) want)
          (let* ((head (+ 268435696 (- want 24)))
                 (cur (ptr-read-u64 head 0)))
            (if (= cur 0)
                0
              (nl_seq2 (ptr-write-u64 head 0 (ptr-read-u64 cur 0))
                       (nl_seq2 (ptr-write-u64 (- cur 8) 0 0) cur)))))))
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
                    (setq obj (+ base (+ old 16)))
                    (ptr-write-u64 (- obj 16) 0 want)
                    (ptr-write-u64 (- obj 8) 0 0)
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
                            (nl_seq2 (if (= r 0) 0 (nl_alloc_zero_fill r 0 (- want 16))) r))))))
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
      (ptr-write-u64 (+ ,b 160) 0 0)        ; collect ENABLED (S0 sound-GC: form-boundary mark-sweep)
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
            base))))))

(defun nelisp-standalone--linux-alloc-chunk-form ()
  "Return Linux chunk allocation forms using mmap(NULL, ...)."
  '((defun nl_os_alloc_chunk (size)
      (let ((p (syscall-direct 9 0 size 3 34 -1 0)))
        (if (< p 4096) 0 p)))
    (defun nl_os_alloc_fail ()
      (syscall-direct 60 88 0 0 0 0 0))))

(defun nelisp-standalone--windows-arena-init-form ()
  "Return the Windows `nl_arena_init' form using VirtualAlloc(NULL, ...)."
  `(defun nl_arena_init ()
     (let ((base (extern-call VirtualAlloc 0
                              ,nelisp-standalone--windows-arena-size
                              12288 4)))
       (if (= base 0)
           (extern-call ExitProcess 88)
         (seq
          (ptr-write-u64 (data-addr nl_arena_base) 0 base)
          ,@(nelisp-standalone--arena-init-metadata-forms-dynamic
             'base nelisp-standalone--windows-arena-size)
          base)))))

(defun nelisp-standalone--windows-alloc-chunk-form ()
  "Return Windows chunk allocation forms using VirtualAlloc(NULL, ...)."
  '((defun nl_os_alloc_chunk (size)
      (extern-call VirtualAlloc 0 size 12288 4))
    (defun nl_os_alloc_fail ()
      (extern-call ExitProcess 88))))

(defun nelisp-standalone--macos-arena-init-form ()
  "Return the macOS `nl_arena_init' form using Darwin mmap(NULL, ...)."
  `(defun nl_arena_init ()
     (let ((base (syscall-direct 197 0
                                 ,nelisp-standalone--macos-arena-size
                                 3 4098 -1 0)))
       (if (< base 4096)
           (syscall-direct 1 88 0 0 0 0 0)
           (seq
            (ptr-write-u64 (data-addr nl_arena_base) 0 base)
            ,@(nelisp-standalone--arena-init-metadata-forms-dynamic
               'base nelisp-standalone--macos-arena-size)
            base)))))

(defun nelisp-standalone--macos-alloc-chunk-form ()
  "Return macOS chunk allocation forms using mmap(NULL, ...)."
  '((defun nl_os_alloc_chunk (size)
      (let ((p (syscall-direct 197 0 size 3 4098 -1 0)))
        (if (< p 4096) 0 p)))
    (defun nl_os_alloc_fail ()
      (syscall-direct 1 88 0 0 0 0 0))))

(defun nelisp-standalone--target-arena-source ()
  "Return the arena source adjusted for the current standalone target."
  (pcase nelisp-standalone--target
    ((or 'linux-x86_64 'windows-x86_64 'macos-aarch64)
     (let ((init-form (pcase nelisp-standalone--target
                        ('linux-x86_64 (nelisp-standalone--linux-arena-init-form))
                        ('windows-x86_64 (nelisp-standalone--windows-arena-init-form))
                        ('macos-aarch64 (nelisp-standalone--macos-arena-init-form))))
           (chunk-forms (pcase nelisp-standalone--target
                          ('linux-x86_64 (nelisp-standalone--linux-alloc-chunk-form))
                          ('windows-x86_64 (nelisp-standalone--windows-alloc-chunk-form))
                          ('macos-aarch64 (nelisp-standalone--macos-alloc-chunk-form)))))
       (cons 'seq
             (mapcar (lambda (form)
                       (if (and (consp form)
                                (eq (car form) 'defun)
                                (eq (cadr form) 'nl_arena_init))
                           init-form
                         (if (and (consp form)
                                  (eq (car form) 'defun)
                                  (memq (cadr form)
                                        '(nl_os_alloc_chunk nl_os_alloc_fail)))
                             (cl-find-if (lambda (chunk-form)
                                           (eq (cadr chunk-form) (cadr form)))
                                         chunk-forms)
                           form)))
                     (cdr nelisp-standalone--arena-source)))))
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
;;   Cell   (tag 11): box+0 value Sexp, box+32 rc.
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
    ;; Mark a block by OBJECT pointer.  Returns 1 if newly marked (caller
    ;; should recurse into children), 0 if foreign / already marked / free.
    (defun nl_gc_mark_block (obj)
      (if (= (nl_gc_in_arena obj) 0) 0
        (if (= (ptr-read-u64 (- obj 8) 0) 0)
            (nl_seq2 (ptr-write-u64 (- obj 8) 0 1) 1)
          0)))
    ;; Mark the char buffer of a string (raw byte block, no Sexp children).
    (defun nl_gc_mark_buf (ptr) (nl_seq2 (nl_gc_mark_block ptr) 0))
    ;; Mark every Sexp slot of a `len'-element buffer starting at data_ptr.
    (defun nl_gc_mark_vec_slots (data_ptr i len)
      (if (< i len)
          (nl_seq2 (nl_gc_mark_slot (+ data_ptr (* i 32)))
                   (nl_gc_mark_vec_slots data_ptr (+ i 1) len))
        0))
    ;; Cons cdr-spine walker (tail-recursive: recurse only on car, loop
    ;; on cdr) so list LENGTH does not bound native mark depth.  SP points
    ;; at a Sexp slot known to be tag 7 (Cons).
    (defun nl_gc_mark_cons (sp)
      (let ((box (ptr-read-u64 sp 8)))
        (if (= (nl_gc_mark_block box) 0)
            0                                      ; foreign / already marked
          (nl_seq2
           (nl_gc_mark_slot box)                   ; car @ box+0
           (if (= (ptr-read-u8 (+ box 32) 0) 7)
               (nl_gc_mark_cons (+ box 32))        ; cdr is a cons -> tail loop
             (nl_gc_mark_slot (+ box 32)))))))     ; cdr atom/other
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
                  ;; Cell: value@box+0.
                  (let ((box (ptr-read-u64 sp 8)))
                    (if (= (nl_gc_mark_block box) 0) 0
                      (nl_gc_mark_slot box)))
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
    (defun nl_gc_free_block_link (hdr head)
      (nl_seq2 (ptr-write-u64 (+ hdr 8) 0 2)
       (nl_seq2 (ptr-write-u64 (+ hdr 16) 0 (ptr-read-u64 head 0))
                (ptr-write-u64 head 0 (+ hdr 16)))))
    (defun nl_gc_free_block (hdr)
      (if (< hdr (ptr-read-u64 268435664 0)) 0   ; HARD: never free below the boot watermark
       (nl_gc_free_block_link hdr
        (if (< (ptr-read-u64 hdr 0) 24) 268435552
          (if (< 480 (ptr-read-u64 hdr 0)) 268435552
            (+ 268435696 (- (ptr-read-u64 hdr 0) 24)))))))
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
      (if (< hdr (ptr-read-u64 268435664 0))
          (nl_seq2 (ptr-write-u64 (+ hdr 8) 0 0)          ; boot block: keep live, reset mark
                   (ptr-read-u64 hdr 0))
      (let ((m (ptr-read-u64 (+ hdr 8) 0)) (bt (ptr-read-u64 hdr 0)))
        (if (= m 1)
            (nl_seq2 (ptr-write-u64 (+ hdr 8) 0 0)        ; survive: clear mark
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
    ;; setqs stay at its own scope (avoids the Phase-47 nested-let+outer-setq
    ;; pitfall that silently drops the mutation).
    (defun nl_gc_sweep_step (hdr end)
      (if (= (nl_gc_bt_ok hdr (ptr-read-u64 hdr 0) end) 0)
          0
        (nl_seq2 (nl_gc_sweep_one hdr) (+ hdr (ptr-read-u64 hdr 0)))))
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
      (nl_seq2 (nl_gc_mark_root_blocks ctx result out pool src cursor bsym)
       (nl_seq2 (nl_gc_mark_slot (+ ctx 0))      ; mirror / globals
        (nl_seq2 (nl_gc_mark_slot (+ ctx 32))    ; frame stack
         (nl_seq2 (nl_gc_mark_slot (+ ctx 64))   ; unbound marker
          (nl_seq2 (nl_gc_mark_slot result)      ; current parsed form
           (nl_seq2 (nl_gc_mark_slot out)        ; in-flight / last result
            (nl_seq2 (nl_gc_mark_slot pool)      ; slot-pool vector
             (nl_seq2 (nl_gc_mark_slot src)      ; source string
              (nl_seq2 (nl_gc_mark_slot cursor)  ; reader cursor Sexp
                       (nl_gc_mark_slot bsym))))))))))) ; builtin symbol slot
    (defun nl_gc_collect (ctx result out pool src cursor bsym)
      (if (= (ptr-read-u64 268435616 0) 1) 0    ; DEBUG: collect = pure no-op
      (seq
       (if (= (ptr-read-u64 268435592 0) 1) 0   ; DEBUG: skip-mark when slot==1
         (nl_gc_mark_roots ctx result out pool src cursor bsym))
       (nl_gc_sweep)))))
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
  '(((:u8 "+")    . (wf_write_int out (wf_sum args 0)))
    ((:u8 "-")    . (wf_write_int out (wf_diff args)))
    ((:u8 "*")    . (wf_write_int out (wf_prod args 1)))
    ((:u8 "/")    . (wf_write_int out (/ (wf_argval args 0) (wf_argval args 1))))
    ((:u8 "mod")  . (wf_write_int out (mod (wf_argval args 0) (wf_argval args 1))))
    ((:u8 "1+")   . (wf_write_int out (+ (wf_argval args 0) 1)))
    ((:u8 "1-")   . (wf_write_int out (- (wf_argval args 0) 1)))
    ((:u8 "=")    . (if (= (wf_num_eq (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "<")    . (if (= (wf_num_lt (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1) (wf_write_t out) (wf_write_nil out)))
    ((:u8 ">")    . (if (= (wf_num_gt (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "<=")   . (if (= (wf_num_le (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1) (wf_write_t out) (wf_write_nil out)))
    ((:u8 ">=")   . (if (= (wf_num_ge (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "eq")   . (if (= (wf_raw_eq (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "null") . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 0) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "not")  . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 0) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "car")  . (wf_copy32 out (nl_cons_car_ptr (wf_arg_ptr args 0))))
    ((:u8 "cdr")  . (wf_copy32 out (nl_cons_cdr_ptr (wf_arg_ptr args 0))))
    ((:u8 "cons") . (seq (nelisp_cons_construct (wf_arg_ptr args 0) (wf_arg_ptr args 1) out) 0))
    ((:u8 "list") . (seq (wf_copy32 out args) 0))
    ;; --- M4 hash tables (cons-alist v1) ---
    ((:lit "make-hash-table")  . (wf_ht_make out))
    ((:lit "puthash")          . (seq (wf_dirty) (wf_ht_put args out)))
    ((:lit "gethash")          . (wf_ht_get args out))
    ((:lit "remhash")          . (seq (wf_dirty) (wf_ht_rem args out)))
    ((:lit "hash-table-count") . (wf_write_int out (wf_ht_count (wf_ht_alist_slot (wf_arg_ptr args 0)) 0)))
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
    ((:lit "nelisp--arena-stats") . (bf_arena_stats out))
    ((:lit "nelisp--arena-force-grow-smoke") . (bf_arena_force_grow_smoke out))
    ((:lit "nelisp--size-census") . (bf_size_census out))
    ;; --- M7 file I/O (impls in m7b-fileio.o glue unit) ---
    ((:u8 "wrf")  . (seq (nl_bi_write_file args out) 0))
    ((:u8 "rdf")  . (seq (nl_bi_read_file args out) 0))
    ((:u8 "slen") . (seq (nl_bi_slen args out) 0))
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

(defun nelisp-standalone--applyfn-build-dispatch (&optional table)
  "Fold the (MATCH . IMPL) TABLE (default the full dispatch table) into a
nested-if Phase47 dispatch chain, defaulting to rc 1 (unknown builtin)."
  (let ((u #'nelisp-standalone--name-u64)
        ;; Unknown-builtin default: write the symbol name to stderr (was a
        ;; silent failure) so interpreted callers surface WHICH registered-but-
        ;; undispatched builtin is missing, then fall through to rc 1.  A symbol
        ;; Sexp (tag 4) keeps its name bytes at ptr@16 / len@24, same as a Str.
        (dispatch '(let* ((unkb (alloc-bytes 1 1)))
                     (seq (ptr-write-u8 unkb 0 10)
                          (nl_os_write_stderr (ptr-read-u64 name_ptr 16)
                                              (ptr-read-u64 name_ptr 24))
                          (nl_os_write_stderr unkb 1)
                          1))))
    (dolist (entry (reverse (or table nelisp-standalone--applyfn-dispatch-table)))
      (let* ((match (car entry))
             (impl (cdr entry))
             (cond-form (pcase (car match)
                          (:u8  `(wf_name_is name_ptr ,(funcall u (cadr match)) ,(length (cadr match))))
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
    ;; bf_size_census (Doc 08 §8.14 diagnostic): BLOCK_TOTAL histogram over the
    ;; whole arena.  Boxes carry NO self type tag (type lives in the Sexp that
    ;; points to the box, top-down), so a bottom-up walk can only bucket by size.
    ;; Reliable read-only walk modeled on `nl_gc_sweep_chunk'.  Accumulates into a
    ;; heap-allocated 64-byte block ACC (never fixed reserved addresses — avoids
    ;; the §8.7 wild-write risk).  ACC layout (u64 each):
    ;;   +0 total-nonfree  +8 free-bytes  +16 cons(BT=88)  +24 small(BT<=64)
    ;;   +32 med(65..256)  +40 big(>256)  +48 block-count  +56 free-count
    ;; ACC slots (refined to localize the "big" bucket by size):
    ;;   +0 total-nonfree  +8 free  +16 cons(BT=88)  +24 le256  +32 b257-4k
    ;;   +40 b4k-256k  +48 b256k-2m (~1MB parse pool lands here)  +56 b>2m
    (defun bf_size_census_block (hdr acc)
      (let ((bt (ptr-read-u64 hdr 0))
            (mark (ptr-read-u64 (+ hdr 8) 0)))
        (if (= mark 2)
            (ptr-write-u64 (+ acc 8) 0 (+ (ptr-read-u64 (+ acc 8) 0) bt))
          (seq
           (ptr-write-u64 acc 0 (+ (ptr-read-u64 acc 0) bt))
           (if (= bt 88)
               (ptr-write-u64 (+ acc 16) 0 (+ (ptr-read-u64 (+ acc 16) 0) bt))
             (if (< bt 257)
                 (ptr-write-u64 (+ acc 24) 0 (+ (ptr-read-u64 (+ acc 24) 0) bt))
               (if (< bt 4097)
                   (ptr-write-u64 (+ acc 32) 0 (+ (ptr-read-u64 (+ acc 32) 0) bt))
                 (if (< bt 262145)
                     (ptr-write-u64 (+ acc 40) 0 (+ (ptr-read-u64 (+ acc 40) 0) bt))
                   (if (< bt 2097153)
                       (ptr-write-u64 (+ acc 48) 0 (+ (ptr-read-u64 (+ acc 48) 0) bt))
                     (ptr-write-u64 (+ acc 56) 0 (+ (ptr-read-u64 (+ acc 56) 0) bt)))))))))))
    (defun bf_size_census_step (hdr end acc)
      (if (= (nl_gc_bt_ok hdr (ptr-read-u64 hdr 0) end) 0)
          0
        (nl_seq2 (bf_size_census_block hdr acc) (+ hdr (ptr-read-u64 hdr 0)))))
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
         (wf_cons_int (ptr-read-u64 (+ acc 56) 0) nil-slot s7)
         (wf_cons_int (ptr-read-u64 (+ acc 48) 0) s7 s6)
         (wf_cons_int (ptr-read-u64 (+ acc 40) 0) s6 s5)
         (wf_cons_int (ptr-read-u64 (+ acc 32) 0) s5 s4)
         (wf_cons_int (ptr-read-u64 (+ acc 24) 0) s4 s3)
         (wf_cons_int (ptr-read-u64 (+ acc 16) 0) s3 s2)
         (wf_cons_int (ptr-read-u64 (+ acc 8) 0) s2 s1)
         (wf_cons_int (ptr-read-u64 acc 0) s1 out)
         0)))
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
            (if (= (ptr-read-u64 rest 0) 7) (wf_subtail rest first) (- 0 first))) 0)))
  "Core wf_* dispatch helpers (shared by baked + reader applyfn).")

;; M4 hash-table helpers (cons-alist v1).  Reader-only: wf_key_eq uses symbol-eq
;; / str-eq grammar ops that lower to extern calls present only in the reader.
(defconst nelisp-standalone--applyfn-ht-helpers
  '((defun wf_ht_copy32 (dst src)
      (seq (ptr-write-u64 dst 0 (ptr-read-u64 src 0)) (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
           (ptr-write-u64 dst 16 (ptr-read-u64 src 16)) (ptr-write-u64 dst 24 (ptr-read-u64 src 24)) 0))
    (defun wf_key_eq (ka kb)
      (let* ((ta (ptr-read-u64 ka 0)) (tb (ptr-read-u64 kb 0)))
        (if (= ta tb)
            (if (= ta 2)
                (if (= (ptr-read-u64 ka 8) (ptr-read-u64 kb 8)) 1 0)
              (if (= ta 4)
                  (symbol-eq ka kb)
                (if (= ta 5)
                    (str-eq ka kb)
                  (if (= ta 0) 1 (if (= ta 1) 1 0)))))
          0)))
    (defun wf_ht_alist_slot (table_ptr) (nl_cons_cdr_ptr table_ptr))
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
    (defun wf_ht_make (out)
      (let* ((marker (alloc-bytes 32 8)) (nil_s (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 marker 0 2) (ptr-write-u64 marker 8 0)
         (ptr-write-u64 marker 16 0) (ptr-write-u64 marker 24 0)
         (ptr-write-u64 nil_s 0 0) (ptr-write-u64 nil_s 8 0)
         (ptr-write-u64 nil_s 16 0) (ptr-write-u64 nil_s 24 0)
         (nelisp_cons_construct marker nil_s out)
         0)))
    (defun wf_ht_put (args out)
      (let* ((key_ptr (wf_arg_ptr args 0)) (val_ptr (wf_arg_ptr args 1))
             (table_ptr (wf_arg_ptr args 2))
             (alist_slot (wf_ht_alist_slot table_ptr))
             (entry_ptr (wf_ht_find alist_slot key_ptr)))
        (if (= entry_ptr 0)
            (let* ((pair_s (alloc-bytes 32 8)) (newhead_s (alloc-bytes 32 8)))
              (seq
               (nelisp_cons_construct key_ptr val_ptr pair_s)
               (nelisp_cons_construct pair_s alist_slot newhead_s)
               (wf_ht_copy32 alist_slot newhead_s)
               (wf_ht_copy32 out val_ptr)
               0))
          (let* ((val_slot (nl_cons_cdr_ptr entry_ptr)))
            (seq
             (wf_ht_copy32 val_slot val_ptr)
             (wf_ht_copy32 out val_ptr)
             0)))))
    (defun wf_ht_get (args out)
      (let* ((key_ptr (wf_arg_ptr args 0)) (table_ptr (wf_arg_ptr args 1))
             (alist_slot (wf_ht_alist_slot table_ptr))
             (entry_ptr (wf_ht_find alist_slot key_ptr)))
        (if (= entry_ptr 0)
            (let* ((rest1 (nl_cons_cdr_ptr args))
                   (rest2 (nl_cons_cdr_ptr rest1)))
              (if (= (ptr-read-u64 rest2 0) 7)
                  (wf_ht_copy32 out (nl_cons_car_ptr rest2))
                (wf_write_nil out)))
          (wf_ht_copy32 out (nl_cons_cdr_ptr entry_ptr)))))
    (defun wf_ht_rem (args out)
      (let* ((key_ptr (wf_arg_ptr args 0)) (table_ptr (wf_arg_ptr args 1))
             (alist_slot (wf_ht_alist_slot table_ptr))
             (rebuilt (alloc-bytes 32 8)))
        (seq
         (wf_ht_rem_walk alist_slot key_ptr rebuilt)
         (wf_ht_copy32 alist_slot rebuilt)
         (wf_write_nil out)
         0)))
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
    (defun m5_float_eq_i64_p (vptr n)
      (if (= (f64-le (bits-to-f64 (sexp-float-unwrap vptr))
                     (i64-to-f64 n))
             1)
          (f64-ge (bits-to-f64 (sexp-float-unwrap vptr))
                  (i64-to-f64 n))
        0))
    (defun m5_push_float (ms vptr)
      (let* ((whole (f64-to-i64-trunc
                     (bits-to-f64 (sexp-float-unwrap vptr)))))
        (seq
         (m5_push_dec ms whole)
         (mut-str-push-byte ms 46)
         (if (= (m5_float_eq_i64_p vptr whole) 1)
             (mut-str-push-byte ms 48)
           (mut-str-push-byte ms 53)))))
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
          (seq (m5_emit_value ms (nl_cons_car_ptr cur))
               (m5_concat_walk ms (nl_cons_cdr_ptr cur)))
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
                                (seq (mut-str-push-byte ms 37)
                                     (mut-str-push-byte ms d)
                                     (m5_fmt_loop ms fmt (+ i 2) n argp)))))))))))
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
            (seq (wf_copy32 out (vector-ref-ptr arr idx)) 0)
          (wf_write_int out (str-byte-at arr idx)))))
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
          (seq (wf_copy32 out val) 0))))
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
      (let* ((sym (wf_arg_ptr args 0)))
        (seq
         (nl_alloc_symbol (ptr-read-u64 sym 16) (ptr-read-u64 sym 24) 268435480)
         (bf_sig_copy32 268435512 (nl_cons_cdr_ptr args))
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
    ;; load FILE &optional ... -> t.  Minimal standalone-reader command surface:
    ;; read FILE into a Sexp::Str, parse each top-level form with the same pure
    ;; reader, and evaluate it in the caller's ENV.  This intentionally mirrors
    ;; the driver's multi-form loop instead of relying on host-side embedding.
    (defun bf_load_eval_loop (src cursor result pool env out bsym more)
      (while (= more 1)
        (seq
         (ptr-write-u64 result 0 0) (ptr-write-u64 result 8 0)
         (let* ((prc (nelisp_reader_parse_one src cursor result pool 0)))
           (if (= prc 1)
               (seq
                (ptr-write-u64 out 0 0) (ptr-write-u64 out 8 0)
                (let* ((rc (nelisp_eval_call result env out)))
                  ;; GC trigger must compare TOTAL allocated bytes across all
                  ;; chunks (268436184 = chunk-bytes-reserved running counter),
                  ;; not the chunk-0 bump offset (268435456) — after Doc 140's
                  ;; chunk-growth refactor the chunk-0 bump caps at the 256 MiB
                  ;; first chunk and never reaches the 512 MiB trigger, so the
                  ;; tracing GC never fired and the arena grew unbounded.
                  (if (< (ptr-read-u64 268436184 0)
                         (ptr-read-u64 268435560 0))
                      0
                    (let* ((live (nl_gc_collect env result out pool src cursor bsym))
                           (bump (ptr-read-u64 268436184 0))
                           (lo (+ (* live 3) 1048576))
                           (hi (+ bump 536870912)))
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
                (let* ((rc (nelisp_eval_call result env out)))
                  (if (= rc 0)
                      ;; GC trigger on TOTAL chunk-bytes-reserved (268436184),
                      ;; not the chunk-0 bump offset (268435456).  See the note
                      ;; in `bf_load_eval_loop'.
                      (if (< (ptr-read-u64 268436184 0)
                             (ptr-read-u64 268435560 0))
                          0
                        (let* ((live (nl_gc_collect env result out pool src cursor bsym))
                               (bump (ptr-read-u64 268436184 0))
                               (lo (+ (* live 3) 1048576))
                               (hi (+ bump 536870912)))
                          (ptr-write-u64 268435560 0
                                         (if (< lo hi) hi lo))))
                    (setq more 2))))
             (setq more 0)))))
      more)
    (defun bf_load (args env out)
      (let* ((src (alloc-bytes 32 8))
             (cursor (alloc-bytes 32 8))
             (result (alloc-bytes 32 8))
             (pool (alloc-bytes 32 8))
             (bsym (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 bsym 0 0) (ptr-write-u64 bsym 8 0)
         (ptr-write-u64 268435624 0 0)
         (nl_bi_read_file args src)
         (ptr-write-u64 cursor 0 2) (ptr-write-u64 cursor 8 0)
         (vector-make 32768 pool)
         (if (= (bf_load_eval_loop src cursor result pool env out bsym 1) 2)
             1
           (seq
            (ptr-write-u64 268435472 0 0)
            (wf_dirty)
            (wf_write_t out)
            0)))))
    (defun bf_eval_source_string (args env out)
      (let* ((src (wf_arg_ptr args 0))
             (cursor (alloc-bytes 32 8))
             (result (alloc-bytes 32 8))
             (pool (alloc-bytes 32 8))
             (bsym (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 bsym 0 0) (ptr-write-u64 bsym 8 0)
         (ptr-write-u64 268435624 0 0)
         (ptr-write-u64 cursor 0 2) (ptr-write-u64 cursor 8 0)
         ;; Doc 08 §8.15: right-size the per-form parse pool to the source length
         ;; instead of a fixed 32768-slot (~1MB) vector.  The parsed form's nodes
         ;; live IN this pool (a GC root), so a retained defun/defvar pins the
         ;; whole pool -> a 1MB pool per small form was 54-68% of the vendor-load
         ;; arena.  8x source length bounds the node count (each parse node needs
         ;; >=1 source char; 8x covers per-node slot overhead) and the 32768 cap
         ;; keeps the previous behaviour for big forms (no parse regression).
         (vector-make (let ((n (* 4 (str-len src))))
                        (if (< n 256) 256 (if (> n 32768) 32768 n)))
                      pool)
         (if (= (bf_eval_source_string_loop src cursor result pool env out bsym 1) 2)
             1
           (seq (wf_dirty) 0)))))
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
                    (if (= (ptr-read-u64 a 8) (ptr-read-u64 b 8)) 1 0)))))
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
    ;; thread-join: spin until *ADDR >= TARGET.  Uses the COMPILED (phase47,
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
    ((:lit "make-symbol") . (seq (bf_intern (wf_arg_ptr args 0) out) 0))
    ((:lit "unibyte-string") . (bf_unibyte_string args out))
    ;; --- vector ops ---
    ((:lit "make-vector") . (bf_make_vector args out))
    ((:lit "vector")      . (bf_vector args out))
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
    ((:lit "nelisp--syscall-read-file") . (nl_bi_read_file args out))
    ((:lit "nl-write-file") . (nl_bi_write_file_t args out))
    ((:lit "nelisp--write-stdout-bytes") . (nl_bi_write_stdout_bytes args out))
    ((:lit "nelisp--write-stderr-line") . (nl_bi_write_stderr_line args out))
    ((:lit "read-stdin-bytes") . (nl_bi_read_stdin_bytes args out))
    ;; nl-current-unix-time: epoch seconds via the linux-x86_64 time(2)
    ;; syscall (__NR_time=201, NULL arg).  Lets interpreted code (e.g.
    ;; nelisp-emacs `float-time'/`current-time' polyfills) read wall-clock
    ;; time without process-exiting on an undispatched builtin.
    ((:lit "nl-current-unix-time") . (wf_write_int out (syscall-direct 201 0 0 0 0 0 0)))
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
    ;; same Phase-47 grammar ops the compiler emits, so no new Rust.
    ((:lit "syscall-direct") . (wf_write_int out (syscall-direct (wf_argval args 0) (wf_argval args 1) (wf_argval args 2) (wf_argval args 3) (wf_argval args 4) (wf_argval args 5) (wf_argval args 6))))
    ((:lit "atomic-fetch-add") . (wf_write_int out (atomic-fetch-add (wf_argval args 0) (wf_argval args 1))))
    ((:lit "ptr-read-u64") . (wf_write_int out (ptr-read-u64 (wf_argval args 0) (wf_argval args 1))))
    ((:lit "ptr-write-u64") . (seq (ptr-write-u64 (wf_argval args 0) (wf_argval args 1) (wf_argval args 2)) (wf_write_int out 0)))
    ((:lit "alloc-bytes") . (wf_write_int out (alloc-bytes (wf_argval args 0) (wf_argval args 1))))
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
    "vectorp" "listp" "zerop" "set" "fboundp" "boundp" "featurep" "provide" "require"
    "symbol-name" "intern" "make-symbol" "unibyte-string"
    "make-vector" "vector" "aref" "elt" "aset"
    "signal" "error" "equal" "setcar" "setcdr" "load"
    ;; Wave-2 (C): bitwise / shift / string<
    "ash" "logand" "logior" "logxor" "lognot" "string<"
    "syscall-direct" "atomic-fetch-add" "ptr-read-u64" "ptr-write-u64" "alloc-bytes" "ptr-call" "thread-spawn" "thread-join" "fork-spawn")
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

(defun nelisp-standalone--applyfn-assemble (helper-groups table)
  "Assemble an applyfn `(seq ...)' unit from HELPER-GROUPS (lists of defun forms,
appended in order) and the dispatch TABLE, ending in nelisp_apply_function."
  (let ((dispatch (nelisp-standalone--applyfn-build-dispatch table)))
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
   nelisp-standalone--applyfn-dispatch-table-baked)
  "Arithmetic/list-only applyfn for the baked-form eval build.")

;; Reader applyfn: core + HT (M4) + string/format (M5) + B-foundation breadth
;; helpers + the reader dispatch table (= the FULL table with nil-safe car/cdr +
;; tag-aware eq + vector-aware length REPLACEMENTS and the breadth arms appended;
;; incl. file-I/O wrf/rdf/slen impls in `nelisp-standalone--fileio-source').
(defconst nelisp-standalone--applyfn-source
  (nelisp-standalone--applyfn-assemble
   (list nelisp-standalone--applyfn-core-helpers
         nelisp-standalone--applyfn-ht-helpers
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
    (defun nl_bi_rf_withfd (fd buf out)
      (if (< fd 0)
          (wf_copy32_strnil out)
        (let* ((n (nl_os_read_file_handle fd buf 4194304)))
          (nl_seq2 (nl_os_close_handle fd)
                   (nl_seq2 (nl_alloc_str buf (if (< n 0) 0 n) out) 0)))))
    (defun nl_bi_read_file (args out)
      (let* ((path_sx (wf_arg_ptr args 0)))
        (let* ((cpath (nl_bi_make_cpath path_sx))
               (buf (alloc-bytes 4194304 1))
               (fd (nl_os_open_read cpath)))
          (nl_bi_rf_withfd fd buf out))))
    (defun nl_bi_slen (args out)
      (wf_write_int out (nl_bi_strlen (wf_arg_ptr args 0))))
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
    (defun nl_bi_write_stderr_line (args out)
      (let* ((sx (wf_arg_ptr args 0))
             (nl (alloc-bytes 1 1)))
        (seq
         (nl_os_write_stderr (nl_bi_strptr sx) (nl_bi_strlen sx))
         (ptr-write-u8 nl 0 10)
         (nl_os_write_stderr nl 1)
         (wf_write_nil out)
         0)))
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
            (nl_cons_sym_eq name_ptr buf 5)))))

(defconst nelisp-standalone--sp-eq-throw
  '(defun nl_sp_eq_throw (name_ptr)
     (let* ((buf (alloc-bytes 8 1)))
       (seq (ptr-write-u64 buf 0 512970877044)
            (nl_cons_sym_eq name_ptr buf 5)))))

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
  "Apply BOTH combiner-cons build-time patches in order: the catch/throw dispatch
patch (`--patch-combiner-cons') first, then the macro-expansion in-place caching
patch (`--patch-macro-cache').  Both patch the same combiner-cons source."
  (nelisp-standalone--patch-macro-cache
   (nelisp-standalone--patch-combiner-cons src)))

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
    (defun nl_eval_arg_list_walk (cur_ptr env_ptr acc_slot)
      (if (= (ptr-read-u64 cur_ptr 0) 7)
          (let* ((car_ptr (nl_cons_car_ptr cur_ptr)) (cdr_ptr (nl_cons_cdr_ptr cur_ptr))
                 (eval_slot (alloc-bytes 32 8)) (rest_slot (alloc-bytes 32 8)))
            (let* ((rc_eval (nelisp_eval_call car_ptr env_ptr eval_slot)))
              (if (= rc_eval 0)
                  (let* ((rc_rest (nl_eval_arg_list_walk cdr_ptr env_ptr rest_slot)))
                    (if (= rc_rest 0)
                        (seq (nelisp_cons_construct eval_slot rest_slot acc_slot) 0)
                      1))
                1)))
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

(defun nelisp-standalone--target-start-unit (&optional reader-p)
  "Return the target-specific standalone start unit."
  (pcase nelisp-standalone--target
    ('windows-x86_64 (nelisp-standalone--windows-start-unit))
    ('macos-aarch64 (nelisp-standalone--macos-aarch64-start-unit reader-p))
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
    ("consbox.o"          nelisp-cc-nlconsbox-alloc               nelisp-cc-nlconsbox-alloc--source)
    ("consbox-clone.o"    nelisp-cc-nlconsbox-clone               nelisp-cc-nlconsbox-clone--source)
    ("consbox-setcar.o"   nelisp-cc-nlconsbox-set-car             nelisp-cc-nlconsbox-set-car--source)
    ("consbox-setcdr.o"   nelisp-cc-nlconsbox-set-cdr             nelisp-cc-nlconsbox-set-cdr--source)
    ("vec-alloc.o"        nelisp-cc-nlvector-alloc                nelisp-cc-nlvector-alloc--source)
    ("vec-set.o"          nelisp-cc-nlvector-set-slot             nelisp-cc-nlvector-set-slot--source)
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
                               '("ExitProcess" "VirtualAlloc")
                               (list :stack-reserve
                                     nelisp-standalone--windows-stack-reserve)))
      ('macos-aarch64
       (nelisp-link-units-macho-exec out units "_main" 'aarch64)
       (set-file-modes out #o755)
       (nelisp-standalone--codesign-macos-adhoc out))
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
  '("+" "-" "*" "/" "mod" "1+" "1-" "=" "<" ">" "<=" ">=" "car" "cdr" "cons" "list" "eq" "null" "not"
    ;; Globals shim bridge for user-loaded .el files (`defvar' / `defconst'
    ;; in the standalone prelude lower through this entry).
    "nelisp--env-globals-op"
    ;; M4 hash tables
    "make-hash-table" "puthash" "gethash" "remhash" "hash-table-count" "maphash"
    ;; M5 strings + format
    "length" "concat" "substring" "make-string" "string="
    "char-to-string" "string-to-char" "number-to-string" "string-to-number" "format"
    "nelisp--repr" "nelisp--arena-stats" "nelisp--arena-force-grow-smoke" "nelisp--size-census"
    ;; M7 file I/O
    "wrf" "rdf" "slen" "load"
    "nelisp--eval-source-string" "nelisp--syscall-read-file" "nl-write-file"
    "nelisp--write-stdout-bytes" "nelisp--write-stderr-line"
    "nl-current-unix-time"
    "exit"
    ;; Wave-1 (B) breadth: predicates / symbol+vector ops / equal / setcar-setcdr
    ;; / signal-error (the names back the breadth arms in the reader applyfn).
    "consp" "atom" "stringp" "symbolp" "integerp" "natnump" "numberp" "floatp"
    "vectorp" "listp" "zerop" "set" "fboundp" "boundp" "featurep" "provide" "require"
    "symbol-name" "intern" "make-symbol" "unibyte-string"
    "make-vector" "vector" "aref" "elt" "aset"
    "signal" "error" "equal" "setcar" "setcdr"
    ;; Wave-2 (C): bitwise / shift / string<
    "ash" "logand" "logior" "logxor" "lognot" "string<"
    "syscall-direct" "atomic-fetch-add" "ptr-read-u64" "ptr-write-u64" "alloc-bytes" "ptr-call" "thread-spawn" "thread-join" "fork-spawn")
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
              (list "ExitProcess" "VirtualAlloc" "GetCommandLineW"
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
    (insert "\n(defun string-match (re s &optional start) (nlre-string-match re s start))\n"
            "(defun string-match-p (re s &optional start) (nlre-string-match re s start))\n"
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

The suffix reads the quit/throw flag at arena-base+8 to decide whether to
print the form's value.  Doc 140 Stage 8: on linux the arena base is a runtime
mmap(NULL) address, and runtime-PARSED REPL code cannot use the compile-time
`data-addr' primitive (it never reaches the chunk-arena rewrite), so the flag
address is computed from `(car (nelisp--arena-stats))' — whose car is the live
runtime base — instead of a baked fixed immediate.  windows/macos keep their
fixed-base immediate until `data-addr' lands for those toolchains."
  (if (eq nelisp-standalone--target 'linux-x86_64)
      "))) (if (= (ptr-read-u64 (+ (car (nelisp--arena-stats)) 8) 0) 0) (progn (nelisp--write-stdout-bytes (nelisp--repr v)) (nelisp--write-stdout-bytes (unibyte-string 10)) v) 0))\n"
    (format "))) (if (= (ptr-read-u64 %d 0) 0) (progn (nelisp--write-stdout-bytes (nelisp--repr v)) (nelisp--write-stdout-bytes (unibyte-string 10)) v) 0))\n"
            (nelisp-standalone--target-arena-metadata-address 8))))

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
                      (nelisp_eval_call result ctx out)
                      (nl_boundary_maybe_reclaim mark_chunk mark_cursor epoch0 out)
                      ;; GC trigger on TOTAL chunk-bytes-reserved (268436184),
                      ;; not the chunk-0 bump offset.  See `bf_load_eval_loop'.
                      (if (< (ptr-read-u64 268436184 0) (ptr-read-u64 268435560 0))
                          0
                        (let* ((live (nl_gc_collect ctx result out pool src cursor builtin_sym))
                               (bump (ptr-read-u64 268436184 0))
                               (lo (+ (* live 3) 1048576))
                               (hi (+ bump 536870912)))
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
   (with-temp-buffer
     (insert-file-contents
      (expand-file-name "scripts/nelisp-stdlib-prelude.el"
                        nelisp-standalone--repo-root))
     (buffer-string))
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
   " ((or (string= (car nelisp-standalone-argv) \"eval-runtime-image\")\n"
   "      (string= (car nelisp-standalone-argv) \"exec-runtime-image\"))\n"
   "  (setq nelisp-runtime-image--standalone-next-form\n"
   "        (car (cdr (cdr nelisp-standalone-argv))))\n"
   "  (nelisp-runtime-image--eval-source\n"
   "   (rdf (car (cdr nelisp-standalone-argv))))\n"
   "  (nelisp-runtime-image--eval-source\n"
   "   nelisp-runtime-image--standalone-next-form)\n"
   "  0)\n"
   " (t 2))\n"))

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

(defun nelisp-standalone--reader-os-source-forms ()
  "Return target-specific OS helper defuns used by the reader driver/file I/O."
  (pcase nelisp-standalone--target
    ('windows-x86_64
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
           (if (= ok 0) -1 (ptr-read-u32 sent 0))))))
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
         (syscall-direct 4 2 ptr len 0 0 0))))
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
         (syscall-direct 1 2 ptr len 0 0 0))))))

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
      "Usage: nelisp [--help] [--repl [--no-prompt] [--no-print]] [--eval EXPR] [--load FILE] [--neln-selftest] [FILE]\nArguments:\n  --help                         Show this argument list\n  --eval EXPR                    Evaluate EXPR and print the value\n  --load FILE                    Load FILE and print the last value\n  --neln-selftest                Run the embedded native exec self-test\n  --repl [--no-prompt] [--no-print]\n                                 Start the REPL\n  FILE                           Load FILE as a source file\nCommands:\n  dump-runtime-image FILE FORM...\n  extend-runtime-image IMAGE OUT FORM...\n  eval-runtime-image IMAGE FORM...\n  exec-runtime-image IMAGE FORM...\n")
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
    (defun nl_runtime_image_eval_exec_command_p (ptr)
      (if (= (nl_cstr_eq_eval_runtime_image ptr) 1)
          1
        (nl_cstr_eq_exec_runtime_image ptr)))
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
                  (nl_runtime_image_command_p ptr))))))))
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
          (seq
           (setq off (nl_cstr_copy_into form_ptr fbuf off))
           (ptr-write-u8 fbuf off 10)
           (nl_runtime_image_copy_argv_forms sp argc (+ i 1) fbuf (+ off 1))))))
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
            (globals (alloc-bytes 32 8)) (frames (alloc-bytes 32 8)) (unbound (alloc-bytes 32 8))
            (ctx (alloc-bytes 120 8))
            (builtin_buf (alloc-bytes 8 1)) (builtin_sym (alloc-bytes 32 8))
            (src (alloc-bytes 32 8)) (cursor (alloc-bytes 32 8))
            (result (alloc-bytes 32 8)) (pool (alloc-bytes 32 8)) (out (alloc-bytes 32 8))
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
        (ptr-write-u64 builtin_buf 0 31078196194145634)
        (nl_alloc_symbol builtin_buf 7 builtin_sym)
        ,@(nelisp-standalone--reader-install-builtins-forms)
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
        (vector-make 32768 pool)                                ; Sexp::Vector(32768) slot-pool — raised from 8192 after vendored eucjp-ms' 2069-entry generated alist exceeded the flat-list tail depth; 32768 => MAX_DEPTH ~8191 for the current 3+4*MAX_DEPTH reader slot shape.
        ;; GC trigger: collect at a form boundary once the bump offset
        ;; crosses this threshold.  Initial 512 MiB keeps small *and*
        ;; moderate programs GC-free (zero overhead) — crucially the full
        ;; 14k-line nelisp-phase47-compiler.el load (420 top-level forms,
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
        (ptr-write-u64 268435560 0 536870912)
        ;; BOOT WATERMARK: freeze the absolute address up to which everything
        ;; was allocated during install + driver setup (the mirror, all 60
        ;; builtins, the env/frame records, the fixed driver scratch slots).
        ;; The GC never frees blocks below this line — the boot image is live
        ;; for the whole program, so this is a sound "permanent generation"
        ;; (leaks only a few KB of install-time scratch) that sidesteps the
        ;; need to enumerate every boot-internal raw-pointer edge precisely.
        ;; Per-form eval garbage (allocated ABOVE the line) is fully collected.
        (ptr-write-u64 268435664 0 (+ 268435456 (ptr-read-u64 268435456 0)))
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
             ,@(nelisp-standalone--reader-repl-prelude-forms
                'fbuf 'src 'cursor 'result 'pool 'out 'ctx 'builtin_sym)
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
               ,@(nelisp-standalone--reader-repl-prelude-forms
                  'fbuf 'src 'cursor 'result 'pool 'out 'ctx 'builtin_sym)
               (nl_cli_eval_source arg2 fbuf src)
               (nl_eval_source_all src cursor result pool out ctx builtin_sym)
               (if (= (ptr-read-u64 268435464 0) 0)
                   0
                 (- (ptr-read-u64 268435464 0) 1)))
            (seq (nl_cli_write_help fbuf) 2)))
         ((= (nl_cstr_eq_load path) 1)
          (if (= (nl_cli_one_arg_p arg2 arg3) 1)
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
                     (- (ptr-read-u64 268435464 0) 1)))))
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
             ,@(nelisp-standalone--reader-repl-prelude-forms
                'fbuf 'src 'cursor 'result 'pool 'out 'ctx 'builtin_sym)
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
         (t
          (seq
           ;; --- source selection: embedded vs. file (M7 dual mode) ---
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
                 ;; file path: open(path,O_RDONLY) -> read -> close -> wrap as Str
                 (let* ((n (nl_os_read_file_cpath
                            path fbuf
                            ,nelisp-standalone--reader-read-cap)))
                   (if (< n 0)
                       (sexp-write-str-lit src "1")
                     (nl_alloc_str fbuf n src)))))))
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
           (let* ((invec (alloc-bytes 32 8)) (alist (alloc-bytes 32 8)))
             (seq
              (vector-make 3 invec)
              (nl_sexp_clone_into frames_ptr (vector-ref-ptr invec 0))
              (sexp-int-make (vector-ref-ptr invec 1) depth)
              (nl_clx_write_nil (vector-ref-ptr invec 2))
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

;; rc-correct nl_apply_do_fset (Doc 137 M3).  The shipped handler in
;; nelisp-cc-evalport-combiner-apply has two rc-plumbing bugs (the non-symbol
;; clone arm returns the dst ptr -- treated as error; the mirror setter returns 1
;; on success -- treated as failure) AND triggers a Phase47 compiler mis-lowering:
;; `(not (= raw-bool 0))' inside a dispatcher-boundary-available defun is routed
;; through nelisp_aot_builtin_call1, which reads 32 bytes from the raw i64 boolean
;; as a *const Sexp -> NULL deref -> SIGSEGV (the real root cause; see the
;; compiler gate at nelisp-phase47-compiler ~L7846/7862, a durable fix TODO).
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
         ;; M6 catch/throw dispatch + Wave-1 (A) macro-expansion in-place caching,
         ;; both patched onto combiner-cons (catch/throw first, then macro cache).
         (combiner-cons (progn
                          (require 'nelisp-cc-evalport-combiner-cons)
                          (nelisp-standalone--cached-unit
                           "combiner-cons-ct-mc.o"
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
            extras (list float-stub) real-sf
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
      ('macos-aarch64
       (nelisp-link-units-macho-exec out units "_main" 'aarch64)
       (set-file-modes out #o755)
       (nelisp-standalone--codesign-macos-adhoc out))
      (_
       (nelisp-link-units out units)
       (set-file-modes out #o755)))
    (message "[standalone-reader] linked %d units -> %s (src=%S)"
             (length units) out
             (nelisp-standalone--reader-src))
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
          (message "[standalone-reader] cli smoke PASS"))
      (ignore-errors (delete-file tmp)))))

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
        (dump-rc nil)
        (eval-rc nil)
        (eval-out nil)
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
        (delete-file tmp)))))

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
