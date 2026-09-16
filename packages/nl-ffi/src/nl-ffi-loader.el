;;; nl-ffi-loader.el --- pure-elisp ELF loader for the static reader -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; FFI step 3, increment 2.  Increment 1 (commit e3d9fd5fc) mapped one
;; self-contained shared object, applied RELATIVE/GLOB_DAT/JUMP_SLOT
;; relocations whose symbols resolve inside that same object, and refused
;; everything else by name -- see that commit's own Commentary, kept below
;; where it still applies, for the primitives this file is built on and
;; the u64-ceiling finding that shapes every byte copy here.  This
;; increment lifts three of those refusals: `DT_NEEDED' dependency
;; loading, `DT_INIT'/`DT_INIT_ARRAY' initializers, and
;; `R_X86_64_IRELATIVE' (IFUNC).  TLS (`:tls-segment'/`:tls-relocation'),
;; `DT_RELR', `PT_GNU_RELRO' re-protection, unloading, and symbol
;; interposition beyond the one search order this file documents stay
;; refused -- see "Out of scope" below.
;;
;; ---------------------------------------------------------------------
;; THE LIBC / SECOND-COPY QUESTION -- read this before touching dependency
;; loading.
;;
;; The design brief for this increment opens with a real safety question:
;; the reader process already has its own libc, loaded by the real
;; dynamic linker, so mapping a second copy of it and calling into it is
;; not obviously safe -- a second copy has its own uninitialized malloc
;; arenas, stdio, locale, TLS, errno, and calling into it can corrupt
;; state in ways a smoke test would not catch.
;;
;; That premise does NOT hold for the process this loader actually runs
;; in.  Measured directly on this repository's own `make standalone-
;; reader' output (2026-09-16, this host): `target/nelisp' is `file'-
;; reported "statically linked", `readelf -d' reports "There is no
;; dynamic section in this file", `readelf -l' shows exactly two PT_LOAD
;; program headers and nothing else (no PT_INTERP, no PT_DYNAMIC, no
;; PT_TLS of its own), and `ldd' reports "not a dynamic executable".
;; `scripts/nelisp-standalone-build.el''s own Commentary confirms why:
;; this binary is produced entirely by NeLisp's OWN pure-elisp AOT
;; compiler and static linker ("linked ... into a freestanding static
;; ELF" -- that file's words, not a paraphrase), not by `cc'/`ld' against
;; glibc.  Every OS interaction this reader makes goes through
;; `syscall-direct', a raw `syscall' instruction -- never a libc wrapper.
;; This is *why* `syscall-direct' exists as its own primitive rather than
;; this loader simply calling `open'/`mmap'/`mprotect' C functions the
;; reader already had linked in: there is no such libc to call.  There is
;; no first copy of libc in this process, so there is no SECOND copy
;; either when this loader maps a shared object -- the premise the brief
;; asks to be confirmed or refuted does not hold here, and this
;; Commentary states that plainly rather than silently building on it.
;;
;; This loader is, in addition, wired to run in exactly this process and
;; no other: `ffi:library' (nl-ffi.el) falls back to `nl-ffi-loader-open'
;; ONLY when `nl-ffi-call'/`dlopen' signal `nl-ffi-unavailable' -- which
;; happens ONLY on the default static reader (the dynamic reader has a
;; real, working `dlopen'/`dlsym' and never reaches this file at all; see
;; `ffi:library''s docstring).  So the "no first copy" finding above is
;; not a fact about some OTHER possible reader configuration this loader
;; might also run under -- it is a fact about the one process this loader
;; is ever actually invoked in.
;;
;; None of that makes initializing an ARBITRARY runtime from cold safe in
;; general, and this file does not claim it does.  The underlying hazard
;; generalizes even without a pre-existing copy to corrupt: real glibc
;; startup assumes scaffolding this process never built for it -- a
;; thread-control block already installed for the main thread by `_dl_
;; start'/`_dl_init', `auxv' reachable the way `__libc_start_main' reads
;; it off the ORIGINAL process stack (long gone by the time Lisp code
;; here runs), a malloc arena bootstrapped by the real startup sequence.
;; Calling into code that silently assumes any of that is the exact
;; failure mode the brief names: something that "appears to work in a
;; smoke test and corrupts state later".
;;
;; This is why dependency loading, IFUNC, and initializers all still stay
;; bounded by the SAME check increment 1 already had for an entirely
;; different reason: a `PT_TLS' program header refuses the object
;; outright (`:tls-segment'), checked before a single relocation is even
;; inspected -- see `nl-ffi-loader--map-node'.  That check turns out to
;; be exactly the fence this increment's safety argument needs, verified
;; empirically against this host's real system libraries rather than
;; merely asserted: `/usr/lib/x86_64-linux-gnu/libc.so.6' carries a real
;; `PT_TLS' (`readelf -l', a `TLS' program header at file offset
;; 0x1e0ba0); `libm.so.6' does not, but its own `DT_NEEDED' list (`readelf
;; -d') names `libc.so.6' first -- so a caller that opens this
;; repository's own `nl-ffi-loader-fixture-needs-dep.so' (a real
;; `DT_NEEDED' on `libm.so.6') is refused two hops down, at `libc.so.6''s
;; TLS, never having mapped a byte of `libc.so.6' read-write or executed
;; any of its code (the read-only file mapping used to check its program
;; headers is harmless and is unmapped along with everything else on
;; refusal -- see "No half-loaded object left mapped" below).  This is
;; not a special case for "libc" by name anywhere in this file; it falls
;; out of the ordinary, per-object TLS check already running on every
;; node this loader discovers, including a dependency's dependency.  The
;; safe, useful subset this increment actually reaches is exactly what
;; the brief predicted: self-contained, TLS-free objects -- a project's
;; own leaf libraries, and this package's own new fixtures
;; (`nl-ffi-loader-fixture-dep-leaf(2).so'/`-dep-root.so', `-ctor-dep.so'/
;; `-ctor-root.so', `-ifunc.so') -- while a dependency graph that reaches
;; real glibc bottoms out cleanly at a named, specific reason instead of
;; running.
;;
;; The same reasoning bounds IFUNC and initializers: a resolver or
;; constructor this loader actually reaches, by construction, belongs to
;; an object (and everything in ITS graph) that has already proven it
;; does not need `PT_TLS'.  This is not a complete safety argument for
;; every conceivable freestanding-unsafe thing a constructor could do
;; (one could still read `argc'/`argv'/`envp' the way `__libc_start_main'
;; would have supplied them, which this loader never does, or call a raw
;; syscall wrapper assuming a libc errno convention that does not exist
;; here) -- this file does not detect that in general, and this
;; Commentary names the gap rather than hiding it.  It is, however, a
;; real, tested boundary, not an assertion: every fixture this increment
;; adds that DOES run an initializer or IFUNC resolver is `-nostdlib',
;; with the loader's own relocation/dependency machinery as the only
;; thing standing between "this needed something the process cannot
;; provide" and "this ran" -- see the fixtures and the report.
;;
;; ---------------------------------------------------------------------
;; SONAME RESOLUTION (`DT_NEEDED' -> a real path)
;;
;; Unlike increment 1's single-object `nl-ffi-loader-open' (still true:
;; the PATH a direct caller passes is used exactly as given, no search),
;; a `DT_NEEDED' entry is a bare SONAME, not a path, and needs real
;; resolution.  Decided and implemented, in this order:
;;   1. A `DT_NEEDED' name containing a "/" is used literally, as a path
;;      -- no search -- matching real `ld.so''s own rule for slash-
;;      containing names.
;;   2. The REQUESTING object's own `DT_RUNPATH' (preferred) or
;;      `DT_RPATH' (only when `DT_RUNPATH' is absent -- matching real
;;      `ld.so' precedence between the two), a colon-separated directory
;;      list.  NO `$ORIGIN'/`$LIB'/`$PLATFORM' token expansion -- a
;;      named, documented gap, not a silent one; this repository's own
;;      fixtures avoid needing it by baking an ABSOLUTE path into the
;;      linked-in rpath at build time (see the Makefile's `ffi-loader'
;;      target) rather than relying on `$ORIGIN'.
;;   3. `LD_LIBRARY_PATH' (colon-separated, read via `getenv' at open
;;      time -- confirmed unset on this host, so this is a no-op today,
;;      but the real, correct behavior for a caller that sets it, and for
;;      this package's own gate, which uses it: see
;;      `nl-ffi-loader--ld-library-path-dirs').
;;   4. A fixed standard-directory list matching this host's own layout
;;      (`/usr/lib/x86_64-linux-gnu/', `/lib/x86_64-linux-gnu/',
;;      `/usr/lib/', `/lib/').
;; The first directory (in this order) that contains an openable file
;; named SONAME wins.  A `DT_NEEDED' name this cannot resolve to ANY real
;; path signals `nl-ffi-loader-unsupported' (reason
;; `:dependency-not-found') -- never silently skipped.
;;
;; Deliberately NOT implemented: parsing `/etc/ld.so.cache' (present on
;; this host, in the binary "new format" -- confirmed a real fact, not
;; assumed).  Three reasons, together: every dependency this increment
;; can actually LOAD (as opposed to refuse via TLS, per the safety
;; argument above) is one this loader's caller controls directly --
;; something a project ships alongside its own code -- and the fixed
;; search list plus `DT_RUNPATH'/`DT_RPATH' already reaches it without
;; needing the cache at all.  Everything else the cache would resolve to
;; on a normal Linux host is glibc-provided and TLS-bearing, refused
;; regardless of how accurately its path was found.  And hand-parsing the
;; cache's binary format correctly (the magic string, the flag/hwcap-
;; tagged extension entries, alignment) is real, unreviewed bug surface
;; that -- per the first two points -- buys this increment nothing it can
;; use.  Left as a named gap for a future increment if a dependency
;; genuinely needs a nonstandard `ld.so.conf.d' directory this list does
;; not already cover.
;;
;; ---------------------------------------------------------------------
;; SYMBOL SEARCH ORDER (multi-object)
;;
;; A single `nl-ffi-loader-open' call discovers a whole dependency GRAPH
;; (the object named, plus every `DT_NEEDED' it names, transitively,
;; breadth-first, each object mapped at most once -- deduplicated by its
;; OWN resolved path string; see "Known simplification" below).  Every
;; relocation, and every `nl-ffi-loader-symbol'/`nl-ffi-loader-symbol-
;; object' lookup against the resulting handle, resolves a name against
;; this SAME graph, in this documented order:
;;   1. If the relocation's own local `.dynsym' entry is DEFINED in the
;;      object the relocation belongs to (`st_shndx <> SHN_UNDEF'), THAT
;;      entry is used directly -- no name search at all (unchanged from
;;      increment 1; this is why a self-contained object's behavior does
;;      not change one bit under this increment).
;;   2. Otherwise, the symbol's NAME is searched across every object in
;;      the graph, in BREADTH-FIRST DISCOVERY order (the object first
;;      passed to `nl-ffi-loader-open', then each `DT_NEEDED' the first
;;      time it is discovered, in the order its `.dynamic' names it) --
;;      the first object whose OWN `.dynsym' defines the name wins.  This
;;      is a flat, `RTLD_GLOBAL'-shaped scope local to this one open()
;;      call's own graph: two SEPARATE `nl-ffi-loader-open' calls (two
;;      different handles) never search each other's graphs, and this
;;      file makes no attempt at real symbol interposition/versioning
;;      beyond this one order.
;; `nl-ffi-loader-fixture-dep-leaf.so'/`nl-ffi-loader-fixture-dep-leaf2.so'
;; (both export the SAME name, deliberately, with different, checkable
;; behavior) plus `nl-ffi-loader-fixture-dep-root.so' (`DT_NEEDED' on
;; both, leaf first) exist specifically to prove this order empirically,
;; not just assert it -- see the smoke test.
;; `nl-ffi-loader-symbol-object' answers "which object in HANDLE's graph
;; would satisfy this name" directly, for a caller (or a test) that wants
;; to confirm the order without re-deriving it from a relocation's
;; side effect.
;;
;; A GLOB_DAT/JUMP_SLOT relocation whose local `.dynsym' entry is
;; UNDEFINED and is found NOWHERE in the graph is refused
;; (`:undefined-symbol', as increment 1 already did) -- UNLESS its
;; binding is `STB_WEAK' (`ST_BIND' = 2), in which case it resolves to
;; address 0 exactly as a real `ld.so' would (see "A real defect found
;; while implementing" below): a legitimately absent optional symbol, not
;; a "this loader could not supply what was needed" refusal.
;;
;; Known simplification: dependency deduplication is by the resolved
;; path STRING, not by device+inode the way real `ld.so' truly
;; canonicalizes identity.  Two different path spellings of the same
;; real file (through a symlink this loader does not resolve, for
;; example) would be mapped and initialized twice.  Not a concern for
;; any object this loader can actually reach (it refuses everything that
;; would make double-initialization dangerous -- see the libc discussion
;; above), documented rather than silently assumed away.
;;
;; ---------------------------------------------------------------------
;; IFUNC (`R_X86_64_IRELATIVE')
;;
;; The relocation's `r_addend' is the ADDRESS OF A RESOLVER FUNCTION
;; (bias + addend), not a target address -- this loader CALLS it, through
;; `ptr-call' with all six argument slots zero (the classic, pre-"ifunc2"
;; x86-64 ABI a resolver like this expects no arguments at all; a
;; resolver relying on the newer `GNU_PROPERTY'-advertised
;; hwcap/`cpu_features' argument convention will not receive it -- a
;; named limitation, and in practice such resolvers are a glibc-internal
;; optimization that is unreachable here anyway per the TLS argument
;; above), and writes the CALL'S RESULT into the relocation's target.
;;
;; Confirmed empirically (compiled and compared with `readelf -r' before
;; writing this fixture) that only a HIDDEN-visibility ifunc, in a
;; `-shared -fPIC' build, compiles to a genuine `R_X86_64_IRELATIVE'
;; entry in `.rela.plt'/`.rela.dyn' with no symbol (`r_sym' = 0, the
;; resolver's address carried directly in `r_addend').  A DEFAULT-
;; visibility (exported, preemptible) ifunc instead compiles to an
;; ordinary `R_X86_64_JUMP_SLOT'/`R_X86_64_GLOB_DAT' relocation whose
;; referenced `.dynsym' entry merely has `st_info' TYPE
;; `STT_GNU_IFUNC' (10) -- a related but DIFFERENT mechanism this
;; increment's scope (`R_X86_64_IRELATIVE' specifically, per the design
;; brief) does not implement.  Silently treating such a symbol's
;; `st_value' as an ordinary function/data address would be WRONG (it is
;; the resolver's own address, not the resolved implementation) and is
;; exactly the kind of silent half-work this file refuses to do
;; elsewhere -- `nl-ffi-loader--apply-one-relocation' detects this case
;; explicitly (checking the resolved symbol's `st_info' TYPE, wherever it
;; is found -- locally or across the graph) and refuses it by name
;; (`:ifunc-symbol-via-relocation') rather than writing a wrong address.
;;
;; Ordering, WITHIN one object: `R_X86_64_IRELATIVE' entries are held
;; back into a separate list during the main relocation pass and applied
;; only AFTER that object's segments receive their FINAL protection
;; (`nl-ffi-loader--protect-segments') -- a resolver's own code must
;; already be executable (PROT_EXEC) before it can be called through
;; `ptr-call' at all, and increment 1's segments are only read-write
;; until final protection runs.  The relocation TARGET a resolver's
;; result is written into stays writable at that point regardless (this
;; loader does not implement `PT_GNU_RELRO' re-protection -- see "Out of
;; scope" -- so a GOT-holding segment's real `p_flags' stays RW straight
;; through).
;;
;; Ordering, ACROSS a dependency graph: every object's ordinary
;; relocations run first (any order -- a `.dynsym' entry's `st_value' is
;; correct as soon as its OWN object is mapped and biased, independent of
;; whether any relocation, in this or another object, has run yet), THEN
;; every object's segments are protected, THEN every object's
;; `R_X86_64_IRELATIVE' entries run, in dependency-first (postorder)
;; order -- a dependency's own IFUNC resolvers (and everything they might
;; reference within their own, already-relocated object) are only ever
;; run before the object that depends on them.  See
;; `nl-ffi-loader--postorder'.
;;
;; ---------------------------------------------------------------------
;; INITIALIZERS (`DT_INIT'/`DT_INIT_ARRAY')
;;
;; Run strictly AFTER every relocation in the WHOLE graph (ordinary and
;; `R_X86_64_IRELATIVE' both) has been applied everywhere, in dependency-
;; first (postorder) order across the graph -- a dependency's
;; initializers always run before its dependent's, per the design brief.
;; Within one object: `DT_INIT' (a single function pointer, if present)
;; runs first, then `DT_INIT_ARRAY' in array order -- matching real
;; `ld.so'.  Each is called through `ptr-call' with all six argument
;; slots zero (an ordinary shared object's `.init_array' entries are
;; `void (*)(void)' in the modern ABI; this loader does not attempt the
;; historical `argc'/`argv'/`envp'-receiving convention some very old
;; toolchains gave `DT_INIT' for a MAIN executable -- not applicable
;; here, since this loader only ever opens `ET_DYN' shared objects).
;;
;; ---------------------------------------------------------------------
;; A REAL DEFECT FOUND WHILE IMPLEMENTING: weak undefined symbols
;;
;; `nl-ffi-loader-fixture-init.so' (an ORDINARY `cc -shared -fPIC' build,
;; no `-nostdlib') has no `DT_NEEDED' at all (binutils's default
;; `--as-needed' drops the implicit libc dependency once nothing in the
;; object actually needs it) -- but DOES carry four `R_X86_64_GLOB_DAT'
;; relocations, from `crtbeginS.o', against `__cxa_finalize',
;; `_ITM_registerTMCloneTable', `_ITM_deregisterTMCloneTable', and
;; `__gmon_start__', every one of them `STB_WEAK' and `SHN_UNDEF'
;; (confirmed with `readelf -sW' before writing this file's fix) -- with
;; nothing, anywhere in this object's own graph, defining any of them.
;; Increment 1's unconditional "`SHN_UNDEF' -> refuse" rule would refuse
;; this object outright the moment `:has-initializers' (increment 1's own
;; blanket refusal for ANY object with `DT_INIT'/`DT_INIT_ARRAY') stopped
;; pre-empting it -- exactly the kind of ordinary, unremarkable object
;; this increment is supposed to newly support.  Real `ld.so' does not
;; refuse this: an undefined WEAK symbol with no definition anywhere
;; resolves to address 0 (NULL) -- legitimate, intentional, and exactly
;; why `crtbeginS.o''s own generated code null-checks each of these
;; before calling through them.  `nl-ffi-loader--apply-one-relocation'
;; now reads the resolved symbol's `ST_BIND' (not just `ST_SHNDX') and
;; writes 0 for an unresolved WEAK reference instead of refusing;
;; anything else (the default, `STB_GLOBAL') unresolved anywhere in the
;; graph still refuses `:undefined-symbol', unchanged.

;;; Code:

(declare-function syscall-direct "ext:nelisp-runtime" (nr a0 a1 a2 a3 a4 a5))
(declare-function alloc-bytes "ext:nelisp-runtime" (nbytes align))
(declare-function ptr-read-u8 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u8 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-read-u32 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u32 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-read-u64 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u64 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-call "ext:nelisp-runtime" (address a b c d e f))
;; Defined earlier in nl-ffi.el, which loads this file after it -- see
;; that file's Commentary on load order.  Declared here so byte-compiling
;; this file alone (its own gate's `nl-ffi.el' load never having run) does
;; not warn that it is undefined.  `nl-ffi--string-to-cstring' is NOT
;; reused here even though it does the same thing -- it is `nl-ffi.el's
;; own private (`--') helper, and `nl-ns-inventory' polices exactly that
;; boundary; see `nl-ffi-loader--cstring' below instead.
(declare-function nl-ffi-get-string "nl-ffi" (ptr))

;;;; --- error conditions ---------------------------------------------------
;;
;; Both are children of `nl-ffi-error' (defined earlier in nl-ffi.el, which
;; loads this file -- see that file's Commentary for the load order this
;; relies on), so an existing broad `nl-ffi-error' handler keeps working
;; unchanged; a caller that wants to tell "the file/mapping step failed"
;; from "the object needs something this loader refuses to half-do" can
;; still catch these two by name.

(define-error 'nl-ffi-loader-error
  "pure-elisp ELF loader error" 'nl-ffi-error)

(define-error 'nl-ffi-loader-open-failed
  "opening or mapping the shared object failed (openat/fstat/mmap)"
  'nl-ffi-loader-error)

(define-error 'nl-ffi-loader-unsupported
  "the shared object needs something this increment's loader refuses \
rather than half-does -- see the DATA reason and this file's Commentary \
\"Out of scope\" list"
  'nl-ffi-loader-error)

;;;; --- syscall / mmap / ELF constants (x86-64 Linux) ------------------------

(defconst nl-ffi-loader--sys-close 3)
(defconst nl-ffi-loader--sys-fstat 5)
(defconst nl-ffi-loader--sys-mmap 9)
(defconst nl-ffi-loader--sys-mprotect 10)
(defconst nl-ffi-loader--sys-munmap 11)
(defconst nl-ffi-loader--sys-openat 257)

(defconst nl-ffi-loader--at-fdcwd -100)
(defconst nl-ffi-loader--o-rdonly 0)

(defconst nl-ffi-loader--prot-none 0)
(defconst nl-ffi-loader--prot-read 1)
(defconst nl-ffi-loader--prot-write 2)
(defconst nl-ffi-loader--prot-exec 4)

(defconst nl-ffi-loader--map-shared 1)
(defconst nl-ffi-loader--map-private 2)
(defconst nl-ffi-loader--map-fixed 16)
(defconst nl-ffi-loader--map-anonymous 32)

(defconst nl-ffi-loader--page-size 4096)

(defconst nl-ffi-loader--pt-load 1)
(defconst nl-ffi-loader--pt-dynamic 2)
(defconst nl-ffi-loader--pt-tls 7)

(defconst nl-ffi-loader--pf-x 1)
(defconst nl-ffi-loader--pf-w 2)
(defconst nl-ffi-loader--pf-r 4)

(defconst nl-ffi-loader--dt-null 0)
(defconst nl-ffi-loader--dt-needed 1)
(defconst nl-ffi-loader--dt-pltrelsz 2)
(defconst nl-ffi-loader--dt-hash 4)
(defconst nl-ffi-loader--dt-strtab 5)
(defconst nl-ffi-loader--dt-symtab 6)
(defconst nl-ffi-loader--dt-rela 7)
(defconst nl-ffi-loader--dt-relasz 8)
(defconst nl-ffi-loader--dt-relaent 9)
(defconst nl-ffi-loader--dt-syment 11)
(defconst nl-ffi-loader--dt-init 12)
(defconst nl-ffi-loader--dt-init-array 25)
(defconst nl-ffi-loader--dt-init-arraysz 27)
(defconst nl-ffi-loader--dt-rpath 15)
(defconst nl-ffi-loader--dt-pltrel 20)
(defconst nl-ffi-loader--dt-jmprel 23)
(defconst nl-ffi-loader--dt-runpath 29)
(defconst nl-ffi-loader--dt-relr-lo #x6fffe035)
(defconst nl-ffi-loader--dt-relr-hi #x6fffe037)
(defconst nl-ffi-loader--dt-gnu-hash #x6ffffef5)

(defconst nl-ffi-loader--reloc-relative 8)
(defconst nl-ffi-loader--reloc-glob-dat 6)
(defconst nl-ffi-loader--reloc-jump-slot 7)
(defconst nl-ffi-loader--reloc-irelative 37)

(defconst nl-ffi-loader--stb-weak 2
  "ELF64_ST_BIND value for STB_WEAK -- see this file's Commentary, \"A real
defect found while implementing\".")

(defconst nl-ffi-loader--stt-gnu-ifunc 10
  "ELF64_ST_TYPE value for STT_GNU_IFUNC -- see this file's Commentary,
\"IFUNC\", for why a GLOB_DAT/JUMP_SLOT relocation resolving to a symbol
of this type is refused rather than silently mishandled.")

(defconst nl-ffi-loader--tls-relocation-types
  '(16 17 18 19 20 21 22 23 34 35 36)
  "R_X86_64_{DTPMOD64,DTPOFF64,TPOFF64,TLSGD,TLSLD,DTPOFF32,GOTTPOFF,
TPOFF32,GOTPC32_TLSDESC,TLSDESC_CALL,TLSDESC} -- refused even though this
file's PT_TLS check (`nl-ffi-loader--map-node') already rejects any
object that needs one of these before a single relocation is inspected;
kept as a second, independent check on the relocation type itself.")

(defconst nl-ffi-loader--standard-dirs
  '("/usr/lib/x86_64-linux-gnu/" "/lib/x86_64-linux-gnu/"
    "/usr/lib/" "/lib/")
  "Fixed SONAME search directories, matching this repository's dev/CI
host's own layout -- see this file's Commentary, \"SONAME resolution\".")

;;;; --- small numeric helpers -------------------------------------------------

(defun nl-ffi-loader--align-down (n align)
  (- n (mod n align)))

(defun nl-ffi-loader--align-up (n align)
  (nl-ffi-loader--align-down (+ n (1- align)) align))

(defun nl-ffi-loader--u16 (ptr offset)
  "Read the 2-byte little-endian field at PTR+OFFSET.
There is no `ptr-read-u16' primitive; every ELF64 u16 field this loader
reads (`e_type'/`e_machine'/`e_phentsize'/`e_phnum', `st_shndx') is
immediately followed, within the same already-mapped struct, by at least
2 more bytes belonging to an adjacent field -- so reading 4 bytes with
`ptr-read-u32' and masking the low 16 bits never reads unmapped memory;
those adjacent bytes (the high 16 bits of the 4-byte read) are simply not
looked at."
  (logand (ptr-read-u32 ptr offset) #xffff))

(defun nl-ffi-loader--syscall-error-p (result)
  "Return non-nil when RESULT is a raw Linux syscall's -errno failure.
A real mmap/openat/fstat/mprotect/munmap/close success is never negative
on this ABI (user-space addresses never set the top bit as a signed
64-bit value); an error is `-errno', always in [-4095, -1] by convention
-- checked as a range rather than plain `(< result 0)' so a caller
mis-reading a real (impossibly huge) address as negative would not be
misclassified as \"a small errno\" and vice versa."
  (and (< result 0) (> result -4096)))

;;;; --- reading the file: open + fstat + read-only mmap -----------------------

(defun nl-ffi-loader--cstring (string)
  "Copy STRING into a fresh NUL-terminated buffer; return its address.
A local copy of `nl-ffi.el's own `nl-ffi--string-to-cstring' (same
`alloc-bytes'/`ptr-write-u8' shape) rather than a cross-file call to it:
that name is `nl-ffi.el's private (`--') helper, and `nl-ns-inventory'
polices exactly that file-privacy boundary.  STRING is treated as a
sequence of bytes, exactly as the original does; see its docstring for
the same caveats (multibyte text, the buffer never being freed)."
  (let* ((n (length string))
         (buf (alloc-bytes (1+ n) 1))
         (i 0))
    (while (< i n)
      (ptr-write-u8 buf i (aref string i))
      (setq i (1+ i)))
    (ptr-write-u8 buf n 0)
    buf))

(defun nl-ffi-loader--open-ro (path)
  "openat(AT_FDCWD, PATH, O_RDONLY); return the fd, or signal.
Signals `nl-ffi-loader-open-failed' (DATA: PATH, the raw -errno) on
failure.  PATH is used exactly as given -- see this file's Commentary and
`nl-ffi-loader-open''s docstring for why this loader does no SONAME
search-path resolution the way real `dlopen' does for a bare library
name; a caller wants a real, openable path here (dependency resolution,
`nl-ffi-loader--resolve-soname', does that search and hands THIS function
a real path too)."
  (let* ((path-c (nl-ffi-loader--cstring path))
         (fd (syscall-direct nl-ffi-loader--sys-openat
                              nl-ffi-loader--at-fdcwd path-c
                              nl-ffi-loader--o-rdonly 0 0 0)))
    (if (nl-ffi-loader--syscall-error-p fd)
        (signal 'nl-ffi-loader-open-failed (list path fd))
      fd)))

(defun nl-ffi-loader--path-openable-p (path)
  "Return non-nil when PATH can be opened read-only right now.
Used only by SONAME search-path resolution to test a CANDIDATE path
before committing to it -- closes the fd immediately either way; never
signals."
  (let* ((path-c (nl-ffi-loader--cstring path))
         (fd (syscall-direct nl-ffi-loader--sys-openat
                              nl-ffi-loader--at-fdcwd path-c
                              nl-ffi-loader--o-rdonly 0 0 0)))
    (if (nl-ffi-loader--syscall-error-p fd)
        nil
      (syscall-direct nl-ffi-loader--sys-close fd 0 0 0 0 0)
      t)))

(defun nl-ffi-loader--fstat-size (fd)
  "Return FD's `st_size' via `fstat(2)'.
`struct stat''s `st_size' field sits at byte offset 48 on this platform's
layout (confirmed against `struct stat' -- see the report's Step 1 for
the field-offset derivation this matches)."
  (let* ((statbuf (alloc-bytes 144 8))
         (rc (syscall-direct nl-ffi-loader--sys-fstat fd statbuf 0 0 0 0)))
    (if (nl-ffi-loader--syscall-error-p rc)
        (signal 'nl-ffi-loader-open-failed (list :fstat-failed fd rc))
      (ptr-read-u64 statbuf 48))))

(defun nl-ffi-loader--map-file-readonly (path)
  "Open, fstat, and read-only `mmap' PATH's whole contents.
Returns (BASE . SIZE); the fd itself is closed immediately after the
`mmap' call succeeds (the mapping persists independently of the fd,
exactly like every other file-backed mapping on Linux).  The caller is
responsible for `munmap'ing (BASE . SIZE) once done with it -- see
`nl-ffi-loader--file-mappings' below, which every caller in this file
uses rather than an ad hoc `unwind-protect' each."
  (let* ((fd (nl-ffi-loader--open-ro path))
         (size (nl-ffi-loader--fstat-size fd)))
    (when (<= size 0)
      (syscall-direct nl-ffi-loader--sys-close fd 0 0 0 0 0)
      (signal 'nl-ffi-loader-open-failed (list :empty-or-unreadable path size)))
    (let ((base (syscall-direct nl-ffi-loader--sys-mmap 0 size
                                 nl-ffi-loader--prot-read
                                 nl-ffi-loader--map-private fd 0)))
      (syscall-direct nl-ffi-loader--sys-close fd 0 0 0 0 0)
      (when (nl-ffi-loader--syscall-error-p base)
        (signal 'nl-ffi-loader-open-failed (list :mmap-file-failed path base)))
      (cons base size))))

;;;; --- process-wide cleanup bookkeeping (see "No half-loaded object left
;;;; mapped" in this file's Commentary) ----------------------------------------
;;
;; Both let-bound around the whole discovery+relocate+protect+init
;; pipeline in `nl-ffi-loader-open'.  `nl-ffi-loader--file-mappings'
;; (read-only file mmaps, needed only transiently to read ELF headers and
;; copy segment bytes out) is always unmapped once the pipeline finishes,
;; success or failure.  `nl-ffi-loader--reservations' (the real,
;; content-bearing address-space reservation for every node successfully
;; mapped so far) is unmapped ONLY on failure -- on success these ARE the
;; loaded library and must stay mapped for the rest of the process, as
;; increment 1's single-object version already documented.

(defvar nl-ffi-loader--file-mappings nil)
(defvar nl-ffi-loader--reservations nil)

(defun nl-ffi-loader--map-file-readonly-tracked (path)
  (let ((fm (nl-ffi-loader--map-file-readonly path)))
    (push fm nl-ffi-loader--file-mappings)
    fm))

(defun nl-ffi-loader--unmap-file-mappings ()
  (dolist (fm nl-ffi-loader--file-mappings)
    (syscall-direct nl-ffi-loader--sys-munmap (car fm) (cdr fm) 0 0 0 0))
  (setq nl-ffi-loader--file-mappings nil))

(defun nl-ffi-loader--unmap-reservations ()
  (dolist (r nl-ffi-loader--reservations)
    (syscall-direct nl-ffi-loader--sys-munmap (car r) (cdr r) 0 0 0 0))
  (setq nl-ffi-loader--reservations nil))

;;;; --- ELF header / program header parsing -----------------------------------

(defconst nl-ffi-loader--elf-magic '(#x7f #x45 #x4c #x46)
  "The four ELF magic bytes: 0x7f, then ASCII \"ELF\".")

(defun nl-ffi-loader--check-elf-header (file-base path)
  "Validate FILE-BASE is a little-endian ELFCLASS64 ET_DYN x86-64 image.
Signals `nl-ffi-loader-unsupported' (reason `:not-elf64-shared-object')
otherwise -- this loader only ever targets a shared object built for the
same architecture/ABI it is itself running on."
  (let ((ok
         (and (= (ptr-read-u8 file-base 0) (nth 0 nl-ffi-loader--elf-magic))
              (= (ptr-read-u8 file-base 1) (nth 1 nl-ffi-loader--elf-magic))
              (= (ptr-read-u8 file-base 2) (nth 2 nl-ffi-loader--elf-magic))
              (= (ptr-read-u8 file-base 3) (nth 3 nl-ffi-loader--elf-magic))
              (= (ptr-read-u8 file-base 4) 2)   ; EI_CLASS = ELFCLASS64
              (= (ptr-read-u8 file-base 5) 1)   ; EI_DATA = little-endian
              (= (nl-ffi-loader--u16 file-base 16) 3)   ; e_type = ET_DYN
              (= (nl-ffi-loader--u16 file-base 18) 62)))) ; e_machine = EM_X86_64
    (unless ok
      (signal 'nl-ffi-loader-unsupported
              (list :not-elf64-shared-object path)))))

(defun nl-ffi-loader--program-headers (file-base)
  "Parse FILE-BASE's program headers.
Returns a plist: `:loads' (a list of (P_VADDR P_OFFSET P_FILESZ P_MEMSZ
P_FLAGS), sorted by P_VADDR ascending), `:dyn-vaddr' (PT_DYNAMIC's
P_VADDR, or nil), `:tls-p' (non-nil when a PT_TLS header is present)."
  (let* ((phoff (ptr-read-u64 file-base 32))
         (phentsize (nl-ffi-loader--u16 file-base 54))
         (phnum (nl-ffi-loader--u16 file-base 56))
         (loads nil) (dyn-vaddr nil) (tls-p nil)
         (i 0))
    (while (< i phnum)
      (let* ((ph (+ file-base phoff (* i phentsize)))
             (p-type (ptr-read-u32 ph 0))
             (p-flags (ptr-read-u32 ph 4))
             (p-offset (ptr-read-u64 ph 8))
             (p-vaddr (ptr-read-u64 ph 16))
             (p-filesz (ptr-read-u64 ph 32))
             (p-memsz (ptr-read-u64 ph 40)))
        (cond
         ((= p-type nl-ffi-loader--pt-load)
          (push (list p-vaddr p-offset p-filesz p-memsz p-flags) loads))
         ((= p-type nl-ffi-loader--pt-dynamic)
          (setq dyn-vaddr p-vaddr))
         ((= p-type nl-ffi-loader--pt-tls)
          (setq tls-p t))))
      (setq i (1+ i)))
    (list :loads (sort (nreverse loads) (lambda (a b) (< (car a) (car b))))
          :dyn-vaddr dyn-vaddr
          :tls-p tls-p)))

;;;; --- mapping the image: reserve, carve, copy, (later) protect --------------

(defun nl-ffi-loader--copy-bytes (src dst n)
  "Copy N bytes from SRC to DST, 4 at a time with a byte-wise tail.
Both SRC and DST may be unaligned -- `ptr-read-u32'/`ptr-write-u32' do
not require alignment on this reader (measured; see the report).  Uses
the 32-bit primitives rather than `ptr-read-u64'/`ptr-write-u64': this
copies ARBITRARY segment content (code, string tables, data), which can
carry any bit pattern, and this file's Commentary documents that the
64-bit primitives do not round-trip a value above `most-positive-fixnum' --
common in ordinary text/data, confirmed by this exact copy corrupting a
symbol name before this fix.  A 32-bit word is always far below that
ceiling, so this is exact for any content."
  (let ((i 0) (full (* 4 (/ n 4))))
    (while (< i full)
      (ptr-write-u32 dst i (ptr-read-u32 src i))
      (setq i (+ i 4)))
    (while (< i n)
      (ptr-write-u8 dst i (ptr-read-u8 src i))
      (setq i (1+ i)))))

(defun nl-ffi-loader--prot-of-pflags (flags)
  "Translate an Elf64_Phdr P_FLAGS bitfield to a Linux PROT_* bitfield.
The two use different bit positions for the same three permissions."
  (logior (if (/= (logand flags nl-ffi-loader--pf-r) 0) nl-ffi-loader--prot-read 0)
          (if (/= (logand flags nl-ffi-loader--pf-w) 0) nl-ffi-loader--prot-write 0)
          (if (/= (logand flags nl-ffi-loader--pf-x) 0) nl-ffi-loader--prot-exec 0)))

(defun nl-ffi-loader--reserve (loads)
  "Reserve one contiguous, currently-inaccessible span covering LOADS.
Returns (BIAS . SPAN-SIZE).  BIAS is added to every P_VADDR in LOADS (and
to every other vaddr-shaped field this loader later reads -- symbol
values, DT_* pointers, relocation offsets) to get a real runtime address;
it is whatever the kernel actually placed the reservation `mmap' at minus
the lowest P_VADDR (normally 0 for a PIE shared object, so BIAS usually
equals the reservation's own address, but this does not assume that).
Pushed onto `nl-ffi-loader--reservations' so a later failure anywhere in
the pipeline unmaps it -- see this file's Commentary."
  (let* ((lo (nl-ffi-loader--align-down (caar loads) nl-ffi-loader--page-size))
         (hi (let ((m 0))
               (dolist (seg loads)
                 (let ((end (+ (nth 0 seg) (nth 3 seg))))
                   (when (> end m) (setq m end))))
               (nl-ffi-loader--align-up m nl-ffi-loader--page-size)))
         (span (- hi lo))
         (reservation (syscall-direct nl-ffi-loader--sys-mmap 0 span
                                       nl-ffi-loader--prot-none
                                       (logior nl-ffi-loader--map-private
                                               nl-ffi-loader--map-anonymous)
                                       -1 0)))
    (when (nl-ffi-loader--syscall-error-p reservation)
      (signal 'nl-ffi-loader-open-failed (list :mmap-reservation-failed reservation)))
    (push (cons reservation span) nl-ffi-loader--reservations)
    (cons (- reservation lo) span)))

(defun nl-ffi-loader--map-and-copy-segments (bias file-base loads)
  "Carve out and populate every PT_LOAD segment in LOADS at BIAS.
Signals `nl-ffi-loader-unsupported' (reason `:overlapping-segments') if
two segments' page-aligned ranges overlap -- see this file's Commentary
for why that is refused rather than resolved via a per-page protection
union."
  (let ((prev-hi nil))
    (dolist (seg loads)
      (let* ((vaddr (nth 0 seg)) (offset (nth 1 seg))
             (filesz (nth 2 seg)) (memsz (nth 3 seg))
             (lo (nl-ffi-loader--align-down vaddr nl-ffi-loader--page-size))
             (hi (nl-ffi-loader--align-up (+ vaddr memsz) nl-ffi-loader--page-size)))
        (when (and prev-hi (< lo prev-hi))
          (signal 'nl-ffi-loader-unsupported
                  (list :overlapping-segments lo prev-hi)))
        (let ((m (syscall-direct nl-ffi-loader--sys-mmap (+ bias lo) (- hi lo)
                                  (logior nl-ffi-loader--prot-read nl-ffi-loader--prot-write)
                                  (logior nl-ffi-loader--map-private
                                          nl-ffi-loader--map-anonymous
                                          nl-ffi-loader--map-fixed)
                                  -1 0)))
          (when (or (nl-ffi-loader--syscall-error-p m) (/= m (+ bias lo)))
            (signal 'nl-ffi-loader-open-failed (list :mmap-segment-failed lo m))))
        (when (> filesz 0)
          (nl-ffi-loader--copy-bytes (+ file-base offset) (+ bias vaddr) filesz))
        (setq prev-hi hi)))))

(defun nl-ffi-loader--protect-segments (bias loads)
  "Apply each PT_LOAD's real final protection, one `mprotect' call each.
Safe only because `nl-ffi-loader--map-and-copy-segments' already refused
any pair of segments whose page-aligned ranges overlap; see its
docstring."
  (dolist (seg loads)
    (let* ((vaddr (nth 0 seg)) (memsz (nth 3 seg)) (flags (nth 4 seg))
           (lo (nl-ffi-loader--align-down vaddr nl-ffi-loader--page-size))
           (hi (nl-ffi-loader--align-up (+ vaddr memsz) nl-ffi-loader--page-size))
           (prot (nl-ffi-loader--prot-of-pflags flags))
           (rc (syscall-direct nl-ffi-loader--sys-mprotect (+ bias lo) (- hi lo)
                                prot 0 0 0)))
      (when (nl-ffi-loader--syscall-error-p rc)
        (signal 'nl-ffi-loader-open-failed (list :mprotect-failed lo rc))))))

;;;; --- .dynamic parsing -------------------------------------------------------

(defun nl-ffi-loader--parse-dynamic (bias dyn-vaddr)
  "Read the Elf64_Dyn array at BIAS+DYN-VADDR into a plist of runtime facts.
Every vaddr-shaped DT_* value (DT_STRTAB/DT_SYMTAB/DT_HASH/DT_GNU_HASH/
DT_RELA/DT_JMPREL) is returned already biased into a real runtime
address; every size/count/flag value (DT_STRSZ and friends, DT_PLTREL) is
returned as-is.  `:needed' is a list of RAW .dynstr OFFSETS (encounter
order) for every DT_NEEDED tag -- not yet resolved to strings, since
DT_STRTAB is not guaranteed to appear before DT_NEEDED in this same
array; the caller resolves each once the full plist (and so :strtab) is
known.  `:rpath-off'/`:runpath-off' are the single (last-seen, matching
ordinary dynamic-tag semantics) raw .dynstr offset for DT_RPATH/
DT_RUNPATH, or nil.  `:init' is DT_INIT already biased, or nil;
`:init-array'/`:init-arraysz' are DT_INIT_ARRAY (biased) and its byte
size.  Also detects DT_RELR/DT_RELRSZ/DT_RELRENT (the
0x6fffe035..0x6fffe037 tag range) so an object using the compact
relative-relocation encoding is refused rather than silently
under-relocated -- see this file's Commentary."
  (let ((strtab nil) (symtab nil) (syment nil)
        (gnu-hash nil) (sysv-hash nil)
        (rela nil) (relasz 0) (relaent 0)
        (jmprel nil) (pltrelsz 0) (pltrel nil)
        (needed nil) (rpath-off nil) (runpath-off nil)
        (dt-init nil) (init-array nil) (init-arraysz 0)
        (relr-p nil)
        (i 0) (go t))
    (while go
      (let* ((entry (+ bias dyn-vaddr (* i 16)))
             (tag (ptr-read-u64 entry 0))
             (val (ptr-read-u64 entry 8)))
        (cond
         ((= tag nl-ffi-loader--dt-null) (setq go nil))
         ((= tag nl-ffi-loader--dt-needed) (push val needed))
         ((= tag nl-ffi-loader--dt-hash) (setq sysv-hash (+ bias val)))
         ((= tag nl-ffi-loader--dt-strtab) (setq strtab (+ bias val)))
         ((= tag nl-ffi-loader--dt-symtab) (setq symtab (+ bias val)))
         ((= tag nl-ffi-loader--dt-rela) (setq rela (+ bias val)))
         ((= tag nl-ffi-loader--dt-relasz) (setq relasz val))
         ((= tag nl-ffi-loader--dt-relaent) (setq relaent val))
         ((= tag nl-ffi-loader--dt-syment) (setq syment val))
         ((= tag nl-ffi-loader--dt-init) (setq dt-init (+ bias val)))
         ((= tag nl-ffi-loader--dt-init-array) (setq init-array (+ bias val)))
         ((= tag nl-ffi-loader--dt-init-arraysz) (setq init-arraysz val))
         ((= tag nl-ffi-loader--dt-pltrelsz) (setq pltrelsz val))
         ((= tag nl-ffi-loader--dt-pltrel) (setq pltrel val))
         ((= tag nl-ffi-loader--dt-jmprel) (setq jmprel (+ bias val)))
         ((= tag nl-ffi-loader--dt-gnu-hash) (setq gnu-hash (+ bias val)))
         ((= tag nl-ffi-loader--dt-rpath) (setq rpath-off val))
         ((= tag nl-ffi-loader--dt-runpath) (setq runpath-off val))
         ((and (>= tag nl-ffi-loader--dt-relr-lo) (<= tag nl-ffi-loader--dt-relr-hi))
          (setq relr-p t))))
      (setq i (1+ i)))
    (list :strtab strtab :symtab symtab :syment (or syment 24)
          :gnu-hash gnu-hash :sysv-hash sysv-hash
          :rela rela :relasz relasz :relaent (if (> relaent 0) relaent 24)
          :jmprel jmprel :pltrelsz pltrelsz :pltrel pltrel
          :needed (nreverse needed)
          :rpath-off rpath-off :runpath-off runpath-off
          :init dt-init :init-array init-array :init-arraysz init-arraysz
          :relr-p relr-p)))

;;;; --- symbol resolution: .gnu.hash / .hash / linear scan ---------------------

(defun nl-ffi-loader--gnu-hash (name)
  "The GNU hash function (uint32_t wraparound) over NAME's raw bytes."
  (let ((h 5381) (i 0) (n (length name)))
    (while (< i n)
      (setq h (logand (+ (* h 33) (aref name i)) #xffffffff))
      (setq i (1+ i)))
    h))

(defun nl-ffi-loader--streq-cstring (addr name)
  "Return non-nil when the NUL-terminated C string at ADDR equals NAME."
  (let ((i 0) (n (length name)) (ok t))
    (while (and ok (< i n))
      (if (= (ptr-read-u8 addr i) (aref name i)) (setq i (1+ i)) (setq ok nil)))
    (and ok (= (ptr-read-u8 addr n) 0))))

(defun nl-ffi-loader--symbol-name-matches-p (dyn symtab-index name)
  (let* ((sym (+ (plist-get dyn :symtab) (* symtab-index (plist-get dyn :syment))))
         (name-off (ptr-read-u32 sym 0)))
    (nl-ffi-loader--streq-cstring (+ (plist-get dyn :strtab) name-off) name)))

(defun nl-ffi-loader--u64-bit-set-p (addr bit)
  "Return non-nil when bit BIT (0-63) of the u64 at ADDR is set.
Reads only the relevant 32-bit half with `ptr-read-u32' and tests the
bit within it -- never combines both halves into one 64-bit Lisp value.
A GNU hash bloom word is an arbitrary bitmask (any of its 64 bits can be
set), which is exactly the case this file's Commentary documents
`ptr-read-u64' as unsafe for."
  (if (< bit 32)
      (/= (logand (ptr-read-u32 addr 0) (ash 1 bit)) 0)
    (/= (logand (ptr-read-u32 addr 4) (ash 1 (- bit 32))) 0)))

(defun nl-ffi-loader--lookup-gnu-hash (dyn name)
  "Resolve NAME via DYN's `.gnu.hash'.  Returns a symtab index, or nil."
  (let* ((base (plist-get dyn :gnu-hash))
         (nbuckets (ptr-read-u32 base 0))
         (symoffset (ptr-read-u32 base 4))
         (bloom-size (ptr-read-u32 base 8))
         (bloom-shift (ptr-read-u32 base 12))
         (bloom-base (+ base 16))
         (buckets-base (+ bloom-base (* bloom-size 8)))
         (chain-base (+ buckets-base (* nbuckets 4)))
         (h (nl-ffi-loader--gnu-hash name)))
    (when (and (> nbuckets 0) (> bloom-size 0))
      (let* ((word-index (mod (/ h 64) bloom-size))
             (word-addr (+ bloom-base (* word-index 8)))
             (bit1 (mod h 64))
             (bit2 (mod (ash h (- bloom-shift)) 64)))
        (when (and (nl-ffi-loader--u64-bit-set-p word-addr bit1)
                   (nl-ffi-loader--u64-bit-set-p word-addr bit2))
          (let ((sym-index (ptr-read-u32 buckets-base (* (mod h nbuckets) 4))))
            (when (> sym-index 0)
              (let ((found nil) (done nil))
                (while (not done)
                  (let ((chain-val (ptr-read-u32 chain-base (* (- sym-index symoffset) 4))))
                    (when (= (logior chain-val 1) (logior h 1))
                      (when (nl-ffi-loader--symbol-name-matches-p dyn sym-index name)
                        (setq found sym-index) (setq done t)))
                    (when (and (not done) (/= (logand chain-val 1) 0))
                      (setq done t))
                    (setq sym-index (1+ sym-index))))
                found))))))))

(defun nl-ffi-loader--elf-hash (name)
  "The classic SysV ELF hash function over NAME's raw bytes."
  (let ((h 0) (i 0) (n (length name)))
    (while (< i n)
      (setq h (logand (+ (ash h 4) (aref name i)) #xffffffff))
      (let ((g (logand h #xf0000000)))
        (when (/= g 0) (setq h (logxor h (ash g -24))))
        (setq h (logand h (lognot g))))
      (setq i (1+ i)))
    h))

(defun nl-ffi-loader--lookup-sysv-hash (dyn name)
  "Resolve NAME via DYN's classic `.hash'.  Returns a symtab index, or nil."
  (let* ((base (plist-get dyn :sysv-hash))
         (nbucket (ptr-read-u32 base 0))
         (buckets-base (+ base 8))
         (chain-base (+ buckets-base (* nbucket 4)))
         (i (ptr-read-u32 buckets-base (* (mod (nl-ffi-loader--elf-hash name) nbucket) 4))))
    (let ((found nil))
      (while (and (> i 0) (not found))
        (if (nl-ffi-loader--symbol-name-matches-p dyn i name)
            (setq found i)
          (setq i (ptr-read-u32 chain-base (* i 4)))))
      found)))

(defun nl-ffi-loader--lookup-linear-scan (dyn name)
  "Resolve NAME by scanning every `.dynsym' entry between `.symtab' and
`.strtab'.  Only reached when the object has neither `.gnu.hash' nor
`.hash' -- see this file's Commentary for why the symbol count this
derives is a fragile, last-resort guess rather than a real field."
  (let* ((syment (plist-get dyn :syment))
         (nsyms (max 0 (/ (- (plist-get dyn :strtab) (plist-get dyn :symtab)) syment)))
         (i 1) (found nil)) ; index 0 is always STN_UNDEF
    (while (and (< i nsyms) (not found))
      (when (nl-ffi-loader--symbol-name-matches-p dyn i name)
        (setq found i))
      (setq i (1+ i)))
    found))

(defun nl-ffi-loader--dynsym-name (dyn symtab-index)
  "Return the symbol name (a string) at SYMTAB-INDEX in DYN's `.dynsym'."
  (let* ((sym (+ (plist-get dyn :symtab) (* symtab-index (plist-get dyn :syment))))
         (name-off (ptr-read-u32 sym 0)))
    (nl-ffi-get-string (+ (plist-get dyn :strtab) name-off))))

(defun nl-ffi-loader--dynsym-entry (dyn symtab-index)
  "Return (VALUE SHNDX BIND TYPE) at SYMTAB-INDEX in DYN's `.dynsym'.
BIND/TYPE come from the single `st_info' byte at offset 4 (upper/lower 4
bits respectively) -- see this file's Commentary, \"A real defect found
while implementing\" (BIND, for STB_WEAK) and \"IFUNC\" (TYPE, for
STT_GNU_IFUNC)."
  (let* ((sym (+ (plist-get dyn :symtab) (* symtab-index (plist-get dyn :syment))))
         (st-value (ptr-read-u64 sym 8))
         (st-shndx (nl-ffi-loader--u16 sym 6))
         (st-info (ptr-read-u8 sym 4)))
    (list st-value st-shndx (ash st-info -4) (logand st-info #xf))))

(defun nl-ffi-loader--lookup-in-node (node name)
  "Resolve NAME within NODE's own `.dynsym' only -- no cross-object search.
Returns (VALUE BIND TYPE), all already biased/absolute where applicable,
for a DEFINED match, or nil."
  (let* ((dyn (plist-get node :dyn))
         (index
          (cond
           ((plist-get dyn :gnu-hash) (nl-ffi-loader--lookup-gnu-hash dyn name))
           ((plist-get dyn :sysv-hash) (nl-ffi-loader--lookup-sysv-hash dyn name))
           (t (nl-ffi-loader--lookup-linear-scan dyn name)))))
    (when index
      (let ((entry (nl-ffi-loader--dynsym-entry dyn index)))
        (when (/= (nth 1 entry) 0) ; SHN_UNDEF
          (list (+ (plist-get node :bias) (nth 0 entry)) (nth 2 entry) (nth 3 entry)))))))

(defun nl-ffi-loader--lookup-across-graph (graph search-order name)
  "Resolve NAME across every node in SEARCH-ORDER (paths into GRAPH, a
hash table path->node), first DEFINED match wins.  Returns (VALUE BIND
TYPE SOURCE-PATH), or nil.  See this file's Commentary, \"Symbol search
order\"."
  (let ((paths search-order) (found nil))
    (while (and paths (not found))
      (let* ((path (car paths))
             (node (gethash path graph))
             (hit (and node (nl-ffi-loader--lookup-in-node node name))))
        (when hit (setq found (append hit (list path)))))
      (setq paths (cdr paths)))
    found))

;;;; --- .dynstr string helpers -------------------------------------------------

(defun nl-ffi-loader--dynstr-string (dyn off)
  "Return the NUL-terminated string at DYN's `:strtab' + OFF."
  (nl-ffi-get-string (+ (plist-get dyn :strtab) off)))

(defun nl-ffi-loader--split-colon-dirs (s)
  "Split S on \":\" into a list of non-empty directory strings, each
ending in \"/\" (appended if missing, so callers can always
`concat' a bare filename directly)."
  (when (and s (> (length s) 0))
    (let (out (start 0) (n (length s)))
      (dotimes (i n)
        (when (= (aref s i) ?:)
          (when (> i start) (push (substring s start i) out))
          (setq start (1+ i))))
      (when (> n start) (push (substring s start n) out))
      (mapcar (lambda (d) (if (= (aref d (1- (length d))) ?/) d (concat d "/")))
              (nreverse out)))))

(defun nl-ffi-loader--ld-library-path-dirs ()
  "`LD_LIBRARY_PATH', split -- see this file's Commentary, \"SONAME
resolution\".  Confirmed unset on this repository's dev/CI host (2026-09-16);
read fresh via `getenv' every call rather than cached, since this is
cheap and a caller/test may set it for exactly this loader's benefit."
  (nl-ffi-loader--split-colon-dirs (getenv "LD_LIBRARY_PATH")))

(defun nl-ffi-loader--node-rpath-dirs (node)
  "NODE's own DT_RUNPATH dirs, or (only when DT_RUNPATH is absent)
DT_RPATH dirs -- see this file's Commentary, \"SONAME resolution\", for
why RUNPATH takes precedence and why no $ORIGIN-style token expansion is
done here."
  (let ((dyn (plist-get node :dyn)))
    (cond
     ((plist-get dyn :runpath-off)
      (nl-ffi-loader--split-colon-dirs
       (nl-ffi-loader--dynstr-string dyn (plist-get dyn :runpath-off))))
     ((plist-get dyn :rpath-off)
      (nl-ffi-loader--split-colon-dirs
       (nl-ffi-loader--dynstr-string dyn (plist-get dyn :rpath-off))))
     (t nil))))

(defun nl-ffi-loader--resolve-soname (soname requester-node)
  "Resolve SONAME to a real, openable path, or nil.
REQUESTER-NODE is the node whose DT_NEEDED names SONAME (its own
DT_RUNPATH/DT_RPATH is searched first) -- see this file's Commentary,
\"SONAME resolution\", for the full order and what is and is not
supported.  Never signals; the caller decides what an unresolved name
means (see `nl-ffi-loader--dependency-not-found-check')."
  (if (string-search "/" soname)
      (and (nl-ffi-loader--path-openable-p soname) soname)
    (let ((dirs (append (nl-ffi-loader--node-rpath-dirs requester-node)
                         (nl-ffi-loader--ld-library-path-dirs)
                         nl-ffi-loader--standard-dirs))
          (found nil))
      (while (and dirs (not found))
        (let ((candidate (concat (car dirs) soname)))
          (when (nl-ffi-loader--path-openable-p candidate)
            (setq found candidate)))
        (setq dirs (cdr dirs)))
      found)))

;;;; --- discovering the dependency graph ---------------------------------------

(defun nl-ffi-loader--map-node (path)
  "Map PATH (a real path, already resolved) and return a node plist:
`:path', `:bias', `:loads', `:dyn' -- everything through parsing
`.dynamic', but with NO relocation applied yet.  Signals
`nl-ffi-loader-open-failed' for an I/O/mapping failure, or
`nl-ffi-loader-unsupported' for `:not-elf64-shared-object',
`:no-load-segments', `:tls-segment' (checked here, before any segment is
even reserved), `:no-dynamic-section', `:relr-relocations', or
`:no-dynamic-symbols'."
  (let* ((fm (nl-ffi-loader--map-file-readonly-tracked path))
         (file-base (car fm)))
    (nl-ffi-loader--check-elf-header file-base path)
    (let* ((ph (nl-ffi-loader--program-headers file-base))
           (loads (plist-get ph :loads)))
      (unless loads
        (signal 'nl-ffi-loader-unsupported (list :no-load-segments path)))
      (when (plist-get ph :tls-p)
        (signal 'nl-ffi-loader-unsupported (list :tls-segment path)))
      (unless (plist-get ph :dyn-vaddr)
        (signal 'nl-ffi-loader-unsupported (list :no-dynamic-section path)))
      (let* ((reservation (nl-ffi-loader--reserve loads))
             (bias (car reservation)))
        (nl-ffi-loader--map-and-copy-segments bias file-base loads)
        (let ((dyn (nl-ffi-loader--parse-dynamic bias (plist-get ph :dyn-vaddr))))
          (when (plist-get dyn :relr-p)
            (signal 'nl-ffi-loader-unsupported (list :relr-relocations path)))
          (unless (or (plist-get dyn :symtab) (plist-get dyn :strtab))
            (signal 'nl-ffi-loader-unsupported (list :no-dynamic-symbols path)))
          (list :path path :bias bias :loads loads :dyn dyn))))))

(defun nl-ffi-loader--map-node-as-dependency (soname resolved-path requester-path)
  "Like `nl-ffi-loader--map-node', but any failure is wrapped as
`nl-ffi-loader-unsupported' reason `:dependency-unsupported' (or
`nl-ffi-loader-open-failed' reason `:dependency-open-failed'), naming
REQUESTER-PATH, SONAME, RESOLVED-PATH, and the inner condition/data --
see this file's Commentary, \"THE LIBC / SECOND-COPY QUESTION\", for why
this is how a dependency chain that reaches something like real glibc
ends up refused, several hops down, with the whole chain visible in the
signal data rather than losing it."
  (condition-case err
      (nl-ffi-loader--map-node resolved-path)
    (nl-ffi-loader-unsupported
     (signal 'nl-ffi-loader-unsupported
             (list :dependency-unsupported requester-path soname resolved-path
                   (car err) (cdr err))))
    (nl-ffi-loader-open-failed
     (signal 'nl-ffi-loader-open-failed
             (list :dependency-open-failed requester-path soname resolved-path
                   (car err) (cdr err))))))

(defun nl-ffi-loader--build-graph (root-path)
  "Discover ROOT-PATH's whole dependency graph, breadth-first.
Returns (GRAPH . ORDER) -- GRAPH a hash table path->node (see
`nl-ffi-loader--map-node'), ORDER the list of paths in breadth-first
discovery order, ROOT-PATH first.  Each `DT_NEEDED' name is resolved
against the REQUESTING node's own search path (see
`nl-ffi-loader--resolve-soname'); an unresolvable name signals
`nl-ffi-loader-unsupported' reason `:dependency-not-found' naming the
requester and the missing SONAME.  Any other failure mapping a
dependency (not the root -- ROOT-PATH's own failure is never wrapped, it
is signalled exactly as increment 1's single-object open always did) is
wrapped by `nl-ffi-loader--map-node-as-dependency', naming the whole
requester/soname/resolved-path chain.  A path already in GRAPH (by
string equality -- see this file's Commentary, \"Known
simplification\") is never mapped twice.  QUEUE entries are (PATH
. REQUESTER-SONAME-OR-NIL): a cons of the path to map and, for anything
but the root, (REQUESTER-PATH . SONAME) so a failure can be wrapped
correctly."
  (let ((graph (make-hash-table :test 'equal))
        (order nil)
        (queue (list (cons root-path nil))))
    (while queue
      (let* ((entry (car queue))
             (path (car entry))
             (requester (cdr entry)))
        (setq queue (cdr queue))
        (unless (gethash path graph)
          (let ((node (if requester
                           (nl-ffi-loader--map-node-as-dependency
                            (cdr requester) path (car requester))
                         (nl-ffi-loader--map-node path))))
            (puthash path node graph)
            (push path order)
            (dolist (off (plist-get (plist-get node :dyn) :needed))
              (let* ((soname (nl-ffi-loader--dynstr-string (plist-get node :dyn) off))
                     (resolved (nl-ffi-loader--resolve-soname soname node)))
                (unless resolved
                  (signal 'nl-ffi-loader-unsupported
                          (list :dependency-not-found path soname)))
                (unless (gethash resolved graph)
                  (setq queue (append queue (list (cons resolved (cons path soname))))))))))))
    (cons graph (nreverse order))))

;;;; --- dependency-first (postorder) traversal ---------------------------------

(defun nl-ffi-loader--postorder-visit (graph path visited result)
  "Depth-first postorder helper for `nl-ffi-loader--postorder'.
VISITED is a hash table, mutated in place.  RESULT is the accumulated
list so far (most-recently-emitted first); returns the new RESULT after
visiting PATH and everything PATH's own `DT_NEEDED' names (recursively)."
  (if (gethash path visited)
      result
    (puthash path t visited)
    (let ((node (gethash path graph))
          (r result))
      (when node
        (dolist (off (plist-get (plist-get node :dyn) :needed))
          (let* ((soname (nl-ffi-loader--dynstr-string (plist-get node :dyn) off))
                 (dep-path (nl-ffi-loader--resolve-soname soname node)))
            ;; Already resolved successfully during `nl-ffi-loader--build-
            ;; graph'; re-resolving here (rather than threading a second
            ;; data structure through just for this) is cheap -- a few
            ;; string compares and `openat'+`close' pairs against paths
            ;; already known to exist.
            (when dep-path
              (setq r (nl-ffi-loader--postorder-visit graph dep-path visited r)))))
        (setq r (cons path r)))
      r)))

(defun nl-ffi-loader--postorder (graph order root-path)
  "Dependency-first (postorder) traversal of GRAPH: every path's own
`DT_NEEDED' dependencies appear before it.  ROOT-PATH is visited first;
ORDER (GRAPH's own breadth-first discovery order) is walked afterward,
defensively -- by construction of `nl-ffi-loader--build-graph' every node
it discovers IS reachable from ROOT-PATH, so this never actually adds
anything ROOT-PATH's own traversal did not already reach."
  (let ((visited (make-hash-table :test 'equal))
        (result nil))
    (setq result (nl-ffi-loader--postorder-visit graph root-path visited result))
    (dolist (path order)
      (setq result (nl-ffi-loader--postorder-visit graph path visited result)))
    (nreverse result)))

;;;; --- relocations -------------------------------------------------------------

(defconst nl-ffi-loader--dt-pltrel-rela 7
  "DT_PLTREL's value when `.rela.plt' entries are Elf64_Rela (the only
form that exists on x86-64 -- Elf64_Rel, without an inline addend, is not
part of this ABI at all).")

(defun nl-ffi-loader--resolve-relocation-symbol (path bias dyn graph search-order r-sym)
  "Resolve dynsym index R-SYM for a GLOB_DAT/JUMP_SLOT relocation
belonging to the object at PATH (bias BIAS, own table DYN).  Returns the
resolved, already-biased runtime address (0 for a legitimately
unresolved `STB_WEAK' reference -- see this file's Commentary, \"A real
defect found while implementing\"), or signals
`nl-ffi-loader-unsupported' (`:undefined-symbol' when nothing anywhere
defines it and it is not weak, or `:ifunc-symbol-via-relocation' when
what WOULD answer it is `STT_GNU_IFUNC'-typed -- see \"IFUNC\").
GRAPH/SEARCH-ORDER may be nil (every increment-1-style single-object call
site, including this file's own smoke test's direct calls to
`nl-ffi-loader--apply-one-relocation', omits them): a local definition
still resolves; an undefined local symbol with no graph to search either
refuses outright (weak or not) or, if weak, resolves to 0 -- exactly
increment 1's own single-object behavior, now expressed as the graph=nil
case of this same function rather than a separate code path."
  (let* ((local (nl-ffi-loader--dynsym-entry dyn r-sym))
         (l-value (nth 0 local)) (l-shndx (nth 1 local))
         (l-bind (nth 2 local)) (l-type (nth 3 local)))
    (if (/= l-shndx 0) ; defined locally -- no name search needed at all
        (progn
          (when (= l-type nl-ffi-loader--stt-gnu-ifunc)
            (signal 'nl-ffi-loader-unsupported
                    (list :ifunc-symbol-via-relocation path
                          (nl-ffi-loader--dynsym-name dyn r-sym) path)))
          (+ bias l-value))
      (let* ((name (nl-ffi-loader--dynsym-name dyn r-sym))
             (hit (and graph search-order
                       (nl-ffi-loader--lookup-across-graph graph search-order name))))
        (cond
         (hit
          (when (= (nth 2 hit) nl-ffi-loader--stt-gnu-ifunc)
            (signal 'nl-ffi-loader-unsupported
                    (list :ifunc-symbol-via-relocation path name (nth 3 hit))))
          (nth 0 hit))
         ((= l-bind nl-ffi-loader--stb-weak) 0)
         (t
          (signal 'nl-ffi-loader-unsupported
                  (list :undefined-symbol path name))))))))

(defun nl-ffi-loader--apply-one-relocation (path bias dyn rela-addr &optional graph search-order)
  "Apply the single Elf64_Rela relocation at RELA-ADDR, belonging to the
object at PATH (bias BIAS, own table DYN).  GRAPH/SEARCH-ORDER (both
optional) enable cross-object symbol search for an otherwise-undefined
GLOB_DAT/JUMP_SLOT symbol -- see `nl-ffi-loader--resolve-relocation-
symbol' and this file's Commentary, \"Symbol search order\".  For
`R_X86_64_IRELATIVE' this CALLS the resolver (through `ptr-call', all six
argument slots zero) and writes its result -- ALWAYS, unconditionally;
deferring this relocation until after final segment protection is the
ORCHESTRATION's job (`nl-ffi-loader--apply-relocation-table'/
`nl-ffi-loader--apply-relocations-for-node'), not this function's -- see
this file's Commentary, \"IFUNC\".  Signals `nl-ffi-loader-unsupported'
for anything else -- see this file's Commentary."
  (let* ((r-offset (ptr-read-u64 rela-addr 0))
         (r-info (ptr-read-u64 rela-addr 8))
         (r-addend (ptr-read-u64 rela-addr 16))
         (r-type (logand r-info #xffffffff))
         (r-sym (ash r-info -32))
         (target (+ bias r-offset)))
    (cond
     ((= r-type nl-ffi-loader--reloc-relative)
      (ptr-write-u64 target 0 (+ bias r-addend)))
     ((or (= r-type nl-ffi-loader--reloc-glob-dat)
          (= r-type nl-ffi-loader--reloc-jump-slot))
      (let ((value (nl-ffi-loader--resolve-relocation-symbol
                    path bias dyn graph search-order r-sym)))
        (ptr-write-u64 target 0 (+ value r-addend))))
     ((= r-type nl-ffi-loader--reloc-irelative)
      (let* ((resolver-addr (+ bias r-addend))
             (result (ptr-call resolver-addr 0 0 0 0 0 0)))
        (ptr-write-u64 target 0 result)))
     ((memq r-type nl-ffi-loader--tls-relocation-types)
      (signal 'nl-ffi-loader-unsupported (list :tls-relocation path r-type)))
     (t
      (signal 'nl-ffi-loader-unsupported
              (list :relocation-type path r-type
                    (condition-case nil (nl-ffi-loader--dynsym-name dyn r-sym)
                      (error nil))))))))

(defun nl-ffi-loader--apply-relocation-table
    (path bias dyn addr size entsize graph search-order irelative-box)
  "Apply every NON-IRELATIVE relocation in the table at ADDR (SIZE bytes,
ENTSIZE each).  An `R_X86_64_IRELATIVE' entry's address is pushed onto
IRELATIVE-BOX (a one-element list used as a mutable box) instead of being
applied here -- see this file's Commentary, \"IFUNC\", and
`nl-ffi-loader--apply-relocations-for-node', which applies them in a
later pass."
  (when (and addr (> size 0))
    (let ((i 0) (n (/ size entsize)))
      (while (< i n)
        (let* ((rela-addr (+ addr (* i entsize)))
               (r-info (ptr-read-u64 rela-addr 8))
               (r-type (logand r-info #xffffffff)))
          (if (= r-type nl-ffi-loader--reloc-irelative)
              (setcar irelative-box (cons rela-addr (car irelative-box)))
            (nl-ffi-loader--apply-one-relocation
             path bias dyn rela-addr graph search-order)))
        (setq i (1+ i))))))

(defun nl-ffi-loader--apply-relocations-for-node (node graph search-order)
  "Apply every NON-IRELATIVE relocation (`.rela.dyn' and `.rela.plt', if
present -- both Elf64_Rela on x86-64; refuses `:rel-style-plt-not-rela'
if `DT_PLTREL' ever claims otherwise) for NODE, THEN protect NODE's
segments to their real final permissions, THEN apply every
`R_X86_64_IRELATIVE' relocation collected along the way -- see this
file's Commentary, \"IFUNC\", for why this exact order (protect, only
then irelative) is required."
  (let* ((path (plist-get node :path)) (bias (plist-get node :bias))
         (dyn (plist-get node :dyn)) (irelative-box (list nil)))
    (when (and (plist-get dyn :jmprel) (plist-get dyn :pltrel)
               (/= (plist-get dyn :pltrel) nl-ffi-loader--dt-pltrel-rela))
      (signal 'nl-ffi-loader-unsupported (list :rel-style-plt-not-rela path)))
    (nl-ffi-loader--apply-relocation-table
     path bias dyn (plist-get dyn :rela) (plist-get dyn :relasz) (plist-get dyn :relaent)
     graph search-order irelative-box)
    (nl-ffi-loader--apply-relocation-table
     path bias dyn (plist-get dyn :jmprel) (plist-get dyn :pltrelsz)
     24 ; .rela.plt entries are always Elf64_Rela (24 bytes) on x86-64.
     graph search-order irelative-box)
    (nl-ffi-loader--protect-segments bias (plist-get node :loads))
    (dolist (rela-addr (nreverse (car irelative-box)))
      (nl-ffi-loader--apply-one-relocation path bias dyn rela-addr graph search-order))))

;;;; --- initializers -------------------------------------------------------------

(defun nl-ffi-loader--run-initializers-for-node (node)
  "Run NODE's `DT_INIT' (if present) then `DT_INIT_ARRAY' entries, in
array order -- see this file's Commentary, \"Initializers\".  Each
`.init_array' slot is read directly, with NO extra bias added: for a
PIC/PIE object each slot is itself the target of its own
`R_X86_64_RELATIVE' relocation (confirmed against this package's own
`-ctor-dep.c'/`-init.c' fixtures with `readelf -r' before writing this
function), and this always runs strictly after every relocation for the
whole graph -- see `nl-ffi-loader--open-graph' -- so by the time this
reads a slot, it already holds the real, biased, directly-callable
address."
  (let ((dyn (plist-get node :dyn)))
    (when (plist-get dyn :init)
      (ptr-call (plist-get dyn :init) 0 0 0 0 0 0))
    (let ((arr (plist-get dyn :init-array)) (sz (plist-get dyn :init-arraysz)))
      (when (and arr (> sz 0))
        (let ((i 0) (n (/ sz 8)))
          (while (< i n)
            (let ((fn (ptr-read-u64 arr (* i 8))))
              (when (/= fn 0) (ptr-call fn 0 0 0 0 0 0)))
            (setq i (1+ i))))))))

;;;; --- public entry point ------------------------------------------------------

(defconst nl-ffi-loader--magic 'nl-ffi-loader)

(defun nl-ffi-loader-handle-p (handle)
  "Return non-nil when HANDLE was produced by `nl-ffi-loader-open'.
Used by nl-ffi.el (`nl-ffi--resolve-via-dlsym') to tell a loader object
apart from a real, positive-integer `dlopen' handle before deciding
whether to resolve a symbol through `nl-ffi-loader-symbol' or `dlsym'."
  (and (consp handle) (eq (plist-get handle :nl-ffi-loader-magic)
                           nl-ffi-loader--magic)))

(defun nl-ffi-loader--open-graph (path)
  "Discover, relocate, protect, and initialize PATH's whole dependency
graph.  Returns (GRAPH ORDER) on success.  On ANY failure anywhere in the
graph, unmaps every address-space reservation made so far (see this
file's Commentary, \"No half-loaded object left mapped\") and re-signals
unchanged -- nothing this loader could not fully load is ever left
mapped, whether the failure was in PATH itself or three dependencies
down."
  (condition-case err
      (let* ((built (nl-ffi-loader--build-graph path))
             (graph (car built)) (order (cdr built))
             (postorder (nl-ffi-loader--postorder graph order path)))
        (dolist (p postorder)
          (nl-ffi-loader--apply-relocations-for-node (gethash p graph) graph order))
        (dolist (p postorder)
          (nl-ffi-loader--run-initializers-for-node (gethash p graph)))
        (list graph order))
    (error
     (nl-ffi-loader--unmap-reservations)
     (signal (car err) (cdr err)))))

(defun nl-ffi-loader-open (path)
  "Map, relocate, protect, and initialize the shared object at PATH, and
every `DT_NEEDED' dependency it (transitively) names -- see this file's
Commentary for the full design, especially \"THE LIBC / SECOND-COPY
QUESTION\" and \"SONAME resolution\".

PATH itself is used exactly as given -- opened with
`openat(AT_FDCWD, PATH, O_RDONLY)', no search -- unlike a `DT_NEEDED'
name found while discovering PATH's own dependencies, which IS resolved
via `nl-ffi-loader--resolve-soname'.

Returns an opaque handle (`nl-ffi-loader-handle-p' recognizes it) that
`nl-ffi-loader-symbol'/`nl-ffi-loader-symbol-object' resolve names
against, searching PATH's own object first, then the rest of its
dependency graph -- see this file's Commentary, \"Symbol search order\".

Signals `nl-ffi-loader-open-failed' for an I/O/mapping failure in PATH
itself, and `nl-ffi-loader-unsupported' for anything out of this
increment's scope, whether in PATH itself (`:not-elf64-shared-object',
`:tls-segment', `:no-dynamic-section', `:no-load-segments',
`:relr-relocations', `:no-dynamic-symbols', `:overlapping-segments',
`:rel-style-plt-not-rela', `:undefined-symbol',
`:ifunc-symbol-via-relocation', `:tls-relocation', `:relocation-type')
or anywhere in its dependency graph (the same set, wrapped as
`:dependency-unsupported'/`:dependency-open-failed' naming the whole
requester/soname/resolved-path/inner-condition chain -- see
`nl-ffi-loader--map-node-as-dependency' -- or `:dependency-not-found'
when a `DT_NEEDED' name cannot be resolved to any real path at all)."
  (let ((nl-ffi-loader--file-mappings nil)
        (nl-ffi-loader--reservations nil))
    (unwind-protect
        (let* ((g (nl-ffi-loader--open-graph path))
               (graph (nth 0 g)) (order (nth 1 g))
               (root (gethash path graph)))
          (list :nl-ffi-loader-magic nl-ffi-loader--magic
                :path path :bias (plist-get root :bias)
                :dyn (plist-get root :dyn)
                :graph graph :search-order order))
      (nl-ffi-loader--unmap-file-mappings))))

(defun nl-ffi-loader-symbol (handle name)
  "Resolve NAME (a string) to its runtime address, searching HANDLE's own
object first, then the rest of its dependency graph -- see this file's
Commentary, \"Symbol search order\".  Returns the resolved address, or
0 -- the same \"not found\" sentinel a real `dlsym' would return, and
what `nl-ffi--resolve-via-dlsym' (nl-ffi.el) already expects -- never
nil.  An `STT_GNU_IFUNC'-typed match is treated as \"not found\" here
rather than returning the resolver's own `st_value' as if it were the
resolved implementation's address -- see this file's Commentary,
\"IFUNC\": unlike a relocation, a bare name lookup never calls anything,
so there is no safe point at which to run the resolver and use ITS
result instead."
  (let ((hit (nl-ffi-loader--lookup-across-graph
              (plist-get handle :graph) (plist-get handle :search-order) name)))
    (if (and hit (/= (nth 2 hit) nl-ffi-loader--stt-gnu-ifunc))
        (nth 0 hit)
      0)))

(defun nl-ffi-loader-symbol-object (handle name)
  "Return the PATH (within HANDLE's own dependency graph) whose
`.dynsym' actually defines NAME -- the same search
`nl-ffi-loader-symbol' performs, exposed so a caller (or a test) can
confirm WHICH object answers a given lookup -- see this file's
Commentary, \"Symbol search order\".  Returns nil when nothing in the
graph defines NAME (matching `nl-ffi-loader-symbol''s 0 sentinel, but
nil here since there is no address to report)."
  (let ((hit (nl-ffi-loader--lookup-across-graph
              (plist-get handle :graph) (plist-get handle :search-order) name)))
    (and hit (nth 3 hit))))

(provide 'nl-ffi-loader)

;;; nl-ffi-loader.el ends here
