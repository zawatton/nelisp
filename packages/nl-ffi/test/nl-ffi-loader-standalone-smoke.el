;;; nl-ffi-loader-standalone-smoke.el --- pure-elisp ELF loader gate -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Gate for `make ffi-loader'.  Unlike `ffi-dsl'/`standalone-reader-ffi-
;; smoke', this rebuilds and runs against the DEFAULT STATIC reader (no
;; `NELISP_READER_DYNAMIC') -- that is the whole point of FFI step 3
;; (packages/nl-ffi/src/nl-ffi-loader.el).  As of increment 2, this
;; covers dependency loading, initializers, and IFUNC too -- run
;; directly with (see `Makefile''s `ffi-loader' target for the real,
;; complete recipe, including the dependency-carrying fixtures' rpath):
;;
;;   cc -c -fPIC -O2 -o target/nl-ffi-loader-fixture-a.o \
;;     packages/nl-ffi/test/fixtures/nl-ffi-loader-fixture-a.c
;;   cc -c -fPIC -O2 -fno-plt -o target/nl-ffi-loader-fixture-b.o \
;;     packages/nl-ffi/test/fixtures/nl-ffi-loader-fixture-b.c
;;   cc -shared -fPIC -nostdlib -Wl,-z,nopack-relative-relocs \
;;     -o target/nl-ffi-loader-fixture.so \
;;     target/nl-ffi-loader-fixture-a.o target/nl-ffi-loader-fixture-b.o
;;   cc -shared -fPIC -o target/nl-ffi-loader-fixture-needs-dep.so \
;;     packages/nl-ffi/test/fixtures/nl-ffi-loader-fixture-needs-dep.c -lm
;;   cc -shared -fPIC -nostdlib \
;;     -o target/nl-ffi-loader-fixture-tls.so \
;;     packages/nl-ffi/test/fixtures/nl-ffi-loader-fixture-tls.c
;;   cc -shared -fPIC -nostdlib -ftls-model=initial-exec \
;;     -o target/nl-ffi-loader-fixture-tls-ie.so \
;;     packages/nl-ffi/test/fixtures/nl-ffi-loader-fixture-tls-ie.c
;;   cc -shared -fPIC -o target/nl-ffi-loader-fixture-init.so \
;;     packages/nl-ffi/test/fixtures/nl-ffi-loader-fixture-init.c
;;   make standalone-reader   # the DEFAULT static build, no dynamic flag
;;   ./target/nelisp --load packages/nl-ffi/test/nl-ffi-loader-standalone-smoke.el
;;
;; Covers, against the original fixture (RELATIVE + GLOB_DAT + JUMP_SLOT,
;; no DT_NEEDED/DT_INIT/PT_TLS -- see the fixture sources' own commentary):
;;   - `nl-ffi-loader-open' + `nl-ffi-loader-symbol' + `ptr-call' directly
;;     (the loader's own low-level API), for a JUMP_SLOT-resolved call, a
;;     GLOB_DAT-resolved call, and a RELATIVE-relocated data pointer read
;;     back and called.
;;   - The SAME two, through the full `ffi:library'/`ffi:defun' surface
;;     on this static reader -- proving `ffi:library' really falls back
;;     to this loader instead of signalling `nl-ffi-unavailable', and
;;     that `ffi:defun''s call shape and `nl-ffi-unresolved-symbol'
;;     contract are unchanged.
;; Covers, against `nl-ffi-loader-fixture-dep-leaf(2).so'/`-dep-root.so'
;; (increment 2, real `DT_NEEDED', see those fixtures' own commentary):
;;   - A dependency actually resolved (via this loader's `DT_RUNPATH'-
;;     based search, not a literal path) and called, through both the
;;     low-level API and the full `ffi:library'/`ffi:defun' surface.
;;   - The documented cross-object symbol search order, using two
;;     dependencies that export the SAME name with different behavior.
;;   - A `DT_NEEDED' name this loader cannot resolve to any real path
;;     (`nl-ffi-loader--resolve-soname' called directly).
;; Covers, against `nl-ffi-loader-fixture-needs-dep.so' (a real
;; `DT_NEEDED' on the SYSTEM `libm.so.6'):
;;   - Increment 2 genuinely ATTEMPTS this dependency (no more blanket
;;     `:needs-dependency' refusal); increment 2 itself refused two hops
;;     down, at `libc.so.6''s own `PT_TLS'.  RE-VERIFIED for increment 3
;;     (`PT_TLS' is no longer a blanket refusal): this now refuses ONE hop
;;     down, at `libm.so.6' ITSELF, via `DT_RELR' -- a specific, and (per
;;     nl-ffi-loader.el's Commentary, "TLS") EXPECTED reason, not the old
;;     blanket one, and not the reason increment 2 found either.
;; Covers, against `nl-ffi-loader-fixture-init.so' (ordinary CRT-supplied
;; `DT_INIT_ARRAY', including real weak-undefined `GLOB_DAT' relocations
;; -- see nl-ffi-loader.el's Commentary, "A real defect found while
;; implementing") and `nl-ffi-loader-fixture-ctor-(dep|root).so'
;; (increment 2, explicit constructors + a real dependency):
;;   - An object with initializers now opens and its ordinary functions
;;     are callable (no more blanket `:has-initializers' refusal).
;;   - An initializer's side effect is observable from Lisp after
;;     `nl-ffi-loader-open' returns, AND a dependency's initializer runs
;;     before its dependent's.
;; Covers, against `nl-ffi-loader-fixture-ifunc.so' (increment 2, a real
;; `R_X86_64_IRELATIVE'):
;;   - An IFUNC-resolved call returns the resolver's actual choice (not
;;     the resolver's own address treated as if it were the target).
;; Covers, against `nl-ffi-loader-fixture-tls-ie.so' (increment 3, a real
;; `PT_TLS' compiled with `-ftls-model=initial-exec' for a single
;; `R_X86_64_TPOFF64' relocation -- see nl-ffi-loader.el's Commentary,
;; "TLS"):
;;   - A thread-local read returns the compiled-in initial value.
;;   - A thread-local WRITE (through a setter, `%fs'-relative) is visible
;;     to a subsequent read -- proving this is real, addressable storage,
;;     not a lucky read of the read-only file image.
;;   - Two INDEPENDENT `nl-ffi-loader-open' calls against the SAME `.so'
;;     get two DISTINCT arena slots that do not collide.
;; Covers, against `nl-ffi-loader-fixture-tls.so' (a real PT_TLS segment,
;; UNCHANGED since increment 1, but no longer refused for having `PT_TLS'
;; at all -- its default TLS model, General-Dynamic, is what is now
;; refused):
;;   - `nl-ffi-loader-unsupported' reason `:tls-relocation' at
;;     `R_X86_64_DTPMOD64', the first relocation this loader actually
;;     applies for this object.
;; Covers, via hand-built in-memory Elf64_Rela entries and a fabricated
;; one-symbol `.dynsym'/`.dynstr' (no compiler needed for these -- they
;; exercise `nl-ffi-loader--apply-one-relocation' directly):
;;   - R_X86_64_RELATIVE applies correctly (positive case).
;;   - R_X86_64_GLOB_DAT applies correctly when the symbol is DEFINED
;;     (positive case).
;;   - R_X86_64_GLOB_DAT refuses when the symbol is UNDEFINED and
;;     STB_GLOBAL (reason `:undefined-symbol', naming it).
;;   - R_X86_64_GLOB_DAT resolves an UNDEFINED STB_WEAK symbol to 0
;;     instead of refusing (increment 2; see nl-ffi-loader.el's
;;     Commentary, "A real defect found while implementing").
;;   - A GLOB_DAT/JUMP_SLOT relocation whose resolved symbol is
;;     STT_GNU_IFUNC-typed refuses (reason
;;     `:ifunc-symbol-via-relocation') rather than using the resolver's
;;     own address as if it were the target -- see nl-ffi-loader.el's
;;     Commentary, "IFUNC", for why this is a DIFFERENT mechanism from
;;     R_X86_64_IRELATIVE and out of this increment's scope either way.
;;   - A GLOB_DAT/JUMP_SLOT relocation whose resolved symbol is
;;     STT_TLS-typed refuses (reason `:tls-symbol-via-relocation') rather
;;     than treating an in-module TLS byte offset as a `bias'-relative
;;     runtime address -- see nl-ffi-loader.el's Commentary, "TLS".
;;   - R_X86_64_TPOFF64 (increment 3) applies correctly for a same-module
;;     (r_sym=0) reference, computing the fake object's own `:tls-offset'
;;     plus the addend.
;;   - A still-refused TLS-classed relocation type (R_X86_64_GOTTPOFF)
;;     refuses (reason `:tls-relocation') even outside a PT_TLS object.
;;   - R_X86_64_64 (a real type, just not one of the supported ones)
;;     refuses (reason `:relocation-type').

;;; Code:

(defvar nl-ffi-loader-smoke--tests nil)

(defmacro nl-ffi-loader-smoke-deftest (name &rest body)
  `(setq nl-ffi-loader-smoke--tests
         (cons (cons ',name (lambda () ,@body)) nl-ffi-loader-smoke--tests)))

(defmacro nl-ffi-loader-smoke-should (form)
  `(let ((nl-ffi-loader-smoke--value ,form))
     (unless nl-ffi-loader-smoke--value
       (error "should failed: %S" ',form))
     nl-ffi-loader-smoke--value))

(defmacro nl-ffi-loader-smoke-should-error (form condition)
  `(let ((nl-ffi-loader-smoke--result
          (condition-case nl-ffi-loader-smoke--err
              (progn ,form 'nl-ffi-loader-smoke--no-error)
            (error nl-ffi-loader-smoke--err))))
     (cond
      ((eq nl-ffi-loader-smoke--result 'nl-ffi-loader-smoke--no-error)
       (error "should-error: no error signaled by %S" ',form))
      ((not (memq ,condition (get (car nl-ffi-loader-smoke--result) 'error-conditions)))
       (error "should-error: expected %S, got %S" ,condition nl-ffi-loader-smoke--result))
      (t nl-ffi-loader-smoke--result))))

(defmacro nl-ffi-loader-smoke-skip (fmt &rest args)
  `(progn
     (princ (format ,(concat "SKIP " fmt "\n") ,@args))
     (throw 'nl-ffi-loader-smoke-skip t)))

(load "packages/nl-ffi/src/nl-ffi.el")

(defconst nl-ffi-loader-smoke--fixture "target/nl-ffi-loader-fixture.so")
(defconst nl-ffi-loader-smoke--fixture-needs-dep
  "target/nl-ffi-loader-fixture-needs-dep.so")
(defconst nl-ffi-loader-smoke--fixture-tls "target/nl-ffi-loader-fixture-tls.so")
(defconst nl-ffi-loader-smoke--fixture-tls-ie "target/nl-ffi-loader-fixture-tls-ie.so")
(defconst nl-ffi-loader-smoke--fixture-init "target/nl-ffi-loader-fixture-init.so")
(defconst nl-ffi-loader-smoke--fixture-dep-root "target/nl-ffi-loader-fixture-dep-root.so")
(defconst nl-ffi-loader-smoke--fixture-ctor-root "target/nl-ffi-loader-fixture-ctor-root.so")
(defconst nl-ffi-loader-smoke--fixture-ifunc "target/nl-ffi-loader-fixture-ifunc.so")

;;;; --- low-level loader API, direct ------------------------------------------

(nl-ffi-loader-smoke-deftest ffi-loader-open-and-jump-slot-call
  (let* ((h (nl-ffi-loader-open nl-ffi-loader-smoke--fixture))
         (addr (nl-ffi-loader-symbol h "nl_ffi_loader_fixture_call_double")))
    (nl-ffi-loader-smoke-should (nl-ffi-loader-handle-p h))
    (nl-ffi-loader-smoke-should (integerp addr))
    (nl-ffi-loader-smoke-should (> addr 0))
    ;; nl_ffi_loader_fixture_call_double(x) = nl_ffi_loader_fixture_double(x) + 1
    ;; = 2x + 1 -- calls through an R_X86_64_JUMP_SLOT-resolved PLT stub.
    (nl-ffi-loader-smoke-should (= (ptr-call addr 20 0 0 0 0 0) 41))))

(nl-ffi-loader-smoke-deftest ffi-loader-glob-dat-call
  (let* ((h (nl-ffi-loader-open nl-ffi-loader-smoke--fixture))
         (addr (nl-ffi-loader-symbol h "nl_ffi_loader_fixture_call_triple")))
    (nl-ffi-loader-smoke-should (> addr 0))
    ;; = nl_ffi_loader_fixture_triple(x) + 2 = 3x + 2 -- the -fno-plt call
    ;; site, resolved via R_X86_64_GLOB_DAT rather than a PLT/JUMP_SLOT.
    (nl-ffi-loader-smoke-should (= (ptr-call addr 20 0 0 0 0 0) 62))))

(nl-ffi-loader-smoke-deftest ffi-loader-relative-data-pointer
  (let* ((h (nl-ffi-loader-open nl-ffi-loader-smoke--fixture))
         (ptr-slot (nl-ffi-loader-symbol h "nl_ffi_loader_fixture_local_ptr")))
    (nl-ffi-loader-smoke-should (> ptr-slot 0))
    ;; The slot itself is DATA (a function pointer variable); its value,
    ;; filled in by an R_X86_64_RELATIVE relocation at load time, is the
    ;; address of the `static' nl_ffi_loader_fixture_local -- read it back
    ;; and call THROUGH it.
    (let ((fn-addr (ptr-read-u64 ptr-slot 0)))
      (nl-ffi-loader-smoke-should (> fn-addr 0))
      (nl-ffi-loader-smoke-should (= (ptr-call fn-addr 20 0 0 0 0 0) 23)))))

(nl-ffi-loader-smoke-deftest ffi-loader-unknown-symbol-returns-zero
  (let ((h (nl-ffi-loader-open nl-ffi-loader-smoke--fixture)))
    ;; `nl-ffi-loader-symbol' itself never signals for a missing name --
    ;; it returns the same 0 sentinel `dlsym' would (see its docstring);
    ;; `ffi:defun''s `nl-ffi-unresolved-symbol' is the layer above this
    ;; one that turns 0 into a signal -- see the ffi:library/ffi:defun
    ;; test below.
    (nl-ffi-loader-smoke-should
     (= (nl-ffi-loader-symbol h "nl_ffi_loader_fixture_no_such_symbol") 0))))

(nl-ffi-loader-smoke-deftest ffi-loader-open-nonexistent-path
  (nl-ffi-loader-smoke-should-error
   (nl-ffi-loader-open "target/nl-ffi-loader-fixture-does-not-exist.so")
   'nl-ffi-loader-open-failed))

;;;; --- the real surface: ffi:library / ffi:defun, static reader --------------

(nl-ffi-loader-smoke-deftest ffi-loader-dsl-jump-slot
  (ffi:library nl-ffi-loader-smoke--fixture)
  (ffi:defun nl-ffi-loader-smoke-call-double "nl_ffi_loader_fixture_call_double"
    [:sint32 :sint32])
  (nl-ffi-loader-smoke-should (= (nl-ffi-loader-smoke-call-double 20) 41)))

(nl-ffi-loader-smoke-deftest ffi-loader-dsl-glob-dat
  (ffi:library nl-ffi-loader-smoke--fixture)
  (ffi:defun nl-ffi-loader-smoke-call-triple "nl_ffi_loader_fixture_call_triple"
    [:sint32 :sint32])
  (nl-ffi-loader-smoke-should (= (nl-ffi-loader-smoke-call-triple 20) 62)))

(nl-ffi-loader-smoke-deftest ffi-loader-dsl-unresolved-symbol
  (ffi:library nl-ffi-loader-smoke--fixture)
  (ffi:defun nl-ffi-loader-smoke-no-such-fn
    "nl_ffi_loader_fixture_no_such_symbol_via_dsl" [:sint32 :sint32])
  (nl-ffi-loader-smoke-should-error (nl-ffi-loader-smoke-no-such-fn 1)
                                     'nl-ffi-unresolved-symbol))

(nl-ffi-loader-smoke-deftest ffi-loader-dsl-handle-is-loader-not-dlopen
  ;; On the static reader `nl-ffi-library-handle' must be a loader object
  ;; (never a real, positive-integer `dlopen' handle -- there is no
  ;; working `dlopen' here at all).
  (ffi:library nl-ffi-loader-smoke--fixture)
  (nl-ffi-loader-smoke-should
   (nl-ffi-loader-handle-p (nl-ffi-library-handle nl-ffi-loader-smoke--fixture))))

;;;; --- increment 2: dependency loading (DT_NEEDED) ----------------------------

(nl-ffi-loader-smoke-deftest ffi-loader-dependency-resolved-and-called
  ;; One nl-ffi-loader-open call, reused below for the search-order
  ;; assertion too -- see this file's Commentary/report for why this
  ;; loader never unmaps a successfully loaded object (matching
  ;; increment 1's own policy) and why this smoke test avoids opening
  ;; the SAME dependency graph independently more times than each
  ;; distinct case needs.
  (let* ((h (nl-ffi-loader-open nl-ffi-loader-smoke--fixture-dep-root))
         (addr (nl-ffi-loader-symbol h "nl_ffi_loader_fixture_dep_root_call"))
         (source (nl-ffi-loader-symbol-object h "nl_ffi_loader_fixture_dep_leaf_value")))
    (nl-ffi-loader-smoke-should (> addr 0))
    ;; nl_ffi_loader_fixture_dep_root_call(x) calls, via a real DT_NEEDED
    ;; this loader resolved (through DT_RUNPATH, not a literal path) and
    ;; mapped, nl_ffi_loader_fixture_dep_leaf_value(x) = x + 1000, then
    ;; adds 1.  1021, not 2021 -- nl-ffi-loader-fixture-dep-leaf.so and
    ;; -dep-leaf2.so both export nl_ffi_loader_fixture_dep_leaf_value,
    ;; with different behavior; -dep-root.so's own DT_NEEDED names
    ;; leaf.so FIRST (see the Makefile) -- proving this loader's
    ;; documented cross-object search order picks it, not leaf2.so.  The
    ;; nl-ffi-loader-symbol-object check below confirms directly WHICH
    ;; object answered the lookup, rather than only inferring it from the
    ;; arithmetic result -- see nl-ffi-loader.el's Commentary, "Symbol
    ;; search order".
    (nl-ffi-loader-smoke-should (= (ptr-call addr 20 0 0 0 0 0) 1021))
    (nl-ffi-loader-smoke-should source)
    (nl-ffi-loader-smoke-should (string-search "nl-ffi-loader-fixture-dep-leaf.so" source))
    (nl-ffi-loader-smoke-should
     (not (string-search "nl-ffi-loader-fixture-dep-leaf2.so" source)))))

(nl-ffi-loader-smoke-deftest ffi-loader-dependency-dsl
  ;; The same dependency, through the full ffi:library/ffi:defun surface
  ;; -- proving dependency loading needs no change to either macro's call
  ;; shape, exactly as nl-ffi-loader.el's Commentary claims.
  (ffi:library nl-ffi-loader-smoke--fixture-dep-root)
  (ffi:defun nl-ffi-loader-smoke-dep-root-call
    "nl_ffi_loader_fixture_dep_root_call" [:sint32 :sint32])
  (nl-ffi-loader-smoke-should (= (nl-ffi-loader-smoke-dep-root-call 20) 1021)))

(nl-ffi-loader-smoke-deftest ffi-loader-dependency-not-found
  ;; A DT_NEEDED name this loader's search (DT_RUNPATH/DT_RPATH,
  ;; LD_LIBRARY_PATH, the standard directory list) cannot resolve to any
  ;; real, openable path -- exercised directly against the resolver
  ;; rather than needing a fixture with a deliberately bogus DT_NEEDED
  ;; entry (which the static LINKER itself would refuse to produce).
  (nl-ffi-loader-smoke-should
   (not (nl-ffi-loader--resolve-soname
         "libtotally-bogus-nl-ffi-fixture-xyz.so.1" nil))))

(nl-ffi-loader-smoke-deftest ffi-loader-needs-dependency-reaches-relr
  ;; nl-ffi-loader-fixture-needs-dep.so has a real DT_NEEDED on the SYSTEM
  ;; libm.so.6.  RE-VERIFIED for increment 3 (this used to refuse two hops
  ;; down, at libc.so.6's PT_TLS -- see nl-ffi-loader.el's Commentary,
  ;; "TLS", "THE REAL-SYSTEM-LIBRARY REACHABILITY QUESTION" for the full
  ;; story): `PT_TLS' is no longer a blanket refusal, and real libc.so.6
  ;; turns out to use ONLY the Initial-Exec model internally -- but
  ;; widening what this loader reaches also exposed a gap in the EXISTING
  ;; `DT_RELR' check (it matched only the old, pre-standardization tag
  ;; range, not the standardized one this host's real toolchain emits --
  ;; see the same Commentary section), now fixed.  With the fix in place,
  ;; this refuses ONE hop down, at libm.so.6 ITSELF (`DT_RELR' present,
  ;; confirmed with `readelf -d'), never reaching libc.so.6 or its TLS at
  ;; all.
  (let ((sig (nl-ffi-loader-smoke-should-error
              (nl-ffi-loader-open nl-ffi-loader-smoke--fixture-needs-dep)
              'nl-ffi-loader-unsupported)))
    ;; DATA: (:dependency-unsupported REQUESTER-PATH SONAME RESOLVED-PATH
    ;;        INNER-CONDITION-SYMBOL INNER-DATA) -- see
    ;;        `nl-ffi-loader--map-node-as-dependency'.
    (nl-ffi-loader-smoke-should (eq (nth 0 (cdr sig)) :dependency-unsupported))
    (nl-ffi-loader-smoke-should (equal (nth 2 (cdr sig)) "libm.so.6"))
    (nl-ffi-loader-smoke-should (eq (nth 4 (cdr sig)) 'nl-ffi-loader-unsupported))
    (let ((inner-data (nth 5 (cdr sig))))
      (nl-ffi-loader-smoke-should (eq (nth 0 inner-data) :relr-relocations)))))

;;;; --- increment 2: initializers (DT_INIT/DT_INIT_ARRAY) ----------------------

(nl-ffi-loader-smoke-deftest ffi-loader-initializers-run-and-callable
  ;; nl-ffi-loader-fixture-init.so has no DT_NEEDED (--as-needed dropped
  ;; the unused implicit libc dependency) but DOES carry real
  ;; DT_INIT_ARRAY entries from crtbeginS.o, AND four weak-undefined
  ;; GLOB_DAT relocations (__cxa_finalize and friends) that only resolve
  ;; to 0 because they are STB_WEAK -- see nl-ffi-loader.el's Commentary,
  ;; "A real defect found while implementing".  No more blanket
  ;; :has-initializers refusal: this now opens and its ordinary function
  ;; is callable.
  (let* ((h (nl-ffi-loader-open nl-ffi-loader-smoke--fixture-init))
         (addr (nl-ffi-loader-symbol h "nl_ffi_loader_fixture_init_probe")))
    (nl-ffi-loader-smoke-should (> addr 0))
    (nl-ffi-loader-smoke-should (= (ptr-call addr 20 0 0 0 0 0) 21))))

(nl-ffi-loader-smoke-deftest ffi-loader-initializer-side-effect-and-dependency-order
  ;; nl-ffi-loader-fixture-ctor-dep.so's constructor sets its own exported
  ;; global to 1; nl-ffi-loader-fixture-ctor-root.so (which depends on it)
  ;; has its OWN constructor that sets the SAME global (resolved via a
  ;; real GLOB_DAT relocation, since it is undefined in ctor-root.so
  ;; itself) to 2 only if it already saw 1 -- so reading back 2 after
  ;; nl-ffi-loader-open returns proves BOTH that an initializer's side
  ;; effect is observable from Lisp, and that the dependency's
  ;; initializer ran before the dependent's.
  (let* ((h (nl-ffi-loader-open nl-ffi-loader-smoke--fixture-ctor-root))
         (addr (nl-ffi-loader-symbol h "nl_ffi_loader_fixture_ctor_order")))
    (nl-ffi-loader-smoke-should (> addr 0))
    (nl-ffi-loader-smoke-should (= (ptr-read-u32 addr 0) 2))))

;;;; --- increment 2: IFUNC (R_X86_64_IRELATIVE) --------------------------------

(nl-ffi-loader-smoke-deftest ffi-loader-ifunc-call-uses-resolvers-result
  ;; nl-ffi-loader-fixture-ifunc.so's resolver runs real CPUID and always
  ;; picks the SSE2 implementation (x+2) on any x86-64 host.
  ;; nl_ffi_loader_fixture_call_ifunc calls the ifunc'd function through
  ;; an ordinary relocated call site and adds 10 -- 20+2+10 = 32.  A
  ;; loader that mishandled R_X86_64_IRELATIVE by treating the addend as
  ;; a plain target address (calling the RESOLVER as if it were the
  ;; two-argument function) would return a huge, obviously-wrong pointer
  ;; value here instead.
  (let* ((h (nl-ffi-loader-open nl-ffi-loader-smoke--fixture-ifunc))
         (addr (nl-ffi-loader-symbol h "nl_ffi_loader_fixture_call_ifunc")))
    (nl-ffi-loader-smoke-should (> addr 0))
    (nl-ffi-loader-smoke-should (= (ptr-call addr 20 0 0 0 0 0) 32))))

;;;; --- increment 3: TLS (R_X86_64_TPOFF64, Initial-Exec) ----------------------

(nl-ffi-loader-smoke-deftest ffi-loader-tls-initial-exec-read
  (let* ((h (nl-ffi-loader-open nl-ffi-loader-smoke--fixture-tls-ie))
         (get (nl-ffi-loader-symbol h "nl_ffi_loader_fixture_tls_ie_get")))
    (nl-ffi-loader-smoke-should (> get 0))
    ;; The compiled-in initial value, read through a real R_X86_64_TPOFF64
    ;; relocation and `%fs' this loader itself established -- see
    ;; nl-ffi-loader.el's Commentary, "TLS".
    (nl-ffi-loader-smoke-should (= (ptr-call get 0 0 0 0 0 0) 7))))

(nl-ffi-loader-smoke-deftest ffi-loader-tls-initial-exec-write-then-read
  (let* ((h (nl-ffi-loader-open nl-ffi-loader-smoke--fixture-tls-ie))
         (get (nl-ffi-loader-symbol h "nl_ffi_loader_fixture_tls_ie_get"))
         (set (nl-ffi-loader-symbol h "nl_ffi_loader_fixture_tls_ie_set")))
    ;; A WRITE through the setter must be visible to a subsequent read --
    ;; proving this is real, addressable per-object storage this loader
    ;; carved out of its own arena, not a lucky read of the read-only file
    ;; mapping (which would never change).
    (ptr-call set 99 0 0 0 0 0)
    (nl-ffi-loader-smoke-should (= (ptr-call get 0 0 0 0 0 0) 99))))

(nl-ffi-loader-smoke-deftest ffi-loader-tls-two-opens-do-not-collide
  ;; A SECOND, independent `nl-ffi-loader-open' of the SAME .so -- see
  ;; nl-ffi-loader.el's Commentary, "Known simplification": this loader
  ;; deduplicates only WITHIN one open() call's own graph, so two separate
  ;; calls really do map (and TLS-assign) two distinct nodes.  Writing
  ;; through one must not be visible through the other -- proving the
  ;; persistent, process-wide arena hands out DISTINCT, non-overlapping
  ;; offsets across independent `nl-ffi-loader-open' calls, not just
  ;; within a single one.
  (let* ((h1 (nl-ffi-loader-open nl-ffi-loader-smoke--fixture-tls-ie))
         (h2 (nl-ffi-loader-open nl-ffi-loader-smoke--fixture-tls-ie))
         (get1 (nl-ffi-loader-symbol h1 "nl_ffi_loader_fixture_tls_ie_get"))
         (set1 (nl-ffi-loader-symbol h1 "nl_ffi_loader_fixture_tls_ie_set"))
         (get2 (nl-ffi-loader-symbol h2 "nl_ffi_loader_fixture_tls_ie_get")))
    (nl-ffi-loader-smoke-should (= (ptr-call get2 0 0 0 0 0 0) 7))
    (ptr-call set1 123 0 0 0 0 0)
    (nl-ffi-loader-smoke-should (= (ptr-call get1 0 0 0 0 0 0) 123))
    (nl-ffi-loader-smoke-should (= (ptr-call get2 0 0 0 0 0 0) 7))))

(nl-ffi-loader-smoke-deftest ffi-loader-refuses-general-dynamic-tls
  ;; nl-ffi-loader-fixture-tls.so, UNCHANGED since increment 1/2: compiled
  ;; with NO `-ftls-model' flag, GCC's default for `-fPIC -shared' code,
  ;; it lowers to the General-Dynamic model (a `R_X86_64_DTPMOD64'
  ;; relocation plus a JUMP_SLOT call to `__tls_get_addr' -- confirmed
  ;; with `readelf -r').  `PT_TLS' itself is NO LONGER a blanket refusal
  ;; (see the Initial-Exec fixture's own tests above), so this now refuses
  ;; at the FIRST relocation this loader actually applies for the object:
  ;; `.rela.dyn' runs before `.rela.plt' (`nl-ffi-loader--apply-
  ;; relocations-for-node'), and DTPMOD64 is this object's only
  ;; `.rela.dyn' entry -- so the refusal fires there, never reaching the
  ;; JUMP_SLOT/`__tls_get_addr' entry at all.
  (let ((sig (nl-ffi-loader-smoke-should-error
              (nl-ffi-loader-open nl-ffi-loader-smoke--fixture-tls)
              'nl-ffi-loader-unsupported)))
    (nl-ffi-loader-smoke-should (eq (nth 0 (cdr sig)) :tls-relocation))
    (nl-ffi-loader-smoke-should (= (nth 2 (cdr sig)) 16)))) ; R_X86_64_DTPMOD64

;;;; --- relocation-type dispatch: synthetic, no compiler needed ----------------
;;
;; Builds a fake one-symbol .dynsym/.dynstr and a fake Elf64_Rela entry by
;; hand (alloc-bytes/ptr-write-u64/ptr-write-u8), then calls
;; `nl-ffi-loader--apply-one-relocation' directly -- exercises the
;; relocation-type dispatch, the undefined-symbol check, the weak-
;; undefined-resolves-to-zero check, and the ifunc-typed-symbol check in
;; isolation, without needing a real object for each one.

(defun nl-ffi-loader-smoke--fake-dyn (name shndx value &optional bind type)
  "A DYN plist with one `.dynsym' entry (index 0) named NAME.
BIND/TYPE (both default 0 -- STB_LOCAL/STT_NOTYPE when omitted) set the
ST_INFO byte increment 2's resolver reads: ST_BIND (for STB_WEAK, see
nl-ffi-loader.el's Commentary, \"A real defect found while
implementing\") and ST_TYPE (for STT_GNU_IFUNC, see \"IFUNC\")."
  (let* ((strtab (alloc-bytes (+ 2 (length name)) 1))
         (sym (alloc-bytes 24 8))
         (i 0)
         (st-info (logior (ash (or bind 0) 4) (or type 0))))
    (ptr-write-u8 strtab 0 0) ; offset 0: STN_UNDEF's own empty name
    (while (< i (length name))
      (ptr-write-u8 strtab (+ 1 i) (aref name i))
      (setq i (1+ i)))
    (ptr-write-u8 strtab (+ 1 (length name)) 0)
    (ptr-write-u64 sym 0 (logior 1 (ash st-info 32) (ash shndx 48))) ; st_name=1, st_info, st_shndx
    (ptr-write-u64 sym 8 value)                     ; st_value
    (ptr-write-u64 sym 16 0)                        ; st_size
    (list :symtab sym :strtab strtab :syment 24 :gnu-hash nil :sysv-hash nil)))

(defun nl-ffi-loader-smoke--fake-rela (r-offset r-sym r-type r-addend)
  (let ((rela (alloc-bytes 24 8)))
    (ptr-write-u64 rela 0 r-offset)
    (ptr-write-u64 rela 8 (logior (ash r-sym 32) r-type))
    (ptr-write-u64 rela 16 r-addend)
    rela))

(defun nl-ffi-loader-smoke--fake-dyn-tls (name shndx value &optional bind type)
  "Like `nl-ffi-loader-smoke--fake-dyn', but places the one real symbol at
dynsym INDEX 1 of a two-entry table (index 0 explicitly zeroed, matching
the real, always-undefined STN_UNDEF convention) instead of index 0.
Needed for an `R_X86_64_TPOFF64' test that must use a NONZERO r_sym: 0
means \"local, same-module, use the addend directly\" for TPOFF64 -- see
`nl-ffi-loader--resolve-tpoff64' -- so it cannot double as \"look up
dynsym index 0\" the way the ordinary GLOB_DAT/JUMP_SLOT tests above use
it via `nl-ffi-loader-smoke--fake-dyn'."
  (let* ((strtab (alloc-bytes (+ 2 (length name)) 1))
         (symtab (alloc-bytes 48 8)) ; two 24-byte Elf64_Sym entries
         (sym (+ symtab 24))
         (i 0)
         (st-info (logior (ash (or bind 0) 4) (or type 0))))
    (ptr-write-u8 strtab 0 0)
    (while (< i (length name))
      (ptr-write-u8 strtab (+ 1 i) (aref name i))
      (setq i (1+ i)))
    (ptr-write-u8 strtab (+ 1 (length name)) 0)
    (ptr-write-u64 symtab 0 0) (ptr-write-u64 symtab 8 0) (ptr-write-u64 symtab 16 0)
    (ptr-write-u64 sym 0 (logior 1 (ash st-info 32) (ash shndx 48)))
    (ptr-write-u64 sym 8 value)
    (ptr-write-u64 sym 16 0)
    (list :symtab symtab :strtab strtab :syment 24 :gnu-hash nil :sysv-hash nil)))

(nl-ffi-loader-smoke-deftest ffi-loader-relative-relocation-applies
  (let* ((target (alloc-bytes 8 8))
         (bias 1000000)
         (rela (nl-ffi-loader-smoke--fake-rela (- target bias) 0 8 555)))
    (nl-ffi-loader--apply-one-relocation "synthetic" bias nil rela)
    (nl-ffi-loader-smoke-should (= (ptr-read-u64 target 0) (+ bias 555)))))

(nl-ffi-loader-smoke-deftest ffi-loader-glob-dat-applies-for-defined-symbol
  (let* ((dyn (nl-ffi-loader-smoke--fake-dyn "defined_sym" 8 4096))
         (target (alloc-bytes 8 8))
         (rela (nl-ffi-loader-smoke--fake-rela target 0 6 0)))
    (nl-ffi-loader--apply-one-relocation "synthetic" 0 dyn rela)
    (nl-ffi-loader-smoke-should (= (ptr-read-u64 target 0) 4096))))

(nl-ffi-loader-smoke-deftest ffi-loader-glob-dat-refuses-undefined-symbol
  (let* ((dyn (nl-ffi-loader-smoke--fake-dyn "undefined_sym" 0 0))
         (target (alloc-bytes 8 8))
         (rela (nl-ffi-loader-smoke--fake-rela target 0 6 0))
         (sig (nl-ffi-loader-smoke-should-error
               (nl-ffi-loader--apply-one-relocation "synthetic" 0 dyn rela)
               'nl-ffi-loader-unsupported)))
    (nl-ffi-loader-smoke-should (eq (nth 0 (cdr sig)) :undefined-symbol))
    (nl-ffi-loader-smoke-should (equal (nth 2 (cdr sig)) "undefined_sym"))))

(nl-ffi-loader-smoke-deftest ffi-loader-glob-dat-weak-undefined-resolves-to-zero
  (let* ((dyn (nl-ffi-loader-smoke--fake-dyn "weak_undefined_sym" 0 0 2)) ; STB_WEAK
         (target (alloc-bytes 8 8)))
    (ptr-write-u64 target 0 999) ; a poison value: must become 0, not stay 999
    (let ((rela (nl-ffi-loader-smoke--fake-rela target 0 6 0)))
      (nl-ffi-loader--apply-one-relocation "synthetic" 0 dyn rela)
      (nl-ffi-loader-smoke-should (= (ptr-read-u64 target 0) 0)))))

(nl-ffi-loader-smoke-deftest ffi-loader-refuses-ifunc-symbol-via-relocation
  ;; A GLOB_DAT/JUMP_SLOT relocation whose resolved (here: local) symbol
  ;; is STT_GNU_IFUNC-typed -- the shape a DEFAULT-visibility ifunc
  ;; compiles to (confirmed empirically with readelf -r/-s before writing
  ;; nl-ffi-loader-fixture-ifunc.c's HIDDEN-visibility workaround; see
  ;; nl-ffi-loader.el's Commentary, "IFUNC").  Must refuse, not silently
  ;; use the resolver's own address as if it were the target.
  (let* ((dyn (nl-ffi-loader-smoke--fake-dyn "ifunc_sym" 8 4096 0 10)) ; STT_GNU_IFUNC = 10
         (target (alloc-bytes 8 8))
         (rela (nl-ffi-loader-smoke--fake-rela target 0 7 0)) ; JUMP_SLOT
         (sig (nl-ffi-loader-smoke-should-error
               (nl-ffi-loader--apply-one-relocation "synthetic" 0 dyn rela)
               'nl-ffi-loader-unsupported)))
    (nl-ffi-loader-smoke-should (eq (nth 0 (cdr sig)) :ifunc-symbol-via-relocation))
    (nl-ffi-loader-smoke-should (equal (nth 2 (cdr sig)) "ifunc_sym"))))

(nl-ffi-loader-smoke-deftest ffi-loader-refuses-tls-symbol-via-relocation
  ;; A GLOB_DAT/JUMP_SLOT relocation whose resolved (here: local) symbol
  ;; is STT_TLS-typed -- mirrors the IFUNC test above for a different
  ;; symbol type: must refuse, not silently treat an in-module TLS byte
  ;; offset as a `bias'-relative runtime address -- see nl-ffi-loader.el's
  ;; Commentary, "TLS".
  (let* ((dyn (nl-ffi-loader-smoke--fake-dyn "tls_sym" 8 4 0 6)) ; STT_TLS = 6
         (target (alloc-bytes 8 8))
         (rela (nl-ffi-loader-smoke--fake-rela target 0 7 0)) ; JUMP_SLOT
         (sig (nl-ffi-loader-smoke-should-error
               (nl-ffi-loader--apply-one-relocation "synthetic" 0 dyn rela)
               'nl-ffi-loader-unsupported)))
    (nl-ffi-loader-smoke-should (eq (nth 0 (cdr sig)) :tls-symbol-via-relocation))
    (nl-ffi-loader-smoke-should (equal (nth 2 (cdr sig)) "tls_sym"))))

(nl-ffi-loader-smoke-deftest ffi-loader-tpoff64-applies-for-local-reference
  ;; r_sym=0 -- the shape this package's own Initial-Exec fixture compiles
  ;; to for a same-module reference (confirmed by parsing the raw
  ;; Elf64_Rela bytes -- see the report).  The addend directly carries the
  ;; in-module byte offset; the result must be the object's OWN
  ;; :tls-offset plus that addend.
  (let* ((dyn (plist-put (nl-ffi-loader-smoke--fake-dyn "irrelevant" 0 0) :tls-offset -64))
         (target (alloc-bytes 8 8))
         (rela (nl-ffi-loader-smoke--fake-rela target 0 18 12))) ; TPOFF64, r_sym=0, addend=12
    (nl-ffi-loader--apply-one-relocation "synthetic" 0 dyn rela)
    (nl-ffi-loader-smoke-should (= (ptr-read-u64 target 0) -52)))) ; -64 + 12

(nl-ffi-loader-smoke-deftest ffi-loader-tpoff64-applies-for-named-local-symbol
  ;; r_sym<>0, defined in the SAME object -- the shape real `libc.so.6'
  ;; ITSELF uses for one of its own 16 TPOFF64 relocations (r_sym=1527,
  ;; confirmed by parsing the raw Elf64_Rela bytes -- see the report),
  ;; even though no FIXTURE this package ships needs it.
  (let* ((dyn (plist-put (nl-ffi-loader-smoke--fake-dyn-tls "named_tls" 8 20 0 6) ; STT_TLS
                          :tls-offset -100))
         (target (alloc-bytes 8 8))
         (rela (nl-ffi-loader-smoke--fake-rela target 1 18 0))) ; TPOFF64, r_sym=1
    (nl-ffi-loader--apply-one-relocation "synthetic" 0 dyn rela)
    (nl-ffi-loader-smoke-should (= (ptr-read-u64 target 0) -80)))) ; -100 + 20 + 0

(nl-ffi-loader-smoke-deftest ffi-loader-refuses-tls-symbol-type-mismatch
  ;; r_sym<>0, defined in the SAME object, but NOT STT_TLS-typed -- must
  ;; refuse rather than silently treat an ordinary symbol's `st_value' as
  ;; a TLS byte offset.
  (let* ((dyn (plist-put (nl-ffi-loader-smoke--fake-dyn-tls "not_tls" 8 20) ; type 0
                          :tls-offset -100))
         (target (alloc-bytes 8 8))
         (rela (nl-ffi-loader-smoke--fake-rela target 1 18 0))
         (sig (nl-ffi-loader-smoke-should-error
               (nl-ffi-loader--apply-one-relocation "synthetic" 0 dyn rela)
               'nl-ffi-loader-unsupported)))
    (nl-ffi-loader-smoke-should (eq (nth 0 (cdr sig)) :tls-symbol-type-mismatch))
    (nl-ffi-loader-smoke-should (equal (nth 2 (cdr sig)) "not_tls"))))

(nl-ffi-loader-smoke-deftest ffi-loader-refuses-undefined-tls-symbol
  ;; r_sym<>0, UNDEFINED locally, and no graph to search -- must refuse
  ;; rather than silently resolve to 0 the way the ordinary GLOB_DAT/
  ;; JUMP_SLOT resolver does for a WEAK reference (this file's Commentary,
  ;; "TLS", explains why that carve-out is not extended here).
  (let* ((dyn (plist-put (nl-ffi-loader-smoke--fake-dyn-tls "missing_tls" 0 0)
                          :tls-offset -8))
         (target (alloc-bytes 8 8))
         (rela (nl-ffi-loader-smoke--fake-rela target 1 18 0))
         (sig (nl-ffi-loader-smoke-should-error
               (nl-ffi-loader--apply-one-relocation "synthetic" 0 dyn rela)
               'nl-ffi-loader-unsupported)))
    (nl-ffi-loader-smoke-should (eq (nth 0 (cdr sig)) :undefined-tls-symbol))
    (nl-ffi-loader-smoke-should (equal (nth 2 (cdr sig)) "missing_tls"))))

(nl-ffi-loader-smoke-deftest ffi-loader-refuses-tls-relocation-type
  ;; R_X86_64_GOTTPOFF -- the GOT-indirect Initial-Exec cousin of TPOFF64
  ;; a real `-fPIC' PIC-style reference would actually use (this file's
  ;; own fixture needed `-ftls-model=initial-exec' to get TPOFF64 instead
  ;; -- see nl-ffi-loader.el's Commentary, "TLS") -- still refused by
  ;; name, unlike TPOFF64 itself (moved out of this refused-types list in
  ;; increment 3; see the positive TPOFF64 tests above).
  (let* ((target (alloc-bytes 8 8))
         (rela (nl-ffi-loader-smoke--fake-rela target 0 22 0)) ; R_X86_64_GOTTPOFF
         (sig (nl-ffi-loader-smoke-should-error
               (nl-ffi-loader--apply-one-relocation "synthetic" 0 nil rela)
               'nl-ffi-loader-unsupported)))
    (nl-ffi-loader-smoke-should (eq (nth 0 (cdr sig)) :tls-relocation))))

(nl-ffi-loader-smoke-deftest ffi-loader-refuses-plain-unsupported-type
  (let* ((dyn (nl-ffi-loader-smoke--fake-dyn "some_sym" 8 4096))
         (target (alloc-bytes 8 8))
         (rela (nl-ffi-loader-smoke--fake-rela target 0 1 0)) ; R_X86_64_64
         (sig (nl-ffi-loader-smoke-should-error
               (nl-ffi-loader--apply-one-relocation "synthetic" 0 dyn rela)
               'nl-ffi-loader-unsupported)))
    (nl-ffi-loader-smoke-should (eq (nth 0 (cdr sig)) :relocation-type))
    (nl-ffi-loader-smoke-should (= (nth 2 (cdr sig)) 1))))

;;;; --- run ------------------------------------------------------------------

(let ((tests (reverse nl-ffi-loader-smoke--tests))
      (ran 0)
      (skipped 0)
      (failures nil))
  (while tests
    (let* ((test (car tests))
           (skip
            (catch 'nl-ffi-loader-smoke-skip
              (condition-case err
                  (progn
                    (funcall (cdr test))
                    (setq ran (1+ ran)))
                (error
                 (setq failures
                       (cons (format "%s: %S" (car test) err) failures))))
              nil)))
      (when skip
        (setq skipped (1+ skipped))))
    (setq tests (cdr tests)))
  (princ (format "GATE-COUNT checked=%d findings=%d skipped=%d\n"
                  ran (length failures) skipped))
  (when failures
    (let ((all failures))
      (while all
        (princ (format "FAIL %s\n" (car all)))
        (setq all (cdr all))))
    (error "nl-ffi-loader-standalone-smoke: %d failure(s), %d passed, %d skipped"
           (length failures) ran skipped))
  (when (< (+ ran skipped) 30)
    (error "nl-ffi-loader-standalone-smoke: only %d test(s) ran + %d skipped (expected >= 30 total)"
           ran skipped))
  (princ (format "nl-ffi-loader-standalone-smoke: PASS (%d tests, %d skipped)\n"
                  ran skipped)))

;;; nl-ffi-loader-standalone-smoke.el ends here