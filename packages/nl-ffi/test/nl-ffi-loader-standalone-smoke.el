;;; nl-ffi-loader-standalone-smoke.el --- pure-elisp ELF loader gate -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Gate for `make ffi-loader'.  Unlike `ffi-dsl'/`standalone-reader-ffi-
;; smoke', this rebuilds and runs against the DEFAULT STATIC reader (no
;; `NELISP_READER_DYNAMIC') -- that is the whole point of FFI step 3
;; increment 1 (packages/nl-ffi/src/nl-ffi-loader.el).  Run directly with:
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
;;   cc -shared -fPIC -o target/nl-ffi-loader-fixture-init.so \
;;     packages/nl-ffi/test/fixtures/nl-ffi-loader-fixture-init.c
;;   make standalone-reader   # the DEFAULT static build, no dynamic flag
;;   ./target/nelisp --load packages/nl-ffi/test/nl-ffi-loader-standalone-smoke.el
;;
;; See `Makefile''s `ffi-loader' target for the real recipe (it also
;; skips, rather than fails, when `cc' is not on PATH or the gate target
;; is not linux-x86_64).
;;
;; Covers, against the real fixture (RELATIVE + GLOB_DAT + JUMP_SLOT, no
;; DT_NEEDED/DT_INIT/PT_TLS -- see the fixture sources' own commentary):
;;   - `nl-ffi-loader-open' + `nl-ffi-loader-symbol' + `ptr-call' directly
;;     (the loader's own low-level API), for a JUMP_SLOT-resolved call, a
;;     GLOB_DAT-resolved call, and a RELATIVE-relocated data pointer read
;;     back and called.
;;   - The SAME three, through the full `ffi:library'/`ffi:defun' surface
;;     on this static reader -- proving `ffi:library' really falls back
;;     to this loader instead of signalling `nl-ffi-unavailable', and
;;     that `ffi:defun''s call shape and `nl-ffi-unresolved-symbol'
;;     contract are unchanged.
;; Covers, against three more real fixtures, each built to trip exactly
;; one whole-object refusal:
;;   - `nl-ffi-loader-fixture-needs-dep.so' (a real DT_NEEDED on libm) ->
;;     `nl-ffi-loader-unsupported' reason `:needs-dependency'.
;;   - `nl-ffi-loader-fixture-tls.so' (a real PT_TLS segment) ->
;;     `nl-ffi-loader-unsupported' reason `:tls-segment'.
;;   - `nl-ffi-loader-fixture-init.so' (real DT_INIT/DT_INIT_ARRAY) ->
;;     `nl-ffi-loader-unsupported' reason `:has-initializers'.
;; Covers, via hand-built in-memory Elf64_Rela entries and a fabricated
;; one-symbol `.dynsym'/`.dynstr' (no compiler needed for these -- they
;; exercise `nl-ffi-loader--apply-one-relocation' directly):
;;   - R_X86_64_RELATIVE applies correctly (positive case).
;;   - R_X86_64_GLOB_DAT applies correctly when the symbol is DEFINED
;;     (positive case).
;;   - R_X86_64_GLOB_DAT refuses when the symbol is UNDEFINED (reason
;;     `:undefined-symbol', naming it).
;;   - R_X86_64_IRELATIVE refuses (reason `:ifunc').
;;   - A TLS-classed relocation type refuses (reason `:tls-relocation')
;;     even outside a PT_TLS object.
;;   - R_X86_64_64 (a real type, just not one of the three supported)
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
(defconst nl-ffi-loader-smoke--fixture-init "target/nl-ffi-loader-fixture-init.so")

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

;;;; --- whole-object refusals: real fixtures -----------------------------------

(nl-ffi-loader-smoke-deftest ffi-loader-refuses-needs-dependency
  (let ((sig (nl-ffi-loader-smoke-should-error
              (nl-ffi-loader-open nl-ffi-loader-smoke--fixture-needs-dep)
              'nl-ffi-loader-unsupported)))
    (nl-ffi-loader-smoke-should (eq (nth 0 (cdr sig)) :needs-dependency))))

(nl-ffi-loader-smoke-deftest ffi-loader-refuses-tls-segment
  (let ((sig (nl-ffi-loader-smoke-should-error
              (nl-ffi-loader-open nl-ffi-loader-smoke--fixture-tls)
              'nl-ffi-loader-unsupported)))
    (nl-ffi-loader-smoke-should (eq (nth 0 (cdr sig)) :tls-segment))))

(nl-ffi-loader-smoke-deftest ffi-loader-refuses-initializers
  (let ((sig (nl-ffi-loader-smoke-should-error
              (nl-ffi-loader-open nl-ffi-loader-smoke--fixture-init)
              'nl-ffi-loader-unsupported)))
    (nl-ffi-loader-smoke-should (eq (nth 0 (cdr sig)) :has-initializers))))

;;;; --- relocation-type refusals: synthetic, no compiler needed ----------------
;;
;; Builds a fake one-symbol .dynsym/.dynstr and a fake Elf64_Rela entry by
;; hand (alloc-bytes/ptr-write-u64/ptr-write-u8), then calls
;; `nl-ffi-loader--apply-one-relocation' directly -- exercises the
;; relocation-type dispatch and the undefined-symbol check in isolation,
;; without needing a real ifunc/TLS/exotic-relocation object.

(defun nl-ffi-loader-smoke--fake-dyn (name shndx value)
  "A DYN plist with one `.dynsym' entry (index 0) named NAME."
  (let* ((strtab (alloc-bytes (+ 2 (length name)) 1))
         (sym (alloc-bytes 24 8))
         (i 0))
    (ptr-write-u8 strtab 0 0) ; offset 0: STN_UNDEF's own empty name
    (while (< i (length name))
      (ptr-write-u8 strtab (+ 1 i) (aref name i))
      (setq i (1+ i)))
    (ptr-write-u8 strtab (+ 1 (length name)) 0)
    (ptr-write-u64 sym 0 (logior 1 (ash shndx 48))) ; st_name=1, st_shndx=SHNDX
    (ptr-write-u64 sym 8 value)                     ; st_value
    (ptr-write-u64 sym 16 0)                        ; st_size
    (list :symtab sym :strtab strtab :syment 24 :gnu-hash nil :sysv-hash nil)))

(defun nl-ffi-loader-smoke--fake-rela (r-offset r-sym r-type r-addend)
  (let ((rela (alloc-bytes 24 8)))
    (ptr-write-u64 rela 0 r-offset)
    (ptr-write-u64 rela 8 (logior (ash r-sym 32) r-type))
    (ptr-write-u64 rela 16 r-addend)
    rela))

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

(nl-ffi-loader-smoke-deftest ffi-loader-refuses-ifunc
  (let* ((target (alloc-bytes 8 8))
         (rela (nl-ffi-loader-smoke--fake-rela target 0 37 4096))
         (sig (nl-ffi-loader-smoke-should-error
               (nl-ffi-loader--apply-one-relocation "synthetic" 0 nil rela)
               'nl-ffi-loader-unsupported)))
    (nl-ffi-loader-smoke-should (eq (nth 0 (cdr sig)) :ifunc))))

(nl-ffi-loader-smoke-deftest ffi-loader-refuses-tls-relocation-type
  (let* ((target (alloc-bytes 8 8))
         (rela (nl-ffi-loader-smoke--fake-rela target 0 18 0)) ; R_X86_64_TPOFF64
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
  (when (< (+ ran skipped) 15)
    (error "nl-ffi-loader-standalone-smoke: only %d test(s) ran + %d skipped (expected >= 15 total)"
           ran skipped))
  (princ (format "nl-ffi-loader-standalone-smoke: PASS (%d tests, %d skipped)\n"
                  ran skipped)))

;;; nl-ffi-loader-standalone-smoke.el ends here
