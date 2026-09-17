;;; nl-ffi-dsl-standalone-smoke.el --- ffi:library / ffi:defun gate -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Gate for `make ffi-dsl'.  `nl-ffi-call' (which `ffi:library'/`ffi:defun'
;; sit on top of) exists only in the dynamic reader, so `make ffi-dsl'
;; rebuilds it unconditionally with `NELISP_READER_DYNAMIC=1', exactly as
;; `standalone-reader-ffi-smoke' does, before loading this file.  Run
;; directly with:
;;
;;   NELISP_READER_DYNAMIC=1 make standalone-reader
;;   ./target/nelisp --load packages/nl-ffi/test/nl-ffi-dsl-standalone-smoke.el
;;
;; Covers: an integer call (toupper), a double call (sqrt), a string-
;; returning call through `ffi:defun' + `nl-ffi-get-string' (SQLite's
;; version string), `nl-ffi-get-string' reconstructing a string spanning
;; more than one internal chunk plus an immediate-NUL "" (both built
;; directly with alloc-bytes/ptr-write-u8, no C call needed), the
;; `ffi-call' compatibility alias, `ffi:library''s SONAME validation, and
;; three of the four named error conditions `ffi:defun' itself contracts
;; for (the fourth, `nl-ffi-unavailable', needs a build WITHOUT a working
;; `nl-ffi-call' -- see `packages/nl-ffi/test/nl-ffi-test.el', which
;; proves it on host Emacs instead, where that is naturally the case).
;;
;; Step 2 additions: a symbol genuinely NOT in the build-time table,
;; resolved through `ffi:library' (a real `dlopen') + `ffi:defun' (a real
;; `dlsym' + `ptr-call') -- `libz.so.1''s `zlibVersion' (no arguments,
;; `const char *' return) and `crc32' (integer arguments); raw
;; `nl-ffi-call' calls to the new `dlopen'/`dlsym'/`dlclose' table rows
;; directly, independent of the DSL; `ffi:library' really failing to
;; `dlopen' a bogus SONAME (`nl-ffi-library-open-failed', replacing the
;; old table-membership `nl-ffi-unknown-library' rejection -- see
;; `nl-ffi-unknown-library''s docstring in nl-ffi.el); the dlsym path's
;; own two guards, end to end through a real `ffi:defun' call
;; (`nl-ffi-dlsym-float-unsupported', `nl-ffi-too-many-arguments'); and
;; `nl-ffi-unresolved-symbol' via a real, live `dlsym' search that finds
;; nothing, not just a build-time-table miss.  `libz.so.1' is skipped
;; (not failed) if this host does not have it -- see
;; `nl-ffi-smoke-skip' below.
;;
;; `ptr-call-typed' additions: a dlsym-resolved `ffi:defun' naming
;; `:double' now succeeds instead of always refusing -- `cbrt' (f64 arg,
;; f64 return), `scalbn' (mixed f64+integer args), `difftime' (integer
;; args, f64 return), and `lround' (f64 arg, integer return), none of
;; them in `nelisp-standalone--reader-extern-table' (see
;; `nl-ffi-smoke--force-dlsym-library' below for why `libm.so.6'/
;; `libc.so.6' -- both `nl-ffi-known-sonames' entries -- need a
;; different route onto the dlsym search path than `ffi:library'
;; itself gives them).  `:float' anywhere, and `:double' in argument
;; position 5 or 6, remain refused (`nl-ffi-dlsym-float-unsupported').

;;; Code:

;; Minimal ert-deftest/should shim, following the same inline pattern as
;; packages/nl-num/test/nl-num-standalone-smoke.el -- target/nelisp has no
;; `ert', and this package has no other standalone package dependency to
;; borrow a shared shim from.
(defvar nl-ffi-smoke--tests nil
  "Alist of (NAME . BODY-FN) registered by `nl-ffi-smoke-deftest'.")

(defmacro nl-ffi-smoke-deftest (name &rest body)
  "Register BODY as test NAME in `nl-ffi-smoke--tests'."
  `(setq nl-ffi-smoke--tests
         (cons (cons ',name (lambda () ,@body)) nl-ffi-smoke--tests)))

(defmacro nl-ffi-smoke-should (form)
  `(let ((nl-ffi-smoke--value ,form))
     (unless nl-ffi-smoke--value
       (error "should failed: %S" ',form))
     nl-ffi-smoke--value))

(defmacro nl-ffi-smoke-should-error (form condition)
  "Evaluate FORM and require it to signal CONDITION (a quoted symbol)."
  `(let ((nl-ffi-smoke--result
          (condition-case nl-ffi-smoke--err
              (progn ,form 'nl-ffi-smoke--no-error)
            (error nl-ffi-smoke--err))))
     (cond
      ((eq nl-ffi-smoke--result 'nl-ffi-smoke--no-error)
       (error "should-error: no error signaled by %S" ',form))
      ((not (memq ,condition (get (car nl-ffi-smoke--result) 'error-conditions)))
       (error "should-error: expected %S, got %S" ,condition nl-ffi-smoke--result))
      (t nl-ffi-smoke--result))))

(defmacro nl-ffi-smoke-skip (fmt &rest args)
  "Abandon the current test as SKIPPED (neither a pass nor a failure).
Prints an explanatory \"SKIP ...\" line and unwinds to the test runner
loop at the bottom of this file via `throw' -- a `condition-case' around
the test body (that loop uses one to catch a real failure) does not
catch this, by design: a skip is not an error."
  `(progn
     (princ (format ,(concat "SKIP " fmt "\n") ,@args))
     (throw 'nl-ffi-smoke-skip t)))

(load "packages/nl-ffi/src/nl-ffi.el")

(defun nl-ffi-smoke--force-dlsym-library (soname)
  "Register SONAME as a real `dlopen' handle on the dlsym search path.
`ffi:library' skips `dlopen' entirely, and never pushes onto
`nl-ffi--library-order', for a SONAME in `nl-ffi-known-sonames' -- by
design, since such a SONAME's symbols resolve through the fixed
build-time table instead (see `ffi:library''s docstring).  `libm.so.6'
and `libc.so.6' are both known SONAMEs, so exercising the new f64
`ptr-call-typed' path with a real double-taking libm/libc function not
in the build-time table needs the exact same real `dlopen' handle
`ffi:library' would produce for an unknown SONAME, registered the same
way -- this is that, done directly.

Checks `nl-ffi-library-handle', NOT `gethash' on `nl-ffi--libraries'
directly: an earlier `ffi:library' call for a known SONAME (for example
`ffi-dsl-sqrt-double''s own `(ffi:library \"libm.so.6\")' above) already
put a `(:soname SONAME :handle nil)' entry in `nl-ffi--libraries', which
is a non-nil, but handle-less, hash value -- gating on `gethash' alone
would see that entry, wrongly conclude SONAME is already forced, and
silently skip the real `dlopen', leaving `nl-ffi--library-order' without
it (caught by hand: `scalbn'/`difftime' happened to still resolve
because `libz.so.1' -- opened by an earlier test -- transitively depends
on `libc.so.6', which glibc >= 2.34 folds both of those into, while
`cbrt'/`lround' stayed libm-only and failed with `nl-ffi-unresolved-
symbol').  A no-op once SONAME has a real handle, whether from an
earlier call here or a genuine `ffi:library' open of an unknown SONAME."
  (unless (nl-ffi-library-handle soname)
    (let ((handle (nl-ffi--dlopen soname)))
      (puthash soname (list :soname soname :handle handle) nl-ffi--libraries)
      (push soname nl-ffi--library-order))))

;;;; --- toupper: integer argument and return -----------------------------

(nl-ffi-smoke-deftest ffi-dsl-toupper-integer
  (ffi:library "libc.so.6")
  (ffi:defun nl-ffi-smoke-toupper "toupper" [:sint32 :sint32]
    "toupper(3) -- uppercase an ASCII char code.")
  (nl-ffi-smoke-should (= (nl-ffi-smoke-toupper 97) 65))
  (nl-ffi-smoke-should (= (nl-ffi-smoke-toupper 65) 65)))

;;;; --- sqrt: double argument and return ----------------------------------

(nl-ffi-smoke-deftest ffi-dsl-sqrt-double
  (ffi:library "libm.so.6")
  (ffi:defun nl-ffi-smoke-sqrt "sqrt" [:double :double]
    "sqrt(3) -- double-precision square root.")
  (nl-ffi-smoke-should (= (nl-ffi-smoke-sqrt 4.0) 2.0))
  ;; An integer argument is promoted to a float, not rejected.
  (nl-ffi-smoke-should (= (nl-ffi-smoke-sqrt 9) 3.0)))

;;;; --- sqlite3_libversion: string-returning call + nl-ffi-get-string --------

(nl-ffi-smoke-deftest ffi-dsl-sqlite-string-return
  (ffi:library "libsqlite3.so.0")
  (ffi:defun nl-ffi-smoke-sqlite-version "sqlite3_libversion" [:pointer]
    "sqlite3_libversion(3) -- SQLite version string (const char*).")
  (let* ((ptr (nl-ffi-smoke-sqlite-version))
         (str (nl-ffi-get-string ptr)))
    (nl-ffi-smoke-should (integerp ptr))
    (nl-ffi-smoke-should (> ptr 0))
    (nl-ffi-smoke-should (stringp str))
    (nl-ffi-smoke-should (> (length str) 0))))

;;;; --- nl-ffi-get-string: chunked reconstruction + immediate-NUL ------------
;;
;; No C call needed for either: the buffer is built directly with
;; alloc-bytes/ptr-write-u8, exactly the primitives nl-ffi-get-string
;; itself reads with.

(nl-ffi-smoke-deftest ffi-dsl-get-string-longer-than-one-chunk
  ;; 600 > nl-ffi--string-chunk-size (256), so this must cross at least
  ;; one chunk boundary and still reconstruct the exact original bytes.
  (let* ((len 600)
         (buf (alloc-bytes (1+ len) 1))
         (i 0))
    (while (< i len)
      (ptr-write-u8 buf i (+ 97 (mod i 26)))
      (setq i (1+ i)))
    (ptr-write-u8 buf len 0)
    (let ((str (nl-ffi-get-string buf)))
      (nl-ffi-smoke-should (stringp str))
      (nl-ffi-smoke-should (= (length str) len))
      (nl-ffi-smoke-should (= (aref str 0) 97))
      (nl-ffi-smoke-should (= (aref str 255) (+ 97 (mod 255 26))))
      (nl-ffi-smoke-should (= (aref str 256) (+ 97 (mod 256 26))))
      (nl-ffi-smoke-should (= (aref str 599) (+ 97 (mod 599 26)))))))

(nl-ffi-smoke-deftest ffi-dsl-get-string-immediate-nul
  (let ((buf (alloc-bytes 1 1)))
    (ptr-write-u8 buf 0 0)
    (nl-ffi-smoke-should (equal (nl-ffi-get-string buf) ""))))

;;;; --- ffi-call compatibility alias --------------------------------------

(nl-ffi-smoke-deftest ffi-dsl-compat-alias
  (nl-ffi-smoke-should (= (ffi-call nil "toupper" [:sint32 :sint32] 97) 65)))

;;;; --- error contract: wrong arity ----------------------------------------

(nl-ffi-smoke-deftest ffi-dsl-wrong-arity
  (ffi:defun nl-ffi-smoke-wrong-arity-fn "toupper" [:sint32 :sint32])
  (nl-ffi-smoke-should-error (nl-ffi-smoke-wrong-arity-fn) 'nl-ffi-wrong-arity)
  (nl-ffi-smoke-should-error (nl-ffi-smoke-wrong-arity-fn 1 2) 'nl-ffi-wrong-arity))

;;;; --- error contract: unresolved symbol -----------------------------------

(nl-ffi-smoke-deftest ffi-dsl-unresolved-symbol
  (ffi:defun nl-ffi-smoke-unresolved-fn
    "nl_ffi_dsl_smoke_no_such_symbol_ever" [:sint32 :sint32 :sint32])
  (nl-ffi-smoke-should-error (nl-ffi-smoke-unresolved-fn 1 2)
                              'nl-ffi-unresolved-symbol))

;;;; --- error contract: unknown type keyword (expansion-time) ----------------

(nl-ffi-smoke-deftest ffi-dsl-unknown-type
  (nl-ffi-smoke-should-error
   (macroexpand-1 '(ffi:defun nl-ffi-smoke-bad-type-fn "toupper"
                              [:sint32 :not-a-real-type]))
   'nl-ffi-unknown-type))

;;;; --- error contract: library soname a real dlopen cannot open ------------
;;
;; "sqlite3" is not one of `nl-ffi-known-sonames', so as of step 2 this no
;; longer signals the old table-membership `nl-ffi-unknown-library'
;; rejection: `ffi:library' now really attempts `dlopen ("sqlite3", ...)',
;; which fails for a real OS reason ("sqlite3" is not a file `dlopen' can
;; find on the loader search path -- it needs "libsqlite3.so.0" or a real
;; path), so this now proves `nl-ffi-library-open-failed' instead.

(nl-ffi-smoke-deftest ffi-dsl-library-open-failed
  (let ((sig (nl-ffi-smoke-should-error (ffi:library "sqlite3")
                                         'nl-ffi-library-open-failed)))
    ;; The condition data is (SONAME DLERROR-TEXT); DLERROR-TEXT must be a
    ;; real, non-empty string, not the fallback placeholder -- a real
    ;; `dlopen' failure always leaves a `dlerror()' message behind.
    (nl-ffi-smoke-should (equal (nth 0 (cdr sig)) "sqlite3"))
    (nl-ffi-smoke-should (stringp (nth 1 (cdr sig))))
    (nl-ffi-smoke-should (> (length (nth 1 (cdr sig))) 0)))
  ;; A known, successfully declared library's handle is still nil: it
  ;; never calls `dlopen' at all (see `ffi:library').
  (nl-ffi-smoke-should (null (nl-ffi-library-handle "libc.so.6"))))

;;;; --- step 2: raw dlopen/dlsym/dlclose table rows, no DSL involved --------

(nl-ffi-smoke-deftest ffi-dsl-raw-dlopen-dlsym-dlclose
  (let ((handle (nl-ffi-call "dlopen" (nl-ffi--string-to-cstring "libz.so.1") 2)))
    (if (or (null handle) (zerop handle))
        (nl-ffi-smoke-skip "libz.so.1 not dlopen-able via raw nl-ffi-call on this host")
      (let ((addr (nl-ffi-call "dlsym" handle
                                (nl-ffi--string-to-cstring "zlibVersion"))))
        (nl-ffi-smoke-should (integerp addr))
        (nl-ffi-smoke-should (> addr 0))
        (nl-ffi-smoke-should (= (nl-ffi-call "dlclose" handle) 0))
        ;; dlerror() itself is a matched row too: NULL (no pending error)
        ;; comes back as a real, boxed 0, never Lisp nil.
        (nl-ffi-smoke-should (integerp (nl-ffi-call "dlerror")))))))

;;;; --- step 2: a symbol NOT in the build-time table, via the real DSL ------
;;
;; libz.so.1 is a safe CI dependency: zlib is linked by dpkg/apt/systemd/
;; the kernel module loader themselves on any Debian-family image
;; (ubuntu-latest, this CI's Linux lane, included), so it is present
;; without this gate ever asking for it as a dependency of its own.  Skip
;; (not fail) if some other Linux image genuinely lacks it.

(nl-ffi-smoke-deftest ffi-dsl-dlsym-resolved-symbol
  (let ((opened (condition-case nil
                    (progn (ffi:library "libz.so.1") t)
                  (nl-ffi-library-open-failed nil))))
    (unless opened
      (nl-ffi-smoke-skip "libz.so.1 could not be dlopen'ed on this host"))
    (nl-ffi-smoke-should (integerp (nl-ffi-library-handle "libz.so.1")))
    (nl-ffi-smoke-should (> (nl-ffi-library-handle "libz.so.1") 0))
    ;; zlibVersion(): no arguments, `const char *' return -- not in the
    ;; build-time table at all, resolved purely via `dlsym'.
    (ffi:defun nl-ffi-smoke-zlib-version "zlibVersion" [:pointer]
      "zlibVersion() -- resolved via dlsym, not the build-time table.")
    (let* ((ptr (nl-ffi-smoke-zlib-version))
           (str (nl-ffi-get-string ptr)))
      (nl-ffi-smoke-should (integerp ptr))
      (nl-ffi-smoke-should (> ptr 0))
      (nl-ffi-smoke-should (stringp str))
      (nl-ffi-smoke-should (> (length str) 0)))
    ;; crc32(uLong crc, const Bytef *buf, uInt len): integer/pointer
    ;; arguments, also dlsym-resolved.  zlib's own documented identity:
    ;; crc32(0, NULL, 0) == 0.
    (ffi:defun nl-ffi-smoke-crc32 "crc32" [:uint32 :uint32 :pointer :uint32]
      "crc32(uLong crc, const Bytef *buf, uInt len) -- resolved via dlsym.")
    (nl-ffi-smoke-should (= (nl-ffi-smoke-crc32 0 0 0) 0))
    ;; Calling it again exercises the address cache in
    ;; `nl-ffi--resolve-via-dlsym' (a second `dlsym' is not observable
    ;; from here, but a wrong cached value would break this).
    (nl-ffi-smoke-should (= (nl-ffi-smoke-crc32 0 0 0) 0))
    (nl-ffi-smoke-should (= (nl-ffi-smoke-crc32 0 (nl-ffi--string-to-cstring "abc") 3)
                             891568578))))

;;;; --- step 2: unresolved symbol via a real, live dlsym search -------------

(nl-ffi-smoke-deftest ffi-dsl-dlsym-unresolved-symbol
  (let ((opened (condition-case nil
                    (progn (ffi:library "libz.so.1") t)
                  (nl-ffi-library-open-failed nil))))
    (unless opened
      (nl-ffi-smoke-skip "libz.so.1 could not be dlopen'ed on this host"))
    (ffi:defun nl-ffi-smoke-dlsym-miss
      "nl_ffi_dsl_smoke_no_such_symbol_in_any_declared_library" [:sint32 :sint32])
    (nl-ffi-smoke-should-error (nl-ffi-smoke-dlsym-miss 1)
                                'nl-ffi-unresolved-symbol)))

;;;; --- step 2: ptr-call-typed -- :double now works in position 1-4 ---------
;;
;; None of `cbrt'/`scalbn'/`difftime'/`lround' are in
;; `nelisp-standalone--reader-extern-table' (verified by grepping the table
;; in scripts/nelisp-standalone-build.el for each name -- unlike `sqrt',
;; which the fixed-table `ffi-dsl-sqrt-double' test above already covers and
;; which would not exercise this dlsym path at all).  `libm.so.6'/
;; `libc.so.6' are always present (the reader already dynamically links
;; against both for the fixed table's own `sqrt'/`toupper' rows), so these
;; do not skip the way the `libz.so.1' cases above can.

(nl-ffi-smoke-deftest ffi-dsl-ptr-call-typed-f64-arg-f64-return
  ;; cbrt(double x) -- an f64 argument with an f64 return, arity 1.
  (nl-ffi-smoke--force-dlsym-library "libm.so.6")
  (ffi:defun nl-ffi-smoke-cbrt "cbrt" [:double :double]
    "cbrt(3) -- resolved via dlsym, not the build-time table.")
  (nl-ffi-smoke-should (= (nl-ffi-smoke-cbrt 8.0) 2.0)))

(nl-ffi-smoke-deftest ffi-dsl-ptr-call-typed-mixed-f64-int-args
  ;; scalbn(double x, int n) = x * 2^n -- a mixed f64 + integer argument
  ;; signature, f64 return.
  (nl-ffi-smoke--force-dlsym-library "libm.so.6")
  (ffi:defun nl-ffi-smoke-scalbn "scalbn" [:double :double :sint32]
    "scalbn(3) -- resolved via dlsym, not the build-time table.")
  (nl-ffi-smoke-should (= (nl-ffi-smoke-scalbn 1.5 4) 24.0)))

(nl-ffi-smoke-deftest ffi-dsl-ptr-call-typed-int-args-f64-return
  ;; difftime(time_t t1, time_t t0) -- integer (time_t) arguments only,
  ;; f64 return.
  (nl-ffi-smoke--force-dlsym-library "libc.so.6")
  (ffi:defun nl-ffi-smoke-difftime "difftime" [:double :sint64 :sint64]
    "difftime(3) -- resolved via dlsym, not the build-time table.")
  (nl-ffi-smoke-should (= (nl-ffi-smoke-difftime 100 40) 60.0)))

(nl-ffi-smoke-deftest ffi-dsl-ptr-call-typed-f64-arg-int-return
  ;; lround(double x) -- an f64 argument with an integer (long) return.
  (nl-ffi-smoke--force-dlsym-library "libm.so.6")
  (ffi:defun nl-ffi-smoke-lround "lround" [:sint64 :double]
    "lround(3) -- resolved via dlsym, not the build-time table.")
  (nl-ffi-smoke-should (= (nl-ffi-smoke-lround 7.6) 8)))

;;;; --- step 2: the dlsym path still refuses :float, and :double past 1-4 ---

(nl-ffi-smoke-deftest ffi-dsl-dlsym-float-single-precision-refused
  ;; `:float' (single precision) is refused wherever it appears -- this
  ;; path only marshals a C `double'.
  (ffi:defun nl-ffi-smoke-dlsym-float-arg
    "nl_ffi_dsl_smoke_no_such_symbol_float_arg" [:sint32 :float])
  (nl-ffi-smoke-should-error (nl-ffi-smoke-dlsym-float-arg 1.5)
                              'nl-ffi-dlsym-float-unsupported))

(nl-ffi-smoke-deftest ffi-dsl-dlsym-double-position-5-6-refused
  ;; A `:double' in argument position 5 or 6 is still refused --
  ;; `ptr-call-typed''s generated SIG mask only classes positions 1-4 (a
  ;; size budget, not an ABI limit; see
  ;; `nl-ffi--ptr-call-typed-max-f64-position').
  (ffi:defun nl-ffi-smoke-dlsym-float-pos6
    "nl_ffi_dsl_smoke_no_such_symbol_float_pos6"
    [:sint32 :sint32 :sint32 :sint32 :sint32 :sint32 :double])
  (nl-ffi-smoke-should-error (nl-ffi-smoke-dlsym-float-pos6 1 2 3 4 5 6.0)
                              'nl-ffi-dlsym-float-unsupported))

(nl-ffi-smoke-deftest ffi-dsl-dlsym-too-many-arguments-refused
  (ffi:defun nl-ffi-smoke-dlsym-7-args
    "nl_ffi_dsl_smoke_no_such_symbol_7_args"
    [:sint32 :sint32 :sint32 :sint32 :sint32 :sint32 :sint32 :sint32])
  (nl-ffi-smoke-should-error (nl-ffi-smoke-dlsym-7-args 1 2 3 4 5 6 7)
                              'nl-ffi-too-many-arguments))

;;;; --- run ------------------------------------------------------------------

(let ((tests (reverse nl-ffi-smoke--tests))
      (ran 0)
      (skipped 0)
      (failures nil))
  (while tests
    (let* ((test (car tests))
           (skip
            (catch 'nl-ffi-smoke-skip
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
  ;; `tools/ai/nelisp-ai.sh gate NAME -- ...' requires this exact line to
  ;; report what the gate checked; its absence is itself a hard failure
  ;; there (see tools/ai/nelisp-ai.sh's `cmd_gate').  `skipped=' is extra
  ;; trailing text its `checked='/`findings=' extraction (a `sed' pattern
  ;; ending in `.*') tolerates without change.
  (princ (format "GATE-COUNT checked=%d findings=%d skipped=%d\n"
                  ran (length failures) skipped))
  (when failures
    (let ((all failures))
      (while all
        (princ (format "FAIL %s\n" (car all)))
        (setq all (cdr all))))
    (error "nl-ffi-dsl-standalone-smoke: %d failure(s), %d passed, %d skipped"
           (length failures) ran skipped))
  ;; Checked against RAN+SKIPPED, not RAN alone: a legitimate skip (libz.so.1
  ;; absent -- see `ffi-dsl-dlsym-resolved-symbol'/`ffi-dsl-dlsym-unresolved-
  ;; symbol') must not read as "fewer tests ran than expected".
  (when (< (+ ran skipped) 20)
    (error "nl-ffi-dsl-standalone-smoke: only %d test(s) ran + %d skipped (expected >= 20 total)"
           ran skipped))
  (princ (format "nl-ffi-dsl-standalone-smoke: PASS (%d tests, %d skipped)\n"
                  ran skipped)))

;;; nl-ffi-dsl-standalone-smoke.el ends here
