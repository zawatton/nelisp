;;; nl-ffi.el --- declarative FFI surface over nl-ffi-call -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Strategy section 8.1 asks for a declarative FFI surface: `ffi:library'
;; names a shared library, `ffi:defun' declares a typed wrapper around one
;; of its C symbols.  The call mechanism already existed (`nl-ffi-call',
;; a reader builtin -- see `standalone-reader-ffi-smoke' in the Makefile
;; and the Step C table in scripts/nelisp-standalone-build.el); this
;; package is the missing declarative layer: named libraries, typed
;; signatures, argument/result conversion, and named error conditions
;; instead of a silent nil.
;;
;; Symbol resolution (read this before writing an `ffi:library' form):
;;
;;   `nl-ffi-call' is NOT a dlopen/dlsym-style loader.  It dispatches
;;   through one FIXED, BUILD-TIME table
;;   (`nelisp-standalone--reader-extern-table' in
;;   scripts/nelisp-standalone-build.el): rows of (SYMBOL SONAME ARITY
;;   SIG) that drive both the binary's own ELF/PE import list (resolved
;;   eagerly by the OS loader when the process starts, not by anything
;;   Lisp calls at runtime) and a nested `if' chain matching the NAME
;;   argument.  Resolution is therefore GLOBAL and FLAT: a C symbol is
;;   either already linked into the CURRENTLY RUNNING BINARY (regardless
;;   of which "library" a caller thinks it belongs to), or it is not
;;   reachable at all -- there is no per-library handle, and nothing in
;;   this package can add a new symbol to that table.  `ffi:library' and
;;   `ffi:defun' are declarative front ends over whatever the binary
;;   already has; they do not, and cannot, load anything new (see
;;   `ffi:library''s docstring, and the package README's "Symbol
;;   resolution" section, for the full reasoning and its consequences).
;;
;;   A name that is not in the table falls through to a default dispatch
;;   arm that writes Lisp `nil' -- silently, today, at the `nl-ffi-call'
;;   level.  Every MATCHED row, by contrast, always boxes its result as a
;;   real Lisp integer or float (`wf_write_int' / a float write), never
;;   `nil'.  That makes a raw `nil' result an unambiguous "this symbol did
;;   not resolve" sentinel for any call whose real arity was honored, and
;;   `nl-ffi--invoke' below uses exactly that to turn a silent nil into
;;   `nl-ffi-unresolved-symbol'.
;;
;;   Conversely, calling `nl-ffi-call' with the WRONG arity is unsafe, not
;;   merely wrong: a matched row reads its Nth argument by walking N cdrs
;;   of the actual argument list (`wf_arg_ptr'), with no bounds check
;;   against how many arguments the Lisp call site actually supplied.
;;   Every `ffi:defun'-generated function therefore validates arity
;;   itself, in Lisp, BEFORE ever calling `nl-ffi-call' -- see
;;   `nl-ffi--invoke'.  This is also why `ffi:library' does not attempt to
;;   probe an arbitrary declared symbol by calling it with synthesized
;;   arguments: without a real, correctly typed call (only available once
;;   an `ffi:defun' wrapper is actually invoked), a probe call is either
;;   unsafe or meaningless.
;;
;; Scope: `nl-ffi-call' exists only in the dynamic reader
;; (`NELISP_READER_DYNAMIC=1'; see `standalone-reader-ffi-smoke').  The
;; default static reader has `nl-ffi-call' `fboundp' but calling it
;; signals the reader's own catchable `nelisp-unsupported-primitive'
;; condition; `nl-ffi--invoke' catches that and re-signals
;; `nl-ffi-unavailable', the same condition it uses when `nl-ffi-call' is
;; entirely absent (host Emacs, or any other build without it).
;;
;; Policy: FFI is for the external-library layer only -- image/font/TLS/
;; compression/XML/SQLite/crypto, the libraries Emacs itself treats as
;; external and dynamically loads.  It is not a path into NeLisp's own
;; core (allocator, evaluator, display); see dev/CLAUDE.md and this
;; repository's own FFI policy notes.  FFI is not a speed argument either
;; -- the call floor is the interpreter, not the glue.
;;
;; Type vocabulary, vector call shape (`[RET ARG...]', return type
;; first), the `get-string' helper, and the `ffi-call' compatibility
;; alias (`(LIBRARY SYMBOL SIGNATURE &rest ARGS)') follow `dev/nelisp-ffi'
;; (itself a fork of `elisp-ffi', Christopher Wellons / skeeto, public
;; domain, https://github.com/skeeto/elisp-ffi) rather than strategy
;; section 8.1's illustrative labeled-argument sketch, so existing
;; elisp-ffi/nelisp-ffi callers keep working -- see the package README.
;;
;; Roadmap (this package is step 2 of 3; see the package README's
;; "Roadmap" section for the full reasoning):
;;
;;   1. DONE.  `ffi:library'/`ffi:defun' over the symbols the reader's
;;      build-time extern table already carries -- a table row and a
;;      reader rebuild was the only way to add a new symbol.
;;   2. THIS STEP.  `dlopen'/`dlsym'/`dlerror'/`dlclose' are now ordinary
;;      rows in `nelisp-standalone--reader-extern-table' (SONAME
;;      "libc.so.6": glibc >= 2.34 keeps them there; see
;;      `nl-ffi--dlopen''s Commentary below for what an older glibc would
;;      need).  A C symbol the build-time table does not carry no longer
;;      signals `nl-ffi-unresolved-symbol' outright: `ffi:library' now
;;      really `dlopen's a SONAME outside `nl-ffi-known-sonames', and
;;      `nl-ffi--invoke' falls back to resolving the symbol with `dlsym'
;;      against a declared library's handle and calling it through
;;      `ptr-call' -- see `nl-ffi--ptr-call-invoke'.  `ptr-call' only
;;      passes/returns integers (six argument slots, no f64 classing), so
;;      this path is integer/pointer-only: a signature naming `:float' or
;;      `:double' for a symbol resolved this way signals
;;      `nl-ffi-dlsym-float-unsupported' instead of a silently wrong
;;      answer, and more than six arguments signals
;;      `nl-ffi-too-many-arguments'.  A symbol still in the build-time
;;      table keeps resolving through `nl-ffi-call' exactly as step 1
;;      left it, with no `dlopen'/`dlsym' call at all.
;;   3. (Later still.)  Grow the pure-elisp ELF loader
;;      (dev/nelisp-ffi/nelisp-ffi-pure.el, leaf functions only today) so
;;      the default STATICALLY linked reader gains the same capability.
;;      The default binary stays statically linked either way; switching
;;      it to dynamic linking is not the plan.  Reaching an f64-capable
;;      `ptr-call' remains open for either step 2 or step 3 to pick up;
;;      see `nl-ffi--ptr-call-invoke''s Commentary for what that needs.
;;
;; `ffi:defun' keeps its call shape from step 1 -- it still takes no
;; library argument, and its signature vector never encodes where
;; C-SYMBOL resolves from.  `nl-ffi--invoke' is the one place that
;; decides per symbol, at call time: the build-time table first (global
;; and flat, as before), then every `dlopen'ed library's handle, in
;; declaration order, via `dlsym' (see `nl-ffi--resolve-via-dlsym').
;;
;; Public API:
;;   `ffi:library'      -- (ffi:library SONAME) declare + validate a library
;;   `ffi:defun'         -- (ffi:defun NAME "c_symbol" [RET ARG...] &optional DOC)
;;   `nl-ffi-get-string' -- read a NUL-terminated C string at an address
;;   `ffi-call'          -- elisp-ffi/nelisp-ffi compatible entry point
;;   `ffi-get-string'    -- elisp-ffi/nelisp-ffi compatible alias

;;; Code:

(declare-function nl-ffi-call "ext:nelisp-runtime" (name &rest args))
(declare-function alloc-bytes "ext:nelisp-runtime" (nbytes align))
(declare-function ptr-read-u8 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u8 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-call "ext:nelisp-runtime" (address a b c d e f))
;; packages/nl-ffi/src/nl-ffi-loader.el (step 3 increment 1), loaded further
;; down this file, after the error conditions and helpers it reuses --
;; see the `(unless (featurep 'nl-ffi-loader) ...)' form below.  Declared
;; here so byte-compiling this file alone (before that `load' has ever
;; run) does not warn that these are undefined.
(declare-function nl-ffi-loader-open "nl-ffi-loader" (path))
(declare-function nl-ffi-loader-handle-p "nl-ffi-loader" (handle))
(declare-function nl-ffi-loader-symbol "nl-ffi-loader" (handle name))

;;;; --- error conditions ---------------------------------------------------

(define-error 'nl-ffi-error "NeLisp FFI error")

(define-error 'nl-ffi-unavailable
  "nl-ffi-call is not available in this build/host"
  'nl-ffi-error)

(define-error 'nl-ffi-wrong-arity
  "wrong number of arguments for an ffi:defun function"
  'nl-ffi-error)

(define-error 'nl-ffi-unknown-type
  "unknown FFI type keyword"
  'nl-ffi-error)

(define-error 'nl-ffi-unresolved-symbol
  "FFI C symbol did not resolve in this binary"
  'nl-ffi-error)

(define-error 'nl-ffi-unknown-library
  "FFI library soname is not in the reader's extern table"
  'nl-ffi-error)
;; No longer signalled by `ffi:library' as of step 2: a SONAME outside
;; `nl-ffi-known-sonames' is now attempted via `dlopen' instead of being
;; rejected outright -- see `nl-ffi-library-open-failed' for what a real
;; open failure signals now.  Left defined (never removed) for any
;; existing caller that still catches it by name.

(define-error 'nl-ffi-library-open-failed
  "dlopen failed to open an FFI library"
  'nl-ffi-error)

(define-error 'nl-ffi-dlsym-float-unsupported
  "a dlsym-resolved FFI call cannot use :float/:double -- only a symbol \
still in the build-time table can; add a table row for it"
  'nl-ffi-error)

(define-error 'nl-ffi-too-many-arguments
  "more arguments than ptr-call can carry (six) for a dlsym-resolved call"
  'nl-ffi-error)

;;;; --- type vocabulary ------------------------------------------------------

(defconst nl-ffi-types
  '(:uint8 :uint16 :uint32 :uint64
    :sint8 :sint16 :sint32 :sint64
    :float :double :pointer :void)
  "Type keywords accepted in an `ffi:defun'/`ffi-call' signature vector.
Matches `dev/nelisp-ffi' (a fork of Christopher Wellons's public-domain
elisp-ffi) so existing signatures written for either keep working here.")

(defconst nl-ffi--integer-types
  '(:uint8 :uint16 :uint32 :uint64 :sint8 :sint16 :sint32 :sint64)
  "The fixed-width integer type keywords among `nl-ffi-types'.")

(defun nl-ffi-type-p (type)
  "Return non-nil when TYPE is one of `nl-ffi-types'."
  (and (memq type nl-ffi-types) t))

;;;; --- argument / result conversion ----------------------------------------

(defun nl-ffi--string-to-cstring (string)
  "Copy STRING into a fresh NUL-terminated buffer; return its address.
STRING is treated as a sequence of bytes (a unibyte string, or an ASCII
unibyte-compatible multibyte string) -- exactly what `nl-ffi-call''s own
existing callers already assume (see the `cs' closure in the Makefile's
`standalone-reader-ffi-smoke' recipe).  Encode multibyte text with
`encode-coding-string' yourself before passing it as a `:pointer'
argument.

The buffer is allocated with `alloc-bytes' and is never freed -- like
every other caller-built argument buffer in this runtime, it lives for
the rest of the process.  Fine for the short, low-volume strings FFI
arguments typically are; do not use this to marshal large or
high-frequency string traffic."
  (let* ((n (length string))
         (buf (alloc-bytes (1+ n) 1))
         (i 0))
    (while (< i n)
      (ptr-write-u8 buf i (aref string i))
      (setq i (1+ i)))
    (ptr-write-u8 buf n 0)
    buf))

(defun nl-ffi--convert-arg (fn-name index type value)
  "Convert VALUE for TYPE, the type of argument INDEX (1-based) of FN-NAME.
Signals `wrong-type-argument' when VALUE does not fit TYPE, and
`nl-ffi-unknown-type' when TYPE itself is not one of `nl-ffi-types'.

Conversions:
  - a fixed-width integer type requires an integer and passes it through;
  - `:float'/`:double' pass a float through unchanged and promote an
    integer to a float (an implicit C int->double widening, not a mask
    on a real bug -- see the README for why this differs from the
    integer types' strictness);
  - `:pointer' accepts an address (an integer), nil (-> address 0), or a
    Lisp string, copied into a fresh NUL-terminated buffer by
    `nl-ffi--string-to-cstring';
  - `:void' is only meaningful as a return type; it is a declaration
    error as an argument type."
  (cond
   ((memq type nl-ffi--integer-types)
    (unless (integerp value)
      (signal 'wrong-type-argument (list 'integerp value fn-name index)))
    value)
   ((memq type '(:float :double))
    (cond
     ((floatp value) value)
     ((integerp value) (float value))
     (t (signal 'wrong-type-argument (list 'numberp value fn-name index)))))
   ((eq type :pointer)
    (cond
     ((null value) 0)
     ((integerp value) value)
     ((stringp value) (nl-ffi--string-to-cstring value))
     (t (signal 'wrong-type-argument (list 'nl-ffi-pointer-arg value fn-name index)))))
   (t
    ;; Everything else is `nl-ffi-unknown-type': either TYPE is not one of
    ;; `nl-ffi-types' at all, or it is `:void' -- a well-known keyword, but
    ;; only meaningful as a return type, so a declaration error here too.
    (signal 'nl-ffi-unknown-type (list fn-name type index)))))

(defconst nl-ffi-max-string-length 65536
  "Upper bound on the bytes `nl-ffi-get-string' reads looking for a NUL.
A runaway pointer (never terminated) is far more likely than a genuine
C string this large crossing the FFI boundary; treat the limit as a
diagnostic, not a real constraint, and raise it locally if a real
callee needs more.")

(defconst nl-ffi--string-chunk-size 256
  "Bytes `nl-ffi-get-string' accumulates before flushing one `unibyte-string'
call.  `nl-ffi-max-string-length' can be as large as 65536; collecting
every byte into one list and applying `unibyte-string' to the whole
list in a single call would apply a function to up to that many
arguments at once -- a real risk on the standalone reader, not merely
an inefficiency.  Flushing in fixed-size chunks and joining them with
`concat' bounds the argument count of every single call to this
constant, no matter how long the actual string turns out to be.")

(defun nl-ffi-get-string (ptr)
  "Read a NUL-terminated C string at address PTR into a unibyte string.
PTR is an integer address, typically the raw return value of an
`ffi:defun' function declared `:pointer' (or `:void'/`:integer' if the
caller wants the raw address instead of a decoded string) that wraps a
`const char *'-returning C function.  Returns nil for a NULL PTR
\(address 0), and \"\" when PTR's first byte is already NUL.  Signals
`nl-ffi-error' when no NUL byte appears within `nl-ffi-max-string-length'
bytes.

Reads and decodes in `nl-ffi--string-chunk-size'-byte pieces (see its
docstring for why) rather than collecting every byte into one list and
converting it in a single call."
  (unless (integerp ptr)
    (signal 'wrong-type-argument (list 'integerp ptr)))
  (if (zerop ptr)
      nil
    (let ((chunks nil)
          (chunk nil)
          (chunk-len 0)
          (i 0)
          (terminated nil))
      (while (and (not terminated) (< i nl-ffi-max-string-length))
        (let ((byte (ptr-read-u8 ptr i)))
          (if (= byte 0)
              (setq terminated t)
            (push byte chunk)
            (setq chunk-len (1+ chunk-len))
            (setq i (1+ i))
            (when (= chunk-len nl-ffi--string-chunk-size)
              (push (apply #'unibyte-string (nreverse chunk)) chunks)
              (setq chunk nil chunk-len 0)))))
      (unless terminated
        (signal 'nl-ffi-error
                (list (format "nl-ffi-get-string: no NUL within %d bytes at address %d"
                              nl-ffi-max-string-length ptr))))
      (when chunk
        (push (apply #'unibyte-string (nreverse chunk)) chunks))
      (apply #'concat (nreverse chunks)))))

;;;; --- step 3 increment 1: pure-elisp ELF loader for the static reader ------
;;
;; Loaded here (after `nl-ffi--string-to-cstring'/`nl-ffi-get-string' above,
;; which it reuses, and before the step 2 `dlopen'/`dlsym' machinery below,
;; which now falls back to it), resolved relative to THIS file's own
;; directory rather than a bare "packages/nl-ffi/src/nl-ffi-loader.el"
;; relative to the process's CWD -- the same `load-file-name' idiom
;; packages/nl-ffi/test/nl-ffi-test.el already uses (see its own `here'
;; binding).  A bare CWD-relative path broke exactly this way under
;; `tools/ai/nelisp-ai.sh test': `nl-ffi-test.el' reaches this file via
;; `(require 'nl-ffi)' against `load-path' (not the smoke tests' own
;; `(load "packages/nl-ffi/src/nl-ffi.el")' from the repository root), and
;; that harness's `default-directory' when it runs Emacs is not the
;; repository root, so the bare relative path did not exist from there --
;; "Cannot open load file ... packages/nl-ffi/src/nl-ffi-loader.el",
;; 2026-09-16.  `load-file-name' is correctly bound to THIS file's own
;; resolved path by both `require' (host Emacs) and a nested `load' (the
;; standalone reader; confirmed empirically: the top-level `--load' target
;; itself sees `load-file-name' nil there, but a `load' nested inside it
;; -- exactly what a smoke test's `(load "packages/nl-ffi/src/nl-ffi.el")'
;; is, from this file's point of view -- does not), so this is robust
;; either way; `default-directory' is the same fallback `nl-ffi-test.el'
;; already uses for the one case where it is not.
(unless (featurep 'nl-ffi-loader)
  (load (expand-file-name
         "nl-ffi-loader.el"
         (or (and load-file-name (file-name-directory load-file-name))
             default-directory))))

;;;; --- shared call engine ---------------------------------------------------

(defun nl-ffi--invoke (fn-name c-symbol arg-types ret-type args)
  "Run the FFI call an `ffi:defun'/`ffi-call' wrapper for FN-NAME describes.
C-SYMBOL is the C symbol name (a string), ARG-TYPES the list of
argument type keywords (return type excluded), RET-TYPE the return
type keyword, and ARGS the actual Lisp argument values supplied by the
caller.

Order of checks, each signalling a distinct named condition instead of
returning a silent nil (see the package README's error contract):

  1. `nl-ffi-wrong-arity' when (length ARGS) does not match ARG-TYPES --
     checked first, and in Lisp, because calling `nl-ffi-call' itself
     with the wrong arity is unsafe (see this file's Commentary).
  2. `nl-ffi-unavailable' when `nl-ffi-call' is not `fboundp' at all
     (host Emacs; a NeLisp build without the dynamic reader's opt-in
     extern table).
  3. Each argument is converted by `nl-ffi--convert-arg' (may signal
     `wrong-type-argument' or `nl-ffi-unknown-type').
  4. The call is dispatched through `nl-ffi-call'.  The reader's own
     `nelisp-unsupported-primitive' -- raised on the default STATIC
     reader, where `nl-ffi-call' is `fboundp' but the whole fixed-table
     dispatch arm is compiled out regardless of C-SYMBOL (see
     packages/nl-ffi/src/nl-ffi-loader.el's Commentary) -- is caught and
     treated exactly like step 5's raw `nil' below, as of step 3
     increment 1: it no longer signals `nl-ffi-unavailable' directly,
     because C-SYMBOL may still resolve through a loader-backed
     `ffi:library' (step 3) even though the fixed table cannot reach it
     at all on this build.
  5. A raw `nil' result -- the build-time table's own unmatched-name
     fallback on a build where it exists at all, since every matched row
     boxes a real number, OR step 4's caught `nelisp-unsupported-
     primitive' on a build where it does not -- no longer means
     \"unresolved\" outright as of step 2/3: `nl-ffi--ptr-call-invoke'
     gets one more chance to resolve C-SYMBOL via `dlsym' or
     `nl-ffi-loader-symbol' (whichever kind of handle a preceding
     `ffi:library' registered -- see
     `nl-ffi--symbol-address-in-library') and call it through `ptr-call',
     and only THAT signals `nl-ffi-unresolved-symbol' when no declared
     library carries it either.
  6. A `:void' RET-TYPE discards the (already known non-nil) raw result
     and returns nil; any other RET-TYPE returns it as-is."
  (let ((declared (length arg-types))
        (given (length args)))
    (unless (= declared given)
      (signal 'nl-ffi-wrong-arity (list fn-name declared given))))
  (unless (fboundp 'nl-ffi-call)
    (signal 'nl-ffi-unavailable (list fn-name c-symbol)))
  (let ((raw-args nil) (index 0) (rest-args args) (rest-types arg-types))
    (while rest-types
      (setq index (1+ index))
      (push (nl-ffi--convert-arg fn-name index (car rest-types) (car rest-args))
            raw-args)
      (setq rest-types (cdr rest-types))
      (setq rest-args (cdr rest-args)))
    (setq raw-args (nreverse raw-args))
    (let ((result
           (condition-case _err
               (apply #'nl-ffi-call c-symbol raw-args)
             ;; Treated the same as a raw table-miss `nil' (see item 4/5
             ;; above), NOT re-signalled here -- a loader-backed
             ;; `ffi:library' (step 3) may still resolve C-SYMBOL even
             ;; though the fixed table cannot on this build.
             (nelisp-unsupported-primitive nil))))
      (when (null result)
        (setq result
              (nl-ffi--ptr-call-invoke fn-name c-symbol arg-types ret-type raw-args)))
      (if (eq ret-type :void) nil result))))

;;;; --- step 2: resolving a symbol the build-time table does not carry ------
;;
;; `nl-ffi--invoke' above reaches this only once `nl-ffi-call' has already
;; said "not in the table" (a raw nil).  Everything here is therefore the
;; SLOW, rare path: a real `dlsym' the first time a given (library .
;; symbol) pair is asked for, cached after that.

(defconst nl-ffi--ptr-call-max-args 6
  "Maximum real arguments `ptr-call' can carry beyond the address.
The reader's own dispatch arm for it (`(:lit \"ptr-call\")' in
scripts/nelisp-standalone-build.el) always reads exactly six `wf_argval'
slots after the address and pads any the Lisp call site did not supply
with garbage from beyond the actual argument list -- see
`nelisp-native-load-raw-call''s and `nelisp-native-load''s own callers
for the same contract (\"ptr-call reads six arguments after the address
unconditionally, so hand it six\").  A `ffi:defun' signature resolved via
`dlsym' with more than this many arguments cannot be called this way;
see `nl-ffi-too-many-arguments'.")

(defconst nl-ffi--rtld-now 2
  "RTLD_NOW, as glibc's <dlfcn.h> defines it.")

(defconst nl-ffi--rtld-local 0
  "RTLD_LOCAL, as glibc's <dlfcn.h> defines it -- the default binding
scope (0, i.e. no bit at all).  Named and ORed into `ffi:library''s
`dlopen' flags anyway, purely so the intent reads at the call site
instead of a bare 2.")

(defun nl-ffi--call-checked (fn-name c-symbol &rest args)
  "Call C-SYMBOL through `nl-ffi-call', attributing an unavailable build
to FN-NAME.  Converts the reader's catchable `nelisp-unsupported-
primitive' (a static-reader build where `nl-ffi-call' is `fboundp' but
not really wired) into `nl-ffi-unavailable', exactly as `nl-ffi--invoke'
does for an `ffi:defun'-generated call.  Used only for this file's own
`dlopen'/`dlsym'/`dlerror' calls, whose C-SYMBOL is always a matched
table row (see the table rows added for step 2), so the raw result here
is never Lisp `nil' the way an arbitrary caller-supplied C-SYMBOL can be
in `nl-ffi--invoke'."
  (condition-case _err
      (apply #'nl-ffi-call c-symbol args)
    (nelisp-unsupported-primitive
     (signal 'nl-ffi-unavailable (list fn-name c-symbol)))))

(defun nl-ffi--dlerror-text ()
  "Return `dlerror()''s text, or a fallback string when it has none.
`dlerror()' returns a `const char *' (NULL when there is no pending
error) that `nl-ffi-get-string' turns into a Lisp string; NULL is a
real, boxed 0 here (`dlerror' is a matched table row), never the `nil'
`nl-ffi--invoke' would otherwise read as \"unresolved\" (see this file's
Commentary on why a matched row's result is a real number, not nil)."
  (let ((ptr (nl-ffi--call-checked 'nl-ffi--dlerror-text "dlerror")))
    (if (and (integerp ptr) (> ptr 0))
        (or (nl-ffi-get-string ptr) "dlerror(): empty message")
      "dlerror(): no error text available")))

(defun nl-ffi--dlopen (soname)
  "`dlopen' SONAME with RTLD_NOW|RTLD_LOCAL; return the non-zero handle.
Signals `nl-ffi-library-open-failed' with `dlerror'\='s text (SONAME and
the message, as the condition data) when the open fails -- for example
a SONAME that names no real, reachable shared object.  Callers check
whether `nl-ffi-call' is `fboundp' first (see `ffi:library'); this
function does not, since without that check `nl-ffi--call-checked'
would already signal `nl-ffi-unavailable' for the same reason, just
less directly attributed."
  (let* ((path (nl-ffi--string-to-cstring soname))
         (flags (logior nl-ffi--rtld-now nl-ffi--rtld-local))
         (handle (nl-ffi--call-checked 'ffi:library "dlopen" path flags)))
    (when (or (not (integerp handle)) (zerop handle))
      (signal 'nl-ffi-library-open-failed (list soname (nl-ffi--dlerror-text))))
    handle))

(defun nl-ffi--dlsym (handle symbol)
  "Return the address `dlsym' resolves SYMBOL to against HANDLE, or 0.
0 is `dlsym'\='s own real \"not found\" answer (a C NULL pointer), not a
signal that something here is unavailable -- see
`nl-ffi--resolve-via-dlsym', the only caller, for how a whole search
across every declared library turns a final 0 into
`nl-ffi-unresolved-symbol'."
  (nl-ffi--call-checked 'nl-ffi--dlsym "dlsym" handle
                         (nl-ffi--string-to-cstring symbol)))

(defvar nl-ffi--library-order nil
  "SONAMEs successfully `dlopen'ed via `ffi:library', most-recently-declared
first.  Only libraries actually opened at run time (a SONAME outside
`nl-ffi-known-sonames') ever appear here -- a known-table SONAME never
calls `dlopen' and so is never a `dlsym' candidate.
`nl-ffi--resolve-via-dlsym' walks this list, in order, as the one place
an unmatched `ffi:defun' symbol gets a chance to resolve.")

(defvar nl-ffi--dlsym-cache (make-hash-table :test 'equal)
  "Resolved `dlsym' addresses, keyed by (SONAME . C-SYMBOL).
Populated by `nl-ffi--resolve-via-dlsym' the first time a given
symbol+library pair resolves; a failed pair is never cached, so an
unresolvable symbol pays a fresh (cheap) `dlsym' call every time it is
asked for again, rather than caching a negative answer that a later
`dlopen' of some other, exporting library could no longer overturn.")

(defun nl-ffi--symbol-address-in-library (handle c-symbol)
  "Resolve C-SYMBOL against HANDLE, whichever kind of handle it is.
HANDLE is either a real, positive-integer `dlopen' handle (resolved via
`nl-ffi--dlsym') or an `nl-ffi-loader-open' result (resolved via
`nl-ffi-loader-symbol' -- see packages/nl-ffi/src/nl-ffi-loader.el,
step 3 increment 1); `nl-ffi-loader-handle-p' tells them apart.  Both
return 0, never nil, when C-SYMBOL is not found."
  (if (nl-ffi-loader-handle-p handle)
      (nl-ffi-loader-symbol handle c-symbol)
    (nl-ffi--dlsym handle c-symbol)))

(defun nl-ffi--resolve-via-dlsym (c-symbol)
  "Return the address of C-SYMBOL in some declared library, or 0.
Walks `nl-ffi--library-order', returning the first non-zero resolved
result (via `nl-ffi--symbol-address-in-library') and caching it in
`nl-ffi--dlsym-cache' keyed by \(SONAME . C-SYMBOL\).  Returns 0 -- the
same \"not found\" sentinel `dlsym' itself would return -- when no
declared library exports C-SYMBOL, including when zero libraries have
been declared at all."
  (let ((sonames nl-ffi--library-order)
        (found 0))
    (while (and sonames (zerop found))
      (let* ((soname (car sonames))
             (key (cons soname c-symbol))
             (cached (gethash key nl-ffi--dlsym-cache)))
        (if cached
            (setq found cached)
          (let* ((handle (nl-ffi-library-handle soname))
                 (addr (and handle (nl-ffi--symbol-address-in-library handle c-symbol))))
            (when (and addr (not (zerop addr)))
              (puthash key addr nl-ffi--dlsym-cache)
              (setq found addr)))))
      (setq sonames (cdr sonames)))
    found))

(defun nl-ffi--ptr-call-invoke (fn-name c-symbol arg-types ret-type raw-args)
  "Resolve C-SYMBOL via `dlsym' and call it through `ptr-call'.
The last resort `nl-ffi--invoke' reaches once the build-time table has
already said C-SYMBOL is not one of its rows.  RAW-ARGS are the same
already-converted values `nl-ffi--invoke' would otherwise have handed
straight to `nl-ffi-call' (see `nl-ffi--convert-arg') -- fine as they
are for this path too, since every `nl-ffi-types' member other than
`:float'/`:double' is already a plain integer/address by the time it
gets here.

Checks, each signalling a distinct named condition, before any real
`dlsym'/`ptr-call' work:

  1. `nl-ffi-dlsym-float-unsupported' when RET-TYPE or any of ARG-TYPES
     is `:float'/`:double'.  `ptr-call' has one dispatch arm
     (`(:lit \"ptr-call\")' in scripts/nelisp-standalone-build.el):
     every argument is read with `wf_argval' and the result boxed with
     `wf_write_int', unconditionally -- there is no f64-classed sibling
     the way a direct table row's SIG plist gives `nl-ffi-call' itself
     (`:args'/`:ret' `f64'). That per-row distinction works because the
     table is closed and known at BUILD time, so the compiler can pick
     the marshalling per symbol; a `dlsym'ed address is only known at
     RUN time, so calling a double-taking function this way would need
     either a new f64-aware `ptr-call' variant or a way to tell it, per
     call, which argument/return slots are `f64' -- neither exists yet
     (see this file's Roadmap section above and the package README).
  2. `nl-ffi-too-many-arguments' when (length ARG-TYPES) exceeds
     `nl-ffi--ptr-call-max-args' (six) -- `ptr-call' always reads exactly
     six argument slots after the address; there is nowhere to put a
     seventh.

Then C-SYMBOL is resolved with `nl-ffi--resolve-via-dlsym'; a 0 result
(`dlsym'\='s own \"not found\", the same sentinel it would have been at
the raw C level) signals `nl-ffi-unresolved-symbol', exactly the
condition a build-time-table miss already signalled before this path
existed -- calling `ptr-call' with a 0 address is not a fallback, it is
undefined behaviour (a call through a null function pointer), so that
check runs before, never after, the call it guards.  Once resolved,
RAW-ARGS are padded with trailing zeros to `nl-ffi--ptr-call-max-args'
(exactly what every other `ptr-call' caller in this runtime already
does -- see `nelisp-native-load-raw-call') and the raw `ptr-call' result
is returned as-is; `nl-ffi--invoke' still owns turning a `:void'
RET-TYPE into nil."
  (when (memq ret-type '(:float :double))
    (signal 'nl-ffi-dlsym-float-unsupported (list fn-name c-symbol ret-type)))
  (dolist (ty arg-types)
    (when (memq ty '(:float :double))
      (signal 'nl-ffi-dlsym-float-unsupported (list fn-name c-symbol ty))))
  (when (> (length arg-types) nl-ffi--ptr-call-max-args)
    (signal 'nl-ffi-too-many-arguments
            (list fn-name c-symbol (length arg-types) nl-ffi--ptr-call-max-args)))
  (let ((addr (nl-ffi--resolve-via-dlsym c-symbol)))
    (when (zerop addr)
      (signal 'nl-ffi-unresolved-symbol (list fn-name c-symbol)))
    (let ((padded (copy-sequence raw-args)))
      (while (< (length padded) nl-ffi--ptr-call-max-args)
        (setq padded (append padded '(0))))
      (apply #'ptr-call addr padded))))

;;;; --- ffi:library -----------------------------------------------------------

(defconst nl-ffi-known-sonames
  '("libc.so.6" "libm.so.6" "libgnutls.so.30" "libfreetype.so.6"
    "libsqlite3.so.0")
  "SONAMEs present in the dynamic reader's build-time extern table.

Mirrors the SONAME set of `nelisp-standalone--reader-extern-table' in
scripts/nelisp-standalone-build.el as of this package's own version --
this file cannot introspect that table, which lives only in the build
script that produces the reader binary, not in anything the running
reader loads.  Update this list by hand whenever that table's SONAME
set changes.  `ffi:library' branches on membership in exactly this
list (see its docstring): a SONAME here skips `dlopen' entirely and
resolves purely through the build-time table, as before step 2; a
SONAME missing from a stale copy of this list is not rejected any
more -- it now takes the `dlopen' branch too, which typically still
succeeds (`dlopen' on an already-linked SONAME returns a valid handle
to the same in-process library, just with one more reference count),
so a stale mirror now costs one redundant `dlopen' call rather than a
false `nl-ffi-unknown-library' rejection.  `nl-ffi-test-known-sonames-
match-build-table' still keeps this list honest.")

(defun nl-ffi-known-soname-p (soname)
  "Return non-nil when SONAME is one of `nl-ffi-known-sonames'."
  (and (member soname nl-ffi-known-sonames) t))

(defvar nl-ffi--libraries (make-hash-table :test 'equal)
  "Registry of libraries declared via `ffi:library', keyed by SONAME.
Each value is a plist `(:soname SONAME :handle HANDLE)'.  HANDLE is nil
for a SONAME `nl-ffi-known-sonames' already carries (no `dlopen' call is
needed or made -- see `ffi:library'), and the real, non-zero `dlopen'
handle for any other SONAME, since `ffi:library' can now open one.")

(defun nl-ffi-library-handle (soname)
  "Return the `dlopen' handle registered for SONAME, or nil.
Nil for a SONAME `ffi:library' has not (successfully) declared yet, and
for one of `nl-ffi-known-sonames' -- those never call `dlopen' (see
`ffi:library'). A real, non-zero integer for any other SONAME
`ffi:library' has opened."
  (plist-get (gethash soname nl-ffi--libraries) :handle))

(defun ffi:library (soname)
  "Declare SONAME as an FFI library for `ffi:defun' to draw symbols from.

SONAME spelled exactly as the reader's build-time extern table spells
it (for example \"libm.so.6\", not \"libm\" or \"math\") is one of
`nl-ffi-known-sonames' -- already linked into the running binary at
build time -- and resolves every C symbol through `nl-ffi-call''s one
fixed table shared by the whole process (see this file's Commentary),
exactly as before: no `dlopen' call, no library argument on any
`ffi:defun' form, and a symbol still resolves, or does not, purely by
whether the running binary's build already linked it in.

Any OTHER SONAME is a real `dlopen' call (RTLD_NOW|RTLD_LOCAL) as of
step 2 -- see `nl-ffi--dlopen' -- on a build where `nl-ffi-call' actually
works (the dynamic reader).  On the default STATIC reader, where
`nl-ffi-call' is `fboundp' but every call signals the reader's own
`nelisp-unsupported-primitive' (re-signalled here as `nl-ffi-unavailable'
-- see `nl-ffi--dlopen'/`nl-ffi--call-checked'), this now falls back to
`nl-ffi-loader-open' (step 3 increment 1; see packages/nl-ffi/src/nl-ffi-
loader.el) instead of propagating that failure -- SONAME must then be a
real, openable path (that loader does no SONAME search-path resolution;
see its own docstring).  Either way, a symbol an `ffi:defun' form names
that `nl-ffi-call' does not resolve then gets one more chance: `dlsym' or
`nl-ffi-loader-symbol' against this library's handle, whichever kind it
is (see `nl-ffi--symbol-address-in-library'), called through `ptr-call'
(see `nl-ffi--resolve-via-dlsym'/`nl-ffi--ptr-call-invoke').  This is the
whole reason `ffi:library' takes a real handle at all now.

Checks, in order:
  1. `wrong-type-argument' when SONAME is not a string.
  2. `nl-ffi-unavailable' when `nl-ffi-call' is not `fboundp' at all --
     host Emacs, or a NeLisp build without the dynamic reader's opt-in
     extern table.  Checked before any SONAME-specific work: neither the
     table-membership branch below nor a real open attempt means
     anything without a live `nl-ffi-call', and this is the ONLY check
     that still applies uniformly to a known and an unknown SONAME alike
     (`nl-ffi-unknown-library' -- the check that used to run here for
     an unmapped SONAME -- is no longer signalled by this function; see
     its own docstring).
  3. For a SONAME outside `nl-ffi-known-sonames': on the dynamic reader,
     `nl-ffi-library-open-failed', carrying SONAME and `dlerror'\='s text,
     when the real `dlopen' attempt fails -- for example a SONAME that
     names no real, reachable shared object; on the static reader,
     whatever `nl-ffi-loader-open' itself signals
     (`nl-ffi-loader-open-failed'/`nl-ffi-loader-unsupported' -- both,
     like every condition here, `nl-ffi-error' children) when the
     pure-elisp loader cannot map or fully relocate SONAME.

Re-declaring the same SONAME is harmless: an already-registered entry
(the known-table branch, or an earlier successful open) is returned
as-is, with no second open attempt.  Returns SONAME."
  (unless (stringp soname)
    (signal 'wrong-type-argument (list 'stringp soname)))
  (unless (fboundp 'nl-ffi-call)
    (signal 'nl-ffi-unavailable (list soname)))
  (unless (gethash soname nl-ffi--libraries)
    (if (nl-ffi-known-soname-p soname)
        (puthash soname (list :soname soname :handle nil) nl-ffi--libraries)
      (let ((handle
             (condition-case _err
                 (nl-ffi--dlopen soname)
               (nl-ffi-unavailable (nl-ffi-loader-open soname)))))
        (puthash soname (list :soname soname :handle handle) nl-ffi--libraries)
        (push soname nl-ffi--library-order))))
  soname)

;;;; --- ffi:defun ---------------------------------------------------------

(defun nl-ffi--signature-types (signature)
  "Return SIGNATURE (a vector [RET ARG...]) as a list (RET ARG...).
The result's `car' is the return type and its `cdr' the argument type
list -- callers destructure it that way (see `ffi:defun' and
`nl-ffi-compat-call').  Signals `wrong-type-argument' when SIGNATURE is
not a non-empty vector.  Element validity against `nl-ffi-types' is the
caller's job (each does it right after calling this), so a bad keyword
can be attributed to the right name (the `ffi:defun' NAME, or the
`ffi-call' SYMBOL)."
  (unless (and (vectorp signature) (> (length signature) 0))
    (signal 'wrong-type-argument (list 'nl-ffi-signature signature)))
  (append signature nil))

(defmacro ffi:defun (name c-symbol signature &optional doc)
  "Define NAME as a Lisp function calling C-SYMBOL through `nl-ffi-call'.

SIGNATURE is a vector [RET ARG...] of `nl-ffi-types' keywords -- the
first element is C-SYMBOL's C return type, the rest describe its
arguments in order (this is `dev/nelisp-ffi''s/elisp-ffi's convention,
not strategy section 8.1's illustrative labeled-argument sketch).  DOC,
if given, becomes part of NAME's docstring.

NAME takes exactly (length SIGNATURE - 1) arguments.  See
`nl-ffi--convert-arg' for how each is converted and
`nl-ffi--invoke' for the full call contract, including the four named
conditions NAME can signal instead of ever returning a silent nil:

  `nl-ffi-wrong-arity'       -- called with the wrong number of arguments
  `nl-ffi-unknown-type'      -- SIGNATURE used a keyword outside `nl-ffi-types'
  `nl-ffi-unresolved-symbol' -- C-SYMBOL is not linked into this binary
  `nl-ffi-unavailable'       -- this build/host has no `nl-ffi-call' at all

`nl-ffi-unknown-type' for a bad SIGNATURE fires immediately, while this
form is being expanded -- NAME is never defined.  The other three can
only be known once NAME is actually called (see the package README's
\"Symbol resolution\" section for why), so they fire there, every time."
  (let* ((types (nl-ffi--signature-types signature))
         (ret-type (car types))
         (arg-types (cdr types)))
    (dolist (ty types)
      (unless (nl-ffi-type-p ty)
        (signal 'nl-ffi-unknown-type (list name ty))))
    (when (memq :void arg-types)
      (signal 'nl-ffi-unknown-type (list name :void)))
    (let ((full-doc
           (concat (or doc (format "Call C function `%s' via `nl-ffi-call'." c-symbol))
                   (format "\n\nSignature: %S -> %S (arity %d)."
                           arg-types ret-type (length arg-types)))))
      `(defun ,name (&rest nl-ffi--call-args)
         ,full-doc
         (nl-ffi--invoke ',name ,c-symbol ',arg-types ,ret-type nl-ffi--call-args)))))

;;;; --- elisp-ffi / nelisp-ffi compatibility ---------------------------------

(defun nl-ffi-compat-call (library symbol signature &rest args)
  "Call SYMBOL with ARGS using SIGNATURE; elisp-ffi/nelisp-ffi compatible.
LIBRARY is accepted for drop-in compatibility with `elisp-ffi'/
`nelisp-ffi' callers and ignored -- see `ffi:library''s docstring for
why this runtime has no per-library handle to open.  SIGNATURE is a
vector [RET ARG...], exactly as `ffi:defun' takes it; ARGS are the
actual call arguments.  Signals the same conditions as an
`ffi:defun'-generated function -- see `nl-ffi--invoke'."
  (ignore library)
  (let* ((types (nl-ffi--signature-types signature))
         (ret-type (car types))
         (arg-types (cdr types)))
    (dolist (ty types)
      (unless (nl-ffi-type-p ty)
        (signal 'nl-ffi-unknown-type (list symbol ty))))
    (nl-ffi--invoke symbol symbol arg-types ret-type args)))

(unless (fboundp 'ffi-call)
  (defalias 'ffi-call 'nl-ffi-compat-call))
(unless (fboundp 'ffi-get-string)
  (defalias 'ffi-get-string 'nl-ffi-get-string))

(provide 'nl-ffi)

;;; nl-ffi.el ends here
