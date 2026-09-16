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
;; Roadmap (this package is step 1 of 3; see the package README's
;; "Roadmap" section for the full reasoning):
;;
;;   1. THIS PACKAGE.  `ffi:library'/`ffi:defun' over the symbols the
;;      reader's build-time extern table already carries.  No loader, no
;;      new table rows for a new library -- calling a symbol not already
;;      in the table signals `nl-ffi-unresolved-symbol'; the only way to
;;      add one is a new table row and a reader rebuild.
;;   2. (Later.)  Add `dlopen'/`dlsym'/`dlerror' rows to that table and
;;      call the resolved address through `ptr-call', so a new binding no
;;      longer needs a reader rebuild.
;;   3. (Later still.)  Grow the pure-elisp ELF loader
;;      (dev/nelisp-ffi/nelisp-ffi-pure.el, leaf functions only today) so
;;      the default STATICALLY linked reader gains the same capability.
;;      The default binary stays statically linked either way; switching
;;      it to dynamic linking is not the plan.
;;
;; `ffi:library' already takes a SONAME spelled exactly as the table
;; spells it (for example "libm.so.6", not "libm") and validates it
;; against `nl-ffi-known-sonames' NOW, so step 2 has a place to attach a
;; real handle (`nl-ffi-library-handle', nil until then) without changing
;; this call shape.  `ffi:defun''s signature vector never encodes where
;; C-SYMBOL resolves from -- only `nl-ffi--invoke' knows that, and it is
;; the one place step 2 needs to change.
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
     `nelisp-unsupported-primitive' (raised by a static-reader build
     where `nl-ffi-call' is `fboundp' but not really wired) is caught
     and re-signalled as `nl-ffi-unavailable'.
  5. `nl-ffi-unresolved-symbol' when the raw result is Lisp `nil' -- the
     table's own unmatched-name fallback, since every matched row boxes
     a real number.
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
             (nelisp-unsupported-primitive
              (signal 'nl-ffi-unavailable (list fn-name c-symbol))))))
      (when (null result)
        (signal 'nl-ffi-unresolved-symbol (list fn-name c-symbol)))
      (if (eq ret-type :void) nil result))))

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
set changes; `ffi:library' validates against exactly this list, so a
stale copy here reads as a false `nl-ffi-unknown-library' (a real
SONAME this list has not caught up to yet) rather than a false
negative.")

(defun nl-ffi-known-soname-p (soname)
  "Return non-nil when SONAME is one of `nl-ffi-known-sonames'."
  (and (member soname nl-ffi-known-sonames) t))

(defvar nl-ffi--libraries (make-hash-table :test 'equal)
  "Registry of libraries declared via `ffi:library', keyed by SONAME.
Each value is a plist `(:soname SONAME :handle HANDLE)'.  HANDLE is
always nil today (see `ffi:library''s docstring) -- this shape exists
so a later `dlopen'-backed step can fill it in without changing what
`ffi:library' returns or how this table is keyed.")

(defun nl-ffi-library-handle (soname)
  "Return the `dlopen' handle registered for SONAME, or nil.
Always nil today: nothing in this package ever calls `dlopen'.  This
accessor exists so callers, and a later loader step, have one place to
ask -- not a working handle yet."
  (plist-get (gethash soname nl-ffi--libraries) :handle))

(defun ffi:library (soname)
  "Declare SONAME as an FFI library for `ffi:defun' to draw symbols from.

SONAME is spelled exactly as the reader's build-time extern table
spells it (for example \"libm.so.6\", not \"libm\" or \"math\") -- see
`nl-ffi-known-sonames'.  There is no `dlopen' here: `nl-ffi-call'
resolves every C symbol through one fixed table shared by the whole
process (see this file's Commentary), so no `ffi:defun' form needs, or
takes, a library argument, and declaring SONAME here does not make any
new symbol callable -- `ffi:defun' forms naming its symbols still
resolve, or do not, purely by whether the running binary's build
already linked them in.

Checks, in order:
  1. `wrong-type-argument' when SONAME is not a string.
  2. `nl-ffi-unknown-library' when SONAME is not one of
     `nl-ffi-known-sonames' -- this IS a real, immediate check against
     the table's own SONAME set, unlike per-symbol resolution (which
     needs an actual, correctly typed call and so cannot happen until
     an `ffi:defun'-generated function is invoked; see
     `nl-ffi--invoke').
  3. `nl-ffi-unavailable' when `nl-ffi-call' is not `fboundp' at all --
     host Emacs, or a NeLisp build without the dynamic reader's opt-in
     extern table.

Re-declaring the same SONAME is harmless.  Returns SONAME."
  (unless (stringp soname)
    (signal 'wrong-type-argument (list 'stringp soname)))
  (unless (nl-ffi-known-soname-p soname)
    (signal 'nl-ffi-unknown-library (list soname nl-ffi-known-sonames)))
  (unless (fboundp 'nl-ffi-call)
    (signal 'nl-ffi-unavailable (list soname)))
  (unless (gethash soname nl-ffi--libraries)
    (puthash soname (list :soname soname :handle nil) nl-ffi--libraries))
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
