;;; nelisp-native-load.el --- load and run a .neln in-process  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 142 section 6.4: execute a `.neln' artifact's native code inside
;; the running reader, with no linker, no `cc', and no subprocess.
;;
;; The reader already shipped a working loader, but as a demo: the
;; artifact's bytes and its externs' addresses were baked into the binary
;; at build time, so it could run exactly one function.  Everything that
;; had to vary -- which artifact, how big, which symbols, what arity --
;; was fixed before the reader was built.
;;
;; This is the same mechanism driven by data read at run time.  It is
;; ordinary interpreted elisp because the reader exposes every primitive
;; it needs: `syscall-direct' for mmap, `ptr-read-*' / `ptr-write-*' for
;; the pages, `ptr-call' to enter the trampoline, and two builtins added
;; for the loader -- `nelisp--native-symbol-addr' for a runtime symbol's
;; address (`data-addr' is a compile-time form) and `nelisp--native-env'
;; for the environment pointer the boundary calls `frames'.
;;
;; What it does, given `FILE.neln' and a function name:
;;
;;   1. read the artifact and take :native's text, relocs and defun
;;      metadata (the file is a `;;;' header line plus one plist);
;;   2. mmap a code page sized to the text plus one 16-byte stub per
;;      extern, and a page for the boundary slots;
;;   3. copy the text, lay the stubs after it, point each stub at its
;;      runtime symbol and patch each plt32 relocation at the stub;
;;   4. build a trampoline for this defun's arity and slot count, fill
;;      the boundary slots, and enter the body past its prologue;
;;   5. box the arguments, `ptr-call' the trampoline, unbox the result.
;;
;; Limits, stated rather than discovered later: x86_64 only; a defun's
;; parameters and result must be integer, nil or t (the Sexp tags this
;; boxes); at most six parameters, since the trampoline passes them in
;; registers; and every extern must be in
;; `nelisp-native-load-bridgeable-symbols'.  `nelisp-native-load-check'
;; reports which of these an artifact fails before anything is mapped.

;;; Code:

(require 'cl-lib)
(require 'nelisp-native-budget)
(require 'nelisp-runtime-reload-abi)
(require 'nelisp-native-funcall-v2)
(require 'nelisp-native-frame-v2)

(defvar nelisp-native-load--active-calls (make-hash-table :test 'eq)
  "Per-handle native call depth, including nested calls.")

(defun nelisp-native-load--with-active-call (handle function)
  "Run FUNCTION while HANDLE is protected against unload."
  (let ((depth (gethash handle nelisp-native-load--active-calls 0)))
    (puthash handle (1+ depth) nelisp-native-load--active-calls)
    (unwind-protect
        (funcall function)
      (let ((remaining (1- (gethash handle nelisp-native-load--active-calls 1))))
        (if (> remaining 0)
            (puthash handle remaining nelisp-native-load--active-calls)
          (remhash handle nelisp-native-load--active-calls))))))

(defconst nelisp-native-load--port-count 32
  "Number of callback-port entries the reader defines.
Must equal `nelisp-cc-eln-callback7-port-count'; the loader/reader list
equality test and the port-count test both check it.")

(defun nelisp-native-load--port-symbol-names ()
  "Return the callback-port entry symbol names in port order."
  (let ((names nil) (i 0))
    (while (< i nelisp-native-load--port-count)
      (push (format "nelisp_eln_callback_port%d_entry_word" i) names)
      (setq i (1+ i)))
    (nreverse names)))

(defconst nelisp-native-load-bridgeable-symbols
  (append
   '("nelisp_aot_builtin_call1"
    "nelisp_aot_builtin_calln"
    "nl_alloc_symbol"
    "nl_alloc_str"
    "nl_alloc_mut_str"
    "nl_mut_str_push_byte"
    "nl_mut_str_finalize"
    "nl_alloc_bytes"
    "nl_sexp_clone_into"
    "nl_alloc_vector"
    "nl_vector_slot_ptr"
    "nl_vector_set_slot"
    "nelisp_env_lookup_value"
    "nl_root_pin_begin"
    "nl_root_pin_reserve"
    "nl_root_pin_end"
    "nelisp_cons_construct"
    "wf_bytecode_call_gateway"
    "nl_arena_base"
    "nelisp_eln_callback_context_push"
    "nelisp_eln_callback_context_status"
    "nelisp_eln_callback_context_pop"
    "nelisp_eln_fixnum1_callback"
    "nl_eln_callback_context"
    "nelisp_eln_callback7_entry"
    "nelisp_eln_callback7_status"
    "nelisp_eln_callback7_root_mark"
    "nl_eln_callback7_context"
    "nelisp_eln_callback7_entry_word"
    "nelisp_eln_callback1_entry_word"
    "nl_root_pin_begin_v2"
    "nl_root_pin_reserve_v2"
    "nl_root_pin_end_v2"
    "nl_root_pin_slot_v2"
    "nl_native_car_v2"
    "nl_native_cons_v2"
    "nl_native_cdr_v2"
    "wf_bytecode_call_gateway_exit")
   (nelisp-native-load--port-symbol-names)
   '("nl_native_funcall_v2" "nl_native_poll_v2")
   (mapcar #'car nelisp-runtime-reload-gc-contract)
   '("nl_native_frame_v2"))
  "Runtime symbols a stub can be pointed at, in `nelisp--native-symbol-addr' order.

The index is the contract: the builtin selects from a chain of
`data-addr' forms fixed when the reader was built, and nothing links
this list to that one.  They are asserted equal by the test suite.")

(defconst nelisp-native-load-stub-bytes 16
  "Bytes reserved per stub.  The stub itself is 12: movabs rax, imm64; jmp rax.")

(defconst nelisp-native-load-slot-bytes 32
  "Size of one Sexp slot.")

(defconst nelisp-native-load-callback-slots 12
  "Callback boundary slots, matching the object-mode hidden boundary.")

(defconst nelisp-native-load-boundary-slots
  (+ 5 nelisp-native-load-callback-slots)
  "Boundary slots a defun reserves: out, mirror, frames, scratch, name_slot, callbacks.")

(defconst nelisp-native-load-page-bytes 4096)

(defconst nelisp-native-load-artifact-format 'nelisp-private-nelc-v2
  "The artifact container format this loader reads.

A symbol, not a string: `:format' and `:object-format' read back as
symbols while `:arch' reads back as a string, and comparing the wrong
one rejects every artifact.")

(defconst nelisp-native-load-object-format 'nelisp-aot-elf-v1
  "The embedded object layout this loader knows how to map.")

(defconst nelisp-native-load-native-section-versions '(2)
  "Native-section versions whose field meanings this loader agrees with.

Doc 142 section 6.4 asks that a cache be rejected before executing any of
it when the ABI, target or artifact version does not match.  Checking the
version matters more here than for a bytecode lane: a mismatched
`.nelc' misbehaves, a mismatched `.neln' is machine code entered with the
wrong frame layout.")

;; Doc 191 native-runtime slice.  This is deliberately a different container
;; from the object-mode `.neln' lane above.  Object-mode entries receive the
;; hidden NeLisp boundary and may box values; runtime entries are ordinary
;; SysV i64 functions and are allowed to touch the explicitly exported
;; runtime state only.  Keeping the formats separate prevents a raw allocator
;; entry from accidentally being called through the Sexp trampoline.
(defconst nelisp-native-load-raw-artifact-format 'nelisp-private-nelr-v1
  "Container format for a raw runtime-unit artifact.")

(defconst nelisp-native-load-raw-runtime-abi "nelisp-runtime-raw-v1"
  "Calling convention contract for raw runtime-unit entries.

The first implementation is Linux x86_64/SysV only.  Each entry receives and
returns unsigned 64-bit words in the normal six GP argument registers; it has
no hidden boundary slots and must not return a Sexp pointer unless its
descriptor says so.")

;; Full development-runtime units use a separate ABI.  The v1 allocator/GC
;; pair remains loadable for old probes; v2 publishes the whole GC contract
;; through a checked table whose address occupies state+8.
(defvar nelisp-native-load--build-target nil
  "Host generator target; execution derives its target from the running reader.")
(defun nelisp-native-load--windows-p ()
  (if (fboundp 'nelisp--target-os-code)
      (= (nelisp--target-os-code) 2)
    (or (eq system-type 'windows-nt)
        (eq nelisp-native-load--build-target 'windows-x86_64)
        (and (fboundp 'nelisp-standalone-arena-rewrite-target)
             (eq (nelisp-standalone-arena-rewrite-target) 'windows-x86_64)))))
(defun nelisp-native-load--runtime-abi-v2 ()
  (if (nelisp-native-load--windows-p) "nelisp-runtime-raw-v2:win64:v1"
    nelisp-native-load-raw-runtime-abi-v2))
(defun nelisp-native-load--target-v2 ()
  (if (nelisp-native-load--windows-p)
      '(:os windows-nt :arch x86_64 :calling-convention win64 :container-version 3)
    '(:os gnu/linux :arch x86_64 :calling-convention sysv :container-version 3)))

(defconst nelisp-native-load-raw-runtime-abi-v2 "nelisp-runtime-raw-v2"
  "Calling convention contract for full native runtime units.

Entries are ordinary Linux x86_64 SysV i64 functions.  Seven arguments are
supported because GC root and compaction entry points use the stack argument
at position seven.  The runtime table, rather than a single GC function
pointer, selects every externally reachable GC entry.")

(defconst nelisp-native-load-raw-artifact-format-v2 'nelisp-private-nelr-v3
  "Container format with explicit runtime-owned or embedded GC addresses.")

(defconst nelisp-native-load-raw-layout-id-v2
  "nelisp-runtime-reload-layout-v2:x86_64:sexp32:block16:bss-state96:gctable"
  "Versioned state/table layout for full runtime replacement.")

(defconst nelisp-native-load-raw-gc-table-magic #x4e4c474332
  "Magic word stored at offset 8 of a v2 GC entry table.")

(defconst nelisp-native-load-raw-max-arity-v2 7)

(defconst nelisp-native-load-raw-object-format 'nelisp-aot-raw-unit-v1
  "Payload format for raw runtime units.

This is a serialized link-unit text section, not an ET_REL object.  The
relocations and explicit imports are retained in the surrounding plist so the
standalone loader can patch them without invoking a system linker.")

(defconst nelisp-native-load-raw-layout-id
  "nelisp-runtime-reload-layout-v1:x86_64:sexp32:block16:bss-state64"
  "Runtime layout digest shared by the raw loader and the opt-in reader.

This is a versioned contract, rather than a guess based on the current binary:
the state block offsets, Sexp size and allocator header size are part of the
ABI.  A build which changes any of them must publish a new layout id.")

(defconst nelisp-native-load-raw-supported-arch "x86_64")
(defconst nelisp-native-load-raw-max-arity 6)
(defconst nelisp-native-load-raw-page-bytes 4096)

;; The raw resolver is a separate numeric namespace.  The ABI module is loaded
;; after this file in some standalone startup paths, so retain the v1 prefix
;; as a bootstrap fallback and consult the shared v2 list dynamically below.
(defconst nelisp-native-load-raw-runtime-symbols
  '("nl_runtime_reload_state"
    "nl_runtime_reload_install"
    "nl_runtime_reload_alloc_original"
    "nl_runtime_reload_gc_original")
  "Bootstrap prefix for the raw runtime resolver namespace.")

(defun nelisp-native-load--raw-runtime-symbol-index (name)
  "Return the numeric raw resolver index for NAME, or nil.

Keep this as a small list walk: the standalone prelude need not provide
`cl-position' merely to load the runtime reload support."
  (let ((rest (if (and (boundp 'nelisp-runtime-reload-symbols)
                       (listp nelisp-runtime-reload-symbols))
                  nelisp-runtime-reload-symbols
                nelisp-native-load-raw-runtime-symbols))
        (index 0)
        (found nil))
    (while (and rest (null found))
      (when (equal name (car rest))
        (setq found index))
      (setq index (1+ index)
            rest (cdr rest)))
    found))

(defvar nelisp-native-load-raw-mappings nil
  "Raw runtime mappings retained until process exit.

There is intentionally no automatic unload.  Existing native callers may
still contain direct relocations to an old body or return through it; keeping
the old mapping is the only safe first-slice reclamation policy.")

;;;; Tags -------------------------------------------------------------

(defconst nelisp-native-load-tag-nil 0)
(defconst nelisp-native-load-tag-t 1)
(defconst nelisp-native-load-tag-int 2)
(defconst nelisp-native-load-tag-float 3)
(defconst nelisp-native-load-tag-symbol 4)
(defconst nelisp-native-load-tag-string 5)
(defconst nelisp-native-load-tag-cons 7)
(defconst nelisp-native-load-tag-unibyte-string 14)
(defconst nelisp-native-load-tag-bignum 13)

(defconst nelisp-native-load-tag-vector 8)

(defconst nelisp-native-load-scratch-slots 16
  "Elements in the boundary scratch vector.

`scratch' is not a spare Sexp slot: compiled code reaches interior
storage through `nelisp-aot-compiler--scratch-slot', which emits
`(vector-ref-ptr SCRATCH INDEX)' and lowers to `nl_vector_slot_ptr'.
Handing it a zeroed slot makes that dereference a null payload, which is
a fault inside the runtime rather than a diagnosable error.

Sized past the ten levels `--top-level-literal-write-forms' can nest.")

(defconst nelisp-native-load-payload-ptr 16
  "Offset of the byte pointer in a symbol or string Sexp.")

(defconst nelisp-native-load-payload-len 24
  "Offset of the byte length in a symbol or string Sexp.")

;;;; Small helpers ----------------------------------------------------

(defun nelisp-native-load--u32le (value)
  "Return VALUE as four little-endian bytes."
  (let ((u (logand value #xffffffff)))
    (list (logand u #xff)
          (logand (ash u -8) #xff)
          (logand (ash u -16) #xff)
          (logand (ash u -24) #xff))))

(defun nelisp-native-load--byte (string index)
  "Return the INDEXth byte of STRING.

On the standalone runtime `string-byte' -- a byte at a BYTE index.  This
helper used `aref' and its docstring defended that, because the decoder
built each output byte with `char-to-string' and every byte therefore WAS
a character; once the decoder was corrected to emit real bytes, `aref'
and `length' undercounted any payload with a high byte (1121 of 1152 for
one compiled object) and a digest was taken over a truncated buffer.

On host Emacs there is no `string-byte'.  A unibyte string there indexes
by byte through `aref' already, so the fallback is correct rather than
approximate -- and the host half of this file is only ever handed the
same decoded artifacts."
  (if (fboundp 'string-byte)
      (logand (string-byte string index) #xff)
    (logand (aref string index) #xff)))

(defun nelisp-native-load--without-midform-collect (thunk)
  "Call THUNK with the mid-form collector disarmed, then re-arm it.

Every buffer this file hands to native code comes from `alloc-bytes',
whose result is a RAW POINTER: the collector does not know about it, so
it is not a root and the storage behind it can be reclaimed and reused
while the pointer is still live in Elisp.  That is fine as long as no
collection happens between the allocation and the last use -- which was
true when this loader was written, because mid-form collection was
opt-in and off by default.

Doc 152 Stage 5 turned it on by default, and the `while' backedge is one
of its safepoints.  `nelisp-native-load--digest' writes 1152 bytes into
such a buffer one `ptr-write-u8' at a time; a collection partway through
that loop reclaimed the buffer and the digest came back different on
every run -- 27c76247, a88c7b03, 5a66d5e0 for the same intact artifact
whose real digest is 42d6a0bf.  Every-run-different is the signature of
reading storage that has been handed to someone else.

Disarming for the duration is the narrow fix.  The alternative -- making
these buffers real roots -- means giving the collector a way to trace a
raw pointer, which is a change to the collector, not to this file.

`nelisp-thread-gc-inhibit' is NOT used here: that primitive is Tier 3a's
bounded-parallel-section gate and demands 8 MiB of headroom in the
current chunk before it will engage, which is unrelated to what this
needs and far heavier.

Switch 6 disarms, switch 5 re-arms with the same state a fresh boot
installs.  Both are no-ops on a build without them, so this stays safe
on host Emacs."
  (if (fboundp 'nelisp--debug-switch)
      (progn
        (nelisp--debug-switch 6)
        (unwind-protect (funcall thunk)
          (nelisp--debug-switch 5)))
    (funcall thunk)))

(defun nelisp-native-load--digest (bytes)
  "Return the sha256 of BYTES as a lowercase hex string, or nil.

Copies and compresses bytes iteratively, with stack usage independent of
input length. Goes through a raw buffer and `nelisp--sha256-bytes' rather than handing
the string to `nelisp--sha256'.  Strings are UTF-8 internally here, so
the string entry point digests the encoded form: it matches other
sha256 implementations on ASCII and diverges on any byte over 127, which
is most of a compiled object.

Returns nil where the byte digest is unavailable -- on host Emacs, and on
a reader built before it existed -- so a caller can skip the check rather
than fail closed against a digest it cannot compute."
  (when (fboundp 'nelisp--sha256-bytes)
    ;; Guarded here as well as in `nelisp-native-load-exec': this is
    ;; reachable through `nelisp-native-load-check' /
    ;; `nelisp-native-load-artifact' without going through `exec', and the
    ;; buffer below is exactly the one whose reclamation was measured.
    (if (fboundp 'ptr-write-bytes)
        ;; An OS mapping needs no GC root and lets diagnostics retain their
        ;; enable state, collection count, and allocation-debt watermark.
        (let* ((n (string-bytes bytes))
               (size (nelisp-native-load--page-round (max 1 n)))
               (buf (nelisp-native-load--mmap size nil)))
          (unwind-protect
              (progn
                (nelisp-native-load--poke-string buf 0 bytes)
                (nelisp--sha256-bytes buf n))
            (nelisp-native-load--unmap buf size)))
      (nelisp-native-load--without-midform-collect
       (lambda ()
         ;; Older readers retain the guarded per-byte compatibility path.
         (let* ((n (string-bytes bytes))
                (buf (alloc-bytes (if (> n 0) n 1) 1)))
           (nelisp-native-load--poke-string buf 0 bytes)
           (nelisp--sha256-bytes buf n)))))))

(defun nelisp-native-load--sha256 (bytes)
  "Return SHA-256 for BYTES using the strongest available runtime path.

Hosted Emacs normally supplies `secure-hash'.  A standalone reader may load
this file before the optional artifact compatibility layer, so fall back to
the byte-oriented native digest instead of turning a valid raw reload into a
spurious `void-function secure-hash' compile failure."
  (or (and (fboundp 'secure-hash)
           (condition-case nil
               (secure-hash 'sha256 bytes)
             (error nil)))
      (nelisp-native-load--digest bytes)))

(defun nelisp-native-load-sha256 (bytes)
  "Return SHA-256 of byte string BYTES when a supported digest is available."
  (nelisp-native-load--sha256 bytes))

(defun nelisp-native-load-sha256-dependency-context ()
  "Return an ordered vector of SHA helper identities and page-size input.
Function values are opaque identities; consumers must compare them with EQ
without printing, copying, or traversing their definitions."
  (vconcat
   (mapcar (lambda (symbol)
             (and (fboundp symbol) (symbol-function symbol)))
           '(nelisp-native-load-sha256-dependency-context
             nelisp-native-load-sha256 nelisp-native-load--sha256
             nelisp-native-load--digest nelisp-native-load--page-round
             nelisp-native-load--mmap nelisp-native-load--unmap
             nelisp-native-load--windows-p nelisp-native-load--poke-string
             nelisp-native-load--byte nelisp-native-load--without-midform-collect
             secure-hash nelisp--sha256-bytes ptr-write-bytes ptr-write-u8
             string-byte string-bytes aref logand alloc-bytes syscall-direct
             nelisp--debug-switch max + - * / < = 1+ fboundp
             symbol-function mapcar vconcat))
   (when (nelisp-native-load--windows-p)
     (mapcar (lambda (symbol) (and (fboundp symbol) (symbol-function symbol)))
             '(nelisp--sha256 nelisp-native-windows-map nelisp-native-windows-unmap
               nelisp-native-windows-call nelisp-native-windows-free nl-ffi-call
               nelisp--target-os-code nelisp--target-arch-code)))
   (vector nelisp-native-load-page-bytes)))

(defvar nelisp-native-load--running-binary-sha256-cache :unset
  "Cached digest of the executable hosting the runtime reload reader.

The executable cannot be replaced in place on the supported Unix targets, so
one digest is sufficient for all units loaded by a REPL.  `:unset' is kept
distinct from nil: a reader without a readable self image must report that
fact instead of treating an absent identity as a wildcard.")

(defun nelisp-native-load--call-process-file (path)
  "Return the `call-process' DESTINATION that writes stdout to PATH.

The two runtimes disagree about what a bare string means here, and the
disagreement is silent.  The standalone runtime's `call-process' treats a
string DESTINATION as the file to write stdout to -- which is what this
file asked for, and got.  GNU Emacs does not: a string is not one of the
DESTINATION forms it accepts, so it DISCARDS the output while still
returning the child's exit status.  Measured 2026-09-12 under Emacs 30.2:
the helper exited 0 and left a zero-byte file, so this entire fast path
had been returning nil under the host Emacs and every caller was falling
back to its slower in-process hashing without anything saying so.  Emacs
spells the same request `(:file PATH)'.

Detected by capability rather than by `system-type', because what differs
is the runtime, not the operating system."
  (if (fboundp 'nelisp--write-stdout-bytes)
      path
    (list :file path)))

(defun nelisp-native-load--complete-file-bytes-p (path bytes)
  "Return non-nil when BYTES contains all of PATH.

The reader's low-level file primitive has a fixed 8 MiB buffer.  Hashing a
prefix as if it were a complete executable would produce a stable but false
runtime identity, so the in-process fallback must verify the stat size."
  (when (and (stringp bytes) (fboundp 'file-attributes))
    (let* ((attributes (condition-case nil
                           (file-attributes path)
                         (error nil)))
           (size (and attributes
                      (if (fboundp 'file-attribute-size)
                          (file-attribute-size attributes)
                        (nth 7 attributes)))))
      (and (integerp size)
           (>= size 0)
           (= (string-bytes bytes) size)))))

(defconst nelisp-native-load--sha256-helpers
  '(("sha256sum")
    ("/usr/bin/sha256sum")
    ("/bin/sha256sum")
    ("/sbin/sha256sum")
    ("/opt/homebrew/bin/sha256sum")
    ("/usr/local/bin/sha256sum")
    ("shasum" "-a" "256")
    ("/usr/bin/shasum" "-a" "256")
    ("/opt/homebrew/bin/shasum" "-a" "256"))
  "Candidate (PROGRAM . FIXED-ARGS) invocations that print a SHA-256 line.
Tried in order; the first one that exits 0 with a 64-hex-digit line wins.

A single bare `sha256sum' is not enough, for two reasons that stack on
macOS.  Under the host Emacs `call-process' searches `exec-path', but
macOS has no `sha256sum' in /usr/bin or /bin at all -- 26.6.2 ships it in
/sbin, Homebrew coreutils installs it in /opt/homebrew/bin, and a stock
install has only `shasum'.  Inside the standalone reader, which is the
caller this fast path exists for, `call-process' does NOT search PATH, so
a bare name cannot resolve even when the binary is installed.  Absolute
paths are therefore listed explicitly, and `shasum -a 256' -- which
prints the same `<64 hex>  <path>' shape -- closes the stock-macOS case.
Measured 2026-09-12 on macos 26.6.2 arm64.")

(defun nelisp-native-load--sha256-file-external (path)
  "Return PATH's SHA-256 using an external SHA-256 helper, or nil.

This is the fast path for a standalone reader: copying a multi-megabyte ELF
image one byte at a time through the interpreted pointer API is both slow and
unnecessary.  The subprocess sees only the pathname and its output is
validated as a 64-character digest; callers still compare that digest with
the manifest before mapping code.

Helper selection walks `nelisp-native-load--sha256-helpers'; returning nil
when none works leaves the caller on its slower in-process path, which is
why every failure here is swallowed rather than signalled."
  (when (fboundp 'call-process)
    (let ((output (condition-case nil
                      (make-temp-file "nelisp-runtime-reload-sha256-")
                    (error nil)))
          (digest nil))
      (when output
        (unwind-protect
            (let ((candidates nelisp-native-load--sha256-helpers))
              (while (and candidates (null digest))
                (let* ((spec (car candidates))
                       (program (car spec))
                       (fixed-args (cdr spec)))
                  (setq candidates (cdr candidates))
                  (setq digest
                        (condition-case nil
                            (when (= 0 (apply #'call-process program nil
                                              (nelisp-native-load--call-process-file
                                               output)
                                              nil (append fixed-args
                                                          (list path))))
                              (let ((line (nelisp-native-load--read-file output)))
                                (when (and (stringp line) (>= (length line) 64))
                                  (let ((candidate (substring line 0 64)))
                                    (when (string-match-p
                                           "\\`[0-9a-fA-F]\\{64\\}\\'" candidate)
                                      (downcase candidate))))))
                          (error nil)))))
              digest)
          (ignore-errors (delete-file output)))))))

(defun nelisp-native-load--running-binary-sha256 ()
  "Return this process's executable identity, or nil when unavailable.
Windows returns the linked file's SHA-256 with its stamp field zeroed.
Linux retains the whole-file SHA-256; its cache digest semantics do not change.

Linux exposes the running image through `/proc/self/exe'.  The path is an OS
interface, not a repository or machine-specific build path.  The loader does
not accept a caller-selected path here: accepting one would let an artifact
claim the digest of a different executable and defeat the same-binary ABI
check."
  (if (nelisp-native-load--windows-p)
      ;; Windows uses the linker-stamped SHA-256 (digest field zeroed),
      ;; already trusted by cold-image loading. The fixed rodata accessor is
      ;; O(1), returns a fresh string, and accepts no path or mutable cache.
      ;; Refuse older/unstamped readers; never fall back to hashing the PE.
      (condition-case nil
          (let ((digest (and (fboundp 'nelisp--build-digest)
                             (nelisp--build-digest))))
            (and (stringp digest)
                 (string-match-p "\\`[0-9a-f]\\{64\\}\\'" digest)
                 (not (equal digest (make-string 64 ?0)))
                 (copy-sequence digest)))
        (error nil))
    (if (not (eq nelisp-native-load--running-binary-sha256-cache :unset))
        nelisp-native-load--running-binary-sha256-cache
    (let* ((proc-self (and (eq system-type 'gnu/linux)
                           "/proc/self/exe"))
           ;; Resolve the symlink in this process before invoking
           ;; `sha256sum': passing `/proc/self/exe' to a child hashes the
           ;; child (the checksum utility), not this reader.
           (resolved (and proc-self
                          (fboundp 'nelisp--syscall-readlink)
                          (condition-case nil
                              (nelisp--syscall-readlink proc-self)
                            (error nil))))
           (path (or (and (stringp resolved) (> (length resolved) 0)
                          resolved)
                     ;; A direct read is still correct when no readlink
                     ;; primitive exists; the external fast path is skipped
                     ;; below in that case.
                     proc-self))
           (external (and resolved
                          (nelisp-native-load--sha256-file-external path)))
           (bytes (and (not external) path
                       (condition-case nil
                           (cond
                            ((fboundp 'nelisp--syscall-read-file)
                             (nelisp--syscall-read-file path))
                            ((fboundp 'rdf) (rdf path))
                            (t (with-temp-buffer
                                 (set-buffer-multibyte nil)
                                 (insert-file-contents-literally path)
                                 (buffer-string))))
                         (error nil))))
           (digest (or external
                       (and path
                            (nelisp-native-load--complete-file-bytes-p
                             path bytes)
                            (> (string-bytes bytes) 0)
                            (nelisp-native-load--sha256 bytes)))))
      (setq nelisp-native-load--running-binary-sha256-cache digest)
      digest))))

(defun nelisp-native-load-running-binary-sha256 ()
  "Return the SHA-256 identity of the currently running NeLisp executable.
On Windows this is the in-memory build stamp (digest field zeroed at link
time); on Linux it remains the whole-file digest."
  (nelisp-native-load--running-binary-sha256))

(defun nelisp-native-load--read-file (path)
  "Return the contents of PATH as a string."
  (cond
   ((fboundp 'nelisp--syscall-read-file)
    (nelisp--syscall-read-file path))
   ((fboundp 'rdf) (rdf path))
   (t (with-temp-buffer
        (insert-file-contents path)
        (buffer-string)))))

;;;; Artifact parsing -------------------------------------------------

(defun nelisp-native-load-manifest (path)
  "Return the manifest plist stored in the `.neln' artifact at PATH.
The file is one `;;;' header line followed by a single readable plist."
  (let* ((text (nelisp-native-load--read-file path))
         (nl (string-match "\n" text)))
    (unless nl
      (error "nelisp-native-load: %s has no header line" path))
    (car (read-from-string (substring text (1+ nl))))))

(defun nelisp-native-load--defun (native name)
  "Return NAME's entry in NATIVE's :defuns, or nil."
  (let ((rest (plist-get native :defuns))
        (found nil))
    (while (and rest (not found))
      (when (equal (plist-get (car rest) :name) name)
        (setq found (car rest)))
      (setq rest (cdr rest)))
    found))

;;;; Pre-flight -------------------------------------------------------

(defun nelisp-native-load-check (manifest name)
  "Return a list of reasons NAME in MANIFEST cannot be loaded, or nil.
Called before anything is mapped, so a refusal costs no pages and names
every problem at once rather than the first one hit."
  (let* ((native (plist-get manifest :native))
         (meta (and native (nelisp-native-load--defun native name)))
         (externs (plist-get native :extern-symbols))
         (problems nil))
    (cond
     ((not (eq (plist-get manifest :kind) 'neln))
      (setq problems (cons (list :not-neln (plist-get manifest :kind)) problems)))
     ((null native)
      (setq problems (cons (list :no-native-object) problems))))
    ;; Container format, before anything inside it is trusted.
    (unless (equal (plist-get manifest :format)
                   nelisp-native-load-artifact-format)
      (setq problems (cons (list :artifact-format (plist-get manifest :format))
                           problems)))
    (when native
      (unless (equal (plist-get native :arch) "x86_64")
        (setq problems (cons (list :arch (plist-get native :arch)) problems)))
      (unless (equal (plist-get native :object-format)
                     nelisp-native-load-object-format)
        (setq problems (cons (list :object-format
                                   (plist-get native :object-format))
                             problems)))
      (unless (memq (plist-get native :native-section-version)
                    nelisp-native-load-native-section-versions)
        (setq problems (cons (list :native-section-version
                                   (plist-get native :native-section-version))
                             problems)))
      ;; The decoded text must be exactly as long as the artifact says.  A
      ;; short read or a mangled base64 otherwise reaches the code page as
      ;; a truncated function, which is a jump into whatever follows it.
      (let ((declared (plist-get native :text-size))
            ;; `string-bytes', not `length': the decoded text is machine
            ;; code, and `length' counts characters, so a payload with high
            ;; bytes measures short and an intact artifact fails its own
            ;; size check.
            (actual (and (plist-get native :text-base64)
                         (string-bytes (base64-decode-string
                                        (plist-get native :text-base64))))))
        (unless (and (integerp declared) actual (= declared actual))
          (setq problems (cons (list :text-size-mismatch declared actual)
                               problems))))
      ;; Doc 142 section 6.4's artifact-hash check.  `:object-sha256'
      ;; covers the embedded object, so this verifies the artifact was not
      ;; corrupted or edited between compile and load -- the text the
      ;; loader maps comes out of the same manifest.
      ;;
      ;; Through `nelisp--sha256-bytes', never `nelisp--sha256': the latter
      ;; takes a string and digests its internal UTF-8, so it agrees on
      ;; ASCII and disagrees on any byte over 127.  Measured before this
      ;; existed: an object hashed the string way gave a5d68054... where
      ;; every other sha256 gives ddc6b64d...
      (let ((declared (plist-get native :object-sha256))
            (encoded (plist-get native :object-base64)))
        (when (and (stringp declared) (stringp encoded)
                   (fboundp 'nelisp--sha256-bytes))
          (let ((actual (nelisp-native-load--digest
                         (base64-decode-string encoded))))
            (unless (equal declared actual)
              (setq problems (cons (list :object-hash-mismatch declared actual)
                                   problems))))))
      ;; An empty extern set reads back as the symbol nil, not the empty list.
      (when (and externs (not (and (symbolp externs) (null externs))))
        (let ((rest externs)
              (bad nil))
          (while rest
            (unless (member (car rest) nelisp-native-load-bridgeable-symbols)
              (setq bad (cons (car rest) bad)))
            (setq rest (cdr rest)))
          (when bad
            (setq problems (cons (cons :unbridgeable bad) problems))))))
    (if (not meta)
        (cons (list :no-such-defun name) problems)
      (unless (eq (plist-get meta :param-class) 'gp)
        (setq problems (cons (list :param-class (plist-get meta :param-class))
                             problems)))
      (when (> (plist-get meta :arity) 6)
        (setq problems (cons (list :arity-over-six (plist-get meta :arity))
                             problems)))
      (when (memq :rest-required-count meta)
        (let ((required (plist-get meta :rest-required-count)))
          (unless (and (integerp required) (>= required 0)
                       (= (plist-get meta :arity) (1+ required))
                       (eq (plist-get meta :param-repr) 'sexp-ptr)
                       (eq (plist-get meta :return-repr) 'sexp-ptr))
            (setq problems (cons (list :invalid-rest-call-abi required)
                                 problems)))))
      (unless (integerp (plist-get meta :body-offset))
        (setq problems (cons (list :no-body-offset) problems)))
      problems)))

;;;; Trampoline -------------------------------------------------------

(defconst nelisp-native-load--arg-regs '(rdi rsi rdx rcx r8 r9))

(defconst nelisp-native-load--mov-rbp-specs
  '((rax #x48 #x45 #x85)
    (rcx #x48 #x4d #x8d)
    (rdx #x48 #x55 #x95)
    (rsi #x48 #x75 #xb5)
    (rdi #x48 #x7d #xbd)
    (r8 #x4c #x45 #x85)
    (r9 #x4c #x4d #x8d))
  "REX and ModRM bytes for `mov [rbp+disp], REG', 8-bit and 32-bit forms.")

(defun nelisp-native-load--mov-rbp-disp-reg (reg disp)
  "Return the bytes of `mov [rbp+DISP], REG'."
  (let ((spec (assq reg nelisp-native-load--mov-rbp-specs)))
    (unless spec
      (error "nelisp-native-load: no encoding for register %S" reg))
    (let ((rex (nth 1 spec))
          (modrm8 (nth 2 spec))
          (modrm32 (nth 3 spec))
          (short (and (<= -128 disp) (<= disp 127))))
      (append (list rex #x89 (if short modrm8 modrm32))
              (if short
                  (list (logand disp #xff))
                (nelisp-native-load--u32le disp))))))

(defun nelisp-native-load--slot-disp (index)
  "Return the rbp-relative displacement of spill slot INDEX."
  (- (* 8 (1+ index))))

(defun nelisp-native-load--frame-bytes (arity rt-slot-count)
  "Return the synthetic frame size for ARITY parameters and RT-SLOT-COUNT slots."
  (let ((rt-rounded (if (= rt-slot-count 0)
                        0
                      (if (= (logand rt-slot-count 1) 0)
                          rt-slot-count
                        (1+ rt-slot-count)))))
    (+ (* 8 arity)
       (if (= (logand arity 1) 1) 8 0)
       (* 8 rt-rounded))))

(defun nelisp-native-load--trampoline (arity rt-slot-count)
  "Return (:bytes B :imm64-offsets O) for a defun of ARITY and RT-SLOT-COUNT.

The trampoline builds the frame the compiled body expects: it spills the
incoming register arguments to their slots, fills the boundary slots
from immediates patched in after the bytes are placed, and jumps to the
body past its own prologue.  There is one immediate per boundary slot
plus one for the entry address."
  (when (> arity (length nelisp-native-load--arg-regs))
    (error "nelisp-native-load: arity %d exceeds the register arguments" arity))
  (let ((bytes nil)
        (imm64-offsets nil)
        (frame-bytes (nelisp-native-load--frame-bytes arity rt-slot-count))
        (i 0))
    ;; push rbp; mov rbp, rsp
    (setq bytes (list #x55 #x48 #x89 #xe5))
    (when (> frame-bytes 0)
      (setq bytes (append bytes
                          (append (list #x48 #x81 #xec)
                                  (nelisp-native-load--u32le frame-bytes)))))
    (while (< i arity)
      (setq bytes (append bytes
                          (nelisp-native-load--mov-rbp-disp-reg
                           (nth i nelisp-native-load--arg-regs)
                           (nelisp-native-load--slot-disp i))))
      (setq i (1+ i)))
    (setq i 0)
    (while (< i nelisp-native-load-boundary-slots)
      ;; movabs rax, <patched>; mov [rbp+disp], rax
      (setq imm64-offsets (cons (+ (length bytes) 2) imm64-offsets))
      (setq bytes (append bytes (list #x48 #xb8 0 0 0 0 0 0 0 0)))
      (setq bytes (append bytes
                          (nelisp-native-load--mov-rbp-disp-reg
                           'rax
                           (nelisp-native-load--slot-disp (+ arity i)))))
      (setq i (1+ i)))
    ;; movabs rax, <entry>; jmp rax
    (setq imm64-offsets (cons (+ (length bytes) 2) imm64-offsets))
    (setq bytes (append bytes (list #x48 #xb8 0 0 0 0 0 0 0 0 #xff #xe0)))
    (list :bytes bytes :imm64-offsets (nreverse imm64-offsets))))

;;;; Memory -----------------------------------------------------------

(defun nelisp-native-load--page-round (n)
  "Round N up to whole pages, with a floor of one page."
  (let ((pages (/ (+ n (- nelisp-native-load-page-bytes 1))
                  nelisp-native-load-page-bytes)))
    (* nelisp-native-load-page-bytes (if (< pages 1) 1 pages))))

(defun nelisp-native-load--mmap (size executable)
  "Map SIZE bytes anonymously, executable when EXECUTABLE."
  (let ((addr (if (nelisp-native-load--windows-p)
                  (progn (when executable (error "Windows legacy RWX mapping refused"))
                         (require 'nelisp-native-windows)
                         (nelisp-native-windows-map size))
                (syscall-direct 9 0 size (if executable 7 3) 34 -1 0))))
    (when (< addr nelisp-native-load-page-bytes)
      (error "nelisp-native-load: mmap of %d bytes failed (%d)" size addr))
    addr))

(defun nelisp-native-load--protect (address size protection)
  "Apply the shared native mapping boundary; Linux syscall bytes stay unchanged."
  (if (nelisp-native-load--windows-p)
      (nelisp-native-windows-protect address size protection)
    (syscall-direct 10 address size protection 0 0 0)))
(defun nelisp-native-load--unmap (address size)
  "Release through the same owner used to allocate ADDRESS."
  (if (nelisp-native-load--windows-p)
      (nelisp-native-windows-unmap address size)
    (syscall-direct 11 address size 0 0 0 0)))

(defun nelisp-native-load-map-anonymous (size executable)
  "Map SIZE bytes anonymously (zero-filled), executable when EXECUTABLE.
Public entry to `nelisp-native-load--mmap' for consumers that need native
memory outside the GC arena (Doc 210 handler substrate)."
  (nelisp-native-load--mmap size executable))

(defun nelisp-native-load--poke-bytes (addr offset bytes)
  "Write the list BYTES into ADDR at OFFSET."
  (let ((i offset)
        (rest bytes))
    (while rest
      (ptr-write-u8 addr i (car rest))
      (setq i (1+ i))
      (setq rest (cdr rest)))))

(defun nelisp-native-load--poke-string (addr offset string)
  "Write STRING's bytes into ADDR at OFFSET.

`string-bytes', not `length': STRING is decoded binary, and `length'
counts characters, which undercounts any payload with a high byte.  A
short write here reaches the code page as a truncated function."
  (let ((n (string-bytes string)))
    (if (fboundp 'ptr-write-bytes)
        (unless (= (ptr-write-bytes (+ addr offset) string) n)
          (error "nelisp-native-load: short native byte copy"))
      (let ((i 0))
        (while (< i n)
          (ptr-write-u8 addr (+ offset i) (nelisp-native-load--byte string i))
          (setq i (1+ i)))))))

(defun nelisp-native-load--zero-slot (addr)
  "Write nil (four zero words) into the Sexp slot at ADDR."
  (ptr-write-u64 addr 0 0)
  (ptr-write-u64 addr 8 0)
  (ptr-write-u64 addr 16 0)
  (ptr-write-u64 addr 24 0))

;;;; Boxing -----------------------------------------------------------

(defun nelisp-native-load--string-bytes (string)
  "Return STRING's bytes, refusing anything that is not one byte per character.
`aref' gives a character; the runtime stores strings UTF-8 internally, so
a character over 255 is more than one byte on the other side and writing
its low byte would hand the runtime a different string than was asked
for.  Refusing is the honest option until this encodes."
  (let ((i 0)
        (n (length string))
        (bytes nil))
    (while (< i n)
      (let ((c (aref string i)))
        (when (> c 255)
          (error "nelisp-native-load: %S has a character past 255 at %d"
                 string i))
        (setq bytes (cons c bytes)))
      (setq i (1+ i)))
    (nreverse bytes)))

(defconst nelisp-native-load-max-list-elements 16384
  "Maximum list length accepted by the native object bridge.

This finite limit bounds rooting and prevents cyclic or hostile lists from
turning conversion into an unbounded walk.")

(defun nelisp-native-load--proper-list-elements (value)
  "Return VALUE's elements in order, refusing dotted, cyclic or huge lists."
  (let ((cursor value)
        (slow value)
        (fast value)
        (count 0)
        (reverse-elements nil))
    (while (consp cursor)
      (setq count (1+ count))
      (when (> count nelisp-native-load-max-list-elements)
        (error "nelisp-native-load: list exceeds the %d element limit"
               nelisp-native-load-max-list-elements))
      (setq reverse-elements (cons (car cursor) reverse-elements))
      (setq cursor (cdr cursor))
      (setq slow (if (consp slow) (cdr slow) slow))
      (setq fast (if (consp fast) (cdr fast) fast))
      (setq fast (if (consp fast) (cdr fast) fast))
      (when (and (consp fast) (eq fast slow))
        (error "nelisp-native-load: cannot box a circular list")))
    (unless (null cursor)
      (error "nelisp-native-load: cannot box an improper list"))
    (nreverse reverse-elements)))

(defun nelisp-native-load-box (addr value &optional env pin-frame)
  "Write VALUE into the GC-root slot ADDR and return ADDR.

Supports integers, nil, t, single-byte strings, interned symbols and proper
finite lists.  Symbol inputs are verified only for predicate calls such as
`symbolp'; symbol identity and general symbol-value roundtrips are unverified.
Uninterned symbols are refused.  Cons cells are allocated by the runtime and
every temporary Sexp stays in the active pinned-root frame.

ADDR must already be a GC-visible root.  ENV and PIN-FRAME are required for
list conversion.  PIN-FRAME remains valid across interpreted calls; ordinary
evaluator root markers do not."
  (cond
   ((integerp value)
    (nelisp-native-load--zero-slot addr)
    (ptr-write-u64 addr 0 nelisp-native-load-tag-int)
    (ptr-write-u64 addr 8 value))
   ((null value)
    (nelisp-native-load--zero-slot addr)
    (ptr-write-u64 addr 0 nelisp-native-load-tag-nil))
   ((eq value t)
    (nelisp-native-load--zero-slot addr)
    (ptr-write-u64 addr 0 nelisp-native-load-tag-t))
   ((stringp value)
    (let* ((bytes (nelisp-native-load--string-bytes value))
           (len (length bytes))
           ;; One byte minimum: a zero-length allocation has no address
           ;; to hand the runtime.
           (buf (alloc-bytes (if (> len 0) len 1) 1)))
      (nelisp-native-load--poke-bytes buf 0 bytes)
      (nelisp-native-load--zero-slot addr)
      (ptr-call (nelisp-native-load--symbol-addr "nl_alloc_str")
                buf len addr 0 0 0)))
   ((symbolp value)
    (let* ((name (symbol-name value))
           (interned (intern-soft name)))
      (unless (eq value interned)
        (error "nelisp-native-load: cannot box uninterned symbol %S" value))
      (let* ((bytes (nelisp-native-load--string-bytes name))
             (len (length bytes))
             (buf (alloc-bytes (if (> len 0) len 1) 1)))
        (nelisp-native-load--poke-bytes buf 0 bytes)
        (nelisp-native-load--zero-slot addr)
        (ptr-call (nelisp-native-load--symbol-addr "nl_alloc_symbol")
                  buf len addr 0 0 0))))
   ((consp value)
    (unless (and (integerp env) (> env 0))
      (error "nelisp-native-load: list boxing requires the active runtime env"))
    (unless (and (integerp pin-frame) (> pin-frame 0))
      (error "nelisp-native-load: list boxing requires a pinned-root frame"))
    (let ((rest (nreverse (nelisp-native-load--proper-list-elements value)))
          (item-slot (nelisp-native-load--pin-reserve env pin-frame)))
      (nelisp-native-load--zero-slot addr)
      (while rest
        (nelisp-native-load-box item-slot (car rest) env pin-frame)
        ;; ADDR is the rooted tail and also the destination.  The constructor
        ;; consumes the cdr before replacing its slot.
        (ptr-call (nelisp-native-load--symbol-addr "nelisp_cons_construct")
                  item-slot addr addr 0 0 0)
        (setq rest (cdr rest)))
      addr))
   (t (error "nelisp-native-load: cannot box %S" value)))
  addr)

(defun nelisp-native-load-unbox (addr &optional env pin-frame)
  "Return the value in the Sexp slot at ADDR.

Symbols come back interned.  With ENV and PIN-FRAME, conses and strings are
shallow-cloned into the evaluator result slot so their object identity is
preserved. Floats and bignums use the same authenticated copy when the reader
provides it, preserving IEEE bits and bignum identity; otherwise they are
decoded while rooted by the active pin frame. Without that context, strings
retain the legacy byte conversion and conses, floats, and bignums are refused."
  (let ((tag (ptr-read-u64 addr 0)))
    (cond
     ((= tag nelisp-native-load-tag-nil) nil)
     ((= tag nelisp-native-load-tag-t) t)
     ((= tag nelisp-native-load-tag-int) (ptr-read-u64 addr 8))
     ((or (= tag nelisp-native-load-tag-string)
          (= tag nelisp-native-load-tag-unibyte-string))
      (if (and (integerp env) (> env 0)
               (integerp pin-frame) (> pin-frame 0))
          (nelisp--native-unbox-reference addr env pin-frame)
        (nelisp-native-load--payload-string addr)))
     ((= tag nelisp-native-load-tag-symbol)
      (intern (nelisp-native-load--payload-string addr)))
     ((and (memq tag '(6 8 9 10 12 15 18))
           (integerp env) (> env 0) (integerp pin-frame) (> pin-frame 0))
      (nelisp--native-unbox-reference addr env pin-frame))
     ((= tag nelisp-native-load-tag-cons)
      (if (and (integerp env) (> env 0)
               (integerp pin-frame) (> pin-frame 0))
          (nelisp--native-unbox-reference addr env pin-frame)
          (error "nelisp-native-load: cons results require a pinned-root frame")))
     ((and (memq tag (list nelisp-native-load-tag-float
                          nelisp-native-load-tag-bignum))
           (integerp env) (> env 0) (integerp pin-frame) (> pin-frame 0)
           (fboundp 'nelisp--native-unbox-reference))
      (nelisp--native-unbox-reference addr env pin-frame))
     ((= tag nelisp-native-load-tag-float)
      (unless (and (integerp env) (> env 0)
                   (integerp pin-frame) (> pin-frame 0))
        (error "nelisp-native-load: float results require a pinned-root frame"))
      ;; ptr-read-u64 returns a signed 64-bit payload and truncates values
      ;; outside NeLisp's fixnum range. Reassemble the two words as a bignum.
      (nelisp-native-load--decode-float64
       (+ (logand (ptr-read-u32 addr 8) #xffffffff)
          (ash (logand (ptr-read-u32 addr 12) #xffffffff) 32))))
     ((= tag nelisp-native-load-tag-bignum)
      (unless (and (integerp env) (> env 0)
                   (integerp pin-frame) (> pin-frame 0))
        (error "nelisp-native-load: bignum results require a pinned-root frame"))
      (nelisp-native-load--decode-bignum addr))
     (t (error "nelisp-native-load: result tag %d is not one this unboxes" tag)))))

(defun nelisp-native-load--car-v2-reserve (env ticket next-index-cell)
  "Reserve and clear one v2 slot, advancing NEXT-INDEX-CELL.
The cell contains the next slot index relative to TICKET's frame marker."
  (let* ((index (car next-index-cell))
         (slot (ptr-call
                (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2")
                env ticket 0 0 0 0)))
    (unless (and (integerp slot) (> slot 0))
      (error "nelisp-native-load: v2 pinned root frame is full or stale"))
    (nelisp-native-load--zero-slot slot)
    (setcar next-index-cell (1+ index))
    slot))

(defun nelisp-native-load--car-v2-box (addr value env ticket next-index-cell)
  "Box VALUE into rooted ADDR using TICKET for any cons temporaries."
  (if (consp value)
      (let ((rest (nreverse (nelisp-native-load--proper-list-elements value))))
        (nelisp-native-load--zero-slot addr)
        (while rest
          (let ((item-slot
                 (nelisp-native-load--car-v2-reserve
                  env ticket next-index-cell)))
            (nelisp-native-load--car-v2-box
             item-slot (car rest) env ticket next-index-cell)
            (ptr-call (nelisp-native-load--symbol-addr "nelisp_cons_construct")
                      item-slot addr addr 0 0 0))
          (setq rest (cdr rest)))
        addr)
    (nelisp-native-load-box addr value)))

(defun nelisp-native-load--car-v2-status (env ticket input-index output-index)
  "Call the native CAR gateway with authenticated root-slot indices."
  (ptr-call (nelisp-native-load--symbol-addr "nl_native_car_v2")
            env ticket input-index output-index 0 0))

(defun nelisp-native-load-car (value)
  "Return VALUE's CAR through the checked boxed native gateway.
VALUE must be nil or a finite proper list.  The temporary v2 frame roots
all input and result slots until `nelisp-native-load-unbox' has copied any
reference result into an evaluator root.  The v2 ticket authenticates each
slot address; slot 0 is also the active frame marker required by the existing
`nelisp--native-unbox-reference' contract."
  (let* ((env (nelisp--native-env))
         (ticket (ptr-call
                  (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2")
                  env 0 0 0 0 0))
         (next-index-cell (list 0))
         (input nil)
         (output nil))
    (unless (and (integerp ticket) (> ticket 0))
      (error "nelisp-native-load: cannot begin v2 CAR root frame"))
    (unwind-protect
        (progn
          (setq input (nelisp-native-load--car-v2-reserve
                       env ticket next-index-cell))
          (setq output (nelisp-native-load--car-v2-reserve
                        env ticket next-index-cell))
          (nelisp-native-load--car-v2-box
           input value env ticket next-index-cell)
          (let ((status
                 (nelisp-native-load--car-v2-status env ticket 0 1)))
            (cond
             ((= status 0)
              (let ((frame-marker
                     (ptr-call
                      (nelisp-native-load--symbol-addr "nl_root_pin_slot_v2")
                      env ticket 0 0 0 0))
                    (result-slot
                     (ptr-call
                      (nelisp-native-load--symbol-addr "nl_root_pin_slot_v2")
                      env ticket 1 0 0 0)))
                (unless (and (= frame-marker input) (= result-slot output))
                  (error "nelisp-native-load: v2 CAR result slot authentication failed"))
                (nelisp-native-load-unbox result-slot env frame-marker)))
             ((= status 1)
              (signal 'wrong-type-argument (list 'listp value)))
             ((= status 2)
              (error "nelisp-native-load: native CAR rejected its v2 request"))
             (t
              (error "nelisp-native-load: invalid native CAR status %S" status)))))
      (unless (= (ptr-call
                  (nelisp-native-load--symbol-addr "nl_root_pin_end_v2")
                  env ticket 0 0 0 0)
                 1)
        (error "nelisp-native-load: v2 CAR root frame ownership lost")))))

(defun nelisp-native-load--cdr-v2-status (env ticket input-index output-index)
  "Call the native CDR gateway with authenticated root-slot indices."
  (ptr-call (nelisp-native-load--symbol-addr "nl_native_cdr_v2")
            env ticket input-index output-index 0 0))

(defun nelisp-native-load-cdr (value)
  "Return VALUE's CDR through the checked boxed native gateway.
VALUE must be nil or a finite proper list so it can be boxed into the v2
root frame.  The input and output slots stay rooted until the result has been
unboxed.  This Lisp caller handles wrong-type and malformed-request statuses;
it does not provide condition or unwind handoff from generated machine code."
  (let* ((env (nelisp--native-env))
         (ticket (ptr-call
                  (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2")
                  env 0 0 0 0 0))
         (next-index-cell (list 0))
         (input nil)
         (output nil))
    (unless (and (integerp ticket) (> ticket 0))
      (error "nelisp-native-load: cannot begin v2 CDR root frame"))
    (unwind-protect
        (progn
          (setq input (nelisp-native-load--car-v2-reserve
                       env ticket next-index-cell))
          (setq output (nelisp-native-load--car-v2-reserve
                        env ticket next-index-cell))
          (nelisp-native-load--car-v2-box
           input value env ticket next-index-cell)
          (let ((status
                 (nelisp-native-load--cdr-v2-status env ticket 0 1)))
            (cond
             ((= status 0)
              (let ((frame-marker
                     (ptr-call
                      (nelisp-native-load--symbol-addr "nl_root_pin_slot_v2")
                      env ticket 0 0 0 0))
                    (result-slot
                     (ptr-call
                      (nelisp-native-load--symbol-addr "nl_root_pin_slot_v2")
                      env ticket 1 0 0 0)))
                (unless (and (= frame-marker input) (= result-slot output))
                  (error "nelisp-native-load: v2 CDR result slot authentication failed"))
                (nelisp-native-load-unbox result-slot env frame-marker)))
             ((= status 1)
              (signal 'wrong-type-argument (list 'listp value)))
             ((= status 2)
              (error "nelisp-native-load: native CDR rejected its v2 request"))
             (t
              (error "nelisp-native-load: invalid native CDR status %S" status)))))
      (unless (= (ptr-call
                  (nelisp-native-load--symbol-addr "nl_root_pin_end_v2")
                  env ticket 0 0 0 0)
                 1)
        (error "nelisp-native-load: v2 CDR root frame ownership lost")))))

(defun nelisp-native-load-call-exit-frame-begin ()
  "Open an authenticated v2 frame for a future native call-status handoff.

The frame reserves a marker, status, exit tag, exit value, and two staging
slots.  CALLERS must copy a gateway's status and payload into these slots
before returning to Lisp.  This helper does not inspect the shared exit
stash or perform a Lisp signal/throw."
  (let* ((env (nelisp--native-env))
         (begin (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2"))
         (reserve (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2"))
         (slot-at (nelisp-native-load--symbol-addr "nl_root_pin_slot_v2"))
         (end (nelisp-native-load--symbol-addr "nl_root_pin_end_v2"))
         (ticket (ptr-call begin env 0 0 0 0 0))
         (slots nil)
         (keep nil))
    (unless (and (integerp env) (> env 0)
                 (integerp ticket) (> ticket 0))
      (error "nelisp-native-load: cannot begin call-exit v2 root frame"))
    (unwind-protect
        (progn
          (dotimes (_ 6)
            (let ((slot (ptr-call reserve env ticket 0 0 0 0)))
              (unless (and (integerp slot) (> slot 0))
                (error "nelisp-native-load: call-exit v2 root frame is full"))
              (push slot slots)))
          (setq slots (nreverse slots))
          (let ((index 0)
                (rest slots))
            (while rest
              (unless (= (ptr-call slot-at env ticket index 0 0 0)
                         (car rest))
                (error "nelisp-native-load: call-exit slot authentication failed"))
              (nelisp-native-load--zero-slot (car rest))
              (setq index (1+ index)
                    rest (cdr rest))))
          (setq keep t)
          (vector env ticket slots))
      (unless keep
        (unless (= (ptr-call end env ticket 0 0 0 0) 1)
          (error "nelisp-native-load: call-exit frame rollback failed"))))))

(defun nelisp-native-load--call-exit-frame-slot (frame index)
  "Return authenticated INDEX slot in call-exit FRAME, or signal on refusal."
  (unless (and (vectorp frame) (= (length frame) 3)
               (integerp index) (<= 0 index) (< index 6))
    (error "nelisp-native-load: malformed call-exit frame slot request"))
  (let* ((env (aref frame 0))
         (ticket (aref frame 1))
         (slots (aref frame 2))
         (address (ptr-call
                   (nelisp-native-load--symbol-addr "nl_root_pin_slot_v2")
                   env ticket index 0 0 0)))
    (unless (and (integerp address) (> address 0)
                 (= address (nth index slots)))
      (error "nelisp-native-load: stale call-exit slot ticket"))
    address))

(defun nelisp-native-load--call-exit-frame-copy-slot (source destination)
  "Copy one 32-byte Sexp SOURCE slot into rooted DESTINATION without allocation."
  (let ((offset 0))
    (while (< offset 32)
      (ptr-write-u32 destination offset (ptr-read-u32 source offset))
      (setq offset (+ offset 4)))))

(defun nelisp-native-load-call-exit-frame-capture (frame status)
  "Capture gateway STATUS and rooted payload into FRAME before native return.

Staging slots 4 and 5 must already contain the value, or tag and value,
copied by the native adapter before it returns. Status 0 copies the staged
value to the result slot; status 1 copies the staged exit pair. Status 2
refuses the request and leaves all frame slots unchanged. The frame's v2
ticket authenticates every source and destination slot. This operation does
not resume or signal an exit."
  (unless (and (vectorp frame) (= (length frame) 3))
    (error "nelisp-native-load: malformed call-exit frame"))
  (unless (memq status '(0 1 2))
    (error "nelisp-native-load: invalid native call status %S" status))
  (if (= status 2)
      2
    (let* ((env (aref frame 0))
           (ticket (aref frame 1))
           (status-slot (nelisp-native-load--call-exit-frame-slot frame 1))
           (tag-slot (nelisp-native-load--call-exit-frame-slot frame 2))
           (value-slot (nelisp-native-load--call-exit-frame-slot frame 3))
           (tag-source (nelisp-native-load--call-exit-frame-slot frame 4))
           (value-source (nelisp-native-load--call-exit-frame-slot frame 5)))
      ;; Resolve all destinations and validate both sources before the first
      ;; write. ptr-read/ptr-write below cannot allocate or trigger GC.
      (nelisp-native-load-box status-slot status env ticket)
      (if (= status 1)
          (nelisp-native-load--call-exit-frame-copy-slot tag-source tag-slot)
        (nelisp-native-load--zero-slot tag-slot))
      (nelisp-native-load--call-exit-frame-copy-slot value-source value-slot)
      status)))

(defun nelisp-native-load-call-exit-frame-end (frame)
  "Release authenticated call-exit FRAME after its payload has been consumed."
  (unless (and (vectorp frame) (= (length frame) 3))
    (error "nelisp-native-load: malformed call-exit frame"))
  (unless (= (ptr-call
              (nelisp-native-load--symbol-addr "nl_root_pin_end_v2")
              (aref frame 0) (aref frame 1) 0 0 0 0)
             1)
    (error "nelisp-native-load: call-exit frame ownership lost"))
  t)

(defun nelisp-native-load--call-exit-frame-call1 (frame function argument)
  "Call FUNCTION's current function cell with ARGUMENT through the exit gateway.

Return the raw gateway status. The native entry receives the v2 ticket and
fixed function, argument, and output indexes; it resolves all frame slots
through the ticket before dereferencing any of them."
  (unless (and (vectorp frame) (= (length frame) 3) (symbolp function))
    (error "nelisp-native-load: malformed native CALL1 request"))
  (let* ((env (aref frame 0))
         (ticket (aref frame 1))
         (marker (nelisp-native-load--call-exit-frame-slot frame 0))
         (status-slot (nelisp-native-load--call-exit-frame-slot frame 1))
         (result-slot (nelisp-native-load--call-exit-frame-slot frame 2))
         (value-slot (nelisp-native-load--call-exit-frame-slot frame 3))
         (function-slot (nelisp-native-load--call-exit-frame-slot frame 4))
         (argument-slot (nelisp-native-load--call-exit-frame-slot frame 5)))
    (nelisp-native-load-box marker ticket env ticket)
    (nelisp-native-load-box function-slot function env ticket)
    (unless (and (fboundp 'nelisp--native-pin-copy-v2)
                 (= (nelisp--native-pin-copy-v2 env ticket 5 argument)
                    argument-slot))
      (error "nelisp-native-load: CALL1 argument could not be pinned"))
    ;; All six frame slots are reauthenticated after boxing and immediately
    ;; before ptr-call. The native adapter validates the same ticket itself.
    (unless (and (= marker (nelisp-native-load--call-exit-frame-slot frame 0))
                 (= status-slot (nelisp-native-load--call-exit-frame-slot frame 1))
                 (= result-slot (nelisp-native-load--call-exit-frame-slot frame 2))
                 (= value-slot (nelisp-native-load--call-exit-frame-slot frame 3))
                 (= function-slot (nelisp-native-load--call-exit-frame-slot frame 4))
                 (= argument-slot (nelisp-native-load--call-exit-frame-slot frame 5)))
      (error "nelisp-native-load: CALL1 v2 frame authentication failed"))
    (ptr-call (nelisp-native-load--symbol-addr "wf_bytecode_call_gateway_exit")
              env ticket 4 5 2 0)))

(defun nelisp-native-load--call-exit-frame-result (frame status)
  "Decode normal STATUS or resume a captured signal/throw from FRAME."
  (let* ((env (aref frame 0))
         (marker (nelisp-native-load--call-exit-frame-slot frame 0))
         (kind-slot (nelisp-native-load--call-exit-frame-slot frame 1))
         (result-slot (nelisp-native-load--call-exit-frame-slot frame 2))
         (value-slot (nelisp-native-load--call-exit-frame-slot frame 3)))
    (cond
     ((= status 0)
      (unless (= (nelisp-native-load-unbox kind-slot env marker) 0)
        (error "nelisp-native-load: CALL1 success status mismatch"))
      (nelisp-native-load-unbox result-slot env marker))
     ((= status 1)
      (let* ((kind (nelisp-native-load-unbox kind-slot env marker))
             (tag (nelisp-native-load-unbox result-slot env marker))
             (value (nelisp-native-load-unbox value-slot env marker)))
        (cond
         ((= kind 1)
          (unless (and (symbolp tag) tag)
            (error "nelisp-native-load: captured signal condition is not a non-nil symbol"))
          (signal tag value))
         ((= kind 2) (throw tag value))
         (t (error "nelisp-native-load: invalid CALL1 exit kind %S" kind)))))
     ((= status 2)
      (error "nelisp-native-load: native CALL1 gateway refused the request"))
     (t (error "nelisp-native-load: invalid native CALL1 status %S" status)))))

(defun nelisp-native-load--bytecode-call1 (function argument)
  "Call FUNCTION's current function cell once with ARGUMENT through the VM gateway.

This private bridge is a bounded CALL1 probe, not public compiler admission.
Signal and throw payloads are copied into authenticated v2 roots by the
native adapter before Lisp resumes; this wrapper then resumes the existing
Lisp/VM unwinder exactly once."
  (let ((frame (nelisp-native-load-call-exit-frame-begin)))
    (unwind-protect
        (nelisp-native-load--call-exit-frame-result
         frame (nelisp-native-load--call-exit-frame-call1
                frame function argument))
      (nelisp-native-load-call-exit-frame-end frame))))

(defun nelisp-native-load-raw-v2-call1 (handle function argument)
  "Invoke fixed raw-v2 HANDLE with symbol FUNCTION and ARGUMENT.
This public boundary owns authenticated frame slots and exit decoding."
  (unless (and (listp handle) (eq (plist-get handle :kind) 'raw-runtime-v2)
               (eq (plist-get handle :retained) t)
               (symbolp function) function)
    (error "nelisp-native-load: invalid raw-v2 CALL1 request"))
  (let* ((frame (nelisp-native-load-call-exit-frame-begin))
         (env (aref frame 0)) (ticket (aref frame 1))
         (marker (nelisp-native-load--call-exit-frame-slot frame 0))
         (function-slot (nelisp-native-load--call-exit-frame-slot frame 4))
         (argument-slot (nelisp-native-load--call-exit-frame-slot frame 5)))
    (unwind-protect
        (progn
          (nelisp-native-load-box marker ticket env ticket)
          (nelisp-native-load-box function-slot function env ticket)
          (unless (and (fboundp 'nelisp--native-pin-copy-v2)
                       (= (nelisp--native-pin-copy-v2 env ticket 5 argument)
                          argument-slot))
            (error "nelisp-native-load: CALL1 argument pin failed"))
          (unless (and (= marker (nelisp-native-load--call-exit-frame-slot frame 0))
                       (= function-slot (nelisp-native-load--call-exit-frame-slot frame 4))
                       (= argument-slot (nelisp-native-load--call-exit-frame-slot frame 5)))
            (error "nelisp-native-load: CALL1 frame authentication failed"))
          (nelisp-native-load--call-exit-frame-result
           frame (ptr-call (plist-get handle :entry) env ticket 0 0 0 0)))
      (nelisp-native-load-call-exit-frame-end frame))))

(defun nelisp-native-load--decode-float64 (bits)
  "Decode IEEE-754 binary64 BITS, preserving signed zero.
NaN and infinities are rejected until their Lisp representation is defined."
  (let* ((low (logand bits #xffffffff))
         (high (logand (ash bits -32) #xffffffff))
         (negative (>= high #x80000000))
         (exponent (logand (ash high -20) #x7ff))
         (fraction (+ low (ash (logand high #xfffff) 32))))
    (when (= exponent #x7ff)
      (error "nelisp-native-load: NaN/infinity float result is unsupported (bits=%S)"
             bits))
    (if (and (= exponent 0) (= fraction 0))
        (if negative -0.0 0.0)
      (let* ((fractional-part (/ (float fraction) 4503599627370496.0))
             (significand (if (= exponent 0)
                              fractional-part
                            (+ 1.0 fractional-part)))
             (power (if (= exponent 0) -1022 (- exponent 1023)))
             (value (* significand (expt 2.0 power))))
        (if negative (- value) value)))))

(defconst nelisp-native-load-max-bignum-limbs 16384
  "Maximum 32-bit limbs read while decoding a rooted native bignum.")

(defun nelisp-native-load--decode-bignum (addr)
  "Decode the canonical rooted Bignum at ADDR into a Lisp integer.
The runtime layout is sign@+8, limb pointer@+16, count@+24; limbs are
little-endian u32. The active pin frame keeps ADDR and its limb storage alive."
  (let ((sign (ptr-read-u64 addr 8))
        (limbs (ptr-read-u64 addr 16))
        (count (ptr-read-u64 addr 24)))
    (unless (and (or (= sign 0) (= sign 1))
                 (> limbs 0) (> count 0)
                 (<= count nelisp-native-load-max-bignum-limbs))
      (error "nelisp-native-load: malformed bignum sign/pointer/count: %S"
             (list sign limbs count)))
    (let ((index count) (value 0))
      (while (> index 0)
        (setq index (1- index)
              value (+ (ash value 32) (ptr-read-u32 limbs (* index 4)))))
      (when (= value 0)
        (error "nelisp-native-load: noncanonical zero bignum"))
      (if (= sign 1) (- value) value))))

(defconst nelisp-native-load-max-payload-bytes 1048576
  "Longest string or symbol payload this will decode from a result.

A wrong result Sexp carries a wrong length, and decoding it byte by byte
is a loop that does not end in any time a caller would wait -- the
failure reads as a hang rather than as a bad value, several layers from
whatever produced it.  A megabyte is far past any plausible name or
string a loaded defun returns and far short of a runaway.")

(defun nelisp-native-load--payload-string (addr)
  "Return the byte payload of the symbol or string Sexp at ADDR."
  (let ((ptr (ptr-read-u64 addr nelisp-native-load-payload-ptr))
        (len (ptr-read-u64 addr nelisp-native-load-payload-len))
        (chars nil)
        (i 0))
    (when (= ptr 0)
      (error "nelisp-native-load: payload pointer is null"))
    (when (> len nelisp-native-load-max-payload-bytes)
      (error "nelisp-native-load: payload claims %d bytes, over the %d cap \
-- the result Sexp at %d is not a string this can trust"
             len nelisp-native-load-max-payload-bytes addr))
    (while (< i len)
      (setq chars (cons (ptr-read-u8 ptr i) chars))
      (setq i (1+ i)))
    (apply (function unibyte-string) (nreverse chars))))

;;;; Loading ----------------------------------------------------------

(defun nelisp-native-load--make-scratch-vector (addr)
  "Write a fresh scratch vector Sexp into root slot ADDR.

`nl_alloc_vector' returns the NlVector box; the Sexp that names it is
tag 8 with the box at payload+8, which is the shape the reader's own
`nl_logic_build_scratch' builds and the shape `nl_vector_slot_ptr'
expects -- it derefs payload+8 to reach the box."
  (let ((box (ptr-call (nelisp-native-load--symbol-addr "nl_alloc_vector")
                       nelisp-native-load-scratch-slots 0 0 0 0 0)))
    (ptr-write-u64 addr 0 0)
    (ptr-write-u64 addr 8 0)
    (ptr-write-u64 addr 16 0)
    (ptr-write-u64 addr 24 0)
    (ptr-write-u64 addr 0 nelisp-native-load-tag-vector)
    (ptr-write-u64 addr 8 box)
    (ptr-write-u64 addr 16 0)
    (ptr-write-u64 addr 24 0)
    (when (or (not (integerp box)) (= box 0))
      (error "nelisp-native-load: scratch vector allocation returned %S" box))
    addr))

(defun nelisp-native-load-abi (native)
  "Return `boxed' or `integer' for the unit NATIVE's calling convention.

The artifact metadata does not record this -- the CLI decides by trying
the integer call and falling back when it fails, which is not available
in-process because a wrong guess corrupts rather than errors.  So it is
derived: a defun reaches the boxed boundary only by calling through it,
and every such call leaves an extern behind.  With no externs there is
nothing for the boundary to do, and the parameters arrive as raw i64
with the result in rax.

It is the DISPATCHER externs that mean boxed --
`nelisp_aot_builtin_call1' / `_calln' -- not any extern at all.  A defun
that allocates a vector carries `nl_alloc_vector' and friends without
ever delegating, and answers in a raw register:

  (defun c1 (n) (let ((v (vector 7 8 9)) (i 0)) (if (< i n) 111 222)))

reads as boxed under an any-extern rule, and its raw 111 is then dereferenced
as a Sexp pointer -- a fault at address 111, inside whatever the caller
does with the result rather than anywhere near the cause.  The defun
metadata's `:return-repr' agrees (`raw-i64' / `sexp-ptr' for the cases
that have one), and this matches it on every artifact to hand.

The consequence of the derivation being wrong is a wrong value or a
fault, so a caller that knows better should override :abi in the handle
rather than trust this."
  (let* ((externs (plist-get native :extern-symbols))
         (list (and (listp externs) externs))
         (dispatches
          (seq-find (lambda (name)
                      (and (stringp name)
                           (string-match-p "aot_builtin_call" name)))
                    list)))
    (if dispatches 'boxed 'integer)))

(defun nelisp-native-load--symbol-addr (name)
  "Return the runtime address of NAME."
  (let ((rest nelisp-native-load-bridgeable-symbols)
        (idx 0)
        (found nil))
    (while (and rest (not found))
      (if (equal (car rest) name)
          (setq found idx)
        ;; Callable imports may run after the public `1+' cell changes.
        ;; Keep this capability lookup independent of that cell.
        (setq idx (+ idx 1)))
      (setq rest (cdr rest)))
    (unless found
      (error "nelisp-native-load: no bridge for %s" name))
    (let ((addr (nelisp--native-symbol-addr found)))
      (when (= addr 0)
        (error "nelisp-native-load: %s resolved to 0" name))
      addr)))

(defun nelisp-native-load--pin-begin (env)
  "Begin an exclusive GC-visible native-loader root frame for ENV."
  (ptr-call (nelisp-native-load--symbol-addr "nl_root_pin_begin")
            env 0 0 0 0 0))

(defun nelisp-native-load--pin-reserve (env marker)
  "Reserve a GC-visible Sexp slot in ENV's active pinned frame."
  (let ((slot (ptr-call
               (nelisp-native-load--symbol-addr "nl_root_pin_reserve")
               env marker 0 0 0 0)))
    (unless (and (integerp slot) (> slot 0))
      (error "nelisp-native-load: pinned root frame is busy or full"))
    slot))

(defun nelisp-native-load--pin-end (env marker)
  "Release ENV's pinned frame, refusing stale or foreign markers."
  (unless (= (ptr-call (nelisp-native-load--symbol-addr "nl_root_pin_end")
                       env marker 0 0 0 0) 1)
    (error "nelisp-native-load: pinned root frame ownership lost")))

;;;; Raw runtime units ------------------------------------------------

(defun nelisp-native-load--raw-supported-p ()
  "Return non-nil when the first raw runtime slice can execute here.

The loader may inspect raw metadata on a host Emacs, but execution requires
the standalone reader's in-process mmap, mprotect and six-GP `ptr-call'."
  (and (memq system-type '(gnu/linux windows-nt))
       (or (not (boundp 'system-configuration))
           (not (stringp system-configuration))
           (string-match-p "x86_64\\|amd64" system-configuration))
       (fboundp 'syscall-direct)
       (fboundp 'ptr-call)
       (or (eq system-type 'gnu/linux)
           (and (fboundp 'nl-ffi-call)
                (fboundp 'nelisp--target-os-code)
                (= (nelisp--target-os-code) 2)
                (= (nelisp--target-arch-code) 0)))))

(defun nelisp-native-load--raw-native (manifest)
  "Return the raw native section of MANIFEST, if it has one."
  (or (plist-get manifest :native)
      (plist-get manifest :raw-native)))

(defun nelisp-native-load--raw-exports (native)
  "Return raw function export descriptors in NATIVE."
  (or (plist-get native :exports)
      (plist-get native :symbols)))

(defun nelisp-native-load--raw-export (native name)
  "Return NATIVE's raw export descriptor named NAME."
  (let ((rest (nelisp-native-load--raw-exports native))
        (found nil))
    (while (and rest (not found))
      (let ((entry (car rest)))
        (when (and (stringp (plist-get entry :name))
                   (equal (plist-get entry :name) name)
                   (or (null (plist-get entry :type))
                       (eq (plist-get entry :type) 'func)
                       (equal (plist-get entry :type) "func")))
          (setq found entry)))
      (setq rest (cdr rest)))
    found))

(defun nelisp-native-load--raw-import-name (entry)
  "Return the name in a raw import ENTRY, or nil for malformed input."
  (cond ((stringp entry) entry)
        ((and (listp entry) (stringp (plist-get entry :name)))
         (plist-get entry :name))
        (t nil)))

(defun nelisp-native-load--raw-import-kind (entry)
  "Return the kind in raw import ENTRY, defaulting to `func'."
  (if (stringp entry) 'func (or (plist-get entry :kind) 'func)))

(defun nelisp-native-load--raw-import-abi (entry)
  "Return the declared ABI in raw import ENTRY, or nil if absent."
  (and (listp entry) (plist-get entry :abi)))

(defun nelisp-native-load--raw-bytes (native field)
  "Decode FIELD from NATIVE as a binary string, or return nil."
  (let ((encoded (plist-get native field)))
    (and (stringp encoded)
         (if (and (> (length encoded) 4096)
                  (fboundp 'nelisp--native-runtime-symbol-addr))
             ;; A full GC unit is much larger than an ordinary function.
             ;; The interpreted base64 compatibility path allocates for each
             ;; character; use the same external decoding approach as the
             ;; artifact compiler, retaining strict failure reporting.
             (let* ((input (make-temp-file "nelisp-runtime-base64-"))
                    (output (concat input ".decoded")))
               (unwind-protect
                   (progn
                     (write-region encoded nil input nil 'silent)
                     (unless (= 0 (call-process
                                   "base64" input
                                   (if (fboundp 'nelisp-process-call-process)
                                       output (list :file output))
                                   nil "-d"))
                       (error "nelisp-native-load: base64 decoding failed for %S"
                              field))
                     (nelisp-native-load--read-file output))
                 (ignore-errors (delete-file input) (delete-file output))))
           (base64-decode-string encoded)))))

(defun nelisp-native-load--raw-plist-without (plist key)
  "Copy PLIST while omitting KEY and its value.

The raw artifact digest is over the manifest before its own digest field is
added.  Removing the pair, rather than setting it to nil, preserves that
canonical representation and avoids a self-referential hash."
  (let ((rest plist)
        (result nil))
    (while rest
      (let ((this-key (pop rest))
            (this-value (pop rest)))
        (unless (eq this-key key)
          (setq result (append result (list this-key this-value))))))
    result))

(defun nelisp-native-load--raw-digest (bytes)
  "Return the byte SHA256 of raw BYTES when this runtime can compute it.

Standalone readers expose `nelisp--sha256-bytes'; host Emacs has the ordinary
`secure-hash' path.  Both consume the decoded unibyte payload, so a high-bit
instruction byte is included in the digest rather than silently truncated."
  (nelisp-native-load--sha256 bytes))

;;;; Full runtime-unit metadata --------------------------------------

(defconst nelisp-native-load-raw-v2-data-symbols
  '("nl_alloc_check" "nl_alloc_diag" "nl_arena_base"
    "nl_bind_clone_force" "nl_frame_push_sym0" "nl_frame_push_sym1"
    "nl_freelist_large_bins" "nl_freelist_small_mask"
    "nl_fvcache_disable_lookup" "nl_fvcache_table_base" "nl_gc_diag"
    "nl_gc_loop_ctx" "nl_gc_reclaim_scratch" "nl_gc_start_index"
    "nl_gc_stats" "nl_mxcache_disable_lookup" "nl_mxcache_epoch"
    "nl_mxcache_table_base" "nl_thread_parallel_ctx"
    "nl_gc_conserv_state" "nl_runtime_reload_state")
  "Known shared writable exports used by a full GC/arena unit.

The final import list still comes from the compiled unit.  This list only
classifies a data relocation so the loader can make a return stub instead of
a jump stub; an unlisted data import is rejected during the pre-flight pass.")

(defconst nelisp-native-load-raw-v2-import-contract-version
  "nelisp-runtime-raw-v2-import-v3"
  "Version for the narrow authenticated v2 callable-import extension.")

(defconst nelisp-native-load-raw-v2-call1-contract-version
  "nelisp-runtime-raw-v2-call1-v1"
  "Manifest version for the fixed CALL1 exit-gateway wrapper.")
(defconst nelisp-native-load-raw-v2-call1-import "wf_bytecode_call_gateway_exit")
(defconst nelisp-native-load-raw-v2-call1-entry "nl_native_bytecode_call1_exit")

(defun nelisp-native-load--raw-v2-call1-contract ()
  "Return the exact typed producer and consumer contract for CALL1."
  (list nelisp-native-load-raw-v2-call1-contract-version
        '(:name "wf_bytecode_call_gateway_exit" :kind func :arity 6
          :params (u64 u64 u64 u64 u64 u64) :return u64)
        '(:name "nl_native_bytecode_call1_exit" :kind func :arity 2
          :params (u64 u64) :return u64)
        '(slots 4 5 2 0)))

(defun nelisp-native-load-raw-v2-call1-contract ()
  "Return a copy of the fixed CALL1 contract for source generation."
  (copy-tree (nelisp-native-load--raw-v2-call1-contract)))

(defun nelisp-native-load-raw-v2-contract ()
  "Return a copy of the ordered v2 runtime GC contract."
  (copy-tree (nelisp-native-load--raw-v2-contract)))

(defun nelisp-native-load-root-v2-resume-exit (env ticket exit-index &optional manifest)
  "Resume an exact signal/throw captured in the authenticated exit root triple."
  (unless (and (integerp env) (> env 0)
               (integerp ticket) (> ticket 0)
               (integerp exit-index) (> exit-index 0)
               (< (+ exit-index 2) 16384))
    (error "nelisp-native-load: arithmetic exit scalar inputs rejected"))
  (let* ((addresses (nelisp-native-load-root-v2-addresses manifest))
         (slot (plist-get addresses :slot)))
    (unless (and (eql env (plist-get addresses :environment))
                 (integerp slot) (> slot 0))
      (error "nelisp-native-load: arithmetic exit environment rejected"))
    (let* (
         (marker (ptr-call slot env ticket 0 0 0 0))
         (kind-slot (ptr-call slot env ticket exit-index 0 0 0))
         (tag-slot (ptr-call slot env ticket (1+ exit-index) 0 0 0))
         (value-slot (ptr-call slot env ticket (+ exit-index 2) 0 0 0)))
    (unless (and (integerp marker) (> marker 0)
                 (integerp kind-slot) (> kind-slot 0)
                 (integerp tag-slot) (> tag-slot 0)
                 (integerp value-slot) (> value-slot 0))
      (error "nelisp-native-load: arithmetic exit roots rejected"))
    (let ((kind (nelisp-native-load-unbox kind-slot env marker))
          (tag (nelisp-native-load-unbox tag-slot env marker))
          (value (nelisp-native-load-unbox value-slot env marker)))
      (unless (integerp kind)
        (error "nelisp-native-load: arithmetic exit kind rejected"))
      (cond ((= kind 1) (unless (and (symbolp tag) tag)
                         (error "nelisp-native-load: malformed arithmetic signal"))
             (signal tag value))
            ((= kind 2) (throw tag value))
            (t (error "nelisp-native-load: arithmetic exit kind rejected")))))))

(defun nelisp-native-load-root-v2-addresses (&optional manifest)
  "Return the environment and authenticated fixed root-v2 runtime addresses.
The returned plist has :environment, :begin, :reserve, :end, and :slot fields."
  (require 'nelisp-runtime-reload-abi)
  (when (and manifest
             (or (nelisp-native-load--rooted-cfg-safe-v3-manifest-p manifest)
                 (plist-get manifest :native-rooted-cfg-contract-version))
             (not (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest)))
    (error "nelisp-native-load: generic rooted-CFG contract rejected before root address use"))
  (let ((contract (nelisp-native-load--raw-v2-contract))
        (names '("nl_root_pin_begin_v2" "nl_root_pin_reserve_v2"
                 "nl_root_pin_end_v2" "nl_root_pin_slot_v2")))
    (unless (and contract (nelisp-native-load--raw-supported-p)
                 (fboundp 'nelisp--native-env)
                 (nelisp-runtime-reload-contract-matches-p))
      (error "nelisp-native-load: root-v2 runtime contract unavailable"))
    (let ((env (nelisp--native-env))
          (addresses (mapcar #'nelisp-native-load--symbol-addr names)))
      (unless (and (integerp env) (> env 0)
                   (cl-every (lambda (address)
                          (and (integerp address) (> address 0)))
                             addresses))
        (error "nelisp-native-load: invalid root-v2 runtime address"))
      (list :environment env :begin (nth 0 addresses) :reserve (nth 1 addresses)
            :end (nth 2 addresses) :slot (nth 3 addresses)))))

(defun nelisp-native-load-root-v2-copy (env ticket index value &optional manifest)
  "Copy VALUE into authenticated root INDEX for ENV and TICKET."
  (when (and manifest
             (or (nelisp-native-load--rooted-cfg-safe-v3-manifest-p manifest)
                 (plist-get manifest :native-rooted-cfg-contract-version))
             (not (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest)))
    (error "nelisp-native-load: generic rooted-CFG contract rejected before root copy"))
  (unless (and (nelisp-native-load--raw-supported-p)
               (fboundp 'nelisp--native-pin-copy-v2)
               (nelisp-runtime-reload-contract-matches-p)
               (integerp env) (> env 0) (integerp ticket) (> ticket 0)
               (integerp index) (>= index 0))
    (error "nelisp-native-load: root-v2 copy boundary unavailable"))
  (let ((addr (nelisp--native-pin-copy-v2 env ticket index value)))
    (unless (and (integerp addr) (> addr 0))
      (error "nelisp-native-load: root-v2 copy failed authentication"))
    addr))

(defun nelisp-native-load--raw-v2-call1-contract-hash ()
  "Hash the fixed CALL1 import and caller contract."
  (nelisp-native-load--sha256
   (prin1-to-string (nelisp-native-load--raw-v2-call1-contract))))

(defun nelisp-native-load-raw-v2-call1-contract-hash ()
  "Return the digest of the fixed CALL1 contract."
  (nelisp-native-load--raw-v2-call1-contract-hash))

(defun nelisp-native-load--raw-v2-call1-import-valid-p (manifest entry)
  "Return non-nil only for ENTRY with the complete typed CALL1 contract."
  (and (equal (plist-get manifest :call1-contract-version)
              nelisp-native-load-raw-v2-call1-contract-version)
       (equal (plist-get manifest :call1-contract-hash)
              (nelisp-native-load--raw-v2-call1-contract-hash))
       (equal (plist-get manifest :call1-producer-validation-version)
              "nelisp-call1-exact-ast-v1")
       (let ((hash (plist-get manifest :call1-producer-ast-sha256)))
         (and (stringp hash) (string-match-p "\\`[0-9a-f]\\{64\\}\\'" hash)))
       (equal (plist-get manifest :call1-caller)
              (list :name nelisp-native-load-raw-v2-call1-entry :arity 2
                    :params '(u64 u64) :return 'u64 :slots '(4 5 2 0)))
       (equal (plist-get entry :name) nelisp-native-load-raw-v2-call1-import)
       (eq (plist-get entry :kind) 'func)
       (equal (plist-get entry :abi) (nelisp-native-load--runtime-abi-v2))
       (= (or (plist-get entry :arity) -1) 6)
       (equal (plist-get entry :params) '(u64 u64 u64 u64 u64 u64))
       (eq (plist-get entry :return) 'u64)))

(defun nelisp-native-load-raw-v2-call1-import-valid-p (manifest entry)
  "Return non-nil when MANIFEST and ENTRY prove the exact CALL1 type."
  (nelisp-native-load--raw-v2-call1-import-valid-p manifest entry))

(defun nelisp-native-load--raw-v2-call1-source-valid-p (forms contract)
  "Accept only the fixed CALL1 wrapper, dummy import, and GC stubs."
  (let ((wrapper '(defun nl_native_bytecode_call1_exit (env ticket)
                    (extern-call wf_bytecode_call_gateway_exit
                                 env ticket 4 5 2 0)))
        (import '(defun wf_bytecode_call_gateway_exit
                   (env ticket slot-function slot-argument slot-status slot-exit)
                   0))
        (gc-forms nil))
    (dolist (entry contract)
      (push (list 'defun (intern (car entry))
                  (cl-loop for i below (cdr entry)
                           collect (intern (format "arg%d" i))) 0)
            gc-forms))
    (and (= (length forms) (+ 2 (length contract)))
         (member wrapper forms) (member import forms)
         (cl-every (lambda (form) (member form forms)) gc-forms)
         (cl-every (lambda (form)
                     (or (equal form wrapper) (equal form import)
                         (member form gc-forms))) forms))))

(defconst nelisp-native-load-native-object-op-contract-version
  "nelisp-native-object-op-v1"
  "Manifest version for the authenticated native object-operation gateway.")

(defconst nelisp-native-load-native-object-opcodes
  '((1 . car) (2 . cdr))
  "Complete opcode-ID allowlist published by the native object-op gateway.")

(defconst nelisp-native-load-rooted-stack-contract-version
  "nelisp-native-rooted-stack-v1")

(defun nelisp-native-load--rooted-stack-contract-hash (imports)
  (nelisp-native-load--sha256
   (prin1-to-string (list nelisp-native-load-rooted-stack-contract-version
                          "nl_native_stack_probe_v1" (sort (copy-sequence imports) #'string<) 256
                          '(u64 u64 u64 u64) 'u64
                          (nelisp-native-load--runtime-abi-v2)))))

(defun nelisp-native-load--native-object-op-contract-hash ()
  "Hash the exact gateway and opcode contract published in v2 manifests."
  (nelisp-native-load--sha256
   (prin1-to-string
    (list nelisp-native-load-native-object-op-contract-version
          '("nl_native_car_v2" "nl_native_cdr_v2")
          nelisp-native-load-native-object-opcodes
          '(0 success 1 wrong-type 2 malformed 3 unsupported-opcode)))))

(defconst nelisp-native-load-raw-v2-bridgeable-imports
  '("nl_native_car_v2" "nl_native_cdr_v2" "nl_native_cons_v2" "nl_native_funcall_v2" "nl_native_poll_v2" "nl_native_frame_v2")
  "Exact v2 native bridge imports backed by the binary symbol-address table.

Root-pin operations remain host-controlled; raw units receive an authenticated
ticket and slot indices and may call only the CAR gateway.")

(defconst nelisp-native-load-raw-v2-conditional-slot-import
  "nl_root_pin_slot_v2"
  "The sole pointer-returning import admitted by the conditional contract.")

(defconst nelisp-native-load-raw-v2-conditional-contract-version
  "nelisp-native-rooted-conditional-v1")

(defconst nelisp-native-load-raw-v2-rooted-branch-contract-version
  "nelisp-native-rooted-branch-v1")

(defconst nelisp-native-load-raw-v2-rooted-branch-join-contract-version
  "nelisp-native-rooted-branch-join-v1")

(defun nelisp-native-load--rooted-branch-contract-hash ()
  (nelisp-native-load--sha256
   (prin1-to-string
    (list nelisp-native-load-raw-v2-rooted-branch-contract-version
          "nl_native_rooted_branch_probe_v1"
          '("nl_native_car_v2" "nl_native_cdr_v2" "nl_root_pin_slot_v2")
          '(u64 u64 u64 u64) '(u64 u64 u64 u64 u64 u64) 'u64 256))))

(defun nelisp-native-load--rooted-branch-join-contract-hash (operation)
  (nelisp-native-load--sha256
   (prin1-to-string
    (list nelisp-native-load-raw-v2-rooted-branch-join-contract-version
          "nl_native_rooted_branch_join_probe_v1" operation
          (sort (list (format "nl_native_%s_v2" operation) "nl_root_pin_slot_v2") #'string<)
          '(u64 u64 u64 u64) '(u64 u64 u64 u64 u64 u64) 'u64 256))))

(defun nelisp-native-load--raw-v2-contract ()
  "Return the shared GC contract, or nil when its ABI module is unavailable."
  (and (boundp 'nelisp-runtime-reload-gc-contract)
       (listp nelisp-runtime-reload-gc-contract)
       nelisp-runtime-reload-gc-contract))

(defun nelisp-native-load--raw-v2-symbols ()
  "Return the shared v2 resolver names, or nil when unavailable."
  (and (boundp 'nelisp-runtime-reload-symbols)
       (listp nelisp-runtime-reload-symbols)
       nelisp-runtime-reload-symbols))

(defun nelisp-native-load--raw-v2-import-mode (name)
  "Return NAME's authenticated v2 import resolver mode, or nil."
  (cond
   ((member name nelisp-native-load-raw-v2-bridgeable-imports)
    'native-bridgeable-v1)
   ((assoc name (nelisp-native-load--raw-v2-contract)) 'runtime-gc-bridge-v1)
   ((member name (nelisp-native-load--raw-v2-symbols)) 'resolver)
   (t nil)))

(defun nelisp-native-load--raw-v2-conditional-import-mode (name)
  "Return the isolated conditional-contract mode for NAME, or nil."
  (when (equal name nelisp-native-load-raw-v2-conditional-slot-import)
    'conditional-root-slot-v1))

(defun nelisp-native-load--raw-v2-conditional-import-index (name)
  "Return NAME's fixed runtime bridge index for the conditional contract."
  (and (nelisp-native-load--raw-v2-conditional-import-mode name)
       (cl-position name nelisp-native-load-bridgeable-symbols :test #'equal)))

(defun nelisp-native-load--raw-v2-import-index (name resolver-symbols)
  "Return NAME's index in its authenticated resolver table."
  (if (memq (nelisp-native-load--raw-v2-import-mode name)
            '(native-bridgeable-v1 runtime-gc-bridge-v1))
      (let ((rest nelisp-native-load-bridgeable-symbols)
            (index 0) (found nil))
        (while (and rest (null found))
          (when (equal name (car rest)) (setq found index))
          (setq index (1+ index) rest (cdr rest)))
        found)
    (nelisp-native-load--raw-v2-resolver-index name resolver-symbols)))

(defun nelisp-native-load--raw-v2-import-contract-hash (resolver-symbols)
  "Hash the versioned v2 import extension and its base resolver."
  (nelisp-native-load--sha256
   (prin1-to-string
    (list nelisp-native-load-raw-v2-import-contract-version
          resolver-symbols
          nelisp-native-load-raw-v2-bridgeable-imports
          (nelisp-native-funcall-v2-descriptor)
          (nelisp-native-frame-v2-descriptor) (nelisp-native-frame-v2-hash)))))

(defun nelisp-native-load--raw-v2-contract-hash (&optional contract)
  "Return the digest of CONTRACT's canonical printed representation."
  (let ((print-length nil) (print-level nil))
    (nelisp-native-load--sha256
     (prin1-to-string (or contract
                          (nelisp-native-load--raw-v2-contract)
                          nil)))))

(defun nelisp-native-load--raw-v2-collect-data-addr-names (form)
  "Return unique symbols used by DATA-ADDR forms in FORM."
  (let ((found nil))
    (letrec ((walk
              (lambda (x)
                (cond
                 ((and (consp x) (eq (car x) 'data-addr)
                       (symbolp (cadr x)))
                  (setq found (cons (symbol-name (cadr x)) found)))
                 ((consp x) (funcall walk (car x)) (funcall walk (cdr x)))))))
      (funcall walk form))
    (delete-dups found)))

(defun nelisp-native-load--raw-v2-rewrite-data-addr (form)
  "Turn `(data-addr NAME)' into an imported zero-argument getter call.

The raw linker cannot assume that an anonymously mapped text page is within a
signed 32-bit PC-relative displacement of the executable's BSS.  The source
call remains relocatable (`call NAME'); the v2 loader installs a tiny
`movabs rax, ADDRESS; ret' stub for data imports.  This preserves the pointer
value expected by all existing `ptr-read-*' and `ptr-write-*' forms without
copying or relocating shared runtime state."
  (cond
   ((and (consp form) (eq (car form) 'data-addr)
         (symbolp (cadr form)) (null (cddr form)))
    (list (cadr form)))
   ((consp form)
    (cons (nelisp-native-load--raw-v2-rewrite-data-addr (car form))
          (nelisp-native-load--raw-v2-rewrite-data-addr (cdr form))))
   (t form)))

(defun nelisp-native-load--raw-v2-chunk-rewrite (forms)
  "Apply the canonical chunk-arena rewrite to raw runtime FORMS when present.

Host source compilation normally has the standalone build script loaded.  If
it does not, the caller must have supplied already rewritten source; leaving
the forms untouched here is paired with the fixed-address scan in the v2
compiler, which refuses the unsafe artifact before publication."
  (if (fboundp 'nelisp-standalone--chunk-arena-rewrite)
      (let ((nelisp-standalone--target (if (nelisp-native-load--windows-p)
                                            'windows-x86_64 'linux-x86_64)))
        (nelisp-standalone--chunk-arena-rewrite forms))
    forms))

(defun nelisp-native-load--raw-v2-fixed-address-p (form)
  "Return non-nil when FORM contains a historical fixed arena address."
  (cond
   ;; `nl_arena_init' owns the reservation-size literals.  They are not arena
   ;; addresses and the canonical rewrite deliberately leaves this one form
   ;; untouched; all metadata addresses outside this initializer are still
   ;; required to go through `nl_arena_base'.
   ((and (consp form) (eq (car form) 'defun)
         (eq (cadr form) 'nl_arena_init)) nil)
   ((integerp form)
    (and (>= form 268435456) (< form 272629760)))
   ((consp form)
    (or (nelisp-native-load--raw-v2-fixed-address-p (car form))
        (nelisp-native-load--raw-v2-fixed-address-p (cdr form))))
   (t nil)))

(defun nelisp-native-load--raw-v2-entry-contract (name contract)
  "Return NAME's `(NAME . ARITY)' entry in CONTRACT."
  (let ((rest contract) (found nil))
    (while (and rest (not found))
      (let ((entry (car rest)))
        (when (and (consp entry) (equal (car entry) name))
          (setq found entry)))
      (setq rest (cdr rest)))
    found))

(defun nelisp-native-load--raw-v2-resolver-index (name symbols)
  "Return the numeric v2 resolver index for NAME in SYMBOLS, or nil."
  (let ((rest symbols) (index 0) (found nil))
    (while (and rest (null found))
      (when (equal name (car rest)) (setq found index))
      (setq index (1+ index) rest (cdr rest)))
    found))

(defun nelisp-native-load--raw-reloc-problem (reloc text-length imports)
  "Return a refusal for RELOC, or nil when its shape is loadable.

The first loader slice accepts only text `plt32' or `pc32' relocations against
explicit function imports.  An unresolved data address is never guessed as a
code address; runtime BSS is obtained through a named export instead."
  (let* ((offset (plist-get reloc :offset))
         (type (plist-get reloc :type))
         (symbol (plist-get reloc :symbol))
         (addend (plist-get reloc :addend)))
    (cond
     ((not (and (integerp offset) (>= offset 0)))
      (list :bad-relocation-offset reloc))
     ((not (member type '(plt32 pc32)))
      (list :unsupported-relocation-type type symbol))
     ((not (<= (+ offset 4) text-length))
      (list :relocation-past-text offset text-length))
     ((not (and (integerp addend) (<= -2147483648 addend 2147483647)))
      (list :bad-relocation-addend reloc))
     ((not (and (stringp symbol) (member symbol imports)))
      (list :unlisted-import symbol))
     ((let* ((index (cl-position symbol imports :test #'equal))
             (stub-base (* 16 (/ (+ text-length 15) 16)))
             (displacement (- (+ stub-base (* nelisp-native-load-stub-bytes index) addend) offset)))
        (not (<= -2147483648 displacement 2147483647)))
      (list :relocation-overflow reloc))
     (t nil))))

(defun nelisp-native-load--raw-source-forms (path &optional source)
  "Read all top-level forms from raw runtime SOURCE PATH.

When SOURCE is supplied it is the already-read immutable snapshot used for
the manifest digest and compiler input; this prevents a concurrent editor
write from making the definitions disagree with `:source-sha256'."
  (with-temp-buffer
    (if source
        (insert source)
      (insert-file-contents-literally path))
    (goto-char (point-min))
    ;; A runtime unit is data for the staging compiler.  In particular, do
    ;; not let a reader macro such as `#.' execute code before the strict
    ;; top-level-form check has had a chance to reject it.
    (let ((read-eval nil)
          (forms nil)
          (done nil))
      (while (not done)
        ;; Establish true EOF before calling `read'.  An EOF signalled while
        ;; reading a form is a truncated source unit and must be rejected.
        ;; A temporary buffer need not use the Elisp syntax table, so
        ;; forward-comment alone does not recognize a trailing semicolon.
        (skip-chars-forward " \t\r\n\f")
        (while (eq (char-after) ?\;)
          (forward-line 1)
          (skip-chars-forward " \t\r\n\f"))
        (if (eobp)
            (setq done t)
          (condition-case err
              (push (read (current-buffer)) forms)
            (end-of-file
             (signal (car err) (cdr err))))))
      (nreverse forms))))

(defun nelisp-native-load--raw-ordinary-params-p (params)
  "Return non-nil when PARAMS are ordinary raw ABI parameters."
  (let ((rest params)
        (valid t))
    (while rest
      (let ((param (car rest)))
        (unless (and (symbolp param)
                     (not (memq param
                                '(&rest &optional &key &allow-other-keys
                                  &aux &body &environment))))
          (setq valid nil)))
      (setq rest (cdr rest)))
    valid))

(defun nelisp-native-load--raw-compile-defun-p (form)
  "Return non-nil when FORM is a strict raw-runtime `defun'."
  (and (listp form)
       (eq (car form) 'defun)
       (symbolp (nth 1 form))
       (listp (nth 2 form))
       (nelisp-native-load--raw-ordinary-params-p (nth 2 form))
       (<= (length (nth 2 form)) nelisp-native-load-raw-max-arity)))

(defun nelisp-native-load-raw-compile-file
    (source-path artifact-path &optional layout-id build-id binary-sha256)
  "Compile strict raw runtime SOURCE-PATH to ARTIFACT-PATH.

Only top-level `defun' forms with at most six ordinary parameters are accepted.
The compiler runs with `nelisp-aot-compiler--runtime-entry-params' nil, which
gives the generated function an ordinary SysV i64 entry rather than the
hidden object-mode Sexp boundary.  The artifact retains text, relocations,
  explicit function imports and export metadata; no private data/BSS copy is
  serialized.  This function is a host compiler entry point.  BINARY-SHA256,
  when supplied, identifies the executable that will consume the unit; when
  omitted, the current process executable digest is recorded when available."
  (unless (and (stringp source-path) (file-readable-p source-path))
    (error "nelisp-native-load: raw source is not readable: %S" source-path))
  (unless (and (stringp artifact-path) (> (length artifact-path) 0))
    (error "nelisp-native-load: raw artifact path is empty"))
  (unless (fboundp 'nelisp-aot-compile-to-link-unit)
    (require 'nelisp-aot-compiler))
  (unless (fboundp 'nelisp-aot-compile-to-link-unit)
    (error "nelisp-native-load: raw compiler is unavailable in this runtime"))
  (let* ((source (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally source-path)
                   (buffer-string)))
         (forms (nelisp-native-load--raw-source-forms source-path source))
         (layout (or layout-id nelisp-native-load-raw-layout-id))
         (build (or build-id
                    (and (boundp 'nelisp--cli-version)
                         nelisp--cli-version)
                    "unknown"))
         (binary (or binary-sha256
                     (nelisp-native-load--running-binary-sha256)))
         (unit nil))
    (unless forms
      (error "nelisp-native-load: raw source has no top-level defun"))
    (dolist (form forms)
      (unless (nelisp-native-load--raw-compile-defun-p form)
        (error "nelisp-native-load: unsupported raw top-level form: %S"
               (if (consp form) (car form) form))))
    (let ((nelisp-aot-compiler--runtime-entry-params nil))
      (setq unit
            (nelisp-aot-compile-to-link-unit
             (cons 'seq forms) :arch 'x86_64 :format (if (nelisp-native-load--windows-p) 'coff 'elf))))
    (let* ((text (or (plist-get unit :text) ""))
           (rodata (or (plist-get unit :rodata) ""))
           (data (or (plist-get unit :data) ""))
           (bss (or (plist-get unit :bss-size) 0))
           (text-size (string-bytes text))
           (defuns (plist-get unit :defuns))
           (symbols (plist-get unit :symbols))
           (exports nil)
           (import-names (plist-get unit :extern-symbols))
           (imports nil)
           (object-sha256 (nelisp-native-load--sha256 text))
           (base nil)
           (manifest nil))
      (when (> (string-bytes rodata) 0)
        (error "nelisp-native-load: raw runtime source cannot carry rodata"))
      (when (or (> (string-bytes data) 0) (> bss 0))
        (error "nelisp-native-load: raw runtime source cannot carry private data/BSS"))
      (dolist (def defuns)
        (let* ((name (plist-get def :name))
               (sym (let ((rest symbols) (found nil))
                      (while (and rest (not found))
                        (when (equal (plist-get (car rest) :name) name)
                          (setq found (car rest)))
                        (setq rest (cdr rest)))
                      found)))
          (unless (and sym (integerp (plist-get sym :value)))
            (error "nelisp-native-load: raw defun has no text symbol: %S" name))
          (push (list :name name
                      :value (plist-get sym :value)
                      :size (or (plist-get sym :size)
                                (plist-get def :size) 0)
                      :type 'func
                      :abi nelisp-native-load-raw-runtime-abi
                      :arity (or (plist-get def :arity) 0)
                      :return 'u64)
                exports)))
      (setq exports (nreverse exports))
      (dolist (import import-names)
        (unless (stringp import)
          (error "nelisp-native-load: raw import is not a name: %S" import))
        (push (list :name import :kind 'func
                    :abi nelisp-native-load-raw-runtime-abi)
              imports))
      (setq imports (nreverse imports))
      (setq base
            (list :format nelisp-native-load-raw-artifact-format
                  :kind 'raw-runtime
                  :runtime-abi nelisp-native-load-raw-runtime-abi
                  :layout-id layout
                  :arch nelisp-native-load-raw-supported-arch
                  :build-id build
                  :binary-sha256 binary
                  :source (expand-file-name source-path)
                  :source-sha256 (nelisp-native-load--sha256 source)
                  :runtime-opt-in t
                  :native
                  (list :raw-abi nelisp-native-load-raw-runtime-abi
                        :object-format nelisp-native-load-raw-object-format
                        :text-size text-size
                        :text-base64 (base64-encode-string text t)
                        :object-sha256 object-sha256
                        :object-size text-size
                        :exports exports
                        :symbols exports
                        :imports imports
                        :relocs (plist-get unit :relocs)
                        :data-size 0
                        :bss-size 0)))
      ;; The artifact hash covers the canonical manifest without its own hash.
      ;; This is stable across a write/read round trip and detects metadata
      ;; edits without creating a self-referential digest.
      (setq manifest
            (append base
                    (list :artifact-sha256
                          (nelisp-native-load--sha256
                           (prin1-to-string base)))))
      (let ((parent (file-name-directory (expand-file-name artifact-path))))
        (when parent (make-directory parent t)))
      ;; Publish the complete manifest in one rename.  A reader that races a
      ;; source compilation must see either the previous unit or a complete
      ;; unit whose canonical hash matches, never a half-written plist.
      (let* ((absolute-artifact (expand-file-name artifact-path))
             (parent (file-name-directory absolute-artifact))
             (temporary (make-temp-file
                         (expand-file-name ".nelr-staging-" parent)
                         nil ".tmp")))
        (unwind-protect
            (progn
              (let ((coding-system-for-write 'utf-8-unix))
                (with-temp-file temporary
                  (insert ";;; nelisp-private-nelr-v1\n")
                  (prin1 manifest (current-buffer))
                  (insert "\n")))
              (rename-file temporary absolute-artifact t)
              (setq temporary nil))
          (when (and temporary (file-exists-p temporary))
            (ignore-errors (delete-file temporary)))))
      manifest)))

(defun nelisp-native-load--rooted-stack-normalize-ast (x)
  (if (consp x)
      (cons (nelisp-native-load--rooted-stack-normalize-ast (car x))
            (nelisp-native-load--rooted-stack-normalize-ast (cdr x)))
    (if (and (symbolp x)
             (equal (symbol-name x) "gateway-status")
             (not (eq x (intern-soft "gateway-status"))))
        (or (intern-soft "gateway-status") :rooted-stack-gateway-status)
      x)))

(defun nelisp-native-load--rooted-stack-gc-forms-valid-p (forms contract)
  "Validate existing GC stub FORMS exactly against CONTRACT without cloning it."
  (let ((seen nil) (valid (= (length forms) (length contract))))
    (dolist (form forms valid)
      (let* ((name (and (consp form) (symbolp (cadr form))
                        (symbol-name (cadr form))))
             (entry (and name (cl-find name contract :key #'car :test #'equal)))
             (args (and entry
                        (cl-loop for i below (cdr entry) collect
                                 (intern (format "arg%d" i))))))
        (unless (and entry (not (member name seen))
                     (= (length form) 4)
                     (eq (car form) 'defun)
                     (equal (nth 2 form) args)
                     (equal (nthcdr 3 form) '(0)))
          (setq valid nil))
        (when name (push name seen))))))

(defun nelisp-native-load--rooted-cfg-provider-owner-valid-p (mode)
  "Check source-owned MODE identities before any provider snapshot executes.
OFF retains the existing arithmetic provider. ON requires the complete public
guarded owner gate; no caller predicate or native capability is accepted."
  (cond
   ((memq mode '(nil off))
    (require 'nelisp-native-arithmetic-v2)
    (when (fboundp 'nelisp-native-arithmetic-v2-owner-valid-p)
      (nelisp-native-arithmetic-v2-owner-valid-p))
    t)
   ((eq mode 'on)
    (require 'nelisp-bytecode-native-guarded-lowering)
    (nelisp-bytecode-native-guarded-lowering-owner-valid-p))
   (t (error "nelisp-native-load: unknown arithmetic guard mode"))))

(defun nelisp-native-load--rooted-cfg-provider-source (mode)
  "Return the exact four OFF or five ON artifact-local definitions."
  (nelisp-native-load--rooted-cfg-provider-owner-valid-p mode)
  (if (eq mode 'on) (nelisp-native-optimization-guard-v1-source 'on)
    (nelisp-native-arithmetic-v2-source)))

(defun nelisp-native-load--rooted-cfg-provider-forms-valid-p (forms entry contract additional &optional cfg-contract)
  "Validate exact local provider forms while retaining the complete GC check."
  (condition-case nil
      (let* ((mode (plist-get cfg-contract :arithmetic-guard-mode))
             (expected (and additional
                            (nelisp-native-load--rooted-cfg-provider-source mode)))
             (provider (and expected (cdr expected)))
             (names (mapcar #'cadr provider))
             (actual (cl-remove-if-not (lambda (form) (memq (cadr form) names)) forms))
             (gc (cl-remove-if (lambda (form) (or (eq form entry) (memq (cadr form) names))) forms)))
        (and (if additional
                 (and (equal additional expected) (= (length provider) (if (eq mode 'on) 5 4))
                      (equal actual provider))
               (null actual))
             (nelisp-native-load--rooted-stack-gc-forms-valid-p gc contract)
             (= (length forms) (+ 1 (length contract) (length provider)))))
    (error nil)))

(defun nelisp-native-load--rooted-cfg-provider-import (contract name)
  "Return NAME's canonical provider descriptor after exact source partition checks.
The enclosing CFG admission must still authenticate the complete contract."
  (condition-case nil
      (when (plist-get contract :additional-source)
        (require 'nelisp-native-arithmetic-v2)
        (let* ((mode (plist-get contract :arithmetic-guard-mode))
               (source (nelisp-native-load--rooted-cfg-provider-source mode))
               (imports (nelisp-native-arithmetic-v2-runtime-imports))
               (locals (mapcar (lambda (form) (symbol-name (cadr form))) (cdr source))))
          (when (and (equal (plist-get contract :additional-source) source)
                     (= (length locals) (if (eq mode 'on) 5 4))
                     (equal (plist-get contract :local-functions) locals)
                     (equal (plist-get contract :runtime-imports) imports))
            (cl-find name imports :key (lambda (record) (plist-get record :name))
                     :test #'equal))))
    (error nil)))

(defun nelisp-native-load--rooted-cfg-provider-import-valid-p (descriptor contract)
  "Check exact provider ABI and the immutable native bridge-table index."
  (let* ((name (plist-get descriptor :name))
         (expected (nelisp-native-load--rooted-cfg-provider-import contract name))
         (index (cl-position name nelisp-native-load-bridgeable-symbols :test #'equal)))
    (and expected (integerp index)
         (eq (plist-get descriptor :address-mode) 'arithmetic-provider-v1)
         (equal (plist-get descriptor :index) index)
         (equal (plist-get descriptor :abi) (nelisp-native-load--runtime-abi-v2))
         (cl-every (lambda (key) (equal (plist-get descriptor key) (plist-get expected key)))
                   '(:name :kind :size :arity :params :return)))))

(defun nelisp-native-load--rooted-cfg-provider-cache-context (manifest)
  "Bind provider memo records to actual helpers and copied public source data."
  (when (plist-get (plist-get manifest :native-rooted-cfg-contract) :additional-source)
    (require 'nelisp-native-arithmetic-v2)
    (require 'cl-seq)
    (nelisp-native-load--rooted-cfg-provider-owner-valid-p
     (plist-get (plist-get manifest :native-rooted-cfg-contract) :arithmetic-guard-mode))
    (cons (mapcar #'symbol-function
                  '(nelisp-native-load--rooted-cfg-provider-owner-valid-p
                    nelisp-native-load--rooted-cfg-provider-source
                    nelisp-native-load--rooted-cfg-provider-import
                    nelisp-native-load--rooted-cfg-provider-import-valid-p
                    nelisp-native-load--rooted-cfg-import-names-valid-p
                    nelisp-native-load--rooted-cfg-provider-cache-context
                    nelisp-native-load--rooted-cfg-provider-cache-context-equal-p
                    nelisp-native-load--rooted-cfg-provider-owner-list-eq-p
                    nelisp-native-load--rooted-cfg-provider-context-data-equal-p
                    nelisp-native-load--raw-v2-rooted-cfg-contract-valid-slow-p
                    cl-find cl-every cl-position mapcar symbol-function plist-get
                    equal eq integerp vectorp consp null car cdr aref length
                    1+ 1- < > <= >= = functionp require error when and unless or cond cl-labels))
          (if (eq (plist-get (plist-get manifest :native-rooted-cfg-contract)
                            :arithmetic-guard-mode) 'on)
              (vector 'guarded-provider-v1 (nelisp-bytecode-native-guarded-lowering-dependency-context))
            (nelisp-native-arithmetic-v2-dependency-context)))))

(defun nelisp-native-load--rooted-cfg-provider-context-data-equal-p (a b)
  "Compare at most 8192 context nodes; function owners remain opaque EQ values."
  (let ((budget 8192))
    (cl-labels ((walk (left right depth)
                  (setq budget (1- budget))
                  (and (>= budget 0) (<= depth 64)
                       (cond
                        ((or (functionp left) (functionp right)) (eq left right))
                        ((and (consp left) (consp right))
                         (and (walk (car left) (car right) (1+ depth))
                              (walk (cdr left) (cdr right) depth)))
                        ((or (consp left) (consp right)) nil)
                        ((and (vectorp left) (vectorp right))
                         (and (= (length left) (length right)) (<= (length left) 256)
                              (let ((index 0) (valid t))
                                (while (and valid (< index (length left)))
                                  (setq valid (walk (aref left index) (aref right index) (1+ depth))
                                        index (1+ index))) valid)))
                        (t (equal left right))))))
      (walk a b 0))))

(defun nelisp-native-load--rooted-cfg-provider-owner-list-eq-p (a b)
  "Compare at most 64 opaque owners without traversing their function bodies."
  (let ((remaining 64) (valid t))
    (while (and valid (> remaining 0) (consp a) (consp b))
      (setq valid (eq (car a) (car b)) a (cdr a) b (cdr b)
            remaining (1- remaining)))
    (and valid (null a) (null b))))

(defun nelisp-native-load--rooted-cfg-provider-cache-context-equal-p (a b)
  "Compare opaque function owners by EQ and bounded public snapshots by EQUAL."
  (condition-case nil
      (if (and (null a) (null b)) t
        (and (consp a) (consp b)
             (nelisp-native-load--rooted-cfg-provider-owner-list-eq-p (car a) (car b))
             (vectorp (cdr a)) (vectorp (cdr b))
             (if (and (= (length (cdr a)) 2) (= (length (cdr b)) 2)
                      (eq (aref (cdr a) 0) 'guarded-provider-v1)
                      (eq (aref (cdr b) 0) 'guarded-provider-v1))
                 (nelisp-native-load--rooted-cfg-provider-context-data-equal-p
                  (aref (cdr a) 1) (aref (cdr b) 1))
             (and (= (length (cdr a)) 10) (= (length (cdr b)) 10)
             (let ((index 0) (valid t))
               (while (and valid (< index 6))
                 (setq valid (eq (aref (cdr a) index) (aref (cdr b) index))
                       index (1+ index)))
               (and valid
                    (nelisp-native-load--rooted-cfg-provider-owner-list-eq-p
                     (aref (cdr a) 6) (aref (cdr b) 6))
                    (equal (aref (cdr a) 7) (aref (cdr b) 7))
                    (equal (aref (cdr a) 8) (aref (cdr b) 8))
                    (equal (aref (cdr a) 9) (aref (cdr b) 9))))))))
    (error nil)))

(defun nelisp-native-load--rooted-cfg-import-names-valid-p (names &optional contract)
  "Validate the narrow generic CFG import set before AOT emission."
  (and (listp names)
       (equal names (sort (delete-dups (copy-sequence names)) #'string<))
       (cl-every
        (lambda (name)
          (if (nelisp-native-load--rooted-cfg-provider-import contract name)
              (integerp (cl-position name nelisp-native-load-bridgeable-symbols :test #'equal))
            (if (equal name "nl_root_pin_slot_v2")
              (and (nelisp-native-load--raw-v2-conditional-import-mode name)
                   (integerp (nelisp-native-load--raw-v2-conditional-import-index name)))
            (and (member name nelisp-native-load-raw-v2-bridgeable-imports)
                 (integerp (cl-position name nelisp-native-load-bridgeable-symbols
                                        :test #'equal))))))
        names)))

(defun nelisp-native-load--rooted-cfg-safe-v3-spec-shape-p (spec)
  "Return non-nil for a bounded, exact safe-v3 compiler spec plist."
  (and (progn
         (require 'nelisp-bytecode-native-rooted-cfg-safe-contract)
         (nelisp-bytecode-native-rooted-cfg-safe-contract-bounded-compiler-spec-p
          spec))
       (let ((tail spec) (seen nil) (count 0) (ok t))
         (while (and ok tail)
           (if (not (and (consp tail) (consp (cdr tail))
                         (memq (car tail) '(:input :plan :emitted :contract))
                         (not (memq (car tail) seen))))
               (setq ok nil)
             (push (car tail) seen)
             (setq count (1+ count) tail (cddr tail))))
         (and ok (null tail) (= count 4)
              (equal (sort seen (lambda (a b)
                                  (string< (symbol-name a) (symbol-name b))))
                     '(:contract :emitted :input :plan))))))

(defun nelisp-native-load--rooted-cfg-safe-v3-manifest-p (manifest)
  "Safely locate any v3 field in MANIFEST, or flag malformed plist spines."
  (let ((tail manifest) (seen nil) (pairs 0) (found nil) (bad nil))
    (while (and tail (not bad) (< pairs 512))
      (if (not (and (consp tail) (consp (cdr tail)) (not (memq tail seen))))
          (setq bad t)
        (push tail seen)
        (when (memq (car tail)
                    '(:native-rooted-cfg-safe-v3-contract-version
                      :native-rooted-cfg-safe-v3-contract
                      :native-rooted-cfg-safe-v3-entry
                      :native-rooted-cfg-safe-v3-imports
                      :native-rooted-cfg-safe-v3-import-descriptors
                      :native-rooted-cfg-safe-v3-contract-hash))
          (setq found t))
        (setq tail (cddr tail) pairs (1+ pairs))))
    (cond (bad :malformed) ((and tail found) :oversized) (found t) (t nil))))

(defun nelisp-native-load--raw-v2-compile-stage (source artifact label)
  "Append one compiler stage without macro-expanding the compiler body."
  (let ((path (getenv "NELISP_ROOTED_CFG_STAGE_LOG")))
    (when (and (stringp path) (> (length path) 0))
      (write-region (format "producer-raw-%s seconds=%.3f source=%s artifact=%s\n" label (float-time) source artifact)
                    nil path t 'silent))))

(require 'nelisp-bytecode-native-rooted-cfg-contract)
(let ((cfg-validator-owner
       (symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p)))
(defun nelisp-native-load-raw-v2-compile-file
    (source-path artifact-path &optional build-id binary-sha256 call1-template rooted-stack-spec conditional-spec rooted-branch-spec rooted-branch-join-spec rooted-cfg-spec safe-v3-spec validation-receiver source-snapshot cache-only)
  "Compile a complete GC/arena SOURCE-PATH to v2 ARTIFACT-PATH.

The source is a snapshot of ordinary raw `defun' forms.  The canonical
chunk-arena rewrite runs before emission, then every `(data-addr NAME)' is
lowered to a zero-argument imported getter.  This matters because an
anonymous mmap page cannot safely use a PC32 relocation directly to the
reader's BSS.  Data getters return the live address through a loader stub;
they never copy the shared heap state.

The ABI module supplies the ordered GC contract and resolver namespace.  All
contract entries must be present with their declared arity (including the
seven-argument entry points), and every external relocation must be one of
the resolver names.  The generated manifest is a v2 raw artifact; v1 callers
continue to use `nelisp-native-load-raw-compile-file'.
VALIDATION-RECEIVER is internal: when supplied, receive the validated CFG
object, its print digest and validator identity after output publication.
SOURCE-SNAPSHOT is internal: (FORMS . SOURCE-BYTES) from the emitter, avoiding
file reading and parsing while retaining identical source provenance.
CACHE-ONLY is internal to the private rooted-CFG cache: return the manifest
without publishing an intermediate artifact.  The cache owns publication."
  (when (and cache-only
             (not (and rooted-cfg-spec (consp source-snapshot)
                       (listp (car source-snapshot)) (stringp (cdr source-snapshot))
                       (not (or safe-v3-spec call1-template rooted-stack-spec
                                conditional-spec rooted-branch-spec rooted-branch-join-spec)))))
    (error "nelisp-native-load: cache compilation requires a rooted-CFG snapshot"))
  (unless (and (stringp source-path)
               (or cache-only (file-readable-p source-path)))
    (error "nelisp-native-load: v2 raw source is not readable: %S" source-path))
  (when safe-v3-spec
    (unless (and (nelisp-native-load--rooted-cfg-safe-v3-spec-shape-p safe-v3-spec)
                 (not (or rooted-cfg-spec call1-template rooted-stack-spec
                          conditional-spec rooted-branch-spec rooted-branch-join-spec)))
      (error "nelisp-native-load: malformed, mixed, or unbounded safe-v3 spec")))
  (unless (and (stringp artifact-path) (> (length artifact-path) 0))
    (error "nelisp-native-load: v2 raw artifact path is empty"))
  (unless (nelisp-native-load--raw-v2-contract)
    (error "nelisp-native-load: GC ABI contract is unavailable"))
  (unless (nelisp-native-load--raw-v2-symbols)
    (error "nelisp-native-load: runtime resolver contract is unavailable"))
  (unless (fboundp 'nelisp-aot-compile-to-link-unit)
    (require 'nelisp-aot-compiler))
  (unless (fboundp 'nelisp-standalone--chunk-arena-rewrite)
    ;; The rewrite has its own source module; do not load the build driver
    ;; merely to compile a runtime unit from a clean session.
    (require 'nelisp-standalone-arena-rewrite))
  (unless (fboundp 'nelisp-aot-compile-to-link-unit)
    (error "nelisp-native-load: raw compiler is unavailable in this runtime"))
  (let* ((source
          (if source-snapshot (cdr source-snapshot)
           (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally source-path)
            (buffer-string))))
         (forms (if source-snapshot (car source-snapshot)
                  (nelisp-native-load--raw-source-forms source-path source)))
         (contract (nelisp-native-load--raw-v2-contract))
         (runtime-owned-gc (or rooted-cfg-spec safe-v3-spec))
         (resolver-symbols (nelisp-native-load--raw-v2-symbols))
         (layout nelisp-native-load-raw-layout-id-v2)
         (build (or build-id
                    (and (boundp 'nelisp--cli-version) nelisp--cli-version)
                    "unknown"))
         (binary (or binary-sha256
                     (nelisp-native-load--running-binary-sha256)))
         (compile-forms (if (or call1-template rooted-stack-spec conditional-spec rooted-branch-spec rooted-branch-join-spec rooted-cfg-spec safe-v3-spec)
                            (cl-remove-if
                             (lambda (form)
                               (and (eq (car form) 'defun)
                                    (eq (cadr form)
                                        'wf_bytecode_call_gateway_exit)))
                             forms)
                          forms))
         (prepared (nelisp-native-load--raw-v2-chunk-rewrite compile-forms))
         (data-names (nelisp-native-load--raw-v2-collect-data-addr-names
                      prepared))
         (rewritten (nelisp-native-load--raw-v2-rewrite-data-addr prepared))
         (unit nil)
         (cfg-validated-contract nil)
         (cfg-validation-result nil)
         (cfg-validation-digest nil))
    (unless forms
      (error "nelisp-native-load: v2 raw source has no top-level defun"))
    (dolist (form forms)
      (unless (and (listp form) (eq (car form) 'defun))
        (error "nelisp-native-load: v2 raw source has non-defun top level")))
    (when (and call1-template
               (not (nelisp-native-load--raw-v2-call1-source-valid-p
                     forms contract)))
      (error "nelisp-native-load: CALL1 source does not match fixed wrapper"))
    (when rooted-stack-spec
      (require 'nelisp-bytecode-native-rooted-stack)
      (let* ((input (plist-get rooted-stack-spec :input))
             (plan (plist-get rooted-stack-spec :plan))
             (entry (cl-find-if (lambda (f) (equal (cadr f) 'nl_native_stack_probe_v1)) forms))
             (expected (list 'defun 'nl_native_stack_probe_v1
                             '(env ticket argument-count root-count)
                             (nelisp-bytecode-native-rooted-stack-body
                              (plist-get plan :operations))))
             (gc-forms (cl-remove-if (lambda (f) (equal (cadr f) 'nl_native_stack_probe_v1)) forms)))
        (unless (and (equal (nelisp-bytecode-native-rooted-stack-plan input) plan)
                     entry (equal (nelisp-native-load--rooted-stack-normalize-ast entry)
                                  (nelisp-native-load--rooted-stack-normalize-ast expected))
                     (nelisp-native-load--rooted-stack-gc-forms-valid-p gc-forms contract)
                     (= (length forms) (1+ (length contract))))
          (error "nelisp-native-load: rooted-stack AST/plan/import mismatch"))))
    (when conditional-spec
      (let ((entry-name (plist-get conditional-spec :entry-name))
            (expected (plist-get conditional-spec :entry-ast))
            (entry (cl-find-if
                    (lambda (form) (and (eq (car form) 'defun)
                                        (equal (cadr form)
                                               (intern (plist-get conditional-spec :entry-name)))))
                    forms)))
        (unless (and (not rooted-stack-spec) (not call1-template)
                     (equal entry-name "nl_native_rooted_conditional_probe_v1")
                     entry expected
                     (equal (nelisp-native-load--rooted-stack-normalize-ast entry)
                            (nelisp-native-load--rooted-stack-normalize-ast expected))
                     (= (length forms) (1+ (length contract))))
          (error "nelisp-native-load: conditional AST contract mismatch"))))
    (when rooted-branch-spec
      (require 'nelisp-bytecode-native-rooted-branch)
      (let* ((input (plist-get rooted-branch-spec :input))
             (plan (plist-get rooted-branch-spec :plan))
             (entry (cl-find-if (lambda (f) (equal (cadr f) 'nl_native_rooted_branch_probe_v1)) forms))
             (expected (plist-get rooted-branch-spec :entry-ast))
             (gc-forms (cl-remove-if (lambda (f) (equal (cadr f) 'nl_native_rooted_branch_probe_v1)) forms)))
        (unless (and (not rooted-stack-spec) (not conditional-spec) (not call1-template)
                     (eq (plist-get (nelisp-bytecode-native-rooted-branch-plan input) :status)
                         'complete)
                     (equal (nelisp-bytecode-native-rooted-branch-plan input) plan)
                     entry expected
                     (equal (nelisp-native-load--rooted-stack-normalize-ast entry)
                            (nelisp-native-load--rooted-stack-normalize-ast expected))
                     (nelisp-native-load--rooted-stack-gc-forms-valid-p gc-forms contract)
                     (= (length forms) (1+ (length contract))))
          (error "nelisp-native-load: rooted branch AST/plan mismatch"))))
    (when rooted-branch-join-spec
      (require 'nelisp-bytecode-native-rooted-branch-join)
      (let* ((input (plist-get rooted-branch-join-spec :input))
             (operation (plist-get rooted-branch-join-spec :operation))
             (plan (plist-get rooted-branch-join-spec :plan))
             (entry (cl-find-if
                     (lambda (f) (equal (cadr f)
                                        'nl_native_rooted_branch_join_probe_v1))
                     forms))
             (expected (plist-get rooted-branch-join-spec :entry-ast))
             (gc-forms (cl-remove-if
                        (lambda (f) (equal (cadr f)
                                           'nl_native_rooted_branch_join_probe_v1))
                        forms)))
        (unless (and (not call1-template) (not rooted-stack-spec)
                     (not conditional-spec) (not rooted-branch-spec)
                     (eq (plist-get (nelisp-bytecode-native-rooted-branch-join-plan
                                     input operation) :status) 'complete)
                     (equal (nelisp-bytecode-native-rooted-branch-join-plan
                             input operation) plan)
                     entry expected
                     (equal (nelisp-native-load--rooted-stack-normalize-ast entry)
                            (nelisp-native-load--rooted-stack-normalize-ast expected))
                     (nelisp-native-load--rooted-stack-gc-forms-valid-p gc-forms contract)
                     (= (length forms) (1+ (length contract))))
          (error "nelisp-native-load: rooted branch-join AST/plan mismatch: %S"
                 (list :plan-status
                       (plist-get (nelisp-bytecode-native-rooted-branch-join-plan
                                   input operation) :status)
                       :plan-equal
                       (equal (nelisp-bytecode-native-rooted-branch-join-plan
                               input operation) plan)
                       :entry (and entry t) :expected (and expected t)
                       :ast-equal
                       (and entry expected
                            (equal (nelisp-native-load--rooted-stack-normalize-ast entry)
                                   (nelisp-native-load--rooted-stack-normalize-ast expected)))
                       :gc (nelisp-native-load--rooted-stack-gc-forms-valid-p
                            gc-forms contract)
                       :counts (list (length forms) (1+ (length contract))))))))
    ;; The producer and artifact admission authenticate the F1 capability,
    ;; including the frame descriptor/hash.  Raw compilation creates no
    ;; executable permission and must not repeat that full memory proof.
    (when rooted-cfg-spec
      (nelisp-native-load--raw-v2-compile-stage source-path artifact-path "contract-validation-start")
      (require 'nelisp-bytecode-native-rooted-cfg-contract)
      (let* ((input (plist-get rooted-cfg-spec :input))
             (plan (plist-get rooted-cfg-spec :plan))
             (emitted (plist-get rooted-cfg-spec :emitted))
             (cfg-contract (plist-get rooted-cfg-spec :contract))
             (shared-v2 (member (plist-get cfg-contract :version)
                                (list nelisp-bytecode-native-rooted-cfg-contract-shared-version
                                      nelisp-bytecode-native-rooted-cfg-contract-f1-shared-version)))
             (version (plist-get cfg-contract :version))
             (entry-name (if shared-v2
                             nelisp-bytecode-native-rooted-cfg-contract-shared-entry
                           "nl_native_rooted_cfg_probe_v1"))
             (entry (cl-find-if (lambda (form) (equal (cadr form) (intern entry-name))) forms))
             (gc-forms (cl-remove-if (lambda (form) (equal (cadr form) (intern entry-name))) forms))
             (validator (symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p))
             (reconstruction
              (and (eq validator cfg-validator-owner)
                   (setq cfg-validation-result
                         (funcall cfg-validator-owner cfg-contract :reconstruction input))
                   (progn
                     ;; Fingerprint immediately after validation, before AOT.
                     (setq cfg-validated-contract cfg-contract
                           cfg-validation-digest
                           (let ((print-length nil) (print-level nil)
                                 (print-circle t) (print-escape-newlines t)
                                 (nelisp--prn-symbol-cache (make-hash-table :test 'equal)))
                             (secure-hash 'sha256 (prin1-to-string cfg-contract))))
                     cfg-validation-result)))
             (verified-input (plist-get reconstruction :input))
             (canonical-input
              (and reconstruction
                   (nelisp-bytecode-compiler-input-build (plist-get input :function))))
             (verified-plan (plist-get reconstruction :plan))
             (verified-emitted (plist-get reconstruction :emitted))
             (expected-contract (plist-get reconstruction :expected-contract)))
        ;; The contract recipe and the caller's full input remain independent
        ;; checks. Only their shared plan/emission reconstruction is reused.
        (cl-labels ((input-data (value)
                      (let ((rest value) (data nil))
                        (while rest
                          (let ((key (pop rest)) (item (pop rest)))
                            (unless (eq key :function)
                              (push key data) (push item data))))
                        (nreverse data))))
        (unless (and (not call1-template) (not rooted-stack-spec)
                     (not conditional-spec) (not rooted-branch-spec)
                     (not rooted-branch-join-spec)
                     (member version (list nelisp-bytecode-native-rooted-cfg-contract-version
                                           nelisp-bytecode-native-rooted-cfg-contract-shared-version
                                           nelisp-bytecode-native-rooted-cfg-contract-f1-version
                                           nelisp-bytecode-native-rooted-cfg-contract-f1-shared-version))
                     reconstruction
                     (eq validator (symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p))
                     (equal input canonical-input)
                     (equal (nelisp-bytecode-native-rooted-cfg-contract-input-recipe input)
                            (nelisp-bytecode-native-rooted-cfg-contract-input-recipe verified-input))
                     (equal (input-data input) (input-data verified-input))
                     (eq (plist-get verified-plan :status) 'complete)
                     (equal plan verified-plan)
                     (eq (plist-get verified-emitted :status) 'complete)
                     (equal emitted verified-emitted)
                     (equal cfg-contract expected-contract)
                     (nelisp-native-load--rooted-cfg-import-names-valid-p
                      (plist-get cfg-contract :imports) cfg-contract)
                     (< (plist-get cfg-contract :root-count) 256)
                     entry (= (length (nth 2 entry)) 4)
                     ;; Equal objects have equal pure normalizations. The
                     ;; independent full emission comparison above still
                     ;; authenticates this shared producer/source subtree.
                     (or (eq entry (plist-get emitted :form))
                         (equal (nelisp-native-load--rooted-stack-normalize-ast entry)
                                (nelisp-native-load--rooted-stack-normalize-ast
                                 (plist-get emitted :form))))
                     (nelisp-native-load--rooted-cfg-provider-forms-valid-p
                      forms entry contract (plist-get verified-emitted :additional-source) cfg-contract))
          (error "nelisp-native-load: generic rooted-CFG AST/plan mismatch"))))
      (nelisp-native-load--raw-v2-compile-stage source-path artifact-path "contract-validation-end"))
    (when safe-v3-spec
      (let* ((input (plist-get safe-v3-spec :input))
             (plan (plist-get safe-v3-spec :plan))
             (emitted (plist-get safe-v3-spec :emitted))
             (safe-contract (plist-get safe-v3-spec :contract))
             (entry-name "nl_native_rooted_cfg_safe_probe_v3")
             (entry (cl-find-if (lambda (form)
                                  (equal (cadr form) (intern entry-name))) forms))
             (gc-forms (cl-remove-if (lambda (form)
                                       (equal (cadr form) (intern entry-name))) forms))
             (verified-plan
              (nelisp-bytecode-native-rooted-cfg-plan input 'safe-primitives-v3))
             (verified-emitted
              (and (eq (plist-get verified-plan :status) 'complete)
                   (nelisp-bytecode-native-rooted-cfg-emit verified-plan entry-name)))
             (expected-contract
              (and verified-emitted
                   (nelisp-bytecode-native-rooted-cfg-safe-contract-create
                    input verified-plan verified-emitted))))
        (unless (and (not rooted-cfg-spec) (not call1-template)
                     (not rooted-stack-spec) (not conditional-spec)
                     (not rooted-branch-spec) (not rooted-branch-join-spec)
                     (eq (plist-get verified-plan :status) 'complete)
                     (equal plan verified-plan)
                     (eq (plist-get verified-emitted :status) 'complete)
                     (equal emitted verified-emitted)
                     (nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p
                      safe-contract)
                     (equal safe-contract expected-contract)
                     (equal (plist-get safe-contract :imports)
                            (sort (copy-sequence (plist-get safe-contract :imports))
                                  #'string<))
                     (let ((safe-imports (plist-get safe-contract :imports)))
                       (and (listp safe-imports)
                            (equal safe-imports
                                   (sort (delete-dups (copy-sequence safe-imports))
                                         #'string<))
                            (cl-some
                             (lambda (name) (member name safe-imports))
                             '("nl_native_car_v2" "nl_native_cdr_v2"
                               "nl_native_cons_v2" "nl_native_funcall_v2" "nl_native_poll_v2"))
                            (cl-every
                             (lambda (name)
                               (member name
                                       '("nl_native_car_v2" "nl_native_cdr_v2"
                                         "nl_native_cons_v2" "nl_native_funcall_v2" "nl_native_poll_v2"
                                         "nl_root_pin_slot_v2")))
                             safe-imports)))
                     (< (plist-get safe-contract :root-count) 256)
                     entry (= (length (nth 2 entry)) 4)
                     (equal (nelisp-native-load--rooted-stack-normalize-ast entry)
                            (nelisp-native-load--rooted-stack-normalize-ast
                             (plist-get emitted :form)))
                     (nelisp-native-load--rooted-stack-gc-forms-valid-p
                      gc-forms contract)
                     (= (length forms) (1+ (length contract))))
          (error "nelisp-native-load: safe-v3 rooted-CFG AST/plan mismatch"))))
    ;; A source without the canonical rewrite is unsafe even if the compiler
    ;; happens to accept it: fixed arena addresses are part of the corruption
    ;; class this lane exists to avoid.
    (when (nelisp-native-load--raw-v2-fixed-address-p rewritten)
      (error "nelisp-native-load: v2 source retains a fixed arena address"))
    (dolist (name data-names)
      (unless (member name resolver-symbols)
        (error "nelisp-native-load: v2 data import is not exported: %s" name)))
    ;; Validate the source against the stable ordered contract before emitting
    ;; code.  This catches a missing body or accidental duplicate before any
    ;; artifact can be published.
    (let ((seen nil))
      (dolist (entry rewritten)
        (let ((name (and (consp entry) (eq (car entry) 'defun)
                         (symbol-name (cadr entry)))))
          (when name
            (when (member name seen)
              (error "nelisp-native-load: v2 source duplicates %s" name))
            (setq seen (cons name seen)))))
      (dolist (entry contract)
        (let ((name (car entry)))
          (unless (member name seen)
            (error "nelisp-native-load: v2 source lacks GC entry %s" name)))))
    ;; Authenticate the complete generated source above before dropping its
    ;; contract stubs. Runtime replacement units keep their actual GC bodies.
    (when runtime-owned-gc
      (setq rewritten
            (cl-remove-if
             (lambda (form)
               (assoc (symbol-name (cadr form)) contract)) rewritten)))
    (let ((nelisp-native-load-raw-max-arity
           nelisp-native-load-raw-max-arity-v2)
          (nelisp-aot-compiler--runtime-entry-params nil))
      (dolist (form rewritten)
        (unless (nelisp-native-load--raw-compile-defun-p form)
          (error "nelisp-native-load: unsupported v2 raw defun: %S"
                 (nth 1 form))))
      (nelisp-native-load--raw-v2-compile-stage source-path artifact-path "aot-start")
      (setq unit
            (nelisp-aot-compile-to-link-unit
             (cons 'seq rewritten) :arch 'x86_64 :format (if (nelisp-native-load--windows-p) 'coff 'elf)))
      (nelisp-native-load--raw-v2-compile-stage source-path artifact-path "aot-end"))
    (nelisp-native-load--raw-v2-compile-stage source-path artifact-path "manifest-materialization-start")
    (let* ((text (or (plist-get unit :text) ""))
           (rodata (or (plist-get unit :rodata) ""))
           (data (or (plist-get unit :data) ""))
           (bss (or (plist-get unit :bss-size) 0))
           (symbols (plist-get unit :symbols))
           (defuns (plist-get unit :defuns))
           (imports0 (plist-get unit :extern-symbols))
           (imports (delete-dups (copy-sequence imports0)))
           (exports nil)
           (import-descriptors nil)
           (gc-entries nil)
           (text-size (string-bytes text))
           (object-sha256 (nelisp-native-load--sha256 text))
           (manifest nil))
      (when (> (string-bytes rodata) 0)
        (error "nelisp-native-load: v2 GC unit cannot carry private rodata"))
      (when (or (> (string-bytes data) 0) (> bss 0))
        (error "nelisp-native-load: v2 GC unit cannot carry private data/BSS"))
      (dolist (def defuns)
        (let* ((name (plist-get def :name))
               (name (if (stringp name) name (symbol-name name)))
               (sym nil))
          (dolist (candidate symbols)
            (when (and (null sym) (equal (plist-get candidate :name) name))
              (setq sym candidate)))
          (unless (and sym (integerp (plist-get sym :value)))
            (error "nelisp-native-load: v2 defun has no text symbol: %s" name))
          (push (list :name name
                      :value (plist-get sym :value)
                      :size (or (plist-get sym :size) (plist-get def :size) 0)
                      :type 'func :abi (nelisp-native-load--runtime-abi-v2)
                      :arity (or (plist-get def :arity) 0)
                      :return 'u64)
                exports)))
      (setq exports (nreverse exports))
      (when call1-template
        (let ((export (nelisp-native-load--raw-export
                       (list :exports exports)
                       nelisp-native-load-raw-v2-call1-entry)))
          (unless export (error "nelisp-native-load: CALL1 entry missing"))
          (plist-put export :params '(u64 u64))))
      (when rooted-stack-spec
        (let ((export (nelisp-native-load--raw-export
                       (list :exports exports) "nl_native_stack_probe_v1")))
          (unless (and export (= (plist-get export :arity) 4))
            (error "nelisp-native-load: rooted-stack entry export mismatch"))
          (plist-put export :params '(u64 u64 u64 u64))))
      (when conditional-spec
        (let ((export (nelisp-native-load--raw-export
                       (list :exports exports) "nl_native_rooted_conditional_probe_v1")))
          (unless (and export (= (plist-get export :arity) 4))
            (error "nelisp-native-load: conditional entry export mismatch"))
          (plist-put export :params '(u64 u64 u64 u64))))
      (when rooted-branch-spec
        (let ((export (nelisp-native-load--raw-export
                       (list :exports exports) "nl_native_rooted_branch_probe_v1")))
          (unless (and export (= (plist-get export :arity) 4))
            (error "nelisp-native-load: rooted branch entry export mismatch"))
          (plist-put export :params '(u64 u64 u64 u64))))
      (when rooted-branch-join-spec
        (let ((export (nelisp-native-load--raw-export
                       (list :exports exports) "nl_native_rooted_branch_join_probe_v1")))
          (unless (and export (= (plist-get export :arity) 4))
            (error "nelisp-native-load: rooted branch-join entry export mismatch"))
          (plist-put export :params '(u64 u64 u64 u64))))
      (when rooted-cfg-spec
        (let* ((cfg-contract (plist-get rooted-cfg-spec :contract))
               (entry-name
                (if (member (plist-get cfg-contract :version)
                            (list nelisp-bytecode-native-rooted-cfg-contract-shared-version
                                  nelisp-bytecode-native-rooted-cfg-contract-f1-shared-version))
                    nelisp-bytecode-native-rooted-cfg-contract-shared-entry
                  "nl_native_rooted_cfg_probe_v1"))
               (export (nelisp-native-load--raw-export
                        (list :exports exports) entry-name)))
          (unless (and export (= (plist-get export :arity) 4))
            (error "nelisp-native-load: generic rooted-CFG entry export mismatch"))
          (plist-put export :params '(u64 u64 u64 u64))))
      (when safe-v3-spec
        (let ((export (nelisp-native-load--raw-export
                       (list :exports exports)
                       "nl_native_rooted_cfg_safe_probe_v3")))
          (unless (and export (= (plist-get export :arity) 4))
            (error "nelisp-native-load: safe-v3 entry export mismatch"))
          (plist-put export :params '(u64 u64 u64 u64))))
      (dolist (import imports)
        (let* ((provider (and rooted-cfg-spec
                              (nelisp-native-load--rooted-cfg-provider-import
                               (plist-get rooted-cfg-spec :contract) import)))
               (typed-call1 (and call1-template
                                 (equal import nelisp-native-load-raw-v2-call1-import)))
               (typed-conditional (and (or conditional-spec rooted-branch-spec rooted-branch-join-spec rooted-cfg-spec safe-v3-spec)
                                       (nelisp-native-load--raw-v2-conditional-import-mode import)))
               (typed-rooted-branch (and (or rooted-branch-spec rooted-branch-join-spec rooted-cfg-spec safe-v3-spec)
                                          (member import
                                                  '("nl_native_car_v2"
                                                    "nl_native_cdr_v2"
                                                    "nl_native_cons_v2" "nl_native_funcall_v2" "nl_native_poll_v2"
                                                    "nl_root_pin_slot_v2"))))
               (mode (cond (provider 'arithmetic-provider-v1)
                           (typed-call1 'call1-typed-v1)
                           (typed-conditional typed-conditional)
                           (t (nelisp-native-load--raw-v2-import-mode import))))
               (index (if (or typed-call1 provider)
                          (cl-position import nelisp-native-load-bridgeable-symbols
                                       :test #'equal)
                        (if typed-conditional
                            (nelisp-native-load--raw-v2-conditional-import-index import)
                          (nelisp-native-load--raw-v2-import-index
                           import resolver-symbols)))))
          (unless (and (stringp import) mode (integerp index))
            (error "nelisp-native-load: v2 import is not exported: %S" import))
          (push (append (list :name import
                      :kind (if (member import data-names) 'data 'func)
                      :abi (nelisp-native-load--runtime-abi-v2)
                      :index index :address-mode mode)
                       (when typed-call1
                         '(:arity 6 :params (u64 u64 u64 u64 u64 u64)
                           :return u64))
                       (when provider
                         (let ((copy (copy-sequence provider)))
                           (setq copy (plist-put copy :name nil)
                                 copy (plist-put copy :kind nil))
                           (cl-loop for (key value) on copy by #'cddr
                                    unless (memq key '(:name :kind)) append (list key value))))
                       (when (and (not provider) (or typed-conditional typed-rooted-branch))
                         '(:arity 6 :params (u64 u64 u64 u64 u64 u64)
                           :return u64)))
                import-descriptors)))
      (setq import-descriptors
            (sort import-descriptors
                  (lambda (a b) (< (plist-get a :index)
                                   (plist-get b :index)))))
      (when rooted-stack-spec
        (let ((actual (sort (copy-sequence imports) #'string<))
              (planned (sort (delete-dups
                              (mapcar (lambda (o) (format "nl_native_%s_v2" (plist-get o :operation)))
                                      (plist-get (plist-get rooted-stack-spec :plan) :operations)))
                             #'string<)))
          (unless (equal actual planned)
            (error "nelisp-native-load: rooted-stack imports mismatch"))))
      (let ((index 0))
        (dolist (entry contract)
          (let ((name (car entry)) (arity (cdr entry)) (export nil))
          (dolist (candidate exports)
            (when (and (null export) (equal (plist-get candidate :name) name))
              (setq export candidate)))
          (unless (or runtime-owned-gc export)
            (error "nelisp-native-load: v2 GC entry was not exported: %s" name))
          (unless (or runtime-owned-gc (= arity (plist-get export :arity)))
            (error "nelisp-native-load: v2 arity mismatch for %s" name))
          (push (list :name name :arity arity :index index
                      :return 'u64 :abi (nelisp-native-load--runtime-abi-v2))
                gc-entries)
            (setq index (1+ index)))))
      (setq gc-entries (nreverse gc-entries))
      (setq manifest
            (list :format nelisp-native-load-raw-artifact-format-v2
                  :kind 'raw-runtime
                  :runtime-kind 'gc-arena
                  :runtime-abi (nelisp-native-load--runtime-abi-v2)
                  :layout-id layout :arch nelisp-native-load-raw-supported-arch
                  :build-id build :binary-sha256 binary
                  :source (expand-file-name source-path)
                  :source-sha256 (nelisp-native-load--sha256 source)
                  :compiled-source-sha256
                  (nelisp-native-load--sha256 (prin1-to-string rewritten))
                  :runtime-opt-in t
                  :gc-contract-hash
                  (nelisp-native-load--raw-v2-contract-hash contract)
                  :gc-address-mode (if runtime-owned-gc 'runtime-bridge-v1 'artifact-export-v1)
                  :gc-entries gc-entries
                  :gc-table-magic nelisp-native-load-raw-gc-table-magic
                  :gc-table-count (length gc-entries)
                  :resolver-contract-version
                  nelisp-native-load-raw-v2-import-contract-version
                  :resolver-contract-hash
                  (nelisp-native-load--raw-v2-import-contract-hash
                   resolver-symbols)
                  :native
                  (list :raw-abi (nelisp-native-load--runtime-abi-v2)
                        :object-format 'nelisp-aot-raw-unit-v2
                        :text-size text-size
                        :text-base64 (base64-encode-string text t)
                        :object-sha256 object-sha256 :object-size text-size
                        :exports exports :symbols exports
                        :imports import-descriptors
                        :extern-symbols imports
                        :relocs (plist-get unit :relocs)
                        :data-size 0 :bss-size 0)))
      (when (nelisp-native-load--windows-p)
        (setq manifest (append manifest (list :target (nelisp-native-load--target-v2)))))
      (when call1-template
        (setq manifest
              (append manifest
                      (list :call1-contract-version
                            nelisp-native-load-raw-v2-call1-contract-version
                            :call1-contract-hash
                            (nelisp-native-load--raw-v2-call1-contract-hash)
                            :call1-caller
                            (list :name nelisp-native-load-raw-v2-call1-entry
                                  :arity 2 :params '(u64 u64) :return 'u64
                                  :slots '(4 5 2 0))
                            :call1-producer-validation-version
                            "nelisp-call1-exact-ast-v1"
                            :call1-producer-ast-sha256
                            (nelisp-native-load--sha256
                             (prin1-to-string forms))))))
      (when (and (not rooted-stack-spec)
                 (equal imports '("nl_native_car_v2" "nl_native_cdr_v2")))
        (setq manifest
              (append manifest
                      (list :native-object-op-contract-version
                            nelisp-native-load-native-object-op-contract-version
                            :native-object-op-gateway-imports
                            '("nl_native_car_v2" "nl_native_cdr_v2")
                            :native-object-opcodes
                            nelisp-native-load-native-object-opcodes
                            :native-object-op-contract-hash
                            (nelisp-native-load--native-object-op-contract-hash)))))
      (when rooted-stack-spec
        (setq manifest
              (append manifest
                      (list :native-rooted-stack-contract-version
                            nelisp-native-load-rooted-stack-contract-version
                            :native-rooted-stack-entry "nl_native_stack_probe_v1"
                            :native-rooted-stack-gateway-imports
                            (sort (copy-sequence imports) #'string<)
                            :native-rooted-stack-status-base 256
                            :native-rooted-stack-contract-hash
                            (nelisp-native-load--rooted-stack-contract-hash imports)))))
      (when conditional-spec
        (unless (equal imports '("nl_root_pin_slot_v2"))
          (error "nelisp-native-load: conditional import set mismatch"))
        (setq manifest
              (append manifest
                      (list :native-rooted-conditional-contract-version
                            nelisp-native-load-raw-v2-conditional-contract-version
                            :native-rooted-conditional-entry
                            "nl_native_rooted_conditional_probe_v1"
                            :native-rooted-conditional-imports imports
                            :native-rooted-conditional-status-base 256
                            :native-rooted-conditional-contract-hash
                            (nelisp-native-load--sha256
                             (prin1-to-string
                              (list nelisp-native-load-raw-v2-conditional-contract-version
                                    "nl_native_rooted_conditional_probe_v1"
                                    '("nl_root_pin_slot_v2")
                                    '(u64 u64 u64 u64 u64 u64) 'u64)))))))
      (when rooted-branch-spec
        (unless (equal (sort (copy-sequence imports) #'string<)
                       '("nl_native_car_v2" "nl_native_cdr_v2" "nl_root_pin_slot_v2"))
          (error "nelisp-native-load: rooted branch import set mismatch"))
        (setq manifest
              (append manifest
                      (list :native-rooted-branch-contract-version
                            nelisp-native-load-raw-v2-rooted-branch-contract-version
                            :native-rooted-branch-entry "nl_native_rooted_branch_probe_v1"
                            :native-rooted-branch-imports
                            '("nl_native_car_v2" "nl_native_cdr_v2" "nl_root_pin_slot_v2")
                            :native-rooted-branch-status-base 256
                            :native-rooted-branch-contract-hash
                            (nelisp-native-load--rooted-branch-contract-hash)))))
      (when rooted-branch-join-spec
        (let* ((operation (plist-get rooted-branch-join-spec :operation))
               (expected (sort (list (format "nl_native_%s_v2" operation)
                                     "nl_root_pin_slot_v2") #'string<)))
          (unless (and (memq operation '(car cdr))
                       (equal (sort (copy-sequence imports) #'string<) expected))
            (error "nelisp-native-load: rooted branch-join import set mismatch"))
          (setq manifest
                (append manifest
                        (list :native-rooted-branch-join-contract-version
                              nelisp-native-load-raw-v2-rooted-branch-join-contract-version
                              :native-rooted-branch-join-entry
                              "nl_native_rooted_branch_join_probe_v1"
                              :native-rooted-branch-join-operation operation
                              :native-rooted-branch-join-imports expected
                              :native-rooted-branch-join-status-base 256
                              :native-rooted-branch-join-contract-hash
                              (nelisp-native-load--rooted-branch-join-contract-hash
                               operation))))))
      (when rooted-cfg-spec
        (let* ((cfg-contract (plist-get rooted-cfg-spec :contract))
               (expected (plist-get cfg-contract :imports)))
          (unless (and (or (and cfg-validation-result
                                (eq cfg-contract cfg-validated-contract)
                                (eq cfg-validator-owner
                                    (symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p))
                                (equal cfg-validation-digest
                                       (let ((print-length nil) (print-level nil)
                                             (print-circle t) (print-escape-newlines t)
                                 (nelisp--prn-symbol-cache (make-hash-table :test 'equal)))
                                         (secure-hash 'sha256 (prin1-to-string cfg-contract)))))
                           (nelisp-bytecode-native-rooted-cfg-contract-valid-p cfg-contract))
                       (equal (sort (copy-sequence imports) #'string<) expected))
            (error "nelisp-native-load: generic rooted-CFG import set mismatch"))
          (setq manifest
                (append manifest
                        (list :native-rooted-cfg-contract-version
                              (plist-get cfg-contract :version)
                              :native-rooted-cfg-contract cfg-contract
                              :native-rooted-cfg-import-descriptors import-descriptors)))))
      (when safe-v3-spec
        (let* ((safe-contract (plist-get safe-v3-spec :contract))
               (expected (plist-get safe-contract :imports)))
          (unless (and (nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p
                        safe-contract)
                       (equal (sort (copy-sequence imports) #'string<) expected))
            (error "nelisp-native-load: safe-v3 rooted-CFG import set mismatch: expected %S, actual %S"
                   expected (sort (copy-sequence imports) #'string<)))
          (setq manifest
                (append manifest
                        (list :native-rooted-cfg-safe-v3-contract-version
                              (plist-get safe-contract :version)
                              :native-rooted-cfg-safe-v3-contract safe-contract
                              :native-rooted-cfg-safe-v3-entry
                              "nl_native_rooted_cfg_safe_probe_v3"
                              :native-rooted-cfg-safe-v3-imports expected
                              :native-rooted-cfg-safe-v3-import-descriptors import-descriptors
                              :native-rooted-cfg-safe-v3-contract-hash
                              (plist-get safe-contract :digest))))))
      (let* ((unsigned (prin1-to-string manifest))
             (digest (nelisp-native-load--sha256 unsigned)))
        (setq manifest (append manifest (list :artifact-sha256 digest)))
        (when nelisp-native-load--serialization-receiver
          (funcall nelisp-native-load--serialization-receiver manifest
                   (nelisp-native-load--signed-manifest unsigned digest))))
      (nelisp-native-load--raw-v2-compile-stage source-path artifact-path "manifest-materialization-end")
      (unless cache-only
      (nelisp-native-load--raw-v2-compile-stage source-path artifact-path "atomic-output-start")
      (let* ((absolute (expand-file-name artifact-path))
             (parent (file-name-directory absolute))
             (temporary nil))
        (when parent (make-directory parent t))
        (setq temporary (make-temp-file
                         (expand-file-name ".nelr-v2-staging-" parent)
                         nil ".tmp"))
        (unwind-protect
            (progn
              (let ((coding-system-for-write 'utf-8-unix))
                (with-temp-file temporary
                  (insert ";;; nelisp-private-nelr-v2\n")
                  (prin1 manifest (current-buffer))
                  (insert "\n")))
              (rename-file temporary absolute t)
              (setq temporary nil))
          (when (and temporary (file-exists-p temporary))
            (ignore-errors (delete-file temporary)))))
      (nelisp-native-load--raw-v2-compile-stage source-path artifact-path "atomic-output-end"))
      (when (and validation-receiver cfg-validation-result)
        (funcall validation-receiver cfg-validated-contract
                 cfg-validation-digest cfg-validator-owner))
      manifest)))
)

(defun nelisp-native-load--raw-v2-compile-file-with-validation
    (source-path artifact-path &optional build-id binary-sha256 call1-template rooted-stack-spec conditional-spec rooted-branch-spec rooted-branch-join-spec rooted-cfg-spec safe-v3-spec source-snapshot)
  "Return the compiled manifest and its call-local CFG validation receipt."
  (let (validated-contract digest validator)
    (let ((manifest
           (nelisp-native-load-raw-v2-compile-file
            source-path artifact-path build-id binary-sha256 call1-template
            rooted-stack-spec conditional-spec rooted-branch-spec
            rooted-branch-join-spec rooted-cfg-spec safe-v3-spec
            (lambda (contract print-digest owner)
              (setq validated-contract contract digest print-digest
                    validator owner))
            source-snapshot)))
      (list :manifest manifest :validated-contract validated-contract
            :digest digest :validator validator))))

(defun nelisp-native-load-raw-v2-compile-call1-file
    (source-path artifact-path &optional build-id binary-sha256)
  "Compile only the fixed CALL1 exit wrapper and exact GC declaration set."
  (nelisp-native-load-raw-v2-compile-file
   source-path artifact-path build-id binary-sha256 t))

(defun nelisp-native-load--raw-v2-import (native name)
  "Return v2 import descriptor NAME from NATIVE."
  (let ((rest (plist-get native :imports)) (found nil))
    (while (and rest (null found))
      (when (equal (nelisp-native-load--raw-import-name (car rest)) name)
        (setq found (car rest)))
      (setq rest (cdr rest)))
    found))

(defun nelisp-native-load--raw-v2-gc-entry (manifest name)
  "Return v2 GC table descriptor NAME from MANIFEST."
  (let ((rest (plist-get manifest :gc-entries)) (found nil))
    (while (and rest (null found))
      (when (equal (plist-get (car rest) :name) name)
        (setq found (car rest)))
      (setq rest (cdr rest)))
    found))

(defun nelisp-native-load--raw-v2-conditional-contract-valid-p (manifest)
  "Validate the complete isolated slot-tag conditional manifest contract."
  (let* ((native (nelisp-native-load--raw-native manifest))
         (entry (nelisp-native-load--raw-export
                 native "nl_native_rooted_conditional_probe_v1"))
         (imports (and native (plist-get native :imports)))
         (descriptor (and (listp imports) (= (length imports) 1) (car imports))))
    (and (equal (plist-get manifest :native-rooted-conditional-contract-version)
                nelisp-native-load-raw-v2-conditional-contract-version)
         (equal (plist-get manifest :native-rooted-conditional-entry)
                "nl_native_rooted_conditional_probe_v1")
         (equal (plist-get manifest :native-rooted-conditional-imports)
                '("nl_root_pin_slot_v2"))
         (= (or (plist-get manifest :native-rooted-conditional-status-base) -1) 256)
         (equal (plist-get manifest :native-rooted-conditional-contract-hash)
                (nelisp-native-load--sha256
                 (prin1-to-string
                  (list nelisp-native-load-raw-v2-conditional-contract-version
                        "nl_native_rooted_conditional_probe_v1"
                        '("nl_root_pin_slot_v2")
                        '(u64 u64 u64 u64 u64 u64) 'u64))))
         entry (= (or (plist-get entry :arity) -1) 4)
         (equal (plist-get entry :params) '(u64 u64 u64 u64))
         (eq (plist-get entry :return) 'u64)
         (eq (plist-get entry :type) 'func)
         (equal (plist-get entry :abi) (nelisp-native-load--runtime-abi-v2))
         (equal (nelisp-native-load--raw-import-name descriptor)
                "nl_root_pin_slot_v2")
         (eq (nelisp-native-load--raw-import-kind descriptor) 'func)
         (equal (plist-get descriptor :abi) (nelisp-native-load--runtime-abi-v2))
         (= (or (plist-get descriptor :index) -1)
            (nelisp-native-load--raw-v2-conditional-import-index
             "nl_root_pin_slot_v2"))
         (eq (plist-get descriptor :address-mode) 'conditional-root-slot-v1)
         (= (or (plist-get descriptor :arity) -1) 6)
         (equal (plist-get descriptor :params) '(u64 u64 u64 u64 u64 u64))
         (eq (plist-get descriptor :return) 'u64)
         (not (or (plist-get manifest :native-rooted-stack-contract-version)
                  (plist-get manifest :call1-contract-version)
                  (plist-get manifest :native-object-op-contract-version))))))

(defun nelisp-native-load--raw-v2-rooted-branch-contract-valid-p (manifest)
  "Validate the isolated rooted branch gateway contract."
  (let* ((native (nelisp-native-load--raw-native manifest))
         (entry (nelisp-native-load--raw-export native "nl_native_rooted_branch_probe_v1"))
         (imports (and native (plist-get native :imports)))
         (expected '("nl_native_car_v2" "nl_native_cdr_v2" "nl_root_pin_slot_v2")))
    (and (equal (plist-get manifest :native-rooted-branch-contract-version)
                nelisp-native-load-raw-v2-rooted-branch-contract-version)
         (equal (plist-get manifest :native-rooted-branch-entry)
                "nl_native_rooted_branch_probe_v1")
         (equal (plist-get manifest :native-rooted-branch-imports) expected)
         (equal (sort (mapcar #'nelisp-native-load--raw-import-name imports) #'string<) expected)
         (= (or (plist-get manifest :native-rooted-branch-status-base) -1) 256)
         (equal (plist-get manifest :native-rooted-branch-contract-hash)
                (nelisp-native-load--rooted-branch-contract-hash))
         entry (= (or (plist-get entry :arity) -1) 4)
         (equal (plist-get entry :params) '(u64 u64 u64 u64))
         (eq (plist-get entry :return) 'u64) (eq (plist-get entry :type) 'func)
         (equal (plist-get entry :abi) (nelisp-native-load--runtime-abi-v2))
         (not (or (plist-get manifest :native-rooted-stack-contract-version)
                  (plist-get manifest :native-rooted-conditional-contract-version)
                  (plist-get manifest :call1-contract-version)
                  (plist-get manifest :native-object-op-contract-version)))
         (cl-every
          (lambda (name)
            (let ((d (nelisp-native-load--raw-v2-import native name)))
              (and d (eq (nelisp-native-load--raw-import-kind d) 'func)
                   (equal (plist-get d :abi) (nelisp-native-load--runtime-abi-v2))
                   (= (or (plist-get d :arity) -1) 6)
                   (equal (plist-get d :params) '(u64 u64 u64 u64 u64 u64))
                   (eq (plist-get d :return) 'u64)
                   (if (equal name "nl_root_pin_slot_v2")
                       (and (eq (plist-get d :address-mode) 'conditional-root-slot-v1)
                            (= (or (plist-get d :index) -1)
                               (nelisp-native-load--raw-v2-conditional-import-index name)))
                     (eq (plist-get d :address-mode) 'native-bridgeable-v1)))))
          expected))))

(defun nelisp-native-load--raw-v2-rooted-branch-join-contract-valid-p (manifest)
  "Validate one selected CAR or CDR gateway for the joined branch contract."
  (let* ((operation (plist-get manifest :native-rooted-branch-join-operation))
         (entry-name "nl_native_rooted_branch_join_probe_v1")
         (native (nelisp-native-load--raw-native manifest))
         (entry (nelisp-native-load--raw-export native entry-name))
         (expected (and (memq operation '(car cdr))
                        (sort (list (format "nl_native_%s_v2" operation)
                                    "nl_root_pin_slot_v2") #'string<)))
         (imports (and native (plist-get native :imports))))
    (and expected
         (equal (plist-get manifest :native-rooted-branch-join-contract-version)
                nelisp-native-load-raw-v2-rooted-branch-join-contract-version)
         (equal (plist-get manifest :native-rooted-branch-join-entry) entry-name)
         (equal (plist-get manifest :native-rooted-branch-join-imports) expected)
         (equal (sort (mapcar #'nelisp-native-load--raw-import-name imports) #'string<)
                expected)
         (= (or (plist-get manifest :native-rooted-branch-join-status-base) -1) 256)
         (equal (plist-get manifest :native-rooted-branch-join-contract-hash)
                (nelisp-native-load--rooted-branch-join-contract-hash operation))
         entry (= (or (plist-get entry :arity) -1) 4)
         (equal (plist-get entry :params) '(u64 u64 u64 u64))
         (eq (plist-get entry :return) 'u64) (eq (plist-get entry :type) 'func)
         (equal (plist-get entry :abi) (nelisp-native-load--runtime-abi-v2))
         (not (or (plist-get manifest :native-rooted-stack-contract-version)
                  (plist-get manifest :native-rooted-branch-contract-version)
                  (plist-get manifest :native-rooted-conditional-contract-version)
                  (plist-get manifest :call1-contract-version)
                  (plist-get manifest :native-object-op-contract-version)))
         (cl-every
          (lambda (name)
            (let ((descriptor (nelisp-native-load--raw-v2-import native name)))
              (and descriptor
                   (eq (nelisp-native-load--raw-import-kind descriptor) 'func)
                   (equal (plist-get descriptor :abi)
                          (nelisp-native-load--runtime-abi-v2))
                   (= (or (plist-get descriptor :arity) -1) 6)
                   (equal (plist-get descriptor :params)
                          '(u64 u64 u64 u64 u64 u64))
                   (eq (plist-get descriptor :return) 'u64)
                   (if (equal name "nl_root_pin_slot_v2")
                       (and (eq (plist-get descriptor :address-mode)
                                'conditional-root-slot-v1)
                            (= (or (plist-get descriptor :index) -1)
                               (nelisp-native-load--raw-v2-conditional-import-index name)))
                     (and (eq (plist-get descriptor :address-mode)
                              'native-bridgeable-v1)
                          (= (or (plist-get descriptor :index) -1)
                             (or (cl-position name nelisp-native-load-bridgeable-symbols
                                              :test #'equal)
                                 -2)))))))
          expected))))

(defun nelisp-native-load--raw-v2-rooted-cfg-contract-valid-slow-p
    (manifest &optional compiled-contract-valid)
  "Validate generic CFG plan, entry, imports, and canonical typed ABIs.
COMPILED-CONTRACT-VALID is internal to the immediate post-compile check;
only semantic reconstruction is omitted, never the manifest structure."
  (require 'nelisp-bytecode-native-rooted-cfg-contract)
  (let* ((contract (plist-get manifest :native-rooted-cfg-contract))
         (native (nelisp-native-load--raw-native manifest))
         (entry-name (plist-get contract :entry))
         (version (plist-get contract :version))
         (shared-v2 (member version
                            (list nelisp-bytecode-native-rooted-cfg-contract-shared-version
                                  nelisp-bytecode-native-rooted-cfg-contract-f1-shared-version)))
         (expected-entry (if shared-v2
                             nelisp-bytecode-native-rooted-cfg-contract-shared-entry
                           "nl_native_rooted_cfg_probe_v1"))
         (entry (and native (nelisp-native-load--raw-export native entry-name)))
         (actual (and native (plist-get native :imports)))
         (expected-names (plist-get contract :imports))
         (actual-names (and (listp actual)
                            (sort (mapcar #'nelisp-native-load--raw-import-name actual)
                                  #'string<)))
         (descriptors (plist-get manifest :native-rooted-cfg-import-descriptors)))
    (and (member version
                 (list nelisp-bytecode-native-rooted-cfg-contract-version
                       nelisp-bytecode-native-rooted-cfg-contract-shared-version
                       nelisp-bytecode-native-rooted-cfg-contract-f1-version
                       nelisp-bytecode-native-rooted-cfg-contract-f1-shared-version))
         (equal (plist-get manifest :native-rooted-cfg-contract-version) version)
         (or compiled-contract-valid
             (nelisp-bytecode-native-rooted-cfg-contract-valid-p contract))
         (equal entry-name expected-entry)
         (equal (plist-get contract :entry-arity) 4)
         (integerp (plist-get contract :argument-count))
         (>= (plist-get contract :argument-count) 0)
         (integerp (plist-get contract :root-count))
         (> (plist-get contract :root-count) 0)
         (< (plist-get contract :root-count) 256)
         (= (plist-get contract :status-base) 512)
         (= (plist-get contract :error-base) 256)
         entry (= (or (plist-get entry :arity) -1) 4)
         (equal (plist-get entry :params) '(u64 u64 u64 u64))
         (eq (plist-get entry :return) 'u64)
         (eq (plist-get entry :type) 'func)
         (equal (plist-get entry :abi) (nelisp-native-load--runtime-abi-v2))
         (not (or (plist-get manifest :native-rooted-stack-contract-version)
                  (plist-get manifest :native-rooted-branch-contract-version)
                  (plist-get manifest :native-rooted-branch-join-contract-version)
                  (plist-get manifest :native-rooted-conditional-contract-version)
                  (plist-get manifest :call1-contract-version)
                  (plist-get manifest :native-object-op-contract-version)))
         (equal expected-names actual-names)
         (equal descriptors actual)
         (cl-every
          (lambda (descriptor)
            (let* ((name (nelisp-native-load--raw-import-name descriptor))
                   (slot (equal name "nl_root_pin_slot_v2"))
                   (index (if slot
                              (nelisp-native-load--raw-v2-conditional-import-index name)
                            (cl-position name nelisp-native-load-bridgeable-symbols
                                         :test #'equal))))
              (or (nelisp-native-load--rooted-cfg-provider-import-valid-p descriptor contract)
              (and (member name '("nl_native_car_v2" "nl_native_cdr_v2"
                                  "nl_native_cons_v2" "nl_native_funcall_v2" "nl_native_poll_v2" "nl_root_pin_slot_v2"))
                   (eq (nelisp-native-load--raw-import-kind descriptor) 'func)
                   (equal (plist-get descriptor :abi) (nelisp-native-load--runtime-abi-v2))
                   (eq (plist-get descriptor :address-mode)
                       (if slot 'conditional-root-slot-v1 'native-bridgeable-v1))
                   (integerp index) (= (or (plist-get descriptor :index) -1) index)
                   (= (or (plist-get descriptor :arity) -1) 6)
                   (equal (plist-get descriptor :params)
                          '(u64 u64 u64 u64 u64 u64))
                   (eq (plist-get descriptor :return) 'u64)))))
          actual)
         (if expected-names
             (or (and (plist-get contract :additional-source)
                           (cl-every (lambda (record) (member (plist-get record :name) actual-names))
                                     (plist-get contract :runtime-imports)))
                  (cl-some (lambda (name) (member name actual-names))
                           '("nl_native_car_v2" "nl_native_cdr_v2"
                             "nl_native_cons_v2" "nl_native_funcall_v2" "nl_native_poll_v2")))
           (and (null actual-names)
                (null expected-names)
                (null descriptors))))))

(defvar nelisp-native-load--raw-v2-rooted-cfg-validation-cache nil)
(defvar nelisp-native-load--raw-v2-rooted-cfg-cache-hits 0)
(defvar nelisp-native-load--raw-v2-rooted-cfg-cache-misses 0)
(defvar nelisp-native-load--raw-v2-rooted-cfg-cache-nodes-remaining nil)

(defun nelisp-native-load-raw-v2-rooted-cfg-cache-statistics ()
  "Return process-local generic CFG cache and key-derivation totals."
  (list :hits nelisp-native-load--raw-v2-rooted-cfg-cache-hits
        :misses nelisp-native-load--raw-v2-rooted-cfg-cache-misses
        :runtime-key-computations
        nelisp-native-load--raw-v2-rooted-cfg-runtime-key-computations))

(defun nelisp-native-load--raw-v2-rooted-cfg-cacheable-data-p (value depth)
  "Whether VALUE contains only stable manifest data within DEPTH bound."
  (and (<= depth 256)
       (integerp nelisp-native-load--raw-v2-rooted-cfg-cache-nodes-remaining)
       (> nelisp-native-load--raw-v2-rooted-cfg-cache-nodes-remaining 0)
       (setq nelisp-native-load--raw-v2-rooted-cfg-cache-nodes-remaining
             (1- nelisp-native-load--raw-v2-rooted-cfg-cache-nodes-remaining))
       (cond
        ((or (null value) (integerp value) (floatp value) (symbolp value)) t)
        ((stringp value) t)
        ((consp value)
         (and (nelisp-native-load--raw-v2-rooted-cfg-cacheable-data-p
               (car value) (1+ depth))
              (nelisp-native-load--raw-v2-rooted-cfg-cacheable-data-p
               (cdr value) (1+ depth))))
        ((and (vectorp value) (not (stringp value)))
         (let ((index 0) (safe t))
           (while (and safe (< index (length value)))
             (setq safe
                   (nelisp-native-load--raw-v2-rooted-cfg-cacheable-data-p
                    (aref value index) (1+ depth)))
             (setq index (1+ index)))
           safe))
        (t nil))))

(defun nelisp-native-load--raw-v2-rooted-cfg-copy-data (value depth)
  "Deep-copy bounded manifest VALUE for mutation-detecting contract cache."
  (cond
   ((> depth 256) :rooted-cfg-cache-depth-exceeded)
   ((consp value)
    (cons (nelisp-native-load--raw-v2-rooted-cfg-copy-data (car value) (1+ depth))
          (nelisp-native-load--raw-v2-rooted-cfg-copy-data (cdr value) (1+ depth))))
   ((stringp value) (copy-sequence value))
   ((and (vectorp value) (not (stringp value)))
    (let ((copy (copy-sequence value)))
      (dotimes (index (length copy))
        (aset copy index
              (nelisp-native-load--raw-v2-rooted-cfg-copy-data
               (aref value index) (1+ depth))))
      copy))
   (t value)))

(defvar nelisp-native-load--raw-v2-rooted-cfg-runtime-key-snapshot nil)
(defvar nelisp-native-load--raw-v2-rooted-cfg-runtime-key-value nil)
(defvar nelisp-native-load--raw-v2-rooted-cfg-runtime-key-computations 0)

(defun nelisp-native-load--raw-v2-rooted-cfg-runtime-key-inputs ()
  "Return mutable process ABI inputs used by the generic CFG cache key."
  (list (nelisp-native-load--running-binary-sha256)
        (nelisp-native-load--runtime-abi-v2)
        (nelisp-native-load--raw-v2-symbols)
        nelisp-native-load-bridgeable-symbols
        nelisp-native-load-raw-v2-bridgeable-imports
        (nelisp-native-load--raw-v2-conditional-import-index
         "nl_root_pin_slot_v2")
        (condition-case nil
            (plist-get (nelisp-native-load-raw-state) :generation)
          (error nil))))

(defun nelisp-native-load--raw-v2-rooted-cfg-runtime-key ()
  "Fingerprint process ABI inputs, reusing only an equal bounded snapshot."
  (let* ((nelisp-native-load--raw-v2-rooted-cfg-cache-nodes-remaining 4096)
         (inputs (nelisp-native-load--raw-v2-rooted-cfg-runtime-key-inputs))
         (cacheable (nelisp-native-load--raw-v2-rooted-cfg-cacheable-data-p
                     inputs 0)))
    (if (and cacheable
             nelisp-native-load--raw-v2-rooted-cfg-runtime-key-snapshot
             (equal inputs
                    nelisp-native-load--raw-v2-rooted-cfg-runtime-key-snapshot))
        nelisp-native-load--raw-v2-rooted-cfg-runtime-key-value
      (let* ((_ (setq nelisp-native-load--raw-v2-rooted-cfg-runtime-key-computations
                      (1+ nelisp-native-load--raw-v2-rooted-cfg-runtime-key-computations)))
             (key (secure-hash 'sha256 (prin1-to-string inputs))))
        (setq nelisp-native-load--raw-v2-rooted-cfg-runtime-key-snapshot
              (and cacheable
                   (nelisp-native-load--raw-v2-rooted-cfg-copy-data inputs 0))
              nelisp-native-load--raw-v2-rooted-cfg-runtime-key-value key)
        key))))

(defun nelisp-native-load--raw-v2-rooted-cfg-safe-v3-contract-valid-p (manifest)
  "Validate the distinct safe-v3 contract after bounded inert-data admission."
  (require 'nelisp-bytecode-native-rooted-cfg-safe-contract)
  (let* ((contract (plist-get manifest :native-rooted-cfg-safe-v3-contract))
         (native (nelisp-native-load--raw-native manifest))
         (entry-name "nl_native_rooted_cfg_safe_probe_v3")
         (entry (and native (nelisp-native-load--raw-export native entry-name)))
         (imports (and native (plist-get native :imports)))
         (names (and (listp imports)
                     (sort (mapcar #'nelisp-native-load--raw-import-name imports)
                           #'string<)))
         (expected (plist-get contract :imports)))
    (and (equal (plist-get manifest :native-rooted-cfg-safe-v3-contract-version)
                (plist-get contract :version))
         (member (plist-get contract :version)
                 (list nelisp-bytecode-native-rooted-cfg-safe-contract-version
                       nelisp-bytecode-native-rooted-cfg-safe-contract-f1-version))
         (equal (plist-get manifest :native-rooted-cfg-safe-v3-entry) entry-name)
         (equal (plist-get manifest :native-rooted-cfg-safe-v3-contract-hash)
                (plist-get contract :digest))
         (nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p contract)
         (equal (plist-get contract :emitter-mode) "safe-primitives-v3")
         (equal (plist-get contract :entry-params) '(u64 u64 u64 u64))
         (eq (plist-get contract :entry-return) 'u64)
         (integerp (plist-get contract :root-count))
         (> (plist-get contract :root-count) 0)
         (< (plist-get contract :root-count) 256)
         (= (plist-get contract :status-base) 512)
         (and (listp expected)
              (equal expected
                     (sort (delete-dups (copy-sequence expected)) #'string<))
              (cl-some (lambda (name) (member name expected))
                       '("nl_native_car_v2" "nl_native_cdr_v2"
                         "nl_native_cons_v2" "nl_native_funcall_v2" "nl_native_poll_v2"))
              (cl-every
               (lambda (name)
                 (member name '("nl_native_car_v2" "nl_native_cdr_v2"
                                "nl_native_cons_v2" "nl_native_funcall_v2" "nl_native_poll_v2" "nl_root_pin_slot_v2")))
               expected))
         (equal (plist-get manifest :native-rooted-cfg-safe-v3-imports) expected)
         (equal names expected)
         (equal (plist-get manifest :native-rooted-cfg-safe-v3-import-descriptors)
                imports)
         (cl-every
          (lambda (descriptor)
            (let* ((name (nelisp-native-load--raw-import-name descriptor))
                   (slot (equal name "nl_root_pin_slot_v2"))
                   (index (if slot
                              (nelisp-native-load--raw-v2-conditional-import-index name)
                            (cl-position name nelisp-native-load-bridgeable-symbols
                                         :test #'equal))))
              (and (member name '("nl_native_car_v2" "nl_native_cdr_v2"
                                  "nl_native_cons_v2" "nl_native_funcall_v2" "nl_native_poll_v2" "nl_root_pin_slot_v2"))
                   (eq (nelisp-native-load--raw-import-kind descriptor) 'func)
                   (equal (plist-get descriptor :abi)
                          (nelisp-native-load--runtime-abi-v2))
                   (eq (plist-get descriptor :address-mode)
                       (if slot 'conditional-root-slot-v1 'native-bridgeable-v1))
                   (integerp index) (= (or (plist-get descriptor :index) -1) index)
                   (= (or (plist-get descriptor :arity) -1) 6)
                   (equal (plist-get descriptor :params)
                          '(u64 u64 u64 u64 u64 u64))
                   (eq (plist-get descriptor :return) 'u64))))
          imports)
         entry (= (or (plist-get entry :arity) -1) 4)
         (equal (plist-get entry :params) '(u64 u64 u64 u64))
         (eq (plist-get entry :return) 'u64)
         (eq (plist-get entry :type) 'func)
         (equal (plist-get entry :abi) (nelisp-native-load--runtime-abi-v2))
         (not (or (plist-get manifest :native-rooted-cfg-contract-version)
                  (plist-get manifest :native-rooted-cfg-contract)
                  (plist-get manifest :native-rooted-cfg-import-descriptors)
                  (plist-get manifest :native-rooted-conditional-contract-version)
                  (plist-get manifest :native-rooted-conditional-entry)
                  (plist-get manifest :native-rooted-conditional-imports)
                  (plist-get manifest :native-rooted-stack-contract-version)
                  (plist-get manifest :native-rooted-stack-entry)
                  (plist-get manifest :native-rooted-stack-gateway-imports)
                  (plist-get manifest :native-rooted-branch-contract-version)
                  (plist-get manifest :native-rooted-branch-entry)
                  (plist-get manifest :native-rooted-branch-imports)
                  (plist-get manifest :native-rooted-branch-join-contract-version)
                  (plist-get manifest :native-rooted-branch-join-entry)
                  (plist-get manifest :native-rooted-branch-join-imports)
                  (plist-get manifest :call1-contract-version)
                  (plist-get manifest :native-object-op-contract-version)
                  (plist-get manifest :native-object-op-gateway-imports)
                  (plist-get manifest :native-object-opcodes)
                  (plist-get manifest :native-object-op-contract-hash))))))

(defvar nelisp-native-load--rooted-cfg-outer-validation-count 0)
(defvar nelisp-native-load--raw-v2-check-count 0)
(defvar nelisp-native-load--trusted-map-count 0)

(defun nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p (manifest)
  "Validate generic CFG contract, reusing only an unchanged verified snapshot."
  (setq nelisp-native-load--rooted-cfg-outer-validation-count
        (1+ nelisp-native-load--rooted-cfg-outer-validation-count))
  (when (nelisp-native-load--rooted-cfg-safe-v3-manifest-p manifest)
    (require 'nelisp-bytecode-native-rooted-cfg-safe-contract))
  (if (nelisp-native-load--rooted-cfg-safe-v3-manifest-p manifest)
      (and (nelisp-bytecode-native-rooted-cfg-safe-contract-bounded-data-p manifest)
           (nelisp-native-load--raw-v2-rooted-cfg-safe-v3-contract-valid-p manifest))
    (if (not (plist-get manifest :native-rooted-cfg-contract-version))
      nil
    (let* ((nelisp-native-load--raw-v2-rooted-cfg-cache-nodes-remaining 20000)
           (snapshot-safe (nelisp-native-load--raw-v2-rooted-cfg-cacheable-data-p
                           manifest 0))
           (runtime-key (nelisp-native-load--raw-v2-rooted-cfg-runtime-key))
           (provider-context (nelisp-native-load--rooted-cfg-provider-cache-context manifest))
           (record (and snapshot-safe
                        (cl-find-if
                         (lambda (entry)
                           (and (equal runtime-key (nth 2 entry))
                                (nelisp-native-load--rooted-cfg-provider-cache-context-equal-p
                                 provider-context (nth 3 entry))
                                (equal manifest (nth 1 entry))))
                         nelisp-native-load--raw-v2-rooted-cfg-validation-cache))))
      (if record
          (progn
            (setq nelisp-native-load--raw-v2-rooted-cfg-cache-hits
                  (1+ nelisp-native-load--raw-v2-rooted-cfg-cache-hits))
            t)
        (setq nelisp-native-load--raw-v2-rooted-cfg-cache-misses
              (1+ nelisp-native-load--raw-v2-rooted-cfg-cache-misses))
        (let ((valid (nelisp-native-load--raw-v2-rooted-cfg-contract-valid-slow-p manifest)))
          (when (and valid snapshot-safe
                     (nelisp-native-load--rooted-cfg-provider-cache-context-equal-p
                      provider-context (nelisp-native-load--rooted-cfg-provider-cache-context manifest)))
            (push (list nil
                        (nelisp-native-load--raw-v2-rooted-cfg-copy-data manifest 0)
                        runtime-key provider-context)
                  nelisp-native-load--raw-v2-rooted-cfg-validation-cache)
            (when (> (length nelisp-native-load--raw-v2-rooted-cfg-validation-cache) 32)
              (setcdr (nthcdr 31 nelisp-native-load--raw-v2-rooted-cfg-validation-cache) nil)))
          valid))))))

(defun nelisp-native-load--raw-v2-check-after-compile
    (manifest name validated-contract digest validator)
  "Check MANIFEST using a receipt from this compile only when still identical."
  (if (and validated-contract
           (not (nelisp-native-load--rooted-cfg-safe-v3-manifest-p manifest))
           (eq (plist-get manifest :native-rooted-cfg-contract) validated-contract)
           (eq validator
               (symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p))
           (equal digest
                  (let ((print-length nil) (print-level nil)
                        (print-circle t) (print-escape-newlines t)
                                 (nelisp--prn-symbol-cache (make-hash-table :test 'equal)))
                    (secure-hash 'sha256 (prin1-to-string validated-contract)))))
      (nelisp-native-load--raw-v2-check manifest name t)
    (nelisp-native-load-raw-v2-check manifest name)))

(defun nelisp-native-load-raw-v2-check (manifest &optional name)
  "Return complete refusal reasons for a v2 raw runtime MANIFEST."
  (nelisp-native-load--raw-v2-check manifest name))

(defun nelisp-native-load--raw-v2-check (manifest name &optional compiled-contract-valid)
  "Return refusal reasons for a full v2 raw runtime MANIFEST.

This check is deliberately complete before mmap: it validates the executable
identity, the shared resolver index, every GC table entry and every import
relocation.  An absent ABI module is a refusal, never a reason to trust the
candidate's self-described table order."
  (if (eq (nelisp-native-load--raw-v2-rooted-import-family manifest) 'template)
      (progn
        (setq nelisp-native-load--raw-v2-check-count (1+ nelisp-native-load--raw-v2-check-count))
        (require 'nelisp-native-template)
        (append (when (and name (not (equal name nelisp-native-template-entry)))
                  (list (list :raw-no-such-export name)))
                (nelisp-native-template-check manifest)))
  (setq nelisp-native-load--raw-v2-check-count
        (1+ nelisp-native-load--raw-v2-check-count))
  (let ((safe-marker (nelisp-native-load--rooted-cfg-safe-v3-manifest-p manifest)))
    (when (memq safe-marker '(:malformed :oversized))
      (error "nelisp-native-load: malformed safe-v3 manifest plist"))
    (when safe-marker
      (require 'nelisp-bytecode-native-rooted-cfg-safe-contract))
    (when (and safe-marker
               (not (nelisp-bytecode-native-rooted-cfg-safe-contract-bounded-data-p
                     manifest)))
      (error "nelisp-native-load: cyclic or oversized safe-v3 manifest")))
  (let* ((print-length nil) (print-level nil)
         (native (nelisp-native-load--raw-native manifest))
         (text (and native (nelisp-native-load--raw-bytes native :text-base64)))
         (text-length (and text (string-bytes text)))
         (exports (and native (nelisp-native-load--raw-exports native)))
         (imports0 (and native (plist-get native :imports)))
         (imports (and (listp imports0)
                       (mapcar #'nelisp-native-load--raw-import-name imports0)))
         (contract (nelisp-native-load--raw-v2-contract))
         (resolver-symbols (nelisp-native-load--raw-v2-symbols))
         (conditional-contract
          (nelisp-native-load--raw-v2-conditional-contract-valid-p manifest))
         (rooted-branch-contract
          (nelisp-native-load--raw-v2-rooted-branch-contract-valid-p manifest))
         (rooted-branch-join-contract
          (nelisp-native-load--raw-v2-rooted-branch-join-contract-valid-p manifest))
         (rooted-cfg-contract
          (if compiled-contract-valid
              (nelisp-native-load--raw-v2-rooted-cfg-contract-valid-slow-p manifest t)
            (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest)))
         (conditional-declared
          (or (plist-get manifest :native-rooted-conditional-contract-version)
              (plist-get manifest :native-rooted-conditional-entry)
              (plist-get manifest :native-rooted-conditional-imports)
              (plist-get manifest :native-rooted-conditional-status-base)
              (plist-get manifest :native-rooted-conditional-contract-hash)))
         (rooted-branch-declared
          (or (plist-get manifest :native-rooted-branch-contract-version)
              (plist-get manifest :native-rooted-branch-entry)
              (plist-get manifest :native-rooted-branch-imports)
              (plist-get manifest :native-rooted-branch-status-base)
              (plist-get manifest :native-rooted-branch-contract-hash)))
         (rooted-branch-join-declared
          (or (plist-get manifest :native-rooted-branch-join-contract-version)
              (plist-get manifest :native-rooted-branch-join-entry)
              (plist-get manifest :native-rooted-branch-join-operation)
              (plist-get manifest :native-rooted-branch-join-imports)
              (plist-get manifest :native-rooted-branch-join-status-base)
              (plist-get manifest :native-rooted-branch-join-contract-hash)))
         (rooted-cfg-declared
          (or (plist-get manifest :native-rooted-cfg-contract-version)
              (plist-get manifest :native-rooted-cfg-contract)
              (plist-get manifest :native-rooted-cfg-import-descriptors)
              (plist-get manifest :native-rooted-cfg-safe-v3-contract-version)
              (plist-get manifest :native-rooted-cfg-safe-v3-contract)
              (plist-get manifest :native-rooted-cfg-safe-v3-entry)
              (plist-get manifest :native-rooted-cfg-safe-v3-imports)
              (plist-get manifest :native-rooted-cfg-safe-v3-import-descriptors)
              (plist-get manifest :native-rooted-cfg-safe-v3-contract-hash)))
         (problems nil)
         (add (lambda (problem) (setq problems (cons problem problems)))))
    (unless (if (nelisp-native-load--windows-p)
                (equal (plist-get manifest :target) (nelisp-native-load--target-v2))
              (null (plist-get manifest :target)))
      (funcall add (list :raw-target (plist-get manifest :target))))
    (unless (eq (plist-get manifest :kind) 'raw-runtime)
      (funcall add (list :raw-kind (plist-get manifest :kind))))
    (unless (eq (plist-get manifest :format)
                nelisp-native-load-raw-artifact-format-v2)
      (funcall add (list :raw-format (plist-get manifest :format))))
    (unless (equal (plist-get manifest :runtime-abi)
                   (nelisp-native-load--runtime-abi-v2))
      (funcall add (list :raw-runtime-abi (plist-get manifest :runtime-abi))))
    (unless (eq (plist-get manifest :runtime-kind) 'gc-arena)
      (funcall add (list :raw-runtime-kind (plist-get manifest :runtime-kind))))
    (unless (eq (plist-get manifest :runtime-opt-in) t)
      (funcall add (list :raw-runtime-opt-in (plist-get manifest :runtime-opt-in))))
    (unless (equal (plist-get manifest :layout-id)
                   nelisp-native-load-raw-layout-id-v2)
      (funcall add (list :raw-layout-id (plist-get manifest :layout-id))))
    (unless (equal (plist-get manifest :arch)
                   nelisp-native-load-raw-supported-arch)
      (funcall add (list :raw-arch (plist-get manifest :arch))))
    (let ((binary (plist-get manifest :binary-sha256)))
      (unless (and (stringp binary)
                   (string-match-p "\\`[0-9a-fA-F]\\{64\\}\\'" binary))
        (funcall add (list :raw-binary-hash-missing binary))))
    (unless native (funcall add (list :raw-no-native-section)))
    (unless contract (funcall add (list :raw-gc-contract-unavailable)))
    (unless resolver-symbols
      (funcall add (list :raw-resolver-contract-unavailable)))
    (when resolver-symbols
      (unless (equal (plist-get manifest :resolver-contract-version)
                     nelisp-native-load-raw-v2-import-contract-version)
        (funcall add (list :raw-resolver-contract-version
                           (plist-get manifest :resolver-contract-version))))
      (unless (equal (plist-get manifest :resolver-contract-hash)
                     (nelisp-native-load--raw-v2-import-contract-hash
                      resolver-symbols))
        (funcall add (list :raw-resolver-contract-hash
                           (plist-get manifest :resolver-contract-hash)))))
    (when native
      (unless (equal (plist-get native :raw-abi)
                     (nelisp-native-load--runtime-abi-v2))
        (funcall add (list :raw-abi (plist-get native :raw-abi))))
      (unless (eq (plist-get native :object-format)
                  'nelisp-aot-raw-unit-v2)
        (funcall add (list :raw-object-format (plist-get native :object-format))))
      (unless (and text text-length
                   (= text-length (or (plist-get native :text-size) -1))
                   (> text-length 0))
        (funcall add (list :raw-text-size (plist-get native :text-size)
                           text-length)))
      (unless (and text-length
                   (= text-length (or (plist-get native :object-size) -1)))
        (funcall add (list :raw-object-size (plist-get native :object-size)
                           text-length)))
      (let ((declared (plist-get native :object-sha256))
            (actual (and text (nelisp-native-load--raw-digest text))))
        (unless (and (stringp declared) (stringp actual)
                     (equal declared actual))
          (funcall add (list :raw-object-hash-mismatch declared actual))))
      (when (> (or (plist-get native :data-size) 0) 0)
        (funcall add (list :raw-data-section-unsupported
                           (plist-get native :data-size))))
      (when (> (or (plist-get native :bss-size) 0) 0)
        (funcall add (list :raw-bss-section-unsupported
                           (plist-get native :bss-size))))
      (when (and conditional-declared (not conditional-contract))
        (funcall add (list :raw-native-rooted-conditional-contract-invalid)))
      (when (and rooted-branch-declared (not rooted-branch-contract))
        (funcall add (list :raw-native-rooted-branch-contract-invalid)))
      (when (and rooted-branch-join-declared (not rooted-branch-join-contract))
        (funcall add (list :raw-native-rooted-branch-join-contract-invalid)))
      (when (and rooted-cfg-declared (not rooted-cfg-contract))
        (funcall add (list :raw-native-rooted-cfg-contract-invalid)))
      (unless (and (listp exports) exports)
        (funcall add (list :raw-no-exports)))
    (unless (and (listp imports0) (not (memq nil imports))
                 (= (length imports)
                      (length (delete-dups (copy-sequence imports)))))
        (funcall add (list :raw-imports-not-explicit imports0)))
      (let* ((root-fields (list (plist-get manifest :native-rooted-stack-contract-version)
                                (plist-get manifest :native-rooted-stack-entry)
                                (plist-get manifest :native-rooted-stack-gateway-imports)
                                (plist-get manifest :native-rooted-stack-status-base)
                                (plist-get manifest :native-rooted-stack-contract-hash)))
             (rooted-stack (cl-some #'identity root-fields)))
        (when rooted-stack
          (let ((export (nelisp-native-load--raw-export
                         (nelisp-native-load--raw-native manifest)
                         "nl_native_stack_probe_v1")))
            (unless (and (equal (plist-get manifest :native-rooted-stack-contract-version)
                                nelisp-native-load-rooted-stack-contract-version)
                         (equal (plist-get manifest :native-rooted-stack-entry) "nl_native_stack_probe_v1")
                         (equal (plist-get manifest :native-rooted-stack-gateway-imports)
                                (sort (copy-sequence imports) #'string<))
                         (member (sort (copy-sequence imports) #'string<)
                                 '( ("nl_native_car_v2") ("nl_native_cdr_v2") ("nl_native_cons_v2")
                                    ("nl_native_car_v2" "nl_native_cdr_v2")
                                    ("nl_native_car_v2" "nl_native_cons_v2")
                                    ("nl_native_cdr_v2" "nl_native_cons_v2")
                                    ("nl_native_car_v2" "nl_native_cdr_v2" "nl_native_cons_v2")))
                         (equal (plist-get manifest :native-rooted-stack-status-base) 256)
                         (equal (plist-get manifest :native-rooted-stack-contract-hash)
                                (nelisp-native-load--rooted-stack-contract-hash imports))
                         export (= (plist-get export :arity) 4)
                         (equal (plist-get export :params) '(u64 u64 u64 u64))
                         (eq (plist-get export :return) 'u64)
                         (eq (plist-get export :type) 'func)
                         (equal (plist-get export :abi) (nelisp-native-load--runtime-abi-v2))
                         (not (or (plist-get manifest :native-object-op-contract-version)
                                  (plist-get manifest :native-object-op-gateway-imports)
                                  (plist-get manifest :native-object-opcodes)
                                  (plist-get manifest :native-object-op-contract-hash))))
              (funcall add (list :raw-native-rooted-stack-contract-invalid)))))
        (if (and (not (or rooted-stack rooted-branch-contract rooted-cfg-contract))
                 (member "nl_native_car_v2" imports)
               (member "nl_native_cdr_v2" imports))
          (unless (and
                   (equal imports '("nl_native_car_v2" "nl_native_cdr_v2"))
                   (equal (plist-get manifest
                                     :native-object-op-contract-version)
                          nelisp-native-load-native-object-op-contract-version)
                   (equal (plist-get manifest
                                     :native-object-op-gateway-imports)
                          '("nl_native_car_v2" "nl_native_cdr_v2"))
                   (equal (plist-get manifest :native-object-opcodes)
                          nelisp-native-load-native-object-opcodes)
                   (equal (plist-get manifest
                                     :native-object-op-contract-hash)
                          (nelisp-native-load--native-object-op-contract-hash)))
            (funcall add (list :raw-native-object-op-contract
                               (plist-get manifest
                                          :native-object-op-contract-version)
                               (plist-get manifest :native-object-opcodes))))
        (when (and (not (or rooted-stack rooted-branch-contract rooted-cfg-contract))
                   (or (plist-get manifest :native-object-op-contract-version)
                  (plist-get manifest :native-object-op-gateway-imports)
                  (plist-get manifest :native-object-opcodes)
                  (plist-get manifest :native-object-op-contract-hash)))
          (funcall add (list :raw-native-object-op-contract-unexpected)))))
      (when resolver-symbols
        (let ((rest imports0))
          (while rest
            (let* ((entry (car rest))
                   (import (nelisp-native-load--raw-import-name entry))
                   (kind (nelisp-native-load--raw-import-kind entry))
                   (index (and (listp entry) (plist-get entry :index)))
                   (call1-p (and (equal import nelisp-native-load-raw-v2-call1-import)
                                 (nelisp-native-load--raw-v2-call1-import-valid-p
                                  manifest entry)))
                   (conditional-import-p
                    (and (or conditional-contract rooted-branch-contract
                             rooted-branch-join-contract rooted-cfg-contract
                             (nelisp-native-load--rooted-cfg-safe-v3-manifest-p manifest))
                         (nelisp-native-load--raw-v2-conditional-import-mode import)))
                   (provider (and rooted-cfg-contract
                                  (nelisp-native-load--rooted-cfg-provider-import
                                   rooted-cfg-contract import)))
                   (mode (cond (provider 'arithmetic-provider-v1)
                               (call1-p 'call1-typed-v1)
                               (conditional-import-p conditional-import-p)
                               (t (nelisp-native-load--raw-v2-import-mode import))))
                   (expected (cond
                              ((or provider call1-p) (cl-position import nelisp-native-load-bridgeable-symbols
                                                    :test #'equal))
                              (conditional-import-p
                               (nelisp-native-load--raw-v2-conditional-import-index import))
                              ((stringp import)
                               (nelisp-native-load--raw-v2-import-index
                                import resolver-symbols))))
                   (call1-import-p
                    (equal import nelisp-native-load-raw-v2-call1-import)))
              (unless (and (stringp import) mode
                           (eq (plist-get entry :address-mode) mode)
                           (integerp expected) (integerp index)
                           (= index expected))
                (funcall add (list :raw-import-index import index expected)))
              (unless (memq kind '(func data))
                (funcall add (list :raw-import-kind import kind)))
              (let ((expected-kind (if (member import nelisp-native-load-raw-v2-data-symbols)
                                       'data 'func)))
                (unless (eq kind expected-kind)
                  (funcall add (list :raw-import-kind-mismatch import
                                      kind expected-kind))))
              (unless (and (listp entry)
                           (equal (plist-get entry :abi)
                                  (nelisp-native-load--runtime-abi-v2)))
                (funcall add (list :raw-import-abi import
                                   (and (listp entry) (plist-get entry :abi)))))
              (when (and provider
                         (not (nelisp-native-load--rooted-cfg-provider-import-valid-p
                               entry rooted-cfg-contract)))
                (funcall add (list :raw-arithmetic-provider-import entry)))
              (when (and call1-import-p
                         (not (nelisp-native-load--raw-v2-call1-import-valid-p
                               manifest entry)))
                (funcall add (list :raw-call1-contract entry)))
              (when (and (equal import "nl_root_pin_slot_v2")
                         (not conditional-import-p))
                (funcall add (list :raw-conditional-slot-import-untyped entry)))
              (when (and (eq kind 'data)
                         (not (member import
                                      nelisp-native-load-raw-v2-data-symbols)))
                (funcall add (list :raw-data-import-not-shared import))))
              (setq rest (cdr rest)))))
      (when (and (not (member nelisp-native-load-raw-v2-call1-import imports))
                 (or (plist-get manifest :call1-contract-version)
                     (plist-get manifest :call1-contract-hash)
                     (plist-get manifest :call1-caller)))
        (funcall add (list :raw-call1-contract-unexpected)))
      (let ((rest (plist-get native :relocs)))
        (while rest
          (let ((bad (nelisp-native-load--raw-reloc-problem
                      (car rest) (or text-length 0) imports)))
            (when bad (funcall add bad)))
          (setq rest (cdr rest))))
      (let ((rest exports) (seen nil))
        (while rest
          (let ((entry (car rest)) (offset (plist-get (car rest) :value))
                (size (plist-get (car rest) :size))
                (arity (plist-get (car rest) :arity)))
            (unless (and (stringp (plist-get entry :name))
                         (integerp offset) (>= offset 0)
                         (integerp size) (> size 0) text-length
                         (<= (+ offset size) text-length))
              (funcall add (list :raw-export-range entry)))
            (unless (and (integerp arity) (>= arity 0)
                         (<= arity nelisp-native-load-raw-max-arity-v2))
              (funcall add (list :raw-export-arity
                                 (plist-get entry :name) arity)))
            (when (member (plist-get entry :name) seen)
              (funcall add (list :raw-export-duplicate (plist-get entry :name))))
            (setq seen (cons (plist-get entry :name) seen))
            (unless (and (eq (plist-get entry :type) 'func)
                         (equal (plist-get entry :abi)
                                (nelisp-native-load--runtime-abi-v2))
                         (eq (plist-get entry :return) 'u64))
              (funcall add (list :raw-export-abi entry))))
          (setq rest (cdr rest)))))
    (unless (memq (plist-get manifest :gc-address-mode)
                  '(nil artifact-export-v1 runtime-bridge-v1))
      (funcall add (list :raw-gc-address-mode (plist-get manifest :gc-address-mode))))
    (when contract
      (let ((entries (plist-get manifest :gc-entries)) (i 0))
        (unless (= (length entries) (length contract))
          (funcall add (list :raw-gc-entry-count (length entries)
                             (length contract))))
        (dolist (expected contract)
          (let* ((name (car expected)) (arity (cdr expected))
                 (entry (nelisp-native-load--raw-v2-gc-entry manifest name))
                 (index (and entry (plist-get entry :index)))
                 (actual-arity (and entry (plist-get entry :arity)))
                 (export (and native
                              (nelisp-native-load--raw-export native name))))
            (unless (and entry (= index i) (= actual-arity arity))
              (funcall add (list :raw-gc-entry name index i actual-arity arity)))
            (if (eq (plist-get manifest :gc-address-mode) 'runtime-bridge-v1)
                (when (or export
                          (not (integerp (cl-position name nelisp-native-load-bridgeable-symbols :test #'equal))))
                  (funcall add (list :raw-gc-runtime-entry name)))
              (unless export
                (funcall add (list :raw-gc-export-missing name))))
            (when (and export (/= (plist-get export :arity) arity))
              (funcall add (list :raw-gc-export-arity name
                                 (plist-get export :arity) arity)))
            (when entry
              (unless (equal (plist-get entry :abi)
                             (nelisp-native-load--runtime-abi-v2))
                (funcall add (list :raw-gc-entry-abi name
                                   (plist-get entry :abi)))))
            (setq i (1+ i)))))
      (unless (equal (plist-get manifest :gc-contract-hash)
                     (nelisp-native-load--raw-v2-contract-hash contract))
        (funcall add (list :raw-gc-contract-hash
                           (plist-get manifest :gc-contract-hash))))
      (unless (= (or (plist-get manifest :gc-table-count) -1)
                 (length contract))
        (funcall add (list :raw-gc-table-count
                           (plist-get manifest :gc-table-count)
                           (length contract)))))
    (unless (= (or (plist-get manifest :gc-table-magic) 0)
               nelisp-native-load-raw-gc-table-magic)
      (funcall add (list :raw-gc-table-magic
                         (plist-get manifest :gc-table-magic))))
    (let* ((declared (plist-get manifest :artifact-sha256))
           (canonical (and (stringp declared)
                           (prin1-to-string
                            (nelisp-native-load--raw-plist-without
                             manifest :artifact-sha256))))
           (actual (and canonical (nelisp-native-load--sha256 canonical))))
      (unless (stringp declared)
        (funcall add (list :raw-artifact-hash-missing declared)))
      (when (and (stringp actual) (not (equal declared actual)))
        (funcall add (list :raw-artifact-hash-mismatch declared actual))))
    (when name
      (when (and (plist-get manifest :native-rooted-branch-contract-version)
                 (not (equal name "nl_native_rooted_branch_probe_v1")))
        (funcall add (list :raw-native-rooted-branch-selected-entry name)))
      (when (and (plist-get manifest :native-rooted-branch-join-contract-version)
                 (not (equal name "nl_native_rooted_branch_join_probe_v1")))
        (funcall add (list :raw-native-rooted-branch-join-selected-entry name)))
      (when (and (plist-get manifest :native-rooted-stack-contract-version)
                 (not (equal name "nl_native_stack_probe_v1")))
        (funcall add (list :raw-native-rooted-stack-selected-entry name)))
      (when (and (plist-get manifest :native-rooted-cfg-contract-version)
                 (not (equal name
                             (if (member (plist-get manifest :native-rooted-cfg-contract-version)
                                         (list nelisp-bytecode-native-rooted-cfg-contract-shared-version
                                               nelisp-bytecode-native-rooted-cfg-contract-f1-shared-version))
                                 nelisp-bytecode-native-rooted-cfg-contract-shared-entry
                               "nl_native_rooted_cfg_probe_v1"))))
        (funcall add (list :raw-native-rooted-cfg-selected-entry name)))
      (when (and (plist-get manifest :native-rooted-cfg-safe-v3-contract-version)
                 name (not (equal name "nl_native_rooted_cfg_safe_probe_v3")))
        (funcall add (list :raw-native-rooted-cfg-safe-v3-selected-entry name)))
      (unless (and native (nelisp-native-load--raw-export native name))
        (funcall add (list :raw-no-such-export name))))
    (when (member nelisp-native-load-raw-v2-call1-import imports)
      (let ((entry (nelisp-native-load--raw-export
                    native nelisp-native-load-raw-v2-call1-entry)))
        (unless (and entry (= (or (plist-get entry :arity) -1) 2)
                     (eq (plist-get entry :return) 'u64)
                     (equal (plist-get entry :params) '(u64 u64))
                     (equal (plist-get (plist-get manifest :call1-caller) :name)
                            nelisp-native-load-raw-v2-call1-entry))
          (funcall add (list :raw-call1-entry entry)))
        (when (and name (not (equal name nelisp-native-load-raw-v2-call1-entry)))
          (funcall add (list :raw-call1-selected-entry name)))))
    (nreverse problems))))

(defun nelisp-native-load-raw-v2-artifact
    (path &optional name expected-binary-sha256 origin)
  "Map v2 raw runtime artifact PATH and return its handle.

Function imports receive the usual absolute jump stub.  Shared data imports
receive a `movabs rax, ADDRESS; ret' getter, so the mapped text never embeds
a private copy of runtime BSS.  The GC contract table is allocated beside the
code and retained by the handle; its first two words are the count and magic,
followed by the 24 contract entry addresses in ABI order."
  (let* ((manifest (if (stringp path) (nelisp-native-load-manifest path) path))
         (origin (if (stringp path) path origin))
         (native (nelisp-native-load--raw-native manifest))
         (exports (and native (nelisp-native-load--raw-exports native)))
         (chosen (or name (and exports (plist-get (car exports) :name))))
         (problems (nelisp-native-load-raw-v2-check manifest chosen))
         (declared-binary (plist-get manifest :binary-sha256)))
    (when problems
      (error "nelisp-native-load: cannot load v2 raw %s from %s: %S"
             chosen origin problems))
    (when (fboundp 'nelisp--native-runtime-symbol-addr)
      (unless (and (stringp expected-binary-sha256)
                   (equal declared-binary expected-binary-sha256))
        (error "nelisp-native-load: v2 binary identity mismatch for %s" origin)))
    (let ((running (nelisp-native-load--running-binary-sha256)))
      (unless (and (stringp expected-binary-sha256)
                   (stringp running)
                   (equal declared-binary running)
                   (equal expected-binary-sha256 running))
        (error "nelisp-native-load: v2 running binary identity unavailable or mismatched")))
    (unless (nelisp-runtime-reload-contract-matches-p)
      (error "nelisp-native-load: running native runtime contract mismatch"))
    (unless (nelisp-native-load--raw-supported-p)
      (error "nelisp-native-load: v2 raw units require Linux x86_64 primitives"))
    (let* ((text (nelisp-native-load--raw-bytes native :text-base64))
           (text-length (string-bytes text))
           (imports0 (plist-get native :imports))
           (stub-base (* 16 (/ (+ text-length 15) 16)))
           (code-size (nelisp-native-load--page-round
                       (+ stub-base (* nelisp-native-load-stub-bytes
                                       (length imports0)))))
           (codepage nil) (table nil) (table-size nil)
           (stub-offsets nil) (success nil))
      (nelisp-native-budget-reserve
       (+ code-size (nelisp-native-load--page-round
                     (+ 16 (* 8 (length (plist-get manifest :gc-entries)))))))
      (unwind-protect
          (progn
            (setq codepage (nelisp-native-load--mmap code-size nil))
            (nelisp-native-load--poke-string codepage 0 text)
            (let ((rest imports0) (idx 0))
              (while rest
                (let* ((entry (car rest))
                       (import (nelisp-native-load--raw-import-name entry))
                       (kind (nelisp-native-load--raw-import-kind entry))
                       (offset (+ stub-base (* nelisp-native-load-stub-bytes idx)))
                       (addr (nelisp-native-load--raw-v2-symbol-addr
                              import entry manifest)))
                  (setq stub-offsets (cons (cons import offset) stub-offsets))
                  (if (eq kind 'data)
                      (nelisp-native-load--poke-bytes
                       codepage offset
                       '(#x48 #xb8 0 0 0 0 0 0 0 0 #xc3))
                    (nelisp-native-load--poke-bytes
                     codepage offset
                     '(#x48 #xb8 0 0 0 0 0 0 0 0 #xff #xe0)))
                  (ptr-write-u64 codepage (+ offset 2) addr))
                (setq idx (1+ idx) rest (cdr rest))))
            (let ((rest (plist-get native :relocs)))
              (while rest
                (let* ((reloc (car rest))
                       (offset (plist-get reloc :offset))
                       (symbol (plist-get reloc :symbol))
                       (addend (or (plist-get reloc :addend) 0))
                       (stub (assoc symbol stub-offsets)))
                  (unless stub
                    (error "nelisp-native-load: v2 relocation has no stub: %s"
                           symbol))
                  (ptr-write-u32 codepage offset
                                 (- (+ codepage (cdr stub) addend)
                                    (+ codepage offset))))
                (setq rest (cdr rest))))
            ;; Table memory is writable only during construction, then R.
            (setq table-size (+ 16 (* 8 (length (plist-get manifest :gc-entries)))))
            (setq table (nelisp-native-load--mmap
                         (nelisp-native-load--page-round table-size) nil))
            (ptr-write-u64 table 0 (length (plist-get manifest :gc-entries)))
            (ptr-write-u64 table 8 nelisp-native-load-raw-gc-table-magic)
            (let ((rest (plist-get manifest :gc-entries)))
              (while rest
                (let* ((entry (car rest))
                       (addr (nelisp-native-load--raw-v2-gc-address
                              entry manifest exports codepage)))
                  (ptr-write-u64 table (+ 16 (* 8 (plist-get entry :index)))
                                 addr))
                (setq rest (cdr rest))))
            (unless (= 0 (nelisp-native-load--protect table (nelisp-native-load--page-round table-size) 1))
              (error "nelisp-native-load: mprotect read-only GC table failed"))
            (nelisp-native-load--mprotect-rx codepage code-size)
            (let ((addresses nil) (rest exports))
              (while rest
                (let ((entry (car rest)))
                  (push (cons (plist-get entry :name)
                              (+ codepage (plist-get entry :value))) addresses))
                (setq rest (cdr rest)))
              (setq addresses (nreverse addresses))
              (let ((handle (list :kind 'raw-runtime-v2 :path origin
                                  :entry (cdr (assoc chosen addresses))
                                  :entry-name chosen :exports addresses
                                  :codepage codepage :code-size code-size
                                  :gc-table table :gc-table-size table-size
                                  :runtime-abi (plist-get manifest :runtime-abi)
                                  :raw-abi (plist-get native :raw-abi)
                                  :layout-id (plist-get manifest :layout-id)
                                  :build-id (plist-get manifest :build-id)
                                  :binary-sha256 declared-binary
                                  :imports (mapcar #'nelisp-native-load--raw-import-name imports0)
                                  :native-object-op-contract-version
                                  (plist-get manifest
                                             :native-object-op-contract-version)
                                  :native-object-op-gateway-imports
                                  (plist-get manifest
                                             :native-object-op-gateway-imports)
                                  :native-object-opcodes
                                  (plist-get manifest :native-object-opcodes)
                                  :native-object-op-contract-hash
                                  (plist-get manifest
                                             :native-object-op-contract-hash)
                                  :source-sha256 (plist-get manifest :source-sha256)
                                  :artifact-sha256 (plist-get manifest :artifact-sha256)
                                  :object-sha256 (plist-get native :object-sha256)
                                  :arity (plist-get (nelisp-native-load--raw-export
                                                     native chosen) :arity)
                                  :retained t)))
                (push handle nelisp-native-load-raw-mappings)
                (setq success t)
                handle)))
        (unless success
          (when (and (integerp codepage) (> codepage 0))
            (ignore-errors (nelisp-native-load--unmap codepage code-size)))
          (when (and (integerp table) (> table 0))
            (ignore-errors
              (nelisp-native-load--unmap table (nelisp-native-load--page-round table-size)))))))))

(defconst nelisp-native-load--trusted-max-text-bytes (* 64 1024 1024))
(defconst nelisp-native-load--trusted-max-entries 65536)

(defun nelisp-native-load--trusted-list-p (value)
  "Accept only proper, bounded, acyclic VALUE lists."
  (let ((n (proper-list-p value)))
    (and n (<= n nelisp-native-load--trusted-max-entries))))

(defun nelisp-native-load--raw-v2-gc-address (entry manifest exports codepage)
  "Resolve GC ENTRY from the authenticated runtime or embedded generation."
  (let* ((name (plist-get entry :name))
         (runtime (eq (plist-get manifest :gc-address-mode) 'runtime-bridge-v1))
         (expected (nth (plist-get entry :index) (nelisp-native-load--raw-v2-contract)))
         (export (cl-find name exports :key (lambda (item) (plist-get item :name)) :test #'equal))
         (address
          (if runtime
              (progn
                (unless (and (equal name (car expected))
                             (eql (plist-get entry :arity) (cdr expected))
                             (null export))
                  (error "nelisp-native-load: runtime GC entry refused"))
                (nelisp-native-load--symbol-addr name))
            (and export (+ codepage (plist-get export :value))))))
    (unless (and (integerp address) (> address nelisp-native-load-page-bytes))
      (error "nelisp-native-load: GC address unavailable: %s" name))
    address))

(defun nelisp-native-load--raw-v2-symbol-addr-trusted (name &optional entry _manifest)
  "Resolve a compile-validated import without semantic contract validation."
  (let ((address
         (if (or (member name nelisp-native-load-raw-v2-bridgeable-imports)
                 (assoc name (nelisp-native-load--raw-v2-contract))
                 (memq (plist-get entry :address-mode)
                       '(arithmetic-provider-v1 call1-typed-v1))
                 (nelisp-native-load--raw-v2-conditional-import-mode name))
             (nelisp-native-load--symbol-addr name)
           (nelisp-native-load--raw-symbol-addr name))))
    (unless (and (integerp address) (> address nelisp-native-load-page-bytes))
      (error "nelisp-native-load: trusted import resolution failed"))
    address))

(defvar nelisp-native-load--serialization-receiver nil
  "Call-local receiver of the producer's signed manifest and prebuilt bytes.")
(defvar nelisp-native-load--trusted-serialization nil
  "Call-local pair of parsed manifest and its unsigned canonical snapshot.")

(defun nelisp-native-load--signed-manifest (unsigned digest)
  "Append DIGEST to the already printed UNSIGNED manifest.
The wire representation and authenticated bytes remain unchanged."
  (concat (substring unsigned 0 -1) " :artifact-sha256 "
          (prin1-to-string digest) ")"))

(defun nelisp-native-load--raw-v2-trusted-decode (manifest name)
  "Decode MANIFEST, checking memory safety independently of semantic admission."
  (unless (and (nelisp-native-load--trusted-list-p manifest)
               (zerop (% (length manifest) 2)))
    (error "nelisp-native-load: malformed trusted manifest"))
  (let* ((native (nelisp-native-load--raw-native manifest))
         (_native-shape
          (unless (and (nelisp-native-load--trusted-list-p native)
                       (zerop (% (length native) 2)))
            (error "nelisp-native-load: malformed trusted native section")))
         (encoded (plist-get native :text-base64))
         (imports (plist-get native :imports))
         (exports (nelisp-native-load--raw-exports native))
         (relocs (plist-get native :relocs))
         (entries (plist-get manifest :gc-entries))
         (contract (nelisp-native-load--raw-v2-contract))
         (text nil) (names nil) (export-names nil) (indices nil))
    (unless (and (nelisp-native-load--trusted-list-p native)
                 (stringp encoded)
                 (<= (length encoded)
                     (* 2 nelisp-native-load--trusted-max-text-bytes))
                 (cl-every #'nelisp-native-load--trusted-list-p
                           (list imports exports relocs entries))
                 exports contract)
      (error "nelisp-native-load: trusted structural decode refused"))
    (unless (and (equal (plist-get manifest :runtime-abi) (nelisp-native-load--runtime-abi-v2))
                 (equal (plist-get native :raw-abi) (nelisp-native-load--runtime-abi-v2))
                 (if (nelisp-native-load--windows-p)
                     (equal (plist-get manifest :target) (nelisp-native-load--target-v2))
                   (null (plist-get manifest :target))))
      (error "nelisp-native-load: trusted target/ABI refused"))
    ;; Authenticate the complete stored artifact once, before executable mapping.
    ;; This covers the encoded object and its metadata without replanning code.
    (let ((print-length nil) (print-level nil)
          (declared (plist-get manifest :artifact-sha256)))
      (unless (and (stringp declared)
                   (equal declared
                          (nelisp-native-load--sha256
                           (if (eq (car nelisp-native-load--trusted-serialization) manifest)
                               (cdr nelisp-native-load--trusted-serialization)
                             (prin1-to-string
                              (nelisp-native-load--raw-plist-without
                               manifest :artifact-sha256))))))
        (error "nelisp-native-load: trusted artifact hash refused")))
    (setq text (nelisp-native-load--raw-bytes native :text-base64))
    (unless (and (stringp text) (> (string-bytes text) 0)
                 (<= (string-bytes text) nelisp-native-load--trusted-max-text-bytes)
                 (eql (string-bytes text) (plist-get native :text-size))
                 (eql (string-bytes text) (plist-get native :object-size))
                 (zerop (or (plist-get native :data-size) 0))
                 (zerop (or (plist-get native :bss-size) 0)))
      (error "nelisp-native-load: trusted text size refused"))
    (dolist (entry imports)
      (unless (and (nelisp-native-load--trusted-list-p entry)
                   (stringp (plist-get entry :name))
                   (not (member (plist-get entry :name) names))
                   (memq (nelisp-native-load--raw-import-kind entry) '(func data)))
        (error "nelisp-native-load: malformed trusted import"))
      (push (plist-get entry :name) names))
    (dolist (entry exports)
      (unless (nelisp-native-load--trusted-list-p entry)
        (error "nelisp-native-load: malformed trusted export"))
      (let ((offset (plist-get entry :value)) (size (plist-get entry :size)))
        (unless (and (nelisp-native-load--trusted-list-p entry)
                     (stringp (plist-get entry :name))
                     (not (member (plist-get entry :name) export-names))
                     (integerp offset) (>= offset 0)
                     (integerp size) (> size 0)
                     (<= (+ offset size) (string-bytes text))
                     (integerp (plist-get entry :arity))
                     (<= 0 (plist-get entry :arity) nelisp-native-load-raw-max-arity-v2))
          (error "nelisp-native-load: trusted export bounds refused"))
        (push (plist-get entry :name) export-names)))
    (unless (member name export-names)
      (error "nelisp-native-load: trusted entry missing"))
    (dolist (reloc relocs)
      (unless (and (nelisp-native-load--trusted-list-p reloc)
                   (not (nelisp-native-load--raw-reloc-problem
                         reloc (string-bytes text) names)))
        (error "nelisp-native-load: trusted relocation refused")))
    (unless (and (= (length entries) (length contract))
                 (eql (plist-get manifest :gc-table-count) (length entries))
                 (eql (plist-get manifest :gc-table-magic)
                      nelisp-native-load-raw-gc-table-magic))
      (error "nelisp-native-load: trusted GC table count refused"))
    (unless (and (eq (plist-get manifest :format) nelisp-native-load-raw-artifact-format-v2)
                 (memq (plist-get manifest :gc-address-mode)
                       '(nil artifact-export-v1 runtime-bridge-v1)))
      (error "nelisp-native-load: trusted GC format refused"))
    (dolist (entry entries)
      (unless (nelisp-native-load--trusted-list-p entry)
        (error "nelisp-native-load: malformed trusted GC entry"))
      (let ((index (plist-get entry :index)))
        (unless (and (nelisp-native-load--trusted-list-p entry)
                     (integerp index) (<= 0 index) (< index (length entries))
                     (not (memq index indices))
                     (if (eq (plist-get manifest :gc-address-mode) 'runtime-bridge-v1)
                         (let ((expected (nth index contract)))
                           (and (equal (plist-get entry :name) (car expected))
                                (eql (plist-get entry :arity) (cdr expected))
                                (not (member (plist-get entry :name) export-names))))
                       (member (plist-get entry :name) export-names)))
          (error "nelisp-native-load: trusted GC table index refused"))
        (push index indices)))
    text))

(defun nelisp-native-load-raw-v2-artifact-trusted (manifest name origin)
  "Map private-cache MANIFEST after the caller checked the process ABI.
Only semantic validation is skipped; all memory boundaries remain checked."
  (let* ((native (nelisp-native-load--raw-native manifest))
         (exports (and native (nelisp-native-load--raw-exports native)))
         (chosen name)
         (declared-binary (plist-get manifest :binary-sha256))
         (decoded (nelisp-native-load--raw-v2-trusted-decode manifest name)))
    (setq nelisp-native-load--trusted-map-count
          (1+ nelisp-native-load--trusted-map-count))
    (let* ((text decoded)
           (text-length (string-bytes text))
           (imports0 (plist-get native :imports))
           (stub-base (* 16 (/ (+ text-length 15) 16)))
           (code-size (nelisp-native-load--page-round
                       (+ stub-base (* nelisp-native-load-stub-bytes
                                       (length imports0)))))
           (codepage nil) (table nil) (table-size nil)
           (stub-offsets nil) (success nil))
      (nelisp-native-budget-reserve
       (+ code-size (nelisp-native-load--page-round
                     (+ 16 (* 8 (length (plist-get manifest :gc-entries)))))))
      (unwind-protect
          (progn
            (setq codepage (nelisp-native-load--mmap code-size nil))
            (nelisp-native-load--poke-string codepage 0 text)
            (let ((rest imports0) (idx 0))
              (while rest
                (let* ((entry (car rest))
                       (import (nelisp-native-load--raw-import-name entry))
                       (kind (nelisp-native-load--raw-import-kind entry))
                       (offset (+ stub-base (* nelisp-native-load-stub-bytes idx)))
                       (addr (nelisp-native-load--raw-v2-symbol-addr-trusted
                              import entry manifest)))
                  (setq stub-offsets (cons (cons import offset) stub-offsets))
                  (if (eq kind 'data)
                      (nelisp-native-load--poke-bytes
                       codepage offset
                       '(#x48 #xb8 0 0 0 0 0 0 0 0 #xc3))
                    (nelisp-native-load--poke-bytes
                     codepage offset
                     '(#x48 #xb8 0 0 0 0 0 0 0 0 #xff #xe0)))
                  (ptr-write-u64 codepage (+ offset 2) addr))
                (setq idx (1+ idx) rest (cdr rest))))
            (let ((rest (plist-get native :relocs)))
              (while rest
                (let* ((reloc (car rest))
                       (offset (plist-get reloc :offset))
                       (symbol (plist-get reloc :symbol))
                       (addend (or (plist-get reloc :addend) 0))
                       (stub (assoc symbol stub-offsets)))
                  (unless stub
                    (error "nelisp-native-load: v2 relocation has no stub: %s"
                           symbol))
                  (let ((displacement (- (+ (cdr stub) addend) offset)))
                    (unless (and (>= displacement (- (expt 2 31)))
                                 (< displacement (expt 2 31)))
                      (error "nelisp-native-load: trusted relocation displacement overflow"))
                    (ptr-write-u32 codepage offset displacement)))
                (setq rest (cdr rest))))
            ;; Table memory is writable only during construction, then R.
            (setq table-size (+ 16 (* 8 (length (plist-get manifest :gc-entries)))))
            (setq table (nelisp-native-load--mmap
                         (nelisp-native-load--page-round table-size) nil))
            (ptr-write-u64 table 0 (length (plist-get manifest :gc-entries)))
            (ptr-write-u64 table 8 nelisp-native-load-raw-gc-table-magic)
            (let ((rest (plist-get manifest :gc-entries)))
              (while rest
                (let* ((entry (car rest))
                       (addr (nelisp-native-load--raw-v2-gc-address
                              entry manifest exports codepage)))
                  (ptr-write-u64 table (+ 16 (* 8 (plist-get entry :index)))
                                 addr))
                (setq rest (cdr rest))))
            (unless (= 0 (nelisp-native-load--protect table (nelisp-native-load--page-round table-size) 1))
              (error "nelisp-native-load: mprotect read-only GC table failed"))
            (nelisp-native-load--mprotect-rx codepage code-size)
            (let ((addresses nil) (rest exports))
              (while rest
                (let ((entry (car rest)))
                  (push (cons (plist-get entry :name)
                              (+ codepage (plist-get entry :value))) addresses))
                (setq rest (cdr rest)))
              (setq addresses (nreverse addresses))
              (let ((handle (list :kind 'raw-runtime-v2 :path origin
                                  :entry (cdr (assoc chosen addresses))
                                  :entry-name chosen :exports addresses
                                  :codepage codepage :code-size code-size
                                  :gc-table table :gc-table-size table-size
                                  :runtime-abi (plist-get manifest :runtime-abi)
                                  :raw-abi (plist-get native :raw-abi)
                                  :layout-id (plist-get manifest :layout-id)
                                  :build-id (plist-get manifest :build-id)
                                  :binary-sha256 declared-binary
                                  :imports (mapcar #'nelisp-native-load--raw-import-name imports0)
                                  :native-object-op-contract-version
                                  (plist-get manifest
                                             :native-object-op-contract-version)
                                  :native-object-op-gateway-imports
                                  (plist-get manifest
                                             :native-object-op-gateway-imports)
                                  :native-object-opcodes
                                  (plist-get manifest :native-object-opcodes)
                                  :native-object-op-contract-hash
                                  (plist-get manifest
                                             :native-object-op-contract-hash)
                                  :source-sha256 (plist-get manifest :source-sha256)
                                  :artifact-sha256 (plist-get manifest :artifact-sha256)
                                  :object-sha256 (plist-get native :object-sha256)
                                  :arity (plist-get (nelisp-native-load--raw-export
                                                     native chosen) :arity)
                                  :retained t)))
                (push handle nelisp-native-load-raw-mappings)
                (setq success t)
                handle)))
        (unless success
          (when (and (integerp codepage) (> codepage 0))
            (ignore-errors (nelisp-native-load--unmap codepage code-size)))
          (when (and (integerp table) (> table 0))
            (ignore-errors
              (nelisp-native-load--unmap table (nelisp-native-load--page-round table-size)))))))))


(defun nelisp-native-load-raw-check (manifest &optional name)
  "Return structured refusal reasons for raw MANIFEST and optional NAME.

This is pure and does not resolve symbols or map memory.  A host compiler can
therefore use it to validate target/ABI/layout before a standalone reader is
asked to execute the artifact."
  (let* ((native (nelisp-native-load--raw-native manifest))
         (exports (and native (nelisp-native-load--raw-exports native)))
         (imports0 (and native (plist-get native :imports)))
         (imports (and (listp imports0)
                       (mapcar #'nelisp-native-load--raw-import-name imports0)))
         (text (and native (nelisp-native-load--raw-bytes native :text-base64)))
         (text-length (and text (string-bytes text)))
         (problems nil)
         (entry nil))
    (unless (eq (plist-get manifest :kind) 'raw-runtime)
      (setq problems
            (cons (list :raw-kind (plist-get manifest :kind)) problems)))
    (unless (eq (plist-get manifest :format)
                nelisp-native-load-raw-artifact-format)
      (setq problems
            (cons (list :raw-format (plist-get manifest :format)) problems)))
    (unless (equal (plist-get manifest :runtime-abi)
                   nelisp-native-load-raw-runtime-abi)
      (setq problems
            (cons (list :raw-runtime-abi (plist-get manifest :runtime-abi))
                  problems)))
    (unless (eq (plist-get manifest :runtime-opt-in) t)
      (setq problems
            (cons (list :raw-runtime-opt-in (plist-get manifest :runtime-opt-in))
                  problems)))
    (unless (equal (plist-get manifest :layout-id)
                   nelisp-native-load-raw-layout-id)
      (setq problems
            (cons (list :raw-layout-id (plist-get manifest :layout-id))
                  problems)))
    ;; The consumer identity is part of the raw artifact contract.  A
    ;; compiler that cannot identify its eventual reader must fail staging;
    ;; the loader never treats an absent or malformed digest as a wildcard.
    (let ((binary-sha256 (plist-get manifest :binary-sha256)))
      (unless (and (stringp binary-sha256)
                   (string-match-p "\\`[0-9a-fA-F]\\{64\\}\\'"
                                   binary-sha256))
        (setq problems
              (cons (list :raw-binary-hash-missing binary-sha256)
                    problems))))
    (unless (equal (plist-get manifest :arch)
                   nelisp-native-load-raw-supported-arch)
      (setq problems
            (cons (list :raw-arch (plist-get manifest :arch)) problems)))
    (unless (and (eq system-type 'gnu/linux)
                 (or (not (boundp 'system-configuration))
                     (not (stringp system-configuration))
                     (string-match-p "x86_64\\|amd64" system-configuration)))
      (setq problems (cons (list :raw-platform system-type) problems)))
    (unless native
      (setq problems (cons (list :raw-no-native-section) problems)))
    (when native
      (unless (equal (plist-get native :raw-abi)
                     nelisp-native-load-raw-runtime-abi)
        (setq problems
              (cons (list :raw-abi (plist-get native :raw-abi)) problems)))
      (unless (equal (plist-get native :object-format)
                     nelisp-native-load-raw-object-format)
        (setq problems
              (cons (list :raw-object-format (plist-get native :object-format))
                    problems)))
      (unless (and (integerp (plist-get native :text-size))
                   text
                   (= (plist-get native :text-size) text-length)
                   (> text-length 0))
        (setq problems
              (cons (list :raw-text-size (plist-get native :text-size)
                          text-length)
                    problems)))
      (unless (and (integerp (plist-get native :object-size))
                   text
                   (= (plist-get native :object-size) text-length))
        (setq problems
              (cons (list :raw-object-size (plist-get native :object-size)
                          text-length)
                    problems)))
      (let ((declared (plist-get native :object-sha256))
            (actual (and text (nelisp-native-load--raw-digest text))))
        (unless (stringp declared)
          (setq problems
                (cons (list :raw-object-hash-missing declared) problems)))
        ;; Older standalone readers may not carry a SHA implementation.  The
        ;; manifest still has to declare the compiler's digest, while an
        ;; available verifier turns a changed payload into a hard refusal.
        (when (and (stringp actual)
                   (not (equal declared actual)))
          (setq problems
                (cons (list :raw-object-hash-mismatch declared actual)
                      problems))))
      ;; Raw units do not carry a private copy of runtime BSS in this slice.
      ;; A replacement reaches `nl_runtime_reload_state' through its named
      ;; import, so a second state block cannot silently diverge from the
      ;; live heap's state.
      (when (> (or (plist-get native :data-size) 0) 0)
        (setq problems
              (cons (list :raw-data-section-unsupported
                          (plist-get native :data-size)) problems)))
      (when (> (or (plist-get native :bss-size) 0) 0)
        (setq problems
              (cons (list :raw-bss-section-unsupported
                          (plist-get native :bss-size)) problems)))
      (unless (and (listp exports) exports)
        (setq problems (cons (list :raw-no-exports) problems)))
      (unless (and (listp imports0)
                   (not (memq nil imports))
                   (= (length imports)
                      (length (delete-dups (copy-sequence imports)))))
        (setq problems (cons (list :raw-imports-not-explicit imports0) problems)))
      (let ((rest imports0))
        (while rest
          (let ((entry (car rest)))
            (unless (eq (nelisp-native-load--raw-import-kind entry) 'func)
              (setq problems
                    (cons (list :raw-import-kind
                                (nelisp-native-load--raw-import-name entry)
                                (nelisp-native-load--raw-import-kind entry))
                          problems)))
            (unless (equal (nelisp-native-load--raw-import-abi entry)
                           nelisp-native-load-raw-runtime-abi)
              (setq problems
                    (cons (list :raw-import-abi
                                (nelisp-native-load--raw-import-name entry)
                                (nelisp-native-load--raw-import-abi entry))
                          problems)))
            (unless (member (nelisp-native-load--raw-import-name entry)
                           nelisp-native-load-raw-runtime-symbols)
              (setq problems
                    (cons (list :raw-import-not-runtime
                                (nelisp-native-load--raw-import-name entry))
                          problems))))
          (setq rest (cdr rest))))
      (let ((rest exports))
        (let ((names nil))
          (while rest
            (let ((export-name (plist-get (car rest) :name)))
              (when (and (stringp export-name) (member export-name names))
                (setq problems (cons (list :raw-duplicate-export export-name)
                                     problems)))
              (when (stringp export-name)
                (setq names (cons export-name names))))
            (setq rest (cdr rest))))
        (setq rest exports)
        (while rest
          (let* ((candidate (car rest))
                 (offset (plist-get candidate :value))
                 (size (plist-get candidate :size))
                 (arity (plist-get candidate :arity)))
            (unless (and (stringp (plist-get candidate :name))
                         (integerp offset) (>= offset 0)
                         (integerp size) (> size 0)
                         text-length (<= (+ offset size) text-length))
              (setq problems (cons (list :raw-export-range candidate) problems)))
            (unless (equal (plist-get candidate :abi)
                           nelisp-native-load-raw-runtime-abi)
              (setq problems
                    (cons (list :raw-export-abi
                                (plist-get candidate :name)
                                (plist-get candidate :abi))
                          problems)))
            (unless (or (eq (plist-get candidate :return) 'u64)
                        (equal (plist-get candidate :return) "u64"))
              (setq problems
                    (cons (list :raw-export-return
                                (plist-get candidate :name)
                                (plist-get candidate :return))
                          problems)))
            (unless (and (integerp arity) (>= arity 0)
                         (<= arity nelisp-native-load-raw-max-arity))
              (setq problems (cons (list :raw-export-arity
                                         (plist-get candidate :name) arity)
                                   problems))))
          (setq rest (cdr rest))))
      (let ((rest (plist-get native :relocs)))
        (while rest
          (let ((bad (nelisp-native-load--raw-reloc-problem
                      (car rest) (or text-length 0) imports)))
            (when bad (setq problems (cons bad problems))))
          (setq rest (cdr rest)))))
    (let* ((declared (plist-get manifest :artifact-sha256))
           (canonical (and (stringp declared)
                           (prin1-to-string
                            (nelisp-native-load--raw-plist-without
                             manifest :artifact-sha256))))
           (actual (and canonical
                        (nelisp-native-load--sha256 canonical))))
      (unless (stringp declared)
        (setq problems (cons (list :raw-artifact-hash-missing declared)
                             problems)))
      ;; As with the object digest, a reader without a digest primitive can
      ;; still load a compiler-produced artifact; a reader that can verify it
      ;; rejects metadata edits before mmap.
      (when (and (stringp actual) (not (equal declared actual)))
        (setq problems
              (cons (list :raw-artifact-hash-mismatch declared actual)
                    problems))))
    (when name
      (setq entry (and native (nelisp-native-load--raw-export native name)))
      (unless entry
        (setq problems (cons (list :raw-no-such-export name) problems))))
    (nreverse problems)))

(defun nelisp-native-load--raw-symbol-addr (name)
  "Resolve named raw runtime import NAME.

The opt-in reader supplies `nelisp--native-runtime-symbol-addr' from its
exact executable export map.  That builtin deliberately accepts the numeric
index in `nelisp-native-load-raw-runtime-symbols', while the loader API uses
names so artifacts remain readable.  Raw units are restricted to this
private four-entry namespace; object-mode bridge addresses have a different
calling convention and are never a fallback here."
  (let ((addr
         (cond
          ((fboundp 'nelisp--native-runtime-symbol-addr)
           (let ((index (nelisp-native-load--raw-runtime-symbol-index name)))
             (if (integerp index)
                 (condition-case nil
                     (nelisp--native-runtime-symbol-addr index)
                   (error 0))
               0)))
          (t 0))))
    (unless (and (integerp addr) (> addr nelisp-native-load-page-bytes))
      (error "nelisp-native-load: raw runtime symbol %s is not exported" name))
    addr))

(defun nelisp-native-load--raw-v2-rooted-import-family (manifest)
  "Find one declared rooted family after a bounded header spine scan.
Conflicting or duplicate family markers refuse before any family validator."
  (let ((tail manifest) (seen nil) (pairs 0) (family nil) (bad nil))
    (while (and tail (not bad) (< pairs 512))
      (if (not (and (consp tail) (consp (cdr tail)) (not (memq tail seen))))
          (setq bad t)
        (setq seen (cons tail seen))
        (let* ((key (car tail))
               (selected (cond
                          ((eq key :native-rooted-conditional-contract-version) 'conditional)
                          ((eq key :native-rooted-branch-contract-version) 'branch)
                          ((eq key :native-rooted-branch-join-contract-version) 'join)
                          ((eq key :native-rooted-cfg-contract-version) 'cfg)
                          ((eq key :native-rooted-cfg-safe-v3-contract-version) 'safe)
                          ((eq key :native-template-proof-version) 'template))))
          (if selected
              (if (or family (not (stringp (cadr tail))))
                  (setq bad t)
                (setq family selected))))
        (setq tail (cddr tail) pairs (1+ pairs))))
    (if (or bad tail) 'refused family)))

(defun nelisp-native-load--raw-v2-rooted-import-contract-valid-p (manifest)
  "Run the complete validator for exactly one declared rooted family."
  (let ((family (nelisp-native-load--raw-v2-rooted-import-family manifest)))
    (cond ((eq family 'template)
           (require 'nelisp-native-template)
           (null (nelisp-native-template-check manifest)))
          ((eq family 'conditional)
           (nelisp-native-load--raw-v2-conditional-contract-valid-p manifest))
          ((eq family 'branch)
           (nelisp-native-load--raw-v2-rooted-branch-contract-valid-p manifest))
          ((eq family 'join)
           (nelisp-native-load--raw-v2-rooted-branch-join-contract-valid-p manifest))
          ((memq family '(cfg safe))
           (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest)))))

(defun nelisp-native-load--raw-v2-symbol-addr (name &optional entry manifest)
  "Resolve authenticated v2 import NAME using its declared address mode.

The exact `nl_native_car_v2' extension uses the binary-hash-authenticated
bridgeable-symbol table.  All base imports retain the v2 runtime resolver."
  (if (and manifest
           (eq (nelisp-native-load--raw-v2-rooted-import-family manifest) 'refused))
      (error "nelisp-native-load: conflicting or malformed rooted import family"))
  (when (eq (plist-get entry :address-mode) 'arithmetic-provider-v1)
    (unless (and manifest
                 (nelisp-native-load--raw-v2-rooted-import-contract-valid-p manifest)
                 (nelisp-native-load--rooted-cfg-provider-import-valid-p
                  entry (plist-get manifest :native-rooted-cfg-contract))
                 (equal name (plist-get entry :name)))
      (error "nelisp-native-load: unauthenticated arithmetic provider import")))
  (if (or (member name nelisp-native-load-raw-v2-bridgeable-imports)
          (assoc name (nelisp-native-load--raw-v2-contract))
          (eq (plist-get entry :address-mode) 'arithmetic-provider-v1)
          (and (equal name nelisp-native-load-raw-v2-call1-import)
               entry manifest
               (nelisp-native-load--raw-v2-call1-import-valid-p
                manifest entry))
          (and entry manifest
               (nelisp-native-load--raw-v2-rooted-import-contract-valid-p manifest)
               (nelisp-native-load--raw-v2-conditional-import-mode name)))
      (nelisp-native-load--symbol-addr name)
    (nelisp-native-load--raw-symbol-addr name)))

(defun nelisp-native-load--mprotect-rx (addr size)
  "Make executable mapping ADDR/SIZE read+execute, or signal a refusal."
  (let ((rc (nelisp-native-load--protect addr size 5)))
    (unless (= rc 0)
      (error "nelisp-native-load: mprotect RX failed at %d (%d)" addr rc))
    addr))

(defun nelisp-native-load-raw-artifact
    (path &optional name expected-binary-sha256)
  "Map raw runtime export NAME from PATH and return a raw handle.

The handle's entries use ordinary SysV i64 arguments.  The page is writable
only while text and PLT32 relocations are copied; it is RX before return and
is retained until process exit.  When EXPECTED-BINARY-SHA256 is supplied, the
manifest and the running executable must match it before any page is mapped.
A failure unmaps the new page."
  (catch 'nelisp-native-load-raw-v2-dispatch
    (let ((format (plist-get (nelisp-native-load-manifest path) :format)))
    (when (eq format nelisp-native-load-raw-artifact-format-v2)
      (throw 'nelisp-native-load-raw-v2-dispatch
        ;; V2 executable/table pages come from mmap, not the GC arena.
        ;; Keep collection available during metadata decoding and checking;
        ;; disabling it across that interpreted work grows the heap without
        ;; bound.  The digest's temporary arena buffer has its own short guard.
        (nelisp-native-load-raw-v2-artifact
         path name expected-binary-sha256))))
  (let* ((manifest (nelisp-native-load-manifest path))
         (native (nelisp-native-load--raw-native manifest))
         (exports (and native (nelisp-native-load--raw-exports native)))
         (chosen (or name (and exports (plist-get (car exports) :name))))
         (problems (nelisp-native-load-raw-check manifest chosen))
         (declared-binary (plist-get manifest :binary-sha256)))
    (when problems
      (error "nelisp-native-load: cannot load raw %s from %s: %S"
             chosen path problems))
    ;; A runtime reader must be given an explicit expected digest.  In
    ;; particular, a missing expected value must not turn a manifest's
    ;; optional-looking field into a wildcard: mapping an unknown unit before
    ;; the install guard would defeat the same-binary ABI contract.
    (when (fboundp 'nelisp--native-runtime-symbol-addr)
      (unless (stringp expected-binary-sha256)
        (error "nelisp-native-load: raw runtime load requires expected binary SHA-256"))
      (unless (and (stringp declared-binary)
                   (equal declared-binary expected-binary-sha256))
        (error "nelisp-native-load: raw binary identity mismatch for %s"
               path)))
    (when expected-binary-sha256
      (let ((actual-binary (nelisp-native-load--running-binary-sha256)))
        (unless (and (stringp actual-binary)
                     (equal actual-binary expected-binary-sha256))
          (error "nelisp-native-load: running binary identity is unavailable or mismatched"))))
    (unless (nelisp-native-load--raw-supported-p)
      (error "nelisp-native-load: raw runtime units require Linux x86_64 in-process primitives"))
    (let* ((text (nelisp-native-load--raw-bytes native :text-base64))
           (text-length (string-bytes text))
           (imports0 (plist-get native :imports))
           (imports (mapcar #'nelisp-native-load--raw-import-name imports0))
           (stub-base (* 16 (/ (+ text-length 15) 16)))
           (code-size (nelisp-native-load--page-round
                       (+ stub-base (* nelisp-native-load-stub-bytes
                                       (length imports)))))
           (codepage nil)
           (stub-offsets nil)
           (success nil))
      (unwind-protect
          (progn
            ;; Keep the mapping writable while bytes and relocations are
            ;; installed, then remove write permission before returning it.
            ;; Mapping RWX even briefly makes a failed/interrupting load an
            ;; unnecessary executable-writable window.
            (setq codepage (nelisp-native-load--mmap code-size nil))
            (nelisp-native-load--poke-string codepage 0 text)
            (let ((rest imports) (idx 0))
              (while rest
                (let ((offset (+ stub-base
                                 (* nelisp-native-load-stub-bytes idx))))
                  (setq stub-offsets
                        (cons (cons (car rest) offset) stub-offsets))
                  (nelisp-native-load--poke-bytes
                   codepage offset
                   '(#x48 #xb8 0 0 0 0 0 0 0 0 #xff #xe0))
                  ;; `movabs rax, imm64' occupies bytes 0..9; `jmp rax'
                  ;; starts at byte 10.  Sixteen bytes leave four harmless
                  ;; padding bytes in the reservation.
                  (ptr-write-u64 codepage (+ offset 2)
                                 (nelisp-native-load--raw-symbol-addr
                                  (car rest))))
                (setq idx (1+ idx))
                (setq rest (cdr rest))))
            (let ((rest (plist-get native :relocs)))
              (while rest
                (let* ((reloc (car rest))
                       (offset (plist-get reloc :offset))
                       (symbol (plist-get reloc :symbol))
                       (addend (or (plist-get reloc :addend) 0))
                       (stub (assoc symbol stub-offsets)))
                  (unless stub
                    (error "nelisp-native-load: raw relocation %s has no import stub"
                           symbol))
                  (ptr-write-u32 codepage offset
                                 (- (+ codepage (cdr stub) addend)
                                    (+ codepage offset))))
                (setq rest (cdr rest))))
            (nelisp-native-load--mprotect-rx codepage code-size)
            (let ((addresses nil) (rest exports))
              (while rest
                (let ((entry (car rest)))
                  (push (cons (plist-get entry :name)
                              (+ codepage (plist-get entry :value)))
                        addresses))
                (setq rest (cdr rest)))
              (setq addresses (nreverse addresses))
              (let ((handle (list :kind 'raw-runtime
                                  :path path
                                  :entry (cdr (assoc chosen addresses))
                                  :entry-name chosen
                                  :exports addresses
                                  :codepage codepage
                                  :code-size code-size
                                  :runtime-abi (plist-get manifest :runtime-abi)
                                  :raw-abi (plist-get native :raw-abi)
                                  :layout-id (plist-get manifest :layout-id)
                                  :build-id (plist-get manifest :build-id)
                                  :binary-sha256 declared-binary
                                  :imports imports
                                  :source-sha256 (plist-get manifest :source-sha256)
                                  :artifact-sha256
                                  (plist-get manifest :artifact-sha256)
                                  :object-sha256
                                  (plist-get native :object-sha256)
                                  :arity (plist-get (nelisp-native-load--raw-export
                                                     native chosen) :arity)
                                  :retained t)))
                (push handle nelisp-native-load-raw-mappings)
                (setq success t)
            handle)))
        (unless success
          (when (and (integerp codepage) (> codepage 0))
            (ignore-errors
              (nelisp-native-load--unmap codepage code-size)))))))))

(defun nelisp-native-load-raw-call (handle args)
  "Call raw HANDLE with integer ARGS and return its u64 result."
  (let* ((arity (plist-get handle :arity))
         (max-arity (if (equal (plist-get handle :runtime-abi)
                              (nelisp-native-load--runtime-abi-v2))
                        nelisp-native-load-raw-max-arity-v2
                      nelisp-native-load-raw-max-arity)))
    (when (and arity (/= arity (length args)))
      (error "nelisp-native-load: raw %s takes %d argument(s), got %d"
             (plist-get handle :entry-name) arity (length args)))
    (when (> (length args) max-arity)
      (error "nelisp-native-load: raw call has more than %d arguments"
             max-arity))
    (dolist (arg args)
      (unless (integerp arg)
        (error "nelisp-native-load: raw argument is not an integer: %S" arg)))
    (let ((passed (copy-sequence args)))
      (while (< (length passed) max-arity)
        (setq passed (append passed '(0))))
      (apply #'ptr-call (plist-get handle :entry) passed))))

(defun nelisp-native-load-raw-v2-car-call (handle value)
  "Call the authenticated raw-v2 CAR entry in HANDLE on evaluator VALUE.

HANDLE must be the narrow four-argument raw-v2 probe importing only
`nl_native_car_v2'. VALUE is copied into a runtime-issued v2 root frame with
`nelisp--native-pin-copy-v2', preserving the evaluator object's identity.
The output is read only after the gateway returns success and all three slot
addresses have been reauthenticated. The frame is released on every exit."
  (unless (and (listp handle)
               (memq handle nelisp-native-load-raw-mappings)
               (eq (plist-get handle :kind) 'raw-runtime-v2)
               (equal (plist-get handle :entry-name) "nl_native_car_probe")
               (= (or (plist-get handle :arity) -1) 4)
               (equal (plist-get handle :runtime-abi)
                      (nelisp-native-load--runtime-abi-v2))
               (equal (plist-get handle :raw-abi)
                      (nelisp-native-load--runtime-abi-v2))
               (equal (plist-get handle :imports) '("nl_native_car_v2")))
    (error "nelisp-native-load: handle is not the authenticated raw-v2 CAR probe"))
  (unless (and (fboundp 'nelisp--native-pin-copy-v2)
               (fboundp 'nelisp--native-env))
    (error "nelisp-native-load: v2 evaluator root-copy boundary unavailable"))
  (let* ((env (nelisp--native-env))
         (begin (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2"))
         (reserve (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2"))
         (end (nelisp-native-load--symbol-addr "nl_root_pin_end_v2"))
         (slot (nelisp-native-load--symbol-addr "nl_root_pin_slot_v2"))
         (ticket (and (integerp env) (> env 0)
                      (ptr-call begin env 0 0 0 0 0)))
         (frame-slot nil)
         (input-slot nil)
         (output-slot nil))
    (unless (and (integerp ticket) (> ticket 0))
      (error "nelisp-native-load: cannot begin v2 CAR root frame"))
    (unwind-protect
        (progn
          (setq frame-slot (ptr-call reserve env ticket 0 0 0 0)
                input-slot (ptr-call reserve env ticket 0 0 0 0)
                output-slot (ptr-call reserve env ticket 0 0 0 0))
          (unless (and (integerp frame-slot) (> frame-slot 0)
                       (integerp input-slot) (> input-slot 0)
                       (integerp output-slot) (> output-slot 0))
            (error "nelisp-native-load: v2 CAR root frame is full or stale"))
          (nelisp-native-load-box frame-slot nil env frame-slot)
          (nelisp-native-load--zero-slot output-slot)
          (unless (= (nelisp--native-pin-copy-v2 env ticket 1 value)
                     input-slot)
            (error "nelisp-native-load: v2 CAR input copy failed authentication"))
          ;; `ptr-call' has six argument registers after its address.  Keep
          ;; the explicit padding here instead of `nelisp-native-load-raw-call',
          ;; whose generic v2 arity padding is for the wider raw ABI.
          (let ((status (ptr-call (plist-get handle :entry)
                                  env ticket 1 2 0 0)))
            (cond
             ((= status 0)
              (unless (and (= (ptr-call slot env ticket 0 0 0 0) frame-slot)
                           (= (ptr-call slot env ticket 1 0 0 0) input-slot)
                           (= (ptr-call slot env ticket 2 0 0 0) output-slot))
                (error "nelisp-native-load: v2 CAR slot authentication failed"))
              (nelisp-native-load-unbox output-slot env frame-slot))
             ((= status 1)
              (signal 'wrong-type-argument (list 'listp value)))
             ((= status 2)
              (error "nelisp-native-load: native CAR rejected its v2 request"))
             (t
              (error "nelisp-native-load: invalid native CAR status %S" status)))))
      (unless (= (ptr-call end env ticket 0 0 0 0) 1)
        (error "nelisp-native-load: v2 CAR root frame ownership lost")))))

(defun nelisp-native-load-raw-v2-cdr-call (handle value)
  "Call the authenticated raw-v2 CDR entry in HANDLE on evaluator VALUE.

HANDLE must be the fixed four-argument raw-v2 probe importing only
`nl_native_cdr_v2'. VALUE is pinned by identity in a runtime-issued root
frame, and the output is read only after slot reauthentication."
  (unless (and (listp handle)
               (memq handle nelisp-native-load-raw-mappings)
               (eq (plist-get handle :kind) 'raw-runtime-v2)
               (equal (plist-get handle :entry-name) "nl_native_cdr_probe")
               (= (or (plist-get handle :arity) -1) 4)
               (equal (plist-get handle :runtime-abi)
                      (nelisp-native-load--runtime-abi-v2))
               (equal (plist-get handle :raw-abi)
                      (nelisp-native-load--runtime-abi-v2))
               (equal (plist-get handle :imports) '("nl_native_cdr_v2")))
    (error "nelisp-native-load: handle is not the authenticated raw-v2 CDR probe"))
  (unless (and (fboundp 'nelisp--native-pin-copy-v2)
               (fboundp 'nelisp--native-env))
    (error "nelisp-native-load: v2 evaluator root-copy boundary unavailable"))
  (let* ((env (nelisp--native-env))
         (begin (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2"))
         (reserve (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2"))
         (end (nelisp-native-load--symbol-addr "nl_root_pin_end_v2"))
         (slot (nelisp-native-load--symbol-addr "nl_root_pin_slot_v2"))
         (ticket (and (integerp env) (> env 0)
                      (ptr-call begin env 0 0 0 0 0)))
         (frame-slot nil) (input-slot nil) (output-slot nil))
    (unless (and (integerp ticket) (> ticket 0))
      (error "nelisp-native-load: cannot begin v2 CDR root frame"))
    (unwind-protect
        (progn
          (setq frame-slot (ptr-call reserve env ticket 0 0 0 0)
                input-slot (ptr-call reserve env ticket 0 0 0 0)
                output-slot (ptr-call reserve env ticket 0 0 0 0))
          (unless (and (integerp frame-slot) (> frame-slot 0)
                       (integerp input-slot) (> input-slot 0)
                       (integerp output-slot) (> output-slot 0))
            (error "nelisp-native-load: v2 CDR root frame is full or stale"))
          (nelisp-native-load-box frame-slot nil env frame-slot)
          (nelisp-native-load--zero-slot output-slot)
          (unless (= (nelisp--native-pin-copy-v2 env ticket 1 value) input-slot)
            (error "nelisp-native-load: v2 CDR input copy failed authentication"))
          (let ((status (ptr-call (plist-get handle :entry) env ticket 1 2 0 0)))
            (cond
             ((= status 0)
              (unless (and (= (ptr-call slot env ticket 0 0 0 0) frame-slot)
                           (= (ptr-call slot env ticket 1 0 0 0) input-slot)
                           (= (ptr-call slot env ticket 2 0 0 0) output-slot))
                (error "nelisp-native-load: v2 CDR slot authentication failed"))
              (nelisp-native-load-unbox output-slot env frame-slot))
             ((= status 1) (signal 'wrong-type-argument (list 'listp value)))
             ((= status 2)
              (error "nelisp-native-load: native CDR rejected its v2 request"))
             (t (error "nelisp-native-load: invalid native CDR status %S" status)))))
      (unless (= (ptr-call end env ticket 0 0 0 0) 1)
        (error "nelisp-native-load: v2 CDR root frame ownership lost")))))

(defun nelisp-native-load-raw-v2-unary-chain-call
    (handle argument result-root-index)
  "Call checked CAR/CDR chain HANDLE on evaluator ARGUMENT.

RESULT-ROOT-INDEX is the authenticated final slot declared by the compiler."
  (unless (and (listp handle)
               (memq handle nelisp-native-load-raw-mappings)
               (eq (plist-get handle :kind) 'raw-runtime-v2)
               (equal (plist-get handle :entry-name) "nl_native_chain_probe_v2")
               (= (or (plist-get handle :arity) -1) 4)
               (integerp (plist-get handle :entry))
               (memq result-root-index '(1 2))
               (equal (plist-get handle :runtime-abi)
                      (nelisp-native-load--runtime-abi-v2))
               (equal (plist-get handle :raw-abi)
                      (nelisp-native-load--runtime-abi-v2))
               (member (plist-get handle :imports)
                       '( ("nl_native_car_v2")
                          ("nl_native_cdr_v2")
                          ("nl_native_car_v2" "nl_native_cdr_v2"))))
    (error "nelisp-native-load: handle is not an authenticated unary-chain artifact"))
  (unless (and (fboundp 'nelisp--native-pin-copy-v2)
               (fboundp 'nelisp--native-env)
               (nelisp-runtime-reload-contract-matches-p)
               (nelisp-native-load--raw-supported-p))
    (error "nelisp-native-load: unary-chain v2 root boundary unavailable"))
  (let* ((env (nelisp--native-env))
         (begin (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2"))
         (reserve (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2"))
         (end (nelisp-native-load--symbol-addr "nl_root_pin_end_v2"))
         (slot (nelisp-native-load--symbol-addr "nl_root_pin_slot_v2"))
         (ticket (and (integerp env) (> env 0)
                      (ptr-call begin env 0 0 0 0 0)))
         (frame-slot nil) (input-slot nil) (output-slot nil) status)
    (unless (and (integerp ticket) (> ticket 0))
      (error "nelisp-native-load: cannot begin unary-chain root frame"))
    (unwind-protect
        (progn
          (setq frame-slot (ptr-call reserve env ticket 0 0 0 0)
                input-slot (ptr-call reserve env ticket 0 0 0 0)
                output-slot (ptr-call reserve env ticket 0 0 0 0))
          (unless (and (integerp frame-slot) (> frame-slot 0)
                       (integerp input-slot) (> input-slot 0)
                       (integerp output-slot) (> output-slot 0))
            (error "nelisp-native-load: unary-chain root frame is full or stale"))
          (nelisp-native-load-box frame-slot nil env frame-slot)
          (nelisp-native-load--zero-slot output-slot)
          (unless (= (nelisp--native-pin-copy-v2 env ticket 1 argument) input-slot)
            (error "nelisp-native-load: unary-chain input copy failed authentication"))
          (setq status (ptr-call (plist-get handle :entry) env ticket 1 2 0 0))
          (unless (and (= (ptr-call slot env ticket 0 0 0 0) frame-slot)
                       (= (ptr-call slot env ticket 1 0 0 0) input-slot)
                       (= (ptr-call slot env ticket 2 0 0 0) output-slot))
            (error "nelisp-native-load: unary-chain root slots failed authentication"))
          (cond
           ((= status 0)
            (garbage-collect)
            (unless (and (= (ptr-call slot env ticket 0 0 0 0) frame-slot)
                         (= (ptr-call slot env ticket 1 0 0 0) input-slot)
                         (= (ptr-call slot env ticket 2 0 0 0) output-slot))
              (error "nelisp-native-load: unary-chain root slots changed during GC"))
            (nelisp-native-load-unbox
             (if (= result-root-index 1) input-slot output-slot) env frame-slot))
           ((memq status '(17 18))
            (garbage-collect)
            (unless (and (= (ptr-call slot env ticket 0 0 0 0) frame-slot)
                         (= (ptr-call slot env ticket 1 0 0 0) input-slot)
                         (= (ptr-call slot env ticket 2 0 0 0) output-slot))
              (error "nelisp-native-load: unary-chain error roots changed during GC"))
            (signal 'wrong-type-argument
                    (list 'listp
                          (nelisp-native-load-unbox
                           (if (= status 17) input-slot output-slot)
                           env frame-slot))))
           ((= status 1)
            (error "nelisp-native-load: legacy unary-chain status refused"))
           ((= status 2) (error "nelisp-native-load: unary-chain gateway rejected request"))
           ((= status 3) (error "nelisp-native-load: unary-chain gateway refused opcode"))
           (t (error "nelisp-native-load: invalid unary-chain status %S" status))))
      (unless (= (ptr-call end env ticket 0 0 0 0) 1)
        (error "nelisp-native-load: unary-chain root ownership lost")))))

(defun nelisp-native-load-raw-v2-object-op-call (handle opcode value)
  "Call authenticated object OPCODE on evaluator VALUE through HANDLE.

HANDLE is a raw ELF entry importing the fixed authenticated CAR and CDR gateways.
The opcode ID is checked against the manifest before roots are allocated.
The evaluator Sexp is pinned by identity across native execution and GC."
  (unless (and (listp handle)
               (memq handle nelisp-native-load-raw-mappings)
               (eq (plist-get handle :kind) 'raw-runtime-v2)
               (equal (plist-get handle :entry-name) "nl_native_object_probe")
               (= (or (plist-get handle :arity) -1) 5)
               (equal (plist-get handle :runtime-abi)
                      (nelisp-native-load--runtime-abi-v2))
               (equal (plist-get handle :raw-abi)
                      (nelisp-native-load--runtime-abi-v2))
               (equal (plist-get handle :imports)
                      '("nl_native_car_v2" "nl_native_cdr_v2"))
               (equal (plist-get handle :native-object-op-contract-version)
                      nelisp-native-load-native-object-op-contract-version)
               (equal (plist-get handle :native-object-op-gateway-imports)
                      '("nl_native_car_v2" "nl_native_cdr_v2"))
               (equal (plist-get handle :native-object-opcodes)
                      nelisp-native-load-native-object-opcodes)
               (equal (plist-get handle :native-object-op-contract-hash)
                      (nelisp-native-load--native-object-op-contract-hash))
               (assq opcode nelisp-native-load-native-object-opcodes))
    (error "nelisp-native-load: handle or opcode is outside the object-op manifest: %S"
           (list :kind (plist-get handle :kind)
                 :entry (plist-get handle :entry-name)
                 :arity (plist-get handle :arity)
                 :imports (plist-get handle :imports)
                 :version (plist-get handle :native-object-op-contract-version)
                 :opcodes (plist-get handle :native-object-opcodes)
                 :opcode opcode)))
  (unless (and (fboundp 'nelisp--native-pin-copy-v2)
               (fboundp 'nelisp--native-env))
    (error "nelisp-native-load: v2 evaluator root-copy boundary unavailable"))
  (let* ((env (nelisp--native-env))
         (begin (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2"))
         (reserve (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2"))
         (end (nelisp-native-load--symbol-addr "nl_root_pin_end_v2"))
         (slot (nelisp-native-load--symbol-addr "nl_root_pin_slot_v2"))
         (ticket (and (integerp env) (> env 0)
                      (ptr-call begin env 0 0 0 0 0)))
         (frame-slot nil) (input-slot nil) (output-slot nil))
    (unless (and (integerp ticket) (> ticket 0))
      (error "nelisp-native-load: cannot begin v2 object-op root frame"))
    (unwind-protect
        (progn
          (setq frame-slot (ptr-call reserve env ticket 0 0 0 0)
                input-slot (ptr-call reserve env ticket 0 0 0 0)
                output-slot (ptr-call reserve env ticket 0 0 0 0))
          (unless (and (integerp frame-slot) (> frame-slot 0)
                       (integerp input-slot) (> input-slot 0)
                       (integerp output-slot) (> output-slot 0))
            (error "nelisp-native-load: v2 object-op root frame is full or stale"))
          (nelisp-native-load-box frame-slot nil env frame-slot)
          (nelisp-native-load--zero-slot output-slot)
          (unless (= (nelisp--native-pin-copy-v2 env ticket 1 value) input-slot)
            (error "nelisp-native-load: v2 object-op input copy failed authentication"))
          (let ((status (ptr-call (plist-get handle :entry)
                                  env ticket opcode 1 2 0)))
            (cond
             ((= status 0)
              (unless (and (= (ptr-call slot env ticket 0 0 0 0) frame-slot)
                           (= (ptr-call slot env ticket 1 0 0 0) input-slot)
                           (= (ptr-call slot env ticket 2 0 0 0) output-slot))
                (error "nelisp-native-load: v2 object-op slot authentication failed"))
              (nelisp-native-load-unbox output-slot env frame-slot))
             ((= status 1)
              (signal 'wrong-type-argument (list 'listp value)))
             ((= status 2)
              (error "nelisp-native-load: native object-op rejected its v2 request"))
             ((= status 3)
              (error "nelisp-native-load: native object-op refused opcode %S" opcode))
             (t
              (error "nelisp-native-load: invalid native object-op status %S" status)))))
      (unless (= (ptr-call end env ticket 0 0 0 0) 1)
        (error "nelisp-native-load: v2 object-op root frame ownership lost")))))

(defun nelisp-native-load-raw-v2-cons-call (handle car-value cdr-value)
  "Call the authenticated raw-v2 CONS entry in HANDLE on evaluator values.

Both values are copied into authenticated root slots.  The output is read only
after success and slot reauthentication; the root frame is released on every
exit.  This narrow helper does not admit bytecode opcode 66."
  (unless (and (listp handle)
               (memq handle nelisp-native-load-raw-mappings)
               (eq (plist-get handle :kind) 'raw-runtime-v2)
               (equal (plist-get handle :entry-name) "nl_native_cons_probe")
               (= (or (plist-get handle :arity) -1) 5)
               (equal (plist-get handle :runtime-abi)
                      (nelisp-native-load--runtime-abi-v2))
               (equal (plist-get handle :raw-abi)
                      (nelisp-native-load--runtime-abi-v2))
               (equal (plist-get handle :imports) '("nl_native_cons_v2")))
    (error "nelisp-native-load: handle is not the authenticated raw-v2 CONS probe"))
  (unless (and (fboundp 'nelisp--native-pin-copy-v2)
               (fboundp 'nelisp--native-env))
    (error "nelisp-native-load: v2 evaluator root-copy boundary unavailable"))
  (let* ((env (nelisp--native-env))
         (begin (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2"))
         (reserve (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2"))
         (end (nelisp-native-load--symbol-addr "nl_root_pin_end_v2"))
         (slot (nelisp-native-load--symbol-addr "nl_root_pin_slot_v2"))
         (ticket (and (integerp env) (> env 0)
                      (ptr-call begin env 0 0 0 0 0)))
         frame-slot left-slot right-slot output-slot)
    (unless (and (integerp ticket) (> ticket 0))
      (error "nelisp-native-load: cannot begin v2 CONS root frame"))
    (unwind-protect
        (progn
          (setq frame-slot (ptr-call reserve env ticket 0 0 0 0)
                left-slot (ptr-call reserve env ticket 0 0 0 0)
                right-slot (ptr-call reserve env ticket 0 0 0 0)
                output-slot (ptr-call reserve env ticket 0 0 0 0))
          (unless (and (integerp frame-slot) (> frame-slot 0)
                       (integerp left-slot) (> left-slot 0)
                       (integerp right-slot) (> right-slot 0)
                       (integerp output-slot) (> output-slot 0))
            (error "nelisp-native-load: v2 CONS root frame is full or stale"))
          (nelisp-native-load-box frame-slot nil env frame-slot)
          (nelisp-native-load--zero-slot output-slot)
          (unless (and (= (nelisp--native-pin-copy-v2 env ticket 1 car-value)
                          left-slot)
                       (= (nelisp--native-pin-copy-v2 env ticket 2 cdr-value)
                          right-slot))
            (error "nelisp-native-load: v2 CONS input copy failed authentication"))
          (let ((status (ptr-call (plist-get handle :entry)
                                  env ticket 1 2 3 0)))
            (unless (= status 0)
              (error "nelisp-native-load: native CONS rejected its v2 request (%S)"
                     status))
            (unless (and (= (ptr-call slot env ticket 0 0 0 0) frame-slot)
                         (= (ptr-call slot env ticket 1 0 0 0) left-slot)
                         (= (ptr-call slot env ticket 2 0 0 0) right-slot)
                         (= (ptr-call slot env ticket 3 0 0 0) output-slot))
              (error "nelisp-native-load: v2 CONS slot authentication failed"))
            (nelisp-native-load-unbox output-slot env frame-slot)))
      (unless (= (ptr-call end env ticket 0 0 0 0) 1)
        (error "nelisp-native-load: v2 CONS root frame ownership lost")))))

(defun nelisp-native-load-raw-export-address (handle name)
  "Return raw export NAME's address from HANDLE, or signal if absent."
  (let ((addr (cdr (assoc name (plist-get handle :exports)))))
    (unless (and (integerp addr) (> addr nelisp-native-load-page-bytes))
      (error "nelisp-native-load: raw handle has no export %s" name))
    addr))

(defun nelisp-native-load-raw-state ()
  "Read the opt-in runtime reload state block as a plist.

The runtime owns the block and checks it again in
`nl_runtime_reload_install'.  This read only supplies diagnostics and an
early refusal; it is not a substitute for the install guard."
  (let ((state (nelisp-native-load--raw-symbol-addr "nl_runtime_reload_state")))
    (list :alloc (ptr-read-u64 state 0)
          :gc (ptr-read-u64 state 8)
          :generation (ptr-read-u64 state 16)
          :active-alloc (ptr-read-u64 state 24)
          :active-gc (ptr-read-u64 state 32)
          :alloc-hits (ptr-read-u64 state 40)
          :gc-hits (ptr-read-u64 state 48)
          :reserved (ptr-read-u64 state 56))))

(defun nelisp-native-load--raw-install-identity-problem
    (alloc-handle gc-handle)
  "Return a binary/ABI refusal for runtime install handles, or nil.

The check is active only in the opt-in reader, identified by its named raw
resolver.  Host-side tests may exercise the publication bookkeeping with
fake handles, while an actual runtime install must carry the executable hash
on every supplied handle and match the current process image."
  (when (fboundp 'nelisp--native-runtime-symbol-addr)
    (let ((running (nelisp-native-load--running-binary-sha256)))
      (cond
       ((not (stringp running))
        (list :raw-running-binary-unavailable))
       ((and alloc-handle
             (not (equal (plist-get alloc-handle :binary-sha256) running)))
        (list :raw-alloc-binary-mismatch
              (plist-get alloc-handle :binary-sha256) running))
       ((and gc-handle
             (not (equal (plist-get gc-handle :binary-sha256) running)))
        (list :raw-gc-binary-mismatch
              (plist-get gc-handle :binary-sha256) running))
       ;; Runtime replacements must remain wrappers around the named original
       ;; entries.  An otherwise ABI-shaped arithmetic unit is useful for raw
       ;; loader tests, but installing it as allocator/collector code would
       ;; silently sever the original implementation binding.
       ((and alloc-handle
             (not (equal (plist-get alloc-handle :runtime-abi)
                         (nelisp-native-load--runtime-abi-v2)))
             (not (member "nl_runtime_reload_alloc_original"
                          (plist-get alloc-handle :imports))))
        (list :raw-alloc-original-unbound))
       ((and gc-handle
             (not (equal (plist-get gc-handle :runtime-abi)
                         (nelisp-native-load--runtime-abi-v2)))
             (not (member "nl_runtime_reload_gc_original"
                          (plist-get gc-handle :imports))))
        (list :raw-gc-original-unbound))
       ((and alloc-handle
             (/= (or (plist-get alloc-handle :arity) -1) 2))
        (list :raw-alloc-arity (plist-get alloc-handle :arity)))
       ((and gc-handle
             (not (equal (plist-get gc-handle :runtime-abi)
                         (nelisp-native-load--runtime-abi-v2)))
             (/= (or (plist-get gc-handle :arity) -1) 1))
        (list :raw-gc-arity (plist-get gc-handle :arity)))))))

(defun nelisp-native-load-raw-install (alloc-handle gc-handle)
  "Publish raw ALLOC-HANDLE/GC-HANDLE through the runtime install gate.

Nil preserves the currently published entry.  `nl_runtime_reload_install'
performs the authoritative worker/GC/active-call checks while publishing; a
nonzero result is returned as a structured rejection.  New pages remain
retained after a failed publish so no running caller can observe an unmapped
entry."
  (let* ((before (nelisp-native-load-raw-state))
         (identity-problem
          (nelisp-native-load--raw-install-identity-problem
           alloc-handle gc-handle))
         (active (or (/= (plist-get before :active-alloc) 0)
                     (/= (plist-get before :active-gc) 0)))
         (old-alloc (plist-get before :alloc))
         (old-gc (plist-get before :gc))
         (old-generation (plist-get before :generation))
         (new-alloc (if alloc-handle
                        (nelisp-native-load-raw-export-address
                         alloc-handle (plist-get alloc-handle :entry-name))
                      old-alloc))
         (new-gc (if gc-handle
                     (nelisp-native-load-raw-export-address
                      gc-handle (plist-get gc-handle :entry-name))
                   old-gc))
         (installer (nelisp-native-load--raw-symbol-addr
                     "nl_runtime_reload_install"))
         (generation (1+ old-generation)))
    (if identity-problem
        (list :status 'rejected :phase :identity
              :reason identity-problem
              :old-alloc old-alloc :old-gc old-gc
              :generation old-generation)
      (if active
        (list :status 'rejected :phase :guard :reason :active-call
              :old-alloc old-alloc :old-gc old-gc
              :generation old-generation)
         (let* ((v2 (or (equal (plist-get alloc-handle :runtime-abi)
                             (nelisp-native-load--runtime-abi-v2))
                        (equal (plist-get gc-handle :runtime-abi)
                               (nelisp-native-load--runtime-abi-v2))))
                (table (and v2 gc-handle (plist-get gc-handle :gc-table)))
                (rc (ptr-call installer new-alloc (or table new-gc)
                              generation 0 0 0)))
          (if (= rc 0)
              (let ((after (nelisp-native-load-raw-state)))
                (list :status 'published :phase :publish
                      :old-alloc old-alloc :old-gc old-gc
                      :new-alloc new-alloc :new-gc new-gc
                      :generation (plist-get after :generation)
                      :alloc-artifact (and alloc-handle
                                           (plist-get alloc-handle :object-sha256))
                      :gc-artifact (and gc-handle
                                        (plist-get gc-handle :object-sha256))
                      :state after))
            (list :status 'rejected :phase :publish :return-code rc
                  :old-alloc old-alloc :old-gc old-gc
                  :new-alloc new-alloc :new-gc new-gc
                  :generation old-generation
                  :state (nelisp-native-load-raw-state))))))))

(defun nelisp-runtime-reload-restore-originals ()
  "Restore the runtime's original allocator and collector entries.

The runtime installer reserves the `(0, 0)' pointer pair for this operation;
it is distinct from `nil' handles in `nelisp-native-load-raw-install', where
nil means preserve the currently published entry.  The generation still
advances, so a later replacement cannot accidentally publish into an older
generation."
  (let* ((before (nelisp-native-load-raw-state))
         (identity-problem
          (nelisp-native-load--raw-install-identity-problem nil nil))
         (old-alloc (plist-get before :alloc))
         (old-gc (plist-get before :gc))
         (old-generation (plist-get before :generation))
         (active (or (/= (plist-get before :active-alloc) 0)
                     (/= (plist-get before :active-gc) 0)))
         (installer (nelisp-native-load--raw-symbol-addr
                     "nl_runtime_reload_install"))
         (generation (1+ old-generation)))
    (cond
     (identity-problem
      (list :status 'rejected :phase :identity
            :reason identity-problem :old-alloc old-alloc :old-gc old-gc
            :generation old-generation))
     (active
      (list :status 'rejected :phase :guard :reason :active-call
            :old-alloc old-alloc :old-gc old-gc :generation old-generation))
     (t
      (let ((rc (ptr-call installer 0 0 generation 0 0 0)))
        (if (= rc 0)
            (let ((after (nelisp-native-load-raw-state)))
              (list :status 'published :phase :restore
                    :mode 'original :old-alloc old-alloc :old-gc old-gc
                    :generation (plist-get after :generation) :state after))
          (list :status 'rejected :phase :restore :return-code rc
                :old-alloc old-alloc :old-gc old-gc
                :generation old-generation
                :state (nelisp-native-load-raw-state))))))))

(defun nelisp-runtime-reload-status ()
  "Return the native runtime reload state as a structured plist.

The status API is deliberately non-signalling: a normal production reader or
a reader built without the opt-in named resolver returns `:unavailable` with
the phase and reason, so a REPL can explain why a candidate was not loaded
without turning that expected capability check into a crash."
  (condition-case err
      (let ((state (nelisp-native-load-raw-state)))
        (list :status 'ready
              :phase :state
              :generation (plist-get state :generation)
              :state state))
    (error
     (list :status 'unavailable
           :phase :state
           :reason (error-message-string err)))))

(defun nelisp-runtime-reload-source-file
    (source-path alloc-name gc-name &optional build-id)
  "Compile and publish two raw runtime entries from SOURCE-PATH.

SOURCE-PATH must contain only strict top-level raw `defun' forms.  ALLOC-NAME
and GC-NAME are explicit exported function names; requiring both names keeps
this API from treating an arbitrary mapped native function as a runtime
replacement.  The source is compiled to a private temporary `.nelr', mapped
with the raw ABI loader, and published through the runtime's generation gate.

Return a plist with `:status' (`published' or `rejected'), `:phase', source
and artifact SHA values, attempted entry names, and the install result.  A
compile or map failure leaves the previous runtime untouched.  A rejected
install retains newly mapped pages for process-lifetime safety and reports the
runtime's generation rather than pretending the candidate was published."
  (let ((artifact-path nil)
        (manifest nil)
        (alloc-handle nil)
        (gc-handle nil)
        (binary-sha256 nil)
        (phase :arguments))
    (cond
     ((not (and (stringp source-path) (file-readable-p source-path)))
      (list :status 'rejected :phase phase
            :reason (list :source-not-readable source-path)))
     ((not (and (stringp alloc-name) (> (length alloc-name) 0)
                (stringp gc-name) (> (length gc-name) 0)
                (not (equal alloc-name gc-name))))
      (list :status 'rejected :phase phase
            :reason (list :entry-names-invalid alloc-name gc-name)))
     (t
      (setq binary-sha256 (nelisp-native-load--running-binary-sha256))
      (if (not (stringp binary-sha256))
          (list :status 'rejected
                :phase :identity
                :reason :running-binary-unavailable
                :source source-path
                :attempted nil)
        (setq artifact-path (make-temp-file "nelisp-runtime-reload-" nil ".nelr"))
        (condition-case err
            (progn
            (setq phase :compile)
            (setq manifest
                  (nelisp-native-load-raw-compile-file
                   source-path artifact-path nil build-id binary-sha256))
            (setq phase :load-alloc)
            (setq alloc-handle
                  (nelisp-native-load-raw-artifact
                   artifact-path alloc-name binary-sha256))
            (setq phase :load-gc)
            (setq gc-handle
                  (nelisp-native-load-raw-artifact
                   artifact-path gc-name binary-sha256))
            (setq phase :publish)
            (let* ((install (nelisp-native-load-raw-install
                             alloc-handle gc-handle))
                   (published (eq (plist-get install :status) 'published))
                   (native (plist-get manifest :native)))
              (append
               install
               (list :source source-path
                     :artifact artifact-path
                     :binary-sha256 binary-sha256
                     :source-sha256 (plist-get manifest :source-sha256)
                     :artifact-sha256 (plist-get manifest :artifact-sha256)
                     :object-sha256 (plist-get native :object-sha256)
                     :attempted (list alloc-name gc-name)
                     :published (and published (list alloc-name gc-name))
                     :build-id (plist-get manifest :build-id)))))
          (error
           (list :status 'rejected
                 :phase phase
                 :reason (error-message-string err)
                 :source source-path
                 :artifact artifact-path
                 :binary-sha256 binary-sha256
                 :source-sha256 (and manifest
                                      (plist-get manifest :source-sha256))
                 :artifact-sha256 (and manifest
                                        (plist-get manifest :artifact-sha256))
                 :attempted (delq nil (list (and alloc-handle alloc-name)
                                            (and gc-handle gc-name)))))))))))

(defun nelisp-native-load-artifact (path name)
  "Map NAME from the `.neln' at PATH and return a callable handle.

The handle is a plist with :entry (the trampoline address), :slots (the
boundary slot region), :arity and :arg-slots.  Pages are never unmapped:
a loaded function stays callable for the life of the process, which is
what a cache wants and what the demo did."
  (let* ((manifest (nelisp-native-load-manifest path))
         (problems (nelisp-native-load-check manifest name)))
    (when problems
      (error "nelisp-native-load: cannot load %s from %s: %S" name path problems))
    (let* ((native (plist-get manifest :native))
           (meta (nelisp-native-load--defun native name))
           (text (base64-decode-string (plist-get native :text-base64)))
           ;; `string-bytes': `text' is decoded machine code, not text.
           (text-length (string-bytes text))
           (relocs (plist-get native :relocs))
           (externs (let ((e (plist-get native :extern-symbols)))
                      (if (and (symbolp e) (null e)) nil e)))
           (arity (plist-get meta :arity))
           (rt-slot-count (plist-get meta :rt-slot-count))
           (body-entry (+ (plist-get meta :offset) (plist-get meta :body-offset)))
           ;; Stubs sit after the text, 16-byte aligned, so a function of
           ;; any length clears the stubs it needs.
           (stub-base (* 16 (/ (+ text-length 15) 16)))
           (stub-offsets nil)
           ;; The unit's writable static data, when it has any.  A symbol
           ;; literal's cache slot lives here.
           (data-bytes (let ((encoded (plist-get native :data-base64)))
                         (and (stringp encoded)
                              (base64-decode-string encoded))))
           ;; Placed IN the code page, after the stubs, rather than in a
           ;; mapping of its own.  Compiled code takes a data symbol's
           ;; address with a pc32, and two independent mmap(NULL) results
           ;; can sit further apart than a signed 32-bit displacement
           ;; reaches -- the same range problem the stubs exist to solve
           ;; for calls, which an address load cannot solve the same way.
           ;; The page is already RWX, so writable data in it is fine.
           (data-base (+ stub-base
                         (* nelisp-native-load-stub-bytes (length externs))))
           (code-size (nelisp-native-load--page-round
                       (+ data-base (if data-bytes (length data-bytes) 0))))
           (codepage (nelisp-native-load--mmap code-size t))
           (datapage (and data-bytes (+ codepage data-base)))
           (data-offsets
            (mapcar (lambda (sym)
                      (cons (plist-get sym :name)
                            (+ data-base (plist-get sym :value))))
                    (plist-get native :data-symbols)))
           (env (nelisp--native-env))
           (arg-slot-base (+ (* 32 5)
                             (* 32 nelisp-native-load-callback-slots)))
           (slots-size (nelisp-native-load--page-round
                        (+ arg-slot-base (* 32 (if (> arity 0) arity 1)))))
           (slots (nelisp-native-load--mmap slots-size nil))
           (trampoline (nelisp-native-load--trampoline arity rt-slot-count))
           (tramp-bytes (plist-get trampoline :bytes))
           (trampage (nelisp-native-load--mmap
                      (nelisp-native-load--page-round (length tramp-bytes)) t))
           (i 0))
      ;; Code page: text, then one stub per extern pointed at its symbol.
      (nelisp-native-load--poke-string codepage 0 text)
      (when data-bytes
        (nelisp-native-load--poke-string codepage data-base data-bytes))
      (let ((rest externs)
            (idx 0))
        (while rest
          (let ((offset (+ stub-base (* nelisp-native-load-stub-bytes idx))))
            (setq stub-offsets (cons (cons (car rest) offset) stub-offsets))
            (nelisp-native-load--poke-bytes
             codepage offset '(#x48 #xb8 0 0 0 0 0 0 0 0 #xff #xe0))
            (ptr-write-u64 (+ codepage offset) 2
                           (nelisp-native-load--symbol-addr (car rest))))
          (setq idx (1+ idx))
          (setq rest (cdr rest))))
      ;; Relocations.  A call to an extern resolves to its stub; a
      ;; reference to a local data symbol resolves into the data page.
      ;; Both are pc32 displacements from the site.
      (let ((rest relocs))
        (while rest
          (let* ((reloc (car rest))
                 (offset (plist-get reloc :offset))
                 (symbol (plist-get reloc :symbol))
                 (addend (or (plist-get reloc :addend) 0))
                 (stub (assoc symbol stub-offsets))
                 (datum (assoc symbol data-offsets))
                 (target (cond
                          (stub (+ codepage (cdr stub)))
                          (datum (+ codepage (cdr datum))))))
            (unless target
              (error "nelisp-native-load: relocation for %s resolves to neither a stub nor a data symbol" symbol))
            (unless (<= (+ offset 4) text-length)
              (error "nelisp-native-load: relocation at %d is past %d bytes of text"
                     offset text-length))
            (ptr-write-u32 codepage offset
                           (- (+ target addend) (+ codepage offset))))
          (setq rest (cdr rest))))
      ;; Boundary slots start as nil; the argument slots are filled per call.
      (setq i 0)
      (while (< i (+ 5 nelisp-native-load-callback-slots))
        (nelisp-native-load--zero-slot (+ slots (* 32 i)))
        (setq i (1+ i)))
      ;; Trampoline: bytes, then the boundary immediates and the entry.
      (nelisp-native-load--poke-bytes trampage 0 tramp-bytes)
      (let ((values (append
                     ;; These Sexp pointers are filled from checked roots for
                     ;; each call.  Mirror and frames both carry the active
                     ;; evaluator env; it is a stable non-Sexp context.
                     (list 0 env env 0 0)
                     (make-list nelisp-native-load-callback-slots 0)
                     (list (+ codepage body-entry))))
            (offsets (plist-get trampoline :imm64-offsets)))
        (while offsets
          (ptr-write-u64 trampage (car offsets) (car values))
          (setq offsets (cdr offsets))
          (setq values (cdr values))))
      (list :entry trampage
            :codepage codepage
            :stubs stub-offsets
            :body-entry body-entry
            ;; Sizes so `nelisp-native-load-unload' can hand munmap the
            ;; same extents mmap was given.
            :code-size code-size
            :datapage datapage
            :slots-size slots-size
            :entry-size (nelisp-native-load--page-round (length tramp-bytes))
            :trampoline-bytes tramp-bytes
            :boundary-imm64-offsets
            (butlast (plist-get trampoline :imm64-offsets))
            :trampoline-entry-imm64-offset
            (car (last (plist-get trampoline :imm64-offsets)))
            :slots slots
            :out slots
            :arity arity
            :abi (nelisp-native-load-abi native)
            ;; How to hand the arguments over is recorded per defun, so it
            ;; no longer has to be inferred from the extern set -- a
            ;; different question, and one that got `(if (< n 1) 5 (aref v
            ;; 1))' answering 8 for every n.
            :param-repr (or (plist-get meta :param-repr) 'unknown)
            :return-repr (or (plist-get meta :return-repr) 'unknown)
            :rest-required-count (plist-get meta :rest-required-count)
            :arg-slots (+ slots arg-slot-base)
            :name name
            :path path))))

(defun nelisp-native-load--decode-raw-bool (raw)
  "Decode a proven raw boolean result RAW as canonical nil or t."
  (cond
   ((eql raw 0) nil)
   ((eql raw 1) t)
   (t (error "nelisp-native-load: invalid raw-bool return %S" raw))))

(defun nelisp-native-load--call-raw (handle args boxed)
  "Call HANDLE with raw integer ARGS, without requiring a runtime env."
  (let ((passed args)
        (raw nil))
    (while passed
      (unless (integerp (car passed))
        (error "nelisp-native-load: %s takes integers, got %S"
               (plist-get handle :name) (car passed)))
      (setq passed (cdr passed)))
    (setq passed args)
    (while (< (length passed) (length nelisp-native-load--arg-regs))
      (setq passed (append passed (list 0))))
    (setq raw (apply (function ptr-call) (plist-get handle :entry) passed))
    (when (and boxed (integerp raw) (< raw nelisp-native-load-page-bytes))
      (setq boxed nil))
    (cond
     ((eq (plist-get handle :return-repr) 'raw-bool)
      (nelisp-native-load--decode-raw-bool raw))
     ((or (eq (plist-get handle :return-repr) 'sexp-ptr)
          (and boxed (eq (plist-get handle :return-repr) 'unknown)))
      (nelisp-native-load-unbox raw))
     (t raw))))

(defun nelisp-native-load-call (handle args)
  "Call the function in HANDLE with ARGS and return its value.

Which convention is used comes from the handle's :abi, and the two are
not interchangeable -- calling a boxed defun with raw integers makes it
do arithmetic on the values, and calling an integer defun with slot
addresses makes it do arithmetic on the pointers.  Measured on `add3',
an extern-less `(+ a (+ b c))': raw arguments answer 6, boxed arguments
answer 406962619651776 and leave `out' untouched.  Boxed calls reserve every
 Sexp boundary and argument slot in an exclusive GC-scanned pinned frame.
The fixed-capacity region refuses overlapping calls while worker threads are
registered. The handle stays protected from unload for the complete call,
including nested native calls and cleanup after an error."
  (let* ((arity (plist-get handle :arity))
         (boxed (eq (plist-get handle :abi) 'boxed))
         ;; Arguments and the result are separate questions.  Prefer what
         ;; the artifact records; the inference stays for artifacts written
         ;; before it did.
         (param-boxed (let ((repr (plist-get handle :param-repr)))
                        (cond ((eq repr 'sexp-ptr) t)
                              ((eq repr 'raw-i64) nil)
                              (t boxed)))))
    (unless (= (length args) arity)
      (error "nelisp-native-load: %s takes %d argument(s), got %d"
             (plist-get handle :name) arity (length args)))
    (nelisp-native-load--with-active-call
     handle
     (lambda ()
      (if (not param-boxed)
          (nelisp-native-load--call-raw handle args boxed)
        (let* ((env (nelisp--native-env)))
      (unless (and (integerp env) (> env 0))
        (error "nelisp-native-load: no active runtime environment"))
      (let ((pin-frame (nelisp-native-load--pin-begin env)))
        (unless (and (integerp pin-frame) (> pin-frame 0))
          (error "nelisp-native-load: pinned root frame is busy"))
        (unwind-protect
            (let* ((out (nelisp-native-load--pin-reserve env pin-frame))
                   (mirror env)
                   (frames (+ env 32))
                   (scratch (nelisp-native-load--pin-reserve env pin-frame))
                   (name-slot (nelisp-native-load--pin-reserve env pin-frame))
                   (callbacks
                    (let ((i 0) (slots nil))
                      (while (< i nelisp-native-load-callback-slots)
                        (setq slots
                              (cons (nelisp-native-load--pin-reserve env pin-frame)
                                    slots))
                        (setq i (1+ i)))
                      (nreverse slots)))
                   (scratch-cell (nelisp-native-load--pin-reserve env pin-frame))
                   (scratch-set-slot
                    (nelisp-native-load--symbol-addr "nl_vector_set_slot"))
                   (scratch-i 0)
                   (passed nil)
                   (rest args)
                   (raw nil)
                   (call-size
                    (nelisp-native-load--page-round
                     (length (plist-get handle :trampoline-bytes))))
                   (call-entry nil)
                   (patch-offsets (plist-get handle :boundary-imm64-offsets))
                   (patch-values (append (list out mirror frames scratch name-slot)
                                         callbacks)))
              (nelisp-native-load--make-scratch-vector scratch)
              ;; Keep each vector element pointer-backed for compiled code
              ;; that retains slot addresses across nested native calls.
              (while (< scratch-i nelisp-native-load-scratch-slots)
                (nelisp-native-load-box scratch-cell "s")
                (ptr-call scratch-set-slot (ptr-read-u64 scratch 8)
                          scratch-i scratch-cell 0 0 0)
                (setq scratch-i (1+ scratch-i)))
              (while rest
                (let ((slot
                       (if (fboundp 'nelisp--native-pin-copy)
                           (nelisp--native-pin-copy env pin-frame (car rest))
                         (let ((legacy-slot
                                (nelisp-native-load--pin-reserve env pin-frame)))
                           (nelisp-native-load-box
                            legacy-slot (car rest) env pin-frame)
                           legacy-slot))))
                  (unless (and (integerp slot) (> slot 0))
                    (error "nelisp-native-load: cannot pin argument value"))
                  (setq passed (cons slot passed)))
                (setq rest (cdr rest)))
              (setq passed (nreverse passed))
              ;; `ptr-call' reads six arguments after the address unconditionally.
              (while (< (length passed) (length nelisp-native-load--arg-regs))
                (setq passed (append passed (list 0))))
              ;; A private trampoline embeds only this invocation's pinned
              ;; root addresses.
              (setq call-entry (nelisp-native-load--mmap call-size t))
              (unwind-protect
                  (progn
                    (nelisp-native-load--poke-bytes
                     call-entry 0 (plist-get handle :trampoline-bytes))
                    (while patch-offsets
                      (ptr-write-u64 call-entry (car patch-offsets) (car patch-values))
                      (setq patch-offsets (cdr patch-offsets))
                      (setq patch-values (cdr patch-values)))
                    (ptr-write-u64 call-entry
                                   (plist-get handle :trampoline-entry-imm64-offset)
                                   (+ (plist-get handle :codepage)
                                      (plist-get handle :body-entry)))
                    (setq raw (apply (function ptr-call) call-entry passed)))
                (let ((rc (nelisp-native-load--unmap call-entry call-size)))
                  (unless (= rc 0)
                    (error "nelisp-native-load: temporary trampoline munmap failed (%d)"
                           rc))))
              ;; Some bodies carry dispatcher externs while returning a raw
              ;; register value.  Their metadata settles raw-bool exactly;
              ;; otherwise retain the existing low-address guard.
              (when (and boxed (integerp raw)
                         (< raw nelisp-native-load-page-bytes))
                (setq boxed nil))
              (cond
               ((eq (plist-get handle :return-repr) 'raw-bool)
                (let ((value (nelisp-native-load--decode-raw-bool raw)))
                  (nelisp-native-load-box out value env pin-frame)
                  value))
               ((or (eq (plist-get handle :return-repr) 'sexp-ptr)
                    (and boxed (eq (plist-get handle :return-repr) 'unknown)))
                (progn
                  ;; The native body may return a temporary slot.  Copy it
                  ;; into the pinned output root before Lisp decoding can
                  ;; allocate and move the referenced object.
                  ;; Do this in u32 lanes: a u64 slot word can exceed NeLisp's
                  ;; fixnum range (notably the upper half of an IEEE-754
                  ;; payload), and the Lisp integer bridge may truncate it.
                  (let ((i 0))
                    (while (< i 8)
                      (ptr-write-u32 out (* i 4) (ptr-read-u32 raw (* i 4)))
                      (setq i (1+ i))))
                  (nelisp-native-load-unbox out env pin-frame)))
               (t raw)))
          (nelisp-native-load--pin-end env pin-frame)))))))))

(defun nelisp-native-load-unload (handle)
  "Unmap HANDLE's pages and return the number of regions released.

A loaded function otherwise stays mapped for the life of the process,
which is what a cache wants but not what a caller loading many artifacts
wants -- the section 9 bench mapped enough of them to be killed.

Calling a handle after unloading it jumps into an unmapped page, so this
blanks the handle's addresses: a stale call then dereferences 0 at the
trampoline rather than executing whatever the kernel maps there next.  An
active call causes an error before any mapping or handle field is changed."
  (let ((released 0)
        (active-calls (gethash handle nelisp-native-load--active-calls 0)))
    (when (> active-calls 0)
      (error "nelisp-native-load: cannot unload active handle (%d call(s))"
             active-calls))
    (let ((inhibit-quit t)
          regions)
      (dolist (pair (list (cons :entry :entry-size)
                          (cons :codepage :code-size)
                          (cons :slots :slots-size)))
        (let ((addr (plist-get handle (car pair)))
              (size (plist-get handle (cdr pair))))
          (when (and (integerp addr) (> addr 0) (integerp size) (> size 0))
            (push (cons addr size) regions))))
      (plist-put handle :entry 0)
      (plist-put handle :codepage 0)
      (plist-put handle :slots 0)
      (plist-put handle :out 0)
      (plist-put handle :exports nil)
      (dolist (region (nreverse regions))
        (let ((addr (car region))
              (size (cdr region)))
          ;; munmap(2) is syscall 11 on x86_64.
          (let ((rc (nelisp-native-load--unmap addr size)))
            (unless (= rc 0)
              (error "nelisp-native-load: munmap of %d bytes at %d failed (%d)"
                     size addr rc))
            (setq released (1+ released)))))
      released)))

(defun nelisp-native-load-exec (path name args)
  "Map NAME from PATH and call it with ARGS, in one step.

The whole sequence runs with the mid-form collector disarmed -- see
`nelisp-native-load--without-midform-collect' for why every raw buffer
in this file needs that."
  (nelisp-native-load--without-midform-collect
   (lambda ()
     (nelisp-native-load-call (nelisp-native-load-artifact path name) args))))

(defconst nelisp-native-load--rooted-production-layout
  '(:domain "nelisp-rooted-elf-v2" :target x86_64-linux :version 2
    :sexp-bytes 32 :root-frame-version 2 :root-slot-limit 16384
    :environment-globals-offset 0 :environment-frames-offset 32
    :environment-lexical-offset 64
    :exports (("nl_root_pin_begin_v2" text 1) ("nl_root_pin_reserve_v2" text 2)
              ("nl_root_pin_end_v2" text 2) ("nl_root_pin_slot_v2" text 3)
              ("nl_native_car_v2" text 4) ("nl_native_cdr_v2" text 4)
              ("nl_native_cons_v2" text 5) ("wf_bytecode_call_gateway_exit" text 6)
              ("wf_bytecode_call_gateway" text 6) ("nl_alloc_symbol" text 3)
              ("nelisp_cons_construct" text 3) ("nl_arena_base" data 8)
              ("nl_gc_mark_pinned_roots" text 0) ("nl_gc_mark_thread_roots" text 0)
              ("nl_gc_mark_recorded_env" text 1)
              ("nl_native_funcall_v2" text 6) ("nl_native_poll_v2" text 6)))
  "Immutable production root layout, independently domain separated from reload.")

(defun nelisp-native-load--rooted-contract-copy-node (item ancestors depth budget)
  "Copy one bounded contract node without runtime macro expansion."
  (setcar budget (1- (car budget)))
  (if (or (< (car budget) 0) (> depth 64))
      (error "nelisp-native-load: rooted contract bound exceeded"))
  (cond ((consp item)
         (if (memq item ancestors) (error "nelisp-native-load: cyclic rooted contract"))
         (let ((next (cons item ancestors)))
           (cons (nelisp-native-load--rooted-contract-copy-node (car item) next (1+ depth) budget)
                 (nelisp-native-load--rooted-contract-copy-node (cdr item) next depth budget))))
        ((stringp item)
         (if (or (> (length item) 256) (text-properties-at 0 item)
                 (< (or (next-property-change 0 item) (length item)) (length item)))
             (error "nelisp-native-load: malformed rooted contract string"))
         (copy-sequence item))
        ((or (symbolp item) (integerp item)) item)
        (t (error "nelisp-native-load: malformed rooted contract atom"))))

(defun nelisp-native-load--rooted-contract-snapshot (value)
  "Copy bounded acyclic contract data without sharing mutable strings."
  (nelisp-native-load--rooted-contract-copy-node value nil 0 (list 4096)))

(defun nelisp-native-load-rooted-production-contract ()
  "Return copied production root layout, GC contract and bridge table order."
  (nelisp-native-load--rooted-contract-snapshot
   (list (if (nelisp-native-load--windows-p)
             (let ((layout (copy-tree nelisp-native-load--rooted-production-layout)))
               (plist-put layout :domain "nelisp-rooted-pe-v2")
               (plist-put layout :target 'x86_64-windows)
               (plist-put layout :calling-convention 'win64) layout)
           nelisp-native-load--rooted-production-layout)
         nelisp-runtime-reload-gc-contract
         nelisp-native-load-bridgeable-symbols)))

(defun nelisp-native-load-rooted-production-contract-hash ()
  "Return the source-owned immutable production root ABI digest."
  (let ((print-length nil) (print-level nil) (print-circle nil)
        (print-escape-newlines nil) (print-escape-control-characters nil))
    (nelisp-native-load-sha256
     (prin1-to-string (nelisp-native-load-rooted-production-contract)))))

(defun nelisp-native-load-rooted-runtime-dependency-context ()
  "Return opaque root owner identities and independent mutable input copies."
  (vector (mapcar (lambda (name) (cons name (and (fboundp name) (symbol-function name))))
                  (append '(nelisp-native-load-rooted-production-contract
                    nelisp-native-load-rooted-production-contract-hash
                    nelisp-native-load-rooted-runtime-dependency-context
                    nelisp-native-load--rooted-contract-snapshot
                    nelisp-native-load--rooted-contract-copy-node
                    car cdr cons list setcar memq copy-sequence text-properties-at next-property-change
                    1- < > consp 1+ stringp length symbolp integerp error
                    prin1-to-string mapcar fboundp symbol-function vector and or cond)
                          (when (nelisp-native-load--windows-p)
                            '(nelisp-native-load--windows-p copy-tree plist-put))))
          (nelisp-native-load-sha256-dependency-context)
          (nelisp-native-load-rooted-production-contract)))

(provide 'nelisp-native-load)

;;; nelisp-native-load.el ends here
