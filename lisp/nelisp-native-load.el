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
(require 'nelisp-runtime-reload-abi)

(defconst nelisp-native-load-bridgeable-symbols
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
    "nelisp_env_lookup_value")
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
(defconst nelisp-native-load-raw-runtime-abi-v2 "nelisp-runtime-raw-v2"
  "Calling convention contract for full native runtime units.

Entries are ordinary Linux x86_64 SysV i64 functions.  Seven arguments are
supported because GC root and compaction entry points use the stack argument
at position seven.  The runtime table, rather than a single GC function
pointer, selects every externally reachable GC entry.")

(defconst nelisp-native-load-raw-artifact-format-v2 'nelisp-private-nelr-v2
  "Container format for full native runtime-unit artifacts.")

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

Goes through a raw buffer and `nelisp--sha256-bytes' rather than handing
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
            (syscall-direct 11 buf size 0 0 0 0)))
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

(defvar nelisp-native-load--running-binary-sha256-cache :unset
  "Cached digest of the executable hosting the runtime reload reader.

The executable cannot be replaced in place on the supported Unix targets, so
one digest is sufficient for all units loaded by a REPL.  `:unset' is kept
distinct from nil: a reader without a readable self image must report that
fact instead of treating an absent identity as a wildcard.")

(defun nelisp-native-load--sha256-file-external (path)
  "Return PATH's SHA-256 using the standard `sha256sum' helper, or nil.

This is the fast path for a standalone reader: copying a multi-megabyte ELF
image one byte at a time through the interpreted pointer API is both slow and
unnecessary.  The subprocess sees only the pathname and its output is
validated as a 64-character digest; callers still compare that digest with
the manifest before mapping code."
  (when (fboundp 'call-process)
    (let ((output (condition-case nil
                      (make-temp-file "nelisp-runtime-reload-sha256-")
                    (error nil))))
      (when output
        (unwind-protect
            (condition-case nil
                (when (= 0 (call-process "sha256sum" nil output nil path))
                  (let ((line (nelisp-native-load--read-file output)))
                    (when (and (stringp line) (>= (length line) 64))
                      (let ((digest (substring line 0 64)))
                        (when (string-match-p
                               "\\`[0-9a-fA-F]\\{64\\}\\'" digest)
                          (downcase digest))))))
              (error nil))
          (ignore-errors (delete-file output)))))))

(defun nelisp-native-load--running-binary-sha256 ()
  "Return the SHA-256 of this process's executable, or nil when unavailable.

Linux exposes the running image through `/proc/self/exe'.  The path is an OS
interface, not a repository or machine-specific build path.  The loader does
not accept a caller-selected path here: accepting one would let an artifact
claim the digest of a different executable and defeat the same-binary ABI
check."
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
                       (and (stringp bytes)
                            (> (string-bytes bytes) 0)
                            (nelisp-native-load--sha256 bytes)))))
      (setq nelisp-native-load--running-binary-sha256-cache digest)
      digest)))

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
  (let ((addr (syscall-direct 9 0 size (if executable 7 3) 34 -1 0)))
    (when (< addr nelisp-native-load-page-bytes)
      (error "nelisp-native-load: mmap of %d bytes failed (%d)" size addr))
    addr))

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

(defun nelisp-native-load-box (addr value)
  "Write VALUE into the Sexp slot at ADDR and return ADDR.

Integers, nil, t and single-byte strings.  A string is materialized by
the runtime's own `nl_alloc_str', reached through the same stub
mechanism the loaded code uses, so the result is a string the runtime
owns rather than a slot this pretends is one.

Anything else is refused rather than written as a raw word: the runtime
takes a slot as a pointer and would dereference it."
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
   (t (error "nelisp-native-load: cannot box %S" value)))
  addr)

(defun nelisp-native-load-unbox (addr)
  "Return the value in the Sexp slot at ADDR.

Symbols come back interned and strings as their bytes.  A float is
refused: its payload is raw f64 bits and turning those back into a
number needs arithmetic this does not do, so returning the bit pattern
as an integer would be a wrong answer rather than a missing one."
  (let ((tag (ptr-read-u64 addr 0)))
    (cond
     ((= tag nelisp-native-load-tag-nil) nil)
     ((= tag nelisp-native-load-tag-t) t)
     ((= tag nelisp-native-load-tag-int) (ptr-read-u64 addr 8))
     ((= tag nelisp-native-load-tag-string)
      (nelisp-native-load--payload-string addr))
     ((= tag nelisp-native-load-tag-symbol)
      (intern (nelisp-native-load--payload-string addr)))
     ((= tag nelisp-native-load-tag-float)
      (error "nelisp-native-load: float results are not decoded"))
     (t (error "nelisp-native-load: result tag %d is not one this unboxes" tag)))))

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
  "Write a fresh scratch vector Sexp into the slot at ADDR.

`nl_alloc_vector' returns the NlVector box; the Sexp that names it is
tag 8 with the box at payload+8, which is the shape the reader's own
`nl_logic_build_scratch' builds and the shape `nl_vector_slot_ptr'
expects -- it derefs payload+8 to reach the box."
  (let ((box (ptr-call (nelisp-native-load--symbol-addr "nl_alloc_vector")
                       nelisp-native-load-scratch-slots 0 0 0 0 0))
        (set-slot (nelisp-native-load--symbol-addr "nl_vector_set_slot"))
        (i 0))
    (when (or (not (integerp box)) (= box 0))
      (error "nelisp-native-load: scratch vector allocation returned %S" box))
    ;; Every element has to hold a POINTER, not an immediate.
    ;;
    ;; `nl_vector_slot_ptr' returns the stored word when it is a pointer
    ;; and a FRESH temporary box when it is an immediate.  Compiled code
    ;; fills an element by calling it once to get somewhere to write the
    ;; value, then again to hand that same storage to
    ;; `nl_vector_set_slot' -- which only works if the two calls return
    ;; the same address.  Over an immediate they return two throwaways,
    ;; the value is written into the first and the second is copied out,
    ;; and `(vector 7 8 9)' comes back as three Nils.
    ;;
    ;; So the element cannot be nil, t or an integer: `nl_val_clone_into'
    ;; folds exactly those three back into an immediate word.  A string
    ;; takes the rebox path instead and leaves a pointer behind.
    (while (< i nelisp-native-load-scratch-slots)
      (let ((cell (alloc-bytes 32 8)))
        (nelisp-native-load-box cell "s")
        (ptr-call set-slot box i cell 0 0 0))
      (setq i (1+ i)))
    (ptr-write-u64 addr 0 nelisp-native-load-tag-vector)
    (ptr-write-u64 addr 8 box)
    (ptr-write-u64 addr 16 0)
    (ptr-write-u64 addr 24 0)
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
        (setq idx (1+ idx)))
      (setq rest (cdr rest)))
    (unless found
      (error "nelisp-native-load: no bridge for %s" name))
    (let ((addr (nelisp--native-symbol-addr found)))
      (when (= addr 0)
        (error "nelisp-native-load: %s resolved to 0" name))
      addr)))

;;;; Raw runtime units ------------------------------------------------

(defun nelisp-native-load--raw-supported-p ()
  "Return non-nil when the first raw runtime slice can execute here.

The loader may inspect raw metadata on a host Emacs, but execution requires
the standalone reader's in-process mmap, mprotect and six-GP `ptr-call'."
  (and (eq system-type 'gnu/linux)
       (or (not (boundp 'system-configuration))
           (not (stringp system-configuration))
           (string-match-p "x86_64\\|amd64" system-configuration))
       (fboundp 'syscall-direct)
       (fboundp 'ptr-call)))

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
      (let ((nelisp-standalone--target 'linux-x86_64))
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
     ((not (integerp addend))
      (list :bad-relocation-addend reloc))
     ((not (and (stringp symbol) (member symbol imports)))
      (list :unlisted-import symbol))
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
             (cons 'seq forms) :arch 'x86_64 :format 'elf)))
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

(defun nelisp-native-load-raw-v2-compile-file
    (source-path artifact-path &optional build-id binary-sha256)
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
continue to use `nelisp-native-load-raw-compile-file'."
  (unless (and (stringp source-path) (file-readable-p source-path))
    (error "nelisp-native-load: v2 raw source is not readable: %S" source-path))
  (unless (and (stringp artifact-path) (> (length artifact-path) 0))
    (error "nelisp-native-load: v2 raw artifact path is empty"))
  (unless (nelisp-native-load--raw-v2-contract)
    (error "nelisp-native-load: GC ABI contract is unavailable"))
  (unless (nelisp-native-load--raw-v2-symbols)
    (error "nelisp-native-load: runtime resolver contract is unavailable"))
  (unless (fboundp 'nelisp-aot-compile-to-link-unit)
    (require 'nelisp-aot-compiler))
  (unless (fboundp 'nelisp-standalone--chunk-arena-rewrite)
    ;; Host compilation normally arrives through the standalone build script;
    ;; this fallback keeps the public API usable from a clean Emacs session.
    (require 'nelisp-standalone-build nil t))
  (unless (fboundp 'nelisp-aot-compile-to-link-unit)
    (error "nelisp-native-load: raw compiler is unavailable in this runtime"))
  (let* ((source
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally source-path)
            (buffer-string)))
         (forms (nelisp-native-load--raw-source-forms source-path source))
         (contract (nelisp-native-load--raw-v2-contract))
         (resolver-symbols (nelisp-native-load--raw-v2-symbols))
         (layout nelisp-native-load-raw-layout-id-v2)
         (build (or build-id
                    (and (boundp 'nelisp--cli-version) nelisp--cli-version)
                    "unknown"))
         (binary (or binary-sha256
                     (nelisp-native-load--running-binary-sha256)))
         (prepared (nelisp-native-load--raw-v2-chunk-rewrite forms))
         (data-names (nelisp-native-load--raw-v2-collect-data-addr-names
                      prepared))
         (rewritten (nelisp-native-load--raw-v2-rewrite-data-addr prepared))
         (unit nil))
    (unless forms
      (error "nelisp-native-load: v2 raw source has no top-level defun"))
    (dolist (form forms)
      (unless (and (listp form) (eq (car form) 'defun))
        (error "nelisp-native-load: v2 raw source has non-defun top level")))
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
    (let ((nelisp-native-load-raw-max-arity
           nelisp-native-load-raw-max-arity-v2)
          (nelisp-aot-compiler--runtime-entry-params nil))
      (dolist (form rewritten)
        (unless (nelisp-native-load--raw-compile-defun-p form)
          (error "nelisp-native-load: unsupported v2 raw defun: %S"
                 (nth 1 form))))
      (setq unit
            (nelisp-aot-compile-to-link-unit
             (cons 'seq rewritten) :arch 'x86_64 :format 'elf)))
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
                      :type 'func :abi nelisp-native-load-raw-runtime-abi-v2
                      :arity (or (plist-get def :arity) 0)
                      :return 'u64)
                exports)))
      (setq exports (nreverse exports))
      (dolist (import imports)
        (let ((index (nelisp-native-load--raw-v2-resolver-index
                      import resolver-symbols)))
          (unless (and (stringp import) (integerp index))
            (error "nelisp-native-load: v2 import is not exported: %S" import))
          (push (list :name import
                      :kind (if (member import data-names) 'data 'func)
                      :abi nelisp-native-load-raw-runtime-abi-v2
                      :index index :address-mode 'resolver)
                import-descriptors)))
      (setq import-descriptors
            (sort import-descriptors
                  (lambda (a b) (< (plist-get a :index)
                                   (plist-get b :index)))))
      (let ((index 0))
        (dolist (entry contract)
          (let ((name (car entry)) (arity (cdr entry)) (export nil))
          (dolist (candidate exports)
            (when (and (null export) (equal (plist-get candidate :name) name))
              (setq export candidate)))
          (unless export
            (error "nelisp-native-load: v2 GC entry was not exported: %s" name))
          (unless (= arity (plist-get export :arity))
            (error "nelisp-native-load: v2 arity mismatch for %s" name))
          (push (list :name name :arity arity :index index
                      :return 'u64 :abi nelisp-native-load-raw-runtime-abi-v2)
                gc-entries)
            (setq index (1+ index)))))
      (setq gc-entries (nreverse gc-entries))
      (setq manifest
            (list :format nelisp-native-load-raw-artifact-format-v2
                  :kind 'raw-runtime
                  :runtime-kind 'gc-arena
                  :runtime-abi nelisp-native-load-raw-runtime-abi-v2
                  :layout-id layout :arch nelisp-native-load-raw-supported-arch
                  :build-id build :binary-sha256 binary
                  :source (expand-file-name source-path)
                  :source-sha256 (nelisp-native-load--sha256 source)
                  :compiled-source-sha256
                  (nelisp-native-load--sha256 (prin1-to-string rewritten))
                  :runtime-opt-in t
                  :gc-contract-hash
                  (nelisp-native-load--raw-v2-contract-hash contract)
                  :gc-entries gc-entries
                  :gc-table-magic nelisp-native-load-raw-gc-table-magic
                  :gc-table-count (length gc-entries)
                  :resolver-contract-hash
                  (nelisp-native-load--sha256
                   (prin1-to-string resolver-symbols))
                  :native
                  (list :raw-abi nelisp-native-load-raw-runtime-abi-v2
                        :object-format 'nelisp-aot-raw-unit-v2
                        :text-size text-size
                        :text-base64 (base64-encode-string text t)
                        :object-sha256 object-sha256 :object-size text-size
                        :exports exports :symbols exports
                        :imports import-descriptors
                        :extern-symbols imports
                        :relocs (plist-get unit :relocs)
                        :data-size 0 :bss-size 0)))
      (setq manifest
            (append manifest
                    (list :artifact-sha256
                          (nelisp-native-load--sha256
                           (prin1-to-string manifest)))))
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
      manifest)))

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

(defun nelisp-native-load-raw-v2-check (manifest &optional name)
  "Return refusal reasons for a full v2 raw runtime MANIFEST.

This check is deliberately complete before mmap: it validates the executable
identity, the shared resolver index, every GC table entry and every import
relocation.  An absent ABI module is a refusal, never a reason to trust the
candidate's self-described table order."
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
         (problems nil)
         (add (lambda (problem) (setq problems (cons problem problems)))))
    (unless (eq (plist-get manifest :kind) 'raw-runtime)
      (funcall add (list :raw-kind (plist-get manifest :kind))))
    (unless (eq (plist-get manifest :format)
                nelisp-native-load-raw-artifact-format-v2)
      (funcall add (list :raw-format (plist-get manifest :format))))
    (unless (equal (plist-get manifest :runtime-abi)
                   nelisp-native-load-raw-runtime-abi-v2)
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
      (unless (equal (plist-get manifest :resolver-contract-hash)
                     (nelisp-native-load--sha256
                      (prin1-to-string resolver-symbols)))
        (funcall add (list :raw-resolver-contract-hash
                           (plist-get manifest :resolver-contract-hash)))))
    (when native
      (unless (equal (plist-get native :raw-abi)
                     nelisp-native-load-raw-runtime-abi-v2)
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
      (unless (and (listp exports) exports)
        (funcall add (list :raw-no-exports)))
      (unless (and (listp imports0) (not (memq nil imports))
                   (= (length imports)
                      (length (delete-dups (copy-sequence imports)))))
        (funcall add (list :raw-imports-not-explicit imports0)))
      (when resolver-symbols
        (let ((rest imports0))
          (while rest
            (let* ((entry (car rest))
                   (import (nelisp-native-load--raw-import-name entry))
                   (kind (nelisp-native-load--raw-import-kind entry))
                   (index (and (listp entry) (plist-get entry :index)))
                   (expected (and (stringp import)
                                  (nelisp-native-load--raw-v2-resolver-index
                                   import resolver-symbols))))
              (unless (and (stringp import) (integerp expected)
                           (integerp index) (= index expected))
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
                                  nelisp-native-load-raw-runtime-abi-v2))
                (funcall add (list :raw-import-abi import
                                   (and (listp entry) (plist-get entry :abi)))))
              (when (and (eq kind 'data)
                         (not (member import
                                      nelisp-native-load-raw-v2-data-symbols)))
                (funcall add (list :raw-data-import-not-shared import))))
            (setq rest (cdr rest)))))
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
                                nelisp-native-load-raw-runtime-abi-v2)
                         (eq (plist-get entry :return) 'u64))
              (funcall add (list :raw-export-abi entry))))
          (setq rest (cdr rest)))))
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
            (unless export
              (funcall add (list :raw-gc-export-missing name)))
            (when (and export (/= (plist-get export :arity) arity))
              (funcall add (list :raw-gc-export-arity name
                                 (plist-get export :arity) arity)))
            (when entry
              (unless (equal (plist-get entry :abi)
                             nelisp-native-load-raw-runtime-abi-v2)
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
      (unless (and native (nelisp-native-load--raw-export native name))
        (funcall add (list :raw-no-such-export name))))
    (nreverse problems)))

(defun nelisp-native-load-raw-v2-artifact
    (path &optional name expected-binary-sha256)
  "Map v2 raw runtime artifact PATH and return its handle.

Function imports receive the usual absolute jump stub.  Shared data imports
receive a `movabs rax, ADDRESS; ret' getter, so the mapped text never embeds
a private copy of runtime BSS.  The GC contract table is allocated beside the
code and retained by the handle; its first two words are the count and magic,
followed by the 24 contract entry addresses in ABI order."
  (let* ((manifest (nelisp-native-load-manifest path))
         (native (nelisp-native-load--raw-native manifest))
         (exports (and native (nelisp-native-load--raw-exports native)))
         (chosen (or name (and exports (plist-get (car exports) :name))))
         (problems (nelisp-native-load-raw-v2-check manifest chosen))
         (declared-binary (plist-get manifest :binary-sha256)))
    (when problems
      (error "nelisp-native-load: cannot load v2 raw %s from %s: %S"
             chosen path problems))
    (when (fboundp 'nelisp--native-runtime-symbol-addr)
      (unless (and (stringp expected-binary-sha256)
                   (equal declared-binary expected-binary-sha256))
        (error "nelisp-native-load: v2 binary identity mismatch for %s" path)))
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
                       (addr (nelisp-native-load--raw-symbol-addr import)))
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
                       (addr (cdr (assoc (plist-get entry :name)
                                         (let ((xs nil) (es exports))
                                           (while es
                                             (push (cons (plist-get (car es) :name)
                                                         (plist-get (car es) :value)) xs)
                                             (setq es (cdr es)))
                                           xs)))))
                  (unless (integerp addr)
                    (error "nelisp-native-load: v2 GC export missing: %s"
                           (plist-get entry :name)))
                  (ptr-write-u64 table (+ 16 (* 8 (plist-get entry :index)))
                                 (+ codepage addr)))
                (setq rest (cdr rest))))
            (unless (= 0 (syscall-direct
                          10 table (nelisp-native-load--page-round table-size)
                          1 0 0 0))
              (error "nelisp-native-load: mprotect read-only GC table failed"))
            (nelisp-native-load--mprotect-rx codepage code-size)
            (let ((addresses nil) (rest exports))
              (while rest
                (let ((entry (car rest)))
                  (push (cons (plist-get entry :name)
                              (+ codepage (plist-get entry :value))) addresses))
                (setq rest (cdr rest)))
              (setq addresses (nreverse addresses))
              (let ((handle (list :kind 'raw-runtime-v2 :path path
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
            (ignore-errors (syscall-direct 11 codepage code-size 0 0 0 0)))
          (when (and (integerp table) (> table 0))
            (ignore-errors
              (syscall-direct 11 table (nelisp-native-load--page-round table-size)
                              0 0 0 0))))))))

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

(defun nelisp-native-load--mprotect-rx (addr size)
  "Make executable mapping ADDR/SIZE read+execute, or signal a refusal."
  (let ((rc (syscall-direct 10 addr size 5 0 0 0))) ; mprotect, R|X
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
              (syscall-direct 11 codepage code-size 0 0 0 0)))))))))

(defun nelisp-native-load-raw-call (handle args)
  "Call raw HANDLE with integer ARGS and return its u64 result."
  (let* ((arity (plist-get handle :arity))
         (max-arity (if (equal (plist-get handle :runtime-abi)
                              nelisp-native-load-raw-runtime-abi-v2)
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
                         nelisp-native-load-raw-runtime-abi-v2))
             (not (member "nl_runtime_reload_alloc_original"
                          (plist-get alloc-handle :imports))))
        (list :raw-alloc-original-unbound))
       ((and gc-handle
             (not (equal (plist-get gc-handle :runtime-abi)
                         nelisp-native-load-raw-runtime-abi-v2))
             (not (member "nl_runtime_reload_gc_original"
                          (plist-get gc-handle :imports))))
        (list :raw-gc-original-unbound))
       ((and alloc-handle
             (/= (or (plist-get alloc-handle :arity) -1) 2))
        (list :raw-alloc-arity (plist-get alloc-handle :arity)))
       ((and gc-handle
             (not (equal (plist-get gc-handle :runtime-abi)
                         nelisp-native-load-raw-runtime-abi-v2))
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
                             nelisp-native-load-raw-runtime-abi-v2)
                        (equal (plist-get gc-handle :runtime-abi)
                               nelisp-native-load-raw-runtime-abi-v2)))
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
      ;; ...except scratch, which has to be a vector.  See
      ;; `nelisp-native-load-scratch-slots'.
      (nelisp-native-load--make-scratch-vector (+ slots 32))
      ;; Trampoline: bytes, then the boundary immediates and the entry.
      (nelisp-native-load--poke-bytes trampage 0 tramp-bytes)
      (let ((values (append
                     ;; out, mirror, frames, scratch, name_slot.  Both
                     ;; providers ignore mirror, so it carries the env
                     ;; pointer rather than a wild one.
                     (list slots env env (+ slots 32) (+ slots 64))
                     (let ((cb nil) (k 0))
                       (while (< k nelisp-native-load-callback-slots)
                         (setq cb (cons (+ slots 96 (* 32 k)) cb))
                         (setq k (1+ k)))
                       (nreverse cb))
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
            :arg-slots (+ slots arg-slot-base)
            :name name
            :path path))))

(defun nelisp-native-load-call (handle args)
  "Call the function in HANDLE with ARGS and return its value.

Which convention is used comes from the handle's :abi, and the two are
not interchangeable -- calling a boxed defun with raw integers makes it
do arithmetic on the values, and calling an integer defun with slot
addresses makes it do arithmetic on the pointers.  Measured on `add3',
an extern-less `(+ a (+ b c))': raw arguments answer 6, boxed arguments
answer 406962619651776 and leave `out' untouched."
  (let* ((arity (plist-get handle :arity))
         (arg-slots (plist-get handle :arg-slots))
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
    (let ((i 0)
          (rest args)
          (passed nil)
          (raw nil))
      (while rest
        (if param-boxed
            (setq passed (cons (nelisp-native-load-box
                                (+ arg-slots (* 32 i)) (car rest))
                               passed))
          (unless (integerp (car rest))
            (error "nelisp-native-load: %s takes integers, got %S"
                   (plist-get handle :name) (car rest)))
          (setq passed (cons (car rest) passed)))
        (setq i (1+ i))
        (setq rest (cdr rest)))
      (setq passed (nreverse passed))
      ;; `ptr-call' reads six arguments after the address unconditionally,
      ;; so hand it six.  Passing fewer leaves it walking off the end of
      ;; the argument list rather than seeing a short call.
      (while (< (length passed) (length nelisp-native-load--arg-regs))
        (setq passed (append passed (list 0))))
      (setq raw (apply (function ptr-call) (plist-get handle :entry) passed))
      ;; A defun can carry a dispatcher extern and still answer in a raw
      ;; register -- `(let ((m 3) (i 0)) (integerp n) (if (< i m) 111 222))'
      ;; delegates once and returns 111.  Its `:return-repr' is `unknown',
      ;; so neither the externs nor the metadata settle it, and unboxing
      ;; 111 dereferences address 111.  Nothing below the first page is a
      ;; Sexp, so treat such a result as the raw value it is.
      (when (and boxed (integerp raw) (< raw nelisp-native-load-page-bytes))
        (setq boxed nil))
      ;; The result is what rax holds, not what `out' holds.  For a body
      ;; that ends in a delegated call the two are the same pointer --
      ;; the dispatcher returns `out' -- which is why reading `out'
      ;; looked right until a body ended in something else.  `(let ((v
      ;; (vector 7 8 9))) n)' leaves the vector in `out' and returns the
      ;; boxed `n' in rax, so reading `out' answered with the vector.
      (if (or (eq (plist-get handle :return-repr) 'sexp-ptr)
              (and boxed (eq (plist-get handle :return-repr) 'unknown)))
          (nelisp-native-load-unbox raw)
        raw))))

(defun nelisp-native-load-unload (handle)
  "Unmap HANDLE's pages and return the number of regions released.

A loaded function otherwise stays mapped for the life of the process,
which is what a cache wants but not what a caller loading many artifacts
wants -- the section 9 bench mapped enough of them to be killed.

Calling a handle after unloading it jumps into an unmapped page, so this
blanks the handle's addresses: a stale call then dereferences 0 at the
trampoline rather than executing whatever the kernel maps there next."
  (let ((released 0))
    (dolist (pair (list (cons :entry :entry-size)
                        (cons :codepage :code-size)
                        (cons :slots :slots-size)))
      (let ((addr (plist-get handle (car pair)))
            (size (plist-get handle (cdr pair))))
        (when (and (integerp addr) (> addr 0) (integerp size) (> size 0))
          ;; munmap(2) is syscall 11 on x86_64.
          (let ((rc (syscall-direct 11 addr size 0 0 0 0)))
            (unless (= rc 0)
              (error "nelisp-native-load: munmap of %d bytes at %d failed (%d)"
                     size addr rc))
            (setq released (1+ released))))))
    (plist-put handle :entry 0)
    (plist-put handle :codepage 0)
    (plist-put handle :slots 0)
    (plist-put handle :out 0)
    released))

(defun nelisp-native-load-exec (path name args)
  "Map NAME from PATH and call it with ARGS, in one step.

The whole sequence runs with the mid-form collector disarmed -- see
`nelisp-native-load--without-midform-collect' for why every raw buffer
in this file needs that."
  (nelisp-native-load--without-midform-collect
   (lambda ()
     (nelisp-native-load-call (nelisp-native-load-artifact path name) args))))

(provide 'nelisp-native-load)

;;; nelisp-native-load.el ends here
