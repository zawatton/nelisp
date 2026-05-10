;;; nelisp-static-linker.el --- Doc 93 §93.a static linker core  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 93 §93.a — relocation core for the Phase 47 static linker.
;;
;; Consumes relocation markers emitted by Doc 92's macro assembler
;; (`nelisp-asm-x86_64-emit-reloc') and patches the raw byte stream
;; so the result can be handed to Doc 91 ELF writer
;; (`nelisp-elf-write-binary') as the loaded `.text' / `.rodata'
;; bytes.  Three relocation types are supported (= Doc 93 §3.1
;; minimum set):
;;
;;   `pc32'   = R_X86_64_PC32   (2) -- S + A - P, 4-byte LE.
;;   `abs64'  = R_X86_64_64     (1) -- S + A,     8-byte LE.
;;   `plt32'  = R_X86_64_PLT32  (4) -- S + A - P, 4-byte LE.
;;
;; For ET_EXEC + static link (= Phase 47 scope), `plt32' is
;; semantically identical to `pc32' (= no PLT trampoline needed).
;;
;; This file ships the §93.a relocation-core slice only.  Section
;; merge + symtab merge land in §93.b; the multi-unit driver
;; (`nelisp-link-units') lands in §93.c.  None of those are wired
;; here -- §93.a is callable standalone via `nelisp-link-apply-relocs'
;; against an externally-built symbol table.
;;
;; Symbol table shape:
;;   (:name S :value VA :size N :section SEC :bind global|local|weak
;;    :type func|object|notype)
;; The shape matches the `:symbols' plist consumed by Doc 91
;; `nelisp-elf-write-binary' so the symtab object can be threaded
;; through the linker into the ELF writer unchanged.
;;
;; Relocation entry shape:
;;   (:offset N :type pc32|abs64|plt32 :symbol NAME :addend A)
;; Matches the per-entry plist produced by `nelisp-asm-x86_64-emit-
;; reloc' (= `:type' / `:sym' / `:offset' / `:addend'); the `:sym'
;; spelling from the assembler is accepted as an alias of `:symbol'
;; so test fixtures and live assembler output can share the helper.

;;; Code:

(require 'cl-lib)

(define-error 'nelisp-link-error "nelisp static linker error")
(define-error 'nelisp-link--unresolved-symbol
  "nelisp static linker: unresolved symbol" 'nelisp-link-error)
(define-error 'nelisp-link--rel32-overflow
  "nelisp static linker: rel32 displacement out of range"
  'nelisp-link-error)

;; ---- §93.a (1) symbol table ----

(defun nelisp-link-symbol (name value &rest rest)
  "Build a symbol-table entry plist.
NAME is the symbol name (string).  VALUE is its virtual address
(integer).  Optional REST is a plist with keys `:size' (default
0), `:section' (default `text'), `:bind' (default `global'),
`:type' (default `func').  Matches the entry shape consumed by
Doc 91 `nelisp-elf-write-binary'."
  (list :name name
        :value value
        :size (or (plist-get rest :size) 0)
        :section (or (plist-get rest :section) 'text)
        :bind (or (plist-get rest :bind) 'global)
        :type (or (plist-get rest :type) 'func)))

(defun nelisp-link-symtab-make ()
  "Return an empty mutable symbol table (= hash keyed on name)."
  (make-hash-table :test 'equal))

(defun nelisp-link-symtab-add (symtab sym)
  "Insert SYM (= a `nelisp-link-symbol' plist) into SYMTAB.
Returns SYMTAB.  Refuses to overwrite an existing GLOBAL/GLOBAL
collision -- signals `nelisp-link-error'.  WEAK / LOCAL semantics
land in §93.b; here LOCAL is permitted to shadow LOCAL silently
(= unit-mangled keys are caller's responsibility)."
  (let* ((name (plist-get sym :name))
         (existing (gethash name symtab)))
    (when (and existing
               (eq (plist-get existing :bind) 'global)
               (eq (plist-get sym :bind) 'global))
      (signal 'nelisp-link-error
              (list :multiply-defined name)))
    (puthash name sym symtab)
    symtab))

(defun nelisp-link-symtab-lookup (symtab name)
  "Return the symbol-table entry for NAME, or nil if absent."
  (gethash name symtab))

;; ---- §93.a (2) relocation entry ----

(defun nelisp-link-reloc (offset type symbol &optional addend)
  "Build a relocation entry plist.
OFFSET is the patch site (= byte position within the target
section).  TYPE is one of `pc32' / `abs64' / `plt32'.  SYMBOL is
the referenced symbol name (string).  ADDEND defaults to 0."
  (unless (memq type '(pc32 abs64 plt32))
    (signal 'nelisp-link-error (list :unknown-reloc-type type)))
  (list :offset offset :type type :symbol symbol
        :addend (or addend 0)))

(defun nelisp-link--reloc-symbol-name (reloc)
  "Return the referenced symbol name for RELOC.
Accepts either `:symbol' (= Doc 93 §93.a shape) or `:sym' (=
assembler-side shape from `nelisp-asm-x86_64-emit-reloc')."
  (or (plist-get reloc :symbol)
      (plist-get reloc :sym)))

;; ---- §93.a (3) byte-stream patch primitives ----

(defun nelisp-link--ensure-mutable (bytes)
  "Return a fresh mutable vector copy of BYTES.
Accepts either a unibyte-string or a vector input.  The caller
mutates the result; the input is left untouched."
  (let* ((n (length bytes))
         (v (make-vector n 0))
         (i 0))
    (while (< i n)
      (aset v i (if (stringp bytes) (aref bytes i) (aref bytes i)))
      (setq i (1+ i)))
    v))

(defun nelisp-link--patch-le32 (vec pos value)
  "Write VALUE as 4-byte little-endian into VEC at POS.
VALUE may be negative (signed).  Two's-complement is encoded via
`logand' against #xff per byte, so `(-4)' becomes
`FC FF FF FF'."
  (let ((i 0))
    (while (< i 4)
      (aset vec (+ pos i) (logand (ash value (- (* i 8))) #xff))
      (setq i (1+ i)))))

(defun nelisp-link--patch-le64 (vec pos value)
  "Write VALUE as 8-byte little-endian into VEC at POS."
  (let ((i 0))
    (while (< i 8)
      (aset vec (+ pos i) (logand (ash value (- (* i 8))) #xff))
      (setq i (1+ i)))))

(defun nelisp-link--check-rel32-range (d sym)
  "Signal `nelisp-link--rel32-overflow' if D is outside int32 range.
SYM is the relocation's referenced symbol (used only for error
reporting)."
  (unless (and (>= d (- (ash 1 31))) (< d (ash 1 31)))
    (signal 'nelisp-link--rel32-overflow (list sym d))))

(defun nelisp-link--vec->ubstring (vec)
  "Convert byte VEC into a unibyte-string (= ELF writer input)."
  (apply #'unibyte-string (append vec nil)))

;; ---- §93.a (4) apply one + many ----

(defun nelisp-link-apply-reloc (bytes reloc symtab section-va)
  "Patch BYTES with one RELOC entry, return patched bytes.
BYTES is a unibyte-string or vector.  RELOC is a plist as built by
`nelisp-link-reloc'.  SYMTAB is a hash table from
`nelisp-link-symtab-make'.  SECTION-VA is the virtual base
address of the target section (= the start of BYTES in the loaded
image).  The return value matches the BYTES type (= string in →
string out, vector in → vector out)."
  (let* ((vec (nelisp-link--ensure-mutable bytes))
         (offset (plist-get reloc :offset))
         (type (plist-get reloc :type))
         (sym-name (nelisp-link--reloc-symbol-name reloc))
         (addend (or (plist-get reloc :addend) 0))
         (entry (nelisp-link-symtab-lookup symtab sym-name))
         (S (or (and entry (plist-get entry :value))
                (signal 'nelisp-link--unresolved-symbol
                        (list sym-name)))))
    (pcase type
      ((or 'pc32 'plt32)
       (let* ((P (+ section-va offset 4))
              (d (- (+ S addend) P)))
         (nelisp-link--check-rel32-range d sym-name)
         (nelisp-link--patch-le32 vec offset d)))
      ('abs64
       (nelisp-link--patch-le64 vec offset (+ S addend)))
      (_ (signal 'nelisp-link-error
                 (list :reloc-unsupported-type type))))
    (if (stringp bytes)
        (nelisp-link--vec->ubstring vec)
      vec)))

(defun nelisp-link-apply-relocs (bytes relocs symtab section-va)
  "Patch BYTES with every entry in RELOCS, return patched bytes.
BYTES, SYMTAB, and SECTION-VA carry the same meaning as
`nelisp-link-apply-reloc'.  Each RELOC is applied in order against
the running buffer (= identical patch math to walking the list and
calling `nelisp-link-apply-reloc' once per entry).  Signals
`nelisp-link--unresolved-symbol' on the first missing referent."
  (let ((vec (nelisp-link--ensure-mutable bytes)))
    (dolist (r relocs)
      (setq vec (nelisp-link-apply-reloc vec r symtab section-va)))
    (if (stringp bytes)
        (nelisp-link--vec->ubstring vec)
      vec)))

;; ---- §93.a (5) test fixture ----

(defun nelisp-link--make-test-asm (bytes &optional relocs symbols)
  "Build a mock compiled-unit plist for §93.a unit testing.
BYTES is the section payload (= unibyte-string or vector).
RELOCS is a list of relocation plists; SYMBOLS is a list of
symbol-table entries.  The returned plist mirrors the
`(:type :sym :offset :addend)' entry shape that
`nelisp-asm-x86_64-emit-reloc' writes, so §93.b can switch the
helper out for live assembler output without breaking callers."
  (list :bytes (if (stringp bytes)
                   bytes
                 (nelisp-link--vec->ubstring bytes))
        :relocs (or relocs nil)
        :symbols (or symbols nil)))

(provide 'nelisp-static-linker)

;;; nelisp-static-linker.el ends here
