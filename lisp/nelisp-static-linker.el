;;; nelisp-static-linker.el --- Doc 93 §93.a + §93.b static linker  -*- lexical-binding: t; -*-

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
;; §93.a ships the relocation-core slice (= byte-stream patch math +
;; symbol-table struct).  §93.b layers section combine, 2-pass symtab
;; merge, WEAK symbol handling, and the partial driver
;; `nelisp-link-units-2pass' on top -- the multi-unit ELF-writer
;; orchestrator (`nelisp-link-units') still lands in §93.c.  §93.b
;; entry points: `nelisp-link-unit-make' (compile-unit struct),
;; `nelisp-link-combine-sections', `nelisp-link--collect-defined-
;; symbols', `nelisp-link--resolve-relocs', and the partial driver
;; `nelisp-link-units-2pass'.
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
(define-error 'nelisp-link--duplicate-symbol
  "nelisp static linker: duplicate STRONG symbol definition"
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

;; ---- §93.b (1) compile-unit struct ----

(defun nelisp-link-unit-make (name sections symbols relocs)
  "Build a compile-unit plist for §93.b multi-unit linking.
NAME is a short string label (= e.g. \"main.o\").  SECTIONS is an
alist `((SECTION-NAME . BYTES) ...)' where SECTION-NAME is one of
`text' / `rodata' / `data' / `bss'; BYTES is a unibyte-string (or
vector) of section payload, except for `bss' where BYTES is an
integer = zero-fill byte size.  SYMBOLS is a list of
`nelisp-link-symbol' plists with `:value' interpreted as the
symbol's byte offset within its section in this unit (NOT a final
VA).  RELOCS is a list of `nelisp-link-reloc' plists with an
extra `:section' key naming the section in which the patch lives
(default `text').  Returns a plist consumable by
`nelisp-link-combine-sections' and `nelisp-link-units-2pass'."
  (list :name name :sections sections
        :symbols (or symbols nil) :relocs (or relocs nil)))

(defun nelisp-link--unit-section (unit section-name)
  "Return UNIT's SECTION-NAME bytes/size from its `:sections' alist,
or nil if the unit has no entry for that section."
  (cdr (assq section-name (plist-get unit :sections))))

(defun nelisp-link--section-length (sec section-name)
  "Return the contributed byte length of SEC under SECTION-NAME.
For `bss' SEC is an integer size; for the byte-bearing sections it
is a string/vector whose `length' is returned.  Returns 0 if SEC
is nil."
  (cond ((null sec) 0)
        ((eq section-name 'bss) sec)
        (t (length sec))))

;; ---- §93.b (2) section combine ----

(defun nelisp-link--bytes-append (acc bytes)
  "Return a fresh unibyte-string of ACC concatenated with BYTES.
ACC and BYTES are unibyte-strings or vectors; BYTES may be nil."
  (let* ((a (if (stringp acc) acc (nelisp-link--vec->ubstring acc)))
         (b (cond ((null bytes) "")
                  ((stringp bytes) bytes)
                  (t (nelisp-link--vec->ubstring bytes)))))
    (concat a b)))

(defun nelisp-link-combine-sections (units)
  "Concat all UNITS' same-named sections into one combined buffer.
For `text' / `rodata' / `data', the returned bytes are the
ordered concatenation of every unit's contribution (unit order =
UNITS order).  For `bss', the result is the integer sum of sizes.
Returns an alist `((SECTION-NAME . (:bytes BYTES :offsets ((UNIT-
NAME . OFFSET) ...))) ...)' covering text/rodata/data/bss.  The
`offsets' entry records each unit's starting position within the
combined section.  A unit that contributes 0 bytes still receives
an entry whose OFFSET equals the section's running length at the
time the unit was visited."
  (let ((result '())
        (sections '(text rodata data bss)))
    (dolist (sn sections)
      (let ((acc (if (eq sn 'bss) 0 ""))
            (offs '()))
        (dolist (u units)
          (let* ((uname (plist-get u :name))
                 (sec (nelisp-link--unit-section u sn))
                 (len (nelisp-link--section-length sec sn))
                 (off (if (eq sn 'bss) acc (length acc))))
            (push (cons uname off) offs)
            (setq acc (if (eq sn 'bss) (+ acc len)
                        (nelisp-link--bytes-append acc sec)))))
        (push (cons sn (list :bytes acc :offsets (nreverse offs)))
              result)))
    (nreverse result)))

(defun nelisp-link--combined-offset (combined section-name unit-name)
  "Return the byte offset of UNIT-NAME within COMBINED SECTION-NAME.
Signals `nelisp-link-error' if the (section, unit) pair is absent
from COMBINED (= caller bug)."
  (let* ((entry (cdr (assq section-name combined)))
         (offs (and entry (plist-get entry :offsets)))
         (cell (and offs (assoc unit-name offs))))
    (unless cell
      (signal 'nelisp-link-error
              (list :missing-unit-offset section-name unit-name)))
    (cdr cell)))

;; ---- §93.b (3) 2-pass symtab merge with WEAK ----

(defun nelisp-link--symbol-section (sym)
  "Return the section-name for SYM (default `text')."
  (or (plist-get sym :section) 'text))

(defun nelisp-link--symbol-bind (sym)
  "Return SYM's binding keyword (= `global', `local', or `weak').
Defaults to `global' if the symbol omits `:bind'."
  (or (plist-get sym :bind) 'global))

(defun nelisp-link--sort-units-by-name (units)
  "Return UNITS sorted by `:name' (= deterministic WEAK tie-break)."
  (sort (copy-sequence units)
        (lambda (a b)
          (string< (plist-get a :name) (plist-get b :name)))))

(defun nelisp-link--compute-symbol-va (sym unit combined section-layout)
  "Compute the absolute VA of SYM defined in UNIT.
Combines the section base VA from SECTION-LAYOUT (= alist
`((SECTION-NAME . VA) ...)'), UNIT's offset within that section
(from COMBINED), and SYM's intra-unit `:value' offset."
  (let* ((sn (nelisp-link--symbol-section sym))
         (sec-va (or (cdr (assq sn section-layout))
                     (signal 'nelisp-link-error
                             (list :unknown-section sn))))
         (uoff (nelisp-link--combined-offset
                combined sn (plist-get unit :name)))
         (soff (plist-get sym :value)))
    (+ sec-va uoff soff)))

(defun nelisp-link--collect-defined-symbols (units combined section-layout)
  "Pass 1: build a global symtab for DEFINED symbols of UNITS.
For each unit's symbol, compute its absolute VA and insert into a
fresh hash table.  Duplicate handling:
  STRONG (= `global') + STRONG of same name  -> signals
  `nelisp-link--duplicate-symbol' (data: NAME, first unit, second
  unit);
  STRONG + WEAK (any order)                  -> STRONG wins;
  WEAK + WEAK                                -> first one wins,
  where \"first\" is determined by sorting UNITS by `:name' so the
  result is deterministic regardless of caller order.
Returns the populated symtab (= a hash table)."
  (let ((symtab (nelisp-link-symtab-make))
        (sorted (nelisp-link--sort-units-by-name units)))
    (dolist (unit sorted)
      (dolist (sym (plist-get unit :symbols))
        (let* ((bind (nelisp-link--symbol-bind sym))
               (name (plist-get sym :name))
               (existing (nelisp-link-symtab-lookup symtab name)))
          (when (memq bind '(global weak))
            (let* ((va (nelisp-link--compute-symbol-va
                        sym unit combined section-layout))
                   (entry (list :name name :value va
                                :size (or (plist-get sym :size) 0)
                                :section (nelisp-link--symbol-section sym)
                                :bind bind
                                :type (or (plist-get sym :type) 'func)
                                :unit (plist-get unit :name))))
              (cond
               ((null existing)
                (puthash name entry symtab))
               ((and (eq bind 'global)
                     (eq (plist-get existing :bind) 'global))
                (signal 'nelisp-link--duplicate-symbol
                        (list name
                              (plist-get existing :unit)
                              (plist-get unit :name))))
               ((and (eq bind 'global)
                     (eq (plist-get existing :bind) 'weak))
                (puthash name entry symtab))
               ;; WEAK incoming + STRONG existing  -> keep STRONG.
               ;; WEAK + WEAK                       -> keep existing
               ;; (= first by sorted UNITS order = deterministic).
               (t nil)))))))
    symtab))

(defun nelisp-link--reloc-section (rel)
  "Return the patch-target section name for RELoc, default `text'."
  (or (plist-get rel :section) 'text))

(defun nelisp-link--resolve-relocs (units combined section-layout symtab)
  "Pass 2: patch every unit's relocs against the merged SYMTAB.
For each RELOC, the target byte position within its combined
section equals UNIT-OFFSET + RELOC-OFFSET.  Looks up the
referenced symbol; if missing, signals
`nelisp-link--unresolved-symbol' with `(SYM-NAME UNIT-NAME)'.
Returns a fresh alist `((SECTION-NAME . BYTES) ...)' covering
text/rodata/data + a `(bss . SIZE)' pass-through entry.  The
COMBINED argument is the output of `nelisp-link-combine-sections';
SECTION-LAYOUT is the same alist passed to pass 1."
  (let ((sections '(text rodata data))
        (out '()))
    (dolist (sn sections)
      (let* ((entry (cdr (assq sn combined)))
             (bytes (plist-get entry :bytes))
             (vec (nelisp-link--ensure-mutable
                   (if (stringp bytes) bytes
                     (or bytes "")))))
        (push (cons sn vec) out)))
    (let ((bss-entry (cdr (assq 'bss combined))))
      (push (cons 'bss (plist-get bss-entry :bytes)) out))
    (setq out (nreverse out))
    (dolist (unit units)
      (let ((uname (plist-get unit :name)))
        (dolist (rel (plist-get unit :relocs))
          (let* ((rsec (nelisp-link--reloc-section rel))
                 (uoff (nelisp-link--combined-offset
                        combined rsec uname))
                 (roff (plist-get rel :offset))
                 (sym-name (nelisp-link--reloc-symbol-name rel))
                 (entry (nelisp-link-symtab-lookup symtab sym-name)))
            (unless entry
              (signal 'nelisp-link--unresolved-symbol
                      (list sym-name uname)))
            (let* ((vec (cdr (assq rsec out)))
                   (sec-va (cdr (assq rsec section-layout)))
                   (combined-off (+ uoff roff))
                   (shifted (list :offset combined-off
                                  :type (plist-get rel :type)
                                  :symbol sym-name
                                  :addend (or (plist-get rel :addend)
                                              0))))
              (unless vec
                (signal 'nelisp-link-error
                        (list :reloc-bad-section rsec)))
              (setcdr (assq rsec out)
                      (nelisp-link-apply-reloc
                       vec shifted symtab sec-va)))))))
    ;; Convert vectors back to unibyte-strings for downstream consumers.
    (mapcar (lambda (cell)
              (let ((sn (car cell)) (v (cdr cell)))
                (cond
                 ((eq sn 'bss) cell)
                 ((stringp v) cell)
                 ((null v) (cons sn ""))
                 (t (cons sn (nelisp-link--vec->ubstring v))))))
            out)))

;; ---- §93.b (4) partial driver ----

(defun nelisp-link-units-2pass (units section-layout)
  "Run §93.b pass 1 + pass 2 over UNITS and return the link result.
SECTION-LAYOUT is an alist `((SECTION-NAME . VIRTUAL-ADDRESS)
...)' that pins each combined section's load address; the caller
(= §93.c orchestrator) chooses these.  Returns a plist:
  `:bytes'           = alist of `((SECTION-NAME . BYTES) ...)' with
                       all relocs applied (= text/rodata/data +
                       `(bss . SIZE)' pass-through)
  `:symtab'          = the merged symbol table (= hash table)
  `:section-offsets' = the unit-offset map from
                       `nelisp-link-combine-sections' (verbatim)
Signals `nelisp-link--duplicate-symbol' on STRONG collisions and
`nelisp-link--unresolved-symbol' on missing externs."
  (let* ((combined (nelisp-link-combine-sections units))
         (symtab (nelisp-link--collect-defined-symbols
                  units combined section-layout))
         (bytes (nelisp-link--resolve-relocs
                 units combined section-layout symtab)))
    (list :bytes bytes :symtab symtab
          :section-offsets combined)))

(provide 'nelisp-static-linker)

;;; nelisp-static-linker.el ends here
