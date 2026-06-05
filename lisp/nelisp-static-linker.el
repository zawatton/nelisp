;;; nelisp-static-linker.el --- Doc 93 §93.a + §93.b + §93.c static linker  -*- lexical-binding: t; -*-

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
;;   `pc32'             = R_X86_64_PC32                 (2)
;;                      -- S + A - P, 4-byte LE.
;;   `abs64'            = R_X86_64_64                   (1)
;;                      -- S + A, 8-byte LE.
;;   `plt32'            = R_X86_64_PLT32                (4)
;;                      -- S + A - P, 4-byte LE.
;;   `adr-prel-pg-hi21' = R_AARCH64_ADR_PREL_PG_HI21  (275)
;;                      -- page(S + A) - page(P), ADRP immhi:immlo.
;;   `add-abs-lo12-nc'  = R_AARCH64_ADD_ABS_LO12_NC   (277)
;;                      -- (S + A) & 0xFFF into ADD imm12.
;;
;; For ET_EXEC + static link (= Phase 47 scope), `plt32' is
;; semantically identical to `pc32' (= no PLT trampoline needed).
;;
;; §93.a ships the relocation-core slice (= byte-stream patch math +
;; symbol-table struct).  §93.b layers section combine, 2-pass symtab
;; merge, WEAK symbol handling, and the partial driver
;; `nelisp-link-units-2pass' on top.  §93.c adds the top-level
;; orchestrator `nelisp-link-units' that wires the 2-pass driver into
;; Doc 91 `nelisp-elf-write-binary' so a list of compile units is
;; emitted as an executable ELF binary.  §93.c entry points:
;; `nelisp-link--compute-layout' (= section VA layout matching the
;; ELF writer's internal placement), `nelisp-link--export-symtab' (=
;; symtab -> ELF writer `:symbols' plist list), and
;; `nelisp-link-units' (= full driver returning FILE-PATH).
;;
;; Symbol table shape:
;;   (:name S :value VA :size N :section SEC :bind global|local|weak
;;    :type func|object|notype)
;; The shape matches the `:symbols' plist consumed by Doc 91
;; `nelisp-elf-write-binary' so the symtab object can be threaded
;; through the linker into the ELF writer unchanged.
;;
;; Relocation entry shape:
;;   (:offset N :type pc32|abs64|plt32|b26-pc|adr-prel-pg-hi21
;;             |add-abs-lo12-nc :symbol NAME :addend A)
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
section).  TYPE is one of `pc32' / `abs64' / `plt32' / `b26-pc' /
`adr-prel-pg-hi21' / `add-abs-lo12-nc'.  SYMBOL is the referenced
symbol name (string).  ADDEND defaults to 0."
  (unless (memq type '(pc32 abs64 plt32 b26-pc
                             adr-prel-pg-hi21 add-abs-lo12-nc))
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

(define-error 'nelisp-link--branch26-overflow
  "nelisp static linker: arm64 branch displacement out of range"
  'nelisp-link-error)

(defun nelisp-link--check-branch26-range (d sym)
  "Signal if byte displacement D cannot fit an AArch64 imm26 branch."
  (unless (and (zerop (logand d #x3))
               (>= (ash d -2) (- (ash 1 25)))
               (<  (ash d -2) (ash 1 25)))
    (signal 'nelisp-link--branch26-overflow (list sym d))))

(define-error 'nelisp-link--adrp-page-overflow
  "nelisp static linker: arm64 ADRP page displacement out of range"
  'nelisp-link-error)

(defun nelisp-link--check-adrp-page-range (d sym)
  "Signal if page displacement D cannot fit an AArch64 ADRP imm21."
  (unless (and (zerop (logand d #xFFF))
               (>= (ash d -12) (- (ash 1 20)))
               (<  (ash d -12) (ash 1 20)))
    (signal 'nelisp-link--adrp-page-overflow (list sym d))))

(defun nelisp-link--vec->ubstring (vec)
  "Convert byte VEC into a unibyte-string (= ELF writer input).
Uses small `unibyte-string' chunks because standalone NeLisp still
mis-handles large-arity `(apply #'unibyte-string ...)' calls."
  (let ((n (length vec))
        (i 0)
        (chunks nil))
    (while (< i n)
      (let ((limit (if (< (+ i 32) n) (+ i 32) n))
            (bytes nil))
        (while (< i limit)
          (setq bytes (cons (aref vec i) bytes))
          (setq i (1+ i)))
        (push (apply #'unibyte-string (nreverse bytes)) chunks)))
    (apply #'concat (nreverse chunks))))

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
         (_patched (nelisp-link-apply-reloc-inplace vec reloc symtab section-va)))
    (if (stringp bytes)
        (nelisp-link--vec->ubstring vec)
      vec)))

(defun nelisp-link-apply-reloc-inplace (vec reloc symtab section-va)
  "Patch mutable byte vector VEC with one RELOC entry, then return VEC.
This is the destructive sibling of `nelisp-link-apply-reloc'.  Link passes that
already own the combined section use it to avoid copying that section once per
relocation."
  (let* ((offset (plist-get reloc :offset))
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
      ('b26-pc
       (let* ((P (+ section-va offset))
              (d (- (+ S addend) P))
              (imm26 (logand (ash d -2) #x3ffffff))
              (cur (logior (aref vec offset)
                           (ash (aref vec (+ offset 1)) 8)
                           (ash (aref vec (+ offset 2)) 16)
                           (ash (aref vec (+ offset 3)) 24)))
              (patched (logior (logand cur #xfc000000) imm26)))
         (nelisp-link--check-branch26-range d sym-name)
         (nelisp-link--patch-le32 vec offset patched)))
      ('adr-prel-pg-hi21
       (let* ((P (+ section-va offset))
              (page-s (logand (+ S addend) (lognot #xFFF)))
              (page-p (logand P (lognot #xFFF)))
              (d (- page-s page-p))
              (imm21 (ash d -12))
              (immlo (logand imm21 #x3))
              (immhi (logand (ash imm21 -2) #x7ffff))
              (cur (logior (aref vec offset)
                           (ash (aref vec (+ offset 1)) 8)
                           (ash (aref vec (+ offset 2)) 16)
                           (ash (aref vec (+ offset 3)) 24)))
              (patched (logior (logand cur #x9F00001F)
                               (ash immlo 29)
                               (ash immhi 5))))
         (nelisp-link--check-adrp-page-range d sym-name)
         (nelisp-link--patch-le32 vec offset patched)))
      ('add-abs-lo12-nc
       (let* ((imm12 (logand (+ S addend) #xFFF))
              (cur (logior (aref vec offset)
                           (ash (aref vec (+ offset 1)) 8)
                           (ash (aref vec (+ offset 2)) 16)
                           (ash (aref vec (+ offset 3)) 24)))
              (patched (logior (logand cur #xFFC003FF)
                               (ash imm12 10))))
         (nelisp-link--patch-le32 vec offset patched)))
      ('abs64
       (nelisp-link--patch-le64 vec offset (+ S addend)))
      (_ (signal 'nelisp-link-error
                 (list :reloc-unsupported-type type))))
    vec))

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
                      (nelisp-link-apply-reloc-inplace
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

;; ---- §93.c (1) layout compute ----

(defconst nelisp-link--default-base-va #x400000
  "Default ET_EXEC load base VA (= matches Doc 91 ELF writer).")

(defconst nelisp-link--default-page-size #x1000
  "Default page size used for RW segment alignment.")

(defconst nelisp-link--ehdr-size 64
  "Size of an ELF64 Ehdr in bytes (= Doc 91 fixed constant).")

(defconst nelisp-link--phdr-size 56
  "Size of an ELF64 Phdr in bytes (= Doc 91 fixed constant).")

(defun nelisp-link--align-up (n alignment)
  "Round N up to the next multiple of ALIGNMENT."
  (let ((rem (mod n alignment)))
    (if (zerop rem) n (+ n (- alignment rem)))))

(defun nelisp-link--section-len (combined section-name)
  "Return the byte length of SECTION-NAME in COMBINED.
COMBINED is the alist returned by `nelisp-link-combine-sections'.
For `bss', the stored payload is an integer size; for the byte-
bearing sections it is a string/vector whose `length' is returned.
Returns 0 if the section entry is absent."
  (let* ((entry (cdr (assq section-name combined)))
         (bytes (and entry (plist-get entry :bytes))))
    (cond ((null bytes) 0)
          ((eq section-name 'bss) bytes)
          (t (length bytes)))))

(defun nelisp-link--compute-layout (combined &optional base-va page-size)
  "Compute section VA layout matching the Doc 91 ELF writer.
COMBINED is the alist returned by `nelisp-link-combine-sections'.
BASE-VA defaults to `nelisp-link--default-base-va' (= #x400000),
PAGE-SIZE defaults to `nelisp-link--default-page-size' (= 4 KB).
Returns an alist `((SECTION-NAME . VA) ...)' covering `text',
`rodata', `data', `bss'.  Placement strategy mirrors
`nelisp-elf--build-rich': Ehdr + Phdrs precede `.text', `.rodata'
follows `.text' in the same RX segment, `.data' starts on a fresh
page (= RW segment), `.bss' immediately follows `.data'.  PHNUM =
2 when either `.data' or `.bss' is non-empty (= matches the writer)."
  (let* ((bv (or base-va nelisp-link--default-base-va))
         (ps (or page-size nelisp-link--default-page-size))
         (text-size   (nelisp-link--section-len combined 'text))
         (rodata-size (nelisp-link--section-len combined 'rodata))
         (data-size   (nelisp-link--section-len combined 'data))
         (bss-size    (nelisp-link--section-len combined 'bss))
         (have-rw     (or (> data-size 0) (> bss-size 0)))
         (phnum       (if have-rw 2 1))
         (text-off    (+ nelisp-link--ehdr-size
                         (* nelisp-link--phdr-size phnum)))
         (text-va     (+ bv text-off))
         (rodata-off  (+ text-off text-size))
         (rodata-va   (+ bv rodata-off))
         (rx-end      (+ rodata-off rodata-size))
         (data-off    (and have-rw (nelisp-link--align-up rx-end ps)))
         (data-va     (and have-rw (+ bv data-off)))
         (bss-va      (and have-rw (+ data-va data-size))))
    (list (cons 'text text-va)
          (cons 'rodata rodata-va)
          (cons 'data (or data-va (+ bv rx-end)))
          (cons 'bss (or bss-va (+ bv rx-end))))))

;; ---- §93.c (2) symtab export to ELF writer shape ----

(defun nelisp-link--export-symtab (symtab section-layout)
  "Convert SYMTAB hash table into ELF writer `:symbols' plist list.
SYMTAB is the merged symbol table produced by §93.b pass 1; each
entry has `:value' set to the symbol's absolute VA.  The Doc 91
ELF writer's rich path adds its own `section-vaddr' to the
supplied `:value' field, so this helper converts each VA back into
a section-relative byte offset using SECTION-LAYOUT (= alist
`((SECTION-NAME . VA) ...)').  Each output entry preserves
`:name', `:size', `:section', `:bind', `:type', and uses
section-offset for `:value'.  Symbols are returned in
deterministic order (= sorted by name)."
  (let (names)
    (maphash (lambda (k _v) (push k names)) symtab)
    (setq names (sort names #'string<))
    (mapcar
     (lambda (name)
       (let* ((sym (gethash name symtab))
              (section (or (plist-get sym :section) 'text))
              (sec-va (or (cdr (assq section section-layout))
                          (signal 'nelisp-link-error
                                  (list :export-unknown-section
                                        section name))))
              (va (plist-get sym :value))
              (offset (- va sec-va)))
         (list :name name
               :value offset
               :size (or (plist-get sym :size) 0)
               :section section
               :bind (or (plist-get sym :bind) 'global)
               :type (or (plist-get sym :type) 'func))))
     names)))

;; ---- §93.c (3) top-level driver ----

(defun nelisp-link--bytes-or-empty (bytes-alist section-name)
  "Return the BYTES-ALIST entry for SECTION-NAME, or empty string."
  (let ((b (cdr (assq section-name bytes-alist))))
    (cond ((null b) "")
          ((stringp b) b)
          (t (nelisp-link--vec->ubstring b)))))

(defun nelisp-link-units (file-path units
                                    &optional entry-sym section-layout
                                    machine)
  "Link UNITS and emit an executable ELF binary to FILE-PATH.
UNITS is a list of compile-unit plists (= `nelisp-link-unit-make').
ENTRY-SYM is the entry-point symbol name; defaults to `\"_start\"'.
SECTION-LAYOUT is an alist `((SECTION-NAME . VA) ...)'; when nil
it is computed by `nelisp-link--compute-layout' from the combined
sections so the linker's reloc patches match the ELF writer's
internal placement.  MACHINE is the arch tag forwarded to the ELF
writer (= `x86_64' / `aarch64' / integer); defaults to `x86_64'.
Pipeline: combine sections (§93.b), compute layout (§93.c), run
2-pass symtab + reloc resolution (§93.b), export symtab to ELF
writer shape (§93.c), then call `nelisp-elf-write-binary' with
`:text' / `:rodata' / `:data' / `:bss-size' / `:symbols' /
`:entry-sym' / `:machine'.  `:relocs' is intentionally omitted (=
relocs are fully resolved into the bytes already, so the writer
must NOT emit a `.rela.text' section).  Returns FILE-PATH."
  (require 'nelisp-elf-write)
  (let* ((combined (nelisp-link-combine-sections units))
         (layout (or section-layout
                     (nelisp-link--compute-layout combined)))
         (link-result (nelisp-link-units-2pass units layout))
         (bytes (plist-get link-result :bytes))
         (symtab (plist-get link-result :symtab))
         (symbols (nelisp-link--export-symtab symtab layout))
         (text (nelisp-link--bytes-or-empty bytes 'text))
         (rodata (nelisp-link--bytes-or-empty bytes 'rodata))
         (data (nelisp-link--bytes-or-empty bytes 'data))
         (bss-size (or (cdr (assq 'bss bytes)) 0))
         (entry (or entry-sym "_start"))
         (mach (or machine 'x86_64))
         (plist (list :text text
                      :rodata rodata
                      :data data
                      :bss-size bss-size
                      :symbols symbols
                      :entry-sym entry
                      :machine mach)))
    (unless (nelisp-link-symtab-lookup symtab entry)
      (signal 'nelisp-link--unresolved-symbol (list entry :entry)))
    (declare-function nelisp-elf-write-binary "nelisp-elf-write" (path s))
    (nelisp-elf-write-binary file-path plist)
    file-path))

(defconst nelisp-link--pe-image-base #x140000000
  "Default PE32+ image base (= matches `nelisp-pe-write').")

(defun nelisp-link--pe-section-rvas (combined imports)
  "Return PE section RVA metadata for COMBINED and IMPORTS."
  (require 'nelisp-pe-write)
  (let* ((text-size (nelisp-link--section-len combined 'text))
         (rodata-size (nelisp-link--section-len combined 'rodata))
         (data-size (+ (nelisp-link--section-len combined 'data)
                       (nelisp-link--section-len combined 'bss)))
         (text-rva nelisp-pe--section-alignment)
         (rodata-rva (and (> rodata-size 0)
                          (nelisp-pe--align-up (+ text-rva text-size)
                                               nelisp-pe--section-alignment)))
         (data-rva (and (> data-size 0)
                        (nelisp-pe--align-up
                         (+ (or rodata-rva text-rva)
                            (if (> rodata-size 0) rodata-size text-size))
                         nelisp-pe--section-alignment)))
         (idata-rva (and imports
                         (nelisp-pe--align-up
                          (+ (cond
                              ((> data-size 0) data-rva)
                              ((> rodata-size 0) rodata-rva)
                              (t text-rva))
                             (cond
                              ((> data-size 0) data-size)
                              ((> rodata-size 0) rodata-size)
                              (t text-size)))
                          nelisp-pe--section-alignment))))
    (list :text-rva text-rva
          :rodata-rva (or rodata-rva
                          (nelisp-pe--align-up (+ text-rva text-size)
                                               nelisp-pe--section-alignment))
          :data-rva (or data-rva
                        (nelisp-pe--align-up
                         (+ (or rodata-rva text-rva)
                            (if (> rodata-size 0) rodata-size text-size))
                         nelisp-pe--section-alignment))
          :idata-rva idata-rva)))

(defun nelisp-link--pe-layout (combined imports &optional image-base)
  "Return an absolute-VA section layout for PE linking."
  (let* ((base (or image-base nelisp-link--pe-image-base))
         (rvas (nelisp-link--pe-section-rvas combined imports))
         (data-va (+ base (plist-get rvas :data-rva))))
    (list (cons 'text (+ base (plist-get rvas :text-rva)))
          (cons 'rodata (+ base (plist-get rvas :rodata-rva)))
          (cons 'data data-va)
          (cons 'bss (+ data-va (nelisp-link--section-len combined 'data))))))

(defun nelisp-link--pe-import-names (imports)
  "Return IMPORTS' function names in import-table order."
  (let (names)
    (dolist (entry imports)
      (dolist (name (cdr entry))
        (push name names)))
    (nreverse names)))

(defun nelisp-link--pe-import-thunk-unit (imports text-va text-offset
                                                  idata-info)
  "Return a link-unit exporting import thunks for IMPORTS.
Each thunk is `jmp qword ptr [rip+disp32]' and is named after the imported
function so direct `call rel32 Function' relocations can target it."
  (require 'nelisp-pe-write)
  (let ((cbuf (nelisp-pe--make-buffer))
        (symbols nil)
        (iat-map (plist-get idata-info :iat-rva-alist))
        (cursor 0))
    (dolist (name (nelisp-link--pe-import-names imports))
      (let* ((iat-rva (or (cdr (assoc name iat-map))
                          (error "nelisp-link: missing IAT slot for %S" name)))
             (thunk-va (+ text-va text-offset cursor))
             (iat-va (+ nelisp-link--pe-image-base iat-rva))
             (disp (- iat-va (+ thunk-va 6))))
        (push (nelisp-link-symbol name cursor
                                  :section 'text :bind 'global :type 'func)
              symbols)
        (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x25))
        (nelisp-pe--write-le32 cbuf disp)
        (setq cursor (+ cursor 6))))
    (nelisp-link-unit-make "pe-import-thunks.o"
                           (list (cons 'text (nelisp-pe--buffer-bytes cbuf)))
                           (nreverse symbols)
                           nil)))

(defun nelisp-link--pe-empty-import-thunk-unit (imports)
  "Return a size-only import thunk unit for PE layout prediction."
  (nelisp-link-unit-make
   "pe-import-thunks.o"
   (list (cons 'text (make-string (* 6 (length (nelisp-link--pe-import-names
                                                imports)))
                                  0)))
   nil nil))

(defun nelisp-link-units-pe32 (file-path units
                                         &optional entry-sym imports options)
  "Link UNITS and emit a PE32+ executable to FILE-PATH.
IMPORTS has the same shape as `nelisp-pe-write-exe-binary' generic :imports.
OPTIONS is a plist accepting PE image options such as :stack-reserve.
Imported function names are resolved by synthesized in-image thunks, so normal
Phase47 `extern-call' direct rel32 calls can target Windows IAT entries."
  (require 'nelisp-pe-write)
  (let* ((entry (or entry-sym "_start"))
         (normalized-imports (nelisp-pe--normalize-imports imports))
         (predict-units (if normalized-imports
                            (append units
                                    (list (nelisp-link--pe-empty-import-thunk-unit
                                           normalized-imports)))
                          units))
         (predict-combined (nelisp-link-combine-sections predict-units))
         (predict-rvas (nelisp-link--pe-section-rvas predict-combined
                                                     normalized-imports))
         (idata-info (and normalized-imports
                          (nelisp-pe--build-idata
                           (plist-get predict-rvas :idata-rva)
                           normalized-imports)))
         (orig-combined (nelisp-link-combine-sections units))
         (orig-text-size (nelisp-link--section-len orig-combined 'text))
         (predict-layout (nelisp-link--pe-layout predict-combined
                                                 normalized-imports))
         (text-va (cdr (assq 'text predict-layout)))
         (all-units (if normalized-imports
                        (append units
                                (list (nelisp-link--pe-import-thunk-unit
                                       normalized-imports text-va
                                       orig-text-size idata-info)))
                      units))
         (combined (nelisp-link-combine-sections all-units))
         (layout (nelisp-link--pe-layout combined normalized-imports))
         (link-result (nelisp-link-units-2pass all-units layout))
         (bytes (plist-get link-result :bytes))
         (symtab (plist-get link-result :symtab))
         (text (nelisp-link--bytes-or-empty bytes 'text))
         (rodata (nelisp-link--bytes-or-empty bytes 'rodata))
         (data (concat (nelisp-link--bytes-or-empty bytes 'data)
                       (make-string (or (cdr (assq 'bss bytes)) 0) 0)))
         (entry-symtab (nelisp-link-symtab-lookup symtab entry))
         (entry-rva (and entry-symtab
                         (- (plist-get entry-symtab :value)
                            nelisp-link--pe-image-base))))
    (unless entry-symtab
      (signal 'nelisp-link--unresolved-symbol (list entry :entry)))
    (nelisp-pe-write-exe-binary
     file-path
     (list :text text
           :rodata rodata
           :data data
           :entry-rva entry-rva
           :imports normalized-imports
           :stack-reserve (plist-get options :stack-reserve)
           :stack-commit (plist-get options :stack-commit)
           :heap-reserve (plist-get options :heap-reserve)
           :heap-commit (plist-get options :heap-commit)))
    file-path))

(defun nelisp-link--macho-exec-layout (combined)
  "Return absolute section layout for the current arm64 Mach-O executable writer.
The writer places code at `nelisp-mach-o--exe-text-vmaddr' plus
`nelisp-mach-o--exe-code-off'.  Its executable path currently has one
__TEXT payload, so .rodata is linked immediately after .text and appended to
the emitted :text bytes."
  (require 'nelisp-mach-o-write)
  (let* ((text-va (+ nelisp-mach-o--exe-text-vmaddr
                    nelisp-mach-o--exe-code-off))
         (text-size (nelisp-link--section-len combined 'text))
         (rodata-va (+ text-va text-size))
         (rodata-size (nelisp-link--section-len combined 'rodata)))
    (list (cons 'text text-va)
          (cons 'rodata rodata-va)
          (cons 'data (+ rodata-va rodata-size))
          (cons 'bss (+ rodata-va rodata-size)))))

(defun nelisp-link-units-macho-exec (file-path units
                                               &optional entry-sym machine)
  "Link UNITS and emit an unsigned native macOS arm64 Mach-O executable.
This is the Mach-O sibling of `nelisp-link-units' / `nelisp-link-units-pe32'
for the standalone pure-Elisp pipeline.  The current executable writer supports
one __TEXT payload; therefore .text and .rodata are concatenated after all
relocations are resolved.  .data/.bss are rejected until the Mach-O writer grows
a writable segment."
  (require 'nelisp-mach-o-write)
  (let* ((entry (or entry-sym "_main"))
         (mach (or machine 'aarch64))
         (combined (nelisp-link-combine-sections units))
         (data-size (nelisp-link--section-len combined 'data))
         (bss-size (nelisp-link--section-len combined 'bss))
         (layout (nelisp-link--macho-exec-layout combined))
         (link-result (nelisp-link-units-2pass units layout))
         (bytes (plist-get link-result :bytes))
         (symtab (plist-get link-result :symtab))
         (text (nelisp-link--bytes-or-empty bytes 'text))
         (rodata (nelisp-link--bytes-or-empty bytes 'rodata))
         (entry-symtab (nelisp-link-symtab-lookup symtab entry)))
    (unless (eq mach 'aarch64)
      (signal 'nelisp-link-error (list :mach-o-exec-only-aarch64 mach)))
    (when (or (> data-size 0) (> bss-size 0))
      (signal 'nelisp-link-error
              (list :mach-o-exec-rw-sections-unsupported
                    :data data-size :bss bss-size)))
    (unless entry-symtab
      (signal 'nelisp-link--unresolved-symbol (list entry :entry)))
    (unless (zerop (- (plist-get entry-symtab :value)
                      (cdr (assq 'text layout))))
      (signal 'nelisp-link-error
              (list :mach-o-entry-must-start-text entry
                    (plist-get entry-symtab :value))))
    (nelisp-mach-o-write-executable
     file-path
     (list :text (concat text rodata)
           :machine mach
           :entry-sym entry))
    file-path))

(provide 'nelisp-static-linker)

;;; nelisp-static-linker.el ends here
