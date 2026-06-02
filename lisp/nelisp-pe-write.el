;;; nelisp-pe-write.el --- PE32+/COFF object writer (Phase 47)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 101 §101.A — pure-elisp PE32+/COFF relocatable object emitter for
;; Windows 64-bit (AMD64 / x86_64).  The public contract matches the ELF
;; writer's rich plist shape for the ET_REL (relocatable object) case but
;; emits a Microsoft COFF object file as accepted by MSVC `link.exe' and
;; LLVM `lld-link'.
;;
;; Layout produced by `nelisp-pe-write-binary':
;;
;;   +0x00   IMAGE_FILE_HEADER          (20 bytes)
;;   +0x14   IMAGE_SECTION_HEADER[0]    (.text, 40 bytes)
;;   +0x14   IMAGE_SECTION_HEADER[1]    (.rdata, 40 bytes, optional)
;;   ...     IMAGE_SECTION_HEADER[N-1]  (40 bytes each)
;;   ...     .text raw bytes
;;   ...     .rdata raw bytes (optional)
;;   ...     IMAGE_RELOCATION[] for .text (optional)
;;   ...     IMAGE_RELOCATION[] for .rdata (optional)
;;   ...     IMAGE_SYMBOL[]             (18 bytes each)
;;   ...     String table              (4-byte size prefix + NUL-term entries)
;;
;; Relocation types supported (x86_64 / AMD64 COFF):
;;   `pc32' / `plt32'  ->  IMAGE_REL_AMD64_REL32  (0x0004)
;;   `abs32'           ->  IMAGE_REL_AMD64_ADDR32NB (0x0003)
;;   `abs64'           ->  IMAGE_REL_AMD64_ADDR64   (0x0001)
;;
;; The module is freestanding and not yet wired into the production
;; STDLIB load (integration deferred per Doc 101 §101.A).

;;; Code:

;; ---- COFF format constants (= Microsoft PE/COFF spec §3) ----

(defconst nelisp-pe--machine-amd64 #x8664
  "IMAGE_FILE_MACHINE_AMD64 (= x86_64 Windows 64-bit).")

;; IMAGE_FILE_HEADER Characteristics — for an object file (= no IMAGE_FILE_EXECUTABLE_IMAGE):
;; We emit 0 for a standard relocatable object.
(defconst nelisp-pe--header-size 20
  "sizeof(IMAGE_FILE_HEADER) — the fixed-size COFF header.")

(defconst nelisp-pe--section-header-size 40
  "sizeof(IMAGE_SECTION_HEADER) — one COFF section descriptor.")

;; Symbol table entry is 18 bytes (COFF standard short entry).
(defconst nelisp-pe--sym-size 18
  "sizeof(IMAGE_SYMBOL) — 18 bytes.")

;; Relocation entry is 10 bytes.
(defconst nelisp-pe--reloc-size 10
  "sizeof(IMAGE_RELOCATION) — 10 bytes.")

;; Section Characteristics flags (§4.1 of COFF spec).
(defconst nelisp-pe--scn-cnt-code       #x00000020 "IMAGE_SCN_CNT_CODE.")
(defconst nelisp-pe--scn-cnt-init-data  #x00000040 "IMAGE_SCN_CNT_INITIALIZED_DATA.")
(defconst nelisp-pe--scn-mem-execute    #x20000000 "IMAGE_SCN_MEM_EXECUTE.")
(defconst nelisp-pe--scn-mem-read       #x40000000 "IMAGE_SCN_MEM_READ.")
(defconst nelisp-pe--scn-mem-write      #x80000000 "IMAGE_SCN_MEM_WRITE.")
(defconst nelisp-pe--scn-align-16bytes  #x00500000 "IMAGE_SCN_ALIGN_16BYTES (= alignment 16).")
(defconst nelisp-pe--scn-align-8bytes   #x00400000 "IMAGE_SCN_ALIGN_8BYTES (= alignment 8).")

;; Combined Characteristics for common sections.
(defconst nelisp-pe--scn-text-flags
  (logior nelisp-pe--scn-cnt-code
          nelisp-pe--scn-mem-execute
          nelisp-pe--scn-mem-read
          nelisp-pe--scn-align-16bytes)
  "Section Characteristics for a .text code section.")

(defconst nelisp-pe--scn-rdata-flags
  (logior nelisp-pe--scn-cnt-init-data
          nelisp-pe--scn-mem-read
          nelisp-pe--scn-align-8bytes)
  "Section Characteristics for a .rdata read-only data section.")

(defconst nelisp-pe--scn-data-flags
  (logior nelisp-pe--scn-cnt-init-data
          nelisp-pe--scn-mem-read
          nelisp-pe--scn-mem-write
          nelisp-pe--scn-align-8bytes)
  "Section Characteristics for a .data read-write data section.")

;; Symbol StorageClass values (§5.4.2).
(defconst nelisp-pe--sym-class-external 2 "IMAGE_SYM_CLASS_EXTERNAL.")
(defconst nelisp-pe--sym-class-static   3 "IMAGE_SYM_CLASS_STATIC.")
(defconst nelisp-pe--sym-class-label    6 "IMAGE_SYM_CLASS_LABEL.")

;; Symbol section number special values (§5.4.3).
(defconst nelisp-pe--sym-sn-undef  0 "IMAGE_SYM_UNDEFINED (= extern / unresolved).")
(defconst nelisp-pe--sym-sn-abs   -1 "IMAGE_SYM_ABSOLUTE.")

;; Symbol Type (§5.4.1) — upper byte = derived type (DT_FUNCTION = 0x20).
(defconst nelisp-pe--sym-dtype-function #x0020
  "DT_FUNCTION (= derived type: pointer to function).")

;; Relocation Type values for AMD64 COFF (§4.2.1 of COFF spec).
(defconst nelisp-pe--rel-addr64    #x0001 "IMAGE_REL_AMD64_ADDR64.")
(defconst nelisp-pe--rel-addr32nb  #x0003 "IMAGE_REL_AMD64_ADDR32NB (= RVA, no base).")
(defconst nelisp-pe--rel-rel32     #x0004 "IMAGE_REL_AMD64_REL32 (= 32-bit PC-rel, equiv PLT32).")
(defconst nelisp-pe--rel-rel32-1   #x0005 "IMAGE_REL_AMD64_REL32_1.")
(defconst nelisp-pe--rel-rel32-4   #x0008 "IMAGE_REL_AMD64_REL32_4.")
(defconst nelisp-pe--rel-section   #x000A "IMAGE_REL_AMD64_SECTION.")
(defconst nelisp-pe--rel-secrel    #x000B "IMAGE_REL_AMD64_SECREL.")

;; ---- PE32+ executable constants (= Doc 138 Stage 1) ----

(defconst nelisp-pe--dos-stub-size #x80
  "Minimal DOS stub size.  e_lfanew points to the PE signature at 0x80.")

(defconst nelisp-pe--optional-header-pe32plus-size 240
  "sizeof(IMAGE_OPTIONAL_HEADER64) with 16 data directories.")

(defconst nelisp-pe--section-alignment #x1000
  "PE image SectionAlignment for executable images.")

(defconst nelisp-pe--file-alignment #x200
  "PE image FileAlignment for executable images.")

(defconst nelisp-pe--image-base-x86-64 #x140000000
  "Default Windows x86_64 ImageBase used by MSVC-style PE32+ executables.")

(defconst nelisp-pe--subsystem-console 3
  "IMAGE_SUBSYSTEM_WINDOWS_CUI.")

(defconst nelisp-pe--file-executable-image #x0002
  "IMAGE_FILE_EXECUTABLE_IMAGE.")

(defconst nelisp-pe--file-large-address-aware #x0020
  "IMAGE_FILE_LARGE_ADDRESS_AWARE.")

(defconst nelisp-pe--exe-text-flags
  (logior nelisp-pe--scn-cnt-code
          nelisp-pe--scn-mem-execute
          nelisp-pe--scn-mem-read)
  "Section Characteristics for an executable image .text section.")

(defconst nelisp-pe--exe-rdata-flags
  (logior nelisp-pe--scn-cnt-init-data
          nelisp-pe--scn-mem-read)
  "Section Characteristics for an executable image .rdata section.")

;; ---- chunk-buffer abstraction (= same pattern as nelisp-elf-write) ----

(defun nelisp-pe--make-buffer ()
  "Make a new chunk-buffer accumulator.
Returns a plist (:chunks REVERSE-LIST :length N)."
  (list :chunks nil :length 0))

(defun nelisp-pe--buffer-bytes (cbuf)
  "Finalize CBUF and return its accumulated unibyte string."
  (nelisp-pe--coerce-unibyte
   (apply #'concat (nreverse (plist-get cbuf :chunks)))))

(defsubst nelisp-pe--buffer-length (cbuf)
  "Return current cumulative byte length of CBUF (= O(1))."
  (plist-get cbuf :length))

(defsubst nelisp-pe--cbuf-push (cbuf bytes)
  "Push unibyte BYTES onto CBUF in place and update :length."
  (let ((len (length bytes))
        (chunks-cell (cdr cbuf))
        (length-cell (cdr (cdr (cdr cbuf)))))
    (setcar chunks-cell (cons bytes (car chunks-cell)))
    (setcar length-cell (+ (car length-cell) len))
    cbuf))

(defsubst nelisp-pe--buf-emit (cbuf bytes)
  "Append unibyte BYTES to CBUF."
  (nelisp-pe--cbuf-push cbuf bytes))

(defun nelisp-pe--coerce-unibyte (str)
  "Return a unibyte copy of STR."
  (if (multibyte-string-p str)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert str)
        (buffer-substring-no-properties (point-min) (point-max)))
    str))

;; ---- byte / integer write helpers ----

(defsubst nelisp-pe--write-u8 (cbuf v)
  "Append byte V (0..255) to CBUF."
  (nelisp-pe--buf-emit cbuf (unibyte-string (logand v #xff))))

(defun nelisp-pe--write-le16 (cbuf v)
  "Append unsigned 16-bit V to CBUF in little-endian order."
  (nelisp-pe--buf-emit
   cbuf
   (unibyte-string (logand v #xff)
                   (logand (ash v -8) #xff))))

(defun nelisp-pe--write-le32 (cbuf v)
  "Append unsigned 32-bit V to CBUF in little-endian order."
  (nelisp-pe--buf-emit
   cbuf
   (unibyte-string (logand v #xff)
                   (logand (ash v -8) #xff)
                   (logand (ash v -16) #xff)
                   (logand (ash v -24) #xff))))

(defun nelisp-pe--write-le32-signed (cbuf v)
  "Append signed 32-bit V to CBUF in two's-complement little-endian."
  (let ((u (if (< v 0)
               (logand (+ v (ash 1 32)) #xFFFFFFFF)
             v)))
    (nelisp-pe--write-le32 cbuf u)))

(defun nelisp-pe--write-le64 (cbuf v)
  "Append unsigned 64-bit V to CBUF in little-endian order."
  (let ((bytes (make-vector 8 0))
        (i 0))
    (while (< i 8)
      (aset bytes i (logand (ash v (- (* i 8))) #xff))
      (setq i (1+ i)))
    (nelisp-pe--buf-emit cbuf (apply #'unibyte-string (append bytes nil)))))

(defun nelisp-pe--write-bytes (cbuf bytes)
  "Append unibyte string BYTES to CBUF verbatim."
  (nelisp-pe--buf-emit cbuf (nelisp-pe--coerce-unibyte bytes)))

(defun nelisp-pe--write-pad (cbuf nbytes)
  "Append NBYTES zero bytes to CBUF."
  (when (> nbytes 0)
    (nelisp-pe--buf-emit cbuf (make-string nbytes 0))))

(defun nelisp-pe--write-align-pad (cbuf align)
  "Pad CBUF with zero bytes until its length is aligned to ALIGN."
  (let ((target (nelisp-pe--align-up (nelisp-pe--buffer-length cbuf) align)))
    (nelisp-pe--write-pad cbuf (- target (nelisp-pe--buffer-length cbuf)))))

(defun nelisp-pe--write-fixed-string (cbuf s width)
  "Append S to CBUF, NUL-padded or truncated to WIDTH bytes."
  (let* ((encoded (encode-coding-string s 'utf-8 t))
         (trimmed (if (> (length encoded) width)
                      (substring encoded 0 width)
                    encoded)))
    (nelisp-pe--write-bytes cbuf trimmed)
    (nelisp-pe--write-pad cbuf (- width (length trimmed)))))

;; ---- string table builder ----

(defun nelisp-pe--strtab-make ()
  "Create a fresh COFF string-table accumulator.
The COFF string table starts with a 4-byte size field (= total table
length including the 4-byte field itself).  Strings are NUL-terminated
entries indexed by their byte offset from the start of the table
(including the 4-byte size prefix).  This builder defers the size
field; `nelisp-pe--strtab-bytes' prepends it on finalization.

Returns a plist (:body STR :index HASH-TABLE) where :body is the
NUL-terminated entries *without* the leading 4-byte size prefix
(prepended by `nelisp-pe--strtab-bytes')."
  (list :body ""
        :index (make-hash-table :test 'equal)))

(defun nelisp-pe--strtab-add (state name)
  "Add NAME to COFF string table STATE.
Returns the byte offset *from the start of the table* (= including the
4-byte size prefix) at which NAME can be found.  Names already present
return their cached offset (= dedup).  Names of 8 bytes or fewer can
be stored inline in the IMAGE_SYMBOL Name field and need not be entered
into the string table; callers are responsible for that decision."
  (let* ((body  (plist-get state :body))
         (index (plist-get state :index))
         (cached (gethash name index)))
    (or cached
        (let* (;; Offset = 4 (size field) + length of accumulated entries so far.
               (offset (+ 4 (length body)))
               (entry  (concat (encode-coding-string name 'utf-8 t)
                               (unibyte-string 0))))
          (plist-put state :body (concat body entry))
          (puthash name offset index)
          offset))))

(defun nelisp-pe--strtab-bytes (state)
  "Return the complete COFF string table as a unibyte string.
Prepends the 4-byte little-endian total-size field as required by
the COFF specification (§5.6).  The returned bytes are ready to
be appended verbatim after the symbol table."
  (let* ((body  (plist-get state :body))
         (total (+ 4 (length body)))
         (size-bytes
          (unibyte-string (logand total #xff)
                          (logand (ash total -8) #xff)
                          (logand (ash total -16) #xff)
                          (logand (ash total -24) #xff))))
    (concat size-bytes body)))

;; ---- reloc type translation ----

(defun nelisp-pe--reloc-type-code (sym)
  "Translate user-facing reloc TYPE symbol SYM to COFF AMD64 constant.
Accepted keywords:
  `pc32'   — IMAGE_REL_AMD64_REL32  (= 32-bit PC-relative, used for
             PLT32-equivalent external calls)
  `plt32'  — IMAGE_REL_AMD64_REL32  (= same as pc32 in COFF)
  `abs32'  — IMAGE_REL_AMD64_ADDR32NB (= 32-bit RVA without image base)
  `abs64'  — IMAGE_REL_AMD64_ADDR64  (= 64-bit absolute)
Raw integers pass through unchanged."
  (cond
   ((integerp sym) sym)
   ((eq sym 'pc32)  nelisp-pe--rel-rel32)
   ((eq sym 'plt32) nelisp-pe--rel-rel32)
   ((eq sym 'abs32) nelisp-pe--rel-addr32nb)
   ((eq sym 'abs64) nelisp-pe--rel-addr64)
   (t (error "nelisp-pe: unknown relocation type %S" sym))))

;; ---- symbol section-number mapping ----

(defun nelisp-pe--section-number (sect text-sn rdata-sn data-sn)
  "Map section keyword SECT to its COFF 1-based section number.
TEXT-SN, RDATA-SN, DATA-SN are the assigned section numbers (or nil
when the section is absent).
`undef' maps to IMAGE_SYM_UNDEFINED (= 0)."
  (cond
   ((eq sect 'text)   text-sn)
   ((eq sect 'rodata) (or rdata-sn
                          (error "nelisp-pe: symbol in rodata but no .rdata section")))
   ((eq sect 'data)   (or data-sn
                          (error "nelisp-pe: symbol in data but no .data section")))
   ((eq sect 'undef)  nelisp-pe--sym-sn-undef)
   (t (error "nelisp-pe: unsupported symbol section %S" sect))))

;; ---- IMAGE_FILE_HEADER writer ----

(defun nelisp-pe--write-file-header (cbuf fields)
  "Write a 20-byte IMAGE_FILE_HEADER to CBUF using FIELDS plist.
FIELDS:
  :machine          Machine type (= nelisp-pe--machine-amd64).
  :num-sections     Number of sections.
  :timestamp        TimeDateStamp (default 0).
  :sym-table-ptr    PointerToSymbolTable file offset.
  :num-symbols      NumberOfSymbols.
  :characteristics  Characteristics (default 0).
Returns 20."
  (let ((machine     (or (plist-get fields :machine)       nelisp-pe--machine-amd64))
        (nsects      (or (plist-get fields :num-sections)  0))
        (timestamp   (or (plist-get fields :timestamp)     0))
        (symtab-ptr  (or (plist-get fields :sym-table-ptr) 0))
        (nsyms       (or (plist-get fields :num-symbols)   0))
        (opthdrsz    0)         ; SizeOfOptionalHeader = 0 for object files
        (chars       (or (plist-get fields :characteristics) 0)))
    (nelisp-pe--write-le16 cbuf machine)
    (nelisp-pe--write-le16 cbuf nsects)
    (nelisp-pe--write-le32 cbuf timestamp)
    (nelisp-pe--write-le32 cbuf symtab-ptr)
    (nelisp-pe--write-le32 cbuf nsyms)
    (nelisp-pe--write-le16 cbuf opthdrsz)
    (nelisp-pe--write-le16 cbuf chars)
    20))

;; ---- IMAGE_SECTION_HEADER writer ----

(defun nelisp-pe--write-section-header (cbuf fields)
  "Write a 40-byte IMAGE_SECTION_HEADER to CBUF using FIELDS plist.
FIELDS:
  :name               Section name string (max 8 bytes; longer names require
                      string table — v1 truncates at 8).
  :virtual-size       Misc/VirtualSize (= 0 for object files).
  :virtual-address    VirtualAddress (= 0 for object files).
  :raw-data-size      SizeOfRawData.
  :raw-data-ptr       PointerToRawData.
  :reloc-ptr          PointerToRelocations.
  :num-relocs         NumberOfRelocations.
  :characteristics    Section flags (= combined SCN_* constants).
Returns 40."
  (let ((name     (or (plist-get fields :name)             ""))
        (virtsize (or (plist-get fields :virtual-size)      0))
        (virtaddr (or (plist-get fields :virtual-address)   0))
        (rawsz    (or (plist-get fields :raw-data-size)     0))
        (rawptr   (or (plist-get fields :raw-data-ptr)      0))
        (relocptr (or (plist-get fields :reloc-ptr)         0))
        (nrelocs  (or (plist-get fields :num-relocs)        0))
        (chars    (or (plist-get fields :characteristics)   0)))
    (nelisp-pe--write-fixed-string cbuf name 8)
    (nelisp-pe--write-le32 cbuf virtsize)   ; Misc.VirtualSize
    (nelisp-pe--write-le32 cbuf virtaddr)   ; VirtualAddress
    (nelisp-pe--write-le32 cbuf rawsz)      ; SizeOfRawData
    (nelisp-pe--write-le32 cbuf rawptr)     ; PointerToRawData
    (nelisp-pe--write-le32 cbuf relocptr)   ; PointerToRelocations
    (nelisp-pe--write-le32 cbuf 0)          ; PointerToLinenumbers (= 0)
    (nelisp-pe--write-le16 cbuf nrelocs)    ; NumberOfRelocations
    (nelisp-pe--write-le16 cbuf 0)          ; NumberOfLinenumbers (= 0)
    (nelisp-pe--write-le32 cbuf chars)      ; Characteristics
    40))

;; ---- IMAGE_SYMBOL writer (18 bytes) ----

(defun nelisp-pe--write-symbol (cbuf fields)
  "Write an 18-byte IMAGE_SYMBOL to CBUF using FIELDS plist.

COFF symbols have a special 8-byte Name field: if the name is <=8
bytes it is stored inline (NUL-padded); otherwise the first 4 bytes
are zero and the next 4 are the string-table offset.  The caller must
pre-compute the Name representation via `nelisp-pe--symbol-name-bytes'.

FIELDS:
  :name-bytes     8-byte unibyte string (= inline or string-table offset form).
  :value          Symbol Value (= section offset for defined symbols).
  :section-num    SectionNumber (1-based, or 0 = UNDEFINED, -1 = ABSOLUTE).
  :type           Type field (= 0x20 for function, 0 otherwise).
  :storage-class  StorageClass (= 2 = EXTERNAL, 3 = STATIC).
  :num-aux        NumberOfAuxSymbols (default 0).
Returns 18."
  (let ((name-bytes    (or (plist-get fields :name-bytes)
                           (make-string 8 0)))
        (value         (or (plist-get fields :value)         0))
        (section-num   (or (plist-get fields :section-num)   0))
        (type          (or (plist-get fields :type)          0))
        (storage-class (or (plist-get fields :storage-class) nelisp-pe--sym-class-external))
        (num-aux       (or (plist-get fields :num-aux)       0)))
    (nelisp-pe--write-bytes cbuf name-bytes)             ; Name[8]
    (nelisp-pe--write-le32 cbuf value)                   ; Value
    (nelisp-pe--write-le16 cbuf (logand section-num #xFFFF)) ; SectionNumber
    (nelisp-pe--write-le16 cbuf type)                    ; Type
    (nelisp-pe--write-u8   cbuf storage-class)           ; StorageClass
    (nelisp-pe--write-u8   cbuf num-aux)                 ; NumberOfAuxSymbols
    18))

(defun nelisp-pe--symbol-name-bytes (name strtab)
  "Return the 8-byte Name field for a COFF symbol with NAME.
If NAME is 8 bytes or fewer, it is stored inline (NUL-padded to 8).
Otherwise the first 4 bytes are zero and the next 4 are the
little-endian string-table offset returned by
`nelisp-pe--strtab-add'.  STRTAB is the string-table accumulator
(= updated in place when the long-name path is taken)."
  (let* ((encoded (encode-coding-string name 'utf-8 t))
         (len     (length encoded)))
    (if (<= len 8)
        ;; Inline: NUL-pad to exactly 8 bytes.
        (concat encoded (make-string (- 8 len) 0))
      ;; Long name: zero[4] + strtab-offset[4].
      (let* ((offset (nelisp-pe--strtab-add strtab name))
             (off-bytes
              (unibyte-string (logand offset #xff)
                              (logand (ash offset -8) #xff)
                              (logand (ash offset -16) #xff)
                              (logand (ash offset -24) #xff))))
        (concat (make-string 4 0) off-bytes)))))

;; ---- IMAGE_RELOCATION writer (10 bytes) ----

(defun nelisp-pe--write-relocation (cbuf fields)
  "Write a 10-byte IMAGE_RELOCATION to CBUF using FIELDS plist.
FIELDS:
  :virt-addr    VirtualAddress (= relocation target offset in section).
  :sym-index    SymbolTableIndex (= 0-based index into symbol table).
  :type         Type (= IMAGE_REL_AMD64_* constant).
Returns 10."
  (let ((va    (or (plist-get fields :virt-addr)  0))
        (sidx  (or (plist-get fields :sym-index)  0))
        (type  (or (plist-get fields :type)       nelisp-pe--rel-rel32)))
    (nelisp-pe--write-le32 cbuf va)
    (nelisp-pe--write-le32 cbuf sidx)
    (nelisp-pe--write-le16 cbuf type)
    10))

;; ---- align-up helper ----

(defun nelisp-pe--align-up (n align)
  "Return N rounded up to the next multiple of ALIGN (ALIGN must be > 0)."
  (if (or (zerop align) (= align 1))
      n
    (* align (/ (+ n align -1) align))))

;; ---- symbol bind / type helpers ----

(defun nelisp-pe--sym-storage-class (bind)
  "Return COFF StorageClass for BIND keyword.
`global' -> IMAGE_SYM_CLASS_EXTERNAL (2).
`local'  -> IMAGE_SYM_CLASS_STATIC   (3).
`weak'   -> IMAGE_SYM_CLASS_WEAK_EXTERNAL (105) — v1 falls back to EXTERNAL."
  (cond
   ((eq bind 'global) nelisp-pe--sym-class-external)
   ((eq bind 'local)  nelisp-pe--sym-class-static)
   ;; weak: COFF WEAK_EXTERNAL requires an aux record (= complex); fall
   ;; back to EXTERNAL so the linker sees it as a regular global.
   ((eq bind 'weak)   nelisp-pe--sym-class-external)
   (t nelisp-pe--sym-class-external)))

(defun nelisp-pe--sym-type (type)
  "Return COFF Type field for TYPE keyword.
`func'  -> 0x0020 (DT_FUNCTION in high byte).
Other / nil -> 0."
  (cond
   ((eq type 'func)   nelisp-pe--sym-dtype-function)
   (t 0)))

;; ---- top-level orchestrator ----

(defun nelisp-pe--build-object (plist)
  "Build a PE32+/COFF relocatable object from PLIST.
Returns a unibyte string of the complete .obj file contents.

PLIST keys (= same shape as `nelisp-elf--build-rel' for the ET_REL case):
  :text      unibyte instruction bytes (required).
  :rodata    unibyte read-only data bytes (optional; emitted as .rdata).
  :data      unibyte read-write data bytes (optional; emitted as .data).
  :symbols   list of symbol plists — each with :name :value :size
             :section (:text / :rodata / :data / :undef)
             :bind (:global / :local / :weak)
             :type (:func / :notype / :object / nil)
  :relocs    list of relocation plists — each with :offset :symbol :type
             :addend (= addend is IGNORED in COFF; embedded in instruction).
  :machine   must be `x86_64' (default; only value supported in v1).

Section numbers (1-based, per COFF spec §4):
  1 = .text
  2 = .rdata  (when :rodata is non-empty)
  3 = .data   (when :data is non-empty)"
  (let* ((text    (or (plist-get plist :text)
                      (error "nelisp-pe: :text is required")))
         (rodata  (plist-get plist :rodata))
         (data    (plist-get plist :data))
         (symbols (plist-get plist :symbols))
         (relocs  (plist-get plist :relocs))
         (machine-arg (or (plist-get plist :machine) 'x86_64))
         (_machine-code
          (cond
           ((or (eq machine-arg 'x86_64) (eq machine-arg 'x86-64))
            nelisp-pe--machine-amd64)
           (t (error "nelisp-pe: unsupported :machine %S (only x86_64 in v1)"
                     machine-arg))))
         (have-rodata (and rodata (> (length rodata) 0)))
         (have-data   (and data   (> (length data)   0)))
         (have-relocs (and relocs (> (length relocs)  0)))

         ;; Section numbers (1-based; absent sections = nil).
         (text-sn   1)
         (rdata-sn  (and have-rodata 2))
         (data-sn   (and have-data   (if have-rodata 3 2)))
         (num-sections (+ 1
                          (if have-rodata 1 0)
                          (if have-data   1 0)))

         ;; Build symbol table and string table first to know sizes.
         (strtab (nelisp-pe--strtab-make))

         ;; Assign a 0-based symbol-table index to every symbol so
         ;; relocations can reference them by index.
         ;; Symbol layout:
         ;;   - Section symbols for each non-null section (STATIC, section-local).
         ;;   - User-supplied symbols in declaration order.
         (section-sym-count num-sections)
         ;; Build ordered symbol list (= section symbols first, then user symbols,
         ;; locals before globals within user symbols — COFF requires locals first
         ;; so the loader can find the first non-local via AuxiliaryCount).
         (user-locals  nil)
         (user-globals nil)
         (_classify
          (dolist (sym symbols)
            (let ((bind (or (plist-get sym :bind) 'local)))
              (if (eq bind 'local)
                  (push sym user-locals)
                (push sym user-globals)))))
         (ordered-user-symbols (append (nreverse user-locals)
                                       (nreverse user-globals)))
         ;; Total symbol count = section symbols + user symbols.
         (total-sym-count (+ section-sym-count (length symbols)))

         ;; Pre-compute name bytes for all user symbols (may extend strtab).
         (sym-name-bytes-map
          (mapcar (lambda (sym)
                    (cons (plist-get sym :name)
                          (nelisp-pe--symbol-name-bytes
                           (plist-get sym :name) strtab)))
                  symbols))

         ;; Text relocs: all relocs whose :section is 'text (or absent).
         ;; v1: we map all relocs to .text (= Phase 47 only emits .text code).
         (text-relocs (when have-relocs relocs))
         (num-text-relocs (if text-relocs (length text-relocs) 0))

         ;; Build a 0-based index → symbol lookup for reloc resolution.
         ;; Index 0..section-sym-count-1 are section symbols.
         ;; Index section-sym-count.. are user symbols in ordered-user-symbols order.
         (sym-index-map
          (let ((acc nil)
                (i section-sym-count))
            (dolist (sym ordered-user-symbols)
              (push (cons (plist-get sym :name) i) acc)
              (setq i (1+ i)))
            acc))

         ;; File layout calculation.
         ;; +0                  IMAGE_FILE_HEADER        (20 bytes)
         ;; +20                 IMAGE_SECTION_HEADER[0]  .text
         ;; +60                 IMAGE_SECTION_HEADER[1]  .rdata (optional)
         ;; ...
         ;; +20+40*num-sections .text raw data
         ;; ...                 .rdata raw data (optional)
         ;; ...                 .data raw data  (optional)
         ;; ...                 IMAGE_RELOCATION[] for .text (optional)
         ;; ...                 IMAGE_RELOCATION[] for .rdata (optional)
         ;; ...                 IMAGE_SYMBOL[]
         ;; ...                 String table

         (section-headers-size (* num-sections nelisp-pe--section-header-size))
         (headers-end          (+ nelisp-pe--header-size section-headers-size))

         ;; Raw data offsets.
         (text-raw-ptr   headers-end)
         (text-raw-size  (length text))
         (rdata-raw-ptr  (and have-rodata (+ text-raw-ptr text-raw-size)))
         (rdata-raw-size (if have-rodata (length rodata) 0))
         (data-raw-ptr   (and have-data
                              (if have-rodata
                                  (+ rdata-raw-ptr rdata-raw-size)
                                (+ text-raw-ptr text-raw-size))))
         (data-raw-size  (if have-data (length data) 0))

         ;; Relocation tables follow raw data (no inter-section padding needed
         ;; per COFF spec; linker alignment is section Characteristics based).
         (after-raw-data (+ text-raw-ptr text-raw-size
                            rdata-raw-size
                            data-raw-size))
         (text-reloc-ptr (and (> num-text-relocs 0) after-raw-data))
         (after-relocs   (if (> num-text-relocs 0)
                             (+ after-raw-data (* num-text-relocs nelisp-pe--reloc-size))
                           after-raw-data))

         ;; Symbol table and string table.
         (symtab-ptr after-relocs)
         (symtab-size (* total-sym-count nelisp-pe--sym-size))

         ;; cbuf accumulator.
         (cbuf (nelisp-pe--make-buffer)))

    ;; ---- IMAGE_FILE_HEADER ----
    (nelisp-pe--write-file-header
     cbuf
     (list :machine       nelisp-pe--machine-amd64
           :num-sections  num-sections
           :timestamp     0
           :sym-table-ptr symtab-ptr
           :num-symbols   total-sym-count
           :characteristics 0))

    ;; ---- IMAGE_SECTION_HEADER for .text ----
    (nelisp-pe--write-section-header
     cbuf
     (list :name            ".text"
           :virtual-size    0
           :virtual-address 0
           :raw-data-size   text-raw-size
           :raw-data-ptr    text-raw-ptr
           :reloc-ptr       (or text-reloc-ptr 0)
           :num-relocs      num-text-relocs
           :characteristics nelisp-pe--scn-text-flags))

    ;; ---- IMAGE_SECTION_HEADER for .rdata (optional) ----
    (when have-rodata
      (nelisp-pe--write-section-header
       cbuf
       (list :name            ".rdata"
             :virtual-size    0
             :virtual-address 0
             :raw-data-size   rdata-raw-size
             :raw-data-ptr    rdata-raw-ptr
             :reloc-ptr       0
             :num-relocs      0
             :characteristics nelisp-pe--scn-rdata-flags)))

    ;; ---- IMAGE_SECTION_HEADER for .data (optional) ----
    (when have-data
      (nelisp-pe--write-section-header
       cbuf
       (list :name            ".data"
             :virtual-size    0
             :virtual-address 0
             :raw-data-size   data-raw-size
             :raw-data-ptr    data-raw-ptr
             :reloc-ptr       0
             :num-relocs      0
             :characteristics nelisp-pe--scn-data-flags)))

    ;; Verify we are at the expected raw-data start offset.
    (unless (= (nelisp-pe--buffer-length cbuf) headers-end)
      (error "nelisp-pe: headers size drift (at %d expected %d)"
             (nelisp-pe--buffer-length cbuf) headers-end))

    ;; ---- Raw section data ----
    (nelisp-pe--write-bytes cbuf text)
    (when have-rodata
      (nelisp-pe--write-bytes cbuf rodata))
    (when have-data
      (nelisp-pe--write-bytes cbuf data))

    ;; ---- IMAGE_RELOCATION[] for .text ----
    (when (> num-text-relocs 0)
      (unless (= (nelisp-pe--buffer-length cbuf) text-reloc-ptr)
        (error "nelisp-pe: reloc table offset drift (at %d expected %d)"
               (nelisp-pe--buffer-length cbuf) text-reloc-ptr))
      (dolist (rel text-relocs)
        (let* ((rsym  (plist-get rel :symbol))
               (rtype (nelisp-pe--reloc-type-code (plist-get rel :type)))
               (sidx  (or (cdr (assoc rsym sym-index-map))
                          (error "nelisp-pe: relocation references unknown symbol %S"
                                 rsym))))
          (nelisp-pe--write-relocation
           cbuf
           (list :virt-addr (or (plist-get rel :offset) 0)
                 :sym-index sidx
                 :type      rtype)))))

    ;; ---- Symbol table ----
    (unless (= (nelisp-pe--buffer-length cbuf) symtab-ptr)
      (error "nelisp-pe: symbol table offset drift (at %d expected %d)"
             (nelisp-pe--buffer-length cbuf) symtab-ptr))

    ;; Section symbols (one per section, STATIC / section-number).
    (nelisp-pe--write-symbol
     cbuf
     (list :name-bytes    (nelisp-pe--symbol-name-bytes ".text" strtab)
           :value         0
           :section-num   text-sn
           :type          0
           :storage-class nelisp-pe--sym-class-static
           :num-aux       0))
    (when have-rodata
      (nelisp-pe--write-symbol
       cbuf
       (list :name-bytes    (nelisp-pe--symbol-name-bytes ".rdata" strtab)
             :value         0
             :section-num   rdata-sn
             :type          0
             :storage-class nelisp-pe--sym-class-static
             :num-aux       0)))
    (when have-data
      (nelisp-pe--write-symbol
       cbuf
       (list :name-bytes    (nelisp-pe--symbol-name-bytes ".data" strtab)
             :value         0
             :section-num   data-sn
             :type          0
             :storage-class nelisp-pe--sym-class-static
             :num-aux       0)))

    ;; User-supplied symbols (locals first per COFF ABI, already ordered).
    (dolist (sym ordered-user-symbols)
      (let* ((nm         (plist-get sym :name))
             (bind       (or (plist-get sym :bind) 'local))
             (type       (or (plist-get sym :type) 'notype))
             (sect       (or (plist-get sym :section) 'text))
             (value      (or (plist-get sym :value) 0))
             (name-bytes (cdr (assoc nm sym-name-bytes-map)))
             (sn         (nelisp-pe--section-number
                          sect text-sn rdata-sn data-sn)))
        (nelisp-pe--write-symbol
         cbuf
         (list :name-bytes    name-bytes
               :value         value
               :section-num   sn
               :type          (nelisp-pe--sym-type type)
               :storage-class (nelisp-pe--sym-storage-class bind)
               :num-aux       0))))

    ;; ---- String table ----
    (nelisp-pe--write-bytes cbuf (nelisp-pe--strtab-bytes strtab))

    ;; Finalize.
    (ignore symtab-size) ; used for offset documentation only
    (nelisp-pe--buffer-bytes cbuf)))

;; ---- Doc 138 Stage 1: minimal PE32+ executable writer ------------

(defun nelisp-pe--write-dos-stub (cbuf pe-offset)
  "Write a minimal DOS header/stub to CBUF with e_lfanew = PE-OFFSET."
  (let ((start (nelisp-pe--buffer-length cbuf)))
    (nelisp-pe--write-bytes cbuf (unibyte-string #x4D #x5A)) ; MZ
    (nelisp-pe--write-pad cbuf (- #x3C 2))
    (nelisp-pe--write-le32 cbuf pe-offset)
    (nelisp-pe--write-pad cbuf (- nelisp-pe--dos-stub-size
                                  (- (nelisp-pe--buffer-length cbuf)
                                     start)))))

(defun nelisp-pe--write-pe32plus-optional-header (cbuf fields)
  "Write IMAGE_OPTIONAL_HEADER64 for a PE32+ executable.
FIELDS is a plist carrying the fixed image layout values."
  (let ((size-code        (plist-get fields :size-code))
        (size-init-data   (plist-get fields :size-init-data))
        (entry-rva        (plist-get fields :entry-rva))
        (base-code        (plist-get fields :base-code))
        (image-base       (plist-get fields :image-base))
        (size-image       (plist-get fields :size-image))
        (size-headers     (plist-get fields :size-headers))
        (import-rva       (plist-get fields :import-rva))
        (import-size      (plist-get fields :import-size)))
    ;; Standard fields.
    (nelisp-pe--write-le16 cbuf #x20B) ; PE32+
    (nelisp-pe--write-u8 cbuf 0)       ; MajorLinkerVersion
    (nelisp-pe--write-u8 cbuf 0)       ; MinorLinkerVersion
    (nelisp-pe--write-le32 cbuf size-code)
    (nelisp-pe--write-le32 cbuf size-init-data)
    (nelisp-pe--write-le32 cbuf 0)     ; SizeOfUninitializedData
    (nelisp-pe--write-le32 cbuf entry-rva)
    (nelisp-pe--write-le32 cbuf base-code)
    ;; Windows-specific fields.
    (nelisp-pe--write-le64 cbuf image-base)
    (nelisp-pe--write-le32 cbuf nelisp-pe--section-alignment)
    (nelisp-pe--write-le32 cbuf nelisp-pe--file-alignment)
    (nelisp-pe--write-le16 cbuf 6)     ; MajorOperatingSystemVersion
    (nelisp-pe--write-le16 cbuf 0)     ; MinorOperatingSystemVersion
    (nelisp-pe--write-le16 cbuf 0)     ; MajorImageVersion
    (nelisp-pe--write-le16 cbuf 0)     ; MinorImageVersion
    (nelisp-pe--write-le16 cbuf 6)     ; MajorSubsystemVersion
    (nelisp-pe--write-le16 cbuf 0)     ; MinorSubsystemVersion
    (nelisp-pe--write-le32 cbuf 0)     ; Win32VersionValue
    (nelisp-pe--write-le32 cbuf size-image)
    (nelisp-pe--write-le32 cbuf size-headers)
    (nelisp-pe--write-le32 cbuf 0)     ; CheckSum
    (nelisp-pe--write-le16 cbuf nelisp-pe--subsystem-console)
    (nelisp-pe--write-le16 cbuf 0)     ; DllCharacteristics
    (nelisp-pe--write-le64 cbuf #x100000) ; SizeOfStackReserve
    (nelisp-pe--write-le64 cbuf #x1000)   ; SizeOfStackCommit
    (nelisp-pe--write-le64 cbuf #x100000) ; SizeOfHeapReserve
    (nelisp-pe--write-le64 cbuf #x1000)   ; SizeOfHeapCommit
    (nelisp-pe--write-le32 cbuf 0)     ; LoaderFlags
    (nelisp-pe--write-le32 cbuf 16)    ; NumberOfRvaAndSizes
    ;; IMAGE_DATA_DIRECTORY[0] Export.
    (nelisp-pe--write-le32 cbuf 0)
    (nelisp-pe--write-le32 cbuf 0)
    ;; IMAGE_DATA_DIRECTORY[1] Import.
    (nelisp-pe--write-le32 cbuf import-rva)
    (nelisp-pe--write-le32 cbuf import-size)
    ;; IMAGE_DATA_DIRECTORY[2..15].
    (dotimes (_ 14)
      (nelisp-pe--write-le32 cbuf 0)
      (nelisp-pe--write-le32 cbuf 0))))

(defun nelisp-pe--build-kernel32-exitprocess-imports (rdata-rva)
  "Build .rdata import data for kernel32.dll!ExitProcess at RDATA-RVA.
Returns a plist (:bytes BYTES :import-rva RVA :import-size SIZE
:iat-rva RVA)."
  (let* ((desc-off 0)
         (null-desc-off 20)
         (ilt-off 40)
         (iat-off 56)
         (hint-name-off 72)
         (dll-name-off (+ hint-name-off 2 (length "ExitProcess") 1))
         (import-size (+ dll-name-off (length "KERNEL32.dll") 1))
         (hint-name-rva (+ rdata-rva hint-name-off))
         (dll-name-rva (+ rdata-rva dll-name-off))
         (ilt-rva (+ rdata-rva ilt-off))
         (iat-rva (+ rdata-rva iat-off))
         (cbuf (nelisp-pe--make-buffer)))
    (ignore null-desc-off)
    ;; IMAGE_IMPORT_DESCRIPTOR for KERNEL32.dll.
    (nelisp-pe--write-le32 cbuf ilt-rva)      ; OriginalFirstThunk
    (nelisp-pe--write-le32 cbuf 0)            ; TimeDateStamp
    (nelisp-pe--write-le32 cbuf 0)            ; ForwarderChain
    (nelisp-pe--write-le32 cbuf dll-name-rva) ; Name
    (nelisp-pe--write-le32 cbuf iat-rva)      ; FirstThunk
    ;; Null descriptor.
    (nelisp-pe--write-pad cbuf 20)
    ;; Import lookup table.
    (nelisp-pe--write-le64 cbuf hint-name-rva)
    (nelisp-pe--write-le64 cbuf 0)
    ;; Import address table.
    (nelisp-pe--write-le64 cbuf hint-name-rva)
    (nelisp-pe--write-le64 cbuf 0)
    ;; IMAGE_IMPORT_BY_NAME.
    (nelisp-pe--write-le16 cbuf 0)            ; Hint
    (nelisp-pe--write-bytes cbuf "ExitProcess")
    (nelisp-pe--write-u8 cbuf 0)
    ;; DLL name.
    (nelisp-pe--write-bytes cbuf "KERNEL32.dll")
    (nelisp-pe--write-u8 cbuf 0)
    (unless (= (nelisp-pe--buffer-length cbuf) import-size)
      (error "nelisp-pe: import blob size drift (at %d expected %d)"
             (nelisp-pe--buffer-length cbuf) import-size))
    (ignore desc-off)
    (list :bytes (nelisp-pe--buffer-bytes cbuf)
          :import-rva rdata-rva
          :import-size import-size
          :iat-rva iat-rva)))

(defun nelisp-pe--build-exitprocess-text (exit-code text-rva iat-rva)
  "Build x86_64 Win64 entry code calling ExitProcess(EXIT-CODE).
TEXT-RVA is the RVA of the .text section and IAT-RVA is the RVA of the
ExitProcess IAT slot."
  (let* ((call-off 9)
         (next-rva (+ text-rva call-off 6))
         (disp (- iat-rva next-rva))
         (cbuf (nelisp-pe--make-buffer)))
    (unless (and (integerp exit-code) (<= 0 exit-code #xFFFFFFFF))
      (error "nelisp-pe: exit code must fit u32: %S" exit-code))
    (unless (and (<= (- (ash 1 31)) disp) (< disp (ash 1 31)))
      (error "nelisp-pe: ExitProcess call displacement out of rel32 range: %S"
             disp))
    ;; Microsoft x64 ABI: reserve 32-byte shadow space plus alignment pad.
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xEC #x28)) ; sub rsp, 40
    (nelisp-pe--write-u8 cbuf #xB9) ; mov ecx, imm32
    (nelisp-pe--write-le32 cbuf exit-code)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xFF #x15)) ; call qword [rip+disp32]
    (nelisp-pe--write-le32-signed cbuf disp)
    (nelisp-pe--write-u8 cbuf #xCC) ; should not return
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-exitprocess-executable (exit-code)
  "Build a minimal PE32+ console executable that calls ExitProcess(EXIT-CODE)."
  (let* ((pe-offset nelisp-pe--dos-stub-size)
         (num-sections 2)
         (headers-raw
          (+ pe-offset 4 nelisp-pe--header-size
             nelisp-pe--optional-header-pe32plus-size
             (* num-sections nelisp-pe--section-header-size)))
         (size-headers (nelisp-pe--align-up headers-raw
                                            nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (rdata-rva (* 2 nelisp-pe--section-alignment))
         (imports0 (nelisp-pe--build-kernel32-exitprocess-imports rdata-rva))
         (text (nelisp-pe--build-exitprocess-text
                exit-code text-rva (plist-get imports0 :iat-rva)))
         (imports (plist-get imports0 :bytes))
         (text-raw-size (nelisp-pe--align-up (length text)
                                             nelisp-pe--file-alignment))
         (rdata-raw-size (nelisp-pe--align-up (length imports)
                                              nelisp-pe--file-alignment))
         (text-raw-ptr size-headers)
         (rdata-raw-ptr (+ text-raw-ptr text-raw-size))
         (size-image (nelisp-pe--align-up
                      (+ rdata-rva (length imports))
                      nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (unless (= (nelisp-pe--buffer-length cbuf) pe-offset)
      (error "nelisp-pe: DOS stub size drift"))
    ;; PE signature.
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    ;; IMAGE_FILE_HEADER.
    (nelisp-pe--write-le16 cbuf nelisp-pe--machine-amd64)
    (nelisp-pe--write-le16 cbuf num-sections)
    (nelisp-pe--write-le32 cbuf 0) ; TimeDateStamp
    (nelisp-pe--write-le32 cbuf 0) ; PointerToSymbolTable
    (nelisp-pe--write-le32 cbuf 0) ; NumberOfSymbols
    (nelisp-pe--write-le16 cbuf nelisp-pe--optional-header-pe32plus-size)
    (nelisp-pe--write-le16
     cbuf (logior nelisp-pe--file-executable-image
                  nelisp-pe--file-large-address-aware))
    ;; IMAGE_OPTIONAL_HEADER64.
    (nelisp-pe--write-pe32plus-optional-header
     cbuf
     (list :size-code text-raw-size
           :size-init-data rdata-raw-size
           :entry-rva text-rva
           :base-code text-rva
           :image-base nelisp-pe--image-base-x86-64
           :size-image size-image
           :size-headers size-headers
           :import-rva (plist-get imports0 :import-rva)
           :import-size (plist-get imports0 :import-size)))
    ;; IMAGE_SECTION_HEADER[.text, .rdata].
    (nelisp-pe--write-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :reloc-ptr 0
           :num-relocs 0
           :characteristics nelisp-pe--exe-text-flags))
    (nelisp-pe--write-section-header
     cbuf
     (list :name ".rdata"
           :virtual-size (length imports)
           :virtual-address rdata-rva
           :raw-data-size rdata-raw-size
           :raw-data-ptr rdata-raw-ptr
           :reloc-ptr 0
           :num-relocs 0
           :characteristics nelisp-pe--exe-rdata-flags))
    (unless (<= (nelisp-pe--buffer-length cbuf) size-headers)
      (error "nelisp-pe: headers exceed SizeOfHeaders"))
    (nelisp-pe--write-pad cbuf (- size-headers (nelisp-pe--buffer-length cbuf)))
    (unless (= (nelisp-pe--buffer-length cbuf) text-raw-ptr)
      (error "nelisp-pe: .text raw offset drift"))
    (nelisp-pe--write-bytes cbuf text)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text)))
    (unless (= (nelisp-pe--buffer-length cbuf) rdata-raw-ptr)
      (error "nelisp-pe: .rdata raw offset drift"))
    (nelisp-pe--write-bytes cbuf imports)
    (nelisp-pe--write-pad cbuf (- rdata-raw-size (length imports)))
    (nelisp-pe--buffer-bytes cbuf)))

;;;###autoload
(defun nelisp-pe-write-binary (file-path build-plist)
  "Emit a PE32+/COFF relocatable object file to FILE-PATH.

BUILD-PLIST is a property list with the same shape as the ELF
writer's ET_REL input (see `nelisp-elf-write-binary' :e-type `rel'):

  :text      unibyte instruction bytes (required).
  :rodata    unibyte read-only data bytes (optional → .rdata section).
  :data      unibyte read-write data bytes (optional → .data section).
  :symbols   list of symbol plists, each with keys:
               :name     symbol name string (required)
               :value    section-relative byte offset (default 0)
               :size     size in bytes (default 0; informational)
               :section  section keyword: `text' / `rodata' / `data' / `undef'
               :bind     `global' / `local' / `weak' (default `local')
               :type     `func' / `object' / `notype' / nil (default nil)
  :relocs    list of relocation plists, each with keys:
               :offset   relocation target offset inside the section
               :symbol   target symbol name string
               :type     `pc32' / `plt32' / `abs32' / `abs64' (or raw int)
               :addend   (IGNORED in COFF — addend lives in the instruction)
  :machine   `x86_64' (default; only value supported in v1)

Writes the resulting .obj bytes to FILE-PATH using `no-conversion'
so raw binary content is preserved.  Returns FILE-PATH."
  (let ((bytes (nelisp-pe--build-object build-plist))
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil file-path nil 'silent)
    file-path))

;;;###autoload
(defun nelisp-pe-write-exitprocess-executable (file-path exit-code)
  "Emit a minimal PE32+ console EXE that calls ExitProcess(EXIT-CODE).
This is Doc 138 Stage 1: prove Windows native execution through a real
PE import table, without an external linker or CRT."
  (let ((bytes (nelisp-pe--build-exitprocess-executable exit-code))
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil file-path nil 'silent)
    file-path))

(provide 'nelisp-pe-write)

;;; nelisp-pe-write.el ends here
