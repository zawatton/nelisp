;;; nelisp-pe-write.el --- PE32+/COFF object and EXE writer  -*- lexical-binding: t; -*-

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
;;
;; Doc 138 Stage 1 adds a minimal PE32+ executable writer for Windows
;; standalone bring-up: it emits a console EXE whose import directory resolves
;; `KERNEL32.dll!ExitProcess' and whose entry point exits with code 42.
;; Stage 2 extends the same import-table path to `VirtualAlloc' so the Windows
;; arena-allocation dependency can be tested without a linker.  Stage 3 writes
;; the allocated arena base/cursor/end triple into a PE .data section.  Stage 4
;; adds a HANDLE-based stdout smoke via `GetStdHandle' + `WriteFile'.  This is
;; intentionally small.  Stage 42 adds `GetCommandLineW' so CRT-free startup
;; can prove command-line discovery through the PE import table.  Stage 43
;; broadens the import writer to more than one DLL and adds a `WS2_32.dll'
;; `WSAStartup' smoke for the standalone Winsock import path.  Stage 44 adds
;; `Shell32.dll!CommandLineToArgvW' for CRT-free argv materialization.  Stage
;; 45 adds a `VirtualProtect' + `VirtualFree' memory-lifecycle smoke.
;; Stage 56 adds a `CreateProcessW' smoke that starts `cmd.exe /c exit 42',
;; waits for the child, reads its exit code, and closes the process HANDLE.
;; Stage 57 adds a `CreateThread' smoke that starts an in-image thread entry,
;; joins it, reads its exit code, and closes the thread HANDLE.
;; Stage 138 adds a nonblocking `ReadFile' stdin smoke with a zero-byte read.
;; Stage 139 adds a `CreateFileW' smoke that writes, closes, and deletes a file.
;; Stage 140 adds a `SetFilePointerEx' smoke for HANDLE-backed lseek wiring.
;; Stage 141 adds a `GetFileType' smoke for HANDLE-backed fstat wiring.
;; Stage 142 adds `GetFileInformationByHandle' for rich fstat metadata wiring.
;; Production standalone wiring still needs the wider OS import surface.

;;; Code:

;; ---- COFF format constants (= Microsoft PE/COFF spec §3) ----

(defconst nelisp-pe--machine-amd64 #x8664
  "IMAGE_FILE_MACHINE_AMD64 (= x86_64 Windows 64-bit).")

(defconst nelisp-pe--optional-header-pe32-plus-size 240
  "sizeof(IMAGE_OPTIONAL_HEADER64) with 16 data directories.")

(defconst nelisp-pe--dos-header-size #x80
  "DOS stub size used by the minimal PE32+ EXE path.")

(defconst nelisp-pe--file-alignment #x200
  "PE FileAlignment used by the minimal PE32+ EXE path.")

(defconst nelisp-pe--section-alignment #x1000
  "PE SectionAlignment used by the minimal PE32+ EXE path.")

(defconst nelisp-pe--image-base #x140000000
  "Default PE32+ ImageBase for x86_64 Windows EXEs.")

(defconst nelisp-pe--characteristic-executable #x0002
  "IMAGE_FILE_EXECUTABLE_IMAGE.")

(defconst nelisp-pe--characteristic-large-address-aware #x0020
  "IMAGE_FILE_LARGE_ADDRESS_AWARE.")

(defconst nelisp-pe--subsystem-windows-cui 3
  "IMAGE_SUBSYSTEM_WINDOWS_CUI.")

(defconst nelisp-pe--dll-characteristics-nx-compat #x0100
  "IMAGE_DLLCHARACTERISTICS_NX_COMPAT.")

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

(defconst nelisp-pe--scn-exe-text-flags
  (logior nelisp-pe--scn-cnt-code
          nelisp-pe--scn-mem-execute
          nelisp-pe--scn-mem-read)
  "Section Characteristics for a PE image .text section.")

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

(defconst nelisp-pe--scn-idata-flags
  (logior nelisp-pe--scn-cnt-init-data
          nelisp-pe--scn-mem-read
          nelisp-pe--scn-mem-write)
  "Section Characteristics for a PE import-data section.")

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

(defun nelisp-pe--utf16le-z-bytes (s)
  "Return S encoded as UTF-16LE with a trailing NUL WCHAR.
This helper is intentionally limited to BMP code points, which is enough for
the ASCII Windows smoke command lines."
  (let ((cbuf (nelisp-pe--make-buffer)))
    (dotimes (idx (length s))
      (nelisp-pe--write-le16 cbuf (aref s idx)))
    (nelisp-pe--write-le16 cbuf 0)
    (nelisp-pe--buffer-bytes cbuf)))

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

;;; ---- Doc 138 Stage 1 PE32+ EXE writer ----

(defun nelisp-pe--write-dos-stub (cbuf pe-offset)
  "Write a minimal DOS header/stub to CBUF with e_lfanew = PE-OFFSET."
  (nelisp-pe--write-bytes cbuf (unibyte-string #x4d #x5a)) ; MZ
  (nelisp-pe--write-pad cbuf 58)
  (nelisp-pe--write-le32 cbuf pe-offset) ; IMAGE_DOS_HEADER.e_lfanew
  (nelisp-pe--write-pad cbuf (- pe-offset (nelisp-pe--buffer-length cbuf))))

(defun nelisp-pe--write-pe-file-header (cbuf fields)
  "Write IMAGE_FILE_HEADER for a PE image using FIELDS plist."
  (nelisp-pe--write-le16 cbuf (or (plist-get fields :machine)
                                  nelisp-pe--machine-amd64))
  (nelisp-pe--write-le16 cbuf (or (plist-get fields :num-sections) 0))
  (nelisp-pe--write-le32 cbuf (or (plist-get fields :timestamp) 0))
  (nelisp-pe--write-le32 cbuf 0) ; PointerToSymbolTable
  (nelisp-pe--write-le32 cbuf 0) ; NumberOfSymbols
  (nelisp-pe--write-le16 cbuf nelisp-pe--optional-header-pe32-plus-size)
  (nelisp-pe--write-le16 cbuf
                         (or (plist-get fields :characteristics)
                             (logior nelisp-pe--characteristic-executable
                                     nelisp-pe--characteristic-large-address-aware))))

(defun nelisp-pe--write-data-directory-table (cbuf import-rva import-size
                                                   iat-rva iat-size)
  "Write the PE32+ 16-entry data-directory table."
  (dotimes (i 16)
    (cond
     ;; IMAGE_DIRECTORY_ENTRY_IMPORT
     ((= i 1)
      (nelisp-pe--write-le32 cbuf import-rva)
      (nelisp-pe--write-le32 cbuf import-size))
     ;; IMAGE_DIRECTORY_ENTRY_IAT
     ((= i 12)
      (nelisp-pe--write-le32 cbuf iat-rva)
      (nelisp-pe--write-le32 cbuf iat-size))
     (t
      (nelisp-pe--write-le32 cbuf 0)
      (nelisp-pe--write-le32 cbuf 0)))))

(defun nelisp-pe--write-optional-header64 (cbuf fields)
  "Write IMAGE_OPTIONAL_HEADER64 for a minimal PE32+ console EXE."
  (let ((section-alignment (or (plist-get fields :section-alignment)
                               nelisp-pe--section-alignment))
        (file-alignment (or (plist-get fields :file-alignment)
                            nelisp-pe--file-alignment))
        (image-base (or (plist-get fields :image-base)
                        nelisp-pe--image-base))
        (entry-rva (or (plist-get fields :entry-rva) 0))
        (text-rva (or (plist-get fields :text-rva) 0))
        (size-of-code (or (plist-get fields :size-of-code) 0))
        (size-of-initialized-data
         (or (plist-get fields :size-of-initialized-data) 0))
        (size-of-image (or (plist-get fields :size-of-image) 0))
        (size-of-headers (or (plist-get fields :size-of-headers) 0))
        (import-rva (or (plist-get fields :import-rva) 0))
        (import-size (or (plist-get fields :import-size) 0))
        (iat-rva (or (plist-get fields :iat-rva) 0))
        (iat-size (or (plist-get fields :iat-size) 0)))
    (nelisp-pe--write-le16 cbuf #x020b) ; PE32+
    (nelisp-pe--write-u8 cbuf 0)        ; MajorLinkerVersion
    (nelisp-pe--write-u8 cbuf 1)        ; MinorLinkerVersion
    (nelisp-pe--write-le32 cbuf size-of-code)
    (nelisp-pe--write-le32 cbuf size-of-initialized-data)
    (nelisp-pe--write-le32 cbuf 0)      ; SizeOfUninitializedData
    (nelisp-pe--write-le32 cbuf entry-rva)
    (nelisp-pe--write-le32 cbuf text-rva)
    (nelisp-pe--write-le64 cbuf image-base)
    (nelisp-pe--write-le32 cbuf section-alignment)
    (nelisp-pe--write-le32 cbuf file-alignment)
    (nelisp-pe--write-le16 cbuf 6)      ; MajorOperatingSystemVersion
    (nelisp-pe--write-le16 cbuf 0)
    (nelisp-pe--write-le16 cbuf 0)      ; MajorImageVersion
    (nelisp-pe--write-le16 cbuf 0)
    (nelisp-pe--write-le16 cbuf 6)      ; MajorSubsystemVersion
    (nelisp-pe--write-le16 cbuf 0)
    (nelisp-pe--write-le32 cbuf 0)      ; Win32VersionValue
    (nelisp-pe--write-le32 cbuf size-of-image)
    (nelisp-pe--write-le32 cbuf size-of-headers)
    (nelisp-pe--write-le32 cbuf 0)      ; CheckSum
    (nelisp-pe--write-le16 cbuf nelisp-pe--subsystem-windows-cui)
    (nelisp-pe--write-le16 cbuf nelisp-pe--dll-characteristics-nx-compat)
    (nelisp-pe--write-le64 cbuf #x100000) ; SizeOfStackReserve
    (nelisp-pe--write-le64 cbuf #x1000)   ; SizeOfStackCommit
    (nelisp-pe--write-le64 cbuf #x100000) ; SizeOfHeapReserve
    (nelisp-pe--write-le64 cbuf #x1000)   ; SizeOfHeapCommit
    (nelisp-pe--write-le32 cbuf 0)        ; LoaderFlags
    (nelisp-pe--write-le32 cbuf 16)       ; NumberOfRvaAndSizes
    (nelisp-pe--write-data-directory-table
     cbuf import-rva import-size iat-rva iat-size)))

(defun nelisp-pe--write-pe-section-header (cbuf fields)
  "Write an IMAGE_SECTION_HEADER for a PE image."
  (nelisp-pe--write-section-header
   cbuf
   (list :name (plist-get fields :name)
         :virtual-size (or (plist-get fields :virtual-size) 0)
         :virtual-address (or (plist-get fields :virtual-address) 0)
         :raw-data-size (or (plist-get fields :raw-data-size) 0)
         :raw-data-ptr (or (plist-get fields :raw-data-ptr) 0)
         :reloc-ptr 0
         :num-relocs 0
         :characteristics (or (plist-get fields :characteristics) 0))))

(defun nelisp-pe--build-idata (idata-rva imports)
  "Build .idata bytes for IMPORTS.
IMPORTS is a list of (DLL-NAME . FUNCTION-NAMES) entries.  Returns a plist with
:bytes, :import-rva, :import-size, :iat-rva and :iat-size.  It also returns
:iat-rva-alist mapping function names to their IAT slot RVAs.  All RVA values
are relative to the image base."
  (let* ((dll-count (length imports))
         (descriptor-size (* 20 (1+ dll-count)))
         (records nil)
         (cursor descriptor-size)
         (iat-start nil)
         (iat-size 0)
         (iat-rva-alist nil)
         (cbuf (nelisp-pe--make-buffer)))
    (dolist (import imports)
      (let* ((dll-name (car import))
             (function-names (cdr import))
             (func-count (length function-names))
             (ilt-off cursor)
             (ilt-rva (+ idata-rva ilt-off))
             (ilt-size (* 8 (1+ func-count))))
        (push (list :dll-name dll-name
                    :function-names function-names
                    :ilt-rva ilt-rva
                    :ilt-size ilt-size)
              records)
        (setq cursor (+ cursor ilt-size))))
    (setq records (nreverse records))
    (setq iat-start cursor)
    (let ((updated nil))
      (dolist (record records)
        (let* ((function-names (plist-get record :function-names))
               (func-count (length function-names))
               (iat-off cursor)
               (iat-rva (+ idata-rva iat-off))
               (this-iat-size (* 8 (1+ func-count))))
          (push (append record
                        (list :iat-rva iat-rva
                              :iat-size this-iat-size))
                updated)
          (setq cursor (+ cursor this-iat-size))
          (setq iat-size (+ iat-size this-iat-size))))
      (setq records (nreverse updated)))
    (let ((updated nil))
      (dolist (record records)
        (let ((hint-entries nil)
              (slot 0))
          (dolist (func-name (plist-get record :function-names))
            (let* ((entry-bytes (concat (unibyte-string 0 0)
                                        (encode-coding-string func-name 'utf-8 t)
                                        (unibyte-string 0)))
                   (entry-rva (+ idata-rva cursor)))
              (push (list :name func-name :rva entry-rva :bytes entry-bytes)
                    hint-entries)
              (push (cons func-name
                          (+ (plist-get record :iat-rva) (* slot 8)))
                    iat-rva-alist)
              (setq cursor (+ cursor (length entry-bytes)))
              (setq slot (1+ slot))))
          (push (append record (list :hint-entries (nreverse hint-entries)))
                updated)))
      (setq records (nreverse updated))
      (setq iat-rva-alist (nreverse iat-rva-alist)))
    (let ((updated nil))
      (dolist (record records)
        (let ((dll-name-rva (+ idata-rva cursor)))
          (push (append record (list :dll-name-rva dll-name-rva))
                updated)
          (setq cursor (+ cursor
                          (length (encode-coding-string
                                   (plist-get record :dll-name) 'utf-8 t))
                          1))))
      (setq records (nreverse updated)))
    ;; IMAGE_IMPORT_DESCRIPTOR entries followed by a null descriptor.
    (dolist (record records)
      (nelisp-pe--write-le32 cbuf (plist-get record :ilt-rva))
      (nelisp-pe--write-le32 cbuf 0)
      (nelisp-pe--write-le32 cbuf 0)
      (nelisp-pe--write-le32 cbuf (plist-get record :dll-name-rva))
      (nelisp-pe--write-le32 cbuf (plist-get record :iat-rva)))
    (nelisp-pe--write-pad cbuf 20)
    ;; Import Lookup Tables.
    (dolist (record records)
      (dolist (entry (plist-get record :hint-entries))
        (nelisp-pe--write-le64 cbuf (plist-get entry :rva)))
      (nelisp-pe--write-le64 cbuf 0))
    ;; Import Address Tables.
    (dolist (record records)
      (dolist (entry (plist-get record :hint-entries))
        (nelisp-pe--write-le64 cbuf (plist-get entry :rva)))
      (nelisp-pe--write-le64 cbuf 0))
    ;; IMAGE_IMPORT_BY_NAME entries + DLL names.
    (dolist (record records)
      (dolist (entry (plist-get record :hint-entries))
        (nelisp-pe--write-bytes cbuf (plist-get entry :bytes))))
    (dolist (record records)
      (nelisp-pe--write-bytes
       cbuf (encode-coding-string (plist-get record :dll-name) 'utf-8 t))
      (nelisp-pe--write-u8 cbuf 0))
    (list :bytes (nelisp-pe--buffer-bytes cbuf)
          :import-rva idata-rva
          :import-size descriptor-size
          :iat-rva (+ idata-rva iat-start)
          :iat-size iat-size
          :iat-rva-alist iat-rva-alist)))

(defun nelisp-pe--build-kernel32-idata (idata-rva function-names)
  "Build .idata bytes for KERNEL32.dll imports named by FUNCTION-NAMES.
Returns a plist with :bytes, :import-rva, :import-size, :iat-rva and
:iat-size.  It also returns :iat-rva-alist mapping function names to their
IAT slot RVAs.  All RVA values are relative to the image base."
  (nelisp-pe--build-idata idata-rva
                          (list (cons "KERNEL32.dll" function-names))))

(defun nelisp-pe--build-exitprocess-idata (idata-rva)
  "Build .idata bytes for one import: KERNEL32.dll!ExitProcess."
  (nelisp-pe--build-kernel32-idata idata-rva (list "ExitProcess")))

(defun nelisp-pe--minimal-exitprocess-text (exit-code text-rva iat-rva)
  "Return x86_64 entry bytes that call ExitProcess(EXIT-CODE) via IAT-RVA."
  (let* ((call-off 9) ; sub rsp,40 (4) + mov ecx,imm32 (5)
         (call-len 6)
         (next-rva (+ text-rva call-off call-len))
         (disp (- iat-rva next-rva))
         (cbuf (nelisp-pe--make-buffer)))
    ;; Win64 ABI: 32-byte shadow space plus 8 bytes to preserve call alignment.
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x28))
    (nelisp-pe--write-u8 cbuf #xb9) ; mov ecx, imm32
    (nelisp-pe--write-le32 cbuf (logand exit-code #xffffffff))
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15)) ; call [rip+disp32]
    (nelisp-pe--write-le32-signed cbuf disp)
    (nelisp-pe--write-u8 cbuf #xcc) ; should not return
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--minimal-virtualalloc-text (text-rva exit-iat-rva virtualalloc-iat-rva)
  "Return x86_64 entry bytes that call VirtualAlloc, then ExitProcess.
The generated code exits 42 when VirtualAlloc(NULL, 4096,
MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE) succeeds and exits 1 otherwise."
  (let* ((virtualalloc-call-off 23)
         (success-exit-call-off 39)
         (fail-exit-call-off 50)
         (call-len 6)
         (virtualalloc-disp (- virtualalloc-iat-rva
                               (+ text-rva virtualalloc-call-off call-len)))
         (success-exit-disp (- exit-iat-rva
                               (+ text-rva success-exit-call-off call-len)))
         (fail-exit-disp (- exit-iat-rva
                            (+ text-rva fail-exit-call-off call-len)))
         (cbuf (nelisp-pe--make-buffer)))
    ;; Win64 ABI: 32-byte shadow space plus 8 bytes for stack alignment.
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x28))
    (nelisp-pe--write-bytes cbuf (unibyte-string #x31 #xc9)) ; xor ecx, ecx
    (nelisp-pe--write-u8 cbuf #xba) ; mov edx, 4096
    (nelisp-pe--write-le32 cbuf #x1000)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x41 #xb8)) ; mov r8d, 0x3000
    (nelisp-pe--write-le32 cbuf #x3000)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x41 #xb9)) ; mov r9d, 4
    (nelisp-pe--write-le32 cbuf #x4)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15)) ; call [rip+disp32]
    (nelisp-pe--write-le32-signed cbuf virtualalloc-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x85 #xc0)) ; test rax,rax
    (nelisp-pe--write-bytes cbuf (unibyte-string #x74 #x0b)) ; jz fail
    (nelisp-pe--write-u8 cbuf #xb9) ; mov ecx, 42
    (nelisp-pe--write-le32 cbuf 42)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf success-exit-disp)
    (nelisp-pe--write-u8 cbuf #xb9) ; fail: mov ecx, 1
    (nelisp-pe--write-le32 cbuf 1)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf fail-exit-disp)
    (nelisp-pe--write-u8 cbuf #xcc)
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--minimal-virtualprotect-free-text
    (text-rva exit-iat-rva virtualalloc-iat-rva
              virtualprotect-iat-rva virtualfree-iat-rva data-rva)
  "Return x86_64 bytes for VirtualAlloc, VirtualProtect, and VirtualFree.
The generated code exits 42 when allocation, protection change, and release
all succeed.  On VirtualProtect failure it releases the allocation before
exiting 1."
  (let* ((virtualalloc-call-off 23)
         (fail-off 117)
         (alloc-fail-jz-next-off 34)
         (protect-fail-jz-next-off 68)
         (free-fail-jz-next-off 89)
         (lea-old-protect-off 51)
         (virtualprotect-call-off 58)
         (free-success-call-off 79)
         (success-exit-call-off 94)
         (free-fail-off 100)
         (free-fail-call-off 111)
         (fail-exit-call-off 122)
         (call-len 6)
         (lea-len 7)
         (virtualalloc-disp (- virtualalloc-iat-rva
                               (+ text-rva virtualalloc-call-off call-len)))
         (old-protect-disp (- data-rva
                              (+ text-rva lea-old-protect-off lea-len)))
         (virtualprotect-disp
          (- virtualprotect-iat-rva
             (+ text-rva virtualprotect-call-off call-len)))
         (free-success-disp
          (- virtualfree-iat-rva (+ text-rva free-success-call-off call-len)))
         (success-exit-disp
          (- exit-iat-rva (+ text-rva success-exit-call-off call-len)))
         (free-fail-disp
          (- virtualfree-iat-rva (+ text-rva free-fail-call-off call-len)))
         (fail-exit-disp
          (- exit-iat-rva (+ text-rva fail-exit-call-off call-len)))
         (jz-alloc-fail-disp (- fail-off alloc-fail-jz-next-off))
         (jz-protect-fail-disp (- free-fail-off protect-fail-jz-next-off))
         (jz-free-fail-disp (- fail-off free-fail-jz-next-off))
         (cbuf (nelisp-pe--make-buffer)))
    ;; Win64 ABI: 32-byte shadow space plus 8 bytes for stack alignment.
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x28))
    (nelisp-pe--write-bytes cbuf (unibyte-string #x31 #xc9)) ; rcx = NULL
    (nelisp-pe--write-u8 cbuf #xba) ; rdx = 4096
    (nelisp-pe--write-le32 cbuf #x1000)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x41 #xb8)) ; r8d = MEM_*
    (nelisp-pe--write-le32 cbuf #x3000)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x41 #xb9)) ; r9d = RW
    (nelisp-pe--write-le32 cbuf #x4)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf virtualalloc-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x85 #xc0)) ; test rax,rax
    (nelisp-pe--write-bytes cbuf
                            (unibyte-string #x74
                                            (logand jz-alloc-fail-disp #xff)))
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xc3)) ; rbx = base
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xc1)) ; rcx = base
    (nelisp-pe--write-u8 cbuf #xba) ; rdx = 4096
    (nelisp-pe--write-le32 cbuf #x1000)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x41 #xb8)) ; r8d = PAGE_READONLY
    (nelisp-pe--write-le32 cbuf #x2)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x4c #x8d #x0d)) ; r9 = &oldProtect
    (nelisp-pe--write-le32-signed cbuf old-protect-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf virtualprotect-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0)) ; test eax,eax
    (nelisp-pe--write-bytes cbuf
                            (unibyte-string #x74
                                            (logand jz-protect-fail-disp #xff)))
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xd9)) ; rcx = base
    (nelisp-pe--write-bytes cbuf (unibyte-string #x31 #xd2)) ; rdx = 0
    (nelisp-pe--write-bytes cbuf (unibyte-string #x41 #xb8)) ; r8d = MEM_RELEASE
    (nelisp-pe--write-le32 cbuf #x8000)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf free-success-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0)) ; test eax,eax
    (nelisp-pe--write-bytes cbuf
                            (unibyte-string #x74
                                            (logand jz-free-fail-disp #xff)))
    (nelisp-pe--write-u8 cbuf #xb9)
    (nelisp-pe--write-le32 cbuf 42)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf success-exit-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xd9)) ; rcx = base
    (nelisp-pe--write-bytes cbuf (unibyte-string #x31 #xd2)) ; rdx = 0
    (nelisp-pe--write-bytes cbuf (unibyte-string #x41 #xb8)) ; r8d = MEM_RELEASE
    (nelisp-pe--write-le32 cbuf #x8000)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf free-fail-disp)
    (nelisp-pe--write-u8 cbuf #xb9)
    (nelisp-pe--write-le32 cbuf 1)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf fail-exit-disp)
    (nelisp-pe--write-u8 cbuf #xcc)
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--minimal-virtualalloc-arena-text
    (text-rva exit-iat-rva virtualalloc-iat-rva data-rva arena-size)
  "Return x86_64 entry bytes that initialize arena metadata in .data.
The generated code calls VirtualAlloc(NULL, ARENA-SIZE,
MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE), writes base/cursor/end pointers at
DATA-RVA + 0/8/16, and exits 42 on success / 1 on failure."
  (let* ((virtualalloc-call-off 23)
         (base-store-off 34)
         (cursor-store-off 41)
         (end-store-off 58)
         (success-exit-call-off 70)
         (fail-off 76)
         (fail-exit-call-off 81)
         (call-len 6)
         (store-len 7)
         (virtualalloc-disp (- virtualalloc-iat-rva
                               (+ text-rva virtualalloc-call-off call-len)))
         (base-disp (- data-rva (+ text-rva base-store-off store-len)))
         (cursor-disp (- (+ data-rva 8)
                         (+ text-rva cursor-store-off store-len)))
         (end-disp (- (+ data-rva 16)
                      (+ text-rva end-store-off store-len)))
         (success-exit-disp (- exit-iat-rva
                               (+ text-rva success-exit-call-off call-len)))
         (fail-exit-disp (- exit-iat-rva
                            (+ text-rva fail-exit-call-off call-len)))
         (jz-disp (- fail-off (+ 32 2)))
         (cbuf (nelisp-pe--make-buffer)))
    ;; Win64 ABI: 32-byte shadow space plus 8 bytes for stack alignment.
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x28))
    (nelisp-pe--write-bytes cbuf (unibyte-string #x31 #xc9)) ; rcx = NULL
    (nelisp-pe--write-u8 cbuf #xba) ; rdx = arena size
    (nelisp-pe--write-le32 cbuf arena-size)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x41 #xb8)) ; r8d = MEM_*
    (nelisp-pe--write-le32 cbuf #x3000)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x41 #xb9)) ; r9d = PAGE_READWRITE
    (nelisp-pe--write-le32 cbuf #x4)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf virtualalloc-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x85 #xc0)) ; test rax,rax
    (nelisp-pe--write-bytes cbuf (unibyte-string #x74 (logand jz-disp #xff)))
    ;; arena_base = rax; arena_cursor = rax.
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #x05))
    (nelisp-pe--write-le32-signed cbuf base-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #x05))
    (nelisp-pe--write-le32-signed cbuf cursor-disp)
    ;; arena_end = rax + arena_size.
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xc2)) ; mov rdx, rax
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x81 #xc2)) ; add rdx, imm32
    (nelisp-pe--write-le32 cbuf arena-size)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #x15))
    (nelisp-pe--write-le32-signed cbuf end-disp)
    (nelisp-pe--write-u8 cbuf #xb9)
    (nelisp-pe--write-le32 cbuf 42)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf success-exit-disp)
    (nelisp-pe--write-u8 cbuf #xb9)
    (nelisp-pe--write-le32 cbuf 1)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf fail-exit-disp)
    (nelisp-pe--write-u8 cbuf #xcc)
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--minimal-writefile-stdout-text
    (text-rva exit-iat-rva get-std-handle-iat-rva write-file-iat-rva
              rdata-rva data-rva message-len)
  "Return x86_64 entry bytes that write a message to stdout via WriteFile.
The generated code calls GetStdHandle(STD_OUTPUT_HANDLE), then
WriteFile(handle, RDATA-RVA, MESSAGE-LEN, DATA-RVA, NULL), and exits 42 on
success / 1 on failure."
  (let* ((get-call-off 9)
         (lea-msg-off 18)
         (lea-written-off 31)
         (write-call-off 47)
         (success-exit-call-off 62)
         (fail-off 68)
         (fail-exit-call-off 73)
         (call-len 6)
         (lea-len 7)
         (get-disp (- get-std-handle-iat-rva
                      (+ text-rva get-call-off call-len)))
         (msg-disp (- rdata-rva (+ text-rva lea-msg-off lea-len)))
         (written-disp (- data-rva (+ text-rva lea-written-off lea-len)))
         (write-disp (- write-file-iat-rva
                        (+ text-rva write-call-off call-len)))
         (success-exit-disp (- exit-iat-rva
                               (+ text-rva success-exit-call-off call-len)))
         (fail-exit-disp (- exit-iat-rva
                            (+ text-rva fail-exit-call-off call-len)))
         (jz-disp (- fail-off (+ 55 2)))
         (cbuf (nelisp-pe--make-buffer)))
    ;; 32-byte shadow + one stack arg + alignment pad.
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x38))
    (nelisp-pe--write-u8 cbuf #xb9) ; mov ecx, STD_OUTPUT_HANDLE (-11)
    (nelisp-pe--write-le32 cbuf #xfffffff5)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf get-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xc1)) ; rcx = handle
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8d #x15)) ; rdx = msg
    (nelisp-pe--write-le32-signed cbuf msg-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x41 #xb8)) ; r8d = len
    (nelisp-pe--write-le32 cbuf message-len)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x4c #x8d #x0d)) ; r9 = written*
    (nelisp-pe--write-le32-signed cbuf written-disp)
    ;; 5th arg lpOverlapped = NULL at [rsp + 32].
    (nelisp-pe--write-bytes cbuf
                            (unibyte-string #x48 #xc7 #x44 #x24 #x20
                                            #x00 #x00 #x00 #x00))
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf write-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0)) ; test eax,eax
    (nelisp-pe--write-bytes cbuf (unibyte-string #x74 (logand jz-disp #xff)))
    (nelisp-pe--write-u8 cbuf #xb9)
    (nelisp-pe--write-le32 cbuf 42)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf success-exit-disp)
    (nelisp-pe--write-u8 cbuf #xb9)
    (nelisp-pe--write-le32 cbuf 1)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf fail-exit-disp)
    (nelisp-pe--write-u8 cbuf #xcc)
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--minimal-readfile-stdin-text
    (text-rva exit-iat-rva get-std-handle-iat-rva read-file-iat-rva data-rva)
  "Return x86_64 entry bytes that issue a zero-byte stdin ReadFile.
The generated code calls GetStdHandle(STD_INPUT_HANDLE), then
ReadFile(handle, DATA-RVA, 0, DATA-RVA+4, NULL), and exits 42 on success /
1 on failure.  The zero-byte read proves the import and Win64 call shape
without blocking for console input."
  (let* ((get-call-off 9)
         (lea-buf-off 18)
         (lea-read-off 28)
         (read-call-off 44)
         (success-exit-call-off 59)
         (fail-off 65)
         (fail-exit-call-off 70)
         (call-len 6)
         (lea-len 7)
         (get-disp (- get-std-handle-iat-rva
                      (+ text-rva get-call-off call-len)))
         (buf-disp (- data-rva (+ text-rva lea-buf-off lea-len)))
         (read-disp (- (+ data-rva 4) (+ text-rva lea-read-off lea-len)))
         (read-file-disp (- read-file-iat-rva
                            (+ text-rva read-call-off call-len)))
         (success-exit-disp (- exit-iat-rva
                               (+ text-rva success-exit-call-off call-len)))
         (fail-exit-disp (- exit-iat-rva
                            (+ text-rva fail-exit-call-off call-len)))
         (jz-disp (- fail-off (+ 52 2)))
         (cbuf (nelisp-pe--make-buffer)))
    ;; 32-byte shadow + one stack arg + alignment pad.
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x38))
    (nelisp-pe--write-u8 cbuf #xb9) ; mov ecx, STD_INPUT_HANDLE (-10)
    (nelisp-pe--write-le32 cbuf #xfffffff6)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf get-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xc1)) ; rcx = handle
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8d #x15)) ; rdx = buffer
    (nelisp-pe--write-le32-signed cbuf buf-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x45 #x31 #xc0)) ; r8d = 0
    (nelisp-pe--write-bytes cbuf (unibyte-string #x4c #x8d #x0d)) ; r9 = bytesRead*
    (nelisp-pe--write-le32-signed cbuf read-disp)
    ;; 5th arg lpOverlapped = NULL at [rsp + 32].
    (nelisp-pe--write-bytes cbuf
                            (unibyte-string #x48 #xc7 #x44 #x24 #x20
                                            #x00 #x00 #x00 #x00))
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf read-file-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0)) ; test eax,eax
    (nelisp-pe--write-bytes cbuf (unibyte-string #x74 (logand jz-disp #xff)))
    (nelisp-pe--write-u8 cbuf #xb9)
    (nelisp-pe--write-le32 cbuf 42)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf success-exit-disp)
    (nelisp-pe--write-u8 cbuf #xb9)
    (nelisp-pe--write-le32 cbuf 1)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf fail-exit-disp)
    (nelisp-pe--write-u8 cbuf #xcc)
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--minimal-createfile-write-text
    (text-rva exit-iat-rva create-file-iat-rva write-file-iat-rva
              close-handle-iat-rva delete-file-iat-rva rdata-rva data-rva
              path-len message-len)
  "Return x86_64 bytes that create, write, close, and delete a file.
The generated code calls CreateFileW(PATH, GENERIC_WRITE, 0, NULL,
CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL), writes MESSAGE-LEN bytes from the
RDATA message after PATH, closes the handle, deletes PATH, and exits 42 only
when the full lifecycle succeeds."
  (let ((cbuf (nelisp-pe--make-buffer))
        (message-rva (+ rdata-rva path-len))
        (handle-rva data-rva)
        (bytes-written-rva (+ data-rva 8)))
    (let (emit-call
          emit-lea-rcx
          emit-lea-rdx
          emit-lea-r9
          emit-mov-data-rax
          emit-mov-rcx-data)
      (setq emit-call
            (lambda (iat-rva)
              (let ((call-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
                (nelisp-pe--write-le32-signed
                 cbuf (- iat-rva (+ text-rva call-off 6))))))
      (setq emit-lea-rcx
            (lambda (target-rva)
              (let ((lea-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8d #x0d))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva lea-off 7))))))
      (setq emit-lea-rdx
            (lambda (target-rva)
              (let ((lea-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8d #x15))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva lea-off 7))))))
      (setq emit-lea-r9
            (lambda (target-rva)
              (let ((lea-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x4c #x8d #x0d))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva lea-off 7))))))
      (setq emit-mov-data-rax
            (lambda (target-rva)
              (let ((mov-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #x05))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva mov-off 7))))))
      (setq emit-mov-rcx-data
            (lambda (target-rva)
              (let ((mov-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8b #x0d))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva mov-off 7))))))
      ;; Win64 ABI: 32-byte shadow, three stack args for CreateFileW, alignment.
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x48))
      (funcall emit-lea-rcx rdata-rva)
      (nelisp-pe--write-u8 cbuf #xba) ; edx = GENERIC_WRITE
      (nelisp-pe--write-le32 cbuf #x40000000)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x45 #x31 #xc0)) ; share = 0
      (nelisp-pe--write-bytes cbuf (unibyte-string #x45 #x31 #xc9)) ; sec = NULL
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #xc7 #x44 #x24 #x20
                                              #x02 #x00 #x00 #x00))
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #xc7 #x44 #x24 #x28
                                              #x80 #x00 #x00 #x00))
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #x48 #xc7 #x44 #x24 #x30
                                              #x00 #x00 #x00 #x00))
      (funcall emit-call create-file-iat-rva)
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #x48 #x83 #xf8 #xff
                                              #x74 #x5e))
      (funcall emit-mov-data-rax handle-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xc1))
      (funcall emit-lea-rdx message-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x41 #xb8))
      (nelisp-pe--write-le32 cbuf message-len)
      (funcall emit-lea-r9 bytes-written-rva)
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #x48 #xc7 #x44 #x24 #x20
                                              #x00 #x00 #x00 #x00))
      (funcall emit-call write-file-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x74 #x2d))
      (funcall emit-mov-rcx-data handle-rva)
      (funcall emit-call close-handle-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x74 #x1c))
      (funcall emit-lea-rcx rdata-rva)
      (funcall emit-call delete-file-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x74 #x0b))
      (nelisp-pe--write-u8 cbuf #xb9)
      (nelisp-pe--write-le32 cbuf 42)
      (funcall emit-call exit-iat-rva)
      (nelisp-pe--write-u8 cbuf #xb9)
      (nelisp-pe--write-le32 cbuf 1)
      (funcall emit-call exit-iat-rva)
      (nelisp-pe--write-u8 cbuf #xcc)
      (nelisp-pe--buffer-bytes cbuf))))

(defun nelisp-pe--minimal-setfilepointer-text
    (text-rva exit-iat-rva create-file-iat-rva set-file-pointer-iat-rva
              close-handle-iat-rva delete-file-iat-rva rdata-rva data-rva)
  "Return x86_64 bytes that create, seek, close, and delete a file.
The generated code calls CreateFileW(PATH, GENERIC_WRITE, 0, NULL,
CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL), then
SetFilePointerEx(handle, 0, DATA-RVA+8, FILE_BEGIN), closes the handle,
deletes PATH, and exits 42 only when the full lifecycle succeeds."
  (let ((cbuf (nelisp-pe--make-buffer))
        (handle-rva data-rva)
        (new-position-rva (+ data-rva 8)))
    (let (emit-call
          emit-lea-rcx
          emit-lea-r8
          emit-mov-data-rax
          emit-mov-rcx-data)
      (setq emit-call
            (lambda (iat-rva)
              (let ((call-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
                (nelisp-pe--write-le32-signed
                 cbuf (- iat-rva (+ text-rva call-off 6))))))
      (setq emit-lea-rcx
            (lambda (target-rva)
              (let ((lea-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8d #x0d))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva lea-off 7))))))
      (setq emit-lea-r8
            (lambda (target-rva)
              (let ((lea-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x4c #x8d #x05))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva lea-off 7))))))
      (setq emit-mov-data-rax
            (lambda (target-rva)
              (let ((mov-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #x05))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva mov-off 7))))))
      (setq emit-mov-rcx-data
            (lambda (target-rva)
              (let ((mov-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8b #x0d))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva mov-off 7))))))
      ;; Win64 ABI: 32-byte shadow, three stack args for CreateFileW, alignment.
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x48))
      (funcall emit-lea-rcx rdata-rva)
      (nelisp-pe--write-u8 cbuf #xba) ; edx = GENERIC_WRITE
      (nelisp-pe--write-le32 cbuf #x40000000)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x45 #x31 #xc0)) ; share = 0
      (nelisp-pe--write-bytes cbuf (unibyte-string #x45 #x31 #xc9)) ; sec = NULL
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #xc7 #x44 #x24 #x20
                                              #x02 #x00 #x00 #x00))
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #xc7 #x44 #x24 #x28
                                              #x80 #x00 #x00 #x00))
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #x48 #xc7 #x44 #x24 #x30
                                              #x00 #x00 #x00 #x00))
      (funcall emit-call create-file-iat-rva)
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #x48 #x83 #xf8 #xff
                                              #x74 #x4d))
      (funcall emit-mov-data-rax handle-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xc1))
      (nelisp-pe--write-bytes cbuf (unibyte-string #x31 #xd2)) ; distance = 0
      (funcall emit-lea-r8 new-position-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x45 #x31 #xc9)) ; FILE_BEGIN
      (funcall emit-call set-file-pointer-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x74 #x2d))
      (funcall emit-mov-rcx-data handle-rva)
      (funcall emit-call close-handle-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x74 #x1c))
      (funcall emit-lea-rcx rdata-rva)
      (funcall emit-call delete-file-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x74 #x0b))
      (nelisp-pe--write-u8 cbuf #xb9)
      (nelisp-pe--write-le32 cbuf 42)
      (funcall emit-call exit-iat-rva)
      (nelisp-pe--write-u8 cbuf #xb9)
      (nelisp-pe--write-le32 cbuf 1)
      (funcall emit-call exit-iat-rva)
      (nelisp-pe--write-u8 cbuf #xcc)
      (nelisp-pe--buffer-bytes cbuf))))

(defun nelisp-pe--minimal-getfiletype-text
    (text-rva exit-iat-rva create-file-iat-rva get-file-type-iat-rva
              close-handle-iat-rva delete-file-iat-rva rdata-rva data-rva)
  "Return x86_64 bytes that create a file and verify FILE_TYPE_DISK.
The generated code calls CreateFileW(PATH, GENERIC_WRITE, 0, NULL,
CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL), then GetFileType(handle), closes
the handle, deletes PATH, and exits 42 only when the type is FILE_TYPE_DISK
and the cleanup succeeds."
  (let ((cbuf (nelisp-pe--make-buffer))
        (handle-rva data-rva))
    (let (emit-call
          emit-lea-rcx
          emit-mov-data-rax
          emit-mov-rcx-data)
      (setq emit-call
            (lambda (iat-rva)
              (let ((call-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
                (nelisp-pe--write-le32-signed
                 cbuf (- iat-rva (+ text-rva call-off 6))))))
      (setq emit-lea-rcx
            (lambda (target-rva)
              (let ((lea-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8d #x0d))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva lea-off 7))))))
      (setq emit-mov-data-rax
            (lambda (target-rva)
              (let ((mov-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #x05))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva mov-off 7))))))
      (setq emit-mov-rcx-data
            (lambda (target-rva)
              (let ((mov-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8b #x0d))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva mov-off 7))))))
      ;; Win64 ABI: 32-byte shadow, three stack args for CreateFileW, alignment.
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x48))
      (funcall emit-lea-rcx rdata-rva)
      (nelisp-pe--write-u8 cbuf #xba) ; edx = GENERIC_WRITE
      (nelisp-pe--write-le32 cbuf #x40000000)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x45 #x31 #xc0)) ; share = 0
      (nelisp-pe--write-bytes cbuf (unibyte-string #x45 #x31 #xc9)) ; sec = NULL
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #xc7 #x44 #x24 #x20
                                              #x02 #x00 #x00 #x00))
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #xc7 #x44 #x24 #x28
                                              #x80 #x00 #x00 #x00))
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #x48 #xc7 #x44 #x24 #x30
                                              #x00 #x00 #x00 #x00))
      (funcall emit-call create-file-iat-rva)
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #x48 #x83 #xf8 #xff
                                              #x74 #x42))
      (funcall emit-mov-data-rax handle-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xc1))
      (funcall emit-call get-file-type-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x83 #xf8 #x01 #x75 #x2d))
      (funcall emit-mov-rcx-data handle-rva)
      (funcall emit-call close-handle-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x74 #x1c))
      (funcall emit-lea-rcx rdata-rva)
      (funcall emit-call delete-file-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x74 #x0b))
      (nelisp-pe--write-u8 cbuf #xb9)
      (nelisp-pe--write-le32 cbuf 42)
      (funcall emit-call exit-iat-rva)
      (nelisp-pe--write-u8 cbuf #xb9)
      (nelisp-pe--write-le32 cbuf 1)
      (funcall emit-call exit-iat-rva)
      (nelisp-pe--write-u8 cbuf #xcc)
      (nelisp-pe--buffer-bytes cbuf))))

(defun nelisp-pe--minimal-getfileinformation-text
    (text-rva exit-iat-rva create-file-iat-rva get-file-information-iat-rva
              close-handle-iat-rva delete-file-iat-rva rdata-rva data-rva)
  "Return x86_64 bytes that create a file and query file information.
The generated code calls CreateFileW(PATH, GENERIC_READ|GENERIC_WRITE, 0,
NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL), then
GetFileInformationByHandle(handle, DATA-RVA+8), closes the handle, deletes
PATH, and exits 42 only when the information call and cleanup succeed."
  (let ((cbuf (nelisp-pe--make-buffer))
        (handle-rva data-rva)
        (info-rva (+ data-rva 8)))
    (let (emit-call
          emit-lea-rcx
          emit-lea-rdx
          emit-mov-data-rax
          emit-mov-rcx-data)
      (setq emit-call
            (lambda (iat-rva)
              (let ((call-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
                (nelisp-pe--write-le32-signed
                 cbuf (- iat-rva (+ text-rva call-off 6))))))
      (setq emit-lea-rcx
            (lambda (target-rva)
              (let ((lea-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8d #x0d))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva lea-off 7))))))
      (setq emit-lea-rdx
            (lambda (target-rva)
              (let ((lea-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8d #x15))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva lea-off 7))))))
      (setq emit-mov-data-rax
            (lambda (target-rva)
              (let ((mov-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #x05))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva mov-off 7))))))
      (setq emit-mov-rcx-data
            (lambda (target-rva)
              (let ((mov-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8b #x0d))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva mov-off 7))))))
      ;; Win64 ABI: 32-byte shadow, three stack args for CreateFileW, alignment.
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x48))
      (funcall emit-lea-rcx rdata-rva)
      (nelisp-pe--write-u8 cbuf #xba) ; edx = GENERIC_READ|GENERIC_WRITE
      (nelisp-pe--write-le32 cbuf #xc0000000)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x45 #x31 #xc0)) ; share = 0
      (nelisp-pe--write-bytes cbuf (unibyte-string #x45 #x31 #xc9)) ; sec = NULL
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #xc7 #x44 #x24 #x20
                                              #x02 #x00 #x00 #x00))
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #xc7 #x44 #x24 #x28
                                              #x80 #x00 #x00 #x00))
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #x48 #xc7 #x44 #x24 #x30
                                              #x00 #x00 #x00 #x00))
      (funcall emit-call create-file-iat-rva)
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #x48 #x83 #xf8 #xff
                                              #x74 #x48))
      (funcall emit-mov-data-rax handle-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xc1))
      (funcall emit-lea-rdx info-rva)
      (funcall emit-call get-file-information-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x74 #x2d))
      (funcall emit-mov-rcx-data handle-rva)
      (funcall emit-call close-handle-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x74 #x1c))
      (funcall emit-lea-rcx rdata-rva)
      (funcall emit-call delete-file-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x74 #x0b))
      (nelisp-pe--write-u8 cbuf #xb9)
      (nelisp-pe--write-le32 cbuf 42)
      (funcall emit-call exit-iat-rva)
      (nelisp-pe--write-u8 cbuf #xb9)
      (nelisp-pe--write-le32 cbuf 1)
      (funcall emit-call exit-iat-rva)
      (nelisp-pe--write-u8 cbuf #xcc)
      (nelisp-pe--buffer-bytes cbuf))))

(defun nelisp-pe--minimal-getcommandline-text
    (text-rva exit-iat-rva get-command-line-iat-rva)
  "Return x86_64 entry bytes that call GetCommandLineW, then ExitProcess.
The generated code exits 42 when GetCommandLineW returns a non-NULL pointer
and exits 1 otherwise."
  (let* ((get-call-off 4)
         (success-exit-call-off 20)
         (fail-off 26)
         (fail-exit-call-off 31)
         (call-len 6)
         (get-disp (- get-command-line-iat-rva
                      (+ text-rva get-call-off call-len)))
         (success-exit-disp (- exit-iat-rva
                               (+ text-rva success-exit-call-off call-len)))
         (fail-exit-disp (- exit-iat-rva
                            (+ text-rva fail-exit-call-off call-len)))
         (jz-disp (- fail-off (+ 13 2)))
         (cbuf (nelisp-pe--make-buffer)))
    ;; Win64 ABI: reserve the 32-byte shadow space and keep call alignment.
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x28))
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf get-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x85 #xc0)) ; test rax,rax
    (nelisp-pe--write-bytes cbuf (unibyte-string #x74 (logand jz-disp #xff)))
    (nelisp-pe--write-u8 cbuf #xb9)
    (nelisp-pe--write-le32 cbuf 42)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf success-exit-disp)
    (nelisp-pe--write-u8 cbuf #xb9)
    (nelisp-pe--write-le32 cbuf 1)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf fail-exit-disp)
    (nelisp-pe--write-u8 cbuf #xcc)
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--minimal-wsastartup-text
    (text-rva exit-iat-rva wsastartup-iat-rva data-rva)
  "Return x86_64 entry bytes that call WSAStartup, then ExitProcess.
The generated code exits 42 when WSAStartup(MAKEWORD(2,2), &WSADATA) succeeds
and exits 1 otherwise."
  (let* ((lea-data-off 9)
         (wsa-call-off 16)
         (fail-exit-call-off 31)
         (success-exit-call-off 42)
         (call-len 6)
         (lea-len 7)
         (data-disp (- data-rva (+ text-rva lea-data-off lea-len)))
         (wsa-disp (- wsastartup-iat-rva
                      (+ text-rva wsa-call-off call-len)))
         (fail-exit-disp (- exit-iat-rva
                            (+ text-rva fail-exit-call-off call-len)))
         (success-exit-disp (- exit-iat-rva
                               (+ text-rva success-exit-call-off call-len)))
         (cbuf (nelisp-pe--make-buffer)))
    ;; Win64 ABI: 32-byte shadow space plus 8 bytes for stack alignment.
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x28))
    (nelisp-pe--write-u8 cbuf #xb9) ; mov ecx, MAKEWORD(2,2)
    (nelisp-pe--write-le32 cbuf #x0202)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8d #x15)) ; rdx = WSADATA*
    (nelisp-pe--write-le32-signed cbuf data-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf wsa-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0)) ; test eax,eax
    (nelisp-pe--write-bytes cbuf (unibyte-string #x74 #x0b)) ; jz success
    (nelisp-pe--write-u8 cbuf #xb9)
    (nelisp-pe--write-le32 cbuf 1)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf fail-exit-disp)
    (nelisp-pe--write-u8 cbuf #xb9)
    (nelisp-pe--write-le32 cbuf 42)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf success-exit-disp)
    (nelisp-pe--write-u8 cbuf #xcc)
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--minimal-commandlinetoargv-text
    (text-rva exit-iat-rva get-command-line-iat-rva local-free-iat-rva
              command-line-to-argv-iat-rva data-rva)
  "Return x86_64 entry bytes that materialize argv without a CRT.
The generated code calls GetCommandLineW, then
CommandLineToArgvW(commandLine, &argc), frees the returned argv vector with
LocalFree, and exits 42 when argc >= 1."
  (let* ((get-call-off 4)
         (lea-argc-off 18)
         (argv-call-off 25)
         (cmp-argc-off 39)
         (local-free-call-off 51)
         (success-exit-call-off 62)
         (fail-off 68)
         (fail-exit-call-off 73)
         (call-len 6)
         (lea-len 7)
         (cmp-len 7)
         (get-disp (- get-command-line-iat-rva
                      (+ text-rva get-call-off call-len)))
         (argc-disp (- data-rva (+ text-rva lea-argc-off lea-len)))
         (argv-disp (- command-line-to-argv-iat-rva
                       (+ text-rva argv-call-off call-len)))
         (cmp-argc-disp (- data-rva (+ text-rva cmp-argc-off cmp-len)))
         (local-free-disp (- local-free-iat-rva
                             (+ text-rva local-free-call-off call-len)))
         (success-exit-disp (- exit-iat-rva
                               (+ text-rva success-exit-call-off call-len)))
         (fail-exit-disp (- exit-iat-rva
                            (+ text-rva fail-exit-call-off call-len)))
         (first-fail-disp (- fail-off (+ 13 2)))
         (second-fail-disp (- fail-off (+ 34 2)))
         (argc-fail-disp (- fail-off (+ 46 2)))
         (cbuf (nelisp-pe--make-buffer)))
    ;; Win64 ABI: 32-byte shadow space plus 8 bytes for stack alignment.
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x28))
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf get-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x85 #xc0)) ; test rax,rax
    (nelisp-pe--write-bytes cbuf (unibyte-string #x74 (logand first-fail-disp #xff)))
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xc1)) ; rcx = cmdline
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8d #x15)) ; rdx = argc*
    (nelisp-pe--write-le32-signed cbuf argc-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf argv-disp)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x85 #xc0)) ; test rax,rax
    (nelisp-pe--write-bytes cbuf (unibyte-string #x74 (logand second-fail-disp #xff)))
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xc3)) ; rbx = argv
    (nelisp-pe--write-bytes cbuf (unibyte-string #x83 #x3d))
    (nelisp-pe--write-le32-signed cbuf cmp-argc-disp)
    (nelisp-pe--write-u8 cbuf 1)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x7c (logand argc-fail-disp #xff)))
    (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xd9)) ; rcx = argv
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf local-free-disp)
    (nelisp-pe--write-u8 cbuf #xb9)
    (nelisp-pe--write-le32 cbuf 42)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf success-exit-disp)
    (nelisp-pe--write-u8 cbuf #xb9)
    (nelisp-pe--write-le32 cbuf 1)
    (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
    (nelisp-pe--write-le32-signed cbuf fail-exit-disp)
    (nelisp-pe--write-u8 cbuf #xcc)
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--createprocess-wait-data ()
  "Return data layout for the CreateProcessW wait smoke.
The command line is writable because CreateProcessW may modify it in place."
  (let* ((command-bytes (nelisp-pe--utf16le-z-bytes "cmd.exe /c exit 42"))
         (startup-info-size 104)
         (process-info-size 24)
         (startup-info-off (nelisp-pe--align-up (length command-bytes) 8))
         (process-info-off (+ startup-info-off startup-info-size))
         (exit-code-off (+ process-info-off process-info-size))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-bytes cbuf command-bytes)
    (nelisp-pe--write-pad cbuf (- startup-info-off (length command-bytes)))
    ;; STARTUPINFOW.cb.
    (nelisp-pe--write-le32 cbuf startup-info-size)
    (nelisp-pe--write-pad cbuf (- startup-info-size 4))
    (nelisp-pe--write-pad cbuf process-info-size)
    (nelisp-pe--write-pad cbuf 4)
    (list :bytes (nelisp-pe--buffer-bytes cbuf)
          :command-off 0
          :startup-info-off startup-info-off
          :process-info-off process-info-off
          :exit-code-off exit-code-off
          :startup-info-size startup-info-size)))

(defun nelisp-pe--minimal-createprocess-wait-text
    (text-rva exit-iat-rva createprocess-iat-rva wait-iat-rva
              get-exit-code-iat-rva close-handle-iat-rva data-rva
              command-off startup-info-off process-info-off exit-code-off)
  "Return x86_64 bytes for a CreateProcessW + wait smoke.
The generated code starts `cmd.exe /c exit 42', waits for the child process,
reads its exit code, closes its HANDLEs, and exits 42 only when the child
reported exit code 42."
  (let ((cbuf (nelisp-pe--make-buffer)))
    (let (emit-call
          emit-lea-rax-data
          emit-lea-rdx-data
          emit-mov-rcx-data-qword
          emit-fail-exit)
      (setq emit-call
            (lambda (iat-rva)
              (let ((call-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
                (nelisp-pe--write-le32-signed
                 cbuf (- iat-rva (+ text-rva call-off 6))))))
      (setq emit-lea-rax-data
            (lambda (off)
              (let ((lea-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8d #x05))
                (nelisp-pe--write-le32-signed
                 cbuf (- (+ data-rva off) (+ text-rva lea-off 7))))))
      (setq emit-lea-rdx-data
            (lambda (off)
              (let ((lea-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8d #x15))
                (nelisp-pe--write-le32-signed
                 cbuf (- (+ data-rva off) (+ text-rva lea-off 7))))))
      (setq emit-mov-rcx-data-qword
            (lambda (off)
              (let ((mov-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8b #x0d))
                (nelisp-pe--write-le32-signed
                 cbuf (- (+ data-rva off) (+ text-rva mov-off 7))))))
      (setq emit-fail-exit
            (lambda ()
              (nelisp-pe--write-u8 cbuf #xb9)
              (nelisp-pe--write-le32 cbuf 1)
              (funcall emit-call exit-iat-rva)))
      ;; Win64 ABI: 32-byte shadow space, six stack arguments, plus alignment.
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x68))
      (nelisp-pe--write-bytes cbuf (unibyte-string #x31 #xc9)) ; appName = NULL
      (funcall emit-lea-rdx-data command-off) ; writable command line
      (nelisp-pe--write-bytes cbuf (unibyte-string #x45 #x31 #xc0)) ; proc attr
      (nelisp-pe--write-bytes cbuf (unibyte-string #x45 #x31 #xc9)) ; thread attr
      (dolist (slot '(#x20 #x28 #x30 #x38))
        (nelisp-pe--write-bytes cbuf
                                (unibyte-string #x48 #xc7 #x44 #x24 slot
                                                #x00 #x00 #x00 #x00)))
      (funcall emit-lea-rax-data startup-info-off)
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #x48 #x89 #x44 #x24 #x40))
      (funcall emit-lea-rax-data process-info-off)
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #x48 #x89 #x44 #x24 #x48))
      (funcall emit-call createprocess-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x75 #x0b))
      (funcall emit-fail-exit)
      ;; Close PROCESS_INFORMATION.hThread.
      (funcall emit-mov-rcx-data-qword (+ process-info-off 8))
      (funcall emit-call close-handle-iat-rva)
      ;; WaitForSingleObject(PROCESS_INFORMATION.hProcess, INFINITE).
      (funcall emit-mov-rcx-data-qword process-info-off)
      (nelisp-pe--write-u8 cbuf #xba)
      (nelisp-pe--write-le32 cbuf #xffffffff)
      (funcall emit-call wait-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x74 #x0b))
      (funcall emit-fail-exit)
      ;; GetExitCodeProcess(PROCESS_INFORMATION.hProcess, &exitCode).
      (funcall emit-mov-rcx-data-qword process-info-off)
      (funcall emit-lea-rdx-data exit-code-off)
      (funcall emit-call get-exit-code-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x75 #x0b))
      (funcall emit-fail-exit)
      (funcall emit-mov-rcx-data-qword process-info-off)
      (funcall emit-call close-handle-iat-rva)
      ;; Exit 42 only if the child process exited 42.
      (let ((cmp-off (nelisp-pe--buffer-length cbuf)))
        (nelisp-pe--write-bytes cbuf (unibyte-string #x83 #x3d))
        (nelisp-pe--write-le32-signed
         cbuf (- (+ data-rva exit-code-off) (+ text-rva cmp-off 7)))
        (nelisp-pe--write-u8 cbuf 42))
      (nelisp-pe--write-bytes cbuf (unibyte-string #x74 #x0b))
      (funcall emit-fail-exit)
      (nelisp-pe--write-u8 cbuf #xb9)
      (nelisp-pe--write-le32 cbuf 42)
      (funcall emit-call exit-iat-rva)
      (nelisp-pe--write-u8 cbuf #xcc)
      (nelisp-pe--buffer-bytes cbuf))))

(defun nelisp-pe--createthread-wait-data ()
  "Return data layout for the CreateThread wait smoke."
  (list :bytes (make-string 8 0)
        :thread-id-off 0
        :exit-code-off 4))

(defun nelisp-pe--minimal-createthread-wait-text
    (text-rva exit-iat-rva createthread-iat-rva wait-iat-rva
              get-exit-code-iat-rva close-handle-iat-rva data-rva
              thread-id-off exit-code-off)
  "Return x86_64 bytes for a CreateThread + wait smoke.
The generated code starts an in-image thread entry that returns 42, waits for
that thread, reads its exit code, closes the thread HANDLE, and exits 42 only
when the thread reported exit code 42."
  (let ((cbuf (nelisp-pe--make-buffer)))
    (let (emit-call
          emit-lea-r8-text
          emit-lea-rax-data
          emit-lea-rdx-data
          emit-fail-exit
          thread-entry-rva)
      (setq emit-call
            (lambda (iat-rva)
              (let ((call-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #xff #x15))
                (nelisp-pe--write-le32-signed
                 cbuf (- iat-rva (+ text-rva call-off 6))))))
      (setq emit-lea-r8-text
            (lambda (target-rva)
              (let ((lea-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x4c #x8d #x05))
                (nelisp-pe--write-le32-signed
                 cbuf (- target-rva (+ text-rva lea-off 7))))))
      (setq emit-lea-rax-data
            (lambda (off)
              (let ((lea-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8d #x05))
                (nelisp-pe--write-le32-signed
                 cbuf (- (+ data-rva off) (+ text-rva lea-off 7))))))
      (setq emit-lea-rdx-data
            (lambda (off)
              (let ((lea-off (nelisp-pe--buffer-length cbuf)))
                (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x8d #x15))
                (nelisp-pe--write-le32-signed
                 cbuf (- (+ data-rva off) (+ text-rva lea-off 7))))))
      (setq emit-fail-exit
            (lambda ()
              (nelisp-pe--write-u8 cbuf #xb9)
              (nelisp-pe--write-le32 cbuf 1)
              (funcall emit-call exit-iat-rva)))
      ;; The main path length before the thread entry is fixed by this emitter.
      (setq thread-entry-rva (+ text-rva #xa4))
      ;; Win64 ABI: 32-byte shadow space, two stack args, plus alignment.
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x83 #xec #x48))
      (nelisp-pe--write-bytes cbuf (unibyte-string #x31 #xc9)) ; attrs = NULL
      (nelisp-pe--write-bytes cbuf (unibyte-string #x31 #xd2)) ; stackSize = 0
      (funcall emit-lea-r8-text thread-entry-rva) ; lpStartAddress
      (nelisp-pe--write-bytes cbuf (unibyte-string #x45 #x31 #xc9)) ; param = NULL
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #x48 #xc7 #x44 #x24 #x20
                                              #x00 #x00 #x00 #x00))
      (funcall emit-lea-rax-data thread-id-off)
      (nelisp-pe--write-bytes cbuf
                              (unibyte-string #x48 #x89 #x44 #x24 #x28))
      (funcall emit-call createthread-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x85 #xc0 #x75 #x0b))
      (funcall emit-fail-exit)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xc3)) ; rbx = handle
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xc1)) ; rcx = handle
      (nelisp-pe--write-u8 cbuf #xba)
      (nelisp-pe--write-le32 cbuf #xffffffff)
      (funcall emit-call wait-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x74 #x0b))
      (funcall emit-fail-exit)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xd9)) ; rcx = handle
      (funcall emit-lea-rdx-data exit-code-off)
      (funcall emit-call get-exit-code-iat-rva)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x85 #xc0 #x75 #x0b))
      (funcall emit-fail-exit)
      (nelisp-pe--write-bytes cbuf (unibyte-string #x48 #x89 #xd9)) ; rcx = handle
      (funcall emit-call close-handle-iat-rva)
      (let ((cmp-off (nelisp-pe--buffer-length cbuf)))
        (nelisp-pe--write-bytes cbuf (unibyte-string #x83 #x3d))
        (nelisp-pe--write-le32-signed
         cbuf (- (+ data-rva exit-code-off) (+ text-rva cmp-off 7)))
        (nelisp-pe--write-u8 cbuf 42))
      (nelisp-pe--write-bytes cbuf (unibyte-string #x74 #x0b))
      (funcall emit-fail-exit)
      (nelisp-pe--write-u8 cbuf #xb9)
      (nelisp-pe--write-le32 cbuf 42)
      (funcall emit-call exit-iat-rva)
      ;; Thread entry: DWORD WINAPI entry(void*) { return 42; }
      (unless (= (nelisp-pe--buffer-length cbuf)
                 (- thread-entry-rva text-rva))
        (error "nelisp-pe: CreateThread entry offset drift"))
      (nelisp-pe--write-u8 cbuf #xb8)
      (nelisp-pe--write-le32 cbuf 42)
      (nelisp-pe--write-u8 cbuf #xc3)
      (nelisp-pe--write-u8 cbuf #xcc)
      (nelisp-pe--buffer-bytes cbuf))))

(defun nelisp-pe--build-minimal-exitprocess-exe (exit-code)
  "Build a minimal PE32+ console EXE that exits with EXIT-CODE."
  (let* ((num-sections 2)
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (idata-rva (* 2 nelisp-pe--section-alignment))
         (idata-info (nelisp-pe--build-exitprocess-idata idata-rva))
         (idata-bytes (plist-get idata-info :bytes))
         (text-bytes
          (nelisp-pe--minimal-exitprocess-text
           exit-code text-rva (plist-get idata-info :iat-rva)))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (idata-raw-ptr (+ text-raw-ptr text-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ idata-rva (length idata-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00)) ; PE\0\0
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data idata-raw-size
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (unless (= (nelisp-pe--buffer-length cbuf) text-raw-ptr)
      (error "nelisp-pe: .text raw pointer drift"))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (unless (= (nelisp-pe--buffer-length cbuf) idata-raw-ptr)
      (error "nelisp-pe: .idata raw pointer drift"))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-virtualalloc-exitprocess-exe ()
  "Build a minimal PE32+ console EXE that proves VirtualAlloc import wiring."
  (let* ((num-sections 2)
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (idata-rva (* 2 nelisp-pe--section-alignment))
         (idata-info
          (nelisp-pe--build-kernel32-idata
           idata-rva (list "ExitProcess" "VirtualAlloc")))
         (iat-map (plist-get idata-info :iat-rva-alist))
         (idata-bytes (plist-get idata-info :bytes))
         (text-bytes
          (nelisp-pe--minimal-virtualalloc-text
           text-rva
           (cdr (assoc "ExitProcess" iat-map))
           (cdr (assoc "VirtualAlloc" iat-map))))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (idata-raw-ptr (+ text-raw-ptr text-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ idata-rva (length idata-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data idata-raw-size
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-virtualprotect-free-exitprocess-exe ()
  "Build a PE32+ EXE that proves VirtualProtect and VirtualFree wiring."
  (let* ((num-sections 3)
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (data-rva (* 2 nelisp-pe--section-alignment))
         (idata-rva (* 3 nelisp-pe--section-alignment))
         (idata-info
          (nelisp-pe--build-kernel32-idata
           idata-rva
           (list "ExitProcess" "VirtualAlloc" "VirtualProtect" "VirtualFree")))
         (iat-map (plist-get idata-info :iat-rva-alist))
         (idata-bytes (plist-get idata-info :bytes))
         (data-bytes (make-string 4 0))
         (text-bytes
          (nelisp-pe--minimal-virtualprotect-free-text
           text-rva
           (cdr (assoc "ExitProcess" iat-map))
           (cdr (assoc "VirtualAlloc" iat-map))
           (cdr (assoc "VirtualProtect" iat-map))
           (cdr (assoc "VirtualFree" iat-map))
           data-rva))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (data-raw-size
          (nelisp-pe--align-up (length data-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (data-raw-ptr (+ text-raw-ptr text-raw-size))
         (idata-raw-ptr (+ data-raw-ptr data-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ idata-rva (length idata-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data (+ data-raw-size idata-raw-size)
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".data"
           :virtual-size (length data-bytes)
           :virtual-address data-rva
           :raw-data-size data-raw-size
           :raw-data-ptr data-raw-ptr
           :characteristics nelisp-pe--scn-data-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (nelisp-pe--write-bytes cbuf data-bytes)
    (nelisp-pe--write-pad cbuf (- data-raw-size (length data-bytes)))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-virtualalloc-arena-exitprocess-exe ()
  "Build a PE32+ EXE that initializes a tiny Windows arena metadata block."
  (let* ((num-sections 3)
         (arena-size #x10000)
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (idata-rva (* 2 nelisp-pe--section-alignment))
         (data-rva (* 3 nelisp-pe--section-alignment))
         (idata-info
          (nelisp-pe--build-kernel32-idata
           idata-rva (list "ExitProcess" "VirtualAlloc")))
         (iat-map (plist-get idata-info :iat-rva-alist))
         (idata-bytes (plist-get idata-info :bytes))
         (data-bytes (make-string 24 0))
         (text-bytes
          (nelisp-pe--minimal-virtualalloc-arena-text
           text-rva
           (cdr (assoc "ExitProcess" iat-map))
           (cdr (assoc "VirtualAlloc" iat-map))
           data-rva
           arena-size))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (data-raw-size
          (nelisp-pe--align-up (length data-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (idata-raw-ptr (+ text-raw-ptr text-raw-size))
         (data-raw-ptr (+ idata-raw-ptr idata-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ data-rva (length data-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data (+ idata-raw-size data-raw-size)
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".data"
           :virtual-size (length data-bytes)
           :virtual-address data-rva
           :raw-data-size data-raw-size
           :raw-data-ptr data-raw-ptr
           :characteristics nelisp-pe--scn-data-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
    (nelisp-pe--write-bytes cbuf data-bytes)
    (nelisp-pe--write-pad cbuf (- data-raw-size (length data-bytes)))
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-writefile-stdout-exitprocess-exe ()
  "Build a PE32+ EXE that writes a short message to stdout via WriteFile."
  (let* ((num-sections 4)
         (message-bytes "hello from nelisp windows\n")
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (rdata-rva (* 2 nelisp-pe--section-alignment))
         (data-rva (* 3 nelisp-pe--section-alignment))
         (idata-rva (* 4 nelisp-pe--section-alignment))
         (idata-info
          (nelisp-pe--build-kernel32-idata
           idata-rva (list "ExitProcess" "GetStdHandle" "WriteFile")))
         (iat-map (plist-get idata-info :iat-rva-alist))
         (idata-bytes (plist-get idata-info :bytes))
         (data-bytes (make-string 4 0))
         (text-bytes
          (nelisp-pe--minimal-writefile-stdout-text
           text-rva
           (cdr (assoc "ExitProcess" iat-map))
           (cdr (assoc "GetStdHandle" iat-map))
           (cdr (assoc "WriteFile" iat-map))
           rdata-rva
           data-rva
           (length message-bytes)))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (rdata-raw-size
          (nelisp-pe--align-up (length message-bytes) nelisp-pe--file-alignment))
         (data-raw-size
          (nelisp-pe--align-up (length data-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (rdata-raw-ptr (+ text-raw-ptr text-raw-size))
         (data-raw-ptr (+ rdata-raw-ptr rdata-raw-size))
         (idata-raw-ptr (+ data-raw-ptr data-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ idata-rva (length idata-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data (+ rdata-raw-size
                                        data-raw-size
                                        idata-raw-size)
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".rdata"
           :virtual-size (length message-bytes)
           :virtual-address rdata-rva
           :raw-data-size rdata-raw-size
           :raw-data-ptr rdata-raw-ptr
           :characteristics nelisp-pe--scn-rdata-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".data"
           :virtual-size (length data-bytes)
           :virtual-address data-rva
           :raw-data-size data-raw-size
           :raw-data-ptr data-raw-ptr
           :characteristics nelisp-pe--scn-data-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (nelisp-pe--write-bytes cbuf message-bytes)
    (nelisp-pe--write-pad cbuf (- rdata-raw-size (length message-bytes)))
    (nelisp-pe--write-bytes cbuf data-bytes)
    (nelisp-pe--write-pad cbuf (- data-raw-size (length data-bytes)))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-readfile-stdin-exitprocess-exe ()
  "Build a PE32+ EXE that proves stdin ReadFile import and call wiring."
  (let* ((num-sections 3)
         (data-bytes (make-string 8 0))
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (data-rva (* 2 nelisp-pe--section-alignment))
         (idata-rva (* 3 nelisp-pe--section-alignment))
         (idata-info
          (nelisp-pe--build-kernel32-idata
           idata-rva (list "ExitProcess" "GetStdHandle" "ReadFile")))
         (iat-map (plist-get idata-info :iat-rva-alist))
         (idata-bytes (plist-get idata-info :bytes))
         (text-bytes
          (nelisp-pe--minimal-readfile-stdin-text
           text-rva
           (cdr (assoc "ExitProcess" iat-map))
           (cdr (assoc "GetStdHandle" iat-map))
           (cdr (assoc "ReadFile" iat-map))
           data-rva))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (data-raw-size
          (nelisp-pe--align-up (length data-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (data-raw-ptr (+ text-raw-ptr text-raw-size))
         (idata-raw-ptr (+ data-raw-ptr data-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ idata-rva (length idata-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data (+ data-raw-size idata-raw-size)
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".data"
           :virtual-size (length data-bytes)
           :virtual-address data-rva
           :raw-data-size data-raw-size
           :raw-data-ptr data-raw-ptr
           :characteristics nelisp-pe--scn-data-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (nelisp-pe--write-bytes cbuf data-bytes)
    (nelisp-pe--write-pad cbuf (- data-raw-size (length data-bytes)))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-createfile-write-exitprocess-exe ()
  "Build a PE32+ EXE that writes and deletes a file through kernel32."
  (let* ((num-sections 4)
         (path-bytes
          (nelisp-pe--utf16le-z-bytes
           "target\\windows-smoke\\nelisp-windows-createfile.tmp"))
         (message-bytes "createfile smoke\n")
         (rdata-bytes (concat path-bytes message-bytes))
         (data-bytes (make-string 12 0))
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (rdata-rva (* 2 nelisp-pe--section-alignment))
         (data-rva (* 3 nelisp-pe--section-alignment))
         (idata-rva (* 4 nelisp-pe--section-alignment))
         (idata-info
          (nelisp-pe--build-kernel32-idata
           idata-rva
           (list "ExitProcess" "CreateFileW" "WriteFile"
                 "CloseHandle" "DeleteFileW")))
         (iat-map (plist-get idata-info :iat-rva-alist))
         (idata-bytes (plist-get idata-info :bytes))
         (text-bytes
          (nelisp-pe--minimal-createfile-write-text
           text-rva
           (cdr (assoc "ExitProcess" iat-map))
           (cdr (assoc "CreateFileW" iat-map))
           (cdr (assoc "WriteFile" iat-map))
           (cdr (assoc "CloseHandle" iat-map))
           (cdr (assoc "DeleteFileW" iat-map))
           rdata-rva
           data-rva
           (length path-bytes)
           (length message-bytes)))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (rdata-raw-size
          (nelisp-pe--align-up (length rdata-bytes) nelisp-pe--file-alignment))
         (data-raw-size
          (nelisp-pe--align-up (length data-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (rdata-raw-ptr (+ text-raw-ptr text-raw-size))
         (data-raw-ptr (+ rdata-raw-ptr rdata-raw-size))
         (idata-raw-ptr (+ data-raw-ptr data-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ idata-rva (length idata-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data (+ rdata-raw-size
                                        data-raw-size
                                        idata-raw-size)
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".rdata"
           :virtual-size (length rdata-bytes)
           :virtual-address rdata-rva
           :raw-data-size rdata-raw-size
           :raw-data-ptr rdata-raw-ptr
           :characteristics nelisp-pe--scn-rdata-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".data"
           :virtual-size (length data-bytes)
           :virtual-address data-rva
           :raw-data-size data-raw-size
           :raw-data-ptr data-raw-ptr
           :characteristics nelisp-pe--scn-data-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (nelisp-pe--write-bytes cbuf rdata-bytes)
    (nelisp-pe--write-pad cbuf (- rdata-raw-size (length rdata-bytes)))
    (nelisp-pe--write-bytes cbuf data-bytes)
    (nelisp-pe--write-pad cbuf (- data-raw-size (length data-bytes)))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-setfilepointer-exitprocess-exe ()
  "Build a PE32+ EXE that proves SetFilePointerEx file seek wiring."
  (let* ((num-sections 4)
         (path-bytes
          (nelisp-pe--utf16le-z-bytes
           "target\\windows-smoke\\nelisp-windows-setfilepointer.tmp"))
         (data-bytes (make-string 16 0))
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (rdata-rva (* 2 nelisp-pe--section-alignment))
         (data-rva (* 3 nelisp-pe--section-alignment))
         (idata-rva (* 4 nelisp-pe--section-alignment))
         (idata-info
          (nelisp-pe--build-kernel32-idata
           idata-rva
           (list "ExitProcess" "CreateFileW" "SetFilePointerEx"
                 "CloseHandle" "DeleteFileW")))
         (iat-map (plist-get idata-info :iat-rva-alist))
         (idata-bytes (plist-get idata-info :bytes))
         (text-bytes
          (nelisp-pe--minimal-setfilepointer-text
           text-rva
           (cdr (assoc "ExitProcess" iat-map))
           (cdr (assoc "CreateFileW" iat-map))
           (cdr (assoc "SetFilePointerEx" iat-map))
           (cdr (assoc "CloseHandle" iat-map))
           (cdr (assoc "DeleteFileW" iat-map))
           rdata-rva
           data-rva))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (rdata-raw-size
          (nelisp-pe--align-up (length path-bytes) nelisp-pe--file-alignment))
         (data-raw-size
          (nelisp-pe--align-up (length data-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (rdata-raw-ptr (+ text-raw-ptr text-raw-size))
         (data-raw-ptr (+ rdata-raw-ptr rdata-raw-size))
         (idata-raw-ptr (+ data-raw-ptr data-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ idata-rva (length idata-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data (+ rdata-raw-size
                                        data-raw-size
                                        idata-raw-size)
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".rdata"
           :virtual-size (length path-bytes)
           :virtual-address rdata-rva
           :raw-data-size rdata-raw-size
           :raw-data-ptr rdata-raw-ptr
           :characteristics nelisp-pe--scn-rdata-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".data"
           :virtual-size (length data-bytes)
           :virtual-address data-rva
           :raw-data-size data-raw-size
           :raw-data-ptr data-raw-ptr
           :characteristics nelisp-pe--scn-data-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (nelisp-pe--write-bytes cbuf path-bytes)
    (nelisp-pe--write-pad cbuf (- rdata-raw-size (length path-bytes)))
    (nelisp-pe--write-bytes cbuf data-bytes)
    (nelisp-pe--write-pad cbuf (- data-raw-size (length data-bytes)))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-getfiletype-exitprocess-exe ()
  "Build a PE32+ EXE that proves GetFileType HANDLE classification."
  (let* ((num-sections 4)
         (path-bytes
          (nelisp-pe--utf16le-z-bytes
           "target\\windows-smoke\\nelisp-windows-getfiletype.tmp"))
         (data-bytes (make-string 8 0))
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (rdata-rva (* 2 nelisp-pe--section-alignment))
         (data-rva (* 3 nelisp-pe--section-alignment))
         (idata-rva (* 4 nelisp-pe--section-alignment))
         (idata-info
          (nelisp-pe--build-kernel32-idata
           idata-rva
           (list "ExitProcess" "CreateFileW" "GetFileType"
                 "CloseHandle" "DeleteFileW")))
         (iat-map (plist-get idata-info :iat-rva-alist))
         (idata-bytes (plist-get idata-info :bytes))
         (text-bytes
          (nelisp-pe--minimal-getfiletype-text
           text-rva
           (cdr (assoc "ExitProcess" iat-map))
           (cdr (assoc "CreateFileW" iat-map))
           (cdr (assoc "GetFileType" iat-map))
           (cdr (assoc "CloseHandle" iat-map))
           (cdr (assoc "DeleteFileW" iat-map))
           rdata-rva
           data-rva))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (rdata-raw-size
          (nelisp-pe--align-up (length path-bytes) nelisp-pe--file-alignment))
         (data-raw-size
          (nelisp-pe--align-up (length data-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (rdata-raw-ptr (+ text-raw-ptr text-raw-size))
         (data-raw-ptr (+ rdata-raw-ptr rdata-raw-size))
         (idata-raw-ptr (+ data-raw-ptr data-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ idata-rva (length idata-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data (+ rdata-raw-size
                                        data-raw-size
                                        idata-raw-size)
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".rdata"
           :virtual-size (length path-bytes)
           :virtual-address rdata-rva
           :raw-data-size rdata-raw-size
           :raw-data-ptr rdata-raw-ptr
           :characteristics nelisp-pe--scn-rdata-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".data"
           :virtual-size (length data-bytes)
           :virtual-address data-rva
           :raw-data-size data-raw-size
           :raw-data-ptr data-raw-ptr
           :characteristics nelisp-pe--scn-data-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (nelisp-pe--write-bytes cbuf path-bytes)
    (nelisp-pe--write-pad cbuf (- rdata-raw-size (length path-bytes)))
    (nelisp-pe--write-bytes cbuf data-bytes)
    (nelisp-pe--write-pad cbuf (- data-raw-size (length data-bytes)))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-getfileinformation-exitprocess-exe ()
  "Build a PE32+ EXE that proves GetFileInformationByHandle wiring."
  (let* ((num-sections 4)
         (path-bytes
          (nelisp-pe--utf16le-z-bytes
           "target\\windows-smoke\\nelisp-windows-getfileinformation.tmp"))
         (data-bytes (make-string 60 0))
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (rdata-rva (* 2 nelisp-pe--section-alignment))
         (data-rva (* 3 nelisp-pe--section-alignment))
         (idata-rva (* 4 nelisp-pe--section-alignment))
         (idata-info
          (nelisp-pe--build-kernel32-idata
           idata-rva
           (list "ExitProcess" "CreateFileW" "GetFileInformationByHandle"
                 "CloseHandle" "DeleteFileW")))
         (iat-map (plist-get idata-info :iat-rva-alist))
         (idata-bytes (plist-get idata-info :bytes))
         (text-bytes
          (nelisp-pe--minimal-getfileinformation-text
           text-rva
           (cdr (assoc "ExitProcess" iat-map))
           (cdr (assoc "CreateFileW" iat-map))
           (cdr (assoc "GetFileInformationByHandle" iat-map))
           (cdr (assoc "CloseHandle" iat-map))
           (cdr (assoc "DeleteFileW" iat-map))
           rdata-rva
           data-rva))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (rdata-raw-size
          (nelisp-pe--align-up (length path-bytes) nelisp-pe--file-alignment))
         (data-raw-size
          (nelisp-pe--align-up (length data-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (rdata-raw-ptr (+ text-raw-ptr text-raw-size))
         (data-raw-ptr (+ rdata-raw-ptr rdata-raw-size))
         (idata-raw-ptr (+ data-raw-ptr data-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ idata-rva (length idata-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data (+ rdata-raw-size
                                        data-raw-size
                                        idata-raw-size)
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".rdata"
           :virtual-size (length path-bytes)
           :virtual-address rdata-rva
           :raw-data-size rdata-raw-size
           :raw-data-ptr rdata-raw-ptr
           :characteristics nelisp-pe--scn-rdata-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".data"
           :virtual-size (length data-bytes)
           :virtual-address data-rva
           :raw-data-size data-raw-size
           :raw-data-ptr data-raw-ptr
           :characteristics nelisp-pe--scn-data-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (nelisp-pe--write-bytes cbuf path-bytes)
    (nelisp-pe--write-pad cbuf (- rdata-raw-size (length path-bytes)))
    (nelisp-pe--write-bytes cbuf data-bytes)
    (nelisp-pe--write-pad cbuf (- data-raw-size (length data-bytes)))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-getcommandline-exitprocess-exe ()
  "Build a PE32+ EXE that proves GetCommandLineW import wiring."
  (let* ((num-sections 2)
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (idata-rva (* 2 nelisp-pe--section-alignment))
         (idata-info
          (nelisp-pe--build-kernel32-idata
           idata-rva (list "ExitProcess" "GetCommandLineW")))
         (iat-map (plist-get idata-info :iat-rva-alist))
         (idata-bytes (plist-get idata-info :bytes))
         (text-bytes
          (nelisp-pe--minimal-getcommandline-text
           text-rva
           (cdr (assoc "ExitProcess" iat-map))
           (cdr (assoc "GetCommandLineW" iat-map))))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (idata-raw-ptr (+ text-raw-ptr text-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ idata-rva (length idata-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data idata-raw-size
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-wsastartup-exitprocess-exe ()
  "Build a PE32+ EXE that proves WS2_32.dll WSAStartup import wiring."
  (let* ((num-sections 3)
         (wsadata-bytes (make-string 512 0))
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (data-rva (* 2 nelisp-pe--section-alignment))
         (idata-rva (* 3 nelisp-pe--section-alignment))
         (idata-info
          (nelisp-pe--build-idata
           idata-rva
           (list (cons "KERNEL32.dll" (list "ExitProcess"))
                 (cons "WS2_32.dll" (list "WSAStartup")))))
         (iat-map (plist-get idata-info :iat-rva-alist))
         (idata-bytes (plist-get idata-info :bytes))
         (text-bytes
          (nelisp-pe--minimal-wsastartup-text
           text-rva
           (cdr (assoc "ExitProcess" iat-map))
           (cdr (assoc "WSAStartup" iat-map))
           data-rva))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (data-raw-size
          (nelisp-pe--align-up (length wsadata-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (data-raw-ptr (+ text-raw-ptr text-raw-size))
         (idata-raw-ptr (+ data-raw-ptr data-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ idata-rva (length idata-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data (+ data-raw-size idata-raw-size)
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".data"
           :virtual-size (length wsadata-bytes)
           :virtual-address data-rva
           :raw-data-size data-raw-size
           :raw-data-ptr data-raw-ptr
           :characteristics nelisp-pe--scn-data-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (nelisp-pe--write-bytes cbuf wsadata-bytes)
    (nelisp-pe--write-pad cbuf (- data-raw-size (length wsadata-bytes)))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-commandlinetoargv-exitprocess-exe ()
  "Build a PE32+ EXE that proves CRT-free argv materialization imports."
  (let* ((num-sections 3)
         (argc-bytes (make-string 4 0))
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (data-rva (* 2 nelisp-pe--section-alignment))
         (idata-rva (* 3 nelisp-pe--section-alignment))
         (idata-info
          (nelisp-pe--build-idata
           idata-rva
           (list (cons "KERNEL32.dll"
                       (list "ExitProcess" "GetCommandLineW" "LocalFree"))
                 (cons "SHELL32.dll" (list "CommandLineToArgvW")))))
         (iat-map (plist-get idata-info :iat-rva-alist))
         (idata-bytes (plist-get idata-info :bytes))
         (text-bytes
          (nelisp-pe--minimal-commandlinetoargv-text
           text-rva
           (cdr (assoc "ExitProcess" iat-map))
           (cdr (assoc "GetCommandLineW" iat-map))
           (cdr (assoc "LocalFree" iat-map))
           (cdr (assoc "CommandLineToArgvW" iat-map))
           data-rva))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (data-raw-size
          (nelisp-pe--align-up (length argc-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (data-raw-ptr (+ text-raw-ptr text-raw-size))
         (idata-raw-ptr (+ data-raw-ptr data-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ idata-rva (length idata-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data (+ data-raw-size idata-raw-size)
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".data"
           :virtual-size (length argc-bytes)
           :virtual-address data-rva
           :raw-data-size data-raw-size
           :raw-data-ptr data-raw-ptr
           :characteristics nelisp-pe--scn-data-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (nelisp-pe--write-bytes cbuf argc-bytes)
    (nelisp-pe--write-pad cbuf (- data-raw-size (length argc-bytes)))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-createprocess-wait-exitprocess-exe ()
  "Build a PE32+ EXE that proves CreateProcessW child wait wiring."
  (let* ((num-sections 3)
         (data-info (nelisp-pe--createprocess-wait-data))
         (data-bytes (plist-get data-info :bytes))
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (data-rva (* 2 nelisp-pe--section-alignment))
         (idata-rva (* 3 nelisp-pe--section-alignment))
         (idata-info
          (nelisp-pe--build-kernel32-idata
           idata-rva
           (list "ExitProcess" "CreateProcessW" "WaitForSingleObject"
                 "GetExitCodeProcess" "CloseHandle")))
         (iat-map (plist-get idata-info :iat-rva-alist))
         (idata-bytes (plist-get idata-info :bytes))
         (text-bytes
          (nelisp-pe--minimal-createprocess-wait-text
           text-rva
           (cdr (assoc "ExitProcess" iat-map))
           (cdr (assoc "CreateProcessW" iat-map))
           (cdr (assoc "WaitForSingleObject" iat-map))
           (cdr (assoc "GetExitCodeProcess" iat-map))
           (cdr (assoc "CloseHandle" iat-map))
           data-rva
           (plist-get data-info :command-off)
           (plist-get data-info :startup-info-off)
           (plist-get data-info :process-info-off)
           (plist-get data-info :exit-code-off)))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (data-raw-size
          (nelisp-pe--align-up (length data-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (data-raw-ptr (+ text-raw-ptr text-raw-size))
         (idata-raw-ptr (+ data-raw-ptr data-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ idata-rva (length idata-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data (+ data-raw-size idata-raw-size)
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".data"
           :virtual-size (length data-bytes)
           :virtual-address data-rva
           :raw-data-size data-raw-size
           :raw-data-ptr data-raw-ptr
           :characteristics nelisp-pe--scn-data-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (nelisp-pe--write-bytes cbuf data-bytes)
    (nelisp-pe--write-pad cbuf (- data-raw-size (length data-bytes)))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
    (nelisp-pe--buffer-bytes cbuf)))

(defun nelisp-pe--build-createthread-wait-exitprocess-exe ()
  "Build a PE32+ EXE that proves CreateThread wait wiring."
  (let* ((num-sections 3)
         (data-info (nelisp-pe--createthread-wait-data))
         (data-bytes (plist-get data-info :bytes))
         (pe-offset nelisp-pe--dos-header-size)
         (nt-headers-size (+ 4
                             nelisp-pe--header-size
                             nelisp-pe--optional-header-pe32-plus-size
                             (* num-sections nelisp-pe--section-header-size)))
         (size-of-headers
          (nelisp-pe--align-up (+ pe-offset nt-headers-size)
                               nelisp-pe--file-alignment))
         (text-rva nelisp-pe--section-alignment)
         (data-rva (* 2 nelisp-pe--section-alignment))
         (idata-rva (* 3 nelisp-pe--section-alignment))
         (idata-info
          (nelisp-pe--build-kernel32-idata
           idata-rva
           (list "ExitProcess" "CreateThread" "WaitForSingleObject"
                 "GetExitCodeThread" "CloseHandle")))
         (iat-map (plist-get idata-info :iat-rva-alist))
         (idata-bytes (plist-get idata-info :bytes))
         (text-bytes
          (nelisp-pe--minimal-createthread-wait-text
           text-rva
           (cdr (assoc "ExitProcess" iat-map))
           (cdr (assoc "CreateThread" iat-map))
           (cdr (assoc "WaitForSingleObject" iat-map))
           (cdr (assoc "GetExitCodeThread" iat-map))
           (cdr (assoc "CloseHandle" iat-map))
           data-rva
           (plist-get data-info :thread-id-off)
           (plist-get data-info :exit-code-off)))
         (text-raw-size
          (nelisp-pe--align-up (length text-bytes) nelisp-pe--file-alignment))
         (data-raw-size
          (nelisp-pe--align-up (length data-bytes) nelisp-pe--file-alignment))
         (idata-raw-size
          (nelisp-pe--align-up (length idata-bytes) nelisp-pe--file-alignment))
         (text-raw-ptr size-of-headers)
         (data-raw-ptr (+ text-raw-ptr text-raw-size))
         (idata-raw-ptr (+ data-raw-ptr data-raw-size))
         (size-of-image
          (nelisp-pe--align-up (+ idata-rva (length idata-bytes))
                               nelisp-pe--section-alignment))
         (cbuf (nelisp-pe--make-buffer)))
    (nelisp-pe--write-dos-stub cbuf pe-offset)
    (nelisp-pe--write-bytes cbuf (unibyte-string #x50 #x45 #x00 #x00))
    (nelisp-pe--write-pe-file-header
     cbuf
     (list :num-sections num-sections
           :characteristics
           (logior nelisp-pe--characteristic-executable
                   nelisp-pe--characteristic-large-address-aware)))
    (nelisp-pe--write-optional-header64
     cbuf
     (list :entry-rva text-rva
           :text-rva text-rva
           :size-of-code text-raw-size
           :size-of-initialized-data (+ data-raw-size idata-raw-size)
           :size-of-image size-of-image
           :size-of-headers size-of-headers
           :import-rva (plist-get idata-info :import-rva)
           :import-size (plist-get idata-info :import-size)
           :iat-rva (plist-get idata-info :iat-rva)
           :iat-size (plist-get idata-info :iat-size)))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".text"
           :virtual-size (length text-bytes)
           :virtual-address text-rva
           :raw-data-size text-raw-size
           :raw-data-ptr text-raw-ptr
           :characteristics nelisp-pe--scn-exe-text-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".data"
           :virtual-size (length data-bytes)
           :virtual-address data-rva
           :raw-data-size data-raw-size
           :raw-data-ptr data-raw-ptr
           :characteristics nelisp-pe--scn-data-flags))
    (nelisp-pe--write-pe-section-header
     cbuf
     (list :name ".idata"
           :virtual-size (length idata-bytes)
           :virtual-address idata-rva
           :raw-data-size idata-raw-size
           :raw-data-ptr idata-raw-ptr
           :characteristics nelisp-pe--scn-idata-flags))
    (let ((header-pad (- size-of-headers (nelisp-pe--buffer-length cbuf))))
      (when (< header-pad 0)
        (error "nelisp-pe: PE headers exceed SizeOfHeaders"))
      (nelisp-pe--write-pad cbuf header-pad))
    (nelisp-pe--write-bytes cbuf text-bytes)
    (nelisp-pe--write-pad cbuf (- text-raw-size (length text-bytes)))
    (nelisp-pe--write-bytes cbuf data-bytes)
    (nelisp-pe--write-pad cbuf (- data-raw-size (length data-bytes)))
    (nelisp-pe--write-bytes cbuf idata-bytes)
    (nelisp-pe--write-pad cbuf (- idata-raw-size (length idata-bytes)))
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
(defun nelisp-pe-write-exe-binary (file-path spec)
  "Emit a PE32+ console executable to FILE-PATH.
SPEC is currently `minimal-exit-42', `virtualalloc-exit-42',
`virtualprotect-free-exit-42', `virtualalloc-arena-exit-42',
`writefile-stdout-exit-42', `readfile-stdin-exit-42',
`createfile-write-exit-42', `setfilepointer-exit-42',
`getfiletype-exit-42', `getfileinformation-exit-42',
`getcommandline-exit-42', `wsastartup-exit-42',
`commandlinetoargv-exit-42', `createprocess-wait-exit-42',
`createthread-wait-exit-42', or a plist with :exit-code.  The output imports
DLL functions through a real PE import directory and writes raw bytes with
`no-conversion'.  Returns FILE-PATH."
  (let* ((exit-code
          (cond
           ((eq spec 'minimal-exit-42) 42)
           ((eq spec 'virtualalloc-exit-42) nil)
           ((eq spec 'virtualprotect-free-exit-42) nil)
           ((eq spec 'virtualalloc-arena-exit-42) nil)
           ((eq spec 'writefile-stdout-exit-42) nil)
           ((eq spec 'readfile-stdin-exit-42) nil)
           ((eq spec 'createfile-write-exit-42) nil)
           ((eq spec 'setfilepointer-exit-42) nil)
           ((eq spec 'getfiletype-exit-42) nil)
           ((eq spec 'getfileinformation-exit-42) nil)
           ((eq spec 'getcommandline-exit-42) nil)
           ((eq spec 'wsastartup-exit-42) nil)
           ((eq spec 'commandlinetoargv-exit-42) nil)
           ((eq spec 'createprocess-wait-exit-42) nil)
           ((eq spec 'createthread-wait-exit-42) nil)
           ((listp spec) (or (plist-get spec :exit-code) 42))
           (t (error "nelisp-pe-write-exe-binary: invalid SPEC %S" spec))))
         (bytes (cond
                 ((eq spec 'virtualalloc-exit-42)
                  (nelisp-pe--build-virtualalloc-exitprocess-exe))
                 ((eq spec 'virtualprotect-free-exit-42)
                  (nelisp-pe--build-virtualprotect-free-exitprocess-exe))
                 ((eq spec 'virtualalloc-arena-exit-42)
                  (nelisp-pe--build-virtualalloc-arena-exitprocess-exe))
                 ((eq spec 'writefile-stdout-exit-42)
                  (nelisp-pe--build-writefile-stdout-exitprocess-exe))
                 ((eq spec 'readfile-stdin-exit-42)
                  (nelisp-pe--build-readfile-stdin-exitprocess-exe))
                 ((eq spec 'createfile-write-exit-42)
                  (nelisp-pe--build-createfile-write-exitprocess-exe))
                 ((eq spec 'setfilepointer-exit-42)
                  (nelisp-pe--build-setfilepointer-exitprocess-exe))
                 ((eq spec 'getfiletype-exit-42)
                  (nelisp-pe--build-getfiletype-exitprocess-exe))
                 ((eq spec 'getfileinformation-exit-42)
                  (nelisp-pe--build-getfileinformation-exitprocess-exe))
                 ((eq spec 'getcommandline-exit-42)
                  (nelisp-pe--build-getcommandline-exitprocess-exe))
                 ((eq spec 'wsastartup-exit-42)
                  (nelisp-pe--build-wsastartup-exitprocess-exe))
                 ((eq spec 'commandlinetoargv-exit-42)
                  (nelisp-pe--build-commandlinetoargv-exitprocess-exe))
                 ((eq spec 'createprocess-wait-exit-42)
                  (nelisp-pe--build-createprocess-wait-exitprocess-exe))
                 ((eq spec 'createthread-wait-exit-42)
                  (nelisp-pe--build-createthread-wait-exitprocess-exe))
                 (t
                  (nelisp-pe--build-minimal-exitprocess-exe exit-code))))
         (coding-system-for-write 'no-conversion))
    (write-region bytes nil file-path nil 'silent)
    file-path))

(provide 'nelisp-pe-write)

;;; nelisp-pe-write.el ends here
