;;; nelisp-mach-o-write.el --- Mach-O 64-bit object writer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 100 §100.D Stage 2/3 — pure-elisp Mach-O ET_REL emitter for
;; macOS arm64 and x86_64 object files.  The public contract matches
;; the existing ELF writer's rich plist shape but emits a minimal
;; MH_OBJECT file:
;;
;;   Mach-O header (32 bytes)
;;   LC_SEGMENT_64 + embedded __text section_64
;;   LC_SYMTAB
;;   __text bytes
;;   nlist_64 symbol table
;;   string table
;;
;; Supported :machine values: `aarch64' and `x86_64'.

;;; Code:

(defconst nelisp-mach-o--mh-magic-64 #xFEEDFACF "MH_MAGIC_64.")
(defconst nelisp-mach-o--cpu-type-arm64  #x0100000C "CPU_TYPE_ARM64.")
(defconst nelisp-mach-o--cpu-type-x86-64 #x01000007 "CPU_TYPE_X86_64.")
(defconst nelisp-mach-o--cpu-subtype-arm64-all 0 "CPU_SUBTYPE_ARM64_ALL.")
(defconst nelisp-mach-o--cpu-subtype-x86-64-all 3 "CPU_SUBTYPE_X86_64_ALL.")
(defconst nelisp-mach-o--mh-object 1 "MH_OBJECT.")
(defconst nelisp-mach-o--lc-segment-64 #x19 "LC_SEGMENT_64.")
(defconst nelisp-mach-o--lc-symtab 2 "LC_SYMTAB.")
(defconst nelisp-mach-o--vm-prot-rwx 7 "VM_PROT_READ | WRITE | EXECUTE.")
(defconst nelisp-mach-o--section-text-flags #x80000400
  "__text flags = S_REGULAR | S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS.")
(defconst nelisp-mach-o--n-ext #x01 "N_EXT.")
(defconst nelisp-mach-o--n-sect #x0E "N_SECT.")

;; x86_64 relocation type constants (for future reloc-aware emission).
(defconst nelisp-mach-o--x86-64-reloc-unsigned    0 "X86_64_RELOC_UNSIGNED.")
(defconst nelisp-mach-o--x86-64-reloc-signed       1 "X86_64_RELOC_SIGNED.")
(defconst nelisp-mach-o--x86-64-reloc-branch       2 "X86_64_RELOC_BRANCH.")
(defconst nelisp-mach-o--x86-64-reloc-got-load     4 "X86_64_RELOC_GOT_LOAD.")
(defconst nelisp-mach-o--x86-64-reloc-got          5 "X86_64_RELOC_GOT.")

(defconst nelisp-mach-o--header-size 32 "sizeof(struct mach_header_64).")
(defconst nelisp-mach-o--segment-command-size 72 "sizeof(struct segment_command_64).")
(defconst nelisp-mach-o--section-size 80 "sizeof(struct section_64).")
(defconst nelisp-mach-o--symtab-command-size 24 "sizeof(struct symtab_command).")
(defconst nelisp-mach-o--nlist-64-size 16 "sizeof(struct nlist_64).")

(defun nelisp-mach-o--coerce-unibyte (str)
  "Return a unibyte copy of STR."
  (if (multibyte-string-p str)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert str)
        (buffer-substring-no-properties (point-min) (point-max)))
    str))

(defun nelisp-mach-o--write-le32 (buf v)
  "Append unsigned 32-bit V to BUF in little-endian order."
  (with-current-buffer buf
    (insert (unibyte-string (logand v #xff)
                            (logand (ash v -8) #xff)
                            (logand (ash v -16) #xff)
                            (logand (ash v -24) #xff)))))

(defun nelisp-mach-o--write-le16 (buf v)
  "Append unsigned 16-bit V to BUF in little-endian order."
  (with-current-buffer buf
    (insert (unibyte-string (logand v #xff)
                            (logand (ash v -8) #xff)))))

(defun nelisp-mach-o--write-le64 (buf v)
  "Append unsigned 64-bit V to BUF in little-endian order."
  (let ((bytes (make-vector 8 0))
        (i 0))
    (while (< i 8)
      (aset bytes i (logand (ash v (- (* i 8))) #xff))
      (setq i (1+ i)))
    (with-current-buffer buf
      (insert (apply #'unibyte-string (append bytes nil))))))

(defun nelisp-mach-o--write-bytes (buf bytes)
  "Append raw BYTES to BUF."
  (with-current-buffer buf
    (insert (nelisp-mach-o--coerce-unibyte bytes))))

(defun nelisp-mach-o--write-pad (buf nbytes)
  "Append NBYTES zero bytes to BUF."
  (with-current-buffer buf
    (insert (make-string nbytes 0))))

(defun nelisp-mach-o--write-fixed-string (buf s width)
  "Append S to BUF, NUL-padded or truncated to WIDTH bytes."
  (let* ((bytes (encode-coding-string s 'utf-8 t))
         (trimmed (if (> (length bytes) width)
                      (substring bytes 0 width)
                    bytes)))
    (nelisp-mach-o--write-bytes buf trimmed)
    (nelisp-mach-o--write-pad buf (- width (length trimmed)))))

(defun nelisp-mach-o--align-up (n align)
  "Return N rounded up to ALIGN."
  (if (<= align 1)
      n
    (* (/ (+ n (1- align)) align) align)))

(defun nelisp-mach-o--normalize-symbol-name (name)
  "Return NAME with the macOS leading underscore prefix."
  (if (string-prefix-p "_" name)
      name
    (concat "_" name)))

(defun nelisp-mach-o--symbol-type (sym)
  "Return Mach-O n_type byte for SYM."
  (let ((bind (or (plist-get sym :bind) 'local))
        (section (plist-get sym :section)))
    (unless (eq section 'text)
      (signal 'error (list "nelisp-mach-o: unsupported symbol section" section)))
    (cond
     ((eq bind 'global) (logior nelisp-mach-o--n-ext nelisp-mach-o--n-sect))
     ((eq bind 'local) nelisp-mach-o--n-sect)
     (t (signal 'error (list "nelisp-mach-o: unsupported symbol bind" bind))))))

(defun nelisp-mach-o--validate-machine (machine)
  "Validate MACHINE and return a cons (CPUTYPE . CPUSUBTYPE)."
  (pcase machine
    ('aarch64 (cons nelisp-mach-o--cpu-type-arm64
                    nelisp-mach-o--cpu-subtype-arm64-all))
    ('x86_64  (cons nelisp-mach-o--cpu-type-x86-64
                    nelisp-mach-o--cpu-subtype-x86-64-all))
    (_ (signal 'error (list :mach-o-unsupported-machine machine)))))

(defun nelisp-mach-o--verify-entry-symbol (symbols entry-sym)
  "Verify ENTRY-SYM exists in SYMBOLS when non-nil."
  (when entry-sym
    (unless
        (catch 'found
          (dolist (sym symbols)
            (when (equal (plist-get sym :name) entry-sym)
              (throw 'found t))))
      (signal 'error (list "nelisp-mach-o: :entry-sym not found" entry-sym)))))

(defun nelisp-mach-o--build-bytes (sections)
  "Build a minimal Mach-O MH_OBJECT byte image from SECTIONS."
  (let* ((text (or (plist-get sections :text)
                   (error "nelisp-mach-o: :text is required")))
         (symbols (or (plist-get sections :symbols)
                      (error "nelisp-mach-o: :symbols is required")))
         (machine (or (plist-get sections :machine)
                      (error "nelisp-mach-o: :machine is required")))
         (cpu-pair (nelisp-mach-o--validate-machine machine))
         (cpu-type    (car cpu-pair))
         (cpu-subtype (cdr cpu-pair))
         (entry-sym (plist-get sections :entry-sym))
         (ncmds 2)
         (sizeofcmds (+ nelisp-mach-o--segment-command-size
                        nelisp-mach-o--section-size
                        nelisp-mach-o--symtab-command-size))
         (text-size (length text))
         (text-off (+ nelisp-mach-o--header-size sizeofcmds))
         (symoff (nelisp-mach-o--align-up (+ text-off text-size) 8))
         (nsyms (length symbols))
         (stroff (+ symoff (* nsyms nelisp-mach-o--nlist-64-size)))
         (strtab-entries (cons (cons "" 0) nil))
         (strx-alist nil)
         (strsize 1))
    (nelisp-mach-o--verify-entry-symbol symbols entry-sym)
    (dolist (sym symbols)
      (unless (memq (or (plist-get sym :type) 'notype) '(func notype))
        (signal 'error
                (list "nelisp-mach-o: unsupported symbol type"
                      (plist-get sym :type))))
      (let* ((name (or (plist-get sym :name)
                       (error "nelisp-mach-o: symbol missing :name")))
             (mangled (nelisp-mach-o--normalize-symbol-name name)))
        (unless (assoc mangled strx-alist)
          (push (cons mangled strsize) strx-alist)
          (setq strsize (+ strsize (length (encode-coding-string mangled 'utf-8 t)) 1))
          (setq strtab-entries (append strtab-entries (list (cons mangled nil)))))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      ;; mach_header_64
      (nelisp-mach-o--write-le32 (current-buffer) nelisp-mach-o--mh-magic-64)
      (nelisp-mach-o--write-le32 (current-buffer) cpu-type)
      (nelisp-mach-o--write-le32 (current-buffer) cpu-subtype)
      (nelisp-mach-o--write-le32 (current-buffer) nelisp-mach-o--mh-object)
      (nelisp-mach-o--write-le32 (current-buffer) ncmds)
      (nelisp-mach-o--write-le32 (current-buffer) sizeofcmds)
      (nelisp-mach-o--write-le32 (current-buffer) 0)
      (nelisp-mach-o--write-le32 (current-buffer) 0)
      ;; LC_SEGMENT_64 + section_64
      (nelisp-mach-o--write-le32 (current-buffer) nelisp-mach-o--lc-segment-64)
      (nelisp-mach-o--write-le32
       (current-buffer)
       (+ nelisp-mach-o--segment-command-size nelisp-mach-o--section-size))
      (nelisp-mach-o--write-pad (current-buffer) 16)
      (nelisp-mach-o--write-le64 (current-buffer) 0)
      (nelisp-mach-o--write-le64 (current-buffer) text-size)
      (nelisp-mach-o--write-le64 (current-buffer) text-off)
      (nelisp-mach-o--write-le64 (current-buffer) text-size)
      (nelisp-mach-o--write-le32 (current-buffer) nelisp-mach-o--vm-prot-rwx)
      (nelisp-mach-o--write-le32 (current-buffer) nelisp-mach-o--vm-prot-rwx)
      (nelisp-mach-o--write-le32 (current-buffer) 1)
      (nelisp-mach-o--write-le32 (current-buffer) 0)
      (nelisp-mach-o--write-fixed-string (current-buffer) "__text" 16)
      (nelisp-mach-o--write-fixed-string (current-buffer) "__TEXT" 16)
      (nelisp-mach-o--write-le64 (current-buffer) 0)
      (nelisp-mach-o--write-le64 (current-buffer) text-size)
      (nelisp-mach-o--write-le32 (current-buffer) text-off)
      (nelisp-mach-o--write-le32 (current-buffer) 2)
      (nelisp-mach-o--write-le32 (current-buffer) 0)
      (nelisp-mach-o--write-le32 (current-buffer) 0)
      (nelisp-mach-o--write-le32 (current-buffer) nelisp-mach-o--section-text-flags)
      (nelisp-mach-o--write-le32 (current-buffer) 0)
      (nelisp-mach-o--write-le32 (current-buffer) 0)
      (nelisp-mach-o--write-le32 (current-buffer) 0)
      ;; LC_SYMTAB
      (nelisp-mach-o--write-le32 (current-buffer) nelisp-mach-o--lc-symtab)
      (nelisp-mach-o--write-le32 (current-buffer) nelisp-mach-o--symtab-command-size)
      (nelisp-mach-o--write-le32 (current-buffer) symoff)
      (nelisp-mach-o--write-le32 (current-buffer) nsyms)
      (nelisp-mach-o--write-le32 (current-buffer) stroff)
      (nelisp-mach-o--write-le32 (current-buffer) strsize)
      ;; __text
      (nelisp-mach-o--write-bytes (current-buffer) text)
      (let ((pad (- symoff (buffer-size))))
        (when (> pad 0)
          (nelisp-mach-o--write-pad (current-buffer) pad)))
      ;; nlist_64[]
      (dolist (sym symbols)
        (let* ((name (plist-get sym :name))
               (mangled (nelisp-mach-o--normalize-symbol-name name))
               (strx (cdr (assoc mangled strx-alist)))
               (value (or (plist-get sym :value) 0)))
          (nelisp-mach-o--write-le32 (current-buffer) strx)
          (insert (unibyte-string (nelisp-mach-o--symbol-type sym)))
          (insert (unibyte-string 1))
          (nelisp-mach-o--write-le16 (current-buffer) 0)
          (nelisp-mach-o--write-le64 (current-buffer) value)))
      ;; string table
      (insert (unibyte-string 0))
      (dolist (entry (nreverse strx-alist))
        (nelisp-mach-o--write-bytes (current-buffer) (car entry))
        (insert (unibyte-string 0)))
      (buffer-substring-no-properties (point-min) (point-max)))))

;;; ---- Doc 100 §100.E MH_EXECUTE — native macOS executable ----
;;
;; A hand-built no-dyld LC_UNIXTHREAD image is REJECTED by the macOS 26
;; kernel ("permission denied"); a runnable executable MUST load
;; /usr/lib/dyld and link libSystem — even though all real work is done
;; through raw `svc' syscalls.  Apple Silicon also mandates a code
;; signature, so the unsigned image emitted here is signed afterwards
;; (system `codesign -s -' for now; pure-elisp ad-hoc signing follows).
;; The exact layout replicated below is documented, byte-for-byte, in
;; tools/macos-probe/e1-blueprint.txt (e1 = a known-good `clang' build).

(defconst nelisp-mach-o--mh-execute 2 "MH_EXECUTE.")
(defconst nelisp-mach-o--exe-flags #x200085
  "MH_NOUNDEFS | MH_DYLDLINK | MH_TWOLEVEL | MH_PIE.")
(defconst nelisp-mach-o--lc-dyld-chained-fixups #x80000034 "LC_DYLD_CHAINED_FIXUPS.")
(defconst nelisp-mach-o--lc-dyld-exports-trie    #x80000033 "LC_DYLD_EXPORTS_TRIE.")
(defconst nelisp-mach-o--lc-dysymtab       #x0B "LC_DYSYMTAB.")
(defconst nelisp-mach-o--lc-load-dylinker  #x0E "LC_LOAD_DYLINKER.")
(defconst nelisp-mach-o--lc-uuid           #x1B "LC_UUID.")
(defconst nelisp-mach-o--lc-build-version  #x32 "LC_BUILD_VERSION.")
(defconst nelisp-mach-o--lc-source-version #x2A "LC_SOURCE_VERSION.")
(defconst nelisp-mach-o--lc-main           #x80000028 "LC_MAIN.")
(defconst nelisp-mach-o--lc-load-dylib     #x0C "LC_LOAD_DYLIB.")
(defconst nelisp-mach-o--lc-function-starts #x26 "LC_FUNCTION_STARTS.")
(defconst nelisp-mach-o--lc-data-in-code   #x29 "LC_DATA_IN_CODE.")
(defconst nelisp-mach-o--vm-prot-rx 5 "VM_PROT_READ | VM_PROT_EXECUTE.")
(defconst nelisp-mach-o--vm-prot-rw 3 "VM_PROT_READ | VM_PROT_WRITE.")
(defconst nelisp-mach-o--vm-prot-r  1 "VM_PROT_READ.")
(defconst nelisp-mach-o--exe-page-size-arm64  #x4000 "arm64 macOS page = 16 KiB.")
(defconst nelisp-mach-o--exe-page-size-x86-64 #x1000 "x86_64 macOS page = 4 KiB.")
(defconst nelisp-mach-o--exe-text-vmaddr #x100000000 "__TEXT vmaddr (top of __PAGEZERO).")
(defconst nelisp-mach-o--exe-code-off 728
  "File offset of __text in the MH_EXECUTE image.
Matches ld/clang; leaves >=16 bytes of slack after the load commands so
`codesign' can insert LC_CODE_SIGNATURE, and keeps the exports-trie /
symtab addresses identical to the e1 blueprint for milestone 1.")
(defconst nelisp-mach-o--exe-minos #x1A0000 "minos 26.0.0.")
(defconst nelisp-mach-o--exe-sdk   #x1A0200 "sdk 26.2.0.")

(defun nelisp-mach-o--exe-page-size (machine)
  "Return executable page size for MACHINE."
  (if (eq machine 'x86_64)
      nelisp-mach-o--exe-page-size-x86-64
    nelisp-mach-o--exe-page-size-arm64))

;; LC_DYLD_CHAINED_FIXUPS payload for an image with no dynamic binding.
;; It is header + starts_in_image with zero seg_info_offset entries.
(defun nelisp-mach-o--exe-chained-fixups (segment-count)
  "Return a no-fixups LC_DYLD_CHAINED_FIXUPS payload for SEGMENT-COUNT."
  (let* ((starts-off 32)
         (starts-size (+ 4 (* segment-count 4)))
         (imports-off (+ starts-off starts-size))
         (symbols-off imports-off))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((b (current-buffer)))
        (nelisp-mach-o--write-le32 b 0)           ; version
        (nelisp-mach-o--write-le32 b starts-off)
        (nelisp-mach-o--write-le32 b imports-off)
        (nelisp-mach-o--write-le32 b symbols-off)
        (nelisp-mach-o--write-le32 b 0)           ; imports_count
        (nelisp-mach-o--write-le32 b 1)           ; imports_format
        (nelisp-mach-o--write-le32 b 0)           ; symbols_format
        (nelisp-mach-o--write-le32 b 0)           ; pad
        (nelisp-mach-o--write-le32 b segment-count)
        (dotimes (_ segment-count)
          (nelisp-mach-o--write-le32 b 0))
        (buffer-substring-no-properties (point-min) (point-max))))))

;; 48-byte LC_DYLD_EXPORTS_TRIE exporting `__mh_execute_header' (addr 0)
;; and `_main' (addr 0x2d8 = file offset 728).  Pinned to the e1
;; blueprint for milestone 1 (valid while `--exe-code-off' = 728); a
;; general trie generator replaces this when code outgrows one page.
(defconst nelisp-mach-o--exe-exports-trie
  (apply #'unibyte-string
         '(#x00 #x01 #x5f #x00 #x12 #x00 #x00 #x00 #x00 #x02 #x00 #x00
           #x00 #x03 #x00 #xd8 #x05 #x00 #x00 #x02 #x5f #x6d #x68 #x5f
           #x65 #x78 #x65 #x63 #x75 #x74 #x65 #x5f #x68 #x65 #x61 #x64
           #x65 #x72 #x00 #x09 #x6d #x61 #x69 #x6e #x00 #x0d #x00 #x00)))

(defun nelisp-mach-o--exe-exports-trie (main-off)
  "Return exports trie bytes for `_main' at MAIN-OFF.
This keeps the proven clang-derived two-symbol trie shape and only patches the
ULEB128 terminal address for `_main'.  The layout currently requires a two-byte
ULEB, which covers every executable code offset used by this writer."
  (let ((addr (nelisp-mach-o--uleb128 main-off)))
    (unless (= (length addr) 2)
      (signal 'error (list :mach-o-exe-main-offset-uleb-size main-off)))
    (concat (substring nelisp-mach-o--exe-exports-trie 0 15)
            addr
            (substring nelisp-mach-o--exe-exports-trie 17))))

(defun nelisp-mach-o--uleb128 (n)
  "Return the ULEB128 unibyte encoding of non-negative integer N."
  (let ((out nil))
    (while (progn
             (let ((b (logand n #x7f)))
               (setq n (ash n -7))
               (push (if (> n 0) (logior b #x80) b) out))
             (> n 0)))
    (apply #'unibyte-string (nreverse out))))

(defun nelisp-mach-o--lc-linkedit-data (buf cmd dataoff datasize)
  "Emit a linkedit_data_command (CMD, DATAOFF, DATASIZE) into BUF."
  (nelisp-mach-o--write-le32 buf cmd)
  (nelisp-mach-o--write-le32 buf 16)
  (nelisp-mach-o--write-le32 buf dataoff)
  (nelisp-mach-o--write-le32 buf datasize))

(defun nelisp-mach-o--exe-uuid (text)
  "Derive a deterministic 16-byte UUID unibyte string from TEXT."
  (let* ((digest (secure-hash 'sha256 (nelisp-mach-o--coerce-unibyte text) nil nil t))
         (raw (substring digest 0 16))
         (b (vconcat raw)))
    ;; Set RFC-4122 version (4) / variant bits so tools accept it.
    (aset b 6 (logior (logand (aref b 6) #x0f) #x40))
    (aset b 8 (logior (logand (aref b 8) #x3f) #x80))
    (apply #'unibyte-string (append b nil))))

(defun nelisp-mach-o--build-executable (sections)
  "Build an UNSIGNED native macOS MH_EXECUTE image from SECTIONS.
Recognised keys: :text (required unibyte code), :data (optional writable
bytes), :bss-size (optional zero-fill bytes), :machine (`aarch64' or
`x86_64'), :entry-sym (optional, verified).  Apple Silicon output still
needs an ad-hoc code signature (`codesign -s -') before it will run."
  (let* ((text (or (plist-get sections :text)
                   (error "nelisp-mach-o: :text is required")))
         (data (nelisp-mach-o--coerce-unibyte (or (plist-get sections :data) "")))
         (bss-size (or (plist-get sections :bss-size) 0))
         (machine (or (plist-get sections :machine) 'aarch64))
         (cpu-pair (nelisp-mach-o--validate-machine machine))
         (cpu-type    (car cpu-pair))
         (cpu-subtype (cdr cpu-pair))
         (text (nelisp-mach-o--coerce-unibyte text))
         (text-size (length text))
         (data-size (length data))
         (have-data (> data-size 0))
         (have-bss (> bss-size 0))
         (have-rw (or have-data have-bss))
         (rw-nsects (+ (if have-data 1 0) (if have-bss 1 0)))
         (rw-cmd-size (if have-rw
                          (+ nelisp-mach-o--segment-command-size
                             (* rw-nsects nelisp-mach-o--section-size))
                        0))
         (ncmds (+ 15 (if have-rw 1 0)))
         (sizeofcmds (+ 648 rw-cmd-size))
         (code-off (if have-rw
                       (nelisp-mach-o--align-up
                        (+ nelisp-mach-o--header-size sizeofcmds 16)
                        16)
                     nelisp-mach-o--exe-code-off))
         (vmbase nelisp-mach-o--exe-text-vmaddr)
         (page (nelisp-mach-o--exe-page-size machine))
         (text-end (+ code-off text-size))
         (text-filesize (nelisp-mach-o--align-up text-end page))
         (data-off text-filesize)
         (data-vmaddr (+ vmbase text-filesize))
         (data-filesize (if have-data (nelisp-mach-o--align-up data-size page) 0))
         (data-vmsize (if have-rw
                          (nelisp-mach-o--align-up (+ data-size bss-size) page)
                        0))
         (linkedit-off (+ data-off data-filesize))
         (linkedit-vmaddr (+ (if have-rw data-vmaddr vmbase)
                             (if have-rw data-vmsize text-filesize)))
         ;; __LINKEDIT pieces, in e1 order.
         (fixups (nelisp-mach-o--exe-chained-fixups (if have-rw 4 3)))
         (trie (nelisp-mach-o--exe-exports-trie code-off))
         (func-starts (let ((u (nelisp-mach-o--uleb128 code-off)))
                        (concat u (make-string (- 8 (length u)) 0))))
         (fixups-off linkedit-off)
         (trie-off (+ fixups-off (length fixups)))
         (fstarts-off (+ trie-off (length trie)))
         (symoff (+ fstarts-off (length func-starts)))
         (nsyms 2)
         (stroff (+ symoff (* nsyms nelisp-mach-o--nlist-64-size)))
         (strtab (let ((s (concat " \0__mh_execute_header\0_main\0")))
                   (concat s (make-string (- (nelisp-mach-o--align-up (length s) 8)
                                             (length s)) 0))))
         (strsize (length strtab))
         (linkedit-size (- (+ stroff strsize) linkedit-off)))
    (when (> (+ nelisp-mach-o--header-size sizeofcmds 16) code-off)
      (error "nelisp-mach-o: load commands overrun code offset"))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((b (current-buffer)))
        ;; mach_header_64
        (nelisp-mach-o--write-le32 b nelisp-mach-o--mh-magic-64)
        (nelisp-mach-o--write-le32 b cpu-type)
        (nelisp-mach-o--write-le32 b cpu-subtype)
        (nelisp-mach-o--write-le32 b nelisp-mach-o--mh-execute)
        (nelisp-mach-o--write-le32 b ncmds)            ; codesign may add one more
        (nelisp-mach-o--write-le32 b sizeofcmds)
        (nelisp-mach-o--write-le32 b nelisp-mach-o--exe-flags)
        (nelisp-mach-o--write-le32 b 0)
        ;; LC_SEGMENT_64 __PAGEZERO
        (nelisp-mach-o--write-le32 b nelisp-mach-o--lc-segment-64)
        (nelisp-mach-o--write-le32 b 72)
        (nelisp-mach-o--write-fixed-string b "__PAGEZERO" 16)
        (nelisp-mach-o--write-le64 b 0)                ; vmaddr
        (nelisp-mach-o--write-le64 b vmbase)           ; vmsize (4 GiB)
        (nelisp-mach-o--write-le64 b 0)                ; fileoff
        (nelisp-mach-o--write-le64 b 0)                ; filesize
        (nelisp-mach-o--write-le32 b 0)                ; maxprot
        (nelisp-mach-o--write-le32 b 0)                ; initprot
        (nelisp-mach-o--write-le32 b 0)                ; nsects
        (nelisp-mach-o--write-le32 b 0)                ; flags
        ;; LC_SEGMENT_64 __TEXT (+ __text section)
        (nelisp-mach-o--write-le32 b nelisp-mach-o--lc-segment-64)
        (nelisp-mach-o--write-le32 b 152)
        (nelisp-mach-o--write-fixed-string b "__TEXT" 16)
        (nelisp-mach-o--write-le64 b vmbase)
        (nelisp-mach-o--write-le64 b text-filesize)    ; vmsize
        (nelisp-mach-o--write-le64 b 0)                ; fileoff
        (nelisp-mach-o--write-le64 b text-filesize)    ; filesize
        (nelisp-mach-o--write-le32 b nelisp-mach-o--vm-prot-rx)
        (nelisp-mach-o--write-le32 b nelisp-mach-o--vm-prot-rx)
        (nelisp-mach-o--write-le32 b 1)                ; nsects
        (nelisp-mach-o--write-le32 b 0)                ; flags
        ;;   section_64 __text
        (nelisp-mach-o--write-fixed-string b "__text" 16)
        (nelisp-mach-o--write-fixed-string b "__TEXT" 16)
        (nelisp-mach-o--write-le64 b (+ vmbase code-off))  ; addr
        (nelisp-mach-o--write-le64 b text-size)            ; size
        (nelisp-mach-o--write-le32 b code-off)             ; offset
        (nelisp-mach-o--write-le32 b 2)                    ; align 2^2
        (nelisp-mach-o--write-le32 b 0)                    ; reloff
        (nelisp-mach-o--write-le32 b 0)                    ; nreloc
        (nelisp-mach-o--write-le32 b nelisp-mach-o--section-text-flags)
        (nelisp-mach-o--write-le32 b 0)                    ; reserved1
        (nelisp-mach-o--write-le32 b 0)                    ; reserved2
        (nelisp-mach-o--write-le32 b 0)                    ; reserved3
        (when have-rw
          ;; LC_SEGMENT_64 __DATA (+ optional __data / __bss sections)
          (nelisp-mach-o--write-le32 b nelisp-mach-o--lc-segment-64)
          (nelisp-mach-o--write-le32 b rw-cmd-size)
          (nelisp-mach-o--write-fixed-string b "__DATA" 16)
          (nelisp-mach-o--write-le64 b data-vmaddr)
          (nelisp-mach-o--write-le64 b data-vmsize)
          (nelisp-mach-o--write-le64 b data-off)
          (nelisp-mach-o--write-le64 b data-filesize)
          (nelisp-mach-o--write-le32 b nelisp-mach-o--vm-prot-rw)
          (nelisp-mach-o--write-le32 b nelisp-mach-o--vm-prot-rw)
          (nelisp-mach-o--write-le32 b rw-nsects)
          (nelisp-mach-o--write-le32 b 0)
          (when have-data
            (nelisp-mach-o--write-fixed-string b "__data" 16)
            (nelisp-mach-o--write-fixed-string b "__DATA" 16)
            (nelisp-mach-o--write-le64 b data-vmaddr)
            (nelisp-mach-o--write-le64 b data-size)
            (nelisp-mach-o--write-le32 b data-off)
            (nelisp-mach-o--write-le32 b 3)
            (nelisp-mach-o--write-le32 b 0)
            (nelisp-mach-o--write-le32 b 0)
            (nelisp-mach-o--write-le32 b 0)
            (nelisp-mach-o--write-le32 b 0)
            (nelisp-mach-o--write-le32 b 0)
            (nelisp-mach-o--write-le32 b 0))
          (when have-bss
            (nelisp-mach-o--write-fixed-string b "__bss" 16)
            (nelisp-mach-o--write-fixed-string b "__DATA" 16)
            (nelisp-mach-o--write-le64 b (+ data-vmaddr data-size))
            (nelisp-mach-o--write-le64 b bss-size)
            (nelisp-mach-o--write-le32 b 0)
            (nelisp-mach-o--write-le32 b 3)
            (nelisp-mach-o--write-le32 b 0)
            (nelisp-mach-o--write-le32 b 0)
            (nelisp-mach-o--write-le32 b 1) ; S_ZEROFILL
            (nelisp-mach-o--write-le32 b 0)
            (nelisp-mach-o--write-le32 b 0)
            (nelisp-mach-o--write-le32 b 0)))
        ;; LC_SEGMENT_64 __LINKEDIT
        (nelisp-mach-o--write-le32 b nelisp-mach-o--lc-segment-64)
        (nelisp-mach-o--write-le32 b 72)
        (nelisp-mach-o--write-fixed-string b "__LINKEDIT" 16)
        (nelisp-mach-o--write-le64 b linkedit-vmaddr)       ; vmaddr
        (nelisp-mach-o--write-le64 b (nelisp-mach-o--align-up linkedit-size page)) ; vmsize
        (nelisp-mach-o--write-le64 b linkedit-off)         ; fileoff
        (nelisp-mach-o--write-le64 b linkedit-size)        ; filesize
        (nelisp-mach-o--write-le32 b nelisp-mach-o--vm-prot-r)
        (nelisp-mach-o--write-le32 b nelisp-mach-o--vm-prot-r)
        (nelisp-mach-o--write-le32 b 0)
        (nelisp-mach-o--write-le32 b 0)
        ;; LC_DYLD_CHAINED_FIXUPS / LC_DYLD_EXPORTS_TRIE
        (nelisp-mach-o--lc-linkedit-data b nelisp-mach-o--lc-dyld-chained-fixups
                                         fixups-off (length fixups))
        (nelisp-mach-o--lc-linkedit-data b nelisp-mach-o--lc-dyld-exports-trie
                                         trie-off (length trie))
        ;; LC_SYMTAB
        (nelisp-mach-o--write-le32 b nelisp-mach-o--lc-symtab)
        (nelisp-mach-o--write-le32 b 24)
        (nelisp-mach-o--write-le32 b symoff)
        (nelisp-mach-o--write-le32 b nsyms)
        (nelisp-mach-o--write-le32 b stroff)
        (nelisp-mach-o--write-le32 b strsize)
        ;; LC_DYSYMTAB (iextdefsym 0 / nextdefsym 2 / iundefsym 2)
        (nelisp-mach-o--write-le32 b nelisp-mach-o--lc-dysymtab)
        (nelisp-mach-o--write-le32 b 80)
        (nelisp-mach-o--write-le32 b 0) (nelisp-mach-o--write-le32 b 0)  ; ilocal nlocal
        (nelisp-mach-o--write-le32 b 0) (nelisp-mach-o--write-le32 b 2)  ; iextdef nextdef
        (nelisp-mach-o--write-le32 b 2) (nelisp-mach-o--write-le32 b 0)  ; iundef nundef
        (dotimes (_ 12) (nelisp-mach-o--write-le32 b 0))                 ; toc..nlocrel
        ;; LC_LOAD_DYLINKER /usr/lib/dyld
        (nelisp-mach-o--write-le32 b nelisp-mach-o--lc-load-dylinker)
        (nelisp-mach-o--write-le32 b 32)
        (nelisp-mach-o--write-le32 b 12)                ; name offset
        (nelisp-mach-o--write-fixed-string b "/usr/lib/dyld" 20)
        ;; LC_UUID
        (nelisp-mach-o--write-le32 b nelisp-mach-o--lc-uuid)
        (nelisp-mach-o--write-le32 b 24)
        (nelisp-mach-o--write-bytes b (nelisp-mach-o--exe-uuid text))
        ;; LC_BUILD_VERSION (platform macOS, 1 tool)
        (nelisp-mach-o--write-le32 b nelisp-mach-o--lc-build-version)
        (nelisp-mach-o--write-le32 b 32)
        (nelisp-mach-o--write-le32 b 1)                 ; PLATFORM_MACOS
        (nelisp-mach-o--write-le32 b nelisp-mach-o--exe-minos)
        (nelisp-mach-o--write-le32 b nelisp-mach-o--exe-sdk)
        (nelisp-mach-o--write-le32 b 1)                 ; ntools
        (nelisp-mach-o--write-le32 b 3)                 ; TOOL_LD
        (nelisp-mach-o--write-le32 b #x04F20800)        ; tool version 1266.8.0
        ;; LC_SOURCE_VERSION
        (nelisp-mach-o--write-le32 b nelisp-mach-o--lc-source-version)
        (nelisp-mach-o--write-le32 b 16)
        (nelisp-mach-o--write-le64 b 0)
        ;; LC_MAIN (entryoff = code offset)
        (nelisp-mach-o--write-le32 b nelisp-mach-o--lc-main)
        (nelisp-mach-o--write-le32 b 24)
        (nelisp-mach-o--write-le64 b code-off)          ; entryoff
        (nelisp-mach-o--write-le64 b 0)                 ; stacksize
        ;; LC_LOAD_DYLIB /usr/lib/libSystem.B.dylib
        (nelisp-mach-o--write-le32 b nelisp-mach-o--lc-load-dylib)
        (nelisp-mach-o--write-le32 b 56)
        (nelisp-mach-o--write-le32 b 24)                ; name offset
        (nelisp-mach-o--write-le32 b 2)                 ; timestamp
        (nelisp-mach-o--write-le32 b #x054C0000)        ; current version 1356.0.0
        (nelisp-mach-o--write-le32 b #x00010000)        ; compat version 1.0.0
        (nelisp-mach-o--write-fixed-string b "/usr/lib/libSystem.B.dylib" 32)
        ;; LC_FUNCTION_STARTS / LC_DATA_IN_CODE
        (nelisp-mach-o--lc-linkedit-data b nelisp-mach-o--lc-function-starts
                                         fstarts-off (length func-starts))
        (nelisp-mach-o--lc-linkedit-data b nelisp-mach-o--lc-data-in-code
                                         symoff 0)
        ;; pad header+commands up to the code offset
        (let ((pad (- code-off (buffer-size))))
          (when (< pad 0) (error "nelisp-mach-o: command region overflow"))
          (nelisp-mach-o--write-pad b pad))
        ;; __text + page padding
        (nelisp-mach-o--write-bytes b text)
        (nelisp-mach-o--write-pad b (- text-filesize (buffer-size)))
        ;; __DATA file-backed payload, if present.  __bss is zero-fill only.
        (when have-data
          (nelisp-mach-o--write-bytes b data)
          (nelisp-mach-o--write-pad b (- (+ data-off data-filesize)
                                         (buffer-size))))
        ;; __LINKEDIT
        (nelisp-mach-o--write-bytes b fixups)
        (nelisp-mach-o--write-bytes b trie)
        (nelisp-mach-o--write-bytes b func-starts)
        ;; nlist_64[]: __mh_execute_header, _main
        (nelisp-mach-o--write-le32 b 2)                 ; n_strx
        (insert (unibyte-string #x0f 1)) (nelisp-mach-o--write-le16 b #x10)
        (nelisp-mach-o--write-le64 b vmbase)
        (nelisp-mach-o--write-le32 b 22)
        (insert (unibyte-string #x0f 1)) (nelisp-mach-o--write-le16 b 0)
        (nelisp-mach-o--write-le64 b (+ vmbase code-off))
        (nelisp-mach-o--write-bytes b strtab)
        (buffer-substring-no-properties (point-min) (point-max))))))

;;;###autoload
(defun nelisp-mach-o-write-executable (file-path sections)
  "Emit an UNSIGNED native macOS MH_EXECUTE to FILE-PATH.
SECTIONS keys: :text (unibyte code, required), :machine (`aarch64' or
`x86_64'), :entry-sym (optional).  Sign arm64 output with `codesign -s -'
on macOS before running.  See `nelisp-mach-o--build-executable'."
  (let ((bytes (nelisp-mach-o--build-executable sections))
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil file-path nil 'silent)
    file-path))

;;;###autoload
(defun nelisp-mach-o-write-binary (file-path sections)
  "Emit a Mach-O 64-bit MH_OBJECT file to FILE-PATH from SECTIONS.
SECTIONS matches the ELF writer's plist contract for the ET_REL case:
  :text       unibyte instruction bytes (required)
  :symbols    list of symbol plists (required)
  :machine    `aarch64' or `x86_64' (required)
  :entry-sym  optional symbol name used only for verification"
  (let ((bytes (nelisp-mach-o--build-bytes sections))
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil file-path nil 'silent)
    file-path))

(provide 'nelisp-mach-o-write)

;;; nelisp-mach-o-write.el ends here
