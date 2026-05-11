;;; nelisp-mach-o-write.el --- Mach-O 64-bit object writer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 100 §100.D Stage 2/3 — pure-elisp Mach-O ET_REL emitter for
;; macOS-arm64 object files.  The public contract matches the existing
;; ELF writer's rich plist shape but emits a minimal MH_OBJECT file:
;;
;;   Mach-O header (32 bytes)
;;   LC_SEGMENT_64 + embedded __text section_64
;;   LC_SYMTAB
;;   __text bytes
;;   nlist_64 symbol table
;;   string table

;;; Code:

(defconst nelisp-mach-o--mh-magic-64 #xFEEDFACF "MH_MAGIC_64.")
(defconst nelisp-mach-o--cpu-type-arm64 #x0100000C "CPU_TYPE_ARM64.")
(defconst nelisp-mach-o--cpu-subtype-arm64-all 0 "CPU_SUBTYPE_ARM64_ALL.")
(defconst nelisp-mach-o--mh-object 1 "MH_OBJECT.")
(defconst nelisp-mach-o--lc-segment-64 #x19 "LC_SEGMENT_64.")
(defconst nelisp-mach-o--lc-symtab 2 "LC_SYMTAB.")
(defconst nelisp-mach-o--vm-prot-rwx 7 "VM_PROT_READ | WRITE | EXECUTE.")
(defconst nelisp-mach-o--section-text-flags #x80000400
  "__text flags = S_REGULAR | S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS.")
(defconst nelisp-mach-o--n-ext #x01 "N_EXT.")
(defconst nelisp-mach-o--n-sect #x0E "N_SECT.")

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
  "Validate MACHINE and return the Mach-O cputype."
  (if (eq machine 'aarch64)
      nelisp-mach-o--cpu-type-arm64
    (signal 'error (list :mach-o-unsupported-machine machine))))

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
         (_cpu-type (nelisp-mach-o--validate-machine machine))
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
      (nelisp-mach-o--write-le32 (current-buffer) nelisp-mach-o--cpu-type-arm64)
      (nelisp-mach-o--write-le32 (current-buffer) nelisp-mach-o--cpu-subtype-arm64-all)
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

;;;###autoload
(defun nelisp-mach-o-write-binary (file-path sections)
  "Emit a Mach-O 64-bit MH_OBJECT file to FILE-PATH from SECTIONS.
SECTIONS matches the ELF writer's plist contract for the ET_REL case:
  :text       unibyte instruction bytes (required)
  :symbols    list of symbol plists (required)
  :machine    must be `aarch64' (required)
  :entry-sym  optional symbol name used only for verification"
  (let ((bytes (nelisp-mach-o--build-bytes sections))
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil file-path nil 'silent)
    file-path))

(provide 'nelisp-mach-o-write)

;;; nelisp-mach-o-write.el ends here
