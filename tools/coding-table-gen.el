;;; coding-table-gen.el --- Generate JIS encoding tables (Phase 7.4.5 real)  -*- lexical-binding: t; -*-

;; Phase 7.4.5 (Doc 31 v2 LOCKED 2026-04-25 §3.3 / §6.10 / §7.2) — Real
;; implementation of the generator that emits
;; `src/nelisp-coding-jis-tables.el' from the canonical Microsoft +
;; Unicode mapping files.
;;
;; T25 (Phase 7.4.3) shipped a partial 885-entry MVP table + a skeleton
;; generator. T34 (this Phase 7.4.5 sub-task) replaces the skeleton with
;; a real downloader / parser / writer that emits the full ~14000 entry
;; table with the corresponding golden SHA-256 hash.
;;
;; Usage:
;;   emacs --batch -L tools -l coding-table-gen.el \
;;         -f nelisp-coding-table-gen-batch
;;
;; Output:
;;   src/nelisp-coding-jis-tables.el (overwritten with full tables)
;;   tools/.cache/CP932.TXT          (downloaded if absent)
;;   tools/.cache/JIS0212.TXT        (downloaded if absent)
;;
;; Provenance / source-of-truth (see docs/coding-table-provenance.org):
;;   - Microsoft CP932.TXT (ASCII + JIS X 0201 + JIS X 0208 + CP932 ext)
;;     https://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP932.TXT
;;   - JIS X 0212 supplementary kanji table (JIS X 0212-1990 standard)
;;     https://unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/JIS0212.TXT
;;
;; Both files are mirrored to `tools/.cache/' on first run and reused
;; offline thereafter; this lets the generator run without network access
;; once the cache is populated (and lets us commit the cached copies for
;; CI reproducibility).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)

;;;; URLs / paths

(defvar nelisp-coding-table-gen-cp932-url
  "https://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP932.TXT"
  "Canonical Microsoft CP932 mapping file URL.
This is the source-of-truth for Shift-JIS + CP932 extension Unicode mapping.")

(defvar nelisp-coding-table-gen-jis0212-url
  "https://unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/JIS0212.TXT"
  "Canonical JIS X 0212-1990 supplementary kanji table URL.
This table covers ~6000 supplementary kanji not in JIS X 0208.")

(defvar nelisp-coding-table-gen-output-path
  "src/nelisp-coding-jis-tables.el"
  "Output path for the regenerated tables file (project-relative).")

(defvar nelisp-coding-table-gen-cache-dir
  "tools/.cache/"
  "Cache directory for downloaded mapping files (project-relative).")

;;;; Range definitions (CP932 split into JIS X 0208 base vs CP932 extension)
;;
;; JIS X 0208 main = 0x8140-0x84BE (rows 1-8)
;;                  + 0x889F-0x9FFC (rows 16-47)
;;                  + 0xE040-0xEAA4 (rows 48-83)
;; CP932 extension = 0x8740-0x879C (NEC special, rows 13-19)
;;                  + 0xED40-0xEEFC (NEC selected IBM, rows 89-92)
;;                  + 0xFA40-0xFC4B (IBM extension, rows 115-119)
;;
;; ASCII (0x00-0x7F) + JIS X 0201 katakana (0xA1-0xDF) are handled
;; algorithmically by `nelisp-coding.el' and not stored in the tables.

(defun nelisp-coding-table-gen--cp932-x0208-p (code)
  "Return non-nil if CODE is in a JIS X 0208 main block."
  (or (and (>= code #x8140) (<= code #x84BE))
      (and (>= code #x889F) (<= code #x9FFC))
      (and (>= code #xE040) (<= code #xEAA4))))

(defun nelisp-coding-table-gen--cp932-extension-p (code)
  "Return non-nil if CODE is in a CP932 vendor extension block."
  (or (and (>= code #x8740) (<= code #x879C))
      (and (>= code #xED40) (<= code #xEEFC))
      (and (>= code #xFA40) (<= code #xFC4B))))

;;;; Download / cache helpers

(defun nelisp-coding-table-gen--ensure-cache (url cache-path)
  "Ensure CACHE-PATH exists; download from URL if not.
Return CACHE-PATH on success.  Signals `error' if both the cache is
missing and the download fails."
  (unless (file-exists-p cache-path)
    (let ((dir (file-name-directory cache-path)))
      (when dir (make-directory dir t)))
    (message "Downloading %s -> %s" url cache-path)
    (condition-case err
        (url-copy-file url cache-path nil)
      (error
       (signal 'error
               (list (format "Cache miss + download failed: %s (URL %s)"
                             (error-message-string err)
                             url))))))
  cache-path)

(defun nelisp-coding-table-gen--read-text (path)
  "Read PATH and return its contents as a unibyte string (no decoding)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-string)))

;;;; Parsers

(defconst nelisp-coding-table-gen--data-line-re
  "\\`0x\\([0-9A-Fa-f]+\\)\\s-+0x\\([0-9A-Fa-f]+\\)"
  "Regex matching a `0xCODE\\t0xUNICODE' data line.
Capture group 1 = source code (hex); group 2 = Unicode codepoint (hex).
Lines whose 2nd column is `#UNDEFINED' / spaces (no 0x prefix) do not
match and are skipped by the parser.")

(defun nelisp-coding-table-gen--parse-cp932-line (line)
  "Parse one line of CP932.TXT format and return (CP932-INT . CODEPOINT) or nil.

CP932.TXT format example:
  0x20\\t0x0020\\t# SPACE
  0x8140\\t0x3000\\t# IDEOGRAPHIC SPACE

Skips comment-only lines (starting with `#') and `#UNDEFINED' rows
(reserved CP932 codes that map to nothing)."
  (when (string-match nelisp-coding-table-gen--data-line-re line)
    (cons (string-to-number (match-string 1 line) 16)
          (string-to-number (match-string 2 line) 16))))

(defun nelisp-coding-table-gen--parse-jis0212-line (line)
  "Parse one line of JIS0212.TXT format and return (EUC-INT . CODEPOINT) or nil.

JIS0212.TXT format example:
  0x222F\\t0x02D8\\t# BREVE

The first column is the X 0212 row+cell as a 16-bit JIS code (no high
bit).  This function rewrites the code into EUC-JP CS3 form by setting
the high bit on each byte:

  EUC-INT = ((row | 0x80) << 8) | (cell | 0x80)

The implicit 0x8F single-shift prefix is added at codec time, not stored
in the table."
  (when (string-match nelisp-coding-table-gen--data-line-re line)
    (let* ((jis (string-to-number (match-string 1 line) 16))
           (cp  (string-to-number (match-string 2 line) 16))
           (row  (logand (ash jis -8) #xFF))
           (cell (logand jis #xFF))
           (euc (logior (ash (logior row #x80) 8)
                        (logior cell #x80))))
      (cons euc cp))))

(defun nelisp-coding-table-gen--parse-text (text parse-fn)
  "Apply PARSE-FN to each line of TEXT, returning the list of non-nil results."
  (let ((results '()))
    (dolist (line (split-string text "\n" nil))
      (let ((parsed (funcall parse-fn line)))
        (when parsed (push parsed results))))
    (nreverse results)))

;;;; SJIS → JIS row/cell helper (used to derive EUC-JP X 0208 from CP932)

(defun nelisp-coding-table-gen--sjis-to-jis (sjis)
  "Convert SJIS 16-bit code to a (JIS-ROW . JIS-CELL) pair (no high bits set).

Returns nil if SJIS is not a valid JIS X 0208 main-block code (the CP932
extension blocks 0x8740-0x879C / 0xED40-0xEEFC / 0xFA40-0xFC4B do *not*
have JIS X 0208 row/cell coordinates and yield nil here).

Algorithm (canonical JIS X 0208 / Shift-JIS conversion):
  lead in 0x81-0x9F:  c1 = (lead - 0x70) * 2  [-1 if trail<0x9F]
  lead in 0xE0-0xEF:  c1 = (lead - 0xB0) * 2  [-1 if trail<0x9F]
  trail < 0x7F:       c2 = trail - 0x3F      ; 0x40..0x7E -> 0x01..0x3F? no
  ...
Full canonical formula used here matches Wikipedia / WHATWG spec."
  (let ((lead  (logand (ash sjis -8) #xFF))
        (trail (logand sjis #xFF)))
    (when (or (and (>= lead #x81) (<= lead #x9F))
              (and (>= lead #xE0) (<= lead #xEF)))
      (when (and (>= trail #x40) (<= trail #xFC) (/= trail #x7F))
        (let* ((c1-base (if (<= lead #x9F)
                            (* 2 (- lead #x70))
                          (* 2 (- lead #xB0))))
               (c1 (if (< trail #x9F) (1- c1-base) c1-base))
               (c2 (cond
                    ((< trail #x7F) (- trail #x3F)) ; 0x40-0x7E -> 0x01-0x3F
                    ((< trail #x9F) (- trail #x40)) ; 0x80-0x9E -> 0x40-0x5E
                    (t (- trail #x9E)))))           ; 0x9F-0xFC -> 0x01-0x5E
          ;; Re-bias c2 to JIS row/cell coordinate space (1-94 → 0x21-0x7E):
          (cons c1 (+ c2 #x20)))))))

(defun nelisp-coding-table-gen--jis-to-euc-int (row cell)
  "Convert JIS (ROW . CELL) — both in 0x21-0x7E — into EUC-INT 0xA1A1-0xFEFE."
  (logior (ash (logior row #x80) 8) (logior cell #x80)))

;;;; Splitters

(defun nelisp-coding-table-gen--split-cp932 (pairs)
  "Split CP932 PAIRS (alist (CP932-INT . CODEPOINT)) by encoding domain.

Returns plist:
  :x0208      → entries in the JIS X 0208 main blocks
  :extension  → entries in the CP932 vendor extension blocks
  :ignored    → ASCII (<0x80) + JIS X 0201 katakana (0xA1-0xDF) entries
                (handled algorithmically by nelisp-coding.el, not in tables)."
  (let (x0208 extension ignored)
    (dolist (entry pairs)
      (let ((code (car entry)))
        (cond
         ((< code #x80)
          (push entry ignored))
         ((and (>= code #xA1) (<= code #xDF))
          ;; Single-byte JIS X 0201 katakana (0xA1-0xDF maps to U+FF61-U+FF9F).
          (push entry ignored))
         ((nelisp-coding-table-gen--cp932-x0208-p code)
          (push entry x0208))
         ((nelisp-coding-table-gen--cp932-extension-p code)
          (push entry extension))
         (t
          ;; Anything outside the documented ranges: leave in :ignored
          ;; (we expect this set to be empty for current CP932.TXT).
          (push entry ignored)))))
    (list :x0208     (nreverse x0208)
          :extension (nreverse extension)
          :ignored   (nreverse ignored))))

(defun nelisp-coding-table-gen--derive-euc-jp-x0208 (sjis-x0208-pairs)
  "Convert SJIS X 0208 PAIRS to EUC-JP X 0208 (EUC-INT . CODEPOINT) pairs.

SJIS-X0208-PAIRS = alist of (SJIS-INT . CODEPOINT) for X 0208 base only
(no CP932 vendor extension; those don't have EUC equivalents)."
  (let (out)
    (dolist (entry sjis-x0208-pairs)
      (let* ((sjis (car entry))
             (cp   (cdr entry))
             (jis  (nelisp-coding-table-gen--sjis-to-jis sjis)))
        (when jis
          (push (cons (nelisp-coding-table-gen--jis-to-euc-int (car jis) (cdr jis))
                      cp)
                out))))
    ;; Sort by EUC-INT ascending for stable output.
    (sort (nreverse out) (lambda (a b) (< (car a) (car b))))))

;;;; Sorter / dedupe

(defun nelisp-coding-table-gen--sort-and-dedupe (pairs)
  "Sort PAIRS by car ascending and drop later-occurring duplicates of the same car."
  (let ((sorted (sort (copy-sequence pairs)
                      (lambda (a b) (< (car a) (car b)))))
        (last-key nil)
        (out '()))
    (dolist (e sorted)
      (unless (and last-key (= (car e) last-key))
        (push e out)
        (setq last-key (car e))))
    (nreverse out)))

;;;; Writer

(defconst nelisp-coding-table-gen--file-header
  ";;; nelisp-coding-jis-tables.el --- JIS tables (full, generated)  -*- lexical-binding: t; -*-

;; Phase 7.4.5 (Doc 31 v2 LOCKED 2026-04-25 §3.3 / §6.10 / §7.2) —
;; Shift-JIS / CP932 / EUC-JP (JIS X 0208 + JIS X 0212) ↔ Unicode mapping
;; tables, full generation.
;;
;; *** GENERATED FILE — DO NOT EDIT BY HAND ***
;;
;; This file is regenerated by `tools/coding-table-gen.el'.  Edits made
;; here are overwritten at the next batch run.  See also
;; `docs/coding-table-provenance.org' for the source-of-truth + golden
;; hash protocol.
;;
;; *** Schema ***
;;
;; Each `defconst' is an alist of `(SOURCE-INT . CODEPOINT)' cons cells:
;; - SOURCE-INT for Shift-JIS / CP932  = (lead << 8) | trail   (16-bit)
;; - SOURCE-INT for EUC-JP X 0208      = (lead << 8) | trail   (16-bit; lead 0xA1-0xFE)
;; - SOURCE-INT for EUC-JP X 0212      = (lead << 8) | trail   (16-bit; the 0x8F
;;                                       prefix is implicit and added by the codec)
;; - CODEPOINT                         = Unicode codepoint
;;
;; All tables are sorted by SOURCE-INT ascending; this allows binary
;; search or hash-table promotion.
;;
;; *** Provenance + Golden Hash ***
;;
;; - Source-of-truth: see `docs/coding-table-provenance.org'
;; - Golden SHA-256 hash check: `nelisp-coding-jis-tables-sha256' constant
;;   below; verified by `nelisp-coding-jis-tables-verify-hash' at runtime
;;   per Doc 31 v2 §6.10 + §7.2 (CI で改竄検出).

;;; Code:

"
  "Header emitted at the top of the generated tables file.")

(defun nelisp-coding-table-gen--emit-defconst (out name pairs docstring)
  "Emit a single `(defconst NAME PAIRS DOCSTRING)' form into buffer OUT.

PAIRS = alist of (SOURCE-INT . CODEPOINT) cons cells, already sorted
ascending by SOURCE-INT.  The output uses `(#xLEAD . #xCP)' hex notation
(one entry per line) for diff-friendliness."
  (with-current-buffer out
    (insert (format "(defconst %s\n  '(\n" name))
    (dolist (entry pairs)
      (insert (format "    (#x%04X . #x%04X)\n" (car entry) (cdr entry))))
    (insert "    )\n")
    (insert "  ")
    (prin1 docstring out)
    (insert ")\n\n")))

(defun nelisp-coding-table-gen--canonical-bytes (sjis-x0208 cp932-ext eucjp-x0208 eucjp-x0212)
  "Mirror of `nelisp-coding-jis-tables--canonical-bytes' (in the runtime).

Returns the canonical concatenation used to compute the golden SHA-256
hash, in declaration order of the four defconsts."
  (concat (prin1-to-string sjis-x0208)
          (prin1-to-string cp932-ext)
          (prin1-to-string eucjp-x0208)
          (prin1-to-string eucjp-x0212)))

(defun nelisp-coding-table-gen--write-tables (output-path tables)
  "Write OUTPUT-PATH with the four regenerated defconsts + golden hash.

TABLES is a plist:
  :x0208            → SJIS X 0208 alist
  :cp932-extension  → CP932 vendor extension alist
  :euc-jp-x0208     → EUC-JP X 0208 alist
  :euc-jp-x0212     → EUC-JP X 0212 alist

All four lists must already be sorted by SOURCE-INT ascending."
  (let* ((sjis (plist-get tables :x0208))
         (ext  (plist-get tables :cp932-extension))
         (eucp (plist-get tables :euc-jp-x0208))
         (euc12 (plist-get tables :euc-jp-x0212))
         (golden (secure-hash 'sha256
                              (nelisp-coding-table-gen--canonical-bytes
                               sjis ext eucp euc12))))
    (with-temp-buffer
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert nelisp-coding-table-gen--file-header)
      (nelisp-coding-table-gen--emit-defconst
       (current-buffer)
       "nelisp-coding-shift-jis-x0208-decode-table"
       sjis
       (format "Shift-JIS (JIS X 0208) → Unicode mapping table.

Entry count = %d (full generated table).
Schema = alist of (SJIS-INT . CP) cons cells, where
SJIS-INT = (lead << 8) | trail.  Sorted by SJIS-INT ascending.

Generated by:  tools/coding-table-gen.el
Provenance:    docs/coding-table-provenance.org
See also:      `nelisp-coding-shift-jis-decode' /
               `nelisp-coding-shift-jis-encode'."
               (length sjis)))
      (nelisp-coding-table-gen--emit-defconst
       (current-buffer)
       "nelisp-coding-cp932-extension-decode-table"
       ext
       (format "CP932 (Windows-31J) vendor extension → Unicode mapping table.

CP932 ext = NEC special (0x8740-0x879C) + NEC selected IBM (0xED40-0xEEFC)
            + IBM extension (0xFA40-0xFC4B).
Entry count = %d (full generated table).

Schema = alist of (SJIS-INT . CODEPOINT) cons cells.  CP932 ext entries
merge with JIS X 0208 entries during decode lookup, but are kept in a
separate defconst for provenance + audit clarity (NEC / IBM mapping =
Microsoft CP932 vendor-specific extension).

Provenance: docs/coding-table-provenance.org §1 CP932 extension."
               (length ext)))
      (nelisp-coding-table-gen--emit-defconst
       (current-buffer)
       "nelisp-coding-euc-jp-x0208-decode-table"
       eucp
       (format "EUC-JP (JIS X 0208) → Unicode mapping table.

Entry count = %d (full generated table).

Schema = alist of (EUC-INT . CODEPOINT) cons cells, where
EUC-INT = (lead << 8) | trail, with both lead/trail in
0xA1-0xFE range (= 2-byte CS1 codeset).

Derived algorithmically from the Shift-JIS X 0208 table via the canonical
SJIS → JIS row/cell transform; CP932 vendor extensions are *not* mapped
here (they don't have EUC-JP equivalents).

Provenance: docs/coding-table-provenance.org §2 EUC-JP X 0208."
               (length eucp)))
      (nelisp-coding-table-gen--emit-defconst
       (current-buffer)
       "nelisp-coding-euc-jp-x0212-decode-table"
       euc12
       (format "EUC-JP (JIS X 0212 supplementary) → Unicode mapping table.

JIS X 0212 = not encodable in Shift-JIS; EUC-JP only (CS3 codeset,
3-byte sequence: 0x8F + lead + trail).

Entry count = %d (full generated table).

Schema = alist of (EUC-INT . CODEPOINT) cons cells, where
EUC-INT = (lead << 8) | trail (= the 0x8F prefix is implicit
and added by the codec at encode time).

Provenance: docs/coding-table-provenance.org §3 JIS X 0212."
               (length euc12)))
      (insert (format "(defconst nelisp-coding-jis-tables-sha256\n  %S\n  \"Golden SHA-256 hash of the four JIS tables, concatenated and
prin1-formatted in declaration order:

  1. `nelisp-coding-shift-jis-x0208-decode-table'
  2. `nelisp-coding-cp932-extension-decode-table'
  3. `nelisp-coding-euc-jp-x0208-decode-table'
  4. `nelisp-coding-euc-jp-x0212-decode-table'

CI で改竄検出 (Doc 31 v2 §6.10 + §7.2).
Re-computed automatically by tools/coding-table-gen.el at every run.

Compute / verify with `nelisp-coding-jis-tables-verify-hash'.\")\n\n"
                      golden))
      (insert "(defun nelisp-coding-jis-tables--canonical-bytes ()\n")
      (insert "  \"Return the canonical byte sequence used to compute the golden hash.\n")
      (insert "This is the prin1-formatted concatenation of the four tables in\n")
      (insert "declaration order.  Encapsulated as a function so the same canonical\n")
      (insert "serialization is used at table-write time (in `tools/coding-table-gen.el')\n")
      (insert "and at runtime verification.\"\n")
      (insert "  (concat\n")
      (insert "   (prin1-to-string nelisp-coding-shift-jis-x0208-decode-table)\n")
      (insert "   (prin1-to-string nelisp-coding-cp932-extension-decode-table)\n")
      (insert "   (prin1-to-string nelisp-coding-euc-jp-x0208-decode-table)\n")
      (insert "   (prin1-to-string nelisp-coding-euc-jp-x0212-decode-table)))\n\n")
      (insert "(defun nelisp-coding-jis-tables-verify-hash ()\n")
      (insert "  \"Compute current table content SHA-256 and compare with the golden hash.\n\n")
      (insert "Returns t on match.  On mismatch, signals `nelisp-coding-table-corruption'\n")
      (insert "with data plist `(:expected GOLDEN :actual COMPUTED)'.\n\n")
      (insert "The check guards against accidental table mutation (= a rebase / merge\n")
      (insert "that silently rewrites entries).  Per Doc 31 v2 §6.10 + §7.2, this hash is\n")
      (insert "treated as part of the file contract and must be re-computed whenever the\n")
      (insert "table content changes (typically by re-running `tools/coding-table-gen.el').\"\n")
      (insert "  (let ((computed (secure-hash 'sha256\n")
      (insert "                               (nelisp-coding-jis-tables--canonical-bytes))))\n")
      (insert "    (if (equal computed nelisp-coding-jis-tables-sha256)\n")
      (insert "        t\n")
      (insert "      (signal 'nelisp-coding-table-corruption\n")
      (insert "              (list :expected nelisp-coding-jis-tables-sha256\n")
      (insert "                    :actual computed)))))\n\n")
      (insert "(provide 'nelisp-coding-jis-tables)\n\n")
      (insert ";;; nelisp-coding-jis-tables.el ends here\n")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) output-path)))
    golden))

;;;; Batch entry-point

;;;###autoload
(defun nelisp-coding-table-gen-batch ()
  "Batch entry-point for table regeneration.

Flow:
  1. Ensure tools/.cache/CP932.TXT  (download if absent).
  2. Ensure tools/.cache/JIS0212.TXT (download if absent).
  3. Parse both files.
  4. Split CP932 entries into JIS X 0208 base + CP932 vendor extension.
  5. Derive EUC-JP X 0208 from the JIS X 0208 base via canonical
     SJIS → JIS row/cell transform.
  6. Use JIS0212 directly for EUC-JP X 0212 (with EUC-INT high-bit
     normalisation in the parser).
  7. Sort + dedupe each table.
  8. Write src/nelisp-coding-jis-tables.el with new defconsts + golden
     SHA-256 hash.
  9. Print summary statistics + computed golden hash to stdout."
  (interactive)
  (let* ((cache-dir (expand-file-name nelisp-coding-table-gen-cache-dir
                                      default-directory))
         (cp932-path (expand-file-name "CP932.TXT" cache-dir))
         (jis0212-path (expand-file-name "JIS0212.TXT" cache-dir))
         (output-path (expand-file-name nelisp-coding-table-gen-output-path
                                        default-directory)))
    (make-directory cache-dir t)
    (nelisp-coding-table-gen--ensure-cache
     nelisp-coding-table-gen-cp932-url cp932-path)
    (nelisp-coding-table-gen--ensure-cache
     nelisp-coding-table-gen-jis0212-url jis0212-path)
    (let* ((cp932-text (nelisp-coding-table-gen--read-text cp932-path))
           (jis0212-text (nelisp-coding-table-gen--read-text jis0212-path))
           (cp932-pairs (nelisp-coding-table-gen--parse-text
                         cp932-text #'nelisp-coding-table-gen--parse-cp932-line))
           (jis0212-pairs (nelisp-coding-table-gen--parse-text
                           jis0212-text #'nelisp-coding-table-gen--parse-jis0212-line))
           (split (nelisp-coding-table-gen--split-cp932 cp932-pairs))
           (sjis-x0208 (nelisp-coding-table-gen--sort-and-dedupe
                        (plist-get split :x0208)))
           (cp932-ext  (nelisp-coding-table-gen--sort-and-dedupe
                        (plist-get split :extension)))
           (eucjp-x0208 (nelisp-coding-table-gen--derive-euc-jp-x0208 sjis-x0208))
           (eucjp-x0212 (nelisp-coding-table-gen--sort-and-dedupe jis0212-pairs))
           (golden (nelisp-coding-table-gen--write-tables
                    output-path
                    `(:x0208 ,sjis-x0208
                      :cp932-extension ,cp932-ext
                      :euc-jp-x0208 ,eucjp-x0208
                      :euc-jp-x0212 ,eucjp-x0212))))
      (princ (format "Generated %s\n" output-path))
      (princ (format "  shift-jis-x0208-decode-table:  %5d entries\n" (length sjis-x0208)))
      (princ (format "  cp932-extension-decode-table:  %5d entries\n" (length cp932-ext)))
      (princ (format "  euc-jp-x0208-decode-table:     %5d entries\n" (length eucjp-x0208)))
      (princ (format "  euc-jp-x0212-decode-table:     %5d entries\n" (length eucjp-x0212)))
      (princ (format "  total:                         %5d entries\n"
                     (+ (length sjis-x0208) (length cp932-ext)
                        (length eucjp-x0208) (length eucjp-x0212))))
      (princ (format "  golden SHA-256: %s\n" golden))
      0)))

(provide 'nelisp-coding-table-gen)

;;; coding-table-gen.el ends here
