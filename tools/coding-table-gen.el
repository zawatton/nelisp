;;; coding-table-gen.el --- Generate JIS encoding tables (Phase 7.5 stub)  -*- lexical-binding: t; -*-

;; Phase 7.4.3 (Doc 31 v2 LOCKED 2026-04-25 §6.10) — Generator skeleton
;; for `src/nelisp-coding-jis-tables.el'. Phase 7.4.3 ships the partial
;; MVP table (~885 entries, hand-derived via host Emacs `decode-coding-string'
;; under cp932/euc-jp). The full ~14000 entry generation from canonical
;; sources is deferred to Phase 7.5 and lives below as a documented stub
;; (= API surface frozen, real download/parse pending).
;;
;; Usage (Phase 7.5 once implemented):
;;   emacs --batch -L tools -l coding-table-gen.el \
;;         -f nelisp-coding-table-gen-batch
;;
;; Output:
;;   src/nelisp-coding-jis-tables.el (regenerated)
;;
;; Provenance / source-of-truth (see docs/coding-table-provenance.org):
;;   - Microsoft CP932.TXT (ASCII + JIS X 0201 + JIS X 0208 + CP932 ext)
;;     https://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP932.TXT
;;   - JIS X 0212 supplementary kanji table (JIS X 0212-1990 standard)
;;     https://unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/JIS0212.TXT

;;; Code:

(require 'cl-lib)

(defvar nelisp-coding-table-gen-cp932-url
  "https://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP932.TXT"
  "Canonical Microsoft CP932 mapping file URL.
Mirrored under Unicode Consortium public hosting; this is the
source-of-truth for Shift-JIS + CP932 extension Unicode mapping.

Phase 7.5 download flow:
  (url-retrieve-synchronously nelisp-coding-table-gen-cp932-url)
  → parse via `nelisp-coding-table-gen--parse-cp932-line'
  → emit `defconst nelisp-coding-shift-jis-x0208-decode-table'
       and `defconst nelisp-coding-cp932-extension-decode-table'.")

(defvar nelisp-coding-table-gen-jis0212-url
  "https://unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/JIS0212.TXT"
  "Canonical JIS X 0212-1990 supplementary kanji table URL.
This table covers ~6000 supplementary kanji not in JIS X 0208.

Phase 7.5 download flow:
  (url-retrieve-synchronously nelisp-coding-table-gen-jis0212-url)
  → parse via `nelisp-coding-table-gen--parse-jis0212-line'
  → emit `defconst nelisp-coding-euc-jp-x0212-decode-table'.")

(defvar nelisp-coding-table-gen-output-path
  "src/nelisp-coding-jis-tables.el"
  "Output path for the regenerated tables file.
Should be a project-relative path; the batch entry assumes the working
directory is the NeLisp project root.")

(defun nelisp-coding-table-gen--parse-cp932-line (line)
  "Parse one line of CP932.TXT format and return (CP932-INT . CODEPOINT) or nil.

CP932.TXT format example:
  0x20\\t0x0020\\t# SPACE
  0x8140\\t0x3000\\t# IDEOGRAPHIC SPACE
  0xA1\\t0xFF61\\t# HALFWIDTH IDEOGRAPHIC FULL STOP

Skips comment-only lines (starting with '#') and lines with no Unicode
mapping (= reserved CP932 codes that map to nothing).

Phase 7.5 stub: not yet implemented. Returns nil for all input."
  ;; TODO Phase 7.5: implement real parsing.
  ;;
  ;; Reference algorithm (pseudocode):
  ;;   (when (string-match
  ;;          \"^\\\\([0-9A-Fx]+\\\\)\\\\s-+\\\\([0-9A-Fx]+\\\\)\"
  ;;          line)
  ;;     (cons (string-to-number (match-string 1 line) 16)
  ;;           (string-to-number (match-string 2 line) 16)))
  (ignore line)
  nil)

(defun nelisp-coding-table-gen--parse-jis0212-line (line)
  "Parse one line of JIS0212.TXT format and return (EUC-INT . CODEPOINT).

JIS0212.TXT format example:
  0x222F\\t0x02D8\\t# BREVE

The first column is the X 0212 row+cell encoded as a 16-bit value
(e.g. 0x222F = row 0x22, cell 0x2F). To convert to EUC-INT (the format
used in `nelisp-coding-euc-jp-x0212-decode-table'):
  EUC-INT = ((row | 0x80) << 8) | (cell | 0x80)

Phase 7.5 stub: not yet implemented. Returns nil."
  (ignore line)
  nil)

(defun nelisp-coding-table-gen--write-tables (sjis-x0208 cp932-ext eucjp-x0208 eucjp-x0212)
  "Write the four mapping tables to `nelisp-coding-table-gen-output-path'.

Each argument is an alist of (SOURCE-INT . CODEPOINT) cons cells, sorted
by SOURCE-INT ascending. The output file format must match the partial
MVP shipped in Phase 7.4.3 (= same defconst names + docstrings + golden
SHA-256 hash placeholder)."
  ;; TODO Phase 7.5: implement real file emission. Flow:
  ;;   1. Open output file for writing.
  ;;   2. Emit file header (lexical-binding line, commentary, Code:).
  ;;   3. Emit each defconst with the alist content + docstring.
  ;;   4. Compute canonical SHA-256 over prin1 of the four alists in
  ;;      declaration order.
  ;;   5. Emit nelisp-coding-jis-tables-sha256 + verify-hash defun.
  ;;   6. Emit provide form + ends-here marker.
  (ignore sjis-x0208 cp932-ext eucjp-x0208 eucjp-x0212)
  (error "TODO: Phase 7.5 で real generation 実装、現在 partial table を手動 placement"))

(defun nelisp-coding-table-gen-batch ()
  "Batch entry-point for table regeneration.

Phase 7.5 flow (planned):
  1. Download CP932.TXT from `nelisp-coding-table-gen-cp932-url'.
  2. Parse via `nelisp-coding-table-gen--parse-cp932-line', split into
     X 0208 base (0x8140-0x9FFC) and CP932 extension (0x8740-0x879F NEC,
     0xFA40-0xFC4B IBM).
  3. Download JIS0212.TXT from `nelisp-coding-table-gen-jis0212-url'.
  4. Parse via `nelisp-coding-table-gen--parse-jis0212-line', emit X 0212
     table (re-encoded into EUC-INT format).
  5. Derive EUC-JP X 0208 from JIS X 0208 mapping by `or'-ing 0x8080 onto
     each X 0208 row+cell (= ISO-2022 high-bit-set transform).
  6. Sort each alist by SOURCE-INT ascending.
  7. Write `src/nelisp-coding-jis-tables.el' via
     `nelisp-coding-table-gen--write-tables'.
  8. Verify the generated file's SHA-256 matches the embedded golden hash.

Phase 7.4.3 stub: signals `error' with explicit TODO message."
  (interactive)
  (error "TODO: Phase 7.5 で real generation 実装、現在 partial table を手動 placement"))

(provide 'nelisp-coding-table-gen)

;;; coding-table-gen.el ends here
