;;; nelisp-coding-table-test.el --- Heavyweight JIS table checksum + bijection ERT  -*- lexical-binding: t; -*-

;; Doc 31 v2 LOCKED 2026-04-25 §7.2 — Phase 7.4 完遂 gate, 必須 deliverable:
;;
;;   *heavyweight test* (codex 観点 2.6 反映): full-table checksum +
;;   bijection (Shift-JIS / EUC-JP table) PASS、=test/nelisp-coding-table-test.el=
;;
;; This file is the dedicated heavyweight ERT separated from
;; `test/nelisp-coding-test.el' (53 + 5 streaming/file tests = MVP smoke
;; tier) so that the *exhaustive* table walks live in their own runner-
;; isolatable suite.  The MVP tier in `nelisp-coding-test.el' covers only
;; spot-checks (~100 sample stride), while this file walks every entry of
;; every JIS table — Doc 31 v2 §7.2 demands the latter as a binding LOCK
;; condition.
;;
;; +10 ERT (heavyweight, §7.2 binding):
;;
;;   1. checksum-golden-hash-matches             — verify-hash returns t
;;   2. checksum-canonical-bytes-format          — canonical-bytes is a
;;                                                 unibyte string of expected
;;                                                 size order (~MB scale)
;;   3. shift-jis-x0208-bijection-full           — every X 0208 entry round-
;;                                                 trip identity at codepoint
;;                                                 level (decode→encode→decode)
;;   4. cp932-extension-bijection-full           — same for CP932 ext (NEC +
;;                                                 IBM ~845 entries)
;;   5. euc-jp-x0208-bijection-full              — same for EUC-JP CS1
;;   6. euc-jp-x0212-bijection-full              — same for EUC-JP CS3
;;                                                 (3-byte 0x8F prefix)
;;   7. decode-tables-no-duplicate-source-int    — every source-int is unique
;;                                                 within each decode table
;;   8. decode-tables-sorted-ascending           — every entry sorted by
;;                                                 source-int (binary search
;;                                                 invariant)
;;   9. shift-jis-vs-euc-jp-encoding-distinct    — same chars produce
;;                                                 different byte sequences in
;;                                                 SJIS vs EUC-JP (>=100
;;                                                 distinct entries)
;;   10. encode-hashes-respect-x0208-precedence  — when same codepoint exists
;;                                                 in both X 0208 + CP932 ext
;;                                                 tables, X 0208 wins on
;;                                                 encode (= `--jis-tables-build'
;;                                                 invariant)
;;
;; Notes on identity definition (codepoint-level vs byte-level):
;;
;;   Some Unicode codepoints map to multiple JIS codes (collision: e.g. NEC
;;   selected IBM duplicates X 0208 entries).  `--jis-tables-build' uses
;;   `unless gethash' so the *first* table inserted wins on encode (X 0208
;;   over CP932 ext, X 0208 over X 0212).  Therefore the bijection tested
;;   here is at the *codepoint* level, not byte level: for every
;;   (source-int . codepoint) entry, decode(source-bytes) yields codepoint
;;   AND decode(encode(codepoint)) yields the same codepoint (possibly via
;;   a different but equivalent byte sequence).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-coding)
(require 'nelisp-coding-jis-tables)

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Helpers
;;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-coding-table-test--bytes (&rest bs)
  "Build a unibyte string from raw byte values BS for test inputs."
  (apply #'unibyte-string bs))

(defun nelisp-coding-table-test--sjis-int-to-bytes (sjis-int)
  "Return unibyte string of (lead trail) for 16-bit SJIS-INT."
  (nelisp-coding-table-test--bytes
   (logand (ash sjis-int -8) #xFF)
   (logand sjis-int #xFF)))

(defun nelisp-coding-table-test--euc-int-to-bytes (euc-int)
  "Return unibyte string of (lead trail) for 16-bit EUC-INT (CS1)."
  (nelisp-coding-table-test--bytes
   (logand (ash euc-int -8) #xFF)
   (logand euc-int #xFF)))

(defun nelisp-coding-table-test--euc-int-to-cs3-bytes (euc-int)
  "Return unibyte string of (0x8F lead trail) for X 0212 EUC-INT (CS3)."
  (nelisp-coding-table-test--bytes
   #x8F
   (logand (ash euc-int -8) #xFF)
   (logand euc-int #xFF)))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Test 1: Golden SHA-256 hash matches in-tree table content
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-table-checksum-golden-hash-matches ()
  "`nelisp-coding-jis-tables-verify-hash' returns t (= the four JIS tables
in the tree match the embedded golden SHA-256 hash).  This is the binding
contract that the file content was not silently mutated by a rebase /
merge.  Per Doc 31 v2 §6.10 + §7.2 the hash is part of the file contract."
  (should (eq (nelisp-coding-jis-tables-verify-hash) t))
  ;; Hash format invariants: 64-char lowercase hex string.
  (should (stringp nelisp-coding-jis-tables-sha256))
  (should (= (length nelisp-coding-jis-tables-sha256) 64))
  (should (string-match-p "\\`[0-9a-f]+\\'"
                          nelisp-coding-jis-tables-sha256)))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Test 2: Canonical bytes format (prin1 concat of 4 tables in order)
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-table-checksum-canonical-bytes-format ()
  "`nelisp-coding-jis-tables--canonical-bytes' returns a string whose
SHA-256 equals the golden hash, *and* whose size is in the documented
range (= ~MB scale for 4 tables of ~14k entries combined, prin1-format)."
  (let* ((bytes (nelisp-coding-jis-tables--canonical-bytes))
         (computed (secure-hash 'sha256 bytes)))
    (should (stringp bytes))
    (should (equal computed nelisp-coding-jis-tables-sha256))
    ;; Size sanity: 4 tables × ~14k entries × ~16 chars each ≈ 200KB-2MB.
    ;; Lower bound = real full table; tight enough to catch accidental
    ;; partial-MVP regression (which would be ~50KB).
    (should (>= (length bytes) (* 200 1024)))   ; >= 200 KB
    (should (<= (length bytes) (* 5  1024 1024))))) ; <= 5 MB

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Test 3: Shift-JIS X 0208 full bijection (every entry round-trips)
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-table-shift-jis-x0208-bijection-full ()
  "Every entry of `nelisp-coding-shift-jis-x0208-decode-table' satisfies:
  decode(SJIS-bytes) = string CP   AND   decode(encode(string CP)) = string CP

This is the *exhaustive* (all 6000+ entries) bijection check.  The
cheaper sample-stride version lives in `nelisp-coding-test.el' as
`nelisp-coding-jis-tables-full-roundtrip-100-random-x0208'."
  (nelisp-coding--jis-tables-build)
  (let ((checked 0)
        (mismatches '()))
    (dolist (entry nelisp-coding-shift-jis-x0208-decode-table)
      (let* ((sjis-int (car entry))
             (cp       (cdr entry))
             (bytes    (nelisp-coding-table-test--sjis-int-to-bytes sjis-int))
             (decoded  (nelisp-coding-shift-jis-decode bytes))
             (ds       (plist-get decoded :string))
             (drep     (plist-get decoded :replacements)))
        (unless (and (= drep 0) (equal ds (string cp)))
          (push (list :stage 'decode :sjis-int sjis-int :cp cp
                      :decoded-string ds :replacements drep)
                mismatches))
        ;; Round-trip via encode.
        (let* ((enc (nelisp-coding-shift-jis-encode (string cp)))
               (eb  (plist-get enc :bytes))
               (erep (plist-get enc :replacements))
               (re   (when (= erep 0)
                       (nelisp-coding-shift-jis-decode (apply #'unibyte-string eb))))
               (rs   (and re (plist-get re :string))))
          (unless (and (= erep 0) (equal rs (string cp)))
            (push (list :stage 'encode-redecode :sjis-int sjis-int :cp cp
                        :encoded-bytes eb :replacements erep
                        :redecoded-string rs)
                  mismatches)))
        (cl-incf checked)))
    (should (>= checked 6000))
    (should (null mismatches))))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Test 4: CP932 extension full bijection
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-table-cp932-extension-bijection-full ()
  "Every entry of `nelisp-coding-cp932-extension-decode-table' decodes to
its target codepoint, and re-encoding yields a byte sequence that decodes
back to the same codepoint (possibly via a different SJIS-INT — X 0208
wins on Unicode collision per `--jis-tables-build' invariant)."
  (nelisp-coding--jis-tables-build)
  (let ((checked 0)
        (mismatches '()))
    (dolist (entry nelisp-coding-cp932-extension-decode-table)
      (let* ((sjis-int (car entry))
             (cp       (cdr entry))
             (bytes    (nelisp-coding-table-test--sjis-int-to-bytes sjis-int))
             (decoded  (nelisp-coding-shift-jis-decode bytes))
             (ds       (plist-get decoded :string))
             (drep     (plist-get decoded :replacements)))
        (unless (and (= drep 0) (equal ds (string cp)))
          (push (list :stage 'decode :sjis-int sjis-int :cp cp
                      :decoded-string ds :replacements drep)
                mismatches))
        (let* ((enc (nelisp-coding-shift-jis-encode (string cp)))
               (eb  (plist-get enc :bytes))
               (erep (plist-get enc :replacements))
               (re   (when (= erep 0)
                       (nelisp-coding-shift-jis-decode (apply #'unibyte-string eb))))
               (rs   (and re (plist-get re :string))))
          (unless (and (= erep 0) (equal rs (string cp)))
            (push (list :stage 'encode-redecode :sjis-int sjis-int :cp cp
                        :encoded-bytes eb :replacements erep
                        :redecoded-string rs)
                  mismatches)))
        (cl-incf checked)))
    (should (>= checked 800))
    (should (null mismatches))))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Test 5: EUC-JP X 0208 (CS1) full bijection
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-table-euc-jp-x0208-bijection-full ()
  "Every entry of `nelisp-coding-euc-jp-x0208-decode-table' satisfies the
codepoint-level bijection: decode = cp AND decode(encode(cp)) = cp."
  (nelisp-coding--jis-tables-build)
  (let ((checked 0)
        (mismatches '()))
    (dolist (entry nelisp-coding-euc-jp-x0208-decode-table)
      (let* ((euc-int (car entry))
             (cp      (cdr entry))
             (bytes   (nelisp-coding-table-test--euc-int-to-bytes euc-int))
             (decoded (nelisp-coding-euc-jp-decode bytes))
             (ds      (plist-get decoded :string))
             (drep    (plist-get decoded :replacements)))
        (unless (and (= drep 0) (equal ds (string cp)))
          (push (list :stage 'decode :euc-int euc-int :cp cp
                      :decoded-string ds :replacements drep)
                mismatches))
        (let* ((enc (nelisp-coding-euc-jp-encode (string cp)))
               (eb  (plist-get enc :bytes))
               (erep (plist-get enc :replacements))
               (re   (when (= erep 0)
                       (nelisp-coding-euc-jp-decode (apply #'unibyte-string eb))))
               (rs   (and re (plist-get re :string))))
          (unless (and (= erep 0) (equal rs (string cp)))
            (push (list :stage 'encode-redecode :euc-int euc-int :cp cp
                        :encoded-bytes eb :replacements erep
                        :redecoded-string rs)
                  mismatches)))
        (cl-incf checked)))
    (should (>= checked 6000))
    (should (null mismatches))))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Test 6: EUC-JP X 0212 (CS3, 0x8F prefix) full bijection
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-table-euc-jp-x0212-bijection-full ()
  "Every entry of `nelisp-coding-euc-jp-x0212-decode-table' satisfies the
codepoint-level bijection.  Encoding emits 3 bytes (0x8F + lead + trail)
unless the codepoint also exists in X 0208, in which case X 0208 (2-byte
CS1) wins on encode per `--jis-tables-build' precedence rule.  Either
way, re-decoding the encoded bytes yields the same codepoint."
  (nelisp-coding--jis-tables-build)
  (let ((checked 0)
        (mismatches '()))
    (dolist (entry nelisp-coding-euc-jp-x0212-decode-table)
      (let* ((euc-int (car entry))
             (cp      (cdr entry))
             (bytes   (nelisp-coding-table-test--euc-int-to-cs3-bytes euc-int))
             (decoded (nelisp-coding-euc-jp-decode bytes))
             (ds      (plist-get decoded :string))
             (drep    (plist-get decoded :replacements)))
        (unless (and (= drep 0) (equal ds (string cp)))
          (push (list :stage 'decode :euc-int euc-int :cp cp
                      :decoded-string ds :replacements drep)
                mismatches))
        (let* ((enc (nelisp-coding-euc-jp-encode (string cp)))
               (eb  (plist-get enc :bytes))
               (erep (plist-get enc :replacements))
               (re   (when (= erep 0)
                       (nelisp-coding-euc-jp-decode (apply #'unibyte-string eb))))
               (rs   (and re (plist-get re :string))))
          (unless (and (= erep 0) (equal rs (string cp)))
            (push (list :stage 'encode-redecode :euc-int euc-int :cp cp
                        :encoded-bytes eb :replacements erep
                        :redecoded-string rs)
                  mismatches)))
        (cl-incf checked)))
    (should (>= checked 5000))
    (should (null mismatches))))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Test 7: Decode tables — no duplicate source-int within each table
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-table-decode-tables-no-duplicate-source-int ()
  "Within each of the 4 decode tables, every SOURCE-INT appears at most
once (= the table is a function from SOURCE-INT to CODEPOINT, not a
relation).  Duplicates would silently shadow earlier mappings in
`--jis-tables-build' (= `puthash' last-write-wins) and likely indicate a
generator regression."
  (dolist (tbl-spec
           (list (list 'shift-jis-x0208 nelisp-coding-shift-jis-x0208-decode-table)
                 (list 'cp932-extension nelisp-coding-cp932-extension-decode-table)
                 (list 'euc-jp-x0208   nelisp-coding-euc-jp-x0208-decode-table)
                 (list 'euc-jp-x0212   nelisp-coding-euc-jp-x0212-decode-table)))
    (let* ((label (nth 0 tbl-spec))
           (tbl   (nth 1 tbl-spec))
           (seen  (make-hash-table :test 'eql :size (length tbl)))
           (dups  '()))
      (dolist (entry tbl)
        (let ((src (car entry)))
          (if (gethash src seen)
              (push (list :table label :duplicate-source-int src) dups)
            (puthash src t seen))))
      (should (null dups)))))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Test 8: Decode tables — source-int sorted ascending (binary search invariant)
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-table-decode-tables-sorted-ascending ()
  "Per `nelisp-coding-jis-tables.el' header: \"All tables are sorted by
SOURCE-INT ascending; this allows binary search or hash-table promotion.\"
This test enforces that contract for all 4 decode tables."
  (dolist (tbl-spec
           (list (list 'shift-jis-x0208 nelisp-coding-shift-jis-x0208-decode-table)
                 (list 'cp932-extension nelisp-coding-cp932-extension-decode-table)
                 (list 'euc-jp-x0208   nelisp-coding-euc-jp-x0208-decode-table)
                 (list 'euc-jp-x0212   nelisp-coding-euc-jp-x0212-decode-table)))
    (let* ((label (nth 0 tbl-spec))
           (tbl   (nth 1 tbl-spec))
           (prev  -1)
           (violations '()))
      (dolist (entry tbl)
        (let ((src (car entry)))
          (unless (> src prev)
            (push (list :table label :prev prev :curr src) violations))
          (setq prev src)))
      (should (null violations)))))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Test 9: Same chars produce different byte sequences in SJIS vs EUC-JP
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-table-shift-jis-vs-euc-jp-encoding-distinct ()
  "Per Doc 31 v2 §3.3 ERT 8: \"同一 char で Shift-JIS / EUC-JP byte 列が
異なる confirm (table data 健全性)\".  The two encodings use disjoint
byte arrangements for X 0208 chars, so encoded bytes must differ on at
least 100 X 0208 codepoints (typically all of them, but >= 100 is the
binding floor).  ASCII passthrough (byte == cp < 0x80) is naturally
identical and excluded."
  (nelisp-coding--jis-tables-build)
  (let ((distinct-count 0)
        (sample-count 0))
    (dolist (entry nelisp-coding-shift-jis-x0208-decode-table)
      (let* ((cp (cdr entry)))
        (when (>= cp #x80)
          (cl-incf sample-count)
          (let* ((s   (string cp))
                 (sj  (plist-get (nelisp-coding-shift-jis-encode s) :bytes))
                 (eu  (plist-get (nelisp-coding-euc-jp-encode    s) :bytes)))
            (unless (equal sj eu)
              (cl-incf distinct-count))))))
    (should (>= sample-count 1000))
    (should (>= distinct-count 100))))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Test 10: encode-hash precedence — X 0208 wins over CP932 ext / X 0212
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-table-encode-hashes-respect-x0208-precedence ()
  "`--jis-tables-build' inserts X 0208 first then guards CP932 ext / X 0212
inserts with `unless gethash' so that X 0208 wins on Unicode collision.
Verify that for every codepoint that appears in both the X 0208 decode
table AND a secondary table (CP932 ext for SJIS, X 0212 for EUC-JP), the
encode hash returns the X 0208 source-int (= the precedence rule actually
holds in the built hash-tables)."
  (nelisp-coding--jis-tables-build)
  (let ((sjis-overlaps 0)
        (sjis-violations '())
        (euc-overlaps 0)
        (euc-violations '())
        (sjis-x0208-cps (make-hash-table :test 'eql)))
    ;; Pre-index SJIS X 0208 codepoint → sjis-int (first occurrence wins,
    ;; matching `--jis-tables-build' iteration order).
    (dolist (e nelisp-coding-shift-jis-x0208-decode-table)
      (let ((cp (cdr e)) (sj (car e)))
        (unless (gethash cp sjis-x0208-cps)
          (puthash cp sj sjis-x0208-cps))))
    ;; SJIS overlap: cp present in CP932 ext AND in X 0208 → encode-hash
    ;; must return the X 0208 source-int.
    (dolist (e nelisp-coding-cp932-extension-decode-table)
      (let* ((cp (cdr e))
             (x0208 (gethash cp sjis-x0208-cps)))
        (when x0208
          (cl-incf sjis-overlaps)
          (let ((got (gethash cp nelisp-coding--shift-jis-encode-hash)))
            (unless (eql got x0208)
              (push (list :cp cp :expected-x0208 x0208 :got got)
                    sjis-violations))))))
    ;; EUC overlap: cp present in X 0212 AND in X 0208 → encode-hash
    ;; (x0208 hash) hit; X 0212 hash should NOT be the one consulted in
    ;; `nelisp-coding-euc-jp-encode' (which checks X 0208 first).
    (let ((euc-x0208-cps (make-hash-table :test 'eql)))
      (dolist (e nelisp-coding-euc-jp-x0208-decode-table)
        (let ((cp (cdr e)) (ec (car e)))
          (unless (gethash cp euc-x0208-cps)
            (puthash cp ec euc-x0208-cps))))
      (dolist (e nelisp-coding-euc-jp-x0212-decode-table)
        (let* ((cp (cdr e))
               (x0208 (gethash cp euc-x0208-cps)))
          (when x0208
            (cl-incf euc-overlaps)
            (let ((got (gethash cp nelisp-coding--euc-jp-x0208-encode-hash)))
              (unless (eql got x0208)
                (push (list :cp cp :expected-x0208 x0208 :got got)
                      euc-violations)))))))
    (should (null sjis-violations))
    (should (null euc-violations))
    ;; Sanity: there *are* overlaps to test (else the precedence rule is
    ;; vacuous).  Real CP932 has hundreds of NEC selected IBM duplicates
    ;; over X 0208; X 0212 has ~0-few X 0208 overlaps but the test is
    ;; meaningful even when 0 (the assertion still validates the full
    ;; CP932 overlap path).
    (should (>= sjis-overlaps 1))))

(provide 'nelisp-coding-table-test)

;;; nelisp-coding-table-test.el ends here
