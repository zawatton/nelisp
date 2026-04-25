;;; nelisp-coding-test.el --- ERT tests for nelisp-coding  -*- lexical-binding: t; -*-

;; Doc 31 v2 LOCKED 2026-04-25 §3.1 sub-phase 7.4.1 — UTF-8 encode/decode +
;; BOM handling + 3 invalid-sequence strategy contract LOCK.
;; Doc 31 v2 LOCKED 2026-04-25 §3.2 sub-phase 7.4.2 — Latin-1 (ISO-8859-1)
;; encode/decode + placeholder handling for U+0100+.
;; Doc 31 v2 LOCKED 2026-04-25 §3.3 sub-phase 7.4.3 — Shift-JIS / CP932 /
;; EUC-JP (X 0208 + X 0212) + table data + golden SHA-256 hash.
;;
;; +10 ERT (Phase 7.4.1, per Doc 31 v2 §3.1 / §5):
;;   1. encode-ascii-passthrough        — "hello" → 5 byte ASCII
;;   2. encode-multibyte-3byte          — "あいう" → 9 byte (3 chars × 3 bytes)
;;   3. encode-emoji-4byte              — "🦀" (U+1F980) → F0 9F A6 80
;;   4. decode-roundtrip                — encode → decode で original 一致
;;   5. decode-replace-strategy         — invalid → U+FFFD + :invalid-positions
;;   6. decode-error-strategy-signals   — signal nelisp-coding-invalid-byte
;;   7. decode-strict-strategy-signals  — signal nelisp-coding-strict-violation
;;   8. decode-rejects-overlong         — 0xC0 0x80 (overlong U+0000) reject
;;   9. decode-rejects-surrogate        — encoded surrogate (ED A0 80) reject
;;   10. bom-strip-and-prepend          — strip on read / prepend opt-in
;;
;; +6 ERT (Phase 7.4.2, per Doc 31 v2 §3.2 / §6.2):
;;   11. latin1-encode-ascii-passthrough           — "hello" → 5 byte ASCII
;;   12. latin1-decode-roundtrip-bmp-low           — 0x00-0xFF identity
;;   13. latin1-decode-no-invalid-byte             — random 256 byte all valid
;;   14. latin1-encode-replace-out-of-range        — "あいう" → 3× 0x3F + count 3
;;   15. latin1-encode-error-signals               — out-of-range signals invalid-codepoint
;;   16. latin1-encode-strict-signals              — out-of-range signals strict-violation
;;
;; +12 ERT (Phase 7.4.3, per Doc 31 v2 §3.3 / §6.10):
;;   17. shift-jis-decode-ascii-passthrough        — 0x41 → "A"
;;   18. shift-jis-decode-x0208-multibyte          — 0x82A0 → あ (U+3042) round-trip
;;   19. shift-jis-decode-cp932-extension          — 0x8740 → ① (U+2460)
;;   20. shift-jis-decode-invalid-trail-byte       — bad trail :replace strategy
;;   21. shift-jis-encode-roundtrip-x0208          — encode→decode identity
;;   22. shift-jis-encode-error-strategy-out-of-table — unmapped + :error signals
;;   23. euc-jp-decode-ascii-passthrough
;;   24. euc-jp-decode-x0208-2byte                 — 0xA4A2 → あ (U+3042)
;;   25. euc-jp-decode-x0212-3byte                 — 0x8FB0A1 → 丂 (U+4E02)
;;   26. euc-jp-decode-katakana-2byte              — 0x8EB1 → ｱ (U+FF71)
;;   27. euc-jp-encode-roundtrip-x0208
;;   28. jis-tables-verify-hash-passes             — golden hash 一致
;;
;; +8 ERT (Phase 7.4.4, per Doc 31 v2 §3.4 / §6.5):
;;   29. stream-decode-utf8-single-chunk             — full input as 1 chunk
;;   30. stream-decode-utf8-multi-chunk              — N chunks → identical result
;;   31. stream-decode-utf8-multibyte-boundary       — 3-byte char split mid-seq
;;   32. stream-decode-shift-jis-multibyte-boundary  — SJIS lead/trail split
;;   33. stream-decode-truncated-finalize-replace    — pending tail at finalize
;;   34. stream-encode-multi-chunk                   — encode N chunks identity
;;   35. read-file-with-encoding-utf8-roundtrip      — write→read identity
;;   36. stream-decode-1mb-stress                    — 1MB random valid UTF-8
;;
;; +6 ERT (Phase 7.4.5, per Doc 31 v2 §3.3 / §6.10 / §7.2 — full table):
;;   37. jis-tables-full-x0208-entry-count           — X 0208 mapping count >= 6000
;;   38. jis-tables-full-cp932-extension-count       — CP932 extension count ~845
;;   39. jis-tables-full-jis0212-count               — X 0212 count >= 5000
;;   40. jis-tables-full-roundtrip-100-random-x0208  — 100 random X 0208 cp round-trip
;;   41. shift-jis-decode-real-windows-japanese-text — sample Windows CP932 bytes decode
;;   42. jis-tables-verify-hash-with-full-table      — golden hash differs from MVP partial

(require 'ert)
(require 'cl-lib)
(require 'nelisp-coding)

;;;; Helpers

(defun nelisp-coding-test--bytes (&rest bs)
  "Build a unibyte string from raw byte values BS for test inputs."
  (apply #'unibyte-string bs))

(defun nelisp-coding-test--bytes-equal (got expected)
  "Compare GOT (list/vector/unibyte string) to EXPECTED (list of bytes)."
  (let ((g-list (cond ((listp got) got)
                      ((vectorp got) (append got nil))
                      ((stringp got)
                       (mapcar (lambda (c) (logand c #xFF))
                               (append got nil)))
                      (t (error "Unsupported type: %S" got)))))
    (equal g-list expected)))

;;;; 1. encode ASCII passthrough — "hello" → 5 byte ASCII

(ert-deftest nelisp-coding-utf8-encode-ascii-passthrough ()
  "ASCII string \"hello\" encodes to 5 single-byte sequence."
  (let ((bytes (nelisp-coding-utf8-encode "hello")))
    (should (= (length bytes) 5))
    (should (nelisp-coding-test--bytes-equal
             bytes '(?h ?e ?l ?l ?o)))))

;;;; 2. encode multibyte 3-byte — "あいう" → 9 bytes

(ert-deftest nelisp-coding-utf8-encode-multibyte-3byte ()
  "Japanese hiragana \"あいう\" encodes to 9 bytes (3 chars × 3 bytes).
あ = U+3042 = E3 81 82
い = U+3044 = E3 81 84
う = U+3046 = E3 81 86"
  (let ((bytes (nelisp-coding-utf8-encode (string #x3042 #x3044 #x3046))))
    (should (= (length bytes) 9))
    (should (nelisp-coding-test--bytes-equal
             bytes
             '(#xE3 #x81 #x82
               #xE3 #x81 #x84
               #xE3 #x81 #x86)))))

;;;; 3. encode emoji 4-byte — "🦀" (U+1F980) → F0 9F A6 80

(ert-deftest nelisp-coding-utf8-encode-emoji-4byte ()
  "Crab emoji 🦀 (U+1F980) encodes to 4 bytes F0 9F A6 80."
  (let ((bytes (nelisp-coding-utf8-encode (string #x1F980))))
    (should (= (length bytes) 4))
    (should (nelisp-coding-test--bytes-equal
             bytes '(#xF0 #x9F #xA6 #x80)))))

;;;; 4. decode roundtrip — encode → decode で original 一致

(ert-deftest nelisp-coding-utf8-decode-roundtrip ()
  "encode → decode round-trip preserves original codepoints across all
ranges (ASCII, BMP non-ASCII, supplementary plane)."
  (let* ((original (concat "Hello, "
                           (string #x3042 #x4E16 #x754C) ; あ世界
                           "! "
                           (string #x1F980 #x1F4DA))) ; 🦀📚
         (encoded (nelisp-coding-utf8-encode original))
         (decoded (nelisp-coding-utf8-decode encoded 'replace))
         (decoded-string (plist-get decoded :string)))
    (should (equal decoded-string original))
    (should (= (plist-get decoded :replacements) 0))
    (should (null (plist-get decoded :invalid-positions)))
    (should-not (plist-get decoded :had-bom))))

;;;; 5. decode replace strategy — invalid byte → U+FFFD with :invalid-positions

(ert-deftest nelisp-coding-utf8-decode-replace-strategy ()
  "Invalid byte (0xFF, never valid in UTF-8) under :replace strategy
emits U+FFFD and reports byte offset in :invalid-positions."
  (let* ((bytes (nelisp-coding-test--bytes ?A #xFF ?B))
         (result (nelisp-coding-utf8-decode bytes 'replace)))
    (should (equal (plist-get result :string)
                   (string ?A #xFFFD ?B)))
    (should (equal (plist-get result :invalid-positions) '(1)))
    (should (= (plist-get result :replacements) 1))
    (should (eq (plist-get result :strategy) 'replace)))
  ;; Consecutive invalid bytes collapse to single U+FFFD (WHATWG).
  (let* ((bytes (nelisp-coding-test--bytes ?A #xFF #xFE #xFD ?B))
         (result (nelisp-coding-utf8-decode bytes 'replace)))
    (should (equal (plist-get result :string)
                   (string ?A #xFFFD ?B)))
    (should (= (plist-get result :replacements) 1))
    (should (equal (plist-get result :invalid-positions) '(1 2 3)))))

;;;; 6. decode error strategy — signal nelisp-coding-invalid-byte (catchable)

(ert-deftest nelisp-coding-utf8-decode-error-strategy-signals ()
  "Invalid byte under :error strategy signals nelisp-coding-invalid-byte
with offset / byte / strategy data, catchable via condition-case."
  (let ((bytes (nelisp-coding-test--bytes ?A #xFF ?B)))
    (should-error
     (nelisp-coding-utf8-decode bytes 'error)
     :type 'nelisp-coding-invalid-byte)
    ;; Verify signal data carries offset and strategy.
    (let ((caught nil))
      (condition-case err
          (nelisp-coding-utf8-decode bytes 'error)
        (nelisp-coding-invalid-byte
         (setq caught (cdr err))))
      (should caught)
      (should (equal (plist-get caught :offset) 1))
      (should (equal (plist-get caught :byte) #xFF))
      (should (eq (plist-get caught :strategy) 'error)))))

;;;; 7. decode strict strategy — signal nelisp-coding-strict-violation

(ert-deftest nelisp-coding-utf8-decode-strict-strategy-signals ()
  "Invalid byte under :strict strategy signals nelisp-coding-strict-violation.

T67 / Doc 31 v2 §2.4 LOCK update: `strict' violations are *not* a
sub-type of `nelisp-coding-error' (= the recoverable-error parent
class).  A `condition-case' on `nelisp-coding-error' MUST NOT catch
a strict violation — that aligns the host-Emacs MVP with the spec's
\"uncatchable / abort\" semantics.  Strict can still be caught by
exact symbol or via toplevel `error' (which the host runtime cannot
prevent in MVP); Phase 7.5+ replaces the `signal' with process
abort."
  (let ((bytes (nelisp-coding-test--bytes ?A #xFF ?B)))
    (should-error
     (nelisp-coding-utf8-decode bytes 'strict)
     :type 'nelisp-coding-strict-violation)
    ;; New contract: `nelisp-coding-error' parent does NOT catch strict.
    (let ((caught-parent nil)
          (caught-toplevel nil))
      (condition-case _err
          (condition-case _err2
              (nelisp-coding-utf8-decode bytes 'strict)
            (nelisp-coding-error
             (setq caught-parent t)))
        (error
         (setq caught-toplevel t)))
      (should-not caught-parent)
      (should caught-toplevel))))

;;;; 8. decode rejects overlong — 0xC0 0x80 (overlong U+0000)

(ert-deftest nelisp-coding-utf8-decode-rejects-overlong ()
  "Overlong encoding (e.g. 0xC0 0x80 = U+0000 in 2 bytes) rejected.
Per RFC 3629, encoders MUST use shortest sequence."
  ;; 0xC0 0x80 = overlong U+0000
  (let* ((bytes (nelisp-coding-test--bytes #xC0 #x80))
         (result (nelisp-coding-utf8-decode bytes 'replace)))
    ;; Both bytes flagged invalid (advance-by-1 resync), 1 U+FFFD emitted.
    (should (equal (plist-get result :string) (string #xFFFD)))
    (should (= (plist-get result :replacements) 1)))
  ;; 0xE0 0x80 0xAF = overlong U+002F (slash) in 3 bytes
  (let* ((bytes (nelisp-coding-test--bytes #xE0 #x80 #xAF))
         (result (nelisp-coding-utf8-decode bytes 'replace)))
    (should (equal (plist-get result :string) (string #xFFFD)))
    (should (= (plist-get result :replacements) 1)))
  ;; Error strategy raises on overlong.
  (should-error
   (nelisp-coding-utf8-decode (nelisp-coding-test--bytes #xC0 #x80) 'error)
   :type 'nelisp-coding-invalid-byte))

;;;; 9. decode rejects surrogate — encoded U+D800 (ED A0 80)

(ert-deftest nelisp-coding-utf8-decode-rejects-surrogate ()
  "Surrogate codepoints (U+D800-U+DFFF) encoded in UTF-8 (e.g. ED A0 80
= U+D800) rejected per RFC 3629."
  (let* ((bytes (nelisp-coding-test--bytes #xED #xA0 #x80))
         (result (nelisp-coding-utf8-decode bytes 'replace)))
    (should (equal (plist-get result :string) (string #xFFFD)))
    (should (= (plist-get result :replacements) 1)))
  ;; And the encoder side rejects surrogate codepoints directly.
  (should-error
   (nelisp-coding--utf8-encode-codepoint #xD800)
   :type 'nelisp-coding-invalid-codepoint)
  (should-error
   (nelisp-coding--utf8-encode-codepoint #xDFFF)
   :type 'nelisp-coding-invalid-codepoint)
  ;; Out-of-range codepoints rejected too.
  (should-error
   (nelisp-coding--utf8-encode-codepoint #x110000)
   :type 'nelisp-coding-invalid-codepoint))

;;;; 10. BOM strip and prepend — read strips, write opt-in prepends

(ert-deftest nelisp-coding-utf8-bom-strip-and-prepend ()
  "BOM (EF BB BF) is stripped on decode (default behavior, RFC 3629).
On encode, BOM is NOT emitted by default; opt-in via per-call argument
or `nelisp-coding-utf8-bom-emit-on-write' defcustom."
  ;; Decode: BOM stripped from result, :had-bom = t.
  (let* ((bytes (nelisp-coding-test--bytes #xEF #xBB #xBF ?h ?i))
         (result (nelisp-coding-utf8-decode bytes 'replace)))
    (should (equal (plist-get result :string) "hi"))
    (should (eq (plist-get result :had-bom) t))
    (should (= (plist-get result :replacements) 0)))
  ;; Decode without BOM: :had-bom = nil.
  (let* ((bytes (nelisp-coding-test--bytes ?h ?i))
         (result (nelisp-coding-utf8-decode bytes 'replace)))
    (should (equal (plist-get result :string) "hi"))
    (should-not (plist-get result :had-bom)))
  ;; Encode default: no BOM.
  (let ((bytes (nelisp-coding-utf8-encode "hi")))
    (should (= (length bytes) 2))
    (should (nelisp-coding-test--bytes-equal bytes '(?h ?i))))
  ;; Encode with explicit bom-emit argument.
  (let ((bytes (nelisp-coding-utf8-encode "hi" t)))
    (should (= (length bytes) 5))
    (should (nelisp-coding-test--bytes-equal
             bytes '(#xEF #xBB #xBF ?h ?i))))
  ;; Encode with defcustom override.
  (let* ((nelisp-coding-utf8-bom-emit-on-write t)
         (bytes (nelisp-coding-utf8-encode "hi")))
    (should (= (length bytes) 5))
    (should (nelisp-coding-test--bytes-equal
             bytes '(#xEF #xBB #xBF ?h ?i))))
  ;; Strip helper preserves non-BOM input unchanged.
  (let ((stripped (nelisp-coding--strip-utf8-bom
                   (list #xEF #xBB #xBF #x41 #x42))))
    (should (equal stripped '(#x41 #x42))))
  (let ((unchanged (nelisp-coding--strip-utf8-bom (list #x41 #x42))))
    (should (equal unchanged '(#x41 #x42))))
  ;; Prepend helper.
  (let ((prepended (nelisp-coding--prepend-utf8-bom (list #x41 #x42))))
    (should (equal prepended (list #xEF #xBB #xBF #x41 #x42)))))

;;;; ──────────────────────────────────────────────────────────────────
;;;; Phase 7.4.2 — Latin-1 (ISO-8859-1) ERT (+6, Doc 31 v2 §3.2 / §6.2)
;;;; ──────────────────────────────────────────────────────────────────

;;;; 11. Latin-1 encode ASCII passthrough — "hello" → 5 bytes (UTF-8 互換)

(ert-deftest nelisp-coding-latin1-encode-ascii-passthrough ()
  "ASCII string \"hello\" encodes to 5 single-byte sequence (Latin-1 / UTF-8
ASCII compatibility, U+0000-U+007F)."
  (let* ((result (nelisp-coding-latin1-encode "hello"))
         (bytes  (plist-get result :bytes)))
    (should (= (length bytes) 5))
    (should (equal bytes '(?h ?e ?l ?l ?o)))
    (should (= (plist-get result :replacements) 0))
    (should (null (plist-get result :invalid-positions)))
    (should (eq (plist-get result :strategy) 'replace)))
  ;; Convenience wrapper returns unibyte string.
  (let ((s (nelisp-coding-latin1-encode-string "hello")))
    (should (equal s (unibyte-string ?h ?e ?l ?l ?o)))
    (should (= (length s) 5))))

;;;; 12. Latin-1 decode roundtrip BMP-low — 0x00-0xFF identity (bijective)

(ert-deftest nelisp-coding-latin1-decode-roundtrip-bmp-low ()
  "Decode 0x00-0xFF then encode produces identical byte sequence
(bijective single-byte Latin-1 mapping). All 256 byte values map
1:1 to U+0000-U+00FF."
  (let* ((all-bytes (let (lst)
                      (dotimes (i 256)
                        (push i lst))
                      (nreverse lst)))
         (decoded (nelisp-coding-latin1-decode all-bytes))
         (decoded-string (plist-get decoded :string)))
    ;; All 256 bytes valid → 0 replacements, no invalid positions.
    (should (= (plist-get decoded :replacements) 0))
    (should (null (plist-get decoded :invalid-positions)))
    ;; Decoded string has 256 chars, each char i has codepoint i.
    (should (= (length decoded-string) 256))
    (dotimes (i 256)
      (should (= (aref decoded-string i) i)))
    ;; Round-trip: encode the decoded string back and compare.
    (let* ((reencoded (nelisp-coding-latin1-encode decoded-string))
           (reencoded-bytes (plist-get reencoded :bytes)))
      (should (equal reencoded-bytes all-bytes))
      (should (= (plist-get reencoded :replacements) 0))
      (should (null (plist-get reencoded :invalid-positions))))))

;;;; 13. Latin-1 decode no invalid byte — Latin-1 spec = all 256 valid

(ert-deftest nelisp-coding-latin1-decode-no-invalid-byte ()
  "Latin-1 仕様により全 256 byte 値が valid (= invalid sequence
が存在しない)。任意の byte 列 decode で :replacements = 0、
:invalid-positions = nil が必ず保証される。"
  ;; Random-ish byte sequence covering edge values.
  (let* ((bytes (list 0 1 #x7F #x80 #x81 #xA0 #xC0 #xE0 #xF0 #xFE #xFF))
         (result (nelisp-coding-latin1-decode bytes))
         (s (plist-get result :string)))
    (should (= (length s) (length bytes)))
    (should (= (plist-get result :replacements) 0))
    (should (null (plist-get result :invalid-positions)))
    (should (eq (plist-get result :strategy) 'replace))
    ;; Each char codepoint matches input byte.
    (cl-loop for i from 0 below (length bytes)
             do (should (= (aref s i) (nth i bytes)))))
  ;; Empty input edge case.
  (let* ((empty (nelisp-coding-latin1-decode '()))
         (s (plist-get empty :string)))
    (should (equal s ""))
    (should (= (plist-get empty :replacements) 0))
    (should (null (plist-get empty :invalid-positions))))
  ;; Vector and unibyte string input forms accepted.
  (let ((vec-result (nelisp-coding-latin1-decode (vector #xA0 #xC1 #xFF))))
    (should (= (length (plist-get vec-result :string)) 3))
    (should (= (aref (plist-get vec-result :string) 1) #xC1)))
  (let ((str-result (nelisp-coding-latin1-decode (unibyte-string #x80 #x90))))
    (should (= (length (plist-get str-result :string)) 2))
    (should (= (aref (plist-get str-result :string) 0) #x80))))

;;;; 14. Latin-1 encode replace strategy — U+0100+ → '?' + count

(ert-deftest nelisp-coding-latin1-encode-replace-strategy-out-of-range ()
  "Encoding \"あいう\" (U+3042/U+3044/U+3046, all > U+00FF) under :replace
strategy emits 3 placeholder bytes (default 0x3F = '?') and reports
:replacements = 3 + :invalid-positions = (0 1 2) (= input char offsets)."
  ;; Default placeholder = 0x3F.
  (let* ((input (string #x3042 #x3044 #x3046))
         (result (nelisp-coding-latin1-encode input 'replace))
         (bytes (plist-get result :bytes)))
    (should (= (length bytes) 3))
    (should (equal bytes (list ?\? ?\? ?\?)))
    (should (= (plist-get result :replacements) 3))
    (should (equal (plist-get result :invalid-positions) '(0 1 2)))
    (should (eq (plist-get result :strategy) 'replace)))
  ;; Mixed in-range + out-of-range: "Aあ" → (0x41 0x3F).
  (let* ((input (string ?A #x3042))
         (result (nelisp-coding-latin1-encode input 'replace)))
    (should (equal (plist-get result :bytes) (list ?A ?\?)))
    (should (= (plist-get result :replacements) 1))
    (should (equal (plist-get result :invalid-positions) '(1))))
  ;; Default strategy (nil) honors `nelisp-coding-error-strategy' = replace.
  (let* ((nelisp-coding-error-strategy 'replace)
         (input (string #x3042))
         (result (nelisp-coding-latin1-encode input)))
    (should (= (plist-get result :replacements) 1))
    (should (equal (plist-get result :bytes) (list ?\?))))
  ;; Customizable placeholder (e.g. SUB U+001A).
  (let* ((nelisp-coding-latin1-replacement-codepoint #x1A)
         (input (string #x3042))
         (result (nelisp-coding-latin1-encode input 'replace)))
    (should (equal (plist-get result :bytes) (list #x1A)))
    (should (= (plist-get result :replacements) 1))))

;;;; 15. Latin-1 encode error strategy — signal nelisp-coding-invalid-codepoint

(ert-deftest nelisp-coding-latin1-encode-error-strategy-signals-invalid-codepoint ()
  "Encoding U+0100+ codepoint under :error strategy signals
`nelisp-coding-invalid-codepoint' with data plist =(:offset N :codepoint
CP :strategy 'error)=, catchable via condition-case."
  (let ((input (string ?A #x3042 ?B)))
    (should-error
     (nelisp-coding-latin1-encode input 'error)
     :type 'nelisp-coding-invalid-codepoint)
    ;; Verify signal data carries offset + codepoint + strategy.
    (let ((caught nil))
      (condition-case err
          (nelisp-coding-latin1-encode input 'error)
        (nelisp-coding-invalid-codepoint
         (setq caught (cdr err))))
      (should caught)
      (should (equal (plist-get caught :offset) 1))
      (should (equal (plist-get caught :codepoint) #x3042))
      (should (eq (plist-get caught :strategy) 'error))))
  ;; Pure-ASCII input under :error must NOT signal.
  (let* ((result (nelisp-coding-latin1-encode "hi" 'error)))
    (should (equal (plist-get result :bytes) '(?h ?i)))
    (should (= (plist-get result :replacements) 0))))

;;;; 16. Latin-1 encode strict strategy — signal nelisp-coding-strict-violation

(ert-deftest nelisp-coding-latin1-encode-strict-strategy-signals-strict-violation ()
  "Encoding U+0100+ codepoint under :strict strategy signals
`nelisp-coding-strict-violation'.

T67 / Doc 31 v2 §2.4 LOCK update: strict violations escape the
`nelisp-coding-error' parent class and propagate to toplevel `error'
only, mirroring the spec's uncatchable / abort policy.  Phase 7.5+
will replace the `signal' with process abort; do not rely on
recovering from a strict violation in user code."
  (let ((input (string ?A #x3042)))
    (should-error
     (nelisp-coding-latin1-encode input 'strict)
     :type 'nelisp-coding-strict-violation)
    ;; New contract: `nelisp-coding-error' parent does NOT catch strict.
    (let ((caught-parent nil)
          (caught-toplevel nil))
      (condition-case _err
          (condition-case _err2
              (nelisp-coding-latin1-encode input 'strict)
            (nelisp-coding-error
             (setq caught-parent t)))
        (error
         (setq caught-toplevel t)))
      (should-not caught-parent)
      (should caught-toplevel))
    ;; Verify signal data carries offset + codepoint + strategy.
    (let ((caught nil))
      (condition-case err
          (nelisp-coding-latin1-encode input 'strict)
        (nelisp-coding-strict-violation
         (setq caught (cdr err))))
      (should caught)
      (should (equal (plist-get caught :offset) 1))
      (should (equal (plist-get caught :codepoint) #x3042))
      (should (eq (plist-get caught :strategy) 'strict)))))

;;;; ──────────────────────────────────────────────────────────────────
;;;; Phase 7.4.3 — Shift-JIS / CP932 / EUC-JP ERT (+12, Doc 31 v2 §3.3)
;;;; ──────────────────────────────────────────────────────────────────

;;;; 17. Shift-JIS decode ASCII passthrough — 0x41 → "A"

(ert-deftest nelisp-coding-shift-jis-decode-ascii-passthrough ()
  "Shift-JIS ASCII range (0x00-0x7F) decodes as single-byte passthrough.
0x41 (= ASCII 'A') → \"A\". Mirrors the UTF-8 / Latin-1 ASCII fast path."
  (let* ((bytes (nelisp-coding-test--bytes ?H ?e ?l ?l ?o))
         (result (nelisp-coding-shift-jis-decode bytes)))
    (should (equal (plist-get result :string) "Hello"))
    (should (= (plist-get result :replacements) 0))
    (should (null (plist-get result :invalid-positions))))
  ;; Empty input.
  (let ((empty (nelisp-coding-shift-jis-decode '())))
    (should (equal (plist-get empty :string) ""))
    (should (= (plist-get empty :replacements) 0)))
  ;; Boundary: 0x7F is still ASCII (highest single-byte ASCII).
  (let ((r (nelisp-coding-shift-jis-decode (nelisp-coding-test--bytes #x7F))))
    (should (equal (plist-get r :string) (string #x7F)))))

;;;; 18. Shift-JIS decode X 0208 multi-byte — 0x82A0 → あ (U+3042)

(ert-deftest nelisp-coding-shift-jis-decode-x0208-multibyte ()
  "JIS X 0208 hiragana あ encoded as 0x82 0xA0 (2-byte SJIS) decodes to
U+3042. Round-trip through `nelisp-coding-shift-jis-encode' yields the
same byte sequence (MVP partial-table coverage validated)."
  (let* ((bytes (nelisp-coding-test--bytes #x82 #xA0))
         (result (nelisp-coding-shift-jis-decode bytes)))
    (should (equal (plist-get result :string) (string #x3042)))
    (should (= (plist-get result :replacements) 0))
    (should (null (plist-get result :invalid-positions))))
  ;; Multiple chars: あいう = 0x82A0 0x82A2 0x82A4 → 6 bytes total.
  (let* ((bytes (nelisp-coding-test--bytes #x82 #xA0 #x82 #xA2 #x82 #xA4))
         (result (nelisp-coding-shift-jis-decode bytes)))
    (should (equal (plist-get result :string)
                   (string #x3042 #x3044 #x3046)))
    (should (= (plist-get result :replacements) 0)))
  ;; Mixed ASCII + 2-byte: "Aあ" = 0x41 0x82 0xA0
  (let* ((bytes (nelisp-coding-test--bytes ?A #x82 #xA0))
         (result (nelisp-coding-shift-jis-decode bytes)))
    (should (equal (plist-get result :string) (string ?A #x3042)))))

;;;; 19. Shift-JIS decode CP932 extension — 0x8740 → ① (U+2460)

(ert-deftest nelisp-coding-shift-jis-decode-cp932-extension ()
  "CP932 NEC special character ① encoded as 0x87 0x40 decodes to U+2460.
This validates the CP932 extension table merge into the SJIS decode hash;
without the merge, this byte sequence would be a table miss."
  (let* ((bytes (nelisp-coding-test--bytes #x87 #x40))
         (result (nelisp-coding-shift-jis-decode bytes)))
    (should (equal (plist-get result :string) (string #x2460)))
    (should (= (plist-get result :replacements) 0)))
  ;; CP932 IBM extension entry: 0xFA40 → U+2170 (small Roman numeral i).
  (let* ((bytes (nelisp-coding-test--bytes #xFA #x40))
         (result (nelisp-coding-shift-jis-decode bytes)))
    (should (equal (plist-get result :string) (string #x2170)))))

;;;; 20. Shift-JIS decode invalid trail — bad trail :replace strategy

(ert-deftest nelisp-coding-shift-jis-decode-invalid-trail-byte ()
  "An SJIS lead byte (0x82) followed by an invalid trail (0x20, ASCII space,
not in 0x40-0xFC range) under :replace strategy emits U+FFFD and reports
the lead byte offset in `:invalid-positions'. The decoder advances by 1
to resync at the next byte."
  (let* ((bytes (nelisp-coding-test--bytes ?A #x82 #x20 ?B))
         (result (nelisp-coding-shift-jis-decode bytes 'replace)))
    ;; A + U+FFFD + space + B  (0x20 is re-tried as ASCII)
    (should (equal (plist-get result :string)
                   (string ?A #xFFFD #x20 ?B)))
    (should (= (plist-get result :replacements) 1))
    (should (equal (plist-get result :invalid-positions) '(1)))
    (should (eq (plist-get result :strategy) 'replace)))
  ;; :error strategy signals on the same input.
  (let ((bytes (nelisp-coding-test--bytes ?A #x82 #x20 ?B)))
    (should-error
     (nelisp-coding-shift-jis-decode bytes 'error)
     :type 'nelisp-coding-invalid-byte))
  ;; Truncated lead at end-of-input.
  (let* ((bytes (nelisp-coding-test--bytes ?A #x82))
         (result (nelisp-coding-shift-jis-decode bytes 'replace)))
    (should (equal (plist-get result :string) (string ?A #xFFFD)))
    (should (= (plist-get result :replacements) 1))))

;;;; 21. Shift-JIS encode round-trip X 0208 — encode→decode identity

(ert-deftest nelisp-coding-shift-jis-encode-roundtrip-x0208 ()
  "Encoding hiragana \"あいうえお\" via `nelisp-coding-shift-jis-encode'
then decoding the resulting bytes via `nelisp-coding-shift-jis-decode'
reproduces the original codepoints (MVP partial-table coverage)."
  (let* ((original (string #x3042 #x3044 #x3046 #x3048 #x304A))
         (encoded (nelisp-coding-shift-jis-encode original))
         (encoded-bytes (plist-get encoded :bytes)))
    ;; 5 chars × 2 bytes = 10 bytes total.
    (should (= (length encoded-bytes) 10))
    (should (= (plist-get encoded :replacements) 0))
    (should (null (plist-get encoded :invalid-positions)))
    ;; Round-trip via decode.
    (let* ((decoded (nelisp-coding-shift-jis-decode encoded-bytes))
           (decoded-string (plist-get decoded :string)))
      (should (equal decoded-string original))
      (should (= (plist-get decoded :replacements) 0))))
  ;; ASCII passthrough on encode.
  (let* ((result (nelisp-coding-shift-jis-encode "Hello")))
    (should (equal (plist-get result :bytes) (list ?H ?e ?l ?l ?o)))
    (should (= (plist-get result :replacements) 0)))
  ;; Halfwidth katakana ｱ (U+FF71) → single byte 0xB1.
  (let* ((result (nelisp-coding-shift-jis-encode (string #xFF71))))
    (should (equal (plist-get result :bytes) (list #xB1)))
    (should (= (plist-get result :replacements) 0))))

;;;; 22. Shift-JIS encode out-of-table — :error strategy signals

(ert-deftest nelisp-coding-shift-jis-encode-error-strategy-out-of-table ()
  "Encoding a Unicode codepoint not in the partial MVP SJIS table under
:error strategy signals `nelisp-coding-unmappable-codepoint' with offset
+ codepoint + encoding tag in the data plist. :replace strategy emits
ASCII '?' (0x3F) instead and reports the position in :invalid-positions.
:strict signals `nelisp-coding-strict-violation'."
  ;; U+1F980 (🦀 crab emoji) is well outside any JIS mapping.
  (let ((input (string ?A #x1F980 ?B)))
    (should-error
     (nelisp-coding-shift-jis-encode input 'error)
     :type 'nelisp-coding-unmappable-codepoint)
    ;; Verify signal data carries offset + codepoint + encoding.
    (let ((caught nil))
      (condition-case err
          (nelisp-coding-shift-jis-encode input 'error)
        (nelisp-coding-unmappable-codepoint
         (setq caught (cdr err))))
      (should caught)
      (should (equal (plist-get caught :offset) 1))
      (should (equal (plist-get caught :codepoint) #x1F980))
      (should (eq (plist-get caught :strategy) 'error))
      (should (eq (plist-get caught :encoding) 'shift-jis))))
  ;; :replace strategy: emits 0x3F at the unmappable position.
  (let* ((input (string ?A #x1F980 ?B))
         (result (nelisp-coding-shift-jis-encode input 'replace)))
    (should (equal (plist-get result :bytes) (list ?A #x3F ?B)))
    (should (= (plist-get result :replacements) 1))
    (should (equal (plist-get result :invalid-positions) '(1))))
  ;; :strict strategy signals the strict-violation symbol.
  (let ((input (string #x1F980)))
    (should-error
     (nelisp-coding-shift-jis-encode input 'strict)
     :type 'nelisp-coding-strict-violation)
    ;; Both strict and unmappable are subtypes of nelisp-coding-error.
    (let ((caught-parent nil))
      (condition-case _err
          (nelisp-coding-shift-jis-encode input 'error)
        (nelisp-coding-error
         (setq caught-parent t)))
      (should caught-parent))))

;;;; 23. EUC-JP decode ASCII passthrough — single-byte 0x00-0x7F

(ert-deftest nelisp-coding-euc-jp-decode-ascii-passthrough ()
  "EUC-JP ASCII range (0x00-0x7F) decodes as single-byte passthrough,
identical to UTF-8 / Shift-JIS ASCII compatibility."
  (let* ((bytes (nelisp-coding-test--bytes ?H ?e ?l ?l ?o))
         (result (nelisp-coding-euc-jp-decode bytes)))
    (should (equal (plist-get result :string) "Hello"))
    (should (= (plist-get result :replacements) 0)))
  ;; Empty input.
  (let ((empty (nelisp-coding-euc-jp-decode '())))
    (should (equal (plist-get empty :string) "")))
  ;; Mixed ASCII + multibyte: "A" + あ (= 0xA4 0xA2)
  (let* ((bytes (nelisp-coding-test--bytes ?A #xA4 #xA2))
         (result (nelisp-coding-euc-jp-decode bytes)))
    (should (equal (plist-get result :string) (string ?A #x3042)))
    (should (= (plist-get result :replacements) 0))))

;;;; 24. EUC-JP decode X 0208 2-byte — 0xA4A2 → あ (U+3042)

(ert-deftest nelisp-coding-euc-jp-decode-x0208-2byte ()
  "EUC-JP CS1 (JIS X 0208) 2-byte sequence 0xA4 0xA2 decodes to U+3042 (あ).
Both bytes are in 0xA1-0xFE range = valid CS1."
  (let* ((bytes (nelisp-coding-test--bytes #xA4 #xA2))
         (result (nelisp-coding-euc-jp-decode bytes)))
    (should (equal (plist-get result :string) (string #x3042)))
    (should (= (plist-get result :replacements) 0)))
  ;; Multi-char: あいう = 0xA4A2 0xA4A4 0xA4A6
  (let* ((bytes (nelisp-coding-test--bytes #xA4 #xA2 #xA4 #xA4 #xA4 #xA6))
         (result (nelisp-coding-euc-jp-decode bytes)))
    (should (equal (plist-get result :string)
                   (string #x3042 #x3044 #x3046))))
  ;; Truncated CS1 lead at end-of-input emits U+FFFD under :replace.
  (let* ((bytes (nelisp-coding-test--bytes ?A #xA4))
         (result (nelisp-coding-euc-jp-decode bytes 'replace)))
    (should (equal (plist-get result :string) (string ?A #xFFFD)))
    (should (= (plist-get result :replacements) 1))))

;;;; 25. EUC-JP decode X 0212 3-byte — 0x8FB0A1 → 丂 (U+4E02)

(ert-deftest nelisp-coding-euc-jp-decode-x0212-3byte ()
  "EUC-JP CS3 (JIS X 0212) 3-byte sequence 0x8F 0xB0 0xA1 decodes to
U+4E02 (丂). 0x8F is the X 0212 single-shift prefix; the next two bytes
are the X 0212 row/cell (both in 0xA1-0xFE range)."
  (let* ((bytes (nelisp-coding-test--bytes #x8F #xB0 #xA1))
         (result (nelisp-coding-euc-jp-decode bytes)))
    (should (equal (plist-get result :string) (string #x4E02)))
    (should (= (plist-get result :replacements) 0)))
  ;; Truncated 0x8F (only 1 trailing byte after the prefix) → U+FFFD under
  ;; :replace. After flagging 0x8F invalid (advance 1), pos 2 is 0xB0 which
  ;; is itself a CS1 X 0208 lead at end-of-input (truncated) → second invalid.
  (let* ((bytes (nelisp-coding-test--bytes ?A #x8F #xB0))
         (result (nelisp-coding-euc-jp-decode bytes 'replace)))
    (should (= (plist-get result :replacements) 2))
    (should (equal (plist-get result :invalid-positions) '(1 2))))
  ;; Bad CS3 byte structure (b1 outside 0xA1-0xFE) → invalid.
  ;; After flagging 0x8F invalid (advance 1), pos 1 is 0x20 (ASCII OK),
  ;; then pos 2 is 0xA1 = CS1 lead but truncated end-of-input (= second
  ;; invalid). Both U+FFFD with offsets 0 and 2.
  (let* ((bytes (nelisp-coding-test--bytes #x8F #x20 #xA1))
         (result (nelisp-coding-euc-jp-decode bytes 'replace)))
    (should (= (plist-get result :replacements) 2))
    (should (equal (plist-get result :invalid-positions) '(0 2)))))

;;;; 26. EUC-JP decode katakana 2-byte — 0x8E + 0xB1 → ｱ (U+FF71)

(ert-deftest nelisp-coding-euc-jp-decode-katakana-2byte ()
  "EUC-JP CS2 (JIS X 0201 halfwidth katakana) 2-byte sequence 0x8E 0xB1
decodes to U+FF71 (ｱ). 0x8E is the katakana single-shift prefix; the next
byte (0xA1-0xDF) maps to U+FF61-U+FF9F."
  (let* ((bytes (nelisp-coding-test--bytes #x8E #xB1))
         (result (nelisp-coding-euc-jp-decode bytes)))
    (should (equal (plist-get result :string) (string #xFF71)))
    (should (= (plist-get result :replacements) 0)))
  ;; Multi katakana ｱｲｳ = 8EB1 8EB2 8EB3 → U+FF71 U+FF72 U+FF73
  (let* ((bytes (nelisp-coding-test--bytes #x8E #xB1 #x8E #xB2 #x8E #xB3))
         (result (nelisp-coding-euc-jp-decode bytes)))
    (should (equal (plist-get result :string)
                   (string #xFF71 #xFF72 #xFF73))))
  ;; Bad CS2 trail (outside 0xA1-0xDF) → :replace emits U+FFFD.
  (let* ((bytes (nelisp-coding-test--bytes #x8E #x20))
         (result (nelisp-coding-euc-jp-decode bytes 'replace)))
    (should (= (plist-get result :replacements) 1))))

;;;; 27. EUC-JP encode round-trip X 0208 — encode→decode identity

(ert-deftest nelisp-coding-euc-jp-encode-roundtrip-x0208 ()
  "Encoding hiragana \"あいうえお\" via `nelisp-coding-euc-jp-encode'
then decoding the resulting bytes via `nelisp-coding-euc-jp-decode'
reproduces the original codepoints (MVP partial-table coverage)."
  (let* ((original (string #x3042 #x3044 #x3046 #x3048 #x304A))
         (encoded (nelisp-coding-euc-jp-encode original))
         (encoded-bytes (plist-get encoded :bytes)))
    ;; 5 chars × 2 bytes = 10 bytes (all CS1 X 0208).
    (should (= (length encoded-bytes) 10))
    (should (= (plist-get encoded :replacements) 0))
    (should (null (plist-get encoded :invalid-positions)))
    ;; Round-trip.
    (let* ((decoded (nelisp-coding-euc-jp-decode encoded-bytes))
           (decoded-string (plist-get decoded :string)))
      (should (equal decoded-string original))
      (should (= (plist-get decoded :replacements) 0))))
  ;; Halfwidth katakana ｱ (U+FF71) → 0x8E 0xB1 (= CS2 2-byte sequence).
  (let* ((result (nelisp-coding-euc-jp-encode (string #xFF71))))
    (should (equal (plist-get result :bytes) (list #x8E #xB1)))
    (should (= (plist-get result :replacements) 0)))
  ;; X 0212 supplementary kanji 丂 (U+4E02) → 0x8F 0xB0 0xA1 (= CS3 3-byte).
  (let* ((result (nelisp-coding-euc-jp-encode (string #x4E02))))
    (should (equal (plist-get result :bytes) (list #x8F #xB0 #xA1)))
    (should (= (plist-get result :replacements) 0)))
  ;; ASCII passthrough.
  (let* ((result (nelisp-coding-euc-jp-encode "Hi")))
    (should (equal (plist-get result :bytes) (list ?H ?i)))
    (should (= (plist-get result :replacements) 0))))

;;;; 28. JIS tables verify-hash passes — golden SHA-256 一致

(ert-deftest nelisp-coding-jis-tables-verify-hash-passes ()
  "Golden SHA-256 hash (`nelisp-coding-jis-tables-sha256') matches the
re-computed SHA-256 of the four concatenated partial-table prin1 strings.
This guards against accidental table mutation from rebase / merge that
would silently rewrite entries (Doc 31 v2 §6.10 + §7.2)."
  ;; Verify returns t on match.
  (should (eq (nelisp-coding-jis-tables-verify-hash) t))
  ;; Re-computing manually matches the constant.
  (let ((computed (secure-hash 'sha256
                               (nelisp-coding-jis-tables--canonical-bytes))))
    (should (equal computed nelisp-coding-jis-tables-sha256)))
  ;; Hash is exactly 64 hex chars (lowercase).
  (should (= (length nelisp-coding-jis-tables-sha256) 64))
  (should (string-match-p "\\`[0-9a-f]+\\'"
                          nelisp-coding-jis-tables-sha256))
  ;; Mismatched hash signals nelisp-coding-table-corruption.
  (let ((nelisp-coding-jis-tables-sha256 "0000000000000000000000000000000000000000000000000000000000000000"))
    (should-error
     (nelisp-coding-jis-tables-verify-hash)
     :type 'nelisp-coding-table-corruption)))

;;;; ──────────────────────────────────────────────────────────────────
;;;; Phase 7.4.4 — streaming API + file I/O ERT (+8, Doc 31 v2 §3.4 / §6.5)
;;;; ──────────────────────────────────────────────────────────────────

;;;; helpers — chunk-splitter

(defun nelisp-coding-test--chunk-bytes (bytes chunk-size)
  "Split BYTES (list/vector/string) into a list of unibyte chunks of CHUNK-SIZE."
  (let* ((lst (cond ((listp bytes) bytes)
                    ((vectorp bytes) (append bytes nil))
                    ((stringp bytes)
                     (mapcar (lambda (c) (logand c #xFF))
                             (append bytes nil)))
                    (t (error "Unsupported type"))))
         (n (length lst))
         (chunks '())
         (i 0))
    (while (< i n)
      (let ((end (min n (+ i chunk-size))))
        (push (cl-subseq lst i end) chunks)
        (setq i end)))
    (nreverse chunks)))

(defun nelisp-coding-test--decode-stream (encoding chunks &optional strategy)
  "Decode CHUNKS (list of byte lists / strings) under ENCODING and return result plist."
  (let ((state (nelisp-coding-stream-state-create encoding 'decode strategy)))
    (dolist (chunk chunks)
      (nelisp-coding-stream-decode-chunk state chunk))
    (nelisp-coding-stream-decode-finalize state)))

;;;; 29. UTF-8 stream decode in a single chunk = identical to one-shot

(ert-deftest nelisp-coding-stream-decode-utf8-single-chunk ()
  "Streaming decode with one big chunk equals the one-shot decode result."
  (let* ((s "Hello, 世界! 🦀")
         (bytes (nelisp-coding-utf8-encode s))
         (one-shot (nelisp-coding-utf8-decode bytes))
         (stream-result (nelisp-coding-test--decode-stream
                         'utf-8 (list bytes))))
    (should (equal (plist-get one-shot :string)
                   (plist-get stream-result :string)))
    (should (= (plist-get stream-result :chunks-processed) 1))
    (should (= (plist-get stream-result :chars-emitted) (length s)))
    (should (equal (plist-get stream-result :invalid-positions) nil))
    (should (= (plist-get stream-result :replacements) 0))))

;;;; 30. UTF-8 stream decode with multiple chunks = identical result

(ert-deftest nelisp-coding-stream-decode-utf8-multi-chunk ()
  "Multi-chunk UTF-8 decode produces identical result to one-shot."
  (let* ((s "あいうえお漢字テスト🎉🎊")
         (bytes (nelisp-coding-utf8-encode s))
         (one-shot (nelisp-coding-utf8-decode bytes)))
    (dolist (chunk-size '(1 2 3 4 5 7 11 16 32 64))
      (let* ((chunks (nelisp-coding-test--chunk-bytes bytes chunk-size))
             (stream-result (nelisp-coding-test--decode-stream
                             'utf-8 chunks)))
        (should (equal (plist-get one-shot :string)
                       (plist-get stream-result :string)))
        (should (= (plist-get stream-result :chars-emitted) (length s)))
        (should (= (plist-get stream-result :replacements) 0))))))

;;;; 31. UTF-8 multi-byte char split exactly across chunk boundary

(ert-deftest nelisp-coding-stream-decode-utf8-multibyte-boundary ()
  "3-byte UTF-8 char split at every interior position decodes correctly."
  ;; あ = E3 81 82 (3 bytes); split [1|2] and [2|1]
  (let* ((bytes (nelisp-coding-utf8-encode "あ"))
         (one-shot-string (plist-get (nelisp-coding-utf8-decode bytes) :string)))
    ;; Split at position 1: [E3] [81 82]
    (let* ((c1 (cl-subseq bytes 0 1))
           (c2 (cl-subseq bytes 1))
           (result (nelisp-coding-test--decode-stream
                    'utf-8 (list c1 c2))))
      (should (equal (plist-get result :string) one-shot-string))
      (should (= (plist-get result :chars-emitted) 1)))
    ;; Split at position 2: [E3 81] [82]
    (let* ((c1 (cl-subseq bytes 0 2))
           (c2 (cl-subseq bytes 2))
           (result (nelisp-coding-test--decode-stream
                    'utf-8 (list c1 c2))))
      (should (equal (plist-get result :string) one-shot-string))))
  ;; 4-byte char (emoji) split at every interior position.
  (let* ((bytes (nelisp-coding-utf8-encode "🎉"))
         (one-shot-string (plist-get (nelisp-coding-utf8-decode bytes) :string)))
    (dolist (split '(1 2 3))
      (let* ((c1 (cl-subseq bytes 0 split))
             (c2 (cl-subseq bytes split))
             (result (nelisp-coding-test--decode-stream
                      'utf-8 (list c1 c2))))
        (should (equal (plist-get result :string) one-shot-string))
        (should (= (plist-get result :chars-emitted) 1))))))

;;;; 32. Shift-JIS multi-byte split across chunk boundary

(ert-deftest nelisp-coding-stream-decode-shift-jis-multibyte-boundary ()
  "Shift-JIS 2-byte sequence split at the lead/trail boundary decodes correctly.

Shift-JIS encoding of あ = 82 A0 (lead 0x82, trail 0xA0). Split between
the two bytes must produce identical decoded char as one-shot."
  (let* ((bytes (list #x82 #xA0))                ; あ in SJIS
         (one-shot (nelisp-coding-shift-jis-decode bytes))
         (one-shot-string (plist-get one-shot :string)))
    ;; Single chunk control.
    (should (equal one-shot-string "あ"))
    ;; Split [82] [A0] = lead in chunk 1, trail in chunk 2.
    (let ((result (nelisp-coding-test--decode-stream
                   'shift-jis (list (list #x82) (list #xA0)))))
      (should (equal (plist-get result :string) "あ"))
      (should (= (plist-get result :chars-emitted) 1))
      (should (equal (plist-get result :invalid-positions) nil))))
  ;; Multi-char SJIS string split at varying chunk sizes.
  (let* ((bytes (list #x82 #xA0 #x82 #xA2 #x82 #xA4))  ; あいう in SJIS
         (one-shot (nelisp-coding-shift-jis-decode bytes))
         (expected (plist-get one-shot :string)))
    (should (equal expected "あいう"))
    (dolist (chunk-size '(1 2 3 4 5))
      (let* ((chunks (nelisp-coding-test--chunk-bytes bytes chunk-size))
             (result (nelisp-coding-test--decode-stream
                      'shift-jis chunks)))
        (should (equal (plist-get result :string) expected))
        (should (= (plist-get result :chars-emitted) 3))))))

;;;; 33. Truncated multi-byte at end-of-stream — finalize :replace

(ert-deftest nelisp-coding-stream-decode-truncated-finalize-replace ()
  "Trailing incomplete UTF-8 sequence emits one U+FFFD on finalize (replace)."
  ;; "あ" (E3 81 82) but truncated to (E3 81) then finalize.
  (let* ((bytes (list #xE3 #x81))
         (state (nelisp-coding-stream-state-create 'utf-8 'decode 'replace)))
    (nelisp-coding-stream-decode-chunk state bytes)
    (let ((result (nelisp-coding-stream-decode-finalize state)))
      (should (= (length (plist-get result :string)) 1))
      (should (= (aref (plist-get result :string) 0)
                 nelisp-coding-utf8-replacement-char))
      (should (= (plist-get result :replacements) 1))
      (should (= (length (plist-get result :invalid-positions)) 2))))
  ;; :error strategy must signal at finalize.
  (let* ((bytes (list #xE3 #x81))
         (state (nelisp-coding-stream-state-create 'utf-8 'decode 'error)))
    (nelisp-coding-stream-decode-chunk state bytes)
    (should-error
     (nelisp-coding-stream-decode-finalize state)
     :type 'nelisp-coding-invalid-byte))
  ;; :strict strategy must signal at finalize.
  (let* ((bytes (list #xE3 #x81))
         (state (nelisp-coding-stream-state-create 'utf-8 'decode 'strict)))
    (nelisp-coding-stream-decode-chunk state bytes)
    (should-error
     (nelisp-coding-stream-decode-finalize state)
     :type 'nelisp-coding-strict-violation)))

;;;; 34. Stream encode multi-chunk identity

(ert-deftest nelisp-coding-stream-encode-multi-chunk ()
  "Multi-chunk encode produces identical bytes to one-shot encode."
  (let* ((s "Hello, 世界! 🦀 + あいうえお"))
    ;; UTF-8
    (let ((expected-bytes (nelisp-coding-utf8-encode s)))
      (dolist (chunk-size '(1 3 5 7 11))
        (let* ((state (nelisp-coding-stream-state-create
                       'utf-8 'encode 'replace))
               (i 0)
               (n (length s)))
          (while (< i n)
            (let* ((end (min n (+ i chunk-size)))
                   (chunk (substring s i end)))
              (nelisp-coding-stream-encode-chunk state chunk)
              (setq i end)))
          (let ((result (nelisp-coding-stream-encode-finalize state)))
            (should (equal (plist-get result :bytes) expected-bytes))
            (should (= (plist-get result :chars-consumed) (length s)))))))
    ;; Latin-1 — only ASCII portion to stay in range.
    (let* ((s "Hello world")
           (expected (plist-get (nelisp-coding-latin1-encode s) :bytes)))
      (dolist (chunk-size '(1 3 7))
        (let* ((state (nelisp-coding-stream-state-create
                       'latin-1 'encode 'replace))
               (i 0)
               (n (length s)))
          (while (< i n)
            (let* ((end (min n (+ i chunk-size)))
                   (chunk (substring s i end)))
              (nelisp-coding-stream-encode-chunk state chunk)
              (setq i end)))
          (let ((result (nelisp-coding-stream-encode-finalize state)))
            (should (equal (plist-get result :bytes) expected))))))))

;;;; 35. read-file-with-encoding round-trip on UTF-8

(ert-deftest nelisp-coding-read-file-with-encoding-utf8-roundtrip ()
  "write → read with UTF-8 + chunked stream codec round-trips cleanly."
  (let* ((s "round-trip テスト 🔁\n2行目\n")
         (tmp (make-temp-file "nelisp-coding-stream-roundtrip-" nil ".bin")))
    (unwind-protect
        (progn
          (let ((write-result
                 (nelisp-coding-write-file-with-encoding tmp s 'utf-8 'replace 8)))
            (should (= (plist-get write-result :replacements) 0))
            (should (equal (plist-get write-result :path) tmp))
            (should (= (plist-get write-result :chars-consumed) (length s))))
          (let ((read-result
                 (nelisp-coding-read-file-with-encoding tmp 'utf-8 'replace 8)))
            (should (equal (plist-get read-result :string) s))
            (should (= (plist-get read-result :replacements) 0))
            (should (equal (plist-get read-result :invalid-positions) nil))
            (should (equal (plist-get read-result :path) tmp))
            (should (>= (plist-get read-result :chunks-processed) 1))))
      (delete-file tmp)))
  ;; Round-trip Shift-JIS too — covers table-based encoding via the same path.
  (let* ((s "AあいB")
         (tmp (make-temp-file "nelisp-coding-stream-sjis-rt-" nil ".bin")))
    (unwind-protect
        (progn
          (nelisp-coding-write-file-with-encoding tmp s 'shift-jis 'replace 4)
          (let ((read-result
                 (nelisp-coding-read-file-with-encoding tmp 'shift-jis 'replace 4)))
            (should (equal (plist-get read-result :string) s))
            (should (= (plist-get read-result :replacements) 0))))
      (delete-file tmp))))

;;;; 36. 1MB UTF-8 stream stress test (Doc 31 v2 §6.5 silent-corruption guard)

(ert-deftest nelisp-coding-stream-decode-1mb-stress ()
  "Stream decode of ~1MB random-valid UTF-8 in 64KB chunks matches one-shot.

Doc 31 v2 §6.5: silent corruption at chunk boundaries is the dominant
risk for streaming codecs. Build a string with every BMP and astral
sequence length (1/2/3/4 byte UTF-8) repeated until the byte count
exceeds 1MB, decode in 64KB chunks, and assert byte-for-byte equality
with the one-shot decode."
  (let* ((seed-pool '(?A ?Z ?a ?z ?0 ?9
                      ?¡ ?¶ ?þ              ; 2-byte (Latin)
                      ?あ ?い ?う ?ア     ; 3-byte (Hiragana/Katakana)
                      ?中 ?文 ?漢              ; 3-byte (CJK)
                      ?\U0001F389 ?\U0001F4A1              ; 4-byte (emoji)
                      ?\U0001F980))
         (target-bytes (* 1 1024 1024))
         (chunks (make-list 0 nil))
         (acc-bytes 0)
         ;; Build by concatenating string fragments until target reached.
         (string-frags '()))
    (while (< acc-bytes target-bytes)
      (let* ((cp (nth (random (length seed-pool)) seed-pool))
             (frag (string cp))
             (frag-bytes (nelisp-coding-utf8-encode frag)))
        (push frag string-frags)
        (cl-incf acc-bytes (length frag-bytes))))
    (let* ((s (apply #'concat (nreverse string-frags)))
           (bytes (nelisp-coding-utf8-encode s))
           (one-shot (nelisp-coding-utf8-decode bytes))
           (chunks (nelisp-coding-test--chunk-bytes bytes (* 64 1024)))
           (stream-result (nelisp-coding-test--decode-stream
                           'utf-8 chunks)))
      (should (equal (plist-get one-shot :string)
                     (plist-get stream-result :string)))
      (should (= (plist-get stream-result :replacements) 0))
      (should (equal (plist-get stream-result :invalid-positions) nil))
      ;; ≥16 chunks confirms streaming path actually exercised.
      (should (>= (plist-get stream-result :chunks-processed) 16))
      (should (= (plist-get stream-result :chars-emitted) (length s))))))

;;;; ──────────────────────────────────────────────────────────────────
;;;; Phase 7.4.5 — full 14000+ entry table validation ERT (+6)
;;;; ──────────────────────────────────────────────────────────────────

;;;; 37. Full X 0208 entry count >= 6000 (vs ~343 MVP partial)

(ert-deftest nelisp-coding-jis-tables-full-x0208-entry-count ()
  "Full SJIS X 0208 decode table has >= 6000 entries (real CP932-derived
mapping, vs MVP partial which had only 343).  This is the smoke test for
Phase 7.4.5 full generation: if the partial MVP table is still in place,
this assertion fires."
  (let ((n (length nelisp-coding-shift-jis-x0208-decode-table)))
    (should (>= n 6000))
    ;; Sanity upper bound: JIS X 0208 has at most 94*94 = 8836 cells.
    (should (<= n 8836)))
  ;; Spot-check a known X 0208 entry that was *not* in the MVP partial
  ;; (i.e., proves the full table replaced the partial).
  ;; SJIS 0x8C78 → 警 (U+8B66) — from CP932.TXT, not in T25 hand-curated MVP.
  (let* ((bytes (nelisp-coding-test--bytes #x8C #x78))
         (result (nelisp-coding-shift-jis-decode bytes)))
    (should (equal (plist-get result :string) (string #x8B66)))))

;;;; 38. Full CP932 extension entry count ~845

(ert-deftest nelisp-coding-jis-tables-full-cp932-extension-count ()
  "Full CP932 vendor extension decode table has 800-900 entries (= NEC
special 0x8740-0x879C + NEC selected IBM 0xED40-0xEEFC + IBM extension
0xFA40-0xFC4B per Microsoft CP932.TXT)."
  (let ((n (length nelisp-coding-cp932-extension-decode-table)))
    (should (>= n 800))
    (should (<= n 900)))
  ;; Spot-check a known NEC selected IBM entry that wasn't in MVP.
  ;; SJIS 0xED40 → 纊 (U+7E8A) — first NEC selected IBM cell.
  (let* ((bytes (nelisp-coding-test--bytes #xED #x40))
         (result (nelisp-coding-shift-jis-decode bytes)))
    (should (= (plist-get result :replacements) 0))
    (should (= (length (plist-get result :string)) 1))))

;;;; 39. Full X 0212 entry count >= 5000

(ert-deftest nelisp-coding-jis-tables-full-jis0212-count ()
  "Full EUC-JP X 0212 decode table has >= 5000 entries (real
JIS0212.TXT-derived mapping, vs MVP partial which had only 64)."
  (let ((n (length nelisp-coding-euc-jp-x0212-decode-table)))
    (should (>= n 5000))
    ;; Sanity upper bound: JIS X 0212 has at most 94*94 = 8836 cells.
    (should (<= n 8836)))
  ;; Spot-check a known X 0212 entry that wasn't in MVP partial:
  ;; JIS 0x222F → BREVE (U+02D8). EUC = (0x22|0x80, 0x2F|0x80) = 0xA2AF.
  (let* ((bytes (nelisp-coding-test--bytes #x8F #xA2 #xAF))
         (result (nelisp-coding-euc-jp-decode bytes)))
    (should (equal (plist-get result :string) (string #x02D8)))
    (should (= (plist-get result :replacements) 0))))

;;;; 40. 100 random X 0208 codepoint round-trip identity

(ert-deftest nelisp-coding-jis-tables-full-roundtrip-100-random-x0208 ()
  "Pick 100 random entries from the full SJIS X 0208 decode table and
verify decode → encode → decode round-trip identity.  This catches table
corruption that would let one direction succeed while the other fails."
  (let* ((table nelisp-coding-shift-jis-x0208-decode-table)
         (n (length table))
         (samples '()))
    (should (>= n 6000))
    ;; Deterministic-ish sample: stride evenly across the table.
    (let ((stride (max 1 (/ n 100))))
      (cl-loop for i from 0 below n by stride
               for entry = (nth i table)
               when entry
               do (push entry samples)
               while (< (length samples) 100)))
    (should (>= (length samples) 50))
    (dolist (entry samples)
      (let* ((sjis-int (car entry))
             (cp       (cdr entry))
             (lead     (logand (ash sjis-int -8) #xFF))
             (trail    (logand sjis-int #xFF))
             ;; decode SJIS bytes → string
             (decoded (nelisp-coding-shift-jis-decode
                       (nelisp-coding-test--bytes lead trail))))
        (should (= (plist-get decoded :replacements) 0))
        (should (equal (plist-get decoded :string) (string cp)))
        ;; encode that string back
        (let* ((encoded (nelisp-coding-shift-jis-encode (string cp)))
               (bytes (plist-get encoded :bytes)))
          (should (= (plist-get encoded :replacements) 0))
          ;; The bytes might map to a *different* SJIS code (= Unicode
          ;; collision: multiple JIS codes share the same CP), so re-decode
          ;; to confirm round-trip identity at the codepoint level.
          (let ((re-decoded (nelisp-coding-shift-jis-decode bytes)))
            (should (equal (plist-get re-decoded :string) (string cp)))))))))

;;;; 41. Real Windows Japanese sample bytes decode

(ert-deftest nelisp-coding-shift-jis-decode-real-windows-japanese-text ()
  "Decode a representative real-world Windows-31J (CP932) byte sequence
and assert the expected Unicode string.  This validates that the full
table covers the JIS X 0208 and CP932-extension entries that appear in
real Windows / DOS Japanese text files (proper-noun kanji, common words,
NEC special chars)."
  ;; "東京都" (Tokyo metropolis) in Shift-JIS:
  ;;   東 = 0x93 0x8C → U+6771
  ;;   京 = 0x8B 0x9E → U+4EAC
  ;;   都 = 0x93 0x73 → U+90FD
  (let* ((bytes (nelisp-coding-test--bytes #x93 #x8C #x8B #x9E #x93 #x73))
         (result (nelisp-coding-shift-jis-decode bytes)))
    (should (equal (plist-get result :string) (string #x6771 #x4EAC #x90FD)))
    (should (= (plist-get result :replacements) 0)))
  ;; "電気管理技術者" (electrical management engineer) — proper-noun-y kanji
  ;; needing the full table, none of which were in the MVP partial.
  ;;   電 = 0x93 0x64 → U+96FB
  ;;   気 = 0x8B 0x43 → U+6C17
  ;;   管 = 0x8A C7   → U+7BA1
  ;;   理 = 0x97 0x9D → U+7406
  ;;   技 = 0x8B 0x5A → U+6280
  ;;   術 = 0x8F 0x70 → U+8853
  ;;   者 = 0x8E 0xD2 → U+8005
  (let* ((bytes (nelisp-coding-test--bytes
                 #x93 #x64 #x8B #x43 #x8A #xC7 #x97 #x9D
                 #x8B #x5A #x8F #x70 #x8E #xD2))
         (result (nelisp-coding-shift-jis-decode bytes)))
    (should (equal (plist-get result :string)
                   (string #x96FB #x6C17 #x7BA1 #x7406
                           #x6280 #x8853 #x8005)))
    (should (= (plist-get result :replacements) 0)))
  ;; CP932 NEC special: ① (U+2460) = 0x87 0x40 — already validated by
  ;; T25 ERT 19, but reaffirm it's still mapped post-regeneration.
  (let* ((bytes (nelisp-coding-test--bytes #x87 #x40))
         (result (nelisp-coding-shift-jis-decode bytes)))
    (should (equal (plist-get result :string) (string #x2460)))))

;;;; 42. Verify hash with full table — golden SHA-256 differs from MVP partial

(ert-deftest nelisp-coding-jis-tables-verify-hash-with-full-table ()
  "Verify the golden SHA-256 hash matches the full-table content (Phase
7.4.5 generator output) AND is *different* from the Phase 7.4.3 MVP
partial-table hash.  This is the binding contract that the file in the
tree was regenerated, not silently reverted."
  ;; Verify-hash passes (= file content matches embedded golden hash).
  (should (eq (nelisp-coding-jis-tables-verify-hash) t))
  ;; The full-table hash differs from the MVP partial hash.
  (let ((mvp-partial-hash
         "eb0e024fd054b293edc263868d3f0d3358af2892fdd6a84f882104f435b590cc"))
    (should-not (equal nelisp-coding-jis-tables-sha256 mvp-partial-hash)))
  ;; Hash format invariants (64-hex-char lowercase).
  (should (= (length nelisp-coding-jis-tables-sha256) 64))
  (should (string-match-p "\\`[0-9a-f]+\\'" nelisp-coding-jis-tables-sha256))
  ;; Total full-table entry count is in the documented range:
  ;; 6879 + 845 + 6879 + 6067 = 20670.
  (let ((total (+ (length nelisp-coding-shift-jis-x0208-decode-table)
                  (length nelisp-coding-cp932-extension-decode-table)
                  (length nelisp-coding-euc-jp-x0208-decode-table)
                  (length nelisp-coding-euc-jp-x0212-decode-table))))
    (should (>= total 14000))   ; Doc 31 v2 §6.10 "~14000 entry" target
    (should (<= total 25000)))) ; sanity upper bound

;;;; ──────────────────────────────────────────────────────────────────
;;;; T67 — EUC-JP streaming boundary safety ERT (+10, Doc 31 v2 §3.4 / T56 fix)
;;;; ──────────────────────────────────────────────────────────────────
;;
;; T56 codex audit identified a CRITICAL bug in
;; `nelisp-coding--stream-euc-jp-tail-pending': the original
;; implementation deferred *any* trailing 0xA1-0xFE byte as if it were
;; an incomplete CS1 lead, which mis-classified valid 2-byte CS1
;; (0xA4 0xA2 = あ), 2-byte CS2 (0x8E 0xB1 = ｱ), and 3-byte CS3
;; (0x8F 0xA2 0xAF = BREVE U+02D8) sequences whenever they happened to
;; sit at the chunk tail.  These tests enumerate every interior split
;; position for the three EUC-JP code-set forms and assert that
;; streaming decode at any chunk size produces byte-for-byte the same
;; result as a one-shot decode.

;;;; T67-1.  CS1 (X 0208) split between lead and trail — A4 | A2 = あ

(ert-deftest nelisp-coding-stream-decode-euc-jp-cs1-boundary ()
  "EUC-JP CS1 (X 0208) 2-byte sequence A4 A2 = あ split at every
interior chunk position decodes identically to one-shot.
Boundary [A4][A2] must NOT mis-classify A2 as a stray pending lead."
  (let* ((bytes (list #xA4 #xA2))
         (one-shot (nelisp-coding-euc-jp-decode bytes))
         (expected (plist-get one-shot :string)))
    (should (equal expected "あ"))
    ;; Single-chunk control.
    (let ((result (nelisp-coding-test--decode-stream
                   'euc-jp (list bytes))))
      (should (equal (plist-get result :string) expected))
      (should (= (plist-get result :replacements) 0)))
    ;; Split [A4][A2] — lead in chunk 1, trail in chunk 2.
    (let ((result (nelisp-coding-test--decode-stream
                   'euc-jp (list (list #xA4) (list #xA2)))))
      (should (equal (plist-get result :string) expected))
      (should (= (plist-get result :replacements) 0))
      (should (equal (plist-get result :invalid-positions) nil)))))

;;;; T67-2.  CS2 (halfwidth katakana) split — 8E | B1 = ｱ

(ert-deftest nelisp-coding-stream-decode-euc-jp-cs2-boundary ()
  "EUC-JP CS2 sequence 8E B1 = ｱ (U+FF71) split between the SS2 lead
(0x8E) and the katakana trail decodes correctly.  Pre-T67 the trail
byte (B1, in 0xA1-0xFE range) was deferred as a phantom CS1 lead and
silently reclassified."
  (let* ((bytes (list #x8E #xB1))
         (one-shot (nelisp-coding-euc-jp-decode bytes))
         (expected (plist-get one-shot :string)))
    (should (equal expected (string #xFF71)))
    ;; Single-chunk control.
    (let ((result (nelisp-coding-test--decode-stream
                   'euc-jp (list bytes))))
      (should (equal (plist-get result :string) expected)))
    ;; Split [8E][B1].
    (let ((result (nelisp-coding-test--decode-stream
                   'euc-jp (list (list #x8E) (list #xB1)))))
      (should (equal (plist-get result :string) expected))
      (should (= (plist-get result :replacements) 0))
      (should (equal (plist-get result :invalid-positions) nil)))))

;;;; T67-3.  CS3 (X 0212) split at 1st byte — 8F | A2 AF

(ert-deftest nelisp-coding-stream-decode-euc-jp-cs3-split-after-1st ()
  "EUC-JP CS3 sequence 8F A2 AF = BREVE (U+02D8) split after the SS3
lead (0x8F) decodes correctly via 2-byte pending carry-over."
  (let* ((bytes (list #x8F #xA2 #xAF))
         (one-shot (nelisp-coding-euc-jp-decode bytes))
         (expected (plist-get one-shot :string)))
    (should (equal expected (string #x02D8)))
    ;; Split [8F][A2 AF].
    (let ((result (nelisp-coding-test--decode-stream
                   'euc-jp (list (list #x8F) (list #xA2 #xAF)))))
      (should (equal (plist-get result :string) expected))
      (should (= (plist-get result :replacements) 0))
      (should (equal (plist-get result :invalid-positions) nil)))))

;;;; T67-4.  CS3 split at 2nd byte — 8F A2 | AF

(ert-deftest nelisp-coding-stream-decode-euc-jp-cs3-split-after-2nd ()
  "EUC-JP CS3 sequence 8F A2 AF split after the second byte (= 8F A2
buffered, AF arriving on next chunk) decodes correctly.  This was the
original mis-classification: pre-T67 the implementation only deferred
2 bytes when the *last* two bytes matched 8F + A1-FE, but if all
three bytes arrive in one chunk it stripped only the trailing byte
and decoded a phantom 2-byte CS1 pair."
  (let* ((bytes (list #x8F #xA2 #xAF))
         (one-shot (nelisp-coding-euc-jp-decode bytes))
         (expected (plist-get one-shot :string)))
    (should (equal expected (string #x02D8)))
    ;; Split [8F A2][AF].
    (let ((result (nelisp-coding-test--decode-stream
                   'euc-jp (list (list #x8F #xA2) (list #xAF)))))
      (should (equal (plist-get result :string) expected))
      (should (= (plist-get result :replacements) 0))
      (should (equal (plist-get result :invalid-positions) nil)))))

;;;; T67-5.  Full CS3 in one chunk — pre-T67 corruption regression

(ert-deftest nelisp-coding-stream-decode-euc-jp-cs3-single-chunk ()
  "Whole CS3 sequence 8F A2 AF arriving in a single chunk decodes
correctly.  The pre-T67 bug deferred 2 bytes (the [A2 AF] pair) and
left only [8F] for decode → emit 0xFFFD * 3.  Asserting :replacements
= 0 catches the silent corruption."
  (let* ((bytes (list #x8F #xA2 #xAF))
         (result (nelisp-coding-test--decode-stream
                  'euc-jp (list bytes))))
    (should (equal (plist-get result :string) (string #x02D8)))
    (should (= (plist-get result :replacements) 0))
    (should (equal (plist-get result :invalid-positions) nil))))

;;;; T67-6.  Multi-char CS1 sequence chunked at every byte boundary

(ert-deftest nelisp-coding-stream-decode-euc-jp-multichar-cs1 ()
  "Multi-character CS1 string chunked at every byte size 1..6 decodes
identically to one-shot.  Exercises the parity walk for runs of all
A1-FE bytes."
  ;; あいう = A4 A2 A4 A4 A4 A6 (6 bytes, all in 0xA1-0xFE range —
  ;; the worst case for the parity walk).
  (let* ((bytes (list #xA4 #xA2 #xA4 #xA4 #xA4 #xA6))
         (one-shot (nelisp-coding-euc-jp-decode bytes))
         (expected (plist-get one-shot :string)))
    (should (equal expected "あいう"))
    (dolist (chunk-size '(1 2 3 4 5 6))
      (let* ((chunks (nelisp-coding-test--chunk-bytes bytes chunk-size))
             (result (nelisp-coding-test--decode-stream
                      'euc-jp chunks)))
        (should (equal (plist-get result :string) expected))
        (should (= (plist-get result :replacements) 0))
        (should (= (plist-get result :chars-emitted) 3))))))

;;;; T67-7.  Mixed ASCII + CS1 + CS2 + CS3 sequence stress

(ert-deftest nelisp-coding-stream-decode-euc-jp-mixed-codesets ()
  "Mixed ASCII + CS1 + CS2 + CS3 sequence chunked at byte sizes 1..7
decodes identically to one-shot.  Exercises every anchor transition
in the parity walker."
  ;; A (41) | あ (A4 A2) | B (42) | ｱ (8E B1) | C (43) | BREVE (8F A2 AF)
  (let* ((bytes (list ?A #xA4 #xA2 ?B #x8E #xB1 ?C #x8F #xA2 #xAF))
         (one-shot (nelisp-coding-euc-jp-decode bytes))
         (expected (plist-get one-shot :string)))
    (should (equal expected
                   (concat "A" "あ" "B" (string #xFF71) "C"
                           (string #x02D8))))
    (dolist (chunk-size '(1 2 3 4 5 6 7))
      (let* ((chunks (nelisp-coding-test--chunk-bytes bytes chunk-size))
             (result (nelisp-coding-test--decode-stream
                      'euc-jp chunks)))
        (should (equal (plist-get result :string) expected))
        (should (= (plist-get result :replacements) 0))))))

;;;; T67-8.  One-shot vs stream equivalence for every interior split

(ert-deftest nelisp-coding-stream-decode-euc-jp-every-split-equivalence ()
  "For every interior split position of a multi-codeset EUC-JP byte
sequence, streaming decode produces byte-for-byte the same result as
one-shot decode.  N-1 split positions × 1 sequence = covers every
chunk-boundary case enumerable in this small input."
  (let* ((bytes (list ?A #xA4 #xA2 #x8E #xB1 #x8F #xA2 #xAF ?Z))
         (one-shot (nelisp-coding-euc-jp-decode bytes))
         (expected-string (plist-get one-shot :string))
         (n (length bytes)))
    (cl-loop for split from 1 below n
             do (let* ((c1 (cl-subseq bytes 0 split))
                       (c2 (cl-subseq bytes split))
                       (result (nelisp-coding-test--decode-stream
                                'euc-jp (list c1 c2))))
                  (should (equal (plist-get result :string)
                                 expected-string))
                  (should (= (plist-get result :replacements) 0))))))

;;;; T67-9.  Large EUC-JP stream stress (1KB-ish boundary smoke)

(ert-deftest nelisp-coding-stream-decode-euc-jp-1kb-stress ()
  "~1KB EUC-JP byte stream chunked at small/medium/large sizes
matches one-shot decode.  Confirms the parity walker scales without
mis-classification on long all-A1-FE runs."
  (let* ((unit (list #xA4 #xA2 #xA4 #xA4 #xA4 #xA6 ?  ; "あいう "
                     #x8E #xB1 ?  ; "ｱ "
                     #x8F #xA2 #xAF ?\n))
         (rep 80)                       ; ~1KB total
         (bytes (apply #'append (make-list rep unit)))
         (one-shot (nelisp-coding-euc-jp-decode bytes))
         (expected (plist-get one-shot :string)))
    (should (= (plist-get one-shot :replacements) 0))
    (dolist (chunk-size '(1 2 7 13 64 256 1024))
      (let* ((chunks (nelisp-coding-test--chunk-bytes bytes chunk-size))
             (result (nelisp-coding-test--decode-stream
                      'euc-jp chunks)))
        (should (equal (plist-get result :string) expected))
        (should (= (plist-get result :replacements) 0))))))

;;;; T67-10.  Truncated CS3 at end-of-stream — finalize :replace

(ert-deftest nelisp-coding-stream-decode-euc-jp-truncated-cs3-finalize ()
  "Trailing incomplete EUC-JP CS3 prefix (8F A2 with no third byte)
emits a single U+FFFD on finalize under :replace strategy, with two
recorded :invalid-positions (one per pending byte)."
  (let* ((bytes (list ?A #x8F #xA2))
         (state (nelisp-coding-stream-state-create
                 'euc-jp 'decode 'replace)))
    (nelisp-coding-stream-decode-chunk state bytes)
    (let ((result (nelisp-coding-stream-decode-finalize state)))
      ;; "A" + U+FFFD = 2 chars; replacement count = 1.
      (should (= (length (plist-get result :string)) 2))
      (should (= (aref (plist-get result :string) 0) ?A))
      (should (= (aref (plist-get result :string) 1)
                 nelisp-coding-utf8-replacement-char))
      (should (= (plist-get result :replacements) 1))
      ;; Both pending bytes recorded as invalid positions.
      (should (= (length (plist-get result :invalid-positions)) 2))))
  ;; :error finalize signals.
  (let* ((bytes (list #x8F #xA2))
         (state (nelisp-coding-stream-state-create
                 'euc-jp 'decode 'error)))
    (nelisp-coding-stream-decode-chunk state bytes)
    (should-error
     (nelisp-coding-stream-decode-finalize state)
     :type 'nelisp-coding-invalid-byte)))

;;;; T67-11.  read-file-with-encoding round-trip on EUC-JP

(ert-deftest nelisp-coding-read-file-with-encoding-euc-jp-roundtrip ()
  "write → read with EUC-JP + chunked stream codec round-trips
cleanly across boundary-prone byte patterns.  Validates that the T67
read-file streaming change (real incremental disk I/O via BEG/END)
preserves multi-byte sequences across chunk windows."
  (let* ((s "AあBｱC")  ; mixed CS0 + CS1 + CS2
         (tmp (make-temp-file "nelisp-coding-euc-jp-rt-" nil ".bin")))
    (unwind-protect
        (progn
          (nelisp-coding-write-file-with-encoding tmp s 'euc-jp 'replace 3)
          (dolist (chunk-size '(1 2 3 4 8 64))
            (let ((read-result
                   (nelisp-coding-read-file-with-encoding
                    tmp 'euc-jp 'replace chunk-size)))
              (should (equal (plist-get read-result :string) s))
              (should (= (plist-get read-result :replacements) 0))
              (should (equal (plist-get read-result :invalid-positions) nil)))))
      (delete-file tmp))))

(provide 'nelisp-coding-test)

;;; nelisp-coding-test.el ends here
