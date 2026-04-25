;;; nelisp-coding-test.el --- ERT tests for nelisp-coding  -*- lexical-binding: t; -*-

;; Doc 31 v2 LOCKED 2026-04-25 §3.1 sub-phase 7.4.1 — UTF-8 encode/decode +
;; BOM handling + 3 invalid-sequence strategy contract LOCK.
;; Doc 31 v2 LOCKED 2026-04-25 §3.2 sub-phase 7.4.2 — Latin-1 (ISO-8859-1)
;; encode/decode + placeholder handling for U+0100+.
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
Phase 7.4.1 では signal のみ; process abort integration は Phase 7.5。"
  (let ((bytes (nelisp-coding-test--bytes ?A #xFF ?B)))
    (should-error
     (nelisp-coding-utf8-decode bytes 'strict)
     :type 'nelisp-coding-strict-violation)
    ;; Verify both error symbols are subtypes of nelisp-coding-error
    ;; (parent type, allows uniform catch).
    (let ((caught-parent nil))
      (condition-case _err
          (nelisp-coding-utf8-decode bytes 'strict)
        (nelisp-coding-error
         (setq caught-parent t)))
      (should caught-parent))))

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
`nelisp-coding-strict-violation'. Phase 7.4.2 では signal のみ;
process abort integration は Phase 7.5。"
  (let ((input (string ?A #x3042)))
    (should-error
     (nelisp-coding-latin1-encode input 'strict)
     :type 'nelisp-coding-strict-violation)
    ;; Verify both error symbols are subtypes of nelisp-coding-error
    ;; (parent type, allows uniform catch with utf-8 strict path).
    (let ((caught-parent nil))
      (condition-case _err
          (nelisp-coding-latin1-encode input 'strict)
        (nelisp-coding-error
         (setq caught-parent t)))
      (should caught-parent))
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

(provide 'nelisp-coding-test)

;;; nelisp-coding-test.el ends here
