;;; nelisp-coding.el --- NeLisp self-hosted coding (encoding) layer  -*- lexical-binding: t; -*-

;; Phase 7.4.1 (Doc 31 v2 LOCKED 2026-04-25) — UTF-8 encode/decode +
;; BOM handling + 3 invalid-sequence strategy contract LOCK.
;; Phase 7.4.2 (Doc 31 v2 LOCKED 2026-04-25) — Latin-1 (ISO-8859-1)
;; encode/decode + placeholder handling for U+0100+ codepoints.
;; Phase 7.4.3 (Doc 31 v2 LOCKED 2026-04-25) — Shift-JIS (JIS X 0208 +
;; CP932 拡張) + EUC-JP (JIS X 0208 + JIS X 0212) encode/decode + table
;; data + golden SHA-256 hash check (MVP partial table, full ~14000 entry
;; deferred to Phase 7.5 generator).
;;
;; Scope (Phase 7.4.1 — Doc 31 v2 §3.1):
;;   - UTF-8 byte sequence (1-4 byte) encode/decode loop
;;   - BOM (=EF BB BF=) strip on read / no emit on write (default)
;;   - error handling 3 strategy (=replace= / =error= / =strict=)、default =replace=
;;   - =U+FFFD REPLACEMENT CHARACTER= emit on invalid byte (=replace= 時)
;;   - emoji / supplementary plane (=U+10000-U+10FFFF=) support
;;   - reject overlong / surrogate / >U+10FFFF / truncated / bad continuation
;;
;; Scope (Phase 7.4.2 — Doc 31 v2 §3.2):
;;   - Latin-1 (=ISO-8859-1=) single-byte encode/decode (=U+0000-U+00FF= ↔
;;     byte 0x00-0xFF, bijective)
;;   - decode = always succeeds (全 256 byte 値が valid Latin-1)
;;   - encode = U+0100+ codepoint で 3 strategy 分岐 (=replace= → '?'
;;     ASCII 0x3F default per §6.2 / =error= → signal
;;     `nelisp-coding-invalid-codepoint' / =strict= → signal
;;     `nelisp-coding-strict-violation')
;;   - placeholder codepoint customizable via
;;     `nelisp-coding-latin1-replacement-codepoint' (default ?\?, U+003F)
;;
;; Scope (Phase 7.4.3 — Doc 31 v2 §3.3):
;;   - Shift-JIS / CP932 (Windows-31J) decode: ASCII passthrough +
;;     JIS X 0201 katakana (0xA1-0xDF) + JIS X 0208 + CP932 拡張
;;     (NEC 特殊文字 + IBM 拡張)
;;   - Shift-JIS / CP932 encode: reverse-lookup Unicode → SJIS bytes,
;;     unmappable codepoint で 3 strategy 分岐 (replace / error / strict)
;;   - EUC-JP decode: ASCII passthrough + 0x8E (JIS X 0201 katakana) +
;;     0x8F (JIS X 0212 3-byte CS3) + 2-byte JIS X 0208 (CS1)
;;   - EUC-JP encode: reverse-lookup Unicode → EUC bytes, X 0212 → 3-byte,
;;     X 0208 → 2-byte
;;   - table data = =src/nelisp-coding-jis-tables.el= (separate file,
;;     generated artifact) with golden SHA-256 hash for tampering detection
;;   - MVP partial table (~885 entries) validates algorithm; full ~14000
;;     entry generation deferred to Phase 7.5 via =tools/coding-table-gen.el=
;;
;; Deferred to later sub-phases:
;;   - Phase 7.4.4: streaming chunk-based callback + file I/O integration
;;   - Phase 7.5: process-coding-system 本体実装、resume-coding primitive、
;;     real 14000 entry table generation via =tools/coding-table-gen.el=
;;
;; SBCL =sb-impl/external-formats.lisp= + Emacs =coding.c= dual precedent。
;; Phase 7.4.1 + 7.4.2 + 7.4.3 はそれら subset で UTF-8 / Latin-1 / Japanese
;; + BOM + 3 strategy。

;;; Code:

(require 'subr-x)
(require 'nelisp-coding-jis-tables)

;;; Customization

(defgroup nelisp-coding nil
  "NeLisp self-hosted coding (character ↔ byte) layer."
  :group 'nelisp
  :prefix "nelisp-coding-")

(defcustom nelisp-coding-utf8-bom-emit-on-write nil
  "If non-nil, emit UTF-8 BOM (EF BB BF) at start of UTF-8 encoded output.
Doc 31 v2 §2.3 推奨 A: default off (RFC 3629 推奨)。
=--coding-bom-emit= flag (Phase 7.5 で CLI 経由) と Windows tool
互換 escape hatch のための per-call =:bom-emit= 引数を front に置く。"
  :type 'boolean
  :group 'nelisp-coding)

(defcustom nelisp-coding-error-strategy 'replace
  "Default invalid-sequence handling strategy.
Doc 31 v2 §2.4 contract LOCK の 3 strategy:
- =replace= (default): U+FFFD REPLACEMENT CHARACTER emit、partial 結果返却
- =error=: signal `nelisp-coding-invalid-byte' (catchable via condition-case)
- =strict=: signal `nelisp-coding-strict-violation' (uncatchable in streaming、
  Phase 7.5 で process abort と integrate)"
  :type '(choice (const :tag "Replace with U+FFFD" replace)
                 (const :tag "Signal error (catchable)" error)
                 (const :tag "Strict (uncatchable, abort)" strict))
  :group 'nelisp-coding)

(defcustom nelisp-coding-latin1-replacement-codepoint ?\?
  "Replacement codepoint for U+0100+ chars in Latin-1 encoding (=:replace=
strategy default).

Doc 31 v2 §6.2: ASCII '?' (=U+003F=, byte =0x3F=) を default per Emacs
=coding.c= precedent。User は U+003F (default)、U+001A (SUB)、U+0020
(space) 等選択可。値は必ず Latin-1 範囲 (=U+0000-U+00FF=) でなければ
ならない (= encode 結果が必ず単 byte に収まる)。範囲外設定時は
encode 呼び出しで `nelisp-coding-invalid-codepoint' signal。"
  :type 'integer
  :group 'nelisp-coding)

;;; Constants

(defconst nelisp-coding-utf8-bom (list #xEF #xBB #xBF)
  "UTF-8 BOM byte sequence (EF BB BF) as list of integers.")

(defconst nelisp-coding-utf8-replacement-char #xFFFD
  "Unicode REPLACEMENT CHARACTER (U+FFFD), emitted by =:replace= strategy
on invalid byte sequence. WHATWG Encoding Standard 準拠 = 連続 invalid byte
は 1 個の U+FFFD に collapse。")

(defconst nelisp-coding-utf8-max-codepoint #x10FFFF
  "Maximum valid Unicode codepoint (RFC 3629).")

(defconst nelisp-coding-utf8-surrogate-min #xD800
  "Minimum codepoint of UTF-16 surrogate range (invalid in UTF-8).")

(defconst nelisp-coding-utf8-surrogate-max #xDFFF
  "Maximum codepoint of UTF-16 surrogate range (invalid in UTF-8).")

;;; Error symbols (Doc 31 v2 §2.4 contract LOCK)

(define-error 'nelisp-coding-error
  "NeLisp coding (encoding) error")

(define-error 'nelisp-coding-invalid-byte
  "Invalid byte sequence in encoded text (catchable)"
  'nelisp-coding-error)

(define-error 'nelisp-coding-strict-violation
  "Strict mode invalid byte sequence (uncatchable, aborts process in Phase 7.5)"
  'nelisp-coding-error)

(define-error 'nelisp-coding-invalid-codepoint
  "Codepoint cannot be encoded (surrogate or > U+10FFFF)"
  'nelisp-coding-error)

(define-error 'nelisp-coding-table-corruption
  "JIS table content does not match golden SHA-256 hash (Phase 7.4.3)"
  'nelisp-coding-error)

(define-error 'nelisp-coding-unmappable-codepoint
  "Codepoint not representable in target encoding (Phase 7.4.3)"
  'nelisp-coding-error)

;;; Internal: byte access helpers
;;
;; Phase 5-X NeLisp string layout = UTF-8 byte string + char count metadata.
;; ここでは host Emacs 上で実装するため、入力 BYTES は string (unibyte
;; expected) もしくは vector / list of integers として受け付ける。

(defun nelisp-coding--bytes-length (bytes)
  "Return number of bytes in BYTES (string or vector or list)."
  (cond
   ((stringp bytes) (length bytes))
   ((vectorp bytes) (length bytes))
   ((listp bytes)   (length bytes))
   (t (signal 'wrong-type-argument
              (list 'sequencep bytes)))))

(defun nelisp-coding--bytes-ref (bytes pos)
  "Return integer byte at POS in BYTES.
For string, returns the raw byte (assumes unibyte content)."
  (cond
   ((stringp bytes)
    ;; If multibyte string, `aref' returns a char which may be > 255;
    ;; we treat unibyte strings as canonical input. For robustness on
    ;; multibyte input (host Emacs literal), mod 256 the value.
    (let ((c (aref bytes pos)))
      (if (multibyte-string-p bytes)
          (logand c #xFF)
        c)))
   ((vectorp bytes) (aref bytes pos))
   ((listp bytes)   (nth pos bytes))
   (t (signal 'wrong-type-argument (list 'sequencep bytes)))))

(defun nelisp-coding--bytes-to-list (bytes)
  "Coerce BYTES (string/vector/list) to list of integers (raw bytes)."
  (cond
   ((listp bytes) (mapcar (lambda (b)
                            (if (and (integerp b) (>= b 0) (< b 256))
                                b
                              (logand b #xFF)))
                          bytes))
   ((vectorp bytes) (append bytes nil))
   ((stringp bytes)
    (if (multibyte-string-p bytes)
        ;; Convert multibyte string to its UTF-8 byte sequence.
        ;; This is the host fallback for tests that pass literal "あ".
        (append (encode-coding-string bytes 'utf-8 t) nil)
      (append bytes nil)))
   (t (signal 'wrong-type-argument (list 'sequencep bytes)))))

;;; UTF-8 codepoint encoder (Doc 31 v2 §2.2 internal repr 整合)

(defun nelisp-coding--utf8-encode-codepoint (codepoint)
  "Encode one Unicode CODEPOINT (integer) to a list of UTF-8 bytes.
Reject surrogate (U+D800-U+DFFF) and > U+10FFFF with
`nelisp-coding-invalid-codepoint'.
Negative codepoints are also rejected.

UTF-8 encoding (RFC 3629):
- U+0000-U+007F     => 1 byte: 0xxxxxxx
- U+0080-U+07FF     => 2 byte: 110xxxxx 10xxxxxx
- U+0800-U+FFFF     => 3 byte: 1110xxxx 10xxxxxx 10xxxxxx
- U+10000-U+10FFFF  => 4 byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx"
  (cond
   ((or (not (integerp codepoint)) (< codepoint 0))
    (signal 'nelisp-coding-invalid-codepoint
            (list :codepoint codepoint :reason 'negative-or-non-integer)))
   ((and (>= codepoint nelisp-coding-utf8-surrogate-min)
         (<= codepoint nelisp-coding-utf8-surrogate-max))
    (signal 'nelisp-coding-invalid-codepoint
            (list :codepoint codepoint :reason 'surrogate)))
   ((> codepoint nelisp-coding-utf8-max-codepoint)
    (signal 'nelisp-coding-invalid-codepoint
            (list :codepoint codepoint :reason 'out-of-range)))
   ((< codepoint #x80)
    ;; 1-byte ASCII fast path
    (list codepoint))
   ((< codepoint #x800)
    ;; 2-byte sequence
    (list (logior #xC0 (ash codepoint -6))
          (logior #x80 (logand codepoint #x3F))))
   ((< codepoint #x10000)
    ;; 3-byte sequence (BMP non-ASCII)
    (list (logior #xE0 (ash codepoint -12))
          (logior #x80 (logand (ash codepoint -6) #x3F))
          (logior #x80 (logand codepoint #x3F))))
   (t
    ;; 4-byte sequence (supplementary plane, U+10000-U+10FFFF)
    (list (logior #xF0 (ash codepoint -18))
          (logior #x80 (logand (ash codepoint -12) #x3F))
          (logior #x80 (logand (ash codepoint -6) #x3F))
          (logior #x80 (logand codepoint #x3F))))))

;;; UTF-8 byte sequence parser (Doc 31 v2 §3.1)
;;
;; Returns (CODEPOINT . NEXT-POS) on success、(:invalid . NEXT-POS) on
;; invalid sequence, where NEXT-POS is the byte offset to resume parsing.
;;
;; Per WHATWG Encoding Standard 準拠 = on invalid leading byte, advance
;; by 1; on truncated multibyte / invalid continuation, advance to the
;; first byte that *could* begin a new sequence (resync at next valid
;; leading byte). We follow the simpler "advance by 1 on any invalid"
;; rule which matches Python codecs and is sufficient for the +10 ERT.

(defun nelisp-coding--utf8-leading-byte-info (byte)
  "Inspect leading BYTE of a UTF-8 sequence, return (LEN . MASKED) or nil.
LEN = expected byte count (1-4), MASKED = high bits cleared per RFC 3629.
Return nil for invalid leading byte (continuation byte or 0xF8+)."
  (cond
   ((< byte #x80) (cons 1 byte))                              ; 0xxxxxxx
   ((< byte #xC0) nil)                                        ; 10xxxxxx (cont byte)
   ((< byte #xE0) (cons 2 (logand byte #x1F)))                ; 110xxxxx
   ((< byte #xF0) (cons 3 (logand byte #x0F)))                ; 1110xxxx
   ((< byte #xF8) (cons 4 (logand byte #x07)))                ; 11110xxx
   (t nil)))                                                  ; 11111xxx invalid

(defun nelisp-coding--utf8-overlong-p (codepoint expected-len)
  "Return non-nil if CODEPOINT was encoded with EXPECTED-LEN bytes overlong.
Per RFC 3629, encoders must use the shortest encoding."
  (cond
   ((= expected-len 1) (>= codepoint #x80))
   ((= expected-len 2) (< codepoint #x80))
   ((= expected-len 3) (< codepoint #x800))
   ((= expected-len 4) (< codepoint #x10000))
   (t nil)))

(defun nelisp-coding--utf8-decode-codepoint (bytes pos len)
  "Decode one UTF-8 codepoint starting at POS in BYTES (length LEN).
Returns one of:
  (CODEPOINT . NEXT-POS)   — successfully decoded valid codepoint
  (:invalid . NEXT-POS)    — invalid sequence (advance by 1 byte for resync)

Rejects:
- overlong encoding (e.g. 0xC0 0x80 = U+0000)
- surrogate code point (U+D800-U+DFFF)
- > U+10FFFF
- truncated multibyte sequence
- invalid continuation byte (not 10xxxxxx)
- invalid leading byte (continuation in leading position, 0xF8+)"
  (let* ((b0 (nelisp-coding--bytes-ref bytes pos))
         (info (nelisp-coding--utf8-leading-byte-info b0)))
    (if (null info)
        ;; Invalid leading byte (cont byte or 0xF8+).
        (cons :invalid (1+ pos))
      (let ((expected-len (car info))
            (cp (cdr info)))
        (if (> (+ pos expected-len) len)
            ;; Truncated sequence — advance by 1 for WHATWG-style resync.
            (cons :invalid (1+ pos))
          (let ((ok t)
                (i 1))
            ;; Validate continuation bytes.
            (while (and ok (< i expected-len))
              (let ((bn (nelisp-coding--bytes-ref bytes (+ pos i))))
                (if (= (logand bn #xC0) #x80)
                    (setq cp (logior (ash cp 6) (logand bn #x3F)))
                  (setq ok nil)))
              (setq i (1+ i)))
            (cond
             ((not ok)
              ;; Bad continuation — advance by 1 for resync.
              (cons :invalid (1+ pos)))
             ((nelisp-coding--utf8-overlong-p cp expected-len)
              (cons :invalid (1+ pos)))
             ((and (>= cp nelisp-coding-utf8-surrogate-min)
                   (<= cp nelisp-coding-utf8-surrogate-max))
              (cons :invalid (1+ pos)))
             ((> cp nelisp-coding-utf8-max-codepoint)
              (cons :invalid (1+ pos)))
             (t
              (cons cp (+ pos expected-len))))))))))

;;; BOM handling (Doc 31 v2 §2.3)

(defun nelisp-coding--has-utf8-bom-p (bytes)
  "Return non-nil if BYTES (sequence) starts with UTF-8 BOM (EF BB BF)."
  (and (>= (nelisp-coding--bytes-length bytes) 3)
       (= (nelisp-coding--bytes-ref bytes 0) #xEF)
       (= (nelisp-coding--bytes-ref bytes 1) #xBB)
       (= (nelisp-coding--bytes-ref bytes 2) #xBF)))

(defun nelisp-coding--strip-utf8-bom (bytes)
  "If BYTES (vector or list) starts with UTF-8 BOM, return BYTES without BOM.
Otherwise return BYTES unchanged. Always returns a list of integers."
  (let ((lst (nelisp-coding--bytes-to-list bytes)))
    (if (and (>= (length lst) 3)
             (= (nth 0 lst) #xEF)
             (= (nth 1 lst) #xBB)
             (= (nth 2 lst) #xBF))
        (nthcdr 3 lst)
      lst)))

(defun nelisp-coding--prepend-utf8-bom (bytes)
  "Prepend UTF-8 BOM to BYTES (returns list of integers)."
  (append nelisp-coding-utf8-bom
          (nelisp-coding--bytes-to-list bytes)))

;;; Public API: UTF-8 decode

(defun nelisp-coding-utf8-decode (bytes &optional strategy)
  "Decode UTF-8 BYTES (string / vector / list of bytes) to NeLisp string.

STRATEGY (default = `nelisp-coding-error-strategy', i.e. `replace'):
- `replace' / nil — invalid byte → U+FFFD; consecutive invalid bytes
  collapse to a single U+FFFD per WHATWG; result returned as plist.
- `error'         — first invalid byte signals `nelisp-coding-invalid-byte'
                    (catchable via `condition-case').
- `strict'        — first invalid byte signals `nelisp-coding-strict-violation'
                    (uncatchable in streaming; Phase 7.5 will integrate
                    process abort).

UTF-8 BOM (EF BB BF) at start of input is stripped before decoding (per
RFC 3629 + Doc 31 v2 §2.3).

Returns a plist:
  (:string DECODED-STRING
   :strategy STRATEGY
   :invalid-positions (LIST OF BYTE-OFFSET)
   :replacements N
   :had-bom BOOLEAN)

Where BYTE-OFFSET in `:invalid-positions' is measured from the *start of
the original input* (BOM included if present)."
  (let* ((effective-strategy (or strategy nelisp-coding-error-strategy))
         (raw-list (nelisp-coding--bytes-to-list bytes))
         (had-bom (and (>= (length raw-list) 3)
                       (= (nth 0 raw-list) #xEF)
                       (= (nth 1 raw-list) #xBB)
                       (= (nth 2 raw-list) #xBF)))
         ;; Skip BOM but preserve original-offset accounting.
         (working (if had-bom (nthcdr 3 raw-list) raw-list))
         (working-vec (vconcat working))
         (working-len (length working-vec))
         (bom-shift (if had-bom 3 0))
         (codepoints '())
         (invalid-positions '())
         (replacements 0)
         (last-was-invalid nil)
         (pos 0))
    (while (< pos working-len)
      (let* ((result (nelisp-coding--utf8-decode-codepoint
                      working-vec pos working-len))
             (head (car result))
             (next-pos (cdr result))
             (orig-offset (+ bom-shift pos)))
        (cond
         ((eq head :invalid)
          (pcase effective-strategy
            ('error
             (signal 'nelisp-coding-invalid-byte
                     (list :offset orig-offset
                           :byte (nelisp-coding--bytes-ref working-vec pos)
                           :strategy 'error)))
            ('strict
             (signal 'nelisp-coding-strict-violation
                     (list :offset orig-offset
                           :byte (nelisp-coding--bytes-ref working-vec pos)
                           :strategy 'strict)))
            (_
             ;; replace / nil / unknown → replace strategy
             (push orig-offset invalid-positions)
             (unless last-was-invalid
               (push nelisp-coding-utf8-replacement-char codepoints)
               (setq replacements (1+ replacements)))
             (setq last-was-invalid t))))
         (t
          (push head codepoints)
          (setq last-was-invalid nil)))
        (setq pos next-pos)))
    (list :string (apply #'string (nreverse codepoints))
          :strategy (if (memq effective-strategy '(replace error strict))
                        effective-strategy
                      'replace)
          :invalid-positions (nreverse invalid-positions)
          :replacements replacements
          :had-bom had-bom)))

;;; Public API: UTF-8 encode

(defun nelisp-coding-utf8-encode (string &optional bom-emit)
  "Encode NeLisp STRING (host Emacs string of codepoints) to UTF-8.

Returns a list of integers (raw bytes). Caller may convert to unibyte
string via \\=`apply #\\='unibyte-string ...\\=' or to vector via `vconcat'.

If BOM-EMIT is non-nil (overrides `nelisp-coding-utf8-bom-emit-on-write'),
the result is prefixed with the UTF-8 BOM (EF BB BF). Default is no BOM
per RFC 3629.

Each codepoint is validated by `nelisp-coding--utf8-encode-codepoint':
surrogates and codepoints > U+10FFFF signal `nelisp-coding-invalid-codepoint'.
ASCII-only strings take the 1-byte fast path."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (let ((emit-bom (or bom-emit nelisp-coding-utf8-bom-emit-on-write))
        (out '())
        (i 0)
        (n (length string)))
    (while (< i n)
      (let ((cp (aref string i)))
        (dolist (b (nelisp-coding--utf8-encode-codepoint cp))
          (push b out)))
      (setq i (1+ i)))
    (let ((bytes (nreverse out)))
      (if emit-bom
          (append nelisp-coding-utf8-bom bytes)
        bytes))))

;;; Convenience: encode to unibyte string

(defun nelisp-coding-utf8-encode-string (string &optional bom-emit)
  "Like `nelisp-coding-utf8-encode' but return an Emacs unibyte string."
  (apply #'unibyte-string
         (nelisp-coding-utf8-encode string bom-emit)))

;;;; ────────────────────────────────────────────────────────────────────
;;;; Phase 7.4.2 — Latin-1 (ISO-8859-1) codec (Doc 31 v2 §3.2 / §6.2)
;;;; ────────────────────────────────────────────────────────────────────
;;
;; Latin-1 = single-byte encoding、=U+0000-U+00FF= ↔ byte =0x00-0xFF=
;; bijective。decode は常に成功 (全 256 値 valid)。encode で U+0100+
;; codepoint 出現時のみ 3 strategy 分岐 = §2.4 contract LOCK 完全準拠。
;;
;; Doc 31 v2 §6.2 placeholder = =:replace= 時 default '?' (=0x3F=) emit
;; per Emacs =coding.c= precedent、=defcustom
;; nelisp-coding-latin1-replacement-codepoint= で customize 可。

(defconst nelisp-coding-latin1-max-codepoint #xFF
  "Maximum Latin-1 representable codepoint (U+00FF).
=U+0100+= codepoints require 3 strategy dispatch on encode.")

;;; Public API: Latin-1 decode

(defun nelisp-coding-latin1-decode (bytes)
  "Decode Latin-1 BYTES (string / vector / list of bytes) to NeLisp string.

全 byte =0x00-0xFF= は直接 codepoint =U+0000-U+00FF= にマップ
(bijective single-byte cast)。Latin-1 仕様により invalid byte sequence
は存在しない (= 256 値全て valid)。

Returns plist (T19 形式踏襲、API 一貫性):
  (:string DECODED-STRING
   :strategy \\='replace
   :invalid-positions nil
   :replacements 0)

STRATEGY field is always \\='replace (= no-op、Latin-1 では invalid byte
が存在しないため strategy 分岐自体が起こらない)。INVALID-POSITIONS / REPLACEMENTS
は API 一貫性のため常に nil / 0。"
  (let* ((raw-list (nelisp-coding--bytes-to-list bytes))
         (codepoints '()))
    ;; Latin-1 = direct byte → codepoint cast。0x00-0xFF 全て valid。
    (dolist (b raw-list)
      (push b codepoints))
    (list :string (apply #'string (nreverse codepoints))
          :strategy 'replace
          :invalid-positions nil
          :replacements 0)))

;;; Public API: Latin-1 encode

(defun nelisp-coding--latin1-encode-codepoint (codepoint)
  "Encode one CODEPOINT to a single Latin-1 byte (integer 0-255).
Reject codepoint < 0 with `nelisp-coding-invalid-codepoint'.
Caller must dispatch U+0100+ via strategy logic (here always returns
the byte if in range, signals if out of Latin-1 range)."
  (cond
   ((or (not (integerp codepoint)) (< codepoint 0))
    (signal 'nelisp-coding-invalid-codepoint
            (list :codepoint codepoint :reason 'negative-or-non-integer)))
   ((> codepoint nelisp-coding-latin1-max-codepoint)
    (signal 'nelisp-coding-invalid-codepoint
            (list :codepoint codepoint :reason 'out-of-latin1-range)))
   (t codepoint)))

(defun nelisp-coding-latin1-encode (string &optional strategy)
  "Encode NeLisp STRING (host Emacs string of codepoints) to Latin-1 bytes.

Returns a plist (Doc 31 v2 §2.4 contract + T19 形式踏襲):
  (:bytes (LIST OF BYTES)
   :strategy STRATEGY
   :invalid-positions (LIST OF CHAR-OFFSET)
   :replacements N)

STRATEGY (default = `nelisp-coding-error-strategy', i.e. `replace'):
- `replace' / nil — U+0100+ codepoint emit replacement byte (default
  =0x3F= question-mark, customizable via
  `nelisp-coding-latin1-replacement-codepoint'); CHAR-OFFSET in
  `:invalid-positions' is the input string char index, NOT byte offset
  since input is char-indexed.
- `error'         — first U+0100+ signals `nelisp-coding-invalid-codepoint'
  with data plist =(:offset N :codepoint CP :strategy \\='error)=
  (catchable via `condition-case'). Partial result discarded.
- `strict'        — first U+0100+ signals `nelisp-coding-strict-violation'
  (uncatchable in streaming; Phase 7.5 で process abort と integrate).

ASCII (U+0000-U+007F) は UTF-8 と互換 (ASCII fast path)。
U+0080-U+00FF は Latin-1 拡張範囲、direct byte cast。
U+0100+ は Latin-1 表現不能 → strategy dispatch。

Note: Latin-1 は仕様上 BOM を持たないため bom-emit 引数なし。"
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (let ((effective-strategy (or strategy nelisp-coding-error-strategy))
        (out '())
        (invalid-positions '())
        (replacements 0)
        (i 0)
        (n (length string)))
    (while (< i n)
      (let ((cp (aref string i)))
        (cond
         ((or (not (integerp cp)) (< cp 0))
          ;; Defensive: malformed string codepoint
          (signal 'nelisp-coding-invalid-codepoint
                  (list :codepoint cp :reason 'negative-or-non-integer
                        :offset i)))
         ((<= cp nelisp-coding-latin1-max-codepoint)
          ;; In-range: bijective byte cast.
          (push cp out))
         (t
          ;; U+0100+ : strategy dispatch.
          (pcase effective-strategy
            ('error
             (signal 'nelisp-coding-invalid-codepoint
                     (list :offset i :codepoint cp :strategy 'error)))
            ('strict
             (signal 'nelisp-coding-strict-violation
                     (list :offset i :codepoint cp :strategy 'strict)))
            (_
             ;; replace / nil / unknown → replace strategy
             ;; Validate replacement codepoint is in Latin-1 range so the
             ;; placeholder is itself a single byte (= encode terminates).
             (let ((repl nelisp-coding-latin1-replacement-codepoint))
               (unless (and (integerp repl)
                            (>= repl 0)
                            (<= repl nelisp-coding-latin1-max-codepoint))
                 (signal 'nelisp-coding-invalid-codepoint
                         (list :codepoint repl
                               :reason 'replacement-out-of-latin1-range)))
               (push repl out)
               (push i invalid-positions)
               (setq replacements (1+ replacements))))))))
      (setq i (1+ i)))
    (list :bytes (nreverse out)
          :strategy (if (memq effective-strategy '(replace error strict))
                        effective-strategy
                      'replace)
          :invalid-positions (nreverse invalid-positions)
          :replacements replacements)))

(defun nelisp-coding-latin1-encode-string (string &optional strategy)
  "Like `nelisp-coding-latin1-encode' but return an Emacs unibyte string.

Convenience wrapper that drops the metadata plist and returns only
the encoded byte sequence as a unibyte string. Use the plist API
(`nelisp-coding-latin1-encode') when caller needs replacement count
or invalid positions."
  (apply #'unibyte-string
         (plist-get (nelisp-coding-latin1-encode string strategy) :bytes)))

;;;; ────────────────────────────────────────────────────────────────────
;;;; Phase 7.4.3 — Shift-JIS + CP932 + EUC-JP codecs (Doc 31 v2 §3.3)
;;;; ────────────────────────────────────────────────────────────────────
;;
;; Algorithm references:
;; - Shift-JIS (CP932): Microsoft CP932 mapping = ASCII passthrough +
;;   JIS X 0201 halfwidth katakana (0xA1-0xDF, single byte) + 2-byte
;;   sequences with lead 0x81-0x9F or 0xE0-0xFC and trail 0x40-0x7E or
;;   0x80-0xFC (excluding 0x7F)
;; - EUC-JP: ASCII + 0x8E katakana shift (CS2) + 0x8F X 0212 shift (CS3) +
;;   2-byte X 0208 (CS1, both bytes 0xA1-0xFE)
;;
;; Table data (=src/nelisp-coding-jis-tables.el=) holds 4 alists which we
;; promote to hash-tables on first use for O(1) lookup. Reverse maps
;; (Unicode → SJIS / EUC) are also memoized lazily.
;;
;; *** Lookup-table memoization ***
;;
;; Table promotion runs once per Emacs session via lazy-init helpers.
;; The four host tables (one per source-of-truth alist) are stored in
;; module-level mutable hash-tables, gated by the `nelisp-coding--jis-tables-built'
;; flag. `nelisp-coding-jis-tables-rebuild' (interactive helper) forces a
;; rebuild after a generator update.

(defvar nelisp-coding--shift-jis-decode-hash nil
  "Hash-table for SJIS-INT → Unicode codepoint lookup (X 0208 + CP932 ext).
Lazily built from `nelisp-coding-shift-jis-x0208-decode-table' merged with
`nelisp-coding-cp932-extension-decode-table' on first use. Phase 7.4.3 MVP.")

(defvar nelisp-coding--shift-jis-encode-hash nil
  "Hash-table for Unicode codepoint → SJIS-INT reverse lookup (X 0208 + CP932 ext).
Built simultaneously with the decode table. When a codepoint maps to both
JIS X 0208 and CP932 extension entries (= the rare collision case), the
*first inserted* entry wins; in practice JIS X 0208 wins by virtue of being
inserted first in `nelisp-coding--jis-tables-build'.")

(defvar nelisp-coding--euc-jp-x0208-decode-hash nil
  "Hash-table for EUC-INT → Unicode codepoint lookup, JIS X 0208 (CS1).
Lazily built from `nelisp-coding-euc-jp-x0208-decode-table'.")

(defvar nelisp-coding--euc-jp-x0208-encode-hash nil
  "Hash-table for Unicode → EUC-INT reverse lookup, JIS X 0208 (CS1).")

(defvar nelisp-coding--euc-jp-x0212-decode-hash nil
  "Hash-table for EUC-INT → Unicode codepoint lookup, JIS X 0212 (CS3, 3-byte).
The EUC-INT here is (lead << 8) | trail; the 0x8F prefix is implicit.")

(defvar nelisp-coding--euc-jp-x0212-encode-hash nil
  "Hash-table for Unicode → EUC-INT reverse lookup, JIS X 0212 (CS3).")

(defvar nelisp-coding--jis-tables-built nil
  "Non-nil once the JIS lookup hash-tables have been built this session.")

(defun nelisp-coding--jis-tables-build ()
  "Promote the 4 JIS alists to hash-tables for O(1) decode/encode lookup.

Idempotent: returns immediately if `nelisp-coding--jis-tables-built' is set.
Use `nelisp-coding-jis-tables-rebuild' to force a rebuild (e.g. after the
generator script regenerates the table file)."
  (unless nelisp-coding--jis-tables-built
    ;; SJIS decode: X 0208 first, then CP932 ext (NEC + IBM) merged in.
    (setq nelisp-coding--shift-jis-decode-hash
          (make-hash-table :test 'eql :size 1024))
    (setq nelisp-coding--shift-jis-encode-hash
          (make-hash-table :test 'eql :size 1024))
    (dolist (entry nelisp-coding-shift-jis-x0208-decode-table)
      (let ((sjis (car entry))
            (cp   (cdr entry)))
        (puthash sjis cp nelisp-coding--shift-jis-decode-hash)
        ;; Reverse: only insert if not already present, so X 0208 wins
        ;; on Unicode collision.
        (unless (gethash cp nelisp-coding--shift-jis-encode-hash)
          (puthash cp sjis nelisp-coding--shift-jis-encode-hash))))
    (dolist (entry nelisp-coding-cp932-extension-decode-table)
      (let ((sjis (car entry))
            (cp   (cdr entry)))
        (puthash sjis cp nelisp-coding--shift-jis-decode-hash)
        (unless (gethash cp nelisp-coding--shift-jis-encode-hash)
          (puthash cp sjis nelisp-coding--shift-jis-encode-hash))))
    ;; EUC-JP X 0208
    (setq nelisp-coding--euc-jp-x0208-decode-hash
          (make-hash-table :test 'eql :size 512))
    (setq nelisp-coding--euc-jp-x0208-encode-hash
          (make-hash-table :test 'eql :size 512))
    (dolist (entry nelisp-coding-euc-jp-x0208-decode-table)
      (let ((euc (car entry))
            (cp  (cdr entry)))
        (puthash euc cp nelisp-coding--euc-jp-x0208-decode-hash)
        (unless (gethash cp nelisp-coding--euc-jp-x0208-encode-hash)
          (puthash cp euc nelisp-coding--euc-jp-x0208-encode-hash))))
    ;; EUC-JP X 0212
    (setq nelisp-coding--euc-jp-x0212-decode-hash
          (make-hash-table :test 'eql :size 256))
    (setq nelisp-coding--euc-jp-x0212-encode-hash
          (make-hash-table :test 'eql :size 256))
    (dolist (entry nelisp-coding-euc-jp-x0212-decode-table)
      (let ((euc (car entry))
            (cp  (cdr entry)))
        (puthash euc cp nelisp-coding--euc-jp-x0212-decode-hash)
        (unless (gethash cp nelisp-coding--euc-jp-x0212-encode-hash)
          (puthash cp euc nelisp-coding--euc-jp-x0212-encode-hash))))
    (setq nelisp-coding--jis-tables-built t)))

(defun nelisp-coding-jis-tables-rebuild ()
  "Force rebuild of all JIS lookup hash-tables.
Useful after the generator script regenerates `nelisp-coding-jis-tables.el'
(=Phase 7.5 hot-reload entry point=)."
  (interactive)
  (setq nelisp-coding--jis-tables-built nil)
  (nelisp-coding--jis-tables-build))

;;; Internal: 3-strategy codec dispatch helper

(defun nelisp-coding--japanese-invalid-byte-replacement (strategy orig-offset byte)
  "Dispatch invalid input byte under STRATEGY at ORIG-OFFSET (BYTE int).

Used by Shift-JIS / EUC-JP decoders to centralise the replace / error /
strict signal logic. Returns the replacement codepoint to emit on
:replace strategy (callers feed this into the codepoint accumulator).
Signals `nelisp-coding-invalid-byte' on :error and
`nelisp-coding-strict-violation' on :strict."
  (pcase strategy
    ('error
     (signal 'nelisp-coding-invalid-byte
             (list :offset orig-offset :byte byte :strategy 'error)))
    ('strict
     (signal 'nelisp-coding-strict-violation
             (list :offset orig-offset :byte byte :strategy 'strict)))
    (_
     ;; replace (default)
     nelisp-coding-utf8-replacement-char)))

;;; ── Shift-JIS / CP932 decode ──

(defun nelisp-coding--shift-jis-lead-byte-p (byte)
  "Return non-nil if BYTE (integer) is a Shift-JIS lead-byte candidate.
Lead range = 0x81-0x9F or 0xE0-0xFC per JIS X 0208 + CP932 (Microsoft
CP932 mapping)."
  (or (and (>= byte #x81) (<= byte #x9F))
      (and (>= byte #xE0) (<= byte #xFC))))

(defun nelisp-coding--shift-jis-trail-byte-p (byte)
  "Return non-nil if BYTE is a valid Shift-JIS trail-byte (0x40-0xFC, except 0x7F)."
  (and (>= byte #x40) (<= byte #xFC) (/= byte #x7F)))

(defun nelisp-coding--shift-jis-katakana-p (byte)
  "Return non-nil if BYTE is JIS X 0201 halfwidth katakana (0xA1-0xDF).
Halfwidth katakana maps directly to U+FF61-U+FF9F (single-byte cast)."
  (and (>= byte #xA1) (<= byte #xDF)))

(defun nelisp-coding-shift-jis-decode (bytes &optional strategy)
  "Decode Shift-JIS / CP932 BYTES (string / vector / list) to a NeLisp string.

Algorithm (Doc 31 v2 §3.3 / Microsoft CP932 mapping):
- byte 0x00-0x7F: ASCII passthrough (single byte)
- byte 0xA1-0xDF: JIS X 0201 halfwidth katakana → U+FF61 + (byte - 0xA1)
- byte 0x81-0x9F or 0xE0-0xFC: lead byte for 2-byte JIS X 0208 + CP932
  extension (NEC special chars + IBM extensions); trail byte must be in
  0x40-0x7E or 0x80-0xFC (= excluding 0x7F)
- otherwise: invalid byte (strategy dispatch)

STRATEGY (default = `nelisp-coding-error-strategy', i.e. `replace'):
- `replace' / nil — invalid byte → U+FFFD; missing table entry also →
  U+FFFD with the byte offset of the lead byte in `:invalid-positions'
- `error'         — first invalid signals `nelisp-coding-invalid-byte'
- `strict'        — first invalid signals `nelisp-coding-strict-violation'

Returns plist (T19 / T22 形式踏襲):
  (:string DECODED-STRING
   :strategy STRATEGY
   :invalid-positions (LIST OF BYTE-OFFSET)
   :replacements N)

Lookup is via the hash-tables built lazily by
`nelisp-coding--jis-tables-build' from
`nelisp-coding-shift-jis-x0208-decode-table' +
`nelisp-coding-cp932-extension-decode-table'. Phase 7.4.3 MVP uses a
partial ~520-entry table; codepoints outside the partial table are
treated as table-miss (replace strategy → U+FFFD)."
  (nelisp-coding--jis-tables-build)
  (let* ((effective-strategy (or strategy nelisp-coding-error-strategy))
         (raw (nelisp-coding--bytes-to-list bytes))
         (vec (vconcat raw))
         (n   (length vec))
         (codepoints '())
         (invalid-positions '())
         (replacements 0)
         (i 0))
    (while (< i n)
      (let ((b0 (aref vec i)))
        (cond
         ;; ASCII passthrough.
         ((< b0 #x80)
          (push b0 codepoints)
          (setq i (1+ i)))
         ;; JIS X 0201 halfwidth katakana (single byte).
         ((nelisp-coding--shift-jis-katakana-p b0)
          (push (+ #xFF61 (- b0 #xA1)) codepoints)
          (setq i (1+ i)))
         ;; 2-byte SJIS / CP932 lead.
         ((nelisp-coding--shift-jis-lead-byte-p b0)
          (cond
           ((>= (1+ i) n)
            ;; Truncated lead at end of input.
            (let ((replacement (nelisp-coding--japanese-invalid-byte-replacement
                                effective-strategy i b0)))
              (push i invalid-positions)
              (push replacement codepoints)
              (setq replacements (1+ replacements)))
            (setq i (1+ i)))
           (t
            (let ((b1 (aref vec (1+ i))))
              (if (not (nelisp-coding--shift-jis-trail-byte-p b1))
                  ;; Bad trail; advance by 1 (resync at next byte).
                  (let ((replacement (nelisp-coding--japanese-invalid-byte-replacement
                                      effective-strategy i b0)))
                    (push i invalid-positions)
                    (push replacement codepoints)
                    (setq replacements (1+ replacements))
                    (setq i (1+ i)))
                ;; Valid 2-byte sequence, consult table.
                (let* ((sjis-int (logior (ash b0 8) b1))
                       (cp (gethash sjis-int nelisp-coding--shift-jis-decode-hash)))
                  (if cp
                      (progn
                        (push cp codepoints)
                        (setq i (+ i 2)))
                    ;; Table miss: treat as invalid (MVP partial table).
                    (let ((replacement (nelisp-coding--japanese-invalid-byte-replacement
                                        effective-strategy i b0)))
                      (push i invalid-positions)
                      (push replacement codepoints)
                      (setq replacements (1+ replacements))
                      (setq i (+ i 2))))))))))
         ;; Invalid leading byte.
         (t
          (let ((replacement (nelisp-coding--japanese-invalid-byte-replacement
                              effective-strategy i b0)))
            (push i invalid-positions)
            (push replacement codepoints)
            (setq replacements (1+ replacements))
            (setq i (1+ i)))))))
    (list :string (apply #'string (nreverse codepoints))
          :strategy (if (memq effective-strategy '(replace error strict))
                        effective-strategy
                      'replace)
          :invalid-positions (nreverse invalid-positions)
          :replacements replacements)))

;;; ── Shift-JIS / CP932 encode ──

(defun nelisp-coding-shift-jis-encode (string &optional strategy)
  "Encode NeLisp STRING to Shift-JIS / CP932 byte sequence (plist return).

Reverse-direction lookup (Unicode → SJIS-INT). Codepoints unmappable in
the partial MVP table dispatch on STRATEGY:
- `replace' / nil — emit ASCII '?' (0x3F) byte; record CHAR-OFFSET in
  `:invalid-positions'
- `error'         — signal `nelisp-coding-unmappable-codepoint'
- `strict'        — signal `nelisp-coding-strict-violation'

Algorithm:
- U+0000-U+007F: ASCII fast path (single byte)
- U+FF61-U+FF9F: JIS X 0201 halfwidth katakana → byte 0xA1+(cp-U+FF61)
- otherwise: hash-table reverse lookup (X 0208 + CP932 ext merged); on
  hit emit `(lead trail)' two bytes from the SJIS-INT

Returns plist (T19 / T22 形式踏襲):
  (:bytes (LIST OF BYTES)
   :strategy STRATEGY
   :invalid-positions (LIST OF CHAR-OFFSET)
   :replacements N)

Note: `:invalid-positions' uses *char* offsets (not byte offsets) since
the input is char-indexed. This mirrors `nelisp-coding-latin1-encode'."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (nelisp-coding--jis-tables-build)
  (let ((effective-strategy (or strategy nelisp-coding-error-strategy))
        (out '())
        (invalid-positions '())
        (replacements 0)
        (i 0)
        (n (length string)))
    (while (< i n)
      (let ((cp (aref string i)))
        (cond
         ;; ASCII fast path.
         ((and (>= cp 0) (< cp #x80))
          (push cp out))
         ;; JIS X 0201 halfwidth katakana range.
         ((and (>= cp #xFF61) (<= cp #xFF9F))
          (push (+ #xA1 (- cp #xFF61)) out))
         (t
          (let ((sjis-int (gethash cp nelisp-coding--shift-jis-encode-hash)))
            (if sjis-int
                (progn
                  (push (logand (ash sjis-int -8) #xFF) out)
                  (push (logand sjis-int #xFF) out))
              ;; Unmappable: dispatch on strategy.
              (pcase effective-strategy
                ('error
                 (signal 'nelisp-coding-unmappable-codepoint
                         (list :offset i :codepoint cp :strategy 'error
                               :encoding 'shift-jis)))
                ('strict
                 (signal 'nelisp-coding-strict-violation
                         (list :offset i :codepoint cp :strategy 'strict
                               :encoding 'shift-jis)))
                (_
                 ;; replace (default): ASCII '?' = 0x3F
                 (push #x3F out)
                 (push i invalid-positions)
                 (setq replacements (1+ replacements)))))))))
      (setq i (1+ i)))
    (list :bytes (nreverse out)
          :strategy (if (memq effective-strategy '(replace error strict))
                        effective-strategy
                      'replace)
          :invalid-positions (nreverse invalid-positions)
          :replacements replacements)))

(defun nelisp-coding-shift-jis-encode-string (string &optional strategy)
  "Like `nelisp-coding-shift-jis-encode' but return an Emacs unibyte string.

Drops the metadata plist and returns only the encoded byte sequence as a
unibyte string. Use the plist API when caller needs replacement count or
invalid positions."
  (apply #'unibyte-string
         (plist-get (nelisp-coding-shift-jis-encode string strategy) :bytes)))

;;; ── EUC-JP decode ──

(defun nelisp-coding--euc-jp-x0208-byte-p (byte)
  "Return non-nil if BYTE is a valid EUC-JP CS1 (X 0208) byte (0xA1-0xFE)."
  (and (>= byte #xA1) (<= byte #xFE)))

(defun nelisp-coding--euc-jp-katakana-trail-p (byte)
  "Return non-nil if BYTE is a valid trailer for the 0x8E katakana-shift sequence.
Per EUC-JP CS2: trail must be 0xA1-0xDF (= JIS X 0201 halfwidth katakana
range, mapped to U+FF61-U+FF9F)."
  (and (>= byte #xA1) (<= byte #xDF)))

(defun nelisp-coding-euc-jp-decode (bytes &optional strategy)
  "Decode EUC-JP BYTES (string / vector / list) to a NeLisp string.

Algorithm (Doc 31 v2 §3.3 / EUC-JP CS1+CS2+CS3):
- byte 0x00-0x7F:           ASCII passthrough
- 0x8E + 0xA1-0xDF (CS2):   JIS X 0201 halfwidth katakana → U+FF61+(b-0xA1)
- 0x8F + 0xA1-0xFE + 0xA1-0xFE (CS3): JIS X 0212 supplementary kanji
- 0xA1-0xFE + 0xA1-0xFE (CS1): JIS X 0208
- otherwise: invalid byte (strategy dispatch)

STRATEGY (default = `nelisp-coding-error-strategy', i.e. `replace'):
- `replace' / nil — invalid → U+FFFD with byte offset in `:invalid-positions'
- `error'         — signal `nelisp-coding-invalid-byte'
- `strict'        — signal `nelisp-coding-strict-violation'

Returns plist:
  (:string DECODED-STRING
   :strategy STRATEGY
   :invalid-positions (LIST OF BYTE-OFFSET)
   :replacements N)

Lookup is via the X 0208 / X 0212 hash-tables built lazily by
`nelisp-coding--jis-tables-build'. Phase 7.4.3 MVP uses partial tables;
table-miss falls through to invalid-byte handling per STRATEGY."
  (nelisp-coding--jis-tables-build)
  (let* ((effective-strategy (or strategy nelisp-coding-error-strategy))
         (raw (nelisp-coding--bytes-to-list bytes))
         (vec (vconcat raw))
         (n   (length vec))
         (codepoints '())
         (invalid-positions '())
         (replacements 0)
         (i 0))
    (while (< i n)
      (let ((b0 (aref vec i)))
        (cond
         ;; ASCII passthrough.
         ((< b0 #x80)
          (push b0 codepoints)
          (setq i (1+ i)))
         ;; CS2 katakana shift = 0x8E + 0xA1-0xDF (2-byte sequence).
         ((= b0 #x8E)
          (cond
           ((>= (1+ i) n)
            ;; Truncated 0x8E.
            (let ((replacement (nelisp-coding--japanese-invalid-byte-replacement
                                effective-strategy i b0)))
              (push i invalid-positions)
              (push replacement codepoints)
              (setq replacements (1+ replacements)))
            (setq i (1+ i)))
           (t
            (let ((b1 (aref vec (1+ i))))
              (if (nelisp-coding--euc-jp-katakana-trail-p b1)
                  (progn
                    (push (+ #xFF61 (- b1 #xA1)) codepoints)
                    (setq i (+ i 2)))
                ;; Bad CS2 trail: advance by 1.
                (let ((replacement (nelisp-coding--japanese-invalid-byte-replacement
                                    effective-strategy i b0)))
                  (push i invalid-positions)
                  (push replacement codepoints)
                  (setq replacements (1+ replacements))
                  (setq i (1+ i))))))))
         ;; CS3 X 0212 shift = 0x8F + 0xA1-0xFE + 0xA1-0xFE (3-byte).
         ((= b0 #x8F)
          (cond
           ((>= (+ i 2) n)
            ;; Truncated 0x8F.
            (let ((replacement (nelisp-coding--japanese-invalid-byte-replacement
                                effective-strategy i b0)))
              (push i invalid-positions)
              (push replacement codepoints)
              (setq replacements (1+ replacements)))
            (setq i (1+ i)))
           (t
            (let ((b1 (aref vec (1+ i)))
                  (b2 (aref vec (+ i 2))))
              (if (and (nelisp-coding--euc-jp-x0208-byte-p b1)
                       (nelisp-coding--euc-jp-x0208-byte-p b2))
                  (let* ((euc-int (logior (ash b1 8) b2))
                         (cp (gethash euc-int
                                      nelisp-coding--euc-jp-x0212-decode-hash)))
                    (if cp
                        (progn
                          (push cp codepoints)
                          (setq i (+ i 3)))
                      ;; Table miss in X 0212.
                      (let ((replacement (nelisp-coding--japanese-invalid-byte-replacement
                                          effective-strategy i b0)))
                        (push i invalid-positions)
                        (push replacement codepoints)
                        (setq replacements (1+ replacements))
                        (setq i (+ i 3)))))
                ;; Bad CS3 byte structure.
                (let ((replacement (nelisp-coding--japanese-invalid-byte-replacement
                                    effective-strategy i b0)))
                  (push i invalid-positions)
                  (push replacement codepoints)
                  (setq replacements (1+ replacements))
                  (setq i (1+ i))))))))
         ;; CS1 X 0208 = 0xA1-0xFE + 0xA1-0xFE (2-byte).
         ((nelisp-coding--euc-jp-x0208-byte-p b0)
          (cond
           ((>= (1+ i) n)
            ;; Truncated CS1.
            (let ((replacement (nelisp-coding--japanese-invalid-byte-replacement
                                effective-strategy i b0)))
              (push i invalid-positions)
              (push replacement codepoints)
              (setq replacements (1+ replacements)))
            (setq i (1+ i)))
           (t
            (let ((b1 (aref vec (1+ i))))
              (if (nelisp-coding--euc-jp-x0208-byte-p b1)
                  (let* ((euc-int (logior (ash b0 8) b1))
                         (cp (gethash euc-int
                                      nelisp-coding--euc-jp-x0208-decode-hash)))
                    (if cp
                        (progn
                          (push cp codepoints)
                          (setq i (+ i 2)))
                      ;; Table miss in X 0208.
                      (let ((replacement (nelisp-coding--japanese-invalid-byte-replacement
                                          effective-strategy i b0)))
                        (push i invalid-positions)
                        (push replacement codepoints)
                        (setq replacements (1+ replacements))
                        (setq i (+ i 2)))))
                ;; Bad CS1 trail: advance by 1.
                (let ((replacement (nelisp-coding--japanese-invalid-byte-replacement
                                    effective-strategy i b0)))
                  (push i invalid-positions)
                  (push replacement codepoints)
                  (setq replacements (1+ replacements))
                  (setq i (1+ i))))))))
         ;; Invalid leading byte.
         (t
          (let ((replacement (nelisp-coding--japanese-invalid-byte-replacement
                              effective-strategy i b0)))
            (push i invalid-positions)
            (push replacement codepoints)
            (setq replacements (1+ replacements))
            (setq i (1+ i)))))))
    (list :string (apply #'string (nreverse codepoints))
          :strategy (if (memq effective-strategy '(replace error strict))
                        effective-strategy
                      'replace)
          :invalid-positions (nreverse invalid-positions)
          :replacements replacements)))

;;; ── EUC-JP encode ──

(defun nelisp-coding-euc-jp-encode (string &optional strategy)
  "Encode NeLisp STRING to EUC-JP byte sequence (plist return).

Algorithm:
- U+0000-U+007F:          ASCII fast path (single byte)
- U+FF61-U+FF9F:          0x8E + (cp - U+FF61 + 0xA1) = CS2 katakana
- X 0212 hash hit (CS3):  0x8F + lead + trail (3-byte sequence, lead/trail
                          extracted from EUC-INT in the table)
- X 0208 hash hit (CS1):  lead + trail (2-byte)
- otherwise (unmappable in MVP partial table): strategy dispatch

STRATEGY same as `nelisp-coding-shift-jis-encode': `replace' emits ASCII
'?', `error' signals `nelisp-coding-unmappable-codepoint', `strict'
signals `nelisp-coding-strict-violation'.

Note: Lookup order is X 0208 first, then X 0212 — = X 0208 wins on the
unlikely Unicode codepoint that maps to both (= EUC-JP convention since
X 0212 is supplementary)。

Returns plist:
  (:bytes (LIST OF BYTES)
   :strategy STRATEGY
   :invalid-positions (LIST OF CHAR-OFFSET)
   :replacements N)"
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (nelisp-coding--jis-tables-build)
  (let ((effective-strategy (or strategy nelisp-coding-error-strategy))
        (out '())
        (invalid-positions '())
        (replacements 0)
        (i 0)
        (n (length string)))
    (while (< i n)
      (let ((cp (aref string i)))
        (cond
         ;; ASCII fast path.
         ((and (>= cp 0) (< cp #x80))
          (push cp out))
         ;; CS2 halfwidth katakana.
         ((and (>= cp #xFF61) (<= cp #xFF9F))
          (push #x8E out)
          (push (+ #xA1 (- cp #xFF61)) out))
         (t
          (let ((euc-208 (gethash cp nelisp-coding--euc-jp-x0208-encode-hash))
                (euc-212 (gethash cp nelisp-coding--euc-jp-x0212-encode-hash)))
            (cond
             (euc-208
              ;; CS1 X 0208: 2-byte sequence.
              (push (logand (ash euc-208 -8) #xFF) out)
              (push (logand euc-208 #xFF) out))
             (euc-212
              ;; CS3 X 0212: 0x8F + 2 bytes.
              (push #x8F out)
              (push (logand (ash euc-212 -8) #xFF) out)
              (push (logand euc-212 #xFF) out))
             (t
              ;; Unmappable: dispatch on strategy.
              (pcase effective-strategy
                ('error
                 (signal 'nelisp-coding-unmappable-codepoint
                         (list :offset i :codepoint cp :strategy 'error
                               :encoding 'euc-jp)))
                ('strict
                 (signal 'nelisp-coding-strict-violation
                         (list :offset i :codepoint cp :strategy 'strict
                               :encoding 'euc-jp)))
                (_
                 (push #x3F out)
                 (push i invalid-positions)
                 (setq replacements (1+ replacements))))))))))
      (setq i (1+ i)))
    (list :bytes (nreverse out)
          :strategy (if (memq effective-strategy '(replace error strict))
                        effective-strategy
                      'replace)
          :invalid-positions (nreverse invalid-positions)
          :replacements replacements)))

(defun nelisp-coding-euc-jp-encode-string (string &optional strategy)
  "Like `nelisp-coding-euc-jp-encode' but return an Emacs unibyte string."
  (apply #'unibyte-string
         (plist-get (nelisp-coding-euc-jp-encode string strategy) :bytes)))

(provide 'nelisp-coding)

;;; nelisp-coding.el ends here
