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
;; Scope (Phase 7.4.4 — Doc 31 v2 §2.5 / §2.7 / §6.5):
;;   - streaming codec state (`nelisp-coding--stream-state' cl-defstruct):
;;     pending-byte buffer + decoder state across chunk boundaries
;;   - stream-decode-chunk / stream-decode-finalize for incremental decode
;;     (4 encodings: utf-8 / latin-1 / shift-jis / euc-jp)
;;   - stream-encode-chunk / stream-encode-finalize for incremental encode
;;     (encode side has no byte-boundary problem since chunks are char-aligned)
;;   - file I/O wrappers (`nelisp-coding-read-file-with-encoding' /
;;     `nelisp-coding-write-file-with-encoding') as MVP simulators that
;;     bridge to the actual stream API; real Phase 7.0 syscall integration
;;     is Phase 7.5.
;;   - chunk-boundary stress test contract (Doc 31 v2 §6.5 silent corruption
;;     mitigation): multi-byte sequence split across chunks must decode
;;     identical to a one-shot decode of the concatenated bytes.
;;
;; Deferred to later sub-phases:
;;   - Phase 7.5: process-coding-system 本体実装、resume-coding primitive、
;;     real 14000 entry table generation via =tools/coding-table-gen.el=、
;;     real Phase 7.0 syscall =read=/=write= integration (replacing the
;;     `insert-file-contents-literally' / `write-region' simulators in
;;     Phase 7.4.4).
;;
;; SBCL =sb-impl/external-formats.lisp= + Emacs =coding.c= dual precedent。
;; Phase 7.4.1 + 7.4.2 + 7.4.3 + 7.4.4 はそれら subset で UTF-8 / Latin-1 /
;; Japanese + BOM + 3 strategy + streaming chunk-based callback。

;;; Code:

(require 'cl-lib)
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

;; T67 / Doc 31 v2 §2.4 LOCK: `strict' policy is documented as
;; "uncatchable / process abort / state undefined".  In the host-Emacs
;; MVP we cannot truly abort the host process (that would kill the
;; debugger session), but we can structurally discourage callers from
;; catching this via the generic `nelisp-coding-error' parent.  The
;; parent is therefore the *toplevel* `error' rather than
;; `nelisp-coding-error' — a `condition-case' on `nelisp-coding-error'
;; will *not* catch strict violations, mirroring the spec's intent.
;; Tests that need to assert strict-violation behaviour catch it by
;; name (`:type 'nelisp-coding-strict-violation') or via the generic
;; `error' parent (which the host runtime cannot prevent).
;;
;; Phase 7.5+ replaces this `signal' call with a process-abort
;; primitive.  Code that catches `nelisp-coding-strict-violation'
;; today will silently change behaviour at that point — DO NOT
;; structure error recovery on this signal.
(define-error 'nelisp-coding-strict-violation
  "Strict mode violation (Phase 7.5+ aborts process; do NOT catch in user code)"
  'error)

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

;;;; ────────────────────────────────────────────────────────────────────
;;;; Phase 7.4.4 — streaming API + file I/O integration (Doc 31 v2 §2.5
;;;; / §2.7 / §6.5)
;;;; ────────────────────────────────────────────────────────────────────
;;
;; Design (Doc 31 v2 §2.5 推奨 A):
;; - chunk-based callback API, Emacs `accept-process-output' compatible.
;; - default chunk size 64 KB. Caller supplies arbitrary chunks (filtered
;;   read from process / file etc).
;; - decode side: a streaming state struct holds pending bytes that may
;;   have started a multi-byte sequence which was truncated at the chunk
;;   boundary. The next chunk is prepended with these pending bytes
;;   *before* decoding — the per-encoding tail-trimmer figures out how
;;   many trailing bytes to defer.
;; - encode side: the state holds no carry-over (chunks are split at
;;   char boundaries by definition), but tracking is symmetric for
;;   stats / invalid-position aggregation.
;; - finalize: any pending bytes still in the buffer at end-of-stream
;;   are treated as a truncated sequence per the configured strategy.
;;
;; Doc 31 v2 §6.5 silent-corruption mitigation: each ERT smoke test for
;; this phase exercises chunk splitting at every byte position of a
;; multi-byte char and asserts the decoded char is identical to the
;; one-shot result.

(defcustom nelisp-coding-stream-default-chunk-size (* 64 1024)
  "Default chunk size for streaming codec (64 KB, Doc 31 v2 §2.5).

The caller is free to feed chunks of any size; this constant is the
suggested page-aligned default for read-loop callers (e.g.
`nelisp-coding-read-file-with-encoding' below)."
  :type 'integer
  :group 'nelisp-coding)

(cl-defstruct (nelisp-coding--stream-state
               (:constructor nelisp-coding--stream-state-make)
               (:copier nelisp-coding--stream-state-copy))
  "Streaming codec state (Doc 31 v2 §2.5).

Slots:
- ENCODING — symbol naming the codec (`utf-8' / `latin-1' / `shift-jis' /
  `cp932' / `euc-jp'). For symmetry with one-shot APIs, both `shift-jis'
  and `cp932' route through the same SJIS/CP932 decoder.
- DIRECTION — either `decode' (bytes → chars) or `encode' (chars → bytes).
- STRATEGY — one of `replace' (default), `error', `strict' per Doc 31
  v2 §2.4 contract LOCK. Inherits `nelisp-coding-error-strategy' if nil.
- PENDING-BYTES — list of byte integers carried over from the previous
  decode chunk because they began an incomplete multi-byte sequence.
  Always empty for `latin-1' (single-byte encoding) and for the encode
  direction (chunks are split at char boundaries).
- DECODED-CHARS — accumulated decoded codepoints, in REVERSE order
  (= push order). Reversed once on finalize.
- ENCODED-BYTES — accumulated encoded bytes, in REVERSE order. Reversed
  once on finalize.
- CHUNKS-PROCESSED — total count of chunks fed to the state.
- BYTES-CONSUMED — total bytes drained from the input side (decode:
  bytes accepted; encode: bytes emitted).
- CHARS-EMITTED — total chars emitted from decode / consumed by encode.
- INVALID-POSITIONS — list of offsets (BYTE for decode side, CHAR for
  encode side) at which an invalid sequence was detected. Aggregated
  across chunks, in REVERSE order (reversed on finalize).
- REPLACEMENTS — total count of replacement codepoints/bytes emitted
  under `replace' strategy.
- BOM-CHECKED — t once a UTF-8 BOM detection has been attempted on
  the very first decoded chunk (so subsequent chunks do not strip BOM
  again, and `had-bom' is recorded once).
- HAD-BOM — t when a UTF-8 BOM was stripped from the very first chunk.
- TOTAL-INPUT-OFFSET — running offset of the *original input* used to
  report `:invalid-positions' in stable absolute coordinates across
  chunks (incremented by every byte consumed, including BOM bytes)."
  (encoding 'utf-8)
  (direction 'decode)
  (strategy nil)
  (pending-bytes nil)
  (decoded-chars nil)
  (encoded-bytes nil)
  (chunks-processed 0)
  (bytes-consumed 0)
  (chars-emitted 0)
  (invalid-positions nil)
  (replacements 0)
  (bom-checked nil)
  (had-bom nil)
  (total-input-offset 0))

(defun nelisp-coding-stream-state-create (encoding direction &optional strategy)
  "Public constructor for a streaming codec state.

ENCODING must be one of `utf-8', `latin-1', `shift-jis', `cp932',
`euc-jp'. DIRECTION is either `decode' or `encode'. STRATEGY (optional,
default `nelisp-coding-error-strategy') is one of `replace', `error',
`strict' per Doc 31 v2 §2.4.

Returns a fresh `nelisp-coding--stream-state' struct."
  (unless (memq encoding '(utf-8 latin-1 shift-jis cp932 euc-jp))
    (signal 'nelisp-coding-error
            (list :reason 'unknown-encoding :encoding encoding)))
  (unless (memq direction '(decode encode))
    (signal 'nelisp-coding-error
            (list :reason 'unknown-direction :direction direction)))
  (let ((eff-strategy (or strategy nelisp-coding-error-strategy)))
    (unless (memq eff-strategy '(replace error strict))
      (setq eff-strategy 'replace))
    (nelisp-coding--stream-state-make
     :encoding encoding
     :direction direction
     :strategy eff-strategy)))

;;; ── Per-encoding tail trimmer (decode side) ──
;;
;; Given a vector of bytes representing the freshly-arrived chunk
;; (possibly already prepended with the previous chunk's pending bytes),
;; return the index FROM THE END at which we must stop decoding because
;; the trailing bytes might be an incomplete multi-byte sequence whose
;; remainder is in the next chunk. The caller decodes [0..N-K) and
;; saves [N-K..N) into PENDING-BYTES.
;;
;; Returns 0 if the chunk has no incomplete tail (= safe to decode the
;; whole chunk).

(defun nelisp-coding--stream-utf8-tail-pending (vec n)
  "How many trailing bytes of VEC (length N) might be an incomplete UTF-8 seq?

Walks back up to 4 bytes (max UTF-8 sequence length) looking for a
leading byte. If found at offset N-K with K bytes available but its
expected sequence length is L > K, return K (defer those bytes). If
no leading byte appears within the last 4 bytes (= all continuation
bytes), the trailing run cannot make sense as a fresh sequence; we
defer 0 (the per-byte error path will handle it). 0 if the last byte
is itself a complete 1-byte ASCII (< 0x80)."
  (cond
   ((<= n 0) 0)
   ;; Last byte is ASCII = no carry-over needed.
   ((< (aref vec (1- n)) #x80) 0)
   (t
    (let ((k 1)
          (found nil))
      ;; Scan backward up to 4 bytes for a leading byte.
      (while (and (not found) (<= k 4) (<= k n))
        (let* ((byte (aref vec (- n k)))
               (info (nelisp-coding--utf8-leading-byte-info byte)))
          (cond
           ((and info (= (car info) 1))
            ;; ASCII (1-byte) — found a complete fresh codepoint just before
            ;; the multi-byte tail. Defer the (k-1) trailing continuation
            ;; bytes (they cannot be a fresh leading byte each).
            (setq found (1- k)))
           (info
            ;; Multi-byte leading byte. If expected length > available, defer.
            (let ((expected-len (car info)))
              (if (> expected-len k)
                  (setq found k)
                ;; Sequence is complete inside the chunk.
                (setq found 0))))
           (t
            ;; Continuation byte — keep scanning.
            (setq k (1+ k))))))
      (or found 0)))))

(defun nelisp-coding--stream-shift-jis-tail-pending (vec n)
  "How many trailing bytes might be an incomplete Shift-JIS sequence?

SJIS multi-byte sequences are exactly 2 bytes. So defer 1 byte iff the
last byte is a SJIS lead byte that has no trail yet."
  (if (and (> n 0)
           (nelisp-coding--shift-jis-lead-byte-p (aref vec (1- n))))
      1
    0))

(defun nelisp-coding--stream-euc-jp-tail-pending (vec n)
  "How many trailing bytes might be an incomplete EUC-JP sequence?

EUC-JP code-set structure:
- CS0 (ASCII):  1 byte  0x00-0x7F
- CS1 (X 0208): 2 byte  0xA1-0xFE + 0xA1-0xFE
- CS2 (X 0201 halfwidth katakana): 2 byte  0x8E + 0xA1-0xDF
- CS3 (X 0212): 3 byte  0x8F + 0xA1-0xFE + 0xA1-0xFE

We must return the *minimal* number of trailing bytes that could be a
truly-incomplete sequence prefix.  Returning too many bytes (the T56
audit bug, T67) corrupts complete sequences whose last byte happens to
fall in the A1-FE range — `A4 A2', `8E B1', `8F A2 AF' all decoded
incorrectly because the old code blindly deferred any A1-FE last byte.

Algorithm — walk backward from byte index N-1 to find the start of the
last (possibly partial) sequence:

  1. Inspect the last byte (b1 = vec[n-1]).
     - If b1 < 0x80 (ASCII)  → pending = 0.
     - If b1 ∈ {0x8E, 0x8F}  → pending = 1 (CS2/CS3 lead alone).
     - If b1 ∈ invalid range (0x80-0x8D / 0x90-0xA0 / 0xFF)
                              → pending = 0 (decoder will signal/replace).
     - If b1 ∈ A1-FE: ambiguous; consult the byte before it.

  2. b1 ∈ A1-FE, n ≥ 2.  Inspect b2 = vec[n-2].
     - b2 == 0x8F → CS3 with 2 of 3 bytes → pending = 2.
     - b2 == 0x8E → CS2 complete (8E + b1 covers exactly 2 bytes;
                   even if b1 ∉ A1-DF the decoder handles it as
                   invalid, but the pair is *complete* — pending = 0).
     - b2 < 0x80 or invalid range → b1 is a fresh CS1 lead alone
                   after an anchor → pending = 1.
     - b2 ∈ A1-FE: ambiguous; both bytes are in the CS1 trail/lead
                   range.  Determine parity by walking back to the
                   nearest anchor (= a byte that is *not* in A1-FE,
                   or position -1 = start-of-buffer).  An anchor's
                   role is fully determined:
                   * 0x8F at pos p → consumes p..p+2 (CS3); A1-FE run
                     after pos p+3 forms CS1 pairs.
                   * 0x8E at pos p → consumes p..p+1 (CS2); A1-FE run
                     after p+2 forms CS1 pairs.
                   * any other anchor (ASCII / invalid) → bytes from
                     pos p+1 onward form CS1 pairs.
                   Count A1-FE bytes between (anchor-end + 1) and
                   n - 1.  Parity = pending (even → 0, odd → 1).

This is O(distance-to-last-anchor) which is bounded in practice (a
chunk of pure CS1 text from start has all-A1-FE bytes; the worst case
is a chunk-size-long walk, but only on chunks that are 100% CS1 with
no anchor — extremely rare in practice and still O(N) once)."
  (cond
   ((<= n 0) 0)
   (t
    (let ((b1 (aref vec (1- n))))
      (cond
       ;; ASCII / CS0 — complete.
       ((< b1 #x80) 0)
       ;; CS3 shifter alone at end → 1 byte pending.
       ((= b1 #x8F) 1)
       ;; CS2 shifter alone at end → 1 byte pending.
       ((= b1 #x8E) 1)
       ;; Invalid byte (0x80-0x8D except 8E/8F, 0x90-0xA0, 0xFF) — decoder
       ;; will record it as invalid; from a streaming standpoint this is
       ;; a complete (1-byte) "unit", so do not defer it.
       ((or (and (>= b1 #x80) (< b1 #xA1))
            (= b1 #xFF))
        0)
       ;; b1 ∈ A1-FE — ambiguous CS1 lead/trail or CS3 trail.  Look at b2.
       (t
        (cond
         ((= n 1) 1)            ; lone A1-FE = CS1 lead alone.
         (t
          (let ((b2 (aref vec (- n 2))))
            (cond
             ;; CS3 with 2 of 3 bytes (8F + A1-FE awaiting final trail).
             ((= b2 #x8F) 2)
             ;; CS2 complete (8E + b1) — pair fully present.  Even if
             ;; b1 ∉ A1-DF (technically out-of-range CS2 trail), the
             ;; sequence is *2 bytes long* by EUC-JP framing rules and
             ;; the one-shot decoder will tag it as invalid.  Pending=0.
             ((= b2 #x8E) 0)
             ;; b2 ∈ ASCII or invalid range → b1 is a fresh CS1 lead
             ;; alone, awaiting trail.
             ((or (< b2 #x80)
                  (and (>= b2 #x80) (< b2 #xA1))
                  (= b2 #xFF))
              1)
             ;; b2 ∈ A1-FE — both b1 and b2 are in the CS1 trail/lead
             ;; range.  Walk further back to find the nearest anchor
             ;; and use parity.
             (t
              (nelisp-coding--euc-jp-tail-parity vec n))))))))))))

(defun nelisp-coding--euc-jp-tail-parity (vec n)
  "Helper for `nelisp-coding--stream-euc-jp-tail-pending'.

Walk backward from VEC index N-1 to the nearest *anchor* — a byte that
is not in 0xA1-0xFE.  Return the pending byte count for the trailing
A1-FE run based on parity (and the anchor type):

- anchor at pos P with byte 0x8F: CS3 consumes P..P+2; A1-FE bytes from
  P+3 to N-1 form CS1 pairs → parity of (N - P - 3).
- anchor at pos P with byte 0x8E: CS2 consumes P..P+1; A1-FE bytes from
  P+2 to N-1 form CS1 pairs → parity of (N - P - 2).
- any other anchor at pos P: A1-FE bytes from P+1 to N-1 form CS1 pairs
  → parity of (N - P - 1).
- no anchor found (all bytes A1-FE from pos 0 to N-1): parity of N.

Even count → pending = 0; odd count → pending = 1."
  (let ((p (- n 1))                     ; we know vec[p] ∈ A1-FE already
        (found-anchor nil)
        (anchor-pos -1)
        (anchor-byte nil))
    ;; Walk back searching for the first non-A1-FE byte.
    (while (and (>= p 0) (not found-anchor))
      (let ((b (aref vec p)))
        (if (and (>= b #xA1) (<= b #xFE))
            (setq p (1- p))
          (setq found-anchor t
                anchor-pos p
                anchor-byte b))))
    (let* ((run-start
            (cond
             ((not found-anchor) 0)
             ((= anchor-byte #x8F) (+ anchor-pos 3))
             ((= anchor-byte #x8E) (+ anchor-pos 2))
             (t                    (+ anchor-pos 1))))
           (run-len (- n run-start)))
      ;; Defensive: run-start may exceed n if the anchor is 0x8F/0x8E
      ;; near the end (e.g., 0x8F at pos n-1 — but then vec[n-1] would
      ;; not be in A1-FE and we wouldn't have entered the parity path).
      (cond
       ((<= run-len 0) 0)
       ((zerop (mod run-len 2)) 0)
       (t 1)))))

(defun nelisp-coding--stream-tail-pending (encoding vec n)
  "Dispatch tail-pending byte count by ENCODING for VEC of length N."
  (pcase encoding
    ('utf-8     (nelisp-coding--stream-utf8-tail-pending vec n))
    ('latin-1   0)
    ((or 'shift-jis 'cp932)
     (nelisp-coding--stream-shift-jis-tail-pending vec n))
    ('euc-jp    (nelisp-coding--stream-euc-jp-tail-pending vec n))
    (_ 0)))

;;; ── Per-encoding one-shot decode dispatcher ──

(defun nelisp-coding--stream-decode-call (encoding bytes strategy)
  "Run a one-shot decode of BYTES under ENCODING + STRATEGY, return plist.

Used internally by `nelisp-coding-stream-decode-chunk' on the trimmed
chunk body (= bytes that are guaranteed to end on a complete-sequence
boundary)."
  (pcase encoding
    ('utf-8     (nelisp-coding-utf8-decode bytes strategy))
    ('latin-1   (nelisp-coding-latin1-decode bytes))
    ((or 'shift-jis 'cp932)
     (nelisp-coding-shift-jis-decode bytes strategy))
    ('euc-jp    (nelisp-coding-euc-jp-decode bytes strategy))
    (_ (signal 'nelisp-coding-error
               (list :reason 'unknown-encoding :encoding encoding)))))

(defun nelisp-coding--stream-encode-call (encoding string strategy)
  "Run a one-shot encode of STRING under ENCODING + STRATEGY, return plist."
  (pcase encoding
    ('utf-8
     ;; nelisp-coding-utf8-encode signals on bad codepoint regardless
     ;; of strategy (Phase 7.4.1 contract). Wrap into the plist shape.
     (let ((bytes (nelisp-coding-utf8-encode string)))
       (list :bytes bytes
             :strategy 'replace
             :invalid-positions nil
             :replacements 0)))
    ('latin-1   (nelisp-coding-latin1-encode string strategy))
    ((or 'shift-jis 'cp932)
     (nelisp-coding-shift-jis-encode string strategy))
    ('euc-jp    (nelisp-coding-euc-jp-encode string strategy))
    (_ (signal 'nelisp-coding-error
               (list :reason 'unknown-encoding :encoding encoding)))))

;;; ── Public API: decode side ──

(defun nelisp-coding-stream-decode-chunk (state chunk-bytes)
  "Decode CHUNK-BYTES into STATE (a `nelisp-coding--stream-state').

CHUNK-BYTES may be a unibyte string, a vector of integers, or a list
of integers. Multi-byte sequences whose tail extends past the chunk
boundary are buffered into STATE.PENDING-BYTES and consumed on the
next chunk.

UTF-8 BOM stripping is performed exactly once, on the very first chunk
fed to the state (Doc 31 v2 §2.3). The stripped BOM bytes count toward
TOTAL-INPUT-OFFSET so subsequent `:invalid-positions' remain absolute.

For \\='replace strategy invalid sequences are recorded with absolute
offsets in STATE.INVALID-POSITIONS (push order = reverse), and U+FFFD
is appended to STATE.DECODED-CHARS. For \\='error / \\='strict, the
per-call one-shot decoder signals; the state is left in a consistent
half-decoded form (caller may catch and inspect).

Returns the same STATE (mutated in place); signals on direction
mismatch."
  (unless (eq (nelisp-coding--stream-state-direction state) 'decode)
    (signal 'nelisp-coding-error
            (list :reason 'wrong-direction
                  :direction (nelisp-coding--stream-state-direction state))))
  (let* ((encoding (nelisp-coding--stream-state-encoding state))
         (strategy (nelisp-coding--stream-state-strategy state))
         (chunk-list (nelisp-coding--bytes-to-list chunk-bytes))
         (combined (append (nelisp-coding--stream-state-pending-bytes state)
                           chunk-list))
         ;; Carry-over offset = absolute starting offset of `combined'
         ;; in the original input. The pending bytes were already at
         ;; (total - len(pending)).
         (pending-len (length
                       (nelisp-coding--stream-state-pending-bytes state)))
         (combined-base
          (- (nelisp-coding--stream-state-total-input-offset state)
             pending-len))
         (had-bom-now nil))
    ;; UTF-8 BOM strip: only on the very first attempt (first chunk that
    ;; brings ≥3 bytes total). We want to detect at the absolute offset
    ;; 0 of the input — so check `combined' iff total-input-offset
    ;; minus pending was 0 at entry (= we have not consumed any non-
    ;; pending bytes yet) and we have not already attempted a strip.
    (when (and (eq encoding 'utf-8)
               (not (nelisp-coding--stream-state-bom-checked state))
               (= combined-base 0)
               (>= (length combined) 3))
      (setf (nelisp-coding--stream-state-bom-checked state) t)
      (when (and (= (nth 0 combined) #xEF)
                 (= (nth 1 combined) #xBB)
                 (= (nth 2 combined) #xBF))
        (setq combined (nthcdr 3 combined))
        (setq combined-base 3)
        (setf (nelisp-coding--stream-state-had-bom state) t)
        (setq had-bom-now t)))
    ;; If we see fewer than 3 bytes total on the first call but BOM
    ;; could still arrive, defer everything (keep bom-checked nil).
    (cond
     ((and (eq encoding 'utf-8)
           (not (nelisp-coding--stream-state-bom-checked state))
           (= combined-base 0)
           (< (length combined) 3))
      ;; Defer the entire combined buffer — we cannot tell whether
      ;; this is BOM yet. Update bookkeeping minimally.
      (setf (nelisp-coding--stream-state-pending-bytes state) combined)
      (cl-incf (nelisp-coding--stream-state-bytes-consumed state)
               (length chunk-list))
      (cl-incf (nelisp-coding--stream-state-total-input-offset state)
               (length chunk-list))
      (cl-incf (nelisp-coding--stream-state-chunks-processed state))
      state)
     (t
      (let* ((vec (vconcat combined))
             (n (length vec))
             (k (nelisp-coding--stream-tail-pending encoding vec n))
             (decode-len (- n k))
             (to-decode (substring vec 0 decode-len))
             (new-pending (if (> k 0)
                              (append (substring vec decode-len) nil)
                            nil)))
        (when (> decode-len 0)
          (let* ((res (nelisp-coding--stream-decode-call
                       encoding to-decode strategy))
                 (decoded-string (plist-get res :string))
                 (rel-invalid (plist-get res :invalid-positions))
                 (replacements (or (plist-get res :replacements) 0))
                 ;; Adjust :invalid-positions to absolute coordinates.
                 (abs-invalid
                  (mapcar (lambda (pos) (+ pos combined-base))
                          rel-invalid)))
            ;; Push decoded codepoints in reverse onto state's reverse list.
            (let ((i 0)
                  (m (length decoded-string)))
              (while (< i m)
                (push (aref decoded-string i)
                      (nelisp-coding--stream-state-decoded-chars state))
                (setq i (1+ i))))
            ;; Append invalid-positions (already absolute, in original
            ;; left-to-right order; we push in reverse so finalize can
            ;; nreverse to a single sorted list).
            (dolist (p (reverse abs-invalid))
              (push p (nelisp-coding--stream-state-invalid-positions state)))
            (cl-incf (nelisp-coding--stream-state-replacements state)
                     replacements)
            (cl-incf (nelisp-coding--stream-state-chars-emitted state)
                     (length decoded-string))))
        ;; Update per-state bookkeeping.
        (setf (nelisp-coding--stream-state-pending-bytes state) new-pending)
        ;; total-input-offset advanced by the length of *new* input
        ;; bytes only (not by the previously-pending bytes, which were
        ;; already counted).
        (cl-incf (nelisp-coding--stream-state-total-input-offset state)
                 (length chunk-list))
        ;; bytes-consumed counts decode-progress bytes (= bytes that
        ;; left the buffer this call). Excludes BOM, since BOM is
        ;; metadata not user data.
        (cl-incf (nelisp-coding--stream-state-bytes-consumed state)
                 (- decode-len (if had-bom-now 0 0)))
        ;; (Note: the BOM bytes are counted toward total-input-offset
        ;; via the chunk-list length; bytes-consumed reflects raw
        ;; user-data bytes effectively decoded.)
        (cl-incf (nelisp-coding--stream-state-chunks-processed state))
        state)))))

(defun nelisp-coding-stream-decode-finalize (state)
  "Finalize decoding for STATE; returns a final result plist.

Any bytes still in PENDING-BYTES are treated as a truncated multi-byte
sequence per STATE.STRATEGY:
- `replace' — emit a single U+FFFD and record each pending byte's
  absolute offset in :invalid-positions
- `error'   — signal `nelisp-coding-invalid-byte' with :truncated
- `strict'  — signal `nelisp-coding-strict-violation' with :truncated

Returns a plist:
  (:string DECODED-FULL-STRING
   :strategy STRATEGY
   :invalid-positions (LIST OF BYTE-OFFSET, sorted ascending)
   :replacements N
   :had-bom BOOLEAN-OR-NIL
   :chunks-processed N
   :bytes-consumed N
   :chars-emitted N)"
  (unless (eq (nelisp-coding--stream-state-direction state) 'decode)
    (signal 'nelisp-coding-error
            (list :reason 'wrong-direction
                  :direction (nelisp-coding--stream-state-direction state))))
  (let* ((strategy (nelisp-coding--stream-state-strategy state))
         (pending (nelisp-coding--stream-state-pending-bytes state))
         (pending-len (length pending))
         (total-offset (nelisp-coding--stream-state-total-input-offset state))
         (truncated-base (- total-offset pending-len)))
    (when (> pending-len 0)
      (pcase strategy
        ('error
         (signal 'nelisp-coding-invalid-byte
                 (list :offset truncated-base
                       :strategy 'error
                       :reason 'truncated
                       :pending pending)))
        ('strict
         (signal 'nelisp-coding-strict-violation
                 (list :offset truncated-base
                       :strategy 'strict
                       :reason 'truncated
                       :pending pending)))
        (_
         ;; replace — single U+FFFD + record one position per pending byte
         (push nelisp-coding-utf8-replacement-char
               (nelisp-coding--stream-state-decoded-chars state))
         (cl-incf (nelisp-coding--stream-state-replacements state))
         (cl-incf (nelisp-coding--stream-state-chars-emitted state))
         (let ((i 0))
           (while (< i pending-len)
             (push (+ truncated-base i)
                   (nelisp-coding--stream-state-invalid-positions state))
             (setq i (1+ i))))
         (setf (nelisp-coding--stream-state-pending-bytes state) nil))))
    (let ((chars (nreverse
                  (nelisp-coding--stream-state-decoded-chars state)))
          (invalid-list
           (nreverse
            (nelisp-coding--stream-state-invalid-positions state))))
      (list :string (apply #'string chars)
            :strategy strategy
            :invalid-positions invalid-list
            :replacements (nelisp-coding--stream-state-replacements state)
            :had-bom (nelisp-coding--stream-state-had-bom state)
            :chunks-processed
            (nelisp-coding--stream-state-chunks-processed state)
            :bytes-consumed
            (nelisp-coding--stream-state-bytes-consumed state)
            :chars-emitted
            (nelisp-coding--stream-state-chars-emitted state)))))

;;; ── Public API: encode side ──
;;
;; Encode chunks are by definition split at codepoint boundaries (the
;; caller hands us a fragment of a NeLisp string), so there is no
;; carry-over byte buffer. The state still tracks aggregated bytes /
;; invalid-positions / replacements for symmetry with the decode side.

(defun nelisp-coding-stream-encode-chunk (state chunk-string)
  "Encode CHUNK-STRING into STATE (a `nelisp-coding--stream-state').

CHUNK-STRING is a host Emacs string (codepoint-indexed). The encoded
bytes are appended to STATE.ENCODED-BYTES (in REVERSE order) and
metadata aggregated. Returns the same STATE (mutated)."
  (unless (eq (nelisp-coding--stream-state-direction state) 'encode)
    (signal 'nelisp-coding-error
            (list :reason 'wrong-direction
                  :direction (nelisp-coding--stream-state-direction state))))
  (unless (stringp chunk-string)
    (signal 'wrong-type-argument (list 'stringp chunk-string)))
  (let* ((encoding (nelisp-coding--stream-state-encoding state))
         (strategy (nelisp-coding--stream-state-strategy state))
         (chars-base (nelisp-coding--stream-state-chars-emitted state))
         (res (nelisp-coding--stream-encode-call encoding chunk-string strategy))
         (bytes (plist-get res :bytes))
         (rel-invalid (plist-get res :invalid-positions))
         (replacements (or (plist-get res :replacements) 0))
         (abs-invalid (mapcar (lambda (pos) (+ pos chars-base))
                              rel-invalid)))
    (dolist (b bytes)
      (push b (nelisp-coding--stream-state-encoded-bytes state)))
    (dolist (p (reverse abs-invalid))
      (push p (nelisp-coding--stream-state-invalid-positions state)))
    (cl-incf (nelisp-coding--stream-state-replacements state)
             replacements)
    (cl-incf (nelisp-coding--stream-state-bytes-consumed state)
             (length bytes))
    (cl-incf (nelisp-coding--stream-state-chars-emitted state)
             (length chunk-string))
    (cl-incf (nelisp-coding--stream-state-chunks-processed state))
    state))

(defun nelisp-coding-stream-encode-finalize (state)
  "Finalize encoding for STATE; returns a final result plist.

Returns:
  (:bytes (LIST OF BYTES)
   :strategy STRATEGY
   :invalid-positions (LIST OF CHAR-OFFSET, sorted ascending)
   :replacements N
   :chunks-processed N
   :bytes-consumed N (= total bytes emitted)
   :chars-consumed N (= total chars accepted))

Note: encode-side `:bytes-consumed' equals the total bytes emitted
(symmetric with decode-side `:bytes-consumed' = bytes accepted), and
`:chars-consumed' is the symmetric peer of `:chars-emitted' for the
decode direction."
  (unless (eq (nelisp-coding--stream-state-direction state) 'encode)
    (signal 'nelisp-coding-error
            (list :reason 'wrong-direction
                  :direction (nelisp-coding--stream-state-direction state))))
  (let* ((bytes (nreverse
                 (nelisp-coding--stream-state-encoded-bytes state)))
         (invalid-list
          (nreverse
           (nelisp-coding--stream-state-invalid-positions state))))
    (list :bytes bytes
          :strategy (nelisp-coding--stream-state-strategy state)
          :invalid-positions invalid-list
          :replacements (nelisp-coding--stream-state-replacements state)
          :chunks-processed
          (nelisp-coding--stream-state-chunks-processed state)
          :bytes-consumed
          (nelisp-coding--stream-state-bytes-consumed state)
          :chars-consumed
          (nelisp-coding--stream-state-chars-emitted state))))

;;;; ────────────────────────────────────────────────────────────────────
;;;; Phase 7.4.4 file I/O wrappers (Doc 31 v2 §2.7 推奨 A)
;;;; ────────────────────────────────────────────────────────────────────
;;
;; MVP implementation reads/writes the whole file via host Emacs
;; primitives (`insert-file-contents-literally' / `write-region') and
;; pipes through the streaming codec API in chunks. The host primitives
;; are used as a *simulator* for the Phase 7.0 syscall stub (real
;; integration is Phase 7.5). NeLisp purity is preserved at the API
;; boundary — callers see the same plist contract as the streaming API.

(defun nelisp-coding-read-file-with-encoding
    (path encoding &optional strategy chunk-size)
  "Read PATH, decode under ENCODING + STRATEGY in chunks, return result plist.

ENCODING / STRATEGY are passed to `nelisp-coding-stream-state-create'.
CHUNK-SIZE defaults to `nelisp-coding-stream-default-chunk-size'.

Returns the same plist shape as `nelisp-coding-stream-decode-finalize'
plus `:path PATH'.

Implementation note (T67 fix): this is a *real streaming* read.  The
file is read in CHUNK-SIZE byte windows via `insert-file-contents-
literally' with explicit BEG/END byte ranges, so the entire file is
*never* buffered in host memory at once.  Each chunk is decoded into
the persistent stream state and the host buffer is wiped before the
next read.  This restores the 1GB OOM-free streaming gate that the
T56 audit flagged as broken in the prior `insert-file-contents-
literally' (whole-file) implementation."
  (unless (file-readable-p path)
    (signal 'file-error (list "File not readable" path)))
  (let* ((eff-strategy (or strategy nelisp-coding-error-strategy))
         (eff-chunk (or chunk-size nelisp-coding-stream-default-chunk-size))
         (state (nelisp-coding-stream-state-create
                 encoding 'decode eff-strategy))
         (file-size (file-attribute-size (file-attributes path)))
         (pos 0))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (while (< pos file-size)
        (let ((end (min file-size (+ pos eff-chunk))))
          ;; Reuse a single host buffer, wiping it between reads so the
          ;; resident memory footprint is O(CHUNK-SIZE), not O(file-size).
          (erase-buffer)
          ;; insert-file-contents-literally with BEG/END reads only the
          ;; specified byte range from disk — this is the actual streaming
          ;; read primitive available to portable Emacs Lisp.
          (insert-file-contents-literally path nil pos end)
          (let ((chunk (buffer-substring-no-properties (point-min)
                                                       (point-max))))
            (nelisp-coding-stream-decode-chunk state chunk))
          (setq pos end))))
    (let ((result (nelisp-coding-stream-decode-finalize state)))
      (plist-put result :path path))))

(defun nelisp-coding-write-file-with-encoding
    (path string encoding &optional strategy chunk-size)
  "Encode STRING under ENCODING + STRATEGY in chunks and write to PATH.

ENCODING / STRATEGY are passed to `nelisp-coding-stream-state-create'.
CHUNK-SIZE defaults to `nelisp-coding-stream-default-chunk-size' but is
applied to STRING char-count (= encode-side chunks are char-aligned).

Returns the same plist shape as `nelisp-coding-stream-encode-finalize'
plus `:path PATH', and writes the encoded bytes to PATH."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (let* ((eff-strategy (or strategy nelisp-coding-error-strategy))
         (eff-chunk (or chunk-size nelisp-coding-stream-default-chunk-size))
         (state (nelisp-coding-stream-state-create
                 encoding 'encode eff-strategy))
         (n (length string))
         (pos 0))
    (while (< pos n)
      (let* ((end (min n (+ pos eff-chunk)))
             (chunk (substring string pos end)))
        (nelisp-coding-stream-encode-chunk state chunk)
        (setq pos end)))
    (let* ((result (nelisp-coding-stream-encode-finalize state))
           (bytes (plist-get result :bytes))
           ;; Convert byte list to a unibyte string for write-region.
           (unibyte (apply #'unibyte-string bytes))
           (coding-system-for-write 'no-conversion))
      (write-region unibyte nil path nil 'silent)
      (plist-put result :path path))))

(provide 'nelisp-coding)

;;; nelisp-coding.el ends here
