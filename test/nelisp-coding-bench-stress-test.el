;;; nelisp-coding-bench-stress-test.el --- Phase 7.4 §7.2 LOCK-close bench + stress  -*- lexical-binding: t; -*-

;; Doc 31 v2 LOCKED 2026-04-25 §5.2 + §7.2 — Phase 7.4 完全 close 条件の
;; *INDEPENDENT* (= Phase 7.1 native baseline 非依存) 部分:
;;
;;   - throughput tier-B (release-audit, pinned reference host):
;;       UTF-8     = 200 MB/sec ± 10%
;;       Latin-1   = 300 MB/sec ± 10%
;;       Shift-JIS = 100 MB/sec ± 10%
;;       EUC-JP    = 100 MB/sec ± 10%
;;     (round-trip = encode + decode, single-core, ≥100MB workload)
;;   - stream-1gb-utf8     = OOM-free + completion < 10 sec
;;   - chunk-boundary-stress = 100K iter random chunk-split mismatch 0
;;
;; tier-A (Phase 7.1 比 5x / 3x speedup ratio gate) は本 file の管轄外
;; (Phase 7.1 native baseline 完成までブロック)。
;;
;; ─────────────────────────────────────────────────────────────────────
;; Heavy gate (NELISP_HEAVY_TESTS=1):
;; ─────────────────────────────────────────────────────────────────────
;;
;; 100MB throughput, 1GB stream decode, 100K iter stress は CI / `make
;; test' を秒単位で完結させたい開発ループでは skip。リリース監査では
;;
;;     NELISP_HEAVY_TESTS=1 make test
;;
;; で全試験を実行。各 heavy test には軽量 smoke 兄弟 (64KB throughput
;; probe / 500 iter chunk-stress) があり、`make test' default で
;; regression を確実に拾う。
;;
;; *Perf reality check (2026-04-26)*: pure-Elisp encode/decode の
;; throughput は ~1-3 MB/sec オーダー (Phase 7.1 native baseline 完成前)。
;; Doc 31 §5.2 の 100-300 MB/sec target は post-Phase 7.1 の数字なので、
;; 本 file の heavy throughput tests は NELISP_HEAVY_TESTS=1 設定時に
;; *数十分* 走り得る。NELISP_PINNED_HOST=1 を立てない限り gate は
;; warning にとどまるため、必ずしも "PASS" が達成可能とは限らない。
;; 100K iter chunk-stress は ~3 分で完走 (実証済 178 sec / 100K)。
;;
;; ─────────────────────────────────────────────────────────────────────
;; Tier-B absolute MB/sec gate behavior:
;; ─────────────────────────────────────────────────────────────────────
;;
;; pure-Elisp encode/decode は host machine の CPU 性能で大きくブレる
;; (M2 Max ↔ Raspberry Pi で 10x 差)。Doc 31 v2 §7.2 の pinned reference
;; host (= release-audit ホスト) を識別する手段は未定義のため、本 file
;; は throughput を *測定して報告* するが、host が pinned reference と
;; 確認できない限り gate は warning に留める (= test PASS、stderr に
;; "BELOW TARGET" 注記)。NELISP_PINNED_HOST=1 が立っている場合のみ
;; ±10% gate を hard-fail に昇格。これにより:
;;   - 開発機での test は緑のまま、性能変化は stderr で見える
;;   - CI release-audit ホストでは NELISP_PINNED_HOST=1 で hard gate
;;
;; +8 ERT (本 file):
;;
;;   1. bench-throughput-utf8-1mb-smoke           — light perf probe (always-on, 64KB)
;;   2. bench-throughput-tier-b-utf8-100mb        — Doc 31 §5.2 200MB/s gate
;;   3. bench-throughput-tier-b-latin1-100mb      — Doc 31 §5.2 300MB/s gate
;;   4. bench-throughput-tier-b-shift-jis-100mb   — Doc 31 §5.2 100MB/s gate
;;   5. bench-throughput-tier-b-euc-jp-100mb      — Doc 31 §5.2 100MB/s gate
;;   6. stream-1gb-utf8-oom-free                  — Doc 31 §5.2 1GB streaming
;;   7. chunk-boundary-stress-1k-iter-smoke       — light coverage (always-on)
;;   8. chunk-boundary-stress-100k-iter           — Doc 31 §5.2 100K iter

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-coding)

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Heavy gate helper
;;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-coding-bench-stress-test--heavy-enabled-p ()
  "Return non-nil iff env NELISP_HEAVY_TESTS=1 — gate for slow tests."
  (let ((v (getenv "NELISP_HEAVY_TESTS")))
    (and v (member v '("1" "true" "yes" "y" "on")))))

(defun nelisp-coding-bench-stress-test--skip-unless-heavy ()
  "Skip current ERT unless NELISP_HEAVY_TESTS=1 is set."
  (unless (nelisp-coding-bench-stress-test--heavy-enabled-p)
    (ert-skip
     "heavy bench/stress test (set NELISP_HEAVY_TESTS=1 to enable)")))

(defun nelisp-coding-bench-stress-test--pinned-host-p ()
  "Return non-nil iff env NELISP_PINNED_HOST=1 — promote ratio gate to
hard-fail.  When non-nil the tier-B ±10% absolute throughput gate is a
binding contract; otherwise the measurement is reported but not enforced."
  (let ((v (getenv "NELISP_PINNED_HOST")))
    (and v (member v '("1" "true" "yes" "y" "on")))))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Throughput measurement
;;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-coding-bench-stress-test--make-utf8-payload (n-chars)
  "Build a synthetic UTF-8 payload string of N-CHARS codepoints.
Mix of ASCII (50%) + 2-byte (20%) + 3-byte (25%) + 4-byte (5%) covers
all UTF-8 sequence lengths so the codec exercises every branch.

Initialised with a 4-byte placeholder so the underlying string is
multibyte from the start (= aset accepts any codepoint in-place)."
  (let ((out (make-string n-chars #x1F300))
        (i 0))
    (while (< i n-chars)
      (let ((bucket (mod i 20)))
        (aset out i
              (cond
               ((< bucket 10) (+ ?A (mod i 26)))         ; ASCII
               ((< bucket 14) (+ #x00C0 (mod i 64)))    ; 2-byte (Latin)
               ((< bucket 19) (+ #x3040 (mod i 96)))    ; 3-byte (hiragana)
               (t             (+ #x1F300 (mod i 256)))))) ; 4-byte (emoji)
      (setq i (1+ i)))
    out))

(defun nelisp-coding-bench-stress-test--make-latin1-payload (n-chars)
  "Build a synthetic Latin-1 payload string of N-CHARS codepoints
covering the full U+0000-U+00FF range.

Initialised with U+00FF so the string is multibyte; aset of any value
≤ U+00FF then succeeds without representation reshuffle."
  (let ((out (make-string n-chars #x00FF))
        (i 0))
    (while (< i n-chars)
      (aset out i (mod i #x100))
      (setq i (1+ i)))
    out))

(defun nelisp-coding-bench-stress-test--make-japanese-payload (n-chars)
  "Build a synthetic CJK payload string of N-CHARS codepoints biased to
JIS X 0208 hiragana + katakana + common kanji that round-trip cleanly
under Shift-JIS / EUC-JP.

Initialised with U+4E00 so the string is multibyte from the start."
  (let ((out (make-string n-chars #x4E00))
        (i 0))
    (while (< i n-chars)
      (let ((bucket (mod i 4)))
        (aset out i
              (cond
               ;; ASCII fast path
               ((= bucket 0) (+ ?A (mod i 26)))
               ;; Hiragana U+3041-U+3093
               ((= bucket 1) (+ #x3041 (mod i 83)))
               ;; Katakana U+30A1-U+30FA
               ((= bucket 2) (+ #x30A1 (mod i 90)))
               ;; Common kanji subset (small range guaranteed in X 0208)
               (t             (+ #x4E00 (mod i 64))))))
      (setq i (1+ i)))
    out))

(defun nelisp-coding-bench-stress-test--measure-mb-sec (work-fn n-bytes)
  "Run WORK-FN once, return throughput in MB/sec computed from N-BYTES.
Garbage-collect first so allocation noise doesn't leak into the timer."
  (garbage-collect)
  (let* ((start (current-time))
         (_     (funcall work-fn))
         (elapsed (float-time (time-subtract (current-time) start)))
         (mb (/ (float n-bytes) (* 1024.0 1024.0))))
    (if (zerop elapsed)
        most-positive-fixnum
      (/ mb elapsed))))

(defun nelisp-coding-bench-stress-test--report-throughput
    (label measured-mbsec target-mbsec tolerance)
  "Report MEASURED-MBSEC vs TARGET-MBSEC ± TOLERANCE for LABEL.
Always emits a stderr line.  When NELISP_PINNED_HOST=1 the test fails
hard if the measurement is below `target * (1 - tolerance)' — otherwise
the deviation is reported but not enforced (developer machines vary)."
  (let* ((min-allowed (* target-mbsec (- 1.0 tolerance)))
         (max-allowed (* target-mbsec (+ 1.0 tolerance)))
         (within (and (>= measured-mbsec min-allowed)
                      (<= measured-mbsec max-allowed)))
         (status (cond (within                       "WITHIN")
                       ((< measured-mbsec min-allowed) "BELOW TARGET")
                       (t                              "ABOVE TARGET"))))
    (message "[bench %s] measured = %.2f MB/sec, target = %.2f ± %.0f%% (%s)"
             label measured-mbsec target-mbsec (* tolerance 100.0) status)
    (when (nelisp-coding-bench-stress-test--pinned-host-p)
      (should (>= measured-mbsec min-allowed)))))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; 1. UTF-8 throughput smoke (always-on, 1MB)
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-bench-throughput-utf8-1mb-smoke ()
  "Light-weight UTF-8 round-trip throughput probe.
Always-on; only asserts that encode + decode complete and round-trip
identity holds.  Logs measured MB/sec to stderr for trend tracking.

Note on workload size: pure-Elisp encode/decode is O(N) but very slow
in absolute terms (~0.3 MB/s decode on a typical dev box without the
Phase 7.1 native baseline), so the smoke probe uses 64KB to keep
`make test' under a 5-second budget.  The Doc 31 §5.2 100MB tier-B
gate runs separately under NELISP_HEAVY_TESTS=1."
  (let* ((target-bytes (* 64 1024))
         ;; ~3.5 bytes/char average for our mixed payload.
         (n-chars (/ target-bytes 4))
         (s       (nelisp-coding-bench-stress-test--make-utf8-payload n-chars))
         (encoded-bytes nil)
         (encode-mbsec
          (nelisp-coding-bench-stress-test--measure-mb-sec
           (lambda () (setq encoded-bytes
                            (nelisp-coding-utf8-encode-string s)))
           (length s)))
         (decode-mbsec
          (nelisp-coding-bench-stress-test--measure-mb-sec
           (lambda () (nelisp-coding-utf8-decode encoded-bytes))
           (length encoded-bytes))))
    (should (stringp encoded-bytes))
    (should (> (length encoded-bytes) 0))
    (let* ((re-decoded (nelisp-coding-utf8-decode encoded-bytes))
           (got (plist-get re-decoded :string)))
      (should (equal got s)))
    (message "[bench utf8-64kb-smoke] encode = %.2f MB/sec, decode = %.2f MB/sec"
             encode-mbsec decode-mbsec)))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; 2-5. Throughput tier-B (heavy, gated)
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-bench-throughput-tier-b-utf8-100mb ()
  "Doc 31 v2 §5.2 tier-B: UTF-8 round-trip ≥ 200 MB/sec ± 10%.
Heavy (100MB workload) — gated on NELISP_HEAVY_TESTS=1.
Hard gate only on NELISP_PINNED_HOST=1; otherwise reports."
  (nelisp-coding-bench-stress-test--skip-unless-heavy)
  (let* ((n-chars (/ (* 100 1024 1024) 4))   ; ~100MB ascii-equivalent
         (s       (nelisp-coding-bench-stress-test--make-utf8-payload n-chars))
         (encoded nil)
         (encode-mbsec
          (nelisp-coding-bench-stress-test--measure-mb-sec
           (lambda () (setq encoded (nelisp-coding-utf8-encode-string s)))
           (length s)))
         (decode-mbsec
          (nelisp-coding-bench-stress-test--measure-mb-sec
           (lambda () (nelisp-coding-utf8-decode encoded))
           (length encoded)))
         ;; round-trip throughput = harmonic mean of encode + decode
         (rt-mbsec (/ 2.0 (+ (/ 1.0 encode-mbsec) (/ 1.0 decode-mbsec)))))
    (nelisp-coding-bench-stress-test--report-throughput
     "utf8-rt" rt-mbsec 200.0 0.10)))

(ert-deftest nelisp-coding-bench-throughput-tier-b-latin1-100mb ()
  "Doc 31 v2 §5.2 tier-B: Latin-1 round-trip ≥ 300 MB/sec ± 10%.
Heavy (100MB) — gated on NELISP_HEAVY_TESTS=1."
  (nelisp-coding-bench-stress-test--skip-unless-heavy)
  (let* ((n-chars (* 100 1024 1024))   ; Latin-1: 1 char = 1 byte
         (s       (nelisp-coding-bench-stress-test--make-latin1-payload n-chars))
         (encoded nil)
         (encode-mbsec
          (nelisp-coding-bench-stress-test--measure-mb-sec
           (lambda () (setq encoded (nelisp-coding-latin1-encode-string s)))
           (length s)))
         (decode-mbsec
          (nelisp-coding-bench-stress-test--measure-mb-sec
           (lambda () (nelisp-coding-latin1-decode encoded))
           (length encoded)))
         (rt-mbsec (/ 2.0 (+ (/ 1.0 encode-mbsec) (/ 1.0 decode-mbsec)))))
    (nelisp-coding-bench-stress-test--report-throughput
     "latin1-rt" rt-mbsec 300.0 0.10)))

(ert-deftest nelisp-coding-bench-throughput-tier-b-shift-jis-100mb ()
  "Doc 31 v2 §5.2 tier-B: Shift-JIS round-trip ≥ 100 MB/sec ± 10%.
Heavy (100MB Japanese) — gated on NELISP_HEAVY_TESTS=1."
  (nelisp-coding-bench-stress-test--skip-unless-heavy)
  (let* ((n-chars (/ (* 100 1024 1024) 2))   ; ~2 bytes/char SJIS
         (s       (nelisp-coding-bench-stress-test--make-japanese-payload n-chars))
         (encoded nil)
         (encode-mbsec
          (nelisp-coding-bench-stress-test--measure-mb-sec
           (lambda () (setq encoded (nelisp-coding-shift-jis-encode-string s)))
           (length s)))
         (decode-mbsec
          (nelisp-coding-bench-stress-test--measure-mb-sec
           (lambda () (nelisp-coding-shift-jis-decode encoded))
           (length encoded)))
         (rt-mbsec (/ 2.0 (+ (/ 1.0 encode-mbsec) (/ 1.0 decode-mbsec)))))
    (nelisp-coding-bench-stress-test--report-throughput
     "sjis-rt" rt-mbsec 100.0 0.10)))

(ert-deftest nelisp-coding-bench-throughput-tier-b-euc-jp-100mb ()
  "Doc 31 v2 §5.2 tier-B: EUC-JP round-trip ≥ 100 MB/sec ± 10%.
Heavy (100MB Japanese) — gated on NELISP_HEAVY_TESTS=1."
  (nelisp-coding-bench-stress-test--skip-unless-heavy)
  (let* ((n-chars (/ (* 100 1024 1024) 2))
         (s       (nelisp-coding-bench-stress-test--make-japanese-payload n-chars))
         (encoded nil)
         (encode-fn (lambda ()
                      (let ((res (nelisp-coding-euc-jp-encode s)))
                        (setq encoded
                              (apply #'unibyte-string
                                     (plist-get res :bytes))))))
         (encode-mbsec
          (nelisp-coding-bench-stress-test--measure-mb-sec
           encode-fn (length s)))
         (decode-mbsec
          (nelisp-coding-bench-stress-test--measure-mb-sec
           (lambda () (nelisp-coding-euc-jp-decode encoded))
           (length encoded)))
         (rt-mbsec (/ 2.0 (+ (/ 1.0 encode-mbsec) (/ 1.0 decode-mbsec)))))
    (nelisp-coding-bench-stress-test--report-throughput
     "eucjp-rt" rt-mbsec 100.0 0.10)))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; 6. stream-1gb-utf8 — OOM-free + < 10 sec
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-bench-stream-1gb-utf8-oom-free ()
  "Doc 31 v2 §5.2: stream-1gb-utf8 = 1GB synthetic UTF-8 file decode,
OOM 無し + completion < 10sec.  Heavy — NELISP_HEAVY_TESTS=1.

Approach: write a 1GB UTF-8 file in 16MB chunks (so the host buffer
never holds >16MB), then drive `nelisp-coding-read-file-with-encoding'
which is a true streaming reader (T67).  We assert:
  - decode completes (no OOM signal)
  - completion < 10 seconds wall (host-machine sensitive — only enforced
    on NELISP_PINNED_HOST=1; otherwise reported)
  - chars-emitted matches the expected codepoint count
  - replacements = 0 (= valid UTF-8 in, valid out)"
  (nelisp-coding-bench-stress-test--skip-unless-heavy)
  (let* ((tmp (make-temp-file "nelisp-coding-1gb-" nil ".bin"))
         (target-bytes (* 1024 1024 1024))   ; 1 GB
         (chunk-chars (* 4 1024 1024))        ; 16MB-ish each chunk write
         (written-bytes 0)
         (written-chars 0))
    (unwind-protect
        (progn
          ;; Write 1GB by appending small chunks so the host never holds >16MB.
          (with-temp-file tmp
            (set-buffer-multibyte nil)
            (let ((coding-system-for-write 'no-conversion))
              (while (< written-bytes target-bytes)
                (let* ((s (nelisp-coding-bench-stress-test--make-utf8-payload
                           chunk-chars))
                       (b (nelisp-coding-utf8-encode-string s)))
                  (insert b)
                  (cl-incf written-bytes (length b))
                  (cl-incf written-chars (length s))))))
          ;; Drive the streaming reader — T67's chunked path.  64KB default
          ;; chunk → ~16384 chunks for 1GB.
          (garbage-collect)
          (let* ((start (current-time))
                 (result (nelisp-coding-read-file-with-encoding
                          tmp 'utf-8 'replace
                          (* 64 1024)))
                 (elapsed (float-time (time-subtract (current-time) start))))
            (message "[bench stream-1gb-utf8] %.2f sec wall, %d chars, %d invalid"
                     elapsed
                     (plist-get result :chars-emitted)
                     (length (plist-get result :invalid-positions)))
            (should (= (plist-get result :replacements) 0))
            (should (= (plist-get result :chars-emitted) written-chars))
            (when (nelisp-coding-bench-stress-test--pinned-host-p)
              (should (< elapsed 10.0)))))
      (delete-file tmp))))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; 7. chunk-boundary-stress smoke (1K iter, always-on)
;;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-coding-bench-stress-test--random-bytes (n payload-bytes)
  "Pick a random contiguous N-byte slice of PAYLOAD-BYTES (a unibyte string).
Returns (start . substring)."
  (let* ((max-start (- (length payload-bytes) n))
         (start (if (<= max-start 0) 0 (random max-start))))
    (cons start (substring payload-bytes start (+ start n)))))

(defun nelisp-coding-bench-stress-test--split-at-random-boundaries
    (bytes max-chunk)
  "Split BYTES (unibyte string) into a random list of chunks each
≤ MAX-CHUNK bytes.  Boundaries are picked uniformly so multi-byte
sequences split at arbitrary internal positions — exactly the input
the streaming codec is required to handle.  Returns a list of unibyte
strings whose concatenation equals BYTES."
  (let ((n (length bytes))
        (i 0)
        (out '()))
    (while (< i n)
      (let* ((remaining (- n i))
             (size (1+ (random (min max-chunk remaining)))))
        (push (substring bytes i (+ i size)) out)
        (setq i (+ i size))))
    (nreverse out)))

(defun nelisp-coding-bench-stress-test--stream-decode-chunks
    (encoding chunks &optional strategy)
  "Run STREAM-DECODE-CHUNK over CHUNKS list, finalize, return result plist."
  (let ((state (nelisp-coding-stream-state-create
                encoding 'decode (or strategy 'replace))))
    (dolist (chunk chunks)
      (nelisp-coding-stream-decode-chunk state chunk))
    (nelisp-coding-stream-decode-finalize state)))

(defun nelisp-coding-bench-stress-test--run-chunk-boundary-stress
    (n-iter payload-len max-chunk)
  "Run N-ITER random chunk-split round-trips on a fresh UTF-8 payload
and return the count of mismatches between streaming and one-shot
decode results.  Each iteration picks a random window of PAYLOAD-LEN
bytes from a pre-built source, splits at random boundaries up to
MAX-CHUNK bytes, and asserts streaming-decode == one-shot-decode."
  (let* ((source (nelisp-coding-utf8-encode-string
                  (nelisp-coding-bench-stress-test--make-utf8-payload
                   (* payload-len 2))))
         (mismatches 0)
         (i 0))
    (while (< i n-iter)
      (let* ((slice (cdr (nelisp-coding-bench-stress-test--random-bytes
                          payload-len source)))
             (one-shot (plist-get (nelisp-coding-utf8-decode slice) :string))
             (chunks (nelisp-coding-bench-stress-test--split-at-random-boundaries
                      slice max-chunk))
             (streamed (plist-get
                        (nelisp-coding-bench-stress-test--stream-decode-chunks
                         'utf-8 chunks 'replace)
                        :string)))
        (unless (equal one-shot streamed)
          (cl-incf mismatches)))
      (setq i (1+ i)))
    mismatches))

(ert-deftest nelisp-coding-bench-chunk-boundary-stress-1k-iter-smoke ()
  "Light-weight chunk-boundary stress (500 iter, 128B slices, ≤16B chunks).
Always-on; mirrors the 100K-iter heavy gate for regression coverage at
a budget that keeps `make test' under a second."
  ;; Deterministic seed so flakes are reproducible across re-runs.
  (random "nelisp-coding-stress-smoke")
  (let ((mismatches
         (nelisp-coding-bench-stress-test--run-chunk-boundary-stress
          500 128 16)))
    (message "[bench chunk-boundary-stress-smoke] mismatches = %d / 500"
             mismatches)
    (should (= mismatches 0))))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; 8. chunk-boundary-stress 100K iter (heavy)
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-bench-chunk-boundary-stress-100k-iter ()
  "Doc 31 v2 §5.2: chunk-boundary-stress = 100K iter random chunk-split
encode/decode, mismatch 0 件.  Heavy — NELISP_HEAVY_TESTS=1.

Per Doc 31 v2 §6.5 mitigation: random chunk size × multi-byte 全境界
位置 を 100K iter.  本実装 は payload = 256B で chunk は 1〜16 byte
(= maximum boundary density per byte processed) — pure-Elisp encode/
decode の N-squared cost を踏まえ、chunk 数 / iter を密にし byte 数
を絞る方が境界 case 网羅性 が高い。Phase 7.1 native baseline 後は
payload を 4KB / 64-byte chunk へ拡大予定 (§6.5 mitigation tier-2)。"
  (nelisp-coding-bench-stress-test--skip-unless-heavy)
  (random "nelisp-coding-stress-100k")
  (let* ((start (current-time))
         (mismatches
          (nelisp-coding-bench-stress-test--run-chunk-boundary-stress
           100000 256 16))
         (elapsed (float-time (time-subtract (current-time) start))))
    (message "[bench chunk-boundary-stress-100k] %.2f sec, mismatches = %d / 100000"
             elapsed mismatches)
    (should (= mismatches 0))))

(provide 'nelisp-coding-bench-stress-test)

;;; nelisp-coding-bench-stress-test.el ends here
