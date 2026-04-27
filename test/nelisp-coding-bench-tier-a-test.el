;;; nelisp-coding-bench-tier-a-test.el --- Phase 7.4 §7.2 LOCK-close tier-A bench  -*- lexical-binding: t; -*-

;; Doc 31 v2 LOCKED 2026-04-25 §7.2 — Phase 7.4 完全 close 条件の
;; *tier-A throughput ratio gate* (= Phase 7.1 baseline 比 ratio):
;;
;;   - tier-A (CI、ratio):
;;       UTF-8     encode/decode = Phase 7.1 比 5x speedup
;;       Shift-JIS encode/decode = Phase 7.1 比 3x speedup
;;       EUC-JP    encode/decode = Phase 7.1 比 3x speedup
;;
;; tier-B (pinned absolute MB/sec gate) は sibling
;; `nelisp-coding-bench-stress-test.el' で運用済。本 file は ratio gate
;; のみ責務。
;;
;; ─────────────────────────────────────────────────────────────────────
;; "Phase 7.1 baseline" の operational definition
;; ─────────────────────────────────────────────────────────────────────
;;
;; Phase 7.1 native compile が NeLisp coding hot loop を JIT 経路に
;; 引き上げた時点での *upper-bound* throughput を、本 phase では host
;; Emacs の C 組込 `encode-coding-string' / `decode-coding-string' の
;; 速度で proxy 化する (= native compile が C 速度に漸近する設計前提、
;; Doc 28 §1.3 SBCL precedent)。
;;
;; これにより:
;;   - baseline-mbsec = host Emacs C 経路の throughput (= 物理 upper-bound)
;;   - nelisp-mbsec   = nelisp-coding pure-Elisp encode/decode の throughput
;;   - ratio = nelisp-mbsec / baseline-mbsec
;;
;; Tier-A gate (post-Phase 7.1 native baseline 実装 後):
;;   ratio >= 5.0 (UTF-8) / 3.0 (SJIS / EUC) = NeLisp が C 経路の
;;   1/5 〜 1/3 以内に肉薄した状態 — 実態は "5x speedup over Phase 7.1
;;   bytecode-only baseline" (= bytecode 比 1/5〜1/3 までは C と等価) と
;;   同義 (Doc 31 §5.1 line 595 "bytecode 比 5x" 列が Phase 7.1 完遂時
;;   到達 throughput として記録)。
;;
;; ─────────────────────────────────────────────────────────────────────
;; Why ratio gate is current "warning-mode"
;; ─────────────────────────────────────────────────────────────────────
;;
;; pure-Elisp encode/decode は Phase 7.1 native compile 完成前は ~1-3
;; MB/sec (sibling stress-test の Perf reality check 参照)、host Emacs C
;; 経路は 200-400 MB/sec、ratio は ~0.005-0.015x で gate hard-fail 確実。
;; 本 file は:
;;
;;   - measure & report に徹する (常時)
;;   - hard-fail は NELISP_PHASE71_NATIVE=1 + NELISP_PINNED_HOST=1 が
;;     両立したとき *のみ* 発動 (= post-Phase 7.1 ship gating CI)
;;   - 通常開発時は stderr に "BELOW TIER-A TARGET" 行を残し、
;;     ERT は green
;;
;; tier-B file の NELISP_PINNED_HOST=1 単独 escalation と同 pattern。
;; tier-A 専用 env (NELISP_PHASE71_NATIVE=1) は Phase 7.1 native baseline
;; ship 後に CI で "1=有効化、tier-A gate 開始" を意味する gate stamp。
;;
;; ─────────────────────────────────────────────────────────────────────
;; K1 BCF (base-case-fold) caveat への対応
;; ─────────────────────────────────────────────────────────────────────
;;
;; K1 const-fold pass は compile-time constant arg を aggressively memo
;; 化する (Doc 28 base-case-fold)。本 bench が `(nelisp-coding-utf8-encode
;; "literal-string-here")' を直書すると BCF が encode 結果を fold し、
;; ratio が無限に近付き偽 PASS となる。回避:
;;
;;   - payload は *runtime に* `random' / loop で構築 (= 関数 entry 後の
;;     setq、let binding 経由で seed を変える、`(random "label")' で
;;     同じ run-id 内決定論)
;;   - encode/decode call argument は `let' bound symbol、literal な
;;     string never
;;   - measure-mb-sec は `funcall' wrap で BCF 経路から外す (= cc は
;;     funcall through pure 判定不能、fold 対象外)
;;
;; ─────────────────────────────────────────────────────────────────────
;; Heavy gate (NELISP_HEAVY_TESTS=1):
;; ─────────────────────────────────────────────────────────────────────
;;
;; Tier-A gate measurement は最低 1MB workload 必要 (= measurement noise
;; を target ratio 以下に押さえる)。`make test' 既定で 64KB smoke を回し、
;; NELISP_HEAVY_TESTS=1 で 1MB / 10MB tier-A gate measurement に拡張する。
;; pure-Elisp 1MB UTF-8 round-trip は ~60-70s wall = `make test' 既定 budget
;; を破壊するため heavy 階層に分離。
;;
;; +6 ERT (本 file):
;;
;;   1. bench-tier-a-utf8-ratio-smoke      — 64KB UTF-8 ratio probe (always-on)
;;   2. bench-tier-a-utf8-ratio            — 1MB UTF-8 ratio gate (heavy)
;;   3. bench-tier-a-shift-jis-ratio       — 1MB SJIS ratio gate (heavy)
;;   4. bench-tier-a-euc-jp-ratio          — 1MB EUC-JP ratio gate (heavy)
;;   5. bench-tier-a-utf8-ratio-10mb-heavy — 10MB UTF-8 ratio (heavy)
;;   6. bench-tier-a-japanese-ratio-10mb-heavy — 10MB SJIS+EUC ratio (heavy)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-coding)

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Env gate helpers
;;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-coding-bench-tier-a--heavy-enabled-p ()
  "Return non-nil iff env NELISP_HEAVY_TESTS=1."
  (let ((v (getenv "NELISP_HEAVY_TESTS")))
    (and v (member v '("1" "true" "yes" "y" "on")))))

(defun nelisp-coding-bench-tier-a--skip-unless-heavy ()
  "Skip current ERT unless NELISP_HEAVY_TESTS=1 is set."
  (unless (nelisp-coding-bench-tier-a--heavy-enabled-p)
    (ert-skip
     "heavy tier-A bench (set NELISP_HEAVY_TESTS=1 to enable)")))

(defun nelisp-coding-bench-tier-a--phase71-native-p ()
  "Return non-nil iff env NELISP_PHASE71_NATIVE=1.
This env stamp is set in CI *after* Phase 7.1 native compile baseline
ship — when active, tier-A ratio gate hard-fails below the target
ratio.  Pre-ship the gate is warning-mode."
  (let ((v (getenv "NELISP_PHASE71_NATIVE")))
    (and v (member v '("1" "true" "yes" "y" "on")))))

(defun nelisp-coding-bench-tier-a--pinned-host-p ()
  "Return non-nil iff env NELISP_PINNED_HOST=1.
Combined with NELISP_PHASE71_NATIVE=1, escalates tier-A gate to
hard-fail on the reference benchmark host."
  (let ((v (getenv "NELISP_PINNED_HOST")))
    (and v (member v '("1" "true" "yes" "y" "on")))))

(defun nelisp-coding-bench-tier-a--gate-binding-p ()
  "Return non-nil iff tier-A gate should hard-fail on miss.
Both NELISP_PHASE71_NATIVE=1 *and* NELISP_PINNED_HOST=1 must be set
(= we are on the reference CI host *and* Phase 7.1 native baseline
landed)."
  (and (nelisp-coding-bench-tier-a--phase71-native-p)
       (nelisp-coding-bench-tier-a--pinned-host-p)))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Runtime-variable payload generators (BCF-safe)
;;;;
;;;; All builders take N-CHARS as a runtime argument.  Each iteration
;;;; uses (mod i K) so the per-character codepoint is computed inside the
;;;; loop body rather than baked into a literal string.  K1 BCF cannot
;;;; const-fold a `while' loop with mutating index, so the encode/decode
;;;; call sites receive a fresh string per ERT run.
;;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-coding-bench-tier-a--make-utf8-payload (n-chars seed)
  "Build a UTF-8 payload of N-CHARS codepoints, seeded by SEED string.
SEED is fed to `random' so the payload differs across ERT runs but is
reproducible for a given seed.  Mix of 1/2/3/4-byte codepoints exercises
every UTF-8 branch.  Returns a multibyte string."
  (random seed)
  (let ((out (make-string n-chars #x1F300))
        (i 0))
    (while (< i n-chars)
      (let ((bucket (mod (+ i (random 7)) 20)))
        (aset out i
              (cond
               ((< bucket 10) (+ ?A (mod (+ i (random 3)) 26)))
               ((< bucket 14) (+ #x00C0 (mod (+ i (random 5)) 64)))
               ((< bucket 19) (+ #x3040 (mod (+ i (random 11)) 96)))
               (t             (+ #x1F300 (mod (+ i (random 13)) 256))))))
      (setq i (1+ i)))
    out))

(defun nelisp-coding-bench-tier-a--make-japanese-payload (n-chars seed)
  "Build a CJK payload of N-CHARS codepoints, seeded by SEED.
Mix of ASCII / hiragana / katakana / common kanji chosen to round-trip
cleanly under both Shift-JIS (CP932) and EUC-JP."
  (random seed)
  (let ((out (make-string n-chars #x4E00))
        (i 0))
    (while (< i n-chars)
      (let ((bucket (mod (+ i (random 5)) 4)))
        (aset out i
              (cond
               ((= bucket 0) (+ ?A (mod (+ i (random 3)) 26)))
               ((= bucket 1) (+ #x3041 (mod (+ i (random 7)) 83)))
               ((= bucket 2) (+ #x30A1 (mod (+ i (random 11)) 90)))
               (t            (+ #x4E00 (mod (+ i (random 13)) 64))))))
      (setq i (1+ i)))
    out))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Throughput measurement
;;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-coding-bench-tier-a--measure-mb-sec (work-fn n-bytes)
  "Run WORK-FN once, return throughput in MB/sec from N-BYTES.
Garbage-collect first so allocation noise doesn't leak into the timer.
Wraps in `funcall' indirection so K1 BCF cannot inline the encode call
through the bench frame."
  (garbage-collect)
  (let* ((start (current-time))
         (_ (funcall work-fn))
         (elapsed (float-time (time-subtract (current-time) start)))
         (mb (/ (float n-bytes) (* 1024.0 1024.0))))
    (if (zerop elapsed)
        most-positive-fixnum
      (/ mb elapsed))))

(defun nelisp-coding-bench-tier-a--report-ratio
    (label nelisp-mbsec baseline-mbsec target-ratio)
  "Report tier-A ratio for LABEL.
NELISP-MBSEC = NeLisp Phase 7.4 measured throughput.
BASELINE-MBSEC = Phase 7.1 baseline (host Emacs C proxy) throughput.
TARGET-RATIO = required ratio (5.0 for UTF-8, 3.0 for SJIS/EUC).

Always emits stderr line.  Hard-fails only when both
NELISP_PHASE71_NATIVE=1 *and* NELISP_PINNED_HOST=1 are set."
  (let* ((ratio (if (zerop baseline-mbsec)
                    most-positive-fixnum
                  (/ nelisp-mbsec baseline-mbsec)))
         (status (cond
                  ((>= ratio target-ratio) "WITHIN TIER-A")
                  (t "BELOW TIER-A TARGET"))))
    (message
     "[bench tier-a %s] nelisp = %.3f MB/sec, baseline = %.3f MB/sec, ratio = %.4f (target %.1fx, %s)"
     label nelisp-mbsec baseline-mbsec ratio target-ratio status)
    (when (nelisp-coding-bench-tier-a--gate-binding-p)
      (should (>= ratio target-ratio)))))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Baseline reference: host Emacs C-coded encode/decode
;;;;
;;;; These wrappers stand in as the "Phase 7.1 native compile baseline"
;;;; upper bound.  Returns plist same shape as nelisp-coding for symmetry.
;;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-coding-bench-tier-a--baseline-utf8-encode (s)
  "Phase 7.1 baseline proxy: encode S to UTF-8 bytes via host Emacs C."
  (encode-coding-string s 'utf-8 t))

(defun nelisp-coding-bench-tier-a--baseline-utf8-decode (b)
  "Phase 7.1 baseline proxy: decode B (unibyte) from UTF-8 via host Emacs C."
  (decode-coding-string b 'utf-8 t))

(defun nelisp-coding-bench-tier-a--baseline-shift-jis-encode (s)
  "Phase 7.1 baseline proxy: encode S to Shift-JIS bytes via host Emacs C."
  (encode-coding-string s 'shift_jis t))

(defun nelisp-coding-bench-tier-a--baseline-shift-jis-decode (b)
  "Phase 7.1 baseline proxy: decode B (unibyte) from Shift-JIS via host."
  (decode-coding-string b 'shift_jis t))

(defun nelisp-coding-bench-tier-a--baseline-euc-jp-encode (s)
  "Phase 7.1 baseline proxy: encode S to EUC-JP bytes via host Emacs C."
  (encode-coding-string s 'euc-jp t))

(defun nelisp-coding-bench-tier-a--baseline-euc-jp-decode (b)
  "Phase 7.1 baseline proxy: decode B (unibyte) from EUC-JP via host."
  (decode-coding-string b 'euc-jp t))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; Round-trip throughput helpers
;;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-coding-bench-tier-a--rt-mbsec (encode-mbsec decode-mbsec)
  "Harmonic mean of ENCODE-MBSEC + DECODE-MBSEC = round-trip MB/sec."
  (if (or (zerop encode-mbsec) (zerop decode-mbsec))
      0.0
    (/ 2.0 (+ (/ 1.0 encode-mbsec) (/ 1.0 decode-mbsec)))))

(defun nelisp-coding-bench-tier-a--measure-utf8-pair (s)
  "Measure NeLisp + baseline round-trip MB/sec for UTF-8 payload S.
Returns (NELISP-RT . BASELINE-RT)."
  (let* ((nelisp-encoded nil)
         (baseline-encoded nil)
         (nelisp-enc-mbsec
          (nelisp-coding-bench-tier-a--measure-mb-sec
           (lambda ()
             (setq nelisp-encoded (nelisp-coding-utf8-encode-string s)))
           (length s)))
         (nelisp-dec-mbsec
          (nelisp-coding-bench-tier-a--measure-mb-sec
           (lambda () (nelisp-coding-utf8-decode nelisp-encoded))
           (length nelisp-encoded)))
         (baseline-enc-mbsec
          (nelisp-coding-bench-tier-a--measure-mb-sec
           (lambda ()
             (setq baseline-encoded
                   (nelisp-coding-bench-tier-a--baseline-utf8-encode s)))
           (length s)))
         (baseline-dec-mbsec
          (nelisp-coding-bench-tier-a--measure-mb-sec
           (lambda ()
             (nelisp-coding-bench-tier-a--baseline-utf8-decode
              baseline-encoded))
           (length baseline-encoded))))
    (cons (nelisp-coding-bench-tier-a--rt-mbsec
           nelisp-enc-mbsec nelisp-dec-mbsec)
          (nelisp-coding-bench-tier-a--rt-mbsec
           baseline-enc-mbsec baseline-dec-mbsec))))

(defun nelisp-coding-bench-tier-a--measure-shift-jis-pair (s)
  "Measure NeLisp + baseline round-trip MB/sec for SJIS payload S.
Returns (NELISP-RT . BASELINE-RT)."
  (let* ((nelisp-encoded nil)
         (baseline-encoded nil)
         (nelisp-enc-mbsec
          (nelisp-coding-bench-tier-a--measure-mb-sec
           (lambda ()
             (setq nelisp-encoded (nelisp-coding-shift-jis-encode-string s)))
           (length s)))
         (nelisp-dec-mbsec
          (nelisp-coding-bench-tier-a--measure-mb-sec
           (lambda () (nelisp-coding-shift-jis-decode nelisp-encoded))
           (length nelisp-encoded)))
         (baseline-enc-mbsec
          (nelisp-coding-bench-tier-a--measure-mb-sec
           (lambda ()
             (setq baseline-encoded
                   (nelisp-coding-bench-tier-a--baseline-shift-jis-encode s)))
           (length s)))
         (baseline-dec-mbsec
          (nelisp-coding-bench-tier-a--measure-mb-sec
           (lambda ()
             (nelisp-coding-bench-tier-a--baseline-shift-jis-decode
              baseline-encoded))
           (length baseline-encoded))))
    (cons (nelisp-coding-bench-tier-a--rt-mbsec
           nelisp-enc-mbsec nelisp-dec-mbsec)
          (nelisp-coding-bench-tier-a--rt-mbsec
           baseline-enc-mbsec baseline-dec-mbsec))))

(defun nelisp-coding-bench-tier-a--measure-euc-jp-pair (s)
  "Measure NeLisp + baseline round-trip MB/sec for EUC-JP payload S.
Returns (NELISP-RT . BASELINE-RT)."
  (let* ((nelisp-encoded nil)
         (baseline-encoded nil)
         (nelisp-encode-fn
          (lambda ()
            (let ((res (nelisp-coding-euc-jp-encode s)))
              (setq nelisp-encoded
                    (apply #'unibyte-string (plist-get res :bytes))))))
         (nelisp-enc-mbsec
          (nelisp-coding-bench-tier-a--measure-mb-sec
           nelisp-encode-fn (length s)))
         (nelisp-dec-mbsec
          (nelisp-coding-bench-tier-a--measure-mb-sec
           (lambda () (nelisp-coding-euc-jp-decode nelisp-encoded))
           (length nelisp-encoded)))
         (baseline-enc-mbsec
          (nelisp-coding-bench-tier-a--measure-mb-sec
           (lambda ()
             (setq baseline-encoded
                   (nelisp-coding-bench-tier-a--baseline-euc-jp-encode s)))
           (length s)))
         (baseline-dec-mbsec
          (nelisp-coding-bench-tier-a--measure-mb-sec
           (lambda ()
             (nelisp-coding-bench-tier-a--baseline-euc-jp-decode
              baseline-encoded))
           (length baseline-encoded))))
    (cons (nelisp-coding-bench-tier-a--rt-mbsec
           nelisp-enc-mbsec nelisp-dec-mbsec)
          (nelisp-coding-bench-tier-a--rt-mbsec
           baseline-enc-mbsec baseline-dec-mbsec))))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; 1. UTF-8 ratio smoke (always-on, 64KB)
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-bench-tier-a-utf8-ratio-smoke ()
  "Light-weight UTF-8 tier-A ratio probe.
Always-on; runtime-built 64KB UTF-8 payload, measures NeLisp vs baseline
round-trip throughput, reports ratio.  No hard gate (probe shape only).

K1 BCF safety: payload built via runtime `random' loop, encode/decode
called through `funcall' wrappers — never const-foldable."
  (let* ((n-chars (/ (* 64 1024) 4))
         (s (nelisp-coding-bench-tier-a--make-utf8-payload
             n-chars "tier-a-utf8-smoke"))
         (pair (nelisp-coding-bench-tier-a--measure-utf8-pair s))
         (nelisp-rt (car pair))
         (baseline-rt (cdr pair)))
    (should (> nelisp-rt 0.0))
    (should (> baseline-rt 0.0))
    (message
     "[bench tier-a utf8-smoke] nelisp = %.3f MB/sec, baseline = %.3f MB/sec, ratio = %.4f"
     nelisp-rt baseline-rt (/ nelisp-rt baseline-rt))))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; 2. UTF-8 ratio gate (always-on, 1MB)
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-bench-tier-a-utf8-ratio ()
  "Doc 31 v2 §7.2 tier-A: UTF-8 round-trip ≥ 5x Phase 7.1 baseline.
Heavy (1MB workload, ~60-70s wall pre-Phase 7.1 native) — gated on
NELISP_HEAVY_TESTS=1 to keep `make test' default fast.

Gate behavior:
  - Always measures + reports ratio to stderr (when not skipped)
  - Hard-fails only when NELISP_PHASE71_NATIVE=1 + NELISP_PINNED_HOST=1
    (= post-ship gating CI on the reference host)
  - Pre-Phase 7.1 native baseline ship: ratio ~0.005-0.015x (pure-Elisp
    vs C builtin), gate is informational

K1 BCF safety: payload runtime-built via seeded `random' + `aset' loop
mutating index, encode/decode via `funcall' so the cc pipeline cannot
fold the call sites."
  (nelisp-coding-bench-tier-a--skip-unless-heavy)
  (let* ((n-chars (/ (* 1 1024 1024) 4))
         (s (nelisp-coding-bench-tier-a--make-utf8-payload
             n-chars "tier-a-utf8-1mb"))
         (pair (nelisp-coding-bench-tier-a--measure-utf8-pair s))
         (nelisp-rt (car pair))
         (baseline-rt (cdr pair)))
    (nelisp-coding-bench-tier-a--report-ratio
     "utf8-rt-1mb" nelisp-rt baseline-rt 5.0)))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; 3. Shift-JIS ratio gate (always-on, 1MB)
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-bench-tier-a-shift-jis-ratio ()
  "Doc 31 v2 §7.2 tier-A: Shift-JIS round-trip ≥ 3x Phase 7.1 baseline.
Heavy (1MB CJK workload, ~50s wall pre-Phase 7.1 native) — gated on
NELISP_HEAVY_TESTS=1.

Same gate behavior as UTF-8 ratio: report always when run, hard-fail
only on (NELISP_PHASE71_NATIVE=1 + NELISP_PINNED_HOST=1).

K1 BCF safety: see UTF-8 sibling docstring."
  (nelisp-coding-bench-tier-a--skip-unless-heavy)
  (let* ((n-chars (/ (* 1 1024 1024) 2))
         (s (nelisp-coding-bench-tier-a--make-japanese-payload
             n-chars "tier-a-sjis-1mb"))
         (pair (nelisp-coding-bench-tier-a--measure-shift-jis-pair s))
         (nelisp-rt (car pair))
         (baseline-rt (cdr pair)))
    (nelisp-coding-bench-tier-a--report-ratio
     "sjis-rt-1mb" nelisp-rt baseline-rt 3.0)))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; 4. EUC-JP ratio gate (always-on, 1MB)
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-bench-tier-a-euc-jp-ratio ()
  "Doc 31 v2 §7.2 tier-A: EUC-JP round-trip ≥ 3x Phase 7.1 baseline.
Heavy (1MB CJK workload, ~60s wall pre-Phase 7.1 native) — gated on
NELISP_HEAVY_TESTS=1.

Same gate behavior as Shift-JIS sibling.

K1 BCF safety: see UTF-8 sibling docstring."
  (nelisp-coding-bench-tier-a--skip-unless-heavy)
  (let* ((n-chars (/ (* 1 1024 1024) 2))
         (s (nelisp-coding-bench-tier-a--make-japanese-payload
             n-chars "tier-a-eucjp-1mb"))
         (pair (nelisp-coding-bench-tier-a--measure-euc-jp-pair s))
         (nelisp-rt (car pair))
         (baseline-rt (cdr pair)))
    (nelisp-coding-bench-tier-a--report-ratio
     "eucjp-rt-1mb" nelisp-rt baseline-rt 3.0)))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; 5. UTF-8 ratio 10MB heavy
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-bench-tier-a-utf8-ratio-10mb-heavy ()
  "Doc 31 v2 §7.2 tier-A heavy: UTF-8 round-trip ≥ 5x baseline at 10MB.
Heavy workload (10MB) — gated on NELISP_HEAVY_TESTS=1 to keep the
default `make test' fast.  Hard gate same as 1MB sibling."
  (nelisp-coding-bench-tier-a--skip-unless-heavy)
  (let* ((n-chars (/ (* 10 1024 1024) 4))
         (s (nelisp-coding-bench-tier-a--make-utf8-payload
             n-chars "tier-a-utf8-10mb"))
         (pair (nelisp-coding-bench-tier-a--measure-utf8-pair s))
         (nelisp-rt (car pair))
         (baseline-rt (cdr pair)))
    (nelisp-coding-bench-tier-a--report-ratio
     "utf8-rt-10mb" nelisp-rt baseline-rt 5.0)))

;;;; ─────────────────────────────────────────────────────────────────────
;;;; 6. Japanese ratio 10MB heavy (covers SJIS + EUC in single test)
;;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-coding-bench-tier-a-japanese-ratio-10mb-heavy ()
  "Doc 31 v2 §7.2 tier-A heavy: Shift-JIS + EUC-JP ≥ 3x baseline at 10MB.
Heavy workload (10MB Japanese for both encodings) — gated on
NELISP_HEAVY_TESTS=1.  Reuses one payload for both codecs to halve build
cost; reports both ratios independently."
  (nelisp-coding-bench-tier-a--skip-unless-heavy)
  (let* ((n-chars (/ (* 10 1024 1024) 2))
         (s (nelisp-coding-bench-tier-a--make-japanese-payload
             n-chars "tier-a-japanese-10mb"))
         (sjis-pair (nelisp-coding-bench-tier-a--measure-shift-jis-pair s))
         (euc-pair (nelisp-coding-bench-tier-a--measure-euc-jp-pair s)))
    (nelisp-coding-bench-tier-a--report-ratio
     "sjis-rt-10mb" (car sjis-pair) (cdr sjis-pair) 3.0)
    (nelisp-coding-bench-tier-a--report-ratio
     "eucjp-rt-10mb" (car euc-pair) (cdr euc-pair) 3.0)))

(provide 'nelisp-coding-bench-tier-a-test)

;;; nelisp-coding-bench-tier-a-test.el ends here
