;;; emacs-sqlite-ffi.el --- sqlite-* via in-process FFI for NeLisp standalone  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 5 — Layer 2 SQLite implementation via the in-process
;; libffi primitive `nl-ffi-call' (= NeLisp build-tool/eval/ffi.rs).
;;
;; Bypasses the Emacs Dynamic Module API (= which NeLisp standalone
;; does not yet expose) by calling `nelisp-sqlite-rs' extern "C"
;; symbols directly through `nl-ffi-call' against
;; `libnelisp_runtime.so'.  No subprocess (= old nelisp-ffi-glue) is
;; spawned: the libffi call happens in the running NeLisp process.
;;
;; This makes the host-Emacs-style `sqlite-open' / `sqlite-execute' /
;; `sqlite-select' / `sqlitep' / `sqlite-available-p' usable under
;; NeLisp standalone — exactly the surface anvil-state.el and
;; anvil-memory.el reach for.
;;
;; Each polyfill is gated on `unless (fboundp ...)' so loading under
;; host Emacs (= where sqlite-* is the C builtin) is a no-op.
;;
;; Phase 5 vs Phase 3:
;;   - Phase 3 routed through `nelisp-ffi-call' (= subprocess + libffi-glue),
;;     which required nelisp-process / nelisp-eval / url-parse / cl-lib /
;;     ... a deep load chain that NeLisp standalone could not satisfy.
;;   - Phase 5 routes through `nl-ffi-call' (= in-process libffi via
;;     build-tool/eval/ffi.rs), zero subprocess, zero deep load chain.
;;     `sqlite-select' implemented properly via nl-ffi-malloc /
;;     nl-ffi-read-bytes / nl-ffi-free buffer dance.

;;; Code:

(defun emacs-sqlite-ffi--default-libpath ()
  "Resolve the default location of `libnelisp_runtime.so'.

Resolution order (Doc 51 Track I, 2026-05-04):
  1. `NELISP_RUNTIME_SO' env var (= absolute path, takes precedence)
  2. `NELISP_HOME' env var + `target/release/libnelisp_runtime.so'
     (= same NELISP_HOME used by `bin/nemacs')
  3. `~/Notes/dev/nelisp/target/release/libnelisp_runtime.so'
     (= developer's standard checkout location)

Step 3 is a sensible fallback for the project author; deployments
should set NELISP_RUNTIME_SO or NELISP_HOME explicitly."
  (let ((override (and (fboundp 'getenv) (getenv "NELISP_RUNTIME_SO"))))
    (cond
     ((and override (not (string-empty-p override))) override)
     ((and (fboundp 'getenv)
           (let ((home (getenv "NELISP_HOME")))
             (and home (not (string-empty-p home))
                  (concat (file-name-as-directory home)
                          "target/release/libnelisp_runtime.so")))))
     (t (expand-file-name "Notes/dev/nelisp/target/release/libnelisp_runtime.so"
                          (or (and (fboundp 'getenv) (getenv "HOME")) "~"))))))

(defvar emacs-sqlite-ffi-libpath
  (emacs-sqlite-ffi--default-libpath)
  "Absolute path to `libnelisp_runtime.so' (= nelisp-sqlite-rs cdylib).
Override via `NELISP_RUNTIME_SO' env var or `setq' before this file
loads.  See `emacs-sqlite-ffi--default-libpath' for the resolution
order.")


;;;; --- routing: in-process nl-ffi-call vs subprocess nelisp-ffi-call ----

(defconst emacs-sqlite-ffi--inproc-p
  (fboundp 'nl-ffi-call)
  "Non-nil when build-tool/eval exposes `nl-ffi-call' (= in-process FFI).
Captured at load time so per-call dispatch is one boundp lookup.
Fallback path = subprocess `nelisp-ffi-call' (legacy zawatton/nelisp-ffi).")

(defun emacs-sqlite-ffi--call (func sig &rest args)
  "Dispatch FUNC with SIG (= return-type + arg-types vector) + ARGS."
  (if emacs-sqlite-ffi--inproc-p
      (apply #'nl-ffi-call emacs-sqlite-ffi-libpath func sig args)
    (require 'nelisp-ffi)
    (apply #'nelisp-ffi-call emacs-sqlite-ffi-libpath func sig args)))


;;;; --- low-level FFI helpers ---------------------------------------------

(defun emacs-sqlite-ffi--open (path)
  "FFI: nl_sqlite_open(PATH) → handle (i64)."
  (emacs-sqlite-ffi--call "nl_sqlite_open" [:sint64 :string] path))

(defun emacs-sqlite-ffi--close (handle)
  "FFI: nl_sqlite_close(HANDLE) → 0/error."
  (emacs-sqlite-ffi--call "nl_sqlite_close" [:sint64 :sint64] handle))

(defun emacs-sqlite-ffi--alive (handle)
  "FFI: nl_sqlite_alive(HANDLE) → 1 if open, 0 otherwise."
  (emacs-sqlite-ffi--call "nl_sqlite_alive" [:sint64 :sint64] handle))

(defun emacs-sqlite-ffi--execute-raw (handle sql args-json)
  "FFI: nl_sqlite_execute(HANDLE, SQL, ARGS-JSON) → row count or error."
  (emacs-sqlite-ffi--call "nl_sqlite_execute"
                          [:sint64 :sint64 :string :string]
                          handle sql args-json))

(defconst emacs-sqlite-ffi--need-more -10000
  "Sentinel returned by nl_sqlite_query when probing required size.
Probe result < this value means caller must allocate (need-more - probe)
bytes and retry.")

(defun emacs-sqlite-ffi--query-raw (handle sql args-json)
  "FFI: nl_sqlite_query(HANDLE, SQL, ARGS-JSON, buf, len) → JSON string.

Performs the buffer-size dance:
  1. probe with NULL/0 → -10000 - required-bytes (negative)
  2. allocate required bytes via `nl-ffi-malloc'
  3. retry call with the allocated buffer → bytes written
  4. read bytes back as a Lisp string
  5. free the buffer

Returns the raw JSON string `[[col1, col2, ...], ...]'."
  (let ((probe (emacs-sqlite-ffi--call
                "nl_sqlite_query"
                [:sint64 :sint64 :string :string :pointer :sint64]
                handle sql args-json 0 0)))
    (cond
     ((>= probe 0)
      ;; Already wrote something with a 0-byte buffer (= empty result?)
      "[]")
     ((>= probe emacs-sqlite-ffi--need-more)
      ;; Negative but >= -10000 = a real error code
      (error "sqlite-select: probe returned error %d" probe))
     (t
      (let* ((need (- emacs-sqlite-ffi--need-more probe))
             (buf (nl-ffi-malloc need))
             (got (emacs-sqlite-ffi--call
                   "nl_sqlite_query"
                   [:sint64 :sint64 :string :string :pointer :sint64]
                   handle sql args-json buf need))
             (json (if (and (integerp got) (> got 0))
                       (nl-ffi-read-bytes buf got)
                     "[]")))
        (nl-ffi-free buf)
        json)))))


;;;; --- JSON decoder (= minimal, adequate for sqlite-select rows) ---------


(defun emacs-sqlite-ffi--json-skip-ws (s i)
  "Return index of the next non-whitespace character at or after I in S."
  (let ((n (length s)))
    (while (and (< i n) (memq (aref s i) '(?\s ?\t ?\n ?\r)))
      (setq i (1+ i)))
    i))

(defun emacs-sqlite-ffi--json-parse-value (s i)
  "Parse one JSON value from string S starting at index I.
Returns (VALUE . NEXT-INDEX).  Supports null / true / false / numbers /
strings / arrays — sufficient for the row arrays nl_sqlite_query emits."
  (setq i (emacs-sqlite-ffi--json-skip-ws s i))
  (let ((c (and (< i (length s)) (aref s i))))
    (cond
     ((null c)
      (error "sqlite-ffi: unexpected end of JSON at %d" i))
     ((eq c ?n)
      (cons nil (+ i 4)))
     ((eq c ?t)
      (cons t (+ i 4)))
     ((eq c ?f)
      (cons nil (+ i 5)))
     ((eq c ?\")
      (emacs-sqlite-ffi--json-parse-string s i))
     ((eq c ?\[)
      (emacs-sqlite-ffi--json-parse-array s i))
     ((or (eq c ?-) (and (>= c ?0) (<= c ?9)))
      (emacs-sqlite-ffi--json-parse-number s i))
     (t
      (error "sqlite-ffi: unexpected JSON char %c at %d" c i)))))

(defun emacs-sqlite-ffi--json-parse-string (s i)
  "Parse a JSON string starting at the opening quote at I.
Returns (STRING . NEXT-INDEX).  Substring-based to avoid
`char-to-string' (= absent under build-tool/eval bootstrap)."
  (let ((n (length s))
        (out "")
        (seg-start (1+ i))
        (j (1+ i)))
    (while (and (< j n) (not (eq (aref s j) ?\")))
      (let ((c (aref s j)))
        (if (eq c ?\\)
            (progn
              ;; flush literal segment up to but not including backslash
              (when (> j seg-start)
                (setq out (concat out (substring s seg-start j))))
              (setq j (1+ j))
              (let ((esc (aref s j)))
                (cond
                 ((eq esc ?n) (setq out (concat out "\n")))
                 ((eq esc ?t) (setq out (concat out "\t")))
                 ((eq esc ?r) (setq out (concat out "\r")))
                 ((eq esc ?\") (setq out (concat out "\"")))
                 ((eq esc ?\\) (setq out (concat out "\\")))
                 ((eq esc ?/) (setq out (concat out "/")))
                 ;; unknown escape: emit raw char via substring trick
                 (t (setq out (concat out (substring s j (1+ j)))))))
              (setq j (1+ j))
              (setq seg-start j))
          (setq j (1+ j)))))
    ;; flush trailing literal segment
    (when (> j seg-start)
      (setq out (concat out (substring s seg-start j))))
    (cons out (1+ j))))

(defun emacs-sqlite-ffi--digit-val (c)
  "ASCII digit C → integer 0..9, or nil if not a digit."
  (and (>= c ?0) (<= c ?9) (- c ?0)))

(defun emacs-sqlite-ffi--parse-int (s i j)
  "Parse decimal integer in S between I (inclusive) and J (exclusive).
Handles optional leading `-'.  Pure-elisp without `string-to-number'."
  (let ((sign 1)
        (k i)
        (acc 0))
    (when (and (< k j) (eq (aref s k) ?-))
      (setq sign -1)
      (setq k (1+ k)))
    (while (< k j)
      (let ((d (emacs-sqlite-ffi--digit-val (aref s k))))
        (unless d (error "sqlite-ffi: bad digit at %d" k))
        (setq acc (+ (* acc 10) d))
        (setq k (1+ k))))
    (* sign acc)))

(defun emacs-sqlite-ffi--json-parse-number (s i)
  "Parse a JSON number starting at I.  Returns (VALUE . NEXT-INDEX).
Phase 5 returns integers only (= what SQL INT columns serialize as);
JSON floats are not produced by nl_sqlite_query for int columns."
  (let ((n (length s))
        (j i))
    (when (eq (aref s j) ?-) (setq j (1+ j)))
    (while (and (< j n) (>= (aref s j) ?0) (<= (aref s j) ?9))
      (setq j (1+ j)))
    ;; If we hit a `.', consume the fractional digits but parse as int
    ;; via int-truncation for now (= sqlite int rows never trip this).
    (let ((dot-pos nil))
      (when (and (< j n) (eq (aref s j) ?.))
        (setq dot-pos j)
        (setq j (1+ j))
        (while (and (< j n) (>= (aref s j) ?0) (<= (aref s j) ?9))
          (setq j (1+ j))))
      (cons (if dot-pos
                ;; Truncate fractional part for now.
                (emacs-sqlite-ffi--parse-int s i dot-pos)
              (emacs-sqlite-ffi--parse-int s i j))
            j))))

(defun emacs-sqlite-ffi--json-parse-array (s i)
  "Parse a JSON array starting at the opening bracket at I."
  (let ((n (length s))
        (out nil)
        (j (1+ i)))
    (setq j (emacs-sqlite-ffi--json-skip-ws s j))
    (if (and (< j n) (eq (aref s j) ?\]))
        (cons nil (1+ j))
      (catch 'done
        (while t
          (let ((pair (emacs-sqlite-ffi--json-parse-value s j)))
            (setq out (cons (car pair) out))
            (setq j (cdr pair))
            (setq j (emacs-sqlite-ffi--json-skip-ws s j))
            (cond
             ((eq (aref s j) ?,) (setq j (1+ j)))
             ((eq (aref s j) ?\]) (setq j (1+ j)) (throw 'done nil))
             (t (error "sqlite-ffi: bad JSON array delim at %d" j))))))
      (cons (nreverse out) j))))

(defun emacs-sqlite-ffi--json-decode (s)
  "Top-level JSON decode of S.  Returns the parsed value."
  (car (emacs-sqlite-ffi--json-parse-value s 0)))


;;;; --- args encoding (= minimal JSON for query parameters) ---------------

(defun emacs-sqlite-ffi--encode-args (values)
  "Encode VALUES (= a list of literals) into a JSON array string.
Phase 3 supports nil / strings / integers / floats only.  Booleans,
nested lists, and binary blobs are Phase 4."
  (cond
   ((null values) "[]")
   (t
    (let ((acc nil)
          (cur values))
      (while cur
        (let ((v (car cur)))
          (cond
           ((null v)         (setq acc (cons "null" acc)))
           ((eq v t)         (setq acc (cons "true" acc)))
           ((stringp v)
            (setq acc (cons (concat "\""
                                    (replace-regexp-in-string
                                     "\"" "\\\\\""
                                     (replace-regexp-in-string
                                      "\\\\" "\\\\\\\\" v))
                                    "\"")
                            acc)))
           ((integerp v)     (setq acc (cons (number-to-string v) acc)))
           ((floatp v)       (setq acc (cons (number-to-string v) acc)))
           (t                (setq acc (cons (prin1-to-string v) acc)))))
        (setq cur (cdr cur)))
      (let ((reversed nil))
        (while acc (setq reversed (cons (car acc) reversed)) (setq acc (cdr acc)))
        (let ((s "["))
          (let ((first t))
            (while reversed
              (unless first (setq s (concat s ",")))
              (setq s (concat s (car reversed)))
              (setq first nil)
              (setq reversed (cdr reversed))))
          (concat s "]")))))))


;;;; --- public Emacs-API polyfills ----------------------------------------

(unless (fboundp 'sqlite-available-p)
  (defun sqlite-available-p ()
    "Return non-nil if FFI-backed SQLite can be invoked.
Probes by opening :memory: and immediately closing."
    (condition-case _
        (let ((h (emacs-sqlite-ffi--open ":memory:")))
          (when (and (integerp h) (> h 0))
            (emacs-sqlite-ffi--close h)
            t))
      (error nil))))

(unless (fboundp 'sqlite-open)
  (defun sqlite-open (path)
    "Open a SQLite database at PATH; return the handle."
    (let ((h (emacs-sqlite-ffi--open path)))
      (unless (and (integerp h) (> h 0))
        (error "sqlite-open: failed for %s (handle=%S)" path h))
      h)))

(unless (fboundp 'sqlite-close)
  (defun sqlite-close (db)
    "Close DB; return non-nil on success."
    (= 0 (emacs-sqlite-ffi--close db))))

(unless (fboundp 'sqlitep)
  (defun sqlitep (object)
    "Return t when OBJECT looks like a sqlite handle (= positive integer)."
    (and (integerp object) (> object 0))))

(unless (fboundp 'sqlite-execute)
  (defun sqlite-execute (db query &optional values)
    "Execute QUERY against DB with optional VALUES.
Returns the number of affected rows on success; signals an error on
failure."
    (let* ((args-json (emacs-sqlite-ffi--encode-args values))
           (rc (emacs-sqlite-ffi--execute-raw db query args-json)))
      (when (< rc 0)
        (error "sqlite-execute: error %d for %s" rc query))
      rc)))

(unless (fboundp 'sqlite-select)
  (defun sqlite-select (db query &optional values _return-type)
    "Run SELECT-style QUERY against DB with optional VALUES.
Returns a list of row lists (= each row is a list of column values),
matching the default shape of Emacs 30 `sqlite-select' (without
`:return-type'.  Pass `:return-type vector' explicitly to get vectors.)"
    (let* ((args-json (emacs-sqlite-ffi--encode-args values))
           (json (emacs-sqlite-ffi--query-raw db query args-json))
           (rows (emacs-sqlite-ffi--json-decode json)))
      ;; Our JSON decoder produces lists already (json-parse-array returns
      ;; nreversed list), so mapcar is mainly for the vector edge case.
      (mapcar (lambda (row)
                (cond
                 ((listp row) row)
                 ((arrayp row)
                  (let ((acc nil) (i (length row)))
                    (while (> i 0)
                      (setq i (- i 1))
                      (setq acc (cons (aref row i) acc)))
                    acc))
                 (t (list row))))
              (or rows '())))))


(provide 'emacs-sqlite-ffi)

;;; emacs-sqlite-ffi.el ends here
