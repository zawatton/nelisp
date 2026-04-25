;;; emacs-tui-event.el --- Phase 2 TUI event source (stdin parser + SIGWINCH)  -*- lexical-binding: t; -*-

;; Phase 2 module per nelisp-emacs Doc 01 (LOCKED-2026-04-25-v2 §3.2),
;; mirroring NeLisp Doc 43 v2 §3.1 Phase 11.A TUI MVP — sibling of
;; `emacs-tui-backend.el' (T148 SHIPPED).
;; Layer: nelisp-emacs (Layer 2/3 extension on top of NeLisp).
;; Namespace: `emacs-tui-event-' so loading inside a host Emacs does
;; NOT shadow any `tui-' or `event-' symbol.
;;
;; Foundation contracts (LOCKED):
;;   - Doc 34 v2 §2.4 EVENT_SOURCE_CONTRACT_VERSION = 1 producer
;;     (= this module is a *producer* registered with the eventloop;
;;     consumers are command-loop / `read-event' / `read-key').
;;   - Doc 43 v2 §2.6 — Phase 11.A TUI backend declares two event
;;     sources: `tui-keyboard' (stdin select) + `tui-resize'
;;     (SIGWINCH).  This module provides both producers as Elisp
;;     reference impl, with a pull-on-demand contract: `event-poll'
;;     returns nil immediately on empty queue and only sleeps when an
;;     explicit TIMEOUT-MS argument is supplied.
;;
;; Role in the architecture:
;;   - `emacs-tui-backend.el' exposes the dispatch surface
;;     (`event-inject') and the canvas / cursor / capability APIs but
;;     does *not* read stdin or trap SIGWINCH.  This module is the
;;     producer side: it parses raw byte streams from a tty into
;;     structured key events and observes terminal resize via the
;;     `window-size-change-functions' hook (the in-process Elisp
;;     analogue of SIGWINCH).
;;   - The two sides talk through a thin handle struct.  An init call
;;     allocates a handle, optionally bound to an INPUT-FD; subsequent
;;     `event-poll' calls drain (a) the parsed-event queue, then (b)
;;     read more bytes from the input fd if any are available.
;;   - For ERT the input source is parameterised via
;;     `emacs-tui-event-input-fn' (function returning the next byte or
;;     nil) so tests can script keystrokes deterministically without
;;     touching a real fd.
;;
;; Event format (consistent with `emacs-tui-backend-event-inject'):
;;
;;   (:type 'key
;;    :name SYMBOL                  ;; e.g. up / down / f1 / ?a / tab
;;    :modifiers (C M S))           ;; subset of (control meta shift),
;;                                  ;; in canonical alphabetic order
;;
;;   (:type 'resize :width W :height H)
;;
;; API surface (~13 public APIs):
;;
;;   A. lifecycle (3 APIs)
;;      emacs-tui-event-init               — allocate a handle
;;      emacs-tui-event-shutdown           — release a handle
;;      emacs-tui-event-handlep            — predicate
;;
;;   B. parser primitives (4 APIs)
;;      emacs-tui-event-parse-byte-stream  — bytes → list of events
;;      emacs-tui-event-encode-key-event   — symbol + mods → event
;;      emacs-tui-event-decode-csi         — CSI seq → key-event
;;      emacs-tui-event-pending-event-p    — queue non-empty?
;;
;;   C. polling (1 API)
;;      emacs-tui-event-poll               — pull next event (Doc 43 §2.6)
;;
;;   D. SIGWINCH / resize (3 APIs)
;;      emacs-tui-event-install-sigwinch   — register callback
;;      emacs-tui-event-uninstall-sigwinch — unregister
;;      emacs-tui-event-current-window-size — query (W H) tuple
;;
;;   E. test / bridge helpers (2 APIs)
;;      emacs-tui-event-feed-bytes         — push raw bytes into a handle
;;      emacs-tui-event-dispatch-resize    — fire the SIGWINCH callback
;;
;; Non-goals (per Doc 43 §3.1 Phase 11.A scope):
;;   - mouse event (= optional Phase 11.A v2.x)
;;   - bracketed paste (= separate Phase 11.A v2.x sub-task)
;;   - terminfo capability detection (= `emacs-tui-terminfo.el',
;;     separate Phase 2 module).
;;   - real-fd select(2) integration (= performed by the integration
;;     glue in `emacs-frame.el' Phase 11.A wiring; this module
;;     provides a pluggable input function so the glue can pass in
;;     `process-filter' bytes or `read-from-minibuffer' bytes).

;;; Code:

(require 'cl-lib)

;;; Errors

(define-error 'emacs-tui-event-error
  "emacs-tui-event error")

(define-error 'emacs-tui-event-bad-handle
  "Not an emacs-tui-event handle"
  'emacs-tui-event-error)

(define-error 'emacs-tui-event-bad-sequence
  "Malformed escape sequence"
  'emacs-tui-event-error)

;;; Contract version constants

(defconst emacs-tui-event-source-contract-version 1
  "EVENT_SOURCE_CONTRACT_VERSION per Doc 34 v2 §2.4 + Doc 43 v2 §2.6.
This module is a *producer* of that contract (= keyboard + resize
sources for the TUI MVP backend).")

(defconst emacs-tui-event-default-window-width 80
  "Default window width when no SIGWINCH / hook has fired yet.
Matches `emacs-tui-backend-frame-default-width' for swap-in compat.")

(defconst emacs-tui-event-default-window-height 24
  "Default window height (matches `emacs-tui-backend-frame-default-height').")

;;; Customization

(defgroup emacs-tui-event nil
  "Phase 2 TUI event source (stdin parser + SIGWINCH)."
  :group 'emacs)

(defcustom emacs-tui-event-input-fn nil
  "Function called with no args returning the next input byte, or nil.
When nil (the default), `emacs-tui-event-poll' only drains the
in-memory parsed-event queue — it does *not* attempt to read from a
real fd.  ERT sets this to a list-popping closure to script
keystrokes deterministically without touching a real terminal.

The function MUST return either an integer in [0..255] (= the next
byte) or nil (= no more bytes available right now).  Returning nil
short-circuits the read pump so the poll falls through to its
optional TIMEOUT-MS sleep loop."
  :type '(choice (const :tag "Default (no fd read)" nil)
                 function)
  :group 'emacs-tui-event)

(defcustom emacs-tui-event-log-enabled nil
  "When non-nil, append a one-line log entry per parser op.
The log buffer is `*emacs-tui-event-log*' and is created lazily.
Default nil keeps ERT runs silent."
  :type 'boolean
  :group 'emacs-tui-event)

;;; Logging

(defun emacs-tui-event--log (fmt &rest args)
  "Append a log entry to `*emacs-tui-event-log*' if logging enabled.
FMT and ARGS are passed straight to `format'."
  (when emacs-tui-event-log-enabled
    (let ((buf (get-buffer-create "*emacs-tui-event-log*")))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (apply #'format fmt args) "\n")))))

;;; Handle struct

(cl-defstruct (emacs-tui-event-handle
               (:constructor emacs-tui-event--make-handle)
               (:copier nil)
               (:predicate emacs-tui-event-handlep))
  "Opaque event-source handle returned by `emacs-tui-event-init'."
  (id            nil :read-only t)        ;; gensym id, e.g. `tev-1'
  (alive-p       t)                       ;; nil after shutdown
  (input-fd      nil :read-only t)        ;; reserved for future select(2)
  (input-buffer  nil)                     ;; partial-byte accumulator (string)
  (event-queue   nil)                     ;; FIFO list of parsed events
  (window-width  nil)                     ;; current width, may be nil
  (window-height nil)                     ;; current height, may be nil
  (sigwinch-cb   nil))                    ;; resize callback or nil

(defvar emacs-tui-event--handle-counter 0
  "Monotonic counter for handle ids (printable as `tev-N').")

(defun emacs-tui-event--check-handle (handle)
  "Signal `emacs-tui-event-bad-handle' unless HANDLE is alive."
  (unless (emacs-tui-event-handlep handle)
    (signal 'emacs-tui-event-bad-handle (list handle)))
  (unless (emacs-tui-event-handle-alive-p handle)
    (signal 'emacs-tui-event-bad-handle
            (list 'shutdown (emacs-tui-event-handle-id handle)))))

;;; A. lifecycle

;;;###autoload
(defun emacs-tui-event-init (&optional input-fd)
  "Initialize a fresh event-source handle and return it.
INPUT-FD is reserved for a future select(2) integration; the
current MVP reads bytes via `emacs-tui-event-input-fn' when set,
and otherwise relies on `emacs-tui-event-feed-bytes' for input.
INPUT-FD is stored on the handle but never dereferenced here.

Returns an `emacs-tui-event-handle' satisfying
`emacs-tui-event-handlep'."
  (let* ((counter (cl-incf emacs-tui-event--handle-counter))
         (id (intern (format "tev-%d" counter)))
         (handle (emacs-tui-event--make-handle
                  :id id
                  :alive-p t
                  :input-fd input-fd
                  :input-buffer ""
                  :event-queue nil
                  :window-width nil
                  :window-height nil
                  :sigwinch-cb nil)))
    (emacs-tui-event--log "init handle=%S input-fd=%S" id input-fd)
    handle))

;;;###autoload
(defun emacs-tui-event-shutdown (handle)
  "Tear down HANDLE, drop pending bytes / events / SIGWINCH callback.
Returns t.  After shutdown, any operation other than
`emacs-tui-event-handlep' on HANDLE signals
`emacs-tui-event-bad-handle'."
  (emacs-tui-event--check-handle handle)
  (emacs-tui-event--log "shutdown handle=%S queue=%d buffer=%d"
                        (emacs-tui-event-handle-id handle)
                        (length (emacs-tui-event-handle-event-queue handle))
                        (length (emacs-tui-event-handle-input-buffer handle)))
  (setf (emacs-tui-event-handle-alive-p handle)      nil
        (emacs-tui-event-handle-input-buffer handle) ""
        (emacs-tui-event-handle-event-queue handle)  nil
        (emacs-tui-event-handle-sigwinch-cb handle)  nil)
  t)

;;; CSI / key-name tables

(defconst emacs-tui-event--csi-final-table
  '((?A . up)
    (?B . down)
    (?C . right)
    (?D . left)
    (?H . home)
    (?F . end)
    (?Z . backtab))                       ;; Shift+Tab on most terminals
  "Mapping from CSI final byte to key-name symbol (no parameters).
Doc 43 §3.1 Phase 11.A scope: arrow + home/end + Shift-Tab.
Other terminal-specific finals route through
`emacs-tui-event--csi-tilde-table' (with the `~' final byte) or are
returned as-is via :name csi-FINAL fallback.")

(defconst emacs-tui-event--csi-tilde-table
  '((1  . home)        (2  . insert)      (3  . delete)
    (4  . end)         (5  . prior)       (6  . next)
    (11 . f1)          (12 . f2)          (13 . f3)
    (14 . f4)          (15 . f5)
    (17 . f6)          (18 . f7)          (19 . f8)
    (20 . f9)          (21 . f10)
    (23 . f11)         (24 . f12))
  "Mapping from CSI numeric parameter to key-name when final byte = `~'.
Covers VT100 navigation cluster (insert/delete/PgUp/PgDown) plus the
DEC F1..F12 alias range (xterm legacy + most modern emulators).")

(defconst emacs-tui-event--ss3-table
  '((?P . f1) (?Q . f2) (?R . f3) (?S . f4)
    (?A . up) (?B . down) (?C . right) (?D . left)
    (?H . home) (?F . end))
  "Mapping from SS3 final byte to key-name.
SS3 = ESC + `O' + final, used by many xterms for F1..F4 and the
application-cursor mode arrow keys.")

(defconst emacs-tui-event--csi-modifier-bits
  '((1 . shift) (2 . meta) (4 . control))
  "xterm modifier bit-field semantics for CSI parameter 2.
The transmitted parameter is the bitwise OR + 1; e.g.
`shift+control' → 1+4+1 = 6.  `meta' here is the xterm
`alt'-as-meta convention.")

;;; B. parser primitives

(defun emacs-tui-event--decode-mod-bits (param)
  "Decode xterm modifier PARAM (an integer ≥ 1) to a sorted modifier list.
Returns a sublist of (control meta shift) in canonical alphabetic
order.  PARAM = 1 → no modifiers (= empty list)."
  (let ((bits (1- param))
        (mods nil))
    (dolist (cell emacs-tui-event--csi-modifier-bits)
      (when (/= 0 (logand bits (car cell)))
        (push (cdr cell) mods)))
    (sort mods (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun emacs-tui-event-encode-key-event (key-name modifiers)
  "Build a `key' event plist from KEY-NAME and MODIFIERS.
KEY-NAME is a symbol (e.g. `up', `f1') or a character (e.g. ?a).
MODIFIERS is a list whose elements are a subset of `(control meta
shift)'.  The resulting plist always sorts modifiers in canonical
alphabetic order so equality testing in ERT is deterministic.

Returns `(:type key :name KEY-NAME :modifiers MODS)'."
  (let ((sorted (sort (copy-sequence modifiers)
                      (lambda (a b) (string< (symbol-name a) (symbol-name b))))))
    (list :type 'key :name key-name :modifiers sorted)))

(defun emacs-tui-event-decode-csi (sequence)
  "Decode SEQUENCE (a string starting with ESC `[') into a key-event.
Recognised forms:
  ESC `[' FINAL                    — bare arrow / home / end / Z
  ESC `[' PARAMS `;' MODS FINAL    — modified arrow / home / end
  ESC `[' NUM `~'                  — VT/PageUp/PageDown/Insert/Del/F-keys
  ESC `[' NUM `;' MODS `~'         — modified F-keys / nav cluster

Returns the same plist shape as `emacs-tui-event-encode-key-event',
or signals `emacs-tui-event-bad-sequence' on a SEQUENCE that does
not match any of the above forms."
  (unless (and (stringp sequence)
               (>= (length sequence) 3)
               (= (aref sequence 0) ?\e)
               (= (aref sequence 1) ?\[))
    (signal 'emacs-tui-event-bad-sequence (list 'not-csi sequence)))
  (let* ((body (substring sequence 2))
         (final (aref body (1- (length body))))
         (params-str (substring body 0 (1- (length body)))))
    (cond
     ;; CSI ~ form: NUM `~' or NUM `;' MODS `~'
     ((= final ?~)
      (let* ((parts (split-string params-str ";"))
             (num (string-to-number (car parts)))
             (mods-param (if (cdr parts)
                             (string-to-number (cadr parts))
                           1))
             (cell (assq num emacs-tui-event--csi-tilde-table)))
        (unless cell
          (signal 'emacs-tui-event-bad-sequence
                  (list 'unknown-tilde-num num sequence)))
        (emacs-tui-event-encode-key-event
         (cdr cell) (emacs-tui-event--decode-mod-bits mods-param))))
     ;; CSI letter form: FINAL or PARAMS `;' MODS FINAL
     ((assq final emacs-tui-event--csi-final-table)
      (let* ((parts (and (> (length params-str) 0)
                         (split-string params-str ";")))
             (mods-param (cond
                          ((null parts) 1)
                          ((>= (length parts) 2) (string-to-number (cadr parts)))
                          ;; Single param (rare for letter form): treat as
                          ;; modifier index per xterm modifyOtherKeys
                          (t (string-to-number (car parts)))))
             (cell (assq final emacs-tui-event--csi-final-table)))
        (emacs-tui-event-encode-key-event
         (cdr cell) (emacs-tui-event--decode-mod-bits mods-param))))
     ;; Unknown final: surface a `csi-FINAL' synthetic name so the
     ;; caller can decide whether to ignore or re-report.
     (t
      (emacs-tui-event-encode-key-event
       (intern (format "csi-%c" final))
       (emacs-tui-event--decode-mod-bits 1))))))

(defun emacs-tui-event--decode-ss3 (sequence)
  "Decode SEQUENCE (= ESC `O' FINAL) into a key-event."
  (unless (and (stringp sequence)
               (= (length sequence) 3)
               (= (aref sequence 0) ?\e)
               (= (aref sequence 1) ?O))
    (signal 'emacs-tui-event-bad-sequence (list 'not-ss3 sequence)))
  (let* ((final (aref sequence 2))
         (cell (assq final emacs-tui-event--ss3-table)))
    (if cell
        (emacs-tui-event-encode-key-event (cdr cell) nil)
      (emacs-tui-event-encode-key-event
       (intern (format "ss3-%c" final)) nil))))

(defun emacs-tui-event--control-char-name (byte)
  "Return the canonical key-name for a control BYTE (0..31), or nil.
The table covers the ASCII control range plus the `\\C-?' alias for
DEL (= 127).  Other control bytes are encoded as `(control . CHAR)'
through `emacs-tui-event--decode-control-byte'."
  (cond
   ((= byte ?\C-i) 'tab)
   ((= byte ?\C-m) 'return)
   ((= byte ?\C-j) 'linefeed)
   ((= byte ?\C-h) 'backspace)
   ((= byte 127)   'backspace)            ;; DEL is treated as backspace
   ((= byte 0)     'nul)
   (t nil)))

(defun emacs-tui-event--decode-control-byte (byte)
  "Decode a single control BYTE (0..31) into a key event.
BYTE = ?\\C-x → (:type key :name ?x :modifiers (control)) when the
byte does not have a more specific named alias (= tab / return /
backspace etc).  Named aliases come back without a control modifier
(= matches Emacs `event-modifiers' convention for `tab')."
  (let ((named (emacs-tui-event--control-char-name byte)))
    (cond
     (named
      (emacs-tui-event-encode-key-event named nil))
     ((and (>= byte 1) (<= byte 26))
      (emacs-tui-event-encode-key-event (+ byte (- ?a 1)) '(control)))
     (t
      (emacs-tui-event-encode-key-event byte '(control))))))

(defun emacs-tui-event--utf8-leading-length (byte)
  "Return how many bytes a UTF-8 char starting with BYTE occupies, or nil.
0xxxxxxx → 1, 110xxxxx → 2, 1110xxxx → 3, 11110xxx → 4.  Returns
nil for continuation bytes or invalid leaders so the caller can
treat them as control / unknown bytes."
  (cond
   ((<= byte #x7f)                         1)
   ((and (>= byte #xc2) (<= byte #xdf))    2)
   ((and (>= byte #xe0) (<= byte #xef))    3)
   ((and (>= byte #xf0) (<= byte #xf4))    4)
   (t                                      nil)))

(defun emacs-tui-event--decode-utf8-char (bytes start len)
  "Decode LEN bytes starting at START in BYTES into one Unicode code point.
BYTES is a unibyte string; LEN is 1..4.  Returns the integer code
point; signals `emacs-tui-event-bad-sequence' on continuation-byte
mismatch."
  (let ((cp 0))
    (cl-case len
      (1 (setq cp (aref bytes start)))
      (2 (let ((b1 (aref bytes start))
               (b2 (aref bytes (+ start 1))))
           (unless (= (logand b2 #xc0) #x80)
             (signal 'emacs-tui-event-bad-sequence
                     (list 'utf8-cont (substring bytes start (+ start len)))))
           (setq cp (logior (ash (logand b1 #x1f) 6)
                            (logand b2 #x3f)))))
      (3 (let ((b1 (aref bytes start))
               (b2 (aref bytes (+ start 1)))
               (b3 (aref bytes (+ start 2))))
           (unless (and (= (logand b2 #xc0) #x80)
                        (= (logand b3 #xc0) #x80))
             (signal 'emacs-tui-event-bad-sequence
                     (list 'utf8-cont (substring bytes start (+ start len)))))
           (setq cp (logior (ash (logand b1 #x0f) 12)
                            (ash (logand b2 #x3f) 6)
                            (logand b3 #x3f)))))
      (4 (let ((b1 (aref bytes start))
               (b2 (aref bytes (+ start 1)))
               (b3 (aref bytes (+ start 2)))
               (b4 (aref bytes (+ start 3))))
           (unless (and (= (logand b2 #xc0) #x80)
                        (= (logand b3 #xc0) #x80)
                        (= (logand b4 #xc0) #x80))
             (signal 'emacs-tui-event-bad-sequence
                     (list 'utf8-cont (substring bytes start (+ start len)))))
           (setq cp (logior (ash (logand b1 #x07) 18)
                            (ash (logand b2 #x3f) 12)
                            (ash (logand b3 #x3f) 6)
                            (logand b4 #x3f))))))
    cp))

(defun emacs-tui-event--csi-complete-p (str start)
  "Return non-nil iff STR has a complete CSI sequence starting at START.
A complete CSI = ESC `[' (params)* (final byte 0x40..0x7e).  When
incomplete (= waiting for more bytes), returns nil so the parser
can leave the partial sequence in the buffer for a future feed."
  (let ((i (+ start 2))
        (n (length str)))
    (catch 'done
      (while (< i n)
        (let ((b (aref str i)))
          (when (and (>= b #x40) (<= b #x7e))
            (throw 'done (1+ i))))
        (setq i (1+ i)))
      nil)))

(defun emacs-tui-event--parse-one (bytes start)
  "Parse one event from BYTES beginning at START.
Returns `(EVENT . NEXT-INDEX)' on success, or `(nil . START)' when
BYTES does not contain enough data yet (= caller buffers the
remainder for a future feed).

EVENT may be nil for a successfully-recognised but discarded byte
(e.g. a partial CSI with NEXT-INDEX past the start)."
  (let* ((n (length bytes))
         (b (aref bytes start)))
    (cond
     ;; ESC handling: ESC alone, ESC ESC, Meta-prefix, CSI, SS3
     ((= b ?\e)
      (cond
       ;; Pure ESC at end of buffer → may be more data coming or
       ;; standalone Escape press; defer.
       ((= (1+ start) n)
        (cons nil start))
       ;; ESC `[' = CSI — wait for full sequence
       ((= (aref bytes (1+ start)) ?\[)
        (let ((end (emacs-tui-event--csi-complete-p bytes start)))
          (if end
              (let ((seq (substring bytes start end)))
                (cons (emacs-tui-event-decode-csi seq) end))
            (cons nil start))))
       ;; ESC `O' = SS3
       ((= (aref bytes (1+ start)) ?O)
        (if (>= n (+ start 3))
            (let ((seq (substring bytes start (+ start 3))))
              (cons (emacs-tui-event--decode-ss3 seq) (+ start 3)))
          (cons nil start)))
       ;; ESC + printable = Meta-printable (xterm `metaSendsEscape')
       (t
        (let ((next (aref bytes (1+ start))))
          (cond
           ;; Recursive: ESC ESC X = Meta-X (X may itself need parsing)
           ((= next ?\e)
            ;; Treat second ESC as start of new event with meta-flag
            ;; pending — simplest: parse the inner event and add meta.
            (let ((inner (emacs-tui-event--parse-one bytes (1+ start))))
              (cond
               ;; Inner needs more data — defer.
               ((null (car inner)) (cons nil start))
               (t
                (let* ((ev (car inner))
                       (mods (plist-get ev :modifiers)))
                  (cons (emacs-tui-event-encode-key-event
                         (plist-get ev :name)
                         (cons 'meta mods))
                        (cdr inner)))))))
           ;; ESC + printable / control: Meta-modifier
           (t
            (let ((inner (emacs-tui-event--parse-one bytes (1+ start))))
              (cond
               ((null (car inner)) (cons nil start))
               (t
                (let ((ev (car inner)))
                  (cons (emacs-tui-event-encode-key-event
                         (plist-get ev :name)
                         (cons 'meta (plist-get ev :modifiers)))
                        (cdr inner))))))))))))
     ;; Control range
     ((< b 32)
      (cons (emacs-tui-event--decode-control-byte b) (1+ start)))
     ((= b 127)
      (cons (emacs-tui-event--decode-control-byte b) (1+ start)))
     ;; UTF-8 / printable: figure out leader length
     (t
      (let ((len (emacs-tui-event--utf8-leading-length b)))
        (cond
         ;; Invalid leader → emit raw byte as fallback
         ((null len)
          (cons (emacs-tui-event-encode-key-event b nil) (1+ start)))
         ;; Multi-byte but not enough yet → defer
         ((> (+ start len) n)
          (cons nil start))
         (t
          (let ((cp (emacs-tui-event--decode-utf8-char bytes start len)))
            (cons (emacs-tui-event-encode-key-event cp nil)
                  (+ start len))))))))))

(defun emacs-tui-event-parse-byte-stream (bytes)
  "Parse BYTES (a unibyte string) and return a list of events.
Stops at the first incomplete sequence and silently discards the
trailing bytes (= the byte-stream caller should keep a partial
buffer between calls; `emacs-tui-event-feed-bytes' handles that for
handle-driven flows).  Use this primitive when you have a complete
known-good byte sequence and want a list of events back.

Returns a list of event plists in order."
  (unless (stringp bytes)
    (signal 'wrong-type-argument (list 'stringp bytes)))
  (let ((events nil)
        (idx 0)
        (n (length bytes)))
    (while (< idx n)
      (let ((parsed (emacs-tui-event--parse-one bytes idx)))
        (cond
         ;; Incomplete → bail out.
         ((and (null (car parsed)) (= (cdr parsed) idx))
          (setq idx n))
         (t
          (when (car parsed) (push (car parsed) events))
          (setq idx (cdr parsed))))))
    (nreverse events)))

(defun emacs-tui-event-pending-event-p (handle)
  "Return non-nil if HANDLE has at least one parsed event ready."
  (emacs-tui-event--check-handle handle)
  (and (emacs-tui-event-handle-event-queue handle) t))

;;; E. test / bridge helpers

(defun emacs-tui-event-feed-bytes (handle bytes)
  "Append BYTES (a string) to HANDLE's input buffer and parse what we can.
Each call appends to the partial buffer, then drains as many
complete events as possible.  Bytes that form an incomplete
sequence (e.g. lone ESC, half a CSI, or a leading UTF-8 byte
without continuations) stay in the buffer for the next feed.

Returns the number of new events queued."
  (emacs-tui-event--check-handle handle)
  (unless (stringp bytes)
    (signal 'wrong-type-argument (list 'stringp bytes)))
  (let* ((accum (concat (emacs-tui-event-handle-input-buffer handle) bytes))
         (n (length accum))
         (idx 0)
         (added 0))
    (while (< idx n)
      (let ((parsed (emacs-tui-event--parse-one accum idx)))
        (cond
         ((and (null (car parsed)) (= (cdr parsed) idx))
          ;; Incomplete — leave the rest in the buffer.
          (setq idx n))
         (t
          (when (car parsed)
            (setf (emacs-tui-event-handle-event-queue handle)
                  (append (emacs-tui-event-handle-event-queue handle)
                          (list (car parsed))))
            (setq added (1+ added)))
          (setq idx (cdr parsed))))))
    ;; Anything not consumed stays in the input buffer.
    (let* ((last-idx
            ;; Recompute the last fully-consumed index by re-scanning;
            ;; the loop above advanced `idx' past consumed events but
            ;; also bumped it to `n' on incomplete tail.  Use a second
            ;; pass to find the precise boundary.
            (let ((i 0))
              (while (< i n)
                (let ((p (emacs-tui-event--parse-one accum i)))
                  (cond
                   ((and (null (car p)) (= (cdr p) i))
                    (setq n i))           ;; stop the outer while
                   (t
                    (setq i (cdr p))))))
              i)))
      (setf (emacs-tui-event-handle-input-buffer handle)
            (substring accum last-idx)))
    (emacs-tui-event--log "feed-bytes handle=%S +%d queue=%d buffer=%d"
                          (emacs-tui-event-handle-id handle)
                          added
                          (length (emacs-tui-event-handle-event-queue handle))
                          (length (emacs-tui-event-handle-input-buffer handle)))
    added))

;;; C. polling (Doc 43 §2.6 pull-on-demand)

(defun emacs-tui-event--pump-input (handle)
  "Drain `emacs-tui-event-input-fn' (if set) into HANDLE's buffer.
Reads until the function returns nil, then runs the parser.  Used
by `emacs-tui-event-poll' before checking the queue for events."
  (when emacs-tui-event-input-fn
    (let ((bytes nil)
          (b nil))
      (while (setq b (funcall emacs-tui-event-input-fn))
        (unless (and (integerp b) (>= b 0) (<= b 255))
          (signal 'wrong-type-argument (list 'byte-in-range b)))
        (push b bytes))
      (when bytes
        (let ((str (apply #'unibyte-string (nreverse bytes))))
          (emacs-tui-event-feed-bytes handle str))))))

;;;###autoload
(defun emacs-tui-event-poll (handle &optional timeout-ms)
  "Pop and return the next pending event from HANDLE, or nil on empty.
TIMEOUT-MS, if non-nil, is a non-negative integer giving a maximum
wait time in milliseconds.  When TIMEOUT-MS is supplied and the
queue is empty, we do a short `sleep-for' loop polling the input
function and the queue at 5ms intervals so test injectors running
on a separate thread / timer can land an event mid-wait without us
busy-spinning.

Doc 43 §2.6 pull-on-demand: a poll without TIMEOUT-MS *never*
blocks; it returns nil immediately if neither the queue nor the
input function produces an event.

Returns the event (a plist) or nil."
  (emacs-tui-event--check-handle handle)
  (emacs-tui-event--pump-input handle)
  (let ((q (emacs-tui-event-handle-event-queue handle)))
    (cond
     (q
      (let ((ev (car q)))
        (setf (emacs-tui-event-handle-event-queue handle) (cdr q))
        (emacs-tui-event--log "event-poll handle=%S ev=%S"
                              (emacs-tui-event-handle-id handle) ev)
        ev))
     ((and timeout-ms (> timeout-ms 0))
      (let* ((deadline (+ (float-time) (/ timeout-ms 1000.0)))
             (interval 0.005)
             (event nil))
        (while (and (null event)
                    (< (float-time) deadline))
          (sleep-for interval)
          (emacs-tui-event--pump-input handle)
          (setq q (emacs-tui-event-handle-event-queue handle))
          (when q
            (setq event (car q))
            (setf (emacs-tui-event-handle-event-queue handle) (cdr q))))
        (when event
          (emacs-tui-event--log "event-poll(wait) handle=%S ev=%S"
                                (emacs-tui-event-handle-id handle) event))
        event))
     (t nil))))

;;; D. SIGWINCH / resize

;; We do NOT trap raw SIGWINCH from Elisp directly (Emacs already
;; owns the signal handler and exposes resize via
;; `window-size-change-functions' for in-process frames).  For host
;; Emacs `--batch' + tty operation, an external supervisor (= the
;; eventloop multiplexer in Doc 39 §3.K, or the process filter side
;; of `emacs-frame.el' Phase 11.A wiring) is expected to call
;; `emacs-tui-event-dispatch-resize' with the new (W H) tuple
;; whenever it receives notice from the platform.
;;
;; This module exposes a per-handle callback registry plus a
;; convenience hook installer that delegates to
;; `window-size-change-functions' for in-process Emacs use.

(defvar emacs-tui-event--installed-handles nil
  "List of handles currently subscribed to `window-size-change-functions'.
Module-private; manipulated by `emacs-tui-event-install-sigwinch' and
`emacs-tui-event-uninstall-sigwinch'.")

(defun emacs-tui-event--window-size-change-hook (frame)
  "Hook fn for `window-size-change-functions' — fan out to handles.
FRAME is the Emacs frame whose size changed; we read its current
size via `frame-width' / `frame-height' and dispatch each
subscribed handle's callback."
  (let ((w (frame-width frame))
        (h (frame-height frame)))
    (dolist (handle emacs-tui-event--installed-handles)
      (when (and (emacs-tui-event-handlep handle)
                 (emacs-tui-event-handle-alive-p handle))
        (emacs-tui-event-dispatch-resize handle w h)))))

(defun emacs-tui-event-install-sigwinch (handle callback)
  "Register CALLBACK on HANDLE to be invoked on a resize event.
CALLBACK is a function of two integers `(WIDTH HEIGHT)'.  This
also subscribes HANDLE to `window-size-change-functions' so that
in-process Emacs frame resizes route through automatically.

Returns the previous callback (or nil)."
  (emacs-tui-event--check-handle handle)
  (unless (or (null callback) (functionp callback))
    (signal 'wrong-type-argument (list 'functionp callback)))
  (let ((prev (emacs-tui-event-handle-sigwinch-cb handle)))
    (setf (emacs-tui-event-handle-sigwinch-cb handle) callback)
    (cl-pushnew handle emacs-tui-event--installed-handles)
    (add-hook 'window-size-change-functions
              #'emacs-tui-event--window-size-change-hook)
    (emacs-tui-event--log "install-sigwinch handle=%S cb=%S prev=%S"
                          (emacs-tui-event-handle-id handle) callback prev)
    prev))

(defun emacs-tui-event-uninstall-sigwinch (handle)
  "Drop HANDLE's resize callback and unsubscribe from the size-change hook.
When HANDLE is the last subscriber the global hook entry is also
removed.  Returns t."
  (emacs-tui-event--check-handle handle)
  (setf (emacs-tui-event-handle-sigwinch-cb handle) nil)
  (setq emacs-tui-event--installed-handles
        (delq handle emacs-tui-event--installed-handles))
  (when (null emacs-tui-event--installed-handles)
    (remove-hook 'window-size-change-functions
                 #'emacs-tui-event--window-size-change-hook))
  (emacs-tui-event--log "uninstall-sigwinch handle=%S"
                        (emacs-tui-event-handle-id handle))
  t)

(defun emacs-tui-event-current-window-size (handle)
  "Return the most recently observed window size for HANDLE.
Returns `(WIDTH . HEIGHT)' (a cons), with both values defaulting to
`emacs-tui-event-default-window-width' /
`emacs-tui-event-default-window-height' if no resize event has
fired yet."
  (emacs-tui-event--check-handle handle)
  (cons (or (emacs-tui-event-handle-window-width handle)
            emacs-tui-event-default-window-width)
        (or (emacs-tui-event-handle-window-height handle)
            emacs-tui-event-default-window-height)))

(defun emacs-tui-event-dispatch-resize (handle width height)
  "Update HANDLE's stored window size and fire its resize callback.
WIDTH and HEIGHT must be positive integers.  Also pushes a
`(:type resize :width W :height H)' event onto the queue so that
consumers polling via `emacs-tui-event-poll' observe the resize
through the same event-source contract as keyboard input.

Returns the callback's return value, or nil if no callback is
registered.  Internal helper for `window-size-change-functions'
fan-out and for ERT."
  (emacs-tui-event--check-handle handle)
  (unless (and (integerp width) (> width 0))
    (signal 'wrong-type-argument (list 'positive-integer width)))
  (unless (and (integerp height) (> height 0))
    (signal 'wrong-type-argument (list 'positive-integer height)))
  (setf (emacs-tui-event-handle-window-width handle) width
        (emacs-tui-event-handle-window-height handle) height
        (emacs-tui-event-handle-event-queue handle)
        (append (emacs-tui-event-handle-event-queue handle)
                (list (list :type 'resize :width width :height height))))
  (emacs-tui-event--log "dispatch-resize handle=%S %dx%d"
                        (emacs-tui-event-handle-id handle) width height)
  (let ((cb (emacs-tui-event-handle-sigwinch-cb handle)))
    (when cb (funcall cb width height))))

(provide 'emacs-tui-event)

;;; emacs-tui-event.el ends here
