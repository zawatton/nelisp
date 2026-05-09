;;; emacs-stdio.el --- chunked stdin reader + read-from-minibuffer shim  -*- lexical-binding: t; -*-

;; Phase A2 (= Doc anvil-runtime pure-elisp roadmap, 2026-05-09)
;;
;; Standalone NeLisp ships `read-stdin-bytes' (= libc.read fd 0 wrapper,
;; lisp/nelisp-stdlib-misc.el) but no `read-from-minibuffer'.  The
;; bulk-stub layer (emacs-stub-bulk.el) installs a fixed-nil shim, so
;; callers like `anvil-server-run-batch-stdio' that read line-by-line
;; with `read-from-minibuffer' loop into immediate EOF.
;;
;; This module provides a chunked stdin line reader plus an activator
;; that overrides the nil stub via `defalias'.  The chunk size is sized
;; for production MCP frame throughput — Phase A0 v2 measured ~6.7
;; frames/sec with a 1-byte-per-call loop; this implementation lifts
;; that to chunked block reads of `emacs-stdio--chunk-size' bytes per
;; libc.read syscall, so per-line cost drops to one syscall plus a
;; linear newline scan over the in-memory buffer.
;;
;; Caller responsibility:
;;   (require 'emacs-stub)        ; pulls in emacs-stub-bulk last
;;   (require 'emacs-stdio)       ; this file
;;   (emacs-stdio-install-stdin-shim)
;;
;; The activator is opt-in so test runners and tools that explicitly
;; want the nil-EOF shim (= e.g. unit tests of fallback paths) can
;; skip the install step.
;;
;; Compatibility: under real Emacs (--batch or interactive),
;; `read-from-minibuffer' is already `fboundp' so the bulk stub never
;; fires, and `emacs-stdio-install-stdin-shim' should NOT be called.
;; The activator detects this and refuses to overwrite a bound symbol.

;; `read-stdin-bytes' is provided by NeLisp standalone (defined in
;; lisp/nelisp-stdlib-misc.el via libc.read on fd 0); under Emacs
;; --batch the symbol is normally unbound, so silence the byte
;; compiler warning while letting runtime resolve the binding.
;; Standalone NeLisp does not ship `declare-function' itself
;; (= an Emacs subr.el macro); provide a no-op stub on that runtime.
(unless (fboundp 'declare-function)
  (defmacro declare-function (&rest _) nil))
(declare-function read-stdin-bytes "ext:nelisp-stdlib-misc" (limit))

(defvar emacs-stdio--buffer ""
  "Accumulated stdin bytes not yet consumed by `emacs-stdio-read-line'.")

(defconst emacs-stdio--chunk-size 4096
  "Bytes requested per `read-stdin-bytes' refill call.")

(defconst emacs-stdio--newline-byte 10
  "Byte value of `\\n' (LF), used as the line terminator.")

(defun emacs-stdio--find-newline (s)
  "Return zero-based index of first LF byte in S, or nil."
  (let ((i 0)
        (n (length s))
        (pos nil))
    (while (and (< i n) (null pos))
      (when (= (aref s i) emacs-stdio--newline-byte)
        (setq pos i))
      (setq i (1+ i)))
    pos))

(defun emacs-stdio--refill ()
  "Block-read up to `emacs-stdio--chunk-size' bytes from stdin.

Appends to `emacs-stdio--buffer' and returns t when bytes were read,
or nil on EOF.  Errors propagate from `read-stdin-bytes'."
  (let ((chunk (read-stdin-bytes emacs-stdio--chunk-size)))
    (cond
     ((null chunk) nil)
     ((and (stringp chunk) (= (length chunk) 0)) nil)
     (t
      (setq emacs-stdio--buffer (concat emacs-stdio--buffer chunk))
      t))))

(defun emacs-stdio-read-line ()
  "Read one LF-terminated line from stdin.

Returns the line as a string with the terminating LF stripped, or
nil at EOF.  Bytes after a partial last line (= EOF without trailing
LF) are returned as the final non-nil value, then subsequent calls
return nil."
  (let ((nl (emacs-stdio--find-newline emacs-stdio--buffer))
        (refilled t))
    (while (and (null nl) refilled)
      (setq refilled (emacs-stdio--refill))
      (when refilled
        (setq nl (emacs-stdio--find-newline emacs-stdio--buffer))))
    (cond
     (nl
      (let ((line (substring emacs-stdio--buffer 0 nl)))
        (setq emacs-stdio--buffer
              (substring emacs-stdio--buffer (1+ nl)))
        line))
     ((> (length emacs-stdio--buffer) 0)
      (let ((line emacs-stdio--buffer))
        (setq emacs-stdio--buffer "")
        line))
     (t nil))))

(defun emacs-stdio-read-bytes (n)
  "Read exactly N bytes from stdin, or fewer at EOF.

Returns a string of up to N bytes consumed from the internal buffer
(refilling via `read-stdin-bytes' as needed) or nil if N <= 0 or
EOF is reached before any byte is available.  Unlike
`emacs-stdio-read-line' this does NOT split on LF, so it is the
correct primitive for MCP framed bodies whose Content-Length runs
straight into the next frame's header line without an intervening
newline."
  (when (and (numberp n) (> n 0))
    (while (and (< (length emacs-stdio--buffer) n)
                (emacs-stdio--refill)))
    (let* ((avail (length emacs-stdio--buffer))
           (take (if (< avail n) avail n)))
      (when (> take 0)
        (let ((chunk (substring emacs-stdio--buffer 0 take)))
          (setq emacs-stdio--buffer
                (substring emacs-stdio--buffer take))
          chunk)))))

(defun emacs-stdio--minibuffer-shim (&rest _ignored)
  "Stdin-backed `read-from-minibuffer' replacement.

Ignores PROMPT / INITIAL-INPUT / KEYMAP / READ / HIST / DEFAULT-VALUE
/ INHERIT-INPUT-METHOD args.  Standalone NeLisp has no minibuffer,
so the only useful behaviour is to read one line of real stdin."
  (emacs-stdio-read-line))

(defun emacs-stdio-install-stdin-shim ()
  "Install `emacs-stdio--minibuffer-shim' as `read-from-minibuffer'.

Refuses to overwrite a real C-level subr (= the real Emacs builtin
under `--batch' or interactive Emacs).  In every other case
(unbound symbol on standalone NeLisp, the bulk-stub closure from
emacs-stub-bulk.el, or a previous call to this very function) the
binding is replaced unconditionally.  Returns t when the shim was
installed, nil when a real subr was preserved.  Idempotent — safe
to call from multiple bootstrap paths."
  (cond
   ((not (fboundp 'read-from-minibuffer))
    (defalias 'read-from-minibuffer #'emacs-stdio--minibuffer-shim)
    t)
   ((subrp (symbol-function 'read-from-minibuffer))
    nil)
   (t
    (defalias 'read-from-minibuffer #'emacs-stdio--minibuffer-shim)
    t)))

(defun emacs-stdio-reset-buffer ()
  "Drop any unread stdin bytes from the internal buffer.

Test-only helper.  Production callers should not need this — the
buffer drains naturally as `emacs-stdio-read-line' is called."
  (setq emacs-stdio--buffer ""))

(provide 'emacs-stdio)
;;; emacs-stdio.el ends here
