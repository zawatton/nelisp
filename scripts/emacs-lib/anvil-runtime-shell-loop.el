;;; anvil-runtime-shell-loop.el --- nelisp-shell replacement for bin/anvil-runtime  -*- lexical-binding: t; -*-

;; Phase B5 Final B Stage 1 (= 2026-05-09)
;;
;; Doc anvil-runtime pure-elisp roadmap: replace the Rust crate
;; `anvil-runtime' (= 5,667 LOC) with a shell launcher that runs this
;; file under standalone NeLisp.  This file:
;;
;;   1. Loads the L2 Emacs C-primitive shims (`emacs-init', `emacs-stub')
;;      and the json / backquote / cl-defstruct fixes shipped in
;;      Phase B2-B5.
;;   2. Loads the anvil-server.el + anvil-server-commands.el +
;;      anvil-server-metrics.el modules from the user's anvil.el
;;      checkout.
;;   3. Activates the `read-from-minibuffer' shim (= line-buffered
;;      stdin from `read-stdin-bytes' bytes) so anvil-server's MCP
;;      Content-Length frame reader has a working line source.
;;   4. Calls `anvil-server-start' to install the active-server
;;      registry then enters `anvil-server-run-batch-stdio' which
;;      blocks reading frames until EOF.
;;
;; Configuration via env vars (matches Rust binary semantics):
;;   ANVIL_EL_DIR — directory containing anvil-server*.el (required;
;;                  default = $HOME/.emacs.d/external-packages/anvil.el).
;;   ANVIL_SERVER_ID — server-id argument passed to `anvil-server-start'
;;                     and `anvil-server-run-batch-stdio' (default
;;                     = "default").
;;   NELISP_EMACS_DIR — directory containing this file's siblings
;;                      `src/emacs-init.el' / `src/emacs-stub.el'
;;                      (= the nelisp-emacs checkout root).
;;
;; Once tested end-to-end, the Rust crate `anvil-runtime/' can be
;; deleted in Final B Stage 2 along with the bin/anvil-runtime symlink
;; rewire.

;;; Code:

(defun anvil-runtime-shell--env (name default)
  (let ((val (and (fboundp 'getenv) (getenv name))))
    (if (and val (> (length val) 0)) val default)))

(let* ((nelisp-emacs-dir
        (anvil-runtime-shell--env
         "NELISP_EMACS_DIR"
         "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs"))
       (anvil-el-dir
        (anvil-runtime-shell--env
         "ANVIL_EL_DIR"
         "/home/madblack-21/.emacs.d/external-packages/anvil.el"))
       (server-id
        (anvil-runtime-shell--env "ANVIL_SERVER_ID" "default"))
       (init-el (concat nelisp-emacs-dir "/src/emacs-init.el"))
       (stub-el (concat nelisp-emacs-dir "/src/emacs-stub.el"))
       (stdio-el (concat nelisp-emacs-dir "/src/emacs-stdio.el"))
       (metrics-el (concat anvil-el-dir "/anvil-server-metrics.el"))
       (server-el (concat anvil-el-dir "/anvil-server.el"))
       (server-commands-el (concat anvil-el-dir "/anvil-server-commands.el")))

  ;; Bootstrap layer 2 + json / backquote fixes.
  (load init-el nil t)
  (load stub-el nil t)

  ;; anvil-server module load chain.  Order: metrics → server → commands
  ;; (= matches anvil-server.el's `(require 'anvil-server-metrics)' and
  ;; anvil-server-commands.el's `(require 'anvil-server)').
  (load metrics-el nil t)
  (load server-el nil t)
  (load server-commands-el nil t)

  ;; stdin shim — anvil-server-run-batch-stdio reads frames via
  ;; `read-from-minibuffer'; emacs-stdio.el's installer overrides the
  ;; bulk-stub nil binding with a chunked reader backed by libc.read.
  (load stdio-el nil t)
  (when (fboundp 'emacs-stdio-install-stdin-shim)
    (emacs-stdio-install-stdin-shim))

  ;; Phase B5 Final B Stage 1b — regex-free overrides for the framing
  ;; helpers anvil-server.el uses.  Standalone NeLisp ships only
  ;; `string-match-p' and even that is a hand-rolled lookup table (=
  ;; not a real regex engine), so the original `string-match' /
  ;; `replace-regexp-in-string' patterns return nil unconditionally.
  ;; The overrides below cover the exact callsites in anvil-server's
  ;; MCP framing path with string-search / substring-based logic.

  ;; `(replace-regexp-in-string "\r\\'" "" line)' just strips a single
  ;; trailing CR.  Replace with a simple suffix check.
  (defun anvil-server--strip-trailing-cr (s)
    (if (and (stringp s)
             (> (length s) 0)
             (eq (aref s (1- (length s))) ?\r))
        (substring s 0 (1- (length s)))
      s))

  (defun anvil-server-mcp-parse-content-length-header (header-block)
    "Phase B5 Stage 1b override — regex-free Content-Length parser.
Walks HEADER-BLOCK line-by-line searching for `Content-Length:'
case-insensitively, returns the integer value or nil."
    (when (stringp header-block)
      (let ((lines (split-string header-block "\r\n"))
            (found nil))
        (while (and lines (not found))
          (let* ((line (anvil-server--strip-trailing-cr (car lines)))
                 (line-down (downcase line))
                 (prefix "content-length:"))
            (when (and (>= (length line-down) (length prefix))
                       (string= (substring line-down 0 (length prefix)) prefix))
              (let* ((rest (substring line (length prefix)))
                     ;; Skip leading whitespace.
                     (i 0)
                     (n (length rest)))
                (while (and (< i n)
                            (let ((c (aref rest i)))
                              (or (eq c ?\s) (eq c ?\t))))
                  (setq i (1+ i)))
                (let* ((num-start i)
                       (num-end i))
                  (while (and (< num-end n)
                              (let ((c (aref rest num-end)))
                                (and (>= c ?0) (<= c ?9))))
                    (setq num-end (1+ num-end)))
                  (when (> num-end num-start)
                    (setq found (string-to-number
                                 (substring rest num-start num-end))))))))
          (setq lines (cdr lines)))
        found)))

  (defun anvil-server-mcp-detect-framing-p (initial)
    "Phase B5 Stage 1b override — case-insensitive prefix check for
`Content-Length:' on INITIAL string (= the first line read from stdin)."
    (when (stringp initial)
      (let* ((stripped (anvil-server--strip-trailing-cr initial))
             (down (downcase stripped))
             (prefix "content-length:"))
        (and (>= (length down) (length prefix))
             (string= (substring down 0 (length prefix)) prefix)))))

  (defun anvil-server-mcp-frame-encode (body)
    "Phase B5 Stage 1b override — emit `Content-Length: N\r\n\r\nBODY'.
N is the UTF-8 byte length of BODY.  Standalone NeLisp strings are
already UTF-8, so `length' is byte-count for ASCII bodies and
`encode-coding-string' is a no-op identity per Phase B5 stub."
    (let* ((bytes (if (fboundp 'encode-coding-string)
                      (encode-coding-string body 'utf-8 t)
                    body))
           (n (length bytes)))
      (concat "Content-Length: " (number-to-string n) "\r\n\r\n" body)))

  ;; Override the framed-with-prefix reader entirely.  The original uses
  ;; `replace-regexp-in-string' for trailing-CR strip, which is a no-op
  ;; stub on standalone NeLisp — that caused the header read loop to
  ;; consume the body line as just another header (= the empty `\r' line
  ;; never matched `string-empty-p'), so by the time body collection
  ;; ran, stdin was at EOF.  This regex-free port restores correctness.
  (defun anvil-server--batch-read-framed-with-prefix (first-header-line)
    "Phase B5 Stage 1b override — regex-free framed reader.

Body bytes are pulled via `emacs-stdio-read-bytes' (Phase B6 fix,
2026-05-10) because the MCP wire format does not insert a newline
between body and the next frame's header — line-based reads would
consume past the body's Content-Length boundary and discard the
head of the next header line."
    (let ((header-lines (list (anvil-server--strip-trailing-cr first-header-line)))
          (seen-blank nil))
      (catch 'done
        (while (not seen-blank)
          (let ((line (ignore-errors (read-from-minibuffer ""))))
            (cond
             ((null line) (throw 'done nil))
             ((string-empty-p (anvil-server--strip-trailing-cr line))
              (setq seen-blank t))
             (t (push (anvil-server--strip-trailing-cr line)
                      header-lines))))))
      (let* ((header-block (mapconcat #'identity
                                      (nreverse header-lines) "\r\n"))
             (n (anvil-server-mcp-parse-content-length-header header-block)))
        (when (and n (>= n 0))
          (let ((body (and (fboundp 'emacs-stdio-read-bytes)
                           (emacs-stdio-read-bytes n))))
            (and body (> (length body) 0) body))))))

  (defun anvil-server--batch-read-framed-message ()
    "Phase B5 Stage 1b override — used on subsequent loop iterations.
Reads the first header line, then delegates to the framed-with-prefix
reader."
    (let ((first (ignore-errors (read-from-minibuffer ""))))
      (cond
       ((null first) nil)
       (t (anvil-server--batch-read-framed-with-prefix first)))))

  (defun anvil-server--batch-skip-blank-lines ()
    "Phase B5 Stage 1b override — drain blanks until non-blank line.
Uses CR-strip to recognise lines that are CRLF artefacts as blank."
    (let ((line ""))
      (while (and (stringp line)
                  (string-empty-p (anvil-server--strip-trailing-cr line)))
        (setq line (ignore-errors (read-from-minibuffer ""))))
      line))

  ;; `anvil-server-run-batch-stdio' itself calls `anvil-server-start'
  ;; on entry, so we MUST NOT call it here (= duplicate call signals
  ;; `MCP server is already running').  Just enter the loop.
  (anvil-server-run-batch-stdio server-id))

;;; anvil-runtime-shell-loop.el ends here
