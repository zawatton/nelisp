;;; nelisp-service.el --- Shared plumbing for NeLisp workers and daemons -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 213.  Common pieces used by `nelisp-service-worker',
;; `nelisp-service-daemon' and `nelisp-service-client':
;;
;;   - message framing: one printed Lisp object per line.  Newlines and
;;     carriage returns inside strings are escaped by hand because the
;;     standalone reader ignores `print-escape-newlines' (measured
;;     2026-10-10: the printed form kept a raw newline).
;;   - a line splitter that accumulates arbitrary chunks (process filter
;;     output, stdin reads) and yields complete lines.
;;   - a blocking stdin line reader that works on the standalone reader
;;     (`read-stdin-bytes') and on host Emacs in batch mode
;;     (`read-from-minibuffer').
;;   - state files: an exclusive lock file and a plist state file under a
;;     per-user directory.
;;   - tokens.  `secure-hash' on the standalone reader returns the hash of
;;     the empty string for any input (measured 2026-10-10) and `(random t)'
;;     does not reseed, so tokens are built from the clock and a counter.
;;     They keep other local programs from talking to a daemon by
;;     accident; they are not a defence against a hostile local user, who
;;     can read the state file anyway.
;;
;; Only standard Emacs process and file primitives are used, so every
;; module here runs unchanged on host Emacs and on the standalone reader.

;;; Code:

(defgroup nelisp-service nil
  "Process lifecycle, worker pools and daemons for NeLisp programs."
  :group 'nelisp)

(defcustom nelisp-service-state-directory nil
  "Directory for daemon lock and state files.
nil means $NELISP_SERVICE_DIR, else ~/.nelisp-service."
  :type '(choice (const nil) directory)
  :group 'nelisp-service)

;;; Framing ---------------------------------------------------------------

(defun nelisp-service--escape-line (printed)
  "Return PRINTED with raw newlines and carriage returns escaped.
Raw line breaks can only occur inside string literals, where the
reader turns the escapes back into the original characters."
  (let ((out nil) (i 0) (n (length printed)) (start 0))
    (while (< i n)
      (let ((c (aref printed i)))
        (when (or (= c ?\n) (= c ?\r))
          (push (substring printed start i) out)
          (push (if (= c ?\n) "\\n" "\\r") out)
          (setq start (1+ i))))
      (setq i (1+ i)))
    (push (substring printed start) out)
    (apply #'concat (nreverse out))))

(defun nelisp-service-encode (object)
  "Return OBJECT printed on one line, terminated by a newline."
  (concat (nelisp-service--escape-line (prin1-to-string object)) "\n"))

(defun nelisp-service-decode (line)
  "Read one object from LINE, or return nil when LINE is not readable."
  (condition-case nil
      (car (read-from-string line))
    (error nil)))

;;; Line splitting --------------------------------------------------------

(defun nelisp-service-splitter-create ()
  "Return a fresh line splitter.
Feed it with `nelisp-service-splitter-feed'."
  (list ""))

(defun nelisp-service-splitter-feed (splitter chunk)
  "Append CHUNK to SPLITTER and return the complete lines, oldest first.
Line terminators (LF, optionally preceded by CR) are removed."
  (let ((buf (concat (car splitter) chunk))
        (lines nil)
        (start 0)
        (i 0))
    (while (< i (length buf))
      (when (= (aref buf i) ?\n)
        (let ((end (if (and (> i start) (= (aref buf (1- i)) ?\r)) (1- i) i)))
          (push (substring buf start end) lines))
        (setq start (1+ i)))
      (setq i (1+ i)))
    (setcar splitter (substring buf start))
    (nreverse lines)))

(defun nelisp-service-splitter-pending (splitter)
  "Return the unterminated tail held by SPLITTER."
  (car splitter))

;;; Stdin -----------------------------------------------------------------
;;
;; One read-ahead buffer serves both line reads (NDJSON, the worker
;; protocol) and exact byte-count reads (MCP Content-Length bodies, which
;; run straight into the next header with no newline in between).

(defvar nelisp-service--stdin-buffer ""
  "Stdin text read ahead of the caller.")

(defvar nelisp-service--stdin-eof nil
  "Non-nil once stdin has reported end of file.")

(defun nelisp-service--stdin-refill ()
  "Append one stdin chunk to the read-ahead buffer.
Return nil at end of file.  Host Emacs has no raw stdin primitive in
batch mode, so there each refill is one line read by
`read-from-minibuffer', with its newline restored."
  (cond
   (nelisp-service--stdin-eof nil)
   ((fboundp 'read-stdin-bytes)
    (let ((chunk (read-stdin-bytes 65536)))
      (if (null chunk)
          (progn (setq nelisp-service--stdin-eof t) nil)
        (setq nelisp-service--stdin-buffer
              (concat nelisp-service--stdin-buffer chunk))
        t)))
   (t
    (condition-case nil
        (progn
          (setq nelisp-service--stdin-buffer
                (concat nelisp-service--stdin-buffer
                        (read-from-minibuffer "") "\n"))
          t)
      (error (setq nelisp-service--stdin-eof t) nil)))))

(defun nelisp-service--newline-position (string)
  "Return the index of the first LF in STRING, or nil."
  (let ((i 0) (n (length string)) (found nil))
    (while (and (not found) (< i n))
      (when (= (aref string i) ?\n) (setq found i))
      (setq i (1+ i)))
    found))

(defun nelisp-service-read-stdin-line ()
  "Block until one line arrives on stdin and return it without its LF.
A CR before the LF is removed too.  Return nil at end of file; a final
line without a terminator is returned before nil."
  (let ((pos nil))
    (while (and (not (setq pos (nelisp-service--newline-position
                                nelisp-service--stdin-buffer)))
                (nelisp-service--stdin-refill)))
    (let ((buf nelisp-service--stdin-buffer))
      (cond
       (pos
        (setq nelisp-service--stdin-buffer (substring buf (1+ pos)))
        (if (and (> pos 0) (= (aref buf (1- pos)) ?\r))
            (substring buf 0 (1- pos))
          (substring buf 0 pos)))
       ((> (length buf) 0)
        (setq nelisp-service--stdin-buffer "")
        buf)
       (t nil)))))

(defun nelisp-service-utf8-length (string)
  "Return the number of bytes STRING occupies in UTF-8."
  (string-bytes string))

(defun nelisp-service-read-stdin-bytes (count)
  "Block until COUNT bytes (UTF-8) arrive on stdin and return them.
Return fewer at end of file, or nil when nothing is left."
  (let ((taken 0) (i 0) (done nil))
    (while (not done)
      (let ((buf nelisp-service--stdin-buffer))
        (while (and (< i (length buf)) (< taken count))
          (setq taken (+ taken (string-bytes (string (aref buf i))))
                i (1+ i)))
        (when (or (>= taken count) (not (nelisp-service--stdin-refill)))
          (setq done t))))
    (let ((buf nelisp-service--stdin-buffer))
      (setq nelisp-service--stdin-buffer (substring buf i))
      (and (> i 0) (substring buf 0 i)))))

(defun nelisp-service-setup-stdio ()
  "Make stdin and stdout carry UTF-8 on host Emacs in batch mode.
Host Emacs decodes batch stdin and encodes stdout with
`locale-coding-system', which is cp932 on a Japanese Windows host
and garbles non-ASCII requests (measured 2026-10-10).  The
standalone reader always uses UTF-8, so this is a no-op there."
  (unless (fboundp 'read-stdin-bytes)
    (setq locale-coding-system 'utf-8)))

;; `princ' with t writes to stdout in batch on both substrates; route
;; through a function so tests can rebind it.
(defvar nelisp-service-stdout-function
  (lambda (string) (princ string t))
  "Function called with each string written to stdout.")

(defun nelisp-service-write-stdout (string)
  "Write STRING to stdout through `nelisp-service-stdout-function'."
  (funcall nelisp-service-stdout-function string))

;;; Files -----------------------------------------------------------------

(defun nelisp-service-state-directory ()
  "Return the state directory, creating it when missing."
  (let ((dir (expand-file-name
              (or nelisp-service-state-directory
                  (let ((env (getenv "NELISP_SERVICE_DIR")))
                    (and env (> (length env) 0) env))
                  "~/.nelisp-service"))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun nelisp-service-state-file (name kind)
  "Return the path of NAME's KIND file (`lock' or `state')."
  (expand-file-name (format "%s.%s" name kind)
                    (nelisp-service-state-directory)))

(defun nelisp-service-read-plist (file)
  "Return the plist stored in FILE, or nil when FILE is absent or unreadable."
  (when (file-exists-p file)
    (condition-case nil
        (let ((obj (with-temp-buffer
                     (insert-file-contents file)
                     (nelisp-service-decode (buffer-string)))))
          (and (listp obj) obj))
      (error nil))))

(defun nelisp-service-write-plist (file plist &optional exclusive)
  "Write PLIST to FILE.  With EXCLUSIVE, fail when FILE already exists.
Return non-nil on success."
  (condition-case nil
      (progn
        (write-region (nelisp-service-encode plist) nil file nil 'silent nil
                      (and exclusive 'excl))
        t)
    (file-already-exists nil)))

(defun nelisp-service-delete-file (file)
  "Delete FILE, ignoring a missing file."
  (condition-case nil (delete-file file) (error nil)))

;;; Tokens ----------------------------------------------------------------

(defvar nelisp-service--token-counter 0
  "Counter mixed into tokens so two tokens in one process differ.")

(defun nelisp-service-make-token ()
  "Return a fresh token string.  See the Commentary for its strength."
  (setq nelisp-service--token-counter (1+ nelisp-service--token-counter))
  (let ((now (float-time)))
    (format "%x-%x-%x-%x"
            (truncate now)
            (truncate (* 1000000 (- now (ffloor now))))
            (random 268435456)
            nelisp-service--token-counter)))

;;; Mutable records -------------------------------------------------------
;;
;; Pools, workers, daemons and connections are (TAG . PLIST) cells so the
;; package does not depend on `cl-defstruct' being available on every
;; substrate.

(defun nelisp-service-record (tag &rest plist)
  "Return a new mutable record tagged TAG holding PLIST."
  (cons tag plist))

(defun nelisp-service-get (record key)
  "Return KEY's value in RECORD."
  (plist-get (cdr record) key))

(defun nelisp-service-put (record key value)
  "Set KEY to VALUE in RECORD and return VALUE."
  (setcdr record (plist-put (cdr record) key value))
  value)

(defun nelisp-service-incf (record key)
  "Increment the integer at KEY in RECORD and return it."
  (nelisp-service-put record key (1+ (or (nelisp-service-get record key) 0))))

(defun nelisp-service-standalone-p ()
  "Return non-nil on the standalone NeLisp reader."
  (fboundp 'read-stdin-bytes))

(defun nelisp-service-self-command ()
  "Return the absolute path of the running NeLisp or Emacs executable."
  (if (file-name-absolute-p invocation-name)
      invocation-name
    (expand-file-name invocation-name invocation-directory)))

;;; Starting scripts ------------------------------------------------------
;;
;; The standalone reader passes neither `command-line-args' nor the
;; environment set with `setenv' to a script it starts (measured
;; 2026-10-10: both empty in the child).  A script that needs settings is
;; therefore started through a generated bootstrap file that `setq's them
;; and then loads the script -- the same shape bin/anvil-runtime uses.

(defvar nelisp-service-bootstrap-args nil
  "Plist handed to a script started by `nelisp-service-script-command'.")

(defun nelisp-service-script-command (script &optional args)
  "Return a command that runs SCRIPT with ARGS on the current substrate.
ARGS is a plist bound to `nelisp-service-bootstrap-args' in the child,
which also inherits `nelisp-service-state-directory'.  The bootstrap
file is written next to the state files."
  (let* ((dir (nelisp-service-state-directory))
         (file (expand-file-name
                (format "bootstrap-%s.el" (nelisp-service-make-token)) dir)))
    (write-region
     (concat
      ";; -*- lexical-binding: t; -*-\n"
      (nelisp-service-encode
       `(setq nelisp-service-state-directory ,dir))
      (nelisp-service-encode
       `(setq nelisp-service-bootstrap-args ',args))
      (nelisp-service-encode
       `(condition-case nil (delete-file ,file) (error nil)))
      (nelisp-service-encode
       `(load ,(expand-file-name script) nil t)))
     nil file nil 'silent)
    (if (nelisp-service-standalone-p)
        (list (nelisp-service-self-command) "--load" file)
      (list (nelisp-service-self-command) "--batch" "-Q" "-l" file))))

;;; Event loop helper -----------------------------------------------------

(defun nelisp-service-wait (seconds)
  "Service process I/O for up to SECONDS.
Return non-nil when some process produced output."
  (accept-process-output nil seconds))

(defun nelisp-service-wait-until (predicate timeout &optional step)
  "Service I/O until PREDICATE returns non-nil or TIMEOUT seconds pass.
Return PREDICATE's value, or nil on timeout.  STEP is the poll
interval (default 0.05 s)."
  (let ((deadline (+ (float-time) timeout))
        (result nil))
    (while (and (not (setq result (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil (or step 0.05)))
    result))

(provide 'nelisp-service)

;;; nelisp-service.el ends here
