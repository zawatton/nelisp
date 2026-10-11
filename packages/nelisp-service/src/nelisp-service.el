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
;;
;; Speed matters here: the standalone reader interprets Lisp loops at
;; several microseconds per step, so a per-character loop over a 25 KB MCP
;; reply took 3 s, and `prin1-to-string' of that string 6 s, while
;; `string-match' and `replace-regexp-in-string' run natively (5 ms and
;; 25 ms; measured 2026-10-10).  Strings are therefore printed and line
;; breaks found with the regexp primitives only -- and on UTF-8 bytes: on a
;; string holding any non-ASCII character the same regexp calls took 51 s
;; instead of 0.2 s, while converting to and from unibyte UTF-8 is
;; instantaneous.

(defun nelisp-service--bytes (string)
  "Return STRING as unibyte UTF-8."
  (if (multibyte-string-p string) (encode-coding-string string 'utf-8 t) string))

(defun nelisp-service--text (bytes)
  "Return unibyte UTF-8 BYTES as a string."
  (decode-coding-string bytes 'utf-8 t))

(defun nelisp-service--print-string (string)
  "Return STRING as a Lisp string literal with no raw line breaks."
  ;; LITERAL is t throughout, so each replacement is inserted as written.
  (concat "\""
          (replace-regexp-in-string
           "\r" "\\r"
           (replace-regexp-in-string
            "\n" "\\n"
            (replace-regexp-in-string
             "\"" "\\\""
             (replace-regexp-in-string "\\\\" "\\\\" (nelisp-service--bytes string) t t)
             t t)
            t t)
           t t)
          "\""))

(defun nelisp-service--escape-line (printed)
  "Return PRINTED with raw newlines and carriage returns escaped.
Raw line breaks can only occur inside string literals, where the
reader turns the escapes back into the original characters."
  (if (string-match "[\n\r]" printed)
      (replace-regexp-in-string
       "\r" "\\r" (replace-regexp-in-string "\n" "\\n" printed t t) t t)
    printed))

(defun nelisp-service--print (object)
  "Return OBJECT printed readably on one line, as unibyte UTF-8.
Strings and proper lists are printed here, everything else by
`prin1-to-string' (small values: symbols, numbers)."
  (cond
   ((stringp object) (nelisp-service--print-string object))
   ((and (consp object) (proper-list-p object))
    (concat "(" (mapconcat #'nelisp-service--print object " ") ")"))
   (t (nelisp-service--bytes
       (nelisp-service--escape-line (prin1-to-string object))))))

;; Blobs.  Escaping still costs a regexp replacement per quote or
;; backslash, and an MCP reply is mostly quotes: 7.8 s for a 25 KB
;; tools/list on the standalone reader (measured 2026-10-10).  So a long
;; string that is a top-level element of a message list is not printed at
;; all: the line carries `(nelisp-service-blob N)' in its place and the N
;; raw UTF-8 bytes follow the line, each blob terminated by a newline.
;; Readers that cannot parse blobs never see one unless such a string is
;; sent to them.

(defconst nelisp-service-blob-threshold 256
  "Top-level message strings longer than this travel as blobs.")

(defun nelisp-service--blob-marker-p (element)
  "Return the byte count when ELEMENT is a blob marker, else nil."
  (and (consp element) (eq (car element) 'nelisp-service-blob)
       (integerp (nth 1 element)) (null (nthcdr 2 element))
       (nth 1 element)))

(defun nelisp-service-encode (object)
  "Return OBJECT printed on one line, terminated by a newline.
Long top-level strings of a list OBJECT follow the line as blobs."
  (if (and (consp object) (proper-list-p object))
      (let ((parts nil) (blobs nil))
        (dolist (element object)
          (if (and (stringp element)
                   (> (length element) nelisp-service-blob-threshold))
              (let ((bytes (nelisp-service--bytes element)))
                (push bytes blobs)
                (push (nelisp-service--bytes
                       (format "(nelisp-service-blob %d)" (length bytes)))
                      parts))
            (push (nelisp-service--print element) parts)))
        (nelisp-service--text
         (apply #'concat "(" (mapconcat #'identity (nreverse parts) " ") ")\n"
                (mapcar (lambda (b) (concat b "\n")) (nreverse blobs)))))
    (nelisp-service--text (concat (nelisp-service--print object) "\n"))))

(defun nelisp-service--fill-blobs (object blobs)
  "Return list OBJECT with its blob markers replaced by BLOBS, in order."
  (mapcar (lambda (element)
            (if (nelisp-service--blob-marker-p element) (pop blobs) element))
          object))

(defun nelisp-service--blob-sizes (object)
  "Return the byte counts of OBJECT's blob markers, in order."
  (and (consp object) (proper-list-p object)
       (delq nil (mapcar #'nelisp-service--blob-marker-p object))))

(defun nelisp-service-reader-create ()
  "Return a fresh message reader; feed it with `nelisp-service-reader-feed'."
  (list "" nil nil nil))

(defun nelisp-service-reader-feed (reader chunk)
  "Append CHUNK to READER and return the complete messages, oldest first.
A message is a decoded object with its blobs filled in; unreadable
lines are dropped."
  (let ((buf (concat (nth 0 reader) (nelisp-service--bytes chunk)))
        (pos 0) (out nil) (stop nil))
    ;; READER = (BUFFER PENDING-OBJECT PENDING-SIZES COLLECTED-BLOBS).
    (while (not stop)
      (if (nth 1 reader)
          (let ((need (car (nth 2 reader))))
            (if (< (- (length buf) pos) (1+ need))
                (setq stop t)
              (setcar (nthcdr 3 reader)
                      (cons (nelisp-service--text (substring buf pos (+ pos need)))
                            (nth 3 reader)))
              (setq pos (+ pos need 1))
              (setcar (nthcdr 2 reader) (cdr (nth 2 reader)))
              (unless (nth 2 reader)
                (push (nelisp-service--fill-blobs (nth 1 reader)
                                                  (nreverse (nth 3 reader)))
                      out)
                (setcar (nthcdr 1 reader) nil)
                (setcar (nthcdr 3 reader) nil))))
        (let ((nl (string-match "\n" buf pos)))
          (if (not nl)
              (setq stop t)
            (let* ((end (if (and (> nl pos) (= (aref buf (1- nl)) ?\r)) (1- nl) nl))
                   (object (nelisp-service-decode
                            (nelisp-service--text (substring buf pos end))))
                   (sizes (nelisp-service--blob-sizes object)))
              (setq pos (1+ nl))
              (cond
               (sizes (setcar (nthcdr 1 reader) object)
                      (setcar (nthcdr 2 reader) sizes))
               (object (push object out))))))))
    (setcar reader (substring buf pos))
    (nreverse out)))

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
  (let ((buf (concat (car splitter) (nelisp-service--bytes chunk)))
        (lines nil)
        (start 0)
        (nl nil))
    (while (setq nl (string-match "\n" buf start))
      (let ((end (if (and (> nl start) (= (aref buf (1- nl)) ?\r)) (1- nl) nl)))
        (push (nelisp-service--text (substring buf start end)) lines))
      (setq start (1+ nl)))
    (setcar splitter (substring buf start))
    (nreverse lines)))

(defun nelisp-service-splitter-pending (splitter)
  "Return the unterminated tail held by SPLITTER."
  (nelisp-service--text (car splitter)))

;;; Stdin -----------------------------------------------------------------
;;
;; One read-ahead buffer serves both line reads (NDJSON, the worker
;; protocol) and exact byte-count reads (MCP Content-Length bodies, which
;; run straight into the next header with no newline in between).

(defvar nelisp-service--stdin-buffer ""
  "Stdin bytes (unibyte UTF-8) read ahead of the caller.")

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
              (concat nelisp-service--stdin-buffer (nelisp-service--bytes chunk)))
        t)))
   (t
    (condition-case nil
        (progn
          (setq nelisp-service--stdin-buffer
                (concat nelisp-service--stdin-buffer
                        (nelisp-service--bytes (read-from-minibuffer ""))
                        "\n"))
          t)
      (error (setq nelisp-service--stdin-eof t) nil)))))

(defun nelisp-service--newline-position (string)
  "Return the index of the first LF in STRING, or nil."
  (string-match "\n" string))

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
        (nelisp-service--text
         (if (and (> pos 0) (= (aref buf (1- pos)) ?\r))
             (substring buf 0 (1- pos))
           (substring buf 0 pos))))
       ((> (length buf) 0)
        (setq nelisp-service--stdin-buffer "")
        (nelisp-service--text buf))
       (t nil)))))

(defun nelisp-service-utf8-length (string)
  "Return the number of bytes STRING occupies in UTF-8."
  (length (nelisp-service--bytes string)))

(defun nelisp-service-read-stdin-bytes (count)
  "Block until COUNT bytes (UTF-8) arrive on stdin and return them.
Return fewer at end of file, or nil when nothing is left."
  (while (and (< (length nelisp-service--stdin-buffer) count)
              (nelisp-service--stdin-refill)))
  (let* ((buf nelisp-service--stdin-buffer)
         (n (min count (length buf))))
    (setq nelisp-service--stdin-buffer (substring buf n))
    (and (> n 0) (nelisp-service--text (substring buf 0 n)))))

(defun nelisp-service-read-stdin-message ()
  "Block until one framed message arrives on stdin and return it decoded.
Blobs are filled in.  Return :eof at end of file and nil for an
unreadable line."
  (let ((line (nelisp-service-read-stdin-line)))
    (if (null line)
        :eof
      (let* ((object (nelisp-service-decode line))
             (blobs (mapcar (lambda (n)
                              (prog1 (or (nelisp-service-read-stdin-bytes n) "")
                                ;; The newline after each blob.
                                (nelisp-service-read-stdin-bytes 1)))
                            (nelisp-service--blob-sizes object))))
        (if blobs (nelisp-service--fill-blobs object blobs) object)))))

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
