;;; emacs-replace.el --- minimal occur / replace machinery  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 2 (nelisp-emacs) search-and-edit semantics: `occur' (collect matching
;; lines into an *Occur* buffer and jump back to them), bulk `replace-regexp' /
;; `replace-string', `how-many', and the `flush-lines' / `keep-lines' line
;; filters.
;;
;; Boundary (../nelisp-gui/docs/design/00-three-layer-architecture.org):
;;   - nelisp-emacs OWNS: the line scan, match collection, occur-buffer
;;     construction, jump-to-occurrence, and the buffer-mutating replacements.
;;   - nelisp-gui OWNS: rendering the *Occur* buffer + key transport.
;;
;; The scan runs `string-match' over the buffer text rather than
;; `re-search-forward': the standalone reader's buffer-search primitives are
;; limited, but its string regexp engine works.  Buffer positions are
;; `point-min' plus the string index.

;;; Code:

(defvar emacs-occur-buffer-name "*Occur*"
  "Buffer name used by `emacs-occur'.")

(defvar emacs-occur--matches nil
  "Last occur result: a list of plists (:line :pos :text).")

(defvar emacs-occur--source-buffer nil
  "Source buffer the last `emacs-occur' scanned.")

;;;; --- line scan ----------------------------------------------------

(defun emacs-replace--scan-lines (regexp text base)
  "Return matches of REGEXP among the lines of TEXT.
Each match is a plist (:line N :pos P :text LINE), where N is the
1-based line number, P is BASE plus the line's start index, and LINE is
the line text (without its newline)."
  (let ((pos 0) (lineno 1) (n (length text)) (out nil))
    (while (< pos n)
      (let* ((nl (or (string-match "\n" text pos) n))
             (line (substring text pos nl)))
        (when (string-match regexp line)
          (push (list :line lineno :pos (+ base pos) :text line) out))
        (setq lineno (1+ lineno)
              pos (1+ nl))))
    (nreverse out)))

(defun emacs-replace--count (regexp text)
  "Return the number of non-overlapping REGEXP matches in TEXT."
  (let ((pos 0) (count 0) (n (length text)))
    (while (and (<= pos n) (string-match regexp text pos))
      (setq count (1+ count)
            pos (if (> (match-end 0) (match-beginning 0))
                    (match-end 0)
                  (1+ (match-beginning 0)))))
    count))

;;;; --- occur --------------------------------------------------------

(defun emacs-occur-matches (regexp &optional buffer)
  "Return the line matches of REGEXP in BUFFER (default current)."
  (with-current-buffer (or buffer (current-buffer))
    (emacs-replace--scan-lines regexp (buffer-string) (point-min))))

(defun emacs-occur (regexp &optional buffer)
  "Collect lines matching REGEXP into `*Occur*'; return the match count.
Stores the matches for `emacs-occur-goto' navigation."
  (interactive "sList lines matching regexp: ")
  (let* ((src (or buffer (current-buffer)))
         (matches (emacs-occur-matches regexp src)))
    (setq emacs-occur--matches matches
          emacs-occur--source-buffer src)
    (let ((buf (get-buffer-create emacs-occur-buffer-name)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "%d matches for %S in buffer %s\n\n"
                          (length matches) regexp (buffer-name src)))
          (dolist (m matches)
            (insert (format "%d:%s\n"
                            (plist-get m :line) (plist-get m :text)))))
        (goto-char (point-min)))
      (when (fboundp 'display-buffer)
        (display-buffer buf)))
    (length matches)))

(defun emacs-occur-goto (n)
  "Move point to the source position of the Nth (1-based) occur match.
Returns the buffer position, or nil when out of range."
  (let ((m (nth (1- n) emacs-occur--matches)))
    (when (and m (buffer-live-p emacs-occur--source-buffer))
      (let ((pos (plist-get m :pos)))
        (set-buffer emacs-occur--source-buffer)
        (goto-char pos)
        pos))))

;;;; --- replace ------------------------------------------------------

(defun emacs-replace-regexp (regexp to-string &optional buffer)
  "Replace every REGEXP match in BUFFER with TO-STRING; return the count.
TO-STRING honours `replace-regexp-in-string' backslash constructs."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((text (buffer-string))
           (count (emacs-replace--count regexp text)))
      (when (> count 0)
        (let ((new (replace-regexp-in-string regexp to-string text)))
          (erase-buffer)
          (insert new)))
      count)))

(defun emacs-replace-string (from-string to-string &optional buffer)
  "Replace every literal FROM-STRING in BUFFER with TO-STRING; return count."
  (emacs-replace-regexp (regexp-quote from-string) to-string buffer))

(defun emacs-replace-how-many (regexp &optional buffer)
  "Return the number of REGEXP matches in BUFFER (default current)."
  (with-current-buffer (or buffer (current-buffer))
    (emacs-replace--count regexp (buffer-string))))

;;;; --- line filters -------------------------------------------------

(defun emacs-replace--filter-lines (regexp keep buffer)
  "Rewrite BUFFER keeping lines that match REGEXP when KEEP, else dropping them.
Return the number of lines removed."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((text (buffer-string))
           (had-final-nl (and (> (length text) 0)
                              (string-suffix-p "\n" text)))
           (lines (split-string text "\n"))
           ;; split-string on a trailing newline leaves a final "" entry; drop
           ;; it (via `reverse', avoiding `last'/`butlast' for reader safety)
           (lines (if (and had-final-nl lines)
                      (let ((rev (reverse lines)))
                        (if (equal "" (car rev))
                            (reverse (cdr rev))
                          lines))
                    lines))
           (removed 0)
           (kept nil))
      (dolist (line lines)
        (let ((match (string-match regexp line)))
          (if (if keep match (not match))
              (push line kept)
            (setq removed (1+ removed)))))
      (setq kept (nreverse kept))
      (erase-buffer)
      (insert (mapconcat #'identity kept "\n"))
      (when (and kept had-final-nl)
        (insert "\n"))
      removed)))

(defun emacs-replace-flush-lines (regexp &optional buffer)
  "Delete lines matching REGEXP from BUFFER; return the count removed."
  (emacs-replace--filter-lines regexp nil buffer))

(defun emacs-replace-keep-lines (regexp &optional buffer)
  "Keep only lines matching REGEXP in BUFFER; return the count removed."
  (emacs-replace--filter-lines regexp t buffer))

;;;; --- query-replace ------------------------------------------------

;; The engine is decision-driven: `emacs-query-replace-region' walks the
;; matches and, for each, calls a DECIDE function returning a symbol --
;; `act' (replace), `skip', `act-all' (replace this and the rest), or `quit'.
;; Interactive `query-replace' supplies a key-reading decider; tests inject a
;; canned decision sequence.  This keeps the replacement logic headless-testable
;; while still supporting a real y/n/!/q prompt.

(defun emacs-query-replace--search (regexp from)
  "Return (BEG . END) of the next REGEXP match at/after buffer position FROM.
Uses `string-match' over the buffer text (the reader lacks buffer search)."
  (let* ((base (point-min))
         (text (buffer-substring-no-properties base (point-max)))
         (idx (string-match regexp text (max 0 (- from base)))))
    (when idx
      (cons (+ base idx) (+ base (match-end 0))))))

(defun emacs-query-replace--expand (to-string regexp matched)
  "Return TO-STRING with backslash constructs expanded against MATCHED.
MATCHED is exactly one REGEXP match; literal TO-STRING is returned as is."
  (if (string-match-p "\\\\[0-9&]" to-string)
      (replace-regexp-in-string regexp to-string matched)
    to-string))

(defun emacs-query-replace--read-decision (matched to-string)
  "Prompt about MATCHED / TO-STRING and read one key; return a decision symbol."
  (let ((char (if (fboundp 'read-char)
                  (progn
                    (when (fboundp 'message)
                      (message "Query replacing %s with %s (y/n/!/q): "
                               matched to-string))
                    (read-char))
                ?q)))
    (cond ((memq char '(?y ?\s)) 'act)
          ((memq char '(?n ?\d)) 'skip)
          ((eq char ?!) 'act-all)
          (t 'quit))))

(defun emacs-query-replace-region (regexp to-string &optional decide buffer)
  "Query-replace REGEXP with TO-STRING from point, asking DECIDE per match.
DECIDE is called as (MATCHED BEG END) and returns `act', `skip',
`act-all', or `quit'; nil DECIDE reads keys interactively.  Returns the
number of replacements made."
  (with-current-buffer (or buffer (current-buffer))
    (let ((count 0) (replace-all nil) (continue t)
          (decide (or decide
                      (lambda (m _b _e)
                        (emacs-query-replace--read-decision m to-string)))))
      (while continue
        (let ((m (emacs-query-replace--search regexp (point))))
          (if (not m)
              (setq continue nil)
            (let* ((beg (car m)) (end (cdr m))
                   (matched (buffer-substring-no-properties beg end))
                   (decision (if replace-all 'act
                               (funcall decide matched beg end)))
                   ;; ensure forward progress even on a zero-width match
                   (past (if (> end beg) end (min (point-max) (1+ beg)))))
              (cond
               ((eq decision 'quit) (setq continue nil))
               ((memq decision '(act act-all))
                (when (eq decision 'act-all) (setq replace-all t))
                (let ((rep (emacs-query-replace--expand to-string regexp matched)))
                  (goto-char beg)
                  (delete-region beg end)
                  (insert rep)
                  (setq count (1+ count))
                  (goto-char (max (+ beg (length rep)) past))))
               (t (goto-char past)))))))
      count)))

(defun emacs-query-replace (from-string to-string &optional decide buffer)
  "Query-replace literal FROM-STRING with TO-STRING.  Return the count."
  (interactive "sQuery replace: \nsQuery replace %s with: ")
  (emacs-query-replace-region (regexp-quote from-string) to-string decide buffer))

(defun emacs-query-replace-regexp (regexp to-string &optional decide buffer)
  "Query-replace REGEXP with TO-STRING (backrefs honoured).  Return the count."
  (interactive "sQuery replace regexp: \nsQuery replace regexp %s with: ")
  (emacs-query-replace-region regexp to-string decide buffer))

;;;; --- stateful query-replace session -------------------------------

(defun emacs-query-replace-session--put (session key value)
  "Return SESSION with KEY set to VALUE."
  (plist-put session key value))

(defun emacs-query-replace-session-count (session)
  "Return SESSION replacement count."
  (or (plist-get session :count) 0))

(defun emacs-query-replace-session-active-p (session)
  "Return non-nil when SESSION is waiting at a match."
  (and (plist-get session :active)
       (plist-get session :match-beg)
       (plist-get session :match-end)))

(defun emacs-query-replace-session-prompt (session)
  "Return the prompt for SESSION's current match."
  (format "Replace %s with %s? (y/n/!/q)"
          (or (plist-get session :from) "")
          (or (plist-get session :to) "")))

(defun emacs-query-replace-session-message (session)
  "Return a UI-facing status message for SESSION."
  (cond
   ((emacs-query-replace-session-active-p session)
    (emacs-query-replace-session-prompt session))
   ((eq (plist-get session :done-reason) 'quit)
    (format "query-replace: quit (%d done)"
            (emacs-query-replace-session-count session)))
   ((eq (plist-get session :done-reason) 'act-all)
    (format "Replaced %d (! all)"
            (emacs-query-replace-session-count session)))
   (t
    (format "Replaced %d occurrence%s"
            (emacs-query-replace-session-count session)
            (if (= (emacs-query-replace-session-count session) 1)
                ""
              "s")))))

(defun emacs-query-replace-session-decision (event)
  "Return a query-replace decision symbol for EVENT.
y and SPC mean `act', n/DEL/backspace mean `skip', ! means
`act-all', and q/C-g/RET/escape mean `quit'.  Unknown input returns
`reask'."
  (cond
   ((or (eq event ?y) (eq event ?\s) (equal event "y")
        (equal event "SPC"))
    'act)
   ((or (eq event ?n) (eq event ?\d) (eq event 127)
        (eq event 'backspace) (equal event "n") (equal event "DEL")
        (equal event "backspace"))
    'skip)
   ((or (eq event ?!) (equal event "!"))
    'act-all)
   ((or (eq event ?q) (eq event ?\r) (eq event 7) (eq event 'return)
        (eq event 'escape) (equal event "q") (equal event "RET")
        (equal event "C-g"))
    'quit)
   (t 'reask)))

(defun emacs-query-replace-session--regexp (session)
  "Return SESSION search regexp."
  (if (plist-get session :regexp-p)
      (plist-get session :from)
    (regexp-quote (or (plist-get session :from) ""))))

(defun emacs-query-replace-session--advance (session)
  "Advance SESSION to the next match and return it."
  (let ((from (or (plist-get session :from) "")))
    (if (equal from "")
        (progn
          (setq session
                (emacs-query-replace-session--put session :active nil))
          (setq session
                (emacs-query-replace-session--put session :done-reason 'empty)))
      (with-current-buffer (plist-get session :buffer)
        (let ((match
               (emacs-query-replace--search
                (emacs-query-replace-session--regexp session)
                (or (plist-get session :pos) (point)))))
          (if match
              (progn
                (goto-char (car match))
                (setq session
                      (emacs-query-replace-session--put
                       session :match-beg (car match)))
                (setq session
                      (emacs-query-replace-session--put
                       session :match-end (cdr match)))
                (setq session
                      (emacs-query-replace-session--put session :active t))
                (setq session
                      (emacs-query-replace-session--put
                       session :done-reason nil)))
            (setq session
                  (emacs-query-replace-session--put session :match-beg nil))
            (setq session
                  (emacs-query-replace-session--put session :match-end nil))
            (setq session
                  (emacs-query-replace-session--put session :active nil))
            (setq session
                  (emacs-query-replace-session--put session :done-reason 'done)))))))
  session)

(defun emacs-query-replace-session-start
    (from-string to-string &optional regexp-p buffer start)
  "Start a stateful query-replace session.
FROM-STRING is literal unless REGEXP-P is non-nil.  TO-STRING is the
replacement.  BUFFER defaults to the current buffer and START defaults
to point.  The returned plist is mutable state for
`emacs-query-replace-session-handle-decision'."
  (let ((session (list :from (or from-string "")
                       :to (or to-string "")
                       :regexp-p regexp-p
                       :buffer (or buffer (current-buffer))
                       :pos (or start (point))
                       :count 0
                       :active nil
                       :match-beg nil
                       :match-end nil
                       :done-reason nil)))
    (emacs-query-replace-session--advance session)))

(defun emacs-query-replace-session--replace-current (session)
  "Replace SESSION's current match and return the updated session."
  (with-current-buffer (plist-get session :buffer)
    (let* ((beg (plist-get session :match-beg))
           (end (plist-get session :match-end))
           (regexp (emacs-query-replace-session--regexp session))
           (matched (buffer-substring-no-properties beg end))
           (replacement
            (emacs-query-replace--expand
             (or (plist-get session :to) "") regexp matched)))
      (goto-char beg)
      (delete-region beg end)
      (insert replacement)
      (setq session
            (emacs-query-replace-session--put
             session :pos (+ beg (length replacement))))
      (setq session
            (emacs-query-replace-session--put
             session :count (1+ (emacs-query-replace-session-count session))))
      session)))

(defun emacs-query-replace-session--skip-current (session)
  "Skip SESSION's current match and return the updated session."
  (with-current-buffer (plist-get session :buffer)
    (let* ((beg (plist-get session :match-beg))
           (end (plist-get session :match-end))
           (next (if (> end beg) end (min (point-max) (1+ beg)))))
      (emacs-query-replace-session--put session :pos next))))

(defun emacs-query-replace-session-handle-decision (session decision)
  "Apply DECISION to SESSION and return the updated session.
DECISION is one of `act', `skip', `act-all', `quit', or `reask'."
  (cond
   ((not (emacs-query-replace-session-active-p session))
    session)
   ((eq decision 'quit)
    (setq session (emacs-query-replace-session--put session :active nil))
    (setq session (emacs-query-replace-session--put session :match-beg nil))
    (setq session (emacs-query-replace-session--put session :match-end nil))
    (emacs-query-replace-session--put session :done-reason 'quit))
   ((eq decision 'reask)
    session)
   ((eq decision 'skip)
    (emacs-query-replace-session--advance
     (emacs-query-replace-session--skip-current session)))
   ((eq decision 'act)
    (emacs-query-replace-session--advance
     (emacs-query-replace-session--replace-current session)))
   ((eq decision 'act-all)
    (while (emacs-query-replace-session-active-p session)
      (setq session
            (emacs-query-replace-session--advance
             (emacs-query-replace-session--replace-current session))))
    (emacs-query-replace-session--put session :done-reason 'act-all))
   (t session)))

(defun emacs-query-replace-session-handle-key (session event)
  "Apply query-replace EVENT to SESSION and return the updated session."
  (emacs-query-replace-session-handle-decision
   session
   (emacs-query-replace-session-decision event)))

(defun emacs-query-replace--run-command-session
    (from to regexp-p current-buffer-function start-function)
  "Start a query-replace command session from frontend hooks."
  (let* ((buffer (if current-buffer-function
                     (funcall current-buffer-function)
                   (current-buffer)))
         (start (if start-function
                    (funcall start-function)
                  (point))))
    (emacs-query-replace-session-start from to regexp-p buffer start)))

;;;###autoload
(defun emacs-query-replace-run-command (&rest plist)
  "Run a frontend query-replace command through the shared session engine.
PLIST accepts `:read-string', `:begin-prompt', `:read-confirmation',
`:from-prompt', `:to-prompt-function', `:decision', `:regexp-p',
`:current-buffer', `:start-function', `:after-success',
`:state-function', `:pending-function', and `:status-function'.

`:read-string' is the synchronous prompt path used by TUI callers.
`:begin-prompt' starts a callback prompt as (PROMPT CALLBACK) and keeps
the returned query-replace session stateful for GUI dispatch loops."
  (let* ((read-string (plist-get plist :read-string))
         (begin-prompt (plist-get plist :begin-prompt))
         (read-confirmation (plist-get plist :read-confirmation))
         (from-prompt (or (plist-get plist :from-prompt)
                          "Query replace: "))
         (to-prompt-function
          (or (plist-get plist :to-prompt-function)
              (lambda (from)
                (format "Query replace %s with: " from))))
         (decision (or (plist-get plist :decision) 'act-all))
         (regexp-p (plist-get plist :regexp-p))
         (current-buffer-function (plist-get plist :current-buffer))
         (start-function (plist-get plist :start-function))
         (after-success (plist-get plist :after-success))
         (state-function (plist-get plist :state-function))
         (pending-function (plist-get plist :pending-function))
         (status-function (plist-get plist :status-function)))
    (cond
     (begin-prompt
      (funcall
       begin-prompt from-prompt
       (lambda (from)
         (cond
          ((or (null from) (= (length from) 0))
           (when status-function
             (funcall status-function "query-replace: empty FROM")))
          (t
           (funcall
            begin-prompt (funcall to-prompt-function from)
            (lambda (to)
              (let* ((session
                      (emacs-query-replace--run-command-session
                       from (or to "") regexp-p
                       current-buffer-function start-function))
                     (active (emacs-query-replace-session-active-p session)))
                (when state-function
                  (funcall state-function session))
                (when pending-function
                  (funcall pending-function active))
                (when status-function
                  (funcall status-function
                           (emacs-query-replace-session-message session)))
                (when after-success
                  (funcall after-success session))
                session))))))))
     (t
      (let ((from (and read-string (funcall read-string from-prompt))))
        (when (and from (> (length from) 0))
          (let ((to (funcall read-string (funcall to-prompt-function from))))
            (when to
              (when read-confirmation
                (funcall read-confirmation 1000))
              (let ((session
                     (emacs-query-replace--run-command-session
                      from to regexp-p
                      current-buffer-function start-function)))
                (setq session
                      (emacs-query-replace-session-handle-decision
                       session decision))
                (when after-success
                  (funcall after-success session))
                (emacs-query-replace-session-count session))))))))))

;;;; --- standard-name facade -----------------------------------------

(defun emacs-replace-install ()
  "Bind the standard occur / replace command names to the `emacs-*'
implementations.  Not run on `require' (keeps a bare load from touching
shared command symbols)."
  (defalias 'occur #'emacs-occur)
  (defalias 'how-many #'emacs-replace-how-many)
  (defalias 'replace-regexp #'emacs-replace-regexp)
  (defalias 'replace-string #'emacs-replace-string)
  (defalias 'flush-lines #'emacs-replace-flush-lines)
  (defalias 'keep-lines #'emacs-replace-keep-lines)
  (defalias 'query-replace #'emacs-query-replace)
  (defalias 'query-replace-regexp #'emacs-query-replace-regexp))

(provide 'emacs-replace)

;;; emacs-replace.el ends here
