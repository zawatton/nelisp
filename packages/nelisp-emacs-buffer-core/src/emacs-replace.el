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
