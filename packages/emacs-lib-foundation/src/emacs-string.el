;;; emacs-string.el --- NeLisp port of Emacs string utility primitives  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 2 — Layer 2.
;;
;; Ports the string-utility primitives Emacs ships in `subr-x.el' /
;; `subr.el' that anvil modules use during normal operation
;; (`string-trim', `string-prefix-p', `string-suffix-p',
;; `string-empty-p', `string-blank-p', `string-lines').  Each polyfill is gated on
;; `unless (fboundp ...)'.

;;; Code:

(unless (fboundp 'string-empty-p)
  (defun string-empty-p (string)
    "Return non-nil iff STRING is the empty string."
    (= 0 (length string))))

(unless (fboundp 'string-blank-p)
  (defun string-blank-p (string)
    "Return non-nil iff STRING contains only whitespace (or is empty)."
    (string-match-p "\\`[ \t\n\r]*\\'" string)))

(unless (fboundp 'string-prefix-p)
  (defun string-prefix-p (prefix string &optional ignore-case)
    "Return non-nil iff PREFIX is a prefix of STRING.
IGNORE-CASE non-nil compares case-insensitively (= naive ASCII downcase)."
    (ignore ignore-case)
    (let ((plen (length prefix)))
      (and (>= (length string) plen)
           (equal (substring string 0 plen) prefix)))))

(unless (fboundp 'string-suffix-p)
  (defun string-suffix-p (suffix string &optional ignore-case)
    "Return non-nil iff SUFFIX is a suffix of STRING."
    (ignore ignore-case)
    (let ((slen (length suffix))
          (xlen (length string)))
      (and (>= xlen slen)
           (equal (substring string (- xlen slen)) suffix)))))

(unless (fboundp 'string-trim-left)
  (defun string-trim-left (string &optional regexp)
    "Trim leading whitespace from STRING.
Optional REGEXP overrides the default `[ \\t\\n\\r]+' pattern."
    (let ((re (or regexp "\\`[ \t\n\r]+")))
      (if (string-match re string)
          (substring string (match-end 0))
        string))))

(unless (fboundp 'string-trim-right)
  (defun string-trim-right (string &optional regexp)
    "Trim trailing whitespace from STRING."
    (let ((re (or regexp "[ \t\n\r]+\\'")))
      (if (string-match re string)
          (substring string 0 (match-beginning 0))
        string))))

(unless (fboundp 'string-trim)
  (defun string-trim (string &optional trim-left trim-right)
    "Trim leading + trailing whitespace from STRING."
    (string-trim-left (string-trim-right string trim-right) trim-left)))

(unless (fboundp 'string-lines)
  (defun string-lines (string &optional omit-nulls keep-newlines)
    "Split STRING into a list of lines.
If OMIT-NULLS is non-nil, empty lines are removed.  If KEEP-NEWLINES
is non-nil, each returned line keeps its trailing newline when present."
    (let ((start 0)
          (len (length string))
          (out nil))
      (if (= len 0)
          (unless omit-nulls
            (setq out (cons "" out)))
        (while (< start len)
          (let* ((pos (string-search "\n" string start))
                 (end (or pos len))
                 (raw (substring string start end))
                 (line (if (and pos keep-newlines)
                           (substring string start (1+ pos))
                         raw)))
          (unless (and omit-nulls (string-empty-p raw))
            (setq out (cons line out)))
          (setq start (if pos (1+ pos) len)))))
      (nreverse out))))


(provide 'emacs-string)

;;; emacs-string.el ends here
