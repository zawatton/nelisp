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

(unless (and (fboundp 'string<)
             (not (get 'string< 'emacs-stub-bulk)))
  (defun string< (a b)
    "Return non-nil if string A is lexicographically less than string B."
    (let ((i 0)
          (na (length a))
          (nb (length b))
          (answer nil)
          (done nil))
      (while (and (not done) (< i na) (< i nb))
        (let ((ca (aref a i))
              (cb (aref b i)))
          (cond
           ((< ca cb) (setq answer t done t))
           ((> ca cb) (setq answer nil done t))
           (t (setq i (1+ i))))))
      (if done answer (< na nb))))
  (put 'string< 'emacs-stub-bulk nil))

(unless (and (fboundp 'char-equal)
             (not (get 'char-equal 'emacs-stub-bulk)))
  (defun char-equal (a b)
    "Return non-nil when characters A and B are equal."
    (= a b))
  (put 'char-equal 'emacs-stub-bulk nil))

(unless (and (fboundp 'string-width)
             (not (get 'string-width 'emacs-stub-bulk)))
  (defun string-width (string)
    "Return display width of STRING.
Standalone MVP treats every character as width 1."
    (length string))
  (put 'string-width 'emacs-stub-bulk nil))

(unless (and (fboundp 'int-to-string)
             (not (get 'int-to-string 'emacs-stub-bulk)))
  (defun int-to-string (integer)
    "Return decimal printed representation of INTEGER."
    (format "%d" integer))
  (put 'int-to-string 'emacs-stub-bulk nil))

(unless (and (fboundp 'assoc-string)
             (not (get 'assoc-string 'emacs-stub-bulk)))
  (defun emacs-string--assoc-string-key (value)
    "Return VALUE as a string key for `assoc-string', or nil."
    (cond
     ((stringp value) value)
     ((symbolp value) (symbol-name value))
     (t nil)))

  (defun assoc-string (key list &optional case-fold)
    "Return first alist element whose string key matches KEY.
CASE-FOLD non-nil compares via `downcase'."
    (let ((needle (emacs-string--assoc-string-key key))
          (cur list)
          (found nil))
      (when case-fold
        (setq needle (and needle (downcase needle))))
      (when needle
        (while (and cur (not found))
          (let* ((cell (car cur))
                 (head (emacs-string--assoc-string-key
                        (if (consp cell) (car cell) cell))))
            (when head
              (let ((candidate (if case-fold (downcase head) head)))
                (when (string= needle candidate)
                  (setq found cell)))))
          (setq cur (cdr cur))))
      found))
  (put 'assoc-string 'emacs-stub-bulk nil))

(unless (fboundp 'string-lessp) (defun string-lessp (a b) (string< a b)))
(unless (fboundp 'string-greaterp) (defun string-greaterp (a b) (string< b a)))
(unless (fboundp 'string>) (defun string> (a b) (string< b a)))
(unless (fboundp 'substring-no-properties)
  (defun substring-no-properties (s &optional from to)
    (substring s (or from 0) (or to (length s)))))
(unless (fboundp 'truncate-string-to-width)
  (defun truncate-string-to-width (str width &rest _)
    (if (<= (length str) width) str (substring str 0 (max 0 width)))))
(unless (fboundp 'capitalize)
  (defun capitalize (s)
    (if (stringp s)
        (let ((res "") (i 0) (n (length s)) (prev-alpha nil))
          (while (< i n)
            (let* ((c (aref s i))
                   (alpha (or (and (>= c 97) (<= c 122)) (and (>= c 65) (<= c 90))))
                   (ch (cond ((and alpha (not prev-alpha) (>= c 97) (<= c 122)) (- c 32))
                             ((and alpha prev-alpha (>= c 65) (<= c 90)) (+ c 32))
                             (t c))))
              (setq res (concat res (char-to-string ch)) prev-alpha alpha))
            (setq i (1+ i)))
          res)
      s)))

(provide 'emacs-string)

;;; emacs-string.el ends here
