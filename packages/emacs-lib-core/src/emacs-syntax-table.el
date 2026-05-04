;;; emacs-syntax-table.el --- Minimal syntax-table for font-lock pre-pass  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track R (2026-05-04) — minimum-viable syntax-table that the
;; font-lock pre-pass uses to identify *string* and *comment* regions.
;;
;; A "syntax table" here is just a hash from char → class symbol.
;; The class set is intentionally narrow (= what the pre-pass
;; needs):
;;
;;   word           default; alphanumeric / symbol-constituent
;;   open / close   ( )
;;   string-fence   "
;;   escape         \   (only meaningful inside a string)
;;   comment-start  ;   (line comment, single char)
;;   comment-end    \n  (line comment terminator)
;;   whitespace     space, tab
;;
;; The complete upstream Emacs syntax-table grammar (= 16+ classes,
;; flag bits, paired comment delimiters, syntactic-keyword
;; overlays) is OUT of scope for Track R.  When a major-mode needs
;; a different per-class char, it builds a fresh hash and binds
;; `font-lock-syntax-table' (or analogous).
;;
;; The integration point for font-lock is
;; `emacs-syntax-apply-faces-region' which walks a region and
;; faces every string + comment range with `font-lock-string-face'
;; / `font-lock-comment-face' respectively.  Run this AFTER the
;; keyword pass so syntactic faces win over keyword fontification
;; in string / comment text.

;;; Code:

(require 'emacs-buffer)
(require 'emacs-faces)

;;;; --- standard table --------------------------------------------------------

(defvar emacs-syntax--standard-table
  (let ((tbl (make-hash-table :test 'eql)))
    (puthash ?\" 'string-fence tbl)
    (puthash ?\\ 'escape       tbl)
    (puthash ?\; 'comment-start tbl)
    (puthash ?\n 'comment-end  tbl)
    (puthash ?\( 'open         tbl)
    (puthash ?\) 'close        tbl)
    (puthash ?\s 'whitespace   tbl)
    (puthash ?\t 'whitespace   tbl)
    tbl)
  "Default syntax table used when no major-mode override is set.
A hash-table mapping integer CHAR → class symbol.  Chars not
present default to `word'.")

(defun emacs-syntax-class-of (char &optional table)
  "Return the syntax class symbol for CHAR in TABLE.
TABLE defaults to `emacs-syntax--standard-table'.  Chars not in
the table default to `word'."
  (or (gethash char (or table emacs-syntax--standard-table))
      'word))

(defun emacs-syntax-modify-entry (char class &optional table)
  "Set CHAR's syntax class to CLASS in TABLE (default = standard).
Returns CLASS.  If CLASS is nil, the entry is removed (= falls
back to `word')."
  (let ((tbl (or table emacs-syntax--standard-table)))
    (if class
        (puthash char class tbl)
      (remhash char tbl))
    class))

;;;; --- font-lock pre-pass ----------------------------------------------------

(defun emacs-syntax--char-at (buf pos)
  "Return the char at 1-based POS in BUF, or nil if out-of-range.
Implemented via `nelisp-ec-buffer-substring' which is the available
single-char accessor on the substrate (= no `char-after' yet)."
  (when (and (fboundp 'nelisp-ec-buffer-substring) buf)
    (let* ((nelisp-ec--current-buffer buf)
           (s (condition-case _
                  (nelisp-ec-buffer-substring pos (1+ pos))
                (error nil))))
      (and (stringp s) (> (length s) 0) (aref s 0)))))

(defun emacs-syntax-apply-faces-region (start end &optional buf table)
  "Walk BUF in [START, END) and face strings + line-comments.

Strings get `font-lock-string-face'; line comments get
`font-lock-comment-face'.  Use this AFTER the keyword pass so
syntactic faces overwrite any keyword face that fired inside a
string / comment.  No-op when neither buffer nor required
substrate is available (= host-driver fixture mode)."
  (when (and (fboundp 'nelisp-ec-buffer-substring)
             (fboundp 'emacs-buffer-put-text-property))
    (let* ((tbl (or table emacs-syntax--standard-table))
           (state 'code)
           (range-start nil)
           (escape nil)
           ;; Snapshot the region in one substrate call instead of
           ;; per-char (= O(n) substrate hops dropped to O(1)).
           (region (let ((nelisp-ec--current-buffer
                          (or buf (and (boundp 'nelisp-ec--current-buffer)
                                       nelisp-ec--current-buffer))))
                     (condition-case _
                         (nelisp-ec-buffer-substring start end)
                       (error nil))))
           (rlen (and region (length region)))
           (i 0))
      (while (and rlen (< i rlen))
        (let* ((ch (aref region i))
               (cls (emacs-syntax-class-of ch tbl))
               (abs-pos (+ start i)))
          (cond
           ;; In code: maybe enter string / comment.
           ((eq state 'code)
            (cond
             ((eq cls 'string-fence)
              (setq state 'string range-start abs-pos escape nil))
             ((eq cls 'comment-start)
              (setq state 'comment range-start abs-pos))))
           ;; In string: handle escape + closing fence.
           ((eq state 'string)
            (cond
             (escape (setq escape nil))
             ((eq cls 'escape) (setq escape t))
             ((eq cls 'string-fence)
              (emacs-buffer-put-text-property
               range-start (1+ abs-pos) 'face 'font-lock-string-face buf)
              (setq state 'code range-start nil))))
           ;; In comment: end-of-line closes it.
           ((eq state 'comment)
            (when (eq cls 'comment-end)
              (emacs-buffer-put-text-property
               range-start (1+ abs-pos) 'face 'font-lock-comment-face buf)
              (setq state 'code range-start nil)))))
        (setq i (1+ i)))
      ;; Unterminated open at end-of-region: face to end.
      (when range-start
        (emacs-buffer-put-text-property
         range-start end 'face
         (if (eq state 'string) 'font-lock-string-face
           'font-lock-comment-face)
         buf))
      nil)))

(defun emacs-syntax-state-at (pos &optional buf table)
  "Walk BUF from BOB to POS, returning the current syntactic state.
One of `code', `string', `comment'.  Used by syntactic-aware
matchers (= e.g. a keyword that should only fire in code)."
  (let* ((tbl (or table emacs-syntax--standard-table))
         (state 'code)
         (escape nil)
         (region (let ((nelisp-ec--current-buffer
                        (or buf (and (boundp 'nelisp-ec--current-buffer)
                                     nelisp-ec--current-buffer))))
                   (condition-case _
                       (nelisp-ec-buffer-substring 1 pos)
                     (error nil))))
         (rlen (and region (length region)))
         (i 0))
    (while (and rlen (< i rlen))
      (let* ((ch (aref region i))
             (cls (emacs-syntax-class-of ch tbl)))
        (cond
         ((eq state 'comment)
          (when (eq cls 'comment-end) (setq state 'code)))
         ((eq state 'string)
          (cond
           (escape (setq escape nil))
           ((eq cls 'escape) (setq escape t))
           ((eq cls 'string-fence) (setq state 'code))))
         (t
          (cond
           ((eq cls 'string-fence) (setq state 'string escape nil))
           ((eq cls 'comment-start) (setq state 'comment))))))
      (setq i (1+ i)))
    state))

(provide 'emacs-syntax-table)

;;; emacs-syntax-table.el ends here
