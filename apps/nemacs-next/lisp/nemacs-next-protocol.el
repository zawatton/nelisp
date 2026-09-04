;;; nemacs-next-protocol.el --- Protocol encoding for nemacs-next  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Minimal protocol encoder for the app/session boundary.  The encoder is
;; deliberately small and dependency-light so it can run in the NeLisp
;; persistent session smoke before a richer JSON library is selected.

;;; Code:

(require 'nemacs-next-session)

(defun nemacs-next-protocol--symbol-json-name (symbol)
  "Return SYMBOL's JSON field/value name."
  (let ((name (symbol-name symbol)))
    (if (and (> (length name) 0)
             (= (aref name 0) ?:))
        (substring name 1)
      name)))

(defun nemacs-next-protocol-json-escape (string)
  "Return STRING escaped as a JSON string body."
  (let ((i 0)
        (out ""))
    (while (< i (length string))
      (let ((ch (aref string i)))
        (setq out
              (concat
               out
               (cond
                ((= ch ?\") "\\\"")
                ((= ch ?\\) "\\\\")
                ((= ch ?\n) "\\n")
                ((= ch ?\r) "\\r")
                ((= ch ?\t) "\\t")
                (t (string ch))))))
      (setq i (+ i 1)))
    out))

(defun nemacs-next-protocol--json-string (string)
  "Return STRING encoded as a JSON string."
  (concat "\"" (nemacs-next-protocol-json-escape string) "\""))

(defun nemacs-next-protocol--plist-p (value)
  "Return non-nil when VALUE has plist shape."
  (and (consp value)
       (symbolp (car value))
       (> (length (symbol-name (car value))) 0)
       (= (aref (symbol-name (car value)) 0) ?:)))

(defun nemacs-next-protocol-encode (value)
  "Encode VALUE as a compact JSON value string.
Supported values are strings, numbers, symbols, nil, plists, and lists."
  (cond
   ((null value) "null")
   ((stringp value) (nemacs-next-protocol--json-string value))
   ((numberp value) (number-to-string value))
   ((symbolp value)
    (nemacs-next-protocol--json-string
     (nemacs-next-protocol--symbol-json-name value)))
   ((nemacs-next-protocol--plist-p value)
    (let ((cur value)
          (first t)
          (out "{"))
      (while cur
        (unless first
          (setq out (concat out ",")))
        (setq first nil)
        (setq out
              (concat
               out
               (nemacs-next-protocol--json-string
                (nemacs-next-protocol--symbol-json-name (car cur)))
               ":"
               (nemacs-next-protocol-encode (car (cdr cur)))))
        (setq cur (cdr (cdr cur))))
      (concat out "}")))
   ((listp value)
    (let ((cur value)
          (first t)
          (out "["))
      (while cur
        (unless first
          (setq out (concat out ",")))
        (setq first nil)
        (setq out (concat out (nemacs-next-protocol-encode (car cur))))
        (setq cur (cdr cur)))
      (concat out "]")))
   (t
    (nemacs-next-protocol--json-string (format "%S" value)))))

(defun nemacs-next-protocol-encode-line (value)
  "Encode VALUE as one newline-terminated protocol message."
  (concat (nemacs-next-protocol-encode value) "\n"))

(defun nemacs-next-protocol-append-line (lines value)
  "Return LINES with VALUE appended as one protocol line."
  (concat (or lines "") (nemacs-next-protocol-encode-line value)))

(defun nemacs-next-protocol-handle-message-line (message)
  "Handle MESSAGE and return one encoded response line."
  (nemacs-next-protocol-encode-line
   (nemacs-next-session-handle-message message)))

(provide 'nemacs-next-protocol)

;;; nemacs-next-protocol.el ends here
