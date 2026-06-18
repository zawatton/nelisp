;;; emacs-man.el --- minimal man-page viewer  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 2 (nelisp-emacs) manual-page viewer: run `man TOPIC' through the
;; `call-process' substrate, strip the overstrike formatting with `col -b',
;; and show the result in a `*Man TOPIC*' buffer.
;;
;; This fits the standalone reader: `man' is an external command and
;; `call-process' works there (a held-open interactive subprocess does not).
;; `woman' (normally a no-subprocess nroff formatter) is provided as the same
;; viewer here -- the user-facing behaviour (display a man page) is identical;
;; the pure-Elisp nroff path is a later target.
;;
;; Boundary (../nelisp-gui/docs/design/00-three-layer-architecture.org):
;;   - nelisp-emacs OWNS: the man invocation, output cleanup, and buffer.
;;   - nelisp-gui OWNS: rendering + key transport.

;;; Code:

(defvar emacs-man-program "man"
  "Program used to fetch manual pages.")

(defvar emacs-man-width 80
  "Column width requested from man (via MANWIDTH).")

(defvar emacs-man-shell "/bin/sh"
  "Shell used to run the man pipeline.")

(defun emacs-man--sh-quote (string)
  "Single-quote STRING for /bin/sh (adequate for a man topic)."
  (concat "'" string "'"))

(defun emacs-man--buffer-name (topic)
  "Return the buffer name for TOPIC's manual page."
  (format "*Man %s*" topic))

(defun emacs-man--nonblank-p (string)
  "Non-nil when STRING contains a non-whitespace character."
  (and (stringp string) (string-match-p "[^ \t\n\r\f\v]" string)))

(defun emacs-man--fetch (topic)
  "Run `man TOPIC' and return the cleaned page text, or nil when not found."
  (when (fboundp 'call-process)
    (with-temp-buffer
      (call-process emacs-man-shell nil t nil "-c"
                    (format "MANWIDTH=%d %s %s 2>/dev/null | col -b"
                            emacs-man-width emacs-man-program
                            (emacs-man--sh-quote topic)))
      (let ((out (buffer-string)))
        (and (emacs-man--nonblank-p out) out)))))

;;;; --- mode + command -----------------------------------------------

(defun emacs-man-mode ()
  "Major mode for a manual-page buffer (read-only display)."
  (interactive)
  (when (fboundp 'kill-all-local-variables)
    (kill-all-local-variables))
  (setq major-mode 'Man-mode
        mode-name "Man")
  nil)

(defun emacs-man (topic)
  "Display the manual page for TOPIC in a `*Man TOPIC*' buffer.
Returns the buffer; signals an error when no page is found."
  (interactive "sManual entry: ")
  (let ((text (emacs-man--fetch topic)))
    (unless text
      (error "No manual entry for %s" topic))
    (let ((buf (get-buffer-create (emacs-man--buffer-name topic))))
      (with-current-buffer buf
        (emacs-man-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text)
          (goto-char (point-min))))
      (when (fboundp 'display-buffer)
        (display-buffer buf))
      buf)))

;;;; --- standard-name facade -----------------------------------------

(defun emacs-man-install ()
  "Bind the standard `man' / `woman' / `Man-mode' command names to the
`emacs-man' implementation.  Not run on `require' (keeps a bare load from
touching shared command symbols)."
  (defalias 'man #'emacs-man)
  (defalias 'woman #'emacs-man)
  (defalias 'Man-mode #'emacs-man-mode))

(provide 'emacs-man)

;;; emacs-man.el ends here
