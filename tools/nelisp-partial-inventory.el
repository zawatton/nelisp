;;; nelisp-partial-inventory.el --- arguments accepted and ignored -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The worst defects fixed on 2026-08-19 were all the same shape: a function
;; that takes an argument Emacs defines, ignores it, and ANSWERS.
;;
;;   string-trim-left  REGEXP     ignored -> stripped whitespace instead
;;   assoc-string      CASE-FOLD  ignored -> case-sensitive lookup, nil
;;   replace-regexp-in-string     recognised one literal regexp, returned
;;                                the input unchanged for every other
;;   format-message               did no curving at all
;;   upcase                       ASCII only, non-ASCII passed through
;;
;; None signalled.  Each returned a plausible value, so the caller had no
;; way to learn that the thing it asked for did not happen -- and each was
;; found by hand, late, a long way from where it was introduced.
;;
;; The tell is mechanical: a parameter named `_something' in a function whose
;; NAME stock Emacs also defines.  The underscore is the author writing down
;; "I am ignoring this" in the one place nobody reads.  This makes the list
;; visible and requires every site to be acknowledged in
;; tools/partial-accepted.txt with a reason.
;;
;; Acknowledging is not fixing, and the file says so per entry.  What it buys
;; is that a NEW one cannot appear silently, and that the count is a number
;; somebody can drive down.
;;
;; Run: make partial-inventory

;;; Code:

(require 'cl-lib)

(defconst nelisp-partial--table "docs/emacs-compat-table.txt")
(defconst nelisp-partial--accepted "tools/partial-accepted.txt")
(defconst nelisp-partial--dirs '("lisp" "scripts"))

(defun nelisp-partial--shared ()
  "Names stock Emacs also defines, as a hash set."
  (let ((h (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents nelisp-partial--table)
      (goto-char (point-min))
      (while (re-search-forward "^shared-\\(?:shadowing\\|deferring\\) +\\([^ ]+\\)" nil t)
        (puthash (match-string 1) t h)))
    h))

(defun nelisp-partial--sites ()
  "Return (NAME FILE ARGS) for every shared name that ignores a parameter."
  (let ((shared (nelisp-partial--shared))
        (sites nil))
    (dolist (dir nelisp-partial--dirs)
      (dolist (f (directory-files dir t "\\.el\\'"))
        (with-temp-buffer
          (insert-file-contents f)
          (goto-char (point-min))
          (while (re-search-forward "^ *(defun \\([^ ()]+\\) (\\([^)]*\\))" nil t)
            (let* ((name (match-string 1))
                   (args (match-string 2))
                   (ignored (cl-remove-if-not
                             (lambda (a) (and (> (length a) 1)
                                              (string-prefix-p "_" a)))
                             (split-string args "[ \t\n]+" t))))
              (when (and (gethash name shared) ignored)
                (push (list name (file-relative-name f)
                            (mapconcat #'identity (sort ignored #'string<) " "))
                      sites)))))))
    (sort (nreverse sites)
          (lambda (a b) (string< (concat (nth 0 a) (nth 1 a))
                                 (concat (nth 0 b) (nth 1 b)))))))

(defun nelisp-partial--key (site)
  (mapconcat #'identity (list (nth 0 site) (nth 1 site) (nth 2 site)) "|"))

(defun nelisp-partial--accepted-rows ()
  "Return a hash of KEY -> REASON from the accepted file."
  (let ((h (make-hash-table :test 'equal)))
    (when (file-exists-p nelisp-partial--accepted)
      (with-temp-buffer
        (insert-file-contents nelisp-partial--accepted)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring (line-beginning-position)
                                                     (line-end-position)))))
            (unless (or (string-empty-p line) (string-prefix-p "#" line))
              (let ((parts (split-string line "|")))
                (when (>= (length parts) 3)
                  (puthash (mapconcat #'identity (cl-subseq parts 0 3) "|")
                           (or (nth 3 parts) "") h)))))
          (forward-line 1))))
    h))

(defconst nelisp-partial--unreviewed "not yet reviewed"
  "Reason text that means nobody has looked at this one.")

(defun nelisp-partial-run ()
  (let* ((sites (nelisp-partial--sites))
         (accepted (nelisp-partial--accepted-rows))
         (new nil) (stale nil) (unreviewed 0))
    (dolist (s sites)
      (let* ((key (nelisp-partial--key s))
             (reason (gethash key accepted)))
        (cond ((null reason) (push s new))
              ((or (string-empty-p reason)
                   (string= reason nelisp-partial--unreviewed))
               (setq unreviewed (1+ unreviewed))))))
    (let ((live (mapcar #'nelisp-partial--key sites)))
      (maphash (lambda (k _v) (unless (member k live) (push k stale))) accepted))
    (princ (format "partial-inventory: %d site(s) accept an Emacs argument and ignore it\n"
                   (length sites)))
    (princ (format "  %d acknowledged with a reason, %d acknowledged but not reviewed\n"
                   (- (length sites) (length new) unreviewed) unreviewed))
    (when new
      (princ (format "\n%d site(s) not in %s:\n" (length new) nelisp-partial--accepted))
      (dolist (s new)
        (princ (format "  %-28s %-34s %s\n" (nth 0 s) (nth 1 s) (nth 2 s)))))
    (when stale
      (princ (format "\n%d accepted entr(ies) no longer match anything -- the argument is\nhandled now, or the function moved.  Drop them:\n" (length stale)))
      (dolist (k (sort stale #'string<)) (princ (format "  %s\n" k))))
    ;; `checked' is the number of DEFUNS scanned, not the findings: a scan
    ;; that stopped finding files would otherwise report zero partials and
    ;; read as a clean tree.
    (princ (format "GATE-COUNT checked=%d findings=%d\n"
                   (length sites) (+ (length new) (length stale))))
    (cond
     ((zerop (length sites))
      (princ "partial-inventory: FAIL (no sites found at all -- the scan is not reaching the tree)\n")
      (kill-emacs 1))
     (new
      (princ (format "partial-inventory: FAIL (%d site(s) ignore an Emacs argument without saying so -- either handle the argument or add the site to %s with what it does instead)\n"
                     (length new) nelisp-partial--accepted))
      (kill-emacs 1))
     (stale
      (princ (format "partial-inventory: FAIL (%d accepted entr(ies) describe nothing -- drop them so the file describes the tree)\n"
                     (length stale)))
      (kill-emacs 1))
     (t (princ "partial-inventory: PASS\n")))))

(nelisp-partial-run)

;;; nelisp-partial-inventory.el ends here
