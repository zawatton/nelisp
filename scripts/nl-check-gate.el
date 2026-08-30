;;; nl-check-gate.el --- run the expansion-time checks over the tree  -*- lexical-binding: t; -*-

;;; Commentary:

;; `nl-check' owns the expansion-time checks -- `nl-must-use', resource
;; tracking, the unsafe surface.  Until now nothing ran them as a gate:
;; `make unsafe-inventory' counted one of the five kinds and the other
;; four -- must-use-discarded, resource-untracked, resource-leak,
;; resource-double -- were reported by no target at all.
;;
;; This runs them from `make compile', which is where a compile-time
;; error belongs.  It deliberately does NOT hook the byte-compiler
;; itself: Doc 170 section 9 requires that with checking disabled the
;; expansion be byte-identical to the plain version, and a separate
;; reading pass cannot affect the emitted code at all, so that property
;; holds by construction rather than by test.
;;
;; The checks read; they never evaluate what they read.
;;
;; Usage:
;;   emacs -Q --batch -L packages/nl-prelude/src -L packages/nl-safe/src \
;;     -L packages/nl-check/src -l scripts/nl-check-gate.el
;;
;; Regenerate the accepted set from a tree you have reviewed:
;;   NL_CHECK_GATE_WRITE=1 emacs -Q --batch ... -l scripts/nl-check-gate.el

;;; Code:

(require 'nl-check)

(defvar nl-check-gate-roots '("lisp" "src" "scripts" "packages")
  "Directories scanned, relative to the repository root.")

(defvar nl-check-gate-kinds
  '(must-use-discarded resource-untracked resource-leak resource-double)
  "Kinds this gate fails on.
`unsafe-call' is deliberately absent: `make unsafe-inventory' already
ratchets it against its own baseline, and counting it twice would mean
two files to update for one change.")

(defvar nl-check-gate-accepted-file "scripts/nl-check-accepted.el"
  "Where the accepted set lives.")

(defun nl-check-gate--files ()
  (let ((files nil))
    (dolist (root nl-check-gate-roots)
      (when (file-directory-p root)
        (setq files (append files (directory-files-recursively root "\\.el$")))))
    files))

(defun nl-check-gate--key (file finding)
  "Return a stable key for FINDING found in FILE.
Subject and kind rather than position: a finding that moves down the
file when a comment is added is the same finding."
  (format "%s\t%s\t%s" file (plist-get finding :kind)
          (plist-get finding :subject)))

(defun nl-check-gate--findings ()
  "Return (KEY . (FILE FINDING)) for every gated finding in the tree."
  (let ((out nil))
    (dolist (file (nl-check-gate--files))
      (dolist (finding (condition-case nil (nl-check-file-expanded file) (error nil)))
        (when (memq (plist-get finding :kind) nl-check-gate-kinds)
          (setq out (cons (cons (nl-check-gate--key file finding)
                                (list file finding))
                          out)))))
    (nreverse out)))

(defun nl-check-gate--accepted ()
  (let ((keys (make-hash-table :test 'equal)))
    (when (file-readable-p nl-check-gate-accepted-file)
      (with-temp-buffer
        (insert-file-contents nl-check-gate-accepted-file)
        (goto-char (point-min))
        (let ((entry (condition-case nil (read (current-buffer)) (error nil))))
          (dolist (key (plist-get entry :keys))
            (puthash key t keys)))))
    keys))

(defun nl-check-gate--write (findings)
  (let ((keys nil))
    (dolist (entry findings)
      (setq keys (cons (car entry) keys)))
    (setq keys (sort keys #'string<))
    (with-temp-buffer
      (insert ";; nl-check accepted findings -- generated, review before commit.\n")
      (insert ";; Do not hand-add keys to silence a finding; fix it or say why\n")
      (insert ";; in the commit that regenerates this file.\n")
      (prin1 (list :keys keys) (current-buffer))
      (insert "\n")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max)
                      nl-check-gate-accepted-file nil 'quiet)))
    (length keys)))

(let ((findings (nl-check-gate--findings)))
  (if (getenv "NL_CHECK_GATE_WRITE")
      (princ (format "wrote %d accepted key(s) to %s\n"
                     (nl-check-gate--write findings)
                     nl-check-gate-accepted-file))
    (let ((accepted (nl-check-gate--accepted))
          (new nil))
      (dolist (entry findings)
        (unless (gethash (car entry) accepted)
          (setq new (cons entry new))))
      (setq new (nreverse new))
      (princ (format "nl-check gate: %d gated finding(s), %d accepted\n"
                     (length findings) (hash-table-count accepted)))
      ;; Machine-readable tail, the contract in tools/ai/README.md: CHECKED is
      ;; every gated finding this run looked at, FINDINGS is the ones not in
      ;; the accepted set.  Without it a clean run and a run that scanned
      ;; nothing print the same thing to `nelisp-ai.sh gate'.
      (princ (format "GATE-COUNT checked=%d findings=%d\n"
                     (length findings) (length new)))
      (if (null new)
          (princ "nl-check gate: clean\n")
        (princ (format "\n%d finding(s) not in the accepted set:\n\n"
                       (length new)))
        (dolist (entry new)
          (let ((file (nth 0 (cdr entry)))
                (finding (nth 1 (cdr entry))))
            (princ (format "  %s: %s %s\n" file
                           (plist-get finding :kind)
                           (plist-get finding :subject)))))
        (kill-emacs 1)))))

;;; nl-check-gate.el ends here
