;;; nl-ns-inventory.el --- CI namespace-boundary inventory via nl-ns -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Batch driver for `nl-ns': scan every .el under lisp/, src/, scripts/
;; and packages/*/src/, count the namespace findings per kind, and fail
;; when any kind EXCEEDS its checked-in baseline in
;; tools/ns-inventory-baseline.txt.
;;
;; Same ratchet as tools/nl-check-inventory.el: a count below the
;; baseline does not fail, it prints a notice so the baseline can be
;; lowered in the change that shrank the number.  The point is to stop
;; the numbers growing, not to demand a cleanup nobody scheduled.
;;
;; `ns-undeclared-dependency' is deliberately NOT counted here.  A tree
;; that has always relied on load order reports one finding per edge of
;; its real dependency graph, which is interesting to read once and
;; useless as a gate.  Pass a non-nil second argument to `nl-ns-check'
;; when you want to read it.
;;
;; `ns-collision' and `ns-collision-divergent' -- the two kinds
;; `nl-ns--check-collisions' produces -- are also pinned by identity, using
;; the same `nl-ns-finding-key' that `scripts/nl-ns-gate.el' uses for its
;; accepted set (kind + subject + sorted files, plus a digest of the
;; definitions when divergent).  A bare count cannot see one collision
;; resolved while a different one appears, net unchanged; the gate that
;; measured exactly that against `ns-gate' on 2026-08-21 is `gate-mutation'.
;; `ns-prefix-violation' and `ns-private-escape' stay count-only: at 853 and
;; 611 respectively they are two orders of magnitude larger, the private
;; escapes are an explicitly CONCEDED, growing debt rather than a curated
;; set (see the baseline file), and most of `ns-prefix-violation' is public
;; API surface already visible through `nl-check-gate' and `ns-gate' --
;; pinning each by identity would multiply the baseline eightfold for
;; findings this file's own history already treats as background noise to
;; be worked down later, not individual defects to catch on sight.
;;
;; Run from the repo root:
;;   emacs --batch -Q -L packages/nl-prelude/src -L packages/nl-ns/src \
;;     -l tools/nl-ns-inventory.el
;; or: make ns-inventory

;;; Code:

(require 'nl-ns)

(defconst nl-ns-inventory--baseline-file
  "tools/ns-inventory-baseline.txt")

(defconst nl-ns-inventory--kinds
  '(ns-collision ns-collision-divergent ns-prefix-violation
    ns-private-escape ns-unreadable)
  "Finding kinds this gate tracks, in report order.")

(defun nl-ns-inventory--baseline ()
  "Read the baseline as an alist of (KIND . COUNT), or nil when absent."
  (when (file-exists-p nl-ns-inventory--baseline-file)
    (with-temp-buffer
      (insert-file-contents nl-ns-inventory--baseline-file)
      (goto-char (point-min))
      (let ((rows nil))
        (while (not (eobp))
          (let* ((line (buffer-substring (line-beginning-position)
                                         (line-end-position)))
                 (space (string-match " " line)))
            (when (and space (> (length line) 0)
                       (not (eq (aref line 0) ?#))
                       (not (string-prefix-p "pinned-collision " line)))
              (setq rows
                    (cons (cons (intern (substring line 0 space))
                                (string-to-number (substring line space)))
                          rows))))
          (forward-line 1))
        (nreverse rows)))))

(defconst nl-ns-inventory--pinned-kinds '(ns-collision ns-collision-divergent)
  "Kinds pinned by identity, not only by count.")

(defun nl-ns-inventory--pins ()
  "Return the pinned collision keys as a hash set.
Read from `pinned-collision KEY' lines, KEY in `nl-ns-finding-key' shape."
  (let ((keys (make-hash-table :test #'equal)))
    (when (file-exists-p nl-ns-inventory--baseline-file)
      (with-temp-buffer
        (insert-file-contents nl-ns-inventory--baseline-file)
        (goto-char (point-min))
        (while (re-search-forward "^pinned-collision \\(.+\\)$" nil t)
          (puthash (match-string 1) t keys))))
    keys))

(defun nl-ns-inventory--files ()
  "Return the .el files this inventory covers."
  (append (file-expand-wildcards "lisp/*.el")
          (file-expand-wildcards "src/*.el")
          (file-expand-wildcards "scripts/*.el")
          (file-expand-wildcards "packages/*/src/*.el")))

(defun nl-ns-inventory-run ()
  "Scan the tree, print the inventory, enforce the baseline."
  (let* ((files (nl-ns-inventory--files))
         (findings (nl-ns-check-files files))
         (baseline (nl-ns-inventory--baseline))
         (pins (nl-ns-inventory--pins))
         (current-keys (make-hash-table :test #'equal))
         (new-collisions nil)
         (over nil))
    (princ (format "ns-inventory: %d files\n" (length files)))
    (dolist (kind nl-ns-inventory--kinds)
      (let* ((n (length (nl-ns-findings-of-kind findings kind)))
             (limit (cdr (assq kind baseline))))
        (princ (format "%6d  %-24s baseline=%s\n"
                       n kind (if limit limit "ABSENT")))
        (cond
         ((null limit) (setq over (cons kind over)))
         ((> n limit) (setq over (cons kind over)))
         ((< n limit)
          (princ (format "        ratchet available: %s is %d below baseline\n"
                         kind (- limit n)))))))
    (dolist (kind nl-ns-inventory--pinned-kinds)
      (dolist (finding (nl-ns-findings-of-kind findings kind))
        (let ((key (nl-ns-finding-key finding)))
          (puthash key t current-keys)
          (unless (gethash key pins) (push finding new-collisions)))))
    (setq new-collisions (nreverse new-collisions))
    ;; Machine-readable tail, printed before the verdict so it survives
    ;; every exit path (contract: tools/ai/README.md).
    (princ (format "GATE-COUNT checked=%d findings=%d\n"
                   (length files) (length findings)))
    (cond
     ((null baseline)
      (princ (format "ns-inventory: FAIL (no baseline file %s)\n"
                     nl-ns-inventory--baseline-file))
      (kill-emacs 1))
     (new-collisions
      (princ (format "ns-inventory: FAIL (%d collision(s) not in `pinned-collision' -- a per-kind count can miss one collision resolving while a different one appears, net unchanged; each of these is new):\n"
                     (length new-collisions)))
      (dolist (finding new-collisions)
        (princ (format "    %-24s %-40s %s\n"
                       (plist-get finding :kind)
                       (plist-get finding :subject)
                       (mapconcat #'identity (sort (copy-sequence
                                                    (plist-get finding :files))
                                                   #'string<)
                                  " "))))
      (princ (format "  Add a `pinned-collision' line for each in %s.\n"
                     nl-ns-inventory--baseline-file))
      (kill-emacs 1))
     (over
      (princ (format "ns-inventory: FAIL (%s over baseline -- a new cross-file collision, stray definition, or private-boundary reference; fix it or raise the baseline in the same commit)\n"
                     (mapconcat #'symbol-name (nreverse over) ", ")))
      (kill-emacs 1))
     (t
      (let ((stale 0))
        (maphash (lambda (k _v) (unless (gethash k current-keys) (setq stale (1+ stale))))
                 pins)
        (when (> stale 0)
          (princ (format "    %d `pinned-collision' line(s) no longer match anything -- safe to drop\n"
                         stale))))
      (princ "ns-inventory: PASS\n")))))

(nl-ns-inventory-run)

;;; nl-ns-inventory.el ends here
