;;; nelisp-pkg-graph.el --- report the package graph, fail on a cycle -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Derives the cross-package dependency graph from `provide' / `require'
;; and prints it, with a load order when one exists.
;;
;; Four findings fail the build:
;;
;;   pkg-cycle                 a ring of requires has no load order at
;;                             all, so no care at the call site can work
;;                             around it
;;   pkg-invalid-manifest      a manifest that cannot be read is worse
;;                             than none, because it is believed
;;   pkg-undeclared-dependency the code depends on a package the
;;                             manifest does not name
;;   pkg-stale-dependency      the manifest names one the code no longer
;;                             uses
;;
;; The last two became gateable once every package had a manifest
;; (2026-08-18).  They are the whole point of having manifests: without
;; them a declaration is decoration.  Both are fixed by one command --
;; `make pkg-manifest-update' -- so the gate costs a keystroke rather
;; than an editing chore, which is the difference between a rule people
;; keep and one they route around.
;;
;; `pkg-missing-manifest' and `pkg-unresolved-require' are reported and
;; not gated: the first cannot fire today, and the second is mostly host
;; Emacs libraries, which would make the check red from birth.
;;
;; The scan walks `packages/' only, so for a long time this gate's "assumed
;; host" list could not contain a name from src/ or lisp/ -- the two
;; directories the standalone runtime is actually built out of.  Every host
;; library that has broken that runtime lived there and was invisible here:
;; subr-x, url-parse, then macroexp, seq and pcase, each found by bisecting
;; a require by hand after something failed a long way from it.  Those two
;; directories are now scanned as well, and the requires nothing in the tree
;; provides are split into hard and optional, because off-host an optional
;; require answers nil while a hard one stops the file and everything that
;; requires it.  The hard count is ratcheted against
;; tools/pkg-host-requires-baseline.txt, which also pins the hard requires by
;; (FEATURE . FILE): a count alone cannot see one hard require dropped while a
;; DIFFERENT one is added, net unchanged -- the same swap `gate-mutation'
;; measured against `parity-coverage' on 2026-08-21.  A pinned pair going
;; away is still green; one that is neither pinned nor already counted fails
;; and names it.

;;; Code:

(require 'nelisp-pkg)

(defconst nelisp-pkg-graph--host-baseline-file
  "tools/pkg-host-requires-baseline.txt")

(defun nelisp-pkg-graph--host-pins ()
  "Return the pinned hard host requires as a list of (FEATURE . FILE).
Read from `pinned-host-require FEATURE FILE' lines in
`nelisp-pkg-graph--host-baseline-file'."
  (let ((pins nil))
    (when (file-exists-p nelisp-pkg-graph--host-baseline-file)
      (with-temp-buffer
        (insert-file-contents nelisp-pkg-graph--host-baseline-file)
        (goto-char (point-min))
        (while (re-search-forward
                "^pinned-host-require +\\([^ \n]+\\) +\\([^ \n]+\\)" nil t)
          (push (cons (intern (match-string 1)) (match-string 2)) pins))))
    (nreverse pins)))

(defun nelisp-pkg-graph--host-baseline ()
  "Return the allowed number of hard host requires, or nil when unreadable.
Unreadable and zero are different facts: a missing baseline must not read
as \"none allowed\" and pass a tree that has some, nor as \"any allowed\"
and pass one that has many.  The caller says which it is."
  (when (file-exists-p nelisp-pkg-graph--host-baseline-file)
    (with-temp-buffer
      (insert-file-contents nelisp-pkg-graph--host-baseline-file)
      (goto-char (point-min))
      (when (re-search-forward "^hard-host-require +\\([0-9]+\\)" nil t)
        (string-to-number (match-string 1))))))

(defun nelisp-pkg-graph--report-host-requires (entries label)
  "Print ENTRIES, a list of (FEATURE . FILE), under LABEL."
  (princ (format "    %s (%d)%s\n" label (length entries)
                 (if entries ":" "")))
  (dolist (entry entries)
    (princ (format "      %-16s %s\n" (car entry) (cdr entry)))))

(defun nelisp-pkg-graph-run ()
  "Print the package graph and exit non-zero on a structural failure."
  (let* ((packages (nelisp-pkg-scan))
         (core (nelisp-pkg-core-features))
         (findings (nelisp-pkg-check packages core))
         (edges (nelisp-pkg-edges packages))
         (resolution (nelisp-pkg-resolve packages))
         (host (nelisp-pkg-core-host-requires packages))
         (host-hard (plist-get host :hard))
         (host-baseline (nelisp-pkg-graph--host-baseline))
         (fatal (append (nelisp-pkg-findings-of-kind findings 'pkg-cycle)
                        (nelisp-pkg-findings-of-kind findings
                                                     'pkg-invalid-manifest)
                        (nelisp-pkg-findings-of-kind findings
                                                     'pkg-undeclared-dependency)
                        (nelisp-pkg-findings-of-kind findings
                                                     'pkg-stale-dependency))))
    (princ (nelisp-pkg-report packages findings))
    (princ "\n  cross-package edges\n")
    (dolist (edge edges)
      (princ (format "    %s -> %s\n" (car edge) (cdr edge))))
    (let ((unresolved (nelisp-pkg-findings-of-kind findings
                                                   'pkg-unresolved-require)))
      (when unresolved
        (princ (format "\n  requires provided nowhere in the tree (%d), assumed host:\n"
                       (length unresolved)))
        (let ((names nil))
          (dolist (finding unresolved)
            (cl-pushnew (plist-get finding :feature) names))
          (princ (format "    %s\n"
                         (mapconcat #'symbol-name (sort names #'string<) " "))))))
    (princ "\n  src/ and lisp/ require, nothing in the tree provides\n")
    (nelisp-pkg-graph--report-host-requires host-hard "hard")
    (nelisp-pkg-graph--report-host-requires (plist-get host :optional)
                                            "optional")
    (princ (format "    baseline hard-host-require %s\n"
                   (if host-baseline (number-to-string host-baseline)
                     "MISSING")))
    (when (plist-get resolution :order)
      (princ (format "\n  load order: %s\n"
                     (mapconcat #'identity (plist-get resolution :order) " "))))
    ;; Machine-readable tail (contract: tools/ai/README.md).  `checked'
    ;; counts packages, so a scan that found no packages -- wrong working
    ;; directory, renamed layout -- cannot read as a clean graph.
    (princ (format "GATE-COUNT checked=%d findings=%d\n"
                   (length packages) (length findings)))
    (dolist (finding fatal)
      (pcase (plist-get finding :kind)
        ('pkg-undeclared-dependency
         (princ (format "\n  UNDECLARED %s depends on %s\n"
                        (plist-get finding :subject)
                        (plist-get finding :dependency))))
        ('pkg-stale-dependency
         (princ (format "\n  STALE %s no longer uses %s\n"
                        (plist-get finding :subject)
                        (plist-get finding :dependency))))))
    (when (null host-baseline)
      (princ (format "pkg-graph: FAIL (no hard-host-require line in %s)\n"
                     nelisp-pkg-graph--host-baseline-file))
      (kill-emacs 1))
    (let ((host-pins (nelisp-pkg-graph--host-pins))
          (new-host nil))
      (dolist (entry host-hard)
        (unless (member entry host-pins) (push entry new-host)))
      (setq new-host (nreverse new-host))
      (when new-host
        (princ (format "pkg-graph: FAIL (%d hard host require(s) not in `pinned-host-require'):\n"
                       (length new-host)))
        (dolist (entry new-host)
          (princ (format "    %-16s %s\n" (car entry) (cdr entry))))
        (princ (format "  Add a `pinned-host-require' line in %s and say there why the file cannot load without the host.\n"
                       nelisp-pkg-graph--host-baseline-file))
        (kill-emacs 1))
      (let ((stale-pins (seq-remove (lambda (p) (member p host-hard)) host-pins)))
        (when stale-pins
          (princ (format "    %d `pinned-host-require' line(s) no longer require a host feature -- safe to drop:\n"
                         (length stale-pins)))
          (dolist (p stale-pins) (princ (format "      %s %s\n" (car p) (cdr p)))))))
    (when (> (length host-hard) host-baseline)
      (princ (format "pkg-graph: FAIL (%d hard host require(s), baseline %d -- make it optional, or raise the baseline and say in that file why the file cannot load without the host)\n"
                     (length host-hard) host-baseline))
      (kill-emacs 1))
    (when (< (length host-hard) host-baseline)
      (princ (format "    ratchet available: hard-host-require is %d, %d below baseline\n"
                     (length host-hard) (- host-baseline (length host-hard)))))
    (if fatal
        (progn
          (princ (format "pkg-graph: FAIL (%d finding(s))\n" (length fatal)))
          (when (or (nelisp-pkg-findings-of-kind findings
                                                'pkg-undeclared-dependency)
                    (nelisp-pkg-findings-of-kind findings
                                                 'pkg-stale-dependency))
            (princ "  fix with: make pkg-manifest-update\n"))
          (kill-emacs 1))
      (princ "pkg-graph: PASS\n"))))

(nelisp-pkg-graph-run)

;;; nelisp-pkg-graph.el ends here
