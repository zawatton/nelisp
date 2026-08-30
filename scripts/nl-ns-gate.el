;;; nl-ns-gate.el --- fail on cross-file name collisions that are new  -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs `nl-ns' over the tree and compares the result against
;; `scripts/nl-ns-accepted.el'.
;;
;; The point is not to reach zero findings.  A bootstrap prelude has to
;; define `when' and `cond' before the file that defines them properly
;; can be read, so its copies necessarily differ from theirs, and that
;; will not change.  Reporting those every run trains a reader to ignore
;; the count, and then the one new collision that matters arrives inside
;; it unnoticed -- which is exactly how a stale copy of the evaluator sat
;; in packages/nelisp-tramp/src for a month, and how ten Doc 22 fixes
;; lived in the prelude while the hosted stdlib kept the bugs.
;;
;; So the accepted set is pinned, and this gate fails on two things:
;; a finding that is not in it, and an entry in it that no longer
;; matches anything.  The second half matters as much as the first: an
;; accepted list that keeps entries for divergences somebody has since
;; resolved stops describing the tree, and starts hiding the next one.
;;
;; Usage:
;;   emacs -Q --batch -L packages/nl-ns/src -l scripts/nl-ns-gate.el
;;
;; Regenerate the accepted set only from a tree you have reviewed:
;;   NL_NS_GATE_WRITE=1 emacs -Q --batch -L packages/nl-ns/src \
;;     -l scripts/nl-ns-gate.el

;;; Code:

(require 'nl-ns)

(defvar nl-ns-gate-roots '("lisp" "src" "scripts" "packages")
  "Directories scanned, relative to the repository root.")

(defvar nl-ns-gate-kinds
  '(ns-partial-override ns-unsafe-shim-guard
    ns-file-shadows-library ns-collision-divergent)
  "Finding kinds this gate is responsible for.
The prefix and private-escape findings are a separate concern and are
deliberately outside it: mixing them in would make the accepted set
thousands of lines and nobody would read a diff of it.")

(defvar nl-ns-gate-accepted-file "scripts/nl-ns-accepted.el"
  "Where the accepted set lives.")

(defvar nl-ns-gate-baseline-file "packages/nl-ns/baseline/emacs-30.1.el"
  "Host baseline used for the host-shadow findings.")

(defun nl-ns-gate--files ()
  "Return every Elisp file under `nl-ns-gate-roots'."
  (let ((files nil))
    (dolist (root nl-ns-gate-roots)
      (when (file-directory-p root)
        (setq files (append files (directory-files-recursively root "\\.el$")))))
    files))

(defun nl-ns-gate--gated (findings)
  "Return the FINDINGS whose kind this gate owns."
  (let ((out nil))
    (dolist (finding findings)
      (when (memq (plist-get finding :kind) nl-ns-gate-kinds)
        (setq out (cons finding out))))
    (nreverse out)))

(defun nl-ns-gate-run ()
  "Run the gate.  Return the number of problems found."
  (let* ((findings (nl-ns-check-files (nl-ns-gate--files) nil
                                      nl-ns-gate-baseline-file))
         (gated (nl-ns-gate--gated findings))
         (accepted (nl-ns-load-accepted nl-ns-gate-accepted-file))
         (new (nl-ns-unaccepted gated accepted))
         (stale (nl-ns-stale-accepted gated accepted)))
    ;; Machine-readable, before the verdict so it survives every exit path.
    ;; `checked' counts FINDINGS SEEN, which is the only number that can show
    ;; the scan happened at all: a gate that reports clean because it looked
    ;; at nothing reads exactly like a gate that reports clean because the
    ;; tree is clean.  Three checks were in that state on 2026-08-19.
    (princ (format "GATE-COUNT checked=%d findings=%d\n"
                   (length findings) (+ (length new) (length stale))))
    (princ (format "nl-ns gate: %d findings, %d in gated kinds, %d accepted\n"
                   (length findings) (length gated)
                   (hash-table-count (plist-get accepted :keys))))
    (when new
      (princ (format "\n%d finding(s) not in the accepted set:\n\n" (length new)))
      (princ (nl-ns-report new)))
    (when stale
      (princ (format "\n%d accepted entr(ies) no longer match anything.\n"
                     (length stale)))
      (princ "Drop them from the accepted set -- it should describe the tree:\n")
      (dolist (key stale)
        (princ (format "  %s\n" key))))
    ;; Reported, not gated.  Every acceptance ought to say why it is one --
    ;; the file-level :reason covers all of them with a single sentence, and
    ;; two fallbacks that answered wrongly rather than deferring sat under it
    ;; until somebody read them one at a time (2026-08-19).  Failing on this
    ;; today would fail on every entry, so it counts down instead.
    (let ((unnoted (nl-ns-unnoted-accepted accepted)))
      (when unnoted
        (princ (format "nl-ns gate: %d accepted entr%s without a per-entry note\n"
                       (length unnoted)
                       (if (= (length unnoted) 1) "y" "ies")))))
    (+ (length new) (length stale))))

(if (getenv "NL_NS_GATE_WRITE")
    (let* ((findings (nl-ns-check-files (nl-ns-gate--files) nil
                                        nl-ns-gate-baseline-file))
           (gated (nl-ns-gate--gated findings))
           ;; Read the file being replaced FIRST, so its per-entry notes
           ;; survive.  Regeneration that drops them makes writing one
           ;; pointless, and a reason nobody can keep is a reason nobody
           ;; writes.
           (previous (nl-ns-load-accepted nl-ns-gate-accepted-file)))
      (princ (format "wrote %d accepted key(s) to %s\n"
                     (nl-ns-write-accepted
                      gated nl-ns-gate-accepted-file
                      (format-time-string "%Y-%m-%d")
                      "Regenerated from a reviewed tree."
                      (plist-get previous :notes))
                     nl-ns-gate-accepted-file)))
  (let ((problems (nl-ns-gate-run)))
    (if (= problems 0)
        (princ "nl-ns gate: clean\n")
      (kill-emacs 1))))

;;; nl-ns-gate.el ends here
