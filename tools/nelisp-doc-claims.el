;;; nelisp-doc-claims.el --- SHIPPED design docs must name a gate that exists -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A `#+STATUS:' line that says SHIPPED is a claim, not a fact, unless
;; something ran and said so.  Doc 142 said SHIPPED for its =--kind elc=
;; lane; the lane died on a bare `void-variable: invocation-name' the first
;; time anything outside its own ERT fixture actually invoked it, and
;; nothing had -- no gate, no smoke, no workflow named it.  This gate closes
;; that gap mechanically: every `docs/design/*.org' whose `#+STATUS:' line
;; contains SHIPPED must carry a
;;
;;   #+VERIFIED-BY: gate-name [gate-name ...]
;;
;; line, and every name on it must be a real gate -- a line in
;; `tools/ai/gates.expected' (required or optional) or a target the
;; Makefile actually defines.  This does not re-run those gates or check
;; their freshness; `nelisp-ai.sh verify' already owns that.  It only
;; checks that a SHIPPED doc points at something that exists, so "SHIPPED"
;; can no longer mean "nothing was ever asked."
;;
;; Migration reality: ~157 docs live under docs/design/, most of them
;; DRAFT/dead states with no `#+STATUS:'; of the 13 whose `#+STATUS:' line
;; actually says SHIPPED, none carried this header before this gate existed
;; -- it is new.  Ratcheting all 13 to a hard requirement in the same commit
;; that adds the check would make "clean tree" and "somebody read all 13
;; docs today" the same fact, which is false.  So this follows the pinned
;; baseline shape unsafe-inventory/fallback-inventory/pkg-graph already use:
;; `tools/nelisp-doc-claims-baseline.txt' lists legacy-SHIPPED docs BY NAME.
;; A doc in the baseline is tolerated and counted (`findings' below, the
;; same "floor, not ceiling" shape `parity-coverage' reports) rather than
;; failing the gate; a SHIPPED doc that is NOT in the baseline must comply,
;; and a doc that drops out of compliance without being added to the
;; baseline goes red and is named.  Six were converted to a real
;; `#+VERIFIED-BY:' rather than filed into the baseline, either because
;; their claims were re-verified this session (Doc 142) or because they
;; already named a real, still-live gate in their own verification prose
;; and only needed the machine-readable line added (Docs 149, 151, 154,
;; 160, 163) -- see the commit that added this gate for which is which.
;;
;; Run: make doc-claims
;; Regenerate the baseline from the tree's current state:
;;   NELISP_DOC_CLAIMS_WRITE=1 make doc-claims

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst nelisp-doc-claims--dir "docs/design")
(defconst nelisp-doc-claims--gates-file "tools/ai/gates.expected")
(defconst nelisp-doc-claims--makefile "Makefile")
(defconst nelisp-doc-claims--baseline-file "tools/nelisp-doc-claims-baseline.txt")

(defun nelisp-doc-claims--file-lines (file)
  "Return FILE's contents as a list of lines, or nil when FILE is absent."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n"))))

(defun nelisp-doc-claims--valid-gates ()
  "Return the set of gate names a `#+VERIFIED-BY:' line may cite.
Union of every non-comment, non-blank line in
`nelisp-doc-claims--gates-file' (a trailing `?' marks an optional gate and
is stripped) and every target `nelisp-doc-claims--makefile' defines -- a
gate can be real and load-bearing (`nelisp-ai.sh check' wires it, or a
tier command does) before it has earned a line in gates.expected, and this
check is about existence, not about promotion."
  (let ((set (make-hash-table :test 'equal)))
    (dolist (line (nelisp-doc-claims--file-lines nelisp-doc-claims--gates-file))
      (let ((trimmed (string-trim line)))
        (unless (or (string-empty-p trimmed) (string-prefix-p "#" trimmed))
          (puthash (string-remove-suffix "?" trimmed) t set))))
    (dolist (line (nelisp-doc-claims--file-lines nelisp-doc-claims--makefile))
      (when (string-match "\\`\\([A-Za-z0-9][A-Za-z0-9_.-]*\\):" line)
        (puthash (match-string 1 line) t set)))
    set))

(defun nelisp-doc-claims--status-shipped-p (lines)
  "Return non-nil when LINES's first `#+STATUS:' line names SHIPPED.

Only the FIRST `#+STATUS:' line is the doc's actual status token (see
the file commentary: \"the `#+STATUS:' line\", singular).  A long status
entry that wraps onto further `#+STATUS:'-prefixed continuation lines
is still one logical entry, and those continuation lines are free-form
prose -- e.g. Doc 189/192's own status is DRAFT, but a later
continuation line mentions an unrelated ADJACENT package shipping the
same week.  Scanning every `#+STATUS:'-prefixed line independently
misreads that prose as this doc's own status."
  (when-let ((first (seq-find (lambda (l) (string-prefix-p "#+STATUS:" l))
                               lines)))
    (string-match-p "SHIPPED" first)))

(defun nelisp-doc-claims--verified-by (lines)
  "Return the gate names LINES's `#+VERIFIED-BY:' line(s) cite, or nil.
Multiple lines (there should be at most one) are unioned rather than
rejected, so a doc is never penalised twice for the same slip."
  (let ((names nil))
    (dolist (l lines)
      (when (string-prefix-p "#+VERIFIED-BY:" l)
        (dolist (name (split-string (substring l (length "#+VERIFIED-BY:")) nil t))
          (push name names))))
    (nreverse names)))

(defun nelisp-doc-claims--shipped-docs ()
  "Return the SHIPPED `docs/design/*.org' files, sorted, as relative paths."
  (sort
   (cl-remove-if-not
    (lambda (f) (nelisp-doc-claims--status-shipped-p (nelisp-doc-claims--file-lines f)))
    (directory-files nelisp-doc-claims--dir t "\\.org\\'"))
   #'string<))

(defun nelisp-doc-claims--baseline ()
  "Return the baseline as a hash set of doc basenames."
  (let ((set (make-hash-table :test 'equal)))
    (dolist (line (nelisp-doc-claims--file-lines nelisp-doc-claims--baseline-file))
      (let ((trimmed (string-trim line)))
        (unless (or (string-empty-p trimmed) (string-prefix-p "#" trimmed))
          (puthash trimmed t set))))
    set))

(defun nelisp-doc-claims-run ()
  "Enforce that every SHIPPED design doc names a gate that exists."
  (let* ((valid-gates (nelisp-doc-claims--valid-gates))
         (docs (nelisp-doc-claims--shipped-docs))
         (baseline (nelisp-doc-claims--baseline))
         (legacy nil)       ; in baseline, still no compliant VERIFIED-BY
         (violations nil)   ; SHIPPED, not in baseline, not compliant
         (stale-baseline nil)) ; in baseline, but now compliant -- droppable
    (dolist (doc docs)
      (let* ((base (file-name-nondirectory doc))
             (lines (nelisp-doc-claims--file-lines doc))
             (verified-by (nelisp-doc-claims--verified-by lines))
             (bad-gates (cl-remove-if (lambda (g) (gethash g valid-gates)) verified-by))
             (compliant (and verified-by (null bad-gates))))
        (cond
         (compliant
          (when (gethash base baseline) (push base stale-baseline)))
         ((gethash base baseline)
          (push (list base verified-by bad-gates) legacy))
         (t
          (push (list base verified-by bad-gates) violations)))))
    (setq legacy (nreverse legacy))
    (setq violations (nreverse violations))
    (setq stale-baseline (nreverse stale-baseline))
    (princ (format "doc-claims: %d SHIPPED doc(s) under %s\n"
                   (length docs) nelisp-doc-claims--dir))
    (when legacy
      (princ (format "\n  %d legacy-SHIPPED (in %s, tolerated):\n"
                     (length legacy) nelisp-doc-claims--baseline-file))
      (dolist (entry legacy)
        (princ (format "    %s%s\n" (nth 0 entry)
                       (if (nth 2 entry)
                           (format "  (VERIFIED-BY names unknown gate(s): %s)"
                                   (mapconcat #'identity (nth 2 entry) " "))
                         "")))))
    (when stale-baseline
      (princ (format "\n  %d baseline entr(ies) now compliant -- safe to drop from %s:\n"
                     (length stale-baseline) nelisp-doc-claims--baseline-file))
      (dolist (b stale-baseline) (princ (format "    %s\n" b))))
    (when violations
      (princ (format "\n  %d SHIPPED doc(s) NOT in %s and not compliant:\n"
                     (length violations) nelisp-doc-claims--baseline-file))
      (dolist (entry violations)
        (pcase-let ((`(,base ,verified-by ,bad-gates) entry))
          (cond
           ((null verified-by)
            (princ (format "    %s: no #+VERIFIED-BY: line\n" base)))
           (bad-gates
            (princ (format "    %s: #+VERIFIED-BY: names gate(s) that do not exist: %s\n"
                           base (mapconcat #'identity bad-gates " ")))))))
      (princ (format "\n  Fix: add `#+VERIFIED-BY: gate-name ...' naming a real gate (a line in\n  %s, or a Makefile target), or add the doc's filename to\n  %s if it is a pre-existing SHIPPED claim being migrated.\n"
                     nelisp-doc-claims--gates-file nelisp-doc-claims--baseline-file)))
    (when (getenv "NELISP_DOC_CLAIMS_WRITE")
      (let ((names (sort (append (mapcar #'car legacy) (mapcar #'car violations))
                         #'string<)))
        (with-temp-file nelisp-doc-claims--baseline-file
          (insert "# Legacy-SHIPPED docs/design/*.org docs, by filename, that predate the\n"
                  "# #+VERIFIED-BY: header tools/nelisp-doc-claims.el requires (see its\n"
                  "# Commentary).  A doc listed here is tolerated: `make doc-claims'\n"
                  "# reports it as legacy rather than failing.  Drop a line here once its\n"
                  "# doc gains a real #+VERIFIED-BY: naming a gate that exists -- the gate\n"
                  "# reports that as safe to drop.  Regenerated by\n"
                  "# NELISP_DOC_CLAIMS_WRITE=1 make doc-claims.\n\n")
          (dolist (n names) (insert n "\n")))
        (princ (format "wrote %s (%d entr(ies))\n"
                       nelisp-doc-claims--baseline-file (length names)))))
    ;; `checked' is every SHIPPED doc found, so a scan that reached no
    ;; files at all -- wrong working directory, docs/design/ renamed --
    ;; cannot read as a clean tree.  `findings' is the legacy-baseline
    ;; count: a floor of known debt, not a ceiling, the same shape
    ;; `parity-coverage' reports -- a non-zero number here is expected and
    ;; is not itself a failure.
    (princ (format "GATE-COUNT checked=%d findings=%d\n"
                   (length docs) (length legacy)))
    (if violations
        (progn
          (princ (format "doc-claims: FAIL (%d SHIPPED doc(s) name no gate that exists)\n"
                         (length violations)))
          (kill-emacs 1))
      (princ (format "doc-claims: PASS (%d legacy-SHIPPED doc(s) tolerated via baseline)\n"
                     (length legacy))))))

(nelisp-doc-claims-run)

;;; nelisp-doc-claims.el ends here
