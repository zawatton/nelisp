;;; nelisp-dev-audit.el --- Phase 7+ release-audit scanner  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of NeLisp.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Internal release-audit scanner for the NeLisp Phase 7+ design
;; pipeline.  Replaces the manual ritual referenced as
;; =anvil-dev release-audit :scope phase-7.X= in
;; docs/design/28-32 §6.6 v2 / §6.7 v2 replan-gate sections.
;;
;; This is a NeLisp-internal mini-tool — it does not depend on, nor
;; modify, the external `anvil-dev' module.  It scans the design
;; doc tree (docs/design/27-32) and produces:
;;
;;   1. Doc-state summary  (DRAFT / LOCKED / SHIPPED + version)
;;   2. Sub-phase SHIPPED stamp inventory (from §3 sub-phase tables)
;;   3. Replan-gate evaluation against a hard-coded gate table
;;      (week-N → required sub-phase milestones, per Doc 28 §6.6 /
;;      Doc 29 §6.6 / Doc 30 §6.7 / Doc 31 §6.6 / Doc 32 §6.7)
;;
;; The doc-state regex is defensive: it accepts both `#+STATE:'
;; (Doc 27) and `#+STATUS:' (Doc 28-32) and pulls out
;;   - DRAFT-YYYY-MM-DD          → :state 'DRAFT
;;   - LOCKED-YYYY-MM-DD[-vN]    → :state 'LOCKED  :state-version "vN"
;;   - SHIPPED-YYYY-MM-DD        → :state 'SHIPPED
;;
;; SHIPPED stamps are scanned by walking each =** Phase 7.X= subtree
;; under §3 and looking for the literal substring "SHIPPED " followed
;; by a YYYY-MM-DD date and an optional "(commit <SHA>)".
;;
;; Replan-gate evaluation is data-driven: each gate is encoded as
;; (:doc N :gate gate-id :week W :required-sub-phases (...)
;;  :recommendation STRING).  Status is one of:
;;
;;   'pass     all required sub-phases are SHIPPED
;;   'fire     gate-week elapsed AND not all required sub-phases SHIPPED
;;   'pending  gate-week not yet reached / data insufficient
;;
;; A "current week" parameter is accepted for evaluation; it defaults
;; to nil = pending judgment (no clock available, used in tests).
;;
;; Public API
;;   nelisp-dev-audit-report (&optional scope)        — buffer report
;;   nelisp-dev-audit-batch                           — CI entry, exit code
;;   nelisp-dev-audit-evaluate-replan-gate (doc week) — single-gate check
;;
;; Private helpers
;;   nelisp-dev-audit--scan-design-docs (&optional design-dir)
;;   nelisp-dev-audit--scan-shipped-stamps (doc-plist)
;;   nelisp-dev-audit--parse-state-frontmatter (text)

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; Customization ------------------------------------------------------

(defgroup nelisp-dev-audit nil
  "Phase 7+ design-doc release-audit scanner."
  :group 'nelisp
  :prefix "nelisp-dev-audit-")

(defcustom nelisp-dev-audit-design-dir nil
  "Directory holding `docs/design/*.org' design docs.
When nil, `nelisp-dev-audit--default-design-dir' computes a path
relative to the file that defines this module."
  :type '(choice (const :tag "Auto-detect" nil) directory)
  :group 'nelisp-dev-audit)

(defcustom nelisp-dev-audit-buffer-name "*NeLisp Audit*"
  "Buffer name used by `nelisp-dev-audit-report'."
  :type 'string
  :group 'nelisp-dev-audit)

;;;; Doc identity table -------------------------------------------------

(defconst nelisp-dev-audit--doc-numbers '(27 28 29 30 31 32)
  "Phase 7+ design doc numbers in scanning order (Doc 27 anchor + 28-32 v2).")

(defconst nelisp-dev-audit--doc-file-glob
  "\\`\\([0-9]+\\)-\\(?:phase\\|stage\\).*\\.org\\'"
  "Filename pattern for a design doc — leading number is the Doc id.")

;;;; Replan-gate table (hard-coded per Doc 28-32 v2 §6.6 / §6.7) -------

;; Keep this table in sync with the v2 LOCKED §6.6 / §6.7 sections of
;; Docs 28-32.  Each entry is a plist with:
;;
;;   :doc       integer doc id (28-32)
;;   :gate      gate symbol (gate-W1 / gate-W2 / ... / gate-W12)
;;   :week      integer week from Phase start
;;   :required  list of sub-phase id strings that must be SHIPPED
;;              (e.g. "7.1.1", "7.2.2", "7.3.4")
;;   :recommend string — kill-criteria recommendation when gate fires
;;
;; Doc 27 itself has no replan gate (it is the umbrella doc); only the
;; per-phase docs (28-32) carry gate-W{N}.

(defconst nelisp-dev-audit--replan-gates
  '(;; Doc 28 — Phase 7.1 native compiler (week 4 / 8 / 12)
    (:doc 28 :gate gate-W4  :week 4
     :required ("7.1.1")
     :recommend
     "second ISA (arm64) defer; Phase 7.1 完遂 gate から arm64 を外し Phase 7.5 で完成")
    (:doc 28 :gate gate-W8  :week 8
     :required ("7.1.2")
     :recommend
     "self-host 以外 freeze; 7.1.4/7.1.5 一時停止、x86_64 first-class 完成集中")
    (:doc 28 :gate gate-W12 :week 12
     :required ("7.1.4")
     :recommend
     "bench gate 下方修正: fib(30) 30x → 20x, user signoff 要")
    ;; Doc 29 — Phase 7.2 allocator (week 2 / 3)
    (:doc 29 :gate gate-W2  :week 2
     :required ("7.2.1")
     :recommend
     "bulk API + stats 後送り: Phase 7.2.3 を Phase 7.5 へ繰越")
    (:doc 29 :gate gate-W3  :week 3
     :required ("7.2.2")
     :recommend
     "2-generation 縮小: 単一 heap MVP, promotion を Phase 7.5 へ")
    ;; Doc 30 — Phase 7.3 GC inner (week 3 / 6 / 9)
    (:doc 30 :gate gate-W3  :week 3
     :required ("7.3.1")
     :recommend
     "write barrier 簡易化: 7.3.4 を card-marking → full-scan major GC 縮小")
    (:doc 30 :gate gate-W6  :week 6
     :required ("7.3.2" "7.3.3")
     :recommend
     "generational 縮小: 単一世代 mark-sweep MVP, Phase 7.5 で 2-gen 再導入")
    (:doc 30 :gate gate-W9  :week 9
     :required ("7.3.4")
     :recommend
     "bench gate 下方修正: mark-throughput 50MB/sec → 30MB/sec")
    ;; Doc 31 — Phase 7.4 coding (week 1 / 2)
    (:doc 31 :gate gate-W1  :week 1
     :required ("7.4.1")
     :recommend
     "Latin-1 / Shift-JIS / EUC-JP 後送り; Phase 7.4 完遂 gate を UTF-8 + streaming に縮小")
    (:doc 31 :gate gate-W2  :week 2
     :required ("7.4.2" "7.4.3")
     :recommend
     "Shift-JIS / EUC-JP v2.0 化: post-v1.0 task 降格")
    ;; Doc 32 — Phase 7.5 integration (week 1)
    (:doc 32 :gate gate-W1  :week 1
     :required ("7.5.1")
     :recommend
     "scope 削減: cross-arch CI を Linux x86_64 のみへ縮小, 24h soak → 8h"))
  "Hard-coded replan-gate table for Phase 7+ docs (28-32 v2).
Each entry: (:doc N :gate SYM :week W :required (\"X.Y.Z\" ...) :recommend STR).")

;;;; Path helpers -------------------------------------------------------

(defun nelisp-dev-audit--default-design-dir ()
  "Resolve the design-dir relative to this source file.
Returns an absolute path ending in /docs/design/."
  (or nelisp-dev-audit-design-dir
      (let* ((this (or load-file-name buffer-file-name
                       (locate-library "nelisp-dev-audit")))
             (src-dir (and this (file-name-directory this)))
             (root (and src-dir
                        (file-name-as-directory
                         (expand-file-name ".." src-dir)))))
        (and root (expand-file-name "docs/design/" root)))))

;;;; Frontmatter parser -------------------------------------------------

(defun nelisp-dev-audit--parse-state-frontmatter (text)
  "Parse TEXT (the head of an org doc) and return a state plist.

Returns (:state SYMBOL :state-version STRING-OR-NIL :state-date STRING-OR-NIL).
Looks at both =#+STATE:= and =#+STATUS:= lines and picks the first
match in {SHIPPED, LOCKED, DRAFT}.  When multiple keywords appear
(e.g. \"LOCKED-2026-04-25-v2\" inside a long STATUS prose), the
keyword nearest the line start wins."
  (let ((state nil)
        (version nil)
        (date nil))
    ;; Search *both* fields; STATE / STATUS share the same regex.
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (when (re-search-forward
             "^#\\+STAT\\(?:E\\|US\\):[ \t]*\\(.*\\)$" nil t)
        (let ((line (match-string 1)))
          (cond
           ((string-match
             "SHIPPED-\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
             line)
            (setq state 'SHIPPED date (match-string 1 line)))
           ((string-match
             "LOCKED-\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\(?:-\\(v[0-9]+\\)\\)?"
             line)
            (setq state 'LOCKED
                  date (match-string 1 line)
                  version (match-string 2 line)))
           ((string-match
             "DRAFT-\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
             line)
            (setq state 'DRAFT date (match-string 1 line)))
           ((string-match "\\bSHIPPED\\b" line) (setq state 'SHIPPED))
           ((string-match "\\bLOCKED\\b"  line) (setq state 'LOCKED))
           ((string-match "\\bDRAFT\\b"   line) (setq state 'DRAFT))))))
    (list :state state :state-version version :state-date date)))

(defun nelisp-dev-audit--extract-title (text)
  "Return the value of the first =#+TITLE:= line in TEXT, or nil."
  (when (string-match "^#\\+TITLE:[ \t]*\\(.*\\)$" text)
    (string-trim (match-string 1 text))))

;;;; Sub-phase scanner --------------------------------------------------

(defconst nelisp-dev-audit--subphase-heading-re
  ;; Matches both Doc-27 style:   ** Phase 7.0 — ...
  ;;       and Doc-28+ style:     ** 3.1 Phase 7.1.1 — ...
  "^\\*\\*[ \t]+\\(?:[0-9.]+[ \t]+\\)?Phase[ \t]+\\(7\\.[0-9]+\\(?:\\.[0-9]+\\)?\\)\\b"
  "Regex matching a sub-phase heading.
Two styles are supported:
  - `** Phase 7.0 — ...'              (Doc 27 §3)
  - `** 3.1 Phase 7.1.1 — ...'        (Doc 28-32 §3.X)")

(defconst nelisp-dev-audit--shipped-stamp-re
  ;; "SHIPPED 2026-04-25" or "SHIPPED 2026-04-25 (commit deadbeef)"
  "SHIPPED[ \t]+\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\(?:[^(\n]*?(commit[ \t]+\\([0-9a-f]+\\))\\)?"
  "Regex matching a SHIPPED stamp inside a sub-phase subtree.")

(defun nelisp-dev-audit--scan-subphases (text)
  "Walk TEXT (full org file body) and return sub-phase plists.

Each plist:
  (:sub-phase-id STR :state SYMBOL :shipped-date STR-OR-NIL
   :shipped-commit STR-OR-NIL :heading STR)

State is \\='SHIPPED if a stamp is detected in the subtree, \\='PENDING
otherwise.  We treat `Phase 7.X' and `Phase 7.X.Y' headings as
distinct sub-phase ids."
  (let (results)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      ;; Move into §3 (Sub-phase breakdown) when present, otherwise scan whole doc.
      (let ((sec3-start
             (save-excursion
               (goto-char (point-min))
               (when (re-search-forward "^\\*[ \t]+§3\\b" nil t)
                 (line-beginning-position))))
            (next-section
             (save-excursion
               (goto-char (point-min))
               (and (re-search-forward "^\\*[ \t]+§3\\b" nil t)
                    (re-search-forward "^\\*[ \t]+§[0-9]+\\b" nil t)
                    (line-beginning-position)))))
        (let ((scan-start (or sec3-start (point-min)))
              (scan-end   (or next-section (point-max))))
          (goto-char scan-start)
          (while (re-search-forward
                  nelisp-dev-audit--subphase-heading-re scan-end t)
            (let* ((id (match-string 1))
                   (heading-start (line-beginning-position))
                   (heading-end (line-end-position))
                   (heading (buffer-substring-no-properties
                             heading-start heading-end))
                   (subtree-start (1+ heading-end))
                   ;; Subtree ends at the next ** heading or scan-end.
                   (subtree-end
                    (save-excursion
                      (goto-char subtree-start)
                      (if (re-search-forward "^\\*\\{1,2\\}[ \t]" scan-end t)
                          (line-beginning-position)
                        scan-end)))
                   (subtree (buffer-substring-no-properties
                             subtree-start subtree-end))
                   shipped-date shipped-commit)
              (when (string-match nelisp-dev-audit--shipped-stamp-re subtree)
                (setq shipped-date (match-string 1 subtree)
                      shipped-commit (match-string 2 subtree)))
              (push
               (list :sub-phase-id id
                     :state (if shipped-date 'SHIPPED 'PENDING)
                     :shipped-date shipped-date
                     :shipped-commit shipped-commit
                     :heading heading)
               results))))))
    (nreverse results)))

;;;; Top-level scan -----------------------------------------------------

(defun nelisp-dev-audit--scan-design-docs (&optional design-dir)
  "Scan DESIGN-DIR (or default) and return a list of doc plists.

Each plist:
  (:doc N :file PATH :title STR :state SYMBOL :state-version STR-OR-NIL
   :state-date STR-OR-NIL :sub-phases LIST)

Only files whose leading number is in `nelisp-dev-audit--doc-numbers'
are returned (Doc 27-32)."
  (let ((dir (or design-dir (nelisp-dev-audit--default-design-dir)))
        results)
    (when (and dir (file-directory-p dir))
      (dolist (file (directory-files dir t "\\.org\\'"))
        (let ((base (file-name-nondirectory file)))
          (when (string-match nelisp-dev-audit--doc-file-glob base)
            (let ((n (string-to-number (match-string 1 base))))
              (when (memq n nelisp-dev-audit--doc-numbers)
                (let* ((text (with-temp-buffer
                               (insert-file-contents file)
                               (buffer-string)))
                       ;; Frontmatter = first 200 lines is plenty.
                       (head (with-temp-buffer
                               (insert text)
                               (goto-char (point-min))
                               (forward-line 200)
                               (buffer-substring-no-properties
                                (point-min) (point))))
                       (st (nelisp-dev-audit--parse-state-frontmatter head))
                       (sub (nelisp-dev-audit--scan-subphases text)))
                  (push
                   (list :doc n
                         :file file
                         :title (nelisp-dev-audit--extract-title head)
                         :state (plist-get st :state)
                         :state-version (plist-get st :state-version)
                         :state-date (plist-get st :state-date)
                         :sub-phases sub)
                   results))))))))
    (cl-sort results #'< :key (lambda (p) (plist-get p :doc)))))

(defun nelisp-dev-audit--scan-shipped-stamps (doc-plist)
  "Return a list of (SUB-PHASE-ID . SHIPPED-INFO-OR-NIL) for DOC-PLIST.
SHIPPED-INFO is (:date STR :commit STR-OR-NIL) when the sub-phase
carries a SHIPPED stamp, otherwise nil."
  (mapcar
   (lambda (sp)
     (let ((id (plist-get sp :sub-phase-id))
           (date (plist-get sp :shipped-date))
           (commit (plist-get sp :shipped-commit)))
       (cons id
             (and date (list :date date :commit commit)))))
   (plist-get doc-plist :sub-phases)))

;;;; Replan-gate evaluation ---------------------------------------------

(defun nelisp-dev-audit--find-doc (docs n)
  "Return the doc plist with :doc = N from DOCS, or nil."
  (cl-find n docs :key (lambda (d) (plist-get d :doc))))

(defun nelisp-dev-audit--shipped-ids (doc)
  "Return the sub-phase id strings of DOC that are SHIPPED."
  (let (ids)
    (dolist (sp (plist-get doc :sub-phases))
      (when (eq (plist-get sp :state) 'SHIPPED)
        (push (plist-get sp :sub-phase-id) ids)))
    (nreverse ids)))

(defun nelisp-dev-audit--id-shipped-p (shipped-ids id)
  "Non-nil if ID (a sub-phase id like \"7.2.1\") is satisfied by SHIPPED-IDS.
Satisfied means there is an exact match OR a prefix match — the
whole umbrella sub-phase being shipped (e.g. \"7.2\") covers any
\"7.2.X\" requirement."
  (cl-some
   (lambda (s)
     (or (string= s id)
         ;; "7.2" covers "7.2.1"
         (and (string-prefix-p (concat s ".") id))))
   shipped-ids))

(defun nelisp-dev-audit-evaluate-replan-gate (doc-id week-n &optional docs)
  "Evaluate the replan gate for DOC-ID at WEEK-N.

DOC-ID is an integer (28..32).  WEEK-N is the elapsed-week
counter from Phase start; pass nil to force \\='pending judgment.

DOCS, if supplied, is the result of `nelisp-dev-audit--scan-design-docs';
otherwise it is computed lazily.

Returns a plist:
  (:doc DOC-ID :gate GATE-SYM :week WEEK :status SYMBOL
   :evaluated-sub-phases LIST :missing-sub-phases LIST
   :recommendation STRING)

:status is one of
  \\='pass     all required sub-phases SHIPPED (gate satisfied)
  \\='fire     gate-week elapsed AND not all required SHIPPED
  \\='pending  no week supplied OR week < gate-week
  \\='no-gate  no gate registered for (DOC-ID, WEEK-N)"
  (let* ((docs (or docs (nelisp-dev-audit--scan-design-docs)))
         (doc (nelisp-dev-audit--find-doc docs doc-id))
         (gates (cl-remove-if-not
                 (lambda (g) (= (plist-get g :doc) doc-id))
                 nelisp-dev-audit--replan-gates))
         ;; Pick the gate whose :week == week-n exactly; else the
         ;; *latest* gate <= week-n.  When week-n is nil we pick the
         ;; first gate (most useful for dry-run reports).
         (gate (cond
                ((null week-n) (car gates))
                (t
                 (or (cl-find-if (lambda (g)
                                   (= (plist-get g :week) week-n))
                                 gates)
                     (car (last (cl-remove-if
                                 (lambda (g)
                                   (> (plist-get g :week) week-n))
                                 gates)))))))
         (shipped (and doc (nelisp-dev-audit--shipped-ids doc))))
    (if (null gate)
        (list :doc doc-id :gate nil :week week-n :status 'no-gate
              :evaluated-sub-phases nil :missing-sub-phases nil
              :recommendation
              (format "No replan gate registered for Doc %s week %s"
                      doc-id week-n))
      (let* ((required (plist-get gate :required))
             (missing
              (cl-remove-if
               (lambda (id)
                 (nelisp-dev-audit--id-shipped-p shipped id))
               required))
             (status
              (cond
               ((null missing) 'pass)
               ((null week-n) 'pending)
               ((< week-n (plist-get gate :week)) 'pending)
               (t 'fire))))
        (list :doc doc-id
              :gate (plist-get gate :gate)
              :week (plist-get gate :week)
              :status status
              :evaluated-sub-phases required
              :missing-sub-phases missing
              :recommendation (plist-get gate :recommend))))))

;;;; Reporting ----------------------------------------------------------

(defun nelisp-dev-audit--state-tag (state)
  "Pretty STATE symbol → string."
  (pcase state
    ('SHIPPED "✅ SHIPPED")
    ('LOCKED  "🔒 LOCKED ")
    ('DRAFT   "📝 DRAFT  ")
    ('PENDING "…  PENDING")
    ('pass    "✅ pass  ")
    ('fire    "🔥 fire  ")
    ('pending "…  pending")
    ('no-gate "—  no-gate")
    (_        (format "?  %S" state))))

(defun nelisp-dev-audit--scope-doc-ids (scope)
  "Return the list of doc ids covered by SCOPE.

SCOPE: nil / \\='all      → all of `nelisp-dev-audit--doc-numbers'
       \\='phase-7.0      → \\='(27)
       \\='phase-7.1      → \\='(28)
       \\='phase-7.2      → \\='(29)
       \\='phase-7.3      → \\='(30)
       \\='phase-7.4      → \\='(31)
       \\='phase-7.5      → \\='(32)
       integer N       → \\='(N)"
  (cond
   ((or (null scope) (eq scope 'all))
    nelisp-dev-audit--doc-numbers)
   ((integerp scope) (list scope))
   ((symbolp scope)
    (let ((name (symbol-name scope)))
      (pcase name
        ("phase-7.0" '(27)) ("phase-7.1" '(28))
        ("phase-7.2" '(29)) ("phase-7.3" '(30))
        ("phase-7.4" '(31)) ("phase-7.5" '(32))
        (_ nelisp-dev-audit--doc-numbers))))
   (t nelisp-dev-audit--doc-numbers)))

(defun nelisp-dev-audit--render (docs scope-ids &optional current-week)
  "Render an org-mode audit report into the current buffer.
DOCS is the scan result, SCOPE-IDS the doc ids to include,
CURRENT-WEEK an integer or nil (pending only)."
  (insert "#+TITLE: NeLisp Dev Audit Report\n")
  (insert (format "#+DATE: %s\n"
                  (format-time-string "%Y-%m-%d %H:%M:%S")))
  (insert
   (format "#+CURRENT_WEEK: %s\n\n"
           (if current-week (number-to-string current-week) "<unset>")))
  ;; -- Section: doc state summary
  (insert "* Doc state summary\n\n")
  (insert "| Doc | State          | Version | Date       | Title |\n")
  (insert "|-----+----------------+---------+------------+-------|\n")
  (dolist (d docs)
    (when (memq (plist-get d :doc) scope-ids)
      (insert
       (format "| %2d  | %s | %-7s | %-10s | %s |\n"
               (plist-get d :doc)
               (nelisp-dev-audit--state-tag (plist-get d :state))
               (or (plist-get d :state-version) "-")
               (or (plist-get d :state-date) "-")
               (or (plist-get d :title) "?")))))
  (insert "\n")
  ;; -- Section: sub-phase SHIPPED stamps
  (insert "* Sub-phase SHIPPED stamps\n\n")
  (dolist (d docs)
    (when (memq (plist-get d :doc) scope-ids)
      (insert (format "** Doc %d sub-phases\n"
                      (plist-get d :doc)))
      (let ((sps (plist-get d :sub-phases)))
        (if (null sps)
            (insert "(no sub-phases detected)\n")
          (dolist (sp sps)
            (insert
             (format "  - %-7s %s%s\n"
                     (plist-get sp :sub-phase-id)
                     (nelisp-dev-audit--state-tag (plist-get sp :state))
                     (let ((d (plist-get sp :shipped-date))
                           (c (plist-get sp :shipped-commit)))
                       (cond
                        ((and d c) (format "  %s (commit %s)" d c))
                        (d         (format "  %s" d))
                        (t         "")))))))
        (insert "\n"))))
  ;; -- Section: replan-gate evaluation
  (insert "* Replan gate evaluation\n\n")
  (dolist (g nelisp-dev-audit--replan-gates)
    (when (memq (plist-get g :doc) scope-ids)
      (let* ((doc-id (plist-get g :doc))
             (week (plist-get g :week))
             (eval-result
              (nelisp-dev-audit-evaluate-replan-gate
               doc-id (or current-week week) docs)))
        (insert
         (format "  - Doc %d %s @ week %d : %s\n"
                 doc-id
                 (plist-get g :gate)
                 week
                 (nelisp-dev-audit--state-tag
                  (plist-get eval-result :status))))
        (let ((missing (plist-get eval-result :missing-sub-phases)))
          (when missing
            (insert
             (format "      missing: %s\n"
                     (mapconcat #'identity missing ", ")))))
        (when (memq (plist-get eval-result :status) '(fire pending))
          (insert
           (format "      recommendation: %s\n"
                   (plist-get eval-result :recommendation)))))))
  (insert "\n")
  ;; -- Section: outstanding TODO
  (insert "* Outstanding TODO\n\n")
  (let ((draft 0) (pending 0))
    (dolist (d docs)
      (when (memq (plist-get d :doc) scope-ids)
        (when (eq (plist-get d :state) 'DRAFT) (cl-incf draft))
        (dolist (sp (plist-get d :sub-phases))
          (when (eq (plist-get sp :state) 'PENDING) (cl-incf pending)))))
    (insert (format "  - DRAFT docs:           %d\n" draft))
    (insert (format "  - PENDING sub-phases:   %d\n" pending))))

;;;###autoload
(defun nelisp-dev-audit-report (&optional scope current-week)
  "Generate a Phase 7+ progress report into `nelisp-dev-audit-buffer-name'.

SCOPE: nil / \\='all (default) / \\='phase-7.0 / \\='phase-7.1 / etc.
CURRENT-WEEK: integer week from Phase start (nil = pending only).

Sections:
  - Doc state summary
  - Sub-phase SHIPPED stamps
  - Replan-gate evaluation
  - Outstanding TODO

Returns the buffer."
  (interactive)
  (let* ((docs (nelisp-dev-audit--scan-design-docs))
         (ids  (nelisp-dev-audit--scope-doc-ids scope))
         (buf  (get-buffer-create nelisp-dev-audit-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (nelisp-dev-audit--render docs ids current-week))
      (when (fboundp 'org-mode) (org-mode))
      (goto-char (point-min)))
    buf))

;;;###autoload
(defun nelisp-dev-audit-batch ()
  "Batch entry point for CI / `make audit'.
Prints a report to stdout; exit code 0 if no gate fires, 1 otherwise.
Reads `NELISP_AUDIT_WEEK' env var for the current-week parameter."
  (let* ((wk-env (getenv "NELISP_AUDIT_WEEK"))
         (week (and wk-env (string-match-p "\\`[0-9]+\\'" wk-env)
                    (string-to-number wk-env)))
         (docs (nelisp-dev-audit--scan-design-docs))
         (any-fire nil))
    (with-temp-buffer
      (nelisp-dev-audit--render docs nelisp-dev-audit--doc-numbers week)
      (princ (buffer-string)))
    (dolist (g nelisp-dev-audit--replan-gates)
      (let ((res (nelisp-dev-audit-evaluate-replan-gate
                  (plist-get g :doc)
                  (or week (plist-get g :week))
                  docs)))
        (when (eq (plist-get res :status) 'fire)
          (setq any-fire t))))
    (kill-emacs (if any-fire 1 0))))

(provide 'nelisp-dev-audit)
;;; nelisp-dev-audit.el ends here
