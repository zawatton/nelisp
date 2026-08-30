;;; nelisp-fallback-inventory.el --- CI inventory of silent degradations -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Counts error handlers that degrade without leaving a trace, and fails
;; when a kind grows past its checked-in baseline.
;;
;; The defect this exists for, measured 2026-08-19: `compile-elisp-artifact'
;; produced a 1391-byte .neln with an empty native section and printed not one
;; line of error output, because the native compile is wrapped in a
;; `condition-case' that falls back to bytecode.  A green compile therefore
;; said nothing about whether anything had been compiled natively, and only
;; the manifest could tell the difference.  The underlying `write-region' bug
;; was a one-symbol fix; finding it was not, and the reason is that nothing
;; recorded the fall.
;;
;; Three kinds are tracked, separately, because they are not equally bad:
;;
;;   silent-fallback   `condition-case' whose handler neither records nor
;;                     re-raises.  This is the shape above: an error is
;;                     converted into a quieter, wrong answer.
;;   ignore-errors     `ignore-errors' / `with-demoted-errors' with no
;;                     recording in sight.  Often a deliberate probe, which is
;;                     why it is counted apart rather than mixed in.
;;   bare-handler      `condition-case' with the variable set to nil, so the
;;                     error object is discarded before anyone could record
;;                     it even if they wanted to.
;;   dbg-note          a live `nl_dbg_note' call.  The native subset has no
;;                     other way to report anything, so the primitive has no
;;                     enable flag to forget -- which is only safe while a
;;                     call left behind cannot reach a commit.  Baseline 0;
;;                     this is the half that makes "no flag" a design rather
;;                     than an oversight.
;;
;; A handler counts as recording when it mentions any name in
;; `nelisp-fallback-inventory--recording-functions', and as re-raising when it
;; mentions `signal', `error' or `throw'.  Both lists are matched by symbol
;; occurrence anywhere in the handler body: cheap, and it errs toward calling
;; a handler innocent, which keeps the number an undercount rather than a
;; scold.
;;
;; This gate deliberately does NOT try to tell a degrading handler from a
;; probing one by intent.  That judgement is not mechanical, and a gate that
;; guesses it would be argued with instead of acted on.  What it does is keep
;; the count from growing quietly.
;;
;; Run from the repo root:
;;   emacs --batch -Q -l tools/nelisp-fallback-inventory.el
;; or: make fallback-inventory

;;; Code:

(defconst nelisp-fallback-inventory--baseline-file
  "tools/fallback-inventory-baseline.txt")

(defconst nelisp-fallback-inventory--roots
  (let ((override (getenv "NELISP_FALLBACK_INVENTORY_ROOTS")))
    (if (and override (> (length override) 0))
        (split-string override ":" t)
      '("lisp" "scripts" "tools")))
  "Directories scanned, non-recursively, for `.el' files.
Overridable so the classifier can be pointed at a fixture and shown to
answer correctly, rather than only ever producing an aggregate nobody
can check.")

(defconst nelisp-fallback-inventory--recording-functions
  '(message princ warn display-warning
    ;; This tree's own way of saying something went wrong.  Verified to
    ;; reach stderr unconditionally, 2026-08-19: `--print-error' calls
    ;; `--write-stderr', which calls `nelisp--write-stderr-line' or prints
    ;; to `external-debugging-output'.
    nelisp-artifact--print-error
    nelisp--cli-print-error
    nelisp-runtime-image--print-error
    nelisp-artifact--write-stderr
    nelisp--write-stderr-line
    ;; Records into a variable that is reported later -- the native
    ;; dispatch report, and the reason the manifest gives for a fallback.
    nelisp-artifact--note-native-dispatch
    nelisp-artifact--native-compiler-error
    ;; tools/nelisp-parity-fuzz.el: a name whose arity cannot be read
    ;; generates no call, which silently shrinks the search.  The recorder
    ;; collects them and the run summary prints how many and which.
    nelisp-parity-fuzz--note-unreadable-arity
    ;; tools/nelisp-prelude-toplevel-check.el: one records that a file ended
    ;; at a form boundary (the loop's exit, not a fallback), the other
    ;; records where it stopped parsing.  Both are printed in the report.
    nelisp-prelude-toplevel--note-eof
    nelisp-prelude-toplevel--note-unreadable)
  "Names whose presence in a handler counts as leaving a trace.

Reviewed 2026-08-19, the first time anything in this inventory was looked
at rather than counted.  Five names were removed:

  nelisp-artifact--profile-log  writes only when `nelisp-artifact-profile-
    stages' is non-nil, which it is not in a normal run.  A handler that
    mentions it leaves no trace where it matters, and this list was calling
    18 such handlers innocent.

  nl-safe-report, nl-safe-report-record, nelisp--log, nelisp-log,
  nelisp--warn  are not defined anywhere in this tree.  Matching is by
    exact symbol, so they could never have exonerated anything: they were
    an intention rather than a measurement, and a list of names nobody
    checked is the same failure this gate exists to count.")

(defconst nelisp-fallback-inventory--reraising-functions
  '(signal error throw
    ;; Thin wrappers around `signal' in lisp/nelisp-jit-substrate.el.  A
    ;; handler calling one of these raises a MORE specific error than the
    ;; one it caught; counting it as a silent fallback was wrong in 13
    ;; places, all in nelisp-jit-strategy.el.
    nelisp--signal-wrong-type
    nelisp--signal-arith
    ;; Doc 168/169's `nl-condition' re-signal entry point (Doc 193 §3:
    ;; `nelisp-native--tier-call' in lisp/nelisp-artifact.el catches a
    ;; native exec tier's plain `error' only to hand it straight to
    ;; `nl-signal', so `nl-handler-bind' frames further out can act on
    ;; it pre-unwind).  Same shape as the `nelisp--signal-*' pair above:
    ;; the condition is not swallowed, it is re-raised through a named
    ;; wrapper this scanner did not know about yet.
    nl-signal)
  "Names whose presence in a handler counts as not swallowing the error.")

(defconst nelisp-fallback-inventory--kinds
  '(silent-fallback ignore-errors bare-handler dbg-note))

(defvar nelisp-fallback-inventory--current-file nil
  "File being scanned, for the site listing.")

(defvar nelisp-fallback-inventory--current-toplevel nil
  "Name of the top-level form being scanned, for the site listing.")

(defvar nelisp-fallback-inventory--sites nil
  "Collected (KIND FILE TOPLEVEL CONDITION) records, newest first.")

(defun nelisp-fallback-inventory--listing-p ()
  "Return non-nil when the run should print one line per finding.
A count is what a gate needs and the wrong thing to hand a person: 91 is
not reviewable, and the number sat unreviewed for exactly that reason.
Same classifier either way -- a second implementation of \"what counts\"
would answer a different question than the gate does.

  NELISP_FALLBACK_INVENTORY_LIST=1 make fallback-inventory
  NELISP_FALLBACK_INVENTORY_LIST=silent-fallback make fallback-inventory"
  (let ((v (getenv "NELISP_FALLBACK_INVENTORY_LIST")))
    (and v (> (length v) 0) v)))

(defun nelisp-fallback-inventory--note-site (kind form)
  "Record a KIND finding at FORM.
Always collected now, not only under `NELISP_FALLBACK_INVENTORY_LIST' --
`nelisp-fallback-inventory--pin-key' needs every site to compare against
the pinned identities, not just the ones a human asked to see printed."
  (push (list kind
              nelisp-fallback-inventory--current-file
              nelisp-fallback-inventory--current-toplevel
              form)
        nelisp-fallback-inventory--sites))

(defun nelisp-fallback-inventory--toplevel-name (form)
  "Return a short name for top-level FORM, or nil."
  (when (consp form)
    (let ((head (car form))
          (name (nth 1 form)))
      (cond
       ((and (symbolp name) name (memq head '(defun defmacro defsubst defvar
                                              defconst defcustom cl-defun
                                              cl-defmacro define-error)))
        (format "%s %s" head name))
       ((symbolp head) (format "%s" head))))))

(defun nelisp-fallback-inventory--baseline ()
  "Read the baseline as an alist of (KIND . COUNT), or nil when absent.
Lines starting `pinned-site' are a different table, read by
`nelisp-fallback-inventory--pins', and are skipped here."
  (when (file-exists-p nelisp-fallback-inventory--baseline-file)
    (with-temp-buffer
      (insert-file-contents nelisp-fallback-inventory--baseline-file)
      (goto-char (point-min))
      (let ((acc nil))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (or (string-match-p "\\`[ \t]*#" line)
                        (string-match-p "\\`[ \t]*\\'" line)
                        (string-prefix-p "pinned-site " line))
              (when (string-match "\\`[ \t]*\\([^ \t]+\\)[ \t]+\\([0-9]+\\)" line)
                (push (cons (intern (match-string 1 line))
                            (string-to-number (match-string 2 line)))
                      acc))))
          (forward-line 1))
        (nreverse acc)))))

(defun nelisp-fallback-inventory--pin-key (kind file toplevel form)
  "Return a stable identity key for a KIND finding.
FILE and TOPLEVEL place it; a 12-hex digest of FORM's printed shape stands
in for exact position, so a comment added above the handler does not
change the key but editing the handler itself does -- the same reason
`nl-ns-finding-key' carries `:shape' (2026-08-21)."
  (format "%s\t%s\t%s\t%s"
          kind (file-name-nondirectory (or file "?")) (or toplevel "(top level)")
          (substring (secure-hash 'sha1 (format "%S" form)) 0 12)))

(defun nelisp-fallback-inventory--pins ()
  "Return the pinned site keys as a hash of KEY -> COUNT.
Read from `pinned-site KEY COUNT' lines.  COUNT, not just presence: two
structurally identical handlers in the same toplevel hash to the same
KEY, and presence alone would let a THIRD identical one land unpinned --
a duplicate is not a new shape, but one more of an already-accepted shape
still has to be counted, the same way `pinned-kernel-call' counts rather
than only naming a file."
  (let ((counts (make-hash-table :test #'equal)))
    (when (file-exists-p nelisp-fallback-inventory--baseline-file)
      (with-temp-buffer
        (insert-file-contents nelisp-fallback-inventory--baseline-file)
        (goto-char (point-min))
        (while (re-search-forward "^pinned-site \\(.+\\) \\([0-9]+\\)$" nil t)
          (puthash (match-string 1) (string-to-number (match-string 2)) counts))))
    counts))

(defun nelisp-fallback-inventory--mentions-p (form names)
  "Return non-nil when FORM contains any symbol in NAMES."
  (cond
   ((symbolp form) (and form (memq form names) t))
   ((consp form)
    (or (nelisp-fallback-inventory--mentions-p (car form) names)
        (nelisp-fallback-inventory--mentions-p (cdr form) names)))
   (t nil)))

(defun nelisp-fallback-inventory--traced-p (body)
  "Return non-nil when BODY records or re-raises."
  (or (nelisp-fallback-inventory--mentions-p
       body nelisp-fallback-inventory--recording-functions)
      (nelisp-fallback-inventory--mentions-p
       body nelisp-fallback-inventory--reraising-functions)))

(defun nelisp-fallback-inventory--classify (form counts)
  "Add FORM's own contribution to COUNTS, a hash of KIND -> count."
  (when (consp form)
    (let ((head (car form)))
      (cond
       ((memq head '(ignore-errors with-demoted-errors))
        (unless (nelisp-fallback-inventory--traced-p form)
          (nelisp-fallback-inventory--note-site 'ignore-errors form)
          (puthash 'ignore-errors (1+ (gethash 'ignore-errors counts 0)) counts)))
       ;; Only a CALL counts.  In `(defun nl_dbg_note ...)' the name is the
       ;; second element, not the head, so the definition is not its own
       ;; finding.
       ((eq head 'nl_dbg_note)
        (nelisp-fallback-inventory--note-site 'dbg-note form)
        (puthash 'dbg-note (1+ (gethash 'dbg-note counts 0)) counts))
       ((eq head 'condition-case)
        (let ((var (nth 1 form))
              (handlers (nthcdr 3 form)))
          (when (null var)
            (nelisp-fallback-inventory--note-site 'bare-handler form)
            (puthash 'bare-handler (1+ (gethash 'bare-handler counts 0)) counts))
          (dolist (h handlers)
            (when (consp h)
              (unless (nelisp-fallback-inventory--traced-p (cdr h))
                (nelisp-fallback-inventory--note-site 'silent-fallback h)
                (puthash 'silent-fallback
                         (1+ (gethash 'silent-fallback counts 0))
                         counts))))))))))

(defun nelisp-fallback-inventory--walk (form counts)
  "Walk FORM, classifying every handler in it into COUNTS."
  (nelisp-fallback-inventory--classify form counts)
  (when (consp form)
    (let ((rest form))
      (while (consp rest)
        (nelisp-fallback-inventory--walk (car rest) counts)
        (setq rest (cdr rest))))))

(defun nelisp-fallback-inventory--only-blanks-p (start)
  "Return non-nil when only whitespace and comments lie between START and eob.
That is what tells a file that ended from a form that did not: `read'
signals `end-of-file' for both, and by then point has been dragged to
the end of the buffer either way -- so the question has to be asked from
where the read BEGAN, not from where it stopped."
  (save-excursion
    (goto-char start)
    (let ((clean t))
      (while (and clean (not (eobp)))
        (skip-chars-forward " \t\n\f\r")
        (cond
         ((eobp))
         ((eq (char-after) ?\;) (forward-line 1))
         (t (setq clean nil))))
      clean)))

(defun nelisp-fallback-inventory--scan-file (file counts)
  "Read FILE and classify every handler in it into COUNTS."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((done nil))
      (while (not done)
        (let* ((start (point))
               (form (condition-case nil
                        (read (current-buffer))
                      ;; `end-of-file' means two different things and they
                      ;; must not be conflated: the file ended, or a form did
                      ;; not.  Treating both as a clean finish made an
                      ;; unbalanced file contribute zero findings and the gate
                      ;; still pass -- this gate's own defect, of exactly the
                      ;; kind it exists to count.  Caught 2026-08-19 when a
                      ;; deliberately planted call went unreported because the
                      ;; edit that planted it left a paren open.
                      (end-of-file
                       (setq done (if (nelisp-fallback-inventory--only-blanks-p start)
                                      'eof
                                    'unreadable))
                       nil)
                      (error (setq done 'unreadable) nil))))
          (when form
            (let ((nelisp-fallback-inventory--current-toplevel
                   (nelisp-fallback-inventory--toplevel-name form)))
              (nelisp-fallback-inventory--walk form counts)))))
      done)))

(defun nelisp-fallback-inventory-run ()
  "Scan the tree, print the inventory, enforce the baseline."
  (let ((counts (make-hash-table :test 'eq))
        (scanned 0)
        (unreadable nil))
    (dolist (dir nelisp-fallback-inventory--roots)
      (when (file-directory-p dir)
        (dolist (f (directory-files dir t "\\.el\\'"))
          (setq scanned (1+ scanned))
          (let ((nelisp-fallback-inventory--current-file f))
            (when (eq (nelisp-fallback-inventory--scan-file f counts) 'unreadable)
              (push f unreadable))))))
    (let ((want (nelisp-fallback-inventory--listing-p)))
      (when want
        (dolist (site (nreverse nelisp-fallback-inventory--sites))
          (when (or (equal want "1")
                    (equal want (symbol-name (nth 0 site))))
            (princ (format "SITE %-15s %-34s %-46s %s\n"
                           (nth 0 site)
                           (file-name-nondirectory (or (nth 1 site) "?"))
                           (or (nth 2 site) "(top level)")
                           (let ((s (format "%S" (nth 3 site))))
                             (if (> (length s) 110)
                                 (concat (substring s 0 110) " ...")
                               s))))))))
    (let* ((baseline (nelisp-fallback-inventory--baseline))
           (total 0)
           (over nil))
      (dolist (kind nelisp-fallback-inventory--kinds)
        (let ((n (gethash kind counts 0))
              (limit (cdr (assq kind baseline))))
          (setq total (+ total n))
          (princ (format "%-18s %6d  baseline %s\n"
                         kind n (if limit (number-to-string limit) "ABSENT")))
          (cond
           ((null limit) (push (cons kind 'no-baseline) over))
           ((> n limit) (push (cons kind (- n limit)) over))
           ((< n limit)
            (princ (format "  ratchet available: %d below baseline\n"
                           (- limit n)))))))
      (dolist (f (nreverse unreadable))
        (princ (format "READ-FAIL %s\n" f)))
      ;; Machine-readable tail before the verdict, so it survives every exit
      ;; path.  `checked' counts files: it is the only number that can tell a
      ;; clean tree from a scan that never ran (contract: tools/ai/README.md).
      (princ (format "GATE-COUNT checked=%d findings=%d\n" scanned total))
      (let* ((pins (nelisp-fallback-inventory--pins))
             (current-counts (make-hash-table :test #'equal))
             (sample (make-hash-table :test #'equal))
             (new-sites nil))
        (dolist (site nelisp-fallback-inventory--sites)
          (let ((key (nelisp-fallback-inventory--pin-key
                      (nth 0 site) (nth 1 site) (nth 2 site) (nth 3 site))))
            (puthash key (1+ (gethash key current-counts 0)) current-counts)
            (unless (gethash key sample) (puthash key site sample))))
        (maphash
         (lambda (key n)
           (when (> n (gethash key pins 0))
             (push (cons key (gethash key sample)) new-sites)))
         current-counts)
        (setq new-sites (sort new-sites (lambda (a b) (string< (car a) (car b)))))
        (cond
         (unreadable
          (princ "fallback-inventory: FAIL (unreadable file)\n")
          (kill-emacs 1))
         (new-sites
          (princ (format "fallback-inventory: FAIL (%d site(s) exceed their `pinned-site' count -- a per-kind count can miss one handler disappearing while a different one appears, net unchanged; each of these is new or grew):\n"
                         (length new-sites)))
          (dolist (entry new-sites)
            (let ((site (cdr entry)))
              (princ (format "    %-15s %-34s %s\n"
                             (nth 0 site) (file-name-nondirectory (or (nth 1 site) "?"))
                             (or (nth 2 site) "(top level)")))))
          (princ (format "  Add or raise a `pinned-site' line for each in %s, or record/re-raise instead.\n"
                         nelisp-fallback-inventory--baseline-file))
          (kill-emacs 1))
         (over
          (dolist (o (nreverse over))
            (if (eq (cdr o) 'no-baseline)
                (princ (format "fallback-inventory: FAIL (%s has no baseline)\n"
                               (car o)))
              (princ (format "fallback-inventory: FAIL (%s +%d over baseline -- a new error handler that neither records nor re-raises; either record the fall or raise the baseline in the same commit)\n"
                             (car o) (cdr o)))))
          (kill-emacs 1))
         (t
          (let ((stale 0))
            (maphash (lambda (k _v)
                       (when (< (gethash k current-counts 0) (gethash k pins 0))
                         (setq stale (1+ stale))))
                     pins)
            (when (> stale 0)
              (princ (format "    %d `pinned-site' line(s) count higher than the tree has now -- safe to lower\n"
                             stale))))
          (princ "fallback-inventory: PASS\n")))))))

(nelisp-fallback-inventory-run)

;;; nelisp-fallback-inventory.el ends here
