;;; nl-check-inventory.el --- CI unsafe-surface inventory via nl-check -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 170 section 4.3: "have nl-check report the unsafe surface and
;; track its growth in CI", and the safe-core/unsafe-kernel structure the
;; same section cites from Rust.
;;
;; Two questions, and only one of them is a number.
;;
;; ESCAPE (gated, no baseline).  Every file under lisp/ and scripts/ that
;; tools/unsafe-kernel.txt does not name must contain no unsafe-primitive
;; call at all, quoted or not.  Raw memory leaving the kernel is the event
;; worth stopping, it is yes/no, and it dissolves the quoted-form problem:
;; a generator body and a plain call are the same question once the
;; question is "does this file belong to the kernel".
;;
;; GROWTH (gated two ways).  Batch driver: scan every .el under lisp/ and
;; scripts/ with `nl-check-file', count `unsafe-call' findings (unsafe
;; primitives called outside an `nl-unsafe' block; quoted forms -- i.e. the
;; AOT grammar data -- are not scanned by design), and fail when the total
;; EXCEEDS the checked-in baseline in tools/unsafe-inventory-baseline.txt.
;; A total below the baseline does not fail; it prints a ratchet notice so
;; the baseline can be lowered in the same change that shrank the surface.
;;
;; A bare count cannot see a swap: one kernel file's calls could drop while
;; a DIFFERENT kernel file's calls rise by the same amount, and `total'
;; alone would not move.  Two identity-pinned sets close that, the same way
;; `tools/parity-coverage-baseline.txt' pins covered names rather than only
;; a count:
;;
;;   `pinned-kernel-touch' in tools/unsafe-kernel.txt is the file set behind
;;   `kernel-files' -- every kernel file that touches raw memory at all
;;   (unsafe-call OR a quoted form).  A file leaving the set is still green;
;;   a file neither pinned nor previously counted starting to touch raw
;;   memory fails and names the file, whatever `kernel-files' does.
;;
;;   `pinned-kernel-call' in tools/unsafe-inventory-baseline.txt is the
;;   per-file `unsafe-call' count behind `total' (three files today).  A
;;   file's count falling is still green; a file's count rising past its
;;   pin, or a file gaining its first unquoted call outside the pinned set,
;;   fails and names the file with its old and new counts.
;;
;; Both are generated from a clean tree, not hand-typed; see the header of
;; either baseline file for how.
;;
;; Run from the repo root:
;;   emacs --batch -Q -L packages/nl-prelude/src -L packages/nl-safe/src \
;;     -L packages/nl-check/src -l tools/nl-check-inventory.el
;; or: make unsafe-inventory

;;; Code:

(require 'nl-check)

(defun nl-check-inventory--minus (a b)
  "Return the elements of A (a list of strings) not present in B."
  (let ((have (make-hash-table :test #'equal)) (out nil))
    (dolist (x b) (puthash x t have))
    (dolist (x a) (unless (gethash x have) (push x out)))
    (nreverse out)))

(defconst nl-check-inventory--baseline-file
  "tools/unsafe-inventory-baseline.txt")

(defconst nl-check-inventory--kernel-file
  "tools/unsafe-kernel.txt")

(defun nl-check-inventory--kernel ()
  "Return (PATTERNS . COUNT) from the kernel file, or nil when unreadable.
PATTERNS are anchored regexps; COUNT is the ratcheted number of kernel
files that hold an unsafe call, or nil when the file names no count.
Lines starting `pinned-kernel-touch' are a different table -- read by
`nl-check-inventory--touch-pins' -- and are skipped here so they are
never mistaken for a glob pattern."
  (when (file-exists-p nl-check-inventory--kernel-file)
    (with-temp-buffer
      (insert-file-contents nl-check-inventory--kernel-file)
      (goto-char (point-min))
      (let ((patterns nil) (count nil))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring (line-beginning-position)
                                                     (line-end-position)))))
            (cond
             ((or (string-empty-p line) (string-prefix-p "#" line)) nil)
             ((string-prefix-p "pinned-kernel-touch " line) nil)
             ((string-match "\\`kernel-files +\\([0-9]+\\)\\'" line)
              (setq count (string-to-number (match-string 1 line))))
             (t (setq patterns
                      (cons (concat "\\`" (wildcard-to-regexp line))
                            patterns)))))
          (forward-line 1))
        (cons (nreverse patterns) count)))))

(defun nl-check-inventory--touch-pins ()
  "Return the kernel files pinned as touching raw memory today.
Read from `pinned-kernel-touch NAME' lines in
`nl-check-inventory--kernel-file'.  This is the set behind `kernel-files':
a file leaving it is still green, a file joining it that was not already
pinned fails and is named."
  (let ((names nil))
    (when (file-exists-p nl-check-inventory--kernel-file)
      (with-temp-buffer
        (insert-file-contents nl-check-inventory--kernel-file)
        (goto-char (point-min))
        (while (re-search-forward "^pinned-kernel-touch +\\([^ \n]+\\)" nil t)
          (push (match-string 1) names))))
    (nreverse names)))

(defun nl-check-inventory--call-pins ()
  "Return an alist of (FILE . COUNT) from `pinned-kernel-call' lines in
`nl-check-inventory--baseline-file'.  This is the per-file breakdown
behind `total': a file's count falling is still green, a file's count
rising past its pin -- or a new file gaining a call -- fails and is
named with its old and new counts."
  (let ((rows nil))
    (when (file-exists-p nl-check-inventory--baseline-file)
      (with-temp-buffer
        (insert-file-contents nl-check-inventory--baseline-file)
        (goto-char (point-min))
        (while (re-search-forward
                "^pinned-kernel-call +\\([^ \n]+\\) +\\([0-9]+\\)" nil t)
          (push (cons (match-string 1) (string-to-number (match-string 2)))
                rows))))
    (nreverse rows)))

(defun nl-check-inventory--kernel-p (path patterns)
  "Return non-nil when PATH is inside the kernel described by PATTERNS."
  (let ((rel (file-relative-name path))
        (hit nil))
    (dolist (rx patterns)
      (when (string-match-p rx rel) (setq hit t)))
    hit))

(defun nl-check-inventory--baseline ()
  "Read the integer baseline, or nil when there is no `unsafe-call\=' line.
A named line rather than the whole buffer, so the file can carry the
reason for its number the way tools/ns-inventory-baseline.txt and
tools/fallback-inventory-baseline.txt carry theirs -- `string-to-number\='
on the buffer would read 0 from any file that led with a comment, and
a baseline of 0 is a different claim from an unreadable one."
  (when (file-exists-p nl-check-inventory--baseline-file)
    (with-temp-buffer
      (insert-file-contents nl-check-inventory--baseline-file)
      (goto-char (point-min))
      (when (re-search-forward "^unsafe-call +\\([0-9]+\\)" nil t)
        (string-to-number (match-string 1))))))

(defconst nl-check-inventory--quoted-top 6
  "How many files to name in the quoted-form report.")

(defun nl-check-inventory--report-quoted (rows total)
  "Print the quoted-form tally: ROWS is (FILE . COUNT), TOTAL their sum.
Reported, never gated.  A gate that says 531 without saying what it
excludes gets read as \"531 is the unsafe surface\", and here that is
wrong by an order of magnitude: this tree writes its runtime as quoted
generator bodies, so most of the unsafe kernel is inside a quote and
the scan steps over all of it by design (the same rule correctly
excludes opcode tables like (ptr-read-u16 . 39), which are data).
Printing the excluded count turns a silent exclusion into a stated
one, and leaves the question of whether to gate it to whoever reads
the number."
  (princ (format "\n  not counted, inside quoted forms (reported, not gated): %d\n"
                 total))
  (let ((shown 0))
    (dolist (row rows)
      (when (< shown nl-check-inventory--quoted-top)
        (princ (format "%6d  %s\n" (cdr row) (car row)))
        (setq shown (+ shown 1)))))
  (when (> (length rows) nl-check-inventory--quoted-top)
    (princ (format "         ... and %d more file(s)\n"
                   (- (length rows) nl-check-inventory--quoted-top)))))

(defun nl-check-inventory-run ()
  "Scan lisp/ and scripts/, print the inventory, enforce the baseline."
  (let* ((kernel (nl-check-inventory--kernel))
         (kernel-patterns (car kernel))
         (kernel-baseline (cdr kernel))
         (kernel-count 0)
         (kernel-touched nil)
         (kernel-calls nil)
         (escapes nil)
         (total 0)
         (scanned 0)
         (failed nil)
         (quoted-total 0)
         (quoted-rows nil))
    (dolist (dir '("lisp" "scripts"))
      (dolist (f (directory-files dir t "\\.el\\'"))
        (setq scanned (+ scanned 1))
        (condition-case err
            (let ((n (length (nl-check-findings-of-kind
                              (nl-check-file f) 'unsafe-call)))
                  (q (length (nl-check-file-quoted-unsafe f))))
              (when (> n 0)
                (princ (format "%6d  %s\n" n f)))
              (setq total (+ total n))
              (setq quoted-total (+ quoted-total q))
              (when (> q 0)
                (setq quoted-rows (cons (cons f q) quoted-rows)))
              (when (> (+ n q) 0)
                (if (nl-check-inventory--kernel-p f kernel-patterns)
                    (progn
                      (setq kernel-count (+ kernel-count 1))
                      (push (file-relative-name f) kernel-touched)
                      (when (> n 0)
                        (push (cons (file-relative-name f) n) kernel-calls)))
                  (setq escapes (cons (list (file-relative-name f) n q)
                                      escapes)))))
          (error
           (princ (format "READ-FAIL %s: %S\n" f err))
           (setq failed t)))))
    (nl-check-inventory--report-quoted
     (sort quoted-rows (lambda (a b) (> (cdr a) (cdr b))))
     quoted-total)
    (princ (format "\n  unsafe kernel: %d file(s) hold a call, baseline %s\n"
                   kernel-count (or kernel-baseline "ABSENT")))
    (when escapes
      (princ (format "  OUTSIDE the kernel: %d file(s)\n" (length escapes)))
      (dolist (e (sort escapes (lambda (a b) (string< (car a) (car b)))))
        (princ (format "      %-46s call=%-4d quoted=%d\n"
                       (nth 0 e) (nth 1 e) (nth 2 e)))))
    (let* ((baseline (nl-check-inventory--baseline))
           (touch-pins (nl-check-inventory--touch-pins))
           (new-touch (nl-check-inventory--minus kernel-touched touch-pins))
           (call-pins (nl-check-inventory--call-pins))
           (call-new nil)
           (call-grew nil))
      (dolist (c (sort (copy-sequence kernel-calls)
                       (lambda (a b) (string< (car a) (car b)))))
        (let ((pin (assoc (car c) call-pins)))
          (cond
           ((null pin) (push c call-new))
           ((> (cdr c) (cdr pin)) (push (list (car c) (cdr pin) (cdr c)) call-grew)))))
      (setq new-touch (sort new-touch #'string<))
      (setq call-new (nreverse call-new))
      (setq call-grew (nreverse call-grew))
      (princ (format "unsafe-inventory: total=%d baseline=%s\n"
                     total (or baseline "ABSENT")))
      ;; Machine-readable tail, before the verdict so it survives every
      ;; exit path.  `total' counts findings; `checked' counts files,
      ;; and only the second one can show that the scan happened at all
      ;; (contract: tools/ai/README.md).
      (princ (format "GATE-COUNT checked=%d findings=%d\n" scanned total))
      (cond
       (failed
        (princ "unsafe-inventory: FAIL (unreadable file)\n")
        (kill-emacs 1))
       ((null kernel-patterns)
        (princ (format "unsafe-inventory: FAIL (no kernel patterns in %s)\n"
                       nl-check-inventory--kernel-file))
        (kill-emacs 1))
       (escapes
        (princ (format "unsafe-inventory: FAIL (%d file(s) outside the kernel call an unsafe primitive -- move the code into a kernel file, or add the file to %s and say there what it does with raw memory)\n"
                       (length escapes) nl-check-inventory--kernel-file))
        (kill-emacs 1))
       ((null kernel-baseline)
        (princ (format "unsafe-inventory: FAIL (no kernel-files line in %s)\n"
                       nl-check-inventory--kernel-file))
        (kill-emacs 1))
       (new-touch
        (princ (format "unsafe-inventory: FAIL (%d kernel file(s) touch raw memory for the first time -- not in `pinned-kernel-touch'):\n"
                       (length new-touch)))
        (dolist (f new-touch) (princ (format "    %s\n" f)))
        (princ (format "  Add a `pinned-kernel-touch' line in %s and say what it does.\n"
                       nl-check-inventory--kernel-file))
        (kill-emacs 1))
       ((> kernel-count kernel-baseline)
        (princ (format "unsafe-inventory: FAIL (%d kernel file(s) hold an unsafe call, baseline %d -- a file the patterns already covered has started touching raw memory; raise the baseline and say there what it does)\n"
                       kernel-count kernel-baseline))
        (kill-emacs 1))
       ((null baseline)
        (princ "unsafe-inventory: FAIL (no baseline file)\n")
        (kill-emacs 1))
       ((or call-new call-grew)
        (princ (format "unsafe-inventory: FAIL (%d kernel file(s) call an unsafe primitive more than their `pinned-kernel-call' line allows):\n"
                       (+ (length call-new) (length call-grew))))
        (dolist (c call-new)
          (princ (format "    %-52s NEW, %d call(s)\n" (car c) (cdr c))))
        (dolist (c call-grew)
          (princ (format "    %-52s %d -> %d\n" (nth 0 c) (nth 1 c) (nth 2 c))))
        (princ (format "  Raise the `pinned-kernel-call' line(s) in %s and say why.\n"
                       nl-check-inventory--baseline-file))
        (kill-emacs 1))
       ((> total baseline)
        (princ (format "unsafe-inventory: FAIL (+%d over baseline -- new unsafe-primitive calls outside nl-unsafe; either wrap them or consciously raise the baseline in the same commit)\n"
                       (- total baseline)))
        (kill-emacs 1))
       (t
        (when (< kernel-count kernel-baseline)
          (princ (format "    ratchet available: %d kernel file(s), %d below baseline\n"
                         kernel-count (- kernel-baseline kernel-count))))
        (when (< total baseline)
          (princ (format "    ratchet available: unsafe-call is %d below baseline\n"
                         (- baseline total))))
        (let ((stale-touch (nl-check-inventory--minus touch-pins kernel-touched)))
          (when stale-touch
            (princ (format "    %d `pinned-kernel-touch' line(s) no longer touch raw memory -- safe to drop:\n"
                           (length stale-touch)))
            (dolist (f (sort stale-touch #'string<)) (princ (format "      %s\n" f)))))
        (princ "unsafe-inventory: PASS\n"))))))

(nl-check-inventory-run)

;;; nl-check-inventory.el ends here
