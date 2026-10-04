;;; ccore-prelude-splice.el --- splice lane overrides into the runtime prelude  -*- lexical-binding: t; -*-

;;; Commentary:
;; Usage:
;;   emacs -Q --batch -l tools/ai/ccore-prelude-splice.el \
;;     --eval '(ccore-prelude-splice "PRELUDE" "OVERRIDE" "NAMES" "OUTPUT")'
;;
;; OVERRIDE is a prelude fix lane's result: complete replacement `defun's for
;; the primitives listed in NAMES (one per line) plus private helpers.  Each
;; replacement is spliced over the single existing `(defun NAME' form in
;; PRELUDE, wherever it is nested, leaving every other byte of the prelude
;; untouched.  Helper forms are inserted as one block at top level just
;; before the top-level form that holds the earliest replacement.  The result
;; is written to OUTPUT (which may be PRELUDE itself).
;;
;; One line per name is printed: `SPLICED NAME', or `MISSING NAME' when the
;; prelude has no such definition, or `AMBIGUOUS NAME' when it has several.
;; Missing and ambiguous names are left alone, their override text is not
;; inserted, and the exit status is 1 so the coordinator handles them.

;;; Code:

(defun ccore-prelude-splice--forms (file)
  "Return the top-level forms of FILE as a list of (FORM . TEXT)."
  (let ((forms nil))
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let* ((form (read (current-buffer)))
                   (end (point))
                   (start (scan-sexps end -1)))
              (push (cons form (buffer-substring-no-properties start end)) forms)))
        (end-of-file nil)))
    (nreverse forms)))

(defun ccore-prelude-splice--definition-ranges (name)
  "Return the (START . END) ranges of `(defun NAME' forms in the current buffer.
Occurrences inside strings or comments are ignored."
  (let ((ranges nil)
        (regexp (concat "(defun[ \t\n]+" (regexp-quote name) "[ \t\n]")))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((start (match-beginning 0)))
        (unless (save-excursion (nth 8 (syntax-ppss start)))
          (push (cons start (scan-sexps start 1)) ranges))))
    (nreverse ranges)))

(defun ccore-prelude-splice (prelude override names-file output)
  "Splice OVERRIDE's definitions of NAMES-FILE's names into PRELUDE; write OUTPUT."
  (let* ((names (with-temp-buffer
                  (insert-file-contents names-file)
                  (split-string (buffer-string) "[ \n]+" t)))
         (forms (ccore-prelude-splice--forms override))
         (replacements nil) (helpers nil) (failed nil))
    (dolist (cell forms)
      (let ((form (car cell)))
        (if (and (eq (car-safe form) 'defun) (member (symbol-name (cadr form)) names))
            (push (cons (symbol-name (cadr form)) (cdr cell)) replacements)
          (push (cdr cell) helpers))))
    (setq replacements (nreverse replacements) helpers (nreverse helpers))
    (with-temp-buffer
      (insert-file-contents prelude)
      (emacs-lisp-mode)
      (let ((edits nil))
        (dolist (cell replacements)
          (let ((ranges (ccore-prelude-splice--definition-ranges (car cell))))
            (cond
             ((null ranges)
              (princ (format "MISSING %s\n" (car cell))) (setq failed t))
             ((cdr ranges)
              (princ (format "AMBIGUOUS %s\n" (car cell))) (setq failed t))
             (t (push (list (caar ranges) (cdar ranges) (cdr cell) (car cell)) edits)))))
        (when edits
          ;; Helpers go before the top-level form holding the earliest edit.
          (let* ((earliest (apply #'min (mapcar #'car edits)))
                 (anchor (save-excursion
                           (goto-char earliest)
                           (let ((state (syntax-ppss)))
                             (if (nth 9 state) (car (nth 9 state)) earliest)))))
            ;; Apply from the end so earlier positions stay valid.
            (dolist (edit (sort edits (lambda (a b) (> (car a) (car b)))))
              (goto-char (car edit))
              (delete-region (car edit) (cadr edit))
              (insert (nth 2 edit))
              (princ (format "SPLICED %s\n" (nth 3 edit))))
            (when helpers
              (goto-char anchor)
              (insert (mapconcat #'identity helpers "\n\n") "\n\n"))))
        (check-parens)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region (point-min) (point-max) output nil 'silent))))
    (kill-emacs (if failed 1 0))))

;;; ccore-prelude-splice.el ends here
