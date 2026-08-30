;;; nelisp-emacs-compat.el --- which names work on stock Emacs  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Answers one question, for every name this tree defines: would it work on
;; a stock Emacs, and if the name exists in both, which definition wins?
;;
;; Four classes:
;;
;;   nelisp-only       stock Emacs does not have the name.  NeLisp-specific:
;;                     code using it does not run on Emacs.
;;   shared-deferring  both have it, and the tree's definition is wrapped in
;;                     `(unless (fboundp ...))' -- on a host you get the
;;                     host's, off-host you get this one.  Compatible by
;;                     construction.
;;   shared-shadowing  both have it, and the tree defines it UNCONDITIONALLY.
;;                     The tree's definition wins even on a host, so loading
;;                     the file changes that Emacs.  This is the class that
;;                     bites: on 2026-08-19 three separate defects were an
;;                     unconditional definition landing on top of a working
;;                     one -- `provide'/`featurep' fset over the native
;;                     builtins while `require' stayed native, a
;;                     `string-match-p' that recognised five literal regexps,
;;                     and a prelude `error-message-string' that dropped the
;;                     error symbol.  Each cost hours precisely because
;;                     nobody could see which definition was in effect.
;;   host-only         reported for completeness: the tree references it but
;;                     never defines it.  Not computed here (this tool reads
;;                     definitions, not call sites).
;;
;; HOW THE HOST ANSWER IS OBTAINED.  This file is loaded by `emacs --batch
;; -Q' and never loads the tree -- it READS the sources as data.  So
;; `fboundp' / `boundp' in this same process are stock Emacs's own answer,
;; with no NeLisp loaded and no user init.  There is no second process to
;; keep in sync and no list of Emacs names to maintain by hand.
;;
;; LIMITS, stated rather than discovered later:
;;   - `fboundp' is t for an autoloaded name, so a name Emacs would load
;;     from a library on demand counts as present.  That is the right answer
;;     for "does Emacs have it" and the wrong one for "is it available
;;     before you require something".
;;   - A name defined behind a `when'/`unless' this tool does not recognise
;;     as an fboundp guard is read as unconditional.  That errs toward
;;     reporting MORE shadowing, which is the safe direction for a ratchet.
;;   - Emacs version dependent: the answer is this Emacs's answer.  The
;;     version is printed with the report so a diff can be read.
;;
;; Usage:
;;   emacs --batch -Q -l tools/nelisp-emacs-compat.el          # report
;;   NELISP_EMACS_COMPAT_WRITE=1 emacs --batch -Q -l tools/nelisp-emacs-compat.el
;;                                                             # regenerate table
;; or: make emacs-compat / make emacs-compat-table

;;; Code:

(defconst nelisp-emacs-compat--table-file "docs/emacs-compat-table.txt")
(defconst nelisp-emacs-compat--baseline-file "tools/emacs-compat-baseline.txt")

(defconst nelisp-emacs-compat--definers
  '(defun defmacro defsubst defvar defconst defcustom
    cl-defun cl-defmacro cl-defsubst defalias fset)
  "Heads whose second element names something this tree defines.")

(defun nelisp-emacs-compat--files ()
  "Return the sources scanned, in a stable order."
  (sort (append (file-expand-wildcards "lisp/*.el")
                (file-expand-wildcards "src/*.el")
                (file-expand-wildcards "scripts/*.el")
                (file-expand-wildcards "packages/*/src/*.el"))
        #'string<))

(defun nelisp-emacs-compat--defined-name (form)
  "Return the symbol FORM defines, or nil."
  ;; `nth' walks the cdr and dies on a dotted pair; this tree is full of
  ;; them, so every accessor here is the -safe form.
  (when (and (consp form)
             (memq (car form) nelisp-emacs-compat--definers))
    (let ((arg (car-safe (cdr-safe form))))
      (cond
       ((and arg (symbolp arg)) arg)
       ((and (consp arg) (eq (car arg) 'quote)
             (symbolp (car-safe (cdr-safe arg))))
        (car-safe (cdr-safe arg)))))))

(defun nelisp-emacs-compat--quoted-p (form)
  "Non-nil when FORM is quoted data the walker must not read as code.

Found 2026-08-19 by the review this tool exists to make possible, which is
the joke: the tool had the defect it was built to detect.  A list like
\='(defvar defconst defcustom setq setq-default) -- a definer\='s own list of
heads it recognises -- was walked as if it were a form, so its car being
`defvar\=' made its second element read as a name being defined.  That
attributed `defconst\=' to src/nelisp-cc-runtime.el, `defvar\=' to
src/nelisp-bytecode.el and `defmacro\=' to src/nelisp-macro-ns.el, none of
which define them; all three live in lisp/nelisp-stdlib-eval-special.el.

A name inside a quote is data.  Backquote is left walked on purpose: this
tree assembles runtime code as backquoted templates, and a definition there
is a definition somewhere, whereas a quoted list of head symbols is not a
definition anywhere."
  (and (consp form) (memq (car form) '(quote function))))

(defun nelisp-emacs-compat--guard-p (form)
  "Non-nil when FORM is an `unless'/`when' whose test is an fboundp/boundp check."
  (and (consp form)
       (memq (car form) '(unless when))
       (let ((test (car-safe (cdr-safe form))))
         (and (consp test) (memq (car test) '(fboundp boundp))))))

(defun nelisp-emacs-compat--walk (form guarded table file)
  "Record every definition in FORM into TABLE, noting whether it is GUARDED.
FILE is where FORM was read from; TABLE maps NAME to (KIND . FILE).

The spine is walked iteratively and only the cars are recursed into.  A
`dolist\=' over a form is wrong the moment it meets a dotted pair, and this
tree is full of them -- the same reason `nelisp-pkg--walk\=' is written this
way, a comment I read and then wrote the bug anyway."
  (let ((name (nelisp-emacs-compat--defined-name form)))
    (when name
      ;; One unguarded definition anywhere makes the name shadowing: it is
      ;; the definition that will land on a host.  The file recorded is the
      ;; one that made it so -- for a name defined in several places, the
      ;; unguarded definition is the one worth looking at.
      (let* ((prior (gethash name table))
             (plain-p (or (eq (car-safe prior) 'plain) (not guarded))))
        (puthash name
                 (cons (if plain-p 'plain 'gated)
                       (if (and prior (eq (car-safe prior) 'plain) guarded)
                           (cdr prior)
                         file))
                 table))))
  (when (and (consp form) (not (nelisp-emacs-compat--quoted-p form)))
    (let ((inner (or guarded (nelisp-emacs-compat--guard-p form)))
          (tail form))
      (while (consp tail)
        (when (consp (car tail))
          (nelisp-emacs-compat--walk (car tail) inner table file))
        (setq tail (cdr tail))))))

(defun nelisp-emacs-compat--only-blanks-p (start)
  "Non-nil when only whitespace and comments lie between START and eob."
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

(defun nelisp-emacs-compat--scan ()
  "Return a hash of NAME -> `plain\='/`gated\=' over the scanned sources.
Signals when a source cannot be read to the end.  A file that stops early
contributes only the definitions before the stopping point, and a name that
silently went missing is exactly what this tool exists to make visible --
reporting a smaller table as if it were the whole one would be the defect,
not a shortcut.  (`end-of-file\=' means two different things and they must
not be conflated: the file ended, or a form did not.  Same distinction
`nelisp-fallback-inventory--scan-file\=' draws, for the same reason.)"
  (let ((table (make-hash-table :test 'eq)))
    (dolist (file (nelisp-emacs-compat--files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((done nil))
          (while (not done)
            (let* ((start (point))
                   (form (condition-case err
                             (read (current-buffer))
                           (end-of-file
                            (if (nelisp-emacs-compat--only-blanks-p start)
                                (progn (setq done t) nil)
                              (error "emacs-compat: %s ends inside a form at %d"
                                     file start)))
                           (error
                            (error "emacs-compat: %s unreadable at %d: %s"
                                   file start (error-message-string err))))))
              (when form
                (nelisp-emacs-compat--walk form nil table file)))))))
    table))

(defun nelisp-emacs-compat--classify (name kind)
  "Return the compatibility class of NAME defined as KIND."
  (if (or (fboundp name) (boundp name))
      (if (eq kind 'gated) 'shared-deferring 'shared-shadowing)
    'nelisp-only))

(defun nelisp-emacs-compat--rows (table)
  "Return sorted \"CLASS NAME FILE\" rows for TABLE.
FILE is where the tree defines the name.  A class without a place to look
answers half the question: \"does this shadow Emacs\" is only useful next
to \"and where would I go to see what it does instead\"."
  (let ((rows nil))
    (maphash
     (lambda (name entry)
       (push (format "%-16s %-44s %s"
                     (nelisp-emacs-compat--classify name (car entry))
                     name (cdr entry))
             rows))
     table)
    (sort rows #'string<)))

(defun nelisp-emacs-compat--baseline ()
  "Return the allowed shared-shadowing count, or nil when unreadable."
  (when (file-exists-p nelisp-emacs-compat--baseline-file)
    (with-temp-buffer
      (insert-file-contents nelisp-emacs-compat--baseline-file)
      (goto-char (point-min))
      (when (re-search-forward "^shared-shadowing +\\([0-9]+\\)" nil t)
        (string-to-number (match-string 1))))))

(defun nelisp-emacs-compat-run ()
  "Classify every defined name; write the table on request; enforce the ratchet."
  (let* ((table (nelisp-emacs-compat--scan))
         (rows (nelisp-emacs-compat--rows table))
         (counts (make-hash-table :test 'eq)))
    (dolist (row rows)
      (let ((class (intern (car (split-string row)))))
        (puthash class (1+ (gethash class counts 0)) counts)))
    (princ (format "emacs-compat: emacs %s, %d files, %d defined names\n"
                   emacs-version (length (nelisp-emacs-compat--files))
                   (length rows)))
    (dolist (class '(nelisp-only shared-deferring shared-shadowing))
      (princ (format "%8d  %s\n" (gethash class counts 0) class)))
    (when (getenv "NELISP_EMACS_COMPAT_WRITE")
      (with-temp-file nelisp-emacs-compat--table-file
        (insert "# Generated by tools/nelisp-emacs-compat.el -- do not edit.\n"
                "# Regenerate: make emacs-compat-table\n"
                (format "# emacs %s, %d names\n#\n" emacs-version (length rows))
                "# nelisp-only       stock Emacs does not have this name\n"
                "# shared-deferring  both have it; this tree defers via (unless (fboundp ...))\n"
                "# shared-shadowing  both have it; this tree defines it unconditionally\n\n")
        (insert (mapconcat #'identity rows "\n") "\n"))
      (princ (format "wrote %s\n" nelisp-emacs-compat--table-file)))
    ;; `checked' counts names, so a scan that found nothing cannot read as a
    ;; clean tree.
    (princ (format "GATE-COUNT checked=%d findings=%d\n"
                   (length rows) (gethash 'shared-shadowing counts 0)))
    (let ((limit (nelisp-emacs-compat--baseline))
          (n (gethash 'shared-shadowing counts 0)))
      (cond
       ((null limit)
        (princ (format "emacs-compat: FAIL (no shared-shadowing line in %s)\n"
                       nelisp-emacs-compat--baseline-file))
        (kill-emacs 1))
       ((> n limit)
        (princ (format "emacs-compat: FAIL (shared-shadowing %d over baseline %d -- a name defined unconditionally on top of one stock Emacs already has; wrap it in (unless (fboundp ...)) or raise the baseline and say why)\n"
                       n limit))
        (kill-emacs 1))
       (t
        (when (< n limit)
          (princ (format "  ratchet available: shared-shadowing is %d, %d below baseline\n"
                         n (- limit n))))
        (princ "emacs-compat: PASS\n"))))))

(nelisp-emacs-compat-run)

;;; nelisp-emacs-compat.el ends here
