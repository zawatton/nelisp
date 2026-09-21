;;; nelisp-vendor-shadow-gate.el --- catch a new hand-written subset of a vendored file  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc segI (vendor-emacs-lisp) exists because two agents hand-wrote a
;; 150-line `rx' and six `cl-seq' functions the day before this segment,
;; when both already existed, verbatim, in vendor/emacs-lisp/ (or in the
;; upstream Emacs sources it is vendored from).  The hand-written `cl-seq'
;; subset then replaced real cl-lib inside the host test suite and turned
;; 664 of 6,175 tests red.  Writing a subset of a file this tree ALREADY
;; VENDORS is the defect this gate exists to catch, before it costs a
;; bisect again.
;;
;; This is not a general "did you duplicate any Emacs function" check --
;; that is `emacs-compat', already wired in, and it is deliberately silent
;; about names Emacs has that this tree does not vendor.  This gate is
;; narrower and louder: an UNCONDITIONAL `defun'/`defmacro' in this tree's
;; OWN sources (lisp/, src/, scripts/, packages/*/src/ -- the same file
;; set `emacs-compat' reads) for a name one of the files under
;; vendor/emacs-lisp/ ALSO defines is a name this tree could simply
;; `require' instead of writing again.
;;
;; GUARDED is exempt, same rule and same recognizer as `emacs-compat':
;; `(unless (fboundp 'NAME) (defun NAME ...))' defers to whatever already
;; won -- on the standalone, once the vendored file has been `require'd,
;; that is the vendored function itself, so a guarded definition can
;; never actually shadow it.  An UNGUARDED one always would, which is
;; exactly the shape the incident this gate answers to was.
;;
;; A small number of UNGUARDED overlaps predate this gate and are
;; intentional: this tree's own subr-x-shaped helpers (`string-trim',
;; `if-let*', ...) were measured, before vendor/emacs-lisp/emacs-lisp/
;; subr-x.el existed, to already match real Emacs's behavior, and load
;; ahead of the vendored file in `load-path' by design (a name this tree
;; defines itself must win over a vendored library, the same ordering
;; rule vendor/README.md documents) -- rewriting them to `require' the
;; vendored file instead is a separate piece of work this gate does not
;; force.  Each is listed in `tools/vendor-shadow-accepted.txt' with a
;; reason; a NEW one is not, and fails here until it is.
;;
;; HOW THE ANSWER IS OBTAINED.  Like `emacs-compat', this reads sources as
;; data (`read' in a `with-temp-buffer', never `load'), so it costs
;; seconds and never runs vendored code to find out what it defines.
;;
;; Usage: emacs --batch -Q -l tools/nelisp-vendor-shadow-gate.el
;; or: make vendor-shadow-gate

;;; Code:

(defconst nelisp-vendor-shadow--vendor-root "vendor/emacs-lisp")
(defconst nelisp-vendor-shadow--accepted-file "tools/vendor-shadow-accepted.txt")

(defconst nelisp-vendor-shadow--definers
  '(defun defmacro defsubst defalias fset)
  "Heads whose second element names something a file defines.
`cl-defun'/`cl-defmacro' are deliberately excluded: nothing in
vendor/emacs-lisp/ or this tree's own scanned sources currently uses
either, and adding them without a vendored counterpart to test against
would be an unverified guess at their exact argument shape (see
`nelisp-emacs-compat--definers', which lists both for the same reason
this file does not: it is proven against files that use them).")

(defun nelisp-vendor-shadow--tree-files ()
  "Return this tree's OWN sources, in a stable order.
Same file set `tools/nelisp-emacs-compat.el' reads, for the same
reason: it is what a host or the standalone actually loads, not a
hand-maintained list of it."
  (sort (append (file-expand-wildcards "lisp/*.el")
                (file-expand-wildcards "src/*.el")
                (file-expand-wildcards "scripts/*.el")
                (file-expand-wildcards "packages/*/src/*.el"))
        #'string<))

(defun nelisp-vendor-shadow--vendor-files ()
  "Return every `.el' file under `vendor/emacs-lisp/', in a stable order."
  (if (file-directory-p nelisp-vendor-shadow--vendor-root)
      (sort (directory-files-recursively nelisp-vendor-shadow--vendor-root "\\.el\\'")
            #'string<)
    nil))

(defun nelisp-vendor-shadow--defined-name (form)
  "Return the symbol FORM defines, or nil.
Mirrors `nelisp-emacs-compat--defined-name': `nth' walks the cdr and
dies on a dotted pair, and both trees this file reads are full of
them, so every accessor here is the -safe form."
  (when (and (consp form)
             (memq (car form) nelisp-vendor-shadow--definers))
    (let ((arg (car-safe (cdr-safe form))))
      (cond
       ((eq (car form) 'fset)
        (and (consp arg) (eq (car arg) 'quote) (symbolp (cadr arg))
             (cadr arg)))
       ((symbolp arg) arg)
       ((and (consp arg) (eq (car arg) 'quote) (symbolp (cadr arg)))
        (cadr arg))
       (t nil)))))

(defun nelisp-vendor-shadow--quoted-p (form)
  "Non-nil when FORM is a `quote' or `function' form (do not walk into it)."
  (and (consp form) (memq (car form) '(quote function))))

(defun nelisp-vendor-shadow--guard-p (form)
  "Non-nil when FORM is an `unless'/`when' whose test is an fboundp check.
Same recognizer as `nelisp-emacs-compat--guard-p', on purpose: a
definition guarded well enough to defer to Emacs itself is guarded
well enough to defer to a vendored copy of Emacs's own file."
  (and (consp form)
       (memq (car form) '(unless when))
       (let ((test (car-safe (cdr-safe form))))
         (and (consp test) (memq (car test) '(fboundp boundp))))))

(defun nelisp-vendor-shadow--walk (form guarded names-table)
  "Record every name FORM defines into NAMES-TABLE, keyed by symbol.
Value is `plain' if any unguarded definition of that name was seen,
else `gated'.  Iterative over the spine, recursing only into cars, for
the same dotted-pair reason `nelisp-emacs-compat--walk' gives."
  (let ((name (nelisp-vendor-shadow--defined-name form)))
    (when name
      (let ((prior (gethash name names-table)))
        (unless (eq prior 'plain)
          (puthash name (if guarded 'gated 'plain) names-table)))))
  (when (and (consp form) (not (nelisp-vendor-shadow--quoted-p form)))
    (let ((inner (or guarded (nelisp-vendor-shadow--guard-p form)))
          (tail form))
      (while (consp tail)
        (when (consp (car tail))
          (nelisp-vendor-shadow--walk (car tail) inner names-table))
        (setq tail (cdr tail))))))

(defun nelisp-vendor-shadow--only-blanks-p (start)
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

(defun nelisp-vendor-shadow--scan-file (file names-table guarded-default)
  "Read FILE's top-level forms into NAMES-TABLE.
GUARDED-DEFAULT is only relevant for symmetry with a future caller
that wants every name in some file treated as pre-guarded; every
current caller passes nil, i.e. \"read the file's own guards\"."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((done nil))
      (while (not done)
        (let* ((start (point))
               (form (condition-case err
                         (read (current-buffer))
                       (end-of-file
                        (if (nelisp-vendor-shadow--only-blanks-p start)
                            (progn (setq done t) nil)
                          (error "vendor-shadow-gate: %s ends inside a form at %d"
                                 file start)))
                       (error
                        (error "vendor-shadow-gate: %s unreadable at %d: %s"
                               file start (error-message-string err))))))
          (when form
            (nelisp-vendor-shadow--walk form guarded-default names-table)))))))

(defun nelisp-vendor-shadow--vendor-names ()
  "Return a hash set of every name any vendored file defines.
A name a vendored file defines behind its OWN `unless'/`when' guard
still counts here -- the point is \"does the vendored tree define
this\", not whether the vendored file itself deferred to something."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (file (nelisp-vendor-shadow--vendor-files))
      (nelisp-vendor-shadow--scan-file file table nil))
    table))

(defun nelisp-vendor-shadow--tree-names ()
  "Return NAME -> (\\='plain . FILE) or (\\='gated . FILE) over this tree's own sources.
FILE is the first UNGUARDED definer if one exists (that is the one
that would actually shadow at runtime), else the first guarded one."
  (let ((table (make-hash-table :test 'eq))
        (owners (make-hash-table :test 'eq)))
    (dolist (file (nelisp-vendor-shadow--tree-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((done nil))
          (while (not done)
            (let* ((start (point))
                   (form (condition-case err
                             (read (current-buffer))
                           (end-of-file
                            (if (nelisp-vendor-shadow--only-blanks-p start)
                                (progn (setq done t) nil)
                              (error "vendor-shadow-gate: %s ends inside a form at %d"
                                     file start)))
                           (error
                            (error "vendor-shadow-gate: %s unreadable at %d: %s"
                                   file start (error-message-string err))))))
              (when form
                (nelisp-vendor-shadow--walk-with-owner form nil table owners file)))))))
    (let ((merged (make-hash-table :test 'eq)))
      (maphash (lambda (name kind)
                 (puthash name (cons kind (gethash name owners)) merged))
               table)
      merged)))

(defun nelisp-vendor-shadow--walk-with-owner (form guarded table owners file)
  "Like `nelisp-vendor-shadow--walk' but also records FILE into OWNERS."
  (let ((name (nelisp-vendor-shadow--defined-name form)))
    (when name
      (let ((prior (gethash name table)))
        (unless (eq prior 'plain)
          (puthash name (if guarded 'gated 'plain) table)
          (when (or (not (gethash name owners)) (not guarded))
            (puthash name file owners))))))
  (when (and (consp form) (not (nelisp-vendor-shadow--quoted-p form)))
    (let ((inner (or guarded (nelisp-vendor-shadow--guard-p form)))
          (tail form))
      (while (consp tail)
        (when (consp (car tail))
          (nelisp-vendor-shadow--walk-with-owner (car tail) inner table owners file))
        (setq tail (cdr tail))))))

(defun nelisp-vendor-shadow--accepted ()
  "Return a hash set of NAME strings listed in the accepted file."
  (let ((table (make-hash-table :test 'equal)))
    (when (file-exists-p nelisp-vendor-shadow--accepted-file)
      (with-temp-buffer
        (insert-file-contents nelisp-vendor-shadow--accepted-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\([^|#\n][^|\n]*\\)|" nil t)
          (puthash (string-trim (match-string 1)) t table))))
    table))

(defun nelisp-vendor-shadow-run ()
  "Report and enforce: no NEW unguarded shadow of a vendored name."
  (let* ((vendor-files (nelisp-vendor-shadow--vendor-files))
         (vendor-names (nelisp-vendor-shadow--vendor-names))
         (tree-names (nelisp-vendor-shadow--tree-names))
         (accepted (nelisp-vendor-shadow--accepted))
         (shadows nil))
    (maphash
     (lambda (name entry)
       (when (and (eq (car entry) 'plain) (gethash name vendor-names))
         (push (list name (cdr entry)) shadows)))
     tree-names)
    (setq shadows (sort shadows (lambda (a b) (string< (symbol-name (car a))
                                                        (symbol-name (car b))))))
    (princ (format "vendor-shadow-gate: %d vendored file(s), %d vendored name(s), %d name(s) this tree defines unconditionally on top of one\n"
                   (length vendor-files) (hash-table-count vendor-names)
                   (length shadows)))
    (let ((new nil))
      (dolist (s shadows)
        (let ((name (symbol-name (car s))) (file (cadr s)))
          (if (gethash name accepted)
              (princ (format "  accepted  %-40s %s\n" name file))
            (push s new))))
      (setq new (nreverse new))
      (princ (format "GATE-COUNT checked=%d findings=%d\n"
                     (hash-table-count vendor-names) (length new)))
      (if new
          (progn
            (princ (format "\n%d name(s) not in %s:\n" (length new)
                           nelisp-vendor-shadow--accepted-file))
            (dolist (s new)
              (princ (format "  %-40s %s\n" (symbol-name (car s)) (cadr s))))
            (princ "\nvendor-shadow-gate: FAIL (wrap each in (unless (fboundp ...)) to defer to the vendored file, `require' the vendored feature at the call site instead of redefining it, or add a reasoned line to the accepted file)\n")
            (kill-emacs 1))
        (princ "vendor-shadow-gate: PASS\n")))))

(nelisp-vendor-shadow-run)

;;; nelisp-vendor-shadow-gate.el ends here
