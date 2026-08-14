;;; nelisp-standalone-lint-stash.el --- census bf_* builtins for bare-abort risk  -*- lexical-binding: t; -*-

;; Warn-only census for the "flagless abort" defect class (an internal
;; builtin returns a nonzero rc WITHOUT stashing a signal into the M6 slots
;; -- flag word @268435472, TAG @268435480, VAL @268435512, epoch counter
;; @268435544).  A bare abort of that shape silently loses its error class
;; the moment it crosses a `condition-case', and used to be silently
;; swallowed on the cold route entirely (see the `nl_eval_source_report_bare_abort'
;; / `nl_eval_source_print_bare_abort' machinery this census motivated).
;;
;; This is a STATIC, HEURISTIC census, not a proof: it flags any `bf_*'
;; builtin whose body (a) contains no symbol matching "stash" anywhere, AND
;; (b) has some tail-position leaf that is either a nonzero integer literal
;; or a call out to another bf_/nl_/m5_-prefixed helper (whose own rc is
;; being propagated unchecked).  False positives are expected and accepted
;; -- e.g. a `bf_*' that returns a nonzero literal as an ordinary boolean
;; "true" (not an error rc), or that calls a callee which itself always
;; stashes on failure two frames down.  The goal is a list to triage, not a
;; verified defect list; this never gates the build (see `lint-stash' in
;; the Makefile -- always exits 0).
;;
;; Usage:  emacs --batch -Q -L lisp -L src -L scripts \
;;           -l nelisp-standalone-lint-stash -f nelisp-standalone-lint-stash

;;; Code:

(defconst nelisp-standalone-lint-stash--build-file
  (or (and load-file-name
           (expand-file-name "nelisp-standalone-build.el"
                              (file-name-directory load-file-name)))
      (locate-library "nelisp-standalone-build.el")
      "nelisp-standalone-build.el")
  "Path to the standalone-build DSL source this census reads.")

(defun nelisp-standalone-lint-stash--read-all-forms (file)
  "Read every top-level form in FILE without evaluating any of them."
  (let (forms)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil)))
    (nreverse forms)))

(defun nelisp-standalone-lint-stash--bf-name-p (sym)
  "Non-nil when SYM looks like a `bf_*' builtin name."
  (and (symbolp sym)
       (string-prefix-p "bf_" (symbol-name sym))))

(defun nelisp-standalone-lint-stash--collect-bf-defuns (forms)
  "Walk FORMS (and every nested cons) for `(defun bf_NAME ARGS . BODY)'.
These live at arbitrary nesting depth: the file is a code-generation DSL, so
most `defun' forms for `bf_*' builtins sit inside backquoted list literals
that are themselves the body of a real (outer) elisp `defun'/`defconst', not
at the file's own top level.  Returns an alist of (NAME . BODY).

Iterative (explicit worklist, not recursion): the file is ~20K lines and its
cons tree is deep enough that a naive recursive car/cdr walk overflows
`max-lisp-eval-depth' (measured: `excessive-lisp-nesting' at depth 1601 on a
straightforward recursive version of this walk)."
  (let ((stack (copy-sequence forms))
        (found nil))
    (while stack
      (let ((x (pop stack)))
        (when (consp x)
          (if (and (eq (car-safe x) 'defun)
                   (nelisp-standalone-lint-stash--bf-name-p (nth 1 x)))
              (push (cons (nth 1 x) (nthcdr 3 x)) found)
            (progn (push (car x) stack) (push (cdr x) stack))))))
    (nreverse found)))

(defun nelisp-standalone-lint-stash--flatten (form)
  "Return every atom reachable from FORM via car/cdr, as a flat list.
Iterative (explicit worklist), for the same depth reason as
`nelisp-standalone-lint-stash--collect-bf-defuns'."
  (let ((stack (list form))
        (atoms nil))
    (while stack
      (let ((x (pop stack)))
        (if (consp x)
            (progn (push (car x) stack) (push (cdr x) stack))
          (when x (push x atoms)))))
    atoms))

(defun nelisp-standalone-lint-stash--has-stash-p (body)
  "Non-nil when some symbol anywhere in BODY has \"stash\" in its name."
  (catch 'found
    (dolist (atom (nelisp-standalone-lint-stash--flatten body))
      (when (and (symbolp atom)
                 (string-match-p "stash" (symbol-name atom)))
        (throw 'found t)))
    nil))

(defun nelisp-standalone-lint-stash--tail-leaves (forms)
  "Resolve the implicit-progn tail position(s) of FORMS (a body list) down
to their leaf sub-forms, expanding this DSL's control-flow shapes
(seq/progn/if/cond/let/let*/when/unless) so the leaves approximate every
value the body could actually return."
  (if (null forms)
      nil
    (nelisp-standalone-lint-stash--tail-leaves-1 (car (last forms)))))

(defun nelisp-standalone-lint-stash--tail-leaves-1 (form)
  (if (not (consp form))
      (list form)
    (let ((head (car form)))
      (cond
       ((memq head '(seq progn)) (nelisp-standalone-lint-stash--tail-leaves (cdr form)))
       ((eq head 'if)
        (append (nelisp-standalone-lint-stash--tail-leaves-1 (nth 2 form))
                (if (nthcdr 3 form)
                    (nelisp-standalone-lint-stash--tail-leaves-1 (nth 3 form))
                  '(nil))))
       ((eq head 'cond)
        (apply #'append
               (mapcar #'nelisp-standalone-lint-stash--tail-leaves (cdr form))))
       ((memq head '(let let*)) (nelisp-standalone-lint-stash--tail-leaves (cddr form)))
       ((memq head '(when unless)) (nelisp-standalone-lint-stash--tail-leaves (cddr form)))
       (t (list form))))))

(defun nelisp-standalone-lint-stash--leaf-risky-p (leaf)
  "Non-nil when LEAF (a tail-position return value) looks like it could
carry a nonzero rc: a nonzero integer literal, or a call out to another
bf_/nl_/m5_-prefixed helper whose return value is propagated unchecked."
  (or (and (integerp leaf) (/= leaf 0))
      (and (consp leaf)
           (symbolp (car leaf))
           (string-match-p "\\`\\(bf_\\|nl_\\|m5_\\)" (symbol-name (car leaf))))))

(defun nelisp-standalone-lint-stash--can-return-nonzero-p (body)
  (catch 'found
    (dolist (leaf (nelisp-standalone-lint-stash--tail-leaves body))
      (when (nelisp-standalone-lint-stash--leaf-risky-p leaf)
        (throw 'found t)))
    nil))

(defun nelisp-standalone-lint-stash ()
  "Print the bf_* stash census and exit 0 unconditionally (warn-only)."
  (let* ((file nelisp-standalone-lint-stash--build-file)
         (forms (nelisp-standalone-lint-stash--read-all-forms file))
         (defuns (nelisp-standalone-lint-stash--collect-bf-defuns forms))
         (total (length defuns))
         (with-stash 0)
         (flagged nil))
    (dolist (entry defuns)
      (let* ((name (car entry))
             (body (cdr entry))
             (has-stash (nelisp-standalone-lint-stash--has-stash-p body)))
        (if has-stash
            (setq with-stash (1+ with-stash))
          (when (nelisp-standalone-lint-stash--can-return-nonzero-p body)
            (push name flagged)))))
    (setq flagged (nreverse flagged))
    (princ (format "STASH-LINT: file=%s\n" file))
    (princ (format "STASH-LINT: total bf_* builtins=%d\n" total))
    (princ (format "STASH-LINT: with stash reference=%d\n" with-stash))
    (princ (format "STASH-LINT: flagged (no stash, tail can be nonzero)=%d\n"
                   (length flagged)))
    (dolist (name flagged)
      (princ (format "STASH-LINT: %s\n" name)))
    ;; Warn-only: never fail the build from this census.
    (kill-emacs 0)))

(provide 'nelisp-standalone-lint-stash)

;;; nelisp-standalone-lint-stash.el ends here
