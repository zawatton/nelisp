;;; nemacs-wrap-init.el --- wrap user init forms for isolated loading -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; M15 user-init lane — host-side generator.  Reads the user's
;; ~/.nemacs.d/early-init.el + init.el, splits them into top-level
;; forms, and emits a wrapped file where every form is bracketed by
;; `nemacs-init--begin' / `nemacs-init--ok' marker calls (defined by
;; the bridge runtime).  On the standalone reader a failing form
;; hard-aborts only itself under per-form loading, so the marker that
;; never fires identifies exactly which forms the substrate could not
;; apply — the bridge writes that as the nemacs-init-report transport
;; file instead of dying silently.
;;
;; Run under HOST Emacs (the splitter is just `read'):
;;   emacs -Q --batch -l scripts/nemacs-wrap-init.el \
;;     --eval '(nemacs-wrap-init "OUT" "early-init.el" "init.el")'

;;; Code:

(require 'seq)
;; inline.el supplies `define-inline'; the wrapper runs in a full Emacs
;; (so it is available here) and lowers each define-inline to a plain
;; fset the bridge runtime can load — see `nemacs-wrap-init--lower'.
(require 'inline)
;; cl-lib / subr-x supply the macros (cl-loop, cl-return-from, cl-incf,
;; when-let, …) that the bridge runtime cannot define (it has no defmacro);
;; the lowering macroexpand-all's each defun body here so they reach the
;; bridge as primitives — see `nemacs-wrap-init--expand-defun-body'.
(require 'cl-lib)
(require 'subr-x)

(defun nemacs-wrap-init--find-defalias (form)
  "Find a (defalias \\='NAME #\\='(lambda ...)) node anywhere in FORM.
`define-inline' macroexpands to such a node for its non-inlined
callable; the bridge runtime has no inline machinery, so we hoist
that lambda out as the package function."
  (cond
   ((not (consp form)) nil)
   ((and (eq (car form) 'defalias)
         (eq (car-safe (nth 1 form)) 'quote)
         (memq (car-safe (nth 2 form)) '(function lambda)))
    form)
   (t (let ((res nil) (xs form))
        (while (and (consp xs) (not res))
          (setq res (nemacs-wrap-init--find-defalias (car xs)))
          (setq xs (cdr xs)))
        res))))

(defun nemacs-wrap-init--fset-from-defalias (da)
  "Given a (defalias \\='NAME FN) node DA, return an equivalent `fset' form
lowered to the bridge dialect, or nil when DA's FN is not a lambda.
Strips a `declare'/`interactive' form and a leading docstring from the
lambda body -- the image evaluator runs the body verbatim and has no use
for either (a stray multiline docstring is pure bloat and can trip the
source-v1 replay reader)."
  (let* ((fn (nth 2 da))
         (lam (if (eq (car-safe fn) 'function) (nth 1 fn) fn)))
    (when (eq (car-safe lam) 'lambda)
      (let ((lbody (seq-remove
                    (lambda (f) (memq (car-safe f) '(declare interactive)))
                    (nthcdr 2 lam))))
        (when (and (stringp (car lbody)) (cdr lbody))
          (setq lbody (cdr lbody)))
        `(fset ,(nth 1 da) (lambda ,(nth 1 lam) ,@lbody))))))

(defun nemacs-wrap-init--forms (file)
  "Read all top-level forms of FILE; return a list of forms."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((forms nil))
        (condition-case nil
            (while t
              (push (read (current-buffer)) forms))
          (end-of-file nil)
          (invalid-read-syntax nil))
        (nreverse forms)))))

(defun nemacs-wrap-init--hint (form)
  "A short single-line description of FORM for the report."
  (let ((s (prin1-to-string form)))
    (setq s (replace-regexp-in-string "[\n\t]" " " s))
    (if (> (length s) 72)
        (concat (substring s 0 72) "...")
      s)))

(defvar nemacs-wrap-init--packages nil
  "Resolved package files, also written to OUT-packages so launchers
can pre-load them at shallow nesting depth.")

(defvar nemacs-wrap-init--load-path nil
  "Load-path directories collected from the init forms (host side).
The standalone reader's `load' wants absolute paths and its `require'
silently succeeds without loading, so the generator resolves
`require' against this list at wrap time.")

(defun nemacs-wrap-init--note-load-path (form)
  "Track (add-to-list 'load-path X) / (push X load-path) style forms."
  (let ((dir nil))
    (cond
     ((and (eq (car-safe form) 'add-to-list)
           (equal (nth 1 form) ''load-path))
      (setq dir (nth 2 form)))
     ((and (eq (car-safe form) 'push)
           (eq (nth 2 form) 'load-path))
      (setq dir (nth 1 form))))
    (when (stringp dir)
      (push (expand-file-name dir) nemacs-wrap-init--load-path))))

(defun nemacs-wrap-init--resolve-require (feature)
  "Find FEATURE.el under the tracked load-path dirs; return the
absolute file or nil."
  (let ((name (concat (symbol-name feature) ".el"))
        (found nil))
    (dolist (dir (reverse nemacs-wrap-init--load-path))
      (unless found
        (let ((cand (expand-file-name name dir)))
          (when (file-readable-p cand)
            (setq found cand)))))
    found))

(defun nemacs-wrap-init--expand-defun-body (name body)
  "Lower a defun BODY (of function NAME) to bridge-runnable primitives.
Strips declare/interactive and a leading docstring, wraps the rest in
`cl-block' NAME, and `macroexpand-all's it (this full Emacs has cl-lib /
subr-x) so cl-loop / cl-return-from / cl-incf / pcase / when-let etc. lower
to catch/throw/let/while/if/setq.  Returns a one-element list (the expanded
form) for splicing; on any expansion error falls back to the cleaned body
forms verbatim (no worse than before)."
  (let ((clean (seq-remove (lambda (f) (memq (car-safe f) '(declare interactive)))
                           body)))
    (when (and (stringp (car clean)) (cdr clean))
      (setq clean (cdr clean)))
    (condition-case nil
        (list (macroexpand-all (cons 'cl-block (cons name clean))))
      (error clean))))

(defun nemacs-wrap-init--lower (form)
  "Lower FORM to the runtime-image dialect the bridge executes.
The image replay evaluator does not wire `defun' / `defvar' /
`defconst' / `defcustom' (the substrate's own convention is
setq + fset), so the generator rewrites them; everything else
passes through verbatim and either applies or lands in the
report's failed list.  Macro-defining forms that themselves expand to
that dialect at a deeper nesting level -- `define-inline',
`define-minor-mode', `define-globalized-minor-mode', and any `progn'
a macroexpansion produces -- are macroexpanded and/or recursed into so
their nested defun/defvar/defalias/defcustom nodes get the same
treatment as a top-level occurrence."
  (cond
   ((not (consp form)) form)
   ((and (eq (car form) 'defun) (>= (length form) 3))
    ;; the image evaluator executes lambda bodies verbatim and has no
    ;; macro layer, so macroexpand the body to primitives here
    `(fset ',(nth 1 form)
           (lambda ,(nth 2 form)
             ,@(nemacs-wrap-init--expand-defun-body (nth 1 form)
                                                    (nthcdr 3 form)))))
   ((and (memq (car form) '(defvar defcustom)) (>= (length form) 3))
    `(if (boundp ',(nth 1 form)) nil (setq ,(nth 1 form) ,(nth 2 form))))
   ((and (eq (car form) 'defconst) (>= (length form) 3))
    `(setq ,(nth 1 form) ,(nth 2 form)))
   ;; `custom-declare-variable' is the function-call shape `defcustom'
   ;; macroexpands to (this is what a macroexpanded `define-minor-mode'
   ;; leaves for its `:global' mode variable, since a global minor mode's
   ;; variable is always a defcustom, not a plain defvar -- see
   ;; `easy-mmode.el').  Its VALUE argument is itself a quoted
   ;; funcall/lambda form (custom.el's usual "unevaluated default"
   ;; trick), so lowering re-evaluates it once with a plain `eval' rather
   ;; than trying to pattern-match the exact nested shape; the runtime
   ;; already has to support `eval'/`funcall'/`lambda' for other lowered
   ;; forms, so this adds no new primitive dependency.  `:set'/`:initialize'/
   ;; `:type' and the rest of the customize metadata are dropped -- the
   ;; bridge runtime has no customize UI to serve them.
   ((and (eq (car form) 'custom-declare-variable) (>= (length form) 3))
    `(if (boundp ,(nth 1 form)) nil
       (setq ,(cadr (nth 1 form)) (eval ,(nth 2 form) t))))
   ;; load-path bookkeeping happens at wrap time (the reader's load
   ;; wants absolute paths); the lowered form is a benign applied
   ;; marker that also aids debugging
   ((or (and (eq (car form) 'add-to-list) (equal (nth 1 form) ''load-path))
        (and (eq (car form) 'push) (eq (nth 2 form) 'load-path)))
    (nemacs-wrap-init--note-load-path form)
    `(setq nemacs-init--last-load-path-dir
           ,(expand-file-name
             (if (eq (car form) 'push) (nth 1 form) (nth 2 form)))))
   ;; require: resolved features become absolute loads; unresolved
   ;; ones lower to an undefined call so the form lands in the
   ;; report's failed list instead of the reader's silent-success
   ((and (eq (car form) 'require) (eq (car-safe (nth 1 form)) 'quote))
    (let ((file (nemacs-wrap-init--resolve-require (cadr (nth 1 form)))))
      (if file
          (progn
            ;; the launcher pre-loads these at depth 2 (deep nested
            ;; loads of big package files still trip the reader); the
            ;; guard uses our own loaded-file registry because the
            ;; reader's `provide' registers nothing (featurep is
            ;; permanently nil there).  The lowered TRANSPILE of the
            ;; package (M19-2) is emitted separately — see
            ;; `nemacs-wrap-init--emit-package'.
            (push file nemacs-wrap-init--packages)
            `(if (nemacs-init--file-loaded-p ,file)
                 nil
               (progn
                 (nemacs-init--note-file ,file)
                 (load ,file nil t))))
        '(nemacs-init--require-unresolved))))
   ;; define-inline (ht.el and many modern libs) defines its functions
   ;; through the inline DSL the bridge runtime lacks.  Macroexpand it
   ;; here (full Emacs has inline.el), hoist the non-inlined lambda out
   ;; of the generated defalias, and emit it in the bridge's fset
   ;; dialect — the same shape `defun' lowers to.  Falls back to the raw
   ;; form if anything unexpected turns up (it then lands in the report).
   ((eq (car form) 'define-inline)
    (condition-case nil
        (let* ((exp (macroexpand-all form))
               (da (nemacs-wrap-init--find-defalias exp)))
          (or (and da (nemacs-wrap-init--fset-from-defalias da)) form))
      (error form)))
   ;; A bare top-level `defalias' -- either from a user/package form
   ;; written directly, or (more commonly here) a nested progn element a
   ;; macroexpansion (define-minor-mode below) left behind -- lowers the
   ;; same way `define-inline' does above, straight to `fset'.  This
   ;; means the bridge runtime never has to define `defalias' itself.
   ((and (eq (car form) 'defalias) (>= (length form) 3)
         (eq (car-safe (nth 1 form)) 'quote))
    (or (nemacs-wrap-init--fset-from-defalias form) form))
   ;; define-minor-mode / define-globalized-minor-mode (marginalia-mode,
   ;; which-key-mode, and most other minor-mode-defining packages) expand
   ;; through easy-mmode.el into a `progn' of defvar/defvar-local/defcustom
   ;; (mode variable), a `defalias' (the mode command), hook plumbing, and
   ;; keymap/lighter registration -- the same "runtime has no macro layer"
   ;; problem `define-inline' solves above, just with more moving parts.
   ;; Macroexpand it here (full Emacs has easy-mmode.el loaded), then feed
   ;; the result straight back into `nemacs-wrap-init--lower': the `progn'
   ;; clause below recurses into every nested defvar/defvar-local-expansion/
   ;; custom-declare-variable/defalias node so each gets the same lowering
   ;; a top-level occurrence would.  Anything left over (add-minor-mode,
   ;; put, make-variable-buffer-local, run-hooks, ...) passes through as
   ;; plain function calls, same as any other form the bridge runtime
   ;; already has to support.  Falls back to the raw form on any
   ;; expansion error (no worse than before; it then lands in the report).
   ((memq (car form) '(define-minor-mode define-globalized-minor-mode))
    (condition-case nil
        (nemacs-wrap-init--lower (macroexpand-all form))
      (error form)))
   ;; Generic recursion: a macroexpansion (define-minor-mode above, but
   ;; potentially any other macro a user/package form uses) commonly
   ;; produces a top-level `progn' whose own elements are themselves
   ;; defvar/defalias/defconst/further-nested-progn nodes that still need
   ;; lowering.  Lower every element instead of passing the whole `progn'
   ;; through verbatim; atoms and forms with no matching clause above
   ;; fall through to the `t' catch-all unchanged, so this is additive
   ;; and does not change any existing top-level defun/defvar/require/
   ;; define-inline lowering.
   ((eq (car form) 'progn)
    (cons 'progn (mapcar #'nemacs-wrap-init--lower (cdr form))))
   (t form)))

(defun nemacs-wrap-init (out &rest files)
  "Write the wrapped init to OUT from the forms of FILES (in order)."
  (let ((n 0))
    (setq nemacs-wrap-init--load-path nil)
    (setq nemacs-wrap-init--packages nil)
    (with-temp-file out
      (insert ";;; nemacs-init-wrapped --- generated by nemacs-wrap-init.el\n")
      (insert ";;; Do not edit; regenerated by the nemacs launcher.\n")
      (dolist (file files)
        (dolist (form (nemacs-wrap-init--forms file))
          (setq n (1+ n))
          (insert (format "(nemacs-init--begin %d %S)\n"
                          n (nemacs-wrap-init--hint form)))
          ;; the ok marker shares the form's top-level unit: when the
          ;; form hard-aborts on the reader, the marker is skipped too,
          ;; which is exactly what the report needs
          (insert "(progn\n")
          (prin1 (nemacs-wrap-init--lower form) (current-buffer))
          (insert (format "\n(nemacs-init--ok %d))\n" n)))))
    (with-temp-file (concat out "-packages")
      (dolist (file (reverse nemacs-wrap-init--packages))
        (insert file "\n")))
    ;; M19-2: a lowered TRANSPILE of each resolved package, for the
    ;; image evaluator (it drops defun, so the GUI editor runtime
    ;; cannot use the raw files).  Every lowered form is emitted as
    ;; its own top-level unit: forms the runtime cannot evaluate
    ;; (defmacro bodies etc.) abort alone and the rest of the package
    ;; still defines.  The session runtime loads this once.
    (with-temp-file (concat out "-pkgs-lowered")
      ;; multibyte bytes anywhere in a source-v1 image crash the
      ;; replay reader, so docstrings print as pure-ASCII escapes;
      ;; print-length/level must stay nil (no truncation)
      (let ((print-escape-nonascii t)
            (print-escape-multibyte t)
            (print-length nil)
            (print-level nil))
        (insert ";;; generated by nemacs-wrap-init.el (M19-2)\n")
        (dolist (file (reverse nemacs-wrap-init--packages))
          (insert (format ";; --- %s\n" file))
          (dolist (form (nemacs-wrap-init--forms file))
            (let ((lowered (nemacs-wrap-init--lower form)))
              (prin1 lowered (current-buffer))
              (insert "\n"))))))
    n))

(provide 'nemacs-wrap-init)

;;; nemacs-wrap-init.el ends here
