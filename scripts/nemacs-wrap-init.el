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

(defun nemacs-wrap-init--lower (form)
  "Lower FORM to the runtime-image dialect the bridge executes.
The image replay evaluator does not wire `defun' / `defvar' /
`defconst' / `defcustom' (the substrate's own convention is
setq + fset), so the generator rewrites them; everything else
passes through verbatim and either applies or lands in the
report's failed list."
  (cond
   ((not (consp form)) form)
   ((and (eq (car form) 'defun) (>= (length form) 3))
    `(fset ',(nth 1 form) (lambda ,(nth 2 form) ,@(nthcdr 3 form))))
   ((and (memq (car form) '(defvar defcustom)) (>= (length form) 3))
    `(if (boundp ',(nth 1 form)) nil (setq ,(nth 1 form) ,(nth 2 form))))
   ((and (eq (car form) 'defconst) (>= (length form) 3))
    `(setq ,(nth 1 form) ,(nth 2 form)))
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
            ;; permanently nil there)
            (push file nemacs-wrap-init--packages)
            `(if (nemacs-init--file-loaded-p ,file)
                 nil
               (progn
                 (nemacs-init--note-file ,file)
                 (load ,file nil t))))
        '(nemacs-init--require-unresolved))))
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
    n))

(provide 'nemacs-wrap-init)

;;; nemacs-wrap-init.el ends here
