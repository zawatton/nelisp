;;; 16-audit-script.el --- Phase 5-E primitive forecast audit -*- lexical-binding: t; -*-
;;
;; One-shot audit for Doc 16 §3.0.  Scans the Phase 5-E forecast
;; source sketches under `docs/design/16-forecast/' to determine
;; which host subrs are referenced but not yet present in
;; `nelisp--primitive-symbols' nor registered as NeLisp-aware
;; wrappers in `nelisp--install-primitives'.  Writes the gap list
;; to `docs/design/16-phase5e-primitive-gap.data.el'.
;;
;; Usage (from nelisp repo root):
;;
;;   emacs --batch -Q \
;;     --eval "(let ((default-directory (expand-file-name \".\"))) \
;;               (load (expand-file-name \"docs/design/16-audit-script.el\")))"
;;
;; Ported verbatim from Phase 5-C 14-audit-script.el; only the
;; directory names and output paths differ.

;;; Code:

(require 'cl-lib)

(defvar phase5e-audit--nelisp-root
  (or (and load-file-name
           (expand-file-name "../.." (file-name-directory load-file-name)))
      default-directory)
  "Absolute path to the nelisp repo root.")

(defun phase5e-audit--load-primitive-table ()
  "Extract symbols in `nelisp--primitive-symbols' plus installed wrappers."
  (let* ((eval-path (expand-file-name "src/nelisp-eval.el"
                                      phase5e-audit--nelisp-root))
         (load-path (expand-file-name "src/nelisp-load.el"
                                      phase5e-audit--nelisp-root))
         (macro-path (expand-file-name "src/nelisp-macro.el"
                                       phase5e-audit--nelisp-root))
         (table (make-hash-table :test 'eq)))
    (dolist (path (list eval-path load-path macro-path))
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (when (re-search-forward
               "^(defconst nelisp--primitive-symbols$" nil t)
          (let ((start (point)))
            (goto-char start)
            (forward-line 1)
            (let ((body-start (point)))
              (goto-char start)
              (forward-sexp 1)
              (let* ((body-end (point))
                     (body (buffer-substring-no-properties
                            body-start body-end)))
                (with-temp-buffer
                  (insert body)
                  (goto-char (point-min))
                  (condition-case nil
                      (while t
                        (let* ((form (read (current-buffer)))
                               (actual
                                (cond
                                 ((and (consp form) (eq (car form) 'quote))
                                  (cadr form))
                                 ((listp form) form)
                                 (t nil))))
                          (when actual
                            (dolist (s actual)
                              (when (symbolp s)
                                (puthash s t table))))
                          (when (and (symbolp form) (null actual))
                            (puthash form t table))))
                    (end-of-file nil)))))))
        (goto-char (point-min))
        (while (re-search-forward
                "^[[:space:]]*(puthash[[:space:]]+'\\([^[:space:]]+\\)[[:space:]]+"
                nil t)
          (let ((sym (intern (match-string 1))))
            (puthash sym t table)))))
    (dolist (s '(funcall apply mapcar mapc mapconcat maphash
                         boundp fboundp symbol-value
                         macroexpand macroexpand-1 macroexpand-all
                         require provide))
      (puthash s t table))
    table))

(defun phase5e-audit--walk (form called context-file line)
  (cond
   ((atom form) nil)
   ((vectorp form) nil)
   ((not (listp form)) nil)
   (t
    (let ((head (car form)))
      (when (symbolp head)
        (push (cons context-file line) (gethash head called))
        (puthash head (gethash head called) called)
        (when (memq head '(function funcall apply mapcar mapc
                                    mapconcat seq-map))
          (let ((arg (cadr form)))
            (cond
             ((and (consp arg) (eq (car arg) 'function)
                   (symbolp (cadr arg)))
              (let ((s (cadr arg)))
                (push (cons context-file line) (gethash s called))
                (puthash s (gethash s called) called)))
             ((and (consp arg) (eq (car arg) 'quote)
                   (symbolp (cadr arg)))
              (let ((s (cadr arg)))
                (push (cons context-file line) (gethash s called))
                (puthash s (gethash s called) called)))))))
      (let ((tail (cdr form)))
        (when (listp tail)
          (dolist (sub tail)
            (phase5e-audit--walk sub called context-file line))))))))

(defun phase5e-audit--scan-file (file called)
  (let ((rel (file-relative-name file phase5e-audit--nelisp-root)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((line (line-number-at-pos (point)))
                  (form (read (current-buffer))))
              (phase5e-audit--walk form called rel line)))
        (end-of-file nil)))))

(defun phase5e-audit--special-form-p (sym)
  (memq sym '(quote function let let* if cond progn prog1 prog2
                    and or while setq defvar defconst defun defmacro
                    lambda catch throw unwind-protect condition-case
                    save-excursion save-restriction save-current-buffer
                    with-output-to-temp-buffer with-current-buffer
                    with-temp-buffer with-temp-file
                    dolist dotimes when unless pcase pcase-let
                    pcase-let* pcase-dolist cl-loop cl-flet cl-labels
                    cl-letf cl-letf*
                    inline declare interactive eval-when-compile
                    eval-and-compile eval)))

(defun phase5e-audit--user-defined-p (sym)
  (let ((name (symbol-name sym)))
    (or (string-prefix-p "nelisp" name)
        (string-prefix-p "hello-" name)
        (string-prefix-p "phase5c-" name)
        (string-prefix-p "phase5b-" name)
        (string-prefix-p "phase5e-" name))))

(defun phase5e-audit--forecast-files ()
  (let ((dir (expand-file-name "docs/design/16-forecast"
                               phase5e-audit--nelisp-root)))
    (when (file-directory-p dir)
      (directory-files dir t "\\.el$"))))

(defun phase5e-audit--run ()
  (let* ((table (phase5e-audit--load-primitive-table))
         (called (make-hash-table :test 'eq))
         (files (phase5e-audit--forecast-files))
         (missing nil)
         (covered nil))
    (dolist (f files)
      (phase5e-audit--scan-file f called))
    (maphash
     (lambda (sym sites)
       (cond
        ((phase5e-audit--special-form-p sym) nil)
        ((phase5e-audit--user-defined-p sym) nil)
        ((not (and (fboundp sym) (subrp (indirect-function sym)))) nil)
        ((gethash sym table) (push (list sym :sites (seq-take (nreverse sites) 2))
                                   covered))
        (t (push (list sym
                       :arity (ignore-errors (subr-arity (indirect-function sym)))
                       :sites (seq-take (nreverse sites) 2))
                 missing))))
     called)
    (setq missing
          (sort missing (lambda (a b) (string< (symbol-name (car a))
                                               (symbol-name (car b))))))
    (setq covered
          (sort covered (lambda (a b) (string< (symbol-name (car a))
                                               (symbol-name (car b))))))
    (let ((out (expand-file-name
                "docs/design/16-phase5e-primitive-gap.data.el"
                phase5e-audit--nelisp-root)))
      (with-temp-file out
        (insert ";;; 16-phase5e-primitive-gap.data.el -- raw audit -*- lexical-binding: t; -*-\n")
        (insert (format ";; Generated: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S%z")))
        (insert (format ";; Scope files: %d\n" (length files)))
        (insert (format ";; MISSING host subrs (add to nelisp--primitive-symbols): %d\n"
                        (length missing)))
        (insert (format ";; COVERED host subrs (already in table): %d\n\n"
                        (length covered)))
        (insert ";;; Code:\n\n")
        (insert "(defconst phase5e-primitive-missing\n  '(\n")
        (dolist (row missing) (insert (format "    %S\n" row)))
        (insert "    ))\n\n")
        (insert "(defconst phase5e-primitive-covered\n  '(\n")
        (dolist (row covered) (insert (format "    %S\n" row)))
        (insert "    ))\n\n")
        (insert "(provide '16-phase5e-primitive-gap)\n")
        (insert ";;; 16-phase5e-primitive-gap.data.el ends here\n"))
      (message "phase5e-audit: missing=%d covered=%d scope-files=%d -> %s"
               (length missing) (length covered) (length files) out))))

(phase5e-audit--run)

;;; 16-audit-script.el ends here
