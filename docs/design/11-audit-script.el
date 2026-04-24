;;; 11-audit-script.el --- NeLisp host-C-dependency gap audit -*- lexical-binding: t; -*-
;;
;; One-shot script for docs/design/11-phase5-gap-audit.org.  Intentionally
;; not registered as a module — running from pristine Emacs batch keeps
;; the subr enumeration free of anvil/NeLisp/user package contamination.
;;
;; Usage (from nelisp/ repo root):
;;
;;   emacs --batch -Q \
;;     --eval "(let ((default-directory (expand-file-name \".\"))) \
;;               (load (expand-file-name \"docs/design/11-audit-script.el\")))"
;;
;; Writes `docs/design/11-phase5-gap-audit.data.el' (Lisp-readable alist).
;;
;;; Code:

(require 'cl-lib)

(defvar gap-audit--nelisp-root
  (or (and load-file-name
           (expand-file-name "../.." (file-name-directory load-file-name)))
      default-directory)
  "Absolute path to the nelisp repo root.")

(defun gap-audit--collect-host-subrs ()
  "Return the set (hash-table) of host-Emacs C subrs.
Keys are symbols whose `indirect-function' is a subrp.  Aliases
resolve to their target — `first' and `cl-first' both map onto
the same C entry point, and we want each C entry point to appear
once with all aliases that reach it as referenced in NeLisp."
  (let ((subrs (make-hash-table :test 'eq)))
    (mapatoms
     (lambda (s)
       (when (fboundp s)
         (let ((f (indirect-function s)))
           (when (subrp f)
             (puthash s (list :subr f
                              :arity (subr-arity f))
                      subrs))))))
    subrs))

(defun gap-audit--walk (form called line context-file)
  "Recursively walk FORM, pushing (SYM FILE LINE) into CALLED.
CALLED is a hash-table `(symbol → (list of (file . line)))'.
Collects symbols in function position plus first-argument
symbols of `function', `funcall', `apply', `mapcar', `mapc',
`mapconcat', `seq-map', `dolist' and `cl-loop' forms — the
common higher-order-function patterns that pass a function by
symbol without using the function call form directly."
  (cond
   ((atom form) nil)
   ((not (listp form)) nil)
   (t
    (let ((head (car form)))
      (when (symbolp head)
        (push (cons context-file line)
              (gethash head called))
        (puthash head (gethash head called) called)
        ;; HOF patterns: first argument is (quote SYM) or #'SYM.
        (when (memq head '(function funcall apply mapcar mapc
                                    mapconcat seq-map seq-do
                                    cl-remove-if cl-remove-if-not
                                    cl-find-if cl-find-if-not
                                    cl-some cl-every))
          (let ((arg (cadr form)))
            (cond
             ((and (consp arg) (eq (car arg) 'function)
                   (symbolp (cadr arg)))
              (let ((s (cadr arg)))
                (push (cons context-file line)
                      (gethash s called))
                (puthash s (gethash s called) called)))
             ((and (consp arg) (eq (car arg) 'quote)
                   (symbolp (cadr arg)))
              (let ((s (cadr arg)))
                (push (cons context-file line)
                      (gethash s called))
                (puthash s (gethash s called) called)))))))
      (dolist (sub (cdr form))
        (gap-audit--walk sub called line context-file))))))

(defun gap-audit--scan-file (file called)
  "Sexp-walk FILE, populating CALLED hash-table.
FILE is recorded relative to `gap-audit--nelisp-root' so audit
artifacts don't bake in absolute paths."
  (let ((rel (file-relative-name file gap-audit--nelisp-root)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((line (line-number-at-pos (point)))
                  (form (read (current-buffer))))
              (gap-audit--walk form called line rel)))
        (end-of-file nil)))))

(defun gap-audit--enumerate-scope-files ()
  "Return the list of .el files to audit (src + bench, not test)."
  (let ((src (directory-files
              (expand-file-name "src" gap-audit--nelisp-root)
              t "\\.el$"))
        (bench (directory-files
                (expand-file-name "bench" gap-audit--nelisp-root)
                t "\\.el$")))
    (append src bench)))

(defun gap-audit--run ()
  "Produce the intersection alist and write the artifact.
Output shape: `((SYMBOL :arity ARITY :ref-count N :sites ((FILE . LINE) ...)) ...)'
sorted by ref-count descending then symbol name."
  (let* ((subrs (gap-audit--collect-host-subrs))
         (called (make-hash-table :test 'eq))
         (files (gap-audit--enumerate-scope-files))
         (rows nil))
    (dolist (f files)
      (gap-audit--scan-file f called))
    (maphash
     (lambda (sym sites)
       (when-let* ((meta (gethash sym subrs)))
         (push (list sym
                     :arity (plist-get meta :arity)
                     :ref-count (length sites)
                     :sites (seq-take (nreverse sites) 3))
               rows)))
     called)
    (setq rows
          (sort rows
                (lambda (a b)
                  (let ((ca (plist-get (cdr a) :ref-count))
                        (cb (plist-get (cdr b) :ref-count)))
                    (cond
                     ((/= ca cb) (> ca cb))
                     (t (string< (symbol-name (car a))
                                 (symbol-name (car b)))))))))
    (let ((out (expand-file-name
                "docs/design/11-phase5-gap-audit.data.el"
                gap-audit--nelisp-root)))
      (with-temp-file out
        (insert ";;; 11-phase5-gap-audit.data.el --- raw audit output -*- lexical-binding: t; -*-\n")
        (insert ";;\n")
        (insert (format ";; Generated: %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S%z")))
        (insert (format ";; Host subr count: %d\n" (hash-table-count subrs)))
        (insert (format ";; NeLisp-referenced subrs: %d\n" (length rows)))
        (insert (format ";; Scope files: %d\n" (length files)))
        (insert ";;\n")
        (insert ";;; Code:\n\n")
        (insert "(defconst nelisp-gap-audit-data\n  '(\n")
        (dolist (row rows)
          (insert (format "    %S\n" row)))
        (insert "    ))\n\n")
        (insert "(provide '11-phase5-gap-audit.data)\n")
        (insert ";;; 11-phase5-gap-audit.data.el ends here\n"))
      (message "gap-audit: host-subr=%d referenced=%d scope-files=%d -> %s"
               (hash-table-count subrs) (length rows) (length files) out))))

(gap-audit--run)

;;; 11-audit-script.el ends here
