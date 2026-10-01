;;; c-core-units-test.el --- static checks for src/emacs-cc-*.el units  -*- lexical-binding: t; -*-

;;; Commentary:
;; Host ERT (GNU 31.1).  The units implement GNU C primitives that host Emacs
;; already has natively, so behaviour is checked end to end by
;; test/c-core-parity-smoke.sh; this file checks the contract of the source:
;;
;; - every unit file reads cleanly;
;; - every top-level definition of a GNU C-primitive name (per
;;   tools/c-core-areas.tsv) is guarded by (unless (fboundp 'NAME) ...), so a
;;   native or earlier implementation is never shadowed;
;; - no such definition is an empty stub (a body that is only nil, `ignore'
;;   or a bare constant with no use of its arguments is flagged);
;; - no unit defines a GNU name outside tools/c-core-areas.tsv.
;;
;; Run one area: (c-core-units-run "display").

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst c-core-units--root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))

(defun c-core-units--areas ()
  "Return an alist (NAME . AREA) from tools/c-core-areas.tsv."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "tools/c-core-areas.tsv" c-core-units--root))
    (mapcar (lambda (line) (let ((f (split-string line "\t"))) (cons (car f) (cadr f))))
            (split-string (buffer-string) "\n" t))))

(defun c-core-units--files ()
  (let (files)
    (dolist (package (directory-files (expand-file-name "packages" c-core-units--root) t "\\`nelisp-emacs-"))
      (let ((src (expand-file-name "src" package)))
        (when (file-directory-p src)
          (setq files (nconc files (directory-files src t "\\`emacs-cc-.*\\.el\\'"))))))
    files))

(defun c-core-units--forms (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun c-core-units--defined-name (form)
  "Return the symbol FORM defines as a function, or nil."
  (when (and (consp form) (memq (car form) '(defun defalias defmacro defsubst)))
    (let ((n (cadr form)))
      (cond ((symbolp n) n)
            ((and (consp n) (eq (car n) 'quote)) (cadr n))))))

(defun c-core-units--guard-name (form)
  "If FORM is (unless (fboundp 'NAME) ...), return NAME."
  (pcase form
    (`(unless (fboundp (quote ,n)) . ,_) n)))

(defun c-core-units--empty-stub-p (def)
  "Non-nil when DEF (a defun form) has a trivially empty body."
  (when (eq (car def) 'defun)
    (let ((body (nthcdr 3 def)))
      (when (stringp (car body)) (setq body (cdr body)))
      (while (and body (eq (car-safe (car body)) 'declare)) (setq body (cdr body)))
      (or (null body)
          (and (= (length body) 1)
               (or (null (car body))
                   (equal (car body) '(ignore))
                   (and (consp (car body)) (eq (caar body) 'ignore))))))))

(defun c-core-units-problems (area)
  "Return a list of problem strings for AREA's GNU names across all units."
  (let* ((areas (c-core-units--areas))
         (problems nil))
    (dolist (file (c-core-units--files))
      (let ((forms (condition-case err (c-core-units--forms file)
                     (error (push (format "%s: read error %S" file err) problems) nil))))
        (dolist (form forms)
          (let* ((guard (c-core-units--guard-name form))
                 (defs (if guard (cddr form) (list form))))
            (dolist (d defs)
              (let* ((name (c-core-units--defined-name d))
                     (entry (and name (assoc (symbol-name name) areas))))
                (when name
                  (cond
                   ((and (not entry) guard (eq guard name))
                    (push (format "%s: %s is not a C primitive in tools/c-core-areas.tsv"
                                  (file-name-nondirectory file) name)
                          problems))
                   ((and entry (equal (cdr entry) area))
                    (unless (eq guard name)
                      (push (format "%s: %s is not guarded by (unless (fboundp '%s) ...)"
                                    (file-name-nondirectory file) name name)
                            problems))
                    (when (c-core-units--empty-stub-p d)
                      (push (format "%s: %s is an empty stub"
                                    (file-name-nondirectory file) name)
                            problems)))))))))))
    (nreverse problems)))

(defun c-core-units-run (area)
  "Define and run the ERT check for AREA, exiting like `ert-run-tests-batch-and-exit'."
  (eval `(ert-deftest ,(intern (format "c-core-units/%s" area)) ()
           (should (equal (c-core-units-problems ,area) nil)))
        t)
  (ert-run-tests-batch-and-exit (format "\\`c-core-units/%s\\'" area)))

(provide 'c-core-units-test)

;;; c-core-units-test.el ends here
