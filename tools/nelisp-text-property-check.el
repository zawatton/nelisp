;;; nelisp-text-property-check.el --- Prepare a no-build property probe -*- lexical-binding: t; -*-

;; Run with host Emacs from the repository root:
;; TP_BEFORE=target/brief11/prelude-before.el emacs -Q --batch
;;   -l tools/nelisp-text-property-check.el
;; Then run target/brief11/check.el with the existing dynamic reader --load.
;; Only named definitions are extracted; the entire prelude is never loaded.

(require 'cl-lib)
(defvar nelisp-tp-check--root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))

(defun nelisp-tp-check--forms (file names)
  "Read only top-level definitions in FILE whose names occur in NAMES."
  (let (found)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^(\\(?:defun\\|ert-deftest\\) " nil t)
        (goto-char (match-beginning 0))
        (let ((form (read (current-buffer))))
          (when (memq (cadr form) names) (push form found)))))
    (unless (= (length found) (length names))
      (error "Missing definitions in %s: %S" file names))
    (nreverse found)))

(defun nelisp-tp-check--assertions (form)
  "Translate ERT assertions to standalone errors, preserving expressions."
  (cond ((atom form) form)
        ((eq (car form) 'quote) form)
        ((eq (car form) 'should)
         `(unless ,(cadr form) (error "Assertion failed: %S" ',(cadr form))))
        ((eq (car form) 'should-not)
         `(when ,(cadr form) (error "Assertion failed: %S" ',(cadr form))))
        (t (mapcar #'nelisp-tp-check--assertions form))))

(let* ((dir (expand-file-name "target/brief11" nelisp-tp-check--root))
       (source (expand-file-name "scripts/nelisp-stdlib-prelude.el" nelisp-tp-check--root))
       (names '(nelisp--w202-property-change nelisp--tp-rebuild))
       (tests '(nelisp-text-property-linear/string-directions
                nelisp-text-property-linear/limits-and-edges
                nelisp-text-property-linear/buffer-and-nil-object
                nelisp-text-property-linear/empty-and-identity
                nelisp-text-property-linear/markers-and-narrowing)))
  (make-directory dir t)
  (dolist (entry (list (cons "old.el" (or (getenv "TP_BEFORE") source))
                       (cons "new.el" source)))
    (with-temp-file (expand-file-name (car entry) dir)
      (insert ";;; -*- lexical-binding: t; -*-\n")
      (dolist (form (nelisp-tp-check--forms (cdr entry) names))
        (prin1 form (current-buffer)) (insert "\n"))))
  (with-temp-file (expand-file-name "assertions.el" dir)
    (insert ";;; -*- lexical-binding: t; -*-\n")
    (dolist (form (nelisp-tp-check--forms
                  (expand-file-name "test/nelisp-marker-test.el" nelisp-tp-check--root)
                  tests))
      (prin1 `(defun ,(cadr form) ()
                ,@(mapcar #'nelisp-tp-check--assertions (cdddr form))) (current-buffer))
      (insert "\n")))
  (with-temp-file (expand-file-name "check.el" dir)
    (insert ";;; -*- lexical-binding: t; -*-\n")
    (dolist (form `((load "target/brief11/old.el")
                   (setq nelisp-tp-check--old-change (symbol-function 'nelisp--w202-property-change)
                         nelisp-tp-check--old-rebuild (symbol-function 'nelisp--tp-rebuild))
                   (load "target/brief11/new.el")
                   (load "target/brief11/assertions.el")
                   ,@(mapcar (lambda (name) (list name)) tests)
                   (princ "ERT-ASSERTIONS PASS 5\n")
                   (load "tools/nelisp-text-property-runtime-check.el")))
      (prin1 form (current-buffer)) (insert "\n"))))

;;; nelisp-text-property-check.el ends here
