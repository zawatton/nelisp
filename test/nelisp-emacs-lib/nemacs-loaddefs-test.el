;;; nemacs-loaddefs-test.el --- Tests for autoload generation -*- lexical-binding: t; -*-

;; Doc 11 M7: loaddefs / autoload generation from ;;;###autoload cookies.

;;; Code:

(require 'ert)
(require 'nemacs-loaddefs)

(ert-deftest nemacs-loaddefs-generates-autoloads-for-cookies ()
  (let* ((dir (make-temp-file "nemacs-ld-" t))
         (file (expand-file-name "ldfeat.el" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; ldfeat.el\n"
                    ";;;###autoload\n"
                    "(defun ld-cmd (x)\n"
                    "  \"Doc for ld-cmd.\"\n"
                    "  (interactive \"p\")\n"
                    "  (* x 2))\n"
                    "(defun ld-private () nil)\n"
                    ";;;###autoload\n"
                    "(defun ld-fn2 () 'ok)\n"))
          (let ((forms (nemacs-loaddefs-generate-for-file file)))
            ;; only the two cookie'd defuns produce autoloads
            (should (= 2 (length forms)))
            (let ((f (car forms)))
              (should (eq (car f) 'autoload))
              (should (eq (cadr (nth 1 f)) 'ld-cmd))    ; (quote ld-cmd)
              (should (equal (nth 2 f) "ldfeat"))       ; file base
              (should (equal (nth 3 f) "Doc for ld-cmd."))
              (should (eq (nth 4 f) t)))                ; interactive
            (let ((f (cadr forms)))
              (should (eq (cadr (nth 1 f)) 'ld-fn2))
              (should-not (nth 3 f))                    ; no docstring
              (should-not (nth 4 f)))))                 ; not interactive
      (when (file-directory-p dir) (delete-directory dir t)))))

(ert-deftest nemacs-loaddefs-generated-autoload-is-loadable ()
  ;; the generated form is a valid autoload call: evaluating it installs a
  ;; lazy binding that loads the file on first use.
  (let* ((dir (make-temp-file "nemacs-ld2-" t))
         (file (expand-file-name "ldlive.el" dir)))
    (unwind-protect
        (let ((load-path (cons dir load-path)))
          (with-temp-file file
            (insert ";;;###autoload\n"
                    "(defun ld-live-fn (x) (+ x 1))\n"
                    "(provide 'ldlive)\n"))
          (let ((forms (nemacs-loaddefs-generate-for-file file)))
            (should (= 1 (length forms)))
            (fmakunbound 'ld-live-fn)
            (eval (car forms) t)            ; install the autoload
            (should (fboundp 'ld-live-fn))
            (should (= 4 (ld-live-fn 3)))))  ; first call loads + runs it
      (fmakunbound 'ld-live-fn)
      (when (file-directory-p dir) (delete-directory dir t)))))

(ert-deftest nemacs-loaddefs-generate-spans-multiple-files ()
  (let* ((dir (make-temp-file "nemacs-ld3-" t))
         (a (expand-file-name "lda.el" dir))
         (b (expand-file-name "ldb.el" dir)))
    (unwind-protect
        (progn
          (with-temp-file a (insert ";;;###autoload\n(defun lda-fn () 1)\n"))
          (with-temp-file b (insert ";;;###autoload\n(defun ldb-fn () 2)\n"))
          (let ((forms (nemacs-loaddefs-generate (list a b))))
            (should (= 2 (length forms)))
            (should (eq (cadr (nth 1 (car forms))) 'lda-fn))
            (should (eq (cadr (nth 1 (cadr forms))) 'ldb-fn))))
      (when (file-directory-p dir) (delete-directory dir t)))))

(ert-deftest nemacs-loaddefs-defers-loading-until-called ()
  "M7 deliverable: loaddefs autoloads keep startup bounded (a vendor feature
stays unloaded until first call) while remaining a callable workflow."
  (let* ((dir (make-temp-file "nemacs-ld-bound-" t))
         (file (expand-file-name "ldbound.el" dir)))
    (unwind-protect
        (let ((load-path (cons dir load-path))
              (features (copy-sequence features)))
          (with-temp-file file
            (insert ";;;###autoload\n"
                    "(defun ld-bound-fn () 'loaded)\n"
                    "(provide 'ldbound)\n"))
          (let ((forms (nemacs-loaddefs-generate-for-file file)))
            (fmakunbound 'ld-bound-fn)
            (setq features (remove 'ldbound features))
            (eval (car forms) t)
            ;; startup stays bounded: the feature is NOT loaded yet
            (should-not (featurep 'ldbound))
            ;; ...but it is a callable workflow: the first call loads it
            (should (eq 'loaded (ld-bound-fn)))
            (should (featurep 'ldbound))))
      (fmakunbound 'ld-bound-fn)
      (when (file-directory-p dir) (delete-directory dir t)))))

(provide 'nemacs-loaddefs-test)

;;; nemacs-loaddefs-test.el ends here
