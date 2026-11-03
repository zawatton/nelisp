;;; emacs-capf-test.el --- Tests for minimal completion-at-point -*- lexical-binding: t; -*-

;; Doc 11 M6: in-buffer CAPF baseline.

;;; Code:

(require 'ert)
(require 'emacs-capf)

(defmacro emacs-capf-test--with-buffer (text &rest body)
  "Create a fresh buffer containing TEXT and run BODY with it current."
  (declare (indent 1) (debug (form body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (completion-at-point-functions nil))
     (let ((buf (nelisp-ec-generate-new-buffer "t-capf")))
       (nelisp-ec-set-buffer buf)
       (nelisp-ec-insert ,text)
       ,@body)))

(defun emacs-capf-test--whole-buffer-source (collection)
  "Return a CAPF function completing the whole buffer against COLLECTION."
  (lambda ()
    (list 1 (1+ (length (nelisp-ec-buffer-string))) collection)))

(ert-deftest emacs-capf-single-match-completes-fully ()
  (emacs-capf-test--with-buffer "foo"
    (setq completion-at-point-functions
          (list (emacs-capf-test--whole-buffer-source '("foobar"))))
    (should (eq t (completion-at-point)))
    (should (string= "foobar" (nelisp-ec-buffer-string)))))

(ert-deftest emacs-capf-multiple-matches-extends-common-prefix ()
  (emacs-capf-test--with-buffer "foo"
    (setq completion-at-point-functions
          (list (emacs-capf-test--whole-buffer-source '("foobar" "foobaz"))))
    (let ((r (completion-at-point)))
      (should (equal '("foobar" "foobaz") r))
      ;; extended to the longest common prefix, no further
      (should (string= "fooba" (nelisp-ec-buffer-string))))))

(ert-deftest emacs-capf-no-match-returns-nil ()
  (emacs-capf-test--with-buffer "xyz"
    (setq completion-at-point-functions
          (list (emacs-capf-test--whole-buffer-source '("foobar"))))
    (should-not (completion-at-point))
    (should (string= "xyz" (nelisp-ec-buffer-string)))))

(ert-deftest emacs-capf-no-applicable-hook-returns-nil ()
  (emacs-capf-test--with-buffer "foo"
    (setq completion-at-point-functions (list (lambda () nil)))
    (should-not (completion-at-point))
    (should (string= "foo" (nelisp-ec-buffer-string)))))

(ert-deftest emacs-capf-runs-first-applicable-source ()
  (emacs-capf-test--with-buffer "foo"
    (setq completion-at-point-functions
          (list (lambda () nil)               ; does not apply
                (emacs-capf-test--whole-buffer-source '("foobar"))))
    (should (eq t (completion-at-point)))
    (should (string= "foobar" (nelisp-ec-buffer-string)))))

(ert-deftest emacs-capf-elisp-symbol-names-prefix-filter ()
  (defun emacs-capf-test--zzqa () nil)
  (defun emacs-capf-test--zzqb () nil)
  (unwind-protect
      (let ((names (emacs-capf-elisp-symbol-names "emacs-capf-test--zzq")))
        (should (member "emacs-capf-test--zzqa" names))
        (should (member "emacs-capf-test--zzqb" names))
        ;; unrelated symbols are excluded
        (should-not (member "car" names)))
    (fmakunbound 'emacs-capf-test--zzqa)
    (fmakunbound 'emacs-capf-test--zzqb)))

(ert-deftest emacs-capf-elisp-completion-at-point-region ()
  (emacs-capf-test--with-buffer "(emacs-capf-test--zzq"
    (defun emacs-capf-test--zzqa () nil)
    (unwind-protect
        (let ((res (emacs-capf-elisp-completion-at-point)))
          (should res)
          ;; END is point; START skips the leading "("
          (should (= (nth 1 res) (nelisp-ec-point)))
          (should (member "emacs-capf-test--zzqa" (nth 2 res))))
      (fmakunbound 'emacs-capf-test--zzqa))))

(ert-deftest emacs-capf-elisp-completion-end-to-end ()
  (emacs-capf-test--with-buffer "(emacs-capf-test--uniqxy"
    (defun emacs-capf-test--uniqxyz () nil)
    (unwind-protect
        (progn
          (setq completion-at-point-functions
                (list #'emacs-capf-elisp-completion-at-point))
          ;; only one symbol matches the prefix -> completed fully
          (should (eq t (completion-at-point)))
          (should (string-match-p "emacs-capf-test--uniqxyz"
                                  (nelisp-ec-buffer-string))))
      (fmakunbound 'emacs-capf-test--uniqxyz))))

(provide 'emacs-capf-test)

;;; emacs-capf-test.el ends here
