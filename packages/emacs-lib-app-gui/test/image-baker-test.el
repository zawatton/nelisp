;;; image-baker-test.el --- ERT for nemacs lisp-image baker  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the `.nli' image-baker / image-loader wrappers.

;;; Code:

(require 'ert)
(require 'image-baker)
(require 'image-loader)
(require 'nemacs-loadup)

(defmacro image-baker-test--with-tmpfile (var &rest body)
  "Bind VAR to a fresh `.nli' tmp-file; delete it after BODY."
  (declare (indent 1) (debug (symbol body)))
  `(let ((,var (make-temp-file "image-baker-test-" nil ".nli")))
     (unwind-protect (progn ,@body)
       (when (file-exists-p ,var)
         (delete-file ,var)))))

(ert-deftest image-baker-test/feature-loaded ()
  (should (featurep 'image-baker))
  (should (featurep 'image-loader))
  (dolist (sym '(image-baker-bake
                 image-baker-bake-batch
                 image-loader-load
                 image-loader-load-if-readable))
    (should (fboundp sym))))

(ert-deftest image-baker-test/bake-creates-nli-after-loadup ()
  (image-baker-test--with-tmpfile path
    (unwind-protect
        (progn
          (nemacs-uninit)
          (let ((image (image-baker-bake path)))
            (should (file-exists-p path))
            (should nemacs-initialized)
            (should (memq 'nemacs-loadup (plist-get image :features)))
            (should (assq 'nemacs-initialized (plist-get image :defvars)))))
      (nemacs-uninit))))

(ert-deftest image-baker-test/loader-restores-baked-defvars ()
  (image-baker-test--with-tmpfile path
    (unwind-protect
        (progn
          (nemacs-uninit)
          (image-baker-bake path)
          (nemacs-uninit)
          (should-not nemacs-initialized)
          (let ((image-loader-restore-buffers nil))
            (image-loader-load path))
          (should nemacs-initialized)
          (should (equal (expand-file-name path)
                         image-loader-last-loaded-file))
          (should (plist-get image-loader-last-image-info :feature-count)))
      (nemacs-uninit))))

(ert-deftest image-baker-test/loader-explicit-nil-skips-buffer-restore ()
  (image-baker-test--with-tmpfile path
    (let ((emacs-dump-extra-buffer-names '("image-loader-skip-buffer"))
          (nelisp-ec--buffers nil)
          (nelisp-ec--current-buffer nil))
      (let ((b (nelisp-ec-generate-new-buffer "image-loader-skip-buffer")))
        (let ((nelisp-ec--current-buffer b))
          (nelisp-ec-insert "should not return")))
      (emacs-dump-save path)
      (setq nelisp-ec--buffers nil)
      (image-loader-load path nil)
      (should-not (assoc "image-loader-skip-buffer" nelisp-ec--buffers)))))

(ert-deftest image-baker-test/load-if-readable-skips-missing-file ()
  (let ((missing (expand-file-name
                  "image-baker-test-missing.nli"
                  temporary-file-directory)))
    (when (file-exists-p missing)
      (delete-file missing))
    (should-not (image-loader-load-if-readable missing))))

(provide 'image-baker-test)

;;; image-baker-test.el ends here
