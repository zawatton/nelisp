;;; nelisp-boot-autoload-test.el --- Deferred fallback visibility -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-boot-image)
(require 'nelisp-boot-libraries)

(ert-deftest nelisp-boot-autoload/conditional-alias ()
  (let ((entries (nelisp-boot-image--autoloads
                  'fixture "(defun owner () t)
                            (when substrate (defalias 'fallback #'owner))
                            (unless (fboundp 'other) (defun other () t))
                            (defalias 'other-alias #'other)")))
    (should-not (nth 3 (assq 'owner entries)))
    (dolist (name '(fallback other other-alias))
      (should (nth 3 (assq name entries))))))

(ert-deftest nelisp-boot-autoload/process-registry ()
  (let ((entries (nelisp-boot-image--autoloads
                  'nelisp-process-buffer
                  (nelisp-boot-image-provider 'nelisp-process-buffer))))
    (dolist (name '(make-pipe-process process-mark set-process-buffer get-buffer-process
                   process-get process-put))
      (should (= 1 (cl-count name entries :key #'car)))
      (should (nth 3 (assq name entries))))))

(ert-deftest nelisp-boot-autoload/install-preserves-fallback-visibility ()
  (let ((missing (make-symbol "missing")) (existing (make-symbol "existing"))
        (ordinary (make-symbol "ordinary"))
        (load-function (symbol-function 'load))
        (nelisp-boot-library--base-load (symbol-function 'load))
        nelisp-boot-library-sources nelisp-boot-library-autoloads)
    (unwind-protect
        (progn
          (fset existing #'ignore)
          (nelisp-boot-library-install
           nil (list (list missing "fixture" nil t)
                     (list existing "fixture" nil t)
                     (list ordinary "fixture" nil nil)))
          (should-not (fboundp missing))
          (should (eq (symbol-function existing) #'ignore))
          (should (autoloadp (symbol-function ordinary))))
      (fset 'load load-function))))
