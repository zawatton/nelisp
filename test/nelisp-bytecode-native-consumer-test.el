;;; nelisp-bytecode-native-consumer-test.el --- Reader extraction controls -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-bytecode-native-consumer)
(defvar nelisp-consumer-side-effect nil)
(defconst nelisp-consumer-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))
(defun nelisp-consumer-test--fixture ()
  (expand-file-name "fixtures/nelisp-consumer-fixture.elc" nelisp-consumer-test--directory))
(defun nelisp-consumer-test--fresh-no-producer-p (module)
  "Load MODULE in a fresh GNU process, independent of suite load order."
  (with-temp-buffer
    (let ((status (call-process
           (expand-file-name invocation-name invocation-directory) nil t nil
           "-Q" "--batch" "--eval"
           (prin1-to-string (list 'setq 'load-path (list 'quote load-path)
                                 'load-prefer-newer t))
           "--load" module "--eval"
           "(kill-emacs (if (or (featurep 'nelisp-bytecode-native-compiler) (featurep 'nelisp-aot-compiler)) 23 0))")))
      (message "consumer-fresh-process exit=%S" status)
      (eq status 0))))
(ert-deftest nelisp-consumer-reader-no-producer ()
  (should (nelisp-consumer-test--fresh-no-producer-p
           (locate-library "nelisp-bytecode-native-consumer"))))
(ert-deftest nelisp-consumer-reader-original-parity ()
  (let* ((package-file (locate-library "nelisp-bytecode-native-package"))
         (root (expand-file-name ".." (file-name-directory package-file)))
         (load-path (append (mapcar (lambda (directory) (expand-file-name directory root))
                                   '("src" "scripts" "packages/nl-prelude/src"))
                            load-path))
         (fixture (or (getenv "NELISP_CONSUMER_ELC") (nelisp-consumer-test--fixture)))
         (consumer (nelisp-bytecode-native-consumer-read-elc-functions fixture)))
    (should consumer)
    (require 'nelisp-bytecode-native-package)
    (should (equal consumer (nelisp-bytecode-native-package-read-elc-functions fixture)))
    (dolist (definition consumer)
      (should (equal (cdr definition)
                     (nelisp-bytecode-native-consumer-read-elc-function fixture (car definition)))))))
(ert-deftest nelisp-consumer-reader-fixture-identity ()
  (let* ((definitions (nelisp-bytecode-native-consumer-read-elc-functions
                       (nelisp-consumer-test--fixture)))
         (actual (mapcar (lambda (entry)
                           (let ((function (cdr entry)))
                             (list (car entry) (aref function 0)
                                   (string-to-list (aref function 1))
                                   (aref function 2) (aref function 3))))
                         definitions)))
    ;; Captured from pinned GNU 31.1 compilation, independently of forwarding.
    (should (equal actual
                   '((nelisp-consumer-fixture-add 514 (1 1 92 135) [] 4)
                     (nelisp-consumer-fixture-constant 0 (192 135) [consumer-marker] 1))))))
(ert-deftest nelisp-consumer-reader-refuses-malformed ()
  (dolist (data '("(setq consumer-probe t)" ";ELC\n#@999 \n" ";ELC\n(defalias 'broken"))
    (let ((file (make-temp-file "nelisp-consumer-")))
      (unwind-protect
          (progn
            (write-region data nil file nil 'silent)
            (should-error (nelisp-bytecode-native-consumer-read-elc-functions file)))
        (delete-file file)))))
(ert-deftest nelisp-consumer-reader-does-not-evaluate ()
  (let ((file (make-temp-file "nelisp-consumer-"))
        (nelisp-consumer-side-effect nil))
    (unwind-protect
        (progn
          (write-region ";ELC\n(setq nelisp-consumer-side-effect t)\n" nil file nil 'silent)
          (should-not (nelisp-bytecode-native-consumer-read-elc-functions file))
          (should-not nelisp-consumer-side-effect))
      (delete-file file))))
(provide 'nelisp-bytecode-native-consumer-test)
