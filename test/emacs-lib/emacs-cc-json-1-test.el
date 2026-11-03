;;; emacs-cc-json-1-test.el --- json-insert fallback checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)

(defconst emacs-cc-json-1-test--root
  (expand-file-name "../.."
                    (file-name-directory (or load-file-name buffer-file-name))))

(load (expand-file-name
       "packages/nelisp-emacs-foundation/src/emacs-cc-json-1.el"
       emacs-cc-json-1-test--root)
      nil t)

(defun emacs-cc-json-1-test--run (function object &rest args)
  (with-temp-buffer
    (insert "prefix")
    (let ((result (apply function object args)))
      (list result (buffer-string) (point)))))

(ert-deftest emacs-cc-json-1-test/compact-serialization-and-point ()
  (should (equal '(nil "prefix{}" 9)
                 (emacs-cc-json-1-test--run
                  #'emacs-cc-json-1-insert nil)))
  (should (equal '(nil "prefix[1,2]" 12)
                 (emacs-cc-json-1-test--run
                  #'emacs-cc-json-1-insert [1 2])))
  (should (equal '(nil "prefix{\"a\":1}" 14)
                 (emacs-cc-json-1-test--run
                  #'emacs-cc-json-1-insert '(:a 1))))
  (should (equal '(nil "prefix{\"a\":1}" 14)
                 (emacs-cc-json-1-test--run
                  #'emacs-cc-json-1-insert '((a . 1))))))

(ert-deftest emacs-cc-json-1-test/unicode-and-escaping ()
  (let* ((object ["line\n\"\\" "雪☃"])
         (expected
          (concat "prefix"
                  (if (fboundp 'emacs-json--to-encodable)
                      (json-encode
                       (emacs-json--to-encodable object :null :false))
                    (json-serialize object))))
         (result (emacs-cc-json-1-test--run
                  #'emacs-cc-json-1-insert object)))
    (should (eq nil (car result)))
    (should (equal (encode-coding-string expected 'utf-8 t)
                   (encode-coding-string (cadr result) 'utf-8 t)))
    (should (= (1+ (length expected)) (nth 2 result)))))

(ert-deftest emacs-cc-json-1-test/custom-null-and-false-objects ()
  (should (equal '(nil "prefixnull" 11)
                 (emacs-cc-json-1-test--run
                  #'emacs-cc-json-1-insert :custom :null-object :custom)))
  (should (equal '(nil "prefixfalse" 12)
                 (emacs-cc-json-1-test--run
                  #'emacs-cc-json-1-insert :custom :false-object :custom))))

(ert-deftest emacs-cc-json-1-test/custom-string-and-nil-sentinels ()
  (let* ((sentinel (copy-sequence "empty"))
         (object (list :empty sentinel :no nil)))
    (should (equal '(nil "prefix{\"empty\":null,\"no\":false}" 32)
                   (emacs-cc-json-1-test--run
                    #'emacs-cc-json-1-insert object
                    :null-object sentinel :false-object nil)))))

(ert-deftest emacs-cc-json-1-test/nil-sentinel-options-precede-empty-object-default ()
  (should (equal '(nil "prefixnull" 11)
                 (emacs-cc-json-1-test--run
                  #'emacs-cc-json-1-insert nil :null-object nil)))
  (should (equal '(nil "prefixfalse" 12)
                 (emacs-cc-json-1-test--run
                  #'emacs-cc-json-1-insert nil :false-object nil)))
  (should (equal '(nil "prefixnull" 11)
                 (emacs-cc-json-1-test--run
                  #'emacs-cc-json-1-insert nil
                  :null-object nil :false-object nil))))

(ert-deftest emacs-cc-json-1-test/bad-arguments-do-not-mutate-buffer ()
  (dolist (args '((:null-object) (:unknown-option t)))
    (with-temp-buffer
      (insert "keep")
      (goto-char 3)
      (should-error (apply #'emacs-cc-json-1-insert [1] args))
      (should (equal "keep" (buffer-string)))
      (should (= 3 (point))))))

(ert-deftest emacs-cc-json-1-test/read-only-buffer-is-unchanged ()
  (with-temp-buffer
    (insert "keep")
    (goto-char (point-max))
    (setq buffer-read-only t)
    (should-error (emacs-cc-json-1-insert [1 2]))
    (should (equal "keep" (buffer-string)))
    (should (= 5 (point)))))

(ert-deftest emacs-cc-json-1-test/inhibit-read-only-is-honored ()
  (with-temp-buffer
    (insert "keep")
    (goto-char (point-max))
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (should (null (emacs-cc-json-1-insert [1])))
      (should (equal "keep[1]" (buffer-string))))))

(ert-deftest emacs-cc-json-1-test/text-property-read-only-is-honored ()
  (with-temp-buffer
    (insert "xy")
    (put-text-property 1 3 'read-only t)
    (goto-char 2)
    (should-error (emacs-cc-json-1-insert [1]))
    (should (equal "xy" (buffer-string)))
    (should (= 2 (point)))))

(ert-deftest emacs-cc-json-1-test/serialization-error-precedes-read-only-error ()
  (with-temp-buffer
    (insert "keep")
    (goto-char (point-max))
    (setq buffer-read-only t)
    (should-error (emacs-cc-json-1-insert 'unsupported-object)
                  :type 'wrong-type-argument)
    (should (equal "keep" (buffer-string)))
    (should (= 5 (point)))))

(ert-deftest emacs-cc-json-1-test/host-public-binding-is-preserved ()
  (when (and (not (fboundp 'nelisp--buffer-multibyte-p))
             (fboundp 'json-insert))
    (should (not (eq (symbol-function 'json-insert)
                     #'emacs-cc-json-1-insert)))))

(provide 'emacs-cc-json-1-test)
;;; emacs-cc-json-1-test.el ends here
