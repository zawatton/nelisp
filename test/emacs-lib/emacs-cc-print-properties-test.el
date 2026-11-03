;;; emacs-cc-print-properties-test.el --- property-aware printing -*- lexical-binding: t; -*-

(require 'ert)

(defconst emacs-cc-print-properties-test--root
  (expand-file-name "../.."
                    (file-name-directory (or load-file-name buffer-file-name))))

(dolist (dir '("packages/nelisp-emacs-foundation/src"
               "packages/nelisp-emacs-buffer-core/src"
               "packages/nelisp-emacs-text-core/src"))
  (add-to-list 'load-path
               (expand-file-name dir emacs-cc-print-properties-test--root)))

(load "nelisp-emacs-compat" nil t)
(load "emacs-buffer" nil t)
(load "emacs-buffer-builtins" nil t)
(load "emacs-string" nil t)
(load "emacs-cc-print-1" nil t)

(ert-deftest emacs-cc-print-properties-test/nested-and-escaped-strings ()
  "Readable printing retains property runs inside nested objects."
  (let* ((styled (propertize "a\n\\b" 'face 'bold))
         (expected "(plain #(\"a\n\\\\b\" 0 4 (face bold)))"))
    (should (equal expected
                   (prin1-to-string (list 'plain styled))))))

(ert-deftest emacs-cc-print-properties-test/plain-objects-keep-native-printing ()
  "Objects without sidecar properties keep the native printer output."
  (let ((object '(plain (1 "text") [symbol])))
    (should (equal "(plain (1 \"text\") [symbol])"
                   (prin1-to-string object)))))

(ert-deftest emacs-cc-print-properties-test/token-boundaries-and-repeated-leaves ()
  "Quoted symbol text must not consume neighboring string property runs."
  (let* ((symbol (intern "quoted\"same\""))
         (plain (copy-sequence "same"))
         (styled (propertize (copy-sequence "same") 'face 'bold))
         (literal (prin1-to-string plain))
         (property-form (format "#(%s 0 4 (face bold))" literal))
         (object (list symbol plain styled plain styled))
         (expected (format "(%s %s %s %s %s)"
                           (prin1-to-string symbol) literal property-form
                           literal property-form)))
    (should (equal expected (prin1-to-string object)))))

(ert-deftest emacs-cc-print-properties-test/prin1-stream-and-return-value ()
  "The public `prin1' keeps its return value and output stream behavior."
  (let* ((styled (propertize "xy" 'face 'bold))
         (object (list 'plain styled))
         (expected (prin1-to-string object))
         (characters nil))
    (with-temp-buffer
      (should (eq object (prin1 object (current-buffer))))
      (should (equal expected (buffer-string))))
    (should
     (eq object
         (prin1 object (lambda (character)
                         (setq characters (cons character characters))))))
    (should (equal expected (apply #'string (nreverse characters))))))

(ert-deftest emacs-cc-print-properties-test/noescape-and-overrides-forward ()
  "Optional printer arguments retain their native semantics."
  (let* ((styled (propertize "a\n\\b" 'face 'bold))
         (object (list 'plain styled)))
    (should (equal "a\n\\b"
                   (prin1-to-string styled t)))
    (should (equal "(plain ...)"
                   (prin1-to-string object nil '((length . 1)))))
    (should (equal "..."
                   (prin1-to-string object nil '((level . 0)))))
    (should (equal (prin1-to-string styled)
                   (prin1-to-string styled nil nil)))))

(provide 'emacs-cc-print-properties-test)
;;; emacs-cc-print-properties-test.el ends here
