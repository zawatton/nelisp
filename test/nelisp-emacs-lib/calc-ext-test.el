;;; calc-ext-test.el --- ERT for the Calc extension facade  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(defconst calc-ext-test--src-dir
  (expand-file-name
   "../src"
   (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest calc-ext-test/facade-loads-with-minimal-calc ()
  "Requiring `calc-ext' must keep the minimal Calc contract loadable.

The vendor extension assumes variables from the full GNU Calc package.  This
test follows the normal source load path and proves that the companion facade
is selected instead, while its supported scalar bindings remain usable."
  (let ((load-path (cons calc-ext-test--src-dir load-path)))
    (should
     (string=
      (file-name-sans-extension
       (file-truename (expand-file-name "calc-ext.el" calc-ext-test--src-dir)))
      (file-name-sans-extension (file-truename (locate-library "calc-ext")))))
    (require 'calc)
    (require 'calc-ext)
    (should (featurep 'calc))
    (should (featurep 'calc-ext))
    (should (boundp 'calc-mode-map))
    (should (keymapp calc-mode-map))
    (should (fboundp 'calc-init-extensions))
    (should (fboundp 'calc-shift-prefix))
    (should (fboundp 'calc-init-prefixes))
    (should-not (calc-init-extensions))
    (should (eq 'calc-plus (lookup-key calc-mode-map "+")))
    (should (eq 'calc-divide (lookup-key calc-mode-map "/")))
    (should (eq 'calc-shift-prefix (lookup-key calc-mode-map "mS")))
    (should-error (calc-shift-prefix) :type 'emacs-tier3-facade-unsupported)))

(ert-deftest calc-ext-test/scalar-integer-compatibility-slice ()
  "Keep the scalar helpers used by UUID generators functional."
  (let ((load-path (cons calc-ext-test--src-dir load-path)))
    (require 'calc-ext)
    (should (= #x1b21dd213814000 (math-read-radix "1b21dd213814000" 16)))
    (should-not (math-read-radix "12z" 16))
    (should (= 42 (math-add 40 2)))
    (should (= 84 (math-mul 42 2)))
    (should (= 1024 (math-power-of-2 10)))
    (should (= #xff (math-clip #x1ff 8)))
    (should (equal '(12 . 3) (math-idivmod 123 10)))
    (should (= 42 (math-fixnum 42)))
    (let ((random-value (calcFunc-random 100)))
      (should (integerp random-value))
      (should (>= random-value 0))
      (should (< random-value 100)))))

(provide 'calc-ext-test)

;;; calc-ext-test.el ends here
