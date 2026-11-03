;;; emacs-cc-xfaces-1-test.el --- focused xfaces fallback checks -*- lexical-binding: t; -*-

(require 'ert)

(defconst emacs-cc-xfaces-1-test--source
  (expand-file-name
   "../../packages/nelisp-emacs-foundation/src/emacs-cc-xfaces-1.el"
   (file-name-directory (or load-file-name buffer-file-name))))

(defconst emacs-cc-xfaces-1-test--functions
  '(clear-face-cache color-distance color-gray-p color-supported-p
    color-values-from-color-spec face-attributes-as-vector
    frame--face-hash-table internal-set-alternative-font-family-alist
    internal-set-alternative-font-registry-alist
    internal-set-font-selection-order
    internal-set-lisp-face-attribute-from-resource
    tty-suppress-bold-inverse-default-colors
    internal-lisp-face-p internal-make-lisp-face internal-copy-lisp-face
    internal-get-lisp-face-attribute internal-set-lisp-face-attribute
    internal-lisp-face-empty-p internal-lisp-face-equal-p))

(ert-deftest emacs-cc-xfaces-1-test/preserves-existing-gnu-bindings ()
  "Loading the compatibility file must leave existing GNU bindings intact."
  (let ((before (mapcar (lambda (symbol)
                          (and (fboundp symbol)
                               (cons symbol (symbol-function symbol))))
                        emacs-cc-xfaces-1-test--functions)))
    (load emacs-cc-xfaces-1-test--source nil t)
    (dolist (entry before)
      (when entry
        (should (eq (cdr entry) (symbol-function (car entry))))))))

(ert-deftest emacs-cc-xfaces-1-test/tty-suppression-fallback-stores-and-returns-value ()
  "The fallback records SUPPRESS and returns its value without replacing GNU state."
  (let* ((function 'tty-suppress-bold-inverse-default-colors)
         (state 'emacs-cc-xfaces-1--suppress-bold-inverse-default-colors)
         (had-function (fboundp function))
         (old-function (and had-function (symbol-function function)))
         (had-state (boundp state))
         (old-state (and had-state (symbol-value state))))
    (unwind-protect
        (progn
          (when had-function (fmakunbound function))
          (when had-state (makunbound state))
          (load emacs-cc-xfaces-1-test--source nil t)
          (should (fboundp function))
          (should (eq t (funcall function t)))
          (should (eq t (symbol-value state)))
          (should (null (funcall function nil)))
          (should (null (symbol-value state))))
      (if had-function
          (fset function old-function)
        (when (fboundp function) (fmakunbound function)))
      (if had-state
          (set state old-state)
        (when (boundp state) (makunbound state))))))

(ert-deftest emacs-cc-xfaces-1-test/color-distance-fallback-matches-gnu-metric ()
  "The fallback uses GNU's fixed-point Riemersma color-distance formula."
  (let* ((function 'color-distance)
         (had-function (fboundp function))
         (old-function (and had-function (symbol-function function))))
    (unwind-protect
        (progn
          (when had-function (fmakunbound function))
          (load emacs-cc-xfaces-1-test--source nil t)
          (should (= 0 (color-distance "red" "red")))
          (should (= 327669 (color-distance "red" "blue")))
          (should (= 0 (color-distance '(1 2 3) '(1 2 3))))
          (should (equal '(65535 0 0)
                         (color-distance "red" "blue" nil
                                         (lambda (a _b) a))))
          (should-error (color-distance nil "blue")))
      (if had-function
          (fset function old-function)
        (when (fboundp function) (fmakunbound function))))))

(ert-deftest emacs-cc-xfaces-1-test/frame-and-default-attributes-are-independent ()
  "FRAME nil and an explicit frame share a store; FRAME t is independent."
  (let ((face 'emacs-cc-xfaces-1-test-isolation))
    (internal-make-lisp-face face)
    (internal-make-lisp-face face (selected-frame))
    (internal-set-lisp-face-attribute face :weight 'bold nil)
    (should (eq 'bold (face-attribute face :weight (selected-frame))))
    (should (eq 'unspecified (face-attribute face :weight t)))
    (should (internal-lisp-face-empty-p face t))
    (should-not (internal-lisp-face-empty-p face))
    (internal-set-lisp-face-attribute face :weight 'light t)
    (internal-set-lisp-face-attribute face :weight 'normal (selected-frame))
    (should (eq 'normal (face-attribute face :weight)))
    (should (eq 'light (face-attribute face :weight t)))
    (internal-set-lisp-face-attribute face :weight 'semibold 0)
    (should (eq 'semibold (face-attribute face :weight)))
    (should (eq 'semibold (face-attribute face :weight t)))))

(ert-deftest emacs-cc-xfaces-1-test/reset-and-copy-respect-store-boundaries ()
  "Resetting and copying defaults must not overwrite the selected face."
  (let ((face 'emacs-cc-xfaces-1-test-copy))
    (internal-make-lisp-face face)
    (internal-make-lisp-face face (selected-frame))
    (internal-set-lisp-face-attribute face :weight 'bold nil)
    (internal-set-lisp-face-attribute face :weight 'light t)
    ;; GNU resets a same-face copy because source and destination alias.
    (internal-copy-lisp-face face face t nil)
    (should (eq 'unspecified (face-attribute face :weight t)))
    (should (eq 'bold (face-attribute face :weight)))
    (internal-set-lisp-face-attribute face :weight 'light t)
    ;; FRAME t ignores NEW-FRAME, so this is another global reset.
    (internal-copy-lisp-face face face t (selected-frame))
    (should (eq 'bold (face-attribute face :weight)))
    (should (eq 'unspecified (face-attribute face :weight t)))
    (internal-set-lisp-face-attribute face :weight 'light t)
    (internal-make-lisp-face face (selected-frame))
    (should (internal-lisp-face-empty-p face))
    (should-not (internal-lisp-face-empty-p face t))))

(ert-deftest emacs-cc-xfaces-1-test/inheritance-and-global-merge-use-correct-store ()
  "Inheritance follows the queried store; merging copies specified defaults."
  (let ((parent 'emacs-cc-xfaces-1-test-parent)
        (child 'emacs-cc-xfaces-1-test-child))
    (dolist (face (list parent child))
      (internal-make-lisp-face face)
      (internal-make-lisp-face face (selected-frame)))
    (internal-set-lisp-face-attribute parent :weight 'bold nil)
    (internal-set-lisp-face-attribute parent :weight 'light t)
    (internal-set-lisp-face-attribute child :inherit parent nil)
    (internal-set-lisp-face-attribute child :inherit parent t)
    (should (eq 'bold (face-attribute child :weight nil t)))
    (should (eq 'light (face-attribute child :weight t t)))
    (internal-set-lisp-face-attribute parent :slant 'italic nil)
    (internal-merge-in-global-face parent (selected-frame))
    (should (eq 'light (face-attribute parent :weight)))
    (should (eq 'italic (face-attribute parent :slant)))
    (should (eq 'unspecified (face-attribute parent :slant t)))))

(provide 'emacs-cc-xfaces-1-test)
;;; emacs-cc-xfaces-1-test.el ends here
