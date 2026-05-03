;;; emacs-faces-builtins-test.el --- ERT for emacs-faces  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 face API (Track F).  Under host Emacs the
;; unprefixed bridges (face-attribute, defface, etc.) are gated off
;; (= host's faces.el wins), so behavioural assertions exercise the
;; prefixed `emacs-faces-*' API directly against the substrate
;; registry.  Featurep / fboundp parity is checked separately.

;;; Code:

(require 'ert)
(let ((load-path (cons "/home/madblack-21/Notes/dev/nelisp/packages/nelisp-regex/src"
                       load-path)))
  (require 'emacs-faces-builtins))
(require 'cl-lib)

(defmacro emacs-faces-builtins-test--with-fresh-registry (&rest body)
  "Run BODY against a fresh face registry."
  (declare (indent 0) (debug (body)))
  `(let ((saved (let ((h (make-hash-table :test 'eq)))
                  (maphash (lambda (k v) (puthash k v h))
                           emacs-redisplay--face-registry)
                  h)))
     (clrhash emacs-redisplay--face-registry)
     (emacs-redisplay-face-cache-clear)
     (unwind-protect
         (progn ,@body)
       (clrhash emacs-redisplay--face-registry)
       (maphash (lambda (k v)
                  (puthash k v emacs-redisplay--face-registry))
                saved)
       (emacs-redisplay-face-cache-clear))))

;;;; A. Load cleanly + fboundp parity

(ert-deftest emacs-faces-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-faces-builtins))
  (should (featurep 'emacs-faces))
  (dolist (sym '(facep make-face face-attribute set-face-attribute
                 face-foreground face-background
                 set-face-foreground set-face-background
                 face-list))
    (should (fboundp sym))))

;;;; B. make-face + facep

(ert-deftest emacs-faces-builtins-test/make-face-and-facep ()
  (emacs-faces-builtins-test--with-fresh-registry
    (should-not (emacs-faces-facep 'no-such-face))
    (emacs-faces-make-face 'my-face)
    (should (eq 'my-face (emacs-faces-facep 'my-face)))))

(ert-deftest emacs-faces-builtins-test/make-face-rejects-non-symbol ()
  (should-error (emacs-faces-make-face "string-not-symbol")
                :type 'wrong-type-argument))

;;;; C. attribute roundtrip

(ert-deftest emacs-faces-builtins-test/attribute-default-unspecified ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-make-face 'a-face)
    (should (eq 'unspecified (emacs-faces-attribute 'a-face :foreground)))))

(ert-deftest emacs-faces-builtins-test/set-attribute-roundtrip ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-set-attribute 'a-face nil :foreground "red"
                               :weight 'bold)
    (should (equal "red" (emacs-faces-attribute 'a-face :foreground)))
    (should (eq 'bold  (emacs-faces-attribute 'a-face :weight)))
    ;; Unset attributes still unspecified.
    (should (eq 'unspecified
                (emacs-faces-attribute 'a-face :background)))))

(ert-deftest emacs-faces-builtins-test/set-attribute-rejects-odd-args ()
  (emacs-faces-builtins-test--with-fresh-registry
    (should-error
     (emacs-faces-set-attribute 'foo nil :foreground)
     :type 'emacs-faces-error)))

;;;; D. convenience accessors

(ert-deftest emacs-faces-builtins-test/foreground-background-accessors ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-set-foreground 'fg-test "blue")
    (emacs-faces-set-background 'bg-test "yellow")
    (should (equal "blue"   (emacs-faces-foreground 'fg-test)))
    (should (equal "yellow" (emacs-faces-background 'bg-test)))
    ;; Unspecified → nil
    (should (null (emacs-faces-foreground 'bg-test)))
    (should (null (emacs-faces-background 'fg-test)))))

;;;; E. face-list returns names

(ert-deftest emacs-faces-builtins-test/face-list-returns-names ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-make-face 'one)
    (emacs-faces-make-face 'two)
    (emacs-faces-make-face 'three)
    (let ((names (emacs-faces-list)))
      (should (= 3 (length names)))
      (should (memq 'one   names))
      (should (memq 'two   names))
      (should (memq 'three names)))))

;;;; F. defface macro — t entry

(ert-deftest emacs-faces-builtins-test/defface-t-entry ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-defface my-defface
      '((t :foreground "green" :weight bold))
      "Doc string")
    (should (eq 'my-defface (emacs-faces-facep 'my-defface)))
    (should (equal "green" (emacs-faces-attribute 'my-defface :foreground)))
    (should (eq 'bold      (emacs-faces-attribute 'my-defface :weight)))))

;;;; G. defface macro — default entry takes precedence over t

(ert-deftest emacs-faces-builtins-test/defface-default-precedence ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-defface dface
      '((default :weight bold)
        (t       :foreground "red"))
      "Doc")
    ;; default entry is honoured first.
    (should (eq 'bold (emacs-faces-attribute 'dface :weight)))
    ;; The t entry is shadowed by the default-as-only-applied semantic.
    ;; We honour `default' OR `t', whichever comes first.
    (should (eq 'unspecified
                (emacs-faces-attribute 'dface :foreground)))))

;;;; H. defface macro — fall back to first entry when no t / default

(ert-deftest emacs-faces-builtins-test/defface-first-entry-fallback ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-defface fface
      '((((class color)) :foreground "magenta"))
      "Doc")
    (should (equal "magenta"
                   (emacs-faces-attribute 'fface :foreground)))))

;;;; I. set-face-attribute invalidates realize cache

(ert-deftest emacs-faces-builtins-test/set-attribute-clears-cache ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-defface c-face '((t :foreground "red")) "doc")
    ;; Realize once to populate cache.
    (emacs-redisplay-realize-face 'c-face)
    (should (= 1 (hash-table-count emacs-redisplay--face-cache)))
    ;; Set attribute → cache cleared.
    (emacs-faces-set-attribute 'c-face nil :background "white")
    (should (= 0 (hash-table-count emacs-redisplay--face-cache)))))

;;;; J. Idempotent require

(ert-deftest emacs-faces-builtins-test/require-is-idempotent ()
  (let ((before-fp (symbol-function 'facep))
        (before-fa (symbol-function 'face-attribute)))
    (require 'emacs-faces-builtins)
    (should (eq before-fp (symbol-function 'facep)))
    (should (eq before-fa (symbol-function 'face-attribute)))))

(provide 'emacs-faces-builtins-test)

;;; emacs-faces-builtins-test.el ends here
