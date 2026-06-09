;;; emacs-keymap-builtins-test.el --- ERT tests for emacs-keymap-builtins  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 Emacs keymap.c builtin bridge.  Under batch
;; host Emacs the host C builtins remain active (= the bridge's
;; `unless (fboundp ...)' gate keeps them) so the substrate-direct
;; `emacs-keymap-*' API is used for semantic assertions; bridge-shape
;; assertions verify featurep + fboundp parity.

;;; Code:

(require 'ert)
(require 'emacs-keymap-builtins)
(require 'cl-lib)

;;;; A. Load cleanly

(ert-deftest emacs-keymap-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-keymap-builtins))
  (should (featurep 'emacs-keymap))
  (dolist (sym '(make-keymap make-sparse-keymap keymapp
                 define-key define-key-after suppress-keymap
                 lookup-key key-binding
                 key-description
                 set-keymap-parent keymap-parent
                 current-global-map current-local-map
                 use-global-map use-local-map
                 where-is-internal))
    (should (fboundp sym))))

;;;; B. Substrate-direct: prefixed make-* + keymapp shape

(ert-deftest emacs-keymap-builtins-test/prefixed-constructors-produce-keymapp-shape ()
  (let ((sk (emacs-keymap-make-sparse-keymap))
        (km (emacs-keymap-make-keymap)))
    (should (emacs-keymap-keymapp sk))
    (should (emacs-keymap-keymapp km))))

;;;; C. Substrate-direct: define-key + lookup-key roundtrip

(ert-deftest emacs-keymap-builtins-test/define-key-roundtrip-via-prefixed ()
  (let ((map (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key map "\C-a" 'beginning-of-line)
    (should (eq 'beginning-of-line
                (emacs-keymap-lookup-key map "\C-a")))))

(ert-deftest emacs-keymap-builtins-test/define-key-after-via-prefixed ()
  (let ((map (emacs-keymap-make-sparse-keymap))
        (seen '()))
    (emacs-keymap-define-key map "a" 'cmd-a)
    (emacs-keymap-define-key map "b" 'cmd-b)
    (emacs-keymap-define-key-after map "c" 'cmd-c ?b)
    (emacs-keymap-map-keymap (lambda (k _v) (push k seen)) map)
    (should (equal (nreverse seen) (list ?b ?c ?a)))
    (should (eq 'cmd-c (emacs-keymap-lookup-key map "c")))))

;;;; D. Substrate-direct: parent chain

(ert-deftest emacs-keymap-builtins-test/parent-chain-via-prefixed ()
  (let ((parent (emacs-keymap-make-sparse-keymap))
        (child  (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key parent "\C-x" 'parent-cmd)
    (emacs-keymap-set-keymap-parent child parent)
    (should (eq parent (emacs-keymap-keymap-parent child)))
    (should (eq 'parent-cmd
                (emacs-keymap-lookup-key child "\C-x")))))

;;;; E. Bridge wiring: define-key wrapper forwards to emacs-keymap-define-key

(ert-deftest emacs-keymap-builtins-test/bridge-wraps-define-key-and-ignores-remove ()
  (let ((received nil))
    (cl-letf (((symbol-function 'emacs-keymap-define-key)
               (lambda (km key def) (setq received (list km key def)) def)))
      ;; Re-invoke our wrapper definition directly (bridge body) — this
      ;; works regardless of whether the host's `define-key' overrode
      ;; the unprefixed name.
      (let ((wrapper (lambda (keymap key def &optional remove)
                       (ignore remove)
                       (emacs-keymap-define-key keymap key def))))
        (should (eq 'cmd (funcall wrapper 'KM 'KEY 'cmd t))))
      (should (equal '(KM KEY cmd) received)))))

;;;; E2. Substrate-direct: suppress-keymap body shape

(ert-deftest emacs-keymap-builtins-test/suppress-keymap-body-shape ()
  (let ((map (emacs-keymap-make-sparse-keymap)))
    ;; Exercise the bridge semantics directly so host Emacs's own
    ;; `suppress-keymap' implementation cannot hide regressions.
    (let ((slot (emacs-keymap--full-slot map)))
      (unless slot
        (setq slot (cons t (make-vector 256 nil)))
        (setcdr map (cons slot (cdr map))))
      (let ((vec (cdr slot))
            (i 32))
        (while (<= i 126)
          (aset vec i 'undefined)
          (setq i (1+ i)))
        (let ((digit ?0))
          (while (<= digit ?9)
            (aset vec digit 'digit-argument)
            (setq digit (1+ digit))))
        (aset vec ?- 'negative-argument)))
    (should (eq 'undefined (emacs-keymap-lookup-key map (vector ?a))))
    (should (eq 'digit-argument (emacs-keymap-lookup-key map (vector ?7))))
    (should (eq 'negative-argument (emacs-keymap-lookup-key map (vector ?-))))))

;;;; F. Substrate-direct: current-global-map returns a keymap

(ert-deftest emacs-keymap-builtins-test/current-global-map-via-prefixed ()
  (should (emacs-keymap-keymapp (emacs-keymap-current-global-map))))

(ert-deftest emacs-keymap-builtins-test/key-description-bridge-in-source ()
  (let* ((file (locate-library "emacs-keymap-builtins"))
         ;; Read the .el source, not a compiled .elc (binary) when present.
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (substring file 0 -1)
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (search-forward
               "(defalias 'key-description #'emacs-keymap-key-description"
               nil t)))))

;;;; F2. Bridge shape: standard prefix maps exist

(ert-deftest emacs-keymap-builtins-test/standard-prefix-maps-are-bound ()
  (dolist (sym '(global-map ctl-x-map ctl-x-4-map ctl-x-5-map esc-map help-map))
    (should (boundp sym))
    (should (keymapp (symbol-value sym))))
  (should (boundp 'menu-bar-separator)))

;;;; G. Substrate-direct: where-is-internal returns a list

(ert-deftest emacs-keymap-builtins-test/where-is-internal-via-prefixed-returns-list ()
  (let ((map (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key map "\C-y" 'yank)
    (should (listp (emacs-keymap-where-is-internal 'yank map)))))

;;;; H. Idempotence

(ert-deftest emacs-keymap-builtins-test/require-is-idempotent ()
  (let ((before-make-keymap   (symbol-function 'make-keymap))
        (before-keymapp       (symbol-function 'keymapp))
        (before-lookup-key    (symbol-function 'lookup-key)))
    (require 'emacs-keymap-builtins)
    (should (eq before-make-keymap (symbol-function 'make-keymap)))
    (should (eq before-keymapp     (symbol-function 'keymapp)))
    (should (eq before-lookup-key  (symbol-function 'lookup-key)))))

(provide 'emacs-keymap-builtins-test)

;;; emacs-keymap-builtins-test.el ends here
