;;; emacs-parity-core-vars-test.el --- ERT for input-method gap fills (T79)  -*- lexical-binding: t; -*-

;;; Commentary:

;; T79: `(evil-mode 1)' on the standalone signalled
;; `(void-variable deactivate-current-input-method-function)' --
;; `evil-core.el's `evil-local-mode' reads that variable directly in a
;; `when' guard while switching evil states, and none of GNU's
;; mule.el/mule-cmds.el/keyboard.c input-method surface was defined
;; anywhere under `src/'.
;;
;; Every definition added to `emacs-parity-core-vars.el' for this
;; ticket is guarded `unless (boundp ...)' / `unless (fboundp ...)', so
;; under host Emacs (where all of it is already real, C-backed or
;; mule-cmds.el-defined) `(require 'emacs-parity-core-vars)' is a
;; no-op and every assertion below exercises Emacs's own primitives;
;; under standalone NeLisp it exercises the port.  Either way the same
;; contract holds -- each value below was read from a clean
;; `emacs -Q --batch' (host Emacs 31.1) before writing the fill.
;;
;; One variable, `input-method-alist', is deliberately not asserted to
;; be `nil' here: `mule-cmds.el' declares its default as `nil', but a
;; real running Emacs has already registered every built-in Quail/LEIM
;; method into it by dump time (via `leim-list.el'), so a fresh host
;; batch process never actually observes the bare `nil' default --
;; only a from-scratch standalone runtime with no LEIM does.  The
;; assertions instead cover what genuinely holds in both places: it is
;; bound, alist-shaped (or empty), and marked `risky-local-variable'.

;;; Code:

(require 'ert)
(require 'emacs-parity-core-vars)

(ert-deftest emacs-parity-core-vars-test/input-method-vars-bound ()
  (dolist (sym '(current-input-method
                 current-input-method-title
                 default-input-method
                 input-method-alist
                 input-method-function
                 input-method-verbose-flag
                 input-method-highlight-flag
                 input-method-activate-hook
                 input-method-deactivate-hook
                 deactivate-current-input-method-function
                 describe-current-input-method-function))
    (should (boundp sym))))

(ert-deftest emacs-parity-core-vars-test/input-method-fns-bound ()
  (dolist (fn '(activate-input-method deactivate-input-method set-input-method))
    (should (fboundp fn))))

(ert-deftest emacs-parity-core-vars-test/input-method-scalar-defaults-match-gnu ()
  "Matches a clean `emacs -Q --batch' (host Emacs 31.1) exactly.
`input-method-function's real GNU default is the *function* `list', not
nil (`keyboard.c': `Vinput_method_function = Qlist;') -- applying it to
a just-read character returns that character unchanged as a
one-element list, which is GNU's built-in identity no-op for \"no input
method is translating this keystroke\"."
  (should (null current-input-method))
  (should (null current-input-method-title))
  (should (null default-input-method))
  (should (eq input-method-function 'list))
  (should (equal (funcall input-method-function ?a) (list ?a)))
  (should (eq input-method-verbose-flag 'default))
  (should (eq input-method-highlight-flag t))
  (should (null input-method-activate-hook))
  (should (null input-method-deactivate-hook))
  (should (null deactivate-current-input-method-function))
  (should (null describe-current-input-method-function)))

(ert-deftest emacs-parity-core-vars-test/input-method-alist-shape ()
  (should (listp input-method-alist))
  (should (eq (get 'input-method-alist 'risky-local-variable) t))
  (dolist (slot input-method-alist)
    (should (consp slot))
    (should (stringp (car slot)))))

(ert-deftest emacs-parity-core-vars-test/deactivate-when-inactive-is-noop ()
  "GNU: `(deactivate-input-method)' with no active method changes nothing."
  (should (null current-input-method))
  (should (null (deactivate-input-method)))
  (should (null current-input-method))
  (should (null current-input-method-title)))

(ert-deftest emacs-parity-core-vars-test/activate-unknown-method-signals ()
  "GNU signals `(error \"Can't activate input method `NAME'\")' for any
name absent from `input-method-alist' -- guaranteed true here since
this name is never a real registered method on host or standalone."
  (should (null current-input-method))
  (let ((err (should-error
              (activate-input-method "t79-parity-test-nonexistent-method")
              :type 'error)))
    ;; Match loosely: `format-message's quoting style renders the
    ;; apostrophe/quotes as curly Unicode on some host configurations
    ;; and as straight ASCII on others (and on the standalone); avoid
    ;; asserting on the quote characters themselves.
    (should (string-match-p "activate input method"
                             (downcase (error-message-string err)))))
  ;; The error happens before `current-input-method' is ever set.
  (should (null current-input-method)))

(ert-deftest emacs-parity-core-vars-test/activate-nil-is-noop ()
  (should (null current-input-method))
  (should (null (activate-input-method nil)))
  (should (null current-input-method)))

(ert-deftest emacs-parity-core-vars-test/set-input-method-nil-round-trip ()
  "`(set-input-method nil)' deactivates (a no-op when already inactive)
and returns the new `default-input-method' (nil), exactly like GNU."
  (should (null current-input-method))
  (should (null (set-input-method nil)))
  (should (null default-input-method))
  (should (null current-input-method)))

(provide 'emacs-parity-core-vars-test)

;;; emacs-parity-core-vars-test.el ends here
