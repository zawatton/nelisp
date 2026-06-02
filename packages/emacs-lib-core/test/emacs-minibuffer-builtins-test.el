;;; emacs-minibuffer-builtins-test.el --- ERT for emacs-minibuffer-builtins  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 minibuffer / completion bridge.  Under host
;; Emacs the unprefixed names stay bound to host C builtins, while
;; standalone NeLisp overwrites bootstrap stubs.  Behavioural assertions
;; exercise the prefixed `emacs-minibuffer-*' API directly via its
;; `emacs-minibuffer-feed-input' deterministic-input helper.
;; Featurep / fboundp / boundp parity is checked separately.

;;; Code:

(require 'ert)
(require 'emacs-minibuffer-builtins)
(require 'cl-lib)

(defmacro emacs-minibuffer-builtins-test--with-fresh-world (&rest body)
  "Run BODY with a clean minibuffer + buffer/match registry."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil)
         (emacs-minibuffer-history nil)
         (emacs-minibuffer-default nil))
     (emacs-minibuffer-reset)
     (unwind-protect
         (progn ,@body)
       (emacs-minibuffer-reset))))

;;;; A. Load cleanly + fboundp / boundp parity

(ert-deftest emacs-minibuffer-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-minibuffer-builtins))
  (should (featurep 'emacs-minibuffer))
  (dolist (sym '(read-from-minibuffer read-string read-no-blanks-input
                 read-key read-buffer read-file-name read-directory-name
                 read-passwd read-number
                 y-or-n-p yes-or-no-p
                 completing-read
                 minibufferp active-minibuffer-window minibuffer-window
                 minibuffer-prompt minibuffer-contents
                 minibuffer-prompt-end minibuffer-prompt-width
                 exit-minibuffer abort-recursive-edit minibuffer-message))
    (should (fboundp sym)))
  (dolist (sym '(minibuffer-history command-history file-name-history
                 read-string-history buffer-name-history
                 regexp-history extended-command-history))
    (should (boundp sym))))

;;;; B. emacs-minibuffer-feed-input + read-string roundtrip

(ert-deftest emacs-minibuffer-builtins-test/feed-input-then-read-string ()
  (emacs-minibuffer-builtins-test--with-fresh-world
    (emacs-minibuffer-feed-input "hello")
    (should (equal "hello" (emacs-minibuffer-read-string "Greet: ")))))

;;;; C. read-from-minibuffer routes through reader

(ert-deftest emacs-minibuffer-builtins-test/read-from-minibuffer-via-prefixed ()
  (emacs-minibuffer-builtins-test--with-fresh-world
    (emacs-minibuffer-feed-input "answer")
    (should (equal "answer"
                   (emacs-minibuffer-read-from-minibuffer "Q: ")))))

;;;; D. completing-read with list collection

(ert-deftest emacs-minibuffer-builtins-test/completing-read-list-collection ()
  (emacs-minibuffer-builtins-test--with-fresh-world
    (emacs-minibuffer-feed-input "alpha")
    (let ((result (emacs-minibuffer-completing-read
                   "Pick: " '("alpha" "beta" "gamma") nil t)))
      (should (equal "alpha" result)))))

;;;; E. completing-read REQUIRE-MATCH rejects unknown

(ert-deftest emacs-minibuffer-builtins-test/completing-read-require-match-rejects-unknown ()
  (emacs-minibuffer-builtins-test--with-fresh-world
    (emacs-minibuffer-feed-input "zzz")
    ;; REQUIRE-MATCH = t.  Implementations vary on rejection: some
    ;; signal, some return nil, some empty string.  Accept any
    ;; non-"zzz" sentinel — what we DON'T want is silent acceptance of
    ;; the unknown input.
    (let ((result (ignore-errors
                    (emacs-minibuffer-completing-read
                     "Pick: " '("alpha" "beta") nil t))))
      (should-not (equal "zzz" result)))))

;;;; F. read-number parses integer input

(ert-deftest emacs-minibuffer-builtins-test/read-number-integer ()
  (emacs-minibuffer-builtins-test--with-fresh-world
    (emacs-minibuffer-feed-input "42")
    (should (= 42 (emacs-minibuffer-read-number "N: " 0)))))

;;;; G. y-or-n-p truthy / falsy

(ert-deftest emacs-minibuffer-builtins-test/y-or-n-p-truthy ()
  (emacs-minibuffer-builtins-test--with-fresh-world
    (cl-letf (((symbol-function 'emacs-minibuffer--y-or-n-default)
               (lambda (_prompt) t)))
      (should (emacs-minibuffer-y-or-n-p "Confirm? ")))))

(ert-deftest emacs-minibuffer-builtins-test/y-or-n-p-falsy ()
  (emacs-minibuffer-builtins-test--with-fresh-world
    (cl-letf (((symbol-function 'emacs-minibuffer--y-or-n-default)
               (lambda (_prompt) nil)))
      (should-not (emacs-minibuffer-y-or-n-p "Confirm? ")))))

;;;; H. yes-or-no-p — same plug-in path

(ert-deftest emacs-minibuffer-builtins-test/yes-or-no-p-feed-yes ()
  (emacs-minibuffer-builtins-test--with-fresh-world
    (emacs-minibuffer-feed-input "yes")
    (should (emacs-minibuffer-yes-or-no-p "Sure? "))))

;;;; I. read-no-blanks-input rejects whitespace

(ert-deftest emacs-minibuffer-builtins-test/read-no-blanks-input-signals-on-blanks ()
  (emacs-minibuffer-builtins-test--with-fresh-world
    (emacs-minibuffer-feed-input "no blanks")
    (should-error (emacs-minibuffer-read-no-blanks-input "X: ")
                  :type 'emacs-minibuffer-error))
  (emacs-minibuffer-builtins-test--with-fresh-world
    (emacs-minibuffer-feed-input "noblanks")
    (should (equal "noblanks"
                   (emacs-minibuffer-read-no-blanks-input "X: ")))))

;;;; J. minibuffer history accumulates (= via the `read-string-history' defvar)

(ert-deftest emacs-minibuffer-builtins-test/history-accumulates ()
  (emacs-minibuffer-builtins-test--with-fresh-world
    ;; HIST symbol must be a dynamic-binding-aware defvar — under
    ;; lexical-binding a `(let ((local-sym ...))` does NOT capture
    ;; `(set 'local-sym ...)' writes.  Use the bridge's
    ;; `read-string-history' defvar.
    (let ((read-string-history nil))
      (emacs-minibuffer-feed-input "first" "second")
      (emacs-minibuffer-read-string "1: " nil 'read-string-history)
      (emacs-minibuffer-read-string "2: " nil 'read-string-history)
      (should (= 2 (length read-string-history)))
      ;; Most recent first.
      (should (equal "second" (car read-string-history))))))

;;;; K. Idempotence

(ert-deftest emacs-minibuffer-builtins-test/require-is-idempotent ()
  (let ((before-read-string (symbol-function 'read-string))
        (before-y-or-n      (symbol-function 'y-or-n-p))
        (before-completing  (symbol-function 'completing-read)))
    (require 'emacs-minibuffer-builtins)
    (should (eq before-read-string (symbol-function 'read-string)))
    (should (eq before-y-or-n      (symbol-function 'y-or-n-p)))
    (should (eq before-completing  (symbol-function 'completing-read)))))

(ert-deftest emacs-minibuffer-builtins-test/bridge-overwrites-standalone-stubs-in-source ()
  (should (fboundp 'emacs-minibuffer-builtins--install-function-p))
  (should-not (emacs-minibuffer-builtins--install-function-p 'read-string))
  (let* ((file (locate-library "emacs-minibuffer-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym '(read-from-minibuffer read-string read-no-blanks-input
                     read-key read-buffer read-file-name read-directory-name
                     read-passwd read-number y-or-n-p yes-or-no-p
                     completing-read minibufferp active-minibuffer-window
                     minibuffer-window minibuffer-prompt minibuffer-contents
                     minibuffer-prompt-end minibuffer-prompt-width
                     exit-minibuffer abort-recursive-edit minibuffer-message))
        (goto-char (point-min))
        (should (search-forward
                 (format "(when (emacs-minibuffer-builtins--install-function-p '%s)"
                         sym)
                 nil t))))))

(provide 'emacs-minibuffer-builtins-test)

;;; emacs-minibuffer-builtins-test.el ends here
