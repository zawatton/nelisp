;;; emacs-frame-builtins-test.el --- ERT tests for emacs-frame-builtins  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 Emacs frame.c builtin bridge.  Under batch
;; host Emacs the host C builtins remain active (= the bridge's host
;; gate keeps them) so the substrate-direct
;; `emacs-frame-*' API is used for semantic assertions; bridge-shape
;; assertions verify featurep + fboundp parity.

;;; Code:

(require 'ert)
(require 'emacs-frame-builtins)
(require 'cl-lib)

(defmacro emacs-frame-builtins-test--with-fresh-world (&rest body)
  "Run BODY against a clean prefixed-frame registry."
  (declare (indent 0) (debug (body)))
  `(progn
     (emacs-frame-reset)
     (unwind-protect
         (progn ,@body)
       (emacs-frame-reset))))

;;;; A. Load cleanly

(ert-deftest emacs-frame-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-frame-builtins))
  (should (featurep 'emacs-frame))
  (dolist (sym '(make-frame framep frame-live-p frame-list
                 selected-frame window-frame
                 delete-frame delete-other-frames
                 frame-width frame-height frame-char-width frame-char-height
                 frame-pixel-width frame-pixel-height
                 set-frame-size set-frame-position
                 frame-parameter frame-parameters
                 set-frame-parameter modify-frame-parameters
                 frame-visible-p make-frame-visible make-frame-invisible
                 raise-frame lower-frame select-frame frame-focus
                 frame-windows display-pixel-width display-pixel-height))
    (should (fboundp sym))))

;;;; B. Substrate-direct: make-frame produces a framep object

(ert-deftest emacs-frame-builtins-test/prefixed-make-frame-produces-framep ()
  (emacs-frame-builtins-test--with-fresh-world
    (let ((f (emacs-frame-make-frame)))
      (should (emacs-frame-framep f))
      (should (emacs-frame-frame-live-p f)))))

;;;; C. Substrate-direct: frame-list contains the made frame

(ert-deftest emacs-frame-builtins-test/prefixed-frame-list-contains-made-frame ()
  (emacs-frame-builtins-test--with-fresh-world
    (let ((f (emacs-frame-make-frame)))
      (should (memq f (emacs-frame-frame-list))))))

;;;; D. Substrate-direct: parameter set/get roundtrip

(ert-deftest emacs-frame-builtins-test/parameter-roundtrip-via-prefixed ()
  (emacs-frame-builtins-test--with-fresh-world
    (let ((f (emacs-frame-make-frame)))
      (emacs-frame-set-frame-parameter f 'background-color "black")
      (should (equal "black"
                     (emacs-frame-frame-parameter f 'background-color))))))

;;;; E. Substrate-direct: modify-frame-parameters bulk update

(ert-deftest emacs-frame-builtins-test/modify-frame-parameters-bulk-via-prefixed ()
  (emacs-frame-builtins-test--with-fresh-world
    (let ((f (emacs-frame-make-frame)))
      (emacs-frame-modify-frame-parameters f '((cursor-type . box)
                                               (foo . bar)))
      (should (eq 'box
                  (emacs-frame-frame-parameter f 'cursor-type)))
      (should (eq 'bar
                  (emacs-frame-frame-parameter f 'foo))))))

;;;; F. Substrate-direct: delete-frame removes from list

(ert-deftest emacs-frame-builtins-test/delete-frame-via-prefixed-removes-from-list ()
  (emacs-frame-builtins-test--with-fresh-world
    (let ((f1 (emacs-frame-make-frame))
          (f2 (emacs-frame-make-frame)))
      (should (memq f1 (emacs-frame-frame-list)))
      (should (memq f2 (emacs-frame-frame-list)))
      (emacs-frame-delete-frame f2)
      (should-not (memq f2 (emacs-frame-frame-list)))
      (should (memq f1 (emacs-frame-frame-list))))))

;;;; G. Substrate-direct: selected-frame is framep

(ert-deftest emacs-frame-builtins-test/prefixed-selected-frame-is-framep ()
  (emacs-frame-builtins-test--with-fresh-world
    (should (emacs-frame-framep (emacs-frame-selected-frame)))))

(ert-deftest emacs-frame-builtins-test/extended-prefixed-surface-is-usable ()
  (emacs-frame-builtins-test--with-fresh-world
    (let ((f (emacs-frame-make-frame '((width . 90) (height . 30)))))
      (should (= 90 (emacs-frame-frame-width f)))
      (should (= 30 (emacs-frame-frame-height f)))
      (emacs-frame-set-frame-size f 100 40)
      (should (= 100 (emacs-frame-frame-width f)))
      (should (= 40 (emacs-frame-frame-height f)))
      (emacs-frame-make-frame-invisible f)
      (should-not (emacs-frame-frame-visible-p f))
      (emacs-frame-make-frame-visible f)
      (should (emacs-frame-frame-visible-p f))
      (emacs-frame-select-frame f)
      (should (eq f (emacs-frame-selected-frame)))
      (should (= emacs-frame--display-cols
                 (emacs-frame-display-pixel-width))))))

(ert-deftest emacs-frame-builtins-test/bridge-overwrites-standalone-stubs-in-source ()
  ;; The bridge must replace `emacs-stub.el' sentinels in standalone
  ;; NeLisp, while preserving host Emacs builtins.
  (should (fboundp 'emacs-frame-builtins--install-function-p))
  (should-not (emacs-frame-builtins--install-function-p 'make-frame))
  (let ((emacs-version 'nelisp--unbound-marker))
    (should (emacs-frame-builtins--install-function-p 'make-frame)))
  (let* ((file (locate-library "emacs-frame-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym '(make-frame framep frame-live-p frame-list
                     selected-frame window-frame delete-frame
                     delete-other-frames frame-width frame-height
                     set-frame-size frame-visible-p select-frame
                     frame-windows display-pixel-width display-pixel-height))
        (goto-char (point-min))
        (should (search-forward
                 (format "(when (emacs-frame-builtins--install-function-p '%s)" sym)
                 nil t))))))

;;;; H. Idempotence

(ert-deftest emacs-frame-builtins-test/require-is-idempotent ()
  (let ((before-make-frame      (symbol-function 'make-frame))
        (before-framep          (symbol-function 'framep))
        (before-frame-parameter (symbol-function 'frame-parameter))
        (before-frame-width     (symbol-function 'frame-width)))
    (require 'emacs-frame-builtins)
    (should (eq before-make-frame      (symbol-function 'make-frame)))
    (should (eq before-framep          (symbol-function 'framep)))
    (should (eq before-frame-parameter (symbol-function 'frame-parameter)))
    (should (eq before-frame-width     (symbol-function 'frame-width)))))

(provide 'emacs-frame-builtins-test)

;;; emacs-frame-builtins-test.el ends here
