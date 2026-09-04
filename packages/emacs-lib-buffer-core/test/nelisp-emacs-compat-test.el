;;; nelisp-emacs-compat-test.el --- ERT for nelisp-emacs-compat  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the `nelisp-ec-*' buffer/point/marker substrate (T39).
;; This module previously shipped with no dedicated ERT file even
;; though it backs `with-temp-buffer', `with-current-buffer',
;; `save-excursion', `save-restriction', and `save-current-buffer'
;; (via `emacs-buffer-builtins').
;;
;; Doc 33 §8 item 222: `nelisp-ec-with-current-buffer',
;; `nelisp-ec-save-excursion', `nelisp-ec-save-restriction', and
;; `nelisp-ec-save-current-buffer' were written as backquote templates.
;; Under host Emacs backquote works fine, so these tests cannot
;; reproduce the standalone-reader-only failure directly; they instead
;; pin the functional contract (BODY's value is returned, prior state
;; is restored on normal exit AND on error) that the backquote-free
;; rewrite must keep intact.  The standalone reproduction and fix
;; verification live in the Doc 33 §8 item 222 gate transcript.

;;; Code:

(require 'ert)
(require 'nelisp-emacs-compat)

(defmacro nelisp-emacs-compat-test--with-fresh-world (&rest body)
  "Run BODY with a clean NeLisp buffer registry/current-buffer state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil))
     ,@body))

;;;; A. Load cleanly

(ert-deftest nelisp-emacs-compat-test/require-loads-cleanly ()
  (should (featurep 'nelisp-emacs-compat))
  (dolist (sym '(nelisp-ec-generate-new-buffer nelisp-ec-current-buffer
                 nelisp-ec-set-buffer nelisp-ec-with-current-buffer
                 nelisp-ec-kill-buffer nelisp-ec-save-excursion
                 nelisp-ec-save-restriction nelisp-ec-save-current-buffer))
    (should (fboundp sym))))

;;;; B. nelisp-ec-with-current-buffer

(ert-deftest nelisp-emacs-compat-test/with-current-buffer-returns-body-value ()
  (nelisp-emacs-compat-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "wcb-value")))
      (should (eq 'the-body-value
                  (nelisp-ec-with-current-buffer buf 'ignored-form
                                                  'the-body-value))))))

(ert-deftest nelisp-emacs-compat-test/with-current-buffer-switches-and-restores ()
  (nelisp-emacs-compat-test--with-fresh-world
    (let ((outer (nelisp-ec-generate-new-buffer "wcb-outer"))
          (inner (nelisp-ec-generate-new-buffer "wcb-inner")))
      (nelisp-ec-set-buffer outer)
      (let ((seen (nelisp-ec-with-current-buffer inner
                    (nelisp-ec-current-buffer))))
        (should (eq inner seen))
        (should (eq outer (nelisp-ec-current-buffer)))))))

(ert-deftest nelisp-emacs-compat-test/with-current-buffer-restores-on-error ()
  (nelisp-emacs-compat-test--with-fresh-world
    (let ((outer (nelisp-ec-generate-new-buffer "wcb-err-outer"))
          (inner (nelisp-ec-generate-new-buffer "wcb-err-inner")))
      (nelisp-ec-set-buffer outer)
      (should-error
       (nelisp-ec-with-current-buffer inner (error "boom")))
      (should (eq outer (nelisp-ec-current-buffer))))))

;;;; C. nelisp-ec-save-excursion

(ert-deftest nelisp-emacs-compat-test/save-excursion-returns-body-value ()
  (nelisp-emacs-compat-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "save-exc-value")))
      (nelisp-ec-set-buffer buf)
      (nelisp-ec-insert "hello world")
      (should (eq 'the-body-value
                  (nelisp-ec-save-excursion 'the-body-value))))))

(ert-deftest nelisp-emacs-compat-test/save-excursion-restores-point ()
  (nelisp-emacs-compat-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "save-exc-point")))
      (nelisp-ec-set-buffer buf)
      (nelisp-ec-insert "hello world")
      (nelisp-ec-goto-char 3)
      (nelisp-ec-save-excursion
        (nelisp-ec-goto-char 7))
      (should (= 3 (nelisp-ec-point))))))

(ert-deftest nelisp-emacs-compat-test/save-excursion-restores-point-on-error ()
  (nelisp-emacs-compat-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "save-exc-err")))
      (nelisp-ec-set-buffer buf)
      (nelisp-ec-insert "hello world")
      (nelisp-ec-goto-char 3)
      (should-error
       (nelisp-ec-save-excursion
         (nelisp-ec-goto-char 7)
         (error "boom")))
      (should (= 3 (nelisp-ec-point))))))

;;;; D. nelisp-ec-save-restriction

(ert-deftest nelisp-emacs-compat-test/save-restriction-returns-body-value ()
  (nelisp-emacs-compat-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "save-restr-value")))
      (nelisp-ec-set-buffer buf)
      (nelisp-ec-insert "hello world")
      (should (eq 'the-body-value
                  (nelisp-ec-save-restriction 'the-body-value))))))

(ert-deftest nelisp-emacs-compat-test/save-restriction-restores-narrowing ()
  (nelisp-emacs-compat-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "save-restr-narrow")))
      (nelisp-ec-set-buffer buf)
      (nelisp-ec-insert "hello world")
      (let ((before-min (nelisp-ec-point-min))
            (before-max (nelisp-ec-point-max)))
        (nelisp-ec-save-restriction
          (nelisp-ec-narrow-to-region 2 5))
        (should (= before-min (nelisp-ec-point-min)))
        (should (= before-max (nelisp-ec-point-max)))))))

(ert-deftest nelisp-emacs-compat-test/save-restriction-restores-narrowing-on-error ()
  (nelisp-emacs-compat-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "save-restr-err")))
      (nelisp-ec-set-buffer buf)
      (nelisp-ec-insert "hello world")
      (let ((before-min (nelisp-ec-point-min))
            (before-max (nelisp-ec-point-max)))
        (should-error
         (nelisp-ec-save-restriction
           (nelisp-ec-narrow-to-region 2 5)
           (error "boom")))
        (should (= before-min (nelisp-ec-point-min)))
        (should (= before-max (nelisp-ec-point-max)))))))

;;;; E. nelisp-ec-save-current-buffer

(ert-deftest nelisp-emacs-compat-test/save-current-buffer-returns-body-value ()
  (nelisp-emacs-compat-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "save-cur-value")))
      (nelisp-ec-set-buffer buf)
      (should (eq 'the-body-value
                  (nelisp-ec-save-current-buffer 'the-body-value))))))

(ert-deftest nelisp-emacs-compat-test/save-current-buffer-restores ()
  (nelisp-emacs-compat-test--with-fresh-world
    (let ((outer (nelisp-ec-generate-new-buffer "save-cur-outer"))
          (inner (nelisp-ec-generate-new-buffer "save-cur-inner")))
      (nelisp-ec-set-buffer outer)
      (nelisp-ec-save-current-buffer
        (nelisp-ec-set-buffer inner)
        (should (eq inner (nelisp-ec-current-buffer))))
      (should (eq outer (nelisp-ec-current-buffer))))))

;;;; F. with-temp-buffer (emacs-buffer-builtins) end-to-end return value
;;
;; This is the exact reported symptom (Doc 33 §8 item 222): under host
;; Emacs `with-temp-buffer' is the host's own C-backed macro (the
;; standalone bridge in `emacs-buffer-builtins' is gated off), so this
;; test documents the contract rather than reproducing the standalone
;; bug; the standalone reproduction is in the gate transcript.

(ert-deftest nelisp-emacs-compat-test/host-with-temp-buffer-returns-body-value ()
  (should (eq t (with-temp-buffer t))))

(provide 'nelisp-emacs-compat-test)

;;; nelisp-emacs-compat-test.el ends here
