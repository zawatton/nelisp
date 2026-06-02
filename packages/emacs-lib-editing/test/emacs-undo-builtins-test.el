;;; emacs-undo-builtins-test.el --- ERT for emacs-undo  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 undo subsystem (Track E.2).  Under host
;; Emacs the unprefixed bridges are gated off (= host's C builtins
;; win), so behavioural assertions exercise the prefixed
;; `emacs-undo-*' API directly against the substrate buffer
;; surface.  Featurep / fboundp / boundp parity is checked
;; separately.

;;; Code:

(require 'ert)
(require 'emacs-undo-builtins)
(require 'emacs-edit-builtins)
(require 'cl-lib)

(defmacro emacs-undo-builtins-test--with-fresh-buffer (text &rest body)
  "Run BODY against a fresh nelisp-ec buffer pre-filled with TEXT,
with a clean undo-list alist and kill-ring."
  (declare (indent 1) (debug (form body)))
  (let ((buf (make-symbol "buf")))
    `(let ((nelisp-ec--buffers nil)
           (nelisp-ec--current-buffer nil)
           (nelisp-ec--match-data nil)
           (kill-ring nil)
           (kill-ring-yank-pointer nil))
       (emacs-undo-reset)
       (let ((,buf (nelisp-ec-generate-new-buffer "undo")))
         (unwind-protect
             (nelisp-ec-with-current-buffer ,buf
               (nelisp-ec-insert ,text)
               (nelisp-ec-goto-char (nelisp-ec-point-min))
               ;; Clear the undo list created by the seed insert.
               (emacs-undo-set-buffer-undo-list nil)
               ,@body)
           (emacs-undo-reset)
           (nelisp-ec-kill-buffer ,buf))))))

;;;; A. Load cleanly + fboundp / boundp parity

(ert-deftest emacs-undo-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-undo-builtins))
  (should (featurep 'emacs-undo))
  (dolist (sym '(undo undo-boundary primitive-undo
                 buffer-disable-undo buffer-enable-undo))
    (should (fboundp sym)))
  (should (boundp 'buffer-undo-list)))

;;;; B. record-insert pushes (BEG . END)

(ert-deftest emacs-undo-builtins-test/record-insert-pushes-cons ()
  (emacs-undo-builtins-test--with-fresh-buffer ""
    (emacs-undo-record-insert 1 4)
    (let ((lst (emacs-undo-buffer-undo-list)))
      (should (equal '((1 . 4)) lst)))))

(ert-deftest emacs-undo-builtins-test/record-insert-coalesces-adjacent-head ()
  "Consecutive printable inserts should extend one undo record."
  (emacs-undo-builtins-test--with-fresh-buffer ""
    (emacs-undo-record-insert 1 2)
    (emacs-undo-record-insert 2 3)
    (emacs-undo-record-insert 3 4)
    (should (equal '((1 . 4)) (emacs-undo-buffer-undo-list)))))

(ert-deftest emacs-undo-builtins-test/record-insert-does-not-coalesce-across-boundary ()
  "A boundary keeps adjacent insertions in separate undo groups."
  (emacs-undo-builtins-test--with-fresh-buffer ""
    (emacs-undo-record-insert 1 2)
    (emacs-undo-undo-boundary)
    (emacs-undo-record-insert 2 3)
    (should (equal '((2 . 3) nil (1 . 2))
                   (emacs-undo-buffer-undo-list)))))

;;;; C. record-delete pushes (STRING . POS)

(ert-deftest emacs-undo-builtins-test/record-delete-pushes-cons ()
  (emacs-undo-builtins-test--with-fresh-buffer ""
    (emacs-undo-record-delete "abc" 5)
    (let ((lst (emacs-undo-buffer-undo-list)))
      (should (equal '(("abc" . 5)) lst)))))

;;;; D. undo-boundary prepends nil — but collapses repeats

(ert-deftest emacs-undo-builtins-test/undo-boundary-collapses ()
  (emacs-undo-builtins-test--with-fresh-buffer ""
    (emacs-undo-record-insert 1 2)
    (emacs-undo-undo-boundary)
    (emacs-undo-undo-boundary)
    (let ((lst (emacs-undo-buffer-undo-list)))
      ;; Two consecutive boundaries → still just one nil at head.
      (should (equal '(nil (1 . 2)) lst)))))

;;;; E. primitive-undo applies an insertion-record by deleting

(ert-deftest emacs-undo-builtins-test/primitive-undo-deletes-inserted ()
  (emacs-undo-builtins-test--with-fresh-buffer "hello"
    ;; Pretend "hello" (chars 1..6) was just inserted.
    (let ((rest (emacs-undo-primitive-undo 1 '((1 . 6) nil))))
      (should (equal nil rest))
      (should (equal "" (nelisp-ec-buffer-string))))))

;;;; F. primitive-undo applies a deletion-record by re-inserting

(ert-deftest emacs-undo-builtins-test/primitive-undo-reinserts-deleted ()
  (emacs-undo-builtins-test--with-fresh-buffer "ab"
    ;; Pretend "X" was deleted at position 2.
    (let ((rest (emacs-undo-primitive-undo 1 '(("X" . 2) nil))))
      (should (equal nil rest))
      (should (equal "aXb" (nelisp-ec-buffer-string))))))

;;;; G. Insertion + undo roundtrip via substrate

;; Under host Emacs the unprefixed `self-insert-command' goes to the
;; host C builtin (= our defalias gate skips), so we exercise the
;; SAME control flow our defun runs by calling
;; `nelisp-ec-insert' + `emacs-undo-record-insert' explicitly.
(ert-deftest emacs-undo-builtins-test/insert-undo-roundtrip ()
  (emacs-undo-builtins-test--with-fresh-buffer ""
    (let ((beg (nelisp-ec-point)))
      (nelisp-ec-insert "a")
      (emacs-undo-record-insert beg (nelisp-ec-point)))
    (should (equal "a" (nelisp-ec-buffer-string)))
    (emacs-undo-undo-boundary)
    (emacs-undo-undo)
    (should (equal "" (nelisp-ec-buffer-string)))))

;;;; H. Deletion + undo roundtrip via substrate

(ert-deftest emacs-undo-builtins-test/delete-undo-roundtrip ()
  (emacs-undo-builtins-test--with-fresh-buffer "abcd"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let* ((p (nelisp-ec-point))
           (start (- p 2))
           (text (nelisp-ec-buffer-substring start p)))
      (nelisp-ec-delete-char -2)
      (emacs-undo-record-delete text start))
    (should (equal "ab" (nelisp-ec-buffer-string)))
    (emacs-undo-undo-boundary)
    (emacs-undo-undo)
    (should (equal "abcd" (nelisp-ec-buffer-string)))))

;;;; I. kill-region body + undo roundtrip

(ert-deftest emacs-undo-builtins-test/kill-region-body-undo-roundtrip ()
  (emacs-undo-builtins-test--with-fresh-buffer "hello world"
    (let* ((s 7) (e 12)
           (text (nelisp-ec-buffer-substring s e)))
      (kill-new text)
      (nelisp-ec-delete-region s e)
      (emacs-undo-record-delete text s))
    (should (equal "hello " (nelisp-ec-buffer-string)))
    (should (equal "world" (car kill-ring)))
    (emacs-undo-undo-boundary)
    (emacs-undo-undo)
    (should (equal "hello world" (nelisp-ec-buffer-string)))))

;;;; J. yank body + undo roundtrip

(ert-deftest emacs-undo-builtins-test/yank-body-undo-roundtrip ()
  (emacs-undo-builtins-test--with-fresh-buffer "AB"
    (let ((kill-ring '("XYZ")))
      (nelisp-ec-goto-char (nelisp-ec-point-max))
      (let ((beg (nelisp-ec-point)))
        (nelisp-ec-insert (car kill-ring))
        (emacs-undo-record-insert beg (nelisp-ec-point)))
      (should (equal "ABXYZ" (nelisp-ec-buffer-string)))
      (emacs-undo-undo-boundary)
      (emacs-undo-undo)
      (should (equal "AB" (nelisp-ec-buffer-string))))))

;;;; K. buffer-disable-undo / buffer-enable-undo

(ert-deftest emacs-undo-builtins-test/disable-undo-suppresses-records ()
  (emacs-undo-builtins-test--with-fresh-buffer ""
    ;; Use the prefixed setter to bypass any host-Emacs gate.
    (emacs-undo-set-buffer-undo-list t)
    (should (eq t (emacs-undo-buffer-undo-list)))
    ;; Records become no-ops while disabled.
    (emacs-undo-record-insert 1 5)
    (should (eq t (emacs-undo-buffer-undo-list)))
    (emacs-undo-set-buffer-undo-list nil)
    (should (null (emacs-undo-buffer-undo-list)))))

(ert-deftest emacs-undo-builtins-test/undo-on-disabled-signals ()
  (emacs-undo-builtins-test--with-fresh-buffer ""
    (buffer-disable-undo)
    (should-error (emacs-undo-undo) :type 'emacs-undo-error)))

;;;; L. undo on empty list signals

(ert-deftest emacs-undo-builtins-test/undo-with-no-records-signals ()
  (emacs-undo-builtins-test--with-fresh-buffer ""
    (should-error (emacs-undo-undo) :type 'emacs-undo-error)))

;;;; M. Two-group undo flow — only the most recent group is undone

(ert-deftest emacs-undo-builtins-test/undo-undoes-one-group ()
  (emacs-undo-builtins-test--with-fresh-buffer ""
    ;; Group 1: insert "a"
    (let ((beg (nelisp-ec-point)))
      (nelisp-ec-insert "a")
      (emacs-undo-record-insert beg (nelisp-ec-point)))
    (emacs-undo-undo-boundary)
    ;; Group 2: insert "b"
    (let ((beg (nelisp-ec-point)))
      (nelisp-ec-insert "b")
      (emacs-undo-record-insert beg (nelisp-ec-point)))
    (emacs-undo-undo-boundary)
    (should (equal "ab" (nelisp-ec-buffer-string)))
    (emacs-undo-undo)                    ; undoes "b"
    (should (equal "a" (nelisp-ec-buffer-string)))
    (emacs-undo-undo)                    ; undoes "a"
    (should (equal "" (nelisp-ec-buffer-string)))))

;;;; N. Idempotence

(ert-deftest emacs-undo-builtins-test/require-is-idempotent ()
  (let ((before-undo  (symbol-function 'undo))
        (before-bnd   (symbol-function 'undo-boundary))
        (before-prim  (symbol-function 'primitive-undo))
        (before-disable (symbol-function 'buffer-disable-undo)))
    (require 'emacs-undo-builtins)
    (should (eq before-undo (symbol-function 'undo)))
    (should (eq before-bnd  (symbol-function 'undo-boundary)))
    (should (eq before-prim (symbol-function 'primitive-undo)))
    (should (eq before-disable (symbol-function 'buffer-disable-undo)))))

(ert-deftest emacs-undo-builtins-test/bridge-overwrites-standalone-stubs-in-source ()
  (should (fboundp 'emacs-undo-builtins--install-function-p))
  (should-not (emacs-undo-builtins--install-function-p 'undo))
  (let* ((file (locate-library "emacs-undo-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym '(undo undo-boundary primitive-undo
                     buffer-disable-undo buffer-enable-undo))
        (goto-char (point-min))
        (should (search-forward
                 (format "(when (emacs-undo-builtins--install-function-p '%s)"
                         sym)
                 nil t))))))

(provide 'emacs-undo-builtins-test)

;;; emacs-undo-builtins-test.el ends here
