;;; emacs-xref-test.el --- ERT for emacs-xref  -*- lexical-binding: t; -*-

;;; Commentary:

;; Jump-to-definition tests.  Current-buffer lookup is a pure buffer unit;
;; cross-file lookup uses throwaway `.el' files; the jump-back stack is
;; exercised end-to-end.  Validates the Layer 2 logic independently of the
;; reader.

;;; Code:

(require 'ert)
(require 'emacs-xref)

(defconst emacs-xref-test--source
  (concat
   "(defun foo (x) (+ x 1))\n"
   "(defvar bar 42)\n"
   "(defun caller () (foo bar))\n")
  "A small buffer with two defs and a use site.")

;;;; --- symbol at point ----------------------------------------------

(ert-deftest emacs-xref-test/symbol-at-point ()
  (with-temp-buffer
    (insert "(foo hello-world bar)")
    (goto-char (point-min))
    (search-forward "hello")
    (should (equal "hello-world" (emacs-xref--symbol-at-point)))))

(ert-deftest emacs-xref-test/symbol-at-point-nil-on-blank ()
  (with-temp-buffer
    (insert "   ")
    (goto-char (point-min))
    (should (null (emacs-xref--symbol-at-point)))))

;;;; --- current-buffer definition ------------------------------------

(ert-deftest emacs-xref-test/find-in-current-buffer ()
  (with-temp-buffer
    (insert emacs-xref-test--source)
    (setq emacs-xref--marker-stack nil)
    (goto-char (point-max))
    (let* ((origin (point))
           (hit (emacs-xref-find-definitions "foo")))
      (should (eq (car hit) (current-buffer)))
      (should (= (point) (cdr hit)))
      (should (= ?\( (char-after (point))))
      (should (string-prefix-p "(defun foo"
                               (buffer-substring-no-properties
                                (point) (min (point-max) (+ (point) 10)))))
      ;; the jump-back stack recorded where we started
      (should (= 1 (length emacs-xref--marker-stack)))
      (should (= origin (cdr (car emacs-xref--marker-stack)))))))

(ert-deftest emacs-xref-test/pop-marker-stack-returns ()
  (with-temp-buffer
    (insert emacs-xref-test--source)
    (setq emacs-xref--marker-stack nil)
    (goto-char (point-max))
    (let ((origin (point)))
      (emacs-xref-find-definitions "bar")
      (should (/= (point) origin))
      (emacs-xref-pop-marker-stack)
      (should (= (point) origin))
      (should (null emacs-xref--marker-stack)))))

(ert-deftest emacs-xref-test/pop-empty-stack-signals ()
  (setq emacs-xref--marker-stack nil)
  (should-error (emacs-xref-pop-marker-stack)))

;;;; --- cross-file definition ----------------------------------------

(ert-deftest emacs-xref-test/find-in-file ()
  (let* ((dir (make-temp-file "emacs-xref-test-" t))
         (file (expand-file-name "lib.el" dir))
         (other (expand-file-name "noise.el" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";; a library\n(defun target-fn (a b) (+ a b))\n"))
          (with-temp-file other
            (insert "(defvar unrelated 1)\n"))
          (with-temp-buffer
            (insert "(defun local-only () nil)\n")
            (setq emacs-xref--marker-stack nil)
            (let ((hit (emacs-xref-find-definitions
                        "target-fn" (list other file))))
              (should (bufferp (car hit)))
              (with-current-buffer (car hit)
                (should (equal (file-truename file)
                               (file-truename (buffer-file-name))))
                (goto-char (cdr hit))
                (should (string-prefix-p "(defun target-fn"
                                         (buffer-substring-no-properties
                                          (point)
                                          (min (point-max) (+ (point) 17))))))
              (when (buffer-live-p (car hit)) (kill-buffer (car hit))))))
      (delete-directory dir t))))

(ert-deftest emacs-xref-test/not-found-signals ()
  (let* ((dir (make-temp-file "emacs-xref-test-" t))
         (file (expand-file-name "empty.el" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "(defvar something-else 1)\n"))
          (with-temp-buffer
            (insert "(defun unrelated-local () nil)\n")
            ;; non-empty FILES list avoids the default-directory scan
            (should-error
             (emacs-xref-find-definitions "no-such-defn" (list file)))))
      (delete-directory dir t))))

(provide 'emacs-xref-test)

;;; emacs-xref-test.el ends here
