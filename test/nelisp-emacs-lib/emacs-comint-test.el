;;; emacs-comint-test.el --- ERT for emacs-comint  -*- lexical-binding: t; -*-

;;; Commentary:

;; comint machinery tests.  The output mark, input ring, and send-input lift
;; are pure buffer units (no live process).  A separate test drives a real
;; `/bin/cat' subprocess to exercise the end-to-end filter path on host.

;;; Code:

(require 'ert)
(require 'emacs-comint)

;;;; --- output filter (mark advance) ---------------------------------

(ert-deftest emacs-comint-test/output-filter-inserts-and-advances ()
  (with-temp-buffer
    (emacs-comint-mode)
    (insert "prompt$ ")
    (emacs-comint--set-mark (point-max))
    (emacs-comint-output-filter nil "hello\n")
    (emacs-comint-output-filter nil "world\n")
    (should (string-suffix-p "hello\nworld\n" (buffer-string)))
    (should (= (emacs-comint--mark) (point-max)))))

;;;; --- input ring ---------------------------------------------------

(ert-deftest emacs-comint-test/input-ring-records-and-skips-blank ()
  (with-temp-buffer
    (emacs-comint-mode)
    (emacs-comint-add-to-input-history "one")
    (emacs-comint-add-to-input-history "two")
    (emacs-comint-add-to-input-history "   ")   ; blank -> skipped
    (emacs-comint-add-to-input-history "")       ; empty -> skipped
    (should (equal '("two" "one") (emacs-comint-input-ring)))))

(ert-deftest emacs-comint-test/input-ring-navigation ()
  (with-temp-buffer
    (emacs-comint-mode)
    (emacs-comint--set-mark (point-max))
    (emacs-comint-add-to-input-history "first")
    (emacs-comint-add-to-input-history "second")
    (goto-char (point-max))
    ;; previous walks toward older: newest (index 0) is "second"
    (should (equal "second" (emacs-comint-previous-input 1)))
    (should (string-suffix-p "second" (buffer-string)))
    (should (equal "first" (emacs-comint-previous-input 1)))
    (should (string-suffix-p "first" (buffer-string)))
    ;; next walks back toward newer
    (should (equal "second" (emacs-comint-next-input 1)))
    (should (string-suffix-p "second" (buffer-string)))))

;;;; --- send-input ---------------------------------------------------

(ert-deftest emacs-comint-test/send-input-lifts-records-advances ()
  (with-temp-buffer
    (emacs-comint-mode)
    (emacs-comint--set-mark (point-max))
    (insert "echo hi")
    (let ((sent (emacs-comint-send-input)))
      (should (equal "echo hi" sent))
      (should (equal '("echo hi") (emacs-comint-input-ring)))
      (should (string-suffix-p "echo hi\n" (buffer-string)))
      (should (= (emacs-comint--mark) (point-max))))))

;;;; --- real subprocess round-trip (host) ----------------------------

(ert-deftest emacs-comint-test/subprocess-roundtrip ()
  (skip-unless (file-executable-p "/bin/cat"))
  (let ((buf (emacs-comint-make "comint-cat-test" nil "/bin/cat")))
    (unwind-protect
        (with-current-buffer buf
          (let ((proc (get-buffer-process buf)))
            (should proc)
            (should (process-live-p proc))
            (process-send-string proc "round-trip-line\n")
            (accept-process-output proc 2)
            ;; cat echoes the line; the comint filter inserts it at the mark
            (should (string-match-p "round-trip-line" (buffer-string)))))
      (let ((proc (get-buffer-process buf)))
        (when (and proc (process-live-p proc)) (delete-process proc)))
      (kill-buffer buf))))

(provide 'emacs-comint-test)

;;; emacs-comint-test.el ends here
