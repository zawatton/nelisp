;;; nelisp-backtrace-handled-snapshot-test.el --- uncaught-error backtrace freshness gate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The standalone prints a bounded backtrace for an uncaught error from
;; `nl_bt_snapshot', captured at the innermost frame the FIRST time a
;; call returns non-zero in a top-level form.  The snapshot used to be
;; reset only once per top-level form, so an error that a
;; `condition-case' or `catch' had already handled kept its snapshot and
;; a later, unrelated uncaught error was reported with the handled
;; error's frames.  Loading GNU bytecomp.el showed this as
;; `(void-function loop)' printed over `copy-alist' or face-resolution
;; frames that had nothing to do with `loop'.  Each case runs on the
;; built standalone binary.

;;; Code:

(require 'ert)

(defun nelisp-backtrace-handled-snapshot--run (forms)
  "Load FORMS on the standalone and return its combined output."
  (let ((binary (expand-file-name (or (getenv "NELISP_BIN") "target/nelisp")
                                  default-directory))
        (file (make-temp-file "nelisp-bt-snapshot-" nil ".el")))
    (unless (file-executable-p binary)
      (ert-skip "standalone binary is not built; standalone-reader gate owns it"))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; -*- lexical-binding: t; -*-\n" forms "\n"))
          (with-temp-buffer
            (call-process binary nil t nil "--load" file)
            (buffer-string)))
      (delete-file file))))

(defun nelisp-backtrace-handled-snapshot--frames (output)
  "Return the frame names printed in OUTPUT's backtrace, innermost first."
  (let ((frames nil) (start 0))
    (while (string-match "^ +[0-9]+: \\(.*\\)$" output start)
      (push (match-string 1 output) frames)
      (setq start (match-end 0)))
    (nreverse frames)))

(ert-deftest nelisp-backtrace-handled-snapshot-condition-case ()
  "A handled `condition-case' error must not leak into a later backtrace."
  (let* ((out (nelisp-backtrace-handled-snapshot--run
               "(defun bt-handled () (condition-case nil (car 5) (error 'caught)))
(defun bt-fail () (funcall 'bt-missing-fn))
(defun bt-top () (let ((x 1)) (bt-handled) (bt-fail)))
(bt-top)"))
         (frames (nelisp-backtrace-handled-snapshot--frames out)))
    (should (string-match-p "void-function: (bt-missing-fn)" out))
    (should (equal (car frames) "funcall"))
    (should (member "bt-fail" frames))
    (should-not (member "bt-handled" frames))
    (should-not (member "car" frames))))

(ert-deftest nelisp-backtrace-handled-snapshot-catch ()
  "A `throw' caught by `catch' must not leak into a later backtrace."
  (let* ((out (nelisp-backtrace-handled-snapshot--run
               "(defun bt-thrower () (catch 'done (list (throw 'done 1))))
(defun bt-fail () (funcall 'bt-missing-fn))
(defun bt-top () (bt-thrower) (bt-fail))
(bt-top)"))
         (frames (nelisp-backtrace-handled-snapshot--frames out)))
    (should (string-match-p "void-function: (bt-missing-fn)" out))
    (should (equal (car frames) "funcall"))
    (should (member "bt-fail" frames))
    (should-not (member "bt-thrower" frames))))

(ert-deftest nelisp-backtrace-handled-snapshot-unwind-protect-keeps-innermost ()
  "An uncaught error through `unwind-protect' keeps its innermost frame."
  (let* ((out (nelisp-backtrace-handled-snapshot--run
               "(defun bt-inner () (funcall 'bt-missing-fn))
(defun bt-top () (unwind-protect (bt-inner) (+ 1 2)))
(bt-top)"))
         (frames (nelisp-backtrace-handled-snapshot--frames out)))
    (should (equal (car frames) "funcall"))
    (should (member "bt-inner" frames))))

(provide 'nelisp-backtrace-handled-snapshot-test)

;;; nelisp-backtrace-handled-snapshot-test.el ends here
