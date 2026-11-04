;;; nelisp-producer-prep-s213-test.el --- S2.13 preparation trims -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Checks the producer's circle-free reader fast path and the removal of cl-seq
;; autoload calls.  Set NELISP_PRODUCER_UNDER_TEST to another producer file for
;; a negative control.
;;; Code:
(require 'ert)
(require 'cl-lib)
(defconst nelisp-producer-s213-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(defconst nelisp-producer-s213-test--source
  (or (getenv "NELISP_PRODUCER_UNDER_TEST")
      (expand-file-name "../tools/nelisp-native-application-producer.el"
                        nelisp-producer-s213-test--dir)))
(load (expand-file-name "../lisp/nelisp-native-application-protocol.el"
                        nelisp-producer-s213-test--dir) nil t)

(defun nelisp-producer-s213-test--text ()
  (with-temp-buffer (insert-file-contents nelisp-producer-s213-test--source)
                    (buffer-string)))

(defun nelisp-producer-s213-test--fn (name)
  (let* ((s (nelisp-producer-s213-test--text))
         (start (string-match (concat "^(defun " (regexp-quote name) "\\_>") s)))
    (and start (car (read-from-string s start)))))

(defun nelisp-producer-s213-test--forms (bytes)
  (let ((def (nelisp-producer-s213-test--fn "nelisp-native-application-producer--forms")))
    (should def)
    (eval def t)
    (funcall 'nelisp-native-application-producer--forms bytes)))

(ert-deftest nelisp-producer-s213-forms-equal-protocol ()
  (dolist (text '("(defun a (x) x)\n;; c\n(defun b () 1)\n"
                  "(a . b) [1 2] \"str\" ?# ?a 'q #'f #b101 #x1f"
                  "(defun a () \"has #1 inside\")"
                  "  \n\n"))
    (let ((bytes (encode-coding-string text 'utf-8-unix)))
      (should (equal (nelisp-producer-s213-test--forms bytes)
                     (nelisp-native-application-protocol-forms bytes))))))

(ert-deftest nelisp-producer-s213-circle-syntax-falls-back ()
  ;; `#N=' text must reach the protocol reader (read-circle nil), which rejects it.
  (let ((bytes (encode-coding-string "(a #1=(b) #1#)" 'utf-8-unix)))
    (should-error (nelisp-native-application-protocol-forms bytes))
    (should-error (nelisp-producer-s213-test--forms bytes))))

(ert-deftest nelisp-producer-s213-structure ()
  (let ((s (nelisp-producer-s213-test--text)))
    (should-not (string-match-p "(cl-find\\_>\\|(cl-remove-if\\_>\\|(cl-remove-if-not\\_>" s))
    (should-not (string-match-p "(nelisp-native-application-protocol-forms bytes)))\n          (when" s))
    (let ((ro (format "%S" (nelisp-producer-s213-test--fn
                            "nelisp-native-application-producer--read-one")))
          (pre (format "%S" (nelisp-producer-s213-test--fn
                             "nelisp-native-application-producer-preflight"))))
      (should (string-match-p "producer--forms" ro))
      (should (string-match-p "producer--forms" pre)))))
(provide 'nelisp-producer-prep-s213-test)
