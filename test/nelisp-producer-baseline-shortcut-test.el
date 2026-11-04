;;; nelisp-producer-baseline-shortcut-test.el --- baseline shortcut + hex check -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Structural checks for the producer's pinned native-row baseline shortcut and
;; the regexp-free SHA-256 hex check.  Set NELISP_PRODUCER_UNDER_TEST to point at
;; another producer file (negative control).
;;; Code:
(require 'ert)
(require 'cl-lib)
(defconst nelisp-producer-shortcut-test--source
  (or (getenv "NELISP_PRODUCER_UNDER_TEST")
      (expand-file-name "../tools/nelisp-native-application-producer.el"
                        (file-name-directory (or load-file-name buffer-file-name)))))
(defun nelisp-producer-shortcut-test--text ()
  (with-temp-buffer (insert-file-contents nelisp-producer-shortcut-test--source) (buffer-string)))
(defun nelisp-producer-shortcut-test--defun (name)
  (let* ((s (nelisp-producer-shortcut-test--text))
         (start (string-match (concat "^(defun " (regexp-quote name) "\\_>") s)))
    (and start (car (read-from-string s start)))))

(defun nelisp-producer-shortcut-test--count (needle text)
  (let ((n 0) (p 0))
    (while (string-match (regexp-quote needle) text p)
      (setq n (1+ n) p (match-end 0)))
    n))

(ert-deftest nelisp-producer-baseline-shortcut-structure ()
  (let* ((s (nelisp-producer-shortcut-test--text))
         (fn (format "%S" (nelisp-producer-shortcut-test--defun
                           "nelisp-native-application-producer--baseline-matches-p"))))
    (should (string-match-p "row-data-pins" s))
    (should (string-match-p "row-baseline-pin" s))
    ;; Every deviation input is checked, and a full recompute remains the fallback.
    (dolist (needle '("nelisp-cc-fxir-native-windows" "nelisp-cc-fxir-native--additional-rows"
                      "row-function-cells" "nelisp-cc-fxir-native--data-files"
                      "secure-hash" "nelisp-cc-fxir-native-all-rows"
                      "nelisp-native-application-protocol-digest"))
      (should (string-match-p (regexp-quote needle) fn)))
    ;; Preflight and recheck both call the shortcut (the defun itself is "(defun ...").
    (should (= 2 (nelisp-producer-shortcut-test--count
                  "(nelisp-native-application-producer--baseline-matches-p" s)))
    ;; The only direct row recompute left is the shortcut's fallback.
    (should (= 1 (nelisp-producer-shortcut-test--count "(nelisp-cc-fxir-native-all-rows)" s)))))

(ert-deftest nelisp-producer-hex-check-equivalent-to-regexp ()
  (let ((form (nelisp-producer-shortcut-test--defun
               "nelisp-native-application-producer--sha256-hex-p")))
    (should form)
    (eval form t)
    (let ((h (make-string 64 ?a)) (cases nil))
      (setq cases (list h (upcase h) (make-string 64 ?0) (make-string 63 ?a) (make-string 65 ?a) ""
                        (concat (substring h 0 63) "g") (concat (substring h 0 63) "G")
                        (concat "/" (substring h 1)) (concat ":" (substring h 1))
                        (concat "`" (substring h 1)) (concat "@" (substring h 1))
                        (concat h "\n") (concat (substring h 0 63) "\n")
                        (concat (substring h 0 63) (string #x660)) nil 'abc 12))
      (dolist (c (append cases (mapcar #'string (number-sequence 0 255))))
        (let ((rx (and (stringp c) (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" c) t)))
          (should (eq rx (and (nelisp-native-application-producer--sha256-hex-p c) t)))))
      (dotimes (ch 256)
        (let ((s (concat (make-string 63 ?a) (string ch))))
          (should (eq (and (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" s) t)
                      (and (nelisp-native-application-producer--sha256-hex-p s) t))))))))
(provide 'nelisp-producer-baseline-shortcut-test)
